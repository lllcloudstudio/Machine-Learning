"""Yellowbrick report service for index.html.

Reads MySQL credentials from config.yml, builds Yellowbrick visuals for one table and
returns them as a PDF (default) or as a mobile-friendly HTML page of images.

  pip install flask pyyaml pandas sqlalchemy pymysql yellowbrick matplotlib scikit-learn
  python yb_report_server.py --config config.yml --port 8001

  GET /report?db=Yb-11&table=yb_occupancy              -> PDF
  GET /report?db=Yb-11&table=yb_occupancy&format=html  -> HTML (embedded PNGs)

config.yml: host/port/user/password either at the top level or under a
`mysql:` / `database:` / `db:` section (name variants user|username, password|pass|pwd).
"""
import argparse
import base64
import io
import re
import sys
import threading
import traceback
from pathlib import Path

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import yaml
from flask import Flask, Response, request
from matplotlib.backends.backend_pdf import PdfPages
from sqlalchemy import create_engine, text
from sqlalchemy.engine import URL

HERE = Path(__file__).resolve().parent
PLOTS_DIR = HERE / "plots"          # per-table plot files live here (see plots/_template.py)
sys.path.insert(0, str(HERE))       # lets plot files do `from ybplots import plot`
import ybplots

MAX_ROWS = 20000      # sample cap per table
MAX_FEATURES = 20     # columns shown in correlation / PCA
TARGET_COL = "target" # column used as y when present (tables loaded by load_yb_to_mysql.py)

app = Flask(__name__)
lock = threading.Lock()   # matplotlib is not thread-safe
engine = None


# ---------- config / db ----------
def load_creds(path):
    with open(path, encoding="utf-8") as f:
        cfg = yaml.safe_load(f) or {}
    for k in ("default", "mysql", "database", "db"):   # R config style: top-level `default:`
        if isinstance(cfg.get(k), dict):
            cfg = cfg[k]
            break

    def g(*names, default=None):
        return next((cfg[n] for n in names if n in cfg), default)

    return dict(host=g("db_host", "host", "hostname", default="localhost"),
                port=int(g("db_port", "port", default=3306)),
                user=g("db_user", "user", "username"),
                password=str(g("db_password", "password", "pass", "pwd", default="")))


def q(name):  # identifier quoting
    return "`" + name.replace("`", "``") + "`"


def read_table(db, table):
    with engine.connect() as c:
        ok = c.execute(text("SELECT 1 FROM information_schema.tables "
                            "WHERE table_schema=:d AND table_name=:t"),
                       {"d": db, "t": table}).first()
        if not ok:
            raise ValueError(f"table not found: {db}.{table}")
        return pd.read_sql(text(f"SELECT * FROM {q(db)}.{q(table)} LIMIT {MAX_ROWS}"), c)


# ---------- figures ----------
def message_fig(title, msg):
    fig = plt.figure(figsize=(8, 6))
    fig.text(0.5, 0.6, title, ha="center", fontsize=16, weight="bold")
    fig.text(0.5, 0.5, msg, ha="center", va="top", fontsize=10, wrap=True)
    return fig


def to_figure(obj):
    """Accept a Yellowbrick visualizer, a matplotlib Figure/Axes, or None (current figure)."""
    if obj is None:
        return plt.gcf()
    if hasattr(obj, "finalize"):          # Yellowbrick visualizer
        obj.finalize()
        return obj.ax.figure
    if hasattr(obj, "savefig"):           # Figure
        return obj
    if hasattr(obj, "figure"):            # Axes
        return obj.figure
    raise TypeError(f"plot returned {type(obj).__name__}; return a visualizer, Figure or Axes")


def plugin_figures(df, name, path):
    """Run every @plot function in plots/<table>.py. The file is re-read on every request."""
    figs = []
    ybplots.REGISTRY.clear()
    try:
        src = path.read_text(encoding="utf-8")
        exec(compile(src, str(path), "exec"), {"__name__": "ybplot_file", "__file__": str(path)})
    except Exception:
        tb = traceback.format_exc().strip().splitlines()[-6:]
        return [(f"Error in {path.name}", message_fig(f"Error loading {path.name}", "\n".join(tb)))]
    registered = list(ybplots.REGISTRY)
    if not registered:
        return [("No plots", message_fig(path.name, "No @plot functions found in this file."))]
    for title, fn in registered:
        plt.close("all")
        try:
            fig = to_figure(fn(df))
            fig.suptitle(title, fontsize=12)
            fig.tight_layout()
        except Exception:
            tb = traceback.format_exc().strip().splitlines()[-5:]
            fig = message_fig(title, "Could not draw:\n" + "\n".join(tb))
        figs.append((title, fig))
    return figs


def collect_figures(df, db, table):
    safe = lambda s: bool(re.fullmatch(r"[A-Za-z0-9_\-]+", s))
    for stem in (f"{db}__{table}", table):
        p = PLOTS_DIR / f"{stem}.py"
        if safe(stem) and p.is_file():
            return plugin_figures(df, f"{db}.{table}", p)
    return build_figures(df, f"{db}.{table}")      # generic visuals when no plots file exists


def build_figures(df, name):
    from yellowbrick.features import PCA as YBPCA, Rank2D
    from yellowbrick.target import ClassBalance, FeatureCorrelation

    figs = []
    y = labels = None
    discrete = False
    if TARGET_COL in df.columns:
        y_raw = df[TARGET_COL]
        discrete = (not pd.api.types.is_numeric_dtype(y_raw)) or y_raw.nunique() <= 20
        if discrete:
            codes, uniq = pd.factorize(y_raw)
            y, labels = codes, [str(u) for u in uniq]
        else:
            y = y_raw.to_numpy(dtype=float)

    X = df.drop(columns=[TARGET_COL], errors="ignore").select_dtypes("number")
    X = X.replace([np.inf, -np.inf], np.nan)
    X = X.dropna(axis=1, how="all")
    X = X.fillna(X.median())
    X = X.loc[:, X.var() > 0]
    skipped = df.shape[1] - X.shape[1] - (TARGET_COL in df.columns)

    summary = (f"{len(df)} rows (cap {MAX_ROWS}) x {df.shape[1]} columns\n"
               f"{X.shape[1]} numeric feature column(s) used"
               + (f", {skipped} non-numeric/constant skipped" if skipped else "")
               + (f"\ntarget: '{TARGET_COL}' "
                  f"({'classes: ' + ', '.join(labels[:10]) if discrete else 'continuous'})"
                  if y is not None else "\nno 'target' column - unsupervised visuals only"))
    figs.append(("Summary", message_fig(name, summary)))

    if X.shape[1] < 2:
        figs.append(("No data", message_fig("Not enough numeric columns",
                                            "Yellowbrick visuals need at least 2 numeric feature columns.")))
        return figs

    Xs = X[X.var().nlargest(MAX_FEATURES).index]
    Xv = Xs.to_numpy(dtype=float)

    def make(title, fn):
        fig, ax = plt.subplots(figsize=(8, 6))
        try:
            viz = fn(ax)
            viz.finalize()
            fig.tight_layout()
        except Exception as e:  # keep the report going if one visual fails
            plt.close(fig)
            fig = message_fig(title, f"Could not draw: {e}")
        figs.append((title, fig))

    def rank2d(ax):
        v = Rank2D(algorithm="pearson", features=list(Xs.columns), ax=ax)
        v.fit(Xv, y if discrete else None)
        v.transform(Xv)
        return v

    def pca(ax):
        v = YBPCA(scale=True, classes=labels if discrete else None, ax=ax)
        v.fit(Xv, y if discrete else None)
        v.transform(Xv)
        return v

    def balance(ax):
        v = ClassBalance(labels=labels, ax=ax)
        v.fit(y)
        return v

    def featcorr(ax):
        v = FeatureCorrelation(method="pearson", labels=list(Xs.columns), ax=ax)
        v.fit(Xv, y)
        return v

    make("Feature correlation (Rank2D)", rank2d)
    make("PCA projection", pca)
    if y is not None and discrete:
        make("Class balance", balance)
    if y is not None and not discrete:
        make("Feature correlation with target", featcorr)
    return figs


def render_pdf(figs):
    buf = io.BytesIO()
    with PdfPages(buf) as pdf:
        for _, fig in figs:
            pdf.savefig(fig, bbox_inches="tight")
    return buf.getvalue()


def render_html(figs, name):
    parts = ["<!doctype html><meta charset='utf-8'><meta name='viewport' content='width=device-width,initial-scale=1'>",
             f"<title>{name}</title><style>body{{font-family:system-ui;margin:12px}}"
             "img{max-width:100%;height:auto}figure{margin:0 0 24px}h2{font-size:1rem}</style>"]
    for title, fig in figs:
        b = io.BytesIO()
        fig.savefig(b, format="png", dpi=100, bbox_inches="tight")
        b64 = base64.b64encode(b.getvalue()).decode()
        parts.append(f"<figure><h2>{title}</h2><img alt='{title}' src='data:image/png;base64,{b64}'></figure>")
    return "\n".join(parts)


# ---------- http ----------
@app.get("/report")
def report():
    db = request.args.get("db", "")
    table = request.args.get("table", "")
    fmt = request.args.get("format", "pdf").lower()
    if not db or not table:
        return Response("db and table are required", 400, mimetype="text/plain")
    try:
        df = read_table(db, table)
    except ValueError as e:
        return Response(str(e), 404, mimetype="text/plain")
    except Exception as e:
        return Response(f"database error: {e}", 500, mimetype="text/plain")

    with lock:
        plt.close("all")
        figs = collect_figures(df, db, table)
        try:
            if fmt == "html":
                return Response(render_html(figs, f"{db}.{table}"), mimetype="text/html")
            return Response(render_pdf(figs), mimetype="application/pdf",
                            headers={"Content-Disposition": f'inline; filename="{table}_report.pdf"'})
        finally:
            plt.close("all")


if __name__ == "__main__":
    ap = argparse.ArgumentParser()
    ap.add_argument("--config", default="config.yml")
    ap.add_argument("--port", type=int, default=8001)
    ap.add_argument("--host", default="0.0.0.0")
    a = ap.parse_args()
    c = load_creds(a.config)
    engine = create_engine(URL.create("mysql+pymysql", c["user"], c["password"], c["host"], c["port"]),
                           pool_pre_ping=True)
    app.run(host=a.host, port=a.port, threaded=True)

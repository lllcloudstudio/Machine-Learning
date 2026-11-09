"""KLb-ml report: Pearson correlation, SBP/DBP joint plot, and residual plots.

Called by py_api.py as run(engine, table). Returns an ordered dict:
  DataFrame -> shown as a simple-datatables grid
  Figure    -> shown as an image
Do not call plt.show(); the API renders the figures itself.
"""
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.linear_model import LinearRegression
from sklearn.model_selection import train_test_split
from yellowbrick.features import Rank2D, JointPlotVisualizer
from yellowbrick.regressor import ResidualsPlot

FEATURES = ["bMI (Kg/m2)", "SBP", "Hb", "WBC", "Platelet", "BUN", "Creatinine",
            "egfr (MDRD)", "HCO3", "ph", "HbA1c", "LDL", "Tsat", "Ferritin"]
TARGETS = ["DBP", "TG", "HDL"]
PAIRS = [("SBP", "DBP"), ("LDL", "HDL"), ("WBC", "TG")]   # pairs with r close to 1

# Columns tested for correlation. Kept separate from FEATURES on purpose:
# FEATURES is the model input, and DBP/HDL/TG are the model targets.
CORR_COLS = FEATURES + ["DBP", "HDL", "TG"]
N_PERM = 5000
alpha = 0.05
seed = 42

# Pairs to plot as (x, y) with a least-squares line, R and R-squared
SCATTER_PAIRS = [("SBP", "DBP"), ("LDL", "HDL"), ("Creatinine", "egfr (MDRD)"),
                 ("BUN", "Creatinine"), ("HCO3", "ph"), ("BUN", "egfr (MDRD)"),
                 ("HbA1c", "LDL")]
#####
import pandas as pd
from sqlalchemy import create_engine
engine=create_engine("mysql+pymysql://root:189999@127.0.0.1:3306/REFERENCE")
df=pd.read_sql("""SELECT * FROM kolachalama_data""",engine)

#####

def significant_correlations(df, cols=CORR_COLS, n_perm=N_PERM, alpha=0.05, seed=42):
    """Pairwise Pearson r with permutation p-values and Benjamini-Hochberg adjustment.

    numpy/pandas only (no scipy). Returns (table of pairs with p_adj < alpha, rows used).
    """
    cols = [c for c in cols if c in df.columns]
    num = df[cols].apply(pd.to_numeric, errors="coerce").dropna()   # dropna, not fillna(0)
    n = len(num)
    cols = [c for c in cols if num[c].std() > 0]                    # constant columns have no r
    empty = pd.DataFrame(columns=["a", "b", "r", "p", "p_adj"])
    if n < 10 or len(cols) < 2:
        return empty, n

    z = (num[cols] - num[cols].mean()) / num[cols].std(ddof=0)
    rng = np.random.default_rng(seed)
    rows = []
    for i, a in enumerate(cols):
        xz = z[a].to_numpy()
        for b in cols[i + 1:]:
            yz = z[b].to_numpy()
            r = (xz * yz).mean()
            null = rng.permuted(np.tile(yz, (n_perm, 1)), axis=1) @ xz / n
            p = (1 + (np.abs(null) >= abs(r)).sum()) / (n_perm + 1)
            rows.append((a, b, r, p))
    res = pd.DataFrame(rows, columns=["a", "b", "r", "p"])

    # Benjamini-Hochberg adjustment (FDR)
    m = len(res)
    order = np.argsort(res.p.to_numpy())
    adj = res.p.to_numpy()[order] * m / np.arange(1, m + 1)
    adj = np.minimum.accumulate(adj[::-1])[::-1].clip(max=1)
    res.loc[res.index[order], "p_adj"] = adj

    sig = res[res.p_adj < alpha].sort_values("r", key=abs, ascending=False)
    return sig.round({"r": 3, "p": 4, "p_adj": 4}).reset_index(drop=True), n


def scatter_fit(df, x, y):
    """Scatter plot of y vs x with the least-squares line, R and R-squared.

    Uses every row where both columns are present (not only rows complete in all
    columns). For one predictor, R-squared is exactly R**2. numpy only.
    Returns (figure, stats dict), or (None, None) if the pair can't be fitted.
    """
    d = df[[x, y]].apply(pd.to_numeric, errors="coerce").dropna()
    n = len(d)
    if n < 3 or d[x].std() == 0 or d[y].std() == 0:
        return None, None
    r = float(np.corrcoef(d[x], d[y])[0, 1])
    slope, intercept = np.polyfit(d[x], d[y], 1)

    fig, ax = plt.subplots(figsize=(6, 5))
    ax.scatter(d[x], d[y], alpha=0.7)
    xs = np.linspace(d[x].min(), d[x].max(), 100)
    ax.plot(xs, slope * xs + intercept, color="red")
    ax.set_xlabel(x)
    ax.set_ylabel(y)
    ax.set_title(f"{y} vs {x}")
    # put the label in a corner the fitted line doesn't cross
    px, ha = (0.03, "left") if r >= 0 else (0.97, "right")
    ax.text(px, 0.97, f"R = {r:.3f}\nR² = {r * r:.3f}\nn = {n}",
            transform=ax.transAxes, va="top", ha=ha,
            bbox=dict(boxstyle="round", facecolor="white", alpha=0.85))
    fig.tight_layout()
    return fig, dict(x=x, y=y, n=n, R=round(r, 3), R2=round(r * r, 3),
                     slope=round(float(slope), 4), intercept=round(float(intercept), 4))


def run(engine, table): ### table name not df
    raw = pd.read_sql(f"SELECT * FROM `{table}`", engine)
    out = {}

    # Raw scatter, rows with a missing value are skipped by matplotlib
    if {"SBP", "DBP"} <= set(raw.columns):
        fig, ax = plt.subplots(figsize=(6, 5))
        raw.plot(x="SBP", y="DBP", kind="scatter", ax=ax, title="SBP vs DBP")
        out["SBP vs DBP scatter"] = fig

    # Same cleaning as the original script: numeric columns only, NaN -> 0
    num = raw.fillna(0).select_dtypes("number")

    # Pearson correlation heatmap
    fig, ax = plt.subplots(figsize=(8, 7))
    viz = Rank2D(algorithm="pearson", ax=ax)
    viz.fit_transform(num)
    viz.finalize()
    out["Pearson correlation (plot)"] = fig

    # Same correlations as a table
    out["Pearson correlation (table)"] = (
        num.corr(method="pearson").round(3).reset_index(names="feature"))

    # Pairs whose correlation survives the multiple-testing adjustment
    sig, n_used = significant_correlations(raw)
    out[f"Significant correlations (BH-adjusted p < 0.05, {n_used} complete rows)"] = sig

    # Joint plots for the highly correlated pairs
    for a, b in PAIRS:
        if {a, b} <= set(num.columns):
            fig, ax = plt.subplots(figsize=(6, 5))
            viz = JointPlotVisualizer(columns=[a, b], ax=ax)
            viz.fit_transform(num)
            viz.finalize()
            out[f"Joint plot {a} vs {b}"] = fig

    # Scatter plots with fitted line, R and R-squared for the chosen pairs
    fits = []
    for x, y in SCATTER_PAIRS:
        if {x, y} <= set(raw.columns):
            fig, stats = scatter_fit(raw, x, y)
            if fig is not None:
                out[f"Scatter {y} vs {x} (R = {stats['R']}, R² = {stats['R2']})"] = fig
                fits.append(stats)
    if fits:
        out["Scatter pair fits"] = pd.DataFrame(fits)

    # Residual plots, one per target
    feats = [c for c in FEATURES if c in raw.columns]
    for target in TARGETS:
        if target not in raw.columns:
            continue
        d = raw[feats + [target]].dropna()          # NaNs break LinearRegression
        if len(d) < 10:
            continue
        X_train, X_test, y_train, y_test = train_test_split(
            d[feats], d[target], test_size=0.2, random_state=42)
        model = LinearRegression()
        model._estimator_type = "regressor"          # Yellowbrick type check workaround
        fig, ax = plt.subplots(figsize=(7, 5))
        viz = ResidualsPlot(model, ax=ax, title=f"Residuals: {target}")
        viz.fit(X_train, y_train)
        viz.score(X_test, y_test)
        viz.finalize()
        out[f"Residuals {target}"] = fig

    return out

#####-----------------------------------------------------------------------------------------

##### Scaled:
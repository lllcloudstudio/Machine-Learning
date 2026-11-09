"""Report plots for table kolachalama_data  (ported from KLb-ml.py).

HOW TO ADD A PLOT: add a function with @plot("title") at the bottom. Save the file.
Next click on "Show Report" shows it - no restart. A broken plot shows its error on its own page.
Each function receives df = the whole table (as it comes from MySQL) and returns a Yellowbrick
visualizer, a matplotlib Figure or Axes. Don't call show().
"""
import matplotlib.pyplot as plt
from sklearn.linear_model import LinearRegression
from sklearn.model_selection import train_test_split
from yellowbrick.features import JointPlotVisualizer, Rank2D
from yellowbrick.regressor import ResidualsPlot
from ybplots import plot

# ---- settings you may want to change ----
FEATURES = ["bMI (Kg/m2)", "SBP", "Hb", "WBC", "Platelet", "BUN", "Creatinine",
            "egfr (MDRD)", "HCO3", "ph", "HbA1c", "LDL", "Tsat", "Ferritin"]
RESIDUAL_TARGETS = ["DBP", "TG", "HDL"]       # one residuals plot per target
JOINT_COLUMNS = ["SBP", "DBP"]                # e.g. ["WBC", "TG"] or ["LDL", "HDL"]


def numeric(df):
    """Same cleaning as KLb-ml.py: NaN -> 0, numeric columns only."""
    return df.fillna(0).select_dtypes("number")


#@plot("SBP vs DBP")
#def scatter_sbp_dbp(df):
    #fig, ax = plt.subplots(figsize=(8, 6))
    #df.plot(x="SBP", y="DBP", kind="scatter", ax=ax)
    #return fig


@plot("Feature correlation (Rank2D, pearson)")
def rank2d(df):
    d = numeric(df)
    viz = Rank2D(algorithm="pearson")
    viz.fit_transform(d)
    return viz


#@plot(f"Joint plot: {' vs '.join(JOINT_COLUMNS)}")
#def joint(df):
    #d = numeric(df)
    #viz = JointPlotVisualizer(columns=JOINT_COLUMNS)
    #viz.fit_transform(d)
    #return viz


def residuals(df, target):
    d = numeric(df)
    d = d[FEATURES + [target]].dropna()
    X_train, X_test, y_train, y_test = train_test_split(
        d[FEATURES], d[target], test_size=0.2, random_state=42)
    viz = ResidualsPlot(LinearRegression(), title=f"Residuals: {target}")
    viz.fit(X_train, y_train)
    viz.score(X_test, y_test)
    return viz
################################################################################
import numpy as np
import pandas as pd

TARGETS = ["DBP", "TG", "HDL"]
SCATTER_PAIRS = [("SBP", "DBP"), ("LDL", "HDL"), ("Creatinine", "egfr (MDRD)"),
                 ("BUN", "Creatinine"), ("HCO3", "ph"), ("BUN", "egfr (MDRD)"),
                 ("HbA1c", "LDL")]

def zscore(d):
    return (d - d.mean()) / d.std(ddof=0)


def scatter_fit(df, x, y):
    """Scatter of y vs x with the least-squares line, R and R-squared.
    Returns (figure, stats dict), or (None, None) if the pair can't be fitted."""
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
    px, ha = (0.03, "left") if r >= 0 else (0.97, "right")   # corner the line doesn't cross
    ax.text(px, 0.97, f"R = {r:.3f}\nR² = {r * r:.3f}\nn = {n}\n"
            f"slope = {slope:.3f}\nintercept = {intercept:.3f}",
            transform=ax.transAxes, va="top", ha=ha,
            bbox=dict(boxstyle="round", facecolor="white", alpha=0.85))
    fig.tight_layout()
    return fig, dict(x=x, y=y, n=n, R=round(r, 3), R2=round(r * r, 3),
                     slope=round(float(slope), 4), intercept=round(float(intercept), 4))


def scatter_fit_scaled(df, x, y):
    d = df[[x, y]].apply(pd.to_numeric, errors="coerce").dropna()
    if len(d) < 3 or d[x].std() == 0 or d[y].std() == 0:
        return None, None
    fig, stats = scatter_fit(zscore(d), x, y)
    ax = fig.axes[0]
    ax.set_xlabel(f"{x} (z-score)")
    ax.set_ylabel(f"{y} (z-score)")
    return fig, stats



def scaled_split(df, features, target, test_size=0.2, seed=42):
    """Split rows first, then scale using the TRAIN mean/sd only (no leakage).
    The target stays in its original units so residuals remain readable."""
    d = df[features + [target]].apply(pd.to_numeric, errors="coerce").dropna()
    X_train, X_test, y_train, y_test = train_test_split(
        d[features], d[target], test_size=test_size, random_state=seed)
    mu = X_train.mean()
    sd = X_train.std(ddof=0).replace(0, 1)
    return (X_train - mu) / sd, (X_test - mu) / sd, y_train, y_test


def residuals_scaled(df, target, features=FEATURES):
    features = [c for c in features if c in df.columns and c != target]                                 ### features = [c for c in features if c in df.columns]
    X_train, X_test, y_train, y_test = scaled_split(df, features, target)
    model = LinearRegression()
    model._estimator_type = "regressor"          # Yellowbrick type check workaround
    fig, ax = plt.subplots(figsize=(7, 5))
    viz = ResidualsPlot(model, ax=ax, title=f"Residuals (scaled features): {target}")
    viz.fit(X_train, y_train)
    viz.score(X_test, y_test)
    viz.finalize()
    return fig


def standardized_coefs(df, target, features=FEATURES):
    """Least-squares coefficients with every column z-scored, so they are
    comparable across features (change in target SD per 1 SD of the feature)."""
    features = [c for c in features if c in df.columns and c != target]                                 ### features = [c for c in features if c in df.columns]
    d = df[features + [target]].apply(pd.to_numeric, errors="coerce").dropna()
    z = zscore(d)
    A = np.column_stack([np.ones(len(z)), z[features].to_numpy()])
    beta = np.linalg.lstsq(A, z[target].to_numpy(), rcond=None)[0]
    return pd.Series(beta[1:], index=features, name=target).round(3)


def coefs_plot(df, targets=TARGETS):
    coefs = pd.concat([standardized_coefs(df, t) for t in targets], axis=1)
    fig, axes = plt.subplots(1, len(targets), figsize=(4.5 * len(targets), 6), sharey=True)
    for ax, t in zip(np.atleast_1d(axes), targets):
        c = coefs[t]
        ax.barh(c.index, c.values, color=["tab:blue" if v >= 0 else "tab:orange" for v in c])
        ax.axvline(0, color="black", lw=0.8)
        ax.set_title(f"target: {t}")
        ax.set_xlabel("target SD per feature SD")
    np.atleast_1d(axes)[0].invert_yaxis()
    fig.tight_layout()
    return fig, coefs


def _scatter_page(df, x, y):
    fig, stats = scatter_fit_scaled(df, x, y)
    if fig is None:
        raise ValueError(f"not enough complete rows for {x} vs {y}")
    return fig


def _residual_page(df, y):
    return residuals_scaled(df, y)       # or: features=[f for f in FEATURES if f != x]


for _x, _y in SCATTER_PAIRS:
    plot(f"Scatter (z-scored): {_y} vs {_x}")(lambda df, x=_x, y=_y: _scatter_page(df, x, y))
    plot(f"Residuals (scaled): {_y}")(lambda df, y=_y: _residual_page(df, y))


@plot("Standardized coefficients (features z-scored)")
def coef_page(df):
    return coefs_plot(df)[0]
#for _t in RESIDUAL_TARGETS:
    #plot(f"Residuals: {_t}")(lambda df, t=_t: residuals(df, t))

################################################################################ Claude Additions 10/26/2026
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from ybplots import plot          # already imported at the top of that file

CORR_COLS = ["bMI (Kg/m2)", "SBP", "Hb", "WBC", "Platelet", "BUN", "Creatinine",
             "egfr (MDRD)", "HCO3", "ph", "HbA1c", "LDL", "Tsat", "Ferritin", "DBP", "HDL", "TG"]
N_PERM = 5000


def significant_correlations(df, cols=CORR_COLS, n_perm=N_PERM, alpha=0.05, seed=42):
    rng = np.random.default_rng(seed)                       # fixed seed: same table every click
    num = df[cols].apply(pd.to_numeric, errors="coerce").dropna()
    z = (num - num.mean()) / num.std()
    rows, names = [], list(num.columns)
    for i, a in enumerate(names):
        for b in names[i + 1:]:
            xz, yz = z[a].to_numpy(), z[b].to_numpy()
            r = (xz * yz).mean()
            perms = rng.permuted(np.tile(yz, (n_perm, 1)), axis=1)    # all shuffles at once
            null = (xz * perms).mean(axis=1)
            p = (1 + (np.abs(null) >= abs(r)).sum()) / (n_perm + 1)
            rows.append((a, b, r, p))
    res = pd.DataFrame(rows, columns=["a", "b", "r", "p"])
    m = len(res)                                            # Benjamini-Hochberg
    order = np.argsort(res.p.to_numpy())
    adj = res.p.to_numpy()[order] * m / np.arange(1, m + 1)
    adj = np.minimum.accumulate(adj[::-1])[::-1].clip(max=1)
    res.loc[res.index[order], "p_adj"] = adj
    sig = res[res.p_adj < alpha].sort_values("r", key=abs, ascending=False)
    return sig.round({"r": 3, "p": 4, "p_adj": 4}).reset_index(drop=True), len(num)


def table_fig(tbl, title, note=""):
    fig, ax = plt.subplots(figsize=(7, 0.5 + 0.35 * (len(tbl) + 2)))
    ax.axis("off")
    if tbl.empty:
        ax.text(0.5, 0.5, "No pairs significant at this level", ha="center", va="center")
    else:
        t = ax.table(cellText=tbl.values, colLabels=list(tbl.columns), loc="center", cellLoc="center")
        t.auto_set_font_size(False); t.set_fontsize(9); t.scale(1, 1.3)
    ax.set_title(title)
    if note:
        fig.text(0.5, 0.02, note, ha="center", fontsize=8)
    return fig


@plot("Significant correlations (BH-adjusted p < 0.05)")
def sig_corr_page(df):
    sig, n = significant_correlations(df)
    return table_fig(sig, "Significant correlations (BH-adjusted p < 0.05)",
                     f"Permutation p-values, Benjamini-Hochberg adjusted. {n} complete rows.")

# ---- PCA (3D, colored by Sex) ----
from yellowbrick.features import PCA

PCA_COLOR_BY = "Sex"                 # or "Sex_grp"
PCA_CLASSES = ["Female", "Male"]
PCA_TOP_ARROWS = 4                   # feature arrows drawn on the 3D plot


def _pca_fit(df, color_by=PCA_COLOR_BY):
    X = df[FEATURES].apply(pd.to_numeric, errors="coerce").dropna()
    y = df.loc[X.index, color_by].map({c: i for i, c in enumerate(PCA_CLASSES)})
    viz = PCA(scale=True, projection=3, classes=PCA_CLASSES, alpha=0.8)
    pcs = viz.fit_transform(X, y)
    viz.finalize()                    # finalize first; it resets labels, so ours come after
    return viz, pcs, X


@plot(f"PCA, 3 components, colored by {PCA_COLOR_BY}")
def pca_3d(df):
    viz, pcs, X = _pca_fit(df)
    ax = viz.ax
    ev = viz.pca_transformer.named_steps["pca"].explained_variance_ratio_
    ax.set_xlabel(f"PC1 ({ev[0]:.1%})")
    ax.set_ylabel(f"PC2 ({ev[1]:.1%})")
    ax.set_zlabel(f"PC3 ({ev[2]:.1%})")
    ax.set_title(f"First 3 components explain {ev[:3].sum():.1%}", fontsize=9)

    load = viz.pca_components_.T                                   # features x 3 PCs
    top = np.argsort(np.linalg.norm(load, axis=1))[::-1][:PCA_TOP_ARROWS]
    k = 0.8 * np.abs(pcs).max() / np.abs(load).max()
    for j in top:
        tip = load[j] * k
        ax.quiver(0, 0, 0, *tip, color="red", linewidth=1.5, arrow_length_ratio=0.08)
        ax.text(*(tip * 1.1), FEATURES[j], color="red", fontsize=9)
    return viz.fig                    # return the Figure, not viz: the server would call finalize() again


@plot("PCA loadings (all features)")
def pca_loadings(df):
    viz, pcs, X = _pca_fit(df)
    L = pd.DataFrame(viz.pca_components_.T, index=FEATURES, columns=["PC1", "PC2", "PC3"])
    fig, axes = plt.subplots(1, 3, figsize=(12, 5), sharey=True)
    for a, pc in zip(axes, L.columns):
        s = L[pc].sort_values()
        a.barh(s.index, s.values, color=["tab:blue" if v >= 0 else "tab:orange" for v in s])
        a.axvline(0, color="k", lw=0.8)
        a.set_title(pc)
    return fig

# ---- K-means silhouette tables (k-means_sc.py lines 184-195): summary + per_row ----
from sklearn.cluster import KMeans
from sklearn.metrics import silhouette_samples

KMEANS_KS = range(2, 8)              # K = 2..7
KMEANS_NOTE = ("Average silhouette: above ~0.5 clear structure, 0.25-0.5 weak, below 0.25 not separated. "
               "Negative rows were probably put in the wrong cluster. "
               "With ~50 rows and 14 features expect low scores.")

def zscore(d):                                                                                              ### Can be replaced helpers?
    return (d - d.mean()) / d.std(ddof=0)
    
def kmeans_scores(df, ks=KMEANS_KS):
    X = df[FEATURES].apply(pd.to_numeric, errors="coerce").dropna()
    ids = df.loc[X.index, "id"] if "id" in df.columns else pd.Series(X.index, index=X.index)
    Xs = zscore(X).to_numpy()                       # same as StandardScaler (ddof=0), numpy only
    rows, summary = [], []
    for k in ks:
        labels = KMeans(k, n_init=10, random_state=42).fit_predict(Xs)
        sil = silhouette_samples(Xs, labels)
        rows.append(pd.DataFrame({"id": ids.values, "k": k, "cluster": labels, "silhouette": sil}))
        summary.append({"k": k, "avg_silhouette": sil.mean(),
                        "n_negative": int((sil < 0).sum()),
                        "smallest_cluster": int(np.bincount(labels).min())})
    per_row = pd.concat(rows, ignore_index=True).round(4)
    return per_row, pd.DataFrame(summary).round(4), len(X)


def _km_table(d, note, fontsize=9, row_h=0.32):
    fig, ax = plt.subplots(figsize=(8, min(14.0, 1.8 + row_h * (len(d) + 1))))
    ax.axis("off")
    cells = [[f"{v:.4g}" if isinstance(v, (float, np.floating)) else str(v) for v in row]
             for row in d.itertuples(index=False)]
    t = ax.table(cellText=cells, colLabels=list(d.columns), loc="upper center", cellLoc="center")
    t.auto_set_font_size(False); t.set_fontsize(fontsize); t.scale(1, 1.3 if fontsize >= 9 else 1.0)
    fig.text(0.5, 0.02, note, ha="center", va="bottom", fontsize=8, wrap=True)
    return fig


@plot("K-means silhouette: summary by K")
def kmeans_summary(df):
    per_row, summary, n = kmeans_scores(df)
    return _km_table(summary, f"{n} complete rows, features z-scored. " + KMEANS_NOTE)


# per_row is 300 rows (50 rows x 6 K's): too long for one page, so one page per K
for _k in KMEANS_KS:
    @plot(f"K-means silhouette per row: K={_k}")
    def _kmeans_rows(df, k=_k):
        per_row, summary, n = kmeans_scores(df, ks=[k])
        return _km_table(per_row, "Silhouette per row, features z-scored. Negative = probably the wrong cluster.",
                         fontsize=7, row_h=0.2)


# ---- Classifier precision / recall / F1 (from prec-recall-f1.py) ----
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import StandardScaler
from sklearn.model_selection import train_test_split
from sklearn.metrics import classification_report
from yellowbrick.classifier import ClassificationReport
from sklearn.svm import LinearSVC, NuSVC, SVC
from sklearn.neighbors import KNeighborsClassifier
from sklearn.linear_model import LogisticRegressionCV, LogisticRegression, SGDClassifier

CLASS_COL = "Sex"                    # or "Sex_grp"
CLASSES = ["Female", "Male"]         # Female = 0, Male = 1

MODELS = {                           # name -> function that makes a fresh model
    "SVC": lambda: SVC(gamma="auto"),
    "NuSVC": lambda: NuSVC(gamma="auto"),
    "LinearSVC": lambda: LinearSVC(),
    "SGDClassifier": lambda: SGDClassifier(max_iter=100, tol=1e-3, random_state=42),
    "KNeighborsClassifier": lambda: KNeighborsClassifier(),
    "LogisticRegression": lambda: LogisticRegression(solver="lbfgs"),
    "LogisticRegressionCV": lambda: LogisticRegressionCV(cv=3),
}
try:                                 # blocked by Windows Application Control on some machines
    from sklearn.ensemble import BaggingClassifier, ExtraTreesClassifier, RandomForestClassifier
    MODELS["BaggingClassifier"] = lambda: BaggingClassifier(random_state=42)
    MODELS["ExtraTreesClassifier"] = lambda: ExtraTreesClassifier(n_estimators=300, random_state=42)
    MODELS["RandomForestClassifier"] = lambda: RandomForestClassifier(n_estimators=300, random_state=42)
except ImportError:
    pass


def _clf_split(df, col=None):
    col = col or CLASS_COL
    X = df[FEATURES].apply(pd.to_numeric, errors="coerce").dropna()
    y = df.loc[X.index, col].map({c: i for i, c in enumerate(CLASSES)})
    return train_test_split(X, y, test_size=0.3, stratify=y, random_state=42)


def _clf_pipeline(name):
    model = Pipeline([("scale", StandardScaler()), ("estimator", MODELS[name]())])
    try:
        model._estimator_type = "classifier"     # needed by newer scikit-learn + Yellowbrick 1.5
    except AttributeError:
        pass                                     # older scikit-learn: already set, read-only
    return model

def clf_results(df):
    """Same table as kolachalama_classification: model, class, precision, recall, f1, support."""
    Xtr, Xte, ytr, yte = _clf_split(df)
    parts, errors = [], []
    for name in MODELS:
        try:
            model = _clf_pipeline(name).fit(Xtr, ytr)
            rep = classification_report(yte, model.predict(Xte), target_names=CLASSES,
                                        output_dict=True, zero_division=0)
        except Exception as e:
            errors.append(f"{name}: {type(e).__name__}: {str(e)[:120]}")
            continue
        rows = pd.DataFrame(rep).T.reset_index(names="class")
        rows.insert(0, "model", name)
        parts.append(rows)
    if not parts:
        raise RuntimeError("Every model failed. First error: " + errors[0])
    res = pd.concat(parts, ignore_index=True).round(3).rename(columns={"f1-score": "f1"})
    return res, len(yte)


def _clf_table(d, note, fontsize=7, row_h=0.2):
    fig, ax = plt.subplots(figsize=(8, min(14.0, 1.8 + row_h * (len(d) + 1))))
    ax.axis("off")
    cells = [[f"{v:.4g}" if isinstance(v, (float, np.floating)) else str(v) for v in row]
             for row in d.itertuples(index=False)]
    t = ax.table(cellText=cells, colLabels=list(d.columns), loc="upper center", cellLoc="center")
    t.auto_set_font_size(False); t.set_fontsize(fontsize); t.scale(1, 1.0)
    fig.text(0.5, 0.02, note, ha="center", va="bottom", fontsize=8, wrap=True)
    return fig


@plot("Classifiers: precision / recall / F1 (kolachalama_classification)")
def clf_table(df):
    res, n_test = clf_results(df)
    return _clf_table(res, f"Held-out test set of {n_test} rows (30%), features z-scored. "
                           f"Positive class = {CLASSES[1]}. Small test set: scores are noisy.")


CLASS_COLS = ["Sex", "Sex_grp"]      # one set of ClassificationReport pages per class column
                                     # (the table page above stays on CLASS_COL only)

for _col in CLASS_COLS:
    for _name in MODELS:             # one ClassificationReport page per model
        @plot(f"Classification report ({_col}): {_name}")
        def _clf_page(df, name=_name, col=_col):
            Xtr, Xte, ytr, yte = _clf_split(df, col)
            viz = ClassificationReport(_clf_pipeline(name), classes=CLASSES, cmap="YlGn", support=True)
            viz.fit(Xtr, ytr)
            viz.score(Xte, yte)
            return viz               # the server finalizes it

# ---- add new plots below this line, e.g. ----
# from yellowbrick.features import PCA
# @plot("PCA")
# def pca(df):
#     d = numeric(df)
#     viz = PCA(scale=True)
#     viz.fit_transform(d)
#     return viz

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


@plot("SBP vs DBP")
def scatter_sbp_dbp(df):
    fig, ax = plt.subplots(figsize=(8, 6))
    df.plot(x="SBP", y="DBP", kind="scatter", ax=ax)
    return fig


@plot("Feature correlation (Rank2D, pearson)")
def rank2d(df):
    d = numeric(df)
    viz = Rank2D(algorithm="pearson")
    viz.fit_transform(d)
    return viz


@plot(f"Joint plot: {' vs '.join(JOINT_COLUMNS)}")
def joint(df):
    d = numeric(df)
    viz = JointPlotVisualizer(columns=JOINT_COLUMNS)
    viz.fit_transform(d)
    return viz


def residuals(df, target):
    d = numeric(df)
    d = d[FEATURES + [target]].dropna()
    X_train, X_test, y_train, y_test = train_test_split(
        d[FEATURES], d[target], test_size=0.2, random_state=42)
    viz = ResidualsPlot(LinearRegression(), title=f"Residuals: {target}")
    viz.fit(X_train, y_train)
    viz.score(X_test, y_test)
    return viz


for _t in RESIDUAL_TARGETS:
    plot(f"Residuals: {_t}")(lambda df, t=_t: residuals(df, t))


# ---- add new plots below this line, e.g. ----
# from yellowbrick.features import PCA
# @plot("PCA")
# def pca(df):
#     d = numeric(df)
#     viz = PCA(scale=True)
#     viz.fit_transform(d)
#     return viz

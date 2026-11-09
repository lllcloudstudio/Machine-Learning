##### Scaled:
# z-score by hand (same as StandardScaler, ddof=0). Avoids sklearn.preprocessing,
# which Application Control blocked earlier on your machine.

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.linear_model import LinearRegression
from sklearn.model_selection import train_test_split
from yellowbrick.features import Rank2D, JointPlotVisualizer
from yellowbrick.regressor import ResidualsPlot
from sqlalchemy import create_engine

FEATURES = ["bMI (Kg/m2)", "SBP", "Hb", "WBC", "Platelet", "BUN", "Creatinine",        ### 14 variables
            "egfr (MDRD)", "HCO3", "ph", "HbA1c", "LDL", "Tsat", "Ferritin"]
TARGETS = ["DBP", "TG", "HDL"]                                                         ### Targets of pairs with correlations nearer to 1 excl. HCO3, egfr, ph 
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


engine=create_engine("mysql+pymysql://root:189999@127.0.0.1:3306/REFERENCE")
df=pd.read_sql("""SELECT * FROM kolachalama_data""",engine)

#######################################################################################################
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
#######################################################################################################


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


# Example pair Usage plot of residuls and standardized coefficients: (R/R2)
#fig, stats = scatter_fit_scaled(df, "SBP", "DBP")                                    ### 1 Example pair point plot, scaled, 
#fig = residuals_scaled(df, "DBP")                                                    ### residuals scaled
#coefs = pd.concat([standardized_coefs(df, t) for t in TARGETS], axis=1)              ### of Targets
#plt.show()
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


#for x, y in SCATTER_PAIRS:                                                           ### Like ^^
    #fig, stats = scatter_fit_scaled(df, x, y)                                        # line 120: unchanged
    #res_fig = residuals_scaled(df, y, features=[f for f in FEATURES if f != x])      # line 121: replaced; correction cond. to line 105 and line 90
    #coefs = pd.concat([standardized_coefs(df, t) for t in TARGETS], axis=1)          # line 122: unchanged
    #if fig is not None:
        #print(stats)

#plt.show()

#fig, stats = scatter_fit_scaled(df, "SBP", "DBP")                                   ### 1 Example pair plot scaled, scatter fit incl: R R2 z-score axis
#print(stats)
#plt.show()



#for x, y in SCATTER_PAIRS:
    #fig, stats = scatter_fit(df, x, y)                                              ### scatter fit normal (non-scaled) with R, R2, no z-core axis
    #if fig is not None:
        #print(stats)
        #fig.savefig("sbp_dbp.png", dpi=150)
#plt.show()    # shows all the figures
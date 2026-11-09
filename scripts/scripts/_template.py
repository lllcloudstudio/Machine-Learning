"""Plots for ONE table. Copy this file to  plots/<table_name>.py  (e.g. plots/kolachalama_data.py)
or plots/<database>__<table>.py to limit it to one database.

- Edit and save: the next click on "Show Report" picks up the change. No restart needed.
- Add a plot = add a function with @plot("title"). Order in this file = order in the report.
- Each function gets df (the whole table, up to 20,000 rows) and returns a Yellowbrick
  visualizer, a matplotlib Figure, or Axes. Don't call viz.show().
- If one plot raises an error, the report shows the error on that page and carries on.
"""
from sklearn.linear_model import LogisticRegression
from sklearn.model_selection import train_test_split
from yellowbrick.classifier import ClassificationReport
from yellowbrick.features import Rank2D

from ybplots import plot

# ---- edit these two for your table ----
TARGET = "target"        # name of your label column
FEATURES = None          # None = every numeric column except TARGET, or a list like ["a", "b"]


def xy(df):
    cols = FEATURES or [c for c in df.select_dtypes("number").columns if c != TARGET]
    X = df[cols].fillna(df[cols].median())
    return X, df[TARGET]


@plot("Feature correlation (Rank2D)")
def correlation(df):
    X, y = xy(df)
    viz = Rank2D(algorithm="pearson", features=list(X.columns))
    viz.fit(X, y)
    viz.transform(X)
    return viz


@plot("Classification report")
def class_report(df):
    X, y = xy(df)
    Xtr, Xte, ytr, yte = train_test_split(X, y, test_size=0.25, random_state=0)
    viz = ClassificationReport(LogisticRegression(max_iter=1000), support=True)
    viz.fit(Xtr, ytr)
    viz.score(Xte, yte)
    return viz


# ---- add new plots below, e.g. ----
# @plot("My new plot")
# def my_new_plot(df):
#     ...
#     return viz

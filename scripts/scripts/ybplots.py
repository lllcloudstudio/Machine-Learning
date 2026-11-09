"""Tiny helper so plot files can register plots with a decorator.

In plots/<table>.py:

    from ybplots import plot

    @plot("My title")
    def my_plot(df):                      # df = the whole MySQL table as a DataFrame
        viz = SomeYellowbrickVisualizer()
        viz.fit(...)
        return viz                        # a visualizer, a matplotlib Figure, or Axes
"""
# compat: yellowbrick 1.5 reads _estimator_type, which scikit-learn >= 1.6 dropped.
# Setting it on the mixins fixes every estimator at once; harmless on older versions.
try:
    from sklearn.base import ClassifierMixin, RegressorMixin, ClusterMixin
    ClassifierMixin._estimator_type = "classifier"
    RegressorMixin._estimator_type = "regressor"
    ClusterMixin._estimator_type = "clusterer"
except Exception:
    pass

REGISTRY = []   # filled while a plots file is loaded; cleared by the server each request


def plot(title):
    def deco(fn):
        REGISTRY.append((title, fn))
        return fn
    return deco

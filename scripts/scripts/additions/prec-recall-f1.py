
from sklearn.metrics import f1_score
from sklearn.pipeline import Pipeline
from sklearn.svm import LinearSVC, NuSVC, SVC
from sklearn.neighbors import KNeighborsClassifier
from sklearn.preprocessing import OneHotEncoder, LabelEncoder
from sklearn.linear_model import LogisticRegressionCV, LogisticRegression, SGDClassifier
from sklearn.ensemble import BaggingClassifier, ExtraTreesClassifier, RandomForestClassifier
import pandas as pd
from sqlalchemy import create_engine
from yellowbrick.features import PCA # as PCA?
from sklearn.preprocessing import StandardScaler, LabelEncoder
from sklearn.model_selection import train_test_split
from yellowbrick.classifier import ClassificationReport
from sklearn.metrics import classification_report
from sklearn.model_selection import StratifiedKFold
from sklearn.model_selection import StratifiedKFold, cross_val_predict

engine = create_engine("mysql+pymysql://root:189999@127.0.0.1:3306/reference")
df = pd.read_sql("select * from kolachalama_data", engine)

FEATURES = ["bMI (Kg/m2)", "SBP", "Hb", "WBC", "Platelet", "BUN", "Creatinine",                             ### 14 Variables
            "egfr (MDRD)", "HCO3", "ph", "HbA1c", "LDL", "Tsat", "Ferritin"]

X = df[FEATURES].apply(pd.to_numeric, errors="coerce").dropna()   # numeric only, no NaN
y = df.loc[X.index, "Sex"]                                        # same rows as X # Indicate selection

##########################################
#X,y=load_data() 


X = df[FEATURES].apply(pd.to_numeric, errors="coerce").dropna()
y = LabelEncoder().fit_transform(df.loc[X.index, "Sex"])      # Female=0, Male=1 their label encoder 

models = [
    SVC(gamma="auto"), NuSVC(gamma="auto"), LinearSVC(),
    SGDClassifier(max_iter=100, tol=1e-3), KNeighborsClassifier(),
    LogisticRegression(solver="lbfgs"), LogisticRegressionCV(cv=3),
    BaggingClassifier(), ExtraTreesClassifier(n_estimators=300),
    RandomForestClassifier(n_estimators=300),
]

try:
    from sklearn.ensemble import BaggingClassifier, ExtraTreesClassifier, RandomForestClassifier
    models += [BaggingClassifier(), ExtraTreesClassifier(n_estimators=300),
               RandomForestClassifier(n_estimators=300)]
except ImportError as e:
    print("ensemble models skipped (blocked by Windows):", str(e)[:80])

cv = StratifiedKFold(5, shuffle=True, random_state=42)

def score_model(X, y, estimator):
    model = Pipeline([("scale", StandardScaler()), ("estimator", estimator)])
    predicted = cross_val_predict(model, X, y, cv=cv)          # each row predicted by a model that never saw it
    print(f"{estimator.__class__.__name__}: {f1_score(y, predicted):.3f}")

for m in models:
    score_model(X, y, m)

#########################################

def visualize_model(X, y, estimator, **kwargs):
    Xtr, Xte, ytr, yte = train_test_split(
        X, y, test_size=0.3, stratify=y, random_state=42)

    model = Pipeline([
        ("scale", StandardScaler()),           # not OneHotEncoder: your features are numeric
        ("estimator", estimator),
    ])
    model._estimator_type = "classifier"       # needed: Yellowbrick 1.5 + scikit-learn 1.9.1

    visualizer = ClassificationReport(
        model, classes=["Female", "Male"],
        cmap="YlGn", size=(600, 360), **kwargs
    )
    visualizer.fit(Xtr, ytr)
    visualizer.score(Xte, yte)                 # score on rows the model never saw
    visualizer.show()

y = LabelEncoder().fit_transform(df.loc[X.index, "Sex"])   # Female=0, Male=1
for m in models:
    visualize_model(X, y, m)
#######################################################

def visualize_model(X, y, estimator, show=True, **kwargs):
    Xtr, Xte, ytr, yte = train_test_split(X, y, test_size=0.3, stratify=y, random_state=42)
    model = Pipeline([("scale", StandardScaler()), ("estimator", estimator)])
    model._estimator_type = "classifier"

    viz = ClassificationReport(model, classes=["Female", "Male"],
                               cmap="YlGn", size=(600, 360), **kwargs)
    viz.fit(Xtr, ytr)
    viz.score(Xte, yte)
    if show:
        viz.show()

    rep = classification_report(yte, viz.predict(Xte), target_names=["Female", "Male"],
                                output_dict=True, zero_division=0)
    rows = pd.DataFrame(rep).T.reset_index(names="class")       # one row per class + averages
    rows.insert(0, "model", estimator.__class__.__name__)
    return rows

results = pd.concat([visualize_model(X, y, m, show=False) for m in models], ignore_index=True)
results = results.round(3).rename(columns={"f1-score": "f1"})





                                         ### file
#results.to_sql("kolachalama_classification", engine, if_exists="replace", index=False)  # database  ### complete
###########################################################

X = df[FEATURES].apply(pd.to_numeric, errors="coerce").dropna()   # numeric only, no NaN
y = df.loc[X.index, "Sex_grp"]                                        # same rows as X # Indicate selection

##########################################
#X,y=load_data() 


X = df[FEATURES].apply(pd.to_numeric, errors="coerce").dropna()
y = LabelEncoder().fit_transform(df.loc[X.index, "Sex_grp"])      # Female=0, Male=1 their label encoder 

models = [
    SVC(gamma="auto"), NuSVC(gamma="auto"), LinearSVC(),
    SGDClassifier(max_iter=100, tol=1e-3), KNeighborsClassifier(),
    LogisticRegression(solver="lbfgs"), LogisticRegressionCV(cv=3),
    BaggingClassifier(), ExtraTreesClassifier(n_estimators=300),
    RandomForestClassifier(n_estimators=300),
]

try:
    from sklearn.ensemble import BaggingClassifier, ExtraTreesClassifier, RandomForestClassifier
    models += [BaggingClassifier(), ExtraTreesClassifier(n_estimators=300),
               RandomForestClassifier(n_estimators=300)]
except ImportError as e:
    print("ensemble models skipped (blocked by Windows):", str(e)[:80])

cv = StratifiedKFold(5, shuffle=True, random_state=42)

def score_model(X, y, estimator):
    model = Pipeline([("scale", StandardScaler()), ("estimator", estimator)])
    predicted = cross_val_predict(model, X, y, cv=cv)          # each row predicted by a model that never saw it
    print(f"{estimator.__class__.__name__}: {f1_score(y, predicted):.3f}")

for m in models:
    score_model(X, y, m)

#########################################

def visualize_model(X, y, estimator, **kwargs):
    Xtr, Xte, ytr, yte = train_test_split(
        X, y, test_size=0.3, stratify=y, random_state=42)

    model = Pipeline([
        ("scale", StandardScaler()),           # not OneHotEncoder: your features are numeric
        ("estimator", estimator),
    ])
    model._estimator_type = "classifier"       # needed: Yellowbrick 1.5 + scikit-learn 1.9.1

    visualizer = ClassificationReport(
        model, classes=["Female", "Male"],
        cmap="YlGn", size=(600, 360), **kwargs
    )
    visualizer.fit(Xtr, ytr)
    visualizer.score(Xte, yte)                 # score on rows the model never saw
    visualizer.show()

y = LabelEncoder().fit_transform(df.loc[X.index, "Sex_grp"])   # Female=0, Male=1
for m in models:
    visualize_model(X, y, m)
#######################################################

def visualize_model(X, y, estimator, show=True, **kwargs):
    Xtr, Xte, ytr, yte = train_test_split(X, y, test_size=0.3, stratify=y, random_state=42)
    model = Pipeline([("scale", StandardScaler()), ("estimator", estimator)])
    model._estimator_type = "classifier"

    viz = ClassificationReport(model, classes=["Female", "Male"],
                               cmap="YlGn", size=(600, 360), **kwargs)
    viz.fit(Xtr, ytr)
    viz.score(Xte, yte)
    if show:
        viz.show()

    rep = classification_report(yte, viz.predict(Xte), target_names=["Female", "Male"],
                                output_dict=True, zero_division=0)
    rows = pd.DataFrame(rep).T.reset_index(names="class")       # one row per class + averages
    rows.insert(0, "model", estimator.__class__.__name__)
    return rows

results = pd.concat([visualize_model(X, y, m, show=False) for m in models], ignore_index=True)
results = results.round(3).rename(columns={"f1-score": "f1"})





                                         ### file
#results.to_sql("kolachalama_classification", engine, if_exists="replace", index=False)  # database  ### complete



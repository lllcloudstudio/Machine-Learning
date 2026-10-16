import pandas as pd
from sqlalchemy import create_engine
import matplotlib.pyplot as plt
from yellowbrick.features import Rank2D
from sklearn.preprocessing import StandardScaler
engine = create_engine("mysql+pymysql://root:189999@127.0.0.1:3306/REFERENCE")
df=pd.read_sql(
    """
    SELECT * FROM kolachalama_data;
    """,engine
)

### Example plot to visualize the relationship between SBP and DBP
df.plot(x='SBP', y='DBP', kind='scatter')
plt.show()

df=df.fillna(0)
df=df.select_dtypes("number")
print(df.head())
visualizer=Rank2D(algorithm="pearson")
visualizer.fit_transform(df)
visualizer.show()

### Example pair R ~1
from yellowbrick.features import JointPlotVisualizer 
visualizer=JointPlotVisualizer(columns=["SBP","DBP"]) #visualizer=JointPlotVisualizer(columns=["WBC","TG"])#visualizer=JointPlotVisualizer(columns=["LDL","HDL"])
visualizer.fit_transform(df)
visualizer.show()

### Example Models Excl 3 VAR 
###   py -3.12 -m venv yb_env
###  yb_env\Scripts\activate
###  python -m pip install yellowbrick "scikit-learn<1.6" pandas matplotlib sqlalchemy pymysql
### No Yellowbrick needed: the residuals plot is easy to draw directly, and it avoids the version problem: python
#import matplotlib.pyplot as plt
#from sklearn.linear_model import LinearRegression

#model = LinearRegression().fit(X_train, y_train)

#for name, X_, y_ in [("train", X_train, y_train), ("test", X_test, y_test)]:
    #pred = model.predict(X_)
    #plt.scatter(pred, y_ - pred, alpha=0.6, label=f"{name} R2={model.score(X_, y_):.2f}")

#plt.axhline(0, color="k")
#plt.xlabel("Predicted")
#plt.ylabel("Residual")
#plt.title(f"Residuals: {target}")
#plt.legend()
#plt.show()

from yellowbrick.regressor import ResidualsPlot
from sklearn.linear_model import LinearRegression
from sklearn.model_selection import train_test_split

features = ["bMI (Kg/m2)","SBP","Hb","WBC","Platelet","BUN","Creatinine",
            "egfr (MDRD)","HCO3","ph","HbA1c","LDL","Tsat","Ferritin"]

for target in ["DBP", "TG", "HDL"]:
    d = df[features + [target]].dropna()          # NaNs break LinearRegression
    X_train, X_test, y_train, y_test = train_test_split(
        d[features], d[target], test_size=0.2, random_state=42
    )
    model = LinearRegression() # addition
    model._estimator_type = "regressor" # addition
    viz = ResidualsPlot(model, title=f"Residuals: {target}") # similar to line below it, 46
    #viz = ResidualsPlot(LinearRegression(), title=f"Residuals: {target}")
    viz.fit(X_train, y_train)
    viz.score(X_test, y_test)
    viz.show()






#### !
#visualizer = ResidualsPlot(LinearRegression())
#visualizer.fit(X_train, y_train)
#visualizer.score(X_test, y_test)
#visualizer.show()
#visualizer.score(X_test, y_test)


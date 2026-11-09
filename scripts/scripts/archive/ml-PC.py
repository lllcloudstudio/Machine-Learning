import pandas as pd
import matplotlib.pyplot as plt
from yellowbrick.features import Rank2D
from sklearn.preprocessing import StandardScaler

df=pd.read_csv(r"C:\Users\aflac\Documents\GitHub\Machine-Learning\kolachalama_data.csv")
X=df.select_dtypes("number")
X=X.fillna(0)
print(X.head())
visualizer=Rank2D(algorithm="pearson")
visualizer.fit_transform(X)
visualizer.show()
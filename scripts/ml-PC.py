"""Kolachalama lab synth. data read from mysql reference to visual.

Prereqs:
  pip install pandas sqlalchemy pymysql yellowbrick
  python -m yellowbrick.download        (already done)

Run:
  python ml-PC.py
"""
import pandas as pd
import matplotlib.pyplot as plt
from yellowbrick.features import Rank2D
from sklearn.preprocessing import StandardScaler

#df=pd.read_csv(r"C:\Users\aflac\Documents\GitHub\Machine-Learning\kolachalama_data.csv")
#X=X.fillna(0)
#X=df.select_dtypes("number")
#print(X.head())
#visualizer=Rank2D(algorithm="pearson")
#visualizer.fit_transform(X)
#visualizer.show()


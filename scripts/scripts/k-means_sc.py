
##### 
### Scaled
### 
from sklearn.cluster import KMeans
from yellowbrick.cluster import SilhouetteVisualizer
from yellowbrick.datasets import load_nfl
import numpy as np
import pandas as pd
from sqlalchemy import create_engine
import matplotlib.pyplot as plt
from sklearn.metrics import silhouette_samples, silhouette_score
from sklearn.preprocessing import StandardScaler

#### Added claude's version to kolachalama_data.py line 64-117
#engine=create_engine("mysql+pymysql://root:189999@127.0.0.1:3306/reference")
#df=pd.read_sql("""select * from kolachalama_data""", engine)                                     ### full table 17 features, 50 rows
# Specify the features to use for clustering
#FEATURES = ["bMI (Kg/m2)", "SBP", "Hb", "WBC", "Platelet", "BUN", "Creatinine",
            #"egfr (MDRD)", "HCO3", "ph", "HbA1c", "LDL", "Tsat", "Ferritin","DBP","HDL","TG"]
#num = df[FEATURES].apply(pd.to_numeric, errors="coerce").dropna()   # use dropna, not fillna(0)  ### select numeric values, drop NA
#rng = np.random.default_rng(42)                                                                  ### kernel
#n_perm = 5000                                                                                    ### Can be increased to 50,000 or 100,000 for more precision, but takes longer

#def perm_p(x, y):
    #xz = (x - x.mean()) / x.std()
    #yz = (y - y.mean()) / y.std()
    #r = (xz * yz).mean()
    #null = np.array([(xz * rng.permutation(yz)).mean() for _ in range(n_perm)])
    #return r, (1 + (np.abs(null) >= abs(r)).sum()) / (n_perm + 1)

#cols = list(num.columns)
#rows = [(a, b, *perm_p(num[a].to_numpy(), num[b].to_numpy()))
        #for i, a in enumerate(cols) for b in cols[i+1:]]
#res = pd.DataFrame(rows, columns=["a", "b", "r", "p"])                                          ### 3 stats incl. p-value

# Benjamini-Hochberg adjustment (FDR)
#m = len(res)
#order = np.argsort(res.p.to_numpy())
#adj = res.p.to_numpy()[order] * m / np.arange(1, m + 1)                                         ### +1 p-value adj. addition to table
#adj = np.minimum.accumulate(adj[::-1])[::-1].clip(max=1)
#res.loc[res.index[order], "p_adj"] = adj

#print(res[res.p_adj < 0.05].sort_values("r", key=abs, ascending=False)) ##### OK-- Creatinine and egfr are highly correlated, as expected.  HCO3 and ph (0.009 p_adj) 

#              a            b         r       p     p_adj
#28          SBP          DBP  0.832696  0.0002  0.005439 ***
#124         LDL          HDL  0.768740  0.0002  0.005439 ***
#81   Creatinine  egfr (MDRD) -0.732133  0.0002  0.005439 ***
#70          BUN   Creatinine  0.566662  0.0002  0.005439
#100        HCO3           ph  0.558314  0.0004  0.009065
#71          BUN  egfr (MDRD) -0.550273  0.0002  0.005439
#115       HbA1c          LDL  0.447383  0.0022  0.042734
# Specify the features to use for clustering
FEATURES = ["bMI (Kg/m2)", "SBP", "Hb", "WBC", "Platelet", "BUN", "Creatinine",                ### 14 Variables, Kmeans = 3
            "egfr (MDRD)", "HCO3", "ph", "HbA1c", "LDL", "Tsat", "Ferritin"]
df = df[FEATURES].apply(pd.to_numeric, errors="coerce").dropna()                               

# Instantiate the clustering model and visualizer                                              
#model = KMeans(3, random_state=42)                                                             ### n_init=10, default not included
#visualizer = SilhouetteVisualizer(model, colors='yellowbrick')                                 ### yellowbrick.exceptions.YellowbrickTypeError: The supplied model is not a clustering estimator; try a classifier or regression score visualizer instead!

#visualizer.fit(df)        # Fit the data to the visualizer
#visualizer.show()        # Finalize and render the figure
####################################################################################################



### K=2
engine = create_engine("mysql+pymysql://root:189999@127.0.0.1:3306/reference")
df = pd.read_sql("select * from kolachalama_data", engine)

FEATURES = ["bMI (Kg/m2)", "SBP", "Hb", "WBC", "Platelet", "BUN", "Creatinine", ### 14 Variables
            "egfr (MDRD)", "HCO3", "ph", "HbA1c", "LDL", "Tsat", "Ferritin"]
X = df[FEATURES].apply(pd.to_numeric, errors="coerce").dropna()                 ### Select numeric and drop NA: KMeans fails on NaN
X_scaled = StandardScaler().fit_transform(X)                                    ### Scaling: (required) equal weight per feature i.e. yb weighted precision recall f1 score nums.

model = KMeans(2, n_init=10, random_state=42)                                   ### Kmeans standard: K=2, n_init=10, random_state=42
model._estimator_type = "clusterer"        # fixes the Yellowbrick type check
visualizer = SilhouetteVisualizer(model, colors="yellowbrick")

visualizer.fit(X_scaled)
visualizer.show()

### K=3

#engine = create_engine("mysql+pymysql://root:189999@127.0.0.1:3306/reference")
#df = pd.read_sql("select * from kolachalama_data", engine)

#FEATURES = ["bMI (Kg/m2)", "SBP", "Hb", "WBC", "Platelet", "BUN", "Creatinine",
#            "egfr (MDRD)", "HCO3", "ph", "HbA1c", "LDL", "Tsat", "Ferritin"]
#X = df[FEATURES].apply(pd.to_numeric, errors="coerce").dropna()   # KMeans fails on NaN
#X_scaled = StandardScaler().fit_transform(X)                       # equal weight per feature

model = KMeans(3, n_init=10, random_state=42)
model._estimator_type = "clusterer"        # fixes the Yellowbrick type check
visualizer = SilhouetteVisualizer(model, colors="yellowbrick")

visualizer.fit(X_scaled)
visualizer.show()

### numpy k=3
### numpy !!! version of silhouette score and samples, with a plot of the distribution of scores per cluster.  This is what Yellowbrick does under the hood.
### Omit from yellowbrick report for now
#k = 3                                                                                           ### I don't see their scaling, but it is required for silhouette score and samples.  I think Yellowbrick does it under the hood.
#labels = KMeans(n_clusters=k, n_init=10, random_state=42).fit_predict(X)                        ### 14 variables Kmeans = 3
#sil = silhouette_samples(X, labels)
#avg = silhouette_score(X, labels)

#fig, ax = plt.subplots(figsize=(7, 5))
#y_lower = 10
#for i in range(k):
    #vals = np.sort(sil[labels == i])
    #y_upper = y_lower + len(vals)
    #ax.fill_betweenx(np.arange(y_lower, y_upper), 0, vals, alpha=0.7, label=f"cluster {i}")
    #y_lower = y_upper + 10
#ax.axvline(avg, color="red", linestyle="--", label=f"mean = {avg:.2f}")
#ax.set_xlabel("Silhouette coefficient")
#ax.set_ylabel("Samples, grouped by cluster")
#ax.legend()
#plt.show()

### K =4 


#engine = create_engine("mysql+pymysql://root:189999@127.0.0.1:3306/reference")
#df = pd.read_sql("select * from kolachalama_data", engine)

#FEATURES = ["bMI (Kg/m2)", "SBP", "Hb", "WBC", "Platelet", "BUN", "Creatinine",
#            "egfr (MDRD)", "HCO3", "ph", "HbA1c", "LDL", "Tsat", "Ferritin"]
#X = df[FEATURES].apply(pd.to_numeric, errors="coerce").dropna()   # KMeans fails on NaN
#X_scaled = StandardScaler().fit_transform(X)                       # equal weight per feature

model = KMeans(4, n_init=10, random_state=42)
model._estimator_type = "clusterer"        # fixes the Yellowbrick type check
visualizer = SilhouetteVisualizer(model, colors="yellowbrick")

visualizer.fit(X_scaled)
visualizer.show()

####################################################################################################

#visualizer.fit(X_scaled)
#print(visualizer.silhouette_score_)       # average silhouette score (one number, -1 to 1)
#print(visualizer.silhouette_samples_)     # one score per row (array)
#print(visualizer.labels_)                 # cluster assigned to each row
#print(visualizer.n_clusters_)             # number of clusters (3)

###To see them (pandas silhouette coeff.) per cluster and per row, put them in a DataFrame:

#scores = pd.DataFrame({
    #"cluster": visualizer.labels_,
    #"silhouette": visualizer.silhouette_samples_,
#}, index=X.index)           # X is the dropna'd frame, so the index matches the original rows

#print(scores.groupby("cluster")["silhouette"].agg(["count", "mean", "min"]))
#print(scores.sort_values("silhouette").head(10))    # worst-fitting rows

###Rows with a negative score were probably assigned to the wrong cluster.
###To join them back to your original data, including the features:

#result = df.loc[X.index].assign(cluster=visualizer.labels_,
#                                silhouette=visualizer.silhouette_samples_)

###If you used the plain scikit-learn version instead of Yellowbrick, the same numbers come from:

#avg = silhouette_score(X_scaled, labels)
#per_row = silhouette_samples(X_scaled, labels)

###To compare different numbers of clusters:

#for k in range(2, 7):
    #labels = KMeans(k, n_init=10, random_state=42).fit_predict(X_scaled)
    #print(k, round(silhouette_score(X_scaled, labels), 3))

###The average score is read roughly like this: above about 0.5 is a clear structure, 0.25 to 0.5 is weak, and below 0.25 means the clusters are not well separated. With about 50 rows and 14 features, expect low scores, and check that the clusters aren’t just one or two outlier rows.

#################################################################################################### tables to SQL of silhouette scores per row and summary of average score, number of negative scores, and size of smallest cluster:

X = df[FEATURES].apply(pd.to_numeric, errors="coerce").dropna()
ids = df.loc[X.index, "id"]
Xs = StandardScaler().fit_transform(X)            # scaled once, reused for every K

rows, summary = [], []
for k in range(2, 8):                             # K = 2..7
    labels = KMeans(k, n_init=10, random_state=42).fit_predict(Xs)
    sil = silhouette_samples(Xs, labels)
    rows.append(pd.DataFrame({"id": ids.values, "k": k,
                              "cluster": labels, "silhouette": sil}))
    summary.append({"k": k, "avg_silhouette": sil.mean(),
                    "n_negative": int((sil < 0).sum()),
                    "smallest_cluster": int(np.bincount(labels).min())})

per_row = pd.concat(rows, ignore_index=True).round(4)    # 50 rows x 6 K's = 300 rows
summary = pd.DataFrame(summary).round(4)


#per_row.to_sql("kolachalama_kmeans_silhouette", engine, if_exists="replace", index=False)               ### complete 
#summary.to_sql("kolachalama_kmeans_summary",    engine, if_exists="replace", index=False)               ### complete 

#print(per_row[per_row.k == 3].groupby("cluster")["silhouette"].agg(["count", "mean", "min"]))            ### example not including in report display
#print(per_row[per_row.k == 3].sort_values("silhouette").head(10))
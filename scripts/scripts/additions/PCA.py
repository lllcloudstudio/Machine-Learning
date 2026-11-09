import pandas as pd
from sqlalchemy import create_engine
from yellowbrick.features import PCA # as PCA?

engine = create_engine("mysql+pymysql://root:189999@127.0.0.1:3306/reference")
df = pd.read_sql("select * from kolachalama_data", engine)

FEATURES = ["bMI (Kg/m2)", "SBP", "Hb", "WBC", "Platelet", "BUN", "Creatinine",                             ### 14 features 
            "egfr (MDRD)", "HCO3", "ph", "HbA1c", "LDL", "Tsat", "Ferritin"]

X = df[FEATURES].apply(pd.to_numeric, errors="coerce").dropna()   # numeric only, no NaN
y = df.loc[X.index, "Sex"]                                        # same rows as X # Indicate selection

classes = ["Female", "Male"]                                      # 
y = y.map({c: i for i, c in enumerate(classes)})                  # text -> 0/1

visualizer = PCA(scale=True, projection=3, classes=classes) # projections !!! (3 components, sklearn to incl. all)
visualizer.fit_transform(X, y)
visualizer.show()


#############################################################################################
X = df[FEATURES].apply(pd.to_numeric, errors="coerce").dropna()   # numeric only, no NaN
y = df.loc[X.index, "Sex"]                                        # same rows as X

classes = ["Female", "Male"]
y = y.map({c: i for i, c in enumerate(classes)})                  # text -> 0/1

visualizer = PCA(scale=True, projection=3, classes=classes)        # First 3 components !!!
visualizer.fit_transform(X, y)
visualizer.show()
#############################################################################################



import numpy as np
##### No y
from sklearn.decomposition import PCA as SkPCA
from sklearn.preprocessing import StandardScaler

Xs = StandardScaler().fit_transform(X)       # not PCA(scale=True ...)
pca = SkPCA()                                # keeps all components
pcs = pca.fit_transform(Xs)                  # shape (rows, 14)
### visualizer.show()
### ...
cols = [f"PC{i+1}" for i in range(pcs.shape[1])]
out = pd.DataFrame(pcs, columns=cols, index=X.index)
out.insert(0, "id", df.loc[X.index, "id"])
out.to_sql("kolachalama_pca_all", engine, if_exists="replace", index=False)                     ### complete

pd.DataFrame({"PC": cols, "explained_variance_ratio": pca.explained_variance_ratio_}            ### complete
            ).to_sql("kolachalama_pca_all_variance", engine, if_exists="replace", index=False)


#############################################################################################

# Yellowbrick (3 components)
pca = visualizer.pca_transformer.named_steps["pca"]

# or scikit-learn (all components): pca = SkPCA().fit(Xs)

pcs = [f"PC{i+1}" for i in range(len(pca.explained_variance_))]

summary = pd.DataFrame({
    "PC": pcs,
    "std_dev": np.sqrt(pca.explained_variance_),
    "prop_variance": pca.explained_variance_ratio_,
    "cum_proportion": pca.explained_variance_ratio_.cumsum(),
})
                                                                            ### complete
#summary.to_csv("pca_summary.csv", index=False)                           # file
#summary.to_sql("kolachalama_pca_summary", engine, if_exists="replace",   # database
               index=False)


### Additions 10/26/2026 Adding with wrapper to /plots/kolachalama_data.py
import pandas as pd
from sqlalchemy import create_engine
from yellowbrick.features import PCA # as PCA?

engine = create_engine("mysql+pymysql://root:189999@127.0.0.1:3306/reference")
df = pd.read_sql("select * from kolachalama_data", engine)

FEATURES = ["bMI (Kg/m2)", "SBP", "Hb", "WBC", "Platelet", "BUN", "Creatinine",                             ### 14 features 
            "egfr (MDRD)", "HCO3", "ph", "HbA1c", "LDL", "Tsat", "Ferritin"]

X = df[FEATURES].apply(pd.to_numeric, errors="coerce").dropna()                                             ### numeric only, no NaN
y = df.loc[X.index, "Sex"]                                                                                  ### same rows as X # Indicate selection or Sex_grp 

classes = ["Female", "Male"]                                                                                ### 
y = y.map({c: i for i, c in enumerate(classes)})                                                            ### text -> 0/1

visualizer = PCA(scale=True, projection=3, classes=classes) # projections !!!                               ### No alpha (3 components, sklearn to incl. all)
visualizer.fit_transform(X, y)
visualizer.show()




visualizer = PCA(scale=True, projection=3, classes=classes, alpha=0.8)   # no proj_features
pcs = visualizer.fit_transform(X, y)
visualizer.finalize()
ax = visualizer.ax

ev = visualizer.pca_transformer.named_steps["pca"].explained_variance_ratio_
ax.set_xlabel(f"PC1 ({ev[0]:.1%})"); ax.set_ylabel(f"PC2 ({ev[1]:.1%})"); ax.set_zlabel(f"PC3 ({ev[2]:.1%})")
ax.set_title(f"PCA, first 3 components explain {ev[:3].sum():.1%}")

TOP = 4                                                  # how many arrows to draw
load = visualizer.pca_components_.T                      # 14 features x 3 PCs
top = np.argsort(np.linalg.norm(load, axis=1))[::-1][:TOP]   # strongest overall pull on PC1-3
k = 0.8 * np.abs(pcs).max() / np.abs(load).max()         # stretch arrows to the size of the points
for j in top:
    tip = load[j] * k
    ax.quiver(0, 0, 0, *tip, color="red", linewidth=1.5, arrow_length_ratio=0.08)
    ax.text(*(tip * 1.1), FEATURES[j], color="red", fontsize=9)
plt.show()


L = pd.DataFrame(load, index=FEATURES, columns=["PC1", "PC2", "PC3"])
fig, axes = plt.subplots(1, 3, figsize=(12, 5), sharey=True)
for a, pc in zip(axes, L.columns):
    s = L[pc].sort_values()
    a.barh(s.index, s.values, color=["tab:blue" if v >= 0 else "tab:orange" for v in s])
    a.axvline(0, color="k", lw=0.8)
    a.set_title(pc)
fig.tight_layout()
plt.show()
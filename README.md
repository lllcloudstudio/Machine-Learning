# ML

A review of 2017 health data from Dr. Vijay Kolachalama. Data is either synthetic or not.

50 patient blood sample(s) 
13 diagnostic variables, as numerics and without annotations 
3 variables are Age, Gender, or Race and are numerical
The sample data is considered a small sample size, less than 300 total samples (<300*)

Variables: ph, LDL, HDL, or hb are diagnostics, I consider an i) extrapolation to modelling (towards selection) or ii) indicator(s) of health status.

The two group (Male or Female) health bias is delved with machine learning (ML) with either k-means, support vector machine (svm), or hierarchical clustering (hc) for: model selection (dimension reduction) by features or variables.

Review:
1) Summary comparison of 2 models (using the R glm() function). As response is either `category` or `Gender`

2) PCA (R prcomp()) function is
centered and scaled
`principal component score vectors have length n=50 and 
the principal component loading vectors have length p=16`

3) biplot displays 2 components or "loading vectors" listed at the table with colnames PC1, or PC2, etc.
Summary per component: Standard deviation, Proportion of Variance, or Cumulative Proportion
multiplot line or bar graph of vectors

4) kmeans semi-step wise evaluation to k=6, and 1 additional parameter
See bias variance .xlsx

6) Model evaluation: training and test set selection (pairing 10-40 or pairing 15-35)
   LOOCV* or k-fold cross validation* 

Inspection of data by category is a preliminary procedure to model(s) production (reducing the number of models), evaluation, or selection.


#Small Datasets: If you have very few instances (e.g., < 300), 20% may be too small to reliably test on. In this scenario, k-fold cross-validation is preferred.
#Use a sample size calculator and argue that you need 74.24, 25.76 split due to wanting a 90%/95% confidence etc.

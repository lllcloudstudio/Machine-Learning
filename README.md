# ML

A review or evaluation of 2017 health data from Dr. Vijay Kolachalama. Data's either synthetic or not.

50 patient blood sample 
13 diagnostic variables, as numerics and without annotations 
Age, Gender, or Race variables are numerical

Variables like ph or hb, I consider an extrapolation to modelling as diagnostics (health status)
and two group (Male of Female) health bias.

Review:
1) Summary comparison of 2 models (glm() function). As response either `category` or `Gender`

2) PCA prcomp() function 
centered and scaled
`principal component score vectors have length n=50 and 
the principal component loading vectors have length p=16`

3) biplot displays 2 components or "loading vectors" listed at table with colnames PC1, or PC2, etc.
Summary per component: Standard deviation, Proportion of Variance, or Cumulative Proportion
multiplot line or bar graph of vectors

4) model evaluation: training and test set selection consideration (10-40 ; 15-35)
5) or
6) k-fold cross validation cv.glm() compared to glm()


#Small Datasets: If you have very few instances (e.g., < 300), 20% may be too small to reliably test on. In this scenario, k-fold cross-validation is preferred.

#Use a sample size calculator and argue that you need 74.24, 25.76 split due to wanting a 90%/95% confidence etc.

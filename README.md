# ML

A review or evaluation of 2017 health data from Dr. Vijay Kolachalama:

50 patient blood sample data either synthetic or not
13 diagnostic variables, as numerics and without annotations 
Age, Gender, or Race variables are numerical 

Variables like ph or hb, I consider an extrapolation to modelling

Review:
1) Summary comparison of 2 models (glm() function). As response either `category` or `Gender`
2) PCA prcomp() function 
centered and scaled
`principal component score vectors have length n=50 and 
the principal component loading vectors have length p=16`
3) biplot displays 2 components or "loading vectors" listed at table with colnames PC1, or PC2, etc.
Summary per component: Standard deviation, Proportion of Variance, or Cumulative Proportion
multiplot line or bar graph of vectors


 

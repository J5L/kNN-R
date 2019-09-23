# kNN-R
Simple program that trains, tests, visualizes and evaluates a k-nearest neighbours algorithm in R. 

If your data has more than two dimensions, the program will visualize the data by a Principal Component Analysis (PCA) via the first to Principal Components.
If your data is two dimensional it will be visualized in a scatterplot.

The program takes 
- train data, 
- test data, 
- name of the column which stores the classifying variable (in string format), 
- k value.
- Optional: variable names if data is 2-dimensional.


### Data requirements
- Must be numeric, except classifier
- Column of classifier must be on the left side of the data set (first column)

### Examples
In the test.R file the algorithm is trained and tested on the wine dataset from the *UCI Machine Learning Repository* (https://archive.ics.uci.edu/ml/datasets/wine).
It also contains an S3-Method to plot the classified data.

This is an example on how to use the algorithm and how to access the results.

```R
wineKNN <- myKNN(Xtrain=winetrain, Xtest=winetest, label = "classifier", k=2)

wineKNN
wineKNN$result
wineKNN$confmatrix
```

```R
myKNN(Xtrain=iristrain, Xtest=iristest, label = "Species", k=2, var1 = "Sepal.Length", var2 = "Sepal.Width")
```


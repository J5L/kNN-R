# kNN-R
Simple program that trains, tests, visualizes and evaluates a k-nearest neighbours algorithm in R

The program takes train data, test data, name of the column which stores the classifyng variable (in string format), and the k value.
Optional variable names if data is 2-dimensional.

In the test.R file the algorithm is trained and tested on the wine dataset from the UCI Machine Learning Repository (https://archive.ics.uci.edu/ml/datasets/wine).
It also contains an S3-Method to plot the classified data.

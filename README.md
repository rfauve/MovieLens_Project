# MovieLens Project

## Rémi Fauve
## 2020-05-15

### Overview
This report was generated as part of the HarvardX online course 
[“Data Science: Capstone” (PH125.9x)](https://www.edx.org/course/data-science-capstone).

The MovieLens project’s goal is to predict movie ratings with a RMSE (Root Mean Square Error) under 0.86490 
using data science techniques. The original dataset is the 
[MovieLens 10M Dataset](https://grouplens.org/datasets/movielens/10m/) 
which includes 10 millions ratings on 10,000 movies, by 72,000 different users.

First, the dataset is imported and explored to gain insights. Then, different models are tested as recommandation
systems, and the best one is selected. Finally, the selected model is applied on the final test set.

### Environment and libraries
The code presented here was executed in the RStudio IDE (Version 1.2.5042) with R version 3.6.1 (2019-07-05).

The required libraries are: 

tidyverse, caret, data.table, ggrepel, FactoMineR, factoextra, recommenderlab, explor

### Main elements
* The data (already cleaned) was retrieved from the Web.

* Nested information have been extracted (release year, different genres) to maximise the accessible
data for training the models.
* MCA (Multiple Correspondence Analysis) was performed on the genres of the movies to reveal potential further nested information.

* The prediction attempt was focused on matrix factorisation, by adding biaises to a naive mean of all ratings.
* For multiple biaises, hierarchical order of the different biaises did not show any siginificant effect.

* Using SVD (Single Value Decomposition) on the top of the best multiple biaises model lowered the RMSE even more.

* The results on the final test set showed no sign of overfitting.
* Final RMSE : 0.86452 (without SVD)
* Final RMSE : 0.81574 (with SVD)

* Models can be further improved, for example by taking into account the genres of the movies as explicit biaises, or by increasing 
the number of implicit features and/or the number of iterations of the SVD.

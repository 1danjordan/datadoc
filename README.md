# databook

databook is an R package for generating a 'databook' from a dataset, using bookdown. A databook is a reproducible, rich and structured format for documenting data. 

Much of the focus of data analysis and data science is building models for describing some phenomenon or predicting outcomes. However, model building accounts for a very small portion of an analyst's time. The majority of an analyst's time is spent on collecting, curating, correcting and combining datasets. Data analysis is an interactive exercise, so analysts often discover  understand the characteristics of a dataset as they analyse it. Their subsequent knowledge about the characteristics of a dataset are rarely recorded or communicated in a structured format. 

In regulated industries where statistical models are audited, a regulator will examine *the suitability of the data first*, not the model - as the saying goes "Garbage in, garbage out". Each variable and data source must be described and documented. This apparently straightforward task can be very complicated and time consuming. databook sets out to automate a lot of this process. 

## Other Forms of Dataset References 

There are many ways of writing references for datasets. I've chosen three cases below that vary widely in their richness and detail. 

### Google Quick Draw Data

The [Quick Draw](https://quickdraw.withgoogle.com/data) documentation. Amazing interacive website.

The [UCI Machine Learning Repository](https://archive.ics.uci.edu/ml/datasets/statlog+(german+credit+data)) provides a reference page for each dataset. The reference starts with a with a summary table and sections regarding the data's source, additional information, attribute information, relevant papers and papers that cite this dataset. 

In some sense the `summary` function is a simple dataset reference. I provides many of the key statistics of interest such as mean, minimum and maximum for numeric variables and simple counts for categorical variables. 

``` r
summary(iris[c(1, 2, 5)])
#>   Sepal.Length    Sepal.Width          Species  
#>  Min.   :4.300   Min.   :2.000   setosa    :50  
#>  1st Qu.:5.100   1st Qu.:2.800   versicolor:50  
#>  Median :5.800   Median :3.000   virginica :50  
#>  Mean   :5.843   Mean   :3.057                  
#>  3rd Qu.:6.400   3rd Qu.:3.300                  
#>  Max.   :7.900   Max.   :4.400
```

### Similarities to Database Design

When designing a database each entity and its relationships are mapped to a table with primary and surrogate keys and (hopefully) each column is appropriately constrained. A well designed and documented database is fundamental for supporting any data analysis, however a database's primary function is effectively storing and retrieving data, rather than recording a datasets characteristics. 


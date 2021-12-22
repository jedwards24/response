
# response

<!-- badges: start -->
<!-- badges: end -->

The response package gives mean and confidence intervals of a response variable by groups of an 
input variable. The response can be numeric with confidence intervals calculated using Gaussian assumptions, or if the response has two classes then a binomial confidence interval can be used.

Output can be viewed as a table or a plot. 

It is in very early stages of development.

## Installation

You can install the development version of response from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jedwards24/response")
```

## Example

``` r
library(response)
set.seed(13)
a <- rbinom(30, 1, 0.1)
b <- rbinom(55, 1, 0.25)
c <- rbinom(15, 1, 0.7)


df <- data.frame(x = c(rep("A", 30), rep("B", 55), rep("C", 15)),
             y = c(a, b, c))
response(df, y, x, pos_class = 1)
```


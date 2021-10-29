LAB 7
================
Caroline He
10/29/2021

# Learning goals

In this lab, you are expected to learn/put in practice the following
skills:

-   Evaluate whether a problem can be parallelized or not.
-   Practice with the parallel package.
-   Use Rscript to submit jobs
-   Practice your skills with Git.

``` r
library(microbenchmark)
```

1.  This function generates a n x k dataset with all its entries
    distributed poission with mean lambda.

``` r
# create matrix filling by rows
fun1 <- function(n = 100, k = 4, lambda = 4) {
  x <- NULL
  for (i in 1:n)
    x <- rbind(x, rpois(k, lambda))
}
# create matrix by how to fill (by columns)
fun1alt <- function(n = 100, k = 4, lambda = 4) {
  matrix(rpois(n*k, lambda), nrow = n, ncol = k)
}
# Benchmarking
microbenchmark::microbenchmark(
  fun1(n = 1000),
  fun1alt(n = 1000), unit = "relative"
)
```

    ## Unit: relative
    ##               expr      min       lq     mean   median       uq     max neval
    ##     fun1(n = 1000) 36.13513 36.69524 41.03246 39.28551 51.17542 10.2061   100
    ##  fun1alt(n = 1000)  1.00000  1.00000  1.00000  1.00000  1.00000  1.0000   100

2.  Find the column max (hint: Checkout the function max.col()).

``` r
# Data Generating Process (10 x 10,000 matrix)
set.seed(1234)
x <- matrix(rnorm(1e4), nrow=10)

# Find each column's max value
fun2 <- function(x) {
  apply(x, 2, max)
}

fun2alt <- function(x) {
  # position of the max value per row of x
  idx <- max.col(t(x))
  
 dim # Get the actual max value
  x[ cbind(idx, 1:ncol(x))]
}
# Do  we get the same?
all.equal(fun2(x), fun2alt(x))
```

    ## [1] TRUE

``` r
# Benchmarking
microbenchmark::microbenchmark(
  fun2(x),
  fun2alt(x), unit = "relative"
)
```

    ## Unit: relative
    ##        expr      min      lq     mean   median       uq       max neval
    ##     fun2(x) 9.663485 8.77092 6.896958 8.191276 7.757629 0.8585593   100
    ##  fun2alt(x) 1.000000 1.00000 1.000000 1.000000 1.000000 1.0000000   100

3.  Among its many uses, non-parametric bootstrapping allow us to obtain
    confidence intervals for parameter estimates without relying on
    parametric assumptions.

The main assumption is that we can approximate many experiments by
resampling observations from our original dataset, which reflects the
population.

This function implements the non-parametric bootstrap:

``` r
my_boot <- function(dat, stat, R, ncpus = 1L) {
  
  # Getting the random indices
  n <- nrow(dat)
  idx <- matrix(sample.int(n, n*R, TRUE), nrow=n, ncol=R)
 
  # Making the cluster using `ncpus`
  # STEP 1: GOES HERE
  # STEP 2: GOES HERE
  
    # STEP 3: THIS FUNCTION NEEDS TO BE REPLACES WITH parLapply
  ans <- lapply(seq_len(R), function(i) {
    stat(dat[idx[,i], , drop=FALSE])
  })
  
  # Coercing the list into a matrix
  ans <- do.call(rbind, ans)
  
  # STEP 4: GOES HERE
  
  ans
  
}
```

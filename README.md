### Update:
Submission File: cachematrix.R

Provides 2 functions within: 
1. makeCacheMatrix
   - makeCacheMatrix creates a special "matrix" with a list of getter and setter functions.
2. cacheSolve
   - Checks whether the input matrix has been changed.
     * Yes - computes the inverse matrix
     * No - implies that the inverse had already been calculated and the cached copy should be used.


### Test Suite
I have provided 2 test sets for the test suite.
- Test Set 2
  * simpler; testing only for fetching the inverse matrix from cache for repeated access.
- Test Set 1 
  * more complete; testing for both fetching from cache for repeated access, as well as testing for when the original input matrix is changed.

<!-- -->
##Test Set 1
> source("cachematrix.R")
> m1 <- matrix(1:4, 2, 2)
> m2 <- matrix(1:4, 2, 2)
> m3 <- matrix(2:5, 2, 2)

> m1
     [,1] [,2]
[1,]    1    3
[2,]    2    4

> m2
     [,1] [,2]
[1,]    1    3
[2,]    2    4

> m3
     [,1] [,2]
[1,]    2    4
[2,]    3    5

> x <- makeCacheMatrix(m1)
> cacheSolve(x)
    [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5

> x <- makeCacheMatrix(m2)
> cacheSolve(x)
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5

> x <- makeCacheMatrix(m3)
> cacheSolve(x)
     [,1] [,2]
[1,] -2.5    2
[2,]  1.5   -1

Should fetch inverse matrix from cache since 2nd time accessing
> cacheSolve(x)
getting cached data
     [,1] [,2]
[1,] -2.5    2
[2,]  1.5   -1

> x <- makeCacheMatrix(m2)
> cacheSolve(x)
    [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5

Should fetch inverse matrix from cache since the input matrix has not changed.
> x$set(m2)
> cacheSolve(x)
getting cached data
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5


## Test Set 2
> x = rbind(c(1, -1/4), c(-1/4, 1))
> m <- makeCacheMatrix(x)
> m$get()
      [,1]  [,2]
[1,]  1.00 -0.25
[2,] -0.25  1.00

> cacheSolve(m)
          [,1]      [,2]
[1,] 1.0666667 0.2666667
[2,] 0.2666667 1.0666667

Should fetch inverse matrix from cache since 2nd time accessing
> cacheSolve(m)
getting cached data
          [,1]      [,2]
[1,] 1.0666667 0.2666667
[2,] 0.2666667 1.0666667


### Introduction

This second programming assignment will require you to write an R
function that is able to cache potentially time-consuming computations.
For example, taking the mean of a numeric vector is typically a fast
operation. However, for a very long vector, it may take too long to
compute the mean, especially if it has to be computed repeatedly (e.g.
in a loop). If the contents of a vector are not changing, it may make
sense to cache the value of the mean so that when we need it again, it
can be looked up in the cache rather than recomputed. In this
Programming Assignment you will take advantage of the scoping rules of
the R language and how they can be manipulated to preserve state inside
of an R object.

### Example: Caching the Mean of a Vector

In this example we introduce the `<<-` operator which can be used to
assign a value to an object in an environment that is different from the
current environment. Below are two functions that are used to create a
special object that stores a numeric vector and caches its mean.

The first function, `makeVector` creates a special "vector", which is
really a list containing a function to

1.  set the value of the vector
2.  get the value of the vector
3.  set the value of the mean
4.  get the value of the mean

<!-- -->

    makeVector <- function(x = numeric()) {
            m <- NULL
            set <- function(y) {
                    x <<- y
                    m <<- NULL
            }
            get <- function() x
            setmean <- function(mean) m <<- mean
            getmean <- function() m
            list(set = set, get = get,
                 setmean = setmean,
                 getmean = getmean)
    }

The following function calculates the mean of the special "vector"
created with the above function. However, it first checks to see if the
mean has already been calculated. If so, it `get`s the mean from the
cache and skips the computation. Otherwise, it calculates the mean of
the data and sets the value of the mean in the cache via the `setmean`
function.

    cachemean <- function(x, ...) {
            m <- x$getmean()
            if(!is.null(m)) {
                    message("getting cached data")
                    return(m)
            }
            data <- x$get()
            m <- mean(data, ...)
            x$setmean(m)
            m
    }

### Assignment: Caching the Inverse of a Matrix

Matrix inversion is usually a costly computation and there may be some
benefit to caching the inverse of a matrix rather than computing it
repeatedly (there are also alternatives to matrix inversion that we will
not discuss here). Your assignment is to write a pair of functions that
cache the inverse of a matrix.

Write the following functions:

1.  `makeCacheMatrix`: This function creates a special "matrix" object
    that can cache its inverse.
2.  `cacheSolve`: This function computes the inverse of the special
    "matrix" returned by `makeCacheMatrix` above. If the inverse has
    already been calculated (and the matrix has not changed), then
    `cacheSolve` should retrieve the inverse from the cache.

Computing the inverse of a square matrix can be done with the `solve`
function in R. For example, if `X` is a square invertible matrix, then
`solve(X)` returns its inverse.

For this assignment, assume that the matrix supplied is always
invertible.

In order to complete this assignment, you must do the following:

1.  Fork the GitHub repository containing the stub R files at
    [https://github.com/rdpeng/ProgrammingAssignment2](https://github.com/rdpeng/ProgrammingAssignment2)
    to create a copy under your own account.
2.  Clone your forked GitHub repository to your computer so that you can
    edit the files locally on your own machine.
3.  Edit the R file contained in the git repository and place your
    solution in that file (please do not rename the file).
4.  Commit your completed R file into YOUR git repository and push your
    git branch to the GitHub repository under your account.
5.  Submit to Coursera the URL to your GitHub repository that contains
    the completed R code for the assignment.

### Grading

This assignment will be graded via peer assessment.

# $Id$
# cachematrix.R
# =============
#
# Introduction
# ------------
#     This second programming assignment will require you to write an R 
#     function is able to cache potentially time-consuming computations. For 
#     example, taking the mean of a numeric vector is typically a fast 
#     operation. However, for a very long vector, it may take too long to 
#     compute the mean, especially if it has to be computed repeatedly (e.g. 
#     in a loop). If the contents of a vector are not changing, it may make 
#     sense to cache the value of the mean so that when we need it again, it 
#     can be looked up in the cache rather than recomputed. In this Programming 
#     Assignment will take advantage of the scoping rules of the R language 
#     and how they can be manipulated to preserve state inside of an R object.
#
# Example: Caching the Mean of a Vector
# -------------------------------------
#     In this example we introduce the <<- operator which can be used to 
#     assign a value to an object in an environment that is different from 
#     the current environment. Below are two functions that are used to create 
#     a special object that stores a numeric vector and cache's its mean.
#
#     The first function, makeVector creates a special "vector", which is 
#     really a list containing a function to
#
#         :: set the value of the vector
#         :: get the value of the vector
#         :: set the value of the mean
#         :: get the value of the mean
#
#         makeVector <- function(x = numeric()) {
#             m <- NULL
#             set <- function(y) {
#                 x <<- y
#                 m <<- NULL
#             }
#             get <- function() x
#             setmean <- function(mean) m <<- mean
#             getmean <- function() m
#             list(set = set, get = get,
#                  setmean = setmean,
#                  getmean = getmean)
#         } # makeVector
#
#     The following function calculates the mean of the special "vector" 
#     created with the above function. However, it first checks to see if 
#     the mean has already been calculated. If so, it gets the mean from the 
#     cache and skips the computation. Otherwise, it calculates the mean of 
#     the data and sets the value of the mean in the cache via the setmean 
#     function.
#
#         cachemean <- function(x, ...) {
#             m <- x$getmean()
#             if(!is.null(m)) {
#                 message("getting cached data")
#                 return(m)
#             }
#             data <- x$get()
#             m <- mean(data, ...)
#             x$setmean(m)
#             m
#         }
# 
# Assignment: Caching the Inverse of a Matrix
# -------------------------------------------
#     Matrix inversion is usually a costly computation and there may be some 
#     benefit to caching the inverse of a matrix rather than compute it 
#     repeatedly (there are also alternatives to matrix inversion that we 
#     will not discuss here). Your assignment is to write a pair of functions 
#     that cache the inverse of a matrix.
#
#     Write the following functions:
# 
#         1. makeCacheMatrix: This function creates a special "matrix" object 
#                             that can cache its inverse.
#         2. cacheSolve: This function computes the inverse of the special 
#                        "matrix" returned by makeCacheMatrix above. If the 
#                        inverse has already been calculated (and the matrix 
#                        has not changed), then the cachesolve should retrieve 
#                        the inverse from the cache.
#
#     Computing the inverse of a square matrix can be done with the solve 
#     function in R. For example, if X is a square invertible matrix, then 
#     solve(X) returns its inverse.
#
#     For this assignment, assume that the matrix supplied is always invertible.
#
#     In order to complete this assignment, you must do the following:
#
#         1. Fork the GitHub repository containing the stub R files at 
#            https://github.com/rdpeng/ProgrammingAssignment2 to create a 
#            copy under your own account.
#         2. Clone your forked GitHub repository to your computer so that you 
#            can edit the files locally on your own machine.
#         3. Edit the R file contained in the git repository and place your 
#            solution in that file (please do not rename the file).
#         4. Commit your completed R file into YOUR git repository and push 
#            your git branch to the GitHub repository under your account.
#         5. Submit to Coursera the URL to your GitHub repository that contains 
#            the completed R code for the assignment.
#
#     In addition to submitting the URL for your GitHub repository, you will 
#     need to submit the 40 character SHA-1 hash (as string of numbers from 
#     0-9 and letters from a-f) that identifies the repository commit that 
#     contains the version of the files you want to submit. You can do this 
#     in GitHub by doing the following:
#
#         1. Going to your GitHub repository web page for this assignment
#         2. Click on the "?? commits" link where ?? is the number of commits 
#            you have in the repository. For example, if you made a total of 10 
#            commits to this repository, the link should say "10 commits".
#         3. You will see a list of commits that you have made to this
#            repository. The most recent commit is at the very top. If this
#            represents the version of the files you want to submit, then just
#            click the "copy to clipboard" button on the right hand side that
#            should appear when you hover over the SHA-1 hash. Paste this SHA-1 
#            hash into the course web site when you submit your assignment. If 
#            you don't want to use the most recent commit, then go down and 
#            find the commit you want and copy the SHA-1 hash.
#
#     A valid submission will look something like (this is just an example!)
#
#         https://github.com/rdpeng/ProgrammingAssignment2
#         7c376cc5447f11537f8740af8e07d6facc3d9645
# 
# Grading
# -------
#     This assignment will be graded via peer assessment. During the 
#     evaluation phase, you must evaluate and grade the submissions of at 
#     least 4 of your classmates. If you do not complete at least 4 
#     evaluations, your own assignment grade will be reduced by 20%.
#
#     $Log$
#

##########
## create a special "matrix" object that can cache its inverse
##########
makeCacheMatrix <- function(x = matrix()) {
    # local object(s) declaration & initialisation
    m <- NULL
    
    # "method" for setting the object's value(s)
    set <- function(y) {
        x <<- y
        m <<- NULL
    } # set
    
    # "method" for retrieving the object's value(s)
    get <- function() {
        x
    } # get
    
    # "method" for setting the object's inverse matrix
    setinvmat <- function(invmat) {
        m <<- invmat
    } # setinvmat
    
    # "method" for retrieving the object's inverse matrix
    getinvmat <- function() {
        m
    } # getinvmat
    
    # return matrix object
    list(set = set, get = get, setinvmat = setinvmat, getinvmat = getinvmat)
} # makeCacheMatrix

##########
## compute the inverse of the special "matrix" returned by makeCacheMatrix
##########
cacheSolve <- function(x, ...) {
    # check for mean having already been calculated
    m <- x$getinvmat()
    
    if(!is.null(m)) {
        message("getting cached data")
        
    # re-calculate the mean value(s)
    } else {
        data <- x$get()
        m <- solve(data, ...)
        x$setinvmat(m)
    }
    
    # Return a matrix that is the inverse of 'x'
    m
} # cacheSolve

##########
## test "makeCacheMatrix" & "cacheSolve"
##########
#testMatFun <- function() {
    ##==========
    ## create source data
    ##==========
    # create square matrices
    m1 <- matrix(c(1, 0, 0, 1), nrow=2, ncol=2)
    m2 <- matrix(c(1, 0, 1, 1), nrow=2, ncol=2)
    m3 <- matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 1), nrow=3, ncol=3)
    m4 <- matrix(c(1, 0, 1, 0, 1, 0, 0, 0, 1), nrow=3, ncol=3)
    
    # calculate the inverse thereof
    m1i <- solve(m1)
    m2i <- solve(m2)
    m3i <- solve(m3)
    m4i <- solve(m4)
    
    ##==========
    ## create matrix objects
    ##==========
    mx1 <- makeCacheMatrix(m1)
    mx2 <- makeCacheMatrix(m2)
    mx3 <- makeCacheMatrix(m3)
    mx4 <- makeCacheMatrix(m4)

    ##==========
    ## check matrix objects just created
    ##==========
    # compare original matrix with corresponding matrix object
    identical(m1, mx1$get())
    identical(m2, mx2$get())
    identical(m3, mx3$get())
    identical(m4, mx4$get())

    # check for inverse matrices
    mx1$getinvmat()
    mx2$getinvmat()
    mx3$getinvmat()
    mx4$getinvmat()
    
    ##==========
    ## create inverse matrices
    ##==========
    cacheSolve(mx1)
    cacheSolve(mx2)
    cacheSolve(mx3)
    cacheSolve(mx4)

    # compare original inverse matrix with corresponding matrix object
    identical(m1i, mx1$getinvmat())
    identical(m2i, mx2$getinvmat())
    identical(m3i, mx3$getinvmat())
    identical(m4i, mx4$getinvmat())
    
    ##==========
    ## retrieve cached inverse matrices
    ##==========
    cacheSolve(mx1)
    cacheSolve(mx2)
    cacheSolve(mx3)
    cacheSolve(mx4)
    
    # compare original inverse matrix with corresponding matrix object
    identical(m1i, mx1$getinvmat())
    identical(m2i, mx2$getinvmat())
    identical(m3i, mx3$getinvmat())
    identical(m4i, mx4$getinvmat())
        
    # clean up
    rm(m1, m2, m3, m4, m1i, m2i, m3i, m4i, mx1, mx2, mx3, mx4)
    rm(makeCacheMatrix, cacheSolve)
    #rm(testMatFun)
#} # testMatFun

#
# end of file
#

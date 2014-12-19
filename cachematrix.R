## The following two functions provide the method to cache 
## calculation of the inverse (use "lazy" value) of the given matrix
## 
##  thecachedmatrix<-makeCacheMatrix(thematrix)
##  cacheSolve(thecachedmatrix)
##  
## Usage example and details: 
## 
## # For some matrix M
##
##   M<-matrix(c(cos(pi/6), sin(pi/6), -sin(pi/6), cos(pi/6)), nrow=2, ncol=2)
##   M
## 
## # let's create "cache matrix" object 
##   cacheM <- makeCacheMatrix(M)
##   
## # The default method for calculation of the inverse matrix:
##   inv(M) # calculates the inverse matrix each time 
## 
## # The fist call:  
##
##   cacheSolve(cacheM) # calculates, caches and returns the inverse matrix
##
## # The second call (and the following calls)
##
##   cacheSolve(cacheM) #  only returns the cached inverse matrix
## 
## #  Note: In general it is possible to reuse the object cacheM:
## #  For 
##   M1<-matrix(c(2, 0, 0, -2), nrow=2, ncol=2)
##
##   cacheM$set(M1)
##   cacheSolve(cacheM)
## 
## But  this method seems to break encapsulation. 
##       More natural would be fully recreate the object:
##   cacheM<-cacheSolve(M1)
##   cacheSolve(cacheM) 


# This function (makeCacheMatrix) 
#           - accepts one parameter: source matrix
#           - creates (returns) an object (special "matrix") 
#             which holds the original matrix 
#           
# Note: This function also provides 3 additional methods to be considered 
#       "private"  for this object:
#    - cacheMatrixObject$get() - get the source matrix 
#    - cacheMatrixObject$set(x) - (re)set the source matrix to new value
#                                 also discard (nullify) the cached value
#                                 of the inverse matrix
#    - cacheMatrixObject$setSolve(inv) -  where inv the value of inverse matrix
#    - cacheMatrixObject$getSolve() - get the inverse matrix
#  None of these 3 functions (probably except of the first one) is supposed to 
#  be called directly!
#    
# This function is complemented by the function cacheSolve defined below

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setSolve <- function(inv) m <<- inv
    getSolve <- function() m
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}


# The following function calculates the inverse of the special "matrix" 
# created by the function makeCacheMatrix 
# However, it first checks to see if the inverse matrix has already been calculated. 
# If so, it gets the inverse matrix from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the original matrix and sets the value 
# of the inverse matrix in the cache via the setSolve function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getSolve()
    if(!is.null(m)) {
        message("getting the cached inverse matrix")
        return(m)
    }
    message("calculating the very first time the inverse matrix (and caching it)")
    data <- x$get()
    m <- solve(data,...)
    x$setSolve(m)
    m
}


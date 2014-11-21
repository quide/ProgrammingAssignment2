## quick calculation of inverse matrices
## as given vector example but changed for matrices 
## and inverse instead of mean

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(mx = matrix()) {
    # clean initialization of special variable
    inv <- NULL
    set <- function(matriz) {
        mx <<- matriz
        inv <<- NULL
    }
    get <- function() mx
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##  If the inverse has already been calculated (and the matrix has not changed), 
##   then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(mx, ...) {
    inv <- mx$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- mx$get()
    inv <- solve(data, ...)
    mx$setinv(inv)
    ## Return a matrix that is the inverse of 'x'
    inv
}

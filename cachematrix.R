# The following functions find the inverse of a matrix. The calculations "cache" 
# the results of potentially time-consuming computations 
#
# Example of use
#a <- makeCacheMatrix(matrix(1:4,2))
#cacheSolve(a)
#cacheSolve(a) <-- at this step a cached value is used and the "getting cached data" message displayed

# makeCacheMatrix creates a special "matrix" object that can cache its inverse.
# It returns a list with 4 functions: set, get, setinv and getinv 
makeCacheMatrix <- function(x = matrix()) {
    inv_x <- NULL
    set <- function(y) {
        x <<- y
        inv_x <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv_x <<-inverse
    getinv <- function() inv_x
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

# cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix.
# If the inverse has been computed already, it is returned from the cache. Otherwise,
# the matrix inverse operation will be performed and the result cached. 
cacheSolve <- function(x, ...) {
    inv_x <- x$getinv()
    if (!is.null(inv_x)) {
        message("getting cached data")
        return(inv_x)
    } 
    inv_x <- solve(x$get())
    x$setinv(inv_x)
    inv_x
}


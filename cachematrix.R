## Two functions that together invert a matrix once and cache it so that it 
## doesn't need computing again  


#Call function below first - set its value to an object. Only need to call this 
# function once for a given matrix
makeCacheMatrix <- function(x = matrix()) {
    inv.x <- NULL
    set <- function(y) {
        x <<- y
        inv.x <<- NULL
    }
    get <- function() x
    set.inverse <- function(inverse) inv.x <<- inverse
    get.inverse <- function() inv.x
    list(set=set,get = get,
         set.inverse = set.inverse,
         get.inverse = get.inverse)
}


## Call this function using as argument the object assigned from
## function above. Keep calling this function everytime you want to invert
## the same matrix. Second time it just reads in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv.x <- x$get.inverse()
    if(!is.null(inv.x)) {
        message("getting cached data")
        return(inv.x)
    }
    mat <- x$get()
    inv.x <-  solve(mat, ...)
    x$set.inverse(inv.x)
    inv.x
}


## cacheMatrix enables caching of matrix inverses for later reuse without the need of recomputation

## makeCacheMatrix creates a list with functions to set & get the data as well as set & get the cached inverse of that data matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(invs) inv <<- invs
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve computes the inverse of a given data matrix, but skips computation if data had not changed and the inverse could be retrieved via the cached result

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting inverse from cache")
                return(inv)
        } else {
                data <- x$get()      
                inv <- solve(data)
                x$setinv(inv)
        }
        inv
}

## When used together the following functions compute and cache 
## the inverse of a given matrix. 
## Usage: If cx = makeCacheMatrix(x) then cacheSolve(cx) will
## always return x inverse but compute it only once.

## Returns a stateful wrapper (object) for x that lets the inverse 
## of x be stored and retrieved.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    store.matrix <- function(y) {
            x <<- y
            inv <<- NULL
    }
    unpack.matrix <- function() x
    
    store.inv <- function(inverse) inv <<- inverse
    unpack.inv <- function() inv
    list(store.matrix = store.matrix,
         unpack.matrix = unpack.matrix,
         store.inv = store.inv,
         unpack.inv = unpack.inv)
}

## Returns the inverse of cx, only when cx = makeCacheMatrix(x). 
## Computes and caches the inverse of x when first called returns
## cached inverse on subsequent calls to cacheSolve.

cacheSolve <- function(cx, ...) {
    ## Return a matrix that is the inverse of cx$unpack.matrix()
    if(!is.null(cx$unpack.inv())) {
        message(paste("getting cached inverse for",
                      deparse(substitute(cx))))
        return(cx$unpack.inv())
    }
    x.inv <- solve(cx$unpack.matrix(), ...)
    cx$store.inv(x.inv)
    x.inv
}

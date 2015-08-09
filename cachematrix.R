
## makeCacheMatrix creates (and returns) a matrix object that can cache its inverse

makeCacheMatrix <- function( M = matrix() ) {
    inverseOfM <- NULL
    set <- function( inputMat ) 
    {
        M <<- as.matrix( inputMat )        
        inverseOfM <<- NULL
    }

    get <- function() M

    setInverse <- function( inverse )
    { 
        inverseOfM <<- inverse
    }
    
    getInverse <- function() inverseOfM

    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse )
}

## cacheSolve accepts a matrix object as an argument and returns the inverse

cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if( !is.null( inverse ) )
    {
        message("getting Cached Inverse")
        return( inverse )
    }

    mat <- x$get()
    inverse <- solve( mat )
    x$setInverse( inverse )
    inverse
}



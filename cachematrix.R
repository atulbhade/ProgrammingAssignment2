## This function is able to cache potentially time-consuming computations. It will use cache
## to give already calculated data rather then recalculating it. 

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
 
}


## This function will calculate the inverse and if it is already calculated then it will retrieve from cache

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.nullinverse(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmean(m)
        m
}

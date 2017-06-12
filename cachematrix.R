## makeCacheMatrix creates a special "matrix", which does the following:
## 1. sets the value of the matrix
## 2. gets the value of the matrix
## 3. sets the value of the inverse
## 4. gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solveMatrix) inv <<- solveMatrix
        getinverse <- function() inv
        list(set = set, 
             get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}

## Calculate the inverse of the special "matrix" created above, after first
## checking to see if the inverse has already been created.  If so, it gets the
## inverse from the cache. Otherwise it calculates it and sets the value of
## the inverse in the cache using setmean.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ## Calculate a matrix that is the inverse of 'x'
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

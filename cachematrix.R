## Solves inversese of matrix if matrix is invertable


## Creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                X <<- y
                inv <<- NULL
        }
        get <- function()x
        setinv <- function(inverse)inv <<- inverse
        getinv <- function()inv
        list(set = set, get = get, setinv = setinv,
             getinv = getinv)
}


##computes inverse of above matrix returned by function above.
##If inverse has already been calculated and matrix is unchanged
##it should retrieve inverse from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...) ##solve = returns inverse
        x$setinv(inv)
        inv
}

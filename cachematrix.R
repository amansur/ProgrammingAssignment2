## Create a special matrix object that can store its inverse and retrieve it
## when needed (i.e. cache)

## Create a "matrix" object with four methods:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the matrix inverse
## 4. get the value of the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list (set = set, get = get, setinv = setinv, getinv = getinv)
}


## Function takes as input a CacheMatrix object
## It will attempt to retrieve a previously calculated matrix inverse 
## stored in the object
## If no inverse is found, this function will calculate the inverse via solve()
## and return the matrix inverse
cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if (!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}

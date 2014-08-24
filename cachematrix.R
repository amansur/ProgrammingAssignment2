## Create a special matrix object that can store its inverse and retrieve it
## when needed (i.e. cache)

## Create a "matrix" object with four methods:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the matrix inverse
## 4. get the value of the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
    # initialize the inv variable to NULL. inv will store the matrix inverse
    inv <- NULL
    # define the set function to modify the value of the matrix
    # when a new value is set(), also assign a NULL value to inv
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    ## define the get function which will retrieve the value of the matrix
    get <- function() x
    
    ## define the setinv function which will allow you to store the matrix
    ## inverse in the inv variable
    setinv <- function(inverse) inv <<- inverse
    ## define the getinv function which will allow you to retrieve the 
    ## matrix inverse
    getinv <- function() inv
    ## return a list of the internal methods we just defined allowing access
    ## to them
    list (set = set, get = get, setinv = setinv, getinv = getinv)
}


## Function takes as input a CacheMatrix object
## It will attempt to retrieve a previously calculated matrix inverse 
## stored in the object
## If no inverse is found, this function will calculate the inverse via solve()
## and return the matrix inverse
cacheSolve <- function(x, ...) {
    ## try to get the inverse value stored in the CacheMatrix object x
    inv <- x$getinv()
    ## if this value is not NULL, notify the user, that this is cached data
    ## and return the matrix inverse
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    ## otherwise, retrieve the matrix data from the CacheMatrix object 
    ## and store it in the data variable
    data <- x$get()
    ## calculate the inverse on this data matrix and 
    ## assign the value to the variable inv
    inv <- solve(data)
    ## store this calculated value in the CacheMatrix object using
    ## the setinv method
    x$setinv(inv)
    ## return the matrix inverse
    inv
}
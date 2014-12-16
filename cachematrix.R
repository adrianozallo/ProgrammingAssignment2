
## The functions 'makeCacheMatrix()' and 'cacheSolve()' cache the inverse of a matrix.
## The matrix inversion is a costly computation. When dealing with large matrices
## it is convenient to cache their values instead of  compute them repeatedly.

## The matrix  passed to 'makeCacheMatrix' function creates an 'object' of type list.
## This object stores the original matrix value and the cached value that initially is set
## to 'NULL'.Four functions are also created, two to read the values  of the two things we are storing
## ('get')and two other functions to change them ('set').

makeCacheMatrix <- function(x = matrix()) { # input x is a matrix
    m <- NULL                       # is the output of the function;
                                    # it is reset to NULL
                                    # every time makeCacheMatrix() is called
    set <- function(y) {            # the following 3 functions are only defined here;
                                    # they are run by cacheSolve() to get values of x or m
                                    # and setting the inverse of x
        x <<- y
        m <<- NULL
    }
    get <- function() x             # this function returns the value of the original matrix
    setinverse <- function(solve) { m <<- solve }
                                    # it is called by cacheSolve() during the first
                                    # 'cachesolve()' access; it stores the value using superassignment
    getinverse <- function() m      # it will return the cached value
                                    # to 'cacheSolve()' on subsequent accesses
    list(set = set, get = get,      # this is a list of the internal functions ('methods')
        setinverse = setinverse,
        getinverse = getinverse)


}


## This function access the 'object' created when 'makeCacheMatrix()' is called,
## fetching the value of the matrix used to create the 'object'.
## If the inverse has not yet been calculated ('NULL'), 'cacheSolve()' calculates the inverse
## matrix and stores it  in the object created by the call to 'makeCacheVector()' and returns
## the inverse matrix value. If the inverse matrix has been calculated earlier 'cacheSolve()'
## fetches it and returns the value, saving computing time.


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of x
                                    # x is an 'object' created by makeCacheMatrix()
    m <- x$getinverse()             # the inverse of x is stored in m

    if(!is.null(m)) {              # if inverse of x is already in the cache
        message("getting cached data")
                                   # this message is send to the console
                                   
        return(m)                  # and the cache value is returned
    }
    data <- x$get()                # this code is reached only if
                                   # x$getinverse() returned NULL;
    m <- solve(data, ...)          # in this case the inverse of x needs
                                   # to be calculated
    x$setinverse(m)                # and stored
    m                              # returns the mean to the code that called this function

}

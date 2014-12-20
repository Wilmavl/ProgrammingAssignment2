## makeCacheMatrix saves the matrix into cache so it can be used in subsequent calls 
## rather than recalculated each time the function cacheSolve is called

## makeCacheMatrix saves the matrix into cache

makeCacheMatrix <- function(x = matrix()) {
        m <-NULL                   # clear contents of m in this environment, every time makeCacheMatrix is called
        set <- function (y) {
                x <<- y            # update x in containing environment
                m <<- NULL         # clear contents of m in containing environment
        }                          # end of function set
        get <- function() x        #return value of original matrix
        setsolve <- function(solve) m <<- solve #<<- means it is the m in containing environment, called by cachesolve() during first cachesolve()
        getsolve <- function() m   # return the cached value on subsequent access
        list(set = set, get = get, #list of internal functions so function knows how to access methods
             setsolve = setsolve,
             getsolve = getsolve)
}                                  # end of function makeCacheMatrix


## cacheSolve returns a matrix that is the inverse of x

cacheSolve <- function(x, ...) {    #input x is object created by makeCacheMatrix
        m <- x$getsolve()           #accesses x, gets the inversed matrix
        if (!is.null(m)) {          #the cache is not empty
                message("getting cached data")   #displays message on console
                return(m)           #display cache contents, exits function
        }                           # end if
        data <- x$get()             #cache was empty, now gets x
        m <- solve(data, ...)       #calculate inverse matrix
        x$setsolve(m)               #store inverse matrix in x
        m                           #return to code that called this function
}                                   # end of function cacheSolve

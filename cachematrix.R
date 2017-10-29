

# The pair of functions coded below are able to cache potentially
# time-consuming computations. It may take a long time to calculate the
# inverse of an array, especially if it has to be computed repeatedly
# (e.g. in a loop).

# If the content of a matrix is not changing, it may make sense to cache
# the value of the inverse matrix so that when we need it again, it can
# be looked up in the cache rather than recomputed.

# We will use the scoping rules of the R language and their handling to
# preserve state inside of an R object.

# In this example we'll use the <<- operator which can be used to
# assign a value to an object in an environment that is different from
# the current environment.

# The functions below are used to create a special object that stores a
# numeric matrix and caches its inverse.

# The first function, makeCacheMatrix creates a special "matrix", which
# is really a list containing a function to

# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse matrix
# 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        
        
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
        
}


# The following function calculates the inverse of the special "matrix"
# created with the above function. However, it first checks to see if
# the inverse matrix of the given "matrix" has already been calculated.

# If so, it gets that one from the cache and skips the computation.
# Otherwise, the inverse is calculated and it sets the resulting value
# in the cache via the setmean function.


cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        
        
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        d <- x$get()
        i <- solve(d, ...)
        x$setinverse(i)
        i
        
}

#Matrix Examples
m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
m1
m2 <- solve(m1)

m1 %*% m2


# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
                        m <- NULL
                        set <- function(y) {
                                x <<- y
                                m <<- NULL
                        }
                        get <- function() x
                        setmatrix <- function(solve) m <<- solve
                        getmatrix <- function() m
                        list(set = set, get = get,
                             setmatrix = setmatrix,
                             getmatrix = getmatrix)
}

makeCacheMatrix(m1)

#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
                m <- x$getmatrix()
                if(!is.null(m)) {
                        message("getting cached data")
                        return(m)
                }
                data <- x$get()
                m <- solve(data, ...)
                x$setmatrix(m)
                m
        
}


#Functions Example

m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)

#Solve m1 to compare with functions results
m2 <- solve(m1)

#Calling makeCacheMatrix
myMatrix_object <- makeCacheMatrix(m1)

#Calling cacheSolve
cacheSolve(myMatrix_object)

#With "Getting cache data" Message
cacheSolve(myMatrix_object)

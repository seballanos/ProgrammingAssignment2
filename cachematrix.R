        ## In combination, the functions allow to cache the inverse of a 
        ## matrix. 
        
        ## Create an special matrix object that catches its inverse.
        
makeCacheMatrix <- function(x = matrix()) {
                
                inver <- NULL
                set <- function(y) {
                        x <<- y
                        inver <<- NULL
                }
                get <- function() x
                setinv <- function(inverse) inver <<- mean
                getinv <- function() inver
                list(set = set, get = get,
                     setinv = setinv,
                     getinv = getinv)
        }


## This function allow to compute the inverse of the special matrix previously
## created with MakeCacheMatrix().  If the inverse has already been calculated
## it retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
                ## Return a matrix that is the inverse of 'x'
                
                inver <- x$getinv()
                if(!is.null(inver)) {
                        message("getting cached data")
                        return(inver)
                }
                data <- x$get()
                inver <- solve(data, ...)
                x$setinv(inver)
                inver
}
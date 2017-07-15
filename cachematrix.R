## The assignment is to write a pair of functions that cache the inverse of a matrix.
## The following functions calculate the inverse of a matrix and saves it
## to the cache so when the next time the user attempts to calculate the
## matrix inverse, the previously saved value is returned instead of repeating the calculation.

## This function creates a special "matrix" object that can cache its inverse. 
## It is s really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                
                x <<- y ## assigns input matrix y to the variable x in the parent environment
                inv <<- NULL ## re-initialize inv parent environment to null
                }
        
        get<-function() x ## return matrix x
        setinverse <-function(inverse) inv <<- inverse ## set the cache inv equal to the inverse of matrix x
        getinverse <- function() inv ## return the cached inverse of x
        
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function computes the inverse of the "matrix" created by 
## makeCacheMatrix function above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache instead of calculating it again.
        
cacheSolve <- function(x, ...) {
                ## Return a matrix that is the inverse of 'x'
                inv <- x$getinverse()
                if (!is.null(inv)) {
                        message("getting cached data")
                        return(inv)
                }
                data <- x$get()
                inv <- solve(data, ...)
                x$setinverse(inv)
                inv
        }

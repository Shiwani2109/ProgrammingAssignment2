##Description about function 1: 
##This function creates a special "matrix" object that can cache its inverse.
## Here in this function first the value of matrix is set with the help of set function.
## We can check the weather the value of matrix 'x' is set or not with the help of get function 
##(e.g. If we write 'x$get()' on consol it will display the matrix 'x')
## Then we can set the inverse of matrix 'x' with the help of setinverse() function.
## We can get the inverse of matrix 'x' with the help of getinverse() function.
##(e.g. If we write 'x$getinverse()' on consol it will display inverse of matrix 'x'
## So this function stores a matrix and caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

##Description about function 2:
##This function computes the inverse of above created matrix. 
##If the inverse is has been already calculated then it showa the message "getting cached data".

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}

## Running the above functions in console

A <- makeCacheMatrix(x=matrix(11:14,2,2))
cacheSolve(A)  # This will compute the inverse as it doesnt exist in cache
#      [,1] [,2]
# [1,]   -7  6.5
# [2,]    6 -5.5

cacheSolve(A)  # Thsi will retrieve the cached inverse 
# getting cached data.
#      [,1] [,2]
# [1,]   -7  6.5
# [2,]    6 -5.5



## I have used the sample provided on the Assignments page to create a cached 
## Matrix object which saves the original matrix and also the inverse of the  matrix.
## The second function caceSolve has some changes in order to ensure that we can
## also match the matrix in case we are not sure what matrix is saved in the cache
## and we need to verify it further before retrieving the cached inverse.

## This function uses the template provided by the instructors to create a function
## for storing the matrix value and its inverse. The functions are -
## 1. Setting the matrix
## 2. Get the value of the matrix
## 3. Set the inverse of the matrix
## 4. Get the saved inverse of the matrix

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


## In this function, we need to supply the cached object. We can also (optional)
## supply the matrix we assume is in the object (varibale 'mat') for verificaton. 
## The function does the following things
## 1. Get the inverse matrix from cached object
## 2. Get the original matrix from cached object
## 3. Check if inverse exists and the matrix that needs to be inverted is actually
##      a. A matrix
##      b. Has same columns and rows as original cached matrix
##      a. Is equal to the cached object matrix
## 4. Or if no new matrix (mat) was supplied then -
## proceed to get the cached inverse
## 5. If the new matrix information does not match the cached matrix, the 
##    the verification fails and message is displayed along with the cached matrix
## 6. The final step, if no matrix inverse is present then it caclulates one and
##    stores it in the object.

cacheSolve <- function(x, mat = NULL, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    matrx <- x$get()
        if(!is.null(i) && 
               ((is.matrix(mat) && dim(matrx) == dim(mat) && all(matrx == mat)) || is.null(mat))) {
            message("getting cached data")
            return(i)
        }
        else if (!is.matrix(mat) || dim(matrx) != dim(mat) || 
                     (dim(matrx) == dim(mat) && all(matrx != mat)))  {
            message("the cached object does not contain the supplied matrix. It contains")
            print(matrx)
        }
    else {
    i <- solve(matrx, ...)
    x$setinverse(i)
    return(i)
    }
}

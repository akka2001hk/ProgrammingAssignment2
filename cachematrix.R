## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
          y <- matrix()  ## initialize local varibales
	  m <- NULL
        set <- function(y) { 
                x <<- y
                m <<- NULL
        }
        getmatrix <- function() x
        invertmatrix <- function(solve) m <<- solve
        getinvert <- function() m
        list(set = set, getmatrix = getmatrix,
             invertmatrix = invertmatrix,
             getinvert = getinvert)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        data <- matrix()   ## initialize variable
        m <- x$getinvert() ## get invert matrix if calculated already
 	  if(!is.null(m) && identical(x$getmatrix(),x)) {    ## inverse has already been calculated (and the matrix has not changed
                message("getting cached data")
                return(m) ## return inverse calculated previously
        }
	  data <- x$getmatrix() ## get the original matrix
	  if(det(data) == 0) stop ("Matrix is singular and cannot be inverted") ## check if it can be inverted
	  m <- solve(data) ## compute the inverse
        m ## return result


}


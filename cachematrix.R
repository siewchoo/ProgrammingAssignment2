## Constructs and provide getter and setter methods for a matrix. 
## functions do.
##
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL		## Inverse matrix
	prevMatrix <- NULL	## Used to track if the matrix has changed

	## Sets the input matrix.
        set <- function(y)
                x <<- y

	## Returns the input matrix.
        get <- function()
		x

	## Sets (ie. cache) the output inverse matrix.
        setInverse <- function(inverse)
		i <<- inverse

	## Returns the cached inverse matrix.
        getInverse <- function()
		i

	## Sets the previous input matrix.
	setPrevMatrix <- function(y)
		prevMatrix <<- y

	## Returns the previous input matrix.
	getPrevMatrix <- function() 
		prevMatrix

	## Returns a list of functions for the input matrix.
	## Usage: list(x)
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse,
	     setPrevMatrix = setPrevMatrix,
	     getPrevMatrix = getPrevMatrix)
}


## Returns a matrix that is the inverse of an input matrix, x
cacheSolve <- function(x, ...) {

	## Get the previous input matrix and cached inverse matrix  
	## ready for downstream comparisons.
	prevMatrix <- x$getPrevMatrix()
        i <- x$getInverse()

	## If the previous input matrix and inverse matrix are 
 	## not null, it means that this is not the first time the 
	## program has been ran.
        if (!is.null(prevMatrix) && !is.null(i)) {

		## Is the current input matrix the same as the 
		## previous one? If yes, save the computation 
		## and just return the cached inverse matrix.
		if (identical(x$get(), prevMatrix)) {

        	        message("getting cached data")
                	return(i)
		}
        }

	## If current and previous matrices are different, then we 
	## will need to call R's solve() function to compute the 
	## inverse of the input matrix.
	i <- solve(x$get())

	## Then cache the inverse matrix output from the previous step.
        x$setInverse(i)

	## Then tag the current input matrix as the previous matrix 
	## in preparation for tracking in the next run.
	x$setPrevMatrix(x$get())

	## Finally, return the inverse matrix.
        i
}
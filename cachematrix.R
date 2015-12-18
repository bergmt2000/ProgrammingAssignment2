
# SUMMARY: These functions/objects will take an invertilbe matrix
# and will return the inverse of that matrix
#
# Also, if, the data are already calculated, don't recompute, but return the cached inverseMatrix that was already calc-ed
# Otherwise, compute the inverse, assign theInverse, and return the inversedData


## This is the object definition of our specialMatrix Object
## which is built to cache the inverse Matrix of a specialMatrix Object

makeCacheMatrix <- function(x = matrix()) 
{
    ## Create an object attribute variable to hold the inversed Matrix Object, different from other OO languages
    theInverse <- NULL
    
    ## Set the original matrix value, default would be an empty matrix object
    set <- function(matrixObj) 
    {
        x <<- matrixObj
        theInverse <<- NULL
    }
    
    #get - returns the original matrix object passed into makeCacheMatrix
    get <- function() 
    {
        x
    }
    
    ##  setInverse just assigns the "theInverse" property to the passed in matrix object
    ##  it doesn't calc the inverse
    setInverse <- function(inversedMatrixObj) 
    {
        theInverse <<- inversedMatrixObj
    }
 
    # just return the inverse
    getInverse <- function() 
    {
        theInverse
    }
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
    
}



## This function takes a specialMatrix object and assumes its invertible
##   -- check's if it's in a stored attribute of the specialObject
##   -- if so return the stored value and return early

cacheSolve <- function(x, ...) 
{
    ## Retrieve the "theInverse" value from the specialMatrix defined as "x"
    invMatrix <- x$getInverse()
    
    ## if we dont have a value yet, then we need to calc it
    if(!is.null(invMatrix)) 
    {
        message("Returning cached inversed-matrix object")
        return(invMatrix)  #leave the function early returning the inversedMatrix
    }

    ## --if we get here, we need to calc the inverse.  
    ## --so, lets just call solve() on the original specialMatrixObj which should be invertible
    tmpMatrix <- solve(x$get(), ...)
    
    #Set the cached value attribute on the passed in object for reuse later
    x$setInverse(tmpMatrix)
    
    #return the newly calculated inversedMatrix
    tmpMatrix
}

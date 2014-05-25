## makeCacheMatrix and cacheSolve alow you to cache already calculated data
## and retrieve it quickyl, if the data does not exist it will be calculated

## You pass a maxtrix to this function and it returns a list of functions 
## that you can use on it as well as storing cached info

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL # Reset inv to null
        set <- function(y) { ## set function sets passed matrix as active
            x <<- y
            inv <<- NULL ## set funtion also resets inv to null
        }
        get <- function() x #get function returns current matrix
        setInv <- function(compInv) inv <<- compInv ## set inv sets completed inverse
        getInv <- function() inv ## prints inverse that has already been 
                                ## calculated and saved
        list(set = set, get = get, setInv = setInv, getInv = getInv) 
        ## this creates a list of functions that can bel called
}

# This checks if the data already exists and if not will calculate it

cacheSolve <- function(x) {
    m <- x$getInv() # set m to existing value in function makeCacheMatrix
    if(!is.null(m)) { # check if  holds a value
        message("getting cached data")
        return(m) # return existing value
    }
    data <- x$get() # set data to matrix from makeCacheMatrix
    m <- solve(data) # set m to the inverse of specified matrix
    x$setInv(m) # save m to cache in makeCacheMatrix function
    m # return inverse matrix
    
}

## R Programming, Week 3 assignment
## Invert a matrix and cache the results
## Retreive cached result instead of
## recalculating the inverted matrix

## fuction to create and store cached matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL # set inverse to NULL
        set <- function(y) { # create the set function
                x <<- y 
                m <<- NULL
        }
        # create get and set functions to access the invererted matrix
        get <- function() x 
        setInv <- function(solve) m <<- solve
        getInv <- function() m
        
        list(set = set, get = get, # make a list of get and set functions
             setInv = setInv,
             getInv = getInv)
        
}

## fuction to check if inverse is cached, return new data or cached 

cacheSolve <- function(x, ...) { 
        ## return a matrix that is the inverse of x
        m <- x$getInv()
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        data <- x$get()
        m <- solve(data)
        x$setInv(m)
        m
}


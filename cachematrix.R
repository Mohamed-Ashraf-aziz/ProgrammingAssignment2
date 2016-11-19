## this function used to Compute the inverse of a square matri

## This function creates a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix())
  {
    m<<-NULL
    set <- function(y) 
     {
       x <<- y
       m <<- NULL
     }
    
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,setsolve = setsolve,getsolve = getsolve)
  }
## This function computes the inverse of the "matrix returned by makeCacheMatrix above

cacheSolve <- function(x, ...) 
    {
       m <- x$getsolve()
       if(!is.null(m)) 
        {
           message("getting cached data")
           return(m)
        }
       data <- x$get()
       m <- solve(data, ...)
       x$setsolve(m)
       m
    }

## This set of functions implement a caching mechanism of matrix inverse.

## Create a cached version of a matrix

makeCacheMatrix <- function(x = matrix()) {
  solved <- list()
  
  set <- function (y) {
    x <<- y
    solved <<- list()
  }
  
  get <- function () x
  
  ## solve() accepts a ... argument. We need to handle it by caching
  ## the result with the each item in following structure:
  ## list( list(arg1, arg2), 'result1' )
  setSolved <- function (s, ...) {
    params <- list(...)
    
    ## append the new key/value pair to the solved list.
    solved[[ length(solved)+1 ]] <<- list(params, s)
    
    solved
  }
  
  ## Iterate within the solved list to find the cached result.
  getSolved <- function(...) {
    params <- list(...)
    
    for (item in solved) {
      if (identical(params, item[[1]])) {
        return(item[[2]])
      }
    }
  }
  
  list(set = set, get = get, setSolved = setSolved, getSolved = getSolved)
}


## Return the inverse of a matrix from the cache. If the cache does not exist, it
## will compute the inverse first then cache the result.

cacheSolve <- function(x, ...) {
  s <- x$getSolved(...)
  
  if (!is.null(s)) {
    message('using cached inverse')
    return(s)
  }
  
  mat <- x$get()
  s <- solve(mat, ...)
  x$setSolved(s, ...)
  s
}


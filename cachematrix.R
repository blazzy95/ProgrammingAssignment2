## The "makeCacheMatrix" function creates a special array object.
## The "cacheSolve" function calculates the inverse of the array. 
## If the inverse of the array has already been computed, it is found 
## in the cache and returns it, recalculation is not needed.

## This function makes the cache data

makeCacheMatrix <- function(x = matrix()) {
inv_x <- NULL
set <- function(y) {
x <<- y
inv_x <<- NULL
}
get <- function() x
setInv <- function(inverse) inv_x <<- inverse
getInv <- function() inv_x
list(set = set,
get = get,
setInv = setInv ,
getiInv = getInv)

}


## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {

cacheSolve <- function(x, ...) {
inv_x <- x$getInv()
if(!is.null(inv_x)) {
message("Getting the cached data.")
return(inv_x)
}
data <- x$get()
inv_x <- solve(data)
x$setInv(inv_x)
inv_x
}

## Put comments here that give an overall description of what your
## functions do

## matrix able to store an inverse
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) i <<- inverse
        getInverse <- function() i
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## get the cached inverse of the matrix x, if inverse is not null then returns directly cached inverse
## else compute the inverse of the matrix x and set into the cache of matrix x
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data)
        x$setInverse(inverse)
        inverse
}


##tests
tested_matrix <- matrix (data = c(1, 3, 5, 2), nrow = 2, ncol = 2)
tested_matrix

expected_solved <- solve(tested_matrix)
expected_solved

tested_cache_matrix <- makeCacheMatrix(tested_matrix)
tested_cache_matrix$get()

oneTime <- cacheSolve(tested_cache_matrix)
oneTime
identical(expected_solved, oneTime)

tested_cache_matrix <- makeCacheMatrix(tested_matrix)
tested_cache_matrix$get()
for(i in 1:10){ten_times <- cacheSolve(tested_cache_matrix)}
ten_times
identical(expected_solved, ten_times)

## test results

# > tested_matrix <- matrix (data = c(1, 3, 5, 2), nrow = 2, ncol = 2)
# > tested_matrix
# [,1] [,2]
# [1,]    1    5
# [2,]    3    2
# > 
#         > expected_solved <- solve(tested_matrix)
# > expected_solved
# [,1]        [,2]
# [1,] -0.1538462  0.38461538
# [2,]  0.2307692 -0.07692308
# > 
#         > tested_cache_matrix <- makeCacheMatrix(tested_matrix)
# > tested_cache_matrix$get()
# [,1] [,2]
# [1,]    1    5
# [2,]    3    2
# > 
#         > oneTime <- cacheSolve(tested_cache_matrix)
# > oneTime
# [,1]        [,2]
# [1,] -0.1538462  0.38461538
# [2,]  0.2307692 -0.07692308
# > identical(expected_solved, oneTime)
# [1] TRUE
# > 
#         > tested_cache_matrix <- makeCacheMatrix(tested_matrix)
# > tested_cache_matrix$get()
# [,1] [,2]
# [1,]    1    5
# [2,]    3    2
# > for(i in 1:10){ten_times <- cacheSolve(tested_cache_matrix)}
# getting cached data
# getting cached data
# getting cached data
# getting cached data
# getting cached data
# getting cached data
# getting cached data
# getting cached data
# getting cached data
# > ten_times
# [,1]        [,2]
# [1,] -0.1538462  0.38461538
# [2,]  0.2307692 -0.07692308
# > identical(expected_solved, ten_times)
# [1] TRUE
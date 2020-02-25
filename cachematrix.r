#This function assigns the inverse of the matrix to Cache. 
# makeCacheMatrix function takes Argument x as matrix and get() function returns it.
# getInverse() functions returns the inverse matrix
# setInverse () function assign the inverse to variable m that will be available in different env function as well



makeCacheMatrix <- function(x = matrix()) {
        
        # Inverse is initially set to null
        m <- NULL
        
        # set value to variable x that will be available in other env as well
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

# caheSolve function 1st check if inverse matrix is available in cache and takes 
# into variable m. If m is null i.e. that means inverse matrix is  not cached.
#Inverse matrix is calculated and added to cache

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

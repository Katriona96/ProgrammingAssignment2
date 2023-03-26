## makeCacheMatrix creates an R object that stores an invertible square matrix 
## and it's inverse 

makeCacheMatrix <- function(x = matrix()) {
        #initialize inv as an object within the makeCacheMatrix() env.
        inv <- NULL 
        #set the value of the matrix 
        set <- function(y) {
                x <<- y
                #clear amy previously cached values of inv
                inv <<- NULL
        }
        #define the getter for the matrix
        get <- function() x
        #define the setter for the inverse
        setinv <- function(inverse) inv <<- inverse
        #define the getter for the inverse
        getinv <- function() inv
        #name the list to allow $ to extract functions by name
        list(set = set, #gives the name `set` to the set() function
             get = get, #gives the name `get` to the get() function
             setinv = setinv, #gives the name `setinv` to setinv() functiom
             getinv = getinv) #gives the name `getinv` to getinv() function
}


## cacheSolve is a function that computes the inverse of the special "matrix" 
## returned by makeCacheMatrix. It first checks to see if the inverse has 
## already been calculated. If so, it gets the matrix from the cache and 
## skips computation.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        #retrieve inverse from object passed through argument       
         inv <- x$getinv()
         #check to see if result is NULL
         #if not equal to null the cached matrix is valid
         if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
         }
         #if equal to null, cacheSolve gets the matrix from input object
         data <- x$get()
         #compute the inverse of the square matrix with solve()
         inv <- solve(data, ...)
         #uses the setinv() function to set the matrix in the input object 
         x$setinv(inv)
         #returns the value to the parent environment by printing the inverse
         inv
}

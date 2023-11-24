## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# Function to create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(mat = matrix()) {
  # Variable to store the matrix
  inv <- NULL
  
  # Function to set the matrix
  set <- function(matrix) {
    mat <<- matrix
    inv <<- NULL  # Update the cache when a new matrix is set
  }
  
  # Function to get the matrix
  get <- function() mat
  
  # Function to set the inverse in the cache
  setInverse <- function(inverse) inv <<- inverse
  
  # Function to get the inverse from the cache
  getInverse <- function() inv
  
  # List of functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Write a short comment describing this function

# Function to calculate the inverse of the matrix, caching if possible
cacheSolve <- function(cacheMatrix, ...) {
  # Retrieve the inverse from the cache
  inverse <- cacheMatrix$getInverse()
  
  # If the inverse is already calculated, retrieve it from the cache
  if (!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  # Otherwise, calculate the inverse using the solve function
  mat <- cacheMatrix$get()
  inverse <- solve(mat, ...)
  
  # Set the inverse in the cache
  cacheMatrix$setInverse(inverse)
  
  # Return the calculated inverse
  inverse
}




# Example usage:
# Create a special "matrix" object
myMatrix <- makeCacheMatrix(matrix(c(1, 2, 3, 4), nrow = 2))

# Calculate the inverse of the matrix, caching if possible
cacheSolve(myMatrix)



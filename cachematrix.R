# General objective: Create a structure for caching a inverse matrix calculation
#  makeCacheMatrix(x) creates a special environment for a given matrix. 
#  cacheSolve(x, ...) accepts a makeCacheMatrix and further input for the solve() function. 
#    Will only calculate inverse matrix if no previous result is known.
#  testCacheSolve() can be used to test the previous functions

# Simple test function for cacheSolve
#   Returns true on success, errors otherwise
testCacheSolve <- function() {
	# Matrix to use for inverse testing
	m <- matrix(1:4, nrow=2)
	# Identity matrix for inverse testing
	id_m <- diag(nrow(m))

	# The test object
	m_cache <- makeCacheMatrix(m)

	# Test if get function works
	stopifnot(all(m == m_cache$get()))

	# Test start condition
	stopifnot(is.null(m_cache$getinverse()))

	# If the matrix * inverse does not match the identity matrix somethings wrong
	inverse1 <- cacheSolve(m_cache)
	stopifnot(all(m %*% inverse1 == id_m))

	# Test if get function works
	stopifnot(all(m == m_cache$get()))

	# Test start condition
	stopifnot(! is.null(m_cache$getinverse()))
	# Check if the cached value is the correct value
	stopifnot(all(m_cache$getinverse() == solve(m)))

	# If the matrix * inversie does not match the identity matrix somethings wrong
	inverse2 <- cacheSolve(m_cache)
	stopifnot(all(m %*% inverse2 == id_m))

	# Make sure we didn't break anything

	# Test start condition
	stopifnot(! is.null(m_cache$getinverse()))
	# Check if the cached value is the correct value
	stopifnot(all(m_cache$getinverse() == solve(m)))

	# Success!

	TRUE
}

# Build a environment for caching the inverse matrix of a given matrix to minimize computation
#  x should be a square matrix
makeCacheMatrix <- function(x = matrix()) {
	# Create variable for caching
        m <- NULL

	# Function to update base matrix
	#   On first call x will already set. Only called on reuse of structure.
        set <- function(y) {
		# Set base matrix in parent environment
                x <<- y
		# Reset now invalid cache
                setinverse(NULL)
        }

	# Get base matrix
        get <- function() x
	
	# Set cached inverse in parent environment
        setinverse <- function(inverse) m <<- inverse

	# Set cached inverse
        getinverse <- function() m

	# Return value
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


# Calculate the inverse matrix for a given square matrix using caching
#   x is a list returned by makeCacheMatrix containing getinverse, setinverse and get functions
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	
	# Try cached value first
        m <- x$getinverse()
        if(!is.null(m)) {
		# Cache hit
                return(m)
        }

	# On cache miss calculate and update cache
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
	
	# Return value
        m
}

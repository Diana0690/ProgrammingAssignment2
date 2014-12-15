# script created by Diana van der Plaat
# The first function (makeCacheMatrix) makes a matrix that can cache its inverse.
# The second function (cacheSolve) computes the inverse of the matrix created by the first function (makeCacheMatrix)

# Input x will be a matrix
# m will be the matrix and it's is reset to NULL every time
# get returns the value of the original vector
# setmatrix stores the matrix created
# getmatrix will retun the cached matrix
# list is a list of the internal functions, so a calling function knows how to access these functions

makeCacheMatrix <- function(x = matrix()) {      
        m <- NULL    
        get <- function() { x }   
        setmatrix <- function(matrix)  { m <<- matrix }
        getmatrix <- function() { m }  
        list(get = get,               
             setmatrix = setmatrix,   
             getmatrix = getmatrix)            
}


# Input x is a matrix created by makeCacheMatrix
# m accesses and gets the matrix 'x'
# if the matrix was already cached (not NULL), the message "getting cached data" is printed and the matrix is returned.
# data is only used if NULL was returned 
# if m was NULL, an inverse (solve function) of the matrix is returned. 
# x$setmatrix stores the inverse matrix in x
# m returns (prints) the inverse matrix

cacheSolve <- function(x, ...) {
        m <- x$getmatrix()               
        if(!is.null(m)) {              
                message("getting cached data")  
                return(m)                        
        }
        data <- x$get()        
        m <- solve(data, ...)  
        x$setmatrix(m)          
        m              
}

## The funcitions creates a matrix object that can cache the inverse of a matirx
## which is usefulle in the case that ones need the same inverted matrix for subsequent calculations
## Lexical scoping is used to retrieve values from objects within the functions 

## The fucntion makeCacheMatrix creates a matrix object that can cache the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL # the two objects x and m are initialized
  # the "setter" for x and m assigns an input argument to the object x and the value NULL to m
  set <- function(y){ 
    x <<- y 
    m <<- NULL
  }
  get <- function() x # the "getter" for x retrieve/access the data in x
  setinv <- function(inv) m <<- inv # the "setter" for m assigns the input argument to the value of m
  getinv <- function() m # the "getter" for m retrieve/access the data in x
  list(set = set, get = get, 
       setinv = setinv, # each of the functions are assigned to a list
       getinv = getinv)
  } 
    

## This is a test
A <- matrix(c(1,5,9,13), nrow = 2)
B <- matrix(c(1,5,9,13,17,2,6,10,14,18,3,7,11,15,19,4), nrow = 4)
aMatrix <- makeCacheMatrix(B)
aMatrix$get()     # retrive the matrix x
aMatrix$getinv()  # retrive the value of m, which should be nul
aMatrix$set(A)  # reset value with a new matrix

## The function cacheSolve completes the funtion  makeCacheMatrix since it calculates the 
## the inverse of a matrix if it has not already been calculated 

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)){ #if there is a valid inverse matrix it can be return form the parant environment
          message("getting cached data") 
          return(m) 
  }
  ## Return a matrix that is the inverse of 'x'
  data <- x$get() 
  m <- solve(data, ...) # solve calculates the invers of a matrix
  x$setinv(m)
  m
}

## This is a test
cacheSolve(aMatrix) 
aMatrix$getinv()
cacheSolve(aMatrix) 

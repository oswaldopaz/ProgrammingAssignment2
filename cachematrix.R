

# My first R assignment, week 3, Coursera


## NOTE TO MY FELLOW MARKER: Dear Colleague. I struggled quite a bit with this assignment as I'm a beginner wih R.
## At the end I understood that the purpose was to get familiar with lexical scoping and the "environmnet" concept.
## In case you didn't do it, I referred myself to the document "Demystifying makeVector()" by Mr Len Greski (very enlightening).
## It was also helpfull the book "Advanced R" by Hadley Wickham.

## I tested my code and it runs as expected, I'm not sure if we'll be asked to run other people's code during the review
## but its worth noticing that for the cacheSolve function to run you need a makeCacheMatrix type object, which is actually
## a list of functions that are called and used by cacheSolve. The object makeCacheMatrix will also contain the variables
## "x" and "m" in the global environment, which is one of the "tricks" for the whole thing to work. I will try to comment
## as much as possible, whcih is, I beleive, the point of this excercise.
## Please also notice that the matrix should be invertible because the script doesn't check for that.

## About makeCacheMatrix function: It returns a list of functions and variables, its funtion is to initialize the variables
## when it first runsand cache teh result for every instance, in other to avoid unnecessary iterations if the same operation
## is called more than once ("m" variable would be the marker for that).

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {      ## | This section intializes teh variables and functions
    x <<- y                 ## > Also ensures that variables are set and reachable in Global Envir. using "<<-"
    m <<- NULL              ## |
  }
  get <- function() x
  setmatriz <- function(matriz) m <<- matriz    ## |"setters" and "getters" are contained here
  getmatriz <- function() m                     ## |
  list(set = set, get = get,  ## |
       setmatriz = setmatriz, ## > This part returns list objects(functions), reachable and called upon by cacheSolve function.
       getmatriz = getmatriz) ## | This beacomes useful to make calls by names using "$" operator
}
  

## About cacheSolve function: This function will perform the actual inversiion function, after checking  through "m"
## variable if the operation had not been cached previously. If so, it will retrieve it from existing data and display
## the "getting cached data" message. For this to happen, one needs to run the function at least twice with the same
## object, which should be a makeCacheMatrix type. I ran it with a couple of square (invertible) matrixs and it worked,
## once I had run it with makeCacheMatrix. 

## It should become apparent at this point that both functions are related and will NOT work independently. Part
## of the assignment is to work with lexical scoping and understand how to make all necessary functions and variables
## reachable between both function, by "forcing" to have one common Global Environ.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getmatriz()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatriz(m)
  m
  
}

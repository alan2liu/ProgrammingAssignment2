

## create a function which contains a function list and a matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL    #set inverse to NULL
    set<-function(y){  #this function can change arguement X with code like makeCacheMatrix$set(maxtrix(c(1,2,5,2),2,2))
        x<<-y          #using <<- to define x in parent environment
        inverse<<-NULL
    }
    get <- function() x # set x to get
    setverse <- function(inverse) inverse <<- inverse # inverse x to setverse
    getverse <- function() inverse  # inverse x to getverse
    list(set = set, get = get,
         setverse = setverse,
         getverse = getverse)    
  
}

# a test for makeCacheMatrix and see what it contains 
try<-makeCacheMatrix(x=matrix(rnorm(16,0,1),4,4))
try


## create a function that can call function above

cacheSolve <- function(x, ...) {
    inverse <- x$getverse() #get inverse in makeCacheMatrix
    if(!is.null(inverse)) { #judge if inverse is null or not
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()         #get matrix used in makeCacheMatrix
    inverse <- solve(data, ...) #get invered matrix of invered above
    x$setverse(inverse) 
    inverse
}

#test for function cacheSolve
cacheSolve(x=try)


##generates a special matrix object from a matrix passed to makeCacheMatrix and can cache it's inverse.
##cacheSolve returns the cached inverse from makeCacheMatrix or computes the inverse of the matrix passed to makeCacheMatrix

##makeCacheMatrix accepts a matrix argument, generates a matrix object, caches the inverted matrix when possible.

makeCacheMatrix <- function(x = matrix()){
        invertedmatrix <- NULL
        set <- function(y){
                x <<- y
                invertedmatrix <<- NULL
        }
        get <- function() x
        setinversion <- function(inversion) invertedmatrix <<- inversion
        getinversion <- function() invertedmatrix
        list(set = set, get = get, setinversion = setinversion, getinversion = getinversion)
        
}

##accepts the previously generated special matrix object as an arguement; returns previously cached inverted matrix or computes matrix inverse.
cacheSolve <- function(x, ...){
        ## Return a matrix that is the inverse of 'x'
        invertedmatrix <- x$getinversion()
        if (!is.null(invertedmatrix)){
                return(invertedmatrix)
        }
        data <- x$get()
        invertedmatrix <- solve(data, ...)
        x$setinversion(invertedmatrix)
        invertedmatrix
}
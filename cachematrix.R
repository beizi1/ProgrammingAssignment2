#Emmanuel Asare Nti
#week 3 assignment 2

makeCacheMatrix <- function(x=matrix()){
        #invers - inverse matrix value
        invers=NULL               #default value - NULL
        
        #go function: matrix and reset invers
        go <- function(y){
                x<<-y
                invers<<-NULL
        }
        #triv function: get matrix
        triv <- function(){x}
        
        #goInvers function: set inverse matrix
        goInverse <- function(inverse){invers <<- inverse}
        
        #trivInvers function: get inverse matrix
        trivInverse <- function(){invers}
        list(go=go,triv=triv,goInverse=goInverse, trivInverse=trivInverse)
}



## Description: This function computes the inverse of the
## 				special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...){
        #get invers Matrix
        invers <- x$trivInverse()
        
        #check if it is not null
        if(!is.null(invers)){
                message("Return cache data")
                return(invers)
        }
        
        ## Return a matrix that is the inverse of 'x'
        #if it is null, calculate the inverse
        mat <- x$triv()
        invers <- solve(mat, ...)
        x$goInverse(invers)
        print(invers)
}

matcache <- makeCacheMatrix(matrix(1:4,nrow =2,ncol = 2))
matcache$triv()
matcache$trivInverse()
cacheSolve(matcache)
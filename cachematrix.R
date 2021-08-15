#Emmanuel Asare Nti
#week 3 assignment 2

makeCacheMatrix <- function(x=matrix()){
        invers=NULL
        go <- function(y){
                x<<-y
                invers<<-NULL
        }
        triv <- function(){x}
        goInverse <- function(inverse){invers <<- inverse}
        trivInverse <- function(){invers}
        list(go=go,triv=triv,goInverse=goInverse, trivInverse=trivInverse)
}

cacheSolve <- function(x, ...){
        invers <- x$trivInverse()
        if(!is.null(invers)){
                message("Return cache data")
                return(invers)
        }
        mat <- x$triv()
        invers <- solve(mat, ...)
        x$goInverse(invers)
        print(invers)
}

matcache <- makeCacheMatrix(matrix(1:4,nrow =2,ncol = 2))
matcache$triv()
matcache$trivInverse()
cacheSolve(matcache)
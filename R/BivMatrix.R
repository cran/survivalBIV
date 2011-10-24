BivMatrix.CKM <- function(object, grid.x, grid.y) {
	z <- matrix( data=0, nrow=length(grid.x), ncol=length(grid.y) )
	with( object$data, .C("BivMatrixCKM", time1, event1, time2, event2, as.integer( nrow(object$data) ), grid.x, grid.y, as.integer( length(grid.x) ), as.integer( length(grid.y) ), z, DUP=FALSE, PACKAGE="survivalBIV") )
	return(z)
}

BivMatrix.IPCW1 <- function(object, grid.x, grid.y) {
	z <- matrix( data=0, nrow=length(grid.x), ncol=length(grid.y) )
	with( object$data, .C("BivMatrixIPCW1", time1, event1, time2, event2, Stime, as.integer( nrow(object$data) ), grid.x, grid.y, as.integer( length(grid.x) ), as.integer( length(grid.y) ), z, DUP=FALSE, PACKAGE="survivalBIV") )
	return(z)
}

BivMatrix.IPCW2 <- function(object, grid.x, grid.y) {
	z <- matrix( data=0, nrow=length(grid.x), ncol=length(grid.y) )
	with( object$data, .C("BivMatrixIPCW2", time1, surv, time2, Stime, survS, as.integer( nrow(object$data) ), grid.x, grid.y, as.integer( length(grid.x) ), as.integer( length(grid.y) ), z, DUP=FALSE, PACKAGE="survivalBIV") )
	return(z)
}

BivMatrix.KMPW <- function(object, grid.x, grid.y) {
	z <- matrix( data=0, nrow=length(grid.x), ncol=length(grid.y) )
	with( object$data, .C("BivMatrixKMPW", time1, time2, m, as.integer( nrow(object$data) ), grid.x, grid.y, as.integer( length(grid.x) ), as.integer( length(grid.y) ), z, DUP=FALSE, PACKAGE="survivalBIV") )
	return(z)
}

BivMatrix.KMW <- function(object, grid.x, grid.y) {
	z <- matrix( data=0, nrow=length(grid.x), ncol=length(grid.y) )
	with( object$data, .C("BivMatrixKMW", time1, time2, event2, as.integer( nrow(object$data) ), grid.x, grid.y, as.integer( length(grid.x) ), as.integer( length(grid.y) ), z, DUP=FALSE, PACKAGE="survivalBIV") )
	return(z)
}

BivMatrix.default <- function(object, grid.x, grid.y) {
	z <- matrix( data=0, nrow=length(grid.x), ncol=length(grid.y) )
	for ( i in 1:length(grid.x) ) {
		for( j in 1:length(grid.y) ) {
			z[i,j] <- BivDist(object, t1=grid.x[i], t2=grid.y[j])
		}
	}
	return(z)
}

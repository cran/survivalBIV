BivMarginal.CKM <- function(object, times, n) {
	probs <- vector(mode="double", length=n)
	with( object$data, .C("BivMarginalCKM", time1, event1, time2, event2, as.integer( nrow(object$data) ), times, probs, n, DUP=FALSE, PACKAGE="survivalBIV") )
	return(probs)
}

BivMarginal.IPCW1 <- function(object, times, n) {
	probs <- vector(mode="double", length=n)
	with( object$data, .C("BivMarginalIPCW1", time1, event1, time2, event2, Stime, as.integer( nrow(object$data) ), times, probs, n, DUP=FALSE, PACKAGE="survivalBIV") )
	return(probs)
}

BivMarginal.IPCW2 <- function(object, times, n) {
	probs <- vector(mode="double", length=n)
	with( object$data, .C("BivMarginalIPCW2", time1, surv, time2, Stime, survS, as.integer( nrow(object$data) ), times, probs, n, DUP=FALSE, PACKAGE="survivalBIV") )
	return(probs)
}

BivMarginal.KMPW <- function(object, times, n) {
	probs <- vector(mode="double", length=n)
	with( object$data, .C("BivMarginalKMPW", time2, m, as.integer( nrow(object$data) ), times, probs, n, DUP=FALSE, PACKAGE="survivalBIV") )
	return(probs)
}

BivMarginal.KMW <- function(object, times, n) {
	probs <- vector(mode="double", length=n)
	with( object$data, .C("BivMarginalKMW", time2, event2, as.integer( nrow(object$data) ), times, probs, n, DUP=FALSE, PACKAGE="survivalBIV") )
	return(probs)
}

BivMarginal.default <- function(object, times, n) {
	probs <- vector(mode="double", length=n)
	t1 <- max(object$data$time1)
	for (i in 1:n) {
		probs[i] <- BivDist(object, t1=t1, t2=times[i])
	}
	return(probs)
}

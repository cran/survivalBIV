bivIPCW <- function(object, t1, t2, conf=FALSE, n.boot=1000, conf.level=0.95, method.boot="percentile", method.cens="KM") {
	if ( missing(object) ) stop("Argument 'object' is missing, with no default")
	if ( missing(t1) ) t1 <- 0
	if (t1 == Inf) t1 <- max(object$data$time1)
	if ( missing(t2) ) t2 <- max(object$data$time2)
	Message <- BivCheck(object, t1, t2, conf, n.boot, conf.level, method.boot)
	if ( !is.null(Message) ) stop(Message)
	rm(Message)
	n.boot <- as.integer(n.boot)
	if ( !( method.cens %in% c("KM", "prodlim") ) ) stop("Argument 'method.cens' must be one of 'KM' or 'prodlim'")
	if (method.cens == "prodlim") new.class <- "IPCW2"
	else new.class <- "IPCW1"
	return( BivMethod(object, t1, t2, conf, n.boot, conf.level, method.boot, new.class) )
}

Biv.IPCW1 <- function(object) {
	x <- with( object, list("data"=data) )
	class(x) <- c("IPCW", "IPCW1")
	return(x)
}

Biv.IPCW2 <- function(object) {
	require(prodlim)
	surv <- as.double( with( object$data, predict(prodlim(Hist(time1, event1)~1, reverse=TRUE, conf.int=FALSE), times=time1, type="surv") ) )
	surv[is.na(surv)] <- 0
	survS <- as.double( with( object$data, predict(prodlim(Hist(Stime, event2)~1, reverse=TRUE, conf.int=FALSE), times=Stime, type="surv") ) )
	survS[is.na(survS)] <- 0
	x <- with( object$data, list( "data"=data.frame("time1"=time1, "surv"=surv, "time2"=time2, "Stime"=Stime, "survS"=survS) ) )
	class(x) <- c("IPCW", "IPCW2")
	return(x)
}

BivSort.IPCW1 <- function(object) {
	with( object$data, .C("BivSortIPCW1", time1, event1, time2, event2, Stime, as.integer( nrow(object$data) ), DUP=FALSE, PACKAGE="survivalBIV") )
}

BivSort.IPCW2 <- function(object) {
	with( object$data, .C("BivSortIPCW2", time1, time2, Stime, as.integer( nrow(object$data) ), DUP=FALSE, PACKAGE="survivalBIV") )
}

BivDist.IPCW1 <- function(object, t1, t2) {
	return( with(object$data, .C("BivDistIPCW1", time1, event1, time2, event2, Stime, as.integer( nrow(object$data) ), as.double(t1), as.double(t2), p = as.double(0), DUP=FALSE, PACKAGE="survivalBIV")$p) )
}

BivDist.IPCW2 <- function(object, t1, t2) {
	return( with(object$data, .C("BivDistIPCW2", time1, surv, time2, Stime, survS, as.integer( nrow(object$data) ), as.double(t1), as.double(t2), p = as.double(0), DUP=FALSE, PACKAGE="survivalBIV")$p) )
}

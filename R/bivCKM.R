bivCKM <- function(object, t1, t2, conf=FALSE, n.boot=1000, conf.level=0.95, method.boot="percentile") {
	if ( missing(object) ) stop("Argument 'object' is missing, with no default")
	if ( missing(t1) ) t1 <- 0
	if (t1 == Inf) t1 <- max(object$data$time1)
	if ( missing(t2) ) t2 <- max(object$data$time2)
	Message <- BivCheck(object, t1, t2, conf, n.boot, conf.level, method.boot)
	if ( !is.null(Message) ) stop(Message)
	rm(Message)
	n.boot <- as.integer(n.boot)
	return( BivMethod(object, t1, t2, conf, n.boot, conf.level, method.boot, "CKM") )
}

Biv.CKM <- function(object) {
	x <- with( object, list("data"=data[,c("time1", "event1", "time2", "event2")]) )
	class(x) <- "CKM"
	return(x)
}

BivSort.CKM <- function(object) {
	with( object$data, .C("BivSortCKM", time1, event1, time2, event2, as.integer( nrow(object$data) ), DUP=FALSE, PACKAGE="survivalBIV") )
}

BivDist.CKM<- function(object, t1, t2) {
	return( with(object$data, .C("BivDistCKM", time1, event1, time2, event2, as.integer( nrow(object$data) ), as.double(t1), as.double(t2), p = as.double(1), DUP=FALSE, PACKAGE="survivalBIV")$p) )
}

bivKMPW <- function(object, t1, t2, conf=FALSE, n.boot=1000, conf.level=0.95, method.boot="percentile") {
	if ( missing(object) ) stop("Argument 'object' is missing, with no default")
	if ( missing(t1) ) t1 <- 0
	if (t1 == Inf) t1 <- max(object$data$time1)
	if ( missing(t2) ) t2 <- max(object$data$time2)
	Message <- BivCheck(object, t1, t2, conf, n.boot, conf.level, method.boot)
	if ( !is.null(Message) ) stop(Message)
	rm(Message)
	n.boot <- as.integer(n.boot)
	return( BivMethod(object, t1, t2, conf, n.boot, conf.level, method.boot, "KMPW") )
}

Biv.KMPW <- function(object) {
	x <- with( object, list( "data"=cbind( data, "m"=double( nrow(data) ) ) ) )
	class(x) <- "KMPW"
	return(x)
}

BivLogit <- function(object) {
	require(stats)
	m <- rep( 0, nrow(object$data) )
	w <- which(object$data$event1 == 1)
	m[w] <- predict.glm( glm(formula = event2~time1+time2, family = binomial(link = "logit"), data=object$data[w,]), type="response" )
	.C("doubleCopy", m, object$data$m, as.integer( nrow(object$data) ), DUP=FALSE)
}

BivSort.KMPW <- function(object) {
	BivLogit(object)
	with( object$data, .C("BivSortKMPW", time1, time2, m, Stime, as.integer( nrow(object$data) ), DUP=FALSE) )
}

BivDist.KMPW <- function(object, t1, t2) {
	return( with(object$data, .C("BivDistKMPW", time1, time2, m, as.integer( nrow(object$data) ), as.double(t1), as.double(t2), p = as.double(1), DUP=FALSE)$p) )
}

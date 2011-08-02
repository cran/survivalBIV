plot.survBIV <- function(x, plot.marginal=FALSE, plot.bivariate=FALSE, plot.persp=FALSE, plot.contour=FALSE, method, method.cens="KM", xlab, ylab, zlab, main=method, col, grid.x=NULL, grid.y=NULL, col.biv=FALSE, xlim=NULL, ylim=NULL, ...) {
	if ( missing(x) ) stop("Argument 'x' is missing, with no default")
	if ( missing(method) ) stop("Argument 'method' is missing, with no default")
	if ( !is.survBIV(x) ) stop("Argument 'x' must be of class 'survBIV'")
	if ( !is.logical(plot.marginal) ) stop("Argument 'plot.marginal' must be logical")
	if ( !is.logical(plot.bivariate) ) stop("Argument 'plot.bivariate' must be logical")
	if ( !is.logical(plot.persp) ) stop("Argument 'plot.persp' must be logical")
	if ( !is.logical(plot.contour) ) stop("Argument 'plot.contour' must be logical")
	if (!plot.marginal & !plot.bivariate) plot.bivariate <- TRUE
	if ( missing(xlab) ) xlab.aux <- TRUE
	else xlab.aux <- FALSE
	if ( missing(ylab) ) ylab.aux <- TRUE
	else ylab.aux <- FALSE
	if ( !( method %in% c("CKM", "IPCW", "KMPW", "KMW") ) ) stop("Argument 'method' must be one of 'CKM', 'IPCW', 'KMPW' or 'KMW'")
	if (method == "IPCW") {
		if ( !( method.cens %in% c("KM", "prodlim") ) ) stop("Argument 'method.cens' must be one of 'KM' or 'prodlim'")
		if (method.cens == "prodlim") new.class <- "IPCW2"
		else new.class <- "IPCW1"
	}
	object <- switch( method, "CKM"=Biv.CKM(x), "IPCW"=switch( new.class, "IPCW1"=Biv.IPCW1(x), "IPCW2"=Biv.IPCW2(x) ), "KMPW"=Biv.KMPW(x), "KMW"=Biv.KMW(x) )
	BivSort(object)
	if (plot.marginal) {
		if (xlab.aux == TRUE) xlab <- "Time"
		if (ylab.aux == TRUE) ylab <- "Estimated marginal dist. F2(t)"
		len <- as.integer( nrow(object$data) )
		if ( inherits(object, "IPCW") ) {
			times <- with( object$data, c( outer(X=unique(time1), Y=unique(Stime[event2 == 0]), FUN="-") ) )
			times <- as.double( unique( sort(times[times >= 0]) ) )
			j <- as.integer( length(times) )
			probs <- vector(mode="double", length=j)
		} else {
			times <- vector(mode="double", length=len)
			probs <- vector(mode="double", length=len)
			j <- as.integer(0)
		}
		BivMarginal(object, len, times, probs, j)
		oask <- devAskNewPage(TRUE)
		on.exit( devAskNewPage(oask) )
		matplot(times[1:j], probs[1:j], xlab=xlab, ylab=ylab, type="s", main=main, xlim=xlim, ylim=ylim, ...)
		rm(len, times, probs, j)
		oask <- devAskNewPage(TRUE)
		on.exit( devAskNewPage(oask) )
	}
	if (plot.bivariate) {
		if (!plot.persp & !plot.contour) {
			plot.persp <- TRUE
			plot.contour <- TRUE
		}
		if (xlab.aux == TRUE) xlab <- "Time in state 1"
		if (ylab.aux == TRUE) ylab <- "Time in state 2"
		if ( missing(zlab) ) zlab <- "F(t1,t2)"
		if ( missing(col) ) col <- "lightblue"
		if ( is.null(grid.x) ) grid.x <- unique( sort( c(0, object$data$time1) ) )
		if ( is.null(grid.y) ) {
			if ( inherits(object, "IPCW") ) {
				grid.y <- with( object$data, c( outer(X=grid.x, Y=unique(Stime[event2 == 0]), FUN="-") ) )
				grid.y <- unique( sort(grid.y[grid.y >= 0]) )
			} else grid.y <- unique( sort( c(0, object$data$time2) ) )
		}
		z <- matrix( data=0, nrow=length(grid.x), ncol=length(grid.y) )
		for ( i in 1:length(grid.x) ) {
			for( j in 1:length(grid.y) ) {
				z[i,j] <- BivDist(object, t1=grid.x[i], t2=grid.y[j])
			}
		}
		oask <- devAskNewPage(TRUE)
		on.exit( devAskNewPage(oask) )
		if (plot.persp) {
			persp(x=grid.x, y=grid.y, z=z, theta=30, phi=30, ticktype="detailed", expand=0.5, xlab=xlab, ylab=ylab, zlab=zlab, col=col, shade=0.2, main=main)
			oask <- devAskNewPage(TRUE)
			on.exit( devAskNewPage(oask) )
		}
		if (plot.contour) {
			if(col.biv == FALSE) {
				bw <- colours()[358-3*0:25]
				filled.contour(x=grid.x, y=grid.y, z=z, xlab=xlab, ylab=ylab, main=main, col=bw, ...)
			} else {
				filled.contour(x=grid.x, y=grid.y, z=z, xlab=xlab, ylab=ylab, main=main, ...)
			}
			oask <- devAskNewPage(TRUE)
			on.exit( devAskNewPage(oask) )
		}
	}
}

BivMarginal.CKM <- function(object, len, times, probs, j) {
	with( object$data, .C("BivMarginalCKM", time1, event1, time2, event2, len, times, probs, j, DUP=FALSE) )
}

BivMarginal.IPCW1 <- function(object, len, times, probs, j) {
	with( object$data, .C("BivMarginalIPCW1", time1, event1, time2, event2, Stime, len, times, probs, j, DUP=FALSE) )
}

BivMarginal.IPCW2 <- function(object, len, times, probs, j) {
	require(prodlim)
	surv <- as.double( with( object$data, predict(prodlim(Hist(time1, event1)~1, reverse=TRUE), times=time1) ) )
	surv[is.na(surv)] <- 0
	survS <- as.double( with( object$data, predict(prodlim(Hist(Stime, event2)~1, reverse=TRUE), times=Stime) ) )
	survS[is.na(survS)] <- 0
	with( object$data, .C("BivMarginalIPCW2", time1, surv, time2, Stime, survS, len, times, probs, j, DUP=FALSE) )
}

BivMarginal.KMPW <- function(object, len, times, probs, j) {
	with( object$data, .C("BivMarginalKMPW", time2, m, len, times, probs, j, DUP=FALSE) )
}

BivMarginal.KMW <- function(object, len, times, probs, j) {
	with( object$data, .C("BivMarginalKMW", time2, event2, len, times, probs, j, DUP=FALSE) )
}

BivBoot <- function(object, t1, t2, n.boot, conf.level, method.boot, new.class) {
	vec <- vector( mode="numeric", length=nrow(object$data) )
	for (i in 2:n.boot) {
		x <- with( object, list("data"=data[sample.int(n=nrow(data), replace=TRUE),]) )
		class(x) <- new.class
		x <- Biv(x)
		BivSort(x)
		vec[i] <- BivDist(x, t1, t2)
	}
	x <- switch( new.class, "CKM"=Biv.CKM(object), "IPCW1"=Biv.IPCW1(object), "IPCW2"=Biv.IPCW2(object), "KMPW"=Biv.KMPW(object), "KMW"=Biv.KMW(object) )
	BivSort(x)
	vec[1] <- BivDist(x, t1, t2)
	band <- quantile( vec, probs=c( (1-conf.level)/2, (1+conf.level)/2 ) )
	if (method.boot == "basic") {
		band <- 2*vec[1]-rev(band)
		names(band) <- rev( names(band) )
	}
	return( c(vec[1], band) )
}

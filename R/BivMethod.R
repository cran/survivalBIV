BivMethod <- function(object, t1, t2, conf, n.boot, conf.level, method.boot, new.class) {
	if (conf) return( BivBoot(object, t1, t2, n.boot, conf.level, method.boot, new.class) )
	x <- switch( new.class, "CKM"=Biv.CKM(object), "IPCW1"=Biv.IPCW1(object), "IPCW2"=Biv.IPCW2(object), "KMPW"=Biv.KMPW(object), "KMW"=Biv.KMW(object) )
	BivSort(x)
	vec <- BivDist(x, t1, t2)
	names(vec) <- ""
	return(vec)
}

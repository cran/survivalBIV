summary.survBIV <- function(object, t1, t2, conf=FALSE, n.boot=1000, conf.level=0.95, method.boot="percentile", method.cens="KM", method="all", ...) {
	if ( missing(object) ) stop("Argument 'object' is missing, with no default")
	if ( missing(t1) ) t1 <- 0
	if (t1 == Inf) t1 <- max(object$data$time1)
	if ( missing(t2) ) t2 <- max(object$data$time2)
	Message <- BivCheck(object, t1, t2, conf, n.boot, conf.level, method.boot)
	if ( !is.null(Message) ) stop(Message)
	rm(Message)
	if ( !( method %in% c("CKM", "IPCW", "KMPW", "KMW", "all") ) ) stop("Argument 'method' must be one of 'CKM', 'IPCW', 'KMPW', 'KMW' or 'all'")
	n.boot <- as.integer(n.boot)
	if (method == "all" | method == "IPCW") {
		if ( !( method.cens %in% c("KM", "prodlim") ) ) stop("Argument 'method.cens' must be one of 'KM' or 'prodlim'")
		if (method.cens == "prodlim") new.class <- "IPCW2"
		else new.class <- "IPCW1"
		IPCW <- BivMethod(object, t1, t2, conf, n.boot, conf.level, method.boot, new.class)
	}
	if (method == "all" | method == "CKM") {
		CKM <- BivMethod(object, t1, t2, conf, n.boot, conf.level, method.boot, "CKM")
	}
	if (method == "all" | method == "KMPW") {
		KMPW <- BivMethod(object, t1, t2, conf, n.boot, conf.level, method.boot, "KMPW")
	}
	if (method == "all" | method == "KMW") {
		KMW <- BivMethod(object, t1, t2, conf, n.boot, conf.level, method.boot, "KMW")
	}
	if (method == "CKM") {
		cat("F(", t1, ",", t2, ")=\n")
		return(CKM)
	}
	if (method == "IPCW") {
		cat("F(", t1, ",", t2, ")=\n")
		return(IPCW)
	}
	if (method == "KMPW") {
		cat("F(", t1, ",", t2, ")=\n")
		return(KMPW)
	}
	if (method == "KMW") {
		cat("F(", t1, ",", t2, ")=\n")
		return(KMW)
	}
	if (method == "all") {
		cat("F(", t1, ",", t2, ")=\n")
		return( list("CKM"=CKM, "IPCW"=IPCW, "KMPW"=KMPW, "KMW"=KMW) )
	}
}

BivCheck <- function(object, t1, t2, conf, n.boot, conf.level, method.boot) {
	if ( !is.survBIV(object) ) return("Argument 'object' must be of class 'survBIV'")
	if ( !is.numeric(t1) ) return("Argument 't1' is not numeric")
	if ( !is.numeric(t2) ) return("Argument 't2' is not numeric")
	if (t1 < 0 | t2 < 0) return("Arguments 't1' and 't2' must both be greater or equal than 0")
	if ( !is.logical(conf) ) return("Argument 'conf' must be logical")
	if ( !is.numeric(n.boot) ) return("Argument 'n.boot' is not numeric")
	if (n.boot <= 1) return("Argument 'n.boot' must be greater than 1")
	if ( !is.numeric(conf.level) ) return("Argument 'conf.level' is not numeric")
	if (conf.level < 0 | conf.level > 1) return("Argument 'conf.level' must be between 0 and 1")
	if ( !( method.boot %in% c("percentile", "basic") ) ) return("Argument 'method.boot' must be one of 'percentile' or 'basic'")
	return(NULL)
}

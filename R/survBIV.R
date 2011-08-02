survBIV <- function(time1, event1, time2, event2) {
	if ( missing(time1) ) stop("Argument 'time1' is missing, with no default")
	if ( missing(event1) ) stop("Argument 'event1' is missing, with no default")
	if ( missing(time2) ) stop("Argument 'time2' is missing, with no default")
	if ( missing(event2) ) stop("Argument 'event2' is missing, with no default")
	if ( !is.numeric(time1) ) stop("Argument 'time1' is not numeric")
	if ( !( is.logical(event1) | is.numeric(event1) ) ) stop("Argument event1 must be logical or numeric")
	if ( !is.numeric(time2) ) stop("Argument 'time2' is not numeric")
	if ( !( is.logical(event2) | is.numeric(event2) ) ) stop("Argument event2 must be logical or numeric")
	length <- length(time1)
	if ( length != length(event1) | length != length(time2) | length != length(event2) ) stop("Arguments 'time1', 'event1', 'time2' and 'event2' must have the same length")
	if ( any( (event1 != 0 & event1 != 1) | (event1 != FALSE & event1 != TRUE) ) ) stop("Argument 'event1' must be 0 or 1 if numeric and TRUE or FALSE if logical")
	if ( any( (event2 != 0 & event2 != 1) | (event2 != FALSE & event2 != TRUE) ) ) stop("Argument 'event2' must be 0 or 1 if numeric and TRUE or FALSE if logical")
	if ( any(time1 < 0 | time2 < 0) ) stop("Arguments 'time1' and 'time2' must be greater than 0")
	if ( any(!event1 & time2 > 0) ) stop("Arguments 'time2' must be equal to 0 when variable event1 equals 0 or FALSE")
	object <- list( "data"=data.frame( "time1"=as.double(time1), "event1"=as.integer(event1), "time2"=as.double(time2), "event2"=as.integer(event2), "Stime"=as.double(time1+time2) ) )
	class(object) <- "survBIV"
	return(object)
}

is.survBIV <- function(object) inherits(object, "survBIV")

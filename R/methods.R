Biv <- function(object) {
	UseMethod("Biv")
}

BivSort <- function(object) {
	UseMethod("BivSort")
}

BivDist <- function(object, t1, t2) {
	UseMethod("BivDist")
}

BivMarginal <- function(object, len, times, probs, j) {
	UseMethod("BivMarginal")
}

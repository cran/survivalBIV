Biv <- function(object) {
	UseMethod("Biv")
}

BivSort <- function(object) {
	UseMethod("BivSort")
}

BivDist <- function(object, t1, t2) {
	UseMethod("BivDist")
}

BivMarginal <- function(object, times, n) {
	UseMethod("BivMarginal")
}

BivMatrix <- function(object, grid.x, grid.y) {
	UseMethod("BivMatrix")
}

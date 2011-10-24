dgpBIV <- function(n, corr, dist, model.cens, cens.par, dist.par, to.data.frame=FALSE) {
	if ( missing(n) ) stop("Argument 'n' is missing, with no default")
	if ( missing(corr) ) stop("Argument 'corr' is missing, with no default")
	if ( missing(dist) ) stop("Argument 'dist' is missing, with no default")
	if ( missing(model.cens) ) stop("Argument 'model.cens' is missing, with no default")
	if ( missing(cens.par) ) stop("Argument 'cens.par' is missing, with no default")
	if ( missing(dist.par) ) stop("Argument 'dist.par' is missing, with no default")
	if (n <= 0) stop("'n' must be greater than zero")
	if ( !( dist %in% c("weibull", "exponential") ) ) stop("Argument 'dist' must be one of 'weibull' or 'exponential'")
	if ( !( model.cens %in% c("uniform", "exponential") ) ) stop("Argument 'model.cens' must be one of 'uniform' or 'exponential'")
	if (model.cens == "uniform" & cens.par < 0) stop("Argument 'cens.par' with 'model.cens=uniform' must be greater or equal than 0")
	if (model.cens == "exponential" & cens.par <= 0) stop("Argument 'cens.par' with 'model.cens=exponential' must be greater than 0")
	if ( !is.logical(to.data.frame) ) stop("Argument 'to.data.frame' must be logical")
	if (dist == "weibull") {
		if (corr <= 0 | corr > 1) stop("Argument 'corr' with 'dist=weibull' must be greater than 0 and lower or equal to 1")
		if (length(dist.par) != 4) stop("Argument 'dist.par' with 'dist=weibull' must be a vector with lenght 4")
		if (dist.par[1] <= 0 | dist.par[2] <= 0 | dist.par[3] <= 0 | dist.par[4] <= 0) stop("Argument 'dist.par' must be greater than 0")
		mydata <- c()
		for (dim in 1:n) {
			if (model.cens == "uniform") c <- runif(1, 0, cens.par)
			else if (model.cens == "exponential") c <- rexp(1, cens.par)
			if(corr == 0) {
				x1 <- rweibull(1, dist.par[1], dist.par[2])
				x2 <- rweibull(1, dist.par[3], dist.par[4])
			} else {
				u <- runif(1, 0, 1)
				u2 <- runif(4, 0, 1)
				if (u2[4] > corr) v <- -log(u2[3])
				else v <- -log(u2[1])-log(u2[2])
				x1 <- u^(corr/dist.par[1])*v^(1/dist.par[1])*dist.par[2]
				x2 <- (1-u)^(corr/dist.par[3])*v^(1/dist.par[3])*dist.par[4]
			}
			time1 <- min(c, x1)
			ifelse(time1 == c, delta <- 0, delta <- 1)
			c2 <- (c-x1)*delta
			time2 <- min(c2, x2)
			ifelse(time1+time2 == c, status <- 0, status <- 1)
			mydata <- rbind( mydata, c(time1=time1, delta=delta, time2=time2, status=status) )
		}
	} else if (dist == "exponential") {
		if (corr < 0 | corr > 1) stop("Argument 'corr' with 'dist=exponential' must be between 0 and 1")
		if (length(dist.par) != 2) stop("Argument 'dist.par' with 'dist=exponential' must be a vector with lenght 2")
		if (dist.par[1] <= 0 | dist.par[2] <= 0) stop("Argument 'dist.par' must be greater than 0")
		mydata<-c()
		for (dim in 1:n) {
			if (model.cens == "uniform") c <- runif(1, 0, cens.par)
			else if (model.cens == "exponential") c <- rexp(1, cens.par)
			if (corr != 0) {
				delta1 <- rbinom(1, 1, 1)
				u1 <- runif(1, 0, 1)
				u2 <- runif(1, 0, 1)
				v1 <- runif(1, 0, 1)
				if (v1 < 0.5) v2 <- min( u1, ( -u2/(2*corr*v1-corr) ) ) else v2 <- max( u1, 1-( u2/(2*corr*v1-corr) ) )
				z <- -(1/dist.par[1])*log(v1)
				t23 <- -(1/dist.par[2])*log(v2)*delta1
			} else {
				t12 <- rexp(1, dist.par[1])
				t23 <- rexp(1, dist.par[2])
				z <- t12
				if (t12 <= c) delta1 <- 1
				else delta1 <- 0
				t23 <- t23*delta1
			}
			ztilde <- min(z, c)
			t23tilde <- min( t23, max(c-z,0) )
			status <- 1
			if (t23tilde < t23) status <- 0
			if (ztilde < z) {delta1 <- 0; status <- 0; t23tilde <- 0}
			t23tilde <- t23tilde*delta1
			delta2 <- delta1+(1-delta1)*status
			mydata <- rbind( mydata, c(ztilde, delta2, t23tilde, status) )
		}
	}
	if (to.data.frame == TRUE) return( data.frame("time1"=mydata[,1], "event1"=mydata[,2], "time2"=mydata[,3], "event2"=mydata[,4], row.names=NULL) )
	else return( survBIV(mydata[,1], mydata[,2], mydata[,3], mydata[,4]) )
}

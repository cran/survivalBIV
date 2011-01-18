corr.biv <-function(dist,corr,dist.par)
{
if(missing(corr)) stop("Argument 'corr' is missing with no default")
if(missing(dist)) stop("Argument 'dist' is missing with no default")
if(missing(dist.par)) stop("Argument 'dist.par' is missing with no default")
if(dist=="exponential") 
{ 
if(length(dist.par)!=2)
stop("Argument 'dist.par' must be vector with lenght 2")
 if(corr<(-1))
stop("Argument 'corr' must be greater or equal -1 ")
 if(corr>1&corr<(-1))
   stop("Argument 'corr' must be less or equal 1 ")
corrxy<-(1/4)*corr
return(corrxy)
}   
if(dist=="weibull") 
{
if(length(dist.par)!=4)
stop("Argument 'dist.par' must be vector with lenght 4")
if(corr<0)
    stop("Argument 'corr' must be greater than 0 ") 
if(corr>1)
    stop("Argument 'corr' must be less or equal 1")
a<-gamma((corr/dist.par[1])+1)
b<-gamma((corr/dist.par[3])+1)
c<-gamma((1/dist.par[1])+(1/dist.par[3])+1)
d<-gamma((1/dist.par[1])+1)
e<-gamma((1/dist.par[3])+1)
f<-gamma((corr/dist.par[1])+(corr/dist.par[3])+1)
cov1<-dist.par[2]*dist.par[4]*(a*b*c-d*e*f)/f  
varx<-dist.par[2]^2*(gamma((2/dist.par[1])+1)-d^2)
vary<-dist.par[4]^2*(gamma((2/dist.par[3])+1)-e^2)
corrxy<-cov1/sqrt(varx*vary)
return(corrxy)
}  
}


`bivKMW`<-function(mydata,t1,t2) 
{ 
if(missing(mydata)) stop("Argument 'mydata' is missing with no default")
if(class(mydata)!="bivariate") stop("Argument 'mydata' must be class 'bivariate'")
mydata<-mydata[[1]]
if(missing(t1)) t1=0
if(missing(t2)) t2=max(mydata[,3])
if(t1<0|t2<0) stop("'t1' and 't2' must be positive")
p<-which(mydata[,1]<=t1 & mydata[,3]<=t2)
res1<-sum(mydata[p,11]) 
res2<-sum(mydata[p,12]) 
res3<-sum(mydata[p,13]) 
dados<-cbind(KMW=res1,KMPW_glm=res2,KMPW_gam=res3)
return(dados)
}


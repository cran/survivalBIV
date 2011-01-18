`bivIPCW`<-function(mydata,t1,t2) 
{
if(missing(mydata)) stop("Argument 'mydata' is missing with no default")
if(class(mydata)!="bivariate") stop("Argument 'mydata' must be class 'bivariate'")
mydata<-mydata[[1]]
if(missing(t1)) t1=0
if(missing(t2)) t2=max(mydata[,3])
if(t1<0|t2<0) stop("'t1' and 't2' must be positive")
require(mgcv)
mydata<-rbind(c(0,0,0,0,0,1),mydata)
p1y<-which(mydata[,1]<=t1 & mydata[,3]>t2)
if(length(p1y)==0) {vec_y<-0}
if(length(p1y)>0) 
{
  vec_y<-rep(0,length(p1y))
for(k in 1:length(p1y)) 
  {
    p2y<-which(mydata[,5]<=mydata[p1y[k],1]+t2)
if(mydata[max(p2y),6]==0) vec_y[k]<-0
else vec_y[k]<-1/mydata[max(p2y),6]
  }
}
p10<-which(mydata[,1]<=t1 & mydata[,3]>0)
if(length(p10)==0) vec_0<-0
if(length(p10)>0) 
{
  vec_0<-1/mydata[p10,8]
}
df<-(sum(vec_0)-sum(vec_y))/(dim(mydata)[1]-1)
return(df)
}


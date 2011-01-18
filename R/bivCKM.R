`bivCKM`<-function(mydata,t1,t2) 
{
if(missing(mydata)) stop("Argument 'mydata' is missing with no default")
if(class(mydata)!="bivariate") stop("Argument 'mydata' must be class 'bivariate'")
mydata<-mydata[[1]]
times1<-mydata[,1]
delta<-mydata[,2]
times2<-mydata[,3]
status<-mydata[,4]
if(missing(t1)) t1=0
if(missing(t2)) t2=max(times2)
if(t1<0|t2<0) stop("'t1' and 't2' must be positive")
ooo<-order(times1)
times1<-times1[ooo]
delta<-delta[ooo]
times2<-times2[ooo] 
status<-status[ooo]
s0<-1
unique.t0<-unique(times1)
unique.t0<-unique.t0[order(unique.t0)]
n.times<-sum(unique.t0<=t1)
for(j in 1:n.times) 
{
  n<-sum(times1>=unique.t0[j])
  d<-sum((times1==unique.t0[j])&(delta==1))
  if(n>0) {s0<-s0*(1-d/n)}
}
s.pooled<-s0
s0<-1
subset<-as.logical(times1<=t1)
t0<-times2[subset]
c0<-status[subset]
if(!is.null(t0)) 
{
  unique.t0<-unique(t0)
  unique.t0<-unique.t0[order(unique.t0)]
  n.times<-sum(unique.t0<=t2)
  if(n.times>0) 
  {
    for(j in 1:n.times) 
    {
      n<-sum(t0>=unique.t0[j])
      d<-sum((t0==unique.t0[j])&(c0==1))
      if(n > 0) s0<-s0*(1-d/n) 
    }
  }
}
res<-(1-s.pooled)*(1-s0) 
return(res)
}


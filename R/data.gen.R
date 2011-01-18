data.gen <-function(n,corr,dist,model.cens,cens.par,dist.par)
{
if(n<=0) stop("'n' must be greater than zero")
if(missing(corr)) stop("Argument 'corr' is missing with no default")
if(missing(dist)) stop("Argument 'dist' is missing with no default")
if(missing(model.cens)) stop("Argument 'model.cens' is missing with no default")
if(missing(cens.par)) stop("Argument 'cens.par' is missing with no default")
if(missing(dist.par)) stop("Argument 'dist.par' is missing with no default")
if(dist=="exponential") 
{
if(length(dist.par)!=2)
    stop("Argument 'dist.par' must be vector with lenght 2") 
  dados<-c()
  for(dim in 1:n) 
{
    delta<-1
if(model.cens=="uniform"){c<-runif(1,0,cens.par) }   
if(model.cens=="exponential"){c<-rexp(1,cens.par) }
x<-rexp(1,dist.par[1])
y<-rexp(1,dist.par[2])
if(c<x) {t1<-c}
else {t1<-x}
if(corr==1) 
{   
if(x<log(2)) 
{
b<-2*exp(-x)-1
d<-rbinom(1,1,b)
if(d==1) t<-rexp(1,2)
else t<-rexp(1,1)
}
else 
{
        a<-2-2*exp(-x)
    u<-runif(1,0,1)
    t<-log(2*a-2)-log(a-sqrt(a^2-4*(a-1)*(1-u)))
}
}
if(corr==0){t<-y}  
t2<-min(t,c-t1)
if(c-t1<=t) 
{  
delta<-0
t2<-max(0,c-t1)
}
ifelse (c<=t1,aux<-c(t1,0,0,0),aux<-c(t1,1,t2,delta))
dados<-rbind(dados,c(aux[1],aux[2],aux[3],aux[4],aux[1]+aux[3]))
}
mydata<-data.frame(dados,row.names=NULL)
colnames(mydata)<-c("time1","delta","time2","status","Stime")
return(mydata)
}
if(dist=="weibull") 
{
if(length(dist.par)!=4)
    stop("Argument 'dist.par' must be vector with lenght 4")
if(corr==0) 
{ 
dados<-c()
for(dim in 1:n)
{
if(model.cens=="uniform"){c<-runif(1,0,cens.par) }  
if(model.cens=="exponential"){c<-rexp(1,cens.par) }
x1<-rweibull(1,dist.par[1],dist.par[2])           
x2<-rweibull(1,dist.par[3],dist.par[4])            
time1<-min(c,x1)
ifelse(time1==c,delta<-0,delta<-1)
c2<-(c-x1)*delta
time2<-min(c2,x2)
ifelse(time1+time2==c,status<-0,status<-1)
dados<-rbind(dados,c(time1=time1,delta=delta,time2=time2,status=status,Stime=time1+time2))
 }
mydata<-data.frame(dados,row.names=NULL)
colnames(mydata)<-c("time1","delta","time2","status","Stime")
return(mydata)
}
if(corr!=0) 
{  
dados<-c()
for(dim in 1:n) 
{
if(model.cens=="uniform"){c<-runif(1,0,cens.par) }   
if(model.cens=="exponential"){c<-rexp(1,cens.par) }
u<-runif(1,0,1)
u2<-runif(4,0,1)
if(u2[4]>corr) {v<- -log(u2[3])}
else {v<- -log(u2[1])-log(u2[2])}
x1<-u^(corr/dist.par[1])*v^(1/dist.par[1])*dist.par[2]
x2<-(1-u)^(corr/dist.par[3])*v^(1/dist.par[3])*dist.par[4]
time1<-min(c,x1)
ifelse(time1==c,delta<-0,delta<-1)
c2<-(c-x1)*delta
time2<-min(c2,x2)
ifelse(time1+time2==c,status<-0,status<-1)
Stime<-time1+time2
dados<-rbind(dados,c(time1=time1,delta=delta,time2=time2,status=status,Stime=Stime))
}
mydata<-data.frame(dados,row.names=NULL)
colnames(mydata)<-c("time1","delta","time2","status","Stime")
return(mydata)
}  
}
}


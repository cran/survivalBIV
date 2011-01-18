adapt <-function(data=NULL) 
{          
if(missing(data)) stop("Argument 'data' is missing with no default")
if(!is.data.frame(data)) 
  stop("Argument 'data' must be a data.frame")
if(any(names(data)[1:5] != c("time1", "delta", "time2","status","Stime"))) 
  stop("'data' must contain the right variables")
if(dim(data[,c(1:5)])[1]!=nrow(data))
  stop("'data' must be dim")    
if(any(data[,2] != 0 & data[,2] != 1)) 
  stop("The variable 'delta' in the argument 'data' must be 0 or 1")
if(any(data[,4] != 0 & data[,4] != 1)) 
  stop("The variable 'status' in the argument 'data' must be 0 or 1")
if(any(data[,1] + data[,3] != data[,5])) 
  stop("The variable 'Stime' in the argument 'data' must be equal to time1+time2")
if(any(data[,2] == 0 & data[,3] > 0)) 
  stop("The variable 'time2' in the argument 'data' must be equal to 0 when delta=0")
if(any(data[, c(1,3)] < 0)) 
  stop("The variable 'time1' and 'time2' must be greater than 0")
n<-nrow(data)
mydata<-matrix(ncol=13,nrow=n)
ord<-order(data[,5],1-data[,4],data[,1],data[,2],data[,3])
mydata[,5]<-sort(data[,5]) 
for(k in 1:n) 
{
  mydata[k,1]<-data[ord[k],1]
mydata[k,2]<-data[ord[k],2]
mydata[k,3]<-data[ord[k],3]
mydata[k,4]<-data[ord[k],4]
} 
mydata[,6]<-1-cumsum(KMW(mydata[,5],1-mydata[,4]))  
ord3<-rank(mydata[,1],ties.method ="first")
ord2<-order(mydata[,1])
aux<-1-sort(cumsum(KMW(mydata[ord2,1],1-mydata[ord2,2])),decreasing =FALSE)

mydata[,7]<-mydata[,1]

for (k in (dim(mydata)[1]):2) {
if (mydata[k-1,5]==mydata[k,5]) mydata[k-1,6]<-mydata[k,6]
if (mydata[k-1,7]==mydata[k,7]) aux[k-1]<-aux[k]}
        
mydata[,8]<-aux[ord3]

require(mgcv)


w<-which(mydata[,2]==1)
mydata2<-mydata[w,1:5]
mydata2<-data.frame(mydata2)
colnames(mydata2)<-c("time1","delta","time2","status","Stime")
obj<-gam(mydata2$status~mydata2$time1+mydata2$time2,family=binomial,data=mydata2)
z<-summary(obj)$p.coeff
Status2<-rep(0,dim(data)[1])
stat_aux<-exp(z[1]+z[2]*mydata2$time1+z[3]*mydata2$time2)/(1+exp(z[1]+z[2]*mydata2$time1+z[3]*mydata2$time2))
for(k in 1:length(stat_aux)) Status2[w[k]]<-stat_aux[k]

Status2<-Status2*mydata[,2]

obj2<-gam(mydata2$status~s(mydata2$time1)+s(mydata2$time2),family=binomial,data=mydata2)
val<-predict.gam(obj2)
Status3<-rep(0,dim(data)[1])
stat_aux2<-exp(val)/(1+exp(val))
for(k in 1:length(stat_aux2)) Status3[w[k]]<-stat_aux2[k]

Status3<-Status3*mydata[,2]

mydata[,9]<-Status2
mydata[,10]<-Status3

mydata[,11]<-KMW(mydata[,5],mydata[,4]) 

mydata[,12]<-KMW(mydata[,5],Status2)

mydata[,13]<-KMW(mydata[,5],Status3)

mydata<-data.frame(mydata,row.names=NULL) 
colnames(mydata)<-c("time1","delta","time2","status","Stime","KM_C","Time1_O","KM1_C","status_glm","status_gam","KMW","KMPW_glm","KMPW_gam") 
mydata<-list(mydata)
class(mydata)<-"bivariate"
return(mydata)
}


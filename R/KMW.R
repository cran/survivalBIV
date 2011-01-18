KMW <-function(time,status) 
{
if(missing(time)) stop("Argument 'time' is missing with no default")
if(missing(status)) stop("Argument 'status' is missing with no default")
M1<-cbind(time,status)  
n=nrow(M1)
M2<-matrix(0,nrow=nrow(M1),ncol=ncol(M1))
ord<-order(M1[,2],decreasing=TRUE)
M2[,2]<-sort(M1[,2],decreasing=TRUE)
M2[,-2]<-M1[ord,-2]
R=rank(M2[,1],ties.method="first")
Pkm2=rep(1,n)
Wkm<-rep(0,n)
Pkm2<-1-M2[,2]/(n-R+1)
count<-outer(R,R,"<")
Pkm2_aux<-matrix(Pkm2,nrow=n,ncol=n,byrow=FALSE)
Pkm2_2<-count*Pkm2_aux
Pkm2_2[Pkm2_2[,]==0]<-1
Pkm2_cum<-apply(Pkm2_2,2,prod)
Pkm3<-M2[,2]/(n-R+1)
Wkm<-Pkm3*Pkm2_cum
ord2<-order(M2[,1],decreasing=FALSE)
Wkm<-Wkm[ord2]   
  ord2<-rank(time,ties.method="first")     
Wkm<-Wkm[ord2]                    
return(Wkm)
}


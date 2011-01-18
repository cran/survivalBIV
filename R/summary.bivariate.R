summary.bivariate<-function(object,t1,t2,method,...)
{
mydata<-object
if(missing(mydata)) stop("Argument 'mydata' is missing with no default")
if(missing(method)) method="all"    
if(class(mydata)!="bivariate") stop("Argument 'mydata' must be class 'bivariate'")
if(missing(t1)) t1=0
if(missing(t2)) t2=max(mydata[[1]][,3])
if(t1<0 | t2<0) stop("'t1' and 't2' must be positive")
if(method=="all"|method=="CKM") 
{
  res1<-bivCKM(mydata=mydata,t1,t2)
  CKM<-res1
}
if(method=="all"|method=="IPCW") 
{
  res2<-bivIPCW(mydata=mydata,t1,t2)
  IPCW<-res2
}
if(method=="all"|method=="KMW") 
{
  res3<-bivKMW(mydata=mydata,t1,t2)
  KMW<-res3[1]
}
if(method=="all"|method=="KMPW") 
{
  res4<-bivKMW(mydata=mydata,t1,t2)
  KMPW<-res4[2]
  KMPW1<-res4[3]
}
if(method=="CKM") 
{  
  cat("F(",t1,",",t2,")=",CKM,"\n")
}
if(method=="IPCW") 
{ 
  cat("F(",t1,",",t2,")=",IPCW,"\n")
}
if(method=="KMW") 
{ 
  cat("F(",t1,",",t2,")=",KMW,"\n")
}
if(method=="KMPW") 
{ 
  cat("F(",t1,",",t2,")=",KMPW,"           ","KMPW_glm","\n")
  cat("F(",t1,",",t2,")=",KMPW1,"           ","KMPW_gam","\n")
}
if(method=="all")
{
  cat("F(",t1,",",t2,")=","\n")
dados<-rbind(c(CKM=CKM,IPCW=IPCW,KMW=KMW,KMPW_glm=KMPW,KMPW_gam=KMPW1))
return (as.data.frame(dados))
}
}


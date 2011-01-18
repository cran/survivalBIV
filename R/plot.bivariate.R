plot.bivariate<-function(x,plot.marginal=NULL,plot.bivariate=NULL,xlab,ylab,zlab,col,method,grid.x=NULL,grid.y=NULL,col.biv=NULL,xlim=NULL,ylim=NULL,...) 
{
mydata<-x
if(missing(mydata)) stop("Argument 'mydata' is missing with no default")
if(class(mydata)!="bivariate") stop("Argument 'mydata' must be class 'bivariate'")
if(missing(method)) stop("Must choose one or all estimator")
if(missing(xlab)) 
{
  xlab <- "Time"
  xlab.aux <- TRUE
} 
else {xlab.aux<-FALSE}
if(missing(ylab)) {ylab.aux <- TRUE}
if(missing(plot.marginal)) plot.marginal <- FALSE
if(missing(plot.bivariate)) plot.bivariate <- FALSE
if(missing(col.biv)) col.biv <- FALSE
if(plot.marginal == "TRUE") 
{
  if(method=="KMW")
  {
    x <- which(mydata[[1]][,4]==1)      
    y1 <- mydata[[1]][x,5]
    y1<-c(y1,max(mydata[[1]][,3]))
    y2 <- unique(y1)
    y3 <- sort(y2)
    y <- c(0, y3)
    mydata2 <- matrix(data = 0, ncol = 2, nrow = length(y))
    for(k in 1:length(y)) 
    {
      mydata2[k,1] <- y[k]
      mydata2[k,2]<-bivKMW(mydata=mydata,t1=max(mydata[[1]][,1]),t2=mydata2[k,1])[1]
    }
  }
  if(method=="KMPW")
  {
    x0<-which(mydata[[1]][,2]==1)
    y10<-mydata[[1]][x0,5]
    y20<-unique(y10)
    y30<-sort(y20)
    y0<-c(0, y30)
    dado<-matrix(data=0,ncol=2,nrow=length(y0))
    gam<-matrix(data=0,ncol=2,nrow=length(y0))
    for(k in 1:length(y0)) 
    {
      dado[k,1]<-y0[k]
      gam[k,1]<-y0[k]
      dado[k,2]<-bivKMW(mydata=mydata,t1=max(mydata[[1]][,1]),t2=dado[k,1])[2]
			gam[k,2]<-bivKMW(mydata=mydata,t1=max(mydata[[1]][,1]),t2=gam[k,1])[3]
    }
  }
  if(method=="CKM")
  {  
    x <- which(mydata[[1]][,2]==1)      
    y1 <- mydata[[1]][x,5]
    y2 <- unique(y1)
    y3 <- sort(y2)
    y <- c(0, y3)
    mydata2 <- matrix(data = 0, ncol = 2, nrow = length(y))
    for(k in 1:length(y)) 
    {
      mydata2[k,1]<- y[k]
      mydata2[k,2]<-bivCKM(mydata=mydata,t1=max(mydata[[1]][,1]),t2=mydata2[k,1])
   	}
 	}
  if(method=="IPCW")
  {
    vec1<-mydata[[1]][,1]
    x1<-unique(vec1[order(vec1)])
    y1<-sort(unique(mydata[[1]][which(mydata[[1]][,4]==0),5]))
    ix<-length(x1)
    iy<-length(y1)
    mat<-matrix(0,ix,iy)
    for(i in 1:ix) for(j in 1:iy) mat[i,j]<-y1[j]-x1[i]
    yy<-unique(c(mat))
    yi<-yy[yy>0]
    yi<-sort(yi)
    mydata2 <- matrix(data = 0, ncol =2, nrow = length(yi))
      for(y in 1:length(yi)) 
      {
        mydata2[y,1]<- yi[y]
        mydata2[y,2]<-bivIPCW(mydata=mydata,t1=max(mydata[[1]][,1]),t2=mydata2[y,1])
      }
  }
  oask<-devAskNewPage(TRUE)
  on.exit(devAskNewPage(oask))
  if(ylab.aux==TRUE) ylab<-"Estimated marginal dist. F2(t)"
  if(method=="CKM" | method=="KMW" | method=="IPCW")
  {
  matplot(mydata2[,1],mydata2[,2],xlab=xlab,ylab=ylab,type="s",main=method,xlim=xlim,ylim=ylim,...)
  }
  if(method=="KMPW") 
  {
    matplot(dado[,1],dado[,2],xlab=xlab,ylab=ylab, type = "s",main="KMPW_glm",xlim=xlim,ylim=ylim,...)
    matplot(gam[,1],gam[,2],xlab=xlab,ylab=ylab,type="s",main="KMPW_gam",xlim=xlim,ylim=ylim,...)
  }
  oask<-devAskNewPage(TRUE)
  on.exit(devAskNewPage(oask))
} 
if(plot.bivariate=="TRUE") 
{
  if(method=="CKM")
  { 
    x<-which(mydata[[1]][,2]==1)
    x1<-unique(sort(mydata[[1]][x,1]))
    y1<-unique(sort(mydata[[1]][x,3]))
    mydata2<-matrix(data=0,nrow=length(x1),ncol=length(y1))  
    for(k in 1:length(x1)) 
    {
      for(j in 1:length(y1)) 
      {
        mydata2[k,j]<-bivCKM(mydata=mydata,t1=x1[k],t2=y1[j])
      }
    }
  }
  if(method=="IPCW")
  {   
    if(missing(grid.x)) 
    {
      x<-which(mydata[[1]][,2]==1)
      vec1<-mydata[[1]][,1]
      x1<-unique(vec1[order(vec1)]) 
    }
    else (x1<-grid.x)
    if(missing(grid.y)) 
    {
      yi<-sort(unique(mydata[[1]][which(mydata[[1]][,4]==0),5]))
      ix<-length(x1)
      iy<-length(yi)
      mat<-matrix(0,ix,iy)
      for(i in 1:ix) for(j in 1:iy) mat[i,j]<-yi[j]-x1[i]
      yy<-unique(c(mat))
      y1<-yy[yy>0]
      y1<-sort(y1)        
    }
    else (y1<-grid.y)
    mydata2<-matrix(data=0,nrow=length(x1),ncol=length(y1))
    for(k in 1:length(x1)) 
    {
      for(j in 1:length(y1)) 
      {
        mydata2[k,j]<-bivIPCW(mydata=mydata,x1[k],y1[j])
      }
    }
  }
  if(method=="KMW")
  { 
    x<-which(mydata[[1]][,4]==1)
    x1<-unique(sort(mydata[[1]][x,1]))
    y1<-unique(sort(mydata[[1]][x,3]))
    mydata2<-matrix(data=0,nrow=length(x1),ncol=length(y1))
    for(k in 1:length(x1)) 
    {
      for(j in 1:length(y1)) 
      {
        mydata2[k,j]<-bivKMW(mydata=mydata,x1[k],y1[j])[1]
      }
    }
  }
  if(method=="KMPW")
  {
    x0<-which(mydata[[1]][,2]==1)
    x1<-unique(sort(mydata[[1]][x0,1]))
    y1<-unique(sort(mydata[[1]][x0,3]))
    dado<-matrix(data=0,nrow=length(x1),ncol=length(y1))   
    gam<-matrix(data=0,nrow=length(x1),ncol=length(y1))
    for(k in 1:length(x1)) 
    {
      for(j in 1:length(y1)) 
      {
        dado[k,j]<-bivKMW(mydata=mydata,x1[k],y1[j])[2]
        gam[k,j]<-bivKMW(mydata=mydata,x1[k],y1[j])[3]
      }
    }    
  }
  if(xlab.aux==TRUE) xlab<-"Time in state 1"
  if(ylab.aux==TRUE) ylab<-"Time in state 2"
  if(missing(zlab)) zlab<-"F(t1,t2)"
  if(missing(col))  col<-"lightblue"
  if(method=="KMPW")
  {
    zz<-dado
    zzz<-gam
    persp(x1,y1,zz,theta=30,phi=30,ticktype="detailed",expand=0.5,xlab=xlab,ylab=ylab,zlab=zlab,col=col,shade=0.2,main="KMPW_glm")
    oask<-devAskNewPage(TRUE)
    on.exit(devAskNewPage(oask))
    persp(x1,y1,zzz,theta=30,phi=30,ticktype="detailed",expand=0.5,xlab=xlab,ylab=ylab,zlab=zlab,col=col,shade=0.2,main="KMPW_gam")
  }
  if(method=="CKM" | method=="KMW" )
  {
    z<-mydata2
    oask<-devAskNewPage(TRUE)
    on.exit(devAskNewPage(oask))
    persp(x1,y1,z,theta=30,phi=30,ticktype="detailed",expand=0.5,xlab=xlab,ylab=ylab,zlab=zlab,col=col,shade=0.2,main=method)
  }
  if(method=="IPCW")
  {
    z<-mydata2
    oask<-devAskNewPage(TRUE)
    on.exit(devAskNewPage(oask))
    persp(x1,y1,z,theta=30,phi=30,ticktype="detailed",expand=0.5,xlab=xlab,ylab=ylab,zlab=zlab,col=col,shade=0.2,main=method)
  }
  oask<-devAskNewPage(TRUE)
  on.exit(devAskNewPage(oask))
  if(method=="CKM")
  {
    x1<-seq(0,max(mydata[[1]][mydata[[1]][,2]==1,1])*1.25,length.out=30)
    y1<-seq(0,max(mydata[[1]][mydata[[1]][,2]==1,3])*1.25,length.out=30)
    data<-expand.grid(x1,y1)
    z<-seq(0,1,length.out=900)
    for(k in 1:900) z[k]<-bivCKM(mydata=mydata,t1=data[k,1],t2=data[k,2])
    z1<-matrix(z,30)
  }
  
  if(method=="IPCW")
  {
    x<-which(mydata[[1]][,2]==1)
    vec1<-mydata[[1]][,1]
    x11<-unique(vec1[order(vec1)]) 
    vec2<-mydata[[1]][x,3]
    vec2<-unique(vec2[order(vec2)])
    yy<-diff(vec2)
    y11<-sort(c(vec2,vec2[-length(vec2)]+yy/3,vec2[-1]-yy/3))
    x1<-seq(0,max(x11),along.with=x11)
    y1<-seq(0,max(y11),along.with=y11)
    data<-expand.grid(x1,y1)    
    z<-seq(0,1,length.out=dim(data)[1])   
    for(k in 1:dim(data)[1]) z[k]<-bivIPCW(mydata=mydata,t1=data[k,1],t2=data[k,2])
    z1<-matrix(z,length(x1))
  }
  if(method=="KMW")
  {
    x1<-seq(0,max(mydata[[1]][mydata[[1]][,4]==1,1])*1.25,length.out=30)
    y1<-seq(0,max(mydata[[1]][mydata[[1]][,4]==1,3])*1.25,length.out=30)
    data<-expand.grid(x1,y1)
    z<-seq(0,1,length.out=900)
    for(k in 1:900)  z[k]<-bivKMW(mydata=mydata,t1=data[k,1],t2=data[k,2])[1]
    z1<-matrix(z,30)
  }
  if(method=="KMPW")
  {  
    x10<-seq(0,max(mydata[[1]][mydata[[1]][,2]==1,1])*1.25,length.out=30)
    y10<-seq(0,max(mydata[[1]][mydata[[1]][,2]==1,3])*1.25,length.out=30)
    data1<-expand.grid(x10,y10)
    zz<-seq(0,1,length.out=900)  
    zzz<-seq(0,1,length.out=900)
    for(k in 1:900) 
    {
      zz[k]<-bivKMW(mydata=mydata,t1=data1[k,1],t2=data1[k,2])[2]
      zzz[k]<-bivKMW(mydata=mydata,t1=data1[k,1],t2=data1[k,2])[3]
    }
    z2<-matrix(zz,30)
    z3<-matrix(zzz,30)
    if(col.biv==FALSE) 
    {
      bw<-colours()[358-3*0:25]
      filled.contour(x10,y10,z2,xlab="Time in state 1",ylab="Time in state 2",main="KMPW_glm",col=bw,...)  
      oask<-devAskNewPage(TRUE)
      on.exit(devAskNewPage(oask))
      filled.contour(x10,y10,z3,xlab="Time in state 1",ylab="Time in state 2",main="KMPW_gam",col=bw,...)
    }
    else 
    {
      filled.contour(x10,y10,z2,xlab="Time in state 1",ylab="Time in state 2",main="KMPW_glm")
     	oask<-devAskNewPage(TRUE)
      on.exit(devAskNewPage(oask))
      filled.contour(x10,y10,z3,xlab="Time in state 1",ylab="Time in state 2",main="KMPW_gam")
    }
  }
  
	oask<-devAskNewPage(TRUE)
  on.exit(devAskNewPage(oask))
	if(method=="CKM" | method=="KMW" | method=="IPCW")
  {
    if(col.biv==FALSE) 
    {
      bw<-colours()[358-3*0:25]#[350-3*0:19]
      filled.contour(x1,y1,z1,xlab="Time in state 1",ylab="Time in state 2",main=method,col=bw,...)   
    }           
    else 
    {
      filled.contour(x1,y1,z1,xlab="Time in state 1",ylab="Time in state 2",main=method,...)
    }    
  }
}
}

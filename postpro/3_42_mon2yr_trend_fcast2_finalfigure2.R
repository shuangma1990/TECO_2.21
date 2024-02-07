# run 3_2 to get xair and xsoil
library(data.table)
library(ggplot2)
library(stringr)
library(readxl)
library(Hmisc)
a=c("f1p1","f1p2","f1p3","f1p4","f1p5","f1p6")
fname='_fcast2_200'
fname='_fcast2_r4'
fname='_fcast2_r5'

moy <- rep(c(1:12),8)
year<- rep(c(2011:2018),each=12)
letter<- c('(a)','(b)','(c)','(d)','(e)','(f)')

annplt <- matrix(NA,ncol = 60,nrow = 48)
annsdplt<- matrix(NA,ncol = 60,nrow = 48)
ann8yr<-matrix(NA,ncol=18)
annsd8yr<-matrix(NA,ncol=18)
for(i in 1:6){
  # i=2
  setwd("~/RESEARCH/TECO_SPRUCE_site/TECO_2.21_Dissertation/jpl_server/TECO_2.21/postpro/statistics/rowmean/statistics/")
  df1 <- read.csv(paste('rowmean_',a[i],fname,'.csv',sep=''),header = T)
  df2 <- read.csv(paste('rowsd_',a[i],fname,'.csv',sep=''),header = T)
  
  df1 <- cbind(year,moy,df1)
  df2 <- cbind(year,moy,df2)
  
  par(mar=c(1,3,1,1),mfrow=c(2,2))
  plot(1:96,df1$QC1_mean1)
  plot(1:96,df1$QC2_mean1)
  plot(1:96,df1$QC3_mean1)
  plot(1:96,df1$QC4_mean1)
  plot(1:96,df1$QC5_mean1)
  plot(1:96,df1$NPPS_mean1)
  
  # df11 <- aggregate(df1[,c(18:25,42:51)],by=list(df1$year),FUN=mean) # Cpools mean
  df11 <- df1[c(12,24,36,48,60,72,84,96),c(18:25,46:55)] # Cpools end of year  
  dfmean9 <- df1[c(12,24,36,48,60,72,84,96)-3,c(18:25,46:55)] # Cpools end of growing season, end of September
  dfmean4 <- df1[c(12,24,36,48,60,72,84,96)-8,c(18:25,46:55)] # Cpools start of growing season, end of April
  df12 <- aggregate(df1[,c(4:17,26:45)],by=list(df1$year),FUN=sum)   # Cfluxes sum
  # df21 <- aggregate(df2[,c(18:25,42:51)],by=list(df2$year),FUN=mean) # Cpools sd
  df21 <- df2[c(12,24,36,48,60,72,84,96),c(18:25,46:55)] # Cpools sd, end of
  dfsd9 <- df2[c(12,24,36,48,60,72,84,96)-3,c(18:25,46:55)] # Cpools sd, end of growing season, end of September
  dfsd4 <- df2[c(12,24,36,48,60,72,84,96)-8,c(18:25,46:55)] # Cpools start of growing season, end of April
  df22 <- aggregate(df2[,c(4:17,26:45)],by=list(df2$year),FUN=sum)   # Cfluxes sd
  
  # # delta C pool during 2016-2018
  # df13 <- df11[6:8,]-df11[5:7,]
  # df23 <- df21[6:8,]-df21[5:7,]
  
  # delta C pool during 2016-2018
  df13 <- dfmean9[6:8,]-dfmean4[6:8,]
  df23 <- dfsd9[6:8,]-dfsd4[6:8,]
  
  # sum C flux during 2016-2018
  df14 <- df12[6:8,]
  df24 <- df22[6:8,]
  
  df11<-df11[6:8,1:8]
  df21<-df21[6:8,1:8]
  
  ann <- cbind(df11,df13,df14[,-1])
  annsd<-cbind(df21,df23,df24[,-1])

  colnames(ann)[1:16] <- c('QC1','QC2','QC3','QC4','QC5','QC6','QC7','QC8',
                           'dQC1','dQC2','dQC3','dQC4','dQC5','dQC6','dQC7','dQC8')
  if (i==1){
    annplt <- ann
    annsdplt<-annsd
    ann8yr <- dfmean9[1:7,] # only saving 2011-2017
    annsd8yr<-dfsd9[1:7,]
  }else{
    annplt <- rbind(annplt,ann)
    annsdplt<-rbind(annsdplt,annsd)
    ann8yr<-rbind(ann8yr,dfmean9[1:7,])
    annsd8yr<-rbind(annsd8yr,dfsd9[1:7,])
  }
}
# replace outlier sd values for ch4
annsdplt[annsdplt>50]=50

junk$nm[which(junk$nm=="B")]<-"b"

# save annplt
setwd("~/RESEARCH/TECO_SPRUCE_site/TECO_2.21_Dissertation/jpl_server/TECO_2.21/postpro/")
write.csv(annplt, file = paste("3_3_mon2yr_trend",fname,".csv",sep=''))
write.csv(annsdplt, file = paste("3_3_mon2yr_trend_sd",fname,".csv",sep=''))


setwd("~/RESEARCH/TECO_SPRUCE_site/TECO_2.21_Dissertation/jpl_server/TECO_2.21/postpro/")
pdf(paste("3_3_yr_trend_final_f2.pdf",sep=''),width=7.5, height=4., compress=FALSE)
par(mfrow=c(2,3),oma=c(2, 0, 0, 1))
par(cex.lab=1.2)
caf=alpha("forest green",0.7)
cab=alpha("#999999",0.7)
cabar="forest green"
pointcex=1
# xrg=c(0,20)  # year round mean
# xat=c(0,5,10,15,20)
xrg=c(2,13)  # year round mean
xat=c(3,6,9,12)
xairseq = seq(0,20,0.01)
xsoilseq = seq(0,20,0.01)
# xsoil = xair
rsize=1

base=6
m=0
for (c in c(55:60)){
  # c=21
  m=m+1
  op <- par(mar=c(0., 4, 0.5, 0))
  x <- xsoil
  if ((c==57)){
    y <- as.vector(annplt[,c])
  }else{
  y <- as.vector(-annplt[,c])  # gpp and npp as carbon gain others loss
  }
  ann2df<-ann2list[[m+base]]
  yvalue <-as.vector(ann2df[6:8,]) 
  errvalue<-as.vector(annsdplt[,c])
  # ************************************************
  # ************************************************
  ms = data.frame(x = x, y = y, sd = errvalue)
  
  if (m<3){
    yrange <- c(-300,0)
  }else if(m==3){
    yrange <- c(0,100)
  }else{
    yrange <- c(-100,10)
  }
  
  plot(x,y,col=caf,lwd=3 ,pch=16, cex=pointcex, xlim=xrg,
       ylim = yrange,
       main=NA,ylab = colnames(annplt[c]),axes=F,ann=F)
  if (m>3){
    axis(side=1, labels=T, tck=0.03, mgp=c(3, 0.3,0), at=xat)
    axis(side=2, labels=T, tck=0.03, las=1,mgp=c(3, 0.3,0), at=c(seq(from=yrange[1],to=yrange[2],by=20)))
  }else{
    axis(side=2, labels=T, tck=0.03, las=1,mgp=c(3, 0.3,0), at=c(seq(from=yrange[1],to=yrange[2],by=50)))
  }
    
  par(fg = cabar)
  with(data = ms, expr = errbar(x,y,y+sd,y-sd,add=T,pch=16,cap=.01,col=caf))
  par(fg = "black")
  
  fit <- lm(y~ x)
  summary(fit)$r.squared
  pvalue <- summary(fit)$coefficients[2,4]  
  
  #-----  OLS  ----------------------------------------------------------------
  x0 <- seq(min(x), max(x), length = 20)  ## prediction grid
  y0 <- predict.lm(fit, newdata = list(x = x0))  ## predicted values
  slp1=round(as.numeric(fit$coefficients[2]),digit=1)
  if (pvalue<0.05){
    lines(x0, y0, col = caf,lwd=1,lty = 'dashed')  ## add regression curve (colour: red)
  }
  
  
  #-----  TLS   ---------------------------------------------------------------
  v <- prcomp(cbind(x,y))$rotation
  beta <- v[2,1]/v[1,1]
  inter <- mean(y) - beta*mean(x)
  x00 <- seq(min(x), max(x), length = 20)
  y00 <- beta*x00+inter
  if (pvalue<0.05){
    # lines(x00,y00,col="blue",lwd=1)
  }
  slp2 <- round(beta,digits = 2)
  # saveslp <- cbind(saveslp,slp2)
  #----------------------------------------------------------------------------
  if ((pvalue<0.05) & (pvalue>0.01)){
    # mtext(paste('P < 0.05 ',sep=''),outer=FALSE,side=1,line = -2.,col = "black",
    #       at=par("usr")[1]+0.8*diff(par("usr")[1:2]),cex=rsize)
    mtext(paste('m = ',slp1,'*',sep=''),outer=FALSE,side=3,line = -8.5,col = caf,
          at=par("usr")[1]+0.25*diff(par("usr")[1:2]),cex=rsize)
  }else if((pvalue<=0.01) & (pvalue>0.001)){
    # mtext(paste('P < 0.01',sep=''),outer=FALSE,side=1,line = -2.,col = "black",
    #       at=par("usr")[1]+0.8*diff(par("usr")[1:2]),cex=rsize)
    mtext(paste('m = ',slp1,'**',sep=''),outer=FALSE,side=3,line = -8.5,col=caf,
          at=par("usr")[1]+0.25*diff(par("usr")[1:2]),cex=rsize)
  }else if(pvalue<=0.001){
    # mtext(paste('P < 0.01',sep=''),outer=FALSE,side=1,line = -2.,col = "black",
    #       at=par("usr")[1]+0.8*diff(par("usr")[1:2]),cex=rsize)
    mtext(paste('m = ',slp1,'***',sep=''),outer=FALSE,side=3,line = -8.5,col = caf,
          at=par("usr")[1]+0.25*diff(par("usr")[1:2]),cex=rsize)
  }else{
    mtext(paste('m = ',slp1,sep=''),outer=FALSE,side=3,line = -8.5,col = caf,
          at=par("usr")[1]+0.25*diff(par("usr")[1:2]),cex=rsize)
  }
  # mtext(paste('(',letter[m+base],')',sep=''),outer=FALSE,side=1,line = -5.,col = "black",
  #       at=par("usr")[1]+0.9*diff(par("usr")[1:2]),cex=rsize)
  # ************************************************

    # ************************************************
  points(xsoil,yvalue,col=cab,lwd=3 ,pch=16, cex=pointcex, ylim=c(0,1000), xlim=xrg,main=NA)
  model3 <- lm(yvalue~ xsoil)
  summary(model3)
  newdata <- data.frame(xsoil=xsoilseq)
  newdata$pred1 <- predict(model3,newdata)
  pvalue <- summary(model3)$coefficients[2,4]
  # #-----  TLS   ---------------------------------------------------------------
  # y=yvalue
  # x=xsoil
  # v <- prcomp(cbind(x,y))$rotation
  # beta <- v[2,1]/v[1,1]
  # inter <- mean(y) - beta*mean(x)
  # x00 <- seq(min(x), max(x), length = 20)
  # y00 <- beta*x00+inter
  # if (pvalue<0.05){
  #   # lines(x00,y00,col="blue",lwd=1)
  # }
  # slp2 <- round(beta,digits = 2)
  # # saveslp <- cbind(saveslp,slp2)

  #-----  OLS  ----------------------------------------------------------------
  x0 <- seq(min(xsoil), max(xsoil), length = 20)  ## prediction grid
  y0 <- predict.lm(model3, newdata = list(xsoil = x0))  ## predicted values
  slp1=round(as.numeric(model3$coefficients[2]),digit=1)
  if (pvalue<0.05){
    lines(x0, y0, col = cab,lwd=1,lty = 'dashed')  ## add regression curve (colour: red)
  }

  #----------------------------------------------------------------------------
  if ((pvalue<0.05) & (pvalue>0.01)){
    # mtext(paste('P < 0.05 ',sep=''),outer=FALSE,side=1,line = -2.,col = "black",
    #       at=par("usr")[1]+0.8*diff(par("usr")[1:2]),cex=rsize)
    mtext(paste('m = ',slp1,'*',sep=''),outer=FALSE,side=3,line = -9.5,col=cab,
          at=par("usr")[1]+0.25*diff(par("usr")[1:2]),cex=rsize)
  }else if((pvalue<=0.01) & (pvalue>0.001)){
    # mtext(paste('P < 0.01',sep=''),outer=FALSE,side=1,line = -2.,col = "black",
    #       at=par("usr")[1]+0.8*diff(par("usr")[1:2]),cex=rsize)
    mtext(paste('m = ',slp1,'**',sep=''),outer=FALSE,side=3,line = -9.5,col=cab,
          at=par("usr")[1]+0.25*diff(par("usr")[1:2]),cex=rsize)
  }else if(pvalue<=0.001){
    # mtext(paste('P < 0.01',sep=''),outer=FALSE,side=1,line = -2.,col = "black",
    #       at=par("usr")[1]+0.8*diff(par("usr")[1:2]),cex=rsize)
    mtext(paste('m = ',slp1,'***',sep=''),outer=FALSE,side=3,line = -9.5,col=cab,
          at=par("usr")[1]+0.25*diff(par("usr")[1:2]),cex=rsize)
  }else{
    mtext(paste('m = ',slp1,sep=''),outer=FALSE,side=3,line = -9.5,col = cab,
          at=par("usr")[1]+0.25*diff(par("usr")[1:2]),cex=rsize)
  }
  mtext(side=2, text=labv[m+base], line=2, cex=rsize-0.2)
  mtext(side=3,text=letter[m],line=-2,cex=rsize,at=3)
  box()
}

dev.off()

# CO2.CH4=ann.er/ann.ch4
# Rh.ratio=ann.rh/ann.er
# oxipercentage=ann.oxid/ann.prod
# df <- rbind(CO2.CH4,Rh.ratio,ann.nee)
# print("save to csv file")
# # setwd("E:/Netbeans_Project/TECO_SPRUCE_2.2_acclimation_server/scripts_on_server/Rscript_on_server/ann/")
# setwd(paste(readpath,"ann/",sep=''))
# write.csv(df, file = paste("statistics/ann_yr_ratios",CS[1],".csv",sep=''))

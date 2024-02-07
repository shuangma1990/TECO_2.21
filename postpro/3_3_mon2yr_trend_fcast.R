# run 3_2 to get xair and xsoil
library(data.table)
library(ggplot2)
library(stringr)
library(readxl)
library(Hmisc)
a=c("f1p1","f1p2","f1p3","f1p4","f1p5","f1p6")
fname='_fcast'
moy <- rep(c(1:12),8)
year<- rep(c(2011:2018),each=12)

annplt <- matrix(NA,ncol = 38,nrow = 48)
annsdplt<- matrix(NA,ncol = 38,nrow = 48)

for(i in 1:6){
  # i=1
  setwd("~/RESEARCH/TECO_SPRUCE_site/TECO_2.21_Dissertation/jpl_server/TECO_2.21/postpro/statistics/rowmean/statistics/")
  df1 <- read.csv(paste('rowmean_',a[i],fname,'.csv',sep=''),header = T)
  df2 <- read.csv(paste('rowsd_',a[i],fname,'.csv',sep=''),header = T)
  
  df1 <- cbind(year,moy,df1)
  df2 <- cbind(year,moy,df2)
  
  # df11 <- aggregate(df1[,c(18:25,42:51)],by=list(df1$year),FUN=mean) # Cpools mean
  df11 <- df1[c(12,24,36,48,60,72,84,96),c(18:25,40:49)] # Cpools end of year amount
  df12 <- aggregate(df1[,c(4:17,26:39)],by=list(df1$year),FUN=sum)   # Cfluxes mean
  # df21 <- aggregate(df2[,c(18:25,42:51)],by=list(df2$year),FUN=mean) # Cpools sd
  df21 <- df2[c(12,24,36,48,60,72,84,96),c(18:25,40:49)] # Cpools sd
  df22 <- aggregate(df2[,c(4:17,26:39)],by=list(df2$year),FUN=sum)   # Cfluxes sd
  
  # delta C during 2016-2018
  df13 <- df11[6:8,]-df11[5:7,]
  df23 <- df21[6:8,]-df21[5:7,]
  df14 <- df12[6:8,]
  df24 <- df22[6:8,]
  
  ann <- cbind(df13,df14[,-1])
  annsd<-cbind(df23,df24[,-1])

  if (i==1){
    annplt <- ann
    annsdplt<-annsd
  }else{
    annplt <- rbind(annplt,ann)
    annsdplt<-rbind(annsdplt,annsd)
  }
}

# save annplt
setwd("~/RESEARCH/TECO_SPRUCE_site/TECO_2.21_Dissertation/jpl_server/TECO_2.21/postpro/")
write.csv(annplt, file = paste("3_3_mon2yr_trend",fname,".csv",sep=''))
write.csv(annsdplt, file = paste("3_3_mon2yr_trend_sd",fname,".csv",sep=''))


setwd("~/RESEARCH/TECO_SPRUCE_site/TECO_2.21_Dissertation/jpl_server/TECO_2.21/postpro/")
pdf(paste("3_3_yr_trend",fname,".pdf",sep=''),width=15.5, height=7.75, compress=FALSE)
par(mfrow=c(2,3),oma=c(3, 0, 0, 1))
par(cex.lab=1.2)
caf="forest green"
cab="#999999"
cabar="forest green"
pointcex=1
xrg=c(0,20)  # year round mean
xat=c(0,5,10,15,20)
xairseq = seq(0,20,0.01)
xsoilseq = seq(0,20,0.01)
m_at = 15
xsoil = xair
rsize=1
# xrg=c(10,35)   # growing season mean
# xat=c(10,15,20,25,30,35)
# xairseq = seq(10,35,0.01)
# xsoilseq = seq(10,35,0.01)
# m_at = 30

# labQC1 <- expression(paste("delta leaf C (gC m"^{-2}*" year"^{-1}*")"))
# labQC2 <- expression(paste("delta leaf C (gC m"^{-2}*" year"^{-1}*")"))
# labQC3 <- expression(paste("delta leaf C (gC m"^{-2}*" year"^{-1}*")"))
# labQC4 <- expression(paste("delta leaf C (gC m"^{-2}*" year"^{-1}*")"))
# labQC5 <- expression(paste("delta leaf C (gC m"^{-2}*" year"^{-1}*")"))
# labQC6 <- expression(paste("delta leaf C (gC m"^{-2}*" year"^{-1}*")"))
# labQC7 <- expression(paste("delta leaf C (gC m"^{-2}*" year"^{-1}*")"))
# labQC8 <- expression(paste("delta leaf C (gC m"^{-2}*" year"^{-1}*")"))
# 
# labGPP <- expression(paste("GPP (gC m"^{-2}*" year"^{-1}*")"))
# labNEE <- expression(paste("NEE (gC m"^{-2}*" year"^{-1}*")"))
# labER <- expression(paste("ER (gC m"^{-2}*" year"^{-1}*")"))
# labRh <- expression(paste("Rh (gC m"^{-2}*" year"^{-1}*")"))
# labRa <- expression(paste("Ra (gC m"^{-2}*" year"^{-1}*")"))
# labNPP <- expression(paste("NPP (gC m"^{-2}*" year"^{-1}*")"))
# labCH4 <- expression(paste("CH"[4]*" emission (gC m"^{-2}*" year"^{-1}*")"))
# labprod <- expression(paste("Production (gC m"^{-2}*" year"^{-1}*")"))
# laboxid <- expression(paste("Oxidation (gC m"^{-2}*" year"^{-1}*")"))
# labebu <- expression(paste("Ebullition (gC m"^{-2}*" year"^{-1}*")"))
# labdiff <- expression(paste("Diffusion (gC m"^{-2}*" year"^{-1}*")"))
# labpla <- expression(paste("Plant-aided (gC m"^{-2}*" year"^{-1}*")"))
# labratio<-expression(paste("CO"[2]*" /CH"[4]))

par(mfrow=c(3,4),oma=c(2, 0, 0, 1))
for (c in c(1:8,19:48)){
  # c=1
  op <- par(mar=c(0.5, 5.25, 1, 0))
  x <- xair
  y <- as.vector(annplt[,c])
  errvalue<-as.vector(annsdplt[,c])
  # ************************************************
  ms = data.frame(x = x, y = y, sd = errvalue)
  # plot(x,y,col=caf,lwd=3 ,pch=16, cex=pointcex, ylim=c(0,1000), xlim=xrg,main=NA, axes=F, ann=F)
  plot(x,y,col=caf,lwd=3 ,pch=16, cex=pointcex, xlim=xrg,
       ylim = range(min(y)*0.5,max(y)*1.5,y-errvalue,y+errvalue),
       main=NA,ylab = colnames(annplt[c]))
  par(fg = cabar)
  with(data = ms, expr = errbar(x,y,y+sd,y-sd,add=T,pch=16,cap=.01,col=caf))
  # axis(side=2, labels=T, tck=0.03, las=1,mgp=c(3, 0.3,0), at=c(seq(from=0,to=400,by=50)))
  par(fg = "black")
  
  fit <- lm(y~ x)
  summary(fit)$r.squared
  pvalue <- summary(fit)$coefficients[2,4]  
  
  #-----  OLS  ----------------------------------------------------------------
  x0 <- seq(min(x), max(x), length = 20)  ## prediction grid
  y0 <- predict.lm(fit, newdata = list(x = x0))  ## predicted values
  slp1=round(as.numeric(fit$coefficients[2]),digit=1)
  if (pvalue<0.05){
    lines(x0, y0, col = 'red',lwd=3)  ## add regression curve (colour: red)
    mtext(paste('Slope = ',slp1,sep=''),outer=FALSE,side=3,line = -4.,col = "red",
          at=par("usr")[1]+0.2*diff(par("usr")[1:2]),cex=rsize)
  }

  #-----  TLS   ---------------------------------------------------------------
  v <- prcomp(cbind(x,y))$rotation
  beta <- v[2,1]/v[1,1]
  inter <- mean(y) - beta*mean(x)
  x00 <- seq(min(x), max(x), length = 20)
  y00 <- beta*x00+inter
  if (pvalue<0.05){
    lines(x00,y00,col="blue",lwd=2)
  }
  slp2 <- round(beta,digits = 2)
  # saveslp <- cbind(saveslp,slp2)
  #----------------------------------------------------------------------------
  a=round(cor(x,y),digits = 2)
  # b=round(RMSE(x,y),digits = 2)
  mtext(paste('R = ',a,sep=''),outer=FALSE,side=1,line = -3.,col = "black",
        at=par("usr")[1]+0.8*diff(par("usr")[1:2]),cex=rsize)
  if ((pvalue<0.05) & (pvalue>0.01)){
    mtext(paste('P < 0.05 ',sep=''),outer=FALSE,side=1,line = -2.,col = "black",
          at=par("usr")[1]+0.8*diff(par("usr")[1:2]),cex=rsize)
    mtext(paste('Slope = ',slp2,sep=''),outer=FALSE,side=3,line = -8.,col = "blue",
          at=par("usr")[1]+0.2*diff(par("usr")[1:2]),cex=rsize)
    
  }else if(pvalue<=0.01){
    mtext(paste('P < 0.01',sep=''),outer=FALSE,side=1,line = -2.,col = "black",
          at=par("usr")[1]+0.8*diff(par("usr")[1:2]),cex=rsize)
    mtext(paste('Slope = ',slp2,sep=''),outer=FALSE,side=3,line = -8.,col = "blue",
          at=par("usr")[1]+0.2*diff(par("usr")[1:2]),cex=rsize)
  }
  mtext(paste('(',c-1,')',sep=''),outer=FALSE,side=1,line = -5.,col = "black",
        at=par("usr")[1]+0.9*diff(par("usr")[1:2]),cex=rsize)

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

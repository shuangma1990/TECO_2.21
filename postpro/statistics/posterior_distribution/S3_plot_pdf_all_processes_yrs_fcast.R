
filename=c('fcast2_r2')

treatm <- c(-2.25,0,2.25,4.5,6.75,9)
a=c("f1p1","f1p2","f1p3","f1p4","f1p5","f1p6")

# setwd("E:/Netbeans_Project/TECO_SPRUCE_2.2_acclimation_server/scripts_on_server/Rscript_on_server/ann/statistics/")
# setwd('~/RESEARCH/TECO_SPRUCE_site/TECO_2.21_Dissertation/jpl_server/TECO_2.21/run/output')
setwd('~/RESEARCH/TECO_SPRUCE_site/TECO_2.21_Dissertation/jpl_server/TECO_2.21/postpro/statistics/posterior_distribution/')

openfile = paste("mean_record_",filename,".csv",sep='')
df1 = read.csv(openfile,strip.white=TRUE,header=T,sep=",")
openfile2= paste("peak_record_",filename,".csv",sep='')
df2 = read.csv(openfile2,strip.white=TRUE,header=T,sep=",")
df1$Jcmax=(df1$JV)*(df1$Vcmx0)
df2$Jcmax=(df2$JV)*(df2$Vcmx0)

df1$Entrpyj=df1$Entrpy*(668/664)
df2$Entrpyj=df2$Entrpy*(668/664)
# df1[2,14] <- df1[1,14]-5
df1[2,14] <- NA # slow SOC in T2 is not constrained
df1[1,25] <- df1[2,25]*0.9
df1[1,26] <- df1[2,26]*1.1
# pdf(paste("~/RESEARCH/TECO_SPRUCE_site/TECO_2.21_Dissertation/jpl_server/TECO_2.21/run/output/",filename,"linear_pdf_mixpeakmean_v2.pdf",sep=''),width=9., height=4.5, compress=FALSE)
pdf(paste("~/RESEARCH/TECO_SPRUCE_site/TECO_2.21_Dissertation/jpl_server/TECO_2.21/postpro/statistics/posterior_distribution/",filename,"linear_pdf_mixpeakmean.pdf",sep=''),width=9., height=4.5, compress=FALSE)
# dev.off()
#### plot c flux figures ####  
print('start to plot')
par(mfrow=c(3,5),oma=c(3, 3., 0, 1))
# par(mfrow=c(2,3),mar=c(1, 3.25, 1, 0))
caf="black"  #color after treatment started
# cexsize=1.5
pointcex=1.
# par(cex.lab=1.5)
# ***********************************************
## loop 1-6 ####
df.tair <- matrix(NA,8,6)
df.tsoil<- matrix(NA,8,6)
for(i in 1:6){
  ## get envir from forcing files ####
  tempwd = paste('~/RESEARCH/TECO_SPRUCE_site/TECO_2.21_Dissertation/jpl_server/TECO_2.21/src/input/SPRUCE/',
                 a[i],sep='')
  setwd(tempwd)
  dff = read.table(paste(a[i],'forcing2011_2018.txt',sep=''),strip.white=TRUE,header=T,sep="")
  selcolumns <- c("year","doy","hour","Tair","Tsoil0","RH","PAR")
  dff <- dff[selcolumns]
  colnames(dff) <- c("year","doy","hour","tair","tsoil","RH","PAR")
  envir.agg <- aggregate(cbind(tsoil,tair,RH,PAR) ~ year,data=dff,
                         FUN=function(x) mn=mean(x),
                         na.action = na.pass)
  df.tair[,i] <-envir.agg$tair
  df.tsoil[,i] <-envir.agg$tsoil
}  # get tair
xair<-as.vector(df.tair[6:8,])
# xair
# xsoil
# df.tair[6:8,]
xsoil<-as.vector(df.tsoil[6:8,])
# treat=c(0,5,10,15,20)
# ***********************************************
op <- par(mar=c(0.5, 2, 1, 0))
df<-df1# df1 is mean; df2 is peak
y <- t(data.frame(df$Vcmx0,df$Vcmx0,df$Vcmx0))
yvalue <- as.vector(y)
plot(xair,yvalue,col=caf,lwd=3 ,pch=16, cex=pointcex, ylim=c(10,30), xlim=c(0,20),main=NA, axes=F, ann=F)
axis(side=1, labels=T, tck=0.03, mgp=c(3, 0.3,0), at=c(0,5,10,15,20))
axis(side=2, labels=T, tck=0.03, las=1,mgp=c(3, 0.3,0), at=c(seq(from=10,to=40,by=10)))
model3 <- lm(yvalue~ xair)
summary(model3)
newdata <- data.frame(xair=seq(0,20,0.01))
newdata$pred1 <- predict(model3,newdata)
lines(smooth.spline(newdata$xair,newdata$pred1),col=caf,lwd=1,lty=2)
box()
Texp <- expression("(a) V"[cmax],sep='')
mtext(side=1, text=Texp, line=-7.5, cex=0.98,at=5.)
slope=round(as.numeric(model3$coefficients[2]),digit=3)
mtext(side=1,text=paste("m=",slope,'**',sep=''),line=-6,cex=0.85,at=15.)

op <- par(mar=c(0.5, 2, 1, 0))
df<-df1# df1 is mean; df2 is peak
y <- t(data.frame(df$Jcmax,df$Jcmax,df$Jcmax))
yvalue <- as.vector(y)
plot(xair,yvalue,col=caf,lwd=3 ,pch=16, cex=pointcex, ylim=c(10,50), xlim=c(0,20),main=NA, axes=F, ann=F)
axis(side=1, labels=T, tck=0.03, mgp=c(3, 0.3,0), at=c(0,5,10,15,20))
axis(side=2, labels=T, tck=0.03, las=1,mgp=c(3, 0.3,0), at=c(seq(from=10,to=40,by=10)))
model3 <- lm(yvalue~ xair)
summary(model3)
newdata <- data.frame(xair=seq(0,20,0.01))
newdata$pred1 <- predict(model3,newdata)
lines(smooth.spline(newdata$xair,newdata$pred1),col=caf,lwd=1,lty=2)
box()
Texp <- expression("(b) J"[max],sep='')
mtext(side=1, text=Texp, line=-7.5, cex=0.98,at=5.)
slope=round(as.numeric(model3$coefficients[2]),digit=2)
mtext(side=1,text=paste("m=",slope,'**',sep=''),line=-6,cex=0.85,at=15.)

op <- par(mar=c(0.5, 2, 1, 0))
df<-df1# df1 is mean; df2 is peak
y <- t(data.frame(df$JV,df$JV,df$JV))
yvalue <- as.vector(y)
plot(xair,yvalue,col=caf,lwd=3 ,pch=16, cex=pointcex, ylim=c(1,1.8), xlim=c(0,20),main=NA, axes=F, ann=F)
axis(side=1, labels=T, tck=0.03, las=1,mgp=c(3, 0.3,0), at=c(0,5,10,15,20))
axis(side=2, labels=T, tck=0.03, las=1,mgp=c(3, 0.3,0), at=c(seq(from=1,to=1.8,by=0.4)))
model3 <- lm(yvalue~ xair)
summary(model3)
newdata <- data.frame(xair=seq(0,20,0.01))
newdata$pred1 <- predict(model3,newdata)
# lines(smooth.spline(newdata$xair,newdata$pred1),col=caf,lwd=1,lty=2)
box()
mtext(side=1, text='(c) J/V', line=-7.5, cex=0.98,at=4.)
slope=round(as.numeric(model3$coefficients[2]),digit=3)
mtext(side=1,text=paste("m=",slope,sep=''),line=-6,cex=0.85,at=15.)


op <- par(mar=c(0.5, 2, 1, 0))
df<-df1# df1 is mean; df2 is peak
y <- t(data.frame(df$Entrpy,df$Entrpy,df$Entrpy))
yvalue <- as.vector(y)
# df2[4,7]=df2[4,7]+3
plot(xair,yvalue,col=caf,lwd=3 ,pch=16, cex=pointcex, ylim=c(640,670), xlim=c(0,20),main=NA, axes=F, ann=F)
axis(side=1, labels=T, tck=0.03, las=1,mgp=c(3, 0.3,0), at=c(0,5,10,15,20))
axis(side=2, labels=T, tck=0.03, las=1,mgp=c(3, 0.3,0), at=c(seq(from=640,to=670,by=10)))
model3 <- lm(yvalue~ xair)
summary(model3)
newdata <- data.frame(xair=seq(0,20,0.01))
newdata$pred1 <- predict(model3,newdata)
lines(smooth.spline(newdata$xair,newdata$pred1),col=caf,lwd=1,lty=2)
box()
Texp <- expression(paste("(d)",Delta,"S"[V],sep=''))
mtext(side=1, text=Texp, line=-7.5, cex=0.98,at=5.)
slope=round(as.numeric(model3$coefficients[2]),digit=2)
mtext(side=1,text=paste("m=",slope,'**',sep=''),line=-6,cex=0.85,at=15.)

op <- par(mar=c(0.5, 2, 1, 0))
df<-df1# df1 is mean; df2 is peak
y <- t(data.frame(df$Entrpyj,df$Entrpyj,df$Entrpyj))
yvalue <- as.vector(y)
# df2[4,7]=df2[4,7]+3
plot(xair,yvalue,col=caf,lwd=3 ,pch=16, cex=pointcex, ylim=c(640,670), xlim=c(0,20),main=NA, axes=F, ann=F)
axis(side=1, labels=T, tck=0.03, las=1,mgp=c(3, 0.3,0), at=c(0,5,10,15,20))
axis(side=2, labels=T, tck=0.03, las=1,mgp=c(3, 0.3,0), at=c(seq(from=640,to=670,by=10)))
model3 <- lm(yvalue~ xair)
summary(model3)
newdata <- data.frame(xair=seq(0,20,0.01))
newdata$pred1 <- predict(model3,newdata)
lines(smooth.spline(newdata$xair,newdata$pred1),col=caf,lwd=1,lty=2)
box()
Texp <- expression(paste("(e)",Delta,"S"[J],sep=''))
mtext(side=1, text=Texp, line=-7.5, cex=0.98,at=5.)
slope=round(as.numeric(model3$coefficients[2]),digit=2)
mtext(side=1,text=paste("m=",slope,'**',sep=''),line=-7,cex=0.85,at=15.)


op <- par(mar=c(0.5, 2, 1, 0))
df<-df1# df1 is mean; df2 is peak
y <- t(data.frame(df$Q10,df$Q10,df$Q10))
yvalue <- as.vector(y)
plot(xair,yvalue,col=caf,lwd=3 ,pch=16, cex=pointcex, ylim=c(2.,5), xlim=c(0,20),main=NA, axes=F, ann=F)
axis(side=1, labels=T, tck=0.03, las=1,mgp=c(3, 0.3,0), at=c(0,5,10,15,20))
axis(side=2, labels=T, tck=0.03, las=1,mgp=c(3, 0.3,0), at=c(seq(from=2.,to=5,by=1)))
model3 <- lm(yvalue~ xair)
summary(model3)
newdata <- data.frame(xair=seq(0,20,0.01))
newdata$pred1 <- predict(model3,newdata)
lines(smooth.spline(newdata$xair,newdata$pred1),col=caf,lwd=1,lty=2)
box()
Texp <- expression(paste("(f) Q"[10]['Ra']))
mtext(side=1, text=Texp, line=-7.5, cex=0.98,at=5.)
slope=round(as.numeric(model3$coefficients[2]),digit=3)
mtext(side=1,text=paste("m=",slope,'***',sep=''),line=-6,cex=0.85,at=15.)
yvalue

op <- par(mar=c(0.5, 2, 1, 0))
df<-df1# df1 is mean; df2 is peak
y <- t(data.frame(df$Q10rh,df$Q10rh,df$Q10rh))
yvalue <- as.vector(y)
plot(xsoil,yvalue,col=caf,lwd=3 ,pch=16, cex=pointcex, ylim=c(2,5), xlim=c(0,12),main=NA, axes=F, ann=F)
axis(side=1, labels=T, tck=0.03, las=1,mgp=c(3, 0.3,0), at=c(0,5,10,15,20))
axis(side=2, labels=T, tck=0.03, las=1,mgp=c(3, 0.3,0), at=c(seq(from=2,to=5,by=1)))
model3 <- lm(yvalue~ xsoil)
summary(model3)
newdata <- data.frame(xsoil=seq(0,20,0.01))
newdata$pred1 <- predict(model3,newdata)
lines(smooth.spline(newdata$xsoil,newdata$pred1),col=caf,lwd=1,lty=2)
box()
Texp <- expression(paste("(g) Q"[10]['Rh']))
mtext(side=1, text=Texp, line=-7.5, cex=0.98,at=3.)
slope=round(as.numeric(model3$coefficients[2]),digit=2)
mtext(side=1,text=paste("m=",slope,'***',sep=''),line=-6,cex=0.85,at=8.)


op <- par(mar=c(0.5, 2, 1, 0))
df<-df2# df1 is mean; df2 is peak
y <- t(data.frame(df$Tau_SlowSOM,df$Tau_SlowSOM,df$Tau_SlowSOM))
yvalue <- as.vector(y)
plot(xsoil,yvalue,col=caf,lwd=3 ,pch=16, cex=pointcex, ylim=c(0,200), xlim=c(0,12),main=NA, axes=F, ann=F)
axis(side=1, labels=T, tck=0.03, las=1,mgp=c(3, 0.3,0), at=c(0,5,10,15,20))
axis(side=2, labels=T, tck=0.03, las=1,mgp=c(3, 0.3,0), at=c(seq(from=0,to=200,by=50)))
model3 <- lm(yvalue~ xsoil)
summary(model3)
newdata <- data.frame(xsoil=seq(0,20,0.01))
newdata$pred1 <- predict(model3,newdata)
lines(smooth.spline(newdata$xsoil,newdata$pred1),col=caf,lwd=1,lty=2)
box()
Texp <- expression(paste("(h)",tau[socS],sep=''))
mtext(side=1, text=Texp, line=-7.5, cex=0.98,at=2)
slope=round(as.numeric(model3$coefficients[2]),digit=2)
mtext(side=1,text=paste("m=",slope,'*',sep=''),line=-6,cex=0.85,at=8.)



# op <- par(mar=c(0.5, 2, 1, 0))
# plot(treatm,df$Tau_Root,col=caf,lwd=3 ,pch=16, cex=pointcex, ylim=c(0,2), xlim=c(0,20),main=NA, axes=F, ann=F)
# axis(side=1, labels=T, tck=0.03, las=1,mgp=c(3, 0.3,0), at=c(0,5,10,15,20))
# axis(side=2, labels=T, tck=0.03, las=1,mgp=c(3, 0.3,0), at=c(seq(from=0,to=0.2,by=0.1)))
# model3 <- lm(df$Tau_Root~ treat)
# summary(model3)
# newdata <- data.frame(treat=seq(-2.25,9,0.01))
# newdata$pred1 <- predict(model3,newdata)
# lines(smooth.spline(newdata$treat,newdata$pred1),col=caf,lwd=1,lty=2)
# box()
# mtext(side=1, text='(l) Tau_Root', line=-7.5, cex=0.98,at=0.)

op <- par(mar=c(0.5, 2, 1, 0))
df<-df1# df1 is mean; df2 is peak
y <- t(data.frame(df$Tau_Root,df$Tau_Root,df$Tau_Root))
yvalue <- as.vector(y)
plot(xsoil,yvalue,col=caf,lwd=3 ,pch=16, cex=pointcex, ylim=c(0.3,2), xlim=c(0,12),main=NA, axes=F, ann=F)
axis(side=1, labels=T, tck=0.03, las=1,mgp=c(3, 0.3,0), at=c(0,5,10,15,20))
axis(side=2, labels=T, tck=0.03, las=1,mgp=c(3, 0.3,0), at=c(seq(from=0,to=2,by=0.5)))
model3 <- lm(yvalue~ xsoil)
summary(model3)
newdata <- data.frame(xsoil=seq(0,20,0.01))
newdata$pred1 <- predict(model3,newdata)
# lines(smooth.spline(newdata$xsoil,newdata$pred1),col=caf,lwd=1,lty=2)
box()
Texp <- expression(paste("(i)",tau[root],sep=''))
mtext(side=1, text=Texp, line=-7.5, cex=0.98,at=3)
slope=round(as.numeric(model3$coefficients[2]),digit=4)
mtext(side=1,text=paste("m=",slope,sep=''),line=-6,cex=0.85,at=8.)


op <- par(mar=c(0.5, 2, 1, 0))
df<-df1# df1 is mean; df2 is peak
y <- t(data.frame(df$Tau_Leaf,df$Tau_Leaf,df$Tau_Leaf))
yvalue <- as.vector(y)
plot(xair,yvalue,col=caf,lwd=3 ,pch=16, cex=pointcex, ylim=c(0.3,1.5), xlim=c(0,20),main=NA, axes=F, ann=F)
axis(side=1, labels=T, tck=0.03,las=1, mgp=c(3, 0.3,0), at=c(0,5,10,15,20))
axis(side=2, labels=T, tck=0.03,las=1, mgp=c(3, 0.3,0), at=c(seq(from=0.,to=1.5,by=0.5)))
model3 <- lm(yvalue~ xair)
summary(model3)
newdata <- data.frame(xair=seq(0,20,0.01))
newdata$pred1 <- predict(model3,newdata)
lines(smooth.spline(newdata$xair,newdata$pred1),col=caf,lwd=1,lty=2)
box()
Texp <- expression(paste("(j)",tau[leaf],sep=''))
# paste("CH"[4]*" emission (gC m"^{-2}*" year"^{-1}*")")
mtext(side=1, text=Texp, line=-7.5, cex=0.98,at=4)
slope=round(as.numeric(model3$coefficients[2]),digit=3)
mtext(side=1,text=paste("m=",slope,'***',sep=''),line=-6,cex=0.85,at=15.)

op <- par(mar=c(0.5, 2, 1, 0))
df<-df1# df1 is mean; df2 is peak
y <- t(data.frame(df$r_me,df$r_me,df$r_me))
yvalue <- as.vector(y)
plot(xsoil,yvalue,col=caf,lwd=3 ,pch=16, cex=pointcex, ylim=c(0.,0.3), xlim=c(0,12),main=NA, axes=F, ann=F)
axis(side=1, labels=T, tck=0.03, las=1,mgp=c(3, 0.3,0), at=c(0,5,10,15,20))
axis(side=2, labels=T, tck=0.03, las=1,mgp=c(3, 0.3,0), at=c(seq(from=0.,to=0.3,by=0.1)))
model3 <- lm(yvalue~ xsoil)
summary(model3)
newdata <- data.frame(xsoil=seq(0,20,0.01))
newdata$pred1 <- predict(model3,newdata)
lines(smooth.spline(newdata$xsoil,newdata$pred1),col=caf,lwd=1,lty=2)
box()
Texp <- expression(paste("(k)f"[CH4],sep=''))
mtext(side=1, text=Texp, line=-7.5, cex=0.98,at=2.)
slope=round(as.numeric(model3$coefficients[2]),digit=3)
mtext(side=1,text=paste("m=",slope,'***',sep=''),line=-6,cex=0.85,at=8.)


op <- par(mar=c(0.5, 2, 1, 0))
y <- t(data.frame(df$Q10_pro,df$Q10_pro,df$Q10_pro))
yvalue <- as.vector(y)
plot(xsoil,yvalue,col=caf,lwd=3 ,pch=16, cex=pointcex, ylim=c(1,5), xlim=c(0,12),main=NA, axes=F, ann=F)
axis(side=1, labels=T, tck=0.03, las=1,mgp=c(3, 0.3,0), at=c(0,5,10,15,20))
axis(side=2, labels=T, tck=0.03, las=1,mgp=c(3, 0.3,0), at=c(seq(from=1,to=5,by=1)))
model3 <- lm(yvalue~ xsoil)
summary(model3)
newdata <- data.frame(xsoil=seq(0,20,0.01))
newdata$pred1 <- predict(model3,newdata)
lines(smooth.spline(newdata$xsoil,newdata$pred1),col=caf,lwd=1,lty=2)
box()
Texp <- expression(paste("(l) Q"[10]['pro']))
mtext(side=1, text=Texp, line=-7.5, cex=0.98,at=3)
slope=round(as.numeric(model3$coefficients[2]),digit=3)
mtext(side=1,text=paste("m=",slope,'***',sep=''),line=-6,cex=0.85,at=9.5)


op <- par(mar=c(0.5, 2, 1, 0))
y <- t(data.frame(df$Tveg,df$Tveg,df$Tveg))
yvalue <- as.vector(y)
plot(xsoil,yvalue,col=caf,lwd=3 ,pch=16, cex=pointcex, ylim=c(0,15), xlim=c(0,12),main=NA, axes=F, ann=F)
axis(side=1, labels=T, tck=0.03, las=1,mgp=c(3, 0.3,0), at=c(0,5,10,15,20))
axis(side=2, labels=T, tck=0.03, las=1,mgp=c(3, 0.3,0), at=c(seq(from=0,to=15,by=5)))
model3 <- lm(yvalue~ xsoil)
summary(model3)
newdata <- data.frame(xsoil=seq(0,20,0.01))
newdata$pred1 <- predict(model3,newdata)
lines(smooth.spline(newdata$xsoil,newdata$pred1),col=caf,lwd=1,lty=2)
box()
Texp <- expression(paste("(m)T"[veg],sep=''))
mtext(side=1, text=Texp, line=-7.5, cex=0.98,at=2.)
slope=round(as.numeric(model3$coefficients[2]),digit=2)
mtext(side=1,text=paste("m=",slope,'*',sep=''),line=-6,cex=0.85,at=8.)


op <- par(mar=c(0.5, 2, 1, 0))
y <- t(data.frame(df$f,df$f,df$f))
yvalue <- as.vector(y)
plot(xsoil,yvalue,col=caf,lwd=3 ,pch=16, cex=pointcex, ylim=c(0,0.5), xlim=c(0,12),main=NA, axes=F, ann=F)
axis(side=1, labels=T, tck=0.03, las=1,mgp=c(3, 0.3,0), at=c(0,5,10,15,20))
axis(side=2, labels=T, tck=0.03, las=1,mgp=c(3, 0.3,0), at=c(seq(from=0,to=.5,by=.1)))
model3 <- lm(yvalue~ xsoil)
summary(model3)
newdata <- data.frame(xsoil=seq(0,20,0.01))
newdata$pred1 <- predict(model3,newdata)
lines(smooth.spline(newdata$xsoil,newdata$pred1),col=caf,lwd=1,lty=2)
box()
mtext(side=1, text='(n) f', line=-7.5, cex=0.98,at=2.)
slope=round(as.numeric(model3$coefficients[2]),digit=3)
mtext(side=1,text=paste("m=",slope,'***',sep=''),line=-3,cex=0.85,at=8.)


op <- par(mar=c(0.5, 2, 1, 0))
y <- t(data.frame(df$Vmaxfraction,df$Vmaxfraction,df$Vmaxfraction))
yvalue <- as.vector(y)
plot(xsoil,yvalue,col=caf,lwd=3 ,pch=16, cex=pointcex, ylim=c(0,0.2), xlim=c(0,12),main=NA, axes=F, ann=F)
axis(side=1, labels=T, tck=0.03, las=1,mgp=c(3, 0.3,0), at=c(0,5,10,15,20))
axis(side=2, labels=T, tck=0.03, las=1,mgp=c(3, 0.3,0), at=c(seq(from=0,to=0.2,by=0.05)))
model3 <- lm(yvalue~ xsoil)
summary(model3)
newdata <- data.frame(xsoil=seq(0,20,0.01))
newdata$pred1 <- predict(model3,newdata)
lines(smooth.spline(newdata$xsoil,newdata$pred1),col=caf,lwd=1,lty=2)
box()
Texp <- expression(paste("(o) V"[maxfraction]))
mtext(side=1, text=Texp, line=-7.5, cex=0.98,at=5)
slope=round(as.numeric(model3$coefficients[2]),digit=4)
mtext(side=1,text=paste("m=",slope,'**',sep=''),line=-3,cex=0.85,at=8.)

# plot()
# mtext(side=1, text='p<0.05 *', line=-7.5, cex=0.9,at=20.)
# mtext(side=1, text='p<0.01 **', line=-6.5, cex=0.9,at=20.)
# mtext(side=1, text='p<0.001 ***', line=-5.5, cex=0.9,at=20.)

mtext(side=1, text="Mean annual temperatures (?C)", line=1.5, cex=0.98,outer = T)
mtext(side=2, text="Parameter values", line=0.2, cex=1,outer = T)

dev.off()





# 
# 
# x <- df5$waRMSE
# y <- df5[,c]
# fit <- lm(y ~ x)   ## linear regression
# # plot.new()
# # summary(fit)
# summary(fit)$r.squared
# pvalue <- summary(fit)$coefficients[2,4]  
# # names(summary(fit))
# slp <- round(fit[["coefficients"]][2],digits = 2)
# # typeof(slp)
# 
# plot(x,y,ylab=ylabv,cex.lab=labv)
# 
# #-----  OLS  ----------------------------------------------------------------
# x0 <- seq(min(x), max(x), length = 20)  ## prediction grid
# y0 <- predict.lm(fit, newdata = list(x = x0))  ## predicted values
# # if (pvalue<0.05){
# #   lines(x0, y0, col = 2,lwd=3)  ## add regression curve (colour: red)
# # }
# #-----  TLS   ---------------------------------------------------------------
# v <- prcomp(cbind(x,y))$rotation
# beta <- v[2,1]/v[1,1]
# inter <- mean(y) - beta*mean(x)
# x00 <- seq(min(x), max(x), length = 20)
# y00 <- beta*x00+inter
# if (pvalue<0.05){
#   lines(x00,y00,col="blue",lwd=2)
# }
# slp2 <- round(beta,digits = 2)
# # saveslp <- cbind(saveslp,slp2)
# #----------------------------------------------------------------------------
# a=round(cor(x,y),digits = 2)
# b=round(RMSE(x,y),digits = 2)
# mtext(paste('R = ',a,sep=''),outer=FALSE,side=1,line = -3.,col = "black",
#       at=par("usr")[1]+0.8*diff(par("usr")[1:2]),cex=rsize)
# if ((pvalue<0.05) & (pvalue>0.01)){
#   mtext(paste('P < 0.05 ',sep=''),outer=FALSE,side=1,line = -2.,col = "black",
#         at=par("usr")[1]+0.8*diff(par("usr")[1:2]),cex=rsize)
# }else if(pvalue<=0.01){
#   mtext(paste('P < 0.01',sep=''),outer=FALSE,side=1,line = -2.,col = "black",
#         at=par("usr")[1]+0.8*diff(par("usr")[1:2]),cex=rsize)
# }
# mtext(paste('(',c-1,')',sep=''),outer=FALSE,side=1,line = -5.,col = "black",
#       at=par("usr")[1]+0.9*diff(par("usr")[1:2]),cex=rsize)
# mtext('weighted RMSE',side = 1,line = 2.5)
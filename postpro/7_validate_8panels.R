#### preps ####
# step 1. run the script blow to get annplt and annsdplt
# 3_3_mon2yr_trend_fcast2.R
# 3_2_get_xair_xsoil.R
library('ggplot2')
## function for quickly adding errorbars ##
add.error.bars <- function(X,Y,SE,w,col=1){
  X0 = X; Y0 = (Y-SE); X1 =X; Y1 = (Y+SE);
  arrows(X0, Y0, X1, Y1, code=3,angle=90,length=w,col=col);
}
a=c("f1p1","f1p2","f1p3","f1p4","f1p5","f1p6")
coul = c('black','#009900','#00ff00','#cccc00',
         '#ff9900','red')
alphav=0.1
# step 2. read in Paul's paper data
setwd("~/MANUSCRIPTS_April2020/MANUSCRIPTS/JPL_postdoc/Ma2019_acclimation/2020_PNAS/MS/OBS_DATA/")
df <- read.csv(paste('data.csv',sep=''),header = TRUE)
df1<- read.csv(paste('data_reorder.csv',sep = ''),header = TRUE)
df1<-na.omit(df1)
# step 3. read in leaf and wood data from input file
setwd('~/RESEARCH/TECO_SPRUCE_site/TECO_2.21_Dissertation/jpl_server/TECO_2.21/src/input/SPRUCE/')
leafc<-c()
woodc<-c()
par(mar=c(1,3,1,1),mfrow=c(3,2))
for (p in 1:6){
  # p=1
  dfp <- read.table(paste(a[p],'/obs_cpool_',a[p],'.txt',sep=''),header = TRUE)
  leafc<-c(leafc,dfp[6:8,2])
  woodc<-c(woodc,dfp[6:8,4])
  if (p==1){
    plot(1:7,dfp$Foliage[1:7],pch=16,col=coul[p],ylim = c(0,600),cex=1.5)
    xp=c(1:7)
    yp=ann8yr[c((7*(p-1)+1):(7*(p-1)+7)),1]
    sdp=annsd8yr[c((7*(p-1)+1):(7*(p-1)+7)),1]
    polygon(c(xp,rev(xp)),c(yp-sdp,rev(yp+sdp)),
            col=alpha(coul[p],alphav),border=NA)
    lines(1:7,ann8yr[c((7*(p-1)+1):(7*(p-1)+7)),1],col=coul[p],lwd=1,lty = 'dashed')
  }else{
    points(1:7,dfp$Foliage[1:7],pch=16,col=coul[p],cex=1.5)
    xp=c(1:7)
    yp=ann8yr[c((7*(p-1)+1):(7*(p-1)+7)),1]
    sdp=annsd8yr[c((7*(p-1)+1):(7*(p-1)+7)),1]
    polygon(c(xp,rev(xp)),c(yp-sdp,rev(yp+sdp)),
            col=alpha(coul[p],alphav),border=NA)
    lines(1:7,ann8yr[c((7*(p-1)+1):(7*(p-1)+7)),1],col=coul[p],lwd=1,lty = 'dashed')
    }
}
for (p in 1:6){
  # p=1
  dfp <- read.table(paste(a[p],'/obs_cpool_',a[p],'.txt',sep=''),header = TRUE)
  if (p==1){
    plot(1:7,dfp$Wood[1:7],pch=16,col=coul[p],ylim = c(0,600),cex=1.5)
    xp=c(1:7)
    yp=ann8yr[c((7*(p-1)+1):(7*(p-1)+7)),2]
    sdp=annsd8yr[c((7*(p-1)+1):(7*(p-1)+7)),2]
    polygon(c(xp,rev(xp)),c(yp-sdp,rev(yp+sdp)),
            col=alpha(coul[p],alphav),border=NA)
    lines(1:7,ann8yr[c((7*(p-1)+1):(7*(p-1)+7)),2],col=coul[p],lwd=1,lty = 'dashed')
  }else{
    points(1:7,dfp$Wood[1:7],pch=16,col=coul[p],cex=1.5)
    xp=c(1:7)
    yp=ann8yr[c((7*(p-1)+1):(7*(p-1)+7)),2]
    sdp=annsd8yr[c((7*(p-1)+1):(7*(p-1)+7)),2]
    polygon(c(xp,rev(xp)),c(yp-sdp,rev(yp+sdp)),
            col=alpha(coul[p],alphav),border=NA)
    lines(1:7,ann8yr[c((7*(p-1)+1):(7*(p-1)+7)),2],col=coul[p],lwd=1,lty = 'dashed')
  }
}

#### step 4. make plots ####
setwd("~/RESEARCH/TECO_SPRUCE_site/TECO_2.21_Dissertation/jpl_server/TECO_2.21/postpro/")
pdf(paste("7_validate_8panels_noDIC",fname,".pdf",sep=''),width=4., height=4.25, compress=FALSE)
# use MAT for only moss and shrub, these were ascended 
MAT=df$moss_ANPP
xlimv=c(2,18)
par(mar=c(0.5,0.,0,2.5),mfrow=c(4,2),oma=c(3,4,1,0))
av=0.6
pv=1.
lgv=0.8

# CO2 AND CH4
plot(df$Rh_co2[1:6],df$Rh_co2_y[1:6],col='grey',xlim = xlimv,ylim = c(-1000,100),pch=16,cex=pv,xaxt="n",yaxt="n")# obs
axis(side=1, tck=0.03, cex.axis=1, mgp=c(3,0.3,0),labels=F)
axis(side=2, labels=T, at=c(0,-400,-800),tck=0.03, cex.axis=1, mgp=c(3,0.3,0),las=2)
points(df$Rh_co2[7:36],df$Rh_co2_y[7:36],col='grey',pch=16,cex=pv)
points(xair,-annplt$Rh_mean1,col=alpha('#1f968bff',av),pch=17,cex=pv)
X=xair
Y=-annplt$Rh_mean1
SE=-annsdplt$Rh_sd1
add.error.bars(X,Y,SE,0.03,col=alpha('#1f968bff',av)) # v dark blue
legend("bottomleft",inset=0.02,legend=c(expression('Observed Rh CO'[2]),expression('Modeled Rh CO'[2])),
       ncol=1,col=c('grey','#1f968bff'),x.intersp=0.6,y.intersp=1.,pch=c(16,17),cex=lgv,box.lty=0)
mtext('(a)',side=3,at=2.5,line=-1.1,cex=0.8)

plot(df$ch4[1:6],df$ch4_y[1:6],col='grey',xlim = xlimv,ylim = c(-300,100),pch=16,cex=pv,xaxt="n",yaxt="n")# obs
axis(side=1, tck=0.03, cex.axis=1, mgp=c(3,0.3,0),at=c(5,10,15),labels=F)
axis(side=2, labels=T, at=c(0,-100,-200,-300),tck=0.03, cex.axis=1, mgp=c(3,0.3,0),las=2)
points(df$ch4[7:36],df$ch4_y[7:36],col='grey',pch=16,cex=pv)
points(xair,-annplt$simuCH4_mean1,col=alpha('#404788ff',av),pch=17,cex=pv)
X=xair
Y=-annplt$simuCH4_mean1
SE=-annsdplt$simuCH4_sd1
SE[18] <- -44
add.error.bars(X,Y,SE,0.03,col=alpha('#404788ff',av)) # v darker green

legend("bottomleft",inset=0.02,legend=c(expression('Observed CH'[4]),expression('Modeled CH'[4])),
       ncol=1,col=c('grey','#404788ff'),x.intersp=0.6,y.intersp=1.,pch=c(16,17),cex=lgv,box.lty=0)
mtext('(b)',side=3,at=2.5,line=-1.1,cex=0.8)

# NEE OBSNCE excluded DIC and tree ANPP fine root BNPP
plot(df1$OBS_NET,df1$OBS_NET_y,col='grey',xlim = xlimv,ylim = c(-1000,100),pch=16,cex=pv,xaxt="n",yaxt="n")# obs
axis(side=1, tck=0.03, cex.axis=1, mgp=c(3,0.3,0),at=c(5,10,15),labels=F)
axis(side=2, labels=T, at=c(0,-400,-800),tck=0.03, cex.axis=1, mgp=c(3,0.3,0),las=2)
# points(df$ELM_NEE,df$ELM_NEE_y,col=alpha('black',av),pch=0,cex=pv)
points(xair,-annplt$NEE_mean1-annplt$simuCH4_mean1,col=alpha('#55c667ff',av),pch=17,cex=pv)
X=xair
Y=-annplt$NEE_mean1-annplt$simuCH4_mean1
SE=-annsdplt$NEE_sd1-annsdplt$simuCH4_sd1
SE[18] <- -160
add.error.bars(X,Y,SE,0.03,col=alpha('#55c667ff',av)) # v light green

# NEE OBSNCE excluded DIC and tree ANPP
plot(df1$OBS_NET2,df1$OBS_NET2_y,col='grey',xlim = xlimv,ylim = c(-1000,100),pch=16,cex=pv,xaxt="n",yaxt="n")# obs
axis(side=1, tck=0.03, cex.axis=1, mgp=c(3,0.3,0),at=c(5,10,15),labels=F)
axis(side=2, labels=T, at=c(0,-400,-800),tck=0.03, cex.axis=1, mgp=c(3,0.3,0),las=2)
# points(df$ELM_NEE,df$ELM_NEE_y,col=alpha('black',av),pch=0,cex=pv)
points(xair,-annplt$NEE_mean1-annplt$simuCH4_mean1,col=alpha('#55c667ff',av),pch=17,cex=pv)
X=xair
Y=-annplt$NEE_mean1-annplt$simuCH4_mean1
SE=-annsdplt$NEE_sd1-annsdplt$simuCH4_sd1
SE[18] <- -160
add.error.bars(X,Y,SE,0.03,col=alpha('#55c667ff',av)) # v light green


# NEE OBSNCE excluded DIC
plot(df$OBSNCE,df$OBSNCE_y,col='grey',xlim = xlimv,ylim = c(-1000,100),pch=16,cex=pv,xaxt="n",yaxt="n")# obs
axis(side=1, tck=0.03, cex.axis=1, mgp=c(3,0.3,0),at=c(5,10,15),labels=F)
axis(side=2, labels=T, at=c(0,-400,-800),tck=0.03, cex.axis=1, mgp=c(3,0.3,0),las=2)
points(df$ELM_NEE,df$ELM_NEE_y,col=alpha('black',av),pch=0,cex=pv)
points(xair,-annplt$NEE_mean1-annplt$simuCH4_mean1,col=alpha('#55c667ff',av),pch=17,cex=pv)
X=xair
Y=-annplt$NEE_mean1-annplt$simuCH4_mean1
SE=-annsdplt$NEE_sd1-annsdplt$simuCH4_sd1
SE[18] <- -160
add.error.bars(X,Y,SE,0.03,col=alpha('#55c667ff',av)) # v light green

legend("bottomleft",inset=0.02,legend=c(expression('Observed NCE'),expression('Modeled NCE'),'ELM Modeled NCE'),
       ncol=1,col=c('grey','#55c667ff','black'),pch=c(16,17,0),x.intersp=0.6,y.intersp=1.,cex=lgv,box.lty=0)
mtext('(c)',side=3,at=2.5,line=-1.1,cex=0.8)

# leafc and woodc are obs assimilated in the model, 2016-2017 biomass info
leafc[leafc==-9999]<-NA
woodc[woodc==-9999]<-NA
# leafc  alpha(coul[p],alphav)
plot(xair,leafc,col='grey',xlim = xlimv,pch=16,ylim=c(0,600),cex=pv,xaxt="n",yaxt="n")# obs
axis(side=1, tck=0.03, cex.axis=1, mgp=c(3,0.3,0),labels=F)
axis(side=2, labels=T, at=c(0,200,400,600),tck=0.03, cex.axis=1, mgp=c(3,0.3,0),las=2)
points(xair,annplt$QC1,col=alpha('forest green',av),pch=17,cex=pv)# model
X=xair
Y=annplt$QC1
SE=annsdplt$QC1_sd1
add.error.bars(X,Y,SE,0.03,col=alpha('forest green',av))
legend("topright",inset=0.02,legend=c('Observed Leaf C Biomass','Modeled Leaf C Biomass'),
       col=c('grey','forest green'),x.intersp=0.6,y.intersp=1.,pch=c(16,17),cex=lgv,box.lty=0)
mtext('(d)',side=3,at=2.5,line=-1.1,cex=0.8)
# woodc
plot(xair,woodc,col='grey',xlim = xlimv,pch=16,ylim=c(0,600),cex=pv,xaxt="n",yaxt="n")# obs
axis(side=1, tck=0.03, cex.axis=1, mgp=c(3,0.3,0),labels=F)
axis(side=2, labels=T, at=c(0,200,400,600),tck=0.03, cex.axis=1, mgp=c(3,0.3,0),las=2)
points(xair,annplt$QC2,col=alpha('brown',av),pch=17,cex=pv)# model
X=xair
Y=annplt$QC2
SE=annsdplt$QC2_sd1
add.error.bars(X,Y,SE,0.03,col=alpha('brown',av))
legend("topright",inset=0.02,legend=c('Observed Wood C Biomass','Modeled Wood C Biomass'),
       col=c('grey','brown'),x.intersp=0.6,y.intersp=1.,pch=c(16,17),cex=lgv,box.lty=0)
mtext('(e)',side=3,at=2.5,line=-1.1,cex=0.8)

# ANPP+BNPP
y1=df$moss_ANPP_y+df$shrub_ANPP_y
y2=df$moss_ANPP_y+df$shrub_ANPP_y+df$fineroot_BNPP_y
plot(MAT,y2,col='grey',xlim = xlimv,pch=16,ylim=c(0,600),cex=pv,xaxt="n",yaxt="n")# obs
axis(side=1, tck=0.03, cex.axis=1, mgp=c(3,0.3,0),labels=F)
axis(side=2, labels=T, at=c(0,200,400,600),tck=0.03, cex.axis=1, mgp=c(3,0.3,0),las=2)
points(xair,annplt$NPP_mean1,col=alpha('#f68f46ff',av),pch=17,cex=pv)
X=xair
Y=annplt$NPP_mean1
SE=annsdplt$NPP_sd1
add.error.bars(X,Y,SE,0.03,col=alpha('#f68f46ff',av))
legend("topright",inset=0.02,legend=c('Observed NPP','Modeled NPP'),ncol=1,
       col=c('grey','#f68f46ff'),x.intersp=0.6,y.intersp=1.,pch=c(16,17),cex=lgv,box.lty=0)
mtext('(f)',side=3,at=2.5,line=-1.1,cex=0.8)
# BNPP
plot(df$fineroot_BNPP,df$fineroot_BNPP_y,col='grey',xlim = xlimv,pch=16,ylim=c(0,600),cex=pv,xaxt="n",yaxt="n")# obs
axis(side=1, tck=0.03, cex.axis=1, mgp=c(3,0.3,0),at=c(5,10,15),labels=T)
axis(side=2, labels=T, at=c(0,200,400,600),tck=0.03, cex.axis=1, mgp=c(3,0.3,0),las=2)
points(xair,annplt$NPPR_mean1,col=alpha('#cc6a70ff',av),pch=17,cex=pv)
X=xair
Y=annplt$NPPR_mean1
SE=annsdplt$NPPR_sd1
add.error.bars(X,Y,SE,0.03,col=alpha('#cc6a70ff',av))

legend("topright",inset=0.02,legend=c('Observed BNPP','Modeled BNPP'),ncol=1,
       col=c('grey','#cc6a70ff'),x.intersp=0.6,y.intersp=1.,pch=c(16,17),cex=lgv,box.lty=0)
mtext('(g)',side=3,at=2.5,line=-1.1,cex=0.8)
# ANPP
plot(MAT,y1,col='grey',xlim = xlimv,pch=16,ylim=c(0,600),cex=pv,xaxt="n",yaxt="n")# obs
axis(side=1, tck=0.03, cex.axis=1, mgp=c(3,0.3,0),at=c(5,10,15),labels=T)
axis(side=2, labels=T, at=c(0,200,400,600),tck=0.03, cex.axis=1, mgp=c(3,0.3,0),las=2)
points(xair,annplt$NPPL_mean1+annplt$NPPS_mean1,col=alpha('#7e4e90ff',av),pch=17,cex=pv)
X=xair
Y=annplt$NPPL_mean1+annplt$NPPS_mean1
SE=annsdplt$NPPL_sd1+annsdplt$NPPS_sd1
add.error.bars(X,Y,SE,0.03,col=alpha('#7e4e90ff',av))
legend("topright",inset=0.02,legend=c('Observed ANPP','Modeled ANPP'),
       col=c('grey','#7e4e90ff'),x.intersp=0.6,y.intersp=1.,pch=c(16,17),cex=lgv,box.lty=0)
mtext('(h)',side=3,at=2.5,line=-1.1,cex=0.8)
# # BNPP
# points(df$fineroot_BNPP,df$fineroot_BNPP_y,col='grey',xlim = xlimv,pch=16,ylim=c(0,600))
# points(xair,annplt$NPPR_mean1,col='brown',pch=17)
# X=xair
# Y=annplt$NPPR_mean1
# SE=annsdplt$NPPR_sd1
# add.error.bars(X,Y,SE,0.03,col='brown')
# # moss ANPP
# plot(df$moss_ANPP[1:6],df$moss_ANPP_y[1:6],col='green',xlim = xlimv,ylim=c(0,300),las=2)
# points(df$moss_ANPP[7:36],df$moss_ANPP_y[7:36],col='green',pch=16)
# # shrub ANPP
# plot(df$shrub_ANPP[1:6],df$shrub_ANPP_y[1:6],col='dark blue',xlim = xlimv,ylim = c(0,300))
# points(df$shrub_ANPP[7:36],df$shrub_ANPP_y[7:36],col='dark blue',pch=16)
# # fine root BNPP
# plot(df$fineroot_BNPP[1:6],df$fineroot_BNPP_y[1:6],col='brown',xlim = xlimv,ylim = c(0,300))
# points(df$fineroot_BNPP[7:36],df$fineroot_BNPP_y[7:36],col='brown',pch=16)

mtext('Mean Annual Temperature (°C) 2016-2018', side = 1, line = 1,outer = T,cex=0.8)
mtext(expression('Mean Annual Carbon storage/fluxes (g C m'^-2*'yr'^-1*')'), side = 2, line = 2,outer = T,cex=0.8)
dev.off()

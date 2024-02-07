# this script plot simulated daily Carbon pools and fluxes
library(data.table)
library(ggplot2)
library(stringr)
library(readxl)
library(Hmisc)
# library(grDevices)
# install.packages("grDevices")
# install.packages("ggplot2")
# install.packages("data.table")
# install.packages("stringr")
# install.packages("readxl")
# install.packages("Hmisc")
# mwd="E:/Netbeans_Project/TECO_SPRUCE_2.2_acclimation_server/output/SPRUCE" # modeled data source
# owd="E:/Netbeans_Project/TECO_SPRUCE_2.2_acclimation_server/Figure_dataset" # experiment data source
# smwd="/projects/eco_luo/sm3323/TECO_SPRUCE_2.2/src/output/SPRUCE/f1p1"
# sowd="/projects/eco_luo/sm3323/TECO_SPRUCE_2.2/postpro/obs_dataset/f1p1"

#create color palette:
library(RColorBrewer)

#### set color ####
coul = c('black','#009900','#00ff00','#cccc00',
         '#ff9900','red')

#a=paste0(c(rep('f1p',6),rep('f2p',6)),rep(c(1:6),2))
a=paste0(c(rep('f1p',6)),c(1:6))
dire=c('MCMC_CS172','MCMC_CS172','MCMC_CS142','MCMC_CS172','MCMC_CS142','MCMC_CS162')
# dire=c("MCMC_CS122")

# for mtext
w=c('-2.25','+0','+2.25','+4.5','+6.75','+9')    

# filename=paste("E:/Netbeans_Project/TECO_SPRUCE_2.2_acclimation_server/figures/forecast/",n,k,"fluxpool.pdf", sep = "")
# cat("pdf_name",filename)
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("/projects/eco_luo/sm3323/TECO_SPRUCE_2.2/postpro/")
getwd()

pdf(paste("figures/DA_results_",dire[1],"_short.pdf", sep = ""),
    width=10.5, height=5.75, compress=FALSE)


for(i in 1:6){
  # i=5
  k=a[i]
  kw=w[i]
  print(k)
  
  # if (i==3){
  #   dire=c("MCMC_CS39")
  # }else{
  #   dire=c("MCMC_CS40")
  # }
  # c=1
  n=as.character(dire[i])  
  print(n)
  
  # smwd=(paste("E:/Netbeans_Project/TECO_SPRUCE_2.2_acclimation_server/figures/forecast/",n,"/",k,"_",n,sep = ""))  # 'sep' set for connection
  smwd=paste("/projects/eco_luo/sm3323/TECO_SPRUCE_2.2/src/output/SPRUCE/",k,"_",n,sep = "")
  
  # sowd=paste("E:/Netbeans_Project/TECO_SPRUCE_2.2_acclimation_server/Figure_dataset/",k,sep = "")
  sowd=paste("/projects/eco_luo/sm3323/TECO_SPRUCE_2.2/postpro/obs_dataset/",k,sep = "")
  
  print(smwd)
  print(sowd)
  
  setwd(smwd)
  getwd()
  #### set a empty n*m matrix  ############
  
  tot_para<-3
  
  GPP<-matrix(NA,2920,tot_para)
  NEE<-matrix(NA,2920,tot_para)
  Reco<-matrix(NA,2920,tot_para)
  QC1<-matrix(NA,2920,tot_para)
  GL<-matrix(NA,2920,tot_para)
  QC2<-matrix(NA,2920,tot_para)
  GW<-matrix(NA,2920,tot_para)
  QC3<-matrix(NA,2920,tot_para)
  GR<-matrix(NA,2920,tot_para)
  QC678<-matrix(NA,2920,tot_para)
  pheno<-matrix(NA,2920,tot_para)
  LAI<-matrix(NA,2920,tot_para)
  NPP<-matrix(NA,2920,tot_para)
  Rh<-matrix(NA,2920,tot_para)
  Ra<-matrix(NA,2920,tot_para)
  
  simuCH4<-matrix(NA,2920,tot_para)
  wt<-matrix(NA,2920,tot_para)
  sw1<-matrix(NA,2920,tot_para)
  sw2<-matrix(NA,2920,tot_para)
  sw3<-matrix(NA,2920,tot_para)
  st1<-matrix(NA,2920,tot_para) 
  st2<-matrix(NA,2920,tot_para) 
  st3<-matrix(NA,2920,tot_para) 
  #### read 001-500 files in a for loop ####
  for (m in 1:tot_para)
  {
    openfile_0<-paste("Simu_dailyflux",str_pad(m, 3, pad = "0"),".txt",sep = "")  ### to open file Simu_dail001-100.txt ###
    # openfile_0<-paste("Simu_dailyflux",str_pad(100, 3, pad = "0"),".txt",sep = "")  ### to open file Simu_dail001-100.txt ###
    d_0 = read.table(openfile_0,strip.white=TRUE,header=T,sep=",")
    d_0 = d_0[,2:17]
    GPP[,m]<-d_0[,2]     
    NEE[,m]<-d_0[,3]
    Reco[,m]<-d_0[,4]
    QC1[,m]<-d_0[,5]
    GL[,m]<-d_0[,6]
    QC2[,m]<-d_0[,7]
    GW[,m]<-d_0[,8]
    QC3[,m]<-d_0[,9]
    GR[,m]<-d_0[,10]
    QC678[,m]<-d_0[,11]
    pheno[,m]<-d_0[,12]
    LAI[,m]<-d_0[,13]
    NPP[,m]<-d_0[,14]
    Rh[,m]<-d_0[,15]
    Ra[,m]<-d_0[,16]
    
    # openfile_1<-paste("Simu_dailybal",str_pad(m, 3, pad = "0"),".txt",sep = "")  ### to open file Simu_dail001-100.txt ###
    # 
    # d_1 = read.table(openfile_1,strip.white=TRUE,header=T,sep=",")
    # simuCH4[,m]<-d_1[,2]     
    # wt[,m]<-d_1[,3]
    # sw1[,m]<-d_1[,4]
    # sw2[,m]<-d_1[,5]
    # sw3[,m]<-d_1[,6]
    # st1[,m]<-d_1[,7]
    # st2[,m]<-d_1[,8]
    # st3[,m]<-d_1[,9]
  }
  
  #### cal mean and sd for the 500 files from different para sets ####
  
  GPP_mean1<-rowMeans(GPP, na.rm =TRUE, dims = 1)
  NEE_mean1<-rowMeans(NEE, na.rm =TRUE, dims = 1)
  Reco_mean1<-rowMeans(Reco, na.rm =TRUE, dims = 1)
  QC1_mean1<-rowMeans(QC1, na.rm =TRUE, dims = 1)
  GL_mean1<-rowMeans(GL, na.rm =TRUE, dims = 1)
  QC2_mean1<-rowMeans(QC2, na.rm =TRUE, dims = 1)
  GW_mean1<-rowMeans(GW, na.rm =TRUE, dims = 1)
  QC3_mean1<-rowMeans(QC3, na.rm =TRUE, dims = 1)
  GR_mean1<-rowMeans(GR, na.rm =TRUE, dims = 1)
  QC678_mean1<-rowMeans(QC678, na.rm =TRUE, dims = 1)
  pheno_mean1<-rowMeans(pheno, na.rm =TRUE, dims = 1)
  LAI_mean1<-rowMeans(LAI, na.rm =TRUE, dims = 1)
  NPP_mean1<-rowMeans(NPP, na.rm =TRUE, dims = 1)
  Rh_mean1<-rowMeans(Rh, na.rm =TRUE, dims = 1)
  Ra_mean1<-rowMeans(Ra, na.rm =TRUE, dims = 1)
  
  simuCH4_mean1<-rowMeans(simuCH4, na.rm =TRUE, dims = 1)
  wt_mean1<-rowMeans(wt, na.rm =TRUE, dims = 1)
  sw1_mean1<-rowMeans(sw1, na.rm =TRUE, dims = 1)*10 #convert unit
  sw2_mean1<-rowMeans(sw2, na.rm =TRUE, dims = 1)*10
  sw3_mean1<-rowMeans(sw3, na.rm =TRUE, dims = 1)*10
  st1_mean1<-rowMeans(st1, na.rm =TRUE, dims = 1)
  st2_mean1<-rowMeans(st2, na.rm =TRUE, dims = 1)
  st3_mean1<-rowMeans(st3, na.rm =TRUE, dims = 1)
  
  GPP_sd1<-apply(GPP, 1, sd) #1 is apply the function sd to all the elements of each row
  NEE_sd1<-apply(NEE, 1, sd) #2 is apply the function sd to all the elements of each column
  Reco_sd1<-apply(Reco, 1, sd)
  QC1_sd1<-apply(QC1, 1, sd)
  QC2_sd1<-apply(QC2, 1, sd)
  QC3_sd1<-apply(QC3, 1, sd)
  QC678_sd1<-apply(QC678, 1, sd)
  GL_sd1<-apply(GL, 1, sd)
  GW_sd1<-apply(GW, 1, sd)
  GR_sd1<-apply(GR, 1, sd)
  pheno_sd1<-apply(pheno, 1, sd)
  LAI_sd1<-apply(LAI, 1, sd)
  NPP_sd1<-apply(NPP, 1, sd)
  Rh_sd1<-apply(Rh, 1, sd)
  Ra_sd1<-apply(Ra, 1, sd)
  
  simuCH4_sd1<-apply(simuCH4,1,sd)
  wt_sd1<-apply(wt, 1, sd)
  sw1_sd1<-apply(sw1, 1, sd)*10 #convert unit
  sw2_sd1<-apply(sw2, 1, sd)*10
  sw3_sd1<-apply(sw3, 1, sd)*10
  st1_sd1<-apply(st1, 1, sd)
  st2_sd1<-apply(st2, 1, sd)
  st3_sd1<-apply(st3, 1, sd)    
  
  
  
  ###   read obs c pool
  setwd(sowd)
  dat.obsbiomass<-read.table(paste("obs_cpool_",k,".txt",sep = ""), header=T)
  leafbmdates <- c(as.Date(271,origin='2010-12-31'),as.Date(636,origin='2010-12-31'),as.Date(1002,origin='2010-12-31'),as.Date(1367,origin='2010-12-31'),as.Date(1732,origin='2010-12-31'),as.Date(2097,origin='2010-12-31'),as.Date(2462,origin='2010-12-31'))
  woodbmdates <- c(as.Date(271,origin='2010-12-31'),as.Date(636,origin='2010-12-31'),as.Date(1002,origin='2010-12-31'),as.Date(1367,origin='2010-12-31'),as.Date(1732,origin='2010-12-31'),as.Date(2097,origin='2010-12-31'),as.Date(2462,origin='2010-12-31'))
  # rootbmdates <- c(as.Date(944,origin='2010-12-31'))
  rootbmdates <- c(as.Date(579,origin='2010-12-31')) # updated date according to Iverson: 2012 data
  # leafbiomass <- c(562.896672, 550.055296, 624.228,640.818,665.218,705.1518188) # old plor area tauleaf 1.2 tauwood 20. tauroot 1.2
  leafbiomass <- dat.obsbiomass$Foliage #c(296.9, 361.3,360.0,411.0,407.2,436.02,465.5) # updated moss leave C mass, in accordance with 2016-2017 data
  # woodbiomass <- c(736.410528,776.354304,865.212,912.002,920.502,982.2305612)
  woodbiomass <- dat.obsbiomass$Wood #c(453,470,540,560,551,601,615)
  rootbiomass <- 377.55
  
  ### read obs c flux
  setwd(sowd)
  dat.obsflux<-read.table(paste("obs_cflux_",k,".txt",sep = ""), header=T)
  DOYobs.date<-as.Date(dat.obsflux$days,origin='2010-12-31')
  msGPP = data.frame(x = DOYobs.date, y = dat.obsflux$GPP, sd = dat.obsflux$GPP*0.2)
  msNEE = data.frame(x = DOYobs.date, y = dat.obsflux$NEE, sd = dat.obsflux$NEE*0.2)
  msReco = data.frame(x = DOYobs.date, y = dat.obsflux$Reco, sd = dat.obsflux$Reco*0.2)
  
  # read obs CH4
  setwd(sowd)
  dat.flux<-read.table(paste("obs_ch4flux_",k,".txt",sep=""), header=T)
  DOY2amb2011_2016.date <- as.Date(dat.flux$days,origin='2010-12-31')
  # msdf = data.frame(x = DOY2amb2011_2016.date, y = dat.flux$CH4, sd = dat.flux$CH4_sd)
  msdf = data.frame(x = DOY2amb2011_2016.date, y = dat.flux$CH4, sd = dat.flux$CH4*0.2)
  
  ## read obs wt sw st
  ###  obs
  setwd(sowd)
  dat.obswater<-read.csv("water.csv", header=T,sep=",", na.strings=c("","NA"))
  # dat.obswater <- dat.obswater[seq(1, nrow(dat.obswater), 48),]   ### remove half hour points
  obswater.date <- as.Date(1826:2920,origin='2010-12-31')
  dat.obswater$rWatertable=dat.obswater$rWatertable*1000
  
  #######  read obs soil tsoilly data
  setwd(sowd)
  # setwd("E:/Netbeans_Project/TECO_SPRUCE_2.1_MethaneForecast/input")
  df<-read.table(paste(k,"forcing2011_2018.txt",sep=''), header=T)
  dat.obstsoil = df[,c(1:3,11:21)]
  dat.obstsoilly <- dat.obstsoil[seq(1, nrow(dat.obstsoil), 24),]   ### pick one hour each day
  obstsoilly.date <- as.Date(1:2920,origin='2010-12-31')
  
  #  ******************************************************************************************************************************
  # start to plot
  
  
  linewid=1
  alphavalue=0.5
  lt1=1
  lt2=3
  text_size=1.
  axis_size=1.
  fontsize=1.
  daystartplot<-as.Date(1,origin='2010-12-31')
  dayendplot = as.Date(2920,origin='2010-12-31')
  
  DOY.date<-c(as.Date(1:2920,origin='2010-12-31'))
  par(cex.axis=fontsize, cex.lab=fontsize, cex.main=fontsize, cex.sub=fontsize)
  par(mfrow=c(3,3),mar=c(2, 4.5, 2, 1))
  par(lty=lt1)
  #### leaf pool
  # plot(1,2)
  plot(DOY.date, QC1_mean1,type="l", ylim=c(0,600),xlim=c(daystartplot,dayendplot),col=coul[i], 
       xaxs = "i",yaxs = "i",
       ylab='Leaf pool (g m-2)',xlab=NA,cex.axis=axis_size,font.axis=fontsize,cex.lab=fontsize)
  axis(side = 1,tck=0.0,labels=F,cex=fontsize,cex.axis=axis_size)
  axis(side = 2,tck=0.03,labels=T,cex=fontsize,cex.axis=axis_size)
  ##
  polygon(c(DOY.date,rev(DOY.date)),c( QC1_mean1-2*QC1_sd1,
                                       rev( QC1_mean1+2*QC1_sd1)),col=adjustcolor(coul[i],alpha.f = alphavalue),border=NA)
  lines(DOY.date, QC1_mean1,type="l",col=coul[i] )
  
  # polygon(c(DOY.date,rev(DOY.date)),c( QC1_mean2-2*QC1_sd2,
  #                                      rev( QC1_mean2+2*QC1_sd2)),col=adjustcolor(coul2[i],alpha.f = alphavalue),border=NA)
  # lines(DOY.date, QC1_mean2,type="l",col=coul2[i])
  # 
  points(leafbmdates,leafbiomass,col = 'red',pch = 16)
  
  mtext(paste("Plot ",kw,sep=""),side=3,-3,cex=fontsize,outer=FALSE)
  box()
  
  # #### wood pool
  plot(DOY.date, QC2_mean1,type="l", ylim=c(0,600),xlim=c(daystartplot,dayendplot),col=coul[i], 
       xaxs = "i",yaxs = "i",
       ylab='Wood pool (g m-2)',xlab=NA,cex.axis=axis_size,font.axis=fontsize,cex.lab=fontsize)
  axis(side = 1,tck=0.0,labels=F,cex=fontsize,cex.axis=axis_size)
  axis(side = 2,tck=0.03,labels=T,cex=fontsize,cex.axis=axis_size)
  
  ##
  polygon(c(DOY.date,rev(DOY.date)),c( QC2_mean1-2*QC2_sd1,
                                       rev( QC2_mean1+2*QC2_sd1)),col=adjustcolor(coul[i],alpha.f = alphavalue),border=NA)
  
  lines(DOY.date, QC2_mean1,type="l",col=coul[i] )
  
  # polygon(c(DOY.date,rev(DOY.date)),c( QC2_mean2-2*QC2_sd2,
  #                                      rev( QC2_mean2+2*QC2_sd2)),col=adjustcolor(coul2[i],alpha.f = alphavalue),border=NA)
  # lines(DOY.date, QC2_mean2,type="l",col=coul2[i] )
  points(woodbmdates,woodbiomass,col = 'red',pch = 16)
  
  box()
  # points(woodbmdates,woodbiomass,col = 'RED',pch = 16)
  # #### root pool
  plot(DOY.date, QC3_mean1,type="l", ylim=c(0,600),xlim=c(daystartplot,dayendplot),col=coul[i], 
       xaxs = "i",yaxs = "i",
       ylab='Root pool (g m-2)',xlab=NA,cex.axis=axis_size,font.axis=fontsize,cex.lab=fontsize)
  axis(side = 1,tck=0.0,labels=F,cex=fontsize,cex.axis=axis_size)
  axis(side = 2,tck=0.03,labels=T,cex=fontsize,cex.axis=axis_size)
  
  polygon(c(DOY.date,rev(DOY.date)),c( QC3_mean1-2*QC3_sd1,
                                       rev( QC3_mean1+2*QC3_sd1)),col=adjustcolor(coul[i],alpha.f = alphavalue),border=NA)
  lines(DOY.date, QC3_mean1,type="l",col=coul[i] )
  # polygon(c(DOY.date,rev(DOY.date)),c( QC3_mean2-2*QC3_sd2,
  #                                      rev( QC3_mean2+2*QC3_sd2)),col=adjustcolor(coul2[i],alpha.f = alphavalue),border=NA)
  # lines(DOY.date, QC3_mean2,type="l",col=coul2[i] )
  # # points(rootbmdates,rootbiomass,col = 'red',pch = 16)
  points(rootbmdates,rootbiomass,col = NA,pch = 16)
  box()
  
  # ## leaf growth pool
  # plot(DOY.date, GL_mean1,type="l", ylim=c(0,400),xlim=c(daystartplot,dayendplot),col=coul[i], 
  #      xaxs = "i",yaxs = "i",
  #      ylab='Leaf Growth (g m-2 d-1)',xlab=NA,cex.axis=axis_size,font.axis=fontsize,cex.lab=fontsize)
  # axis(side = 1,tck=0.0,labels=F,cex=fontsize,cex.axis=axis_size)
  # axis(side = 2,tck=0.03,labels=T,cex=fontsize,cex.axis=axis_size)
  # # axis(side=1,tck=0.03,labels=T,cex=fontsize)
  # # axis(side=2,tck=0.03,labels=T,cex=fontsize)
  # ##
  # polygon(c(DOY.date,rev(DOY.date)),c( GL_mean1-2*GL_sd1,
  #                                      rev( GL_mean1+2*GL_sd1)),col=adjustcolor(coul[i],alpha.f = alphavalue),border=NA)
  # lines(DOY.date, GL_mean1,type="l",col=coul[i] )
  # # polygon(c(DOY.date,rev(DOY.date)),c( GL_mean2-2*GL_sd2,
  # #                                      rev( GL_mean2+2*GL_sd2)),col=adjustcolor(coul2[i],alpha.f = alphavalue),border=NA)
  # # lines(DOY.date, GL_mean2,type="l",col=coul2[i] )
  # box()
  # 
  # ## wood growth pool
  # plot(DOY.date, GW_mean1,type="l", ylim=c(0,400),xlim=c(daystartplot,dayendplot),col=coul[i], 
  #      xaxs = "i",yaxs = "i",
  #      ylab='Wood Growth (g m-2 d-1)',xlab=NA,cex.axis=axis_size,font.axis=fontsize,cex.lab=fontsize)
  # axis(side = 1,tck=0.0,labels=F,cex=fontsize,cex.axis=axis_size)
  # axis(side = 2,tck=0.03,labels=T,cex=fontsize,cex.axis=axis_size)
  # # axis(side=1,tck=0.03,labels=T,cex=fontsize)
  # # axis(side=2,tck=0.03,labels=T,cex=fontsize)
  # ##
  # polygon(c(DOY.date,rev(DOY.date)),c( GW_mean1-2*GW_sd1,
  #                                      rev( GW_mean1+2*GW_sd1)),col=adjustcolor(coul[i],alpha.f = alphavalue),border=NA)
  # lines(DOY.date, GW_mean1,type="l",col=coul[i] )
  # # polygon(c(DOY.date,rev(DOY.date)),c( GW_mean2-2*GW_sd2,
  # #                                      rev( GW_mean2+2*GW_sd2)),col=adjustcolor(coul2[i],alpha.f = alphavalue),border=NA)
  # # lines(DOY.date, GW_mean2,type="l",col=coul2[i] )
  # box()
  # 
  # ## root growth pool
  # plot(DOY.date, GR_mean1,type="l", ylim=c(0,400),xlim=c(daystartplot,dayendplot),col=coul[i], 
  #      xaxs = "i",yaxs = "i",
  #      ylab='Root Growth (g m-2 d-1)',xlab=NA,cex.axis=axis_size,font.axis=fontsize,cex.lab=fontsize)
  # axis(side = 1,tck=0.0,labels=F,cex=fontsize,cex.axis=axis_size)
  # axis(side = 2,tck=0.03,labels=T,cex=fontsize,cex.axis=axis_size)
  # # axis(side=1,tck=0.03,labels=T,cex=fontsize)
  # # axis(side=2,tck=0.03,labels=T,cex=fontsize)
  # ##
  # polygon(c(DOY.date,rev(DOY.date)),c( GR_mean1-2*GR_sd1,
  #                                      rev( GR_mean1+2*GR_sd1)),col=adjustcolor(coul[i],alpha.f = alphavalue),border=NA)
  # lines(DOY.date, GR_mean1,type="l",col=coul[i] )
  # # polygon(c(DOY.date,rev(DOY.date)),c( GR_mean2-2*GR_sd2,
  # #                                      rev( GR_mean2+2*GR_sd2)),col=adjustcolor(coul2[i],alpha.f = alphavalue),border=NA)
  # # lines(DOY.date, GR_mean2,type="l",col=coul2[i] )
  # box()
  
  #GPP
  plot(DOY.date,GPP_mean1,col=coul[i], type='l',xlab=NA,ylab='GPP (gC m-2 d-1)',
       xaxs = "i",yaxs = "i",
       cex.lab=fontsize,pch=19,cex=1,ylim = c(0,10.0),
       lwd=linewid)
  axis(side = 1,tck=0.0,labels=F,cex=fontsize,cex.axis=axis_size)
  axis(side = 2,tck=0.03,labels=T,cex=fontsize,cex.axis=axis_size)
  
  polygon(c(DOY.date,rev(DOY.date)),c(GPP_mean1-2*GPP_sd1,
                                      rev( GPP_mean1+2*GPP_sd1)),col=adjustcolor(coul[i],alpha.f = alphavalue),border=NA)
  lines(DOY.date, GPP_mean1,type="l",col=coul[i] )

  
  # points(msGPP$x,msGPP$y,col='red',lwd=3 ,main='GPP',xlab = 'Date',
  #        ylab='GPP (gC m-2 d-1)')
  # with(data = msGPP, expr = errbar(x,y,y+sd,y-sd,add=T,pch=0,cap=.01))
  box()
  
  # # NEE
  plot(DOY.date,NEE_mean1,col=coul[i], type='l',xlab=NA,ylab='NEE (gC m-2 d-1)',
       xaxs = "i",yaxs = "i",
       cex.lab=fontsize,pch=19,cex=1,ylim = c(-10,10.0),
       lwd=linewid)
  axis(side = 1,tck=0.0,labels=F,cex=fontsize,cex.axis=axis_size)
  axis(side = 2,tck=0.03,labels=T,cex=fontsize,cex.axis=axis_size)
  
  polygon(c(DOY.date,rev(DOY.date)),c( NEE_mean1-2*NEE_sd1,
                                       rev( NEE_mean1+2*NEE_sd1)),col=adjustcolor(coul[i],alpha.f = alphavalue),border=NA)
  lines(DOY.date, NEE_mean1,type="l",col=coul[i] )

  # 
  # points(msNEE$x,msNEE$y,col='red',lwd=3 ,main='NEE',xlab = 'Date',
  #        ylab='NEE (gC m-2 d-1)')
  # with(data = msNEE, expr = errbar(x,y,y+sd,y-sd,add=T,pch=0,cap=.01))
  box()
  # #ER
  plot(DOY.date,Reco_mean1,col=coul[i], type='l',xlab=NA,ylab='Reco (gC m-2 d-1)',
       xaxs = "i",yaxs = "i",
       cex.lab=fontsize,pch=19,cex=1,ylim = c(0,10.0),
       lwd=linewid)
  axis(side = 1,tck=0.0,labels=F,cex=fontsize,cex.axis=axis_size)
  axis(side = 2,tck=0.03,labels=T,cex=fontsize,cex.axis=axis_size)
  
  polygon(c(DOY.date,rev(DOY.date)),c( Reco_mean1-2*Reco_sd1,
                                       rev( Reco_mean1+2*Reco_sd1)),col=adjustcolor(coul[i],alpha.f = alphavalue),border=NA)
  lines(DOY.date, Reco_mean1,type="l",col=coul[i] )
  # 
  # points(msReco$x,msReco$y,col='red',lwd=3 ,main='Reco',xlab = 'Date',
  #        ylab='Reco (gC m-2 d-1)')
  # with(data = msReco, expr = errbar(x,y,y+sd,y-sd,add=T,pch=0,cap=.01))
  box()
  # 
  # # NPP
  plot(DOY.date,NPP_mean1,col=coul[i], type='l',xlab=NA,ylab='NPP (gC m-2 d-1)',
       xaxs = "i",yaxs = "i",
       cex.lab=fontsize,pch=19,cex=1,ylim = c(0,10.0),
       lwd=linewid)
  axis(side = 1,tck=0.0,labels=F,cex=fontsize,cex.axis=axis_size)
  axis(side = 2,tck=0.03,labels=T,cex=fontsize,cex.axis=axis_size)
  
  polygon(c(DOY.date,rev(DOY.date)),c( NPP_mean1-2*NPP_sd1,
                                       rev( NPP_mean1+2*NPP_sd1)),col=adjustcolor(coul[i],alpha.f = alphavalue),border=NA)
  lines(DOY.date, NPP_mean1,type="l",col=coul[i] )
  # polygon(c(DOY.date,rev(DOY.date)),c( NPP_mean2-2*NPP_sd2,
  #                                      rev( NPP_mean2+2*NPP_sd2)),col=adjustcolor(coul2[i],alpha.f = alphavalue),border=NA)
  # lines(DOY.date, NPP_mean2,type="l",col=coul2[i] )
  mtext(paste("Plot ",kw,sep=""),side=3,-3,cex=fontsize,outer=FALSE)
  # # Rh
  plot(DOY.date,Rh_mean1,col=coul[i], type='l',xlab=NA,ylab='Rh (gC m-2 d-1)',
       xaxs = "i",yaxs = "i",
       cex.lab=fontsize,pch=19,cex=1,ylim = c(0,10.0),
       lwd=linewid)
  axis(side = 1,tck=0.0,labels=F,cex=fontsize,cex.axis=axis_size)
  axis(side = 2,tck=0.03,labels=T,cex=fontsize,cex.axis=axis_size)
  
  polygon(c(DOY.date,rev(DOY.date)),c( Rh_mean1-2*Rh_sd1,
                                       rev( Rh_mean1+2*Rh_sd1)),col=adjustcolor(coul[i],alpha.f = alphavalue),border=NA)
  lines(DOY.date, Rh_mean1,type="l",col=coul[i] )
  # polygon(c(DOY.date,rev(DOY.date)),c( Rh_mean2-2*Rh_sd2,
  #                                      rev( Rh_mean2+2*Rh_sd2)),col=adjustcolor(coul2[i],alpha.f = alphavalue),border=NA)
  # lines(DOY.date, Rh_mean2,type="l",col=coul2[i] )
  # # Ra
  plot(DOY.date,Ra_mean1,col=coul[i], type='l',xlab=NA,ylab='Ra (gC m-2 d-1)',
       xaxs = "i",yaxs = "i",
       cex.lab=fontsize,pch=19,cex=1,ylim = c(0,10.0),
       lwd=linewid)
  axis(side = 1,tck=0.0,labels=F,cex=fontsize,cex.axis=axis_size)
  axis(side = 2,tck=0.03,labels=T,cex=fontsize,cex.axis=axis_size)
  
  polygon(c(DOY.date,rev(DOY.date)),c( Ra_mean1-2*Ra_sd1,
                                       rev( Ra_mean1+2*Ra_sd1)),col=adjustcolor(coul[i],alpha.f = alphavalue),border=NA)
  lines(DOY.date, Ra_mean1,type="l",col=coul[i] )
  # polygon(c(DOY.date,rev(DOY.date)),c( Ra_mean2-2*Ra_sd2,
  #                                      rev( Ra_mean2+2*Ra_sd2)),col=adjustcolor(coul2[i],alpha.f = alphavalue),border=NA)
  # lines(DOY.date, Ra_mean2,type="l",col=coul2[i] )
  
  # methane flux
  plot(DOY.date,simuCH4_mean1,col=coul[i], type='l',xlab=NA,ylab='CH4 emission (gC m-2 d-1)',
       xaxs = "i",yaxs = "i",
       cex.lab=fontsize,pch=19,cex=1,ylim = c(0,2.0),
       lwd=linewid)
  axis(side = 1,tck=0.0,labels=F,cex=fontsize,cex.axis=axis_size)
  axis(side = 2,tck=0.03,labels=T,cex=fontsize,cex.axis=axis_size)
  
  polygon(c(DOY.date,rev(DOY.date)),c( simuCH4_mean1-2*simuCH4_sd1,
                                       rev( simuCH4_mean1+2*simuCH4_sd1)),col=adjustcolor(coul[i],alpha.f = alphavalue),border=NA)
  lines(DOY.date, simuCH4_mean1,type="l",col=coul[i] )
  # polygon(c(DOY.date,rev(DOY.date)),c( simuCH4_mean2-2*simuCH4_sd2,
  #                                      rev( simuCH4_mean2+2*simuCH4_sd2)),col=adjustcolor(coul2[i],alpha.f = alphavalue),border=NA)
  # lines(DOY.date, simuCH4_mean2,type="l",col=coul2[i] )
  
  points(msdf$x,msdf$y,col='red',pch=19,cex=1)
  with(data = msdf, expr = errbar(x,y,y+sd,y-sd,add=T,pch=0,cap=.01))
  
  
  
  
  # start to plot
  ## wt
  plot(DOY.date,wt_mean1,col='black',xlab='Date',ylab='water table depth (cm)',type='l',
       xaxs = "i",yaxs = "i", lwd=linewid,ylim = c(-500,300),cex.lab=fontsize)
  
  axis(side = 1,tck=0.0,labels=F,cex=fontsize,cex.axis=axis_size)
  axis(side = 2,tck=0.03,labels=T,cex=fontsize,cex.axis=axis_size)
  
  polygon(c(DOY.date,rev(DOY.date)),c(wt_mean1-2*wt_sd1,
                                      rev(wt_mean1+2*wt_sd1)),col=adjustcolor(coul[i],alpha.f = alphavalue),border=NA)
  lines(DOY.date, wt_mean1,type="l",col=coul[i])  
  # polygon(c(DOY.date,rev(DOY.date)),c(wt_mean2-2*wt_sd2,
  #                                     rev(wt_mean2+2*wt_sd2)),col=adjustcolor(coul2[i],alpha.f = alphavalue),border=NA)
  # lines(DOY.date, wt_mean2,type="l",col=coul2[i])  
  
  lines(obswater.date,dat.obswater$rWatertable,col='red',lwd=linewid)
  # 
  
  
  #### SOC pool
  # plot(1,2)
  plot(DOY.date, QC6_mean1,type="l", ylim=c(200,800),xlim=c(daystartplot,dayendplot),col=coul[i], 
       xaxs = "i",yaxs = "i",
       ylab='labile C pool (g m-2)',xlab=NA,cex.axis=axis_size,font.axis=fontsize,cex.lab=fontsize)
  axis(side = 1,tck=0.0,labels=F,cex=fontsize,cex.axis=axis_size)
  axis(side = 2,tck=0.03,labels=T,cex=fontsize,cex.axis=axis_size)
  ##
  polygon(c(DOY.date,rev(DOY.date)),c( QC6_mean1-2*QC6_sd1,
                                       rev( QC6_mean1+2*QC6_sd1)),col=adjustcolor(coul[i],alpha.f = alphavalue),border=NA)
  lines(DOY.date, QC6_mean1,type="l",col=coul[i] )
  
  plot(DOY.date, QC7_mean1,type="l", ylim=c(20000,60000),xlim=c(daystartplot,dayendplot),col=coul[i], 
       xaxs = "i",yaxs = "i",
       ylab='recalcitrient C pool (g m-2)',xlab=NA,cex.axis=axis_size,font.axis=fontsize,cex.lab=fontsize)
  axis(side = 1,tck=0.0,labels=F,cex=fontsize,cex.axis=axis_size)
  axis(side = 2,tck=0.03,labels=T,cex=fontsize,cex.axis=axis_size)
  ##
  polygon(c(DOY.date,rev(DOY.date)),c( QC7_mean1-2*QC7_sd1,
                                       rev( QC7_mean1+2*QC7_sd1)),col=adjustcolor(coul[i],alpha.f = alphavalue),border=NA)
  lines(DOY.date, QC7_mean1,type="l",col=coul[i])
  
  plot(DOY.date, QC8_mean1,type="l", ylim=c(200,800),xlim=c(daystartplot,dayendplot),col=coul[i], 
       xaxs = "i",yaxs = "i",
       ylab='passive C pool (g m-2)',xlab=NA,cex.axis=axis_size,font.axis=fontsize,cex.lab=fontsize)
  axis(side = 1,tck=0.0,labels=F,cex=fontsize,cex.axis=axis_size)
  axis(side = 2,tck=0.03,labels=T,cex=fontsize,cex.axis=axis_size)
  ##
  polygon(c(DOY.date,rev(DOY.date)),c( QC8_mean1-2*QC8_sd1,
                                       rev( QC8_mean1+2*QC8_sd1)),col=adjustcolor(coul[i],alpha.f = alphavalue),border=NA)
  lines(DOY.date, QC8_mean1,type="l",col=coul[i] )
  # ###  soil temperature ####################
  # ## st1
  # plot(DOY.date,st1_mean1,col=coul[i],xlab=NA,ylab='soiltemp_surface',
  #      xaxs = "i",yaxs = "i", type='l',lwd=linewid,ylim = c(-20,35),cex.lab=fontsize)
  # axis(side = 1,tck=0.0,labels=F,cex=fontsize,cex.axis=axis_size)
  # axis(side = 2,tck=0.03,labels=T,cex=fontsize,cex.axis=axis_size)
  # polygon(c(DOY.date,rev(DOY.date)),c(st1_mean1-2*st1_sd1,
  #                                     rev( st1_mean1+2*st1_sd1)),col=adjustcolor(coul[i],alpha.f = alphavalue),border=NA)
  # lines(DOY.date, st1_mean1,type="l",col=coul[i])
  # # polygon(c(DOY.date,rev(DOY.date)),c(st1_mean2-2*st1_sd2,
  # #                                     rev( st1_mean2+2*st1_sd2)),col=adjustcolor(coul2[i],alpha.f = alphavalue),border=NA)
  # # lines(DOY.date, st1_mean2,type="l",col=coul2[i])
  # 
  # lines(obstsoilly.date,dat.obstsoilly$Tsoil0,lwd=linewid,col='red')
  # 
  # ## st2
  # plot(DOY.date,st2_mean1,col=coul[i],xlab=NA,ylab='soiltemp -10cm',
  #      xaxs = "i",yaxs = "i", type='l',lwd=linewid,ylim = c(-20,35),cex.lab=fontsize)
  # axis(side = 1,tck=0.0,labels=F,cex=fontsize,cex.axis=axis_size)
  # axis(side = 2,tck=0.03,labels=T,cex=fontsize,cex.axis=axis_size)
  # polygon(c(DOY.date,rev(DOY.date)),c(st2_mean1-2*st2_sd1,
  #                                     rev( st2_mean1+2*st2_sd1)),col=adjustcolor(coul[i],alpha.f = alphavalue),border=NA)
  # lines(DOY.date, st2_mean1,type="l",col=coul[i])
  # # polygon(c(DOY.date,rev(DOY.date)),c(st2_mean2-2*st2_sd2,
  # #                                     rev( st2_mean2+2*st2_sd2)),col=adjustcolor(coul2[i],alpha.f = alphavalue),border=NA)
  # # lines(DOY.date, st2_mean2,type="l",col=coul2[i])
  # 
  # lines(obstsoilly.date,dat.obstsoilly$Tsoil10,lwd=linewid,col='red')
  # 
  # ## st3
  # plot(DOY.date,st3_mean1,col=coul[i],xlab=NA,ylab='soiltemp -20cm',
  #      xaxs = "i",yaxs = "i", type='l',lwd=linewid,ylim = c(-20,35),cex.lab=fontsize)
  # axis(side = 1,tck=0.0,labels=F,cex=fontsize,cex.axis=axis_size)
  # axis(side = 2,tck=0.03,labels=T,cex=fontsize,cex.axis=axis_size)
  # polygon(c(DOY.date,rev(DOY.date)),c(st3_mean1-2*st3_sd1,
  #                                     rev( st3_mean1+2*st3_sd1)),col=adjustcolor(coul[i],alpha.f = alphavalue),border=NA)
  # lines(DOY.date, st3_mean1,type="l",col=coul[i])
  # # polygon(c(DOY.date,rev(DOY.date)),c(st3_mean2-2*st3_sd2,
  # #                                     rev( st3_mean2+2*st3_sd2)),col=adjustcolor(coul2[i],alpha.f = alphavalue),border=NA)
  # # lines(DOY.date, st3_mean2,type="l",col=coul2[i])
  # 
  # lines(obstsoilly.date,dat.obstsoilly$Tsoil20,lwd=linewid,col='red')
  # 
  # ## sw1
  # plot(DOY.date,sw1_mean1,col=coul[i],xlab='Date',ylab='soilwater -10cm',type='l',
  #      xaxs = "i",yaxs = "i",lwd=linewid,ylim = c(0,1.0),cex.lab=fontsize)
  # # axis(side = 1,tck=0.0,labels=T,cex=fontsize,cex.axis=axis_size)
  # axis(side = 2,tck=0.03,labels=T,cex=fontsize,cex.axis=axis_size)
  # 
  # polygon(c(DOY.date,rev(DOY.date)),c(sw1_mean1-2*sw1_sd1,
  #                                     rev(sw1_mean1+2*sw1_sd1)),col=adjustcolor(coul[i],alpha.f = alphavalue),border=NA)
  # lines(DOY.date, sw1_mean1,type="l",col=coul[i])
  # # polygon(c(DOY.date,rev(DOY.date)),c(sw1_mean2-2*sw1_sd2,
  # #                                     rev(sw1_mean2+2*sw1_sd2)),col=adjustcolor(coul2[i],alpha.f = alphavalue),border=NA)
  # # lines(DOY.date, sw1_mean2,type="l",col=coul2[i])
  # 
  # lines(obswater.date,dat.obswater$VWC0,lwd=linewid,col='red')
  # ## sw2
  # plot(DOY.date,sw2_mean1,col=coul[i],xlab='Date',ylab='soilwater -20cm',type='l',
  #      xaxs = "i",yaxs = "i",lwd=linewid,ylim = c(0,1.0),cex.lab=fontsize)
  # # axis(side = 1,tck=0.0,labels=T,cex=fontsize,cex.axis=axis_size)
  # axis(side = 2,tck=0.03,labels=T,cex=fontsize,cex.axis=axis_size)
  # polygon(c(DOY.date,rev(DOY.date)),c(sw2_mean1-2*sw2_sd1,
  #                                     rev(sw2_mean1+2*sw2_sd1)),col=adjustcolor(coul[i],alpha.f = alphavalue),border=NA)
  # lines(DOY.date, sw2_mean1,type="l",col=coul[i])
  # # polygon(c(DOY.date,rev(DOY.date)),c(sw2_mean2-2*sw2_sd2,
  # #                                     rev(sw2_mean2+2*sw2_sd2)),col=adjustcolor(coul2[i],alpha.f = alphavalue),border=NA)
  # # lines(DOY.date, sw2_mean2,type="l",col=coul2[i])
  # 
  # lines(obswater.date,dat.obswater$VWC20,lwd=linewid,col='red')
  # ## sw3
  # plot(DOY.date,sw3_mean1,col=coul[i],xlab='Date',ylab='soilwater -30cm',type='l',
  #      xaxs = "i",yaxs = "i",lwd=linewid,ylim = c(0,1.0),cex.lab=fontsize)
  # # axis(side = 1,tck=0.0,labels=T,cex=fontsize,cex.axis=axis_size)
  # axis(side = 2,tck=0.03,labels=T,cex=fontsize,cex.axis=axis_size)
  # polygon(c(DOY.date,rev(DOY.date)),c(sw3_mean1-2*sw3_sd1,
  #                                     rev(sw3_mean1+2*sw3_sd1)),col=adjustcolor(coul[i],alpha.f = alphavalue),border=NA)
  # lines(DOY.date, sw3_mean1,type="l",col=coul[i]) 
  # # polygon(c(DOY.date,rev(DOY.date)),c(sw3_mean2-2*sw3_sd2,
  # #                                     rev(sw3_mean2+2*sw3_sd2)),col=adjustcolor(coul2[i],alpha.f = alphavalue),border=NA)
  # # lines(DOY.date, sw3_mean2,type="l",col=coul2[i])
  
}
dev.off()

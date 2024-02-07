# check this web for sd or se
# https://datascienceplus.com/standard-deviation-vs-standard-error/


rm(list=ls())
# ce 52 62 A
# k=c('_MCMC_CS192','_MCMC_CS172','_MCMC_CS182','_MCMC_CS192','_MCMC_CS172','_MCMC_CS192')
n=c('_CS172','_CS172','_CS142','_CS172','_CS142','_CS162')
n=c('_CS172','_CS172','_CS142','_CS142','_CS142','_CS162')
n=c(rep('_CS201',6))
n=c('_CS202','_CS202','_CS202','_CS202','_CS206','_CS206')
n=c('_CS211','_CS211','_CS211','_CS211','_CS213','_CS211')
n=c('_CS212','_CS212','_CS212','_CS212','_CS214','_CS212')
n=c(rep('_CS214',6))
n=c('_CS1172','_CS1172','_CS1142','_CS1172','_CS1142','_CS1162')
# JPL runs
#n=c('_CS1721','_CS1721','_CS1421','_CS1721','_CS1421','_CS1621')
n=c('_CS1722','_CS1722','_CS1422','_CS1622','_CS1422','_CS1622')
#n=c('_CS1723','_CS1723','_CS1423','_CS1723','_CS1423','_CS1623')
#n=c('_CS1724','_CS1724','_CS1424','_CS1724','_CS1424','_CS1624')
n=c('_CS1722','_CS1423','_CS1422','_CS1623','_CS1422','_CS1622')

skn=2000

#setwd("/projects/eco_luo/sm3323/TECO_SPRUCE_2.2/postpro")
# setwd("/scratch_lg/cardamom/shuangma/TECO/TECO_2.21/postpro")
setwd("~/RESEARCH/TECO_SPRUCE_site/TECO_2.21_Dissertation/jpl_server/TECO_2.21/postpro/statistics/posterior_distribution/")

getwd()

pdf(paste("fcast_plot_all_pdf_cf2_kernel_combo.pdf", sep = ""),
    width=13.5, height=6.75, compress=FALSE)
# tiff("kernel.tiff",units="in",width=12.5, height=5.75, res=300)
# tiff("plot/aeCH4_TS_together.tiff",units="in", width=15, height=9, res=300)

setwd(paste("~/RESEARCH/TECO_SPRUCE_site/TECO_2.21_Dissertation/jpl_server/TECO_2.21/src/input/SPRUCE/f1p1/",sep=''))
data1=read.table(paste('Paraest_f1p1.txt',sep=''),header = F,skip =skn,sep = ',')
# data1=data1[1:2000,]
colnames(data1) <- c("isimu","upgraded", "GLmax","GRmax","Gsmax","Vcmx0",
                     "Tau_Leaf", "Tau_Root","Tau_F","Tau_C","Tau_Micro","Tau_SlowSOM","Tau_Passive",
                     "gddonset","Q10","Rl0","Rs0","Rr0",
                     "r_me","Q10_pro","Omax","Tveg","f","bubprob","Vmaxfraction",
                     "Q10rh","JV","Entrpy", 'f_F2M','f_C2M', 'f_M2S','f_M2P', 
                     "NA")
# k="MCMC_CS40"
# n="_CS40"
setwd(paste("~/RESEARCH/TECO_SPRUCE_site/TECO_2.21_Dissertation/jpl_server/TECO_2.21/src/input/SPRUCE/f1p2/",sep=''))
data2=read.table(paste('Paraest_f1p2.txt',sep=''),header = F,skip =skn,sep = ',')
# data2=data2[1:2000,]
colnames(data2) <- c("isimu","upgraded", "GLmax","GRmax","Gsmax","Vcmx0",
                     "Tau_Leaf", "Tau_Root","Tau_F","Tau_C","Tau_Micro","Tau_SlowSOM","Tau_Passive",
                     "gddonset","Q10","Rl0","Rs0","Rr0",
                     "r_me","Q10_pro","Omax","Tveg","f","bubprob","Vmaxfraction",
                     "Q10rh","JV","Entrpy", 'f_F2M','f_C2M', 'f_M2S','f_M2P', 
                     "NA")
# k="MCMC_CS39"
# n="_CS39"
# k="MCMC_CS41"
# n="_CS41"
setwd(paste("~/RESEARCH/TECO_SPRUCE_site/TECO_2.21_Dissertation/jpl_server/TECO_2.21/src/input/SPRUCE/f1p3/",sep=''))
data3=read.table(paste('Paraest_f1p3.txt',sep=''),header = F,skip =skn,sep = ',')
# data3=data3[1:2000,]
colnames(data3) <- c("isimu","upgraded", "GLmax","GRmax","Gsmax","Vcmx0",
                     "Tau_Leaf", "Tau_Root","Tau_F","Tau_C","Tau_Micro","Tau_SlowSOM","Tau_Passive",
                     "gddonset","Q10","Rl0","Rs0","Rr0",
                     "r_me","Q10_pro","Omax","Tveg","f","bubprob","Vmaxfraction",
                     "Q10rh","JV","Entrpy", 'f_F2M','f_C2M', 'f_M2S','f_M2P', 
                     "NA")
# k="MCMC_CS40"
# n="_CS40"
setwd(paste("~/RESEARCH/TECO_SPRUCE_site/TECO_2.21_Dissertation/jpl_server/TECO_2.21/src/input/SPRUCE/f1p4/",sep=''))
data4=read.table(paste('Paraest_f1p4.txt',sep=''),header = F,skip =skn,sep = ',')
# data4=data4[1:2000,]
colnames(data4) <- c("isimu","upgraded", "GLmax","GRmax","Gsmax","Vcmx0",
                     "Tau_Leaf", "Tau_Root","Tau_F","Tau_C","Tau_Micro","Tau_SlowSOM","Tau_Passive",
                     "gddonset","Q10","Rl0","Rs0","Rr0",
                     "r_me","Q10_pro","Omax","Tveg","f","bubprob","Vmaxfraction",
                     "Q10rh","JV","Entrpy", 'f_F2M','f_C2M', 'f_M2S','f_M2P', 
                     "NA")
# k="MCMC_CS40"
# n="_CS40"
setwd(paste("~/RESEARCH/TECO_SPRUCE_site/TECO_2.21_Dissertation/jpl_server/TECO_2.21/src/input/SPRUCE/f1p5/",sep=''))
data5=read.table(paste('Paraest_f1p5.txt',sep=''),header = F,skip =skn,sep = ',')
# data5=data5[1:2000,]
colnames(data5) <- c("isimu","upgraded", "GLmax","GRmax","Gsmax","Vcmx0",
                     "Tau_Leaf", "Tau_Root","Tau_F","Tau_C","Tau_Micro","Tau_SlowSOM","Tau_Passive",
                     "gddonset","Q10","Rl0","Rs0","Rr0",
                     "r_me","Q10_pro","Omax","Tveg","f","bubprob","Vmaxfraction",
                     "Q10rh","JV","Entrpy", 'f_F2M','f_C2M', 'f_M2S','f_M2P', 
                     "NA")
# k="MCMC_CS38"
# n="_CS38"
setwd(paste("~/RESEARCH/TECO_SPRUCE_site/TECO_2.21_Dissertation/jpl_server/TECO_2.21/src/input/SPRUCE/f1p6/",sep=''))
data6=read.table(paste('Paraest_f1p6.txt',sep=''),header = F,skip =skn,sep = ',')
# data6=data6[1:2000,]
colnames(data6) <- c("isimu","upgraded", "GLmax","GRmax","Gsmax","Vcmx0",
                     "Tau_Leaf", "Tau_Root","Tau_F","Tau_C","Tau_Micro","Tau_SlowSOM","Tau_Passive",
                     "gddonset","Q10","Rl0","Rs0","Rr0",
                     "r_me","Q10_pro","Omax","Tveg","f","bubprob","Vmaxfraction",
                     "Q10rh","JV","Entrpy", 'f_F2M','f_C2M', 'f_M2S','f_M2P', 
                     "NA")

#create color palette:
library(RColorBrewer)
#### set color ####
coul = c('black','#009900','#00ff00','#cccc00',
         '#ff9900','red')
# coul = c('black','#009900','#00ff00','#cccc00','#cccc00','#cccc00')

##### plot setting #####
fontsize=1.
labsize=1.25
# par(cex=2)
par(cex.axis=fontsize, cex.lab=labsize, cex.main=fontsize, cex.sub=fontsize)
par(mfrow=c(4,8),oma=c(4,3,1,1))

# par(mar=c(1, 2.5, 2, 1))
linewid=2
alphafvalue=0.4
yl=c(0,15)
par(mai=c(0.3,0.1,0.1,0.1),lwd=2)
par(xaxs = "i", yaxs="i")
par(yaxt='n')
bk1=50
bk2=100
#### plot hist####
adjnum=1
mti=1
##
data1.GLmax = density (data1$GLmax, adjust = adjnum)
data2.GLmax = density (data2$GLmax, adjust = adjnum)
data3.GLmax = density (data3$GLmax, adjust = adjnum)
data4.GLmax = density (data4$GLmax, adjust = adjnum)
data5.GLmax = density (data5$GLmax, adjust = adjnum)
data6.GLmax = density (data6$GLmax, adjust = adjnum)
plot(data1.GLmax,col=coul[1],xlim = c(10.0,50.),ylim = c(0,0.1),main = NA)
lines(data2.GLmax,col=coul[2])
lines(data3.GLmax,col=coul[3])
lines(data4.GLmax,col=coul[4])
lines(data5.GLmax,col=coul[5])
lines(data6.GLmax,col=coul[6])
mtext(paste("(",mti,") GLmax",sep=""),side=3,-3,cex=fontsize,outer=FALSE,at=20.4)
mti=mti+1
##
data1.GRmax = density (data1$GRmax, adjust = adjnum)
data2.GRmax = density (data2$GRmax, adjust = adjnum)
data3.GRmax = density (data3$GRmax, adjust = adjnum)
data4.GRmax = density (data4$GRmax, adjust = adjnum)
data5.GRmax = density (data5$GRmax, adjust = adjnum)
data6.GRmax = density (data6$GRmax, adjust = adjnum)
plot(data1.GRmax,col=coul[1],xlim = c(10.0,30.),ylim = c(0,0.3),main = NA)
lines(data2.GRmax,col=coul[2])
lines(data3.GRmax,col=coul[3])
lines(data4.GRmax,col=coul[4])
lines(data5.GRmax,col=coul[5])
lines(data6.GRmax,col=coul[6])
mtext(paste("(",mti,") GRmax",sep=""),side=3,-3,cex=fontsize,outer=FALSE,at=18)
mti=mti+1
##
data1.Gsmax = density (data1$Gsmax, adjust = adjnum)
data2.Gsmax = density (data2$Gsmax, adjust = adjnum)
data3.Gsmax = density (data3$Gsmax, adjust = adjnum)
data4.Gsmax = density (data4$Gsmax, adjust = adjnum)
data5.Gsmax = density (data5$Gsmax, adjust = adjnum)
data6.Gsmax = density (data6$Gsmax, adjust = adjnum)
plot(data1.Gsmax,col=coul[1],xlim = c(10.0,30.),ylim = c(0,0.4),main = NA)
lines(data2.Gsmax,col=coul[2])
lines(data3.Gsmax,col=coul[3])
lines(data4.Gsmax,col=coul[4])
lines(data5.Gsmax,col=coul[5])
lines(data6.Gsmax,col=coul[6])
mtext(paste("(",mti,") Gsmax",sep=""),side=3,-3,cex=fontsize,outer=FALSE,at=18)
mti=mti+1
##
data1.JV = density (data1$JV, adjust = adjnum+1)
data2.JV = density (data2$JV, adjust = adjnum+1)
data3.JV = density (data3$JV, adjust = adjnum+1)
data4.JV = density (data4$JV, adjust = adjnum+1)
data5.JV = density (data5$JV, adjust = adjnum+1)
data6.JV = density (data6$JV, adjust = adjnum+1)
plot(data1.JV,col=coul[1],xlim = c(1,3.),ylim = c(0,3.),main = NA)
lines(data2.JV,col=coul[2])
lines(data3.JV,col=coul[3])
lines(data4.JV,col=coul[4])
lines(data5.JV,col=coul[5])
lines(data6.JV,col=coul[6])
mtext(paste("(",mti,") JV",sep=""),side=3,-3,cex=fontsize,outer=FALSE,at=1.8)
mti=mti+1
##
data1.Vcmx0 = density (data1$Vcmx0, adjust = adjnum)
data2.Vcmx0 = density (data2$Vcmx0, adjust = adjnum)
data3.Vcmx0 = density (data3$Vcmx0, adjust = adjnum)
data4.Vcmx0 = density (data4$Vcmx0, adjust = adjnum)
data5.Vcmx0 = density (data5$Vcmx0, adjust = adjnum,bw=0.5)
data6.Vcmx0 = density (data6$Vcmx0, adjust = adjnum,bw=0.2)
plot(data1.Vcmx0,col=coul[1],xlim = c(14.0,60.),ylim = c(0,0.25),main = NA)
# plot(data1.Vcmx0,col=coul[1],xlim = c(14.0,80.),ylim = c(0,0.2),main = NA)
lines(data2.Vcmx0,col=coul[2])
lines(data3.Vcmx0,col=coul[3])
lines(data4.Vcmx0,col=coul[4])
lines(data5.Vcmx0,col=coul[5])
lines(data6.Vcmx0,col=coul[6])
mtext(paste("(",mti,") Vcmax",sep=""),side=3,-3,cex=fontsize,outer=FALSE,at=40.0)
mti=mti+1
##
data1.Entrpy = density (data1$Entrpy, adjust = adjnum)
data2.Entrpy = density (data2$Entrpy, adjust = adjnum)
data3.Entrpy = density (data3$Entrpy, adjust = adjnum)
data4.Entrpy = density (data4$Entrpy, adjust = adjnum)
data5.Entrpy = density (data5$Entrpy, adjust = adjnum)
data6.Entrpy = density (data6$Entrpy, adjust = adjnum)
plot(data1.Entrpy,col=coul[1],xlim = c(640,670.),ylim = c(0,0.15),main = NA)
lines(data2.Entrpy,col=coul[2])
lines(data3.Entrpy,col=coul[3])
lines(data4.Entrpy,col=coul[4])
lines(data5.Entrpy,col=coul[5])
lines(data6.Entrpy,col=coul[6])
mtext(paste("(",mti,") Entropy",sep=""),side=3,-3,cex=fontsize,outer=FALSE,at=650)
mti=mti+1
##
data1.gddonset = density (data1$gddonset, adjust = adjnum)
data2.gddonset = density (data2$gddonset, adjust = adjnum,bw=5)
data3.gddonset = density (data3$gddonset, adjust = adjnum)
data4.gddonset = density (data4$gddonset, adjust = adjnum)
data5.gddonset = density (data5$gddonset, adjust = adjnum)
data6.gddonset = density (data6$gddonset, adjust = adjnum)#,bw=10
plot(data1.gddonset,col=coul[1],xlim = c(50,350.),ylim = c(0,0.08),main = NA)
lines(data2.gddonset,col=coul[2])
lines(data3.gddonset,col=coul[3])
lines(data4.gddonset,col=coul[4])
lines(data5.gddonset,col=coul[5])
lines(data6.gddonset,col=coul[6])
mtext(paste("(",mti,") gddonset",sep=""),side=3,-3,cex=fontsize,outer=FALSE,at=125)
mti=mti+1
##
data1.Tau_Leaf = density (data1$Tau_Leaf, adjust = adjnum)
data2.Tau_Leaf = density (data2$Tau_Leaf, adjust = adjnum)
data3.Tau_Leaf = density (data3$Tau_Leaf, adjust = adjnum)
data4.Tau_Leaf = density (data4$Tau_Leaf, adjust = adjnum)
data5.Tau_Leaf = density (data5$Tau_Leaf, adjust = adjnum,bw=0.05)
data6.Tau_Leaf = density (data6$Tau_Leaf, adjust = adjnum,bw=0.05)
# data5.Tau_Leaf = density (data5$Tau_Leaf, adjust = adjnum,bw=0.1)
# data6.Tau_Leaf = density (data6$Tau_Leaf, adjust = adjnum,bw=0.1)
# plot(data1.Tau_Leaf,col=coul[1],xlim = c(0.3,3.),ylim = c(0,6.),main = NA)
plot(data1.Tau_Leaf,col=coul[1],xlim = c(0.3,2.),ylim = c(0,10.),main = NA)
lines(data2.Tau_Leaf,col=coul[2])
lines(data3.Tau_Leaf,col=coul[3])
lines(data4.Tau_Leaf,col=coul[4])
lines(data5.Tau_Leaf,col=coul[5])
lines(data6.Tau_Leaf,col=coul[6])
mtext(paste("(",mti,") Tt_Leaf",sep=""),side=3,-3,cex=fontsize,outer=FALSE,at=1.3)
mti=mti+1
##
data1.Tau_Root = density (data1$Tau_Root, adjust = adjnum)
data2.Tau_Root = density (data2$Tau_Root, adjust = adjnum,bw=5)
data3.Tau_Root = density (data3$Tau_Root, adjust = adjnum)
data4.Tau_Root = density (data4$Tau_Root, adjust = adjnum)
data5.Tau_Root = density (data5$Tau_Root, adjust = adjnum)
data6.Tau_Root = density (data6$Tau_Root, adjust = adjnum)
plot(data1.Tau_Root,col=coul[1],xlim = c(0.3,2.),ylim = c(0,5.),main = NA)
lines(data2.Tau_Root,col=coul[2])
lines(data3.Tau_Root,col=coul[3])
lines(data4.Tau_Root,col=coul[4])
lines(data5.Tau_Root,col=coul[5])
lines(data6.Tau_Root,col=coul[6])
mtext(paste("(",mti,") Tt_Root",sep=""),side=3,-3,cex=fontsize,outer=FALSE,at=0.8)
mti=mti+1
##
data1.Tau_F = density (data1$Tau_F, adjust = adjnum)
data2.Tau_F = density (data2$Tau_F, adjust = adjnum)
data3.Tau_F = density (data3$Tau_F, adjust = adjnum)
data4.Tau_F = density (data4$Tau_F, adjust = adjnum)
data5.Tau_F = density (data5$Tau_F, adjust = adjnum)
data6.Tau_F = density (data6$Tau_F, adjust = adjnum)
plot(data1.Tau_F,col=coul[1],xlim = c(0.1,0.5),ylim = c(0,10.),main = NA)
lines(data2.Tau_F,col=coul[2])
lines(data3.Tau_F,col=coul[3])
lines(data4.Tau_F,col=coul[4])
lines(data5.Tau_F,col=coul[5])
lines(data6.Tau_F,col=coul[6])
mtext(paste("(",mti,") Tt_F",sep=""),side=3,-3,cex=fontsize,outer=FALSE,at=0.19)
mti=mti+1
##
data1.Tau_C = density (data1$Tau_C, adjust = adjnum)
data2.Tau_C = density (data2$Tau_C, adjust = adjnum)
data3.Tau_C = density (data3$Tau_C, adjust = adjnum)
data4.Tau_C = density (data4$Tau_C, adjust = adjnum)
data5.Tau_C = density (data5$Tau_C, adjust = adjnum)
data6.Tau_C = density (data6$Tau_C, adjust = adjnum)
plot(data1.Tau_C,col=coul[1],xlim = c(1,20.),ylim = c(0,0.25),main = NA)
lines(data2.Tau_C,col=coul[2])
lines(data3.Tau_C,col=coul[3])
lines(data4.Tau_C,col=coul[4])
lines(data5.Tau_C,col=coul[5])
lines(data6.Tau_C,col=coul[6])
mtext(paste("(",mti,") Tt_C",sep=""),side=3,-3,cex=fontsize,outer=FALSE,at=8.)
mti=mti+1
##
data1.Tau_Micro = density (data1$Tau_Micro, adjust = adjnum)
data2.Tau_Micro = density (data2$Tau_Micro, adjust = adjnum)
data3.Tau_Micro = density (data3$Tau_Micro, adjust = adjnum)
data4.Tau_Micro = density (data4$Tau_Micro, adjust = adjnum)
data5.Tau_Micro = density (data5$Tau_Micro, adjust = adjnum)
data6.Tau_Micro = density (data6$Tau_Micro, adjust = adjnum)
plot(data1.Tau_Micro,col=coul[1],xlim = c(0.05,0.5),ylim = c(0,15.),main = NA)
lines(data2.Tau_Micro,col=coul[2])
lines(data3.Tau_Micro,col=coul[3])
lines(data4.Tau_Micro,col=coul[4])
lines(data5.Tau_Micro,col=coul[5])
lines(data6.Tau_Micro,col=coul[6])
mtext(paste("(",mti,") Tt_Micro",sep=""),side=3,-3,cex=fontsize,outer=FALSE,at=0.23)
mti=mti+1
##
data1.Tau_SlowSOM = density (data1$Tau_SlowSOM, adjust = adjnum,bw=50)
data2.Tau_SlowSOM = density (data2$Tau_SlowSOM, adjust = adjnum)
data3.Tau_SlowSOM = density (data3$Tau_SlowSOM, adjust = adjnum,bw=50)
data4.Tau_SlowSOM = density (data4$Tau_SlowSOM, adjust = adjnum,bw=50)
data5.Tau_SlowSOM = density (data5$Tau_SlowSOM, adjust = adjnum,bw=50)
data6.Tau_SlowSOM = density (data6$Tau_SlowSOM, adjust = adjnum,bw=50)
plot(data1.Tau_SlowSOM,col=coul[1],xlim = c(5,400.),ylim = c(0,0.01),main = NA)
lines(data2.Tau_SlowSOM,col=coul[2])
lines(data3.Tau_SlowSOM,col=coul[3])
lines(data4.Tau_SlowSOM,col=coul[4])
lines(data5.Tau_SlowSOM,col=coul[5])
lines(data6.Tau_SlowSOM,col=coul[6])
mtext(paste("(",mti,") Tt_Slow",sep=""),side=3,-3,cex=fontsize,outer=FALSE,at=250)
mti=mti+1
##
data1.Tau_Passive = density (data1$Tau_Passive, adjust = adjnum)
data2.Tau_Passive = density (data2$Tau_Passive, adjust = adjnum)
data3.Tau_Passive = density (data3$Tau_Passive, adjust = adjnum)
data4.Tau_Passive = density (data4$Tau_Passive, adjust = adjnum)
data5.Tau_Passive = density (data5$Tau_Passive, adjust = adjnum)
data6.Tau_Passive = density (data6$Tau_Passive, adjust = adjnum)
plot(data1.Tau_Passive,col=coul[1],xlim = c(190,4000.),ylim = c(0,0.002),main = NA)
lines(data2.Tau_Passive,col=coul[2])
lines(data3.Tau_Passive,col=coul[3])
lines(data4.Tau_Passive,col=coul[4])
lines(data5.Tau_Passive,col=coul[5])
lines(data6.Tau_Passive,col=coul[6])
mtext(paste("(",mti,") Tt_Passive",sep=""),side=3,-3,cex=fontsize,outer=FALSE,at=2000)
mti=mti+1
##
data1.Q10 = density (data1$Q10, adjust = adjnum)
data2.Q10 = density (data2$Q10, adjust = adjnum)
data3.Q10 = density (data3$Q10, adjust = adjnum)
data4.Q10 = density (data4$Q10, adjust = adjnum)
data5.Q10 = density (data5$Q10, adjust = adjnum)
data6.Q10 = density (data6$Q10, adjust = adjnum)
plot(data1.Q10,col=coul[1],xlim = c(1,4.),ylim = c(0,1.5),main = NA)
lines(data2.Q10,col=coul[2])
lines(data3.Q10,col=coul[3])
lines(data4.Q10,col=coul[4])
lines(data5.Q10,col=coul[5])
lines(data6.Q10,col=coul[6])
mtext(paste("(",mti,") Q10",sep=""),side=3,-3,cex=fontsize,outer=FALSE,at=1.9)
mti=mti+1
##
data1.Rl0 = density (data1$Rl0, adjust = adjnum)
data2.Rl0 = density (data2$Rl0, adjust = adjnum)
data3.Rl0 = density (data3$Rl0, adjust = adjnum)
data4.Rl0 = density (data4$Rl0, adjust = adjnum)
data5.Rl0 = density (data5$Rl0, adjust = adjnum)
data6.Rl0 = density (data6$Rl0, adjust = adjnum)
plot(data1.Rl0,col=coul[1],xlim = c(10,45.),ylim = c(0,0.12),main = NA)
lines(data2.Rl0,col=coul[2])
lines(data3.Rl0,col=coul[3])
lines(data4.Rl0,col=coul[4])
lines(data5.Rl0,col=coul[5])
lines(data6.Rl0,col=coul[6])
mtext(paste("(",mti,") Rl0",sep=""),side=3,-3,cex=fontsize,outer=FALSE,at=18)
mti=mti+1
##
data1.Rs0 = density (data1$Rs0, adjust = adjnum)
data2.Rs0 = density (data2$Rs0, adjust = adjnum)
data3.Rs0 = density (data3$Rs0, adjust = adjnum)
data4.Rs0 = density (data4$Rs0, adjust = adjnum)
data5.Rs0 = density (data5$Rs0, adjust = adjnum)
data6.Rs0 = density (data6$Rs0, adjust = adjnum)
plot(data1.Rs0,col=coul[1],xlim = c(4.5,10.5),ylim = c(0,0.8),main = NA)
lines(data2.Rs0,col=coul[2])
lines(data3.Rs0,col=coul[3])
lines(data4.Rs0,col=coul[4])
lines(data5.Rs0,col=coul[5])
lines(data6.Rs0,col=coul[6])
mtext(paste("(",mti,") Rs0",sep=""),side=3,-3,cex=fontsize,outer=FALSE,at=6.5)
mti=mti+1
##
data1.Rr0 = density (data1$Rr0, adjust = adjnum)
data2.Rr0 = density (data2$Rr0, adjust = adjnum)
data3.Rr0 = density (data3$Rr0, adjust = adjnum)
data4.Rr0 = density (data4$Rr0, adjust = adjnum)
data5.Rr0 = density (data5$Rr0, adjust = adjnum)
data6.Rr0 = density (data6$Rr0, adjust = adjnum)
plot(data1.Rr0,col=coul[1],xlim = c(10,45.),ylim = c(0,0.1),main = NA)
lines(data2.Rr0,col=coul[2])
lines(data3.Rr0,col=coul[3])
lines(data4.Rr0,col=coul[4])
lines(data5.Rr0,col=coul[5])
lines(data6.Rr0,col=coul[6])
mtext(paste("(",mti,") Rr0",sep=""),side=3,-3,cex=fontsize,outer=FALSE,at=25)
mti=mti+1
##
data1.Q10rh = density (data1$Q10rh, adjust = adjnum)
data2.Q10rh = density (data2$Q10rh, adjust = adjnum)
data3.Q10rh = density (data3$Q10rh, adjust = adjnum)
data4.Q10rh = density (data4$Q10rh, adjust = adjnum+1.5)
data5.Q10rh = density (data5$Q10rh, adjust = adjnum)
data6.Q10rh = density (data6$Q10rh, adjust = adjnum)
plot(data1.Q10rh,col=coul[1],xlim = c(1,4.),ylim = c(0,1.5),main = NA)
lines(data2.Q10rh,col=coul[2])
lines(data3.Q10rh,col=coul[3])
lines(data4.Q10rh,col=coul[4])
lines(data5.Q10rh,col=coul[5])
lines(data6.Q10rh,col=coul[6])
mtext(paste("(",mti,") Q10rh",sep=""),side=3,-3,cex=fontsize,outer=FALSE,at=2.)
mti=mti+1
# ## allocation rates
data1.f_F2M = density (data1$f_F2M, adjust = adjnum)
data2.f_F2M = density (data2$f_F2M, adjust = adjnum)
data3.f_F2M = density (data3$f_F2M, adjust = adjnum)
data4.f_F2M = density (data4$f_F2M, adjust = adjnum)
data5.f_F2M = density (data5$f_F2M, adjust = adjnum)
data6.f_F2M = density (data6$f_F2M, adjust = adjnum)
plot(data1.f_F2M,col=coul[1],xlim = c(0.45,0.65),ylim = c(0,20),main = NA)
lines(data2.f_F2M,col=coul[2])
lines(data3.f_F2M,col=coul[3])
lines(data4.f_F2M,col=coul[4])
lines(data5.f_F2M,col=coul[5])
lines(data6.f_F2M,col=coul[6])
mtext(paste("(",mti,") f_F2M",sep=""),side=3,-3,cex=fontsize,outer=FALSE,at=0.55)
mti=mti+1
##
data1.f_C2M = density (data1$f_C2M, adjust = adjnum)
data2.f_C2M = density (data2$f_C2M, adjust = adjnum)
data3.f_C2M = density (data3$f_C2M, adjust = adjnum)
data4.f_C2M = density (data4$f_C2M, adjust = adjnum)
data5.f_C2M = density (data5$f_C2M, adjust = adjnum)
data6.f_C2M = density (data6$f_C2M, adjust = adjnum)
plot(data1.f_C2M,col=coul[1],xlim = c(0.25,0.3),ylim = c(0,80),main = NA)
lines(data2.f_C2M,col=coul[2])
lines(data3.f_C2M,col=coul[3])
lines(data4.f_C2M,col=coul[4])
lines(data5.f_C2M,col=coul[5])
lines(data6.f_C2M,col=coul[6])
mtext(paste("(",mti,") f_C2M",sep=""),side=3,-3,cex=fontsize,outer=FALSE,at=0.275)
mti=mti+1
# ##
data1.f_M2S = density (data1$f_M2S, adjust = adjnum)
data2.f_M2S = density (data2$f_M2S, adjust = adjnum)
data3.f_M2S = density (data3$f_M2S, adjust = adjnum)
data4.f_M2S = density (data4$f_M2S, adjust = adjnum)
data5.f_M2S = density (data5$f_M2S, adjust = adjnum)
data6.f_M2S = density (data6$f_M2S, adjust = adjnum)
plot(data1.f_M2S,col=coul[1],xlim = c(0.2,0.4),ylim = c(0,40),main = NA)
lines(data2.f_M2S,col=coul[2])
lines(data3.f_M2S,col=coul[3])
lines(data4.f_M2S,col=coul[4])
lines(data5.f_M2S,col=coul[5])
lines(data6.f_M2S,col=coul[6])
mtext(paste("(",mti,") f_M2S",sep=""),side=3,-3,cex=fontsize,outer=FALSE,at=0.3)
mti=mti+1
##
data1.f_M2P = density (data1$f_M2P, adjust = adjnum)
data2.f_M2P = density (data2$f_M2P, adjust = adjnum)
data3.f_M2P = density (data3$f_M2P, adjust = adjnum)
data4.f_M2P = density (data4$f_M2P, adjust = adjnum)
data5.f_M2P = density (data5$f_M2P, adjust = adjnum)
data6.f_M2P = density (data6$f_M2P, adjust = adjnum)
plot(data1.f_M2P,col=coul[1],xlim = c(0.05,0.2),ylim = c(0,40),main = NA)
lines(data2.f_M2P,col=coul[2])
lines(data3.f_M2P,col=coul[3])
lines(data4.f_M2P,col=coul[4])
lines(data5.f_M2P,col=coul[5])
lines(data6.f_M2P,col=coul[6])
mtext(paste("(",mti,") f_M2P",sep=""),side=3,-3,cex=fontsize,outer=FALSE,at=0.125)
mti=mti+1

##
data1.r_me = density (data1$r_me, adjust = adjnum)
data2.r_me = density (data2$r_me, adjust = adjnum)
data3.r_me = density (data3$r_me, adjust = adjnum)
data4.r_me = density (data4$r_me, adjust = adjnum)
data5.r_me = density (data5$r_me, adjust = adjnum)
data6.r_me = density (data6$r_me, adjust = adjnum)
# plot(data2.r_me,col=coul[1],xlim = c(0,0.9),ylim = c(0,12.),main = NA)
plot(data1.r_me,col=coul[1],xlim = c(0,0.7),ylim = c(0,12.),main = NA)
lines(data2.r_me,col=coul[2])
lines(data3.r_me,col=coul[3])
lines(data4.r_me,col=coul[4])
lines(data5.r_me,col=coul[5])
lines(data6.r_me,col=coul[6])
mtext(paste("(",mti,") r_me",sep=""),side=3,-3,cex=fontsize,outer=FALSE,at=0.4)
mti=mti+1
##
data1.Q10_pro = density (data1$Q10_pro, adjust = adjnum+0.5)
data2.Q10_pro = density (data2$Q10_pro, adjust = adjnum+0.5)
data3.Q10_pro = density (data3$Q10_pro, adjust = adjnum,bw=0.5)
data4.Q10_pro = density (data4$Q10_pro, adjust = adjnum,bw=0.5)
data5.Q10_pro = density (data5$Q10_pro, adjust = adjnum,bw=0.5)
data6.Q10_pro = density (data6$Q10_pro, adjust = adjnum,bw=0.5)
# data3.Q10_pro = density (data3$Q10_pro, adjust = adjnum)
# data4.Q10_pro = density (data4$Q10_pro, adjust = adjnum)
# data5.Q10_pro = density (data5$Q10_pro, adjust = adjnum)
# data6.Q10_pro = density (data6$Q10_pro, adjust = adjnum)
plot(data1.Q10_pro,col=coul[1],xlim = c(1,6.),ylim = c(0,0.9),main = NA)
lines(data2.Q10_pro,col=coul[2])
lines(data3.Q10_pro,col=coul[3])
lines(data4.Q10_pro,col=coul[4])
lines(data5.Q10_pro,col=coul[5])
lines(data6.Q10_pro,col=coul[6])
mtext(paste("(",mti,") Q10_pro",sep=""),side=3,-3,cex=fontsize,outer=FALSE,at=3)
mti=mti+1
##
data1.Omax = density (data1$Omax, adjust = adjnum)
data2.Omax = density (data2$Omax, adjust = adjnum)
data3.Omax = density (data3$Omax, adjust = adjnum)
data4.Omax = density (data4$Omax, adjust = adjnum)
data5.Omax = density (data5$Omax, adjust = adjnum)
data6.Omax = density (data6$Omax, adjust = adjnum)
plot(data1.Omax,col=coul[1],xlim = c(0,45.),ylim = c(0,0.1),main = NA)
lines(data2.Omax,col=coul[2])
lines(data3.Omax,col=coul[3])
lines(data4.Omax,col=coul[4])
lines(data5.Omax,col=coul[5])
lines(data6.Omax,col=coul[6])
mtext(paste("(",mti,") Omax",sep=""),side=3,-3,cex=fontsize,outer=FALSE,at=15)
mti=mti+1
##
data1.Tveg = density (data1$Tveg, adjust = adjnum,bw=2)
data2.Tveg = density (data2$Tveg, adjust = adjnum,bw=2)
data3.Tveg = density (data3$Tveg, adjust = adjnum,bw=2)
data4.Tveg = density (data4$Tveg, adjust = adjnum)
data5.Tveg = density (data5$Tveg, adjust = adjnum)
data6.Tveg = density (data6$Tveg, adjust = adjnum,bw=2)
plot(data1.Tveg,col=coul[1],xlim = c(0,15.),ylim = c(0,0.25),main = NA)
lines(data2.Tveg,col=coul[2])
lines(data3.Tveg,col=coul[3])
lines(data4.Tveg,col=coul[4])
lines(data5.Tveg,col=coul[5])
lines(data6.Tveg,col=coul[6])
mtext(paste("(",mti,") Tveg",sep=""),side=3,-3,cex=fontsize,outer=FALSE,at=6)
mti=mti+1
##
data1.f = density (data1$f, adjust = adjnum)
data2.f = density (data2$f, adjust = adjnum)
data3.f = density (data3$f, adjust = adjnum)
data4.f = density (data4$f, adjust = adjnum)
data5.f = density (data5$f, adjust = adjnum)
data6.f = density (data6$f, adjust = adjnum)
plot(data1.f,col=coul[1],xlim = c(0,0.5),ylim = c(0,10.),main = NA)
lines(data2.f,col=coul[2])
lines(data3.f,col=coul[3])
lines(data4.f,col=coul[4])
lines(data5.f,col=coul[5])
lines(data6.f,col=coul[6])
mtext(paste("(",mti,") f",sep=""),side=3,-3,cex=fontsize,outer=FALSE,at=0.1)
mti=mti+1
##
data1.bubprob = density (data1$bubprob, adjust = adjnum)
data2.bubprob = density (data2$bubprob, adjust = adjnum)
data3.bubprob = density (data3$bubprob, adjust = adjnum)
data4.bubprob = density (data4$bubprob, adjust = adjnum)
data5.bubprob = density (data5$bubprob, adjust = adjnum)
data6.bubprob = density (data6$bubprob, adjust = adjnum)
plot(data1.bubprob,col=coul[1],xlim = c(0,0.5),ylim = c(0,10.),main = NA)
lines(data2.bubprob,col=coul[2])
lines(data3.bubprob,col=coul[3])
lines(data4.bubprob,col=coul[4])
lines(data5.bubprob,col=coul[5])
lines(data6.bubprob,col=coul[6])
mtext(paste("(",mti,") bubprob",sep=""),side=3,-3,cex=fontsize,outer=FALSE,at=0.2)
mti=mti+1
##
data1.Vmaxfraction = density (data1$Vmaxfraction, adjust = adjnum)
data2.Vmaxfraction = density (data2$Vmaxfraction, adjust = adjnum)
data3.Vmaxfraction = density (data3$Vmaxfraction, adjust = adjnum)
data4.Vmaxfraction = density (data4$Vmaxfraction, adjust = adjnum)
data5.Vmaxfraction = density (data5$Vmaxfraction, adjust = adjnum)
data6.Vmaxfraction = density (data6$Vmaxfraction, adjust = adjnum)
plot(data1.Vmaxfraction,col=coul[1],xlim = c(0,0.2),ylim = c(0,15.),main = NA)
lines(data2.Vmaxfraction,col=coul[2])
lines(data3.Vmaxfraction,col=coul[3])
lines(data4.Vmaxfraction,col=coul[4])
lines(data5.Vmaxfraction,col=coul[5])
lines(data6.Vmaxfraction,col=coul[6])
mtext(paste("(",mti,") Vmaxfrac",sep=""),side=3,-3,cex=fontsize,outer=FALSE,at=0.08)
mti=mti+1
##### plot outer label #####
mtext(paste("Frequency",sep=''), 2, 2.5, cex=fontsize, outer=TRUE) #adhere to which edge, distrance to the edge
# mtext(paste("Porewater methane concentration (mol m"^{-3}*")")), 1, 2.5, cex=1.25, outer=TRUE) #adhere to which edge, distrance to the edge
mtext(paste("Parameter ranges",sep=''), 1,1.5, cex=fontsize, outer=TRUE) #adhere to which edge, distrance to the edge

dev.off()


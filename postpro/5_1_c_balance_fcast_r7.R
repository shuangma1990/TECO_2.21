fname='_fcast2_200'
fname='_fcast2_r4'
fname='_fcast2_r5' # r5 is the same with r4 but two extra output columns
fname='_fcast2_r7'
setwd("~/RESEARCH/TECO_SPRUCE_site/TECO_2.21_Dissertation/jpl_server/TECO_2.21/postpro/")
df1 <- read.csv(paste("3_3_mon2yr_trend",fname,".csv",sep=''),header = T)
df2 <- read.csv(paste("3_3_mon2yr_trend_sd",fname,".csv",sep=''),header = T)

plt <- rep(c(1:6),each=3)
year<- rep(c(2016:2018),6)
df1 <- cbind(plt,year,df1[,-1])
df2 <- cbind(plt,year,df2[,-1])

df3 <- aggregate(df1,by=list(df1$plt),FUN=mean)
df4 <- aggregate(df2,by=list(df2$plt),FUN=mean)

co2_678<-df3$Rh6_mean1+df3$Rh7_mean1+df3$Rh8_mean1
ch4_678<-co2_678*(df3$simuCH4_mean1/df3$Rh_mean1)
co2_678_sd<-df4$Rh6_sd1+df4$Rh7_sd1+df4$Rh8_sd1
ch4_678_sd<-co2_678_sd*(df4$simuCH4_sd1/df4$Rh_sd1)
co2_78<-df3$Rh7_mean1+df3$Rh8_mean1
ch4_78<-co2_78*(df3$simuCH4_mean1/df3$Rh_mean1)
co2_78_sd<-df4$Rh7_sd1+df4$Rh8_sd1
ch4_78_sd<-co2_78_sd*(df4$simuCH4_sd1/df4$Rh_sd1)
co2_6<-df3$Rh6_mean1
ch4_6<-co2_6*(df3$simuCH4_mean1/df3$Rh_mean1)
co2_6_sd<-df4$Rh6_sd1
ch4_6_sd<-co2_6_sd*(df4$simuCH4_sd1/df4$Rh_sd1)
par(mfrow=c(2,3),mar=c(2,2,2,2),oma=c(1, 0, 0, 1))
# # --------------------------------------------------------------
# barplot(df3$GPP_mean1)
# barplot(df3$NPP_mean1+df3$Ra_mean1)
# barplot(df3$Rh_mean1+df3$Ra_mean1+df3$QC1+df3$QC2+df3$QC3+df3$QC4+
#           df3$QC5+df3$QC6+df3$QC7+df3$QC8)
# # --------------------------------------------------------------
# #  whole system balance
# barplot(df3$QC1+df3$QC2+df3$QC3+df3$QC4+
#           df3$QC5+df3$QC6+df3$QC7+df3$QC8)
# l=df3$QC1+df3$QC2+df3$QC3+df3$QC4+
#   df3$QC5+df3$QC6+df3$QC7+df3$QC8
# barplot(-df3$Rh_mean1+df3$NPP_mean1-df3$simuCH4_mean1)
# r=-df3$Rh_mean1+df3$NPP_mean1-df3$simuCH4_mean1
# r-l
# # --------------------------------------------------------------
# #   SOC balance
# par(mfrow=c(2,3),mar=c(2,2,2,2),oma=c(1, 0, 0, 1))
# barplot(-df3$QC7-df3$QC8+df3$QCt9)
# l=-df3$QC7-df3$QC8+df3$QCt9
# co2_678<-df3$Rh6_mean1+df3$Rh7_mean1+df3$Rh8_mean1
# ch4_678<-co2_678*(df3$simuCH4_mean1/df3$Rh_mean1)
# co2_678_sd<-df4$Rh6_sd1+df4$Rh7_sd1+df4$Rh8_sd1
# ch4_678_sd<-co2_678_sd*(df4$simuCH4_sd1/df4$Rh_sd1)
# barplot(-df3$QC6_mean1+co2_678+ch4_678-df3$QCt10_mean1)
# r = df3$QC6_mean1+co2_678+ch4_678-df3$QCt10_mean1
# r = df3$QC6_mean1+co2_678+ch4_678-df3$QCt10_mean1
# r-l
# barplot(df3$QC6_mean1-df3$QCt10_mean1)
# # --------------------------------------------------------------
# #  ch4 storage change
# barplot(df3$conc1_mean1+df3$conc2_mean1+df3$conc3_mean1+df3$conc4_mean1+df3$conc5_mean1+
#           df3$conc6_mean1+df3$conc7_mean1+df3$conc8_mean1+df3$conc9_mean1+df3$conc10_mean1)
# methane<-df3$conc1_mean1+df3$conc2_mean1+df3$conc3_mean1+df3$conc4_mean1+df3$conc5_mean1+
#   df3$conc6_mean1+df3$conc7_mean1+df3$conc8_mean1+df3$conc9_mean1+df3$conc10_mean1
# methane
# 
# 
# 
# 
# library(RColorBrewer)
# # display.brewer.all(colorblindFriendly = TRUE)
# # display.brewer.pal(n = 12, name = 'Paired')
# colv2 <- brewer.pal(n = 12, name = 'Paired')
# colv <- brewer.pal(n = 8, name = "Set2")
# # --------------------------------------------------------------
# #  plot SOC balance stacked barplot
# df_rc <- t(cbind(co2_678,ch4_678))
# df_rc_sd<-t(cbind(co2_678_sd,ch4_678_sd))
# # rownames(df_rc) <- c('delta Labile C','CO2','CH4')
# rownames(df_rc) <- c('CO2','CH4')
# colnames(df_rc) <- c(1:6)
# 
# barplot(-df3$QC7_mean1-df3$QC8_mean1+df3$QCt9_mean1,ylim=c(0,900))
# 
# par(mfrow=c(1,1),mar=c(7,5,2,2),oma=c(1, 0, 0, 1))
# barplot(df_rc,yaxs='i',las=1,yaxt="n",
#         ylim=c(-200,900),
#         main = NA,
#         col = colv[6:7])
# 
# barplot(df3$QC6_mean1-df3$QCt10_mean1,
#         yaxt="n",yaxs='i',
#         col=colv2[12],add=T)
# axis(side=2,at = c(-200,0,200,400,600,800), labels=T, tck=0.03, cex.axis=0.85, mgp=c(3,0.3,0),las=1)
# # axis(side=1,at = c(1:6))
# 
# mtext("Warming gradient",side = 1,line = 5)
# box()
# 
# xsoil_av
# tsoil <- c(5.59,4.44,6.44,7.65,9.02,10.04)
# tsoil <- round(xsoil_av,2)
# # --------------------------------------------------------------
# 
# # --------------------------------------------------------------
# # --------------------------------------------------------------
# df_rc <- t(cbind(df3$QC6,df3$Rh6_mean1,df3$Rh7_mean1+df3$Rh8_mean1,df3$simuCH4_mean1))
# rownames(df_rc) <- c('delta Labile C','Rh lab','Rh rec','CH4')
# colnames(df_rc) <- c(1:6)
# # --------------------------------------------------------------
# pdf(paste("5_1_c_balance",fname,".pdf",sep=''),width=10.5, height=4.75, compress=FALSE)
# par(mfrow=c(1,2),mar=c(3,4,2,2),oma=c(1, 0, 0, 1))
# # yaxs='i' let yaxis show at 
# barplot(df_rc,xaxt="n",yaxt="n",xlim = c(0,30),xaxs='i',ylim=c(0,500),yaxs='i',
#         main = NA,
#         xlab = "Warming gradient",
#         col = colv[5:8],
#         beside = TRUE
# )
# axis(1, at=c(3,8,13,18,23,28),pos=0,mgp=c(3,0.5,0),tck=0.03,
#      labels=c(1:6))
# axis(side=2, labels=T, tck=0.03, cex.axis=0.85, mgp=c(3,0.3,0))
# mtext('C loss (gC/m2/yr)',side = 2,line = 2)
# box()
# text='delta Labile C'
# legend("topleft",inset=c(0.03,0.01),cex=0.8,y.intersp=0.8,
#        rownames(df_rc),text.width = strwidth(text)[1]/1,
#        fill = colv[5:8])
# # --------------------------------------------------------------
# barplot(-df3$QC7-df3$QC8,col=colv2[12],yaxt="n",xaxs='i',yaxs='i',
#         names=row.names(df3),ylim = c(0,800))
# axis(side=2, labels=T, tck=0.03, cex.axis=0.85, mgp=c(3,0.3,0))
# mtext('Recalcitrant C loss (gC/m2/yr)',side = 2,line = 2)
# box()
# text='delta Recalcitrant C'
# legend("topleft",inset=c(0.03,0.01),cex=0.8,y.intersp=0.8,
#        'delta Recalcitrant C',text.width = strwidth(text)[1]/1,
#        fill = colv2[12])
# 
# dev.off()
# 

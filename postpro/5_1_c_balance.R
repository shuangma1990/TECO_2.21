
setwd("~/RESEARCH/TECO_SPRUCE_site/TECO_2.21_Dissertation/jpl_server/TECO_2.21/postpro/statistics/rowmean/")
df1 <- read.csv(paste("3_3_mon2yr_trend.csv",sep=''),header = T)
df2 <- read.csv(paste("3_3_mon2yr_trend_sd.csv",sep=''),header = T)

plt <- rep(c(1:6),each=3)
year<- rep(c(2016:2018),6)
df1 <- cbind(plt,year,df1[,-1])
df2 <- cbind(plt,year,df2[,-1])

df3 <- aggregate(df1,by=list(df1$plt),FUN=mean)
df4 <- aggregate(df2,by=list(df2$plt),FUN=mean)

par(mfrow=c(2,3),mar=c(2,2,2,2),oma=c(1, 0, 0, 1))
barplot(df3$GPP_mean1)
barplot(df3$NPP_mean1+df3$Ra_mean1)
barplot(df3$Rh_mean1+df3$Ra_mean1+df3$QC1_mean1+df3$QC2_mean1+df3$QC3_mean1+df3$QC4_mean1+
          df3$QC5_mean1+df3$QC6_mean1+df3$QC7_mean1+df3$QC8_mean1)
barplot(df3$QC1_mean1+df3$QC2_mean1+df3$QC3_mean1+df3$QC4_mean1+
          df3$QC5_mean1+df3$QC6_mean1+df3$QC7_mean1+df3$QC8_mean1)
barplot(-df3$Rh_mean1+df3$NPP_mean1-df3$simuCH4_mean1)
# barplot(df3$Rh_mean1+df3$Ra_mean1)
# barplot(df3$Ra_mean1)
# barplot(df3$Rleaf_mean1+df3$Rstem_mean1+df3$Rroot_mean1)
# barplot(df3$Reco_mean1)
barplot(df3$QC6_mean1)
barplot(-df3$QC7_mean1-df3$QC8_mean1)
barplot(-df3$QC6_mean1-df3$Rh6_mean1-df3$Rh7_mean1-df3$Rh8_mean1-df3$simuCH4_mean1)
barplot(df3$NEE_mean1)
barplot(df3$Rh_mean1,names=row.names(df3))
barplot(df3$QC4_mean1+df3$QC5_mean1)
barplot(df3$QC5_mean1)
barplot(df3$QC5_mean1+df3$QC6_mean1+df3$QC7_mean1+df3$QC8_mean1)
litterin <- df3$QC4_mean1+df3$QC5_mean1+df3$QC6_mean1+df3$QC7_mean1+df3$QC8_mean1+df3$Rh_mean1+df3$simuCH4_mean1
barplot(df3$QC4_mean1+df3$QC5_mean1+df3$QC6_mean1+df3$QC7_mean1+df3$QC8_mean1+df3$Rh_mean1+df3$simuCH4_mean1)
barplot(-df3$QC4_mean1-df3$QC5_mean1+litterin)  #QC45gainedC,
# --------------------------------------------------------------
df_rc <- t(cbind(df3$QC6_mean1,df3$Rh6_mean1,df3$Rh7_mean1+df3$Rh8_mean1,df3$simuCH4_mean1))
rownames(df_rc) <- c('delta Labile C','Rh lab','Rh rec','CH4')
colnames(df_rc) <- c(1:6)

library(RColorBrewer)
# display.brewer.all(colorblindFriendly = TRUE)
# display.brewer.pal(n = 12, name = 'Paired')
colv2 <- brewer.pal(n = 12, name = 'Paired')
colv <- brewer.pal(n = 8, name = "Set2")
# --------------------------------------------------------------
pdf(paste("5_1_c_balance.pdf",sep=''),width=10.5, height=4.75, compress=FALSE)
par(mfrow=c(1,2),mar=c(3,4,2,2),oma=c(1, 0, 0, 1))
# yaxs='i' let yaxis show at 
barplot(df_rc,xaxt="n",yaxt="n",xlim = c(0,30),xaxs='i',ylim=c(0,500),yaxs='i',
        main = NA,
        xlab = "Warming gradient",
        col = colv[5:8],
        beside = TRUE
)
axis(1, at=c(3,8,13,18,23,28),pos=0,mgp=c(3,0.5,0),tck=0.03,
     labels=c(1:6))
axis(side=2, labels=T, tck=0.03, cex.axis=0.85, mgp=c(3,0.3,0))
mtext('C loss (gC/m2/yr)',side = 2,line = 2)
box()
text='delta Labile C'
legend("topleft",inset=c(0.03,0.01),cex=0.8,y.intersp=0.8,
       rownames(df_rc),text.width = strwidth(text)[1]/1,
       fill = colv[5:8])
# --------------------------------------------------------------
barplot(-df3$QC7_mean1-df3$QC8_mean1,col=colv2[12],yaxt="n",xaxs='i',yaxs='i',
        names=row.names(df3),ylim = c(0,800))
axis(side=2, labels=T, tck=0.03, cex.axis=0.85, mgp=c(3,0.3,0))
mtext('Recalcitrant C loss (gC/m2/yr)',side = 2,line = 2)
box()
text='delta Recalcitrant C'
legend("topleft",inset=c(0.03,0.01),cex=0.8,y.intersp=0.8,
       'delta Recalcitrant C',text.width = strwidth(text)[1]/1,
       fill = colv2[12])

# --------------------------------------------------------------
barplot(df3$NPP_mean1-df3$QC1_mean1-df3$QC2_mean1-df3$QC3_mean1-df3$QC4_mean1-df3$QC5_mean1)
barplot(df3$NPP_mean1)
barplot(-df3$QC1_mean1-df3$QC2_mean1-df3$QC3_mean1-df3$QC4_mean1-df3$QC5_mean1)

barplot(df3$NPP_mean1-df3$QC1_mean1-df3$QC2_mean1-df3$QC3_mean1-df3$QC4_mean1-df3$QC5_mean1-df3$QC6_mean1
        -df3$QC7_mean1-df3$QC8_mean1,ylim=c(0,900))
barplot(df_rc,xaxs='i',ylim=c(0,900),yaxs='i',
        main = NA,
        xlab = "Warming gradient",
        col = colv[5:8])
dev.off()
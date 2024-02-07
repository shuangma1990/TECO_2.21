
a=c("f1p1","f1p2","f1p3","f1p4","f1p5","f1p6")
CS<-c('_MCMC_CS1722','_MCMC_CS1722','_MCMC_CS1422','_MCMC_CS1722','_MCMC_CS1422','_MCMC_CS1622')
CS<-c('_MCMC_CS1722','_MCMC_CS1722','_MCMC_CS1422','_MCMC_CS1622','_MCMC_CS1422','_MCMC_CS1622')
# CS<-c('_MCMC_CS1722','_MCMC_CS1423','_MCMC_CS1422','_MCMC_CS1623','_MCMC_CS1422','_MCMC_CS1622')
# _MCMC_CS1423
coul = c('black','#009900','#00ff00','#cccc00',
         '#ff9900','red')

qc4 <- matrix(NA,96,6)
qc5 <- matrix(NA,96,6)
qc6 <- matrix(NA,96,6)
qc78<- matrix(NA,96,6)
qc4sd <- matrix(NA,96,6)
qc5sd <- matrix(NA,96,6)
qc6sd <- matrix(NA,96,6)
qc78sd<- matrix(NA,96,6)
for(i in 1:6){
  # i=1
  setwd("~/RESEARCH/TECO_SPRUCE_site/TECO_2.21_Dissertation/jpl_server/TECO_2.21/postpro/statistics/rowmean/")
  df1 <- read.csv(paste('rowmean',CS[i],'_',a[i],'.csv',sep=''),header = T)
  df2 <- read.csv(paste('rowsd',CS[i],'_',a[i],'.csv',sep=''),header = T)
  
  qc4[,i] <- df1$QC4_mean1
  qc5[,i] <- df1$QC5_mean1
  qc6[,i] <- df1$QC6_mean1
  qc78[,i] <- df1$QC7_mean1+df1$QC8_mean1
  
  qc4sd[,i] <- df2$QC4_sd1
  qc5sd[,i] <- df2$QC5_sd1
  qc6sd[,i] <- df2$QC6_sd1
  qc78sd[,i] <- df2$QC7_sd1+df2$QC8_sd1
  
}

# pdf(paste("figure5_v1.pdf",sep=''),width=15.5, height=7.75, compress=FALSE)
par(mfrow=c(2,3),mar=c(2,2,2,2),oma=c(1, 0, 0, 1))

matplot(qc4,type = 'l',col=coul)
matplot(qc5,type = 'l',col=coul)
matplot(qc6,type = 'l',col=coul)
matplot(qc78,type = 'l',col=coul)
# dev.off()

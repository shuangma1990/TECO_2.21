
# this script prepare data for noadjust, mean annual temperature (growting season), and ylabs for ploting in 3_42

# this script plot simulated daily Carbon pools and fluxes
library(data.table)
library(ggplot2)
library(stringr)
library(readxl)
library(Hmisc)
readpath <- "~/MANUSCRIPTS_April2020/MANUSCRIPTS/JPL_postdoc/Ma2019_acclimation/2020_PNAS/Rscript_on_server/"
#### set color ####
coul = c('black','#009900','#00ff00','#cccc00',
         '#ff9900','red')
# set index variables
# a=paste0(c(rep('f1p',6)),c(1:6))
a=c("f1p1","f1p2","f1p3","f1p4","f1p5","f1p6")
CS=c('_MCMC_CS172','_MCMC_CS172','_MCMC_CS142','_MCMC_CS172','_MCMC_CS142','_MCMC_CS162')

w=c('-2.25','+0','+2.25','+4.5','+6.75','+9')    
##### set pdf location ####
# pdf(paste("E:/Netbeans_Project/TECO_SPRUCE_2.2_acclimation_server/scripts_on_server/Rscript_on_server/ann/figures/ann_yr_results",CS,".pdf",sep=''),width=5.75, height=5.75, compress=FALSE)

labGPP <- expression(paste("GPP (gC m"^{-2}*" year"^{-1}*")"))
labNEE <- expression(paste("NEE (gC m"^{-2}*" year"^{-1}*")"))
labER <- expression(paste("ER (gC m"^{-2}*" year"^{-1}*")"))
labRh <- expression(paste("Rh (gC m"^{-2}*" year"^{-1}*")"))
labRa <- expression(paste("Ra (gC m"^{-2}*" year"^{-1}*")"))
labNPP <- expression(paste("NPP (gC m"^{-2}*" year"^{-1}*")"))
labCH4 <- expression(paste("CH"[4]*" emission (gC m"^{-2}*" year"^{-1}*")"))
labprod <- expression(paste("Production (gC m"^{-2}*" year"^{-1}*")"))
laboxid <- expression(paste("Oxidation (gC m"^{-2}*" year"^{-1}*")"))
labebu <- expression(paste("Ebullition (gC m"^{-2}*" year"^{-1}*")"))
labdiff <- expression(paste("Diffusion (gC m"^{-2}*" year"^{-1}*")"))
labpla <- expression(paste("Plant-aided (gC m"^{-2}*" year"^{-1}*")"))
labratio<-expression(paste("CO"[2]*" /CH"[4]))

#### set dimensions for vectors and matrix ####
ann2.leaf <- matrix(NA,8,6)
ann2.wood <- matrix(NA,8,6)
ann2.root <- matrix(NA,8,6)
ann2.4 <- matrix(NA,8,6)
ann2.5 <- matrix(NA,8,6)
ann2.6 <- matrix(NA,8,6)
ann2.7 <- matrix(NA,8,6)
ann2.8 <- matrix(NA,8,6)
ann2.gpp <- matrix(NA,8,6)
ann2.nee <- matrix(NA,8,6)
ann2.er <- matrix(NA,8,6)
ann2.ra <- matrix(NA,8,6)
ann2.rh <- matrix(NA,8,6)
ann2.npp <- matrix(NA,8,6)
ann2.ch4 <- matrix(NA,8,6)
ann2.prod <- matrix(NA,8,6)
ann2.oxid <- matrix(NA,8,6)
ann2.diff <- matrix(NA,8,6)
ann2.ebu <- matrix(NA,8,6)
ann2.pla <- matrix(NA,8,6)
ann2.conca<- matrix(NA,10,6)
ann2.concb<- matrix(NA,10,6)

####  for 1-6 ####
for(i in 1:6){
  # if (i==3){
  #   dire=c("MCMC_CS39")
  # }else{
  #   dire=c("MCMC_CS40")
  # }
  # i=2
  k=a[i]
  # setwd("E:/Netbeans_Project/TECO_SPRUCE_2.2_acclimation_server/scripts_on_server/Rscript_on_server/ann/")
  # setwd(paste("E:/Netbeans_Project/TECO_SPRUCE_2.2_acclimation_server/scripts_on_server/Rscript_on_server/ann/no_adjust/",k,sep = ""))  # 'sep' set for connection
  setwd(paste(readpath,"ann/no_adjust/",k,sep = ""))  # 'sep' set for connection
  # setwd(smwd)
  # i=1
  openfile_0=paste('Simu_dailyflux14001.csv')
  df0 = read.csv(openfile_0,strip.white=TRUE,header=T,sep=",")
  
  openfile_1=paste('Simu_dailyCH4.csv')
  df1 = read.csv(openfile_1,strip.white=TRUE,header=T,sep=",")
  
  print(a[i])
  getwd()  # print(smwd)
  #### cal mean and sd for the 500 files from different para sets ####
  QC1_mean1<-df0$QC1
  QC2_mean1<-df0$QC2
  QC3_mean1<-df0$QC3
  QC4_mean1<-df0$QC4
  QC5_mean1<-df0$QC5
  QC6_mean1<-df0$QC6
  QC7_mean1<-df0$QC7
  QC8_mean1<-df0$QC8
  
  GPP_mean1<-df0$GPP_d
  NEE_mean1<-df0$NEE_d
  Reco_mean1<-df0$Reco_d
  NPP_mean1<-df0$NPP_d
  Rh_mean1<-df0$Rh_d
  Ra_mean1<-df0$Ra_d
  
  simuCH4_mean1<-df1$simuCH4_d
  prod_mean1<-df1$pro_sum_d
  oxid_mean1<-df1$oxi_sum_d
  diff_mean1<-df1$fdifu1_d
  ebu_mean1<-df1$ebu_sum_sat_d
  pla_mean1<-df1$pla_sum_d
  
  
  #### cal annual ####
  # there should be a average of ann2.leaf across 6 plots and use a ratio for each plot to normalize annual gpp by /scaler
  # i=3
  for(yr in 1:8){
    # yr=5
    # i=1
    ann2.leaf[yr,i] <- mean(QC1_mean1[(1+365*(yr-1)):(365+365*(yr-1))])
    ann2.wood[yr,i] <- mean(QC2_mean1[(1+365*(yr-1)):(365+365*(yr-1))])
    ann2.root[yr,i] <- mean(QC3_mean1[(1+365*(yr-1)):(365+365*(yr-1))])
    
    ann2.4[yr,i] <- mean(QC4_mean1[(1+365*(yr-1)):(365+365*(yr-1))])
    ann2.5[yr,i] <- mean(QC5_mean1[(1+365*(yr-1)):(365+365*(yr-1))])
    ann2.6[yr,i] <- mean(QC6_mean1[(1+365*(yr-1)):(365+365*(yr-1))])
    ann2.7[yr,i] <- mean(QC7_mean1[(1+365*(yr-1)):(365+365*(yr-1))])
    ann2.8[yr,i] <- mean(QC8_mean1[(1+365*(yr-1)):(365+365*(yr-1))])
    # ann2.wood[5,i] <- mean(QC2_mean1[1461:((365+365*(yr-1)))])
    
    ann2.gpp[yr,i]<- sum(GPP_mean1[(1+365*(yr-1)):(365+365*(yr-1))])
    ann2.ra[yr,i]<- sum(Ra_mean1[(1+365*(yr-1)):(365+365*(yr-1))])
    ann2.rh[yr,i]<- sum(Rh_mean1[(1+365*(yr-1)):(365+365*(yr-1))])  
    
    ann2.er[yr,i]<- ann2.ra[yr,i]+ann2.rh[yr,i]
    ann2.nee[yr,i]<-ann2.gpp[yr,i]-ann2.er[yr,i]
    ann2.npp[yr,i]<- ann2.gpp[yr,i]-ann2.ra[yr,i]
    
    ann2.ch4[yr,i]<- sum(simuCH4_mean1[(1+365*(yr-1)):(365+365*(yr-1))])
    ann2.prod[yr,i]<- sum(prod_mean1[(1+365*(yr-1)):(365+365*(yr-1))])
    ann2.diff[yr,i]<- sum(diff_mean1[(1+365*(yr-1)):(365+365*(yr-1))])
    ann2.ebu[yr,i]<- sum(ebu_mean1[(1+365*(yr-1)):(365+365*(yr-1))])
    ann2.pla[yr,i]<- sum(pla_mean1[(1+365*(yr-1)):(365+365*(yr-1))])
    ann2.oxid[yr,i]<-(sum(oxid_mean1[(1+365*(yr-1)):(365+365*(yr-1))])+sum(pla_mean1[(1+365*(yr-1)):(365+365*(yr-1))]))
    
  }  # end of for 1-8 years
  # sd in 2011-2014 are zero because the pars do not change
}  # get yr total fluxes

# ### *********************************

CO2.2CH4=ann2.er/ann2.ch4
Rh.2ratio=ann2.rh/ann2.er
oxipercentage2=ann2.oxid/ann2.prod
df <- rbind(CO2.2CH4,Rh.2ratio,ann2.nee)

# print("save to csv file")
# setwd(readpath,"ann/")
# write.csv(df, file = paste("statistics/no_adjust/ann_yr_ratios.csv",sep=''))

ann2list <- list()

  ann2list[[1]] <- ann2.gpp
  ann2list[[2]] <- -ann2.nee
  ann2list[[3]] <- -ann2.er
  ann2list[[4]] <- -ann2.ra
  ann2list[[5]] <- -ann2.rh
  ann2list[[6]] <- ann2.npp
  ann2list[[7]] <- -ann2.ch4
  ann2list[[8]] <- -ann2.prod
  ann2list[[9]] <- ann2.oxid
  ann2list[[10]] <- -ann2.diff
  ann2list[[11]] <- -ann2.ebu
  ann2list[[12]] <- -ann2.pla

  
  # ### *********************************
  labQC1 <- expression(paste("delta leaf C (gC m"^{-2}*" year"^{-1}*")"))
  labQC2 <- expression(paste("delta leaf C (gC m"^{-2}*" year"^{-1}*")"))
  labQC3 <- expression(paste("delta leaf C (gC m"^{-2}*" year"^{-1}*")"))
  labQC4 <- expression(paste("delta leaf C (gC m"^{-2}*" year"^{-1}*")"))
  labQC5 <- expression(paste("delta leaf C (gC m"^{-2}*" year"^{-1}*")"))
  labQC6 <- expression(paste("delta leaf C (gC m"^{-2}*" year"^{-1}*")"))
  labQC7 <- expression(paste("delta leaf C (gC m"^{-2}*" year"^{-1}*")"))
  labQC8 <- expression(paste("delta leaf C (gC m"^{-2}*" year"^{-1}*")"))

  labGPP <- expression(paste("GPP (gC m"^{-2}*" year"^{-1}*")"))
  labNEE <- expression(paste("NEE (gC m"^{-2}*" year"^{-1}*")"))
  labER <- expression(paste("ER (gC m"^{-2}*" year"^{-1}*")"))
  labRh <- expression(paste("Rh (gC m"^{-2}*" year"^{-1}*")"))
  labRa <- expression(paste("Ra (gC m"^{-2}*" year"^{-1}*")"))
  labNPP <- expression(paste("NPP (gC m"^{-2}*" year"^{-1}*")"))
  labCH4 <- expression(paste("CH"[4]*" emission (gC m"^{-2}*" year"^{-1}*")"))
  labprod <- expression(paste("Production (gC m"^{-2}*" year"^{-1}*")"))
  laboxid <- expression(paste("Oxidation (gC m"^{-2}*" year"^{-1}*")"))
  labebu <- expression(paste("Ebullition (gC m"^{-2}*" year"^{-1}*")"))
  labdiff <- expression(paste("Diffusion (gC m"^{-2}*" year"^{-1}*")"))
  labpla <- expression(paste("Plant-aided (gC m"^{-2}*" year"^{-1}*")"))
  labratio<-expression(paste("CO"[2]*" /CH"[4]))
  
  labv<-c(labGPP,labNEE,labER,labRa,labRh,labNPP,labCH4,labprod,laboxid,labebu,labdiff,labpla,labratio)

  
  
  
  # ### *********************************
  ### get mean annual temperatures ####
  ## loop 1-6 ####
  df.tair <- matrix(NA,8,6)
  df.tsoil<- matrix(NA,8,6)
  for(i in 1:6){
    ## get envir from forcing files ####
    # tempwd = paste('E:/Netbeans_Project/TECO_SPRUCE_2.2_acclimation_server/scripts_on_server/Rscript_on_server/ann')
    tempwd = paste('~/MANUSCRIPTS_April2020/MANUSCRIPTS/JPL_postdoc/Ma2019_acclimation/2020_PNAS/Rscript_on_server/ann/')
    setwd(tempwd)
    df = read.table(paste(a[i],'forcing2011_2018.txt',sep=''),strip.white=TRUE,header=T,sep="")
    selcolumns <- c("year","doy","hour","Tair","Tsoil10","RH","PAR")
    df <- df[selcolumns]
    colnames(df) <- c("year","doy","hour","tair","tsoil","RH","PAR")
    envir.agg <- aggregate(cbind(tsoil,tair,RH,PAR) ~ year,data=df,
                           FUN=function(x) mn=mean(x),
                           na.action = na.pass)
    df.tair[,i] <-envir.agg$tair
    df.tsoil[,i] <-envir.agg$tsoil
  }  # get tair
  
  xair<-as.vector(df.tair[6:8,])
  xsoil<-as.vector(df.tsoil[6:8,])  
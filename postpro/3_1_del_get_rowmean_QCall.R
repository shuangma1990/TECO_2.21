# this script plot simulated daily Carbon pools and fluxes
library(data.table)
library(ggplot2)
library(stringr)
library(readxl)
library(Hmisc)
install.packages()
#### set color ####
coul = c('black','#009900','#00ff00','#cccc00',
         '#ff9900','red')
# set index variables
a=paste0(c(rep('f1p',6)),c(1:6))
# CS=c('_MCMC_CS192','_MCMC_CS172','_MCMC_CS182','_MCMC_CS192','_MCMC_CS172','_MCMC_CS192')
CS=c('_MCMC_CS172','_MCMC_CS172','_MCMC_CS142','_MCMC_CS172','_MCMC_CS142','_MCMC_CS162')
# CS=c(rep('_MCMC_CS1422',6))
# CS=c('_MCMC_CS202','_MCMC_CS202','_MCMC_CS202','_MCMC_CS202','_MCMC_CS206','_MCMC_CS206')
# # CS=c('_MCMC_CS211','_MCMC_CS211','_MCMC_CS211','_MCMC_CS211','_MCMC_CS213','_MCMC_CS211')
# CS=c(rep('_MCMC_CS200',6))

# JPL runs
CS<-c('_MCMC_CS1722','_MCMC_CS1722','_MCMC_CS1422','_MCMC_CS1722','_MCMC_CS1422','_MCMC_CS1622')
# for mtext
w=c('-2.25','+0','+2.25','+4.5','+6.75','+9')    

####  for 1-6 ####
for(i in 1:6){
  # if (i==3){
  #   dire=c("MCMC_CS39")
  # }else{
  #   dire=c("MCMC_CS40")
  # }
  # setwd("E:/Netbeans_Project/TECO_SPRUCE_2.2_acclimation_server/scripts_on_server/Rscript_on_server/ann/")
  # smwd=paste("/projects/eco_luo/sm3323/TECO_SPRUCE_2.2/src/output/SPRUCE/",a[i],CS[i],sep = "")
  smwd=paste("~/RESEARCH/TECO_SPRUCE_site/TECO_2.21_Dissertation/jpl_server/TECO_2.21/run/",a[i],CS[i],"/",sep = "")
  setwd(smwd)
  
  print(a[i])
  getwd()  # print(smwd)
  #### set empty n*m matrix  ####
  tot_para<-1
  
  GPP<-matrix(NA,96,tot_para)
  NPP<-matrix(NA,96,tot_para)
  Reco<-matrix(NA,96,tot_para)
  NEE<-matrix(NA,96,tot_para)
  Rh<-matrix(NA,96,tot_para)
  Ra<-matrix(NA,96,tot_para)
  Rleaf<-matrix(NA,96,tot_para)
  Rstem<-matrix(NA,96,tot_para)
  Rroot<-matrix(NA,96,tot_para)
  Rh4<-matrix(NA,96,tot_para)
  Rh5<-matrix(NA,96,tot_para)
  Rh6<-matrix(NA,96,tot_para)
  Rh7<-matrix(NA,96,tot_para)
  Rh8<-matrix(NA,96,tot_para)
  
  QC1<-matrix(NA,96,tot_para)
  QC2<-matrix(NA,96,tot_para)
  QC3<-matrix(NA,96,tot_para)
  QC4<-matrix(NA,96,tot_para)
  QC5<-matrix(NA,96,tot_para)
  QC6<-matrix(NA,96,tot_para)
  QC7<-matrix(NA,96,tot_para)
  QC8<-matrix(NA,96,tot_para)

  simuCH4<-matrix(NA,96,tot_para)    
  prod<-matrix(NA,96,tot_para)
  oxid<-matrix(NA,96,tot_para)
  diff<-matrix(NA,96,tot_para)
  ebu<-matrix(NA,96,tot_para)
  pla<-matrix(NA,96,tot_para)
  
  conc1<-matrix(NA,96,tot_para)
  conc2<-matrix(NA,96,tot_para)
  conc3<-matrix(NA,96,tot_para)
  conc4<-matrix(NA,96,tot_para)
  conc5<-matrix(NA,96,tot_para)
  conc6<-matrix(NA,96,tot_para)
  conc7<-matrix(NA,96,tot_para)
  conc8<-matrix(NA,96,tot_para)
  conc9<-matrix(NA,96,tot_para)
  conc10<-matrix(NA,96,tot_para)
  #### read 001-500 files in a for loop ####
  for (m in 1:tot_para)
  {
    openfile_0<-paste("Simu_mout",str_pad(m, 3, pad = "0"),".txt",sep = "")  ### to open file Simu_dail001-100.txt ###
    # openfile_0<-paste("Simu_dailyflux",str_pad(100, 3, pad = "0"),".txt",sep = "")  ### to open file Simu_dail001-100.txt ###
    d_0 = read.table(openfile_0,strip.white=TRUE,header=T,sep=",")
    GPP[,m]<-d_0$GPP_m
    NPP[,m]<-d_0$NPP_m
    Reco[,m]<-d_0$Reco_m
    NEE[,m]<-d_0$NEE_m
    Rh[,m]<-d_0$Rh_m
    Ra[,m]<-d_0$Ra_m
    Rleaf[,m]<-d_0$RLEAV_m
    Rstem[,m]<-d_0$RWOOD_m
    Rroot[,m]<-d_0$RROOT_m
    Rh4[,m]<-d_0$Rh4_m
    Rh5[,m]<-d_0$Rh5_m
    Rh6[,m]<-d_0$Rh6_m
    Rh7[,m]<-d_0$Rh7_m
    Rh8[,m]<-d_0$Rh8_m
    QC1[,m]<-d_0$QC.1.
    QC2[,m]<-d_0$QC.2.
    QC3[,m]<-d_0$QC.3.
    QC4[,m]<-d_0$QC.4.
    QC5[,m]<-d_0$QC.5.
    QC6[,m]<-d_0$QC.6.
    QC7[,m]<-d_0$QC.7.
    QC8[,m]<-d_0$QC.8.
    
    simuCH4[,m]<-d_0$ch4emission     
    prod[,m]<-d_0$ch4pro
    oxid[,m]<-d_0$ch4oxi
    diff[,m]<-d_0$ch4difu
    ebu[,m]<-d_0$ch4ebusat
    pla[,m]<-d_0$ch4pla
    conc1[,m]<-d_0$ch4v1
    conc2[,m]<-d_0$ch4v2
    conc3[,m]<-d_0$ch4v3
    conc4[,m]<-d_0$ch4v4 
    conc5[,m]<-d_0$ch4v5
    conc6[,m]<-d_0$ch4v6
    conc7[,m]<-d_0$ch4v7
    conc8[,m]<-d_0$ch4v8
    conc9[,m]<-d_0$ch4v9
    conc10[,m]<-d_0$ch4v10 
  }  # end of totpar
  
  #### cal mean and sd for the 500 files from different para sets ####
  GPP_mean1<-rowMeans(GPP, na.rm =TRUE, dims = 1)
  NPP_mean1<-rowMeans(NPP, na.rm =TRUE, dims = 1)
  Reco_mean1<-rowMeans(Reco, na.rm =TRUE, dims = 1)  
  NEE_mean1<-rowMeans(NEE, na.rm =TRUE, dims = 1)
  Rh_mean1<-rowMeans(Rh, na.rm =TRUE, dims = 1)
  Ra_mean1<-rowMeans(Ra, na.rm =TRUE, dims = 1)
  Rleaf_mean1<-rowMeans(Rleaf, na.rm =TRUE, dims = 1)
  Rstem_mean1<-rowMeans(Rstem, na.rm =TRUE, dims = 1)
  Rroot_mean1<-rowMeans(Rroot, na.rm =TRUE, dims = 1)
  Rh4_mean1<-rowMeans(Rh4, na.rm =TRUE, dims = 1)
  Rh5_mean1<-rowMeans(Rh5, na.rm =TRUE, dims = 1)
  Rh6_mean1<-rowMeans(Rh6, na.rm =TRUE, dims = 1)
  Rh7_mean1<-rowMeans(Rh7, na.rm =TRUE, dims = 1)
  Rh8_mean1<-rowMeans(Rh8, na.rm =TRUE, dims = 1)
  
  QC1_mean1<-rowMeans(QC1, na.rm =TRUE, dims = 1)
  QC2_mean1<-rowMeans(QC2, na.rm =TRUE, dims = 1)
  QC3_mean1<-rowMeans(QC3, na.rm =TRUE, dims = 1)
  QC4_mean1<-rowMeans(QC4, na.rm =TRUE, dims = 1)
  QC5_mean1<-rowMeans(QC5, na.rm =TRUE, dims = 1)
  QC6_mean1<-rowMeans(QC6, na.rm =TRUE, dims = 1)
  QC7_mean1<-rowMeans(QC7, na.rm =TRUE, dims = 1)
  QC8_mean1<-rowMeans(QC8, na.rm =TRUE, dims = 1)

  simuCH4_mean1<-rowMeans(simuCH4, na.rm =TRUE, dims = 1)
  prod_mean1<-rowMeans(prod, na.rm =TRUE, dims = 1)
  oxid_mean1<-rowMeans(oxid, na.rm =TRUE, dims = 1)
  diff_mean1<-rowMeans(diff, na.rm =TRUE, dims = 1)
  ebu_mean1<-rowMeans(ebu, na.rm =TRUE, dims = 1)
  pla_mean1<-rowMeans(pla, na.rm =TRUE, dims = 1)
  conc1_mean1<-rowMeans(conc1, na.rm =TRUE, dims = 1)
  conc2_mean1<-rowMeans(conc2, na.rm =TRUE, dims = 1)
  conc3_mean1<-rowMeans(conc3, na.rm =TRUE, dims = 1)
  conc4_mean1<-rowMeans(conc4, na.rm =TRUE, dims = 1)
  conc5_mean1<-rowMeans(conc5, na.rm =TRUE, dims = 1)
  conc6_mean1<-rowMeans(conc6, na.rm =TRUE, dims = 1)
  conc7_mean1<-rowMeans(conc7, na.rm =TRUE, dims = 1)
  conc8_mean1<-rowMeans(conc8, na.rm =TRUE, dims = 1)
  conc9_mean1<-rowMeans(conc9, na.rm =TRUE, dims = 1)
  conc10_mean1<-rowMeans(conc10, na.rm =TRUE, dims = 1)
  
  GPP_sd1<-apply(GPP, 1, sd) #1 is apply the function sd to all the elements of each row
  NPP_sd1<-apply(NPP, 1, sd)
  Reco_sd1<-apply(Reco, 1, sd)
  NEE_sd1<-apply(NEE, 1, sd) #2 is apply the function sd to all the elements of each column
  Rh_sd1<-apply(Rh, 1, sd)
  Ra_sd1<-apply(Ra, 1, sd)
  Rleaf_sd1<-apply(Rleaf, 1, sd)
  Rstem_sd1<-apply(Rstem, 1, sd)
  Rroot_sd1<-apply(Rroot, 1, sd)
  Rh4_sd1<-apply(Rh4, 1, sd)
  Rh5_sd1<-apply(Rh5, 1, sd)
  Rh6_sd1<-apply(Rh6, 1, sd)
  Rh7_sd1<-apply(Rh7, 1, sd)
  Rh8_sd1<-apply(Rh8, 1, sd)
  
  QC1_sd1<-apply(QC1, 1, sd)
  QC2_sd1<-apply(QC2, 1, sd)
  QC3_sd1<-apply(QC3, 1, sd)
  QC4_sd1<-apply(QC4, 1, sd)
  QC5_sd1<-apply(QC5, 1, sd) 
  QC6_sd1<-apply(QC6, 1, sd)
  QC7_sd1<-apply(QC7, 1, sd)
  QC8_sd1<-apply(QC8, 1, sd)
  
  simuCH4_sd1<-apply(simuCH4,1,sd)
  prod_sd1<-apply(prod,1,sd)
  oxid_sd1<-apply(oxid,1,sd)
  diff_sd1<-apply(diff,1,sd)
  ebu_sd1<-apply(ebu,1,sd)
  pla_sd1<-apply(pla,1,sd)
  conc1_sd1<-apply(conc1, 1,sd)
  conc2_sd1<-apply(conc2, 1,sd)
  conc3_sd1<-apply(conc3, 1,sd)
  conc4_sd1<-apply(conc4, 1,sd)
  conc5_sd1<-apply(conc5, 1,sd)
  conc6_sd1<-apply(conc6, 1,sd)
  conc7_sd1<-apply(conc7, 1,sd)
  conc8_sd1<-apply(conc8, 1,sd)
  conc9_sd1<-apply(conc9, 1,sd)
  conc10_sd1<-apply(conc10,1,sd)

#### create dataframe for saving ####
  df1 = data.frame(GPP_mean1,NPP_mean1,Reco_mean1,NEE_mean1,Rh_mean1,Ra_mean1,
                   Rleaf_mean1,Rstem_mean1,Rroot_mean1,Rh4_mean1,Rh5_mean1,Rh6_mean1,Rh7_mean1,Rh8_mean1,
                  QC1_mean1,QC2_mean1,QC3_mean1,QC4_mean1,QC5_mean1,QC6_mean1,QC7_mean1,QC8_mean1,
                  simuCH4_mean1,prod_mean1,oxid_mean1,diff_mean1,ebu_mean1,pla_mean1,
                  conc1_mean1,conc2_mean1,conc3_mean1,conc4_mean1,conc5_mean1,
                  conc6_mean1,conc7_mean1,conc8_mean1,conc9_mean1,conc10_mean1)
  df2 = data.frame(GPP_sd1,NPP_sd1,Reco_sd1,NEE_sd1,Rh_sd1,Ra_sd1,
                   Rleaf_sd1,Rstem_sd1,Rroot_sd1,Rh4_sd1,Rh5_sd1,Rh6_sd1,Rh7_sd1,Rh8_sd1,
                   QC1_sd1,QC2_sd1,QC3_sd1,QC4_sd1,QC5_sd1,QC6_sd1,QC7_sd1,QC8_sd1,
                   simuCH4_sd1,prod_sd1,oxid_sd1,diff_sd1,ebu_sd1,pla_sd1,
                   conc1_sd1,conc2_sd1,conc3_sd1,conc4_sd1,conc5_sd1,
                   conc6_sd1,conc7_sd1,conc8_sd1,conc9_sd1,conc10_sd1)
# df$simuCH4_mean1[df$simuCH4_mean1>3]<-0
# df$simuCH4_sd1[df$simuCH4_sd1>3]<-0
# df$ebu_mean1[df$ebu_mean1>2]<-0
# df$ebu_sd1[df$ebu_sd1>2]<-0
#
print("save to csv file")

# setwd("/projects/eco_luo/sm3323/TECO_SPRUCE_2.2/postpro/statistics") 
setwd("~/RESEARCH/TECO_SPRUCE_site/TECO_2.21_Dissertation/jpl_server/TECO_2.21/postpro/")
write.csv(df1, file = paste('rowmean',CS[i],'_',a[i],'.csv',sep=''))
write.csv(df2, file = paste('rowsd',CS[i],'_',a[i],'.csv',sep=''))
}

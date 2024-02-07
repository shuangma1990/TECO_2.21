


rm(list=ls())

a=paste0(c(rep('f1p',6)),c(1:6))
skn=2000

# setwd("/projects/eco_luo/sm3323/TECO_SPRUCE_2.2/postpro")
# getwd()
# # 
# pdf(paste("figures/",n[1],"kernel.pdf", sep = ""),
#     width=13.5, height=6.75, compress=FALSE)
# # tiff("kernel.tiff",units="in",width=12.5, height=5.75, res=300)
# # tiff("plot/aeCH4_TS_together.tiff",units="in", width=15, height=9, res=300)
df.names <- paste('data',1:6,sep='')
ParaNames=c("isimu","upgraded", "GLmax","GRmax","Gsmax","Vcmx0",
            "Tau_Leaf", "Tau_Root","Tau_F","Tau_C","Tau_Micro","Tau_SlowSOM","Tau_Passive",
            "gddonset","Q10","Rl0","Rs0","Rr0",
            "r_me","Q10_pro","Omax","Tveg","f","bubprob","Vmaxfraction",
            "Q10rh","JV","Entrpy", 'f_F2M','f_C2M', 'f_M2S','f_M2P', 
            "NA")
para_num = length(ParaNames)-3
Peak_Record<-matrix(NA,6,para_num)
#create color palette:
library(RColorBrewer)
#### set color ####
coul = c('black','#009900','#00ff00','#cccc00',
         '#ff9900','red')
adjnum=1

for(i in 1:6){
  # setwd(paste("/scratch_lg/cardamom/shuangma/TECO/TECO_2.21/run/",a[i],"_MCMC",n[i],"/output/",sep = ""))
  # setwd(paste("~/RESEARCH/TECO_SPRUCE_site/TECO_2.21_Dissertation/jpl_server/TECO_2.21/src/input/SPRUCE/",a[i],"/",sep=''))
  setwd(paste("~/MANUSCRIPTS_April2020/MANUSCRIPTS/JPL_postdoc/Ma2019_acclimation/2020_PNAS/MS/data_publish/Para_pdf_fluxes",sep=''))
  if (i==3){
    df=read.table(paste('Paraest_',a[i],'.txt',sep=''),header = F,skip =skn,sep = '')
    df=cbind(df,NA)
    }else{
    df=read.table(paste('Paraest_',a[i],'.txt',sep=''),header = F,skip =skn,sep = ',')
  }

  # data1=data1[1:2000,]
  
  colnames(df) <- ParaNames
  assign(df.names[i],df)
}

data.list <- list(data1,data2,data3,data4,data5,data6)
for(i in 1:6){
  for (ipara in 1:para_num){
    # i=1
    # ipara = 12
    df=data.list[[i]]
    meanvalue = mean(df[,ipara+2])
    # assign(df.names[i],df)
    
    # find peak of other parameters
    Peak_Record[i,ipara] = meanvalue
  }
}

dff=round(Peak_Record,2)
coln=c("GLmax","GRmax","Gsmax","Vcmx0",
       "Tau_Leaf", "Tau_Root","Tau_F","Tau_C","Tau_Micro","Tau_SlowSOM","Tau_Passive",
       "gddonset","Q10","Rl0","Rs0","Rr0",
       "r_me","Q10_pro","Omax","Tveg","f","bubprob","Vmaxfraction",
       "Q10rh","JV","Entrpy", 'f_F2M','f_C2M', 'f_M2S','f_M2P')
colnames(dff) <- coln

selcolumns <- c("GLmax","GRmax","Gsmax","JV","Vcmx0","Entrpy","gddonset",
                "Tau_Leaf", "Tau_Root","Tau_F","Tau_C","Tau_Micro","Tau_SlowSOM","Tau_Passive",
                "Q10","Rl0","Rs0","Rr0","Q10rh",'f_F2M','f_C2M', 'f_M2S','f_M2P',
                "r_me","Q10_pro","Omax","Tveg","f","bubprob","Vmaxfraction")
ddff <- dff[,selcolumns]

print("save to csv file")
setwd("~/RESEARCH/TECO_SPRUCE_site/TECO_2.21_Dissertation/jpl_server/TECO_2.21/postpro/statistics/posterior_distribution/")
# write.csv(ddff, file = paste("mean_record_from_nau.csv",sep=''))
write.csv(ddff, file = paste("mean_record_finalfluxes.csv",sep=''))


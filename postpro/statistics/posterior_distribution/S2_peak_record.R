


rm(list=ls())

# n=c('_CS172','_CS172','_CS142','_CS172','_CS142','_CS162')
n=c('_CS172','_CS172','_CS142','_CS172','_CS142','_CS162')
# n=c('_CS1422','_CS1422','_CS1422','_CS172','_CS142','_CS162')
# n=c(rep('_CS1422',6))
# JPL runs
n=c('_CS1721','_CS1721','_CS1421','_CS1721','_CS1421','_CS1621')
n=c('_CS1722','_CS1722','_CS1422','_CS1722','_CS1422','_CS1622')
n=c('_CS1723','_CS1723','_CS1423','_CS1723','_CS1423','_CS1623')
n=c('_CS1724','_CS1724','_CS1424','_CS1724','_CS1424','_CS1624')
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
  setwd(paste("/scratch_lg/cardamom/shuangma/TECO/TECO_2.21/run/",a[i],"_MCMC",n[i],"/output/",sep = ""))
  # setwd('E:/Netbeans_Project/TECO_SPRUCE_2.2_acclimation_server/scripts_on_server/Rscript_on_server')
  
  
  df=read.table('Paraest.txt',header = F,skip =skn,sep = ',')
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
DensityInfo = density (df[,ipara+2], adjust = adjnum)
# assign(df.names[i],df)

  # find peak of other parameters
  Peak_Record[i,ipara] = DensityInfo$x[which.max(DensityInfo$y)]
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
setwd("/scratch_lg/cardamom/shuangma/TECO/TECO_2.21/postpro/statistics/")
write.csv(ddff, file = paste("peak_record",n[1],".csv",sep=''))


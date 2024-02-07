a=c("f1p1","f1p2","f1p3","f1p4","f1p5","f1p6")

### get mean annual temperatures ####
readpath <- "~/MANUSCRIPTS_April2020/MANUSCRIPTS/JPL_postdoc/Ma2019_acclimation/2020_PNAS/Rscript_on_server/"
## loop 1-6 ####
df.tair <- matrix(NA,8,6)
df.tsoil<- matrix(NA,8,6)
for(i in 1:6){
  ## get envir from forcing files ####
  setwd(paste(readpath,"ann/",sep=''))
  df = read.table(paste(a[i],'forcing2011_2018.txt',sep=''),strip.white=TRUE,header=T,sep="")
  selcolumns <- c("year","doy","hour","Tair","Tsoil10","RH","PAR")
  df <- df[selcolumns]
  # # keep growing season days records only
   # df<-df[!(df$doy<122 | df$doy>244),]
  colnames(df) <- c("year","doy","hour","tair","tsoil","RH","PAR")
  envir.agg <- aggregate(cbind(tsoil,tair,RH,PAR) ~ year,data=df,
                         FUN=function(x) mn=mean(x),
                         na.action = na.pass)
  df.tair[,i] <-envir.agg$tair
  df.tsoil[,i] <-envir.agg$tsoil
}  # get tair
# 
# xair<-as.vector(df.tair[6:8,2:6])
# xsoil<-as.vector(df.tsoil[6:8,2:6])
xair<-as.vector(df.tair[6:8,])
xsoil<-as.vector(df.tsoil[6:8,])

xsoil_av<- colMeans(df.tsoil[6:8,])
xair_av<- colMeans(df.tair[6:8,])

plot(xair_av)
 
plot(xsoil_av)

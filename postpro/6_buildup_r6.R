# fname='_fcast2_r6' 
# --------------------------------------------------------------
library(ggplot2)
library(viridis)
# library(hrbrthemes)
# install.packages("hrbrthemes")
# --------------------------------------------------------------
# create a dataset
xsoil_av
tsoil <- round(c(5.59,4.44,6.44,7.65,9.02,10.04),1)
tsoil <- round(xsoil_av,1)
# --------------------------------------------------------------
# dataframe for stacked bars on positive y axis
soilt <-rep(tsoil,2)
component<-rep(c('CH4','CO2'),each=6)
dtmean <- c(ch4_678,co2_678)
dtmeanp<-dtmean
dtstd <- c(ch4_678_sd,co2_678_sd)
dtmin <- dtmean-dtstd
dtmax <- dtmean+dtstd
dt <- data.frame(soilt,component,dtmean,dtmin,dtmax,dtmeanp)
# stack the sd for dataframe dt
dt$dtmin[dt$component == "CO2"] <- with(dt,dtmin[component == "CO2"] +
                                          dtmin[component == "CH4"])
dt$dtmax[dt$component == "CO2"] <- with(dt,dtmax[component == "CO2"] +
                                          dtmax[component == "CH4"])
dt$dtmeanp[dt$component == "CO2"] <- with(dt,dtmean[component == "CO2"] +
                                          dtmean[component == "CH4"])
# --------------------------------------------------------------
# dataframe for the bar on negative y axis
soilt2<-tsoil
component2<-rep('delta Labile C',6)
dtmean2<-df3$dQC6-df3$QCt10
dtstd2<-df4$dQC6-df4$QCt10_sd1
dtmin2 <- dtmean2-dtstd2
dtmax2 <- dtmean2+dtstd2
dt2 <- data.frame(soilt2,component2,dtmean2,dtmin2,dtmax2)
# --------------------------------------------------------------
# dataframe for the line chart for the total of all bars (net recalcitrant c change)
soilt3<-tsoil
dtmean3<-df3$dQC6+co2_678+ch4_678-df3$QCt10
dtstd3<-df4$dQC6+co2_678_sd+ch4_678_sd-df4$QCt10_sd1
dtmin3 <- dtmean3-dtstd3
dtmax3 <- dtmean3+dtstd3
component3<-rep('net recalcitrant C loss,6')
dt3<-data.frame(soilt3,component3,dtmean3,dtmin3,dtmax3)
# --------------------------------------------------------------
# # --------------------------------------------------------------
# # dataframe for stacked bars on positive y axis, litter in
# soilt <-rep(tsoil,2)
# component<-rep(c('CH4','CO2'),each=6)
# dtmean <- c(ch4_678,co2_678)
# dtstd <- c(ch4_678_sd,co2_678_sd)
# dtmin <- dtmean-dtstd
# dtmax <- dtmean+dtstd
# dt <- data.frame(soilt,component,dtmean,dtmin,dtmax)
# # stack the sd for dataframe dt
# dt$dtmin[dt$component == "CO2"] <- with(dt,dtmin[component == "CO2"] +
#                                           dtmin[component == "CH4"])
# dt$dtmax[dt$component == "CO2"] <- with(dt,dtmax[component == "CO2"] +
#                                           dtmax[component == "CH4"])
# plot
levels(as.factor(dt$component))
a<-factor(dt$component, levels=c("CO2","CH4"))
# scale_fill_viridis(discrete = T) +
# --------------------------------------------------------------
setwd("~/RESEARCH/TECO_SPRUCE_site/TECO_2.21_Dissertation/jpl_server/TECO_2.21/postpro/")
pdf(paste("6_buildup",fname,".pdf",sep=''),width=6, height=3., compress=FALSE)

barcolv <- c( "#35978f","#80cdc1","#bf812d")
barcolv <- c( "#4575b4","#74add1","#bf812d",alpha("#c00000", alpha = 0.8))
barcolv <- c( "#39568cff","#287d8eff","#29AF7FFF",alpha("#fde725ff", alpha = 0.8))
# blue yellow green green    DCE319FF
# barcolv <- alpha(barcolv, alpha = 0.6)
myplot=ggplot()+
  geom_bar(data = dt,aes(fill=a, y=dtmean, x=soilt),position="stack", stat="identity") +
  xlab("Mean Annual Soil Temperature (°C, -0.1m)")+
  scale_y_continuous(limits=c(-200, 1000))+
  scale_x_continuous(breaks=tsoil)+
  geom_bar(data = dt2,aes(y=dtmean2, x=soilt2,fill=component2),position="stack", stat="identity") +
  geom_errorbar(data=dt,aes(y=dtmean, x=soilt,ymin=dtmin, ymax=dtmax),position = "identity",width=0.2)+
  geom_point(data=dt,aes(y=dtmeanp, x=soilt),color='black',fill="white",size = 1,shape=21,stroke=1)+
  geom_errorbar(data=dt2,aes(y=dtmean2, x=soilt2,ymin=dtmin2, ymax=dtmax2),position = "identity",width=0.2)+
  geom_point(data=dt2,aes(y=dtmean2, x=soilt2),color='black',fill="white",size = 1,shape=21,stroke=1)+
  geom_errorbar(data=dt3,aes(y=dtmean3,x=soilt3,ymin=dtmin3,ymax=dtmax3),position = "identity",width=0.1,size=1.2,color=barcolv[4])+
  geom_line(data = dt3, aes(x = soilt3, y = dtmean3,color="Net recalcitrant C loss"), size=1.5)+
  ylab(expression("Mean Annual Carbon flux (gC/m"^2*"/yr)"))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_fill_manual(name=NULL,values=barcolv[1:3],labels=c(expression('CH'[4]),expression('CO'[2]),expression("C gain from labile C and litter")))+
  scale_color_manual(name=NULL,values = barcolv[4],labels=expression("Net recalcitrant C loss"))+
  theme(legend.position=c(0.35,0.8),legend.direction = "horizontal")+
  theme(axis.ticks.length=unit(-0.25, "cm"), 
        # adjust X-axis title
        axis.title.x = element_text(size = 10),
        # adjust X-axis labels; also adjust their position using margin (acts like a bounding box)
        # using margin was needed because of the inwards placement of ticks
        # http://stackoverflow.com/questions/26367296/how-do-i-make-my-axis-ticks-face-inwards-in-ggplot2
        axis.text.x = element_text(size = 10, margin = unit(c(t = 4.5, r = 0, b = 0, l = 0), "mm")),
        # adjust Y-axis title
        axis.title.y = element_text(size = 10, face = "bold"),
        # adjust Y-axis labels
        axis.text.y = element_text(size = 10, margin = unit(c(t = 0, r = 4.5, b = 0, l = 0), "mm")))
print(myplot)
dev.off()
# geom_point(shape = 1,size = 2,colour = "black")+

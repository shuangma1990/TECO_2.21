# OLS AND TLS in R and math:
#  best described here:
#  http://fabian-kostadinov.github.io/2015/01/27/cointegration-and-total-least-squares-regression/

#  https://cerebralmastication.com/2010/09/principal-component-analysis-pca-vs-ordinary-least-squares-ols-a-visual-explination/
#  https://stats.stackexchange.com/questions/13152/how-to-perform-orthogonal-regression-total-least-squares-via-pca
#  https://stackoverflow.com/questions/31862766/linear-regression-using-lm-surprised-by-the-result
#  https://towardsdatascience.com/total-least-squares-in-comparison-with-ols-and-odr-f050ffc1a86a

#----------------------------------------------------------------------------
x <- dt3$soilt3
y <- dt3$dtmean3
fit <- lm(y ~ x)   ## linear regression
slp <- round(fit[["coefficients"]][2],digits = 2)
summary(fit)
# plot.new()
plot(x,y,
     xlab = paste0(" CH4 emission Tg/month",sep=''),
     ylab = "GC Posterior CH4 emission Tg/month",cex=1.2)
# #-----  OLS  ----------------------------------------------------------------
x0 <- seq(min(x), max(x), length = 20)  ## prediction grid
y0 <- predict.lm(fit, newdata = list(x = x0))  ## predicted values
lines(x0, y0, col = 2)  ## add regression curve (colour: red)
#-----  TLS   ---------------------------------------------------------------
v <- prcomp(cbind(x,y))$rotation
beta <- v[2,1]/v[1,1]
inter <- mean(y) - beta*mean(x)
x00 <- seq(min(x), max(x), length = 20)
y00 <- beta*x00+inter
lines(x00,y00,col="blue",lwd=2)
slp2 <- round(beta,digits = 2)
#----------------------------------------------------------------------------
a=round(rsq(x,y),digits = 2)
b=round(RMSE(x,y),digits = 2)
mtext(paste('R square = ',a,sep=''),outer=FALSE,side=3,line = -2.,col = "red",
      at=par("usr")[1]+0.22*diff(par("usr")[1:2]),cex=rsize)
mtext(paste('Slope = ',slp2,sep=''),outer=FALSE,side=3,line = -4.,col = "red",
      at=par("usr")[1]+0.2*diff(par("usr")[1:2]),cex=rsize) 
mtext(paste('RMSE =',b,sep=''),outer=FALSE,side=3,line = -6.,col = "black",
      at=par("usr")[1]+0.2*diff(par("usr")[1:2]),cex=rsize)
abline (0,1,lty="dashed")
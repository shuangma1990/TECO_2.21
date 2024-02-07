#  this script find exponential regression for a dataframe of x and y, 
#  the regression line is names with (xvalues, yhat)
# summary(exp_model) run the statistic results, Adjusted R-squared shows how much the variance is explained
# the change of y and x

# script 8_build_regression
y = dt[1:6,3]
x = dt[1:6,1]
exp_model<-lm(log(y) ~ x)
# this fit a model in form of 'y= alpha e^(beta x)', 
# if need a 'y= alpha e^(beta x) + theta' form see below: (enzyme reaction rate problems can assume zero at 0 degree)
# you can find user defined functions below:
# https://rpubs.com/mengxu/exponential-model  
# https://www.statforbiology.com/nonlinearregression/usefulequations#linear_equation
summary(exp_model)
coef(exp_model)[1]

xvalues <- seq(0, 10, 0.1)

yhat <- exp(predict(exp_model,list(x=xvalues)))

plot(x, y,pch=16)
lines(xvalues, yhat,lwd=2, col = "red")

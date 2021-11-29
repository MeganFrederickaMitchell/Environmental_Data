setwd("C:/Users/Megan Mitchell/OneDrive - University of Massachusetts/Environmental_Data/ECO_634_Data")
dis = read.csv("dispersal (1).csv")
sal_dis = read.csv("salamander_dispersal.csv")


## Prelab- Ricker Function

ricker_fun = function(x, a, b) 
{
  return(a * x * exp(-b * x))
}

curve(
  ricker_fun(x, 1, 1), 
  from = 0, to = 5, add = FALSE, 
  main = "Ricker function: a = 1, b = 1",
  ylab = "f(x)", xlab = "x")

exp_fun = function(x, a, b) 
{
  return(a * exp(-b * x))
}
curve(
  exp_fun(x, 2.2, 1/15), add = FALSE, from = 0, to = 50,
  ann = FALSE, axes = TRUE, ylab = "f(x)"); box()

salamander
dispersal
sal_dis$disp.rate.ftb
head(salamander)
disp.rate.ftb
salamander$disp.rate.ftb  
salamander$dist.class

## Prelab Simulated Data on a Line No errors
set.seed(1234567)
n_pts = 50
x_min = 2
x_max = 10
x_sim = runif(n_pts, min = x_min, max = x_max)
param_intercept = 2.3
param_slope = 0.67
y_pred = param_intercept + x_sim * param_slope
plot(x_sim, y_pred, main = "Simulated Data\nNo Errors", xlab = "", ylab = "")

## Prelab  normal errors 1

error_mean = 0
error_sd = 0.25

y_observed= 
  y_pred + 
  rexp(
    n = n_pts, 
    mean = error_mean, 
    sd = error_sd)
plot(x_sim, y_observed, main = "Normally Distributed Errors\n Constant Variance", xlab = "", ylab = "")

## Normal Errors 2

error_mean = 0
error_sd = 0.1

y_observed_2 = 
  y_pred + 
  rnorm(
    n = n_pts, 
    mean = error_mean, 
    sd = error_sd * x_sim)


par(mfrow = c(1, 2))
plot(x_sim, y_observed, main = "Normally Distributed Errors\n Constant Variance", xlab = "", ylab = "")
plot(x_sim, y_observed_2, main = "Normally Distributed Errors\n Increasing Variance", xlab = "", ylab = "")

## normal errors 3 prelab

rexp(n, rate= 1.2)

y_observed_3 = 
  y_pred + 
  rexp(
    n = n_pts, 
    rate = 1.2 ) 

par(mfrow = c(3, 1))
plot(x_sim, y_observed)
plot(x_sim, y_observed_2)
plot(x_sim, y_observed_3)

par(mfrow = c(3, 1))
hist(y_observed - y_pred, main = "sim data 1", xlab = "observed y=values")
hist(y_observed_2 - y_pred, main = "sim data 2", xlab = "observed y=values")
hist(y_observed_3 - y_pred, main = "sim data 3", xlab = "observed y=values")

## prelab exponential function curve

exp_fun = function(x, a, b) 
{
  return(a * exp(-b * x))
}
ricker_fun = function(x, a, b) 
{
  return(a * x * exp(-b * x))
}
dev.off()
curve(
  ricker_fun(x, 1, 1), 
  from = 0, to = 5, add = FALSE, 
  main = "Ricker function: a = 1, b = 1",
  ylab = "f(x)", xlab = "x")

curve(
  ricker_fun(x, 2.2, 1/15), add = FALSE, from = 0, to = 50,
  ann = FALSE, axes = TRUE, ylab = "f(x)"); box()

 ## prelab 4 exponential plots
exp_fun_1 = function(x, a, b) 
{
  return(a * exp(-b * x))
}
curve(
  exp_fun(x, 1.9, 0.1), add = FALSE, from = 0, to = 50,
  ann = FALSE, axes = TRUE, ylab = "f(x)"); box()

exp_fun_2 = function(x, a, b) 
{
  return(a * exp(-b * x))
}
curve(
  exp_fun(x, 1.9, 0.3), add = FALSE, from = 0, to = 50,
  ann = FALSE, axes = TRUE, ylab = "f(x)"); box()

exp_fun_3 = function(x, a, b) 
{
  return(a * exp(-b * x))
}
curve(
  exp_fun(x, 1.2, 0.2), add = FALSE, from = 0, to = 50,
  ann = FALSE, axes = TRUE, ylab = "f(x)"); box()

exp_fun_4 = function(x, a, b) 
{
  return(a * exp(-b * x))
}
curve(
  exp_fun(x, 1.2, 0.4), add = FALSE, from = 0, to = 50,
  ann = FALSE, axes = TRUE, ylab = "f(x)"); box()


help(exp)


hist(x = sal_dis$disp.rate.ftb)
hist(x = sal_dis$dist.class)

plot(x= sal_dis$dist.class, y= sal_dis$disp.rate.ftb)
curve(logistic_midpoint_slope(x, midpoint = 750, slope = 0.1), add = TRUE)




curve(logistic_midpoint_slope(x, midpoint = 80, slope = -1.5), add = TRUE)
help(curve)


get_logistic_param_a = function(slope, midpoint)
{
  b = slope / 4
  return (-midpoint * (slope / 4))
  
}

help(rexp)

param_intercept = 2.3
param_slope = 0.67
y_pred = param_intercept + x_sim * param_slope
plot(sal_dis$dist.class, sal_dis$disp.rate.ftb)

exp_fun = function(x, a, b) 
{
  return(a * exp(-b * x))
}
curve(
  exp_fun(x, 2.2, 1/15), add = FALSE, from = 0, to = 50,
  ann = FALSE, axes = TRUE, ylab = "f(x)"); box()

plot(sal_dis$dist.class, sal_dis$disp.rate.ftb, main = "Salamander Dispersal", xlab= "Distribution class", ylab = "Dispersal rate")

#################################################################################################
## Q4 LAB 05
dev.off()
par(mfrow=c(2,2))
exp_fun = function(x, a, b) 
{
  return(a * exp(-b * x))
}
curve(
  exp_fun(x, 1.9, 0.1), add = TRUE, from = 0, to = 50,
  main = "Exponential Curve",
  ylab= "", xlab= "")

curve(
  exp_fun(x, 1.9, 0.3), add = TRUE, from = 0, to = 50,
  main = "Exponential Curve",
  ylab= "", xlab= "",
  lty=2)

curve(
  exp_fun(x, 1.2, 0.2), add = TRUE, from = 0, to = 50,
  main = "Exponential Curve",
  ylab= "", xlab= "",
  col="red")

curve(
  exp_fun(x, 1.2, 0.4), add = TRUE, from = 0, to = 50,
  main = "Exponential Curve",
  ylab= "", xlab= "",
  lty=2,
  col="red")

curve(
  exp_fun(x, 1.9, 0.3), add = TRUE, from = 0, to = 50,
  main = "Exponential Curve",
  ylab= "", xlab= "")

curve(
  exp_fun(x, 1.9, 0.4), add = TRUE, from = 0, to = 50,
  main = "Exponential Curve",
  ylab= "", xlab= "",
  lty=2)

curve(
  exp_fun(x, 1.9, 0.5), add = TRUE, from = 0, to = 50,
  main = "Exponential Curve",
  ylab= "", xlab= "",
  col="red")

curve(
  exp_fun(x, 1.9, 0.6), add = TRUE, from = 0, to = 50,
  main = "Exponential Curve",
  ylab= "", xlab= "",
  lty=2,
  col="red")
dev.off()
#  ann = FALSE, axes = TRUE, ylab = "f(x)"); box()



dev.off()
## Q5 LAB 05

par(mfrow = c(3, 3))

#exp_fun = function(x, a, b) 
#{
 # return(a * x * exp(-b * x))
#}
ricker_fun = function(x, a, b) 
{
  return(a * x * exp(-b * x))
}
curve(
  ricker_fun(x, 25, 0.1), add = TRUE, from = 0, to = 50, 
  main = "Ricker Function",
  ylab= "f(x)", xlab = "x")

curve(
  ricker_fun(x, 20, 0.2), add = TRUE, from = 0, to = 50, 
  main = "Ricker Function",
  ylab= "f(x)", xlab = "x",
  lty=2)

curve(
  ricker_fun(x, 10, 0.2), add = TRUE, from = 0, to = 50, 
  main = "Ricker Function",
  ylab= "f(x)", xlab = "x",
  lty=2)

curve(
  ricker_fun(x, 75, 0.3), add = TRUE, from = 0, to = 50, 
  main = "Ricker Function",
  ylab= "f(x)", xlab = "x",
  col = "red")

curve(
  ricker_fun(x, 50, 0.3), add = TRUE, from = 0, to = 50, 
  main = "Ricker Function",
  ylab= "f(x)", xlab = "x",
  lty=2,
  col="red")

curve(
  ricker_fun(x, 40, 0.3), add = TRUE, from = 0, to = 50, 
  main = "Ricker Function Curve",
  ylab= "f(x)", xlab = "x",
  lty=2,
  col="red")

dev.off ()
  #ann = FALSE, axes = TRUE, ylab = "f(x)"); box() 
#plot(sal_dis$dist.class, sal_dis$disp.rate.ftb, main = "Salamander Dispersal", xlab= "Distribution class", ylab = "Dispersal rate")

dev.off()

curve(
  exp_fun(x, 1.9, 0.1), add = FALSE, from = 0, to = 50, 
main = "TEST",
ylab = "f(x)", xlab = "x")

#Q9
setwd("C:/Users/Megan Mitchell/OneDrive - University of Massachusetts/Environmental_Data/ECO_634_Data")
dis = read.csv("dispersal (1).csv")
dis
sal_dis = read.csv("salamander_dispersal.csv")
sal_dis$dist.class
sal_dis$disp.rate.ftb
sal_dis$disp.rate.eb

hist(x = sal_dis$disp.rate.ftb)
hist(x = sal_dis$dist.class)

plot(x= sal_dis$dist.class, y= sal_dis$disp.rate.ftb)

line_point_slope = function(x, x1, y1, slope)
{
  get_y_intercept = 
    function(x1, y1, slope) 
      return(-(x1 * slope) + y1)++++++++++++++
    
  
  linear = 
    function(x, yint, slope) 
      return(yint + x * slope)
  
  return(linear(x, get_y_intercept(x1, y1, slope), slope))
}

plot(x= sal_dis$dist.class, y= sal_dis$disp.rate.ftb, lwd= 2.0, main = "Salamander data with fitted linear model", xlab="distribution class", ylab="dispersal probability")
curve(line_point_slope(x, 600, .39, -0.0001), lwd= 2.0, add= TRUE)

#abline(h=1)
dev.off()

###################### Q 10 EXPONENTIAL MODEL WITH SALAMANDER SCATTER PLOT
exp_fun = function(x, a, b) 
{
  return(a * x * exp(-b * x))
}
plot(x= sal_dis$dist.class, y= sal_dis$disp.rate.ftb, lwd= 2.0, main = "Salamander data with fitted exponential model", xlab="distribution class", ylab="dispersal probability")
curve(exp_fun(x, 0.045, 0.01), lwd = 2.0, add = TRUE) 
  
sal_dis$dist.class  
sal_dis$disp.rate.ftb
dev.off()
#from = 0, to = 50,
  #ann = FALSE, axes = TRUE, ylab = "f(x)"); box()
#############Q13 SALAMANDER DATA WITH FITTED RICKER FUNCTION

ricker_fun = function(x, a, b) 
{
  return(a * x * exp(-b * x))
}
plot(sal_dis$dist.class,sal_dis$disp.rate.ftb, lwd= 2.0,  main = "Salamander data with fitted ricker function", xlab="distribution class", ylab="dispersal probability")
curve(ricker_fun(x, 0.008, 0.0044), lwd= 2.0, add = TRUE)


dev.off()


#ricker_fun = function(x, a, b) 
#{
 # return(a * x * exp(-b * x))
#}
#plot(x= sal_dis$dist.class, y= sal_dis$disp.rate.ftb, main = "Salamander data with fitted exponential model", xlab="distribution class", ylab="dispersal probability")
#curve(
 # ricker_fun(x, 0.0050, 0.001), add = TRUE,)


#curve(logistic_midpoint_slope(x, midpoint = 80, slope = -1.5), add = TRUE)
#help(curve)

dev.off()
#Q11
#Q13
#Q14 Residuals
par(mfrow=c(3,1))
sal_dis$linear = line_point_slope(sal_dis$dist.class, 600, .4, -0.0001)
sal_dis$exp_fun = exp_fun(sal_dis$dist.class, 0.045, 0.01)
sal_dis$ricker_fun = ricker_fun(sal_dis$dist.class, 0.007, 0.0044)

sal_dis$resids_linear = sal_dis$disp.rate.ftb - sal_dis$linear
sal_dis$resids_exp = sal_dis$disp.rate.ftb - sal_dis$exp_fun
sal_dis$resids_ricker = sal_dis$disp.rate.ftb - sal_dis$ricker_fun

hist(sal_dis$resids_linear, lwd = 2.0, main = "Histogram of linear residuals", xlab = "", ylab = "", col = "maroon")
hist(sal_dis$resids_exp, lwd= 2.0, main = "Histogram of exponential residuals", xlab = "", ylab = "", col = "darkturquoise")
hist(sal_dis$resids_ricker, lwd = 2.0, main = "Histogram of ricker function residuals", xlab = "", ylab = "", col = "purple")

data_frame = data.frame(sal_dis$resids_exp, sal_dis$resids_linear, sal_dis$resids_ricker)
data_frame
data.frame = sal_dis
data.frame


setwd("C:/Users/Megan Mitchell/OneDrive - University of Massachusetts/Environmental_Data/ECO_634_Data")
#png(file="C:/Users/Megan Mitchell/OneDrive - University of Massachusetts/Environmental_Data/ECO_634_Data/Lab_1_hist_1.png",
  #  width=1500, height=1600)  



norm_30 = rnorm(n= 30, mean= 10.4, sd = 2.4) 
norm_300 = rnorm(n= 300, mean= 10.4, sd = 2.4)
norm_3000 = rnorm(n= 3000, mean= 10.4 , sd = 2.4)

par(mfrow = c(2,2)) 

hist(norm_17, main = "Histogram with 17 observations", xlab= "")
hist(norm_30, main = "Histogram with 30 observations",  xlab= "")
hist(norm_300, main = "Histogram with 300 observations",  xlab= "")
hist(norm_3000, main = "Histogram with 3000 observations",  xlab= "")

#png(file="C:/Users/Megan Mitchell/OneDrive - University of Massachusetts/Environmental_Data/ECO_634_Data/Hist_0_1.png",
  #  width=1500, height=1600)  

#png(file="C:/Users/Megan Mitchell/OneDrive - University of Massachusetts/Environmental_Data/ECO_634_Data/Lab_4_Hist_01.png", width=1500, height=1600)

  par(mfrow = c(2,2)) 
hist(norm_17, main = "Histogram with 17 observations", xlab= "")
hist(norm_30, main = "Histogram with 30 observations",  xlab= "")
hist(norm_300, main = "Histogram with 300 observations",  xlab= "")
hist(norm_3000, main = "Histogram with 3000 observations",  xlab= "")
dev.off()


pnorm(norm_17, mean = 2.4, sd = 10.4, lower.tail = TRUE, log.p = FALSE)
pnorm(norm_30, mean = 2.4, sd = 10.4, lower.tail = TRUE, log.p = FALSE)
pnorm(norm_300, mean = 2.4, sd = 10.4, lower.tail = TRUE, log.p = FALSE)
pnorm(norm_3000, mean = 2.4, sd = 10.4, lower.tail = TRUE, log.p = FALSE)


help(dnorm)

x = seq(-3, 3, length.out = 1000)
y = dnorm(x)

plot(x, y, main = "Normal PDF", type = "l")
abline(h = 0)



#svg(filename = if(onefile) "C:/Users/Megan Mitchell/OneDrive - University of Massachusetts/Environmental_Data/ECO_634_Data/norm_1.svg",
 #   width=7, height=7, pointsize = 12,
 #   onefile = FALSE, family = "sans", bg = "white",
 #   antialias = c("default", "none", "gray", "subpixel"),
 #   symbolfamily) 
x = seq(-50, 50, length.out = 1000)
y = dnorm(x, mean = 10.4, sd = 2.4)
plot(x, y, main = "Standard Normal PDF, mean = 10.4, sd = 2.4", type = "l", xlim = c(2, 18))
abline(h = 0)

#svg(filename = if(onefile) "C:/Users/Megan Mitchell/OneDrive - University of Massachusetts/Environmental_Data/ECO_634_Data/norm_1.svg",
 #   width=7, height=7, pointsize = 12,
  #  onefile = FALSE, family = "sans", bg = "white",
   # antialias = c("default", "none", "gray", "subpixel"),
#    symbolfamily) 
#svg("norm_1.svg")

x = seq(-50, 50, length.out = 1000)
y = dnorm(x, mean = 10.4, sd = 2.4)
plot(x, y, main = "Standard Normal PDF, mean = 10.4, sd = 2.4", type = "l", xlim = c(2, 18))
abline(h = 0)

help(svg)

dev.off()


help(svg)
help(png)


set.seed(123)
n = 17
slope = 0.7
intcp = 0.2

guess_x = 6
guess_y = 4
guess_slope = 0.72

x = runif(n = n, min = 1, max = 10)
y = rnorm(n = n, mean = slope * x + intcp)

plot(x, y, pch = 16)
curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T)
dev.off()
#9
##
set.seed(711)
n_pts = 25
x_min = 5
x_max = 25
x = runif(n = n_pts, min = x_min, max = x_max)

dat = data.frame(x = x, y_observed = rnorm(n_pts))

plot(y_observed ~ x, data = dat, pch = 5)


  
#png(file="C:/Users/Megan Mitchell/OneDrive - University of Massachusetts/Environmental_Data/ECO_634_Data/plot_w_fitted_line.png",
 #   width=1500, height=1600)  
  
  
  
n_pts = 100
x_min = 0
x_max = 100


x = runif(n = n_pts, min = x_min, max = x_max)

dat = data.frame(x = x, y_observed = rnorm(n_pts))


line_point_slope = function(x, midpoint, slope)

plot(y_observed ~ x, data = dat, pch = 5, main = "Megan's Second Scatterplot", xlab = "", ylab= "", col = "orange")
#curve(midpoint_slope(), guess_x, guess_y, guess_slope)

hist(dat$y_observed, main ="Megan's Histogram") 

hist(dat$x, main = "Megan's Histogram", xlab="", col="purple")
plot(y_observed ~ x, data = dat, pch = 18, main = "Megan's Scatterplot", xlab = "", ylab= "", col = "purple")
boxplot(dat$x, main = "Megan's Boxplot", col="pink")





## code from class activity
## 11

set.seed(299)
n = 50
x_min = 1
x_max = 3

x = runif(n = n, min = x_min, max = x_max)
dat = data.frame(x = x, y_observed = rnorm(n))

line_point_slope = function(x, x1, y1, slope)

{
  get_y_intercept = 
    function(x1, y1, slope) 
      return(-(x1 * slope) + y1)
  
  linear = 
    function(x, yint, slope) 
      return(yint + x * slope)
  
  return(linear(x, get_y_intercept(x1, y1, slope), slope))
}



plot(y_observed ~ x, data = dat, pch = 15, main = "Megan's Scatterplot w/ fitted linear", xlab = "", ylab= "", col = "maroon3")

guess_x = 1.1
guess_y = 0.1
guess_slope = 0.9
curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = TRUE)

dev.off()


help(png)

#plot(x, y, pch = 16)
#curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T)




#slope = 0.7
#intcp = 0.2

#guess_x = 6
#guess_y = 4
#guess_slope = 0.72

x = runif(n = n, min = 1, max = 10)
y = rnorm(n = n, mean = slope * x + intcp)
line_point_slope = function(x, x1, y1, slope)
{
  get_y_intercept = 
    function(x1, y1, slope) 
      return(-(x1 * slope) + y1)
  
  linear = 
    function(x, yint, slope) 
      return(yint + x * slope)
  
  return(linear(x, get_y_intercept(x1, y1, slope), slope))
}


plot(y_observed ~ x, data = dat, pch = 5, main = "Megan's third Scatterplot", xlab = "", ylab= "", col = "pink")
guess_x = 6
guess_y = 4
guess_slope = 0.72
curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = TRUE)

###13
help(here)
y_predicted = line_point_slope(dat$x, guess_x, guess_y, guess_slope)
#dat = cbind(dat, y_predicted)
dat$resids = (dat$y_observed - dat$y_predicted)
sum(dat$resids)
dat$abs_resids = abs(dat$resids)
dat$abs_resids

hist(dat$resids, main = "Histogram of residuals", xlab = "residuals", ylab = "", col="maroon3")
plot(dat$y_predicted, dat$resids, pch = 15, main = "Model of predicated values vs residuals", xlab= "predicated values", ylab= "observed values", col="maroon3")

dev.off()




help(plot)
help(cbind)

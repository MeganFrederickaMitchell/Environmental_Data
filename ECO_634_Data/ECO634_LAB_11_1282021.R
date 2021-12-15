rm(list = ls())
dev.off()

## Lab 11 Prelab

setwd("C:/Users/Megan Mitchell/OneDrive - University of Massachusetts/Environmental_Data/data")
bird = read.csv("bird.sub (1).csv")
hab = read.csv("hab.sub.csv")

birdhab = data.frame(merge(bird, hab, by = c("basin", "sub")))
dim(birdhab)

## Working with Brown Creeper abundance (column BRCR) and late successional forest (column ls) in birdhab

plot("BRCR", "ls", data = birdhab)
plot(birdhab$ls, birdhab$BRCR, col = "purple", lwd = 4, font = 2)
fit_1 = lm(birdhab$ls, birdhab$BRCR)


plot(birdhab$ls, birdhab$BRCR, col = "purple", lwd = 4, font = 2)
fit_1 = lm(formula = BRCR ~ ls, data = birdhab)
abline(fit_1)
summary(fit_1)

## take 2 ## THIS FUNCTION WORKS!

Linear_function = function(x, y1, slope)
{
  get_y_intercept = 
    function(x, y1, slope) 
      return(-(x * slope) + y1)
  
  linear = 
    function(x, y_int, slope) 
      return(y_int + x * slope)
  
  return(linear(x, get_y_intercept(x, y1, slope), slope))
}
linear(x = 1, y_int = 1, slope = 1)
linear(x = 3:5, y_int = 1, slope = 1)
linear(x = 3:5, y_int = -1, slope = 1)
linear(x = 3:5, y_int = -1, slope = 0.01)


Linear_Simulator = function(x, y1, slope, st_dev)
{
  get_y_intercept = 
    function(x, y1, slope) 
      return(-(x * slope) + y1)
  
  linear = 
    function(x, y_int, slope) 
      return(y_int + x * slope)
 
   st_dev =
    function(x,y1,slope)
      return(sd)
  return(linear(x, get_y_intercept(x, y1, slope), slope, st_dev))
}


linear_simulator = function(x, y_int, slope, st_dev)
{
  
y_int + (x * slope) + rnorm(length(x), sd = st_dev)

}

n= 200

par(mfrow = c(2, 2))
for (i in 1:4)
{
  x = runif(n = n)
  plot(
    x,
    linear_simulator(x, y_int = 1, slope = 4.5, st_dev = 0.1),
    main = "", xlab = "x", ylab = "y",
    pch = 16, col = rgb(0, 0.2, 0, 0.2))
}
dev.off()

n = 400

par(mfrow = c(2, 2))
for (i in 1:4)
{
  x = runif(n = n)
  plot(
    x, linear_simulator(x, y_int = 10, slope = -6.5, st_dev = 1.1),
    main = "", xlab = "x", ylab = "y",
    pch = 16, col = rgb(0, 0.2, 0, 0.2))
}

fit_1_coefs = coefficients(fit_1)
str(fit_1_coefs)

fit_1_summary = summary(fit_1)
str(fit_1_summary)
fit_1_summary$sigma

summary(fit_1)

int_obs = 0.0991039
slope_obs = 0.0058
sd_obs = 0.0507636

plot(
  x = birdhab$ls, 
  y = linear_simulator(
    x = birdhab$ls,
    y_int = int_obs,
    slope = slope_obs,
    st_dev = sd_obs
  ),
  main = "Simulated Data",
  xlab = "late-successional forest",
  ylab = "Brown Creeper Abundance")


plot(
  birdhab$ls, birdhab$BRCR, 
  xlab = "late-successional forest extent",
  ylab = "Brown Creeper abundance",
  pch = 19)

points(
  x = birdhab$ls, 
  y = linear_simulator(
    x = birdhab$ls,
    y_int = int_obs,
    slope = slope_obs,
    st_dev = sd_obs
  ),
  col = adjustcolor("red", alpha = 0.3),
  pch = 16)

legend(
  "topleft",
  legend = c("data", "simulation"),
  pch = 16,
  col = c(1, adjustcolor("red", alpha = 0.3)))




## IGNORE ALL OF THIS BAD CODE! I am trying to build a linear function with x1, y1 (intercept), and slope
line_point_slope = function(x, x1, y1, slope)
{
  get_y_intercept = 
    function(x1, y1, slope) 
      return(-(x1 * slope) + y1)
  
  linear = 
    function(x, y_int, slope) 
      return(y_int + x * slope)
  
  return(linear(x, get_y_intercept(x1, y1, slope), slope))
}
linear(x1 = 0, x = 1, y_int = 1, slope = 1)
linear(x1 = 0, x = 3:5, y_int = 1, slope = 1)


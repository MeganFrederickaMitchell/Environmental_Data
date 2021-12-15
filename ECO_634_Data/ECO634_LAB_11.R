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
  get_st_dev =
    function(x,y1,slope)
      return(sd)
  return(linear(x, get_y_intercept(x, y1, slope), slope, get_st_dev))
}


Linear_Simulator(x, y1 = 1, slope = 4.5, st_dev = 0.1)




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


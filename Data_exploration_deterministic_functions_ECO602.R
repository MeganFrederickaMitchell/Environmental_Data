## main="A Floral Scatter Plot", xlab="Sepal width", ylab="Sepal length"
## plot(x = iris$Sepal.Width, y = iris$Sepal.Length, main="A Floral Scatter Plot",
##     xlab="Sepal width", ylab="Sepal length")


install.packages("here")
setwd("C:/Users/Megan Mitchell/OneDrive - University of Massachusetts/Environmental_Data/data")
dat_habitat <- read.csv('hab.sta.csv')
dat_habitat

par(mfrow = c(6, 1))
hist(dat_habitat$elev, main="Elevation", xlab="Elevation" , ylab="# Sampling Sites") 
hist(dat_habitat$slope, main= "Slope", xlab="Slope", ylab= "# Sampling Sites")
hist(dat_habitat$aspect, main= "Aspect", xlab="Aspect", ylab="# Sampling Sites")

plot(x= dat_habitat$elev, y=dat_habitat$ba.tot, main= "Elevation vs Total Basal Area", xlab= "Elevation", ylab= "Total Basal Area")
points(x = data_center_x, y = data_center_y, col = "red")
curve(line_point_slope(x, x1 = 3.5, y1 = 1.25, slope = 0.08), add = TRUE) 

plot(x= dat_habitat$slope, y=dat_habitat$ba.tot, main= "Slope vs Total Basal Area", xlab= "Slope", ylab= "Total Basal Area")
points(x = data_center_x, y = data_center_y, col = "red")
curve(line_point_slope(x, x1 = 3.5, y1 = 1.25, slope = 0.4), add = TRUE) 

plot(x= dat_habitat$aspect, y=dat_habitat$ba.tot, main= "Aspect vs Total Basal Area", xlab= "Aspect", ylab= "Total Basal Area")
points(x = data_center_x, y = data_center_y, col = "red")
curve(line_point_slope(x, x1 = 3.5, y1 = 1.25, slope = 0.1), add = TRUE) 

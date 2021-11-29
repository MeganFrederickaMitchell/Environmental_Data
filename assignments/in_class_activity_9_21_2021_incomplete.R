dat_rope <- read.csv("rope.csv")
dat_rope
install.package(here)
require(here)
here()
here("data")
dat_rope
dat_catrate <- read.csv("catrate.csv")
dat_delomys <- read.csv("delomys.csv")
dat_delomys
dat_catrate
head(dat_rope)
head(dat_catrate)
head(dat_delomys)

hist(dat_delomys$body_mass, main= "Megan's histogram of delomys body mass")
dat_catrate
dat_delomys
boxplot(penguins$"body_mass_g", main= "Megan's boxplot of penguin body mass") 

plot(x= dat_delomys$body_mass, y=dat_delomys$body_length, main= "Megan's scatterplot of body mass vs body length", xlab= "body mass", ylab= "body length")
points(x = data_center_x, y = data_center_y, col = "red")


plot(x = dat_delomys$genus, xlab="Elevation" , ylab="Cedar Wax Wings")
curve(logistic_midpoint_slope(x, midpoint = 400, slope = 0.1), add = TRUE)

boxplot(bill_depth_mm ~ sex, data = penguins) 

read.csv(path_to_file("penguins.csv"))

install.packages('palmerpenguins')
install.packages("here")
require(palmerpenguins)
require(here)
class(palmerpenguins)
library("palmerpenguins")
palmerpenguins
hist(penguins$bill_depth_mm, main= "Megan's histogram of penguin bill depth", xlab= "bill depth mm")

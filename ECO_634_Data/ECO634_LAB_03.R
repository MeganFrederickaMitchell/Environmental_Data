dat_habitat <- read.csv("hab.sta.csv")
dat_bird <- read.csv('hab.sta.csv')



install.packages("here")
setwd("C:/Users/Megan Mitchell/OneDrive - University of Massachusetts/Environmental_Data/data")
dat_habitat <- read.csv('hab.sta.csv')
dat_habitat
dat_bird <- read.csv('bird.sta (1).csv')
dat_bird
head(dat_bird)
dat_all <- merge(dat_bird, dat_habitat)
dat_all
plot(ba.tot ~ elev, data = dat_all)
sample(dat_all$CEWA, 100)

dat_all$CEWA >=1
dat_all == 1
as.numeric (dat_all > 1)
(dat_all$CEWA >1)
cewa_present_absent <- as.numeric(dat_all$CEWA > 1)

cewa_present_absent[TRUE]

plot(x = dat_all$elev, y = cewa_present_absent)

get_logistic_param_a = function(slope, midpoint)
{
  b = slope / 4
  return (-midpoint * (slope / 4))
}

# Function to calculate the logistic parameter b given the slope
get_logistic_param_b = function(slope)
{
  return (slope / 4)
}


# Calculate the value of the logistic function at x, given the parameters a and b.
logistic = function(x, a, b)
{
  val = exp(a + b * x)
  return(val / (1 + val))
}

# Calculate the value of the logistic function at x, given a slope and midpoint.
logistic_midpoint_slope = function(x, midpoint, slope)
{
  b = get_logistic_param_b(slope)
  a = get_logistic_param_a(slope, midpoint)
  return(logistic(x, a, b))
}

#par(mfrow = c(1, 1))

# positive slope
plot(x = dat_all$elev, y = cewa_present_absent, main="Elevation", xlab="Elevation" , ylab="Cedar Wax Wings")
curve(logistic_midpoint_slope(x, midpoint = 400, slope = 0.1), add = TRUE)

#negative slope
plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = -0.1), add = TRUE)

#shallow negative slope
plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = -0.05), add = TRUE)

# code to change titles and axis -->  main="Elevation", xlab="Elevation" , ylab="# Sampling Sites"

install.packages("psych")
require(psych)
pairs.panels(iris)

pairs.panels()
pairs.panels(x= dat_all$elev, y = cewa_present_absent) 

pairs.panels(dat_all, cewa_present_absent, dat_habitat, dat_bird)

dat_terrain = dat_all[c("elev", "slope", "aspect", "ba.tot")]

pairs.panels(dat_terrain)

dat_bird
names(dat_bird)
plot(dat_bird)


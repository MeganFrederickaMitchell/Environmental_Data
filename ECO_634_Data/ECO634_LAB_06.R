# Prelab
setwd("C:/Users/Megan Mitchell/OneDrive - University of Massachusetts/Environmental_Data/ECO_634_Data")
install.packages(penguins)
require(palmerpenguins)
help(sd)
help("sample")
help(length)
 # Standard error of the mean
sse_mean = function (x, na.rm = TRUE) sd (x, na.rm = TRUE) / sqrt(length(x))

sse_mean(penguins$bill_depth_mm)
sse_mean(penguins$bill_length_mm)
 # Resampling methods
boxplot(flipper_length_mm ~ species, data = penguins)
dat_pen = subset(penguins, species != "Gentoo")
boxplot(flipper_length_mm ~ species, data = dat_pen)

dat_pen = droplevels(subset(penguins, species != "Gentoo"))
{
  #par(mfrow = c(1, 2))
  boxplot(flipper_length_mm ~ species, data = penguins)
  boxplot(flipper_length_mm ~ species, data = dat_pen)
}
dev.off()
set.seed(123)

flipper_shuffled = sample(penguins$flipper_length_mm, replace = TRUE)
par(mfrow = c(1, 2))
boxplot(flipper_length_mm ~ species, data = penguins)
boxplot(flipper_shuffled ~ penguins$species, xlab = "species")
 # t-test, a frequentist approach
t.test(dat_pen$flipper_length_mm ~ dat_pen$species)

 #  Two sample resampling

set.seed(1)
flipper_shuffled = sample(dat_pen$flipper_length_mm)
boxplot(flipper_shuffled ~ dat_pen$species)








help(rm)

help(list)
help(ls)
# Lab 06 

# Q1 (SSE)
rm(list = ls())
sse_mean = function (x, na.rm = TRUE) sd (x, na.rm = TRUE) / sqrt(length(x))
sse_mean(penguins$body_mass_g)
sse_mean(mtcars$mpg)


# Q2 and Q3
# two_group_resample = function (x, n_1, n_2)
#= sample(dat_pen$flipper_length_mm)

two_group_resample = function(x, n_1, n_2) 
{
  dat_pen = droplevels(subset(penguins, species != "Gentoo"))
  x = dat_pen$flipper_length_mm
  n_1 = 68
  n_2 = 152
  
  dat_1 = sample(x, n_1, replace = TRUE)
  dat_2 = sample(x, n_2, replace = TRUE)
  
  difference_in_means = mean(dat_1, na.rm = TRUE) - mean(dat_2, na.rm = TRUE)
  return(difference_in_means)
}
print(difference_in_means)

two_group_resample = function(x, n_1, n_2) 
{
  mean(
    sample(x, n_1, replace = TRUE), na.rm = TRUE) - mean(
      sample(x, n_2, replace = TRUE), na.rm = TRUE)
}

two_group_resample()
set.seed(123)
two_group_resample(penguins$flipper_length_mm, 50, 50)


#difference_in_means
#two_group_resample()

# Q4
help(abs)
two_group_resample = function(x, n_1, n_2) 
{
  set.seed(2000)
  dat_pen = droplevels(subset(penguins, species != "Gentoo"))
  x = dat_pen$flipper_length_mm
  n_1 = 68
  n_2 = 152
  
  dat_1 = sample(x, n_1, replace = TRUE)
  dat_2 = sample(x, n_2, replace = TRUE)
  
  difference_in_means = mean(dat_1, na.rm = TRUE) - mean(dat_2, na.rm = TRUE)
  difference_in_means
}

set.seed(2000)
two_group_resample(dat_pen$flipper_length_mm, 68, 152)
hist(difference_in_means)


n = 2000
difference_in_means = c()
for (i in 5.8:n)
{
difference_in_means = c(
    difference_in_means,
    two_group_resample(dat_pen$flipper_length_mm, 68, 152)
  )
}
hist(difference_in_means, lwd = 2.0, main = "Histogram of Resampled Difference of Means", xlab = "difference in means", col = "maroon")

t_test = t.test(dat_pen$flipper_length_mm ~ dat_pen$species)
t_test
difference_in_means
diff_observed = round(diff(t_test$estimate), digits = 3)
print(diff_observed, digits = 3)
sum(abs(difference_in_means) >= diff_observed)

sum(abs(difference_in_means) >= 5.80)

dev.off()

# Q7 7 -11

agg_means = aggregate(
  body_mass_g ~ species, 
  data = dat_pen, 
  FUN = "mean", 
  na.rm = TRUE)
diff_crit = diff(agg_means[, 2])

dat_pen$body_mass_g
agg_means
diff_crit
boxplot(dat_pen$body_mass_g)
boxplot(dat_pen$body_mass_g - diff_crit)

boxplot(diff_crit)

boxplot(body_mass_g ~ species, dat_pen, lwd = 2.0, main = "Boxplot of body mass (g) of two penguin species",
        xlab = "Penguin species", ylab="Body mass (grams)", col = "darkseagreen3")

dat_pen$body_mass_g

str(t.test(dat_pen$body_mass_g ~ dat_pen$species))
str(t_test)

sum(abs(diff_crit) >= 32.42598)

dev.off()

hist(penguins$body_mass_g, main = "1000 Trial Simulated Mean Difference\nPenguin Body Mass", xlab = "Mean Difference")
hist(mean_differences_mass, main = "1000 Trial Simulated Mean Difference\nPenguin Body Mass", xlab = "Mean Difference")
n = 1000
mean_differences_mass = c()
for (i in 1:n)
{
  mean_differences_mass = c(
    mean_differences_mass,
    two_group_resample(dat_pen$body_mass_g, 68, 152)
  )
}




mean_differences_mass = c()
for (i in 1:n)
{
  mean_differences_mass = c(
    mean_differences_mass,
    two_group_resample(dat_pen$body_mass_g, 68, 152)
  )
}

sum(abs(mean_differences_mass) >= diff_crit)
hist(mean_differences_mass, lwd = 2, main = "Simulation of Mean Difference in Penguin Body Mass with 1000 repetitions", xlab = "mean difference in penguin body mass", col = "palevioletred")

setwd("C:/Users/Megan Mitchell/OneDrive - University of Massachusetts/Environmental_Data/ECO_634_Data")
library(readr)
moths <- read_csv("moths.csv")
View(moths)
head(moths)

## ignore this
sample_sd(penguins$bill_length_mm)

sse_mean = function (x, na.rm = TRUE) sd (x, na.rm = TRUE) / sqrt(length(x))
sse_mean(penguins$bill_depth_mm)
sse_mean(penguins$bill_length_mm)

is.na(View(penguins))
as.null(View(penguins$bill_length_mm))
dim(penguins)
help(nrow)

length(na.omit(penguins$bill_length_mm))
length(penguins$bill_length_mm)

help(is.na)
## use n.row function instead of view function, large datasets might not work
ncol(penguins)
NCOL(penguins)
NROW(penguins)
is.null(nrow(penguins))
as.null(nrow(penguins))
is.na(nrow(penguins$bill_length_mm))
length(penguins$bill_length_mm)
head(penguins$bill_length_mm)
summary(penguins$bill_length_mm)
help(qt)


### PRELAB

m = 10000
result = numeric(m)
head(result)

for(i in 1:m)
{
  result[i] = mean(sample(moths$anst, replace=TRUE))
}
mean(result)
quantile(result,c(0.025,0.975))
install.packages("boot")
require(boot)

##boot(moths, mean, m)
boot_mean = function(x, i)
{
  return(mean(x[i], na.rm = TRUE))
}

myboot = 
  boot(
    data = dat_penguins$bill_length_mm,
   boot_mean,
    R = 10000)
print(myboot)
str(myboot)

mean(dat_penguins$bill_length_mm)
myboot$t0
mean(myboot$t) - myboot$t0
sd(myboot$t)
quantile(
  myboot$t,
  c(0.025, 0.975))
dat_pen = dat_penguins[,-1]
head(dat_pen)
n = nrow(pen_dat) #number of rows or sample observations
m = 100 #number of bootstrap iterations
pen_result = matrix(
  nrow = m,
  ncol = n)
## abs function for critical t value
n
m
# lab 07

## Q1
dat_penguins = subset(penguins, species == "Gentoo")
sample_size = sum(!is.na(dat_penguins$bill_length_mm))
print(sample_size)


install.packages(penguins)
require(palmerpenguins)
x = nrow(penguins$bill_length_mm)

## sample sd calculation
mean_bill = mean(dat_penguins$bill_length_mm, na.rm = TRUE)

sample_sd = function(x, na.rm = TRUE)
bill_sd = sd(dat_penguins$bill_length_mm, na.rm = TRUE)
bill_sd/sqrt(sample_size) ## USE THIS

## standard error calculation 
sse_mean = function (x, na.rm = TRUE) sd (x, na.rm = TRUE) / sqrt(length(x))
sse_mean(dat_penguins$bill_length_mm)
 ## directly calculate standard error from sample size and sample standard deviation
##sd/ sqrt sample size


## critical t value, Q3 --> should get two values
qt(c(0.025, 0.975), df= 122, lower.tail = TRUE, log.p = FALSE)


mean_bill + (qt(c(0.025, 0.975), df= 122, lower.tail = TRUE, log.p = FALSE)) * (sse_mean(dat_penguins$bill_length_mm))


## confidence interval

(sse_mean(dat_penguins$bill_length_mm)) * (qt(p= 0.05, df= 122, lower.tail = FALSE, log.p = FALSE))


qt(p= 0.05, df= 122, lower.tail = FALSE, log.p = FALSE)


(1.657439 * 0.276759)


## Q6

m = 10000


result = numeric(m)
head(result)



require(boot)

boot(data, statistic, R)

m = 10000
boot_mean = function(x, i)
{
  return(mean(x[i], na.rm = TRUE))
}
myboot = 
  boot(
    data = dat_penguins$bill_length_mm,
    boot_mean,
    R = m)
print(myboot)
str(myboot)

mean(dat_penguins$bill_length_mm, na.rm = TRUE)
myboot$t0
mean(myboot$t) - myboot$t0
sd(myboot$t)

quantile(
  myboot$t,
  c(0.025, 0.975))

hist(dat_penguins$bill_length_mm)



## Q9-13 draft

moth_dat = moths[,-1]
head(moth_dat)

n = nrow(moth_dat) 
m = 1000 
moth_result = matrix(
  nrow = m,
  ncol = n)

rarefaction_sampler = function(input_dat, n_iterations)
{
  n = nrow(moth_dat) 
  m = 1000
  
  moth_result = matrix(
    nrow = m,
    ncol = n)
  
  # The outer loop: runs once for each bootstrap iteration.  index variable is i
  for(i in 1:m)
  {
    for(j in 1:n)
    {
      
      rows_j = sample(n, size = j, replace=TRUE)
      
      t1 = moth_dat[rows_j, ]
      
      t2 = apply(t1, 2, sum)
      
      moth_result[i, j] = sum(t2 > 0)
    }
  }
  
  return(moth_result)
}

rarefact = rarefaction_sampler(moth_dat, 1000)
head(rarefact)

## Q9

rm(list = ls())
library(readr)
moths <- read_csv("moths.csv")
moth_dat = moths[,-1]

#rarefact= rarefaction_sampler(moth_dat, 10000)

rarefaction_sampler = function(input_dat, n_iterations)

{
  n = nrow(moth_dat) 
  
  n_input_rows = nrow(input_dat)
  
  results_out = matrix(
    nrow = n_iterations,
    ncol = n_input_rows)
  
  for(i in 1:n_iterations)
  {
    
    for(j in 1:n)
    {
      rows_j = sample(n, size = j, replace=TRUE)
      
      t1 = input_dat[rows_j, ]
      
      t2 = apply(t1, 2, sum)
      
      results_out[i, j] = sum(t2 > 0)
    }
  }
  return(results_out)
}

rarefact= rarefaction_sampler(moth_dat, 10000)

nrow(rarefact)
head(rarefact)


rare_mean = apply(rarefact, 2, mean)
rare_quant = apply(rarefact, 2, quantile, probs=c(0.025, 0.975))
rare = t(rbind(rare_mean, rare_quant))
matplot(
  rare,
  type='l', lwd = 4, col=c(5,6,7),
  xlab='Number of sampling sites',
  ylab='Moth Species richness',
  main='Rarefaction Curve of Rare Moth Abundance')
 
legend(
  'bottomright',
  legend=c('mean abundance','2.5% Confidence Interval','97.5% Confidence Interval'), lwd = 4,
  lty=c(5,6,7),col=c(5,6,7), inset=c(.1,.1))

dev.off()


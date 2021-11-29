setwd("C:/Users/Megan Mitchell/OneDrive - University of Massachusetts/Environmental_Data/data")
require(palmerpenguins)
penguin_dat = droplevels(subset(penguins, species != "Gentoo"))
penguin_dat
#install.packages("boot")
#require(boot)
#help(two.boot)
require(simpleboot)
install.packages("simpleboot")
veg = read.csv("vegdata.csv")
dat_hab <- read.csv("hab.sub.csv")
dat_bird <- read.csv("bird.sub (1).csv")
dat_hab
dat_sub
veg
#PRELAB STUFF#
dat_tree = droplevels(subset(veg, treatment %in% c("control", "clipped")))
tree_boot = 
  two.boot(
    subset(dat_tree, treatment == "clipped")$pine,
    subset(dat_tree, treatment == "control")$pine,
    FUN = mean,
    R = 10000,
    na.rm = TRUE
  )

tree_boot
quantile(tree_boot$t, 0.025)
wilcox.test(pine ~ treatment, data = dat_tree)

mean(subset(dat_tree, treatment == "clipped")$pine) - mean(subset(dat_tree, treatment == "control")$pine)

tree_boot
dat_tree
sum(tree_boot$t >= 0)
sum(tree_boot$t < 0)

boot.ci(tree_boot)

help(boot.ci)
hist(tree_boot$t, main = "Bootstrap sampling distribution")
quantile(tree_boot$t, c(0.025, 0.975))

dat_all = merge(
  dat_bird, 
  dat_hab,
  by = c("basin", "sub"))

head(dat_all[, c("b.sidi", "s.sidi")])
head(dat_all)
b_sidi_mean = mean(dat_all$b.sidi, na.rm = TRUE)
b_sidi_sd   = sd(dat_all$b.sidi, na.rm = TRUE)

s_sidi_mean = mean(dat_all$s.sidi, na.rm = TRUE)
s_sidi_sd = sd(dat_all$s.sidi, na.rm = TRUE)

# Use the subset-by-name symbol ($) to create a 
# new column of z-standardized values.

dat_all$b.sidi.standardized = (dat_all$b.sidi - b_sidi_mean)/b_sidi_sd
dat_all$s.sidi.standardized = (dat_all$s.sidi - s_sidi_mean)/s_sidi_sd

mean(dat_all$b.sidi.standardized)
sd(dat_all$b.sidi.standardized)

mean(dat_all$s.sidi.standardized)
sd(dat_all$s.sidi.standardized)

fit_1 = lm(b.sidi ~ s.sidi, data = dat_all)
coef(fit_1)
slope_observed = coef(fit_1)[2]


plot(
  b.sidi ~ s.sidi, data = dat_all,
  main = "Simpson's diversity indices",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")
abline(fit_1)

dat_1 = 
  subset(
    dat_all,
    select = c(b.sidi, s.sidi))

index_1 = sample(nrow(dat_1), replace = TRUE)
index_2 = sample(nrow(dat_1), replace = TRUE)

dat_resampled_i = 
  data.frame(
    b.sidi = dat_1$b.sidi[index_1],
    s.sidi = dat_1$s.sidi[index_2]
  )

fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i)
slope_resampled_i = coef(fit_resampled_i)[2]

print(slope_resampled_i)

plot(
  b.sidi ~ s.sidi, data = dat_resampled_i,
  main = "Simpson's diversity indices",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")
abline(fit_resampled_i)

# 14 in lab

dat_all = merge(
  dat_bird, 
  dat_hab,
  by = c("basin", "sub"))

head(dat_all[, c("b.sidi", "s.sidi")])
head(dat_all)
b_sidi_mean = mean(dat_all$b.sidi, na.rm = TRUE)
b_sidi_sd   = sd(dat_all$b.sidi, na.rm = TRUE)

s_sidi_mean = mean(dat_all$s.sidi, na.rm = TRUE)
s_sidi_sd = sd(dat_all$s.sidi, na.rm = TRUE)

# Use the subset-by-name symbol ($) to create a 
# new column of z-standardized values.

dat_all$b.sidi.standardized = (dat_all$b.sidi - b_sidi_mean)/b_sidi_sd
dat_all$s.sidi.standardized = (dat_all$s.sidi - s_sidi_mean)/s_sidi_sd

mean(dat_all$b.sidi.standardized)
sd(dat_all$b.sidi.standardized)

mean(dat_all$s.sidi.standardized)
sd(dat_all$s.sidi.standardized)

fit_1 = lm(b.sidi ~ s.sidi, data = dat_all)
coef(fit_1)
slope_observed = coef(fit_1)[2]


set.seed(500)
fit_1 = lm(b.sidi ~ s.sidi, data = dat_all)
coef(fit_1)
slope_observed = coef(fit_1)[2]

dat_1 = 
  subset(
    dat_all,
    select = c(b.sidi, s.sidi))

m = 10000 
result = numeric(m) 

for(i in 1:m)
{
  index_1 = sample(nrow(dat_1), replace = TRUE)
  index_2 = sample(nrow(dat_1), replace = TRUE)
  
  dat_resampled_i = 
    data.frame(
      b.sidi = dat_1$b.sidi[index_1], s.sidi = dat_1$s.sidi[index_2])

fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i)
result[i] = coef(fit_resampled_i)[2]

}
head(result)
tail(result)

result[i]
hist(result) 
dev.off()
# ... your loop code ...  
  
  #result[i] = coef(fit_resampled_i)[2] }
  
  # lab 8 Q1-4
  
penguin_dat = droplevels(subset(penguins, species != "Gentoo"))
tail(penguin_dat)
penguin_dat = droplevels(subset(penguins, species != "Gentoo"))

#dat_pen = droplevels(subset(penguin_dat, flipper_length_mm %in% c("Adelie", "Chinstrap")))
#dat_pen
#dat_tree = droplevels(subset(veg, treatment %in% c("control", "clipped")))

##start after here

pen_boot = 
  two.boot(
    subset(penguin_dat, species == "Adelie")$flipper_length_mm,
    subset(penguin_dat, species == "Chinstrap")$flipper_length_mm,
    FUN = mean,
    R = 10000,
    na.rm = TRUE
  )
quantile(pen_boot$t, c(0.025, 0.975))
mean(pen_boot$t)
sd(pen_boot$t)
sum(pen_boot$t >= 0)
sum(pen_boot$t < 0)

boot.ci(pen_boot)
t.test(flipper_length_mm ~ species, data = penguin_dat, alternative = "less")
hist(pen_boot$t, lwd = 2, main = "Histogram of 10000 bootstrap differences in mean penguin flipper length",
     xlab = "Difference in mean flipper length (mm) Adelie and Chinstrap Penguins", col = "mistyrose3")

mean(penguin_dat$flipper_length_mm, na.rm = TRUE)
sd(penguin_dat$flipper_length_mm, na.rm = TRUE)
median(penguin_dat$flipper_length_mm, na.rm = TRUE)

quantile(pen_boot$t, 0.025)
quantile(tree_boot$t, 0.025)

dev.off()
# lab 8 QA 5-8
pen_ecdf = ecdf(pen_boot$t)
pen_ecdf(-4.5)
1- pen_ecdf(-4.5)
1 - pen_ecdf(0)
pen_ecdf(0)
pen_ecdf(-8)

# lab 8 Q 9
## someone elses code because mine sucks
set.seed(49549)
fit_1 = lm(b.sidi ~ s.sidi, data = dat_all)
slope_observed = coef(fit_1)[2]

dat_1 =
  subset(
    dat_all,
    select = c(b.sidi, s.sidi))

m = 10000
result = numeric(m)

for(i in 1:m)
{
  index_1 = sample(nrow(dat_1), replace = TRUE)
  index_2 = sample(nrow(dat_1), replace = TRUE)
  
  dat_resampled_i =
    data.frame(
      b.sidi = dat_1$b.sidi[index_1],
      s.sidi = dat_1$s.sidi[index_2]
    )
  
  fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i)
  result[i] = coef(fit_resampled_i)[2]
}
head(result)
hist(result)
dev.off()

## take 3
dat_all = merge(
  dat_bird, 
  dat_hab,
  by = c("basin", "sub"))
head(dat_all[, c("b.sidi", "s.sidi")])

b_sidi_mean = mean(dat_all$b.sidi, na.rm = TRUE)
b_sidi_sd   = sd(dat_all$b.sidi, na.rm = TRUE)

dat_all$b.sidi.standardized = (dat_all$b.sidi - b_sidi_mean)/b_sidi_sd

# Z-standardization code: 
s_sidi_mean = mean(dat_all$s.sidi, na.rm = TRUE)
s_sidi_sd = sd(dat_all$s.sidi, na.rm = TRUE)

s_sidi_mean
dat_all$s.sidi.stardardized = (dat_all$s.sidi - s_sidi_mean) / s_sidi_sd

fit_1 = lm(b.sidi ~ s.sidi, data = dat_all)
coef(fit_1)

slope_observed = coef(fit_1)[2]

dat_1 = 
  subset(
    dat_all,
    select = c(b.sidi, s.sidi))

m = 10000
result = numeric(m) 

for(i in 1:m)
{
  index_1 = sample(nrow(dat_1), replace = TRUE)
  index_2 = sample(nrow(dat_1), replace = TRUE)
  
  dat_resampled_i = 
    data.frame(
      b.sidi = dat_1$b.sidi[index_1],
      s.sidi = dat_1$s.sidi[index_2]
    )
  
  fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i)
  result[i] = coef(fit_resampled_i)[2]
} 
head(result)
hist(result, font = 2, lwd = 2, col = "wheat3", main = "Monte Carlo Resampling: Simpsons Diversity Indices for Vegetation and Birds", xlab = "slope parameter")
abline(v = slope_observed, col = "blue", lwd = 4)
abline(v = quantile(result, 0.05), lty = 2, col = "red", lwd = 4)
dev.off()

quantile(result, c(0.05, 0.975))
slope_observed
plot(result)
sd(pen_boot$t)

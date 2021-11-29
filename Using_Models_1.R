setwd("C:/Users/Megan Mitchell/OneDrive - University of Massachusetts/Environmental_Data/ECO_634_Data")
setwd("C:/Users/Megan Mitchell/OneDrive - University of Massachusetts/Environmental_Data/data")
catrate = read.csv(here("catrate.csv"))
catrate = read.csv("catrate.csv")
install.packages("here")
here(catrate)
head(catrate)
summary(catrate)
# Q1-4
hist(catrate$cat.rate, lwd = 2, font = 2, col= "rosybrown3",main = "Salamander reproduction catastrophic rates", xlab = "reproductive catastrohpic rate")
help(shapiro.test)
shapiro.test(catrate$cat.rate)
# Q5-15
help(t.test)
t.test(x = catrate$cat.rate, mu= 0.2857143, alternative = "less")
t.test(x = catrate$cat.rate, mu= 2/7)
t.test(x = catrate$cat.rate, mu= 2/7, alternative = "greater")
sum(0.69, 0.3)
help(sum)

wilcox.test(x= catrate$cat.rat, mu = 2/7, alternative = "greater")
wilcox.test(x= catrate$cat.rat, mu = 2/7, alternative = "less")
wilcox.test(x= catrate$cat.rat, mu = 0.2857143)
wilcox.test(x= catrate$cat.rat, mu = 2/7)
help("wilcox.test")

# Q16

require(palmerpenguins)
install.packages(palmerpenguins)
penguin_dat = droplevels(subset(penguins, species != "Gentoo"))
penguin_dat
summary(penguin_dat)
boxplot(
  flipper_length_mm ~ species, 
  data = penguin_dat,
  ylab = "Flipper Length (mm)")

dat_adelie = subset(penguin_dat, species == "Adelie")
dat_chinstrap = subset(penguin_dat, species == "Chinstrap")
dat_adelie
dat_chinstrap
shapiro.test(dat_adelie$flipper_length_mm)
shapiro.test(dat_chinstrap$flipper_length_mm)
dev.off()

png(
  filename = "C:/Users/Megan Mitchell/OneDrive - University of Massachusetts/Environmental_Data/data",
  width = 1500, height = 800,
  res = 180, units = "px")
dev.off()

require(here)
png(filename= here("lab_8_penguin_histogram"),  width=1500, height=800, res = 180, units = "px" )

#png(filename ="C:/Users/Megan Mitchell/OneDrive - University of Massachusetts/Environmental_Data/ECO_634_Data/Lab_8_Histogram_penguin_species.png",
#    width=1500, height=1600, res = 180, units = "px")

par(mfrow = c(1,2))
hist(dat_chinstrap$flipper_length_mm, lwd = 2, font = 2, col = "coral1", main = "Flipper Length of Chinstrap Penguins", xlab = "flipper length (mm)")
hist(dat_adelie$flipper_length_mm, lwd = 2, font = 2, col = "cadetblue1", main = "Flipper Length of Adelie Penguins", xlab = "flipper length (mm)")

t.test(dat_adelie$flipper_length_mm, dat_chinstrap$flipper_length_mm, mu = 0)
t.test(flipper_length_mm ~ species, data = penguin_dat)

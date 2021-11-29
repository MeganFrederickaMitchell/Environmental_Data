setwd("C:/Users/Megan Mitchell/OneDrive - University of Massachusetts/Environmental_Data")
dat_all = read.csv(bird_sta.csv)
dat_all
wiwa_counts = c(2, 6)
dpois(x = wiwa_counts, lambda = 5)
hist(wiwa_counts, lambda = 5)

sum(log(dpois(x = wiwa_counts, lambda = 4.5)))
hist(dat_all$WIWA, breaks = 0:7 - .5)
dat_bird = read.csv("bird.sta(1).csv")

bird_sta_1_
bird_sta_1_$wiwa_counts


log(dpois(c(3,7), lambda = 5))
log(dpois(2, lambda = 1.5))

dat_bird = read.csv("data", "bird.sta.csv")


dat_habitat = read.csv(here::here("data", "hab.sta.csv"))
dat_all = merge(dat_bird, dat_habitat)

bird = read.csv("bird.sta (1).csv")
habs = read.csv("hab.sta.csv")
dat_all = merge(bird, habs)
dat_all
wiwa_counts = c(2, 6)
dpois(x = wiwa_counts, lambda = 5)
hist(dat_all$WIWA, breaks =7)
dev.off()
wiwa_counts = c(2, 6)
dpois(x = wiwa_counts, lambda = 2)

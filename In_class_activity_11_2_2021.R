require(palmerpenguins)
install.packages(penguins)
dat_ade = droplevels(subset(penguins, species == "Adelie"))
hist(dat_ade$body_mass_g, main = "Adelie Penguins: Body Mass", xlab = "body mass (g)")
boxplot(body_mass_g ~ sex, dat_ade)
help(t.test)
dat_ade$sex
dat_mal = droplevels(subset(dat_ade, sex == "male"))
dat_fem = droplevels(subset(dat_ade, sex == "female"))
t.test(dat_fem$body_mass_g, mu = 4000, alternative = "greater")
t.test(dat_mal$body_mass_g, mu = 4000, alternative = "greater")

t.test(dat_fem$body_mass_g, dat_mal$body_mass_g, alternative ="less")
t.test(dat_fem$body_mass_g, dat_mal$body_mass_g, alternative ="greater")

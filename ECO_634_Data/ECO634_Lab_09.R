setwd("C:/Users/Megan Mitchell/OneDrive - University of Massachusetts/Environmental_Data/data")
install.packages("here")
require(here)
birds = read.csv(here("data", "bird.sta (1).csv"))
hab = read.csv(here("data", "birdhab (1).csv"))
birdshab = merge(birds, hab, by=c("basin", "sub", "sta"))
head(birdshab)
tail(birdshab)

table(birdshab$s.edge, birdshab$BRCR.x > 0)
br_creeper_table = table(birdshab$s.edge, birdshab$BRCR.x > 0)[, 2:1]
br_creeper_table
chisq.test(br_creeper_table)

## Q3-5

require(palmerpenguins)
fit_fl_sp = 
  lm(
    formula = flipper_length_mm ~ species,
    data = penguins)

fit_species = 
  lm(
    formula = body_mass_g ~ species,
    data = penguins)
fit_species

fit_both = lm(formula=body_mass_g ~ species * sex,
              data = penguins)
fit_both
fit_sex = 
  lm( 
    formula = body_mass_g ~ sex,
    data = penguins)
fit_sex

fit_both =
  lm(
    formula = body_mass_g ~ sex,
    formula = body_mass_g ~ species,
    data = penguins)

fit_both = list(fit_species, fit_sex)

class(fit_both[[1]])
fit_both
help(lm)

## Q6-8

boxplot(formula = body_mass_g ~ species,
    data = penguins,
    main = "Plot of penguin body mass (g) by species", xlab="",
    ylab = "Penguin Body mass (grams)", col = "mediumturquoise",
    names = c("Adelie Penguins", "Chinstrap Penguins", "Gentoo Penguins"))


boxplot(formula = body_mass_g ~ sex, 
             data = penguins, lwd = 2, col = "deeppink3", font = 3,
        main = " Plot of penguin body mass (g) by sex",
        xlab = "", ylab = "Penguin body mass (grams)")
  
boxplot(formula = body_mass_g ~ sex:species,
        data = penguins, lwd = 2, col = "plum3", font = 3,
        main = "Plot of penguin body mass (g) by sex and species", las = 2, 
        ylab= "Penguin body mass (grams)", xlab = " ",
        names = c("Female\nAdelie", "Male\nAdelie", "Female\nChinstrap", "Male\nChinstrap", "Female\nGentoo", "Male\nGentoo"))
        
dev.off()
        #names = c("penguin", "penguin", "penguins"))

help(las)
        # ylab = "penguin body mass (grams)")
 
#formula = body_mass_g ~ sex, formula = body_mass_g ~ species,
          # data = penguins)

## Q10-Q12


bartlett.test(body_mass_g ~ species, data = penguins)
bartlett.test(body_mass_g ~ sex, data = penguins)

## Q13-14


dat_groups = aggregate(
  body_mass_g ~ species * sex,
  data = penguins,
  FUN = c)
str(dat_groups)

bartlett.test(dat_groups$body_mass_g)
help("bartlett.test")


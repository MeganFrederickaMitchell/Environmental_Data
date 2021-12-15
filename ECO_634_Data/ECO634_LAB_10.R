## Lab 10 code template

## start here

rm(list = ls())
require(here)

rope = read.csv (here("rope.csv"))
rope$rope.type = factor(rope$rope.type)
levels(rope$rope.type)                       

## TSS  
  
    n_obs = nrow(rope)
    n_groups = length(levels(rope$rope.type))
    ss_tot = sum((rope$p.cut - mean(rope$p.cut))^2)
    df_tot = n_obs - 1
    ss_tot 

## Within group SS
    
    agg_resids = aggregate(
      x = rope$p.cut,
      by = list(rope$rope.type),
      FUN = function(x) (x - mean(x)))
    str(agg_resids)
    
    agg_sq_resids = aggregate(
      x = rope$p.cut,
      by = list(rope$rope.type),
      FUN = function(x) sum((x - mean(x))^2))
    str(agg_sq_resids)
    
    ss_within = sum(agg_sq_resids$x)
   
    df_within = (n_obs - n_groups)
 
  ## Partioning Variance: Among Groups
    
    ss_among = (ss_tot - ss_within)
    df_among = n_groups - 1
    
## Normalizing: Mean Squares                      
  
    ms_within = ss_within / (n_obs - n_groups)
    ms_among  = ss_among / (n_groups - 1)

## The Test Statistic: F
    
    f_ratio = ms_among/ ms_within
    f_pval = 1 - pf(f_ratio, df_among, df_within, lower.tail = TRUE, log.p = FALSE)   

  
## ANOVA in R
    fit_1 = lm(p.cut ~ rope.type, data=rope)
    anova(fit_1)
    
anova_fit_1 = anova(fit_1)
str(anova_fit_1)
str(anova_fit_1$"Sum Sq")
str(anova_fit_1$`Mean Sq`)

## Q1 self test

# number comparison tolerance
digits_check = 5

# Build the reference model using R functions
fit_1 = lm(p.cut ~ rope.type, data=rope)
anova(fit_1)
anova_fit_1 = anova(fit_1)

# Check degrees of freedom
anova_fit_1$Df == c(df_among, df_within)

# Check sums of squares
round(anova_fit_1$`Sum Sq`, digits = digits_check) == round(c(ss_among, ss_within), digits = digits_check)

# Check mean squares
round(anova_fit_1$`Mean Sq`, digits = digits_check) == round(c(ms_among, ms_within), digits = digits_check)

# Check the F-ratio
round(anova_fit_1$`F value`[1], digits = digits_check) == round(f_ratio, digits = digits_check)

# Check the F test statistic p-value
round(anova_fit_1$`Pr(>F)`[1], digits = digits_check) == round(f_pval, digits = digits_check)


## Q3

bartlett.test(p.cut ~ rope.type, data = rope)

## Q7

0.36714  + (-0.10164)

# Summary
# 1. clean EPA data for models
# 2. try different models

# load libraries
library(tidyverse)
library(performance) # install.packages("performance")
library(lme4) # install.packages("lme4")

# load EPA data -----------------------------------------------------------
epa <- read_csv("data/EPA.csv")

# clean data --------------------------------------------------------------
## transfer wide to long data
epa_per_rating = epa %>%
  # select variables we need
  select(Institution,
         idpatientencounter,
         EPA,
         Faculty,
         Trainee,
         idpgy,
         # 3 rating values below
         Preop,
         Intraop,
         Postop) %>%  
  # transfer wide to long data; one row per rating
  pivot_longer(
    cols = c("Preop", "Intraop", "Postop"),
    names_to = "phase",
    values_to = "ratings"
  )

glimpse(epa_per_rating) 


# model -------------------------------------------------------------------
## 1. encounter ID as the only random effect --------------
model <- lmer(
  formula = ratings ~ (1 | idpatientencounter),
  data = epa_per_rating
)

# inspect the model results
summary(model)
# Number of obs (remove missing rating cases): 1100
# idpatientencounter, 412

# Get icc
icc(model, by_group = TRUE)


## 2. add trainee ID as a random effect---------------
model2 <- lmer(
  formula = ratings ~ (1 | idpatientencounter) + (1 | Trainee),
  data = epa_per_rating
)

summary(model2)
# Number of obs: 1100
# n idpatientencounter, 412; n Trainee, 192

# Get icc
icc(model2, by_group = TRUE)


## 3. add faculty ID as a random effect---------------
model3 <- lmer(
  formula = ratings ~ (1 | idpatientencounter) + (1 | Trainee) + (1 | Faculty),
  data = epa_per_rating
)

summary(model3)

# Get icc
icc(model3, by_group = TRUE)

## 4. add PGY as a fixed effect---------------
model4 <- lmer(
  formula = ratings ~ idpgy + (1 | idpatientencounter) + (1 | Trainee) + (1 | Faculty),
  data = epa_per_rating
)

summary(model4)

# Get icc
icc(model4, by_group = TRUE)


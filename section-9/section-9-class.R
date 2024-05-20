library(tidyverse)
install.packages("dotwhisker")
library(dotwhisker)
install.packages("fastDummies")
library(fastDummies)
house<- read_csv("house2020_elections.csv")

# lm() automatically creates dummy variables from factors, 
# dropping the first factor level
mod <- lm(voteshare~incumbent_challenge_full, 
          data = house %>%
            mutate(incumbent_challenge_full = 
                     factor(incumbent_challenge_full, 
                            levels = c("Incumbent", 
                                       "Challenger", 
                                       "Open seat"))))
# we can also create dummies manually using the mutate function
house %>%
  mutate(incumbent = ifelse(incumbent_challenge_full==
                              "Incumbent", 1, 0), 
         challenger = ifelse(incumbent_challenge_full==
                               "Challenger", 1, 0))

# we can create dummies using the fastDummies package
house <- house %>%
  dummy_columns(select_columns = 
                  c("party", 
                    "incumbent_challenge_full"))
mod <- lm(voteshare~log(disbursements) + 
     incumbent_challenge_full_Incumbent + 
     `incumbent_challenge_full_Open seat`, 
   data = house)
summary(mod)

# let's run a series of models of vote on campaign spending
# we can progressively add more predictor variables
mod1 <- lm(voteshare~log(disbursements), 
           data = house)
mod2 <- lm(voteshare~log(disbursements) + 
             incumbent_challenge_full_Incumbent + 
             `incumbent_challenge_full_Open seat`, 
           data = house)
mod3 <- lm(voteshare~log(disbursements) + 
             incumbent_challenge_full_Incumbent + 
             `incumbent_challenge_full_Open seat`+
             party_REP, 
           data = house)

# using the dotwhisker package we can make a nice coefficient plot 
# from our models. 
dwplot(list(mod1, mod2, mod3), #model inputs
       ci = 0.95, #95% confidence intervals around coefficients
       vline = geom_vline( #creates a vertical line at x= 0
         xintercept = 0, 
         color = "red", #colors line red
         linetype = "dashed" #makes the line type dashed
       ))  %>%
  #this next function allows us to give our predictor variables 
  #nice names (which they might not have coming out of regression)
  relabel_predictors(c(
           `log(disbursements)` = "log(Spending)",
           incumbent_challenge_full_Incumbent = "Incumbent",
           `incumbent_challenge_full_Open seat` = "Open Seat",
           party_REP = "Republican"
         ))



# load tidyverse into RStudio for use
library(tidyverse)
# read house data into R from folder and create object "house"
house <- read_csv("house2020_elections.csv")

#calculate the average voteshare of candidates in our dataset
mean(house$voteshare)
#calculate the median amount spent by candidates in our dataset
median(house$disbursements)

#calculate the standard deviation, range, and variance of campaign spending
sd(house$disbursements)
range(house$disbursements)
var(house$disbursements)

#calculate overall summary statistics
summary(house$voteshare)

#turn "incumbent_challenge_full" variable into a factor variable
# re-order the levels in the order of incumbent, challenger, open seat
house$incumbent_challenge_full <-
  factor(house$incumbent_challenge_full, 
       levels = c("Incumbent", "Challenger", 
                  "Open seat"))
# create a proportion table of the incumbent_challenge_full factor variable
prop.table(table(house$incumbent_challenge_full)) %>%
  round(digits = 2)#round the proportion table to 2 digits

#create an object that contains only the Republican candidates 
#from the house dataset
rep <- house %>%
  filter(party=="REP") 

#use mutate to turn incumbent challenge full into a factor
#use mutate to create a new variable that is TRUE if a candidate won 
#and FALSE if not
house <- house %>%
  mutate(incumbent_challenge_full = 
           factor(incumbent_challenge_full), 
         winner = voteshare >50)



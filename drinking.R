#################
#### PART 1 #####
#################


library("dplyr")

drinking <- read.csv(file = "data/any_drinking.csv", stringsAsFactors = FALSE)
drinking_2012 <- select(drinking, state, location, both_sexes_2012, females_2012, males_2012)
drinking_2012_difference <- mutate(drinking_2012, difference = males_2012 - females_2012)

more_females <- filter(drinking_2012_difference, difference < 0)
print(paste("Females drank more than males in", nrow(more_females), "locations"))

closest <- which(drinking_2012_difference$difference == min(drinking_2012_difference$difference))
print(paste("Drinking is most similar between genders in", drinking_2012_difference$location[closest], drinking_2012_difference$state[closest]))

states_only <- filter(drinking_2012_difference, state != "National")  %>% 
  group_by(state) %>%
   summarize(both_sexes_2012 = sum(both_sexes_2012),
             females_2012 = sum(females_2012),
             males_2012 = sum(males_2012),
             difference = sum(difference))

highest_drinking_state <- filter(states_only, both_sexes_2012 == max(states_only$both_sexes_2012)) %>%
                          select(state, both_sexes_2012)
print(highest_drinking_state)


lowest_drinking_state <- filter(states_only, both_sexes_2012 == min(states_only$both_sexes_2012)) %>%
                          select(state, both_sexes_2012)
print(lowest_drinking_state)


print(paste("Range in drinking rate was ", highest_drinking_state$both_sexes_2012 - lowest_drinking_state$both_sexes_2012))



#################
#### PART 2 #####
#################


binge_drinking <- read.csv(file = "data/binge_drinking.csv", stringsAsFactors = FALSE)
counties <- filter(binge_drinking, state != "National", state != location)



counties <- mutate(counties,
                   decade_change = both_sexes_2012 - both_sexes_2002,
                   female_decade_change = females_2012 - females_2002,
                   male_decade_change = males_2012 - males_2002)

#Section 2

both_mean_2012 <- summarize(counties, mean = mean(both_sexes_2012))
print(both_mean_2012)  

state_binge_rates <- group_by(counties, state) %>%
summarize( min = min(both_sexes_2012),
           max = max(both_sexes_2012)
)

write.csv(state_binge_rates, file = "data/state_binge_drinking.csv", row.names = FALSE)

male_increase <- nrow(filter(counties, male_decade_change > 0 )) / nrow(counties)
print(paste("Male binging increased in", round(100 * male_increase), "% of counties."))

female_increase <- nrow(filter(counties, female_decade_change > 0)) / nrow(counties)
print(paste("Female binging increased in", round(100 * female_increase), "% of counties."))

higher_female_lower_male <- nrow(filter(counties, female_decade_change > 0,male_decade_change < 0)) / nrow(counties) 
print(paste("Female binging increased and male binging decreased in", round(100 * higher_female_lower_male), "% of counties"))
  
  
largest_median_male_increase <- group_by(counties, state) %>%
summarize(male_median = median(male_increase)) %>%
filter(male_median == max(male_median))

print(largest_median_male_increase)
  


#################
#### PART 3 #####
#################

any_columns <-colnames(drinking)
binge_columns <- colnames(binge_drinking)
new_any_columns <- any_columns != "state" & any_columns != "location"
new_binge_columns <- binge_columns != "state" & binge_columns != "location"

colnames(drinking) <- c(any_columns[!new_any_columns], paste("any_",any_columns[new_any_columns]))
colnames(binge_drinking) <- c(binge_columns[!new_binge_columns], paste("binge_",binge_columns[new_binge_columns]))

combined_drinking <- left_join(drinking, binge_drinking)

average_drinking <- filter(combined_drinking, state != "National",state == location) %>%
                    summarize(average = mean(any_both_sexes_2012 - binge_both_sexes_2012)) 
print(average_drinking)                  







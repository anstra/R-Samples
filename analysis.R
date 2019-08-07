#install.packages("httr")
#install.packages("jsonlite")
library(httr)
library(jsonlite)
library(dplyr)
#install.packages("tidyr")
library(tidyr)

#Part 1

source('api_keys.R')
base_uri <- "https://api.propublica.org/congress/v1/" 


#Function takes in a topic for a bill and returns a data frame that has 
#the title, last action, and url


get_bills_on_topic <- function(topic) {

  path <- GET(paste0(base_uri, "bills/search.json?query=", topic), add_headers('X-API-Key' = api_key))
  
  body_data <- fromJSON(content(path, "text"))  
  
  bills <- body_data$results$bills[[1]]
  
  #Selects and mutates the data in order to give me a data frame with only the bill info i need
  bills_topic <- bills %>%
    select(number, short_title, latest_major_action, congressdotgov_url) %>%
    mutate(primary_legislator_involved_info = paste0(bills$sponsor_title, " ", bills$sponsor_name,
                                                      bills$sponsor_state, "-", bills$sponsor_party))
  get_bills_on_topic <- bills_topic[1:5, ]

}


legislation_topic <- get_bills_on_topic("Russia")


#Part 1.1

#Takes in bill identifiers and returns details about the bill
get_bill_information <- function(bill_number, congress_number) {

  response <- GET(paste0(base_uri, "115/bills/", bill_number, ".json"), add_headers('X-API-Key' = api_key))
  

  body_data <- fromJSON(content(response, "text"))$results
  
  chamber <- body_data$actions[[1]] %>%
    filter(id == 1) %>%
    .$chamber
  

  selected_bill <- body_data %>%
    select(short_title, sponsor, latest_major_action) %>%
    mutate(chamber = chamber)
  

  get_bill_information <- selected_bill

}

 my_bill <- get_bill_information("S341")



#Section 2
 
 #Get the Washington Reps
 response <- GET(paste0(base_uri, "members/senate/WA/current.json"),add_headers('X-API-Key' = api_key))
 body <- fromJSON(content(response, "text"))
 WA_senators <- body$results
 
 
 # Choose representative I want by their id to use in my function
 WA_rep_id <- WA_senators$id[2]

#Gets the info about your local representative and then gets the info about them and their actions

get_rep_info <- function(WA_rep_id){
  

  response <- GET(paste0(base_uri, "members/", WA_rep_id, ".json"), add_headers('X-API-Key' = api_key))
  
  body <- fromJSON(content(response, "text"))
  
  rep_data <- body$results
  
  party <- rep_data$roles[[1]]$party[1]
  #Gets their vote percentage
  vote_data <- body$results$votes[[1]]
  
  
   my_rep_info <- rep_data %>%
    select(most_recent_vote, votesmart_id, url ,  twitter_account)
    
  
  get_rep_info <- my_rep_info
}

rep_actions <- get_rep_info(WA_rep_id)

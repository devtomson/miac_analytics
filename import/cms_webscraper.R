#load packages

rm(list = ls())

library(dplyr)
library(tidyr)
library(rvest)
library(stringr)
library(lubridate)
library(purrr)


# read in 2014 year -------------------------------------------------------

url_14 <- "https://apps.carleton.edu/athletics/varsity_sports/mens_soccer/schedule_and_results/?season=2014"
url_list <- str_c("https://apps.carleton.edu/athletics/varsity_sports/mens_soccer/schedule_and_results/?season=",
                  2001:2017)

col_names <- list(Date = "date",
                  `Opponent / Event` = "opponent", 
                  `City, State (Site)` = "city_state",
                  Result = "result")

scrap_standing <- function(url, year){
  
  
  webpage <- read_html(url)
  
  #headers of table 
  features_html <- html_nodes(webpage, '.schedHead')
  features_text <- html_text(features_html)
  
  #get positions of html cols to scrap
  pos <- match(c("Date", "Opponent / Event", "City, State (Site)", "Result"), features_text)
  cols_html <- html_nodes(webpage, str_c('.schedTD:nth-child(', pos, ')', collapse = ", "))
  cols_text <- html_text(cols_html)
  
  col_names <- rep(c("date", "opponent", "city_state", "result"),
                   length(cols_text)/4)
  
  games_df <- data_frame(
            game_id = rep((1:(length(cols_text)/4)), each = 4),
            value = cols_text, 
             key = col_names) %>% 
     spread(key, value) %>% 
    separate(city_state, c("city_state", "stadium"), 
             sep = "\\(") %>% 
    separate(result, c("result", "score", "over_time"), "\\s") %>% 
    separate(score, c("goals_for", "goals_against"), "-") %>% 
    #create other vars: home-away, opponent, date, over time, etc.
    mutate(stadium = str_replace(stadium, "\\)", ""),
           turf = if_else(stadium == "Bell Field", 
                          "H", "A"),
           date = mdy(str_c(date, "/", year)), 
           conf_game = str_detect(opponent, "\\*"),
           opp_rank = str_extract(opponent, "\\d+"), 
           opponent = str_extract(opponent, "[\\s\\.[[:alpha:]]]+"), 
           over_time = str_detect(over_time, "[2OT]"), 
           goals_for = as.numeric(goals_for),
           goals_against = as.numeric(goals_against)) %>% 
    separate(city_state, c("city", "state"), ",")
  
  return(games_df)
  
}
  
 df_2001 <- scrap_standing(url = url_list[[1]], year = 2001) 

df_games_01_17 <- map2_df(.x = url_list,
                          .y = 2001:2017,
                         .f = scrap_standing)

#still some filtering for NCAA and ployoffs games, fix St. John's cutoff, clean the H/A var

#playoff opponents are not listed, but could be added from other source...

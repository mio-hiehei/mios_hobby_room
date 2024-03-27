library(shiny)
library(tidyverse)
library(randomNames)
library(shinyWidgets)
library(truncnorm)
library(lubridate)
library(dygraphs)
library(xts)
library(leaflet)
#library(nominatim) # only needed for finding coordinates of adresses
library(foreign)
library(fuzzyjoin)
library(readxl)
library(shinyBS)
library(plotly)
library(shinythemes)
library(shinyjs)

# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## Surname length and productivity

source("generate_surname_data.R")

## classification accuracy of german volksparteien

df_ca <- read_csv2("accuracy_spdcdu.csv") %>%
  mutate(Year = str_extract(yearMo , "\\d{4}"),
         Quarter = str_extract(yearMo, "(?<=\\d{4}).+"),
         Month = case_when(Quarter == 1  ~ "01",
                           Quarter == 2  ~ "03",
                           Quarter == 3  ~ "06",
                           Quarter == 4  ~ "09"),
         date = as.Date(paste0(Year, "-", Month, "-01")),
         start_date = date,
         end_date = NA)


df_ca <-   df_ca %>% arrange(yearMo)

for(row in 1:nrow(df_ca)){
  
  
  if(row != nrow(df_ca)){
    df_ca$end_date[row] <- as_date(df_ca$start_date[row + 1] - 1,
                                               origin = "1970-01-01")
    
  } else {
    
    df_ca$end_date[row] <- max(df_ca$date) + 120
    
  }
  
}

df_ca <- df_ca %>%
  mutate(start_date = as_date(start_date),
         end_date = as_date(end_date)) %>%
  group_by(yearMo) %>%
  mutate(date = list(seq(from = start_date, to = end_date, by = 1)), created_at = NULL) %>%
  unnest(cols = c(date))



parlgov_elections <- read_csv("view_election.csv") %>%
  filter(country_name_short == "DEU" & election_date >= "1990-12-02" & party_name_short %in% c("CDU", "CSU", "SPD") & election_type == "parliament") %>%
  dplyr::select(election_date, vote_share, seats, party_name_short, left_right, election_id) %>%
   left_join(x = ., y = dplyr::select(read_csv("https://www.parlgov.org/data/parlgov-development_csv-utf-8/view_cabinet.csv"),  cabinet_name, election_id), by = "election_id") %>%
   distinct() %>%
  gather(variable, value, vote_share, seats, left_right) %>%
  unite(temp, party_name_short, variable) %>%
  spread(temp, value) %>%
  mutate(Union_left_right = CDU_left_right * (CDU_vote_share / (CDU_vote_share + CSU_vote_share) ) + CSU_left_right * ( CSU_vote_share / (CSU_vote_share + CDU_vote_share)),
         Union_seats = CDU_seats + CSU_seats,
         Union_vote_share = CDU_vote_share + CSU_vote_share) %>%
  dplyr::select(-(CDU_left_right:CSU_vote_share)) %>%
  rename(date = election_date) %>%
  filter(cabinet_name != "Merkel V") %>%
  rename(start_date = date) %>%
  mutate(end_date = NA)

# create end_date of row

for(row in 1:nrow(parlgov_elections)){
  
  
  if(row != nrow(parlgov_elections)){
    parlgov_elections$end_date[row] <- as_date(parlgov_elections$start_date[row + 1] - 1,
                                               origin = "1970-01-01")
    
  } else {
    
    parlgov_elections$end_date[row] <- max(df_ca$date)
    
  }
  
}

parlgov_dates <- parlgov_elections %>%
  mutate(start_date = as_date(start_date),
         end_date = as_date(end_date)) %>%
  group_by(cabinet_name) %>%
  filter(start_date != end_date) %>%
  mutate(date = list(seq(from = start_date, to = end_date, by = 1)), created_at = NULL) %>%
  unnest(cols = c(date))

df_merged <- left_join(df_ca, parlgov_dates, by = "date") %>%
  dplyr::select(yearMo, accuracy_list, date, start_date.y, cabinet_name:Union_vote_share) %>%
  rename(election_date = start_date.y) %>%
  ungroup()

## Schools in NRW


# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# 
# schools_nrw_df <- read_csv("schulen_nrw.csv")
# 
# colnames(schools_nrw_df) <- gsub("(?=,).*", "", colnames(schools_nrw_df), perl = TRUE)
# 
# schools_nrw_df$ORT <- gsub("(?= \\().+?(?<=\\))", "", perl = TRUE, schools_nrw_df$ORT)
# 
# full_adress <- gsub("ö", "oe", schools_nrw_df$ADRESSE)
# full_adress <- gsub("ä", "ae", full_adress)
# full_adress <- gsub("ü", "ue", full_adress)
# 
# einwohner <- read_xlsx("05-staedte.xlsx", sheet = "Städte") %>%
#   filter(bl_id == "05") %>%
#   mutate(ORT = gsub("(?=,).*", "", stadt, perl = TRUE),
#          ORT = gsub("(?= \\().+?(?<=\\))", "", ORT, perl = TRUE))
# 
# df_cities_schools <- data.frame(ORT = sort(unique(schools_nrw_df$ORT)))
# 
# df_cities_schools <- stringdist_left_join(df_cities_schools, einwohner, by = c("ORT"),max_dist = 0,
#                                           distance_col = "distance") %>%
#   select(ORT.x, einwohner) %>%
#   rename(ORT = ORT.x)
# 
# schools_nrw_df <- left_join(schools_nrw_df, df_cities_schools, by = "ORT")
# 
# ort <- gsub("ö", "oe", schools_nrw_df$ORT)
# ort <- gsub("ä", "ae", ort)
# ort <- gsub("ü", "ue", ort)
# 
# 
# schools_nrw_df$full_adress <- paste(full_adress, schools_nrw_df$POSTLEITZA, ort, "Germany", sep = ", ")
# 
# 
# ## geocode adresses into coordinates
# # from https://developer.mapquest.com/user/me/apps
# api_key = "8CIZsbQkZU3iqiM767Gcub6z2FDUMkmQ"
# 
# geocode_schools <- function(adress, api_key){
# 
# 
#   result <- osm_geocode(query = adress,
#                         key = getOption("OSM_API_KEY", api_key),
#                         email = getOption("OSM_API_EMAIL", "mio.hiehei@outlook.de"),
#                         country_codes = "de")
# 
#   if(nrow(result) == 0){
# 
#     result <- data.frame(lat = NA,
#                          lon = NA)
# 
#   } else {
# 
#   result <- result[, c("lat", "lon")]
# 
#   }
# 
#   Sys.sleep(1.3)
#   
#   message(paste0(adress, ":", result[,1],", ", result[,2]))
#   
#   return(result)
# 
# 
# 
# }
# 
# # # testing
# # schulenkoeln <- sample(schools_nrw_df$full_adress[grepl(x = schools_nrw_df$ORT, pattern = "ä|ö|ü")], 2)
# # results_koelle <- lapply(X = schulenkoeln, geocode_schools, api_key = api_key)
# 
# results <- lapply(X = schools_nrw_df$full_adress, geocode_schools, api_key = api_key)
# 
# results2 <- do.call("rbind", results)
# 
# schools_nrw_df <- cbind.data.frame(schools_nrw_df, results2)
# 
# write_csv2(schools_nrw_df, "C:/Users/miohi/Documents/Github/HieHei_Showoff_Dashboard/HieHeiShowoff/schools_df_coded.csv")
# 
#   
# schools_df_omitted <- schools_nrw_df %>%
#   filter(!is.na(lat) | !is.na(lon))
# 
# write_csv2(schools_df_omitted, "C:/Users/miohi/Documents/Github/HieHei_Showoff_Dashboard/HieHeiShowoff/schools_df_ommited.csv")


schools_df_omitted <- read_csv2("schools_df_ommited.csv") %>%
  mutate(SCHULFORM = ifelse(grepl("rderschule", SCHULFORM), "Förderschule", SCHULFORM))


# https://stackoverflow.com/questions/70898442/add-na-option-in-the-sliderinput-of-shiny-app



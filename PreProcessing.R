library(dplyr)
library(tidyverse)

nyc_airbnb <- read.csv("airbnbmark1.csv")

nyc_airbnb_clean <- nyc_airbnb %>%
  select(property_type, room_type, name, host_name, neighbourhood, host_identity_verified, 
         city, latitude, longitude, accommodates, bathrooms, bedrooms, price, 
         number_of_reviews, review_scores_rating, cancellation_policy) %>%
  filter(review_scores_rating == 100,
         host_identity_verified == "t",
         !duplicated(paste(round(latitude, 5), round(longitude, 5))),
         city != "",
         neighbourhood != "",
         city != "纽约市") %>%
  mutate(city = str_trim(city),
         city = str_to_title(city),
         review_scores_rating = as.numeric(review_scores_rating),
         price = gsub("\\$", "", price),
         price = as.numeric(gsub("\\.00", "", price))) %>%
  drop_na()

nyc_crime <- read.csv("NYPD_Complaint_Data_Current__Year_To_Date__20241204.csv")

nyc_crime_clean <- nyc_crime %>%
  select(CMPLNT_FR_DT, LAW_CAT_CD, Latitude, Longitude, BORO_NM, OFNS_DESC) %>%
  filter(BORO_NM != "(null)",
         OFNS_DESC != "(null)") %>%
  mutate(BORO_NM = str_to_title(BORO_NM),
         CMPLNT_FR_DT = as.Date(CMPLNT_FR_DT, format = "%d/%m/%Y"),
         MONTH = month(CMPLNT_FR_DT)) %>%
  drop_na()

#write.csv(nyc_airbnb_clean, "nyc_airbnb_clean.csv", row.names = FALSE)
#write.csv(nyc_crime_clean, "nyc_crime_clean.csv", row.names = FALSE)
save(nyc_airbnb_clean, nyc_crime_clean, file = "final.RData")

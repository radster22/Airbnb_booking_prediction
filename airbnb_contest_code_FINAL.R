#load libraries
library(tidyverse)
library(readr)
library(pROC)
library(ROCR)
library(mice)
library(zoo)
library(forcats)
#install.packages('quanteda')
library(tidytext)
library(text2vec)
library(quanteda)
library(tm)



set.seed(1)

#load data files
#setwd('C:/Users/aradh/OneDrive/Desktop/Data mining T/files/Project')
setwd('~/Downloads')
train_x <- read_csv("airbnb_train_x_2024.csv")
train_y <- read_csv("airbnb_train_y_2024.csv")
test_x <- read_csv("airbnb_test_x_2024.csv")
ext_data <- read_csv("zip_code_demographics.csv")

ext_data <- ext_data %>%
  mutate(zip = ifelse(nchar(zip) == 4, paste0("0", zip), zip)) 

ext_data$zip <- ifelse(nchar(ext_data$zip) > 5, substr(ext_data$zip, 1, 5),ext_data$zip)
view(ext_data)

train <- cbind(train_x, train_y) %>%
  mutate(perfect_rating_score = as.factor(perfect_rating_score),
         high_booking_rate = as.factor(high_booking_rate)) 


airbnb_train <- train %>%
  mutate(host_since = as.Date(host_since, format = "%Y-%m-%d"),  #1
         host_since = na.approx(host_since),
         host_response_rate = parse_number(as.character(host_response_rate)),
         host_response_time = ifelse(is.na(host_response_time), 0, host_response_time),  #2
         host_response_rate = ifelse(is.na(host_response_rate), 0, host_response_rate),  #3
         host_acceptance_rate = parse_number(as.character(host_acceptance_rate)),   #4
         host_acceptanced = as.factor(ifelse(is.na(host_acceptance_rate), "Missing", ifelse(host_acceptance_rate == 100,"All","Few"))),  #31
         host_neighbourhood = as.factor(ifelse(is.na(host_neighbourhood), "Missing", host_neighbourhood)), #5
         host_total_listings_count = ifelse(is.na(host_total_listings_count), median(host_total_listings_count, na.rm = TRUE), host_total_listings_count), #6
         neighborhood = as.factor(ifelse(is.na(neighborhood), "Missing", neighborhood)), #7
         city = as.character(city),  #8
         state = as.factor(tolower(state)),   #9
         country_code = as.factor(country_code), #12
         country = as.factor(country), #13
         property_type = as.factor(property_type), #14
         room_type = as.factor(room_type),  #15
         bed_type = as.factor(bed_type), #20
         cleaning_fee = ifelse(is.na(cleaning_fee), 0, cleaning_fee),  #35
         security_deposit = ifelse(is.na(security_deposit), 0, security_deposit),  #36
         guests_included = parse_number(as.character(guests_included)),  #27  these 2 are just for show
         extra_people = parse_number(as.character(extra_people)),  #26
         minimum_nights = ifelse(is.na(minimum_nights), mean(minimum_nights, na.rm = TRUE), minimum_nights),  #32
         maximum_nights = ifelse(is.na(maximum_nights), mean(maximum_nights, na.rm = TRUE), maximum_nights),  #33
         availability = as.factor(ifelse(availability_365 > 100, "Unpopular", "Popular")),   #37
         first_review = as.Date(first_review, format = "%Y-%m-%d"),  #28
         license = parse_number(as.character(license)),   #29  useful or not? warning here
         cancellation_policy = ifelse(cancellation_policy %in% c("super_strict_30", "super_strict_60", "strict", "no_refunds"), "strict", cancellation_policy),  #30
         )
airbnb_train$property_type <- fct_explicit_na(airbnb_train$property_type, "Other")

airbnb_train <- airbnb_train %>%  #10
  group_by(market) %>%
  mutate(market_count=n(),
         market = ifelse(is.na(market),'Other', market),
         market = ifelse((market_count < 200), 'Other', market))%>%
  ungroup() %>%
  mutate(market = as.factor(market))

airbnb_train <- airbnb_train %>%
  mutate(market = as.factor(case_when(market  == "East Bay, CA" ~ "San Francisco",
                            market  == "Other (Domestic)" ~ "Other",
                            market  == "Monterey Region" ~ "San Francisco",
                            TRUE ~ market)))

airbnb_train <- airbnb_train %>%  #11
  group_by(smart_location) %>%
  mutate(smart_count=n(),
         smart_location = ifelse(is.na(smart_location),'Other', smart_location),
         smart_location = ifelse((smart_count < 200), 'Other', smart_location))%>%
  ungroup()%>%
  mutate(smart_location = as.factor(smart_location))

airbnb_train <- airbnb_train %>%
  mutate(smart_location = as.factor(case_when(
    smart_location  %in% c("Beverly Hills, CA", "Glendale, CA", "Long Beach, CA", "Malibu, CA", 
                        "Marina del Rey, CA", "Pasadena, CA", "Redondo Beach, CA", "Santa Cruz, CA", 
                        "Santa Monica, CA", "Venice, CA", "West Hollywood, CA") ~ "Los Angeles, CA",
    smart_location  %in% c("Brooklyn, NY", "Bronx, NY", "Queens, NY") ~  "New York, NY",
    smart_location  == "Oakland, CA" ~ "San Francisco, CA",
    smart_location == "Brooklyn , NY" ~ "Brooklyn, NY",
    TRUE ~ smart_location)))

airbnb_train <- airbnb_train %>%
  group_by(property_type)%>%
  mutate(accommodates = ifelse(is.na(accommodates),median(accommodates, na.rm = TRUE),accommodates), #16
         bathrooms = (ifelse(is.na(bathrooms),median(bathrooms, na.rm = TRUE),bathrooms)),  #17
         bedrooms = (ifelse(is.na(bedrooms),median(bedrooms, na.rm = TRUE),bedrooms)),      #18
         beds = (ifelse(is.na(beds),median(beds, na.rm = TRUE),beds))                       #19
  ) %>%
  ungroup()

airbnb_train <- airbnb_train %>%
  mutate(bed_bath_ratio = beds/bathrooms,
    bed_bath_ratio = ifelse((is.na(bed_bath_ratio) | is.infinite(bed_bath_ratio)), 0, bed_bath_ratio))

category_mapping <- c(
  "Apartment" = "Apartment",
  "Bed & Breakfast" = "Bed & Breakfast",
  "Boat" = "Unique Accommodation",
  "Boutique hotel" = "Hotel",
  "Bungalow" = "House",
  "Cabin" = "Unique Accommodation",
  "Camper/RV" = "Unique Accommodation",
  "Castle" = "Unique Accommodation",
  "Cave" = "Unique Accommodation",
  "Chalet" = "Unique Accommodation",
  "Condominium" = "Apartment",
  "Dorm" = "Hostel",
  "Earth House" = "Unique Accommodation",
  "Entire Floor" = "Apartment",
  "Guest suite" = "Apartment",
  "Guesthouse" = "House",
  "Hostel" = "Hostel",
  "House" = "House",
  "Hut" = "Unique Accommodation",
  "In-law" = "Apartment",
  "Island" = "Unique Accommodation",
  "Lighthouse" = "Unique Accommodation",
  "Loft" = "Apartment",
  "Other" = "Other",
  "Plane" = "Unique Accommodation",
  "Serviced apartment" = "Apartment",
  "Tent" = "Unique Accommodation",
  "Timeshare" = "Apartment",
  "Tipi" = "Unique Accommodation",
  "Townhouse" = "Apartment",
  "Train" = "Unique Accommodation",
  "Treehouse" = "Unique Accommodation",
  "Vacation home" = "House",
  "Villa" = "House",
  "Yurt" = "Unique Accommodation"
)


airbnb_train <- airbnb_train %>%
  mutate(major_category = category_mapping[airbnb_train$property_type],
         major_category = as.factor(ifelse(is.na(major_category), 'Other', major_category)))  #34

airbnb_train <- airbnb_train %>%
  group_by(major_category, room_type) %>% 
  mutate(price = ifelse(is.na(price) | price == 0, mean(price, na.rm = TRUE), price),  #20
  ) %>%
  ungroup()%>%
  group_by(bedrooms)%>%  
  mutate(price_per_person = ifelse(accommodates > 0, price/accommodates, 0)) %>%  #21
  mutate(price_per_person = ifelse(is.na(price_per_person), mean(price_per_person, na.rm = TRUE), price_per_person))%>%
  ungroup()%>%
  group_by(room_type)%>%
  mutate(
    weekly_price = ifelse(is.na(weekly_price), mean(weekly_price, na.rm = TRUE), weekly_price),      #22
    monthly_price = ifelse(is.na(monthly_price), mean(monthly_price, na.rm = TRUE), monthly_price),  #23
    has_cleaning_fee = ifelse(!is.na(cleaning_fee) & cleaning_fee > 0, 1, 0),                        #24
    has_security_deposit = ifelse(!is.na(security_deposit) & security_deposit > 0, 1, 0)             #25
  )

airbnb_train <- airbnb_train %>%   #38
  mutate(host_listings = as.factor(case_when(host_listings_count >= 200 ~ 'Hotelier',
                                   host_listings_count >= 100 ~ 'Guest House Manager',
                                   host_listings_count >=50 ~ 'Businessman',
                                   TRUE ~ 'Property Owner')))

airbnb_train  <- airbnb_train  %>%
  mutate(id = row_number())

airbnb_train  <- airbnb_train  %>%
  group_by(major_category) %>%
  mutate(median_ppp_ind = median(price_per_person, na.rm= TRUE)) %>%
  ungroup()

airbnb_train  <- airbnb_train  %>%
  mutate(ppp_ind = as.factor(ifelse((price_per_person > median_ppp_ind), "1", "0")))

cleaning_tokenizer_delimeted <- function(v) {
  v %>%
    space_tokenizer(sep = ',') 
}
it_train_f <- itoken(airbnb_train$features, 
                     preprocessor = tolower, 
                     tokenizer = cleaning_tokenizer_delimeted, 
                     progressbar = FALSE)
vocab <- create_vocabulary(it_train_f)
vectorizer <- vocab_vectorizer(vocab)
# Convert the training documents into a DTM
dtm_train_f <- create_dtm(it_train_f, vectorizer)
dim(dtm_train_f)
dim(airbnb_train)
dtm_train_f_matrix <- as.matrix(dtm_train_f)

# Combine airbnb_train with dtm_train_matrix
airbnb_train <- cbind(airbnb_train, dtm_train_f_matrix)
dim(airbnb_train)

airbnb_train <- airbnb_train %>%
  rename(require_guest_profile_picture = `require guest profile picture`,
         require_guest_phone_verification = `require guest phone verification`,
         requires_license = `requires license`,
         host_is_superhost = `host is superhost`,
         instant_bookable = `instant bookable`,
         host_identity_verified = `host identity verified`,
         is_location_exact = `is location exact`,
         host_has_profile_pic = `host has profile pic`)


airbnb_train <- airbnb_train %>%
  mutate(host_verifications = gsub("\\[","", host_verifications),
         host_verifications = gsub("\\]","", host_verifications),
         host_verifications = gsub("\\'","", host_verifications))

airbnb_train$host_verifications <- ifelse(is.na(airbnb_train$host_verifications), "None", airbnb_train$host_verifications)

verification_methods <- unique(unlist(strsplit(as.character(airbnb_train$host_verifications), ', ')))
verification_methods <- setdiff(verification_methods, "None")

airbnb_train$verif_count <- apply(airbnb_train, 1, function(x) {
  sum(sapply(verification_methods, function(method) grepl(method, x["host_verifications"])))
})

airbnb_train <- airbnb_train %>% 
  mutate(price_x_7 = price * 7)

airbnb_train <- airbnb_train %>% 
  mutate(comp_price = ifelse(weekly_price <= price_x_7, "good deal", "bad deal"),
         comp_price = as.factor(comp_price))

airbnb_train <- airbnb_train %>%
  mutate(min_booking_fee = price*minimum_nights)

airbnb_train <- airbnb_train %>%
  mutate(beds_accommodates = accommodates/beds,
         beds_accommodates = ifelse((is.na(beds_accommodates) | is.infinite(beds_accommodates)), 0, beds_accommodates))

airbnb_train <- airbnb_train %>%
  mutate(
    jurisdiction_names = ifelse(is.na(jurisdiction_names), "Other", jurisdiction_names),
    jurisdiction_names = gsub("[^[:alnum:]]", " ", jurisdiction_names),
    jurisdiction_names = gsub("\\s+", " ", jurisdiction_names),  # Replace consecutive whitespace with a single space
    jurisdiction_names = as.factor(jurisdiction_names)
  ) %>%
  group_by(jurisdiction_names) %>%
  mutate(
    jurisdiction_count = n(),
    jurisdiction_namess = if_else(jurisdiction_count < 100, "Other", jurisdiction_names)
  ) %>%
  ungroup()


airbnb_train$amenities = gsub("\\{", "", airbnb_train$amenities)
airbnb_train$amenities = gsub("\\}", "", airbnb_train$amenities)
# splitting based on ","
amenities.split <- strsplit(airbnb_train$amenities, ",")
lev <- unique(unlist(amenities.split))
amenities.dummy <- (lapply(amenities.split, function(x) table(factor(x, levels=lev))))
airbnb_train_new <- with(airbnb_train, data.frame(access, do.call(rbind, amenities.dummy), accommodates))
airbnb_train_new <- subset(airbnb_train_new, select = -c(access,accommodates))

airbnb_train <- cbind(airbnb_train, airbnb_train_new)

cleaning_tokenizer <- function(v) {
  v %>%
    removeNumbers %>% #remove all numbers
    removePunctuation %>% #remove all punctuation
    removeWords(tm::stopwords(kind="en")) %>% #remove stopwords
    stemDocument %>%
    word_tokenizer 
}


it_train_summary <- itoken(airbnb_train$summary, 
                           preprocessor = tolower, #preprocessing by converting to lowercase
                           tokenizer = cleaning_tokenizer, 
                           progressbar = FALSE)


vocab_summary_train <- create_vocabulary(it_train_summary, ngram = c(1L, 2L))


vocab_final_summary_train <- prune_vocabulary(vocab_summary_train, term_count_min = 10)
print(vocab_final_summary_train)
vectorizer_train_summary <- vocab_vectorizer(vocab_final_summary_train)

dtm_train_summary <- create_dtm(it_train_summary, vectorizer_train_summary)
dim(dtm_train_summary)
get_sentiments("bing")
library(textdata)
library(quanteda)

bing_negative <- get_sentiments("bing") %>%
  filter(sentiment == 'negative')

bing_positive <- get_sentiments("bing") %>%
  filter(sentiment == 'positive')

mydict <- dictionary(list(negative = bing_negative$word, positive = bing_positive$word))
tr_dfm_summary <- as.dfm(dtm_train_summary)

tr_dfm_summary

sentiments_summary_train <- dfm_lookup(tr_dfm_summary, mydict, valuetype = 'fixed')
sentiments_summary_train 
sentiments_summary_train <- convert(sentiments_summary_train, to = "data.frame") %>%
  mutate(sent_score_summary = as.factor(ifelse(positive >= negative, 1, 0)))
dim(sentiments_summary_train)

airbnb_train <- airbnb_train %>%
  mutate(house_rules = as.character(ifelse(is.na(house_rules), "Not Specified", house_rules)))


it_train_rules <- itoken(airbnb_train$house_rules, 
                         preprocessor = tolower, #preprocessing by converting to lowercase
                         tokenizer = cleaning_tokenizer, 
                         progressbar = FALSE)


vocab_house_rules <- create_vocabulary(it_train_rules, ngram = c(1L, 2L))


vocab_final_house_rules <- prune_vocabulary(vocab_house_rules, term_count_min = 10)
print(vocab_final_house_rules)
vectorizer_house_rules <- vocab_vectorizer(vocab_final_house_rules)


dtm_train_house_rules <- create_dtm(it_train_rules, vectorizer_house_rules)
dim(dtm_train_house_rules)
tr_dfm_house_rules <- as.dfm(dtm_train_house_rules)

tr_dfm_house_rules

sentiments_house_rules <- dfm_lookup(tr_dfm_house_rules, mydict, valuetype = 'fixed')
sentiments_house_rules 

sentiments_house_rules <- convert(sentiments_house_rules, to = "data.frame") %>%
  mutate(sent_score_house_rules = as.factor(ifelse(positive >= negative, 1, 0)))
dim(sentiments_house_rules)








airbnb_train <- cbind(airbnb_train, sent_score_houserules = sentiments_house_rules[, 4], sent_score_summary = sentiments_summary_train[,4])
names(airbnb_train)

#--------------------------------------------------------External Dataset Below-----------------------------------------------------
airbnb_train <- airbnb_train %>%
mutate(zipcode = ifelse(nchar(zipcode) == 4, paste0("0", zipcode), zipcode))
airbnb_train$zipcode <- ifelse(nchar(airbnb_train$zipcode) > 5, substr(airbnb_train$zipcode, 1, 5), airbnb_train$zipcode)

# Replace missing zipcodes with mode of zipcodes in the same city
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

airbnb_train_subset <- airbnb_train %>%
  group_by(city) %>%
  mutate(zipcode = ifelse(is.na(zipcode) | nchar(zipcode) < 4, 
                          Mode(zipcode), 
                          zipcode)) %>%
  ungroup() %>%
  group_by(state) %>%
  mutate(zipcode = ifelse(is.na(zipcode) | nchar(zipcode) < 4, 
                          Mode(zipcode), 
                          zipcode)) %>%
  ungroup()

airbnb_train <- merge(airbnb_train_subset, 
                      ext_data %>% select(zip, population, density, dist2_medium_airport), 
                      by.x = "zipcode", 
                      by.y = "zip", 
                      all.x = TRUE)

airbnb_train <- airbnb_train %>%
  mutate(population = ifelse(is.na(population), median(population, na.rm = TRUE), population),
         density = ifelse(is.na(density), median(density, na.rm = TRUE), density),
         dist2_medium_airport = ifelse(is.na(dist2_medium_airport), median(dist2_medium_airport, na.rm = TRUE), dist2_medium_airport))

airbnb_test <- test_x %>%
  mutate(host_since = as.Date(host_since, format = "%Y-%m-%d"),  #1
         host_since = na.approx(host_since),
         host_response_rate = parse_number(as.character(host_response_rate)),
         host_response_time = ifelse(is.na(host_response_time), 0, host_response_time),  #2
         host_response_rate = ifelse(is.na(host_response_rate), 0, host_response_rate),  #3
         host_acceptance_rate = parse_number(as.character(host_acceptance_rate)),   #4   this contains NA and we are fine
         host_acceptanced = as.factor(ifelse(is.na(host_acceptance_rate), "Missing", ifelse(host_acceptance_rate == 100,"All","Few"))),  #31
         host_neighbourhood = as.factor(ifelse(is.na(host_neighbourhood), "Missing", host_neighbourhood)), #5
         host_total_listings_count = ifelse(is.na(host_total_listings_count), median(host_total_listings_count, na.rm = TRUE), host_total_listings_count), #6
         neighborhood = as.factor(ifelse(is.na(neighborhood), "Missing", neighborhood)), #7
         city = as.character(city),  #8
         state = as.factor(tolower(state)),   #9
         country_code = as.factor(country_code), #12
         country = as.factor(country), #13
         property_type = as.factor(property_type), #14
         room_type = as.factor(room_type),  #15
         bed_type = as.factor(bed_type), #20
         cleaning_fee = ifelse(is.na(cleaning_fee), 0, cleaning_fee),  #35
         security_deposit = ifelse(is.na(security_deposit), 0, security_deposit),  #36
         guests_included = parse_number(as.character(guests_included)),  #27  
         extra_people = parse_number(as.character(extra_people)),  #26
         minimum_nights = ifelse(is.na(minimum_nights), mean(minimum_nights, na.rm = TRUE), minimum_nights),  #32
         maximum_nights = ifelse(is.na(maximum_nights), mean(maximum_nights, na.rm = TRUE), maximum_nights),  #33
         availability = as.factor(ifelse(availability_365 > 100, "Unpopular", "Popular")),   #37
         first_review = as.Date(first_review, format = "%Y-%m-%d"),  #28
         license = parse_number(as.character(license)),   #29 
         cancellation_policy = ifelse(cancellation_policy %in% c("super_strict_30", "super_strict_60", "strict", "no_refunds"), "strict", cancellation_policy),  #30
  )
airbnb_test$property_type <- fct_explicit_na(airbnb_test$property_type, "Other")

summary(airbnb_test)
airbnb_test <- airbnb_test %>%  #10
  group_by(market) %>%
  mutate(market_count=n(),
         market = ifelse(is.na(market),'Other', market),
         market = ifelse((market_count < 200), 'Other', market))%>%
  ungroup()%>%
  mutate(market = as.factor(market))

airbnb_test <- airbnb_test %>%
  mutate(market = as.factor(case_when(market  == "East Bay, CA" ~ "San Francisco",
                                      market  == "Other (Domestic)" ~ "Other",
                                      market  == "Monterey Region" ~ "San Francisco",
                                      TRUE ~ market)))

airbnb_test <- airbnb_test %>%  #11
  group_by(smart_location) %>%
  mutate(smart_count=n(),
         smart_location = ifelse(is.na(smart_location),'Other', smart_location),
         smart_location = ifelse((smart_count < 200), 'Other', smart_location))%>%
  ungroup()%>%
  mutate(smart_location = as.factor(smart_location))

airbnb_test <- airbnb_test %>%
  mutate(smart_location = as.factor(case_when(
    smart_location  %in% c("Beverly Hills, CA", "Glendale, CA", "Long Beach, CA", "Malibu, CA", 
                           "Marina del Rey, CA", "Pasadena, CA", "Redondo Beach, CA", "Santa Cruz, CA", 
                           "Santa Monica, CA", "Venice, CA", "West Hollywood, CA") ~ "Los Angeles, CA",
    smart_location  %in% c("Brooklyn , NY", "Bronx, NY", "Queens, NY") ~  "New York, NY",
    smart_location  == "Oakland, CA" ~ "San Francisco, CA",
    smart_location == "Brooklyn , NY" ~ "Brooklyn, NY",
    TRUE ~ smart_location)))

airbnb_test <- airbnb_test %>%
  group_by(property_type)%>%
  mutate(accommodates = ifelse(is.na(accommodates),median(accommodates, na.rm = TRUE),accommodates), #16
         bathrooms = (ifelse(is.na(bathrooms),median(bathrooms, na.rm = TRUE),bathrooms)),  #17
         bedrooms = (ifelse(is.na(bedrooms),median(bedrooms, na.rm = TRUE),bedrooms)),      #18
         beds = (ifelse(is.na(beds),median(beds, na.rm = TRUE),beds))                       #19
  ) %>%
  ungroup()

airbnb_test <- airbnb_test %>%
  mutate(bed_bath_ratio = beds/bathrooms,
    bed_bath_ratio = ifelse((is.na(bed_bath_ratio) | is.infinite(bed_bath_ratio)), 0, bed_bath_ratio))

category_mapping <- c(
  "Apartment" = "Apartment",
  "Bed & Breakfast" = "Bed & Breakfast",
  "Boat" = "Unique Accommodation",
  "Boutique hotel" = "Hotel",
  "Bungalow" = "House",
  "Cabin" = "Unique Accommodation",
  "Camper/RV" = "Unique Accommodation",
  "Castle" = "Unique Accommodation",
  "Cave" = "Unique Accommodation",
  "Chalet" = "Unique Accommodation",
  "Condominium" = "Apartment",
  "Dorm" = "Hostel",
  "Earth House" = "Unique Accommodation",
  "Entire Floor" = "Apartment",
  "Guest suite" = "Apartment",
  "Guesthouse" = "House",
  "Hostel" = "Hostel",
  "House" = "House",
  "Hut" = "Unique Accommodation",
  "In-law" = "Apartment",
  "Island" = "Unique Accommodation",
  "Lighthouse" = "Unique Accommodation",
  "Loft" = "Apartment",
  "Other" = "Other",
  "Plane" = "Unique Accommodation",
  "Serviced apartment" = "Apartment",
  "Tent" = "Unique Accommodation",
  "Timeshare" = "Apartment",
  "Tipi" = "Unique Accommodation",
  "Townhouse" = "Apartment",
  "Train" = "Unique Accommodation",
  "Treehouse" = "Unique Accommodation",
  "Vacation home" = "House",
  "Villa" = "House",
  "Yurt" = "Unique Accommodation"
)


airbnb_test <- airbnb_test %>%
  mutate(major_category = (category_mapping[airbnb_test$property_type]),
         major_category = as.factor(ifelse(is.na(major_category), 'Other', major_category)))  #34

airbnb_test <- airbnb_test %>%
  group_by(major_category, room_type) %>%   #property_type, room_type, market, city, neighborhood
  mutate(price = ifelse(is.na(price) | price == 0, mean(price, na.rm = TRUE), price),  #20
  ) %>%
  ungroup()%>%
  group_by(bedrooms)%>% 
  mutate(price_per_person = ifelse(accommodates > 0, price/accommodates, 0)) %>%  #21
  mutate(price_per_person = ifelse(is.na(price_per_person), mean(price_per_person, na.rm = TRUE), price_per_person))%>%
  ungroup()%>%
  group_by(room_type)%>%  
  mutate(
    weekly_price = ifelse(is.na(weekly_price), mean(weekly_price, na.rm = TRUE), weekly_price),      #22
    monthly_price = ifelse(is.na(monthly_price), mean(monthly_price, na.rm = TRUE), monthly_price),  #23
    has_cleaning_fee = ifelse(!is.na(cleaning_fee) & cleaning_fee > 0, 1, 0),                        #24
    has_security_deposit = ifelse(!is.na(security_deposit) & security_deposit > 0, 1, 0)             #25
  )

airbnb_test <- airbnb_test %>%   #38
  mutate(host_listings = as.factor(case_when(host_listings_count >= 200 ~ 'Hotelier',
                                   host_listings_count >= 100 ~ 'Guest House Manager',
                                   host_listings_count >=50 ~ 'Businessman',
                                   TRUE ~ 'Property Owner')))

airbnb_test <- airbnb_test %>%
  mutate(min_booking_fee = price*minimum_nights)

airbnb_test  <- airbnb_test  %>%
  mutate(id = row_number())

airbnb_test  <- airbnb_test  %>%
  group_by(major_category) %>%
  mutate(median_ppp_ind = median(price_per_person, na.rm= TRUE)) %>%
  ungroup()

airbnb_test  <- airbnb_test  %>%
  mutate(ppp_ind = as.factor(ifelse((price_per_person > median_ppp_ind), "1", "0")))

airbnb_test <- airbnb_test %>%
  mutate(host_verifications = gsub("\\[","", host_verifications),
         host_verifications = gsub("\\]","", host_verifications),
         host_verifications = gsub("\\'","", host_verifications))

airbnb_test$host_verifications <- ifelse(is.na(airbnb_test$host_verifications), "None", airbnb_test$host_verifications)

verification_methods <- unique(unlist(strsplit(as.character(airbnb_test$host_verifications), ', ')))
verification_methods <- setdiff(verification_methods, "None")

unique(airbnb_test$host_verifications)

airbnb_test$verif_count <- apply(airbnb_test, 1, function(x) {
  sum(sapply(verification_methods, function(method) grepl(method, x["host_verifications"])))
})

table(is.na(airbnb_test$verif_count))

airbnb_test <- airbnb_test %>% 
  mutate(price_x_7 = price * 7)

airbnb_test <- airbnb_test %>% 
  mutate(comp_price = ifelse(weekly_price <= price_x_7, "good deal", "bad deal"),
         comp_price = as.factor(comp_price))

airbnb_test <- airbnb_test %>%
  mutate(min_booking_fee = price*minimum_nights)

airbnb_test <- airbnb_test %>%
  mutate(beds_accommodates = accommodates/beds,
         beds_accommodates = ifelse((is.na(beds_accommodates) | is.infinite(beds_accommodates)), 0, beds_accommodates))

airbnb_test <- airbnb_test %>%
  mutate(
    jurisdiction_names = ifelse(is.na(jurisdiction_names), "Other", jurisdiction_names),
    jurisdiction_names = gsub("[^[:alnum:]]", " ", jurisdiction_names),
    jurisdiction_names = gsub("\\s+", " ", jurisdiction_names),  # Replace consecutive whitespace with a single space
    jurisdiction_names = as.factor(jurisdiction_names)
  ) %>%
  group_by(jurisdiction_names) %>%
  mutate(
    jurisdiction_count = n(),
    jurisdiction_namess = if_else(jurisdiction_count < 100, "Other", jurisdiction_names)
  ) %>%
  ungroup()

airbnb_test$amenities = gsub("\\{", "", airbnb_test$amenities)
airbnb_test$amenities = gsub("\\}", "", airbnb_test$amenities)
# splitting based on ","
amenities.split <- strsplit(airbnb_test$amenities, ",")
lev <- unique(unlist(amenities.split))
amenities.dummy <- (lapply(amenities.split, function(x) table(factor(x, levels=lev))))
airbnb_test_new <- with(airbnb_test, data.frame(access, do.call(rbind, amenities.dummy), accommodates))
airbnb_test_new <- subset(airbnb_test_new, select = -c(access,accommodates))

airbnb_test <- cbind(airbnb_test, airbnb_test_new)


##text mining test data

it_test_f <- itoken(airbnb_test$features, 
                    preprocessor = tolower, #preprocessing by converting to lowercase
                    tokenizer = cleaning_tokenizer_delimeted, 
                    progressbar = FALSE)
# Convert the training documents into a DTM
dtm_test_f <- create_dtm(it_test_f, vectorizer)
dim(dtm_test_f)
dim(airbnb_test)
dtm_test_f_matrix <- as.matrix(dtm_test_f)

# Combine airbnb_train with dtm_train_matrix
airbnb_test <- cbind(airbnb_test, dtm_test_f_matrix)
dim(airbnb_test)
dim(airbnb_train)
dim(dtm_train_f)
dim(dtm_test_f)
airbnb_test <- airbnb_test %>%
  rename(require_guest_profile_picture = `require guest profile picture`,
         require_guest_phone_verification = `require guest phone verification`,
         requires_license = `requires license`,
         host_is_superhost = `host is superhost`,
         instant_bookable = `instant bookable`,
         host_identity_verified = `host identity verified`,
         is_location_exact = `is location exact`,
         host_has_profile_pic = `host has profile pic`)


airbnb_test <- airbnb_test %>%
  mutate(house_rules = as.character(ifelse(is.na(house_rules), "Not Specified", house_rules)))



it_test_rules <- itoken(airbnb_test$house_rules, 
                        preprocessor = tolower, #preprocessing by converting to lowercase
                        tokenizer = cleaning_tokenizer, 
                        progressbar = FALSE)


vocab_test_house_rules <- create_vocabulary(it_test_rules, ngram = c(1L, 2L))


vocab_final_house_rules_test <- prune_vocabulary(vocab_test_house_rules, term_count_min = 10)
print(vocab_final_house_rules_test)
vectorizer_house_rules_test <- vocab_vectorizer(vocab_final_house_rules_test)

dtm_test_house_rules_test <- create_dtm(it_test_rules, vectorizer_house_rules_test)
dim(dtm_test_house_rules_test)
tr_dfm_house_rules_test <- as.dfm(dtm_test_house_rules_test)
tr_dfm_house_rules_test

sentiments_house_rules_test <- dfm_lookup(tr_dfm_house_rules_test, mydict, valuetype = 'fixed')
sentiments_house_rules_test 
sentiments_house_rules_test <- convert(sentiments_house_rules_test, to = "data.frame") %>%
  mutate(sent_score_house_rules = as.factor(ifelse(positive >= negative, 1, 0)))
dim(sentiments_house_rules_test)



it_test_summary <- itoken(airbnb_test$summary, 
                          preprocessor = tolower, #preprocessing by converting to lowercase
                          tokenizer = cleaning_tokenizer, 
                          progressbar = FALSE)

vocab_test_summary <- create_vocabulary(it_test_summary, ngram = c(1L, 2L))


vocab_final_summary_test <- prune_vocabulary(vocab_test_summary, term_count_min = 10)
print(vocab_final_summary_test)
vectorizer_summary_test <- vocab_vectorizer(vocab_final_summary_test)

dtm_test_summary <- create_dtm(it_test_summary, vectorizer_summary_test)

dim(dtm_test_summary)

tr_dfm_summary_test <- as.dfm(dtm_test_summary)
tr_dfm_summary_test

sentiments_summary_test <- dfm_lookup(tr_dfm_summary_test, mydict, valuetype = 'fixed')
sentiments_summary_test 
sentiments_summary_test <- convert(sentiments_summary_test, to = "data.frame") %>%
  mutate(sent_score_summary = as.factor(ifelse(positive >= negative, 1, 0)))
dim(sentiments_summary_test)

airbnb_test <- cbind(airbnb_test, sent_score_houserules = sentiments_house_rules_test[, 4], sent_score_summary = sentiments_summary_test[,4])


#--------------------------------------------------------External Dataset Below-----------------------------------------------------
airbnb_test <- airbnb_test %>%
  mutate(zipcode = ifelse(nchar(zipcode) == 4, paste0("0", zipcode), zipcode))
airbnb_test$zipcode <- ifelse(nchar(airbnb_test$zipcode) > 5, substr(airbnb_test$zipcode, 1, 5), airbnb_test$zipcode)

# Replace missing zipcodes with mode of zipcodes in the same city
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

airbnb_test_subset <- airbnb_test %>%
  group_by(city) %>%
  mutate(zipcode = ifelse(is.na(zipcode) | nchar(zipcode) < 4, 
                          Mode(zipcode), 
                          zipcode)) %>%
  ungroup() %>%
  group_by(state) %>%
  mutate(zipcode = ifelse(is.na(zipcode) | nchar(zipcode) < 4, 
                          Mode(zipcode), 
                          zipcode)) %>%
  ungroup()

airbnb_test <- merge(airbnb_test_subset, 
                     ext_data %>% select(zip, population, density, dist2_medium_airport), 
                     by.x = "zipcode", 
                     by.y = "zip", 
                     all.x = TRUE)

airbnb_test <- airbnb_test %>%
  mutate(population = ifelse(is.na(population), median(population, na.rm = TRUE), population),
         density = ifelse(is.na(density), median(density, na.rm = TRUE), density),
         dist2_medium_airport = ifelse(is.na(dist2_medium_airport), median(dist2_medium_airport, na.rm = TRUE), dist2_medium_airport))

dim(airbnb_test)   
names(airbnb_test)
names(airbnb_train)

summary(airbnb_train$smart_location)
summary(airbnb_test$smart_location)

extra_categories1 <- setdiff(levels(airbnb_train$market), levels(airbnb_test$market))
extra_categories1

extra_categories2 <- setdiff(levels(airbnb_train$smart_location), levels(airbnb_test$smart_location))
extra_categories2

train_insts <- sample(nrow(airbnb_train), 0.7 * nrow(airbnb_train))
data_train <- airbnb_train[train_insts, ]
train_X <- data_train %>% select(-high_booking_rate)
train_Y <- data_train$high_booking_rate
data_valid <- airbnb_train[-train_insts, ]
valid_X <- data_valid %>% select(-high_booking_rate)
valid_Y <- data_valid %>% select(high_booking_rate)

#--------------------------------------------------------PLOTS-----------------------------------------------------
# Distribution of listing prices
library(viridis)

hist(airbnb_train$price, 
     breaks = 30, 
     col = "skyblue",
     border = "darkblue",
     main = "Distribution of Listing Prices",
     xlab = "Price")

# Distribution of room types
my_palette <- c("#1f77b4", "#ff7f0e", "#2ca02c")

barplot(table(airbnb_train$room_type), col = my_palette, 
        main = "Distribution of Room Types",
        xlab = "Room Type",
        ylab = "Count",
        border = "black")


# Boxplot of price by room type
palette <- c("red", "green", "yellow")
outlier_color <- "gray50"

# Create the boxplot with viridis_lite color palette
boxplot(price ~ room_type, data = airbnb_train, 
        col = palette,
        outcol = outlier_color,
        main = "Price by Room Type",
        xlab = "Room Type",
        ylab = "Price")

# Correlation matrix
library(corrplot)

correlation_matrix <- cor(airbnb_train[, c("price", "accommodates", "bathrooms", "bedrooms", "beds")])

# Plot the correlation matrix with customized colors and numerical values
corrplot(correlation_matrix, method = "color", order = "hclust", 
         tl.cex = 0.7,  cl.cex = 0.7, addCoef.col = "white",)


# Distribution of Property Types
property_freq <- table(airbnb_train$property_type)

# Sort the property types in decreasing order of frequency
sorted_property <- names(sort(property_freq, decreasing = TRUE))

# Get the top 10 property types
top10_property <- head(sorted_property, 10)

# Subset the data to include only the top 10 property types
airbnb_top10 <- airbnb_train[airbnb_train$property_type %in% top10_property, ]

# Plot the distribution of the top 10 property types
barplot(property_freq[top10_property], main = "Top 10 Property Types", 
        xlab = "Property Type", ylab = "Frequency", col = "skyblue", border = "black", las = 2)

ggplot(airbnb_train, aes(x = price, fill = state)) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribution of Property Prices by State",
       x = "Price",
       y = "Density",
       fill = "State") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  scale_fill_discrete(name = "State",
                      labels = c("ny" = "New York",
                                 "ca" = "California",
                                 "tx" = "Texas",
                                 "il" = "Illinois",
                                 "co" = "Colorado",
                                 "dc" = "District of Columbia",
                                 "la" = "Louisiana",
                                 "ma" = "Massachusetts",
                                 "md" = "Maryland",
                                 "or" = "Oregon",
                                 "tn" = "Tennessee",
                                 "wa" = "Washington"))

# Calculate the average price for each property type
avg_price <- tapply(airbnb_train$price, airbnb_train$property_type, mean)

# Sort the property types based on average price
sorted_avg_price <- sort(avg_price, decreasing = TRUE)

# Select the top 10 property types
top_10_avg_price <- sorted_avg_price[1:10]

# Plot the average price for the top 10 property types
my_palette <- viridis_pal()(10)
barplot(top_10_avg_price, main = "Average Price for Top 10 Property Types",
        ylab = "Average Price", col = my_palette, border = "black",
        las = 2, cex.names = 0.7)

response_counts <- table(airbnb_train$host_response_time)

# Calculate the percentages
response_percentages <- prop.table(response_counts) * 100

par(mfrow=c(1,1))

# Create the outer pie chart
pie(response_counts,
    main = "Proportion of Host Response Time",
    col = rainbow(length(response_counts)),
    labels = paste(names(response_counts), ": ", round(response_percentages, 2), "%"), 
    border = NA, 
)

draw.circle <- function(x, y, radius, ...){
  theta <- seq(0, 2*pi, length.out=100)
  x <- x + radius * cos(theta)
  y <- y + radius * sin(theta)
  polygon(x, y, ...)
}

draw.circle(0, 0, 0.6, border = "white", col = "white")


library(RColorBrewer)

# Define a new color palette
custom_palette <- brewer.pal(3, "Set3")

# Count of listings by cancellation policy with custom color palette
barplot(table(airbnb_train$cancellation_policy), 
        main = "Count of Listings by Cancellation Policy", 
        ylab = "Count", 
        col = custom_palette,
        border = "black")

state_avg_price <- airbnb_train %>%
  group_by(state) %>%
  summarise(avg_price = mean(price, na.rm = TRUE))

state_avg_price$state <- factor(state_avg_price$state, levels = c("ny", "ca", "tx", "il", "co", "dc", "la", "ma", "md", "or", "tn", "wa"), labels = c("New York", "California", "Texas", "Illinois", "Colorado", "DC", "Louisiana", "Massachusetts", "Maryland", "Oregon", "Tennessee", "Washington"))

library(viridis)

# Calculate number of states
num_states <- nrow(state_avg_price)

custom_palette <- inferno(num_states)

# Plot
barplot(state_avg_price$avg_price,
        names.arg = levels(state_avg_price$state),
        col = custom_palette,
        main = "Average Listing Price by State",
        ylab = "Average Price",
        border = "black",
        ylim = c(0, max(state_avg_price$avg_price) * 1.1),
        las = 2,
        cex.names = 0.7)

state_avg_month <- airbnb_train %>%
  group_by(state) %>%
  summarise(avg_price = mean(monthly_price, na.rm = TRUE))

state_avg_month$state <- factor(state_avg_month$state, levels = c("ny", "ca", "tx", "il", "co", "dc", "la", "ma", "md", "or", "tn", "wa"), labels = c("New York", "California", "Texas", "Illinois", "Colorado", "DC", "Louisiana", "Massachusetts", "Maryland", "Oregon", "Tennessee", "Washington"))

library(viridis)

# Calculate number of states
num_states <- nrow(state_avg_month)

custom_palette <- cividis(num_states)

# Plot
barplot(state_avg_month$avg_price,
        names.arg = levels(state_avg_month$state),
        col = custom_palette,
        main = "Average Monthly Listing Price by State",
        ylab = "Average Monthly Price",
        border = "black",
        ylim = c(0, max(state_avg_month$avg_price) * 1.1),
        las = 2,
        cex.names = 0.7)

filtered_data <- airbnb_train[!is.na(airbnb_train$cancellation_policy) & !is.na(airbnb_train$price), ]

# Create a violin plot
ggplot(filtered_data, aes(x = cancellation_policy, y = price, fill = cancellation_policy)) +
  geom_violin(trim = FALSE) +
  stat_summary(fun.data = "median_hilow", geom = "errorbar", width = 0.5, size = 1, color = "black") +
  stat_summary(fun.y = median, geom = "point", shape = 23, size = 3, fill = "white", color = "black") +
  labs(title = "Violin Plot of Price by Cancellation Policy", x = NULL, y = "Price") +
  theme_minimal() +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5))

#--------------------------------------------------------RANDOM FOREST-----------------------------------------------------
#------------------THIS IS OUR PREDICTION MODEL----------------------------------------------------------------------------

library(randomForest)
modelformula_rf <- high_booking_rate ~ host_response_time + host_response_rate  + host_listings_count + host_total_listings_count + host_response_time +
  price_per_person + state + market + smart_location + country_code + country + room_type + accommodates + bathrooms + 
  bedrooms + beds + bed_type + price + weekly_price + monthly_price + security_deposit + cleaning_fee + guests_included + extra_people + minimum_nights + 
  maximum_nights + availability_30 + availability_60 + availability_90 + availability_365 + first_review + cancellation_policy + host_acceptanced + 
  availability + market_count + smart_count + major_category + latitude + longitude + price_per_person + has_cleaning_fee + has_security_deposit + host_listings + bed_bath_ratio + 
  require_guest_profile_picture + requires_license + require_guest_phone_verification + host_is_superhost + instant_bookable +
  host_identity_verified + is_location_exact + host_has_profile_pic + sent_score_houserules + sent_score_summary + min_booking_fee + population + density + dist2_medium_airport



rf.mod <- randomForest(modelformula_rf, 
                         data = data_train, mtry = 6, ntree = 2000, importance=TRUE)


rf_preds <- predict(rf.mod, newdata=data_valid)
rf_preds

rf_probs <- predict(rf.mod, newdata = data_valid, type = "prob")[,2]
rf_probs

summary(data_valid$high_booking_rate)

rf_acc <- mean(ifelse(rf_preds==data_valid$high_booking_rate,1,0))

rf.mod
rf_acc

importance(rf.mod)
varImpPlot(rf.mod)

pred_full <- prediction(rf_probs, data_valid$high_booking_rate)

##measures the TP, FP, TN, FN, and more for each cutoff
pred_full@cutoffs
pred_full@tp
pred_full@n.pos.pred

# Plot ROC curve
roc_full <- performance(pred_full, "tpr", "fpr")

# 3. plot the (tpr, fpr) performance - you can specify the color and line weight as well
plot(roc_full, col = "red", lwd = 2)

# Calculate AUC
cat("AUC:", performance(pred_full, measure = "auc")@y.values[[1]], "\n")

probs_rate <- predict(rf.mod, newdata = airbnb_test, type = "prob")[,2]
probs_rate
write.table(probs_rate, "high_booking_rate_group15.csv", row.names = FALSE)

#--------------------------------------------------------Boosting-----------------------------------------------------

boost_modelformula <- high_booking_rate ~ host_response_rate  + host_listings_count + host_total_listings_count +
  price_per_person + state + market + smart_location + room_type + accommodates + bathrooms + 
  bedrooms + beds + bed_type + price + weekly_price + monthly_price + security_deposit + cleaning_fee + guests_included + extra_people + minimum_nights + 
  maximum_nights + availability_30 + availability_60 + availability_90 + availability_365  + host_acceptanced + 
  availability + market_count + smart_count + major_category + latitude + longitude + price_per_person + has_cleaning_fee + has_security_deposit + host_listings + bed_bath_ratio + 
  require_guest_profile_picture + requires_license + require_guest_phone_verification + host_is_superhost + instant_bookable +
  host_identity_verified + is_location_exact + host_has_profile_pic + sent_score_houserules + sent_score_summary + min_booking_fee + population + density + dist2_medium_airport

boost_data <- airbnb_train

boost_data$high_booking_rate <- ifelse(boost_data$high_booking_rate =="YES",1,0)
boost_train <- boost_data[train_insts,]
boost_valid <- boost_data[-train_insts,]
dim(boost_train)
library(gbm)
boost.mod <- gbm(boost_modelformula, data=boost_train,
                 distribution="bernoulli",
                 n.trees=1000,
                 interaction.depth=3)

boost_preds <- predict(boost.mod,
                       newdata=boost_valid,
                       type='response',
                       n.trees=500)


boost_class <- ifelse(boost_preds>0.5, 1, 0)
boost_acc <- mean(ifelse(boost_class==boost_valid$high_booking_rate,1,0))
boost_acc

summary(boost.mod)

# Plot ROC curve
roc_curve_boost <- roc(boost_valid$high_booking_rate, boost_preds)

# Plot ROC curve
plot(roc_curve_boost, main = "ROC Curve", col = "blue")

# Calculate AUC
auc_value_boost <- auc(roc_curve_boost)
cat(paste0("AUC: ", round(auc_value_boost, 3), "\n"))





#--------------------------------------bagging-------------------------------------

library(randomForest)
modelformula_bag <- high_booking_rate ~ host_response_time + host_response_rate  + host_listings_count + host_total_listings_count + host_response_time +
  price_per_person + state + market + smart_location + room_type + accommodates + bathrooms + 
  bedrooms + beds + bed_type + price + weekly_price + monthly_price + security_deposit + cleaning_fee + guests_included + extra_people + minimum_nights + 
  maximum_nights + availability_30 + availability_60 + availability_90 + availability_365 + first_review + cancellation_policy + host_acceptanced + 
  availability + market_count + smart_count + major_category + latitude + longitude + price_per_person + has_cleaning_fee + has_security_deposit + host_listings + bed_bath_ratio + 
  require_guest_profile_picture + requires_license + require_guest_phone_verification + host_is_superhost + instant_bookable +
  host_identity_verified + is_location_exact + host_has_profile_pic + sent_score_houserules + sent_score_summary + min_booking_fee


bag.mod <- randomForest(modelformula_bag, 
                       data = data_train, mtry = 51 , ntree=1000, importance=TRUE)


bag_preds <- predict(bag.mod, newdata=data_valid)
bag_preds

bag_probs <- predict(bag.mod, newdata = data_valid, type = "prob")[,2]
bag_probs

summary(data_valid$high_booking_rate)

bag_acc <- mean(ifelse(bag_preds==data_valid$high_booking_rate,1,0))

bag.mod
bag_acc

importance(bag.mod)
varImpPlot(bag.mod)

pred_full_bag <- prediction(bag_probs, data_valid$high_booking_rate)

##measures the TP, FP, TN, FN, and more for each cutoff
pred_full_bag@cutoffs
pred_full_bag@tp
pred_full_bag@n.pos.pred

# Plot ROC curve
roc_full_bag <- performance(pred_full_bag, "tpr", "fpr")

# 3. plot the (tpr, fpr) performance - you can specify the color and line weight as well
plot(roc_full_bag, col = "red", lwd = 2)

# Calculate AUC
cat("AUC:", performance(pred_full_bag, measure = "auc")@y.values[[1]], "\n")


#--------------------------------------Logistic-------------------------------------
modelformula_log <- high_booking_rate ~ accommodates + bathrooms + bedrooms + beds + price  + guests_included + extra_people + minimum_nights + maximum_nights + availability_30 + availability_60 + availability_90 + availability_365 + room_type  + cancellation_policy + bed_type + weekly_price + monthly_price

# Define k for k-fold cross-validation
k <- 10

# Perform k-fold cross-validation
fold_indices <- sample(1:k, nrow(airbnb_train), replace = TRUE)

# Initialize vector to store accuracies
accuracies <- numeric(k)
levels(airbnb_train$high_booking_rate)

for (i in 1:k) {
  # Split data into training and validation sets
  data_train <- airbnb_train[fold_indices != i, ]
  data_valid <- airbnb_train[fold_indices == i, ]
  
  # Fit logistic regression model
  model <- glm(modelformula_log, data = data_train, family = "binomial")
  
  # Predict probabilities on validation set
  predictions <- predict(model, newdata = data_valid, type = "response")
  
  # Classify predictions
  classified_predictions <- ifelse(predictions > 0.5, "YES", "NO")
  
  # Calculate accuracy
  accuracy <- mean(classified_predictions == data_valid$high_booking_rate)
  
  # Store accuracy
  accuracies[i] <- accuracy
}
# Calculate average accuracy
average_accuracy <- mean(accuracies)
cat(paste0("Average ", k, "-fold cross-validated accuracy: ", round(average_accuracy * 100, 2), "%\n"))

# Fit logistic regression model on the entire training data
final_model <- glm(modelformula_log, data = data_train, family = "binomial")

# Predict probabilities on validation set
predictions <- predict(final_model, newdata = data_valid, type = "response")

# Create ROC curve
roc_curve <- roc(data_valid$high_booking_rate, predictions)

# Plot ROC curve
plot(roc_curve, main = "ROC Curve", col = "blue")

# Calculate AUC
auc_value <- auc(roc_curve)



cat(paste0("AUC: ", round(auc_value, 3), "\n"))


#--------------------------------------Ridge-------------------------------------
library(glmnet)
ridge_data <- airbnb_train

# Convert target variable to binary
ridge_data$high_booking_rate <- ifelse(ridge_data$high_booking_rate == "YES", 1, 0)

# Define formula
ridge_formula <- as.formula("high_booking_rate ~ host_response_rate + host_listings_count + host_total_listings_count +
                            price_per_person + state + market + smart_location + room_type + accommodates + bathrooms + 
                            bedrooms + beds + bed_type + price + weekly_price + monthly_price + security_deposit + cleaning_fee + 
                            guests_included + extra_people + minimum_nights + maximum_nights + availability_30 + 
                            availability_60 + availability_90 + availability_365 + host_acceptanced + 
                            availability + market_count + smart_count + major_category + latitude + longitude + 
                            price_per_person + has_cleaning_fee + has_security_deposit + host_listings + 
                            bed_bath_ratio + require_guest_profile_picture + requires_license + 
                            require_guest_phone_verification + host_is_superhost + instant_bookable +
                            host_identity_verified + is_location_exact + host_has_profile_pic + 
                            sent_score_houserules + sent_score_summary + min_booking_fee + population + density + dist2_medium_airport")

# Prepare predictors matrix and response vector
predictors <- model.matrix(ridge_formula, data = ridge_data)
response_vector <- ridge_data$high_booking_rate

# Define k for k-fold cross-validation
k <- 10

# Perform k-fold cross-validation
fold_indices <- sample(1:k, nrow(ridge_data), replace = TRUE)

# Initialize vector to store accuracies
accuracies_ridge <- numeric(k)

for (i in 1:k) {
  # Split data into training and validation sets
  data_train <- predictors[fold_indices != i, ]
  data_valid <- predictors[fold_indices == i, ]
  response_train <- response_vector[fold_indices != i]
  response_valid <- response_vector[fold_indices == i]
  
  # Fit logistic regression model
  model_ridge <- glmnet(data_train, response_train, alpha = 0, family = "binomial")
  
  # Predict probabilities on validation set
  predictions_ridge <- predict(model_ridge, newx = data_valid, type = "response")
  
  # Classify predictions
  classified_predictions_ridge <- ifelse(predictions_ridge > 0.5, 1, 0)
  
  # Calculate accuracy
  accuracy_ridge <- mean(classified_predictions_ridge == response_valid)
  
  # Store accuracy
  accuracies_ridge[i] <- accuracy_ridge
}

# Calculate average accuracy
average_accuracy_ridge <- mean(accuracies_ridge)
cat(paste0("Average ", k, "-fold cross-validated accuracy: ", round(average_accuracy_ridge * 100, 2), "%\n"))

# Fit logistic regression model on the entire training data
final_model_ridge <- glmnet(predictors, response_vector, alpha = 0, family = "binomial")
predictions_train_ridge <- predict(final_model_ridge, newx = predictors, type = "response")
predictions_train_ridge <- predictions_train_ridge[,88]
# Calculate AUC
library(pROC)
roc_obj <- roc(response_vector, predictions_train_ridge)
auc_value <- auc(roc_obj)
cat(paste0("AUC on entire training set: ", round(auc_value, 4), "\n"))


#--------------------------------------Lasso-------------------------------------
library(glmnet)

# Assuming lasso_data is defined similarly to ridge_data in your previous code
lasso_data <- airbnb_train

# Convert target variable to binary
lasso_data$high_booking_rate <- ifelse(lasso_data$high_booking_rate == "YES", 1, 0)

# Define formula
lasso_formula <- as.formula("high_booking_rate ~ host_response_rate + host_listings_count + host_total_listings_count +
                            price_per_person + state + market + smart_location + room_type + accommodates + bathrooms + 
                            bedrooms + beds + bed_type + price + weekly_price + monthly_price + security_deposit + cleaning_fee + 
                            guests_included + extra_people + minimum_nights + maximum_nights + availability_30 + 
                            availability_60 + availability_90 + availability_365 + host_acceptanced + 
                            availability + market_count + smart_count + major_category + latitude + longitude + 
                            price_per_person + has_cleaning_fee + has_security_deposit + host_listings + 
                            bed_bath_ratio + require_guest_profile_picture + requires_license + 
                            require_guest_phone_verification + host_is_superhost + instant_bookable +
                            host_identity_verified + is_location_exact + host_has_profile_pic + 
                            sent_score_houserules + sent_score_summary + min_booking_fee")

# Prepare predictors matrix and response vector
predictors_lasso <- model.matrix(lasso_formula, data = lasso_data)
response_vector_lasso <- lasso_data$high_booking_rate

# Define k for k-fold cross-validation
k <- 10

# Perform k-fold cross-validation
fold_indices <- sample(1:k, nrow(lasso_data), replace = TRUE)

# Initialize vector to store accuracies
accuracies_lasso <- numeric(k)

for (i in 1:k) {
  # Split data into training and validation sets
  data_train_lasso <- predictors_lasso[fold_indices != i, ]
  data_valid_lasso <- predictors_lasso[fold_indices == i, ]
  response_train_lasso <- response_vector_lasso[fold_indices != i]
  response_valid_lasso <- response_vector_lasso[fold_indices == i]
  
  # Fit logistic regression model with Lasso penalty
  model_lasso <- glmnet(data_train_lasso, response_train_lasso, alpha = 1, family = "binomial")
  
  # Predict probabilities on validation set
  predictions_lasso <- predict(model_lasso, newx = data_valid_lasso, type = "response")
  
  # Classify predictions
  classified_predictions_lasso <- ifelse(predictions_lasso > 0.5, 1, 0)
  
  # Calculate accuracy
  accuracy_lasso <- mean(classified_predictions_lasso == response_valid_lasso)
  
  # Store accuracy
  accuracies_lasso[i] <- accuracy_lasso
}

# Calculate average accuracy
average_accuracy_lasso <- mean(accuracies_lasso)
cat(paste0("Average ", k, "-fold cross-validated accuracy: ", round(average_accuracy_lasso * 100, 2), "%\n"))

# Fit logistic regression model on the entire training data with Lasso penalty
final_model_lasso <- glmnet(predictors_lasso, response_vector_lasso, alpha = 1, family = "binomial")

predictions_train_lasso <- predict(final_model_lasso, newx = predictors_lasso, type = "response")
predictions_train_lasso <- predictions_train_lasso[,52]
# Calculate AUC
roc_obj_lasso <- roc(response_vector_lasso, predictions_train_lasso)
auc_value_lasso <- auc(roc_obj_lasso)
cat(paste0("AUC on entire training set: ", round(auc_value_lasso, 4), "\n"))



#--------------------------------------learning curves------------------------------
#--------------------------------------boosting-------------------------------------
training_sizes <- seq(2000, 60000, by = 5000)

# Initialize vector to store validation accuracies
accuracies <- numeric(length(training_sizes))

# Train models with different training sizes and compute accuracy
for (i in seq_along(training_sizes)) {
  # Subset the training set based on the current training size
  subset_indices <- sample(1:nrow(boost_train), size = training_sizes[i])
  boost_train_subset <- boost_train[subset_indices, ]
  
  # Train GBM model
  boost.mod <- gbm(boost_modelformula, data = boost_train_subset,
                   distribution = "bernoulli",
                   n.trees = 1000,
                   interaction.depth = 3)
  
  # Predict on validation set
  boost_preds <- predict(boost.mod, newdata = boost_valid, type = 'response', n.trees = 500)
  
  # Convert predicted probabilities to class predictions
  boost_class <- ifelse(boost_preds > 0.5, 1, 0)
  
  # Compute accuracy
  accuracies[i] <- mean(boost_class == boost_valid$high_booking_rate)
}

# Plot learning curve
plot(training_sizes, accuracies,  type = "l", col = "red", lwd = 2,
     xlab = "Number of Training Instances", ylab = "Validation Accuracy",
     main = "Learning Curve for Boosting")
points(training_sizes, accuracies, col = "red", pch = 19)

#--------------------------------------ridge-------------------------------------
compute_accuracy <- function(predictors_train, response_train, predictors_valid, response_valid) {
  model_ridge <- glmnet(predictors_train, response_train, alpha = 0, family = "binomial")
  predictions_valid <- predict(model_ridge, newx = predictors_valid, type = "response")
  classified_predictions <- ifelse(predictions_valid > 0.5, 1, 0)
  accuracy <- mean(classified_predictions == response_valid)
  return(accuracy)
}

# Define training sizes
training_sizes <- seq(2000, 60000, by = 5000)

# Initialize vector to store accuracies
accuracies <- numeric(length(training_sizes))

# Hold out validation set
validation_indices <- sample(1:nrow(predictors), size = 1000, replace = FALSE)
predictors_valid <- predictors[validation_indices, ]
response_valid <- response_vector[validation_indices]

for (i in seq_along(training_sizes)) {
  # Select training instances
  train_indices <- sample(setdiff(1:nrow(predictors), validation_indices), size = training_sizes[i])
  predictors_train <- predictors[train_indices, ]
  response_train <- response_vector[train_indices]
  
  # Compute accuracy
  accuracies[i] <- compute_accuracy(predictors_train, response_train, predictors_valid, response_valid)
}

# Plot learning curve
plot(training_sizes, accuracies, type = "b", xlab = "Number of Training Instances", ylab = "Validation Accuracy", main = "Learning Curve")

#--------------------------------------lasso-------------------------------------
training_instance_sizes <- seq(2000, 60000, by = 5000)
validation_accuracy <- numeric(length(training_instance_sizes))

predictors_lasso <- model.matrix(lasso_formula, data = lasso_data)
response_vector_lasso <- lasso_data$high_booking_rate

validation_indices <- sample(1:nrow(lasso_data), size = 200) 

for (i in seq_along(training_instance_sizes)) {
  training_indices <- sample(setdiff(1:nrow(lasso_data), validation_indices), size = training_instance_sizes[i])
  
  predictors_train <- predictors_lasso[training_indices, ]
  response_train <- response_vector_lasso[training_indices]
  
  model_lasso <- glmnet(predictors_train, response_train, alpha = 1, family = "binomial")
  
  predictions_lasso <- predict(model_lasso, newx = predictors_lasso[validation_indices, ], type = "response")
  
  classified_predictions_lasso <- ifelse(predictions_lasso > 0.5, 1, 0)
  
  validation_accuracy[i] <- mean(classified_predictions_lasso == response_vector_lasso[validation_indices])
}

# Plot learning curve
plot(training_instance_sizes, validation_accuracy, type = "l", col = "red", lwd = 2,
     xlab = "Number of Training Instances", ylab = "Validation Accuracy",
     main = "Learning Curve for Lasso")
points(training_instance_sizes, validation_accuracy, col = "red", pch = 19)

#--------------------------------------logistic-------------------------------------
training_instances <- seq(2000, 60000, by = 10000)

# Initialize vector to store validation accuracies
validation_accuracies <- numeric(length(training_instances))

validation_indices <- sample(1:nrow(airbnb_train), size = 5000)
validation_set <- airbnb_train[validation_indices, ]
remaining_data <- airbnb_train[-validation_indices, ]

for (i in seq_along(training_instances)) {
  # Select training instances
  training_indices <- sample(1:nrow(remaining_data), size = training_instances[i])
  training_set <- remaining_data[training_indices, ]
  
  # Fit logistic regression model
  model <- glm(modelformula_log, data = training_set, family = "binomial")
  
  # Predict probabilities on validation set
  predictions <- predict(model, newdata = validation_set, type = "response")
  
  # Classify predictions
  classified_predictions <- ifelse(predictions > 0.5, "YES", "NO")
  
  # Calculate accuracy
  accuracy <- mean(classified_predictions == validation_set$high_booking_rate)
  
  # Store accuracy
  validation_accuracies[i] <- accuracy
}

# Plot learning curve
plot(training_instances, validation_accuracies, type = "l", col = "red", lwd = 2,
     xlab = "Number of Training Instances", ylab = "Validation Accuracy",
     main = "Learning Curve for Logistic")
points(training_instances, validation_accuracies, col = "red", pch = 19)

#--------------------------------------bagging-------------------------------------
training_instance_sizes <- seq(2000, 60000, by = 5000)

accuracies <- numeric(length(training_instance_sizes))

for (i in seq_along(training_instance_sizes)) {
  training_data_subset <- data_train[1:training_instance_sizes[i], ]
  
  bag.mod <- randomForest(modelformula_bag, 
                          data = training_data_subset, mtry = 51 , ntree=1000, importance=TRUE)
  bag_preds <- predict(bag.mod, newdata=data_valid)
  accuracy <- mean(bag_preds == data_valid$high_booking_rate)
  accuracies[i] <- accuracy
}

plot(training_instance_sizes, accuracies, type = "l", col = "red", lwd = 2,
     xlab = "Number of Training Instances", ylab = "Validation Accuracy",
     main = "Learning Curve")
points(training_instance_sizes, accuracies, col = "red", pch = 19)

#--------------------------------------random forest-------------------------------------

training_instances <- seq(2000, 60000, by = 5000)
validation_accuracy <- numeric(length(training_instances))

# Choose a held-out validation set
validation_set <- data_valid

# Loop over different numbers of training instances
for (i in seq_along(training_instances)) {
  # Sample training instances
  train_insts <- sample(nrow(data_train), training_instances[i])
  train_data <- data_train[train_insts, ]
  
  # Train random forest model
  rf_mod <- randomForest(modelformula_rf, data = train_data, mtry = 6, ntree = 2000)
  
  # Predict on validation set
  rf_preds <- predict(rf_mod, newdata = validation_set)
  
  # Compute accuracy
  validation_accuracy[i] <- mean(rf_preds == validation_set$high_booking_rate)
}

# Plot learning curve
plot(training_instances, validation_accuracy, type = "b",
     xlab = "Number of Training Instances", ylab = "Validation Accuracy",
     main = "Learning Curve")







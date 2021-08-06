# Load Package dependencies and set enviroments
library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(ggplot2)
library(Cairo)

CairoWin()

# Read in unclassified Build
df <- read_csv("C:/Users/Pluto/Documents/NTS_C/unclassified builds/ub_tfn.csv")

# Select necessary columns to reproduce plot
df <- df %>%
  select(IndividualID, HouseholdID, TripPurpFrom_B01ID, TripPurpTo_B01ID, MainMode_B04ID, SurveyYear, W1, W2, W5, W5xHH)

# Filter for same number of years as report and select the diary sample
df <- df %>% 
  filter(between(SurveyYear, 2002, 2012), 
         W1 == 1)

# Sanity check: Report claims aprox 210,000 records of individuals
n_distinct(df$IndividualID) # 209,873

# Classifications based on Ian Williams Trip Rates report - rectified to match ntem
# i.e. in IW report - purposes 4 & 5 are in reverse order, purposes 7 & 8 are in reverse order
# Table below rectifies the classification

tibble(No = c(1, "",
              2, "",
              3, "",
              4, "", "",
              5, "",
              6, "", "", "", "", "", "",
              7, "",
              8, ""),
       NTEM_Purpose = c("Commute", "",
                        "Employer Business", "",
                        "Education", "",
                        "Personal Business", "", "", 
                        "Shopping", "",
                        "Recreation", rep("", 6),
                        "Visit Friend", "",
                        "Holiday", ""),
       Composition = c("Work", 
                       "Escort to Work",
                       "In course of work", 
                       "Escort in course of work",
                       "Education", 
                       "Escort Education",
                       "PB Medical",
                       "PB eat/drink",
                       "PB Other",
                       "Shopping",
                       "Escort PB/Shopping",
                       "Eat/drink W friends",
                       "Other Social",
                       "Entertainment",
                       "Sport", 
                       "Day trip (just walk trips)",
                       "Other non-escort", 
                       "Other escort",
                       "Visit friends",
                       "Escort Home",
                       "Holiday",
                       "Day Trip (Excluding Just Walk Trips)")) %>% 
  print(n = 22)


# Classify hb trip purposes for outbound only based on above:

df <- df %>%
  lu_trip_origin() %>%
  lu_hb_purpose() %>%
  lu_nhb_purpose() %>% 
  lu_nhb_purpose_hb_leg() %>%
  mutate(trip_purpose = ifelse(trip_origin == "hb", hb_purpose, nhb_purpose))

df <- df %>%
  lu_sw_weight()

# Calculate trip rate using Ian Williams Methodology
# Multiply trip weight by Household weight and sum over the week by trip purpose for all individuals
df2 <- df %>% 
  mutate(weight = W5xHH * W5 * W2 * sw_weight) %>% 
  group_by(IndividualID, trip_purpose, SurveyYear, W2) %>% 
  summarise(weekly_weight = sum(weight)) %>% 
  ungroup()

# Every individual must have a trip record for every purpose
df2 <- df2 %>% 
  complete(nesting(IndividualID, SurveyYear, W2),
           trip_purpose = 1:8,
           fill = list(weekly_weight = 0))

# Replicate the plot - Aggregate by trip purpose and survey year
# Calculate trip rate as the average number of trips by purpose and year by
# summing up the total weighted trips and dividing by the sum of the total household weight

df2 <- df2 %>% 
  group_by(trip_purpose, SurveyYear) %>% 
  summarise(trip_rate = sum(weekly_weight)/sum(W2)) %>% 
  ungroup()

# Prepare dataframe for plotting

df2 <- df2 %>% 
  mutate(trip_purpose = case_when(
    trip_purpose == 1 ~ "Commute",
    trip_purpose == 2 ~ "Employer's Business",
    trip_purpose == 3 ~ "Education",
    trip_purpose == 4 ~ "Shopping",
    trip_purpose == 5 ~ "Personal Business",
    trip_purpose == 6 ~ "Recreational",
    trip_purpose == 7 ~ "Visit Friends",
    trip_purpose == 8 ~ "Holiday/Day trip (Excl Walk)"
  )) %>% 
  mutate(SurveyYear = factor(SurveyYear),
         trip_purpose = factor(trip_purpose, levels = c("Commute", 
                                                        "Employer's Business", 
                                                        "Education", 
                                                        "Shopping", 
                                                        "Personal Business",
                                                        "Recreational",
                                                        "Visit Friends",
                                                        "Holiday/Day trip (Excl Walk)")))

df2 %>% 
  ggplot(aes(x = SurveyYear, 
             y = trip_rate,
             group = trip_purpose,
             colour = trip_purpose)) +
  geom_line() +
  geom_point() +
  ylim(0, 2.5) +
  ylab("Trip Rate") +
  xlab("Year") +
  scale_color_manual(values = c("darkblue","pink","purple","darkred","darkorange","lightblue","green","grey")) +
  theme(panel.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linetype = "solid" ,colour = "grey50"),
        panel.grid.minor.y = element_blank())
  ggsave("C:/Users/Pluto/Documents/test.png", type = 'cairo', units = 'cm', width = 25, height = 15)
  
########################################################################################
  
df <- read_csv("C:/Users/Pluto/Documents/NTS_C/unclassified builds/ub_tfn.csv")

df_test <- df %>%
  filter(W1 == 1,
         SurveyYear %in% 2002:2012,
         !is.na(TripPurpTo_B01ID)) %>%
  lu_trip_origin() %>%
  lu_hb_purpose() %>%
  lu_nhb_purpose() %>% 
  lu_nhb_purpose_hb_leg() %>%
  lu_sw_weight %>%
  lu_age_work_status() %>% 
  mutate(trip_purpose = ifelse(trip_origin == "hb", hb_purpose, nhb_purpose)) %>%
  filter(trip_purpose %in% 1:8)


    
df2 <- df %>%
  filter(W1 == 1,
         SurveyYear %in% 2002:2012,
         !is.na(TripPurpTo_B01ID)) %>%
  lu_trip_origin() %>%
  lu_hb_purpose() %>%
  lu_nhb_purpose() %>% 
  lu_nhb_purpose_hb_leg() %>%
  lu_sw_weight %>%
  lu_age_work_status() %>%
  mutate(trip_purpose = ifelse(trip_origin == "hb", hb_purpose, nhb_purpose)) %>%
  filter(trip_purpose %in% 1:8)

df3 <- df2 %>% 
  mutate(weight = W5 * W2 * sw_weight) %>% 
  group_by(IndividualID, trip_purpose, SurveyYear, W2) %>% 
  summarise(weekly_weight = sum(weight)) %>% 
  ungroup()


df2_test <- df2 %>%
  mutate(weight = W5 * W2 * sw_weight) %>% 
  group_by(IndividualID, trip_purpose, age_work_status) %>% 
  summarise(weekly_weight = sum(weight)) %>% 
  ungroup() %>%
  complete(nesting(IndividualID, age_work_status),
           trip_purpose = 1:8,
           fill = list(weekly_weight = 0))



df_test <- df %>%
  mutate(age_work_status = case_when(
    Age_B01ID %in% 1:5 ~ "Child",
    Age_B01ID %in% 6:16 & EcoStat_B01ID %in% c(1,3) ~ "FTE",
    Age_B01ID %in% 6:16 & EcoStat_B01ID %in% c(2,4) ~ "PTE",
    Age_B01ID %in% 6:16 & EcoStat_B01ID == 7 ~ "STU",
    Age_B01ID %in% 6:16 & EcoStat_B01ID %in% c(5,6,8,9,10,11) ~ "UNM",
    Age_B01ID %in% 17:21 ~ "65+"
  )) %>%
  lu_trip_origin() %>%
  lu_hb_purpose() %>%
  lu_nhb_purpose() %>%
  mutate(trip_purpose = ifelse(trip_origin == "hb", hb_purpose, nhb_purpose)) %>%
  filter(trip_purpose %in% 1:8) %>%
  filter(W1 == 1,
         SurveyYear %in% 2002:2012,
         !is.na(TripPurpTo_B01ID)) 

df_test %>%
  mutate(weight = W5 * W2) %>% 
  group_by(IndividualID, trip_purpose, age_work_status) %>% 
  summarise(weekly_weight = sum(weight)) %>% 
  ungroup() %>%
  #complete(nesting(IndividualID, age_work_status),
  #         trip_purpose = 1:8,
  #         fill = list(weekly_weight = 0)) %>%
  group_by(age_work_status) %>%
  count() %>%
  ungroup() %>%
  mutate(prop = n/sum(n) * 100)
  
  
 

#####

  
df2_test %>%
  group_by(age_work_status) %>%
  count() %>%
  ungroup() %>%
  mutate(prop = n/sum(n) * 100)

# Every individual must have a trip record for every purpose
df4 <- df3 %>% 
  complete(nesting(IndividualID, SurveyYear, W2),
           trip_purpose = 1:8,
           fill = list(weekly_weight = 0))
  

df5 <- df4 %>%
  group_by(trip_purpose, SurveyYear) %>% 
  summarise(trip_rate = sum(weekly_weight)/sum(W2)) %>% 
  ungroup()

df5 <- df5 %>% 
  mutate(trip_purpose = case_when(
    trip_purpose == 1 ~ "Commute",
    trip_purpose == 2 ~ "Employer's Business",
    trip_purpose == 3 ~ "Education",
    trip_purpose == 4 ~ "Shopping",
    trip_purpose == 5 ~ "Personal Business",
    trip_purpose == 6 ~ "Recreational",
    trip_purpose == 7 ~ "Visit Friends",
    trip_purpose == 8 ~ "Holiday/Day trip (Excl Walk)"
  )) %>% 
  mutate(SurveyYear = factor(SurveyYear),
         trip_purpose = factor(trip_purpose, levels = c("Commute", 
                                                        "Employer's Business", 
                                                        "Education", 
                                                        "Shopping", 
                                                        "Personal Business",
                                                        "Recreational",
                                                        "Visit Friends",
                                                        "Holiday/Day trip (Excl Walk)")))


df5 %>% 
  ggplot(aes(x = SurveyYear, 
             y = trip_rate,
             group = trip_purpose,
             colour = trip_purpose)) +
  geom_line() +
  geom_point() +
  ylab("Trip Rate") +
  xlab("Year") +
  ylim(0, 2.5)+
  scale_color_manual(values = c("darkblue","pink","purple","darkred","darkorange","lightblue","green","grey")) +
  theme(panel.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linetype = "solid" ,colour = "grey50"),
        panel.grid.minor.y = element_blank())


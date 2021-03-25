# Load Package dependencies and set enviroments
library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(ggplot2)
library(Cairo)

CairoWin()

# Read in unclassified Build
df <- read_csv("Y:/NTS/import/tfn_unclassified_build19.csv")

# Select necessary columns to reproduce plot
df <- df %>%
  select(IndividualID, TripPurpFrom_B01ID, TripPurpTo_B01ID, MainMode_B04ID, SurveyYear, W1, W2, W5, W5xHH)

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
  filter(TripPurpFrom_B01ID == 23) %>% # Trips from Home only
  mutate(trip_purpose = case_when(
    TripPurpTo_B01ID == 1 ~ 1,
    TripPurpTo_B01ID == 2 ~ 2,
    TripPurpTo_B01ID == 3 ~ 3,
    TripPurpTo_B01ID == 4 ~ 4, 
    TripPurpTo_B01ID == 5 ~ 4, 
    TripPurpTo_B01ID == 6 ~ 5,
    TripPurpTo_B01ID == 7 ~ 5, 
    TripPurpTo_B01ID == 8 ~ 5, 
    TripPurpTo_B01ID == 9 ~ 6,
    TripPurpTo_B01ID == 10 ~ 7, 
    TripPurpTo_B01ID == 11 ~ 6, 
    TripPurpTo_B01ID == 12 ~ 6,
    TripPurpTo_B01ID == 13 ~ 6, 
    TripPurpTo_B01ID == 14 ~ 8, 
    TripPurpTo_B01ID == 15 & MainMode_B04ID == 1 ~ 1, 
    TripPurpTo_B01ID == 15 & MainMode_B04ID != 1 ~ 8,
    TripPurpTo_B01ID == 16 ~ 6, 
    TripPurpTo_B01ID == 17 ~ 7, 
    TripPurpTo_B01ID == 18 ~ 2,
    TripPurpTo_B01ID == 19 ~ 2, 
    TripPurpTo_B01ID == 20 ~ 3, 
    TripPurpTo_B01ID == 21 ~ 4,
    TripPurpTo_B01ID == 22 ~ 1))

# Calculate trip rate using Ian Williams Methodology
# Multiply trip weight by Household weight and sum over the week by trip purpose for all individuals
df2 <- df %>% 
  mutate(weight = W5xHH * W2) %>% 
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
  expand_limits(y = c(0,2.5)) +
  scale_y_continuous(expand = c(0,0)) +
  ylab("Trip Rate") +
  xlab("Year") +
  scale_color_manual(values = c("darkblue","pink","purple","darkred","darkorange","lightblue","green","grey")) +
  theme(panel.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linetype = "solid" ,colour = "grey50"),
        panel.grid.minor.y = element_blank())
  ggsave("C:/Users/Pluto/Documents/test.png", type = 'cairo', units = 'cm', width = 25, height = 15)
  

  

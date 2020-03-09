library("tidyverse")
library("survey")
library("sjstats")
library("MASS")
library("compare")

# Import weighted weekly trip rates
week_weighted_trips <- read_csv("Y:/NTS/weekly_trip_rates.csv")

# Import unclassified build
unclassified_build <- read_csv("Y:/NTS/tfn_unclassified_build.csv")

# Recode gender Male or Female
unclassified_build <- unclassified_build %>%
  mutate(Sex_B01ID = case_when(
    Sex_B01ID == 1 ~ 'Male',
    Sex_B01ID == 2 ~ 'Female',
    TRUE ~ as.character(Sex_B01ID)
  ))

# Combine Age and work status to Age_workstatus
unclassified_build %>%
  mutate(Age_Workstatus = case_when(
    Age_B01ID %in% c(1,2,3,4,5) & EcoStat_B01ID == 7 ~ 'under 16_child')) %>% select(Age_Workstatus) %>% distinct()

unclassified_build %>% filter(Age_B01ID %in% c(1,2,3,4,5))

# Main loop around regression

week_weighted_trips$hb_purpose %>% factor()

purposes <- c(1,2,3,4,5,6,7,8,99)

regression_function <- function(x){
  
  
  
}

glm.nb(tfn_trip_rate ~ hb_purpose, data = df1, subset = train_ind) %>% summary()
svyglm.nb(tfn_trip_rate ~ hb_purpose, design = des)





svyglm.nb(tfn_trip_rate ~ household , des) %>% summary()

stepAIC(svyglm.nb(tfn_trip_rate ~ household , des), direction = "forward")

glm.nb(tfn_trip_rate ~ household, data = df1) %>% summary()

# Recode Age
unclassified_build <- unclassified_build %>%
  mutate(Age_B01ID = case_when(
    Age_B01ID == 1 ~ 'under 16',
    Age_B01ID == 2 ~ 'under 16',
    Age_B01ID == 3 ~ 'under 16',
    Age_B01ID == 4 ~ 'under 16',
    Age_B01ID == 5 ~ 'under 16',
    Age_B01ID == 6 ~ '16-74',
    Age_B01ID == 7 ~ '16-74',
    Age_B01ID == 8 ~ '16-74',
    Age_B01ID == 9 ~ '16-74', 
    Age_B01ID == 10 ~ '16-74', 
    Age_B01ID == 11 ~ '16-74',
    Age_B01ID == 12 ~ '16-74',
    Age_B01ID == 13 ~ '16-74',
    Age_B01ID == 14 ~ '16-74', 
    Age_B01ID == 15 ~ '16-74', 
    Age_B01ID == 16 ~ '16-74', 
    Age_B01ID == 17 ~ '16-74', 
    Age_B01ID == 18 ~ '16-74',
    Age_B01ID == 19 ~ '75 or over',
    Age_B01ID == 20 ~ '75 or over',
    Age_B01ID == 21 ~ '75 or over',
    TRUE ~ as.character(Age_B01ID)
  ))

# We convert EcoStat_B01ID to NTEM employment types
unclassified_build <- unclassified_build %>%
  mutate(EcoStat_B01ID = case_when(
    EcoStat_B01ID == 1 ~ 'fte',
    EcoStat_B01ID == 2 ~ 'pte',
    EcoStat_B01ID == 3 ~ 'fte', 
    EcoStat_B01ID == 4 ~ 'pte',
    EcoStat_B01ID == 5 ~ 'unm',
    EcoStat_B01ID == 6 ~ 'unm',
    EcoStat_B01ID == 7 ~ 'stu',
    EcoStat_B01ID == 8 ~ 'unm',
    EcoStat_B01ID == 9 ~ 'unm',
    EcoStat_B01ID == 10 ~ 'unm',
    EcoStat_B01ID == 11 ~ 'unm',
    TRUE ~ as.character(EcoStat_B01ID)
  ))



unclassified_build %>% colnames()



# Convert gender to Male or Female
nts_df <- nts_df %>%
  mutate(Sex_B01ID = case_when(
    Sex_B01ID == 1 ~ 'Male',
    Sex_B01ID == 2 ~ 'Females',
    TRUE ~ as.character(Sex_B01ID)
  ))

df_test %>% select(Sex_B01ID)


# Remove NA
df1 <- week_weighted_trips %>% na.omit()

# Transform catgeorical variables to factors
facts <- c("hb_purpose", "age", "gender", "household", "household_composition", "cars", "employment", "area_type")

df1 <- df1 %>% mutate_at(facts, funs(factor))

# Split data into 75% train and 25% test
smp_size <- floor(0.75*nrow(df1))

train_ind <- sample(seq_len(nrow(df1)), size = smp_size)
train <- df1 %>% slice(train_ind)
test <- df1 %>% slice(-train_ind)

# Full model with all variables
glm.nb(tfn_trip_rate ~ . - factor(hb_purpose, exclude all but 1), data = df1, subset = train_ind) %>% summary()

d



MASS::stepAIC(glm.nb(tfn_trip_rate ~ ., data = df1, subset = train_ind)) %>% summary()

glm.nb(tfn_trip_rate ~ ,
       subset = train_ind, data = df1) %>% summary()

# Stepwise regression
MASS::stepAIC(glm.nb(tfn_trip_rate ~ ., data = df1, subset = train_ind)) %>% summary()

## TODO:
# Overdispersion test for poisson model to provide evidence of it's overdispersion
# Use AER Package: 'dispersiontest'


# Ian williams package attempt


df_test

nts_ntem_df$gender %>% factor()
df_test %>% colnames()
df_test %>% str()
df_test %>% select(Sex_B01ID) %>% pull() %>% factor()

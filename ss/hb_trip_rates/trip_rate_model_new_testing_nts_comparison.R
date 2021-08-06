# New trip rates model approach
#library(MASS)
library(tidyverse)
library(pscl)

select <- dplyr::select

hb_csv <- read_csv("C:/Users/Pluto/Documents/Trip_rate_testing/classified_nts_trip_rates_vntem.csv")
model_forms <- read_csv("C:/Users/Pluto/Documents/Trip_rate_testing/model_forms.csv")
tt_lu <- read_csv("C:/Users/Pluto/Documents/Trip_rate_testing/traveller_type---age_work_status--gender--hh_adults--cars.csv")
year_weights <- read_csv("C:/Users/Pluto/Documents/Trip_rate_Testing/nts_comparison/year_weights_2002-2017.csv")

hb_csv <- hb_csv %>% 
  filter(SurveyYear == 2015:2017) %>% 
  select(age_work_status, gender, hh_adults, cars, tfn_area_type, trip_purpose, weekly_trips, trip_rate, SurveyYear) %>% 
  mutate(gender = factor(gender, levels = 0:2),
         hh_adults = factor(hh_adults, levels = 1:3),
         cars = factor(cars, levels = 1:4),
         tfn_area_type = factor(tfn_area_type, levels = 1:8),
         SurveyYear = factor(SurveyYear, levels = 2016:2017))

all_vars <- list(trip_purpose = 1:8,
                 age_work_status = 1:6,
                 hh_adults = 1:3,
                 gender = 0:2,
                 cars = 1:4,
                 tfn_area_type = 1:8,
                 SurveyYear = 2016:2017)

all_predictors <- list_modify(all_vars, "trip_purpose" = zap(), "age_work_status" = zap())
child_predictors <- list_modify(all_predictors, "gender" = zap())

child_predictors <- child_predictors %>% 
  names() %>% 
  str_c(., collapse = " + ")

com_bus_predictors <- all_predictors %>% 
  names() %>% 
  str_c(., collapse = " + ")

oth_predictors <- all_predictors %>% 
  names() %>% 
  str_c(., collapse = " + ")

model_forms <- model_forms %>% 
  mutate(formula = case_when(
    aws == 1 & mod_form == "nb" ~ child_predictors,
    aws == 1 & mod_form != "nb" ~ str_c(child_predictors, " | 1"),
    aws != 1 & p %in% 1:2 & mod_form == "nb" ~ com_bus_predictors,
    aws != 1 & p %in% 1:2 & mod_form != "nb" ~ str_c(com_bus_predictors, " | 1"),
    aws != 1 & p %in% 3:8 & mod_form == "nb" ~ oth_predictors,
    aws != 1 & p %in% 3:8 & mod_form != "nb" ~ str_c(oth_predictors, " | 1"))) %>% 
  mutate(response = ifelse(mod_form == "nb", "trip_rate", "weekly_trips"),
         formula = paste(response, paste(formula), sep = " ~ ")) %>% 
  select(-response)

model_forms <- transpose(model_forms)

df_trip_rates <- list()

for(i in 1:length(model_forms)){
  
  cur_iter <- model_forms[[i]]
  cur_df <- filter(hb_csv, trip_purpose == cur_iter$p, age_work_status == cur_iter$aws)
  cur_year_weights <- filter(year_weights, trip_purpose == cur_iter$p, age_work_status == cur_iter$aws)
  
  if(cur_iter$mod_form == "nb"){
    
    mod <- MASS::glm.nb(formula = as.formula(cur_iter$formula),
                        data = cur_df)
    
    mod$levels <- mod$xlevels
    
  } else if(cur_iter$mod_form == "zip"){
    
    mod <- zeroinfl(formula = as.formula(cur_iter$formula),
                    data = cur_df,
                    dist = "poisson")
    
  } else if (cur_iter$mod_form == "zinb"){
    
    mod <- zeroinfl(formula = as.formula(cur_iter$formula),
                    data = cur_df,
                    dist = "negbin")
    
  }
  
  newdata <- mod$levels %>% 
    cross() %>% 
    bind_rows()
  
  newdata_factor <- mutate_all(newdata, factor)
  
  group_vars <- str_subset(colnames(newdata), "SurveyYear", negate = TRUE)
  
  mod_predictions <- predict(mod, newdata = newdata_factor, type = "response")
  
  new_data <- newdata %>% 
    mutate(trip_rates = mod_predictions,
           SurveyYear = as.double(SurveyYear)) %>% 
    left_join(cur_year_weights) %>% 
    mutate(trip_rates = trip_rates * weights) %>% 
    group_by(!!!syms(group_vars)) %>% 
    summarise(trip_rates = sum(trip_rates), .groups = 'drop') %>% 
    mutate(age_work_status = as.character(cur_iter$aws),
           trip_purpose = as.character(cur_iter$p))
    
  #new_data <- newdata %>% 
  #  mutate(trip_rates = mod_predictions,
  #         SurveyYear = as.integer(levels(SurveyYear)[SurveyYear])) %>% 
  #  left_join(cur_year_weights) %>% 
  #  mutate(trip_rates = trip_rates * weights) %>% 
  #  group_by(!!!syms(group_vars)) %>% 
  #  summarise(trip_rates = sum(trip_rates), .groups = 'drop') %>% 
  #  ungroup() %>% 
  #  mutate(trip_purpose = as.factor(cur_iter$p),
  #         age_work_status = as.factor(cur_iter$aws))
  
  if(cur_iter$aws == 1){
    
    df_trip_rates[[i]] <- mutate(new_data, gender = "2")
    
  } else {
    
    df_trip_rates[[i]] <- new_data
    
  }
  
}

df_trip_rates <- df_trip_rates %>% 
  bind_rows()

purpose_aws_report <- df_trip_rates %>% 
  group_by(age_work_status, trip_purpose) %>% 
  summarise(trip_rates = mean(trip_rates)) %>% 
  pivot_wider(names_from = "age_work_status",
              values_from = "trip_rates")

colnames(purpose_aws_report) <- c("Purpose",
                                  "Child",
                                  "FTE",
                                  "PTE",
                                  "STU",
                                  "UNM",
                                  "RET")
purpose_aws_report

tt_trip_rates <- df_trip_rates %>% 
  mutate_at(vars(age_work_status, gender, hh_adults, cars), list(~ as.double(.))) %>% 
  left_join(tt_lu, by = c("hh_adults", "cars", "age_work_status", "gender")) %>% 
  na.omit()

df_trip_rates %>% 
  mutate_at(vars(age_work_status, gender, hh_adults, cars), list(~ as.numeric(levels(.))[.])) %>% 
  left_join(tt_lu, by = c("hh_adults", "cars", "age_work_status", "gender"))

tt_trip_rates <- tt_trip_rates %>% 
  group_by(trip_purpose, tt, tfn_area_type) %>%
  summarise(trip_rates = mean(trip_rates)) %>% 
  ungroup()

trip_rates_out <- tt_trip_rates %>% 
  rename(p = trip_purpose,
         traveller_type = tt,
         trip_rate = trip_rates,
         area_type = tfn_area_type) %>% 
  dplyr::select(p, traveller_type, area_type, trip_rate)

write_csv(trip_rates_out, "C:/Users/Pluto/Documents/Trip_rate_testing/hb_trip_rates_ntsv2.csv")

report_productions()


###

unclassified_build %>% 
  filter(trip_purpose %in% 1:8) %>% 
  group_by(trip_purpose, age_work_status) %>% 
  count(SurveyYear) %>% 
  mutate(weights = n/sum(n)) %>% 
  select(-n) %>% 
  write_csv("C:/Users/Pluto/Documents/Trip_rate_Testing/nts_comparison/year_weights_2002-2017.csv")

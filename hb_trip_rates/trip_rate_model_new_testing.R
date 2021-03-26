# New trip rates model approach
#library(MASS)
library(tidyverse)
library(pscl)

select <- dplyr::select

hb_csv <- read_csv("C:/Users/Pluto/Documents/Trip_rate_testing/classified_nts_trip_rates_v10.csv")
model_forms <- read_csv("C:/Users/Pluto/Documents/Trip_rate_testing/model_forms.csv")
tt_lu <- read_csv("C:/Users/Pluto/Documents/Trip_rate_testing/traveller_type---age_work_status--gender--hh_adults--cars.csv")

hb_csv <- hb_csv %>% 
  select(age_work_status, gender, hh_adults, cars, soc_cat, ns_sec, tfn_area_type, trip_purpose, weekly_trips, trip_rate, W2) %>% 
  mutate(gender = factor(gender, levels = 0:2),
         hh_adults = factor(hh_adults, levels = 1:3),
         cars = factor(cars, levels = 1:4),
         soc_cat = factor(soc_cat, levels = c(1:3, 99)),
         ns_sec = factor(ns_sec, levels = c(1:5, 99)),
         tfn_area_type = factor(tfn_area_type, levels = 1:8)) %>% 
  mutate(W2 = ifelse(W2 == 0, 1, W2))

#test_df <- filter(hb_csv, trip_purpose == 1, age_work_status == 2) %>% 
#  rename(trip_rates = trip_weights)
#
#test_mod <- glm.nb(weekly_trips ~ hh_adults + gender,
#                   data = test_df)
#
#new_data = tibble(hh_adults = "1", gender = "0")
#
#predict.glm(test_mod, new_data, type = "response")

# model_forms <- transpose(model_forms)
all_vars <- list(trip_purpose = 1:8,
                 age_work_status = 1:6,
                 hh_adults = 1:3,
                 gender = 0:2,
                 cars = 1:4,
                 tfn_area_type = 1:8,
                 soc_cat = c(1:3, 99),
                 ns_sec = c(1:5, 99))

all_predictors <- list_modify(all_vars, "trip_purpose" = zap(), "age_work_status" = zap())
child_predictors <- list_modify(all_predictors, "soc_cat" = zap(), "ns_sec" = zap(), "gender" = zap())
com_bus_predictors <- list_modify(all_predictors, "ns_sec" = zap())
oth_predictors <- list_modify(all_predictors, "soc_cat" = zap())

child_predictors <- child_predictors %>% 
  names() %>% 
  str_c(., collapse = " + ")

com_bus_predictors <- com_bus_predictors %>% 
  names() %>% 
  str_c(., collapse = " + ")

oth_predictors <- oth_predictors %>% 
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

#test_df <- filter(hb_csv, trip_purpose == 3, age_work_status == 1)
#
#test_mod <- glm.nb(weekly_trips ~ hh_adults + cars + tfn_area_type,
#                   data = test_df)
#
#df <- tibble(dependent_rate = c(5.2, 3.4, 7.8, 9.5),
#             dependent_count = c(5, 3, 7, 9),
#             pred1 = c(1, 2, 3, 4),
#             pred2 = c(1, 2, 1, 2))
#
#nb_mod <- glm.nb(dependent_count ~ pred1 + pred2 + 
#         offset(log(dependent_count/dependent_rate)), data = df)
#
#new_data <- tibble(pred1 = 4, pred2 = 1)

#library(emmeans)
#
#emmeans(nb_mod,
#        ~ pred1 + pred2,
#        type = "response",
#        offset = mean(df$dependent_count))
#
#predict.glm(nb_mod, new_data, "response")

model_forms <- transpose(model_forms)

df_trip_rates <- list()

for(i in 1:length(model_forms)){
  
  cur_iter <- model_forms[[i]]
  cur_df <- filter(hb_csv, trip_purpose == cur_iter$p, age_work_status == cur_iter$aws)
  
  final_predictors <- c()
  
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
    bind_rows() %>% 
    mutate_all(factor)
  
  mod_predictions <- predict(mod, newdata = newdata, type = "response")
  
  new_data <- newdata %>% 
    mutate(trip_rates = mod_predictions,
           trip_purpose = as.factor(cur_iter$p),
           age_work_status = as.factor(cur_iter$aws))
  
  if(cur_iter$p %in% 1:2 & cur_iter$aws == 1){
    
    final_predictors <- c(final_predictors, "gender", "ns_sec", "soc_cat")
    missing_vars <- all_predictors[!names(all_predictors) %in% final_predictors]
    
    df_trip_rates[[i]] <- new_data %>%
      mutate(soc_cat = factor(0),
             ns_sec = "none",
             gender = factor(2)) 
    
    
  } else if (cur_iter$p %in% 3:8 & cur_iter$aws == 1){
    
    final_predictors <- c(final_predictors, "gender", "ns_sec", "soc_cat")
    missing_vars <- all_predictors[!names(all_predictors) %in% final_predictors]
    
    df_trip_rates[[i]] <- new_data %>%
      mutate(soc_cat = factor("none"),
             gender = factor(2)) %>% 
      crossing(tibble(ns_sec = 1:5)) %>% 
      mutate(ns_sec = factor(ns_sec))
    
  } else if (cur_iter$p %in% 1:2 & cur_iter$aws != 1){
    
    df_trip_rates[[i]] <- new_data %>% 
      mutate(ns_sec = factor("none"))
    
  } else if (cur_iter$p %in% 3:8 & cur_iter$aws != 1){
    
    #NS SEC == 4 needs to be added back in
    new_data <- new_data %>%
      filter(ns_sec != 99) %>% 
      mutate(soc_cat = factor("none"))
    
    ns_sec4 <- new_data %>% 
      group_by(hh_adults, cars, tfn_area_type, gender, trip_purpose, age_work_status, soc_cat) %>% 
      summarise(trip_rates = mean(trip_rates)) %>% 
      ungroup() %>% 
      mutate(ns_sec = factor(4))
    
    df_trip_rates[[i]] <- new_data %>% 
      bind_rows(ns_sec4) %>% 
      mutate(ns_sec = factor(ns_sec))
    
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
  mutate_at(vars(age_work_status, gender, hh_adults, cars), list(~ as.numeric(levels(.))[.])) %>% 
  left_join(tt_lu, by = c("hh_adults", "cars", "age_work_status", "gender")) %>% 
  na.omit()

df_trip_rates %>% 
  mutate_at(vars(age_work_status, gender, hh_adults, cars), list(~ as.numeric(levels(.))[.])) %>% 
  left_join(tt_lu, by = c("hh_adults", "cars", "age_work_status", "gender"))

tt_trip_rates <- tt_trip_rates %>% 
  group_by(trip_purpose, tt, tfn_area_type, soc_cat, ns_sec) %>%
  summarise(trip_rates = mean(trip_rates)) %>% 
  ungroup()

trip_rates_out <- tt_trip_rates %>% 
  rename(p = trip_purpose,
         traveller_type = tt,
         soc = soc_cat,
         ns = ns_sec,
         trip_rate = trip_rates,
         area_type = tfn_area_type) %>% 
  dplyr::select(p, traveller_type, soc, ns, area_type, trip_rate)

write_csv(trip_rates_out, "C:/Users/Pluto/Documents/Trip_rate_testing/hb_trip_rates_v3.5.csv")

report_productions()


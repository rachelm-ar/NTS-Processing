### Regression of mode share against job accessibility by rail ###


#### Load libraries and import data ####
require("tidyverse")
require("data.table")
require("ggplot2")

# Import uward_mode_share and rail accessibility data
# ward_mode_share has been filtered to exclude observations with < 5 trips and purpose/area/car combinations with <20 wards
ward_mode_share <- read_csv("Y:/NTS/mode_time_splits/rail share regression/input_railshare_jobs_by_ward.csv")
colnames(ward_mode_share) <- c("id","ward", "cars", "area_type", "hb_purpose", "total_trips",	"m1",	"m2",	"m3",	"m5",	"m6")

rail_accessibility <- read_csv("Y:/NTS/accessibility_metric/jobs_by_pt.csv")
colnames(rail_accessibility) <- c("msoa","jobs", "ward", "fit", "<40%")

# Join accessible jobs by ward
ward_mode_share <- ward_mode_share %>% left_join(rail_accessibility)#, by = c("HHoldOSWard_B01ID" = "ward"))

# Remove NAs
ward_mode_share <- ward_mode_share[complete.cases(ward_mode_share), ]


# Backup df
backup_df <- ward_mode_share


#### Regression of rail share against accessible jobs, loop through all trip purposes (8), area types (8) and car availability (2)####
# for id in ward_mode_share$id

regression_list <- list()
id_list <- unique(ward_mode_share$id)

# Loop through all regressions, append outputs to regression_list
for (i in id_list) {
  fit_id <- lm(data = subset(ward_mode_share, id == i), m6 ~ jobs)
  #print(length(summary(fit_id)$coefficients)>4)
  list_temp <- list(regression_df$id[1], summary(fit_id)$coefficients[1,1], summary(fit_id)$coefficients[2,1])
  regression_list <- append(regression_list, list_temp)
  
  lm_plot <- ggplot(data = subset(ward_mode_share, id == i), aes(x = jobs, y = m6)) +
    geom_point(data = subset(ward_mode_share, id == i)) +
    geom_smooth(method='lm')

  file_name = paste("C:/Users/scar2.0/Documents/lm_plots/plot_", i, ".png", sep="")
  png(file_name)
  print(lm_plot)
  dev.off()

}

dim(regression_list) <- c(3,128)
rownames(regression_list) <- c("id", "intercept", "coefficent")

regression_df <- as.data.frame(t(regression_list))

regression_df$intercept <- vapply(regression_df$intercept, paste, collapse = ", ", character(1L))
regression_df$id <- vapply(regression_df$id, paste, collapse = ", ", character(1L))
regression_df$coefficent <- vapply(regression_df$coefficent , paste, collapse = ", ", character(1L))

regression_df %>% write.csv("Y:/NTS/accessibility_metric/ward_railshare_parameters.csv")
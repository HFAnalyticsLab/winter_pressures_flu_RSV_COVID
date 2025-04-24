rm(list=ls())

library(aws.s3)
library(readxl)
library(tidyverse)
library(lubridate)
library(tsibble)

#read in the data 

library(readxl)
url <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/04/Monthly-AE-Time-Series-March-2025.xls"
destfile <- "Monthly_AE_Time_Series_March_2025.xls"
curl::curl_download(url, destfile)
Monthly_AE_Time_Series_March_2025 <- read_excel(destfile)
View(Monthly_AE_Time_Series_March_2025)

#read in the activity sheet 

aevolume <- read_excel(destfile, sheet = 'Activity')
View(aevolume)


aevolume<-aevolume %>% 
  clean_names() %>% 
  slice(which(title=="Period"):n()) %>% 
  row_to_names(., 1) %>% 
  clean_names()


aevolume<-aevolume %>% 
  clean_names() %>% 
  select(c("period","type_1_departments_major_a_e",
           "type_2_departments_single_specialty", "type_3_departments_other_a_e_minor_injury_unit")) %>% 
  mutate(period=as.Date(as.numeric(period), origin="1899-12-30")) %>% 
  filter(period>as.Date("2014-12-01"))

aevolume[2:4] = lapply(aevolume[2:4], FUN = function(y){as.numeric(y)})

write.csv(aevolume, file = "typegraphdata.csv", row.names = FALSE)


############################


y16_17 <- format(as.Date(seq(lubridate::ymd('2016-11-01'), lubridate::ymd('2017-03-01'), by='1 month')), "%Y-%m")
y17_18 <- format(as.Date(seq(lubridate::ymd('2017-11-01'), lubridate::ymd('2018-03-04'), by='1 month')), "%Y-%m")
y18_19 <- format(as.Date(seq(lubridate::ymd('2018-11-01'), lubridate::ymd('2019-03-03'), by='1 month')), "%Y-%m")
y19_20 <- format(as.Date(seq(lubridate::ymd('2019-11-01'), lubridate::ymd('2020-03-01'), by='1 month')), "%Y-%m")
y20_21 <- format(as.Date(seq(lubridate::ymd('2020-11-01'), lubridate::ymd('2021-03-04'), by='1 month')), "%Y-%m")
y21_22 <- format(as.Date(seq(lubridate::ymd('2021-11-01'), lubridate::ymd('2022-03-03'), by='1 month')), "%Y-%m")
y22_23 <- format(as.Date(seq(lubridate::ymd('2022-11-01'), lubridate::ymd('2023-03-03'), by='1 month')), "%Y-%m")
y23_24 <- format(as.Date(seq(lubridate::ymd('2023-11-01'), lubridate::ymd('2024-03-31'), by='1 month')), "%Y-%m")
y24_25 <- format(as.Date(seq(lubridate::ymd('2024-11-01'), lubridate::ymd('2025-03-31'), by='1 month')), "%Y-%m")

# Convert sequences to date format
winter_dates <- c(y16_17, y17_18, y18_19, y19_20, y20_21, y21_22, y22_23, y23_24, y24_25)
winter_dates <- as.Date(paste0(winter_dates, "-01"))

# Filter and summarize the data
aevolume <- aevolume %>%
  mutate(period_month = format(period, "%Y-%m")) %>%
  filter(as.Date(paste0(period_month, "-01")) %in% winter_dates) %>%
  mutate(winter = case_when(
    period_month %in% y16_17 ~ "16/17",
    period_month %in% y17_18 ~ "17/18",
    period_month %in% y18_19 ~ "18/19",
    period_month %in% y19_20 ~ "19/20",
    period_month %in% y20_21 ~ "20/21",
    period_month %in% y21_22 ~ "21/22",
    period_month %in% y22_23 ~ "22/23",
    period_month %in% y23_24 ~ "23/24",
    period_month %in% y24_25 ~ "24/25"
  ))

# Summarize total attendances for each winter period

winter_summary <- aevolume %>%
  group_by(winter) %>%
  summarise(
    total_type_1_departments_major_a_e = sum(type_1_departments_major_a_e, na.rm = TRUE),
    total_type_2_departments_single_specialty = sum(type_2_departments_single_specialty, na.rm = TRUE),
    total_type_3_departments_other_a_e_minor_injury_unit = sum(type_3_departments_other_a_e_minor_injury_unit, na.rm = TRUE)
  )


# Save the summary to a CSV file with the new name
write.csv(winter_summary, "ane_c1c3winter.csv", row.names = FALSE)

print("Summary saved to ane_c1c3winter.csv")





###################################


# y16_17 <- format(as.Date(seq(lubridate::ymd('2016-11-01'), lubridate::ymd('2017-03-01'), by='1 month')), "%Y-%m")
# y17_18 <- format(as.Date(seq(lubridate::ymd('2017-11-01'), lubridate::ymd('2018-03-04'), by='1 month')), "%Y-%m")
# y18_19 <- format(as.Date(seq(lubridate::ymd('2018-11-01'), lubridate::ymd('2019-03-03'), by='1 month')), "%Y-%m")
# y19_20 <- format(as.Date(seq(lubridate::ymd('2019-11-01'), lubridate::ymd('2020-03-01'), by='1 month')), "%Y-%m")
# y20_21 <- format(as.Date(seq(lubridate::ymd('2020-11-01'), lubridate::ymd('2021-03-04'), by='1 month')), "%Y-%m")
# y21_22 <- format(as.Date(seq(lubridate::ymd('2021-11-01'), lubridate::ymd('2022-03-03'), by='1 month')), "%Y-%m")
# y22_23 <- format(as.Date(seq(lubridate::ymd('2022-11-01'), lubridate::ymd('2023-03-03'), by='1 month')), "%Y-%m")
# y23_24 <- format(as.Date(seq(lubridate::ymd('2023-11-01'), lubridate::ymd('2024-03-31'), by='1 month')), "%Y-%m")
# y24_25 <- format(as.Date(seq(lubridate::ymd('2024-11-01'), lubridate::ymd('2025-02-23'), by='1 month')), "%Y-%m")
# 
# # Convert sequences to date format
# winter_dates <- c(y16_17, y17_18, y18_19, y19_20, y20_21, y21_22, y22_23, y23_24, y24_25)
# winter_dates <- as.Date(paste0(winter_dates, "-01"))
# 
# # Filter and summarize the data
# aevolume <- aevolume %>%
#   mutate(period_month = format(period, "%Y-%m")) %>%
#   filter(as.Date(paste0(period_month, "-01")) %in% winter_dates) %>%
#   mutate(winter = case_when(
#     period_month %in% y16_17 ~ "16/17",
#     period_month %in% y17_18 ~ "17/18",
#     period_month %in% y18_19 ~ "18/19",
#     period_month %in% y19_20 ~ "19/20",
#     period_month %in% y20_21 ~ "20/21",
#     period_month %in% y21_22 ~ "21/22",
#     period_month %in% y22_23 ~ "22/23",
#     period_month %in% y23_24 ~ "23/24",
#     period_month %in% y24_25 ~ "24/25"
#   ))
# 
# # Summarize total attendances for each winter period
# winter_summary <- aevolume %>%
#   group_by(winter) %>%
#   summarise(total_attendances = sum(total_attendances))
# 
# # Summarize total attendances for each winter period for each column
# winter_summary <- aevolume %>%
#   group_by(winter) %>%
#   summarise(
#     total_other_emergency_admissions = sum(other_emergency_admissions_i_e_not_via_a_e, na.rm = TRUE),
#     total_emergency_admissions = sum(total_emergency_admissions, na.rm = TRUE),
#     total_emergency_admissions_via_a_e = sum(total_emergency_admissions_via_a_e, na.rm = TRUE)
#   )
# 
# # Save the summary to a CSV file with the new name
# write.csv(winter_summary, "ane_totals_year_added.csv", row.names = FALSE)
# 
# print("Summary saved to ane_totals_year_added.csv")

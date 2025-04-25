
rm(list=ls())

library(aws.s3)
library(readxl)
library(tidyverse)
library(lubridate)
library(tsibble)

library(readxl)
url <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/04/Monthly-AE-Time-Series-March-2025.xls"
destfile <- "Monthly_AE_Time_Series_March_2025.xls"
curl::curl_download(url, destfile)
Monthly_AE_Time_Series_March_2025 <- read_excel(destfile)
View(Monthly_AE_Time_Series_March_2025)


aevolume <- read_excel(destfile, sheet = 'Activity')
View(aevolume)


aevolume<-aevolume %>% 
  clean_names() %>% 
  slice(which(title=="Period"):n()) %>% 
  row_to_names(., 1) %>% 
  clean_names()


aevolume<-aevolume %>% 
  clean_names() %>% 
  select(c("period","other_emergency_admissions_i_e_not_via_a_e",
           "total_emergency_admissions", "total_emergency_admissions_via_a_e")) %>% 
  mutate(period=as.Date(as.numeric(period), origin="1899-12-30")) %>% 
  filter(period>as.Date("2014-12-01"))

aevolume[2:4] = lapply(aevolume[2:4], FUN = function(y){as.numeric(y)})



##################### plot code use this if you want to visualise ############################
options(scipen = 999)

library(ggplot2)
library(dplyr)
library(scales)
library(ggtext)

# Assuming aevolume is your data frame
plot <- aevolume %>%
  select(c(period, other_emergency_admissions_i_e_not_via_a_e, total_emergency_admissions, total_emergency_admissions_via_a_e)) %>%
  pivot_longer(cols = -period, names_to = "admission_type", values_to = "admissions") %>%
  mutate(admission_type = recode(admission_type,
                                 "other_emergency_admissions_i_e_not_via_a_e" = "Other emergency admissions i.e not via A&E",
                                 "total_emergency_admissions" = "Total emergency admissions",
                                 "total_emergency_admissions_via_a_e" = "Total emergency admissions via A&E")) %>%
  ggplot(aes(x = period, y = admissions, color = admission_type, group = admission_type)) +
  geom_line() +
  scale_x_date(breaks = date_breaks("6 months"), labels = date_format("%b %y")) +
  theme_minimal() +
  annotate("rect", xmin = as.Date("2020-03-01"), xmax = as.Date("2021-05-01"), 
           ymin = 0, ymax = max(aevolume$other_emergency_admissions_i_e_not_via_a_e, 
                                aevolume$total_emergency_admissions, 
                                aevolume$total_emergency_admissions_via_a_e), 
           fill = "grey20", alpha = .1) +
  annotate("richtext", x = as.Date("2020-03-01"), y = max(aevolume$other_emergency_admissions_i_e_not_via_a_e, 
                                                          aevolume$total_emergency_admissions, 
                                                          aevolume$total_emergency_admissions_via_a_e) - 0.015, 
           label = "First two waves <br> of COVID-19", size = 3, colour = "black", hjust = 0, fill = NA, label.color = NA) +
  labs(x = "", y = "Number of Admissions", caption = "NHS England, A&E Attendances and Emergency Admissions") +
  theme(legend.text = element_text(size = 11),
        legend.title = element_blank(),
        axis.text.x = element_text(size = 8, angle = 60), 
        axis.text.y = element_text(size = 11),
        plot.caption = element_markdown(hjust = 0, size = 9),
        plot.margin = unit(c(1, 1.5, 0.5, 0.5), "cm"),
        legend.margin = margin(0, 0, 0, 0),
        legend.box.margin = margin(-10, -10, -10, -10))

# Save the plot as a PNG file
ggsave("emergency_admissions_plot.png", plot = plot, width = 10, height = 6)

print("The plot has been saved as 'emergency_admissions_plot.png'.")

print(plot)


#########################################################################################################################################################################
# this code is for the total number of ane emergency hospital visits within the winters 

### note that winters include nov, dec, jan, feb ,march



library(dplyr)
library(lubridate)

# Create sequences for each winter period including November
y16_17 <- format(as.Date(seq(lubridate::ymd('2016-11-01'), lubridate::ymd('2017-03-04'), by='1 month')), "%Y-%m")
y17_18 <- format(as.Date(seq(lubridate::ymd('2017-11-01'), lubridate::ymd('2018-03-04'), by='1 month')), "%Y-%m")
y18_19 <- format(as.Date(seq(lubridate::ymd('2018-11-01'), lubridate::ymd('2019-03-03'), by='1 month')), "%Y-%m")
y19_20 <- format(as.Date(seq(lubridate::ymd('2019-11-01'), lubridate::ymd('2020-03-01'), by='1 month')), "%Y-%m")
y20_21 <- format(as.Date(seq(lubridate::ymd('2020-11-01'), lubridate::ymd('2021-03-04'), by='1 month')), "%Y-%m")
y21_22 <- format(as.Date(seq(lubridate::ymd('2021-11-01'), lubridate::ymd('2022-03-03'), by='1 month')), "%Y-%m")
y22_23 <- format(as.Date(seq(lubridate::ymd('2022-11-01'), lubridate::ymd('2023-03-03'), by='1 month')), "%Y-%m")
y23_24 <- format(as.Date(seq(lubridate::ymd('2023-11-01'), lubridate::ymd('2024-03-31'), by='1 month')), "%Y-%m")
y24_25 <- format(as.Date(seq(lubridate::ymd('2024-11-01'), lubridate::ymd('2025-03-23'), by='1 month')), "%Y-%m")

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

# Summarize total attendances for each winter period for each column
winter_summary <- aevolume %>%
  group_by(winter) %>%
  summarise(
    total_other_emergency_admissions = sum(other_emergency_admissions_i_e_not_via_a_e, na.rm = TRUE),
    total_emergency_admissions = sum(total_emergency_admissions, na.rm = TRUE),
    total_emergency_admissions_via_a_e = sum(total_emergency_admissions_via_a_e, na.rm = TRUE)
  )

# Save the summary to a CSV file with the new name
write.csv(winter_summary, "hospital_totals.csv", row.names = FALSE)

print("Summary saved to hospital_totals.csv") 


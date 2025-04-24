##To download, process, analyse and visualise D&V, flu, RSV, and COVID, A&E diverts, Bed Occupancy, Delayed Dischargers and NHS Staff sickness and absence data

rm(list=ls())




# Setup -------------------------------------------------------------------

library (here)
library(curl)
library(tidyverse)
library(janitor)
library(readxl)
library(broom)
library(lubridate)
library(ggplot2)
library(ISOweek)
library(ggrepel)
library(THFstyle)

options(scipen=999)


# Dates --------------------------------------------------------------

#Update this or new data#



y16_17<-format(as.Date(seq(lubridate::ymd('2016-11-28'),lubridate::ymd('2017-03-12'),by='1 day')),"%Y-%m-%d")
y17_18<-format(as.Date(seq(lubridate::ymd('2017-11-20'),lubridate::ymd('2018-03-04'),by='1 day')),"%Y-%m-%d")
y18_19<-format(as.Date(seq(lubridate::ymd('2018-12-03'),lubridate::ymd('2019-04-03'),by='1 day')),"%Y-%m-%d")
y19_20<-format(as.Date(seq(lubridate::ymd('2019-12-02'),lubridate::ymd('2020-03-01'),by='1 day')),"%Y-%m-%d")
y20_21<-format(as.Date(seq(lubridate::ymd('2020-11-30'),lubridate::ymd('2021-04-04'),by='1 day')),"%Y-%m-%d")
y21_22<-format(as.Date(seq(lubridate::ymd('2021-11-29'),lubridate::ymd('2022-04-03'),by='1 day')),"%Y-%m-%d")
y22_23<-format(as.Date(seq(lubridate::ymd('2022-11-14'),lubridate::ymd('2023-04-03'),by='1 day')),"%Y-%m-%d")
y23_24<-format(as.Date(seq(lubridate::ymd('2023-11-20'),lubridate::ymd('2024-03-31'),by='1 day')),"%Y-%m-%d")
y24_25<-format(as.Date(seq(lubridate::ymd('2024-11-25'),lubridate::ymd('2025-03-31'),by='1 day')),"%Y-%m-%d")

d <- paste0("W", sprintf("%02d", c(46:53, 1:14))) 

# Data download -----------------------------------------------

# Urgent and Emergency Care Sit Reps --------------------------------------

 #2015-16
link<-'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2015/12/DailySR-Timeseries-WE-28.02.16.xlsx'
destfile <- here::here('data', "raw2015.xlsx")
curl_download(link, destfile = destfile)


#2016-17
link<-'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2016/12/DailySR-Web-file-Time-Series-18.xlsx'

destfile <- here::here('data', "raw2016.xlsx")
curl_download(link, destfile = destfile)

 #2017-18
link<-'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2018/03/Winter-data-Timeseries-20180304.xlsx'

destfile <- here::here('data', "raw2017.xlsx")
curl_download(link, destfile = destfile)

 #2018-19
link<-'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2019/03/Winter-data-timeseries-20190307.xlsx'
destfile <- here::here('data', "raw2018.xlsx")
curl_download(link, destfile = destfile)

#2019-20
link<-'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/03/Winter-SitRep-Acute-Time-series-2-December-2019-1-March-2020.xlsx'
destfile <- here::here('data', "raw2019.xlsx")
curl_download(link, destfile = destfile)

 #2020-21
link<-'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/04/UEC-Daily-SitRep-Acute-Web-File-Timeseries-1.xlsx'
destfile <- here::here('data', "raw2020.xlsx")
curl_download(link, destfile = destfile)


#####----Please note----######
#for 2021-22, Need to manually change the RSV sheet name to get rid of the trailing space
#####-------------------######

# #2021-22
link<-'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/04/UEC-Daily-SitRep-Web-File-Timeseries.xlsx'
destfile <- here::here('data', "raw2021.xlsx")
curl_download(link, destfile = destfile) 

#2022-23
link<-'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/04/UEC-Daily-SitRep-Web-File-Timeseries.xlsx'
destfile <- here::here('data', "raw2022.xlsx")
curl_download(link, destfile = destfile)

#2023-24
link<-'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2024/04/Web-File-Timeseries-UEC-Daily-SitRep.xlsx'
destfile <- here::here('data', "raw2023.xlsx")
curl_download(link, destfile = destfile)

#2024-25
link<-'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/04/Web-File-Timeseries-UEC-Daily-SitRep.xlsx'

destfile <- here::here('data', "raw2024.xlsx")
curl_download(link, destfile = destfile)

# UKHSA - national flu report ---------------------------------------------

##2024-25
link<-'https://assets.publishing.service.gov.uk/media/67ed5ac5632d0f88e8248c1e/weekly-influenza-and-COVID-19-report-data-week-14-2025.ods'
destfile <- here::here('data', "weekly_flu.ods")
curl_download(link, destfile = destfile)

##2023-24
# link<-'https://assets.publishing.service.gov.uk/media/66d9e1fae87ad2f121826516/surveillance-of-influenza-and-other-seasonal-respiratory-viruses-in-the-UK-data_CORRECTION.ods'
# destfile <- here::here('data', "weekly_flu2024.ods")
# curl_download(link, destfile = destfile)


# Read in all the sheets --------------------------------------------------

# Directory containing Excel files
folder_path <- here('data')

# List all Excel files in the directory
excel_files <- list.files(path = folder_path, pattern = "\\.xlsx$", full.names = TRUE)

# Loop through each file and get sheet names
for (file in excel_files) {
  sheet_names <- readxl::excel_sheets(file)
  cat("Sheet names in file", basename(file), ":\n")
  print(sheet_names)
  cat("\n")
}


# Create an empty list to store sheet names per file
sheet_info_list <- lapply(excel_files, function(file) {
  # Extract sheet names
  sheets <- readxl::excel_sheets(file)
  # Create a data frame with the file name and sheet names
  data.frame(
    file_name = basename(file),
    file_path=file,
    sheet_name = sheets,
    stringsAsFactors = FALSE
  )
})

# Combine the list into a single data frame - to check which metrics exist in which csv files 
sheet_info_full <- bind_rows(sheet_info_list)

sheet_info_df <-sheet_info_full %>% 
  mutate(value=1) %>% 
  pivot_wider(
    names_from = sheet_name,
    values_from = value,
    values_fill = 0  # Fill missing values with 0
  ) %>% 
  clean_names()

write.csv(sheet_info_df,"list_metrics.csv")


#Specify the sheets you want to import 

sheets_to_import <- c("Flu", "RSV","Adult D&V, Norovirus", "Paediatric D&V, Norovirus", "D&V, Norovirus")

list_file <- sheet_info_full %>%
  rowwise() %>%
  filter(any(sheets_to_import %in% sheet_name)) %>% 
  ungroup() %>%
  select(file_path) %>%
  pull()

import_sitrep <- function(file, indicator){
  data <- readxl::read_xlsx(path = file, sheet = indicator, skip = 15, col_names = FALSE) 
  
  # Extract first header line containing dates and fill the gaps: 
  # Read 2 lines but guess the data types only from the first row
  # R will be looking for dates and convert the second row
  # to NA but the right length will be preserved. 
  header_1 <- readxl::read_xlsx(path = file, sheet = indicator, skip = 13, col_names = FALSE, n_max = 2, guess_max = 1)
  
  # Convert to columns, fill in the gaps and convert into vector
  header_1 <- header_1 %>% 
    t() %>% 
    as.data.frame() %>% 
    fill(.,'V1') 
  
  header_1 <- as.character(header_1$V1)  
  
  # Extract second header and convert into vector
  header_2 <- readxl::read_xlsx(path = file, sheet = indicator, skip = 14, col_names = FALSE, n_max = 1)
  header_2 <- unname(unlist(header_2[1,]))
  header_2 <- gsub(" [0-9]", "", header_2)

  # Concatenating headers to create column names
  # Replace NAs with a placeholder, otherwise concatenation fails
  column_names <- str_c(str_replace_na(header_1, "placeholder"), str_replace_na(header_2, "placeholder"), sep = "_")
  
  # Add column names to data and tidy
  names(data) <- tolower(column_names)
  names(data) <- gsub(" ", ".", names(data))
  names(data) <- gsub("placeholder_", "", names(data))
  names(data) <- gsub("'", "", names(data))
  names(data) <- gsub("<", "less.than", names(data))
  names(data) <- gsub(">", "more.than", names(data))
  
  # Tidy up table
  data_tidy <- data %>% 
    # remove empty column and line
    select(-placeholder) %>% 
    filter(!is.na(name)) %>%
    # Separate variables and dates
    gather(-1, -2, -3, key = "date_type", value = 'value') %>%
    separate(date_type, into = c("date", "type"), sep = "_") %>%
    spread(key = 'type', value = 'value') %>%
    # convert to the right variable types
    mutate(date = as.Date(date)) %>%
    mutate_at(vars(5:ncol(.)), funs(as.numeric))
  
  data_tidy
}

 


# Imprort the sheets from to all files in list_file
Sitrep_daily_all_files <- list_file %>% 
  map(~ {
    # Identify sheets present in this file that are also in sheets_to_import
    sheets_in_file <- sheet_info_full %>%
      filter(file_path == .x) %>%
      pull(sheet_name)
    
    valid_sheets <- intersect(sheets_in_file, sheets_to_import)
    
    # Only process the sheets that are actually in the file
    Sitrep_daily_file <- valid_sheets %>%
      map(import_sitrep, file = .x) %>% 
      reduce(left_join, by = c("nhs.england.region", "code", "name", "date"))
    
    return(Sitrep_daily_file)
  })


#Create one data set
Sitrep_daily_all <- Sitrep_daily_all_files %>%
  reduce(bind_rows)




# Sit rep data clean up -----------------------------------------------------------

winter_illness<-Sitrep_daily_all %>% 
  filter(str_detect(name,"ENGLAND")) %>% 
  mutate(isoweek=date2ISOweek(date)) %>%
  mutate(isoweek_short=str_sub(isoweek, 1,8)) %>% 
  select(-c(nhs.england.region,code)) %>% 
  rename(country=name) %>% 
  mutate(country=ifelse(country=="ENGLAND (All Acute Trusts)","ENGLAND",country)) %>% 
  filter(country=="ENGLAND") %>% 
  # group_by(isoweek_short) %>%  
  # summarise(across(where(is.numeric), sum, na.rm = TRUE))%>% 
  # mutate(date=ISOweek2date(paste0(isoweek_short,"-1"))) %>% 
  pivot_longer(-c(isoweek_short, date, country, isoweek), names_to="metric", values_to="count") %>% 
  mutate(metric2=case_when(metric %in% c("beds.closed.norovirus","beds.closed", "adult.d&v.beds.closed", "paeds.d&v.beds.closed")~ "beds.d&v.closed", 
                          metric %in% c("beds.closed.unocc", "adult.d&v.beds.closed.unocc","paeds.d&v.beds.closed.unocc")~ "beds.d&v.closed.unocc", 
                          TRUE~ metric)) %>% 
  filter(!is.na(count)) %>% 
  mutate(metric_label= case_when(metric2=="beds.d&v.closed"~  "D&V, Norovirus beds closed",
                                 metric2=="cc.flu.beds"~ "Critical Care Flu Beds", 
                                 metric2=="g&a.flu.beds"~ "General and Acute Flu Beds", 
                                 metric2=="beds.d&v.closed.unocc"~ "D&V, Norovirus beds - closed and Unoccupied", 
                                 metric2=="paeds.rsv.beds.closed"~ "Paeds RSV Beds Ccosed", 
                                 metric2=="paeds.rsv.beds.closed.unocc"~ "Paeds RSV Beds Closed and Unoccpied")) %>% 
  mutate(ft=case_when(date %in% as.Date(y16_17)~"16/17",
                      date %in% as.Date(y17_18)~"17/18",
                      date %in% as.Date(y18_19)~"18/19", 
                      date %in% as.Date(y19_20)~"19/20", 
                      date %in% as.Date(y20_21)~"20/21",
                      date %in% as.Date(y21_22)~"21/22", 
                      date %in% as.Date(y22_23)~"22/23", 
                      date %in% as.Date(y23_24)~ "23/24",
                      date %in% as.Date(y24_25)~"24/25")) %>% 
  distinct() %>% 
  group_by(isoweek_short,ft,metric_label) %>% 
  summarise(count=sum(count)) %>% 
  mutate(week=str_sub(isoweek_short,-3)) %>% 
  mutate(broad_metric=case_when(str_detect(metric_label,"Flu")~"FLU", 
                                str_detect(metric_label, "D&V")~"D&V, Norovirus", 
                                str_detect(metric_label, "Norovirus")~"D&V, Norovirus", 
                                str_detect(metric_label,"RSV")~"RSV")) 


#Total number 
num_total_winter_illness<-winter_illness %>% 
  group_by(ft,broad_metric) %>% 
   summarise(count=sum(count))

#Data for D&V chart (Figure 6)

winter_d_v<-winter_illness %>% 
  filter(broad_metric=="D&V, Norovirus") %>% 
  mutate(count=ifelse(count==0,NA,count)) %>%
  mutate(week = factor(week, levels = d)) %>% 
  group_by(broad_metric,ft, week) %>% 
  arrange() %>%
  pivot_wider(id_cols = c(week,ft,broad_metric), names_from=metric_label, values_from=count) %>% 
  clean_names() %>% 
  mutate(d_v_beds_occupied=d_v_norovirus_beds_closed-d_v_norovirus_beds_closed_and_unoccupied)


winter_d_v_flourish<-winter_d_v %>% 
  pivot_longer(d_v_beds_occupied,names_to="metric", values_to="count") %>% 
  pivot_wider(id_cols = c(week,broad_metric), names_from=ft, values_from=count) 

write_csv(winter_d_v_flourish,'winter_illness.csv')

num_total<-winter_d_v %>% 
  group_by(ft) %>% 
  summarise(across(where(is.numeric), sum, .names = "sum_{.col}"))


#To identify the peak 
max_week_df <- winter_d_v_flourish %>%
  group_by(ft) %>%
  filter(count == max(count)) %>%  # Select rows with the max count per group
  select(week, broad_metric, ft, count)   # Keep only relevant columns


winter_d_v %>% 
  mutate(week = factor(week, levels = d)) %>% 
  filter(ft %in% c("16/17","17/18","18/19","22/23", "23/24", "24/25")) %>%
  group_by(broad_metric,ft, week) %>% 
  arrange() %>% 
  ggplot(aes(x = week, y = d_v_beds_occupied, group = ft, colour = ft)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=8))+
  labs(x="Week", y="Count")+
  theme_THF() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))


# Flu --------------------------------------------------------------

#Total bed days for flu
num_total<-winter_illness %>% 
  filter(broad_metric=="FLU") %>% 
  group_by(ft, broad_metric) %>% 
  summarise(sum=sum(count))

#Peak bed days for flu
max_week_df <- winter_illness %>%
  group_by(broad_metric,ft) %>%
  filter(count == max(count)) %>%  # Select rows with the max count per group
  select(week, broad_metric, ft, count)   # Keep only relevant columns




#Load UKHSA data for flu
flu_admi<-readODS::read_ods(path = here::here('data', 'weekly_flu.ods') , sheet = 'Figure_28', skip = 3) 


flu_admi <-flu_admi %>% 
  clean_names() %>% 
  mutate(date=as.Date(date, format = "%d %B %Y")) %>% 
  mutate(isoweek=date2ISOweek(date)) %>%
  mutate(isoweek_short=str_sub(isoweek, 1,8)) %>% 
  mutate(week=str_sub(isoweek_short,-3)) %>% 
  mutate(ft=case_when(date %in%  as.Date(y16_17)~"16/17",
                      date %in% as.Date(y17_18)~"17/18",
                      date %in% as.Date(y18_19)~"18/19", 
                      date %in% as.Date(y19_20)~"19/20", 
                      date %in% as.Date(y20_21)~"20/21",
                      date %in% as.Date(y21_22)~"21/22", 
                      date %in% as.Date(y22_23)~"22/23", 
                      date %in% as.Date(y23_24)~ "23/24",
                      date %in% as.Date(y24_25)~"24/25")) %>% 
  filter(!is.na(ft)) %>% 
  mutate(metric="admission_rates")


#Load ICU and HDU admissions for flu
flu_admi_icu<-readODS::read_ods(path = here::here('data', 'weekly_flu.ods') , sheet = 'Figure_31', skip = 3) 


flu_admi_icu <-flu_admi_icu %>% 
  clean_names() %>% 
  mutate(date=as.Date(date, format = "%d %B %Y")) %>% 
  mutate(isoweek=date2ISOweek(date)) %>%
  mutate(isoweek_short=str_sub(isoweek, 1,8)) %>% 
  mutate(week=str_sub(isoweek_short,-3)) %>% 
  mutate(ft=case_when(date %in%  as.Date(y16_17)~"16/17",
                      date %in% as.Date(y17_18)~"17/18",
                      date %in% as.Date(y18_19)~"18/19", 
                      date %in% as.Date(y19_20)~"19/20", 
                      date %in% as.Date(y20_21)~"20/21",
                      date %in% as.Date(y21_22)~"21/22", 
                      date %in% as.Date(y22_23)~"22/23", 
                      date %in% as.Date(y23_24)~ "23/24",
                      date %in% as.Date(y24_25)~"24/25")) %>% 
  filter(!is.na(ft)) %>% 
  mutate(metric="ICU_admission_rates")
  
  winter_flu_admi<-rbind(flu_admi, flu_admi_icu)
  
winter_flu_admi %>% 
    mutate(week = factor(week, levels = d)) %>% 
    filter(ft %in% c("16/17","17/18","18/19","22/23", "23/24", "24/25")) %>%
    group_by(metric,ft, week) %>% 
    arrange() %>% 
    ggplot(aes(x = week, y = rate, group = ft, colour = ft)) +
    geom_line() +
    facet_grid(cols=vars(metric), scales="free")+
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size=8))+
    labs(x="Week", y="Rate per 100,000")+
    theme_THF() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
  

winter_flu_flourish<-winter_flu_admi %>% 
  mutate(count=ifelse(rate==0,NA,rate)) %>%
  mutate(week = factor(week, levels = d)) %>% 
  pivot_wider(id_cols = c(week,metric), names_from=ft, values_from=count) %>%
  ungroup() %>% 
  arrange(week) %>% 
  mutate(metric_label=ifelse(metric=="admission_rates","Flu hospital admission rates, per 100,000", "Flu ICU or HDU hospital admission rates, per 100,000"))


write_csv(winter_flu_flourish,'winter_flu.csv')



# RSV ---------------------------------------------------------------------

#Load UKHSA data for RSV
rsv_admi<-readODS::read_ods(path = here::here('data', 'weekly_flu.ods') , sheet = 'Figure_34') 
  
  
rsv_admi <-rsv_admi %>% 
    clean_names() %>% 
    slice(which(.[[1]] == "Date"):n()) %>% 
    row_to_names(., 1) %>% 
    clean_names() %>% 
    mutate(date=as.Date(date, format = "%d %B %Y")) %>% 
    mutate(isoweek=date2ISOweek(date)) %>%
    mutate(isoweek_short=str_sub(isoweek, 1,8)) %>% 
    mutate(week=str_sub(isoweek_short,-3)) %>% 
    mutate(ft=case_when(date %in% as.Date(y17_18)~"17/18",
                        date %in% as.Date(y18_19)~"18/19", 
                        date %in% as.Date(y19_20)~"19/20", 
                        date %in% as.Date(y20_21)~"20/21",
                        date %in% as.Date(y21_22)~"21/22", 
                        date %in% as.Date(y22_23)~"22/23", 
                        date %in% as.Date(y23_24)~ "23/24",
                        date %in% as.Date(y24_25)~"24/25")) %>% 
    filter(!is.na(ft)) %>% 
    mutate(metric="Winter 2024 to 2025 season data release", 
         metric_label= "RSV hospital admission rates, per 100,000") %>% 
  rename(count=rate) %>% 
  filter(ft %in% c("22/23", "23/24", "24/25"))
  


rsv_admi_past<-readODS::read_ods(path = here::here('data', 'weekly_flu2024.ods') , sheet = 'Figure_40', skip = 7) 


rsv_admi_past <-rsv_admi_past %>% 
  clean_names() %>% 
  mutate(across(starts_with("x"), as.numeric)) %>% 
  pivot_longer(-week_number, names_to="season", values_to="rate") %>% 
  mutate(
    #week = as.numeric(str_extract(week_number, "^[0-9]+")),
    season_year_start = as.numeric(str_extract(season, "(?<=x)\\d{4}")),
    season_year_end = as.numeric(str_extract(season, "\\d{4}$")),
    iso_date = ISOweek2date(paste0(
      ifelse(week_number >= 40, season_year_start, season_year_end),
      "-W", sprintf("%02d", week_number),
      "-1"
    ))) %>% 
  mutate(date=as.Date(iso_date, format = "%d %B %Y")) %>% 
  mutate(isoweek=date2ISOweek(date)) %>%
  mutate(isoweek_short=str_sub(isoweek, 1,8)) %>% 
  mutate(week=str_sub(isoweek_short,-3)) %>% 
  mutate(ft=case_when(date %in% as.Date(y17_18)~"17/18",
                      date %in% as.Date(y18_19)~"18/19", 
                      date %in% as.Date(y19_20)~"19/20", 
                      date %in% as.Date(y20_21)~"20/21",
                      date %in% as.Date(y21_22)~"21/22", 
                      date %in% as.Date(y22_23)~"22/23", 
                      date %in% as.Date(y23_24)~ "23/24",
                      date %in% as.Date(y24_25)~"24/25")) %>% 
  filter(!is.na(ft)) %>% 
  mutate(metric="Winter 2023 to 2024 season data release", 
         metric_label= "RSV hospital admission rates, per 100,000") %>% 
  rename(count=rate) %>% 
  filter(ft %in% c("17/18", "18/19"))


rsv_admi_past<-rsv_admi_past %>% 
  select(colnames(rsv_admi))

winter_rsv<-rbind(rsv_admi, rsv_admi_past)


winter_rsv_flourish<-winter_rsv %>% 
         mutate(count=ifelse(count==0,NA,count)) %>%
         mutate(week = factor(week, levels = d)) %>% 
         arrange(ft,week,metric) %>% 
         pivot_wider(id_cols = c(week,metric, metric_label), names_from=ft, values_from=count) %>%
         ungroup() %>% 
         arrange(week)


write_csv(winter_rsv_flourish,'winter_rsv.csv')

winter_rsv %>% 
  mutate(rate=as.numeric(count)) %>% 
  mutate(week = factor(week, levels = d)) %>% 
  filter(ft %in% c("16/17","17/18","18/19","22/23", "23/24", "24/25")) %>%
  group_by(metric,ft, week) %>% 
  arrange() %>% 
  ggplot(aes(x = week, y = rate, group = ft, colour = ft)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=8))+
  labs(x="Week", y="Rate per 100,000")+
  theme_THF() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))



# COVID -------------------------------------------------------------------

covid_admi<-readODS::read_ods(path = here::here('data', 'weekly_flu.ods') , sheet = 'Figure_24') 


covid_admi <-covid_admi %>%
  clean_names() %>% 
  slice(which(.[[1]] == "Date"):n()) %>% 
  row_to_names(., 1) %>% 
  clean_names() %>% 
  mutate(date=as.Date(date, format = "%d %B %Y")) %>% 
  mutate(isoweek=date2ISOweek(date)) %>%
  mutate(isoweek_short=str_sub(isoweek, 1,8)) %>% 
  mutate(week=str_sub(isoweek_short,-3)) %>% 
  mutate(ft=case_when(date %in% as.Date(y17_18)~"17/18",
                      date %in% as.Date(y18_19)~"18/19", 
                      date %in% as.Date(y19_20)~"19/20", 
                      date %in% as.Date(y20_21)~"20/21",
                      date %in% as.Date(y21_22)~"21/22", 
                      date %in% as.Date(y22_23)~"22/23", 
                      date %in% as.Date(y23_24)~ "23/24",
                      date %in% as.Date(y24_25)~"24/25")) %>% 
  filter(!is.na(ft)) %>% 
  mutate(metric="covid_admi_rate", 
         metric_label= "COVID hospital admission rates, per 100,000 (UKHSA)") %>% 
  rename(count=rate)


winter_covid_flourish<-covid_admi %>% 
  mutate(count=ifelse(count==0,NA,count)) %>%
  mutate(week = factor(week, levels = d)) %>% 
  pivot_wider(id_cols = c(week, metric_label), names_from=ft, values_from=count) %>%
  ungroup() %>% 
  arrange(week)

write_csv(winter_covid_flourish,'winter_covid.csv')


covid_admi %>% 
  mutate(rate=as.numeric(count)) %>% 
  mutate(week = factor(week, levels = d)) %>%
  group_by(metric,ft, week) %>% 
  arrange() %>% 
  ggplot(aes(x = week, y = rate, group = ft, colour = ft)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=8))+
  labs(x="Week", y="Rate per 100,000")+
  theme_THF() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))


# A&E diverts -------------------------------------------------------------

sheets_to_import <- c("A&E diverts", "A&E Diverts", "A&E closures", "A&E Closures")

#Get list of csv with these sheets 
list_file <- sheet_info_full %>%
  rowwise() %>%
  filter(any(sheets_to_import %in% sheet_name)) %>% 
  ungroup() %>%
  select(file_path) %>%
  pull()


#Need to change the sit rep read function because there is now just one column of data 
import_sitrep <- function(file, indicator){
  
  data <- readxl::read_xlsx(path = file, sheet = indicator, skip = 15, col_names = FALSE) 
  
  # Extract first header line containing dates and fill the gaps: 
  # Read 2 lines but guess the data types only from the first row
  # R will be looking for dates and convert the second row
  # to NA but the right length will be preserved. 
  
  header_1 <- readxl::read_xlsx(path = file, sheet = indicator, skip = 13, col_names = FALSE, n_max = 2, guess_max = 1)
  
  # Convert to columns, fill in the gaps and convert into vector
  header_1 <- header_1 %>% 
    t() %>% 
    as.data.frame %>% 
    mutate(V1=ifelse(is.na(`V1`),"placeholder",`V1`)) %>% 
    fill(.,'V1')

  header_1 <- as.character(header_1$V1)  
  
  # # Extract second header and convert into vector
  # header_2 <- readxl::read_xlsx(path = file, sheet = indicator, skip = 14, col_names = FALSE, n_max = 1)
  # header_2 <- unname(unlist(header_2[1,]))
  # 
  # # Concatenating headers to create column names
  # # Replace NAs with a placeholder, otherwise concatenation fails
  # column_names <- str_c(str_replace_na(header_1, "placeholder"), str_replace_na(header_2, "placeholder"), sep = "_")
  
  # Add column names to data and tidy
  names(data) <- tolower(header_1)
  names(data) <- gsub(" ", ".", names(data))
  names(data) <- gsub("placeholder_", "", names(data))
  names(data) <- gsub("'", "", names(data))
  names(data) <- gsub("<", "less.than", names(data))
  names(data) <- gsub(">", "more.than", names(data))
  
  # Tidy up table
  data_tidy <- data %>% 
    select(1:length(header_1))%>% 
    select(-placeholder) %>% 
    filter(!is.na(name)) %>% 
    # # Separate variables and dates
     gather(-1, -2, -3, key = "date_type", value = 'value') %>%
    # # convert to the right variable types
    mutate(date = as.Date(date_type)) %>% 
    mutate(value=as.numeric(value)) %>% 
    mutate(metric=indicator)

  data_tidy
}

# Apply the process to all files in list_file
Sitrep_daily_all_files <- list_file %>% 
  map(~ {
    # Identify sheets present in this file that are also in sheets_to_import
    sheets_in_file <- sheet_info_full %>%
      filter(file_path == .x) %>%
      pull(sheet_name)
    
    valid_sheets <- intersect(sheets_in_file, sheets_to_import)
    
    # Only process the sheets that are actually in the file
    Sitrep_daily_file <- valid_sheets %>%
      map(import_sitrep, file = .x) %>% 
      reduce(left_join, by = c("nhs.england.region", "code", "name", "date"))
    
    return(Sitrep_daily_file)
  })

# Now Sitrep_daily_all_files will contain a list of processed data frames for each file

Sitrep_daily_all <- Sitrep_daily_all_files %>%
  reduce(bind_rows) 

a_and_e <- Sitrep_daily_all %>% 
  filter(name == "ENGLAND" | name == "ENGLAND (All Acute Trusts)") %>% 
  pivot_longer(cols = c(metric.x, metric.y), 
               names_to = "metric_type", 
               values_to = "metric") %>% 
  pivot_longer(cols = c(value.x, value.y), 
               names_to = "value_type", 
               values_to = "value") %>% 
  filter(substring(metric_type, 8, 8) == substring(value_type, 7, 7)) %>% 
  select(-metric_type, -value_type) %>% 
  select(-c(date_type.x, date_type.y)) %>% 
  distinct() %>% 
  mutate(isoweek=date2ISOweek(date)) %>%
  mutate(isoweek_short=str_sub(isoweek, 1,8)) %>% 
  mutate(metric=tolower(metric)) %>% 
  mutate(ft=case_when(date %in% as.Date(y16_17)~"16/17",
                      date %in% as.Date(y17_18)~"17/18",
                      date %in% as.Date(y18_19)~"18/19", 
                      date %in% as.Date(y19_20)~"19/20", 
                      date %in% as.Date(y20_21)~"20/21",
                      date %in% as.Date(y21_22)~"21/22", 
                      date %in% as.Date(y22_23)~"22/23", 
                      date %in% as.Date(y23_24)~ "23/24",
                      date %in% as.Date(y24_25)~"24/25")) %>% 
  group_by(isoweek_short,ft,metric, country=name) %>%  
  summarise(across(where(is.numeric), sum, na.rm = TRUE))%>% 
  mutate(date=ISOweek2date(paste0(isoweek_short,"-1"))) %>% 
  mutate(week=str_sub(isoweek_short,-3)) %>% 
  filter(!is.na(ft)) %>% 
  mutate(country="ENGLAND")




a_and_e %>% 
   group_by(ft,metric) %>% 
  summarise(count=sum(value)) %>% 
  filter(metric=="a&e diverts") %>% 
  ggplot(aes(x = ft, y = count),) +
  geom_col(fill='#dd0031') +
  scale_colour_continuous_THF()+
  labs(x="winters", y="", title="")+
  theme_THF() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))


flourish_a_and_e_diverts<-a_and_e %>% 
  group_by(ft,metric) %>% 
  summarise(count=sum(value)) %>% 
  filter(metric=="a&e diverts") 

write.csv(flourish_a_and_e_diverts, 'flourish_a_and_e_diverts.csv') 


# Bed occupancy -----------------------------------------------------------

#Data download 
link<-'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/04/Beds-publication-Timeseries-March-2020-March-2025.xlsx'
destfile <- here::here('data', "bedoccup.xlsx")
curl_download(link, destfile = destfile)


#Data load
bedoccup<-readxl::read_excel(path = here::here('data', "bedoccup.xlsx") , sheet = 'Timeseries all acute trusts', skip = 12) 


#Processing and calculations
bedoccup<-bedoccup %>% 
  clean_names() %>% 
  mutate(date=as.Date(paste("28", month), format = "%d %B %Y")) %>% 
  select(date,g_a_beds_available) 

bedoccup_calc<-bedoccup %>% 
  mutate(ft=case_when( date %in% as.Date(y22_23)~"22/23", 
                       date %in% as.Date(y23_24)~ "23/24",
                       date %in% as.Date(y24_25)~"24/25")) %>% 

  filter(!is.na(ft)) %>% 
  group_by(ft) %>% 
  summarise(count=mean(g_a_beds_available))
    


# Delay discharges --------------------------------------------------------

#Data download
link<-'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/04/Daily-discharge-sitrep-timeseries-data-webfile-April2021-March2025.xlsx'
destfile <- here::here('data', "delayed_discharges.xlsx")
curl_download(link, destfile = destfile)




#Data load
dd<-readxl::read_excel(path =  here::here('data', "delayed_discharges.xlsx") , sheet = 'Daily Series', skip = 4) 

dd<-dd %>% 
  clean_names() %>% 
  mutate(date=as.Date(date,format = "%Y-%m-%d")) %>% 
  select(date,number_of_patients_remaining_in_hospital_who_no_longer_meet_the_criteria_to_reside)


#Calculations 


#Total bed days for delayed discharges 

y24_25<-format(as.Date(seq(lubridate::ymd('2024-11-01'),lubridate::ymd('2025-03-31'),by='1 day')),"%Y-%m-%d")

dd_calc<-dd %>% 
  filter(date %in% as.Date(y24_25)) %>% 
  summarise(count=sum(number_of_patients_remaining_in_hospital_who_no_longer_meet_the_criteria_to_reside))
#Total bed days for flu
flu_total<-winter_illness %>% 
  filter(broad_metric=="FLU") %>% 
  group_by(ft, broad_metric) %>% 
  summarise(sum=sum(count)) %>% 
  filter(ft=="24/25")

dd_calc$count/flu_total$sum



# Staff sickness and absence rate -----------------------------------------

link<-'https://files.digital.nhs.uk/1F/68672F/NHS%20Sickness%20Absence%20rates%2C%20December%202024.xlsx'
destfile <- here::here('data', 'staff_sickness.xlsx')
curl_download(link, destfile = destfile)


staff_sickness<-readxl::read_excel(path =  here::here('data', "staff_sickness.xlsx") , sheet = 'Table 1', skip = 2) 


y16_17<-format(as.Date(seq(lubridate::ymd('2016-11-01'),lubridate::ymd('2017-03-01'),by='1 day')),"%Y-%m-%d")
y17_18<-format(as.Date(seq(lubridate::ymd('2017-11-01'),lubridate::ymd('2018-03-01'),by='1 day')),"%Y-%m-%d")
y18_19<-format(as.Date(seq(lubridate::ymd('2018-11-01'),lubridate::ymd('2019-03-01'),by='1 day')),"%Y-%m-%d")
y22_23<-format(as.Date(seq(lubridate::ymd('2022-11-01'),lubridate::ymd('2023-03-01'),by='1 day')),"%Y-%m-%d")
y23_24<-format(as.Date(seq(lubridate::ymd('2023-11-01'),lubridate::ymd('2024-03-31'),by='1 day')),"%Y-%m-%d")

staff_sickness<-staff_sickness %>% 
  clean_names() %>% 
  mutate(date=as.Date(paste("01", month), format = "%d %B %Y")) %>% 
  select(date,england) %>% 
  mutate(ft=case_when(date %in% as.Date(y16_17)~"16/17",
                      date %in% as.Date(y17_18)~"17/18",
                      date %in% as.Date(y18_19)~"18/19",
                      date %in% as.Date(y22_23)~"22/23", 
                      date %in% as.Date(y23_24)~ "23/24")) %>% 
  filter(!is.na(ft)) %>% 
  group_by(ft) %>% 
  summarise(average=mean(england))

staff_sickness_calcs<-staff_sickness %>% 
  mutate(winter=ifelse(ft %in% c("22/23","23/24"),"post-covid","pre-covid")) %>% 
  group_by(winter) %>% 
  summarise(average=mean(average))

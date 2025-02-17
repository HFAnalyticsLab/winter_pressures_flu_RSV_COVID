
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


# sitreps -----------------------------------------------

# #2017-18
# link<-'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2018/03/Winter-data-Timeseries-20180304.xlsx'
# 
# destfile <- here::here('data', "raw2017.xlsx")
# curl_download(link, destfile = destfile)
# 
# #2018-19
# link<-'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2019/03/Winter-data-timeseries-20190307.xlsx'
# 
# destfile <- here::here('data', "raw2018.xlsx")
# curl_download(link, destfile = destfile)
# 
# #2019-20
# link<-'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/03/Winter-SitRep-Acute-Time-series-2-December-2019-1-March-2020.xlsx'
# 
# destfile <- here::here('data', "raw2019.xlsx")
# curl_download(link, destfile = destfile)
# 
# #2020-21
# link<-'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/04/UEC-Daily-SitRep-Acute-Web-File-Timeseries-1.xlsx'
# 
# destfile <- here::here('data', "raw2020.xlsx")
# curl_download(link, destfile = destfile)
# 
# #2021-22
# link<-'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/04/UEC-Daily-SitRep-Web-File-Timeseries.xlsx'
# 
# destfile <- here::here('data', "raw2021.xlsx")
# curl_download(link, destfile = destfile) 
# #Need to manually change the RSV sheet name to get rid of the trailing space 
# 
# 
# #2022-23
# link<-'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/04/UEC-Daily-SitRep-Web-File-Timeseries.xlsx'
# 
# destfile <- here::here('data', "raw2022.xlsx")
# curl_download(link, destfile = destfile)
# 
# #2023-24
# link<-'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2024/04/Web-File-Timeseries-UEC-Daily-SitRep.xlsx'
# 
# destfile <- here::here('data', "raw2023.xlsx")
# curl_download(link, destfile = destfile)
# 
# #2024-25 
# link<-'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/01/Web-File-Timeseries-UEC-Daily-SitRep-4.xlsx'
# 
# destfile <- here::here('data', "raw2024.xlsx")
# curl_download(link, destfile = destfile)
# 
# 
# 

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
    sheet_name = sheets,
    stringsAsFactors = FALSE
  )
})

# Combine the list into a single data frame
sheet_info_df <- bind_rows(sheet_info_list)

sheet_info_df <-sheet_info_df %>% 
  mutate(value=1) %>% 
  pivot_wider(
    names_from = sheet_name,
    values_from = value,
    values_fill = 0  # Fill missing values with 0
  ) %>% 
  clean_names()

# 
# # Initialize an empty list to store files with more than 7 sheets
# files_with_many_sheets <- c()
# 
# # Loop through each file and check the number of sheets
# for (file in excel_files) {
#   sheet_count <- length(readxl::excel_sheets(file))
#   
#   # Check if sheet count exceeds 7
#   if (sheet_count > 10) {
#     files_with_many_sheets <- c(files_with_many_sheets, file)
#   }
# }
# 
# # Print the result
# cat("Files with more than 10 sheets:\n")
# print(files_with_many_sheets)
# 

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


sheets_to_import <- c("Flu", "RSV","Adult D&V, Norovirus")





# Sitrep_daily <- sheets_to_import %>% 
#   map(import_sitrep, 
#       file = files_with_many_sheets) %>% 
#   reduce(left_join, 
#          by = c("nhs.england.region", "code", "name", "date"))


# Apply the process to all files in files_with_many_sheets
Sitrep_daily_all_files <- files_with_many_sheets %>% 
  map(~ {
    
    # Map the import_sitrep function to the sheets of the current file
    # Note: We pass the file path as an argument to import_sitrep inside map()
    Sitrep_daily_file <- sheets_to_import %>%
      map(import_sitrep, file = .x) %>% 
      reduce(left_join, by = c("nhs.england.region", "code", "name", "date"))
    
    return(Sitrep_daily_file)
  })

# Now Sitrep_daily_all_files will contain a list of processed data frames for each file

Sitrep_daily_all <- Sitrep_daily_all_files %>%
  reduce(bind_rows) 


england<-Sitrep_daily_all %>% 
  filter(str_detect(name,"ENGLAND")) %>% 
  mutate(isoweek=date2ISOweek(date)) %>%
  mutate(isoweek_short=str_sub(isoweek, 1,8)) %>% 
  group_by(isoweek_short,country=name) %>%  
  summarise(across(where(is.numeric), sum, na.rm = TRUE))%>% 
  mutate(date=ISOweek2date(paste0(isoweek_short,"-1"))) %>% 
  pivot_longer(-c(isoweek_short, date, country), names_to="metric", values_to="count")

# %>% 
#   mutate()
#   group_by(year_isoweek) %>% 
#   mutate(order=dense_rank(date)) %>% 


y21_22<-format(as.Date(seq(lubridate::ymd('2021-11-29'),lubridate::ymd('2022-04-03'),by='1 day')),"%Y-%m-%d")
y22_23<-format(as.Date(seq(lubridate::ymd('2022-11-14'),lubridate::ymd('2023-04-03'),by='1 day')),"%Y-%m-%d")
y23_24<-format(as.Date(seq(lubridate::ymd('2023-11-20'),lubridate::ymd('2024-03-31'),by='1 day')),"%Y-%m-%d")
y24_25<-format(as.Date(seq(lubridate::ymd('2024-11-25'),lubridate::ymd('2025-01-19'),by='1 day')),"%Y-%m-%d")


england<-england %>% 
  mutate(week=str_sub(isoweek_short,-3)) %>% 
  mutate(ft=case_when(date %in% as.Date(y21_22)~"21/22", 
                       date %in% as.Date(y22_23)~"22/23", 
                      date %in% as.Date(y23_24) ~ "23/24",
                     date %in% as.Date(y24_25)~"24/25")) %>% 
  mutate(metric_label= case_when(metric=="cc.flu.beds"~ "Critical Care Flu Beds", 
                                 metric=="g&a.flu.beds"~ "General and Acute Flu Beds", 
                                 metric=="adult.d&v.beds.closed"~ "Adult D&V beds closed", 
                                 metric=="adult.d&v.beds.closed.unocc"~ "Adult D&V beds Closed and Unoccupied", 
                                 metric=="paeds.rsv.beds.closed"~ "Paeds RSV Beds Closed", 
                                 metric=="paeds.rsv.beds.closed.unocc"~ "Paeds RSV Beds Closed and Unoccpied")) %>% 
  mutate(broad_metric=case_when(str_detect(metric,"flu")~"FLU", 
                                str_detect(metric, "d&v")~"D&V, Norovirus", 
                                str_detect(metric,"rsv")~"RSV"))

head(england)
  

england %>% 
  filter(country=="ENGLAND (All Acute Trusts)") %>% 
  ggplot(aes(x = date, y = count, group = metric_label, colour = metric_label)) +
  geom_line() +
  scale_x_date(
    breaks = england$date[seq(1, length(england$date), by = 4)],  # Show labels every 4th week
    labels = england$week[seq(1, length(england$week), by = 4)]   # Matching week labels
  ) +
  facet_grid(cols = vars(ft), rows=vars(broad_metric), scales = "free") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=8))



# Find peak dates for each metric_label
peak_data <- england %>%
  filter(country == "ENGLAND (All Acute Trusts)") %>%
  group_by(metric_label, ft) %>%
  filter(count == max(count, na.rm = TRUE)) %>%
  ungroup() %>%
  select(date, metric_label, ft, week, count)

# Updated ggplot with peak markers
england %>%
  filter(country == "ENGLAND (All Acute Trusts)") %>%
  filter(broad_metric=="FLU") %>% 
  mutate(metric_label = case_when(
    metric == "cc.flu.beds" ~ "Critical Care Flu Beds",
    metric == "g&a.flu.beds" ~ "General and Acute Flu Beds",
    metric == "adult.d&v.beds.closed" ~ "Adult D&V beds closed",
    metric == "adult.d&v.beds.closed.unocc" ~ "Adult D&V beds Closed and Unoccupied",
    metric == "paeds.rsv.beds.closed" ~ "Paeds RSV Beds Closed",
    metric == "paeds.rsv.beds.closed.unocc" ~ "Paeds RSV Beds Closed and Unoccupied"
  )) %>%
  mutate(broad_metric = case_when(
    str_detect(metric, "flu") ~ "FLU",
    str_detect(metric, "d&v") ~ "D&V, Norovirus",
    str_detect(metric, "rsv") ~ "RSV"
  )) %>%
  ggplot(aes(x = date, y = count, group = metric_label, colour = metric_label)) +
  geom_line() +
  # Add vertical lines for peak times
  geom_vline(data = peak_data, aes(xintercept = as.numeric(date), colour = metric_label), 
             linetype = "dashed", size = 0.8, show.legend = FALSE) +
  # Optionally annotate peaks with dates or labels
  geom_text(data = peak_data, aes(x = date, y = max(count, na.rm = TRUE), 
                                  label = format(date, "%Y-%m-%d")), 
            angle = 45, vjust = -0.5, size = 3, show.legend = FALSE) +
  scale_x_date(
    breaks = england$date[seq(1, length(england$date), by = 4)],
    labels = england$week[seq(1, length(england$week), by = 4)]
  ) +
  facet_grid(cols = vars(ft), rows = vars(broad_metric), scales = "free") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))



max_week_df <- england %>%
  filter(metric=="adult.d&v.beds.closed") %>% 
  group_by(metric, ft) %>%
  filter(count == max(count)) %>%  # Select rows with the max count per group
  select(week, metric, ft, count)   # Keep only relevant columns



num_total<-england %>% 
  filter(metric=="g&a.flu.beds") %>% 
  group_by(ft) %>% 
  summarise(sum=sum(count))



peak_data <- england %>%
  filter(country == "ENGLAND (All Acute Trusts)") %>%
  filter(broad_metric=="FLU") %>% 
  group_by(metric_label, ft) %>%
  filter(count == max(count, na.rm = TRUE)) %>%
  ungroup() %>%
  select(date, metric_label, ft, week, count) %>% 
  mutate(lab=paste0(week,": ",count))


england %>%
  filter(country == "ENGLAND (All Acute Trusts)") %>%
  filter(broad_metric == "FLU") %>% 
  ggplot(aes(x = date, y = count, group = metric_label, colour = metric_label)) +
  geom_line() +
  # Highlight peak points
  geom_point(data = peak_data, aes(x = date, y = count), size = 2, shape = 21, fill = "white") +
  # Annotate peaks using geom_label_repel
  ggrepel::geom_label_repel(
    data = peak_data, 
    aes(x = date, y = count, label = lab),
    size = 3, label.size = 0.2, label.padding = unit(0.15, "lines"),
    box.padding = 0.35, point.padding = 0.5, max.overlaps = 10,
    show.legend = FALSE
  ) +
  scale_x_date(
    breaks = england$date[seq(1, length(england$date), by = 4)],
    labels = england$week[seq(1, length(england$week), by = 4)]
  ) +
  facet_grid(cols = vars(ft), scales = "free") +
  labs(x="Week", y="Count", title="FLU")+
  theme_THF() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))




peak_data <- england %>%
  filter(country == "ENGLAND (All Acute Trusts)") %>%
  group_by(metric_label, ft) %>%
  filter(count == max(count, na.rm = TRUE)) %>%
  ungroup() %>%
  select(date,broad_metric, metric_label, ft, week, count) %>% 
  mutate(lab=paste0(week,": ",count))


england %>%
  filter(country == "ENGLAND (All Acute Trusts)") %>%
  ggplot(aes(x = date, y = count, group = metric_label, colour = metric_label)) +
  geom_line() +
  # Highlight peak points
  geom_point(data = peak_data, aes(x = date, y = count), size = 2, shape = 21, fill = "white") +
  # Annotate peaks using geom_label_repel
  ggrepel::geom_label_repel(
    data = peak_data, 
    aes(x = date, y = count, label = lab),
    size = 3, label.size = 0.2, label.padding = unit(0.15, "lines"),
    box.padding = 0.35, point.padding = 0.5, max.overlaps = 10,
    show.legend = FALSE
  ) +
  scale_x_date(
    breaks = england$date[seq(1, length(england$date), by = 4)],
    labels = england$week[seq(1, length(england$week), by = 4)]
  ) +
  facet_grid(cols = vars(ft), rows=vars(broad_metric), scales = "free") +
  labs(x="Week", y="Count", title="FLU")+
  theme_THF() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))


england %>%
  filter(country == "ENGLAND (All Acute Trusts)") %>%
  ggplot(aes(x = date, y = count, group = metric_label, colour = metric_label)) +
  geom_line() +
  # Highlight peak points
  geom_point(data = peak_data, aes(x = date, y = count), size = 2, shape = 21, fill = "white") +
  # Annotate peaks using geom_label_repel for visible metrics in each panel
  ggrepel::geom_label_repel(
    data = peak_data,
    aes(x = date, y = count, label = lab, colour = metric_label),
    inherit.aes = FALSE, 
    size = 3, label.size = 0.2, label.padding = unit(0.15, "lines"),
    box.padding = 0.35, point.padding = 0.5, max.overlaps = 10,
    show.legend = FALSE
  ) +
  scale_x_date(
    breaks = england$date[seq(1, length(england$date), by = 4)],
    labels = england$week[seq(1, length(england$week), by = 4)]
  ) +
  facet_grid(cols = vars(ft), rows = vars(broad_metric), scales = "free") +
  labs(x = "Week", y = "Count", title = "") +
  theme_THF() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))


num_total<-england %>% 
  filter(broad_metric=="FLU") %>% 
  group_by(ft,metric) %>% 
  summarise(sum=sum(count))


num_total_flu<-england %>% 
  group_by(ft, broad_metric) %>% 
  summarise(sum=sum(count))



max_week_df <- england %>%
  group_by(broad_metric,ft) %>%
  filter(count == max(count)) %>%  # Select rows with the max count per group
  select(week, broad_metric, ft, count)   # Keep only relevant columns


# System performance ------------------------------------------------------

sheets_to_import <- c("Beds Occ by long stay patients","Adult G&A beds", "Adult critical care")

# Sitrep_daily <- sheets_to_import %>% 
#   map(import_sitrep, 
#       file = files_with_many_sheets) %>% 
#   reduce(left_join, 
#          by = c("nhs.england.region", "code", "name", "date"))


# Apply the process to all files in files_with_many_sheets
Sitrep_daily_all_files <- files_with_many_sheets %>% 
  map(~ {
    
    # Map the import_sitrep function to the sheets of the current file
    # Note: We pass the file path as an argument to import_sitrep inside map()
    Sitrep_daily_file <- sheets_to_import %>%
      map(import_sitrep, file = .x) %>% 
      reduce(left_join, by = c("nhs.england.region", "code", "name", "date"))
    
    return(Sitrep_daily_file)
  })

# Now Sitrep_daily_all_files will contain a list of processed data frames for each file

Sitrep_daily_all <- Sitrep_daily_all_files %>%
  reduce(bind_rows) 




england<-Sitrep_daily_all %>% 
  filter(str_detect(name,"ENGLAND")) %>% 
  mutate(isoweek=date2ISOweek(date)) %>%
  mutate(isoweek_short=str_sub(isoweek, 1,8)) %>% 
  group_by(isoweek_short,country=name) %>%  
  summarise(across(where(is.numeric), sum, na.rm = TRUE))%>% 
  mutate(date=ISOweek2date(paste0(isoweek_short,"-1"))) %>% 
  mutate(prop_adult_GA_bed_occup=(`adult.g&a.beds.occd`/`adult.g&a.beds.open`)*100) %>% 
  mutate(prop_adult_CC_bed_occup=(`cc.adult.occ`/cc.adult.open)*100) %>% 
  mutate(week=str_sub(isoweek_short,-3)) %>% 
  mutate(ft=case_when(date %in% as.Date(y21_22)~"21/22", 
                      date %in% as.Date(y22_23)~"22/23", 
                      date %in% as.Date(y23_24) ~ "23/24",
                      date %in% as.Date(y24_25)~"24/25")) %>% 
  pivot_longer(-c(isoweek_short, date, country,week, ft), names_to="metric", values_to="count") %>% 
  mutate(metric_label= case_when(metric=="prop_adult_CC_bed_occup"~ "Critical Care Bed Occupancy", 
                                 metric=="prop_adult_GA_bed_occup"~ "General and Acute Bed Occupancy", 
                                 metric=="more.than.14.days"~ "More than 14 days", 
                                 metric=="more.than.21.days"~ "More than 21 days", 
                                 metric=="more.than.7.days"~ "More than 7 days")) %>% 
  mutate(broad_metric=case_when(str_detect(metric,"prop")~"Bed Occupancy", 
                                str_detect(metric, "more.than")~"Long stay patients"))



england %>%
  filter(country == "ENGLAND (All Acute Trusts)"& !is.na(metric_label)) %>%  
  ggplot(aes(x = date, y = count, group = metric_label, colour = metric_label)) +
  geom_line() +
  # Highlight peak points
  # geom_point(data = peak_data, aes(x = date, y = count), size = 2, shape = 21, fill = "white") +
  # Annotate peaks using geom_label_repel
  # ggrepel::geom_label_repel(
  #   data = peak_data, 
  #   aes(x = date, y = count, label = lab),
  #   size = 3, label.size = 0.2, label.padding = unit(0.15, "lines"),
  #   box.padding = 0.35, point.padding = 0.5, max.overlaps = 10,
  #   show.legend = FALSE
  # ) +
  scale_x_date(
    breaks = england$date[seq(1, length(england$date), by = 4)],
    labels = england$week[seq(1, length(england$week), by = 4)]
  ) +
  facet_grid(cols = vars(ft), rows=vars(broad_metric), scales = "free") +
  labs(x="Week", y="Count", title="FLU")+
  theme_THF() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))



england %>%
  filter(country == "ENGLAND (All Acute Trusts)"& broad_metric=="Bed Occupancy") %>%  
  ggplot(aes(x = date, y = count, group = metric_label, colour = metric_label)) +
  geom_line() +
  # Highlight peak points
  # geom_point(data = peak_data, aes(x = date, y = count), size = 2, shape = 21, fill = "white") +
  # Annotate peaks using geom_label_repel
  # ggrepel::geom_label_repel(
  #   data = peak_data, 
  #   aes(x = date, y = count, label = lab),
  #   size = 3, label.size = 0.2, label.padding = unit(0.15, "lines"),
  #   box.padding = 0.35, point.padding = 0.5, max.overlaps = 10,
  #   show.legend = FALSE
  # ) +
  scale_x_date(
    breaks = england$date[seq(1, length(england$date), by = 4)],
    labels = england$week[seq(1, length(england$week), by = 4)]
  ) +
  facet_grid(cols = vars(ft), scales = "free") +
  labs(x="Week", y="Count", title="Bed occupancy")+
  theme_THF() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))



england %>%
  filter(country == "ENGLAND (All Acute Trusts)"& broad_metric!="Bed Occupancy") %>%  
  ggplot(aes(x = date, y = count, group = metric_label, colour = metric_label)) +
  geom_line() +
  # Highlight peak points
  # geom_point(data = peak_data, aes(x = date, y = count), size = 2, shape = 21, fill = "white") +
  # Annotate peaks using geom_label_repel
  # ggrepel::geom_label_repel(
  #   data = peak_data, 
  #   aes(x = date, y = count, label = lab),
  #   size = 3, label.size = 0.2, label.padding = unit(0.15, "lines"),
  #   box.padding = 0.35, point.padding = 0.5, max.overlaps = 10,
  #   show.legend = FALSE
  # ) +
  scale_x_date(
    breaks = england$date[seq(1, length(england$date), by = 4)],
    labels = england$week[seq(1, length(england$week), by = 4)]
  ) +
  facet_grid(cols = vars(ft), scales = "free") +
  labs(x="Week", y="Count", title="Long stay patients")+
  theme_THF() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))




indicator= "A&E Diverts"
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
    mutate(value=as.numeric(value))

  data_tidy
}



# Apply the process to all files in files_with_many_sheets
Sitrep_daily_all_files <- files_with_many_sheets %>% 
  map(~ {
    
    # Map the import_sitrep function to the sheets of the current file
    # Note: We pass the file path as an argument to import_sitrep inside map()
    Sitrep_daily_file <- indicator %>%
      map(import_sitrep, file = .x) %>% 
      reduce(left_join, by = c("nhs.england.region", "code", "name", "date"))
    
    return(Sitrep_daily_file)
  })

# Now Sitrep_daily_all_files will contain a list of processed data frames for each file

Sitrep_daily_all <- Sitrep_daily_all_files %>%
  reduce(bind_rows) 

england<-Sitrep_daily_all %>% 
  filter(str_detect(name,"ENGLAND")) %>% 
  mutate(isoweek=date2ISOweek(date)) %>%
  mutate(isoweek_short=str_sub(isoweek, 1,8)) %>% 
  group_by(isoweek_short,country=name) %>%  
  summarise(across(where(is.numeric), sum, na.rm = TRUE))%>% 
  mutate(date=ISOweek2date(paste0(isoweek_short,"-1"))) %>% 
  mutate(week=str_sub(isoweek_short,-3)) %>% 
  mutate(ft=case_when(date %in% as.Date(y21_22)~"21/22", 
                      date %in% as.Date(y22_23)~"22/23", 
                      date %in% as.Date(y23_24) ~ "23/24",
                      date %in% as.Date(y24_25)~"24/25")) %>% 
  pivot_longer(-c(isoweek_short, date, country,week, ft), names_to="metric", values_to="count") 



england %>%
  filter(country == "ENGLAND (All Acute Trusts)") %>%  
  ggplot(aes(x = date, y = count, group = ft, colour = ft)) +
  geom_line() +
  # Highlight peak points
  # geom_point(data = peak_data, aes(x = date, y = count), size = 2, shape = 21, fill = "white") +
  # Annotate peaks using geom_label_repel
  # ggrepel::geom_label_repel(
  #   data = peak_data, 
  #   aes(x = date, y = count, label = lab),
  #   size = 3, label.size = 0.2, label.padding = unit(0.15, "lines"),
  #   box.padding = 0.35, point.padding = 0.5, max.overlaps = 10,
  #   show.legend = FALSE
  # ) +
  scale_x_date(
    breaks = england$date[seq(1, length(england$date), by = 4)],
    labels = england$week[seq(1, length(england$week), by = 4)]
  ) +
  facet_grid(cols = vars(ft), scales = "free") +
  labs(x="Week", y="Count", title="A&E diverts")+
  theme_THF() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))

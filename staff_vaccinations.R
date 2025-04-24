library(RODBC)
library(DBI)
library(tidyverse)
library(stringr)

### Get connection and query ------

conn <- odbcConnect('Facts_and_Dimensions',
                    uid = uid,
                    pwd = pwd)

# get all raw data from staff vacc
staff_vacc_query <- 'SELECT [Organisation_Code]
      ,[Staff_Group]
      ,[No_Involved_With_Direct_Patient_Care]
      ,[No_Involved_With_Direct_Patient_Care_Str]
      ,[Doses_Given]
      ,[Doses_Given_Str]
      ,[Period_Start]
      ,[Period_End]
      ,[Effective_Snapshot_Date]
      ,[DataSourceFileForThisSnapshot_Version]
      ,[Report_Period_Length]
      ,[Unique_ID]
      ,[AuditKey]
  FROM [Flu_Vacc_Uptake_Staff].[Staff_Group1]'
  
staff_vacc_raw <- sqlQuery(conn, staff_vacc_query)

# close the connection
odbcCloseAll()

# ---- initial checks----- 
# # QA -- check site codes as some are 'total' or larger regions
# check <- staff_vacc_raw %>% 
#   filter(Organisation_Code== "Total" | str_detect(Organisation_Code, "^HCW")) %>% 
#   distinct(Organisation_Code, Effective_Snapshot_Date) %>% 
#   group_by(year = year(Effective_Snapshot_Date), Organisation_Code) %>% 
#   summarise(n = n()) %>% 
#   ungroup() %>% 
#   pivot_wider(id_cols = Organisation_Code, names_from = year, values_from = n)
# 
# check_indiv_trusts <- staff_vacc_raw %>% 
#   filter(Organisation_Code != "Total" & str_detect(Organisation_Code, "^HCW", negate = T)) %>% 
#   distinct(Organisation_Code, Effective_Snapshot_Date) %>% 
#   group_by(year = year(Effective_Snapshot_Date), Organisation_Code) %>% 
#   summarise(n = n()) %>% 
#   ungroup() %>% 
#   pivot_wider(id_cols = Organisation_Code, names_from = year, values_from = n)

# ------ aggregate data to get proportions of staff vaccinated -------
# get proportion of staff vaccinated
# when excluding the 'Total' and 'HCW' ones, this aligns 2021 onward with the summary releases
# 2020 and earlier only match when including them
# this selectively codes to align with official releases:
staff_vacc_aggregate_21onward <- staff_vacc_raw %>% 
  group_by(Staff_Group, Period_Start, Period_End, Effective_Snapshot_Date) %>% 
  filter(Effective_Snapshot_Date > ymd('2021-03-01')) %>% 
  filter(Organisation_Code != "Total" & str_detect(Organisation_Code, "^HCW", negate = T)) %>%  
  summarise(total_doses = sum(Doses_Given, na.rm = T), 
            total_n = sum(No_Involved_With_Direct_Patient_Care, na.rm = T),
            prop_vacc = total_doses/total_n) %>% 
  mutate(Staff_Group = as.factor(Staff_Group))

staff_vacc_aggregate_pre21 <- staff_vacc_raw %>% 
  group_by(Staff_Group, Period_Start, Period_End, Effective_Snapshot_Date) %>% 
  filter(Effective_Snapshot_Date <= ymd('2021-03-01')) %>% 
  summarise(total_doses = sum(Doses_Given, na.rm = T), 
            total_n = sum(No_Involved_With_Direct_Patient_Care, na.rm = T),
            prop_vacc = total_doses/total_n) %>% 
  mutate(Staff_Group = as.factor(Staff_Group))

staff_vacc_aggregate <- rbind(staff_vacc_aggregate_pre21, staff_vacc_aggregate_21onward)

# # ------ qa checks -------
# staff_vacc_aggregate %>% 
#   group_by(Staff_Group) %>% 
#   summarise(min_date = min(Effective_Snapshot_Date), 
#             max_date = max(Effective_Snapshot_Date), 
#             min_period_st = min(Period_Start), 
#             max_period_st = max(Period_Start)) %>% 
#   filter(str_detect(tolower(Staff_Group), 'all front')) %>% 
#   arrange(min_date)
# 
# # check all years there
# staff_vacc_aggregate %>% 
#   filter(str_detect(tolower(Staff_Group), 'all front')) %>% 
#   mutate(date = as.factor(Effective_Snapshot_Date)) %>% 
#   ungroup() %>% 
#   dplyr::select(-Staff_Group, -Period_Start) %>% 
#   distinct(date) %>% 
#   print(n = 40)
# 
# sum(staff_vacc_aggregate$Period_End != staff_vacc_aggregate$Effective_Snapshot_Date) 
# # period end always matches effective snapshot

# ------ prep data for graphing -------- 

nonwinter <- c('Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug')
winter <- c('Sep', 'Oct', 'Nov', 'Dec', 'Jan', 'Feb')

all_frontline <- staff_vacc_aggregate %>% 
  filter(str_detect(tolower(Staff_Group), 'all front')) %>% 
  mutate(year = str_sub(quarter(Effective_Snapshot_Date, with_year = TRUE, fiscal_start = 4), 1, 4) # year needs to be FY rather than CY 
         , month = month(Effective_Snapshot_Date, label = T, abbr = T)) %>% 
  mutate(month = fct_drop(month, nonwinter)) %>%   # make month an ordered factor - drop other months
  mutate(month = fct_relevel(month, winter)) %>%   # make month an ordered factor - go from sept - feb
  mutate(Staff_Group = 'All frontline HCWs') %>% 
  mutate(fiscal_year = as.factor(paste(as.numeric(year)-1, year, sep = '-'))) # make year label nicer

# ------- graph all years as individual lines ----- 
plot <- all_frontline %>% 
  ggplot() +
  geom_line(aes(x = month, y = prop_vacc, group = fiscal_year, colour = fiscal_year), size = 1) +
  ggtitle('Proportion of all frontline HCWs vaccinated for flu - needs QA') +
  ylab('Proportion vaccinated') +
  xlab('Month') +
  labs(colour = 'Fiscal year winter') +
  theme_minimal()

# ------- prep data for use in flourish ------ 
flourish <- all_frontline[all_frontline$month != 'Sep', c(7:10)]

flourish <- flourish %>% 
  pivot_wider(id_cols = month, names_from = fiscal_year, values_from = prop_vacc)

write_csv(flourish, 'staff_vaccines_flourish.csv')

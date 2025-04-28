##Data download

#Library 
library(curl)
library(here)

#Data Download

#Need to create a 'data' folder in your working directory before this can work

# raw files are in my data folder 


# Ambulance handover delays -----------------------------------------------

#2017
link<-'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2018/03/Winter-data-Timeseries-20180304.xlsx'

destfile <- here::here('data', "raw2017handovers.xlsx")
curl_download(link, destfile = destfile)

#2018
link<-'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2019/03/Winter-data-timeseries-20190307.xlsx'

destfile <- here::here('data', "raw2018handovers.xlsx")
curl_download(link, destfile = destfile)

#2019
link<-'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/03/Winter-SitRep-Acute-Time-series-2-December-2019-1-March-2020.xlsx'

destfile <- here::here('data', "raw2019handovers.xlsx")
curl_download(link, destfile = destfile)

#2020
link<-'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/04/UEC-Daily-SitRep-Acute-Web-File-Timeseries-1.xlsx'

destfile <- here::here('data', "raw2020handovers.xlsx")
curl_download(link, destfile = destfile)

#2021
link<-'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/04/UEC-Daily-SitRep-Web-File-Timeseries.xlsx'

destfile <- here::here('data', "raw2021handovers.xlsx")
curl_download(link, destfile = destfile)

#2022
link<-'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/04/Ambulance-Collection-Web-File-Timeseries.xlsx'

destfile <- here::here('data', "raw2022handovers.xlsx")
curl_download(link, destfile = destfile)

#2023
link<-'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2024/04/Web-File-Timeseries-Ambulance-Collection.xlsx'

destfile <- here::here('data', "raw2023handovers.xlsx")
curl_download(link, destfile = destfile)

# note for additional years add them in here

#2024
link<-'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/02/Web-File-Timeseries-Ambulance-Collection-1.xlsx'
destfile <- here::here('data', "raw2024handovers.xlsx")
curl_download(link, destfile = destfile)


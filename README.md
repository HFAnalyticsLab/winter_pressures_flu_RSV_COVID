# Did the NHS experience record pressures this winter?

## Project Status: Ongoing

## Project Description

Using routinely collected and publicly available data on health service perfromance, we explore how bad this winter and present possible factors that contributing to winter pressures. Our analysis mainly compares this winter (2024/25) with the previous two winters (2022/23 and 2023/24) and, where possible, to the three full winters before the COVID-19 pandemic (2016/17, 2017/18 and 2018/19) or where necessary or appropriate we also looked back to 2010 or during COVID winters. This means that we priorised metrics that alowed us to compare trends, however, this was not always possible or the time series was not always straight forward to construct. 
## Outputs

* Health Foundation publication 
* To aid others who may want to construct time series using the urgent and emergency care sit rep data, we have included in this repository an [excel sheet](https://github.com/HFAnalyticsLab/winter_pressures_flu_RSV_COVID/blob/main/Metrics%20from%20SitReps.xlsx) that reports the different metrics available for the different years of the urgent and emergency care sit rep data. 

## Data sources:

This repository only includes code for the charts in the above publication and is not an extensive reference list for the publication.

* [Urgent and Emergency Care Daily Situation Reports](https://www.england.nhs.uk/statistics/statistical-work-areas/uec-sitrep/)
* [A&E waiting times](https://www.england.nhs.uk/statistics/statistical-work-areas/ae-waiting-times-and-activity/)
* [Ambulance quality indicators](https://www.england.nhs.uk/statistics/statistical-work-areas/ambulance-quality-indicators/)
* [National flu and COVID-19 surveillance report](https://www.gov.uk/government/collections/weekly-national-flu-reports)
* [Critical care and general acute beds- urgent and emergency care daily situation reports](https://www.england.nhs.uk/statistics/statistical-work-areas/bed-availability-and-occupancy/critical-care-and-general-acute-beds-urgent-and-emergency-care-daily-situation-reports/)
* [NHS workforce statistics](https://digital.nhs.uk/data-and-information/publications/statistical/nhs-workforce-statistics/december-2024)
* [COVID-19 Hospital Activity](https://digital.nhs.uk/data-and-information/publications/statistical/nhs-workforce-statistics/december-2024)
* [NHS staff sickness absence rates]([https://digital.nhs.uk/data-and-information/publications/statistical/nhs-sickness-absence-rates/april-2020-provisional-statistics](https://digital.nhs.uk/data-and-information/publications/statistical/nhs-sickness-absence-rates))    

## How does it work? 

This repository outlines how to download the data, process, analyse and visualise the data used in the publication. **The analysis can be reproduced using the latest data by updating the data source links, updating the data range for some of the sources and creating a 'data' folder within your projects main folder.** 

It must be noted that we use AWS S3 cloud storage therefore where necessary the code for saving and retrieving files will need to be adapted based on your local storage structure. 

### Requirements 

These scripts were written in R version 4.0.2 and RStudio Workbench Version 1.1.383. The following R packages (available on CRAN) are needed:

* here
* curl
* tidyverse
* readxl
* broom
* lubridate
* ggplot2
* ISOweek
* ggrepel
* THFstyle

In addition our plots make use of our in house style package [THFstyle](https://github.com/THF-evaluative-analytics/THFstyle) available here on GitHub.

### Getting started

## Authors
* Anne Alarilla - [Twitter](https://twitter.com/AlarillaAnne) - [GitHub](https://github.com/annealarilla)
* Francesca Cavallaro
* Melissa Co
* Hollie Miller

Project team also consisted of Tim Garnder , Josh Keith and Charles Tallack. 

## License

This project is licensed under the [MIT License](https://github.com/HFAnalyticsLab/ambulance_and_emergency_care/blob/main/LICENSE).


## Acknowledgements

Code from [Ambulance and Emergencey care](https://github.com/HFAnalyticsLab/ambulance_and_emergency_care) and [Winter pressures](https://github.com/HFAnalyticsLab/Winter_pressures/blob/85313135c7dee393f52fa47596f04eb390bc43a3/winter_pressures_analysis.R#L4) was adapted for this respository.  







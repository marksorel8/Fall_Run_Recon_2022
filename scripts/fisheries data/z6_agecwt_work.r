library(tidyverse)
library(readxl)
library(lubridate)

#path <- "S:/Reg5/DO/Fall Chinook/2019 Run Reconstruction-2020 Forecasting/Data Received/ODFW/Fisheries/"
path <- "Data/"
z6_dat <- read_xlsx(paste0(path, "2021 Zone 6 CHF Scale Aging BioData Report.xlsx"),sheet="2021 Treaty CHF",.name_repair=function(x)str_replace_all(x,"\\s","_"))


z6 <- z6_dat %>%  
  mutate(Date=mdy(Date),BroodYear=as.numeric(Brood_Year),Fishery="Z6") %>% 
  select(Date,Week, Age, SC, Weight, Length, Mark, VSI, TagCode=Tag_Code, BroodYear,Fishery) %>% 
  mutate(scaleAge=str_sub(Age,1,1) %>% str_extract("\\d") %>% parse_number() %>% na_if(9),
         CWTAge=year(Date) - BroodYear,
         Age_grp="Z6") # add in the group variable for the Ager (to make correction matrix for age comp)

 
  z6_cwt <-   z6 %>% 
     filter(!(TagCode %in% c(NA, "NT", "TL"))) %>%  
     group_by(Fishery,VSI,Date,Length,Week,TagCode) %>% 
     summarize(n_tags=n())

  z6_age <- z6 %>% select(Date,Week,Length,Weight,SC, VSI,Mark, Fishery, scaleAge, CWTAge,Age_grp)

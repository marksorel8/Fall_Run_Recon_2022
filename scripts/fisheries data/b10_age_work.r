# library(tidyverse)
# library(readxl)
# library(lubridate)

#source("rr_functions.r")

path <- "Data/"

or_b10_ages <- read_xlsx(paste0(path,"2021 OR B10 Scale Card Biodata.xlsx"),sheet="Scale Card Data, Age, and CWT") %>% 
  select(Week=`Stat Week`, Date, Length, scaleAge=`Total Age`, BroodYear=`Brood Year`) %>%
  mutate(BroodYear=as.numeric(BroodYear),Date=ymd(Date), CWTAge=year(Date)-BroodYear, scaleAge=na_if(scaleAge,9),State="OR") %>% 
  mutate(scaleAge=as.numeric(scaleAge))

wa_b10_ages <- read_xlsx(paste0(path,"Buoy 10 Scale Ages 2021.xlsx"),sheet="Sheet1") %>% 
  select(Week=`Sat Week`,Date, Length, scaleAge=`Scale Age`,CWTAge=`CWT Age`,`Snout ID`) %>% #,BroodYear=`Brood Year`) %>% 
  #add brood year data from sheet 2 based on CWT. Could draw data from b10_cwt_work instead of sheet 2 in future.
 left_join(read_xlsx(paste0(path,"Buoy 10 Scale Ages 2021.xlsx"),sheet="Sheet2") %>% select(`SnoutID`,`BroodYear`), b=c("Snout ID"="SnoutID")) %>% select(-`Snout ID`) %>% 
  
  mutate(Date=date(Date),
         scaleAge=str_sub(scaleAge,1,1) %>% as.numeric(),
         CWTAge=year(Date)-BroodYear,
         State="WA") %>% 
  mutate(scaleAge=na_if(scaleAge,8))

# Split the age data into periods, will need to change the dates below. The split was on 8/16 in 2019.
b10_age <- bind_rows(or_b10_ages, wa_b10_ages) %>% 
  mutate(Age_grp="B 10") %>% # add in the  Fishery and group variable for the Ager (to make correction matrix for age comp)
  filter(!is.na(scaleAge)) %>% 
  # mutate(Period=1) #NO PERIODS in 2020
  mutate(Period=if_else(Date < date("2021-08-11"),1,2),
         Fishery=ifelse(Period==1,"B 10 MS","B 10"))


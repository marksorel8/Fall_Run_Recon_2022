library(tidyverse)
library(readxl)
library(lubridate)
library(RSocrata)

#path <- "S:/Reg5/DO/Fall Chinook/2019 Run Reconstruction-2020 Forecasting/Data Received/ODFW/Fisheries/"
path <- "Data/"

# OR B10 CWTs # NEED TO CONVERT DATES WITH TEXT TO COLUMNS
#hung up on brood year
or_b10_cwt <- read_xlsx(path=paste0(path,"2021 OR B10 CWT and Mark Sample Summary - Final.xlsx"), sheet="B10 Chinook") %>% 
  #reformat weird date things. some read in in format "m/d/y" others as an integer for days since as 1899-12-30
  mutate(Date=case_when(
    grepl("/",Date)~lubridate::mdy(Date), # does date have a "/" in it 
    TRUE~ as.Date(as.numeric(Date), origin = "1899-12-30"))) %>%  #if in format "days since 12/30/1988
select(Date, Mark, TagCode=CWT,Length, BroodYear = `Release Brood Year`) %>% 

  mutate(BroodYear=as.numeric(BroodYear)) %>% 
  
  filter(!(TagCode %in% c(NA, "NT", "TL")))


wa_b10_cwt <- read.socrata("https://data.wa.gov/resource/auvb-4rvk.json?locationname=1A (BUOY10 - BRIDGE)&returnyear=2021&species=Chinook") %>% 
  filter(!is.na(tagcode)) %>% select(tagcode,recoverydate,forklength_cm, mark, baglabel) %>% 
  transmute(TagCode=tagcode,Length=as.numeric(forklength_cm), 
            Date=lubridate::date(recoverydate), 
            Mark=mark,
            SnoutID=baglabel) %>% 
  as_tibble()


  #Combine B10 CWTs
 b10_cwt <-  bind_rows(or_b10_cwt, wa_b10_cwt) %>%
   # mutate(Period=1) %>% # NO PERIODS in 2020
  mutate(Period=if_else(Date<date("2021-08-11"),1,2),
         Fishery=ifelse(Period==1,"B 10 MS","B 10")) %>%
   group_by(Period,Mark,Length,TagCode,Fishery) %>% 
   summarise(n_tags=n()) %>% 
   mutate(TagCode=ifelse(str_length(TagCode)==5,paste0("0",TagCode),TagCode)) %>% 
   arrange(TagCode)

   
# NEED TO FIND OUT IF THERE ARE WA SCALE AGES FOR B10
  

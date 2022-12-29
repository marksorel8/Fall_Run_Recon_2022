library(tidyverse)
library(readxl)
library(lubridate)
#source("rr_functions.r")

path <- "Data/"
# Filenames for select area biodata (age and cwts)
#dr_file <- ""
tp_file <- "TP2021.xlsx"
yb_file <- "YB2021.xlsx"  
bs_file <- "BS2021.xlsx" 

# Deep river
# UNREADABLE SCALES ARE 88 in this file but 9 in every other file ARRRRRRGH!
# dr_dat <- read_xlsx(path=paste0(path, dr_file), sheet="2019 Deep River Chinook Biodata") %>% 
#   select(Date, Week, Length, VSI, Mark, TagCode=`Tag Code`, Age=`Age Read`,CWTAge=`Age Actual (CWT)`) %>% 
#   mutate(scaleAge=str_sub(Age,1,1) %>% str_extract("\\d") %>% parse_number() %>% na_if(8),
#          CWTAge=str_sub(CWTAge,1,1) %>% str_extract("\\d") %>% parse_number() %>% na_if(8),
#          Date=mdy(Date),
#          Fishery="DEEP RIVER")

# TP Select Areas
tp_dat <- read_xlsx(path=paste0(path, tp_file), sheet="CWTFR352 Scale Aging BioData Re") %>% 
  rename(TagCode=`Tag Code`) %>% 
  mutate(Fishery="TONGUE PT.", 
         Date=mdy(Date),
        BroodYear=as.numeric(`Brood Year`),
        scaleAge=str_sub(Age,1,1) %>% str_extract("\\d") %>% parse_number() %>% na_if(9),
        CWTAge=year(Date)-BroodYear)

yb_dat <- read_xlsx(path=paste0(path, yb_file),sheet="CWTFR352 Scale Aging BioData Re") %>% 
  rename(TagCode=`Tag Code`) %>% 
  mutate(Fishery="YOUNGS BAY",
         Date=mdy(Date),
         BroodYear=as.numeric(`Brood Year`),
         scaleAge=str_sub(Age,1,1) %>% str_extract("\\d") %>% parse_number() %>% na_if(9),
         CWTAge=year(Date) - BroodYear)

# Blind slough
bs_dat <- read_xlsx(path=paste0(path, bs_file),sheet="CWTFR352 Scale Aging BioData Re") %>%
  rename(TagCode=`Tag Code`) %>% 
  mutate(Fishery="BLIND SLOUGH/KNAPPA SLOUGH",
         Date=mdy(Date),
         BroodYear=as.numeric(`Brood Year`),
         scaleAge=str_sub(Age,1,1) %>% str_extract("\\d") %>% parse_number() %>% na_if(9),
         CWTAge=year(Date) - BroodYear)
#dr_dat,
select_areas <- bind_rows(tp_dat,yb_dat,bs_dat) %>% 
  select(Date,Week,Length,VSI,Mark,TagCode,scaleAge,CWTAge,Fishery)

select_area_cwt <- select_areas %>% filter(!is.na(TagCode)) %>% 
  group_by(Date,Week,Length,Mark,TagCode,VSI, Fishery) %>%  summarise(n_tags=n()) %>% ungroup()


select_areas_age <- select_areas %>% 
  mutate(Age_grp="SA") %>%  
  select(Fishery, Age_grp, Date,Week,Length,VSI,Mark,scaleAge,CWTAge)


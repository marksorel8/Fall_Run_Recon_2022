 library(tidyverse)
 library(readxl)
 library(lubridate)

# Path to data files
path <- "Data/"
file <- "2021 Fall Sport Ages Final.xlsx"

# Read in bio data (no ages), name repair replaces spaces with _
# SHEET NAMES WILL LIKELY CHANGE YEAR TO YEAR
# WATCH OUT FOR TRAILING COLUMNS WITH EMPTY HEADER NAMES, deleted manually in 2020
bio_dat <- read_xlsx(paste0(path,file), sheet="All biodata Aug, Sep, Oct",.name_repair=function(x)str_replace_all(x,"\\s","_")) %>% select(-last_col()) %>% mutate(BroodYear=as.numeric(Brood_Year)) %>% select(-Brood_Year)
age_dat <- read_xlsx(paste0(path,file), sheet="All age data wSCs Aug, Sep, Oct") %>%
   rename_all(~str_replace_all(.x, "\\s", "_")) %>% #,.name_repair=function(x)str_replace_all(x,"\\s","_")) %>%
   mutate(BroodYear=as.numeric(Brood_Year), 
          SC=as.character(SC)) %>% 
   select(Record_Id, Year, SC, Age, BroodYear)
section_lu <- read_xlsx(paste0(path,file), sheet="section_lu")


# Join up ages
lcr_sport <- bio_dat %>% left_join(age_dat, by="Record_Id") %>%
   #Clean Ages
   mutate(scaleAge=str_sub(Age,1,1) %>% str_extract("\\d") %>% parse_number() %>% na_if(9),
    #Calculate CWT Age
          CWTAge = Year.x - BroodYear.x) %>% 
   left_join(section_lu, by="Section") %>% 
     # Fix dates
    mutate(Date=mdy(paste0(Date,"/", Year.x))) %>% 
   # Keep columns needed
   select(Week,Date,Species,Mark=Fin_Clip,VSI,Length,Fishery,TagCode=Tag_Code,SC,scaleAge,CWTAge)


#   # Tag data for stock comp, filter out NAs NTs and TLs
lcr_sport_cwt <-  lcr_sport %>%
     filter(!(TagCode %in% c(NA, "NT", "TL"))) %>%
     group_by(Fishery,Date,Length,Mark,VSI,TagCode) %>%
     summarize(n_tags=n())

lcr_sport_age <- lcr_sport %>% 
   select(Fishery, Week, Date, Length, Mark, SC, scaleAge,CWTAge) %>% 
   mutate(Age_grp="LCR_sport",
          scaleAge=if_else(scaleAge==8,NA_real_,scaleAge)) # add in the group variable for the Ager (to make correction matrix for age comp)


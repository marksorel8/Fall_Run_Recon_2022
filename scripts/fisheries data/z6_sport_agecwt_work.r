library(readxl)
library(tidyverse)
library(lubridate)

path <- "Data/"
file <- "2021 Fishery 40 CWT data.xlsx"

z6_sport <- 
  # FIELD NAMES HAD CARRIAGE RETURNS AND NEWLINES in 2020. USE REGEX in .name_repair to remove.
  read_xlsx(paste0(path,file),sheet="CWTFR038 RC by SNID",.name_repair=function(x)str_replace_all(x, "[\r\n]" , "")) %>% #filter(is.na('Recovery Date'))
  # RECOVERY DATE WAS FORMATTED AS EXCEL DATE-TIMES IN 2020, was character in 2019.
    mutate(`Recovery Date`= mdy(`Recovery Date`)) %>% 
    rename(Date=`Recovery Date`,
           Mark=`Mark Code`,
           Length_mm=`Length (mm)`,
           Species=RecoverySpecies,
           BroodYear=`Release Brood Year`) %>% 
  select(Date, Mark, Length_mm, Species, CWT,BroodYear) %>% 
  mutate(Fishery="Bonn-Hwy395",
         CWTAge=year(Date)-as.numeric(BroodYear),
         # NEED THIS TO TRICK THE CORRECTION FUNCTION
         scaleAge=NA_real_) %>% 
  filter(!CWT %in% c("NT","TL","09BLANK",NA),Mark=="AD",
         Species=="Chinook", month(Date)>=8)


z6_sport_age <- z6_sport %>% 
  select(Date,Mark,Length=Length_mm,Fishery,CWTAge,scaleAge) %>% 
  mutate(Age_grp="Z6_sport") # add in the group variable for the Ager (to make correction matrix for age comp)

z6_sport_cwt <- z6_sport %>% select(Date,Mark,Length=Length_mm,Fishery,TagCode=CWT) %>% mutate(n_tags=1)


# library(tidyverse)
# library(readxl)
# library(lubridate)


# Path to data files
#path <- "S:/Reg5/DO/Fall Chinook/2019 Run Reconstruction-2020 Forecasting/Data Received/ODFW/Fisheries/"
path <- "Data/"
file <- "2021 LCR zone 1-5 scale aging complete.xlsx" # DELETE EMPTY FIELDS TO RIGHT OF TABLE

lcr_comm <- read_xlsx(paste0(path, file),sheet="CWTFR352 Scale Aging BioData Re",
                      .name_repair=function(x)str_replace_all(x,"\\s","_")) %>% 
  mutate(Date=mdy(Date),
         BroodYear=as.numeric(Brood_Year),
         Zone=Primary__Zone, 
         Fishery=ifelse(Zone >3,"Z 4-5 Gillnet", "Z 1-3 Tanglenet"),
         TagCode=Tag_Code,
         scaleAge=str_sub(Age, 1, 1) %>% str_extract("\\d") %>% parse_number() %>% na_if(9),
    #Calculate CWT Age
          CWTAge=year(Date) - BroodYear,
    Age_grp="LCR_comm") %>% # add in the group variable for the Ager (to make correction matrix for age comp)
  select(Week, Date, Fishery, Zone, SC, scaleAge, CWTAge, Length,  Weight, VSI, Mark, TagCode, Age_grp)



lcr_comm_cwt <-   lcr_comm %>%
     filter(!(TagCode %in% c(NA, "NT", "TL"))) %>%
     group_by(Fishery, Week, Mark, VSI, Length, TagCode) %>%
     summarize(n_tags=n())

lcr_comm_age <- lcr_comm %>% select(Fishery, Week, Length, SC, scaleAge, CWTAge, VSI, Mark, Age_grp)

#lcr_comm %>% make_corr_matrix()


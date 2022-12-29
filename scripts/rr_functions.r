library(tidyverse)
library(lubridate)
library(readxl)


# Gather age and CWT data
# Returns a list with 2 elements: age_data and cwt
get_data <- function(files= c("sport_work.r"
                              , "z6_agecwt_work.r"
                              , "b10_cwt_work.r"
                              , "b10_age_work.r"
                              , "select_areas_work.r"
                              , "lcr_comm_work.r"
                              , "z6_sport_agecwt_work.r"
                              , "drano_agecwt.r")){

#files <- c("sport_work.r", "z6_work.r", "b10_work.r", "select_areas_work.r", "lcr_comm_work.r") 

# Local environment within function, so items sourced in walk are available to bind_rows but not dumped into global env
tmp <- environment()

# suppressPackageStartupMessages(library(tidyverse))
# library(readxl)
# suppressPackageStartupMessages(library(lubridate))

#source("rr_functions.r", local=tmp)

# Run the work scripts in the local environment (i.e., within this function)
 walk(files, source, local=tmp)

# Bind the cwt data into 1 table
list(cwt_data=bind_rows(lcr_comm_cwt, lcr_sport_cwt, z6_cwt, b10_cwt, select_area_cwt, z6_sport_cwt,drano_cwt),

# Bind the age data into 1 table
age_data=bind_rows(lcr_comm_age, lcr_sport_age, z6_age, b10_age, select_areas_age,z6_sport_age,drano_age))

}

# Gather tidy catch data
# Filenames in this function will need to be updated for each year if the file and/or sheet names change.
gather_catch <- function(path="Data/"){
# library(tidyverse)
# library(lubridate)
# library(readxl)

#source("z6_landings.r")

#path <- "S:/Reg5/DO/Fall Chinook/2019 Run Reconstruction-2020 Forecasting/Data Received/ODFW/Fisheries/"
#path <- "Data/"

# Used props from daily estimate spreadsheet to split the total catch in week 33 for B10 Period 1/2
#b10_period_split <- tibble(Fishery="B 10", Week="33", KeptProp=.48, RelProp=.55)

# TP-WR, WR-BON catch 
sport_catch <- read_xlsx(paste0(path, "2021 tidySportCatch.xlsx"), sheet="Catch_LCR_sport") %>% 
  mutate(Period=case_when(is.na(Week) ~ 1,
                          TRUE~Week))
 # mutate(Period=case_when(Fishery=="B 10" & Week < 33 ~ 1,
 #                         Fishery=="B 10" & Week > 33 ~ 2)) 

# Split the week 33 B10 Catch by the props- 2019
# week33_split <- sport_catch %>% 
#   filter(Fishery=="B 10", Week==33) %>% 
#   left_join(b10_period_split,by=c("Fishery","Week")) %>% 
#   mutate(New_1=if_else(Disposition=="Kept", N*KeptProp, N*RelProp), New_2=N - New_1) %>% 
#   select(Fishery, Week, Stage, Month, Species, Disposition,New_1,New_2) %>% 
#   pivot_longer(cols=c(New_1, New_2), names_to="Period", values_to="N") %>% 
#   mutate(Period=as.numeric(str_remove(Period,"New_")))

sport_catch <- sport_catch %>% 
  # Filter out B10 Week 33
  #filter(!(Fishery=="B 10" & Week=="33")) %>% 
  # Bind the B10 week 33 split rows created above
  #bind_rows(week33_split) %>% 
  arrange(Week) %>% 
  group_by(Fishery, Disposition, Period) %>%
  summarise(N=sum(N)) %>% 
  mutate(Species="Chinook")

# Includes select areas
# Will need to change file and sheet names and cell ranges for current year if they are different
lcr_comm_catch <- read_xlsx(paste0(path,"LR Landings 2021.xlsx"), sheet="Landings") %>% 
  select(Fishery,Week,Species,N) %>% mutate(Disposition="Kept")

z6_catch <- read_xlsx(paste0(path, "2021_z6_landings_forRR.xlsx"),sheet="Landings") %>% 
  filter(Species=="CHINOOK", Catch_Area_Code != "Klickitat",Week>31) %>% 
  mutate(Fishery="Z6") %>% 
  group_by(Week,Fishery,VSI) %>% 
  summarize(N=sum(Landings)) %>% mutate(Species="Chinook", Disposition="Kept")

#Calculate 2021 proportions of landings by Zone 6 pool
read_xlsx(paste0(path, "2021_z6_landings_forRR.xlsx"),sheet="Landings") %>%#read in Z6 landings
  group_by(Catch_Area_Code) %>% summarize(Landings=sum(Landings)) %>% #sum within pools
  ungroup() %>% mutate(total=sum(Landings),prop=Landings/total) %>% select(-total) %>% #calculate proportions
 write.csv(file = paste0(path,"Zone_6_Props.csv")) #write csv

#Calculate 2020 proportions of landings by Zone 6 pool
read_xlsx(paste0("2020 Data/", "2020_z6_landings_forRR.xlsx"),sheet="Landings") %>% #read in Z6 landings
  filter(Catch_Area_Code!="Klickitat") %>% #exclude Klickitat
  group_by(Catch_Area_Code) %>% summarize(Landings=sum(Landings)) %>% #sum within pools
  ungroup() %>% mutate(total=sum(Landings),prop=Landings/total) %>% select(-total) %>% #calculate proportions
 write.csv(file = paste0("2020 Data/","2020_Zone_6_Props.csv")) #write csv


drano_catch <- tibble(Period=1,Fishery="Drano",VSI=NA,Disposition="Kept",N=3520) #FROM 2015_2020YNAnnualTribHarvest.xlsx DERRRRP

bind_rows(sport_catch, lcr_comm_catch, z6_catch,drano_catch)
}
# Function makes age correction matrix for age data grouped by the Ager
# age_data is a nested dataframe, nested by Ager group
# possible_ages are all the possible ages that could be encountered
# min_samples is the minimum number of samples for a given age class in order to do the correction

#age_data <- get_data()$age_data %>% filter(Age_grp=="WA lab")

make_corr_matrix <- function(age_data, possible_ages=c(2,3,4,5,6,7), min_samples=5){
  
x <- age_data %>% 
  select(CWTAge, scaleAge) %>% 
    filter(!is.na(CWTAge),!is.na(scaleAge)) %>% 
    group_by(CWTAge,scaleAge) %>% 
    count() %>% 
    group_by(scaleAge) %>% 
    mutate(prop=prop.table(n)) %>% 
    full_join(tibble(CWTAge=possible_ages, scaleAge=possible_ages),by=c("CWTAge","scaleAge")) %>%  
    mutate(n=replace_na(n,0),
      prop=replace_na(prop, 0)) %>% 
    ungroup()

# This function checks if there are correctly aged known-age samples, returns ages to correct. 
check_ages_to_correct <- function(x){
  
  #  Are there any scale ages for all possible ages
any_scale_ages <- x %>% group_by(Age=scaleAge) %>% 
  mutate(ScaleAges= n >= min_samples) %>% 
  summarize(AnyScaleAges=any(ScaleAges))

# any_cwt_ages <- x %>% group_by(Age=CWTAge) %>% 
#   mutate(CWTAges= n >= 1) %>% 
#   summarize(AnyCWTAges=any(CWTAges))

# Are there any correctly aged known-age fish 
known_correct <- x %>% group_by(Age=CWTAge) %>% 
  mutate(KnownCorrectAges= (CWTAge==scaleAge)& n > 0)  %>% 
  summarize(AnyKnownAgeCorrect=any(KnownCorrectAges)) 

# any_scale_ages %>% 
#   left_join(any_cwt_ages,by="Age") %>% 
#   left_join(known_correct,by="Age") %>% 
#   filter(AnyScaleAges | AnyCWTAges | AnyKnownAgeCorrect) %>% 
#   arrange(Age) %>% pull(Age)

# Return ages to correct, only ages that have scale age samples and at least 1 correct age reading
any_scale_ages %>%
  left_join(known_correct,by="Age") %>%
  filter(AnyScaleAges & AnyKnownAgeCorrect) %>%
  arrange(Age) %>%
  pull(Age)
}

# Ages that pass the check
 ages_to_correct <- check_ages_to_correct(x)
 
# filter data for ages that pass the check
if(length(ages_to_correct)>0){
 x <- x %>% 
   filter(!is.na(scaleAge) & scaleAge %in% ages_to_correct) 
# Correction matrix for ages pass the check_for_samples above.
conf_mtrx <- x %>% 
    pivot_wider(names_from=CWTAge, id_cols=-n, values_from=prop, values_fill=list(prop=0)) %>% 
    select(-scaleAge) %>% 
    as.matrix()

conf_mtrx } else {NULL}
}

# These functions do the age corrections, z6 is by Week and VSI everywhere else is by Week (or time strata/period) only
# Need to update this to be 1 function using some rlang:: to pass in the grouping variables 
#then could just use same function and change  the grouping arg to c("Week","VSI") for Z6 and just "Week" everywhere else.
return_age_comp_z6 <- function(age_data, correction_matrix){
 
  age_data <- age_data %>% filter(!is.na(VSI)) %>%  group_by(Week, VSI)
  # If there is no data to do correction, the make_corr_matrix function returns NULL
  # If there is no data to do correction, corrected ages are NULL and raw ages are NULL
  if(is.null(correction_matrix)){
    #corrected_age_counts <- NULL
    # If no correction and no CWT ages just use raw scale ages
    return(age_data %>% 
             mutate(Age=case_when(!is.na(CWTAge)~ CWTAge,
                                  is.na(CWTAge) & !is.na(scaleAge)~ scaleAge)) %>% 
             count(Age) %>% 
             mutate(prop=n/sum(n)) %>% 
             select(Week,VSI,Age,prop))
    } else {
      
  ages_to_correct <- colnames(correction_matrix) %>% as.numeric
  
  age_data_to_correct <- age_data %>% 
    filter(!is.na(scaleAge) & scaleAge %in% ages_to_correct & is.na(CWTAge)) %>% 
    #select(scaleAge) %>%
    count(scaleAge) %>% 
    select(Week,VSI,scaleAge,n) %>% 
    arrange(Week,VSI,scaleAge)
  
all_ages_weeks <- expand(age_data_to_correct %>% ungroup,Week,VSI,scaleAge)
  
corrected_age_counts <- all_ages_weeks %>% 
  left_join(age_data_to_correct, by=c("Week","VSI","scaleAge")) %>% 
  arrange(Week, VSI, scaleAge) %>% 
  mutate(n=replace_na(n,0)) %>% 
  nest(data=c(scaleAge, n)) %>% 
  mutate(age_vec=map(data, function(x){x %>% pull(n)})) %>% 
  mutate(corr_matrix=map(data,~(correction_matrix))) %>% 
  mutate(age_comp=map2(age_vec, corr_matrix, 
                       ~(.x %*% .y) %>% 
                         as_tibble() %>% 
                         pivot_longer(cols=1:ncol(.),names_to="Age",values_to="n") %>% 
                         mutate(Age=as.numeric(Age)))) %>% 
  select(Week,VSI,age_comp) %>% 
  unnest(age_comp)

# Ages that didnt have data to correct, and dont have a CWT age, being put in raw.
raw_ages <- age_data %>% 
  filter(!is.na(scaleAge) & !scaleAge %in% ages_to_correct & is.na(CWTAge)) %>% 
  group_by(Week,VSI,scaleAge) %>% 
  count() %>% 
  rename(Age=scaleAge)

# Known age fish
known_ages <- age_data %>% 
  filter(!is.na(CWTAge)) %>% 
  group_by(Week,VSI,Age=CWTAge) %>% count()

# Return the Age/prop Lookup
  bind_rows(raw_ages, corrected_age_counts, known_ages) %>% 
    group_by(Week,VSI,Age) %>% 
    summarize(n=sum(n)) %>% 
    mutate(prop=n/sum(n)) %>% 
    select(Week,VSI,Age, prop)
  } 
}
return_age_comp <- function(age_data, correction_matrix){
  age_data <- age_data %>%  group_by(Week)
  # If there is no data to do correction, the make_corr_matrix function returns NULL
  # If there is no data to do correction, corrected ages are NULL and raw ages are NULL
  if(is.null(correction_matrix)){
    #corrected_age_counts <- NULL
    # If no correction and no CWT ages just use raw scale ages
    return(age_data %>% 
             mutate(Age=case_when(!is.na(CWTAge)~ CWTAge,
                                  is.na(CWTAge) & !is.na(scaleAge)~ scaleAge)) %>% 
             count(Age) %>% 
             mutate(prop=n/sum(n)) %>% 
             select(Week,Age,prop))
    } else {
      
  ages_to_correct <- colnames(correction_matrix)[1:nrow(correction_matrix)] %>% as.numeric
  
  age_data_to_correct <- age_data %>% 
    filter(!is.na(scaleAge) & scaleAge %in% ages_to_correct & is.na(CWTAge)) %>% 
    #select(scaleAge) %>%
    count(scaleAge) %>% 
    select(Week,scaleAge,n) %>% 
    arrange(Week,scaleAge)
  
all_ages_weeks <-# expand(age_data_to_correct %>% ungroup, Week,scaleAge)
  crossing(scaleAge=ages_to_correct,Week=age_data_to_correct$Week)

corrected_age_counts <- all_ages_weeks %>% 
  left_join(age_data_to_correct, by=c("Week", "scaleAge")) %>% 
  arrange(Week, scaleAge) %>% 
  mutate(n=replace_na(n,0)) %>% 
  nest(data=c(scaleAge, n)) %>% 
  mutate(age_vec=map(data, function(x){x %>% pull(n)})) %>% #pluck("age_vec",1)
  mutate(corr_matrix=map(data,~(correction_matrix))) %>% 
  mutate(age_comp=map2(age_vec, corr_matrix, 
                       ~(.x %*% .y) %>% 
                         as_tibble() %>% 
                         pivot_longer(cols=1:ncol(.),names_to="Age",values_to="n") %>% 
                         mutate(Age=as.numeric(Age)))) %>% 
  select(Week,age_comp) %>% 
  unnest(age_comp)

# Ages that didnt have data to correct, and dont have a CWT age, being put in raw.
raw_ages <- age_data %>% 
  filter(!is.na(scaleAge) & !scaleAge %in% ages_to_correct & is.na(CWTAge)) %>% 
  group_by(Week,scaleAge) %>% 
  count() %>% 
  rename(Age=scaleAge)

# Known age fish
known_ages <- age_data %>% 
  filter(!is.na(CWTAge)) %>% 
  group_by(Week,Age=CWTAge) %>% count()

# Return the Age/prop Lookup
  bind_rows(raw_ages, corrected_age_counts, known_ages) %>% 
    group_by(Week,Age) %>% 
    summarize(n=sum(n)) %>% 
    mutate(prop=n/sum(n)) %>% 
    select(Week,Age, prop)
  } 
}

# Function to calculate calendar week consistent with Col R fisheries
# Default, starts week on sunday (week_start=7), requires lubridate pkg
# Requires arg dates to be date format
  calweek <- function(dates, week_start=7){
    
    # Jan 1st of year for each date in d
    jan1 <- lubridate::floor_date(dates, unit = "year")
    
    # Date of 1st day of week including jan1 for dates
    day1_week1 <- lubridate::floor_date(jan1, unit="week", week_start=week_start)
    
    # Calculate weeks since day1 of week including jan1
    as.numeric(dates - day1_week1) %/% 7 + 1
  }
  
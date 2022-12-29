 # pull in functions from this file- it also sources necessary packages: tidyverse, readxl, openxlsx
  source("rr_functions.r")
 
# Return year of fishery analysis 
  ret_yr <- 2021

  #gt age data and CWTs
age_CWT_dat<-get_data(files= c("sport_work.r"
                               , "z6_agecwt_work.r"
                               , "b10_cwt_work.r"
                               , "b10_age_work.r"
                               , "select_areas_work.r"
                               , "lcr_comm_work.r"
                               , "z6_sport_agecwt_work.r"
                               , "drano_agecwt.r"
))

# assign weeks to age data
   d1 <- age_CWT_dat$age_data %>% 
   # Create the actual week field, replace og week with a 1 if all weeks are pooled, 
   # the Week field is needed to group by in the age_comp function
   # make sure these line up with the cwt and catch data. 
   mutate(Week_actual=Week, 
          Week=case_when(Fishery =="Z6" & Week_actual<=35 ~ 35,
                         Fishery =="Z6" & Week_actual>=41 ~ 41,
                         Fishery=="Z6"~ Week_actual,
                         Fishery=="Z 4-5 Gillnet" & (between(Week_actual, 33,34)) ~ 34,
                         Fishery=="Z 4-5 Gillnet" & (Week_actual == 35) ~ 35,
                         Fishery=="Z 4-5 Gillnet" & (Week_actual == 36) ~ 36,
                         Fishery=="Z 4-5 Gillnet" & (Week_actual==39) ~ 39,
                         Fishery=="Z 4-5 Gillnet" & (Week>=40) ~ 40,
                         Fishery=="B 10" ~ Period,
                         TRUE ~ 1)) %>% 
   # filter(!(Fishery=="B 10" & Date < as.Date("2021-08-11")))%>% #before august 11 was a mark-selective fishery. Will be entered below.
   mutate(VSI=if_else(VSI=="Tule"&Week>39&Fishery=="Z6","Bright",VSI)) %>%  
   group_by(Age_grp) %>% 
   nest()
  
 correction_matrix_lut <- d1 %>% mutate(corr_matrix=map(data,
                               ~make_corr_matrix(.x))) %>% select(-data) 


d2 <- d1 %>% 
    unnest(data) %>% 
    group_by(Fishery,Age_grp) %>% 
  filter(Fishery !="WA LAB DATA") %>%  
    nest() %>% 
  left_join(correction_matrix_lut, by="Age_grp") 
  
age_comp <- d2 %>%
    mutate(age_comp=case_when(Fishery=="Z6" ~ map2(.x=data,.y=corr_matrix,
                                                   ~return_age_comp_z6(age_data=.x, correction_matrix=.y)),
                              TRUE ~ map2(.x=data,.y=corr_matrix,
                                                   ~return_age_comp(age_data=.x, correction_matrix=.y)))) %>% 
  ungroup %>% 
  select(Fishery,age_comp)  %>%  
  unnest(age_comp) %>% 
  filter(is.finite(prop))# INFINTE AGE COMPS FROM 0s BC/ had to pool Tules/Brights in weeks>38 in 2020 

#age_comp %>% filter(Fishery=="Drano")
# age_comp %>% filter(Fishery=="Z 4-5 Gillnet",Week==3)

# Check all sum to 1
# age_comp %>% group_by(Fishery,Week,VSI) %>% summarize(P=sum(prop)) %>% as.data.frame()
#catch_dat %>% filter(Fishery=="Z6") %>% print(n=Inf)

catch_dat <- gather_catch()  %>%
  # Add WEEK indices to group however is necessary, make sure these line up with the age and cwt data. 
  
  mutate(Week_actual=Week, 
         Week=case_when(Fishery =="Z6" & Week_actual<=35 ~ 35,
                        Fishery =="Z6" & Week_actual>=41 ~ 41,
                        Fishery=="Z6"~ Week_actual,
                        Fishery=="Z 4-5 Gillnet" & (between(Week_actual, 33,34)) ~ 34,
                        Fishery=="Z 4-5 Gillnet" & (Week_actual == 35) ~ 35,
                        Fishery=="Z 4-5 Gillnet" & (Week_actual == 36) ~ 36,
                        Fishery=="Z 4-5 Gillnet" & (Week_actual==39) ~ 39,
                        Fishery=="Z 4-5 Gillnet" & (Week>=40) ~ 40,
                        Fishery=="B 10" ~ Period,
                        TRUE ~ 1)) %>% 
  mutate(VSI=if_else(VSI=="Tule"&Week>39&Fishery=="Z6","Bright",VSI)) %>% #POOLED VSI IN Z6 for Week>39 in 2021, no VSI tule CWTs but many fish purchased for < $1
  group_by(Fishery,Disposition,Species,Week,VSI) %>% 
  summarize(N=sum(N))
  

# catch_dat %>% filter(Fishery=="Z 4-5 Gillnet")
# catch_dat %>% filter(Fishery=="Z6")

# Sport release mort rate- will need a table by fishery to generalize to more mark-selective fisheries.
release_mort <- 0.19

catch_age_comp <- catch_dat %>% #filter(Fishery=="B 10 MS") %>% 
  # Calculate release mortalities- THIS WILL NEED TO BE GENERALIZED FOR MULTIPLE MARK-SELECTIVE FISHERIES, CURRENTLY THIS ONLY APPLIES TO SPORT RELEASES ARE
   mutate(N=if_else(Disposition=="Released", N *release_mort , N)) %>%  
  group_by(Fishery,Species,Disposition,Week,VSI) %>% 
  summarize(N=sum(N)) %>% 
  left_join(age_comp, by=c("Fishery","Week","VSI")) %>% 
  #unnest(age_comp) %>% 
  mutate(Catch=N*prop) %>% 
  ungroup %>% 
  select(Fishery,Disposition, Week,VSI,Age,N=Catch)  #%>% filter(Fishery=="Drano")

#catch_age_comp %>% as.data.frame()
# Get CWT data ####


cwt_dat <-age_CWT_dat$cwt %>% ungroup %>%  
# Add WEEK indices to group however is necessary, make sure these line up with the age and catch data.  
  mutate(Week_actual=Week, 
         Week=case_when(Fishery =="Z6" & Week_actual<=35 ~ 35,
                        Fishery =="Z6" & Week_actual>=41 ~ 41,
                        Fishery=="Z6"~ Week_actual,
                        Fishery=="Z 4-5 Gillnet" & (between(Week_actual, 33,34)) ~ 34,
                        Fishery=="Z 4-5 Gillnet" & (Week_actual == 35) ~ 35,
                        Fishery=="Z 4-5 Gillnet" & (Week_actual == 36) ~ 36,
                        Fishery=="Z 4-5 Gillnet" & (Week_actual==39) ~ 39,
                        Fishery=="Z 4-5 Gillnet" & (Week>=40) ~ 40,
                        Fishery=="B 10" ~ Period,
                        TRUE ~ 1)) %>% 
  mutate(VSI=if_else(VSI=="Tule"&Week>39&Fishery=="Z6","Bright",VSI))
  
# filter(!(Fishery=="B 10" & Date < as.Date("2021-08-11")))



# CWT lookups ####
# CWT info from RMIS
tag_info_lut <- read_tsv(file="Data/tag_info_lookup.txt", col_types=cols(.default="c"))

# 3. Stock lookup tables  from "Do-er"  ####
doer_tbl_file <- "Data/new_DoerTables.xlsx"

basin_lut <- read_xlsx(doer_tbl_file, sheet="BasinLU") %>%
  # There are duplicated Stream/SubRun/MgmtStock combos so CWT codes can be joined by Rel_site, 
  # but we aren't using Rel_site to join in here so dropping and selecting distinct rows
  select(Stream, SubRun, MgmtStock, BasinGroup) %>% distinct()

# NA's are text NA in this table: BE SURE TO KEEP na="NA"
mgmt_stock_lut <- read_xlsx(doer_tbl_file, sheet="MgmtStockLU", na="NA")
fishery_lut <- read_xlsx(doer_tbl_file, sheet="Fishery_lookup")

# which_tr_where is a junction table that identifies which stock gets which tagrate in which fishery. 
# With new fisheries, will need to update this table to tell it which tagrates to use for MgmtStocks
which_TR_where_lut <- read_xlsx("Data/mgmt_stock_tagrate_lookup.xlsx",sheet="tagrate_lut")

#4. TAGRATE LOOKUP TABLES ####
# Juvenile TR lookup,created by RMIS.r script from RMIS release info. 
# TagRateType: Yes=Mark Selective TR, No=Non Mark Selective TR
juv_tr_lut <- read_tsv(file="Data/juv_tr_lookup.txt", col_types=cols(TagCode="c", TagRateType="c", TR="n")) 

# These tagrate tables are in the 'TR Tables.xlsx' file created by esc_stock_comp.r
mgmt_stock_TR_lut <- read_xlsx("TR Tables.xlsx", sheet="MgmtStockTR_lookup")
basin_TR_lut <- read_xlsx("TR Tables.xlsx", sheet="BasinTR_lookup")
prop_lrb_lut <- read_xlsx("TR Tables.xlsx",sheet="prop_LRB")

# DOER ####
cwt_w_info <- cwt_dat %>% 
  # Remove any 09BLANK tags, they provide no stock info
  filter(!(TagCode %in% c("09BLANK","NT","TL","TU","SNT"))) %>% 
  # Join to the RMIS release info
  left_join(tag_info_lut, by=c("TagCode")) %>%
  # Lookup mgmt_stock by RMIS run/hatchery/stock/release location
  left_join(mgmt_stock_lut,by=c("run","hatchery","stock","release_loc"))

# CHECK FOR MISSING MGMT STOCKS, either wrong species, or tag code typo, or run/stock/hatchery/release_loc with undefined MgmtStock
tags_not_found <- cwt_w_info %>%  filter(is.na(MgmtStock))  # Several coho

# Nest dataframe by Fishery and WhichTagRate
nested_cwts <- cwt_w_info %>% 
  filter(!is.na(MgmtStock),is.na(DIT)) %>% # filter for not missing MgmtStock and not DIT tags.
  left_join(which_TR_where_lut, by=c("Fishery","MgmtStock")) %>% 
  select(Fishery, Week, VSI, TagCode, n_tags, Week_actual, MgmtStock, WhichTagRate, brood_year, Homestream) %>%
  left_join(basin_lut, by=c("Homestream"="Stream", "MgmtStock")) %>% 
  select(Fishery, Week, VSI, TagCode, n_tags, Week_actual, MgmtStock, WhichTagRate, brood_year, Basin=BasinGroup) %>% # uses BasinGroup as Basin to lookup tagrate, allows pooling tribs 
  mutate(Age=ret_yr - as.numeric(brood_year)) %>% 
  group_by_at(vars(-n_tags)) %>% # group by everything but number of tags
  summarize(n_tags=sum(n_tags)) %>% # sum the number of tags by group, this collapses duplicate rows into 1 row for each fishery/week/VSI/TagCode etc.
  group_by(Fishery,WhichTagRate) %>% # nest the tag data by fishery and which tagrate where
  nest() %>% 
  left_join(fishery_lut, by="Fishery")# lookup fishery info, not sure this is used, but would have fishery type(i.e., sport/comm), as well as above/below Bon or Lewis


# Functions to do different lookups depending on which mgmt stock.

LRH_lookup <- function(cwts, juv_tr_lut, mgmt_stock_TR_lut, ms_fishery="NonMS"){
#cwts <- nested_cwts %>% filter(WhichTagRate=="LRH") %>% unnest(data) #%>% filter(Fishery=="B 10") 
  cwts %>% 
  # lookup aggregate LRH tag rate
  mutate(Dummy="LRH") %>% 
  left_join(mgmt_stock_TR_lut %>% filter(TagRateType==ms_fishery), by=c("Dummy"="MgmtStock","Age")) %>% 
  rename(AggregateTR=TR) %>% # rename here keeps case_when  from getting confused with TR in other lookup tables in pull(TR) below.
  mutate(LRH1_2_split_TR=case_when(MgmtStock=="LRH1"~ left_join(cwts, mgmt_stock_TR_lut %>% filter(TagRateType==ms_fishery),by=c("MgmtStock","Age")) %>% pull(TR),
                                   MgmtStock=="LRH2"~ left_join(cwts, juv_tr_lut %>% filter(TagRateType==ms_fishery), by=c("TagCode")) %>% pull(TR))) %>%
    rename(TR=AggregateTR)
   
}

MgmtStock_lookup <- function(cwts, mgmt_stock_TR_lut, ms_fishery="NonMS"){
  cwts %>% left_join(mgmt_stock_TR_lut %>% filter(TagRateType==ms_fishery), by=c("MgmtStock", "Age"))
}

LRH1_BasinTR_lookup <- function(cwts, basin_TR_lut, ms_fishery="NonMS"){
  cwts %>% left_join(basin_TR_lut,by=c("Basin","Age"))
} 

# For PUB-LRB above BON, NonMS rates without LRBS are in the Basin table under PUB-LRB mgmt stock
PUB_LRB_BasinTR_lookup <- function(cwts, basin_TR_lut, ms_fishery="NonMS"){
  cwts %>% left_join(basin_TR_lut %>% filter(TagRateType==ms_fishery),by=c("MgmtStock","Age"))
} 

BasinTR_lookup <- function(cwts, basin_TR_lut, ms_fishery="NonMS"){
  cwts %>% left_join(basin_TR_lut %>% select(-MgmtStock) %>% filter(TagRateType==ms_fishery), by=c("Basin"="Stream","Age"))
} 

Juvenile_lookup <- function(cwts, juv_tr_lut,ms_fishery="NonMS"){
  cwts %>% left_join(juv_tr_lut %>% filter(TagRateType==ms_fishery),by="TagCode")
}


# DOER TABLE
DoEr <- nested_cwts %>%
  mutate(data2=case_when(WhichTagRate=="MgmtStock" ~ map2(.x=data,.y=MarkSelective,~MgmtStock_lookup(cwts=.x, ms_fishery=.y, mgmt_stock_TR_lut)),
                         WhichTagRate=="PUB_Basin" ~ map2(.x=data, .y=MarkSelective,~PUB_LRB_BasinTR_lookup(cwts=.x, ms_fishery=.y, basin_TR_lut)),
                         WhichTagRate=="LRH" ~ map2(.x=data,.y=MarkSelective, ~LRH_lookup(cwts=.x, ms_fishery=.y,juv_tr_lut= juv_tr_lut,mgmt_stock_TR_lut=mgmt_stock_TR_lut)),
                         WhichTagRate=="Juv" ~ map2(.x=data,.y=MarkSelective, ~Juvenile_lookup(cwts=.x, ms_fishery=.y,juv_tr_lut)),  
                         WhichTagRate=="Basin" ~ map2(.x=data,.y=MarkSelective,~BasinTR_lookup(cwts=.x, ms_fishery=.y,basin_TR_lut)))) %>% #TRUE ~ list(NULL))) %>% 
  ungroup %>% 
  select(Fishery,data2) %>% 
  unnest(data2)  %>% 
  mutate(MgmtStockGrouped=if_else(is.na(Dummy),MgmtStock, Dummy)) %>% 
  select(Fishery,TagCode,Week,VSI,MgmtStock,Age,n_tags,TR,LRH1_2_split_TR,MgmtStockGrouped) %>% 
  mutate(Expanded=n_tags/TR,
         ExpandedLRH=n_tags/LRH1_2_split_TR) %>% 
  # NULL OUT VSI for cwts everywhere but Z6- only doing Tule/Bright Stock Comp in Z6 bc of oversampling at Am Can
  mutate(VSI=if_else(Fishery=="Z6",VSI, NA_character_)) #%>% write.xlsx(file="Qcheck1.xlsx")#checks TRs used by tagCode/fishery
# DoEr %>% filter(Fishery %in% c("TONGUE PT.","BLIND SLOUGH/KNAPPA SLOUGH","YOUNGS BAY"),MgmtStock=="LRH1") %>% 
#   openxlsx::write.xlsx(file="SAFE_LRH1_2020.xlsx")

stock_comps <- DoEr %>% 
  group_by(Fishery,Week,VSI,Age) %>% 
  mutate(TotExpanded=sum(Expanded),
         propTotal=Expanded/TotExpanded) %>% 
  select(Fishery,Week,VSI,Age,MgmtStock,MgmtStockGrouped,propTotal) %>% 
  arrange(Fishery,Week,VSI,Age) %>% 
  group_by(Fishery, Week, VSI,Age) %>% nest() #%>% write.csv(file="Qcheck1.5")
# USE AVG FOR AGE CLASSES W NO TAGS
avg_stock_comps <- DoEr %>% 
  group_by(Fishery,Week,VSI) %>% 
  mutate(TotExpanded=sum(Expanded),
         propTotal=Expanded/TotExpanded) %>% 
  select(Fishery,Week,VSI,MgmtStock,MgmtStockGrouped,propTotal) %>% 
  arrange(Fishery,Week,VSI) %>% 
  group_by(Fishery, Week, VSI) %>% nest() %>% rename(data_avgstockcomp=data)



# Function to filter LRH and BPH out of age 6 stock comp in fisheries with no tags.
age_6_fn <- function(avg_stockcomp){
  avg_stockcomp %>% 
    filter(!MgmtStockGrouped %in% c("LRH","BPH","SAB")) %>% 
    mutate(propTotal=propTotal/sum(propTotal))
}


# Look for no tags
bs_fisheries <- catch_age_comp %>%
  filter(!(Fishery=="B 10 MS" & Disposition=="Released")) %>% 
  group_by(Fishery,Disposition,Week,VSI,Age) %>% 
  nest() %>% 
full_join(stock_comps,by=c("Fishery","Week","VSI","Age"),suffix=c("_agecomp","_stockcomp")) %>%

  # Logical column look for age comp with no tags
mutate(AgeNoTags=map_lgl(data_stockcomp,~length(dim(.x))==0)) %>% 
  # Lookup avg stock comp by fishery, week,vsi
  left_join(avg_stock_comps,by=c("Fishery","Week","VSI")) %>%
   # Replace avg stockcomp for age with no tags 
   mutate(data_stockcomp= case_when(Age==6 & AgeNoTags ~ map(data_avgstockcomp, ~age_6_fn(.x)),
                                    !AgeNoTags ~ map(data_stockcomp,~.x),
                                    AgeNoTags ~  map(data_avgstockcomp,~.x)))  %>% #filter(Fishery=="Z6")
   unnest(c(data_agecomp,data_stockcomp)) %>% 
  select(Fishery,Disposition,Week,VSI,Age,N,MgmtStock,MgmtStockGrouped,propTotal) %>% 
  #Unnest everything
  # Calculate catch
  mutate(N_stock=N*propTotal) %>% 
  select(Fishery,Disposition,Week,VSI,Age,MgmtStock,MgmtStockGrouped,N=N_stock) %>% 
  group_by_at(vars(-N)) %>% 
  summarize(N=sum(N)) %>% 
  left_join(fishery_lut,by="Fishery") # %>% write.csv(file="Qcheck2") 

# NO AGE 5 TAGS in Z 1-3 Tanglenet
# DoEr %>% filter(Fishery=="Z 1-3 Tanglenet",Age==5)

#LRH1-2 Split
lrh_split <- DoEr %>% filter(!is.na(ExpandedLRH)) %>% 
  group_by(Fishery,Week,VSI,Age) %>% 
  mutate(LRHTot=sum(ExpandedLRH),
         propLRH=ExpandedLRH/LRHTot) %>% ungroup %>% 
  select(Fishery,Week,VSI, Age, MgmtStock, propLRH) %>% 
  group_by(Fishery,Week,VSI,Age,MgmtStock) %>% 
  summarize(propLRH=sum(propLRH)) %>% 
  arrange(Fishery,Week,VSI,Age,MgmtStock)

# Avg lrh 1/2 split for ages w/ no tags- only Z 1-3 tanglenet in 2019
avg_lrh_split <- DoEr %>% filter(!is.na(ExpandedLRH)) %>% 
  group_by(Fishery,Week,VSI) %>% 
  mutate(LRHTot=sum(ExpandedLRH),
         propLRH=ExpandedLRH/LRHTot) %>% ungroup %>% 
  select(Fishery,Week,VSI, MgmtStock, propLRH) %>% 
  group_by(Fishery,Week,VSI,MgmtStock) %>% 
  summarize(propLRH=sum(propLRH)) %>% 
  arrange(Fishery,Week,VSI,MgmtStock)

# All LRH tags in Z 1-3 Tanglenet were LRH1- so avg stock comp gives all to LRH1
# Will need to have an avg LRH1/2 split to handle this everywhere but code below just replaces LRH1 in mgmt stock

LRH12_bs <- bs_fisheries %>% 
  filter(MgmtStockGrouped=="LRH") %>% #filter(Fishery=="Z 1-3 Tanglenet",Age==5) %>% 
  left_join(lrh_split, by=c("Fishery","Week","VSI","Age"), suffix=c("_in","_final")) 

lrh_w_split <- LRH12_bs %>% filter(!is.na(propLRH)) %>%  
  mutate(N_split= N*propLRH) %>% ungroup %>%
  select(Fishery, Disposition, Week,VSI,Age,MgmtStock=MgmtStock_final,N=N_split)

lrh_wo_split <- LRH12_bs %>% filter(is.na(propLRH)) %>% 
  left_join(avg_lrh_split, by=c("Fishery","Week","VSI"),suffix=c("","_avg")) %>% 
  mutate(N_split=N*propLRH_avg) %>% ungroup %>% 
  select(Fishery, Disposition, Week, VSI, Age, MgmtStock, N=N_split)

PUBLRB_bs <- bs_fisheries %>% 
  filter(MgmtStock=="PUB-LRB" & BelowBon=="Yes" & MarkSelective=="NonMS") %>% 
  left_join(prop_lrb_lut,by="Age") %>% 
  mutate(LRB=N*propLRB,
         `PUB-LRB`= N-LRB) %>% ungroup %>% 
  select(Fishery,Disposition, Week, VSI, Age,LRB,`PUB-LRB`) %>% 
  pivot_longer(cols=c(LRB,`PUB-LRB`),names_to="MgmtStock",values_to="N")

bs_1 <- bs_fisheries %>% 
  # REPLACE LRH w LRH1/2 and PUB-LRB with PUB-LRB/LRB
  filter(MgmtStockGrouped!="LRH" & !(MgmtStockGrouped=="PUB-LRB"&BelowBon=="Yes"&MarkSelective=="NonMS")) %>% 
  bind_rows(lrh_w_split,
            lrh_wo_split,
            PUBLRB_bs) %>% 
  ungroup()

# Mark selective fisheries below
mark_rate_lut <- readxl::read_xlsx("prelim_esc_stock_comps.xlsx",sheet=1) %>% 
  filter(EscType %in% c("NS","H","W")) %>% 
  group_by(MgmtStock,Age,Mark) %>% 
  summarize(Esc=sum(N)) %>% 
  group_by(MgmtStock,Age) %>% 
  mutate(Tot=sum(Esc)) %>%
  ungroup() %>% 
  filter(Mark=="ad") %>% 
  mutate(MarkRate=Esc/Tot) %>% 
  select(MgmtStock, Age, MarkRate) #%>% write.csv(file="Qcheck2-EscMRs.csv") #MgmtStock mark rate check

# FIRST CALC STOCK PROPS FOR RELEASED FISH WITH MARK RATE FROM THE CALCULATED STOCK COMP FOR KEPT CATCH
ms_rel_stock_comp <- bs_1 %>% 
  filter(Fishery=="B 10 MS",MgmtStock!="LRH2") %>% 
  left_join(mark_rate_lut,by=c("MgmtStock","Age")) %>% 
  mutate(MarkRate=if_else(MgmtStock %in% c("SAB","Stray","Stray-OB"),1,MarkRate)) %>% 
  mutate(Handle=N/MarkRate,
         RelReleaseN=Handle-N) %>% 
  group_by(Fishery,Disposition="Released",Week,Age) %>% 
  mutate(RelTot=sum(RelReleaseN)) %>% 
  ungroup() %>% 
  mutate(p=RelReleaseN/RelTot) %>% select(Fishery,Disposition, Week,Age,MgmtStock,p) 

# APPLY PROPS TO THE RELEASE MORTS
ms_rel_morts <- catch_age_comp %>%
  filter((Fishery=="B 10 MS" & Disposition=="Released")) %>% 
  left_join(ms_rel_stock_comp,c("Fishery", "Disposition", "Week", "Age")) %>% 
  mutate(N=N*p) %>% select(-p) #%>% 
  #left_join(fishery_lut) # this will happen 

# Stock/Age props for treaty OTB
otb_props <- bs_1 %>% 
  filter(Fishery=="Z6") %>% 
  group_by_at(vars(-VSI,-N)) %>% 
  summarize(N=sum(N,na.rm = TRUE)) %>% 
  group_by(Week) %>% 
  mutate(Total=sum(N),
         prop=N/Total) %>%
select(Week,Age,MgmtStock,prop)
  
otb_catch <- read_xlsx("Data/z6_OTB.xlsx",sheet=1) %>% 
  mutate(Week=case_when(Week<=35~35,
                        Week>=41~41,
                        TRUE ~ Week))

otb_bs <- otb_catch %>% 
  left_join(otb_props,by="Week") %>% 
  mutate(N=Catch*prop) %>% 
  select(Fishery, Week, MgmtStock,Age, N)

bs_out <- bind_rows(bs_1,otb_bs,ms_rel_morts) %>% ungroup

# Source the function to do poundnet stock comp, this has all same code, 
# just need to get poundnet data in the get_data function to pull in catch/agecomp/tags
# source("pn_stockcomp.r")

# in 2019, week 41 z45 gillnet fishery caught a ton of low-quality fish, likely LRB's staged in z5. 
# But no tags representing the LRBs were recovered in sampling, so we split the catch in proportion to lbs paid < $1 or > $1. 
# There was pretty clear break in distribution of price paid in Fish Tickets for these landings, with one mode <$1 and one >$1
# Assumed that all lbs payed < $1 were poor-quality LRBs. 
# The remainder (paid >$1) were split using the CWTs
# This file has the LRB rows on the LRB_BS_rows tab.
# lrb_Z45_wk41 <- read_xlsx("Data/Z45GN_wk41_LRB-URB_2019.xlsx", "LRB_BS_rows") %>% mutate(Week=3)
#lrb_Z45_wk41
fishery_output <- bs_out %>% 
  # bind_rows(pn_stock_comp()) %>% 
  select(Fishery,Disposition,Week,Age,MgmtStock,N)

library(openxlsx)

fishery_output %>% group_by(Fishery, Week, Disposition, Age,MgmtStock) %>% summarize(N=sum(N)) %>% 
write.xlsx(file="bigsheet_fisheries_output.xlsx", overwrite = TRUE)

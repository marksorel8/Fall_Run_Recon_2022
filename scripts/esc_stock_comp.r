# 0. Load packages ####
library(tidyverse)
library(readxl)
library(openxlsx)

# Set the Return Year for the analysis.
ret_yr <- 2021

# 1. Get data from excel files ####

esc_file <- "Data/2021_Natural_Weir_Hatchery_Sport_Escapements.xlsx"

# Escapement estimates
esc_dat <- read_xlsx(esc_file, sheet="EscEstimateTable") %>% select(Stream:MgmtStock) %>% mutate(MgmtStock=as.character(MgmtStock))

# CWT sample summary
cwt_sampsizes <- read_xlsx(esc_file, sheet="CWT_SampSizeTable") %>% select(Stream:N_Examined_for_CWT)

# CWT recoveries
esc_cwt <- read_xlsx(esc_file, sheet="CWT_Recoveries(Raw)")
#esc_cwt %>% filter(is.na(TagCode))
# Sample rate lookup table
 sr_lut <- esc_dat %>% 
           group_by(Stream, EscType, SubRun) %>% 
           summarize(Total_N=sum(N)) %>% 
           left_join(cwt_sampsizes,by=c("Stream","EscType","SubRun")) %>% 
           mutate(samp_rate=N_Examined_for_CWT/Total_N)
 
#sr_lut %>% filter(Stream=="Elochoman")
# 2. Get CWT lookup tables for Stock comp #### 
# These tables are generated by RMIS.r script

# CWT info from RMIS
tag_info_lut <- read_tsv(file="Data/tag_info_lookup.txt", 
                         col_types=cols(.default="c",brood_year="n"))

# Juvenile TR LUT, TagRateType: Yes=Mark Selective TR, No=Non Mark Selective TR
juv_tr_lut <- read_tsv(file="Data/juv_tr_lookup.txt",
                       col_types=cols(TagCode="c", TagRateType="c", TR="n"))

if(is.null(esc_cwt$Age)){
esc_cwt <- esc_cwt %>% 
  left_join(tag_info_lut %>% select(TagCode,brood_year),by="TagCode") %>% 
  mutate(Age=ret_yr-brood_year) %>% select(-brood_year)
}

# 3. Stock lookup tables  from "Do-er"  ####
doer_tbl_file <- "Data/new_DoerTables.xlsx"

basin_lut <- read_xlsx(doer_tbl_file, sheet="BasinLU") %>%
  # There are duplicated Stream/SubRun/MgmtStock combos so CWT codes can be joined by Rel_site, 
  # but we aren't using Rel_site to join in here so dropping and selecting distinct rows
  select(Stream, SubRun, MgmtStock, BasinGroup) %>% distinct()

# NA's are text NA in this table: BE SURE TO KEEP na="NA"
mgmt_stock_lut <- read_xlsx(doer_tbl_file, sheet="MgmtStockLU", na="NA")


# 4. Lookup info for recovered CWTs  ####
cwts_w_info <- esc_cwt %>% 
  # Lookup tag info
  left_join(tag_info_lut, by="TagCode") %>% 
  #assign mark type based on DIT
  mutate(Mark=ifelse(is.na(DIT),"ad","um")) %>% 
  # Lookup Mgmt stock- has the homestream for each combo of run/hatchery/stock/release location
  left_join(mgmt_stock_lut, by=c("run","hatchery","stock","release_loc")) %>%
  # Lookup the mark selective juvenile tagrate by tagcode- only applying CWT stock comps to clipped fish.
  left_join(juv_tr_lut %>% filter(TagRateType=="MS"), by="TagCode") %>%
  # Make a TRUE/FALSE column to determine if a recovered tags is from the escapement area where recovered
  mutate(homestream_tag= Stream==Homestream) %>% 
  #get rid of mini-jacks 
  filter(Age>1)

# cwts_w_info %>% filter(is.na(MgmtStock))


# 5. Some summary tables to check conditions for which method to use where ####
# Counts of tag recoveries in escapement areas
cwt_smry <- cwts_w_info %>% 
  group_by(Stream,EscType,SubRun,Mark,Age) %>% 
  summarize(N_tags=sum(TagCount))

# Tags not found- either tag code transcription errors or tag code reported to RMIS as another species 
tags_not_found <- cwts_w_info %>% 
  filter(is.na(agency),is.na(run),is.na(stock),is.na(release_loc),is.na(brood_year))


# Filter for only tags with info from RMIS
cwts_w_info <- cwts_w_info %>% filter(!is.na(agency) | !is.na(run) | !is.na(stock) | !is.na(release_loc) | !is.na(brood_year))

# THESE NEED TO BE ADDED TO MGMT STOCK LUT in new_DoerTables.xlsx
  cwts_w_info %>% filter(is.na(MgmtStock)) %>% select(run,hatchery,stock,release_loc) %>% distinct() #%>% 
   # write.table(file="clipboard",row.names=FALSE)

  
  
  
# 6. Prep escapement data with CWT summary and checks to determine which method to use ####
esc_info <- esc_dat %>% 
  left_join(cwt_smry, by=c("Stream", "EscType", "SubRun", "Mark", "Age")) %>% 
  mutate(has_tags=!is.na(N_tags)) %>% 
  left_join(cwts_w_info %>%  group_by(Stream,EscType,SubRun,Mark,Age) %>% 
              summarize(has_homestream=any(homestream_tag)), 
            by=c("Stream", "EscType","SubRun","Age","Mark")) %>% 
    mutate(has_homestream=if_else(is.na(has_homestream), FALSE, has_homestream)) %>% 
    mutate(method=case_when(!is.na(MgmtStock) ~ "Straight to BigSheet", # Escapement with pre-assigned mgmt stocks-URB and NF Lewis Bright
                           # Stream=="NF Lewis" & SubRun=="B" & EscType=="NS" & Mark=="ad" ~ "Subtract Expanded/DITs", # LRW fish
                            Stream=="Bonneville" & SubRun=="B" & Mark=="ad" ~ "Relative Props", # Bonneville Hatchery
                            !has_tags & is.na(MgmtStock) ~ "Assign Homestream", # Escapements with no tags and no pre-assigned mgmt stock, assigned to homestream mgmt stock
                            (has_tags & Mark == "um") | (has_tags & !has_homestream & Mark!="um") ~ "Subtract Expanded/DITs", # Only stray tags found in ad-clipped or L/V escapement
                            has_tags &  has_homestream & Mark!="um" ~ "Relative Props")) # Ad-clipped escapements with homestream tags, stock comp done with juvenile tag rates

  #cwts_w_info %>% filter(Age==3,Mark=="ad",EscType=="NS",Stream=="Big Creek")              

  # Check NF Lewis
# esc_info %>% filter(Stream=="NF Lewis"& SubRun=="B"&Mark=="ad")

# 7. Functions to implement methods for the different situations ####
# Function to pull straight to bigsheet
straight_to_BigSheet <- function(esc_dat){
  esc_dat %>% 
    select(Stream, EscType, SubRun, Age, Mark, N, MgmtStock)
  }  
 
# Function to assign homestream managment stock.
esc_assigned_to_homestream <- function(esc_dat, basin_lut){ 
   esc_dat %>% 
  left_join(basin_lut, by=c("Stream","SubRun"), suffix=c("_pre","_lu")) %>% 
  mutate(MgmtStock=case_when(is.na(MgmtStock_pre) ~ MgmtStock_lu,
                           TRUE ~ MgmtStock_pre),
         Homestream=Stream) %>% 
  select(Stream, EscType, SubRun, Age, Mark, N, MgmtStock)
  
}

# Relative props using juv trs - Ad clipped esc w/ tags
# Ad Clipped escs w/ tags
ad_esc_w_home_tags <- function(esc_dat, cwts_w_info){
  
  # used to be outside this fn., filter happened inside the semi-join. 
  # the filter is implicit in the case_when(method=="Relative Props") in the do_esc_stock_comp function below.
  # esc_dat %>% filter(method=="Relative Props")
  
ad_esc_stock_props <-
  cwts_w_info  %>% 
  # STRAY LRW TAGS GET TR=1
  mutate(TR=case_when(is.na(hatchery) & str_detect(stock,"LEWIS R") & run=="Fall" ~ 1,
                      TRUE ~ TR)) %>% 
  semi_join(esc_dat , by=c("Stream","EscType","SubRun","Mark","Age")) %>% 
  # filter(is.na(TR)) # Check for NA Tag Rates here- Shane's fish or unmarked fish that dont have a mark-selective TR
  filter(!is.na(TR)) %>%  
  mutate(ExpandedTags=TagCount/TR) %>% 
  group_by(Stream, SubRun, EscType, Mark, Age) %>% 
  mutate(TotExpandedTags=sum(ExpandedTags,na.rm=TRUE)) %>% 
  select(TagCode, MgmtStock, Stream, EscType, SubRun, Mark, Age, TR, ExpandedTags, TotExpandedTags, hatchery, stock, run, release_loc, Homestream) %>% 
  mutate(p_Stock=ExpandedTags/TotExpandedTags)

  esc_dat %>% 
  inner_join(ad_esc_stock_props, by=c("Stream", "EscType", "Age","SubRun","Mark"),suffix=c("_assigned","_fromCWT")) %>% 
  # Here we're splitting off into only escs with tags, need to deal with is.na(p_Stock)
  filter(!is.na(p_Stock)) %>% 
  mutate(N_stock=N * p_Stock,
         N_stock_round=N_stock) %>% 
  select(#TagCode, 
         Stream, Homestream, run, EscType, SubRun, Age, Mark, MgmtStock=MgmtStock_fromCWT, N=N_stock_round) %>% 
  group_by_at(vars(-N)) %>% 
    summarize(N=sum(N))
}

# Expansion/Subtraction method
subtraction_method <- function(esc_dat, cwts_w_info, sr_lut, basin_lut){

cwt_dat <- esc_dat %>% 
  select(Stream, EscType, SubRun, Age, Mark) %>% 
  left_join(cwts_w_info, by=c("Stream","EscType","SubRun","Mark","Age")) %>% 
  left_join(sr_lut, by=c("Stream","EscType","SubRun")) %>%
  mutate(TR=if_else(!is.na(DIT), 1, TR),
         N_new=(TagCount/samp_rate)/TR) 
cwt_dat %>% filter(is.na(TR))
tots <- cwt_dat %>%
  group_by(Stream, EscType, SubRun, Age, Mark) %>% 
  summarize(N_new_total=sum(N_new)) %>% 
  left_join(esc_dat, by=c("Stream","EscType","SubRun","Mark","Age")) %>% 
  mutate(new_greater= N_new_total > N) %>% 
  ungroup %>% select(Stream, EscType, SubRun, Age, Mark, N_new_total, N, new_greater)

# Rows subtracted -expanded tags that take less than available esc.
subtracted <- tots %>% filter(!new_greater) %>% #pull(N) %>% sum()
  select(Stream,EscType,SubRun,Age,Mark) %>% 
 left_join(cwt_dat, by=c("Stream","EscType","SubRun","Mark","Age")) %>%  
  #select(Stream,EscType,SubRun,Age,Mark,TagCode,N_new)  #distinct() #select(Stream, EscType, SubRun, Age, Mark,N_new)
  select(Stream, EscType, Age, Mark,SubRun, N=N_new, MgmtStock, Homestream, run)

# Remainders for expanded tags that take less than available esc, assigned to homestream Mgmt Stock
remainders <- tots %>% 
  filter(!new_greater) %>%  
  mutate(N_new=N - N_new_total) %>% 
  select(Stream, EscType, SubRun, Age, Mark, N_new) %>% 
  filter(N_new>0) %>% 
  left_join(basin_lut, by=c("Stream","SubRun")) %>%
  mutate(Homestream=Stream) %>% 
  select(Stream, EscType, Age, SubRun, Mark, MgmtStock, N=N_new)

# Props for tags expanded > than Escapement (takes 100% of escapment)
props <- cwt_dat %>% 
  select(Stream,EscType,SubRun,Age,Mark,TagCode,TagCount,N_new,MgmtStock,Homestream,run) %>% 
  group_by(Stream, EscType, SubRun, Age, Mark) %>% 
  mutate(N_tot_new=sum(N_new),
         p_New=N_new/N_tot_new) %>% 
  inner_join(tots %>% filter(new_greater) %>% select(Stream,EscType,SubRun,Mark,Age,N), by=c("Stream","EscType","SubRun","Mark","Age")) %>%
  mutate(N_final=p_New*N) %>% 
  ungroup %>% 
  select(Stream, EscType, Age, SubRun, Mark, MgmtStock, N=N_final, Homestream, run)


out <- bind_rows(subtracted, remainders, props)

#Total_post <- sum(out$N)

# Check to debug. Doesnt seem to work in the list column workflow, likely a scoping problem.
#if(Total_pre != Total_post){warning(paste("INPUT TOTAL DOES NOT MATCH OUTPUT TOTAL, difference is:", Total_pre-Total_post,"fish"))}

out

}

#esc_info %>% filter(method == "Subtract Expanded/DITs") %>% subtraction_method(cwts_w_info, sr_lut, basin_lut)

# 8. Function to take all the data and do esc_stock comp. ####
do_esc_stock_comp <- function(esc_info){ 
esc_info  %>% 
   #filter(method=="Straight to BigSheet" | method=="Assign Homestream"|method=="Relative Props"|str_detect(method,"Subtract")) %>% 
   group_by(method) %>% 
   nest() %>% 
   mutate(newDat=case_when(method=="Straight to BigSheet" ~ map(data, ~straight_to_BigSheet(.x)),
                           method=="Assign Homestream" ~  map(data, ~esc_assigned_to_homestream(.x, basin_lut)),
                           method=="Relative Props" ~ map(data, ~ad_esc_w_home_tags(.x, cwts_w_info)),
                           method=="Subtract Expanded/DITs" ~ map(data, ~subtraction_method(.x, cwts_w_info, sr_lut, basin_lut)))) %>% #%>% 
    mutate(Tot_in=map_dbl(data,~.x %>% pull(N) %>% sum),
           Tot_out=map_dbl(newDat,~.x %>% pull(N) %>% sum),
           Match= Tot_in==Tot_out ) %>%
    ungroup
  #select(newDat) %>% 
  #ungroup %>% 
  #unnest(newDat) 
}
  
# Function to round results
#rnd <- function(x){ifelse(x < 1, ceiling(x), round(x))}
#guar
bs_output2 <- do_esc_stock_comp(esc_info) #%>%
  
# Collapse over multiple tags
out <-   bs_output2 %>% 
  select(newDat) %>% 
  unnest(newDat) %>% 
  group_by(Stream,Homestream,run,EscType,SubRun,Age,Mark,MgmtStock) %>% 
  summarize(N=sum(N)) 

#out %>% filter(Stream=="Kalama") %>% print(n=Inf)
#out %>% filter(Stream=="Bonneville",Age==2)
#out %>% filter(Stream=="Herman")

# Write the output
  out %>% write.xlsx(file="prelim_esc_stock_comps.xlsx")

# 9. Calculate TRs from Escs ####
calc_MgmtStock_TRs <- function(cwts_w_info, bs_output){
#bs_output <- out %>% filter(MgmtStock=="LRW")
t1 <- cwts_w_info %>% #filter(MgmtStock=="LRW") %>% 
  filter(EscType %in% c("NS","H","W","T"), # Only using Natural Spawn, Hatchery, Weir, and Trap escapements to calc. tagrates
         MgmtStock %in% c("BPH", "LRH1","LRH2", "LRW","URB-S"), #"URB","URB-S"), # Mgmt stocks to calculate Escapement Tagrates URB and URB-S have been done outside, but could easily be added.
         is.na(DIT)) %>% 
  left_join(sr_lut %>% select(Stream,EscType,SubRun,samp_rate),by=c("Stream", "EscType","SubRun")) %>% 
  mutate(exp_tags=TagCount/samp_rate) %>% 
  group_by(Stream, EscType, SubRun, Mark, Age, MgmtStock) %>% 
  summarize(N_tags=sum(exp_tags)) %>% 
  mutate(MgmtStock2=if_else(MgmtStock %in% c("LRH1","LRH2"), "LRH", MgmtStock)) %>% 
  ungroup() %>% 
  filter(Mark!="um")
  #out %>% filter(Stream=="Kalama") %>% print(n=Inf)
esc_totals <- bs_output %>% 
  filter(EscType %in% c("NS", "H", "W", "T")) %>% 
  group_by(MgmtStock, Age, Mark) %>% 
  summarize(TotalEsc=sum(N)) %>% 
bind_rows(
bs_output %>% 
  mutate(MgmtStock=if_else(MgmtStock %in% c("LRH1","LRH2"), "LRH", MgmtStock)) %>% 
  filter(MgmtStock=="LRH") %>% 
  group_by(MgmtStock,Age,Mark) %>% 
  summarize(TotalEsc=sum(N))) %>% ungroup 

tag_totals <- t1 %>% 
  group_by(MgmtStock2,Age) %>% 
  summarize(N_tags=sum(N_tags)) %>% 
  rename(MgmtStock=MgmtStock2) %>% 
  # this just binds aggregate LRH and LRH1-only tag totals into same table
  bind_rows(t1 %>% filter(MgmtStock=="LRH1") %>% group_by(MgmtStock,Age) %>% summarize(N_tags=sum(N_tags))) #%>% 
  
tag_totals %>% left_join(
esc_totals %>% 
  group_by(MgmtStock, Age) %>% 
  summarize(NonMS_Esc=sum(TotalEsc)) %>% 
  inner_join(
  esc_totals %>% filter(Mark!="um") %>% 
  group_by(MgmtStock, Age) %>% 
    summarize(MS_Esc=sum(TotalEsc)), 
  by=c("MgmtStock", "Age")), 
by=c("MgmtStock","Age")) %>% 
  mutate(MS=N_tags/MS_Esc,
         NonMS=N_tags/NonMS_Esc) %>% 
  pivot_longer(cols=c("MS","NonMS"),names_to="TagRateType",values_to="TR") %>% 
  select(MgmtStock, Age, TagRateType,TR)
  
}

#sr_lut %>% filter(Stream=="Elochoman")

calc_LRH1_basin_TRs <- function(cwts_w_info, bs_output, basin_lut){
  
basin_tag_totals <- cwts_w_info %>% 
  filter(EscType %in% c("H","NS","W","T")) %>% 
  filter(homestream_tag) %>% 
  left_join(basin_lut,c("Stream", "SubRun", "MgmtStock")) %>% 
  left_join(sr_lut,c("Stream", "EscType", "SubRun")) %>% 
  #select(TagCode,TagCount,Stream,homestream_tag,BasinGroup,Mark,Age,samp_rate) %>% 
  mutate(exp_tags=TagCount/samp_rate) %>% 
  select(Stream,EscType, SubRun,Mark,Age,MgmtStock,Homestream,BasinGroup,exp_tags) %>% 
  filter(MgmtStock=="LRH1") %>% 
  group_by(BasinGroup,MgmtStock,Age) %>% 
  summarize(TotalTags=sum(exp_tags))

basin_escs <- bs_output %>%
  filter(MgmtStock=="LRH1",
         EscType %in% c("H", "NS", "W", "T")) %>%
  left_join(basin_lut %>% select(-MgmtStock),by=c("Stream","SubRun"))
 
basin_escs %>% 
  group_by(BasinGroup,Age,MgmtStock) %>% 
  # Summ all the escs (ad+um) for NonMark-Selective TR denominator
  summarize(Non_MSF_Esc=sum(N)) %>% 
  inner_join(
    basin_escs %>%
      filter(Mark!="um") %>% 
      group_by(BasinGroup, Age,MgmtStock) %>% # Join to Mark-selective TR denominator
      summarize(MSF_Esc=sum(N)), by=c("BasinGroup","Age","MgmtStock")) %>% 
  left_join(basin_tag_totals, by=c("BasinGroup","Age","MgmtStock")) %>% 
  filter(!is.na(TotalTags)) %>% 
#  Calculate tag rates MS(mark selective): CWT/(Ad clip escs), NonMS (non mark-selective): CWT/(Ad+Um escs)
  mutate(MS=TotalTags/MSF_Esc,
         NonMS=TotalTags/Non_MSF_Esc) %>% 
  # Now tidy into a TagRateType and Tag rate (TR) column
pivot_longer(cols=c("MS","NonMS"),names_to="TagRateType",values_to="TR") %>% 
  select(Stream=BasinGroup, Age, MgmtStock, TagRateType, TR)
}  

# Ended up pooling Toutle in Cowlitz basin due to low tag recoveries in esc area
#calc_LRH1_basin_TRs(cwts_w_info, bs_output, basin_lut) %>% filter(Stream=="Cowlitz", Age==4)

# Calculate tag rates for Little-White PUBs with LRB escapement for lower river fisheries
calc_PUB_LRB_rates <- function(cwts_w_info, sr_lut, bs_output){
pub_lrb_tags <- cwts_w_info %>% 
  filter(Stream %in%(c("Wind", "Little White Salmon", "White Salmon")), 
         is.na(DIT),
         Mark!="um",
         EscType %in% c("NS","H","W","T"),
         MgmtStock=="PUB-LRB",
         SubRun=="B") %>%
  left_join(sr_lut,by=c("Stream","EscType","SubRun")) %>% 
  mutate(exp_tags=TagCount/samp_rate) %>% 
  group_by(MgmtStock, Mark, Age) %>% 
  summarize(TotalTags=sum(exp_tags))

pub_lrb_escs <- bs_output %>% 
  filter(Stream %in% c("Wind", "Little White Salmon", "White Salmon", "Hamilton","Ives/Pierce"),
         SubRun=="B",
         MgmtStock %in% c("PUB-LRB","LRB"),
         EscType %in% c("NS", "H","W","T"))

# Mark-selective above or below BON (no lrbs, only ad-clipped fish)
# THESE NEED TO GO INTO BOTH BASIN AND MGMT STOCK TR LUTS
pub_lrb_MS <- function(pub_lrb_escs, pub_lrb_tags){
pub_lrb_escs %>% 
  filter(Mark=="ad", MgmtStock!="LRB") %>% 
  group_by(Age) %>% 
  summarize(MS_Total_esc=sum(N)) %>% 
  left_join(pub_lrb_tags,by="Age") %>% 
  mutate(TR=TotalTags/MS_Total_esc) %>% 
  mutate(TagRateType="MS",
         MgmtStock="PUB-LRB") %>% 
  select(MgmtStock, Age,TagRateType, TR)
  }

# Non mark-selective below BON includes LRB- THIS GOES TO MGMT STOCK TR LUT
pub_lrb_NonMS_belowBON <- function(pub_lrb_escs, pub_lrb_tags){
  pub_lrb_escs %>% 
  group_by(Age) %>% 
  summarize(MS_Total_esc=sum(N)) %>% 
  left_join(pub_lrb_tags,by="Age") %>% 
  mutate(TR=TotalTags/MS_Total_esc) %>%
  #fill in age 6 tag rate with age 5 if not age 6 tags 
  # mutate(TR=case_when((Age==6&is.na(TR))~ (. %>% filter(Age==5) %>% pull(TR)
                                      # TRUE~R)) )%>%
  mutate(MgmtStock="PUB-LRB",
         TagRateType="NonMS") %>% 
   select(MgmtStock, Age, TagRateType,TR) %>% 
    mutate(toTable="MgmtStockTR_lookup")
}

# Non mark-selective above BON- THIS NEED TO GO INTO BASIN LUT
pub_lrb_NonMS_aboveBON <- function(pub_lrb_escs, pub_lrb_tags){  
  pub_lrb_escs %>% 
  filter(MgmtStock !="LRB") %>% 
  group_by(Age) %>% 
  summarize(MS_Total_esc=sum(N)) %>% 
  left_join(pub_lrb_tags,by="Age") %>% 
  mutate(TR=TotalTags/MS_Total_esc)%>% 
  mutate(MgmtStock="PUB-LRB",
         TagRateType="NonMS") %>% 
   select(MgmtStock, Age, TagRateType,TR) %>% 
    # Helper column to know which lookup table the TRs go in.
    mutate(toTable="BasinTR_lookup")
}

bind_rows( 
 pub_lrb_MS(pub_lrb_escs, pub_lrb_tags) %>% mutate(toTable="MgmtStockTR_lookup"),
 pub_lrb_MS(pub_lrb_escs, pub_lrb_tags) %>% mutate(toTable="BasinTR_lookup"),
 pub_lrb_NonMS_aboveBON(pub_lrb_escs, pub_lrb_tags),
 pub_lrb_NonMS_belowBON(pub_lrb_escs, pub_lrb_tags),
)
    
}

#calc_PUB_LRB_rates(cwts_w_info,sr_lut,bs_output)
# Calculate the proportion of LRBs to PUB-LRB escapement by Age
calc_prop_LRB <- function(bs_output) {
  pub_lrb_escs <- bs_output %>% 
  filter(Stream %in% c("Wind", "Little White Salmon", "White Salmon", "Hamilton","Ives/Pierce"),
         SubRun=="B",
         MgmtStock %in% c("PUB-LRB","LRB"),
         EscType %in% c("NS", "H","W"))
    
  tots <- pub_lrb_escs %>% 
    group_by(Age) %>% 
    summarize(N_tot=sum(N))
  
  lrb_tots <- pub_lrb_escs %>% filter(MgmtStock=="LRB") %>% 
    group_by(Age) %>% 
    summarize(N_lrb=sum(N))
  tots %>% left_join(lrb_tots,by="Age") %>% 
    mutate(propLRB=N_lrb/N_tot) %>% 
    select(Age,propLRB)
}

# Make the prop_lrb lookup table
prop_lrb_table <- calc_prop_LRB(out)

# Make the MgmtStock TR lookup table
mgmt_stock_tr_table <- 
  calc_MgmtStock_TRs(cwts_w_info, out) %>% 
  bind_rows(
    # URB tagrates were calculated by Hanford folks in 2019 so had to bind them to the MgmtStock tagrate table calculated here.
read.xlsx(esc_file, sheet="URB_TR") %>% select(-X5),
# Bind the PUB-LRB rates for the Mgmt Stock TR table
calc_PUB_LRB_rates(cwts_w_info, sr_lut, out) %>% filter(toTable=="MgmtStockTR_lookup")) %>% 
  select(-toTable) %>% # Drop the pub-lrb helper column 
  mutate(ReturnYear=ret_yr) %>% # add a column for return year (ret_yr is defined at top of this script)
  ungroup() # drop groupings

#mgmt_stock_tr_table %>% select(MgmtStock) %>% distinct()

# Make the basin tag rate table
basin_tr_table <- bind_rows(
  # Bind the PUB-LRB tagrates that go in Basin TR table
  calc_PUB_LRB_rates(cwts_w_info, sr_lut, out) %>% filter(toTable=="BasinTR_lookup") %>% select(-toTable),
 # Bind the LRH1 basin tagrates 
  calc_LRH1_basin_TRs(cwts_w_info,out,basin_lut)) %>% 
  # Add in the ReturnYear
  mutate(ReturnYear=ret_yr)

# Write the tagrate tables out to "TR Tables.xlsx"
# uncomment the line below when ready to write the TR tables
write.xlsx(list(BasinTR_lookup=basin_tr_table, MgmtStockTR_lookup=mgmt_stock_tr_table,prop_LRB=prop_lrb_table),file="TR Tables.xlsx")

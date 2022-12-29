# Script to pull data from RMIS. Creates a Tag Lookup Table and Calculates Juvenile Release tag rates.
require(tidyverse)||install.packages("tidyverse");library(tidyverse)
require(sqldf)||install.packages("sqldf");library(sqldf)

#setwd("S:/Reg5/FP/ColumbiaRiverManagementUnit/FisheryModels/Tools/StockCompDoer")

# Get & Prep Data ####
RMIS_releases <- read.csv("https://www.rmpc.org/files/data/RL041_ALL_FULLSET.csv", header=TRUE, stringsAsFactors=FALSE)
RMIS_locations <- read.csv("https://www.rmpc.org/files/data/LC041_ALL_FULLSET.csv", header=TRUE, stringsAsFactors=FALSE)
RMIS_runs <- read.csv("https://www.rmpc.org/files/data/run.csv", header=TRUE, stringsAsFactors=FALSE)


# Subset hatchery fish only, will need to include wild fish for the tag info lookup table, currently goes 9 years back
cur_yr <- as.numeric(format(Sys.Date(),format="%Y"))
yrs_ago <- 10

start_yr <- cur_yr - yrs_ago

d2 <- subset(RMIS_releases, brood_year >= start_yr & species==1) #& hatchery_location_code!="") WILD BROOD HATCHERY FISH ARE NOW REPORTED WITH NA HATCHERY FUUUUUCK!!!!

cols <- c("record_code",
          "tag_code_or_release_id",
          "release_agency",
          "run",
        #  "species",
          "hatchery_location_code",
          "stock_location_code",
          "release_location_code",
          "brood_year",
          "avg_weight",
          "last_release_date",
          "cwt_1st_mark",
          "non_cwt_1st_mark",
          "cwt_2nd_mark",
          "non_cwt_2nd_mark",
          "cwt_1st_mark_count",
          "non_cwt_1st_mark_count",
          "cwt_2nd_mark_count",
          "non_cwt_2nd_mark_count")

# Drop unused columns
d2 <- d2[,cols]

# Convert last release date to date format
d2$LastRelDate <- as.Date(as.character(d2$last_release_date),format="%Y%m%d")

# Some agencies only report month and year, which returns NA in line above. Give these arbitrary day 14 (mid-month)
d2$LastRelDate[is.na(d2$LastRelDate)] <- as.Date(paste(as.character(d2$last_release_date[is.na(d2$LastRelDate)]),"14",sep=""),format="%Y%m%d")

# Some only have year, give arbitrary Jan-1
d2$LastRelDate[is.na(d2$LastRelDate)] <-  as.Date(paste(as.character(d2$last_release_date[is.na(d2$LastRelDate)]),"0101",sep=""),format="%Y%m%d")
#key_cols <- c("tag_code_or_release_id","hatchery_location_code","release_location_code","stock_location_code","run","brood_year","LastRelDate","avg_weight")

# Make a composite key field for hatchery, run, stock, release location and brood year
d2$comp_key <- paste(d2$hatchery_location_code, d2$run, d2$stock_location_code, d2$release_location_code, d2$brood_year)

# Assign release groups ####

# Unique releases
unique_rels <- unique(d2$comp_key)

# Empty vector to put release ids 
rel.id <- vector("integer", length=length(d2$comp_key))

# Progress bar 
pb <- txtProgressBar(min=0,max=length(unique_rels),char="><((*> ", style=3, width=10)

# For each unique release group     
for (i in 1:length(unique_rels)) {
    # print(i)
  #i <- 2501
 # Subset original data for only the releases from unique release [i]
      d <- subset(d2, d2$comp_key == unique_rels[i], drop = F)    
  #d$LastRelDate
 #  The first release for hatchery/release site/ brood year [i] is #1
      id <- 1
          
 # While any tags in d have not been assigned a release id 
      while (any(rel.id[match(d$tag_code_or_release_id, d2$tag_code_or_release_id)] == 0)) {
           
       # If any avg weights for d are NA,     
           if (any(is.na(d$avg_weight))) {
                
               # Then identify which tags in d are within 21 days of the minimum date of all the tags in d,
               # the abs () is unnecessary
               g <- which(abs(d$LastRelDate - min(d$LastRelDate)) <= 21 ) } 
           
     # Otherwise, 
           else {
                
               # identify which tags were released within 21 days of the first release in d 
               # AND 
               # within 4.5 grams of the first release's avg.wt
               g <- which(abs(d$LastRel - min(d$LastRelDate)) <= 21 &  abs(d$avg_weight - min(d$avg_weight[which.min(d$LastRelDate)])) <= 4.5)
           }
           
     # Assign the tags identifed by g (above) the current release number
       rel.id[match(d$tag_code_or_release_id, d2$tag_code_or_release_id)[g]] <- id
     
     # Subset d for the tags with no assigned release numbers   
       d <- d[ which(rel.id[ match(d$tag_code_or_release_id, d2$tag_code_or_release_id) ] == 0), ]
                         #which(d2$tag %in% d$tag)
                         
     # Bump release number up one
       id <- id + 1
          
# Loop back around to While condition
      }

      # When while loop for hatchery/rel site/by [i] is done, bump up progress bar 
      # and loop around to next hatchery/release site/ brood year
      
      setTxtProgressBar(pb, i)
}

# Add the release ids to the data
d2$rel_id <-rel.id
#d2 %>% filter(tag_code_or_release_id=="220245")


# 220250
# 220362
# ")
#sqldf(paste("select",paste(key_cols,collapse=", "),"from d2",sep=" "))

# Summary qry's and juv. tag rate calcs ####

AD_CWT <-
  sqldf(
    "select 
      Key as comp_key,tag_code_or_release_id, rel_id, sum(ADcwt) as ADcwt from(

        select  
        comp_key as Key, 
        IfNull(sum(cwt_1st_mark_count),0) as ADcwt, 
        rel_id,
        tag_code_or_release_id
        from d2
        where cwt_1st_mark >= 5000
        group by comp_key,rel_id,tag_code_or_release_id
      union all
        select  
        comp_key as Key, 
        IfNull(sum(cwt_2nd_mark_count),0) as ADcwt, 
        rel_id,
        tag_code_or_release_id
        from d2
        where cwt_2nd_mark >= 5000
        group by comp_key, rel_id,tag_code_or_release_id)

      group by Key, rel_id, tag_code_or_release_id"
  )



AD_NOCWT <-  sqldf(
    "select 
      Key as comp_key,tag_code_or_release_id, rel_id, sum(ADnocwt) as ADnocwt from(

        select  
        comp_key as Key, 
        IfNull(sum(non_cwt_1st_mark_count),0) as ADnocwt, 
        rel_id,
        tag_code_or_release_id
        from d2
        where non_cwt_1st_mark >= 5000
        group by comp_key,rel_id,tag_code_or_release_id
      union all
        select  
        comp_key as Key, 
        IfNull(sum(non_cwt_2nd_mark_count),0) as ADnocwt, 
        rel_id,
        tag_code_or_release_id
        from d2
        where non_cwt_2nd_mark >= 5000
        group by comp_key, rel_id,tag_code_or_release_id)

      group by Key, rel_id, tag_code_or_release_id"
  )

UC_CWT <-  sqldf(
    "select 
      Key as comp_key,tag_code_or_release_id, rel_id, sum(UCcwt) as UCcwt from(

        select  
        comp_key as Key, 
        IfNull(sum(cwt_1st_mark_count),0) as UCcwt, 
        rel_id,
        tag_code_or_release_id
        from d2
        where cwt_1st_mark < 5000
        group by comp_key,rel_id,tag_code_or_release_id
      union all
        select  
        comp_key as Key, 
        IfNull(sum(cwt_2nd_mark_count),0) as UCcwt, 
        rel_id,
        tag_code_or_release_id
        from d2
        where cwt_2nd_mark < 5000
        group by comp_key, rel_id,tag_code_or_release_id)

      group by Key, rel_id, tag_code_or_release_id"
  )
  

UC_NOCWT <-  sqldf(
    "select 
      Key as comp_key,tag_code_or_release_id, rel_id, sum(UCnocwt) as UCnocwt from(

        select  
        comp_key as Key, 
        IfNull(sum(non_cwt_1st_mark_count),0) as UCnocwt, 
        rel_id,
        tag_code_or_release_id
        from d2
        where non_cwt_1st_mark < 5000
        group by comp_key,rel_id,tag_code_or_release_id
      union all
        select  
        comp_key as Key, 
        IfNull(sum(non_cwt_2nd_mark_count),0) as UCnocwt, 
        rel_id,
        tag_code_or_release_id
        from d2
        where non_cwt_2nd_mark < 5000
        group by comp_key, rel_id,tag_code_or_release_id)

      group by Key, rel_id, tag_code_or_release_id"
  )


# ")
# Merge all of the Ad/CWT combos above, replace the NA's with 0s
o <- Reduce(function(x, y) merge(x, y, all=T), list(AD_CWT, AD_NOCWT, UC_CWT, UC_NOCWT))
o[is.na(o)] <- 0

o <- o %>% mutate(DIT=ifelse(ADcwt==0,"DIT",NA))

# Calc tag rates
juv_tr_lu <- o %>% group_by(comp_key,rel_id) %>% 
  summarise(ADcwt=sum(ADcwt),ADnocwt=sum(ADnocwt),UCcwt=sum(UCcwt),UCnocwt=sum(UCnocwt)) %>% 
  left_join(select(o,tag_code_or_release_id,comp_key,rel_id,DIT),by=c("comp_key"="comp_key","rel_id"="rel_id")) %>% 
  mutate(NonMS=ADcwt/(ADcwt+ADnocwt+UCcwt+UCnocwt), MS=ADcwt/(ADcwt+ADnocwt)) %>% 
  ungroup %>% 
  left_join(select(d2, tag_code_or_release_id, record_code),by=c("tag_code_or_release_id"="tag_code_or_release_id")) %>% 
  filter(record_code=="T" & is.na(DIT)) %>% 
  select(TagCode=tag_code_or_release_id, NonMS, MS) %>% 
  gather(key=TagRateType,value="TR",c("NonMS","MS")) %>% arrange(TagCode) %>% as.data.frame

# Check missing tags
#mssng <- hatch_tags[which(!(hatch_tags %in% tr_lu$TagCode))]

# All dits?
#o %>%filter(tag_code_or_release_id %in% mssng) %>% summarise(ALL_DITS=all(DIT=="DIT"))
#head(o)

# Make the Juvy TR lookup table
#juv_tr_lu <- tr_lu %>% gather( key="TagRateType", value="TR",c("NS","MS")) %>% select(TagCode=tag,TagRateType,TR)
#head(juv_tr_lu)

# Now make lookup for Tag info including CWT'd wild stocks

all_tags <- RMIS_releases[RMIS_releases$record_code=="T" & RMIS_releases$species==1 & RMIS_releases$brood_year >= 2010,]

tag_lu <- RMIS_releases %>% 
  filter(record_code=="T" & species==1 & brood_year>=2010) %>% 
  select(tag=tag_code_or_release_id,agency=reporting_agency, hatch_code=hatchery_location_code,stock_code=stock_location_code,release_loc_code=release_location_code,run_code=run,brood_year) %>%

  # Lookup Hatchery location name
  left_join(filter(select(RMIS_locations,c(location_code,location_type, hatchery=name,hatch_reg=psc_region)), location_type==3),by=c("hatch_code"="location_code")) %>%
  
  # Lookup Stock name
  left_join(filter(select(RMIS_locations,c(location_code,location_type, stock=name,stock_reg=psc_region)), location_type==5), by=c("stock_code"="location_code")) %>%  
  
  # Lookup location name
  left_join(filter(select(RMIS_locations,c(location_code, location_type, release_loc=name,rel_reg=psc_region)),location_type==4),by=c("release_loc_code"="location_code")) %>%
  
     # Lookup Run
  left_join(select(RMIS_runs,c(run_code=run,run=run_name)),by=c("run_code"="run_code")) %>% 
  
  # Lookup DIT
  left_join(select(o,tag_code_or_release_id,DIT),by=c("tag"="tag_code_or_release_id"))
  
stopifnot(nrow(tag_lu)==nrow(all_tags))


# Drop codes 
  tag_lu <- tag_lu %>% select(-matches("_code|_type")) %>% 
  select(TagCode=tag, agency, run, hatchery, stock, release_loc, brood_year, hatch_reg, stock_reg, rel_reg, DIT)

  
# Write tbls to working dir ####
  #setwd("S:\Reg5\FP\ColumbiaRiverManagementUnit\FisheryModels\Tools\StockCompDoer\juv_tr_lookup.txt")
juv_tr_lu %>% write.table(file="Data/juv_tr_lookup.txt", sep="\t", row.names=FALSE)
tag_lu %>% write.table(file="Data/tag_info_lookup.txt", sep="\t", row.names=FALSE)


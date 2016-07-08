## Project title: MAP 
##    Created on: June 04 2016
##        Author: Jamie Knight
##          Data: ds0
##       Summary: Adding binary and grouping variables to MAP subset for use in Thesis
## ---------------------------------------------------------------------- ##

options(width=160)
rm(list=ls())
cat("\f")

# --- load-packages ------
# library(dplyr)
if (!require("magrittr")) install.packages('magrittr')
if (!require("Hmisc")) install.packages('Hmisc')
if (!require("pastecs")) install.packages('pastecs')
requireNamespace("dplyr")

# ----- load-data ------
#load data from the 1-subsetting file
#create the na.omit file after labeling: 4-apply-codebook
ds <- readRDS("./data/unshared/derived/ds0.rds") 
#ds0: 11225 obs of 28 vars

# ds0 <- read.csv("./data/derived/SPSS/MAPjk.csv") #csv version example code


#olfaction variable
Hmisc::describe(ds$total_smell_test) #4522, 32 unique, 6703 missing
Hmisc::describe(ds$projid) #11225, 1803 unique
names(ds)
is.numeric(ds$total_smell_test)

str(ds$age_death)

# ---- tweak-data ---------------------------------------
ds2 <- ds %>% 
  dplyr::mutate(
    vital_status    =  ifelse(is.na(age_death) , 0 , 1), # jamie, this is it.
    dementia_status =  ifelse(is.na(age_death) , 0 , 1),
    stroke_status   =  stroke_cum,
    path_status     =  ad_reagan,
    apoe_genotype   =  ifelse(apoe_genotype %in% c(44,34,24), 1, 0),
    group_smell     =  ordered(cut(total_smell_test, c(0,5,10,12), 
                                   labels=c("anosmic", "hyposmic", "normosmic")))
)
#can save here if needed, in order to load it into next file: 4-apply-codebook
saveRDS(ds2, "./data/derived/ds2.rds")

#######################################
## Below is the older version of the same code, do 






# ---- Groupings for smell ----------------- 
ds$group_smell <- ordered(cut(ds$total_smell_test, c(0,5,10,12), labels=c("anosmic", "hyposmic", "normosmic")))

levels(ds$group_smell)
summary(ds$group_smell)
# ---- Code binary variables for events ----------------- 
# event at 1: 
# 0 = alive/no event 
# 1 = dead/event

#make vital status a binary variable, 
subject_is_dead <- ds$age_death > 0 
depended_value <- ifelse (subject_is_dead, 1 , 0 )
# table(depended_value)
# d <- with ( ds, depended_value )
# table(d)  
ds$vital_status <- as.numeric ( with ( ds, depended_value ))
ds$vital_status[is.na(ds$vital_status)] <- 0
is.numeric(ds$vital_status)

#make vital status a binary variable, 
ds$vital_status <- as.numeric ( with ( ds, ifelse ( ( ds$age_death > 0 ), 1 , 0 ) ) )
ds$vital_status[is.na(ds$vital_status)] <- 0
is.numeric(ds$vital_status)


#code binary events for dementia 1 = event, 0 = no event
# could just Use dementia variable, but there are many missing from the new data set for that one
# use "dementia" after new data pull is implemented
# recode it instead for now, then compare it with what's available from dementia variable
ds$dementia_status <- as.numeric ( with ( ds, ifelse ( ( ds$cdx > 1 ), 1 , 0 ) ) )
unique.default(sapply(ds$dementia_status, unique)) #are there just 0's, NAs and 1's?
#try coding it this way:
# ds$dementia_status <- as.numeric ( with ( ds, ifelse ( ( ds$cdx == 1 ), 0 , 1 ) ) )


#code binary event for stroke
#use stroke_cum as the binary event for stroke.
ds$stroke_status <- ds$stroke_cum
#no need to re-code it
# ds$stroke_status <- as.numeric ( with ( ds, ifelse ( ( ds$stroke_cum < 4 ), 1 , 0 ) ) )
unique.default(sapply(ds$stroke_cum, unique)) #are there just 0's, NA's and 1's- yes
# ds$stroke_status[is.na(ds$stroke_status)] <- 0 #keep na's
is.numeric(ds$stroke_cum)
is.logical(ds$stroke_cum)

#binary pathology status at autopsy: adreagan based on niareagansc: have AD or not
is.numeric(ds$ad_reagan)#true
ds$path_status <- ds$ad_reagan
#no need to recode it
# ds$path_status <- as.integer ( with ( ds, ifelse ( ( ds$niareagansc < 3 ), 1 , 0 ) ) )
# ds$path_level[is.na(ds$path_level)] <- 0 #keep na's
is.integer(ds$path_status)

# ---- Groupings for apoe ----------------- 
#binary grouping for participants who have one copy of apoe allele #4
####help here####
dplyr::n_distinct(ds$apoe_genotype)#7
table(ds$apoe_genotype)
ds$group_apoe <- as.integer ( with ( ds, ifelse ( ( ds$apoe_genotype== 44 | ds$apoe_genotype== 34 | ds$apoe_genotype== 24 ), 1 , 0 ) ) )
dplyr::n_distinct(ds$group_apoe)#3
table(ds$group_apoe)
# 0    1 
# 6558 2052 

# for 1: should be: 108+1781+163=2052
#correct!


# ---- Groupings for smell ----------------- 
ds$group_smell <- ordered(cut(ds$total_smell_test, c(0,5,10,12), labels=c("anosmic", "hyposmic", "normosmic")))

levels(ds$group_smell)
summary(ds$group_smell)
# anosmic  hyposmic normosmic      NA's 
#       514      2776      1228      6707 


#------------------------- save files
ds2<-ds
#can save here if needed, in order to load it into next file: 4-apply-codebook
# saveRDS(ds2, "./data/derived/ds2.rds")

# next go to 3- data-cleaning then 4-apply-codebook
#after labels are applied, continue to transformations.

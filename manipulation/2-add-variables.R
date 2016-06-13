## Project title: MAP 
##    Created on: June 04 2016
##        Author: Jamie Knight
##          Data: ds1
##       Summary: Adding binary and grouping variables to MAP subset for use in Thesis
## ---------------------------------------------------------------------- ##

options(width=160)
rm(list=ls())
cat("\f")

# --- load-packages ------
library(dplyr)
requireNamespace("dplyr")
# library(survival)
library(Hmisc)
library(pastecs)

# ----- load-data ------
#load data from the 1-subsetting file
#create the na.omit file after labeling: 4-apply-codebook
ds <- readRDS("./data/derived/ds1.rds") 
#ds1: 8234 obs of 28 vars

# ds0 <- read.csv("./data/derived/SPSS/MAPjk.csv") #csv version example code


#olfaction variable
describe(ds$total_smell_test) #3312, 29 unique
describe(ds$projid) #8234, 1046 unique
names(ds)
is.numeric(ds$total_smell_test)

# ---- Code binary variables for events ----------------- 
# event at 1: 
# 0 = alive/no event 
# 1 = dead/event

#make vital status a binary variable, 
ds$vital_status <- as.numeric ( with ( ds, ifelse ( ( ds$age_death > 0 ), 1 , 0 ) ) )
ds$vital_status[is.na(ds$vital_status)] <- 0
is.numeric(ds$vital_status)

#code binary events for dementia 1 = event, 0 = no event
ds$dementia_status <- as.numeric ( with ( ds, ifelse ( ( ds$cdx > 1 ), 1 , 0 ) ) )
unique.default(sapply(ds$dementia_status, unique)) #are there just 0's and 1's?

#code binary event for stroke
ds$stroke_status <- as.numeric ( with ( ds, ifelse ( ( ds$stroke < 4 ), 1 , 0 ) ) )
unique.default(sapply(ds$stroke_status, unique)) #are there just 0's and 1's?
# ds$stroke_status[is.na(ds$stroke_status)] <- 0 #keep na's
is.numeric(ds$stroke_status)
is.logical(ds$stroke_status)

#make pathology status at autopsy a binary variable, have AD or not
ds$path_status <- as.integer ( with ( ds, ifelse ( ( ds$niareagansc < 3 ), 1 , 0 ) ) )
# ds$path_level[is.na(ds$path_level)] <- 0 #keep na's
is.integer(ds$path_status)


# ---- Groupings for smell ----------------- 
ds$group_smell <- ordered(cut(ds$total_smell_test, c(0,5,10,12), labels=c("anosmic", "hyposmic", "normosmic")))

levels(ds$group_smell)
summary(ds$group_smell)
#    anosmic  hyposmic normosmic      NAs 
#       281      2019      1012      4922


#------------------------- save files
ds2<-ds
#can save here if needed, in order to load it into next file: 4-apply-codebook
saveRDS(ds2, "./data/derived/ds2.rds")

# next go to 3-apply-codebook
#after labels are applied, continue to transformations.

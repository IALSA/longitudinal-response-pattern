## Project title: MAP 
##    Created on: June 03 2016
##    Updated on: July 05th 2016
##        Author: Jamie Knight
##          Data: Subsetting variables in MAP for use in Thesis
##       Summary: Using Andrey Koval's base script file and subsetting to what is needed
##                for the Stroke project and my thesis data
## ---------------------------------------------------------------------- ##

options(width=160)
rm(list=ls())
cat("\f")

# --- load-packages ------

search() # see which pachages are currently attached
if (!require("dplyr")) install.packages('dplyr')
if (!require("magrittr")) install.packages('magrittr')
if (!require("Amelia")) install.packages('Amelia')
# requireNamespace("dplyr")



# ----- load-data ------
getwd()
ds <-readRDS("./data/derived/ds0_raw.rds")
str(ds) 
names(ds)

# ---- rename-variables ---------

ds <- plyr::rename(x=ds, replace = c(
  "msex"="sex",
  "dcfdx.x" = "cdx", #alive cognitive diagnosis
  "cogdx" = "final_dx", #autopsy diagnosis
  "cts_mmse30" = "mmse",
  "r_stroke" = "stroke",
  "age_bl.x" = "age_bl",
  "age_death.x" = "age_death",
  "educ.x" = "educ",
  "msex.x" = "sex",
  "race.x" = "race",
  "spanish.x" = "spanish",
  "age_at_visit.x" = "age_at_visit"

))

names(ds)

# ---- subsetting ---------
#calling this file dsw
names(ds)

####verify cog tests needed here####
jkvars <- c("projid", "fu_year","cdx", "final_dx", "dementia", "age_bl", "age_death", "age_at_visit", "race", "educ", "sex", "braaksc", "ceradsc", "niareagansc","ad_reagan" , "apoe_genotype", "total_smell_test", "stroke", "stroke_cum", "vasc_risks_sum", "cogn_ep", "cogn_global", "mmse", "cts_ebmt", "cts_ebdr", "cts_story", "cts_delay")
dsw <- ds[jkvars]

## compute exact time in study
dsw$time_yr <- dsw$age_at_visit - dsw$age_bl #this is optional, essentially same as fu_year

# ---- explore ---------
# number of uniuqe ids
dsw$projid %>% unique() %>% length() #1803

#check missing data
#this can be intensive, save everything in case computer crashes 
missmap(dsw, main = "Missing values vs observed") 

dplyr::n_distinct(ds$fu_year) #20 waves
dplyr::n_distinct(ds$projid) #1803 subjects

#check to see if there are any duplicates by looking at projid and fu_year duplicates
dsd <- ds %>% dplyr::group_by(projid, fu_year) %>% filter(n()>1) %>% dplyr::summarise(n=n())
#there are no duplicates - good
rm(dsd)


# ---- save ---------
#save data as ds0
ds0 <- dsw
saveRDS(ds0, "./data/derived/ds0.rds")



#------------------------- next add variables

#after the base subsets have been completed:
#go to 2-add-variables and create binary: stroke, dementia, pathology level and vital status
#and the grouping: group_smell

#add those for the final data set then go to 3-apply-codebook
#after labels are applied, continue to transformations.

# 5-transformations
#after 3, take the final data set and get a wide version of it and an na.omit subset
#reshape data from long to wide

#then continue to #6-assumtions 


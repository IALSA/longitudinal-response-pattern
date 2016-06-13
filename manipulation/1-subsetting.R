## Project title: MAP 
##    Created on: June 03 2016
##        Author: Jamie Knight
##          Data: Subsetting MAP for use in Thesis
##       Summary: Using Andrey Koval's base script file and subsetting to what is needed
##                for the Stroke project and my thesis data
## ---------------------------------------------------------------------- ##

options(width=160)
rm(list=ls())
cat("\f")

# --- load-packages ------

search() # see which pachages are currently attached
library(dplyr)
requireNamespace("dplyr")
library(Amelia)#missmap


# ----- load-data ------
getwd()
ds <-readRDS("./data/derived/ds0_raw.rds")
str(ds) 
names(ds)

# ---- rename-variables ---------

ds <- plyr::rename(x=ds, replace = c(
  "msex"="sex",
  "dcfdx" = "cdx", #alive cognitive diagnosis
  "cogdx" = "final_dx", #autopsy diagnosis
  "cts_mmse30" = "mmse",
  "r_stroke" = "stroke"
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
dsw$projid %>% unique() %>% length() #1804

#check missing data
#this can be intensive, save everything in case computer crashes 
missmap(dsw, main = "Missing values vs observed") 

dplyr::n_distinct(ds$fu_year) #20 waves
dplyr::n_distinct(ds$projid) #1804 subjects

dplyr::n_distinct(sub1$fu_year)#19 waves
dplyr::n_distinct(sub1$projid) #1460 subjects (344 less)


####help####
#check to see if there are any duplicates by looking at projid and fu_year duplicates
# ie. person 9121 has a duplicate of fu_year 4, one with more information than the other.

# ---- subset-base -----------------

# base subset should include people who do not have dementia at baseline
# have 3+ waves of data, including 3+ occasions of BSIT measurement

# count how many times each id is measured in the dataset
d <- dsw %>% dplyr::group_by(projid) %>% dplyr::summarize(n_waves = n_distinct(fu_year))
d <-as.data.frame(d) #neccesary step, will not work if you disclude this
# create a list of ids that have 3 or more waves of observation
keep_ids <- d[d$n_waves > 2, "projid"]
# subset the original file, keeping only the ids with more than 3 waves of observation
sub1 <- dsw %>% dplyr::filter(projid %in% keep_ids)
# verify that each unique id has more than one observation wave
d <- sub1 %>% dplyr::group_by(projid) %>% dplyr::summarize(n_waves = n_distinct(fu_year))
d <- as.data.frame(d)
table(d$n_waves)
# number of waves:                          3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19 
# number of participants with that many:   181 212 172 163 146  92  91  81  89  78  71  47  12   6   7  11   1 


#subset to remove any participants with dementia at baseline
#cdx is assesment while alive, 1:normal, 2-3:mild MCI, 4-5:AD
varsREM <- sub1[ which(sub1$fu_year==0 & sub1$cdx != 1),]


sub2 <- sub1
for (rows in varsREM["projid"]) {
  for (id in rows) {
    sub2 <- sub2[sub2$projid != id, ]
  }
}

dplyr::n_distinct(sub2$projid)#1046 (414 less)
dplyr::n_distinct(sub1$projid) #1460 subjects 
dplyr::n_distinct(varsREM$projid) #414 
#correct numer was removed

#sub2 = all people who were normal at baseline and have 3+ waves of data
#1046
?n()
####help####
#now filter out those who have less than 3 measures of total_smell_test
#now filter out those who have less than 3 measures of total_smell_test
d2 <- sub2 %>% group_by(projid) %>% count(total_smell_test) %>%  summarize(total_smell_test) # %>% mutate(total_smell_test, n= n())
d2 <- as.data.frame(d2)
# create a list of ids that have 3 or more waves of observation
keep_ids <- d2[d2$n_smell > 2, "projid"]
# subset the original file, keeping only the ids with more than 3 waves of observation
sub3 <- sub2 %>% dplyr::filter(projid %in% keep_ids)
# verify that each unique id has more than one observation wave
d <- sub3 %>% dplyr::group_by(projid) %>% dplyr::summarize(n_waves = n_distinct(fu_year))
d <- as.data.frame(d)
table(d$n_waves) #only 8 people at wave 23

# ---- save ---------
#save subset data as ds1
ds1<-sub2
saveRDS(ds1, "./data/derived/ds1.rds")

#------------------------- next add variables

#after the base subsets have been completed:
#go to 2-add-variables and create binary: stroke, dementia, pathology level and vital status
#and the grouping: group_smell

#add those for the final data set then go to 3-apply-codebook
#after labels are applied, continue to transformations.

# 4-transformations
#after 3, take the final data set and get a wide version of it and an na.omit subset
#reshape data from long to wide
names(MAPo)
MAPol <- select(MAPo, projid, fu_year, dementia, r_stroke, cogn_ep, cogn_global, cts_mmse30, stroke_cum, vasc_risks_sum) #remove excess variables
names(MAPol)

max(MAPol$fu_year) #16

measures <- names(MAPol)[c(-1,-2)]
measures

MAPow <- reshape(MAPol, v.names=measures, timevar='fu_year', idvar='projid', direction="wide")
names(MAPow)

#then continue to #5-assumtions 


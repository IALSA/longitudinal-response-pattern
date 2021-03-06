## Project title: MAP 
##    Created on: June 30 2016
##        Author: Jamie Knight
##          Data: RADC MAP and ROS
##       Summary: Using Andrey Koval's base script file merging new MAP files to existing ones on IALSA server
##                Then splitting the files so there are separate ones for MAP and ROS
## ---------------------------------------------------------------------- ##

rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console

#set up project 
getwd()

# --- load-packages ------

search() # see which pachages are currently attached
if (!require("gdata")) install.packages('gdata') #read.xls
if (!require("dplyr")) install.packages('dplyr')
if (!require("magrittr")) install.packages('magrittr')
if (!require("readr")) install.packages('readr')


if (!require("reshape2")) install.packages('reshape2')
if (!require("psych")) install.packages('psych')
if (!require("Hmisc")) install.packages('Hmisc')



# ----- import-raw-files ------

# ---------------------
# # old version
# # new files 2016:
# longa <- read.xls("./data/raw/dataset_465_long.xls") #long format, 23918, 6 vars new data
# basic <- read.xls("./data/raw/dataset_465_basic.xls") #wide format new data
# #notes: basic has 3125 unique id's and 1803 unique id's in MAP
# #old original file from RADC 2014:
# longb <- read.xls("./data/raw/From IALSA server/dataset_285_long03-2014.xlsx")
# # 26707, 89
# oldbasic <- read.xls("./data/raw/From IALSA server/dataset_285_basic03-2014.xlsx")
# #notes: oldbasic has 3477 unique id's, in 3 different studies (MAP,MARS,ROS)
# # and 1696 unique id's in MAP, less than the above: could use left join
# ---------------------

#you need to create csv's from the raw .xls sent from RADC
longaPath    <- "./data/unshared/raw/dataset_465_long.csv"
basicPath    <- "./data/unshared/raw/dataset_465_basic.csv"
longbPath    <- "./data/unshared/raw/dataset_285_long_03-2014.csv" 
oldbasicPath <- "./data/unshared/raw/dataset_285_basic_03-2014.csv"

testit::assert("No such file exists", base::file.exists(longaPath) )
testit::assert("No such file exists", base::file.exists(basicPath) )
testit::assert("No such file exists", base::file.exists(longbPath) )
testit::assert("No such file exists", base::file.exists(oldbasicPath) )

longa <- readr::read_csv(longaPath)
basic <- readr::read_csv(basicPath)
longb <- readr::read_csv(longbPath)
oldbasic <- readr::read_csv(oldbasicPath)


####new####
#note June 30th,2016: get a new pull of all vaiables - some are missing in new data 

# ---- view_data -------------------------------------------------
#which variables overlap in the data?
(namesB1 <- names(basic)) #14, no apoe
(namesB2 <- names(oldbasic)) #26, includes apoe
(namesLonga <- names(longa)) #6
(namesLongb <- names(longb)) #89

intersect(namesB1, namesB2) # common variables
# [1] "projid"    "study"     "scaled_to" "age_bl"    "age_death" "educ"     
# [7] "msex"      "race"      "spanish" 

intersect(namesLonga, namesLongb) # common variables
# [1] "projid"       "study"        "fu_year"      "dcfdx"        "age_at_visit"

#check unique ids
basic$projid %>% unique() %>% length() #3125
oldbasic$projid %>% unique() %>% length() #3477, more participants in the old pull?

# ----- merge ------
#update june 14 changed method of merging as there are errors/duplicate entried if you do not use this method:
# merge old version of basic and long together into one long format file
# then merge new version of basic and long togethe into long format file
# then join both longs together with: df = left_join(demo, df)

d_wide <- oldbasic %>% 
  dplyr::filter(study=="MAP")
d_long <- longb %>% 
  dplyr::filter(study=="MAP")

old <- dplyr::full_join(d_wide, d_long, by = c("projid","study"))
old %>% 
  dplyr::group_by(projid, fu_year) %>% 
  dplyr::filter(n()>1) %>% 
  dplyr::summarise(n=n())


# merge old version of basic and long together into one long format file
# old <- dplyr::full_join(oldbasic, longb, by = c("projid","study"))
#reorder variables, 119
old1 <- dplyr::select(old, projid, study, scaled_to=scaled_to.x, fu_year, everything()) 
names(old)
names(old1) #fine
old <- old1
rm(old1)

# ids <- c("00831405","00831081")
# d_wide <- oldbasic %>% 
#   dplyr::filter(projid %in% ids)
# t(d_wide)
  
# 
# dsd0 <- old %>% dplyr::group_by(projid, fu_year) %>% dplyr::filter(n()>1) %>% dplyr::summarise(n=n())
# 
# ds_duplicats <- old %>% 
#   dplyr::group_by(projid, study, fu_year) %>% 
#   dplyr::filter(n()>1) %>% 
#   dplyr::summarise(n=n()) %>% 
#   dplyr::ungroup() %>% 
#   dplyr::group_by(study) %>% 
#   dplyr::summarize(n=())
# 
# ds_duplicats %>% 
#   dplyr::group_by(study)

# merge new version of basic and long togethe into long format file
d_wide_new <- basic %>% 
  dplyr::filter(study=="MAP")
d_long_new <- longa %>% 
  dplyr::filter(study=="MAP")

new <- dplyr::full_join(d_wide_new, d_long_new, by = c("projid","study"))

#what names are overlapping?
(namesold <- names(old)) #113
(namesnew <- names(new)) #18
intersect(namesold, namesnew) #12 vars in common
# [1] "projid"       "study"        "scaled_to"    "fu_year"      "age_bl"      
# [6] "age_death"    "educ"         "msex"         "race"         "spanish"     
# [11] "dcfdx"        "age_at_visit"

ds1 <- dplyr::left_join(new, old, by = c("projid","study", "fu_year"))
names(ds1)
#duplicates ".y" for vars that intersect above + one double of "scaled_to.y"
ds1 <- ds1[ , !duplicated(colnames(ds1))] #removes the double of "scaled_to.y"
ds1 <- dplyr::select(ds1, projid, study, scaled_to=scaled_to.x, fu_year, everything(), -contains(".y")) 
# removed duplicated of the intersected vars: -scaled_to.y, -age_bl.y,-age_death.y, -educ.y,-msex.y, -race.y ,-spanish.y, -dcfdx.y) 
#0-6 fu_years with moreinfo from the new data, missing info from old data
names(ds1)#118

####of note####
# some variables that are constant, ie apoe, not carrying all the way down
# this is due to missing apoe in the new data but since apoe is constant can we somehow fill the data in?


# ----- verification ------
#check variables
names(basic)
names(wideb)
names(longa)
names(longb)

#check number of unique participants 
ds1$projid %>% unique() %>% length() #3125, left join
ds1$fu_year %>% unique() %>% length() #24, left join

#check for duplicates
dsd0 <- ds1 %>% dplyr::group_by(projid, fu_year) %>% dplyr::filter(n()>1) %>% dplyr::summarise(n=n())
# dsd0 shoud have none = no duplicates!!! yaya! use LEFT JOIN
rm(dsd0)


ds1$projid %>% unique() %>% length() #3125
basic$projid %>% unique() %>% length() #3125
longa$projid %>% unique() %>% length() #3124
longb$projid %>% unique() %>% length() #3476 - why more here?? this is the old file, participants die off??
oldbasic$projid %>% unique() %>% length() #3477

# check for other studies
table(ds1$study)
# check for duplication
ds1 %>% 
  dplyr::group_by(projid, fu_year) %>% 
  dplyr::filter(n()>1) %>% 
  dplyr::summarise(n=n())

# ---- save -----
saveRDS(ds1,"./data/unshared/derived/ds0_raw.rds")

#now import this file into 1-subsetting
#or skip straight to 4-apply-codebook for GitHub
#after labels but before subsetting you can save a version called ds0


####INVESTIGATE####
#what's the difference between left and full?

# left_join(x, y): Return all rows from x, and all columns from x and y. 
# If there are multiple matches between x and y, all combination of the matches are returned. This is a mutating join.

# full_join(x, y): Return all rows and all columns from both x and y.
# Where there are not matching values, returns NA for the one missing. This is a mutating join.


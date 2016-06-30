## Project title: MAP 
##    Created on: June 03 2016
##        Author: Jamie Knight
##          Data: RADC MAP and ROS
##       Summary: Using Andrey Koval's base script file merging new MAP files to existing ones on IALSA server
##                Then splitting the files so there are separate ones for MAP and ROS
## ---------------------------------------------------------------------- ##


rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console

#set up project or
getwd()

# --- load-packages ------

search() # see which pachages are currently attached
# install.packages("gdata")
library("gdata") #read.xls
requireNamespace("dplyr")
library("reshape2")
requireNamespace("testit")
# ----- import_raw_files ------
#new data from RADC 2016
#need to change pathway to fit where you have saved these files
longaPath    <- "./data/unshared/raw/dataset_465_long.csv"
basicPath    <- "./data/unshared/raw/dataset_465_basic.csv"
longcPath    <- "./data/unshared/raw/dataset_285_long03-2014.csv" 
oldbasicPath <- "./data/unshared/raw/dataset_285_basic03-2014.csv"

testit::assert("No such file exists", base::file.exists(longaPath) )
testit::assert("No such file exists", base::file.exists(basicPath) )
testit::assert("No such file exists", base::file.exists(longcPath) )
testit::assert("No such file exists", base::file.exists(oldbasicPath) )

### THIS IS A FAR AS WE GOT ON 2016-06-16 

longa <- readr::read_csv(longaPath)
basic <- readr::read_csv(basicPath)

#notes: basic has 3125 unique id's and 1803 unique id's in MAP

#old original file from RADC 2014
#need to change pathway to fit where you have saved these files
longc <- readr::read_csv(longcPath)
# 26707, 89
oldbasic <- readr::read_csv(oldbasicPath)
#notes: oldbasic has 3477 unique id's, in 3 different studies (MAP,MARS,ROS) 
# and 1696 unique id's in MAP, less than the above: could use left join


# ---- view_data -------------------------------------------------
#which variables overlap in the data?
(namesB1 <- names(basic)) #14
(namesB2 <- names(oldbasic)) #26
(namesLonga <- names(longa)) #6
(namesLongc <- names(longc)) #89

intersect(namesB1, namesB2) # common variables
# [1] "projid"    "study"     "scaled_to" "age_bl"    "age_death" "educ"     
# [7] "msex"      "race"      "spanish" 

intersect(namesLonga, namesLongc) # common variables
# [1] "projid"       "study"        "fu_year"      "dcfdx"        "age_at_visit"

#check unique ids
basic$projid %>% unique() %>% length() #3125
oldbasic$projid %>% unique() %>% length() #3477, more participants?

# ----- merge 1 ------
#option 1 of 2
#first merge both basic data sets, 
#then merge the basic and long data sets with fulljoin

# 1a
#merge basic and old basic using leftjoin in dplyr
dsb0 <- dplyr::left_join(basic, oldbasic, by = c("projid","study"))
#after merge there are 3125 participants
# 1803 in MAP, but when used full join it was 1804 (where is the one more?)

names(dsb0)
#subset out some of the copied variables, ie race.x & race.y, remove the y's
dsb01 <- select(dsb0, everything(), -ends_with(".y"))
names(dsb01)
dsb02 <- select(dsb01, projid, study, cogdx, scaled_to=scaled_to.x, age_bl=age_bl.x, age_death=age_death.x, educ=educ.x, msex=msex.x,race=race.x,spanish=spanish.x, everything())

#check variables from dsb, dsb0, dsb01 and dsb02 - all looks good
dsb <- dsb02 #final version of both old and new basic files
rm(basic)#clean up any old versions
(namesB3 <- names(dsb)) #merged basic files= 31 (14+26=40, minus the common 9 = 31) correct!

#1b. join the 2 long datasets together
dsbl <- dplyr::full_join(longa, longc, by = c("projid","study", "fu_year","dcfdx", "age_at_visit" ))
# left join 23918, 90
# Warning message:
#   In outer_join_impl(x, y, by$x, by$y) :
#   joining factors with different levels, coercing to character vector
####error####
#possibly 2 things thar were not supposed to be merged got merged here, or something was coded wrong
#go back and see what variables are factors : as.factor (june3, 2016)
str(dsbl) #study is no longer a factor in the merged data - why would that be? - 
is.factor(dsbl$study)
####add factors back in####
#with MAP as 1

#what variables are overlapping between the basic and long merges?
(namesLong3 <- names(dsbl)) #90
intersect(namesLong3, namesB3)
# [1] "projid"    "study"     "scaled_to"

#1c. final join of dsb and dsl final basic & long data sets
ds0 <- dplyr::full_join(dsb, dsbl, by = c("projid","study"))
#reorder variables, 119
ds0 <- dplyr::select(ds0, projid, study, scaled_to=scaled_to.x, fu_year, everything(), -scaled_to.y) 
rm(ds01)

# ds0 #full merge long format data

# ----- verification ------
#check variables
names(basic)
names(wideb)
names(longa)
names(longb)

#check number of unique participants before and after merge
ds0$projid %>% unique() %>% length() #3730, full merge
longa$projid %>% unique() %>% length() #3124
longc$projid %>% unique() %>% length() #3476

dsb$projid %>% unique() %>% length() #3690
dsml$projid %>% unique() %>% length() #3475

radc$projid %>% unique() %>% length()  #3786

# ----- RADC to MAP ------
#subset down to just MAP

## write out only MAP project data
ds0$study<- as.factor(ds0$study)
is.factor(ds0$study)
levels(ds0$study) #3
subsets<-split(ds0, ds0$study, drop=TRUE)
ds0 <-subsets[[1]]
rm(ds1)
class(ds0)

# ---- save -----
saveRDS(ds0,"./data/derived/ds0_raw.rds")

#now import this file into 1-subsetting
#or skip straight to 4-apply-codebook for GitHub
#after labels but before subsetting you can save a version called ds0


#june 3,2016 : will work on completing this second method in order to double check that
#the output is the same from both methods of merging
# ----- merge 2 ------
#2a
# merge old version of basic and long together into one long format file
# then merge new version of basic and long togethe into long format file
# then join both longs together with: df = inner_join(demo, df), or full_join




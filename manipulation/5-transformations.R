## Project title: MAP 
##    Created on: June 03 2016
##        Author: Jamie Knight
##          Data: ds4
##       Summary: Making final formats: wide format and na.omit to use in Thesis.
##                
## ---------------------------------------------------------------------- ##

options(width=160)
rm(list=ls())
cat("\f")

# ---- load_packages ----------------------------------
library(dplyr)
library(stats)
search()

# ----- load-data ------
getwd()
ds <-readRDS("./data/derived/ds4.rds")
str(ds) 
names(ds)
# ----- long-to-wide ------
# take the final data set and get a wide version of it and an na.omit subset
####this is done in 7-subsets for analysis####
#skip
#reshape data from long to wide
names(ds)
# dsw <- select(ds, projid, fu_year, dementia, r_stroke, cogn_ep, cogn_global, cts_mmse30, stroke_cum, vasc_risks_sum) #remove excess variables, if needed

max(ds$fu_year, na.rm = T) #18

#time invariant : -1, -2, -4, -6, -7, -9:-16
measures <- names(ds)[c(-1,-2, -4, -6, -7, -9:-16, -28, -33, -36)] #discluded projid and fu_year
measures #time varying measures

dsw <- reshape(ds, v.names=measures, timevar='fu_year', idvar='projid', direction="wide")
names(dsw)

# --------version-2-------
# what about using time-yr as the time var? use this in SPSS analysis for cox
# names(ds)
# measures2 <- names(ds)[c(-1,-2, -4, -6, -7, -9:-16, -28, -33, -36)] #discluded projid and fu_year
# measures2 #time varying measures
# dsw2 <- reshape(ds, v.names=measures, timevar='time_yr', idvar='projid', direction="wide")
# names(dsw2)

# ---- checks ---------
#check unique ids
#wide
dsw$projid %>% unique() %>% length() #1803
#long
ds$projid %>% unique() %>% length() #1803
#they are the same

#what other checks can be done?



# ---- na.omit ---------
####maybe don't do these###


# omit missing data complete cases on ds: ds4ls
#reconvert that small version to a wide version: ds4ws
ds4ls<- na.omit(ds)
ds4ls$projid %>% unique() %>% length() #311 people with complete files (vs.1046)
max(ds4ls$fu_year) #16 waves vs. 18

#convert this to an na.omit wide version
names(ds4ls)
#time invariant : -1,-2, -4, -6, -7, -9:-16, -28, -32, -33, -36
measures <- names(ds4ls)[c(-1,-2, -7, -9:-16, -28, -32, -33, -36)] #discluded projid and fu_year
measures

ds4ws <- reshape(ds4ls, v.names=measures, timevar='fu_year', idvar='projid', direction="wide")
names(dsw)

#check unique id's
ds4ls$projid %>% unique() %>% length() #299 people with complete files (vs.1046) in long
ds4ws$projid %>% unique() %>% length() #299 people with complete files in wide 
#same in both

####complete cases####
names(ds)
#different method try with long data, complete cases for these vars: 
# complete cases for BSIT and niareagansc 
ds4ls2<-ds[complete.cases(ds[,14, 17]),]
ds4ls2$projid %>% unique() %>% length() #650 people 

#complete cases for BSIT
ds4ls3<-ds[complete.cases(ds[,17]),]
ds4ls3$projid %>% unique() %>% length() #1660 - a lot more!

# #what if we select complete cases for all columns?
# ds4ls3<-ds4l[complete.cases(ds4l[,1:27]),]
# ds4ls3$projid %>% unique() %>% length() #299 people - same as na.omit
# #select complete cases for all variables used in analyses: 16:27 =751; 9:27=305; c(9:11, 16:27)=751
# ds4ls4<-ds4l[complete.cases(ds4l[,c(9:11, 16:27)]),]
# ds4ls4$projid %>% unique() %>% length() #811 people

#select complete cases for age of death and BSIT
# ds4ls5<-ds[complete.cases(ds[,c(7, 17)]),]
# ds4ls5$projid %>% unique() %>% length() #703 people


# ---- save ---------
#save subset data as ds4

#original long
ds4l<-ds
saveRDS(ds4l, "./data/derived/ds4l.rds")
write.csv(ds4l, "./data/derived/CSV/ds4l.csv")

#original wide
ds4w <- dsw
saveRDS(ds4w, "./data/derived/ds4w.rds")
write.csv(ds4w, "./data/derived/CSV/ds4w.csv")

#subsetted complete cases versions
ds4ls <- ds4ls2
str(ds4ls)
saveRDS(ds4ls, "./data/derived/ds4ls.rds")
write.csv(ds4ls, "./data/derived/CSV/ds4ls.csv")
# write.csv(ds4ls5, "./data/derived/CSV/ds4ls5.csv")
ds4ls2 <- ds4ls3
str(ds4ls)
saveRDS(ds4ls2, "./data/derived/ds4ls2.rds")
write.csv(ds4ls2, "./data/derived/CSV/ds4ls2.csv")


# #wide version of the na.omit
# str(ds4ws)
# saveRDS(ds4ws, "./data/derived/ds4ws.rds")

# continue to #6-assumtions


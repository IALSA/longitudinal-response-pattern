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

# ----- load-data ------
getwd()
ds <-readRDS("./data/derived/ds4.rds")
str(ds) 
names(ds)
# ----- long-to-wide ------
# take the final data set and get a wide version of it and an na.omit subset

#reshape data from long to wide
names(ds)
# dsw <- select(ds, projid, fu_year, dementia, r_stroke, cogn_ep, cogn_global, cts_mmse30, stroke_cum, vasc_risks_sum) #remove excess variables, if needed

max(ds$fu_year) #18

#time invariant : -1, -2, -4, -6, -7, -9:-16
measures <- names(ds)[c(-1,-2, -4, -6, -7, -9:-16)] #discluded projid and fu_year
measures

dsw <- reshape(ds, v.names=measures, timevar='fu_year', idvar='projid', direction="wide")
names(dsw)


# ---- checks ---------
#check unique ids
#wide
dsw$projid %>% unique() %>% length() #1046
#long
ds$projid %>% unique() %>% length() #1046
#they are the same

#what other checks can be done?



# ---- na.omit ---------
# omit missing data complete cases on ds: ds4ls
#reconvert that small version to a wide version: ds4ws
ds4ls<- na.omit(ds)
ds4ls$projid %>% unique() %>% length() #299 people with complete files (vs.1046)
max(ds4ls$fu_year) #15 waves vs. 18

#convert this to an na.omit wide version
names(ds4ls)
#time invariant : -1, -2, -4, -6, -7, -9:-16
measures <- names(ds4ls)[c(-1,-2, -4, -6, -7, -9:-16)] #discluded projid and fu_year
measures

ds4ws <- reshape(ds4ls, v.names=measures, timevar='fu_year', idvar='projid', direction="wide")
names(dsw)

#check unique id's
ds4ls$projid %>% unique() %>% length() #299 people with complete files (vs.1046) in long
ds4ws$projid %>% unique() %>% length() #299 people with complete files in wide 
#same in both

names(ds4l)
#different method try with long data, complete cases for these vars: 17:27
ds4ls2<-ds4l[complete.cases(ds4l[,17:27]),]
ds4ls2$projid %>% unique() %>% length() #965 people - a lot more!
#what if we select complete cases for all columns?
ds4ls3<-ds4l[complete.cases(ds4l[,1:27]),]
ds4ls3$projid %>% unique() %>% length() #299 people - same as na.omit
#select complete cases for all variables used in analyses: 16:27 =751; 9:27=305; c(9:11, 16:27)=751
ds4ls4<-ds4l[complete.cases(ds4l[,c(9:11, 16:27)]),]
ds4ls4$projid %>% unique() %>% length() #751 people

# ---- save ---------
#save subset data as ds4

#original long
ds4l<-ds
saveRDS(ds4l, "./data/derived/ds4l.rds")
#subsetted na.omit version
str(ds4ls)
saveRDS(ds4ls, "./data/derived/ds4ls.rds")

#wide version
ds4w<-dsw
saveRDS(ds4w, "./data/derived/ds4w.rds")
#wide version of the na.omit
str(ds4ws)
saveRDS(ds4ws, "./data/derived/ds4ws.rds")

# continue to #5-assumtions


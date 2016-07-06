## Project title: MAP 
##    Created on: June 08 2016
##        Author: Jamie Knight
##          Data: ds2
##       Summary: Data Cleaning
##                
## ---------------------------------------------------------------------- ##

options(width=160)
rm(list=ls())
cat("\f")
# ---- load_packages ----------------------------------
requireNamespace("dplyr")

# ----- load-data ------
getwd()
ds <-readRDS("./data/derived/ds2.rds")
str(ds) 
names(ds)

dplyr::n_distinct(ds$projid) #1803


# ----- constant-variables ------
####help###
# apoe needs to have the values come all the way down the column!
# install.packages("zoo")
library(zoo)
?na.locf #last observation carried forward

any(is.na(ds$apoe_genotype))
ds$apoe_genotype <- na.locf(ds$apoe_genotype)
ds$apoe <- na.locf(ds$apoe)
#any other missing constant values?
any(is.na(ds$race))
any(is.na(ds$sex)) #false
any(is.na(ds$educ))
any(is.na(ds$apoe))

####problem####
#omit's NA's for people who are missing the data, need to do it by person only.

# ----- deletions ------

# ----- BSIT score ------
# In the MAP data, for the BSIT scores, they have assigned 0.25 to missing responses to a maximum of two; 
# if more than two response were missing, the entire test was treated as missing). 

####potential replication issue here - all papers include the decimals ####

#here we remove everything past the decimal to get an even number.

n_distinct(ds$total_smell_test)#33, should be 12
table(ds$total_smell_test)

ds$total_smell_test[ds$total_smell_test == 2.25] <- 2
ds$total_smell_test[ds$total_smell_test == 2.5] <- 2
table(ds$total_smell_test)

ds$total_smell_test[ds$total_smell_test == 3.25] <- 3
ds$total_smell_test[ds$total_smell_test == 3.5] <- 3
ds$total_smell_test[ds$total_smell_test == 4.25] <- 4
ds$total_smell_test[ds$total_smell_test == 4.5] <- 4
ds$total_smell_test[ds$total_smell_test == 5.25] <- 5
ds$total_smell_test[ds$total_smell_test == 5.5] <- 5
ds$total_smell_test[ds$total_smell_test == 6.25] <- 6
ds$total_smell_test[ds$total_smell_test == 6.5] <- 6
ds$total_smell_test[ds$total_smell_test == 7.25] <- 7
ds$total_smell_test[ds$total_smell_test == 7.5] <- 7
ds$total_smell_test[ds$total_smell_test == 8.25] <- 8
ds$total_smell_test[ds$total_smell_test == 8.5] <- 8
ds$total_smell_test[ds$total_smell_test == 9.25] <- 9
ds$total_smell_test[ds$total_smell_test == 9.5] <- 9
ds$total_smell_test[ds$total_smell_test == 10.25] <- 10
ds$total_smell_test[ds$total_smell_test == 10.5] <- 10
ds$total_smell_test[ds$total_smell_test == 11.25] <- 11
table(ds$total_smell_test)
# 0   1   2   3   4   5   6   7   8   9  10  11  12 
# 4  18  50 100 167 193 305 374 478 723 909 836 365  

n_distinct(ds$total_smell_test)#14 - should be 13 +  NAs
glimpse(ds)
str(ds$total_smell_test)
class(ds$total_smell_test)

# ---- explore ---------
str(ds)
glimpse(ds)
summary(ds)

plot(ds$fu_year, ds$age_at_visit)

# ---- variable-types ---------
#setting the propper variable types to the variables
names(ds)
class(ds$age_bl)
levels(ds$group_smell)

#total smell test as intger.
ds$total_smell_test <- as.integer(ds$total_smell_test)
n_distinct(ds$total_smell_test) #14
unique.default(sapply(ds$total_smell_test, unique))
# 8  9 10 NA 11  6  7  4 12  2  5  1  3
#should these be ordered?
is.ordered(ds$total_smell_test)#no
ds$BSIT <- as.ordered(ds$total_smell_test)
levels(ds$BSIT)
is.factor(ds$BSIT) #true
unique.default(sapply(ds$BSIT, unique))
# [1] 8    9    10   <NA> 11   6    7    4    12   2    5    1    3   
# Levels: 1 2 3 4 5 6 7 8 9 10 11 12
n_distinct(ds$BSIT) #14


# apoe_genotype as factor with 3 levels:
summary(ds$apoe_genotype)
n_distinct(ds$apoe_genotype) #7
unique.default(sapply(ds$apoe_genotype, unique))
# 34 33 NA 23 24 44 22
is.ordered(ds$apoe_genotype)
ds$apoe<- as.ordered(ds$apoe_genotype)
levels(ds$apoe)
is.factor(ds$apoe) #true
unique.default(sapply(ds$apoe, unique))
# 34   33   <NA> 23   24   44   22  
# Levels: 22 23 24 33 34 44
n_distinct(ds$apoe) #7
#good

#binomial variables as integers or numbers?
#this can be changed at 2-add-variables
#vital status
n_distinct(ds$vital_status)#2
ds$vital_status <- as.integer(ds$vital_status)
n_distinct(ds$vital_status) 
unique.default(sapply(ds$vital_status, unique)) #0 or 1

#dementia status
n_distinct(ds$dementia_status)#3
unique.default(sapply(ds$dementia_status, unique))
ds$dementia_status <- as.integer(ds$dementia_status)
n_distinct(ds$dementia_status) #13
unique.default(sapply(ds$dementia_status, unique)) #0, 1, NA

#stroke status
n_distinct(ds$stroke_status)#3
unique.default(sapply(ds$stroke_status, unique))
ds$stroke_status <- as.integer(ds$stroke_status)
n_distinct(ds$stroke_status) #3
unique.default(sapply(ds$stroke_status, unique)) #0, 1, NA

#path status
n_distinct(ds$path_status)#3
unique.default(sapply(ds$path_status, unique))
ds$path_status <- as.integer(ds$path_status)
n_distinct(ds$path_status) #13
unique.default(sapply(ds$path_status, unique)) #0, 1, NA

# ---- outliers ---------
glimpse(ds)
str(ds)
summary(ds)

hist(ds$total_smell_test) #neg skew
hist(ds$mmse) #neg skew, one outlier
boxplot(ds$mmse) #many outliers at the low end



# ---- save ---------
#save subset data as ds3
ds3<-ds
saveRDS(ds3, "./data/derived/ds3.rds")
#continue on to 4-apply-codebook



# #code examples
# mtcars$mpg[mtcars$cyl == 4] <- NA
# #code example using dplyr
# mtcars %>% mutate(mpg=replace(mpg, cyl==4, NA)) %>% as.data.frame()




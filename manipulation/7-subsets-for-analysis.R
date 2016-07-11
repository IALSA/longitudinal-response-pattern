## Project title: MAP 
##    Created on: June 03 2016
##        Author: Jamie Knight
##          Data: Creating final subsets for analysis. MAP for use in Thesis
##       Summary: 
## ---------------------------------------------------------------------- ##

options(width=160)
rm(list=ls())
cat("\f")

# --- load-packages ------

search() # see which pachages are currently attached
library(dplyr)
library(magrittr)
# requireNamespace("dplyr")


# ----- load-data ------
getwd()
#full version long
ds <-readRDS("./data/unshared/derived/ds4.rds")
#full version wide
# dsw <-readRDS("./data/derived/ds4w.rds")

#complete cases for BSIT and niareagansc
# ds1 <- readRDS("./data/derived/ds4ls.rds")
#complete cases for BSIT 
# ds2 <- readRDS("./data/derived/ds4ls2.rds")

str(ds) 
names(ds)

# ---- tweak-data -------------------
ds <- ds %>% 
  dplyr::mutate(
    id = as.integer(projid)
  )

# ---- subset-variables ---------------------------------
t <- table(ds$fu_year, ds$cdx); t[t==0] <- "."; t
names(ds)

keep_these_ids <-  ds %>% 
  dplyr::select(id, fu_year, cdx) %>% 
  dplyr::filter(fu_year==0, cdx == 1 ) %>% 
  dplyr::select(id)
keep_these_ids <- as.integer(keep_these_ids$id)
length(unique(keep_these_ids)) # 1236
length(unique(ds$id)) # 1803

ds_subset <- ds %>% 
  dplyr::filter(id %in% keep_these_ids)

length(unique(ds_subset$id))

ds_subset %>% 
  # dplyr::slice(1:10)
  dplyr::group_by(cdx) %>% 
  dplyr::summarize(count = n())

t <- table(ds_subset$fu_year, ds_subset$cdx); t[t==0] <- "."; t
names(ds)


# ---- subset-base -----------------

# base subset should only include people who do not have dementia at baseline
#change original file ie. ds to be each of the subsets above.

sub1<-ds

#subset to remove any participants with dementia at baseline
#cdx is assesment while alive, 1:normal, 2-3:mild MCI, 4-5:AD
varsREM <- sub1[ which(sub1$fu_year==0 & sub1$cdx != 1),]


sub2 <- sub1
for (rows in varsREM["projid"]) {
  for (id in rows) {
    sub2 <- sub2[sub2$projid != id, ]
  }
}

dplyr::n_distinct(sub2$projid)#ds: 1239 (564 less)|ds1:392 | ds2:1239
dplyr::n_distinct(sub1$projid) #ds:1803 subjects | ds1:650 | ds2:1660
dplyr::n_distinct(varsREM$projid) #ds: #564 | ds1:258 | ds2: 421
#correct numer was removed

#sub2 = all people who were cognitively normal at baseline 

# rename the subsets
dsbase <- sub2 # 1239
dplyr::n_distinct(dsbase$projid)
max(dsbase$fu_year)# NA, why not working?
dsbase$projid %>% unique() %>% length() 
                  
ds1base <- sub2 # 392, people who have BSIT and niareagansc
max(ds1base$fu_year) #17
dplyr::n_distinct(ds1base$projid)

ds2base<- sub2 #1239, people who have BSIT
max(ds2base$fu_year) #18
dplyr::n_distinct(ds2base$projid)

#ds and ds2 are the same number of people?
#use ds1 for autopsy pathology investigation and ds2 as other

# -------reshape data from long to wide-------
names(ds1base)
max(ds1base$fu_year) #17

measures <- names(ds1base)[c(-1,-2, -4, -6, -7, -9:-16, -28, -32, -33, -36)]
measures #time varying measures

ds1w <- reshape(ds1base, v.names=measures, timevar='fu_year', idvar='projid', direction="wide")
names(ds1w)

####help here####
# sub out people who do not have a BSIT scores at baseline
keep_ids <- ds1w[ds1w$total_smell_test.0 != is.na(), "projid"]

sub1 <- ds1w %>% dplyr::filter(projid %in% keep_ids)

#sub3: people who have 2 or more waves of BSIT or overall?

#3sub4: people who have 3 or more waves of BSIT or overall?

# ---- save ---------
#save subset data 

saveRDS(dsbase, "./data/derived/COX/dsbase.rds")
write.csv(dsbase, "./data/derived/CSV/dsbase.csv")

saveRDS(ds1base, "./data/derived/COX/ds1base.rds")
write.csv(ds1base, "./data/derived/CSV/ds1base.csv")

saveRDS(ds2base, "./data/derived/COX/ds2base.rds")
write.csv(ds2base, "./data/derived/CSV/ds2base.csv")





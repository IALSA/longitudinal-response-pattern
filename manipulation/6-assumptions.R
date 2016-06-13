## Project title: MAP 
##    Created on: June 03 2016
##        Author: Jamie Knight
##          Data: ds4
##       Summary: checking assumptions for Thesis
## ---------------------------------------------------------------------- ##

# Assumptions
# pg 94. Tabachnick and Fidel


# ----- load-data ------
getwd()
ds <-readRDS("./data/derived/ds4.rds")
str(ds) 
names(ds)


#should do these on wide data?
#load wide data
# ds <- readRDS("./data/derived/COX/ds0.rds") 

# ---- load libraries ------
library(Hmisc)
library(outliers)
library(psych)
names(ds)

# ----- normality, linearity, homoscadasticity ------ 

# histograms

#mmse
hist(ds$mmse, xlab="MMSE", ylab="y", col = "green") 
plot(density(dsm$cts_mmse30))
boxplot(ds$cts_mmse30) #wide data?
skew(dsm$cts_mmse30) #[1] -2.602142
#very negatively skewed, no apparent outliers

#episodic memory scores, composite
hist(ds$cogn_ep, xlab="Episodic Memory", ylab="y", col = "green") 
plot(density(dsm$cogn_ep))
(outlier(dsm$cogn_ep)) # [1] -3.519921
shapiro.test(dsm$cogn_ep)#wide data?
boxplot(dsm$cogn_ep) # shows lots, wide data?
skew(dsm$cogn_ep) #[1] -1.079238
#negative skew, no apparent outliers in histogram

#global memory scores, composite
hist(ds$cogn_global, xlab="Global Memory", ylab="y", col = "green") 
plot(density(dsm$cogn_global))
skew(dsm$cogn_global) #[1] -1.128928
boxplot(dsm$cogn_global) # shows lots, wide data?
#negative skew, no apparent outliers

#olfactory scores, BSIT
hist(ds$total_smell_test, xlab="BSIT", ylab="y", col = "green") #shows the decimal score - need to find out about those
plot(density(dsm$total_smell_test))
(outlier(ds$total_smell_test)) # [1] 1, is this an error
shapiro.test(dsm$total_smell_test)#wide data?, not working
boxplot(dsm$total_smell_test) # shows none
skew(dsm$total_smell_test) #[1] -0.5459449
#negative skew, no outliers


#skewness and kurtosis
describe(ds)

# ----- Missing Values -----
# complete.cases(ds) #very few complete cases

#check individual variables to see if any variables contribute more to missing values
apply(apply(ds,2,is.na),2,sum) 
# variables requiring death are higher, but thats fine (ie autopsy data scores are higher: braak, ceradsc, cogdx)
# total smell test looks high: 150294
#compared to mmse which is: 6886
#apoe: 14164

# projid           age_bl        age_death     age_at_visit          fu_year             time 
# 0                0           106210                0                0                0 
# race             educ              sex          cesdsum          cogn_ep      cogn_global 
# 60              440                0             7688             9380             3299 
# cts_mmse30        cts_bname         cts_ebdr        cts_delay            cogdx              cdx 
# 6886            11363             4711             9963           112408             2643 
# dementia          braaksc          ceradsc    apoe_genotype total_smell_test      group_smell 
# 3505           113609           113609            14164           150294           150294 
# dementia_status    stroke_status           stroke          time.yr     vital_status 
# 2643            47456            47456                0                0 

# install.packages("Amelia")
library(Amelia)
missmap(ds, main = "Missing values vs observed") 
# there are a lot of BSIT scores missing, the rest look ok. why so many missing?
# run on omitted data just to double chec it's working correctly
missmap(dsm, main = "Missing values vs observed") #none missing, looks like it's working good.
#run on base subset of data with no dementia at baseline and 3+ waves
missmap(ds_subset, main = "Missing values vs observed") #this one looks about the same 
####Input here####
#get remerged data and re-run the subsetting of data sets and re-run this



# ----- univariate outliers ------


# ----- multivariate outliers ----

# ----- residuals ------
#need wide data here?
#check normality of residuals
#first need model
outcome<-cbind(marazillierData$Anxiety.Change, marazillierData$Sad.Change, marazillierData$Happy.Change, marazillierData$Angry.Change, marazillierData$Disgust.Change, marazillierData$Contempt.Change)
marzillierModel<-manova(outcome ~ Induction*Mood, data = marazillierData)
summary(marzillierModel, intercept = TRUE, test="Wilks")
# qq plot for studentized resid
qqPlot(marzillierModel, main="QQ Plot")

# ----NOTES------

# need to find out about the BSIT decimal scores. check codebooks. email data manager.
#lots missing too - why? is this the same after getting a propper subset in? (no dementia at baseline, and 3+ waves of BSIT scores)

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
ds <-readRDS("./data/derived/ds4l.rds")
str(ds) 
names(ds)


#should do these on wide data?
#load wide data
# ds <- readRDS("./data/derived/COX/ds0.rds") 

# ---- load libraries ------

if (!require("pastecs")) install.packages('pastecs')
if (!require("psych")) install.packages('psych')
if (!require("Hmisc")) install.packages('Hmisc')
if (!require("outliers")) install.packages('outliers')
if (!require("mvOutlier")) install.packages('mvOutlier')
if (!require("scatterplot3d")) install.packages('scatterplot3d')
names(ds)

# ----- normality, linearity, homoscadasticity ------ 

# histograms

#mmse long
hist(ds$mmse, 
     main="Histogram for MAP", 
     xlab="MMSE", 
     border="blue", 
     col="green", 
     xlim=c(0,35), 
     las=1, 
     breaks=5, 
     prob = TRUE)


hist(ds$mmse, xlab="MMSE", ylab="y", col = "green") 
boxplot(ds$mmse)

#wide data
hist(dsw$mmse.3, xlab="MMSE", ylab="y", col = "green") 
plot(density(dsw$mmse.0))
boxplot(dsw$mmse.0) 

hist(dsw$mmse.0, 
     main="Histogram for MAP", 
     xlab="Baseline MMSE", 
     border="blue", 
     col="green", 
     xlim=c(0,35), 
     las=1, 
     breaks=5, 
     prob = TRUE)

(outlier(ds$mmse))

boxplot.stats(ds$mmse, coef = 2)$out# loads of outliers!

skew(ds$mmse) #[1] -2.602142
#very negatively skewed, no apparent outliers

#episodic memory scores, composite
boxplot.stats(ds$cogn_ep, coef = 2)$out #loads of outliers

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
hist(ds$total_smell_test, xlab="BSIT", ylab="y", col = "green") 
plot(density(ds$total_smell_test))
outlier(ds$total_smell_test) # [1] 1, is this an error
shapiro.test(ds$total_smell_test)#wide data?, not working
boxplot(ds$total_smell_test) # shows one
skew(ds$total_smell_test) #[1] -0.8764194
#negative skew, one outliers

#education
boxplot.stats(ds$educ, coef = 2)$out # a few outliers

#skewness and kurtosis
describe(ds)


# ----- outliers -----
outlier_tf = outlier(ds$cogn_ep,logical=TRUE)
#This gives an array with all values False, except for the outlier (as
# defined in the package documentation "Finds value with largest difference
# between it and sample mean, which can be an outlier").  That value is
# returned as True.
find_outlier = which(outlier_tf==TRUE,arr.ind=TRUE)
#This finds the location of the outlier by finding that "True" value within the "outlier_tf" array.
data_new = data_full[-find_outlier,]
#This creates a new dataset based on the old data, removing the one row that contains the outlier



# ----- bivariate-plots -----
#bivariate plots
plot(ds$total_smell_test, ds$cogn_ep)
plot(ds$cogn_ep, ds$total_smell_test)

plot(ds$mmse, ds$cogn_global, main="MMSE/Global", xlab="mmse", ylab="global", col="red") 
abline(lm(ds$cogn_global~ds$mmse), col="blue")

plot(ds$mmse, ds$cogn_ep, main="MMSE/Episodic", xlab="mmse", ylab="episodic", col="red") 
abline(lm(ds$cogn_ep~ds$mmse), col="blue")

plot(ds$total_smell_test, ds$cogn_ep, main="BSIT/Episodic", xlab="BSIT", ylab="episodic", col="red") 
abline(lm(ds$cogn_ep~ds$total_smell_test), col="blue")

plot(ds$total_smell_test, ds$mmse)

plot(ds$total_smell_test, ds$cogn_global)
plot(ds$total_smell_test, ds$cogn_global, main="Scatterplot Example", 
     xlab="Global Cognition ", ylab="BSIT ", pch=19)
abline(lm(ds$total_smell_test~ds$cogn_global), col="red") # regression line (y~x)

library(car)
scatterplot.matrix(~ds$total_smell_test+ds$cogn_ep+ds$cogn_global+ds$mmse+ds$niareagansc, data=ds,
                   main="BSIT and Cognition")

scatterplot.matrix(~ds$cdx+ds$cogn_ep+ds$cogn_global+ds$mmse, data=ds,
                   main="Dementia Diagnosis and Cognition")

scatterplot.matrix(~ds$niareagansc+ds$cogn_ep+ds$cogn_global+ds$mmse, data=ds,
                   main="AD Pathology and Cognition")

#dsbase
scatterplot.matrix(~dsbase$total_smell_test+dsbase$cogn_ep+dsbase$cogn_global+dsbase$mmse+dsbase$niareagansc, data=dsbase,
                   main="BSIT and Cognition")

scatterplot.matrix(~dsbase$cdx+dsbase$cogn_ep+dsbase$cogn_global+dsbase$mmse, data=dsbase,
                   main="Dementia Diagnosis and Cognition")

scatterplot.matrix(~dsbase$niareagansc+dsbase$cogn_ep+dsbase$cogn_global+dsbase$mmse, data=dsbase,
                   main="AD Pathology and Cognition")

#ds2base
scatterplot.matrix(~ds2base$total_smell_test+ds2base$cogn_ep+ds2base$cogn_global+ds2base$mmse+ds2base$niareagansc, data=ds2base,
                   main="BSIT and Cognition 2")

scatterplot.matrix(~ds2base$cdx+ds2base$cogn_ep+ds2base$cogn_global+ds2base$mmse, data=ds2base,
                   main="Dementia Diagnosis and Cognition")

scatterplot.matrix(~ds2base$niareagansc+ds2base$cogn_ep+ds2base$cogn_global+ds2base$mmse, data=ds2base,
                   main="AD Pathology and Cognition")

#more complex
scatterplot(ds$total_smell_test ~ ds$cogn_ep | ds$fu_year, data=ds, 
            xlab="Episodic Memory", ylab="BSIT", 
            main="Enhanced Scatter Plot", 
            labels=row.names(ds))

#3d are there more dimensions in play here?
# 3D Scatterplot

attach(mtcars)
scatterplot3d(ds$total_smell_test,ds$final_dx,ds$cogn_ep, main="3D Scatterplot")
#not useful



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
#re-run using subsets from 7-subsets-for-analysis

missmap(dsbase, main = "Missing values vs observed") #full version 

missmap(ds1base, main = "Missing values vs observed") #complete cases for BSIT and niareagansc, this doesnt look right
missmap(ds2base, main = "Missing values vs observed") #complete cases for BSIT 



# ----- univariate outliers ------



# bivariate
boxplot(ds$total_smell_test ~ ds$fu_year, data=ds, main="BSIT reading across waves")  # clear pattern is noticeable.
boxplot(ds$cogn_ep ~ ds$fu_year, data=ds, main="episodic memory reading across waves")

# ----- multivariate outliers ----
#malhanobis distance
result <- mvoutlier.CoDa(ds) #$cogn_global, qqplot = TRUE, method = "quan")
labelsO<-rownames(result$outlier)[result$outlier[,2]==TRUE]
xcoord<-result$outlier[result$outlier[,2]==TRUE,1]
#recalculate chi-squared values for ranks 50 and 49 (i.e., p=(size:(size-n.outliers + 1))-0.5)/size and df = n.variables = 3
chis = qchisq(((50:49)-0.5)/50,3)
text(xcoord,chis,label=labelsO)

library(mvoutlier)
?mvoutlier.CoDa

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

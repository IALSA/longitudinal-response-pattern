
# Clear memory from previous runs
base::rm(list=base::ls(all=TRUE))
cat("\f")


# @knitr LoadPackages --------------------
# Load the necessary packages.
source("./scripts/common-functions.R")
# install.packages("nlmeU")
if (!require("DT")) install.packages('DT')
# sessionInfo()
requireNamespace("nlmeU")
requireNamespace("dplyr")
library("magrittr")
library(knitr)

# @knitr Intro --------------------


# @knitr LoadData --------------------
# Link to the data source 
pathDir <- getwd()
ds4w <- readRDS("./data/derived/ds4w.rds")


# @knitr Table --------------------
varnames <- names(ds4w %>%
                    dplyr::select(contains("stroke_cum")))
dots = c("projid", varnames)
# d <- ds4w %>% dplyr::select_(.dots = dots)
d <- ds4w %>% dplyr::select_(.dots = dots)

d$pattern <- nlmeU::missPat(d)

d <- d %>%  
  dplyr::group_by(pattern) %>% 
  dplyr::summarize(counts = n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(desc(counts))

d %>%
  DT::datatable(
    class     = 'cell-border stripe',
    caption   = "Pattern of missingness across time points",
    filter    = "top",
    options   = list(pageLength = 10, autoWidth = TRUE)
  )



varnames <- names(ds4w %>%
                    dplyr::select(contains("stroke_cum")))
dots = c("projid", varnames)
ds <- ds4w %>% dplyr::select_(.dots = dots)
# ds <- ds[,ds==NA]
ds[is.na(ds)] <- "."
# ds <- ds[,1:4]
head(ds)

varnames


# ds$pattern <- paste0(ds$stroke_status.0, ds$stroke_status.1, ds$stroke_status.2)

# Create a string variables that records the response pattern of each individual
# see http://stackoverflow.com/questions/14568662/paste-multiple-columns-together-in-r
ds$pattern <- apply( ds[, varnames], 1 , paste , collapse = "" )
# remove the unnecessary rows
keep_variables <- setdiff(colnames(ds), varnames)
ds <- as.data.frame(ds[ , keep_variables ])

ds_pattern <- ds %>% 
  dplyr::group_by(pattern) %>% 
  dplyr::summarize(count = n()) %>% 
  dplyr::arrange(desc(count))
ds_pattern



ds_pattern %>%
  DT::datatable(
    class     = 'cell-border stripe',
    caption   = "Pattern of missingness across time points",
    filter    = "top",
    options   = list(pageLength = 10, autoWidth = TRUE)
  )
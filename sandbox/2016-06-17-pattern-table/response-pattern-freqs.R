# knitr::stitch_rmd(script="./___/___.R", output="./___/___/___.md")
#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console 



# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.
source("./scripts/common-functions.R") # used in multiple reports

# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) # enables piping : %>% 
# Verify these packages are available on the machine, but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("ggplot2") # graphing
# requireNamespace("readr") # data input
requireNamespace("tidyr") # data manipulation
requireNamespace("dplyr") # Avoid attaching dplyr, b/c its function names conflict with a lot of packages (esp base, stats, and plyr).
requireNamespace("testit")# For asserting conditions meet expected patterns.
# requireNamespace("car") # For it's `recode()` function.
if (!require("DT")) install.packages('DT')

# ---- declare-globals ---------------------------------------------------------
dataPath <- "./data/unshared/derived/ds4w.rds"

# ---- load-data ---------------------------------------------------------------
ds <- readRDS(dataPath)
# ---- inspect-data -------------------------------------------------------------

# ---- tweak-data --------------------------------------------------------------

# ---- missing-values-table --------------------------------------------------------------
# select the variables over which you'd like to view response pattern
varnames <- names(ds %>%
                    dplyr::select(contains("stroke_cum")))
dots = c("projid", varnames)
d <- ds %>% dplyr::select_(.dots = dots)

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


# ---- binary-values-table --------------------------------------------------------------
# select the variables over which you'd like to view response pattern
varnames <- names(ds %>%
                    dplyr::select(contains("stroke_cum")))
dots = c("projid", varnames)
d <- ds %>% dplyr::select_(.dots = dots)
# recode NA 
d[is.na(d)] <- "."
# ds <- ds[,1:4]
# head(d)

# Create a string variables that records the response pattern of each individual
# see http://stackoverflow.com/questions/14568662/paste-multiple-columns-together-in-r
d$pattern <- apply( d[, varnames], 1 , paste , collapse = "" )
# remove the unnecessary rows
keep_variables <- setdiff(colnames(d), varnames)
d <- as.data.frame(d[ , keep_variables ])

ds_pattern <- d %>% 
  dplyr::group_by(pattern) %>% 
  dplyr::summarize(count = n()) %>% 
  dplyr::arrange(pattern)
  # dplyr::arrange(desc(count))

ds_pattern %>%
  DT::datatable(
    class     = 'cell-border stripe',
    caption   = "Response pattern across time points ( dots stand for missing values)",
    filter    = "none",
    options   = list(pageLength = 10, autoWidth = TRUE)
  )

# ---- reproduce ---------------------------------------
rmarkdown::render(input = "./sandbox/2016-06-17-pattern-table/response-pattern-freqs.Rmd" ,
                  output_format="html_document", clean=TRUE)


# @knitr Table --------------------

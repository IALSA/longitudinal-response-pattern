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
dataPath <- "./data/unshared/derived/ds0_raw.rds"

# ---- load-data ---------------------------------------------------------------
ds0 <- readRDS(dataPath)
# ---- inspect-data -------------------------------------------------------------
variables_to_select <- c("projid", "fu_year",  "age_death", "age_at_visit", "age_bl","stroke_cum")
# variables_to_select <- c("projid", "fu_year", "stroke_cum")
ds_long <- ds0 %>% 
  dplyr::select_(.dots = variables_to_select) %>% 
  dplyr::rename(id = projid)
# recode NA 
ds_long$stroke_cum[is.na(ds_long$stroke_cum)] <- "."
# ds <- ds[,1:4]
head(ds_long)

ds_long %>%
  DT::datatable(
    class     = 'cell-border stripe',
    caption   = "Pattern of missingness across time points",
    filter    = "top",
    options   = list(pageLength = 10, autoWidth = TRUE)
  )

# ---- tweak-data --------------------------------------------------------------

# ----- long-to-wide ------
# define variable properties for long-to-wide conversion
variables_static <- c("id", "age_death", "age_bl")
variables_longitudinal <- setdiff(colnames(ds_long),variables_static)  # not static
(variables_longitudinal <- variables_longitudinal[!variables_longitudinal=="fu_year"]) # all except wave
# establish a wide format
ds_wide <- ds_long %>%
  dplyr::filter(!is.na(fu_year)) %>% 
  # dplyr::select(id, wave, animals, word_recall_de ) %>%
  # gather(variable, value, -(id:wave)) %>%
  dplyr::select_(.dots=c(variables_static, "fu_year", variables_longitudinal)) %>%
  tidyr::gather_(key="variable", value="value", variables_longitudinal) %>%
  # dplyr::mutate(wave = paste0("t", fu_year)) %>%
  tidyr::unite(temp, variable, fu_year) %>%
  tidyr::spread(temp, value)
ds_wide %>% dplyr::glimpse()

knitr::kable(ds_wide %>% 
  dplyr::group_by(temp) %>% 
  dplyr::summarize(n()))



# take the final data set and get a wide version of it and an na.omit subset

#reshape data from long to wide
names(d)

max(ds$fu_year, na.rm = TRUE) #18

#time invariant : -1, -2, -4, -6, -7, -9:-16
time_variant <- c("age_at_visit", "stroke_cum")
measures <- time_variant
measures <- "stroke_cum"
(measures <- names(d)[c(-1,-2)])
# measures <- names(ds)[c(-1,-2, -4, -6, -7, -9:-16)] #discluded projid and fu_year
measures

dsw <- stats::reshape(d, v.names=measures, timevar='fu_year', idvar='projid', direction="wide")
names(dsw)

ds_wide <- ds_long %>%
  tidyr::spread(key=variable,value=value) %>%
  dplyr::arrange_(.dots=time_invariant)

# ---- missing-values-table --------------------------------------------------------------


# ---- binary-values-table --------------------------------------------------------------


# Create a string variables that records the response pattern of each individual
# see http://stackoverflow.com/questions/14568662/paste-multiple-columns-together-in-r
varnames <- "stroke_cum"
d$pattern <- apply( d[,varnames], 1 , paste , collapse = "" )
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

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
dataPath <- "../MAP/data-unshared/raw/ds0_raw.rds"

# ---- load-data ---------------------------------------------------------------
ds0 <- readRDS(dataPath) 
ds0 <- ds0 %>% dplyr::filter(study == "MAP ")

# ---- inspect-data -------------------------------------------------------------
variables_to_select <- c("id", "fu_year",  "age_death", "age_at_visit", "age_bl","stroke_cum")
# variables_to_select <- c("projid", "fu_year", "stroke_cum")
ds_long <- ds0 %>% 
  dplyr::select_(.dots = variables_to_select) # %>%
  # dplyr::rename(id = projid)
# recode NA 
# ds_long$stroke_cum[is.na(ds_long$stroke_cum)] <- "."
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
#check for duplicates
ds_duplicates <- ds_long %>% dplyr::group_by(id, fu_year) %>% dplyr::filter(n()>1) %>% dplyr::summarise(n=n())
# dsd0 shoud have no duplicates. Obs should equal 0
# July 4, 2016: there are many duplicates - inviestigate why this is in MAP curator
rm(ds_duplicates)

#remove duplicates for now
ds_distinct <- ds_long %>%
  dplyr::distinct()


# ----- long-to-wide ------
# define variable properties for long-to-wide conversion
variables_static <- c("id", "age_death", "age_bl")
variables_longitudinal <- setdiff(colnames(ds_distinct),variables_static)  # not static
(variables_longitudinal <- variables_longitudinal[!variables_longitudinal=="fu_year"]) # all except wave
# establish a wide format
ds_wide <- ds_distinct %>%
  dplyr::filter(!is.na(fu_year)) %>% 
  # dplyr::select(id, wave, animals, word_recall_de ) %>%
  # gather(variable, value, -(id:wave)) %>%
  dplyr::select_(.dots=c(variables_static, "fu_year", variables_longitudinal)) %>%
  tidyr::gather_(key="variable", value="value", variables_longitudinal) %>%
  # dplyr::mutate(wave = paste0("t", fu_year)) %>%
  tidyr::unite(temp, variable, fu_year) %>%
  tidyr::spread(temp, value)

ds_wide %>% dplyr::glimpse()

# ---- compute-pattern --------------------------------------------------------------
# using ds_wide data

# select the variables over which you'd like to view response pattern
focal_variable <- "stroke_cum"
varnames <- names(ds_wide %>%
                    dplyr::select(contains(focal_variable)))
ordered_var_names <- paste0(focal_variable,"_",0:16)
dots = c("id", ordered_var_names)
d <- ds_wide %>% dplyr::select_(.dots = dots)
# recode NA for easier inspection of the patterns
d[is.na(d)] <- "."
# compile the pattern from individual waves
ordered_var_names <- paste0(focal_variable,"_",0:16)
# reorder variables to ensure correct sequence of waves
d <- d %>% dplyr::select_(.dots = dots)

# Create a string variables that records the response pattern of each individual
# see http://stackoverflow.com/questions/14568662/paste-multiple-columns-together-in-r
d$pattern <- apply(d[,ordered_var_names], 1 , paste , collapse = "" )

# The string to search in
s <- "aababac"
# The character to search for
p <- "a"
# Replace all occurrences by the empty string - note that gsub uses regular expressions, so escape p accordingly
s2 <- gsub(p,"",s) # Count the length difference
numOcc <- nchar(s) - nchar(s2) # numOcc now contains the number of occurrences of p in s
countCharOccurrences <- function(char, s) {
  s2 <- gsub(char,"",s)
  return (nchar(s) - nchar(s2))
}
countCharOccurrences("a", "aabbaa")

d <- d %>% 
  # dplyr::mutate(stroke_ever = countCharOccurrences("1", pattern)) 
  dplyr::mutate(
    stroke_ever  = ifelse(countCharOccurrences("1", pattern)==0L,FALSE,TRUE),
    stroke_start = ifelse(countCharOccurrences("0", pattern)==0L, TRUE, FALSE),
    pattern      = ifelse(!stroke_ever,0,
                          ifelse(stroke_start,"1",pattern))
    
  ) 
head(d)  

# remove the unnecessary columns
keep_variables <- setdiff(colnames(d), ordered_var_names)
d <- as.data.frame(d[ , keep_variables ])

# compute frequency patternn
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

# ---- inspect-tables -----------------------------

# ---- line-graph-stroke-cum ---------------------
str(ds_distinct)
library(ggplot2)
dg <- ds_distinct %>% 
  dplyr::group_by(fu_year) %>% 
  dplyr::summarize(
    count = n(),
    pct_stroke = mean(stroke_cum, na.rm = T)
  )
g <- ggplot2::ggplot(dg, aes(x = fu_year, y = pct_stroke))
g <- g + ggplot2::geom_line(stat="identity")
g <- g + geom_point(stat="identity")
g



# ---- reproduce ---------------------------------------
rmarkdown::render(input = "./sandbox/2016-06-30-transition-count/transition-count.Rmd" ,
                  output_format="html_document", clean=TRUE)


# @knitr Table --------------------

# ---- count-transitions ---------------------------------------
# count the transitions from no stroke to stroke

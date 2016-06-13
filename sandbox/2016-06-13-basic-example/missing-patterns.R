options(width=160)
rm(list=ls())
cat("\f")

# this report tests out (WB&AK) code using nmleU as Procfreq substitute

# ---- load-packages -----------------------------
source("./scripts/common-functions.R")
# install.packages("nlmeU")
requireNamespace("nlmeU")
requireNamespace("dplyr")
library("magrittr")

# start with the nlmeU vignette: https://cran.r-project.org/web/packages/nlmeU/nlmeU.pdf
# ---- example-1 --------------
ds <-  subset(nlmeU::armd.wide, select = c(visual12, visual24, visual52))
head(ds)
# visual** (12, 24, 52 weeks) - three observation in time
ds$pattern <-  nlmeU::missPat(ds)
head(ds)

table(ds$pattern)

# ----- start-here ------

#load data
# ds4ws <-readRDS("./data/unshared/derived/ds4ws.rds")# small version of wide format data
ds4w <- readRDS("./data/unshared/derived/ds4w.rds")# small version of wide format data
# ds4w <-readRDS("./data/derived/ds4w.rds")#full version of wide format data




#variables:
#stroke 0-12
#stroke_cum 0-12
#stroke_status 0-12 : binary created variable with 0 as no stroke and stroke as 1
# names(ds4ws)
varnames <- names(ds4w %>%
  dplyr::select(contains("stroke_status")))
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
    options   = list(pageLength = 6, autoWidth = TRUE)
  )


  



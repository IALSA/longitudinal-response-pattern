# ---- long-to-wide-data.table-dcast ----------------------------------------
# wave_counter <- 0:17
wave_counter <- unique(ds$fu_year)
(ms_outcome_wide_names <- paste0("mmse_",wave_counter))
(ms_age_at_visit_wide_names <- paste0("age_", wave_counter))
(not_long_stem <- c( "age_at_visit", "mmse"))
(long_stem <- setdiff(names(ds),not_long_stem))
# ds_wide <- ds_no_duplicates %>%
# ds_wide <- ds %>%
#   tidyr::spread_(key="fu_year", value %in% c("age_at_visit", "mmse") )

library(data.table) ## v >= 1.9.6
ds_wide <- data.table::dcast(
  data.table::setDT(ds),
  id + age_death + msex + educ + smoke_bl + alco_life ~ fu_year, value.var = c(
    "age_at_visit", "mmse")) 


# ---- long-to-wide-dplyr-spread --------------
# establish a wide format
ds_wide <- ds_long %>%
  # dplyr::select(id, wave, animals, word_recall_de ) %>%
  # gather(variable, value, -(id:wave)) %>%
  dplyr::select_(.dots=c(variables_static, "wave", variables_longitudinal)) %>%
  tidyr::gather_(key="variable", value="value", variables_longitudinal) %>%
  dplyr::mutate(wave = paste0("t", wave)) %>%
  tidyr::unite(temp, variable, wave) %>%
  tidyr::spread(temp, value)
ds_wide %>% dplyr::glimpse()
# prepare data to be read by MPlus
ds_mplus <- ds_wide
ds_mplus[is.na(ds_mplus)] <- -9999 # replace NA with a numerical code
ds_mplus %>% dplyr::glimpse()

# ---- long-to-wide-stats-reshape ---------------------

dsw <- stats::reshape(d, v.names=measures, timevar='fu_year', idvar='projid', direction="wide")
names(dsw)
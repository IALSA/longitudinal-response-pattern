## Project title: MAP 
##    Created on: June 04 2016
##        Author: Jamie Knight
##          Data: ds2
##       Summary: Adding labels
## ---------------------------------------------------------------------- ##

#notes: change data cleaning to be before labels at 3


options(width=160)
rm(list=ls())
cat("\f")

# ---- load_packages ----------------------------------
requireNamespace("dplyr")

# ----- load-data ------
getwd()
ds <-readRDS("./data/unshared/derived/ds3.rds")
str(ds) 
names(ds)
# ---- add_labels -----------------------------------------------------------
# Add a descriptive label for each variable except "projid"

attr(ds$fu_year, "label") <- "Follow-up year"
attr(ds$cdx, "label") <- "Clinical dx summary"
attr(ds$final_dx, "label") <- "Autopsy dx summary"
attr(ds$dementia, "label") <- "Dementia diagnosis"
attr(ds$age_bl,"label") <- "Age at baseline"
attr(ds$age_death, "label") <- "Age at death"
attr(ds$race, "label") <- "Participant's race"
attr(ds$educ, "label") <- "Years of education"
attr(ds$sex, "label") <- "Gender"
attr(ds$braaksc, "label") <- "Braak score: tangles"
attr(ds$ceradsc, "label") <- "CERAD score: plaques"
attr(ds$niareagansc, "label") <- "Neuropathologist determined AD pathology level:1 is high"
attr(ds$ad_reagan, "label") <- "Binary AD pathology by Reagan Criteria"
attr(ds$apoe_genotype, "label") <- "ApoE genotype"
attr(ds$total_smell_test, "label") <- "BSIT smell test score"
attr(ds$stroke, "label") <- "Clinical stroke dx"
attr(ds$stroke_cum, "label") <- "Clinical Diagnoses - Stroke - cumulative"
attr(ds$vasc_risks_sum, "label") <- "Vascular disease risk factors"
attr(ds$cogn_ep, "label") <- "Calculated domain score-episodic memory"
attr(ds$cogn_global, "label") <- "Global cognitive score"
attr(ds$mmse, "label") <- "MMSE - 2014"
attr(ds$cts_ebmt, "label") <- "East Boston story - immediate - 2014"
attr(ds$cts_ebdr, "label") <- "East Boston story - delayed recall - 2014"
attr(ds$cts_story, "label") <- "Logical memory Ia - immediate - 2014"
attr(ds$cts_delay, "label") <- "Logical memory IIa - 2014"
attr(ds$time_yr, "label") <- "."
attr(ds$vital_status, "label") <- "Binary vital status"
attr(ds$dementia_status, "label") <- "Binary dementia status"
attr(ds$stroke_status, "label") <- "Binary stroke stats"
attr(ds$path_status, "label") <- "Binary pathology status: AD or not"
attr(ds$group_smell, "label") <- "3 groups: smell ability"
attr(ds$BSIT, "label") <- "Factored version of smell score"
attr(ds$apoe, "label") <- "Factored version of apoe"



# If you have more of the database in your subset you can use these
# 
# attr(ds$agreeableness, "label") <- "NEO agreeableness-ROS"
# attr(ds$conscientiousness, "label") <- "Conscientiousness-ROS/MAP"
# attr(ds$extraversion, "label") <- "NEO extraversion-ROS/MAP"
# attr(ds$neo_altruism, "label") <- "NEO altruism scale-MAP"
# attr(ds$neo_conscientiousness, "label") <- "NEO conscientiousness-MAP"
# attr(ds$neo_trust, "label") <- "NEO trust-MAP"
# attr(ds$openness,"label") <- "NEO openess-ROS"
# attr(ds$anxiety_10items, "label") <- "Anxiety-10 item version - ROS and MAP"
# attr(ds$neuroticism_12, "label") <- "Neuroticism - 12 item version-RMM"
# attr(ds$neuroticism_6, "label") <- "Neuroticism - 6 item version - RMM"
# attr(ds$died, "label") <- "Indicator of death"
# 
# attr(ds$spanish, "label") <- "Spanish/Hispanic origin"
# attr(ds$ldai_bl, "label") <- "Lifetime daily alcohol intake -baseline"
# attr(ds$q3smo_bl, "label") <- "Smoking quantity-baseline"
# attr(ds$q4smo_bl, "label") <- "Smoking duration-baseline"
# attr(ds$smoke_bl, "label") <- "Smoking at baseline"
# attr(ds$smoking, "label") <- "Smoking"
# 
# attr(ds$scaled_to.y, "label") <- "No label found in codebook"
# attr(ds$cesdsum, "label") <- "CESD-Measure of depressive symptoms"
# attr(ds$r_depres, "label") <- "Major depression dx-clinic rating"
# attr(ds$intrusion, "label") <- "Negative social exchange-intrusion-MAP"
# attr(ds$neglifeevents, "label") <- "Negative life events"
# attr(ds$negsocexchange, "label") <- "Negative social exchange"
# attr(ds$nohelp, "label") <- "Negative social exchange-help-MAP"
# attr(ds$panas, "label") <- "Panas score"
# attr(ds$perceivedstress, "label") <- "Perceived stress"
# attr(ds$rejection, "label") <- "Negative social exchange - rejection-MAP"
# attr(ds$unsympathetic, "label") <- "Negative social exchange-unsymapathetic-MAP"
# 
# attr(ds$cogn_po, "label") <- "Calculated domain score - perceptual orientation"
# attr(ds$cogn_ps, "label") <- "Calculated domain score - perceptual speed"
# attr(ds$cogn_se, "label") <- "Calculated domain score - semantic memory"
# attr(ds$cogn_wo, "label") <- "Calculated domain score - working memory"
# attr(ds$cts_bname, "label") <- "Boston naming - 2014"
# attr(ds$cts_catflu, "label") <- "Category fluency - 2014"
# attr(ds$cts_db, "label") <- "Digits backwards - 2014"
# attr(ds$cts_df, "label") <- "Digits forwards - 2014"
# attr(ds$cts_doperf, "label") <- "Digit ordering - 2014"
# attr(ds$cts_ebdr, "label") <- "East Boston story - delayed recall - 2014"
# attr(ds$cts_idea, "label") <- "Complex ideas - 2014"
# attr(ds$cts_lopair, "label") <- "Line orientation - 2014"
# attr(ds$cts_nccrtd, "label") <- "Number comparison - 2014"
# attr(ds$cts_pmat, "label") <- "Progressive Matrices - 2014"
# attr(ds$cts_read_nart, "label") <- "Reading test-NART-2014"
# attr(ds$cts_read_wrat, "label") <- "Reading test - WRAT - 2014"
# attr(ds$cts_sdmt, "label") <- "Symbol digit modalitities - 2014"
# attr(ds$cts_wli, "label") <- "Word list I- immediate- 2014"
# attr(ds$cts_wlii, "label") <- "Word list II - delayed - 2014"
# attr(ds$cts_wliii, "label") <- "Word list III - recognition - 2014"
# attr(ds$age_at_visit, "label") <-"Age at cycle - fractional"
# attr(ds$iadlsum, "label") <- "Instrumental activities of daily liviing"
# attr(ds$katzsum, "label") <- "Katz measure of disability"
# attr(ds$rosbscl, "label") <- "Rosow-Breslau scale"
# attr(ds$rosbsum, "label") <- "Rosow-Breslau scale"
# attr(ds$vision, "label") <- "Vision acuity"
# attr(ds$visionlog, "label") <- "Visual acuity"
# attr(ds$fev, "label") <- "forced expiratory volume"
# attr(ds$mep, "label") <- "maximal expiratory pressure"
# attr(ds$mip, "label") <- "maximal inspiratory pressure"
# attr(ds$pvc, "label") <- "pulmonary vital capacity"
# attr(ds$bun, "label") <- "Blood urea nitrogen"
# attr(ds$ca, "label") <- "Calcium"
# attr(ds$chlstrl, "label") <- "Cholesterol"
# attr(ds$cl, "label") <- "Chloride"
# attr(ds$co2, "label") <- "Carbon Dioxide"
# attr(ds$crn, "label") <- "Creatinine"
# attr(ds$fasting, "label") <- "Whether blood was collected on fasting participant"
# attr(ds$glucose, "label") <- "Glucose"
# attr(ds$hba1c, "label") <- "Hemoglobin A1c"
# attr(ds$hdlchlstrl, "label") <- "HDL cholesterol"
# attr(ds$hdlratio, "label")<- "HDL ratio"
# attr(ds$k, "label") <- "Potassium"
# attr(ds$ldlchlstrl, "label") <- "LDL cholesterol"
# attr(ds$na, "label")<- "Sodium"
# attr(ds$alcohol_g, "label") <- "Grams of alcohol per day"
# attr(ds$bmi, "label") <- "Body mass index"
# attr(ds$htm, "label") <- "Height(meters)"
# attr(ds$phys5itemsum, "label") <- "Summary of self reported physical activity
# measure (in hours) ROS/MAP"
# attr(ds$wtkg, "label") <- "Weight (kg)"
# attr(ds$bp11, "label") <- "Blood pressure measurement- sitting - trial 1"
# attr(ds$bp2, "label") <- "Blood pressure measurement- sitting - trial 2"
# attr(ds$bp3, "label") <- "Hx of Meds for HTN"
# attr(ds$bp31, "label") <- "Blood pressure measurement- standing"
# attr(ds$hypertension_cum, "label") <- "Medical conditions - hypertension - cumulative"
# attr(ds$dm_cum, "label") <- "Medical history - diabetes - cumulative"
# attr(ds$thyroid_cum, "label") <- "Medical Conditions - thyroid disease - cumulative"
# attr(ds$chf_cum, "label") <- "Medical Conditions - congestive heart failure -
# cumulative"
# attr(ds$claudication_cum, "label") <- "Medical conditions - claudication -cumulative"
# attr(ds$heart_cum, "label") <- "Medical Conditions - heart - cumulative"
# attr(ds$vasc_3dis_sum, "label") <- "Vascular disease burden (3 items w/o chf)
# ROS/MAP/MARS"
# attr(ds$vasc_4dis_sum, "label") <- "Vascular disease burden (4 items) - MAP/MARS
# only"
# attr(ds$gait_speed, "label") <- "Gait Speed - MAP"
# attr(ds$gripavg, "label") <- "Extremity strength"


# ---- save ---------
#save subset data as ds3
ds4<-ds
saveRDS(ds4, "./data/unshared/derived/ds4.rds")





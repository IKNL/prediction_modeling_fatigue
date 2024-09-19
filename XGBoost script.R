## Installing the necessary libraries ##
install.packages(readxl)
install.packages(dplyr)
install.packages(ggplot2)
install.packages(naniar)
install.packages(gridExtra)
install.packages(missRanger)
install.packages(caret)
install.packages(xgboost)
install.packages(pROC)
install.packages(predtools)
install.packages(rms)
install.packages(boot)

## Loading the necessary libraries and setting seed ##
library(haven)
library(readxl)
library(dplyr)
library(ggplot2)
library(naniar)
library(gridExtra)
library(missRanger)
library(performanceEstimation)
library(caret)
library(xgboost)
library(pROC)
library(predtools)
library(metamisc)
set.seed(100)
rm(list = ls()) # removes any memory left in R's Global Environment

## Loading the dataset ##
#raw_data <- "insert path here"
profileslongitudinal_18082023 <- read_excel("FILE_NAME.xlsx", 
                                            col_types = c("numeric", "numeric", "numeric", 
                                                          "numeric", "numeric", "numeric", 
                                                          "numeric", "numeric", "numeric", 
                                                          "numeric", "numeric", "numeric", 
                                                          "numeric", "numeric", "numeric", 
                                                          "numeric", "numeric", "numeric", 
                                                          "numeric", "numeric", "numeric", 
                                                          "numeric", "numeric", "numeric", 
                                                          "numeric", "numeric", "numeric", 
                                                          "numeric", "numeric", "numeric", 
                                                          "numeric", "numeric", "numeric", 
                                                          "numeric", "numeric", "numeric", 
                                                          "numeric", "numeric", "numeric", 
                                                          "numeric", "numeric", "numeric", 
                                                          "numeric", "numeric", "numeric", 
                                                          "numeric", "numeric", "numeric", 
                                                          "numeric", "numeric", "numeric", 
                                                          "numeric", "numeric", "numeric", 
                                                          "numeric", "numeric", "numeric", 
                                                          "numeric", "numeric", "text", "numeric", 
                                                          "numeric", "numeric", "numeric", 
                                                          "numeric", "numeric", "numeric", 
                                                          "numeric", "numeric", "numeric", 
                                                          "numeric", "numeric", "numeric", 
                                                          "numeric", "numeric"))
raw_data <- profileslongitudinal_18082023[order(profileslongitudinal_18082023$participant_id, profileslongitudinal_18082023$wave),]

##-----------------------------------------------------------------------------------------------------------------------------------##

## Overview of the dataset ##
summary(raw_data)
cat("Total number of patients:", n_distinct(raw_data$participant_id)) # there are 7,835 patients in the data
cat("Total number of rows:", nrow(raw_data))
cat("Total number of columns:", ncol(raw_data))

## Recoding variables ##
raw_data <- raw_data %>% # recoding variables to NAs based on guidebook
  mutate_at(c("education"), ~na_if(., 98)) %>% 
  mutate_at(c("education"), ~na_if(., 99)) %>%
  mutate_at(c("smoking"), ~na_if(., 98)) %>%
  mutate_at(c("smoking"), ~na_if(., 99)) %>%
  mutate_at(c("alcoholuse"), ~na_if(., 99))
raw_data <- raw_data %>% # recoding variables with outliers
  mutate(bmi = ifelse(raw_data$bmi > 251.1 | raw_data$bmi < 6.7, NA, raw_data$bmi)) %>%
  mutate(time_diagnosis = ifelse(raw_data$time_diagnosis < 0, NA, raw_data$time_diagnosis)) %>%
  mutate(fi = ifelse(raw_data$fi > 100, NA, raw_data$fi)) %>%
  mutate(sf = ifelse(raw_data$sf > 100, NA, raw_data$sf)) %>%
  mutate(source = ifelse(is.na(raw_data$source), "procore", raw_data$source))
raw_data <- raw_data %>%
  mutate(time_diagnosis = ifelse(raw_data$age_questionnaire > 122, NA, raw_data$time_diagnosis)) %>%
  mutate(time_dx_days = ifelse(raw_data$age_questionnaire > 122, NA, raw_data$time_dx_days)) %>%
  mutate(age_questionnaire = ifelse(raw_data$age_questionnaire > 122, NA, raw_data$age_questionnaire))
summary(raw_data)

## Recoding character variables ##
raw_data$source <- as.factor(raw_data$source)
summary(raw_data)

## Combining cancer stage variables ##
raw_data <- raw_data %>%
  mutate(stage = ifelse(is.na(raw_data$stage_annarbor) == TRUE, raw_data$stage_tnm, raw_data$stage_annarbor)) %>%
  relocate("stage", .after = "tumortype")
raw_data <- raw_data[-c(55:56)]
summary(raw_data) # checking whether the recoding process worked properly

##-----------------------------------------------------------------------------------------------------------------------------------##

## Conducting initial preprocessing (no imputation) on individual cohorts ##

# For PROCORE
df_procore <- raw_data[raw_data$source == "procore",] # separating PROCORE cohort and additional code needed because of a rogue entry
df_procore$participant_id[is.na(df_procore$participant_id)] <- 415
df_procore$time_diagnosis <- (df_procore$time_dx_days)/365 # some missing time since diagnosis values can be filled in using days count because of fewer NA's
df_procore <- df_procore %>% # filling time since diagnosis values
  group_by(participant_id) %>%
  mutate(time_diagnosis = ifelse(is.na(time_diagnosis) & wave == 4, df_procore[df_procore$wave == 1 & df_procore$participant_id == participant_id,]$time_diagnosis + 2, time_diagnosis)) %>% # filling in time since diagnosis at last timepoint (wave = 4) using first value (wave = 1)
  mutate(time_diagnosis = ifelse(is.na(time_diagnosis) & wave == 3, df_procore[df_procore$wave == 1 & df_procore$participant_id == participant_id,]$time_diagnosis + 1, time_diagnosis)) %>% # filling in time since diagnosis at wave = 3 using last value (wave = 4)
  mutate(time_dx_days = ifelse(is.na(time_dx_days), time_diagnosis*365, time_dx_days))
df_procore <- df_procore %>% # filling values for time between diagnosis and end of treatment
  group_by(participant_id) %>%
  mutate(time_dx_end_treatment_days = ifelse(is.na(time_dx_end_treatment_days) & wave != 1, df_procore[df_procore$wave == 1 & df_procore$participant_id == participant_id,]$time_dx_end_treatment_days, time_dx_end_treatment_days))
df_procore <- df_procore %>% # filling values for sociodemographic variables based on baseline
  group_by(participant_id) %>%
  mutate(education = ifelse(is.na(education) & wave != 1, df_procore[df_procore$wave == 1 & df_procore$participant_id == participant_id,]$education, education)) %>%
  mutate(partner = ifelse(is.na(partner) & wave != 1, df_procore[df_procore$wave == 1 & df_procore$participant_id == participant_id,]$partner, partner)) %>%
  mutate(smoking = ifelse(is.na(smoking) & wave != 1, df_procore[df_procore$wave == 1 & df_procore$participant_id == participant_id,]$smoking, smoking)) %>%
  mutate(alcoholuse = ifelse(is.na(alcoholuse) & wave != 1, df_procore[df_procore$wave == 1 & df_procore$participant_id == participant_id,]$alcoholuse, alcoholuse))
summary(df_procore) # checking if all coding is correct

# For LYMPHOMA
df_lymphoma <- raw_data[raw_data$source == "lymphoma",] # separating LYMPHOMA cohort
df_lymphoma <- df_lymphoma %>% # filling in age at diagnosis values
  group_by(participant_id) %>%
  mutate(age_diagnosis = ifelse(is.na(age_diagnosis), df_lymphoma[df_lymphoma$wave == 2 & df_lymphoma$participant_id == participant_id,]$age_diagnosis, age_diagnosis))
df_lymphoma <- df_lymphoma %>% # filling values for sociodemographic variables based on baseline
  group_by(participant_id) %>%
  mutate(education = ifelse(is.na(education) & wave != 1, df_lymphoma[df_lymphoma$wave == 1 & df_lymphoma$participant_id == participant_id,]$education, education)) %>%
  mutate(partner = ifelse(is.na(partner) & wave != 1, df_lymphoma[df_lymphoma$wave == 1 & df_lymphoma$participant_id == participant_id,]$partner, partner)) %>%
  mutate(smoking = ifelse(is.na(smoking) & wave != 1, df_lymphoma[df_lymphoma$wave == 1 & df_lymphoma$participant_id == participant_id,]$smoking, smoking)) %>%
  mutate(alcoholuse = ifelse(is.na(alcoholuse) & wave != 1, df_lymphoma[df_lymphoma$wave == 1 & df_lymphoma$participant_id == participant_id,]$alcoholuse, alcoholuse))
summary(df_lymphoma) # checking if all coding is correct

# For ROGY
df_rogy <- raw_data[raw_data$source == "rogy",] # separating ROGY cohort
df_rogy <- df_rogy %>% # filling in time since diagnosis values based on assumed follow-up time (6 months)
  group_by(participant_id) %>%
  mutate(time_diagnosis = ifelse(is.na(time_diagnosis) & wave == 2, df_rogy[df_rogy$wave == 1 & df_rogy$participant_id == participant_id,]$time_diagnosis + 0.5, time_diagnosis)) %>%
  mutate(time_diagnosis = ifelse(is.na(time_diagnosis) & wave == 3, df_rogy[df_rogy$wave == 1 & df_rogy$participant_id == participant_id,]$time_diagnosis + 1, time_diagnosis))
df_rogy <- df_rogy %>% # filling in time since diagnosis values based on assumed follow-up time (1 year)
  mutate(time_diagnosis = ifelse(is.na(time_diagnosis) & wave == 5, df_rogy[df_rogy$wave == 1 & df_rogy$participant_id == participant_id,]$time_diagnosis + 2, time_diagnosis)) %>%
  mutate(time_diagnosis = ifelse(is.na(time_diagnosis) & wave == 1, df_rogy[df_rogy$wave == 2 & df_rogy$participant_id == participant_id,]$time_diagnosis - 0.5, time_diagnosis)) %>%
  mutate(time_dx_days = ifelse(is.na(time_dx_days), time_diagnosis*365, time_dx_days))
df_rogy <- df_rogy %>% # filling in age at questionnaire using time since diagnosis values
  mutate(age_questionnaire = ifelse(is.na(age_questionnaire), round(age_diagnosis + time_diagnosis), age_questionnaire))
df_rogy <- df_rogy %>% # filling (some) under treatment values based on time since diagnosis and treatment duration
  mutate(under_treatment = ifelse(is.na(under_treatment) & wave == 1, ifelse((time_dx_end_treatment_days - time_dx_start_treatment_days) >= time_diagnosis, 1, 0), under_treatment)) # filling in under treatment based on treatment time values
df_rogy <- df_rogy %>% # filling in values for sociodemographic variables based on baseline
  group_by(participant_id) %>%
  mutate(education = ifelse(is.na(education) & wave != 1, df_rogy[df_rogy$wave == 1 & df_rogy$participant_id == participant_id,]$education, education)) %>%
  mutate(partner = ifelse(is.na(partner) & wave != 1, df_rogy[df_rogy$wave == 1 & df_rogy$participant_id == participant_id,]$partner, partner)) %>%
  mutate(smoking = ifelse(is.na(smoking) & wave != 1, df_rogy[df_rogy$wave == 1 & df_rogy$participant_id == participant_id,]$smoking, smoking)) %>%
  mutate(alcoholuse = ifelse(is.na(alcoholuse) & wave != 1, df_rogy[df_rogy$wave == 1 & df_rogy$participant_id == participant_id,]$alcoholuse, alcoholuse))
summary(df_rogy) # checking if all coding is correct

# For BlaZib
df_blazib <- raw_data[raw_data$source == "blazib",] # separating BlaZib cohort
df_blazib$time_diagnosis <- (df_blazib$time_dx_days)/365 # some missing time since diagnosis values can be filled in using days count because of fewer NA's
df_blazib <- df_blazib %>% # filling in time since diagnosis using waves (6 and 12 months)
  group_by(participant_id) %>%
  mutate(time_diagnosis = ifelse(is.na(time_diagnosis) & wave == 6, df_blazib[df_blazib$wave == 0 & df_blazib$participant_id == participant_id,]$time_diagnosis + 0.5, time_diagnosis)) %>%
  mutate(time_diagnosis = ifelse(is.na(time_diagnosis) & wave == 12, df_blazib[df_blazib$wave == 0 & df_blazib$participant_id == participant_id,]$time_diagnosis + 1, time_diagnosis))
df_blazib <- df_blazib %>% # filling in time since diagnosis using waves (24 months) and completing time since diagnosis days
  mutate(time_diagnosis = ifelse(is.na(time_diagnosis) & wave == 24, df_blazib[df_blazib$wave == 12 & df_blazib$participant_id == participant_id,]$time_diagnosis + 1, time_diagnosis)) %>%
  mutate(time_dx_days = ifelse(is.na(time_dx_days), time_diagnosis*365, time_dx_days))
df_blazib <- df_blazib %>% # filling in age at questionnaire values using known age at diagnosis and wave
  mutate(age_questionnaire = ifelse(is.na(age_questionnaire), round(age_diagnosis + (wave)/12), age_questionnaire))
df_blazib <- df_blazib %>% # filling in under treatment variable based on known patient's data (ID = 2597)
  mutate(under_treatment = ifelse(is.na(under_treatment), 0, under_treatment))
df_blazib <- df_blazib %>% # rewriting time since diagnosis outlier
  mutate(time_diagnosis = ifelse(time_diagnosis < 0, (wave)/12, time_diagnosis)) %>%
  mutate(time_dx_days = time_diagnosis*365)
df_blazib <- df_blazib %>% # filling in values for sociodemographic variables based on baseline
  group_by(participant_id) %>%
  mutate(education = ifelse(is.na(education) & wave != 0, df_blazib[df_blazib$wave == 0 & df_blazib$participant_id == participant_id,]$education, education)) %>%
  mutate(partner = ifelse(is.na(partner) & wave != 0, df_blazib[df_blazib$wave == 0 & df_blazib$participant_id == participant_id,]$partner, partner)) %>%
  mutate(smoking = ifelse(is.na(smoking) & wave != 0, df_blazib[df_blazib$wave == 0 & df_blazib$participant_id == participant_id,]$smoking, smoking)) %>%
  mutate(alcoholuse = ifelse(is.na(alcoholuse) & wave != 0, df_blazib[df_blazib$wave == 0 & df_blazib$participant_id == participant_id,]$alcoholuse, alcoholuse))
summary(df_blazib) # checking if all coding is correct

# For ProZib
df_prozib <- raw_data[raw_data$source == "prozib",] # separating ProZib cohort
df_prozib$time_diagnosis <- (df_prozib$time_dx_days)/365 # some missing time since diagnosis values can be filled in using days count because of fewer NA's
df_prozib <- df_prozib %>% # filling in time since diagnosis and age at questionnaire values
  group_by(participant_id) %>%
  mutate(time_diagnosis = ifelse(is.na(time_diagnosis) & wave == 3, df_prozib[df_prozib$wave == 2 & df_prozib$participant_id == participant_id,]$time_diagnosis + 1, time_diagnosis)) %>% # filling in time since diagnosis at last timepoint (wave = 3) using previous value (wave = 2)
  mutate(age_questionnaire = ifelse(is.na(age_questionnaire) & wave == 3, df_prozib[df_prozib$wave == 2 & df_prozib$participant_id == participant_id,]$age_questionnaire + 1, age_questionnaire)) %>% # filling in age at questionnaire at last timepoint (wave = 3) using previous value (wave = 2)
  mutate(time_dx_days = ifelse(is.na(time_dx_days), time_diagnosis*365, time_dx_days))
df_prozib <- df_prozib %>% # filling in under treatment values
  mutate(under_treatment = ifelse(is.na(under_treatment) & wave == 3, 0, under_treatment)) %>% # filling in under treatment values at final timepoint (wave = 3) based on known patient information
  mutate(under_treatment = ifelse(is.na(under_treatment) & wave == 1, ifelse((time_dx_end_treatment_days - time_dx_start_treatment_days) >= time_diagnosis, 1, 0), under_treatment)) # filling in under treatment based on treatment time values
df_prozib <- df_prozib %>% # filling in values for sociodemographic variables based on baseline
  group_by(participant_id) %>%
  mutate(education = ifelse(is.na(education) & wave != 1, df_prozib[df_prozib$wave == 1 & df_prozib$participant_id == participant_id,]$education, education)) %>%
  mutate(smoking = ifelse(is.na(smoking) & wave != 1, df_prozib[df_prozib$wave == 1 & df_prozib$participant_id == participant_id,]$smoking, smoking)) %>%
  mutate(alcoholuse = ifelse(is.na(alcoholuse) & wave != 1, df_prozib[df_prozib$wave == 1 & df_prozib$participant_id == participant_id,]$alcoholuse, alcoholuse))
summary(df_prozib) # checking if all coding is correct

# For PLCRC
df_plcrc <- raw_data[raw_data$source == "PLCRC",] # separating PLCRC cohort
df_plcrc <- df_plcrc %>% # filling in values for sociodemographic variables based on baseline
  group_by(participant_id) %>%
  mutate(education = ifelse(is.na(education) & wave != 1, df_plcrc[df_plcrc$wave == 1 & df_plcrc$participant_id == participant_id,]$education, education)) %>%
  mutate(partner = ifelse(is.na(partner) & wave != 1, df_plcrc[df_plcrc$wave == 1 & df_plcrc$participant_id == participant_id,]$partner, partner)) %>%
  mutate(smoking = ifelse(is.na(smoking) & wave != 1, df_plcrc[df_plcrc$wave == 1 & df_plcrc$participant_id == participant_id,]$smoking, smoking))
summary(df_plcrc) # checking if all coding is correct

# Recoding patient IDs to avoid overlap
df_procore$participant_id[df_procore$participant_id == 484] <- 14
df_procore$participant_id[df_procore$participant_id == 485] <- 46
df_procore$participant_id[df_procore$participant_id == 486] <- 114
df_procore$participant_id[df_procore$participant_id == 487] <- 198
df_procore$participant_id[df_procore$participant_id == 488] <- 240
df_procore$participant_id[df_procore$participant_id == 489] <- 250
df_procore$participant_id[df_procore$participant_id == 490] <- 275
df_procore$participant_id[df_procore$participant_id == 491] <- 312
df_procore$participant_id[df_procore$participant_id == 492] <- 325
df_procore$participant_id[df_procore$participant_id == 494] <- 338
df_procore$participant_id[df_procore$participant_id == 495] <- 349
df_procore$participant_id[df_procore$participant_id == 496] <- 389

# Combining the data
df_raw <- rbind(df_blazib, df_lymphoma, df_procore, df_prozib, df_rogy, df_plcrc)
df_raw <- df_raw[order(df_raw$participant_id, df_raw$wave),]
summary(df_raw)

##-----------------------------------------------------------------------------------------------------------------------------------##

## Conducting further preprocessing on data ##

# Recoding alcohol use variable to accommodate PLCRC cohort
df_raw[df_raw$source != "PLCRC",]$alcoholuse <- ifelse(df_raw[df_raw$source != "PLCRC",]$alcoholuse > 1, 1, 0) # recoding variable for non-PLCRC cohorts
df_raw[df_raw$source == "PLCRC",]$alcoholuse <- ifelse(df_raw[df_raw$source == "PLCRC",]$alcoholuse == 1, 1, 0) # recoding variable for PLCRC cohort
summary(df_raw$alcoholuse) # checking if all coding is correct

# Exploring clinically relevant fatigue in model-building data
df_raw$fa_cat <- as.factor(ifelse(df_raw$fa >= 40, 1, 0)) # creating new variable for clinically relevant fatigue
summary(df_raw$fa_cat)
# Of recorded fatigue, there are 5,842 cases of clinically relevant fatigue (~19.3%).
# There are 3,257 missing fatigue scores (~9.7%).
ggplot(data = subset(df_raw, !is.na(fa_cat)), aes(x = fa_cat, fill = source)) +
  geom_bar() +
  xlab("Fatigue category") # bar chart for fatigue of each category, coloured per cohort

##-----------------------------------------------------------------------------------------------------------------------------------##

## Conducting further preprocessing on model-building data ##

# Omitting patients with *entirely* missing questionnaire responses for fatigue
df_raw_less_prozib <- df_raw[df_raw$source != "prozib",] %>% # filtering and creating new dataframe 
  filter_at(vars(qlqc30_10, qlqc30_12, qlqc30_18),any_vars(!is.na(.)))
df_raw_prozib <- df_raw[df_raw$source == "prozib",] %>%
  filter_at(vars(ql, pf, rf, ef, cf, sf, fa, nv, pa, dy, sl, ap, co, di, fi),any_vars(!is.na(.)))
df_raw <- rbind(df_raw_less_prozib, df_raw_prozib) # restoring original dataset makeup
summary(df_raw) # checking if coding was done correctly
cat("Total number of patients:", n_distinct(df_raw$participant_id)) # 7,785 patients are now included (~99.4% of initial)

# Subsetting patients with *entirely* missing values for all sociodemographic variables
df_raw <- df_raw %>%
  group_by(participant_id) %>%
  filter_at(vars(education, partner, smoking, alcoholuse),any_vars(!is.na(.)))
summary(df_raw) # checking if all the coding is correct

# Subsetting model-building data for time elapsed being 24-36 months and minimum baseline at 3 months
df_raw <- df_raw %>%
  group_by(participant_id) %>%
  mutate(time_elapsed = max(round(time_diagnosis, 1), na.rm = TRUE) - min(round(time_diagnosis, 1), na.rm = TRUE)) %>%
  subset(time_elapsed >= 2 & time_elapsed <= 3) # getting the number of people who have the appropriate fatigue trajectories
df_raw <- df_raw %>%
  group_by(participant_id) %>%
  mutate(time_min = min(round(time_diagnosis, 2), na.rm = TRUE)) %>%
  subset(time_min >= -0.25 & time_min <= 0.25)
df_raw <- df_raw[order(df_raw$participant_id, df_raw$wave),]
summary(df_raw) # checking if all the coding is correct
cat("Total number of patients:", n_distinct(df_raw$participant_id)) # 3,239 patients are now included (~41.6% of initial)
# Patient numbers: PROCORE - 323, LYMPHOMA - 0, ROGY - 109, ProZib - 637, PLCRC - 1,944, BlaZib - 226
# Applicable treatment types: radiotherapy, surgery, systemic

# Exploring clinically relevant fatigue in model-building datasets
summary(df_raw$fa_cat)
# Of recorded fatigue, there are 2,443 cases of clinically relevant fatigue (~16.4%).
# There are 20 missing fatigue scores (<0.01%).
ggplot(data = subset(df_raw, !is.na(fa_cat)), aes(x = fa_cat, fill = source)) +
  geom_bar() +
  xlab("Fatigue category") # bar chart for fatigue of each category, coloured per cohort
ggplot(data = subset(df_raw, !is.na(fa_cat)), aes(x = time_diagnosis, y = fa_cat, colour = source, group = participant_id)) +
  geom_line() +
  geom_point() + 
  scale_y_discrete(na.translate = FALSE) # dotted line graph for progress of fatigue (category) per individual 

##-----------------------------------------------------------------------------------------------------------------------------------##

## Preparing datasets for imputation and analysis ##

# Splitting model-building dataset to baseline and endpoint sets
df_raw <- df_raw[,c(1:7,38:54,56,59:64,67:68,70:77)]
df_base_less_blazib <- df_raw[df_raw$source != "blazib",] %>% # Creating a dataset with the baseline input
  group_by(participant_id) %>%
  filter(time_min == round(time_diagnosis, 2)) %>%
  distinct(participant_id, .keep_all = TRUE)
df_base_blazib <- df_raw[df_raw$source == "blazib",] %>%
  group_by(participant_id) %>%
  filter(wave == 0) %>%
  distinct(participant_id, .keep_all = TRUE)
df_base <- rbind(df_base_less_blazib, df_base_blazib)
df_base <- df_base[order(df_base$participant_id, df_base$wave),]
df_end_less_blazib <- df_raw[df_raw$source != "blazib",] %>% # Creating a dataset with data after 24-36 months
  group_by(participant_id) %>%
  mutate(time_max = max(round(time_diagnosis, 2), na.rm = TRUE)) %>%
  filter(time_max == round(time_diagnosis, 2)) %>%
  distinct(participant_id, .keep_all = TRUE)
df_end_blazib <- df_raw[df_raw$source == "blazib",] %>%
  group_by(participant_id) %>%
  filter(wave == 24) %>%
  distinct(participant_id, .keep_all = TRUE)
df_end <- rbind(df_end_less_blazib, df_end_blazib)
df_end <- df_end[order(df_end$participant_id, df_end$wave),]
df_base <- df_base[,-c(28,31,36,40:41)]
df_end <- df_end[,-c(28,31,36,40:42)]
summary(df_base)
summary(df_end)

# Appending endpoint outcome with baseline predictors
df_end$fa_cat_end <- df_end$fa_cat
df_raw <- cbind(df_base, df_end$fa_cat_end)
df_raw$fa_cat_base <- df_raw$fa_cat # renaming baseline outcome
df_raw$fa_cat_end <- df_raw$...37 # renaming endpoint outcome
df_raw <- df_raw[,c(1:35,38:39)]
summary(df_raw)

# Subsetting patients with *no* fatigue records
df_raw <- df_raw %>%
  group_by(participant_id) %>%
  filter(!is.na(fa_cat_base) & !is.na(fa_cat_end))
df_raw <- df_raw[,-c(36)] # omitting clinically-relevant fatigue at baseline
summary(df_raw)
cat("Total number of patients:", n_distinct(df_raw$participant_id)) # 3,225 patients are now included (~41.2% of initial)
# Patient numbers: PROCORE - 322, LYMPHOMA - 0, ROGY - 109, ProZib - 628, PLCRC - 1,941, BlaZib - 225

# Subsetting time-to-diagnosis-related variables
df_raw <- df_raw %>% # recoding "under_treatment" variable
  mutate(under_treatment = as.factor(ifelse(is.na(under_treatment), ifelse(time_dx_start_treatment_days <= time_dx_days & time_dx_end_treatment_days >= time_dx_days,1,0), under_treatment)))
df_raw$under_treatment <- as.factor(ifelse(is.na(df_raw$time_dx_start_treatment_days) & is.na(df_raw$time_dx_end_treatment_days), NA, df_raw$under_treatment))
df_raw <- df_raw %>%
  mutate(under_treatment = as.factor(ifelse(under_treatment == 2, 1, 0))) %>%
  mutate(time_dx_end_treatment_days = ifelse(time_dx_end_treatment_days < 0, NA, time_dx_end_treatment_days)) %>%
  filter(!is.na(under_treatment))
df_raw <- df_raw[,-c(34)] # omitting "time_dx_end_treatment_days" variable from analysis
summary(df_raw)
cat("Total number of patients:", n_distinct(df_raw$participant_id)) # 3,183 patients are now included (~40.6% of initial)
# Patient numbers: PROCORE - 322, LYMPHOMA - 0, ROGY - 109, ProZib - 586, PLCRC - 1,941, BlaZib - 225

# Subsetting patients with stage IV cancer
df_raw <- df_raw %>%
  filter(stage != 4)
summary(df_raw)
cat("Total number of patients:", n_distinct(df_raw$participant_id)) # 3,165 patients are now included (~40.4% of initial)
# Patient numbers: PROCORE - 316, LYMPHOMA - 0, ROGY - 106, ProZib - 586, PLCRC - 1,941, BlaZib - 216

# Recoding tumour types
df_raw$tumortype <- as.factor(ifelse(df_raw$tumortype == 8, 7, df_raw$tumortype))
summary(df_raw)

##-----------------------------------------------------------------------------------------------------------------------------------##

## Imputing input variables separately for the baseline dataset ##

sapply(df_raw, function(x) sum(is.na(x)))
# Missing variables present in all except: participant_id, wave, fa, tumortype, radiotherapy, source, surgery, systemic, age_diagnosis, sex, age_questionnaire, time_dx_days

# Conducting additional data manipulation
df_raw <- df_raw %>% mutate_at(c(2:7,23:26,28:30,34:35), as.factor) # converting categorical variables to factors
summary(df_raw)

# Conducting imputation using missRanger (improved version of missForest)
imp_raw <- list()
prog <- 0
for (i in 1:10){ # create a for-loop to create multiple datasets to account for variation in imputation
  imp <- missRanger(df_raw, formula = . - participant_id - wave ~ . - participant_id - wave, maxiter = 10, num.trees = 100, seed = 100*i, verbose = 0)
  imp$imp <- i # adding new variable to denote imputed variable
  imp_raw[[i]] <- imp
  prog <- prog + 1
  print(prog) # printing progress tracker
}

# Combining the imputed datasets and conducting final data manipulation
data_raw <- data.frame()
for (i in 1:10){
  data_raw <- rbind(data_raw, imp_raw[[i]])
}
data_raw <- data_raw %>%
  #  mutate(under_treatment = as.factor(ifelse(time_dx_start_treatment_days <= time_dx_days & time_dx_end_treatment_days >= time_dx_days,1,0))) %>%
  #  mutate(time_dx_treatment_days = time_dx_end_treatment_days - time_dx_start_treatment_days) %>%
  relocate("imp", .before = "participant_id")
summary(data_raw) # checking whether the imputation process worked properly (no NA's left)

##-----------------------------------------------------------------------------------------------------------------------------------##

## Codes for model development and analysis of the XGBoost model ##

# Creating lists to store prediction and statistical outputs from internal validation
xgb_pred_test <- list()
xgb_acc_test <- list()
xgb_balancedacc_test <- list()
xgb_precision_test <- list()
xgb_sensitivity_test <- list()
xgb_specificity_test <- list()
xgb_npv_test <- list()
xgb_roc_test <- list()
xgb_cstat_test <- list()
xgb_rsquared_test <- list()
xgb_slope_test <- list()
n_imp <- n_distinct(data_raw$imp)

# Running a for-loop so that the model runs on each imputed dataset
xgb_prog <- 0
xgb_start <- Sys.time() # for tracking computation time
for (i in 1:n_imp){
  set.seed(100)
  
  # Isolating the imputed datasets
  n <- sample(c(TRUE,FALSE), nrow(data_raw[data_raw$imp == i,]), replace = TRUE, prob = c(0.7,03))
  df_train <- data_raw[data_raw$imp == i,-c(27)][n,]
  df_smote_train <- smote(fa_cat_end ~ ., df_train, perc.over = 4, perc.under = 3) # using SMOTE to oversample the outcome measure
  dummy_train <- dummyVars(" ~ . - imp - participant_id - wave - fa_cat_end", data = df_smote_train)
  train_x <- as.matrix(data.frame(predict(dummy_train, newdata = df_smote_train))) # creating a data matrix for input variables in training data
  train_y <- data.matrix(df_smote_train$fa_cat_end) # creating a data matrix for the clinically-relevant fatigue in training data
  df_test <- data_raw[data_raw$imp == i,-c(27)][!n,]
  dummy_test <- dummyVars(" ~ . - imp - participant_id - wave - fa_cat_end", data = df_test)
  test_x <- as.matrix(data.frame(predict(dummy_test, newdata = df_test))) # creating a data matrix for input variables in test data
  test_y <- data.matrix(df_test$fa_cat_end) # creating a data matrix for the clinically-relevant fatigue in test data

  # Running the training functions
  xgb_train <- xgb.DMatrix(data = train_x, label = train_y) # creating appropriate data matrix for training data
  xgb_model <- xgboost(data = xgb_train, nrounds = 2, objective = "binary:logistic") # fitting the gradient-boosted model on all variables
# xgb_grid <- expand.grid(nrounds = 10, max_depth = c(3,5,7), eta = c(0.01,0.05,0.1), gamma = c(0.1,1,10), colsample_bytree = 1, min_child_weight = 1, subsample = c(0.6,0.7,0.8))
  xgb_tuned <- train(x = train_x, y = train_y, method = "xgbTree", verbose = FALSE, tuneLength = 10, trControl = trainControl(method = "repeatedcv", number = 10, search = "random")) # training tuned parameters
  xgb_final <- xgboost(data = xgb_train, objective = "binary:logistic", nrounds = xgb_tuned$bestTune$nrounds, max_depth = xgb_tuned$bestTune$max_depth, 
                       eta = xgb_tuned$bestTune$eta, gamma = xgb_tuned$bestTune$gamma, subsample = xgb_tuned$bestTune$subsample, min_child_weight = xgb_tuned$bestTune$min_child_weight) # fitting the tuned hyperparameters into the final model
  
  # Conducting statistical analysis on the dataset with bootstrapping
  xgb_acc_func <- function(data, index){ # creating a function for calculating the accuracy
    df <- data[index,]
    dum <- dummyVars(" ~ . - imp - participant_id - wave - fa_cat_end", data = df)
    mat <- as.matrix(data.frame(predict(dum, newdata = df)))    
    mod <- xgb_final
    pred <- predict(mod, mat)
    test <- as.factor(ifelse(pred >= 0.5, 1, 0))
    confusionMatrix <- confusionMatrix(test, df$fa_cat_end, positive = "1")
    acc <- confusionMatrix[["overall"]][["Accuracy"]]
    return(acc)
  }
  xgb_acc <- boot::boot(data = df_test, statistic = xgb_acc_func, R = 1000) # applying the bootstrapping function on the testing dataset
  xgb_acc_test[[i]] <- xgb_acc$t # storing the function results from each bootstrapped instance
  
  xgb_balancedacc_func <- function(data, index){ # creating a function for calculating the balanced accuracy
    df <- data[index,]
    dum <- dummyVars(" ~ . - imp - participant_id - wave - fa_cat_end", data = df)
    mat <- as.matrix(data.frame(predict(dum, newdata = df)))
    mod <- xgb_final
    pred <- predict(mod, mat)
    test <- as.factor(ifelse(pred >= 0.5, 1, 0))
    confusionMatrix <- confusionMatrix(test, df$fa_cat_end, positive = "1")
    balancedacc <- confusionMatrix[["byClass"]][["Balanced Accuracy"]]
    return(balancedacc)
  }
  xgb_balancedacc <- boot::boot(data = df_test, statistic = xgb_balancedacc_func, R = 1000) # applying the bootstrapping function on the testing dataset
  xgb_balancedacc_test[[i]] <- xgb_balancedacc$t # storing balanced accuracy values
  
  xgb_precision_func <- function(data, index){ # creating a function for calculating the precision
    df <- data[index,]
    dum <- dummyVars(" ~ . - imp - participant_id - wave - fa_cat_end", data = df)
    mat <- as.matrix(data.frame(predict(dum, newdata = df)))
    mod <- xgb_final
    pred <- predict(mod, mat)
    test <- as.factor(ifelse(pred >= 0.5, 1, 0))
    confusionMatrix <- confusionMatrix(test, df$fa_cat_end, positive = "1")
    precision <- confusionMatrix[["byClass"]][["Precision"]]
    return(precision)
  }
  xgb_precision <- boot::boot(data = df_test, statistic = xgb_precision_func, R = 1000) # applying the bootstrapping function on the testing dataset
  xgb_precision_test[[i]] <- xgb_precision$t # storing precision values
  
  xgb_sensitivity_func <- function(data, index){ # creating a function for calculating the sensitivity
    df <- data[index,]
    dum <- dummyVars(" ~ . - imp - participant_id - wave - fa_cat_end", data = df)
    mat <- as.matrix(data.frame(predict(dum, newdata = df)))
    mod <- xgb_final
    pred <- predict(mod, mat)
    test <- as.factor(ifelse(pred >= 0.5, 1, 0))
    confusionMatrix <- confusionMatrix(test, df$fa_cat_end, positive = "1")
    sensitivity <- confusionMatrix[["byClass"]][["Sensitivity"]]
    return(sensitivity)
  }
  xgb_sensitivity <- boot::boot(data = df_test, statistic = xgb_sensitivity_func, R = 1000) # applying the bootstrapping function on the testing dataset
  xgb_sensitivity_test[[i]] <- xgb_sensitivity$t # storing sensitivity values
  
  xgb_specificity_func <- function(data, index){ # creating a function for calculating the specificity
    df <- data[index,]
    dum <- dummyVars(" ~ . - imp - participant_id - wave - fa_cat_end", data = df)
    mat <- as.matrix(data.frame(predict(dum, newdata = df)))
    mod <- xgb_final
    pred <- predict(mod, mat)
    test <- as.factor(ifelse(pred >= 0.5, 1, 0))
    confusionMatrix <- confusionMatrix(test, df$fa_cat_end, positive = "1")
    specificity <- confusionMatrix[["byClass"]][["Specificity"]]
    return(specificity)
  }
  xgb_specificity <- boot::boot(data = df_test, statistic = xgb_specificity_func, R = 1000) # applying the bootstrapping function on the testing dataset
  xgb_specificity_test[[i]] <- xgb_specificity$t # storing specificity values
  
  xgb_npv_func <- function(data, index){ # creating a function for calculating the specificity
    df <- data[index,]
    dum <- dummyVars(" ~ . - imp - participant_id - wave - fa_cat_end", data = df)
    mat <- as.matrix(data.frame(predict(dum, newdata = df)))
    mod <- xgb_final
    pred <- predict(mod, mat)
    test <- as.factor(ifelse(pred >= 0.5, 1, 0))
    confusionMatrix <- confusionMatrix(test, df$fa_cat_end, positive = "1")
    npv <- confusionMatrix[["byClass"]][["Neg Pred Value"]]
    return(npv)
  }
  xgb_npv <- boot::boot(data = df_test, statistic = xgb_npv_func, R = 1000) # applying the bootstrapping function on the testing dataset
  xgb_npv_test[[i]] <- xgb_npv$t # storing specificity values
  
  xgb_cstat_func <- function(data, index){ # creating a function for calculating the C-statistic
    df <- data[index,]
    dum <- dummyVars(" ~ . - imp - participant_id - wave - fa_cat_end", data = df)
    mat <- as.matrix(data.frame(predict(dum, newdata = df)))
    mod <- xgb_final
    pred <- predict(mod, mat)
    cstat <- as.list(rms::val.prob(unlist(pred), as.numeric(df$fa_cat_end), pl = FALSE, smooth = FALSE, statloc = FALSE))
    return(cstat$C)
  }
  xgb_cstat <- boot::boot(data = df_test, statistic = xgb_cstat_func, R = 1000) # applying the bootstrapping function on the testing dataset
  xgb_cstat_test[[i]] <- xgb_cstat$t # storing the function results from each bootstrapped instance
  
  xgb_rsquared_func <- function(data, index){ # creating a function for calculating the C-statistic
    df <- data[index,]
    dum <- dummyVars(" ~ . - imp - participant_id - wave - fa_cat_end", data = df)
    mat <- as.matrix(data.frame(predict(dum, newdata = df)))
    mod <- xgb_final
    pred <- predict(mod, mat)
    rsquared <- as.list(rms::val.prob(unlist(pred), as.numeric(df$fa_cat_end), pl = FALSE, smooth = FALSE, statloc = FALSE))
    return(rsquared$R2)
  }
  xgb_rsquared <- boot::boot(data = df_test, statistic = xgb_rsquared_func, R = 1000) # applying the bootstrapping function on the testing dataset
  xgb_rsquared_test[[i]] <- xgb_rsquared$t # storing the function results from each bootstrapped instance
  
  xgb_slope_func <- function(data, index){ # creating a function for calculating the C-statistic
    df <- data[index,]
    dum <- dummyVars(" ~ . - imp - participant_id - wave - fa_cat_end", data = df)
    mat <- as.matrix(data.frame(predict(dum, newdata = df)))
    mod <- xgb_final
    pred <- predict(mod, mat)
    slope <- as.list(rms::val.prob(unlist(pred), as.numeric(df$fa_cat_end), pl = FALSE, smooth = FALSE, statloc = FALSE))
    return(slope$Slope)
  }
  xgb_slope <- boot::boot(data = df_test, statistic = xgb_slope_func, R = 1000) # applying the bootstrapping function on the testing dataset
  xgb_slope_test[[i]] <- xgb_slope$t # storing the function results from each bootstrapped instance
  
  # Conducting statistical analysis on testing dataset
  xgb_pred <- predict(xgb_final, test_x) # applying the model to predict fatigue
  xgb_pred_test[[i]] <- xgb_pred # storing predicted values
  xgb_roc_test[[i]] <- roc(df_test$fa_cat_end, as.numeric(xgb_pred_test[[i]]))

  xgb_prog <- xgb_prog + 1 # tracking progress based on imputed dataset
  print(xgb_prog)
} # close for-loop
xgb_time <- Sys.time() - xgb_start
xgb_time

# Creating and printing ROC curves
xgb_roc_plot <- plot(xgb_roc_test[[1]], col = 1, lty = 2, main = "XGBoost model")
xgb_roc_plot <- plot(xgb_roc_test[[2]], col = 2, lty = 3, add = TRUE)
xgb_roc_plot <- plot(xgb_roc_test[[3]], col = 3, lty = 4, add = TRUE)
xgb_roc_plot <- plot(xgb_roc_test[[4]], col = 4, lty = 5, add = TRUE)
xgb_roc_plot <- plot(xgb_roc_test[[5]], col = 5, lty = 6, add = TRUE)
xgb_roc_plot <- plot(xgb_roc_test[[6]], col = 6, lty = 7, add = TRUE)
xgb_roc_plot <- plot(xgb_roc_test[[7]], col = 7, lty = 8, add = TRUE)
xgb_roc_plot <- plot(xgb_roc_test[[8]], col = 8, lty = 9, add = TRUE)
xgb_roc_plot <- plot(xgb_roc_test[[9]], col = 9, lty = 10, add = TRUE)
print(plot(xgb_roc_test[[10]], col = 10, lty = 11, add = TRUE))

# Creating and printing calibration plot for internal validation
xgb_pred_test_df <- data.frame(Predicted = unlist(xgb_pred_test[[10]]), Observed = as.numeric(df_test$fa_cat_end))
xgb_pred_test_df$Observed <- ifelse(xgb_pred_test_df$Observed == 1, 0, 1)
#rms::val.prob(xgb_pred_test_df$Predicted, as.numeric(data_test$fa_cat), pl = TRUE, smooth = FALSE, statloc = FALSE)
CalibrationCurves::val.prob.ci.2(xgb_pred_test_df$Predicted, xgb_pred_test_df$Observed, main = "XGBoost Model Calibration Curve")

# Creating and printing plots from bootstrapping analysis for internal validation
hist(unlist(xgb_acc_test), main = "Histogram of Accuracy of XGBoost Model", xlab = "Accuracy")
hist(unlist(xgb_balancedacc_test), main = "Histogram of Balanced Accuracy of XGBoost Model", xlab = "Balanced Accuracy")
hist(unlist(xgb_precision_test), main = "Histogram of Precision of XGBoost Model", xlab = "Precision")
hist(unlist(xgb_sensitivity_test), main = "Histogram of Sensitivity of XGBoost Model", xlab = "Sensitivity")
hist(unlist(xgb_specificity_test), main = "Histogram of Specificity of XGBoost Model", xlab = "Specificity")
hist(unlist(xgb_cstat_test), main = "Histogram of C-statistic for XGBoost Model", xlab = "C-statistic")
hist(unlist(xgb_rsquared_test), main = "Histogram of R-squared for XGBoost Model", xlab = "R-squared")
hist(unlist(xgb_slope_test), main = "Histogram of Calibration Slokpe for XGBoost Model", xlab = "Calibration Slope")

# Printing statistical results for internal validation
result <- cat("Average Accuracy for XGB:", mean(unlist(xgb_acc_test), na.rm = TRUE),
              "\nSD of Accuracy for XGB:", sd(unlist(xgb_acc_test), na.rm = TRUE),
              "\nAverage Balanced Accuracy for XGB:", mean(unlist(xgb_balancedacc_test), na.rm = TRUE),
              "\nSD of Balanced Accuracy for XGB:", sd(unlist(xgb_balancedacc_test), na.rm = TRUE),
              "\nAverage Precision for XGB:", mean(unlist(xgb_precision_test), na.rm = TRUE),
              "\nSD of Precision for XGB:", sd(unlist(xgb_precision_test), na.rm = TRUE),
              "\nAverage Sensitivity for XGB:", mean(unlist(xgb_sensitivity_test), na.rm = TRUE),
              "\nSD of Sensitivity for XGB:", sd(unlist(xgb_sensitivity_test), na.rm = TRUE),
              "\nAverage Specificity for XGB:", mean(unlist(xgb_specificity_test), na.rm = TRUE),
              "\nSD of Specificity for XGB:", sd(unlist(xgb_specificity_test), na.rm = TRUE),
              "\nAverage C-statistic for XGB:", mean(unlist(xgb_cstat_test), na.rm = TRUE),
              "\nSD of C-statistic for XGB:", sd(unlist(xgb_cstat_test), na.rm = TRUE),
              "\nAverage R-squared for XGB:", mean(unlist(xgb_rsquared_test), na.rm = TRUE),
              "\nSD of R-squared for XGB:", sd(unlist(xgb_rsquared_test), na.rm = TRUE),
              "\nAverage Calibration Slope for XGB:", mean(unlist(xgb_slope_test), na.rm = TRUE),
              "\nSD of Calibration Slope for XGB:", sd(unlist(xgb_slope_test), na.rm = TRUE))

##-----------------------------------------------------------------------------------------------------------------------------------##

## Conducting SHAP analysis on the XGBoost model ##

xgb.plot.shap(data = test_x, n_col = 2, top_n = 10, model = xgb_final)
xgb.ggplot.shap.summary(data = test_x, top_n = 10, model = xgb_final)

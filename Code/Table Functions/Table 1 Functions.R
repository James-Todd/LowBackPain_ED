#### Author: James Todd
#### Date: 2021/09/03
#### Last Updated: 2021/09/06
#### Table 1 Function Definitions

## Purpose
# This file defines the functions used when constructing Table 1





#Data preparation#
#' Function subsetting the EDIS and FN data for use in other functions
#' @param EDIS - a dataframe of the refined EDIS data, subset to the year currently of interest
#' @param FN - a dataframe of the refined FirstNet data, subset to the year currently of interest
#' @param category - a string of "NSLBP", "Radicular", "Sinister", or "All" to determine data subsetting
#' @param age - a string of "Young", "Old", or "All" to determine data subsetting
#' @return subset_list - a list of two data frames. These are the subset EDIS and FN data
data_subset <- function(EDIS, FN, category, age) {
  
  #LBP Data prep:
  if(category == "All") {
    subset_fn <- FN
    subset_edis <- EDIS
  } else if (category %in% c("NSLBP", "Radicular", "Sinister")) {
    subset_fn <- FN[FN[,paste0(category, "_Flag")] == 1,]
    subset_edis <- EDIS[EDIS[,paste0(category, "_Flag")] == 1,]
  } else {
    stop("category needs to take an allowable value")
  }
  
  #Age Data prep:
  if(age == "All") {
    #Do nothing, no need to subset further
  } else if(age == "Young") {
    subset_fn <- subset(subset_fn, Age < 65 & Age >= 18)
    subset_edis <- subset(subset_edis, Age.at.Arrival < 65 & Age.at.Arrival >= 18)
  } else if (age == "Old") { 
    subset_fn <- subset(subset_fn, Age >= 65)
    subset_edis <- subset(subset_edis, Age.at.Arrival >= 65)
  } else {
    stop("age needs to take an allowable value")
  }
  
  #Put the two filtered data frames in a list and return them
  subset_list <- list(subset_fn,subset_edis)
  return(subset_list)
  
}


#Gender:#
#' Function returning the number and % of female presenting patients for a given data subset
#' @param EDIS - a dataframe of the refined EDIS data, subset to the year currently of interest
#' @param FN - a dataframe of the refined FirstNet data, subset to the year currently of interest
#' @param category - a string of "NSLBP", "Radicular", "Sinister", or "All" to determine data subsetting
#' @param age - a string of "Young", "Old", or "All" to determine data subsetting
#' @return gender_summary - a string of the number and (%) of female presentations
female_pres <- function(EDIS, FN, category, age) {
  
  #Data prep:
  prepped_data <- data_subset(EDIS = EDIS, FN = FN, category = category, age = age)
  fn_subset <- prepped_data[[1]]
  edis_subset <- prepped_data[[2]]
  
  #calculation of female presentations: n (%)
  count_female <- nrow(fn_subset[fn_subset$Gender == "FEMALE",]) + nrow(edis_subset[edis_subset$Gender == "F",])
  percent_female <- 100*count_female / (nrow(fn_subset) + nrow(edis_subset))
  
  #Format and return the result
  gender_summary <- paste0(count_female, " (",round(percent_female, 1), ")")
  return(gender_summary)
  
}


#Age:#
#' Function returning the mean and SD of presenting patient ages for a given data subset
#' @param EDIS - a dataframe of the refined EDIS data, subset to the year currently of interest
#' @param FN - a dataframe of the refined FirstNet data, subset to the year currently of interest
#' @param category - a string of "NSLBP", "Radicular", "Sinister", or "All" to determine data subsetting
#' @param age - a string of "Young", "Old", or "All" to determine data subsetting
#' @return age_summary - a string of the mean and (SD) of age of presentations
age_pres <- function(EDIS, FN, category, age) {
  
  #Data prep:
  prepped_data <- data_subset(EDIS = EDIS, FN = FN, category = category, age = age)
  fn_subset <- prepped_data[[1]]
  edis_subset <- prepped_data[[2]]
  
  #calculation of mean and sd presentation age: mean (sd)
  age_mean <- mean(c(fn_subset$Age, edis_subset$Age.at.Arrival))
  age_sd <- sd(c(fn_subset$Age, edis_subset$Age.at.Arrival))
  
  
  #Format and return the result
  age_summary <- paste0(round(age_mean,1), " (",round(age_sd, 1), ")")
  return(age_summary)
  
}


#Adults# #FLAG: now using this for total presentations, but no modification needed
#' Function returning the number and % of presentations of adults
#' @param EDIS - a dataframe of the refined EDIS data, subset to the year currently of interest
#' @param FN - a dataframe of the refined FirstNet data, subset to the year currently of interest
#' @param category - a string of "NSLBP", "Radicular", "Sinister", or "All" to determine data subsetting
#' @param age - a string of "Young", "Old", or "All" to determine data subsetting
#' @return adult_summary - a string of the number and (%) of  presentations of adults
adult_pres <- function(EDIS, FN, category, age) {
  
  #Data prep:
  prepped_data <- data_subset(EDIS = EDIS, FN = FN, category = category, age = age)
  fn_subset <- prepped_data[[1]]
  edis_subset <- prepped_data[[2]]
  
  #calculation of weekend  presentations: n (%)
  count_adult <- sum(fn_subset$Age >= 18) + sum(edis_subset$Age.at.Arrival >= 18)
  percent_adult <- 100 * count_adult / (nrow(fn_subset) + nrow(edis_subset))
  
  #Format and return the result
  adult_summary <- paste0(count_adult, " (",round(percent_adult, 1), ")")
  return(adult_summary)
  
}


#Old Adults#
#' Function returning the number and % of presentations of adults aged >=65
#' @param EDIS - a dataframe of the refined EDIS data, subset to the year currently of interest
#' @param FN - a dataframe of the refined FirstNet data, subset to the year currently of interest
#' @param category - a string of "NSLBP", "Radicular", "Sinister", or "All" to determine data subsetting
#' @param age - a string of "Young", "Old", or "All" to determine data subsetting
#' @return old_summary - a string of the number and (%) of  presentations of adults aged >=65
old_adult_pres <- function(EDIS, FN, category, age) {
  
  #Data prep:
  prepped_data <- data_subset(EDIS = EDIS, FN = FN, category = category, age = age)
  fn_subset <- prepped_data[[1]]
  edis_subset <- prepped_data[[2]]
  
  #calculation of weekend  presentations: n (%)
  count_old <- sum(fn_subset$Age >= 65) + sum(edis_subset$Age.at.Arrival >= 65)
  percent_old <- 100 * count_old / (nrow(fn_subset) + nrow(edis_subset))
  
  #Format and return the result
  old_summary <- paste0(count_old, " (",round(percent_old, 1), ")")
  return(old_summary)
  
}


#Mode of Arrival#
#' Function returning the number and % of presentations matching a certain mode of arrival
#' @param EDIS - a dataframe of the refined EDIS data, subset to the year currently of interest
#' @param FN - a dataframe of the refined FirstNet data, subset to the year currently of interest
#' @param category - a string of "NSLBP", "Radicular", "Sinister", or "All" to determine data subsetting
#' @param age - a string of "Young", "Old", or "All" to determine data subsetting
#' @param arrival_type - a string of "walked in", "ambulance", "helicopter"
#' @return type_summary - a string of the number and (%) of "arrival_type" presentations
arrival_mode_pres <- function(EDIS, FN, category, age, arrival_type) {
  
  #Data prep:
  prepped_data <- data_subset(EDIS = EDIS, FN = FN, category = category, age = age)
  fn_subset <- prepped_data[[1]]
  edis_subset <- prepped_data[[2]]
  
  #calculation of "arrival_type"  presentations: n (%)
  count_type <- sum(str_detect(string = tolower(c(fn_subset$Arrival.Mode, edis_subset$Arrival.Mode)), 
                                            pattern = arrival_type))
  percent_type <- 100 * count_type / (nrow(fn_subset) + nrow(edis_subset))

  #Format and return the result
  type_summary <- paste0(count_type, " (",round(percent_type, 1), ")")
  return(type_summary)
  
}


#Triage#
#' Function returning the number and % of presentations of a given triage level
#' @param EDIS - a dataframe of the refined EDIS data, subset to the year currently of interest
#' @param FN - a dataframe of the refined FirstNet data, subset to the year currently of interest
#' @param category - a string of "NSLBP", "Radicular", "Sinister", or "All" to determine data subsetting
#' @param age - a string of "Young", "Old", or "All" to determine data subsetting
#' @param triage_scale - an integer indicating the triage scale of interest
#' @return ATS_summary - a string of the number and (%) of  presentations at the specified triage level
triage_scale_pres <- function(EDIS, FN, category, age, triage_scale) {
  
  #Data prep:
  prepped_data <- data_subset(EDIS = EDIS, FN = FN, category = category, age = age)
  fn_subset <- prepped_data[[1]]
  edis_subset <- prepped_data[[2]]
  
  #calculation of triage_scale  presentations: n (%)
  count_ATS <- nrow(fn_subset[fn_subset$ATS == triage_scale,]) + nrow(edis_subset[edis_subset$ATS == triage_scale,])
  percent_ATS <- 100 * count_ATS / (nrow(fn_subset) + nrow(edis_subset))
  
  #Format and return the result
  ATS_summary <- paste0(count_ATS, " (",round(percent_ATS, 1), ")")
  return(ATS_summary)
  
}


#Weekend#
#' Function returning the number and % of presentations occurring on weekends
#' @param EDIS - a dataframe of the refined EDIS data, subset to the year currently of interest
#' @param FN - a dataframe of the refined FirstNet data, subset to the year currently of interest
#' @param category - a string of "NSLBP", "Radicular", "Sinister", or "All" to determine data subsetting
#' @param age - a string of "Young", "Old", or "All" to determine data subsetting
#' @return weekend_summary - a string of the number and (%) of  presentations on weekends
weekend_pres <- function(EDIS, FN, category, age) {
  
  #Data prep:
  prepped_data <- data_subset(EDIS = EDIS, FN = FN, category = category, age = age)
  fn_subset <- prepped_data[[1]]
  edis_subset <- prepped_data[[2]]
  
  #calculation of weekend  presentations: n (%)
  count_weekend <- sum(fn_subset$Weekend_Flag) + sum(edis_subset$Weekend_Flag)
  percent_weekend <- 100 * count_weekend / (nrow(fn_subset) + nrow(edis_subset))
  
  #Format and return the result
  weekend_summary <- paste0(count_weekend, " (",round(percent_weekend, 1), ")")
  return(weekend_summary)
  
}


#Work hours#
#' Function returning the number and % of presentations occurring in work hours (8-5, M-F)
#' @param EDIS - a dataframe of the refined EDIS data, subset to the year currently of interest
#' @param FN - a dataframe of the refined FirstNet data, subset to the year currently of interest
#' @param category - a string of "NSLBP", "Radicular", "Sinister", or "All" to determine data subsetting
#' @param age - a string of "Young", "Old", or "All" to determine data subsetting
#' @return working_summary - a string of the number and (%) of  presentations in working hours
working_hour_pres <- function(EDIS, FN, category, age) {
  
  #Data prep:
  prepped_data <- data_subset(EDIS = EDIS, FN = FN, category = category, age = age)
  fn_subset <- prepped_data[[1]]
  edis_subset <- prepped_data[[2]]
  
  #calculation of weekend  presentations: n (%)
  count_working <- sum(fn_subset$WorkingHour_Flag) + sum(edis_subset$WorkingHour_Flag)
  percent_working <- 100 * count_working / (nrow(fn_subset) + nrow(edis_subset))
  
  #Format and return the result
  working_summary <- paste0(count_working, " (",round(percent_working, 1), ")")
  return(working_summary)
  
}


#ED LOS#
#' Function returning the median and IQR of presenting patient ED LOS for a given data subset
#' @param EDIS - a dataframe of the refined EDIS data, subset to the year currently of interest
#' @param FN - a dataframe of the refined FirstNet data, subset to the year currently of interest
#' @param category - a string of "NSLBP", "Radicular", "Sinister", or "All" to determine data subsetting
#' @param age - a string of "Young", "Old", or "All" to determine data subsetting
#' @return ed_los_summary - a string of the median and (IQR) of presentation ED LOS
ed_los_pres <- function(EDIS, FN, category, age) {
  
  #Data prep:
  prepped_data <- data_subset(EDIS = EDIS, FN = FN, category = category, age = age)
  fn_subset <- prepped_data[[1]]
  edis_subset <- prepped_data[[2]]
  
  #calculation of mean and sd presentation age: mean (sd)
  ed_los_quartiles <- quantile(x = c(fn_subset$ED.Total.LOS.Hrs, edis_subset$ED.Total.LOS.Hrs),
                              probs = c(0.25,0.5,0.75),
                              na.rm = T)
  
  
  #Format and return the result
  ed_los_summary <- paste0(round(ed_los_quartiles[2],1), 
                           " (",round(ed_los_quartiles[1], 1),"-",
                           round(ed_los_quartiles[3], 1),")")
  return(ed_los_summary)
  
}


#Representations:#
#' Function returning the number and % of 48 hour representations for a given data subset
#' @param EDIS - a dataframe of the refined EDIS data, subset to the year currently of interest
#' @param FN - a dataframe of the refined FirstNet data, subset to the year currently of interest
#' @param category - a string of "NSLBP", "Radicular", "Sinister", or "All" to determine data subsetting
#' @param age - a string of "Young", "Old", or "All" to determine data subsetting
#' @return represent_summary - a string of the number and (%) of presentations that are representations
represent_pres <- function(EDIS, FN, category, age) {
  
  #Data prep:
  prepped_data <- data_subset(EDIS = EDIS, FN = FN, category = category, age = age)
  fn_subset <- prepped_data[[1]]
  edis_subset <- prepped_data[[2]]
  
  #calculation of representations: n (%)
  count_represent <- sum(fn_subset$Represent_48hr == "Y") + sum(edis_subset$Represent_48hr == "Y")
  percent_represent <- 100*count_represent / (nrow(fn_subset) + nrow(edis_subset))
  
  #Format and return the result
  represent_summary <- paste0(count_represent, " (",round(percent_represent, 1), ")")
  return(represent_summary)
  
}


#Hospital Admissions#
#' Function returning the number and % of presentations resulting in hospital admission for a given data subset
#' @param EDIS - a dataframe of the refined EDIS data, subset to the year currently of interest
#' @param FN - a dataframe of the refined FirstNet data, subset to the year currently of interest
#' @param category - a string of "NSLBP", "Radicular", "Sinister", or "All" to determine data subsetting
#' @param age - a string of "Young", "Old", or "All" to determine data subsetting
#' @return admit_summary - a string of the number and (%) of presentations that required hospital admission
hosp_admit_pres <- function(EDIS, FN, category, age) {
  
  #Data prep:
  prepped_data <- data_subset(EDIS = EDIS, FN = FN, category = category, age = age)
  fn_subset <- prepped_data[[1]]
  edis_subset <- prepped_data[[2]]
  
  #calculation of representations: n (%)
  count_admit <- sum(fn_subset$HospAdmit_Flag) + sum(edis_subset$HospAdmit_Flag)
  percent_admit <- 100*count_admit / (nrow(fn_subset) + nrow(edis_subset))
  
  #Format and return the result
  admit_summary <- paste0(count_admit, " (",round(percent_admit, 1), ")")
  return(admit_summary)
  
}


#Hospital LOS#
#' Function returning the median and IQR of hospital LOS for presentations admitted to hospital for a given subset
#' @param EDIS - a dataframe of the refined EDIS data, subset to the year currently of interest
#' @param FN - a dataframe of the refined FirstNet data, subset to the year currently of interest
#' @param category - a string of "NSLBP", "Radicular", "Sinister", or "All" to determine data subsetting
#' @param age - a string of "Young", "Old", or "All" to determine data subsetting
#' @return hosp_los_summary - a string of the median and (IQR) of hospital LOS for presentations admitted to hospital
hosp_los_pres <- function(EDIS, FN, category, age) {
  
  #Data prep:
  prepped_data <- data_subset(EDIS = EDIS, FN = FN, category = category, age = age)
  fn_subset <- prepped_data[[1]]
  edis_subset <- prepped_data[[2]]
  
  #calculation of median and IQR hospital LOS: median (IQR)
  hosp_los_quartiles <- quantile(x = as.numeric(fn_subset$INP.LOS.Days), #Only FirstNet
                               probs = c(0.25,0.5,0.75),
                               na.rm=T)
  
  #Format and return the result
  hosp_los_summary <- paste0(round(hosp_los_quartiles[2],1), 
                           " (",round(hosp_los_quartiles[1], 1),"-",
                           round(hosp_los_quartiles[3], 1),")")
  return(hosp_los_summary)
  
}

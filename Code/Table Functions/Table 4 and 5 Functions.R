#### Author: James Todd
#### Date: 2021/09/10
#### Last Updated: NA
#### Table 4 Function Definitions

## Purpose
# This file defines the functions used when constructing Table 4





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


#Pathology#
#' Function returning the number and % of presentations involving a pathology test
#' @param EDIS - a dataframe of the refined EDIS data, subset to the year currently of interest
#' @param FN - a dataframe of the refined FirstNet data, subset to the year currently of interest
#' @param category - a string of "NSLBP", "Radicular", "Sinister", or "All" to determine data subsetting
#' @param age - a string of "Young", "Old", or "All" to determine data subsetting
#' @return patho_summary - a string of the number and (%) of  presentations involving a pathology test
pathology_pres <- function(EDIS, FN, category, age) {
  
  #Data prep:
  prepped_data <- data_subset(EDIS = EDIS, FN = FN, category = category, age = age)
  fn_subset <- prepped_data[[1]]
  edis_subset <- prepped_data[[2]]
  
  #calculation of presentations involving a pathology test: n (%)
  count_patho <- sum(fn_subset$Pathology_Flag)
  percent_patho <- 100 * count_patho / (nrow(fn_subset)) #Only using FN
  
  #Format and return the result
  patho_summary <- paste0(count_patho, " (",round(percent_patho, 1), ")")
  return(patho_summary)
  
}


#Surgery#
#' Function returning the number and % of presentations involving surgery
#' @param EDIS - a dataframe of the refined EDIS data, subset to the year currently of interest
#' @param FN - a dataframe of the refined FirstNet data, subset to the year currently of interest
#' @param category - a string of "NSLBP", "Radicular", "Sinister", or "All" to determine data subsetting
#' @param age - a string of "Young", "Old", or "All" to determine data subsetting
#' @return surgery_summary - a string of the number and (%) of  presentations involving surgery
surgery_pres <- function(EDIS, FN, category, age) {
  
  #Data prep:
  prepped_data <- data_subset(EDIS = EDIS, FN = FN, category = category, age = age)
  fn_subset <- prepped_data[[1]]
  edis_subset <- prepped_data[[2]]
  
  #calculation of presentations involving surgery: n (%)
  count_surgery <- sum(fn_subset$Surgery_Flag)
  percent_surgery <- 100 * count_surgery / (nrow(fn_subset)) #Only using FN
  
  #Format and return the result
  surgery_summary <- paste0(count_surgery, " (",round(percent_surgery, 1), ")")
  return(surgery_summary)
  
}


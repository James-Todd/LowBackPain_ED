#### Author: James Todd
#### Date: 2021/09/06
#### Last Updated: 2021/09/10
#### Table 2 Function Definitions

## Purpose
# This file defines the functions used when constructing Table 2





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


#Medications#
#' Function returning the number and % of presentations involving specified medications
#' @param med_cat - a string of corresponding to a medication flag column in EDIS & FN
#' @param EDIS - a dataframe of the refined EDIS data, subset to the year currently of interest
#' @param FN - a dataframe of the refined FirstNet data, subset to the year currently of interest
#' @param category - a string of "NSLBP", "Radicular", "Sinister", or "All" to determine data subsetting
#' @param age - a string of "Young", "Old", or "All" to determine data subsetting
#' @return med_summary - a string of the number and (%) of  presentations involving specified medications
med_pres <- function(med_cat, EDIS, FN, category, age) {
  
  #Data prep:
  prepped_data <- data_subset(EDIS = EDIS, FN = FN, category = category, age = age)
  fn_subset <- prepped_data[[1]]
  edis_subset <- prepped_data[[2]]
  
  #Check the input medication category is appropriate:
  allowed_cats <- c("Analgesic", "Paracetamol", "NSAIDs","Opiate","OpiateComb",
                    "Benzo","Relaxant","Antiepileptic",
                    "Antidepressant","OralCortic","Antipsychotic")
  if(!(med_cat %in% allowed_cats)) {
    stop(paste0("med_cat must be one of the following: ", paste0(allowed_cats, collapse = ", ")))
  }
  
  #calculation of presentations involving specified medication type: n (%)
  count_med <- sum(fn_subset[,paste0(med_cat, "_Flag")])
  percent_med <- 100 * count_med / (nrow(fn_subset)) #Only using FN
  # percent_med <- 100 * count_med / (nrow(fn_subset) + nrow(edis_subset))
  
  #Format and return the result
  med_summary <- paste0(count_med, " (",round(percent_med, 1), ")")
  return(med_summary)
  
}


#### Author: James Todd
#### Date: 2021/09/10
#### Last Updated: NA
#### Table 3 Function Definitions

## Purpose
# This file defines the functions used when constructing Table 3





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


#Imaging#
#' Function returning the number and % of presentations involving a specified type of imaging
#' @param image_cat - a string of corresponding to an image flag column in EDIS & FN
#' @param EDIS - a dataframe of the refined EDIS data, subset to the year currently of interest
#' @param FN - a dataframe of the refined FirstNet data, subset to the year currently of interest
#' @param category - a string of "NSLBP", "Radicular", "Sinister", or "All" to determine data subsetting
#' @param age - a string of "Young", "Old", or "All" to determine data subsetting
#' @return image_summary - a string of the number and (%) of  presentations involving specified imaging
image_pres <- function(image_cat, EDIS, FN, category, age) {
  
  #Data prep:
  prepped_data <- data_subset(EDIS = EDIS, FN = FN, category = category, age = age)
  fn_subset <- prepped_data[[1]]
  edis_subset <- prepped_data[[2]]
  
  #Check the input image category is appropriate:
  allowed_cats <- c("CT", "MR", "XR", "Imaging")
  if(!(image_cat %in% allowed_cats)) {
    stop(paste0("image_cat must be one of the following: ", paste0(allowed_cats, collapse = ", ")))
  }
  
  #calculation of presentations involving specified image type: n (%)
  count_image <- sum(fn_subset[,paste0(image_cat, "_Flag")]) + sum(edis_subset[,paste0(image_cat, "_Flag")])
  percent_image <- 100 * count_image / (nrow(fn_subset) + nrow(edis_subset)) 
  
  #Format and return the result
  image_summary <- paste0(count_image, " (",round(percent_image, 1), ")")
  return(image_summary)
  
}


#Advanced Imaging#
#' Function returning the number and % of presentations involving advanced imaging (MR & CT scans)
#' @param EDIS - a dataframe of the refined EDIS data, subset to the year currently of interest
#' @param FN - a dataframe of the refined FirstNet data, subset to the year currently of interest
#' @param category - a string of "NSLBP", "Radicular", "Sinister", or "All" to determine data subsetting
#' @param age - a string of "Young", "Old", or "All" to determine data subsetting
#' @return advanced_summary - a string of the number and (%) of  presentations involving advanced imaging
advanced_image_pres <- function(EDIS, FN, category, age) {
  
  #Data prep:
  prepped_data <- data_subset(EDIS = EDIS, FN = FN, category = category, age = age)
  fn_subset <- prepped_data[[1]]
  edis_subset <- prepped_data[[2]]
  
  #calculation of presentations involving at least one advanced imaging: n (%)
  count_advanced <- sum(pmax(fn_subset$MR_Flag, fn_subset$CT_Flag))+
    sum(pmax(edis_subset$MR_Flag, edis_subset$CT_Flag))  
  percent_advanced <- 100 * count_advanced / (nrow(fn_subset)+nrow(edis_subset)) #Only using FN
  
  #Format and return the result
  advanced_summary <- paste0(count_advanced, " (",round(percent_advanced, 1), ")")
  return(advanced_summary)
  
}


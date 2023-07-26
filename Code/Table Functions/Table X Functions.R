#### Author: James Todd
#### Date: 2021/09/01
#### Last Updated: 2021/09/03
#### Table X Function Definitions

## Purpose
# This file defines the functions used when constructing Table X






#Total LBP presentations#
#' Function taking in EDIS and FirstNet data and returning the total number of LBP presentations
#' @param EDIS - a dataframe of the refined EDIS data, subset to the year currently of interest
#' @param FirstNet - a dataframe of the refined FirstNet data, subset to the year currently of interest
#' @return lbp_pres - an integer value representing the number of rows in both data sources
total_lbp_pres <- function(EDIS, FN) {
  lbp_pres <- nrow(EDIS) + nrow(FN)
  return(lbp_pres)
}


#Total Old LBP presentations#
#' Function taking in EDIS and FirstNet data and returning the number of older LBP presentations
#' @param EDIS - a dataframe of the refined EDIS data, subset to the year currently of interest
#' @param FirstNet - a dataframe of the refined FirstNet data, subset to the year currently of interest
#' @return count_old - a numeric of the number of LBP presentations >= 65
old_lbp_pres <- function(EDIS, FN) {
  
  #calculation of older presentations: n 
  count_old <- nrow(FN[FN$Age >= 65,]) + nrow(EDIS[EDIS$Age.at.Arrival >= 65,])
  return(count_old)
  
}


#Total Very Old LBP presentations#
#' Function taking in EDIS and FirstNet data and returning the number of much older LBP presentations
#' @param EDIS - a dataframe of the refined EDIS data, subset to the year currently of interest
#' @param FirstNet - a dataframe of the refined FirstNet data, subset to the year currently of interest
#' @return count_old - a numeric of the number of LBP presentations >= 85
very_old_lbp_pres <- function(EDIS, FN) {
  
  #calculation of older presentations: n 
  count_old <- nrow(FN[FN$Age >= 85,]) + nrow(EDIS[EDIS$Age.at.Arrival >= 85,])
  return(count_old)
  
}


#LBP Category Presentations#
#' Functions taking in EDIS and FirstNet data and returning the number of presentations associated with NSLBP, Radicular, or Sinister
#' @param EDIS - a dataframe of the EDIS data, subset to the year currently of interest
#' @param FirstNet - a dataframe of the FirstNet data, subset to the year currently of interest
#' @param category - a character string of "NSLBP", "Radicular", or "Sinister", corresponding to category of interest
#' @return category_pres - an integer value representing the number of presentations matching the current category
total_lbp_type <- function(EDIS, FN, category) {
  
  type_name <- paste0(category, "_Flag")
  category_pres <- sum(EDIS[,type_name]) + sum(FN[,type_name])
  return(category_pres)
  
}


#Ambulance Presentations#
#' Functions taking in EDIS and FirstNet data and returns the number of presentations that arrived via ambulance
#' @param EDIS - a dataframe of the EDIS data, subset to the year currently of interest
#' @param FirstNet - a dataframe of the FirstNet data, subset to the year currently of interest
#' @return amb_pres - an integer value representing the number of presentations by ambulance
total_amb_pres <- function(EDIS, FN) {
  
  amb_pres <- sum(EDIS[,"Ambulance_Flag"]) + sum(FN[,"Ambulance_Flag"])
  return(amb_pres)
  
}


#Representations#
#' Functions taking in EDIS and FirstNet data and returns the number of representations 
#' @param EDIS - a dataframe of the EDIS data, subset to the year currently of interest
#' @param FirstNet - a dataframe of the FirstNet data, subset to the year currently of interest
#' @return represent_count - an integer value representing the number of representations in 48 hours
total_represent <- function(EDIS, FN) {
  
  represent_count <- sum(EDIS[,"Represent_48hr"]=="Y") + sum(FN[,"Represent_48hr"]=="Y")
  return(represent_count)
  
}


#Physiotherapy Review#
#' Functions taking in EDIS and FirstNet data and returns the number of presentations involving physio review
#' @param EDIS - a dataframe of the EDIS data, subset to the year currently of interest
#' @param FirstNet - a dataframe of the FirstNet data, subset to the year currently of interest
#' @return num_physio - an integer value representing the number of presentations involving physio review
total_physio_review <- function(EDIS, FN) {
  
  num_physio <- sum(EDIS[,"Physio_Review"]) + sum(FN[,"Physio_Review"])
  return(num_physio)
  
}


#Total Imaging#
#' Functions taking in EDIS and FirstNet data and returns the number of presentations involving imaging
#' @param EDIS - a dataframe of the EDIS data, subset to the year currently of interest
#' @param FirstNet - a dataframe of the FirstNet data, subset to the year currently of interest
#' @return num_imaging - an integer value representing the number of presentations involving imaging
total_imaging <- function(EDIS, FN) {
  
  num_imaging <- sum(EDIS[,"Imaging_Flag"]) + sum(FN[,"Imaging_Flag"])
  return(num_imaging)
  
}


#Opioids#
#' Functions taking in EDIS and FirstNet data and returns the number of presentations involving opioids
#' @param EDIS - a dataframe of the EDIS data, subset to the year currently of interest
#' @param FirstNet - a dataframe of the FirstNet data, subset to the year currently of interest
#' @return num_opioids - an integer value representing the number of presentations involving opioids
total_opioids <- function(EDIS, FN) {
  
  num_opioids <- sum(EDIS[,"Opiate_Flag"]) + sum(FN[,"Opiate_Flag"])
  return(num_opioids)
  
}


#Surgery#
#' Functions taking in EDIS and FirstNet data and returns the number of presentations involving surgery
#' @param EDIS - a dataframe of the EDIS data, subset to the year currently of interest
#' @param FirstNet - a dataframe of the FirstNet data, subset to the year currently of interest
#' @return num_surgery - an integer value representing the number of presentations involving surgery
total_surgery <- function(EDIS, FN) {
  
  num_surgery <- sum(EDIS[,"Surgery_Flag"]) + sum(FN[,"Surgery_Flag"])
  return(num_surgery)
  
}


#Add Percentages#
#' Function adding percentages to Table X relative to total LBP ED presentations or all ED presentations
#' @param Table - Table X
#' @param row_name - row of Table to add percentages to
#' @param relative_row - row of Table percentages are relative to
#' @return Table - Table X with row_name values having percentages added
add_perc_table <- function(Table, row_name, relative_row) {
  
  #Extract rows needed for percentage
  cur_row <- as.numeric(Table[row_name,])
  rel_row <- as.numeric(Table[relative_row,])
  
  #Calculate percentage:
  perc_to_add <- round(100*cur_row / rel_row, 1)
  
  #Add to the row:
  Table[row_name,] <- paste0(cur_row, " (", perc_to_add, ")")
  
  #Return the table:
  return(Table)
  
  
}




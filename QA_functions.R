library(dplyr)

# QA Functions ----

#' Check field compliance 
#'
#' @param template data frame 
#' @param ncountryData data frame
#'
#' @return string with field differences
#' @export
#'
#' @examples
#' checkFieldsCompliantWithTemplate(df_population_template, df_population_country)
checkFieldsCompliantWithTemplate <- function(template, countryData){
  difference_a = setdiff(colnames(template), colnames(countryData))
  difference_b = setdiff(colnames(countryData), colnames(template))
  paste(difference_a,difference_b, sep=" ")
  return(paste("Non compliant columns", paste(difference_a,difference_b, sep=" "),sep=" "))
}
#' Check records compliance 
#'
#' @param template data frame
#' @param countryData data frame
#' @param joinfields vector
#'
#' @return data frame with non compliant records, if all records are compliant
#' returns empty data frame
#' @export
#'
#' @examples
#' checkRecordsCompliancy(df_population_template,df_population_country,
#' c("Platform", "Country", "Admin 1"))
checkRecordsCompliancy <- function(template, countryData, joinFields){
  anti_join_a = anti_join(template, countryData, joinFields)
  anti_join_a['Status'] <- "Admin1 missing from template"
  anti_join_b = anti_join(countryData, template, joinFields)
  anti_join_b['Status'] <- "Category not found in template, check spelling"
  reportFields = append(joinFields, 'Status')
  no_match = rbind(anti_join_a,anti_join_b)
  no_match = no_match[,reportFields]
  return (no_match)
}
#'Convert NA to Zero
#'
#' @param dataframe
#' Condition, all columns must be of number type
#' @return dataframe with NA values converted to Zero
#' @export
#'
#' @examples
#' naToZero(df_population_country)
naToZero <- function (x) {
  return(x %>% replace(is.na(.), 0))
}

#'Check if stored number is round
#'
#' @param dataframe
#' Condition, all columns must be of number type
#' @return dataframe with flagged non round numbers
#' @export
#'
#' @examples
#' IsRound_numData_pop_country <- data.frame(lapply( numData_pop_country, isWholeNumber))
isWholeNumber <- function(x) {ifelse(floor(x)==x,"","Review")}


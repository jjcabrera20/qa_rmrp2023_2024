library(dplyr)
#library(tidyr)
#library(tidyverse)

checkNumberOfRecords <- function(template, countrydata, joinFields){
  anti_join_a = anti_join(template, countrydata, joinFields)
  anti_join_b = anti_join(countrydata, template, joinFields)
  no_match = rbind(anti_join_a,anti_join_b)
  return (no_match)
}

naToZero <- function (x) {
  return(x %>% replace(is.na(.), 0))
}

isWholeNumber <- function(x) {ifelse(floor(x)==x,"","Review")}

# isWholeNumber <- function(x) {
#   return(all(floor(x)==x))
# }
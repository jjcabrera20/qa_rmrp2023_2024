library(tidyverse)
library(dplyr)
library(tidyr)
checkNumberOfRecords <- function(template, countrydata, joinFields){
  anti_join_a = anti_join(template, countrydata, joinFields)
  anti_join_b = anti_join(countrydata, template, joinFields)
  no_match = rbind(anti_join_a,anti_join_b)
  return (no_match)
}

naToZero <- function (x) {
  return(mutate_all(x,~replace_na(.,0)))
}

isWholeNumber <- function(x) {
  x= select_if(x , is.numeric)
  return(all(floor(x)==x))
}
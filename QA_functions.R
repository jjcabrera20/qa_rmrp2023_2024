library(dplyr)

# QA Functions ----

checkNumberOfRecords <- function(template, countrydata, joinFields){
  anti_join_a = anti_join(template, countrydata, joinFields)
  anti_join_a['Status'] <- "Admin1 missing from template"
  anti_join_b = anti_join(countrydata, template, joinFields)
  anti_join_b['Status'] <- "Category not found in template, check spelling"
  reportFields = append(joinFields, 'Status')
  no_match = rbind(anti_join_a,anti_join_b)
  no_match = no_match[,reportFields]
  return (no_match)
}

naToZero <- function (x) {
  return(x %>% replace(is.na(.), 0))
}

isWholeNumber <- function(x) {ifelse(floor(x)==x,"","Review")}


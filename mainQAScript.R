rm(list = ls())
library(tidyr)
library(dplyr)
library(readxl)
library(writexl)
source("QA_functions.R")
# Global variables setup
COUNTRY="Ecuador"
BASE_PATH = "C:/Users/jorcabrera/Downloads"
POPULATION_PROJECTION_TEMPLATE_FILE = "Template_Population_projections_2023-24 (1).xlsx"

POPULATION_PROJECTION_COUNTRY_FILE = "Ecuador_pop_projection.xlsx"

# Import excel files population projections TEMPLATES
POPULATION_PROJECTION_FILE_PATH = paste(BASE_PATH, POPULATION_PROJECTION_TEMPLATE_FILE, sep = "/")
df_population_template = read_excel(POPULATION_PROJECTION_FILE_PATH)
df_population_template = filter(df_population_template, df_population_template$Country==COUNTRY)
# Import excel files population projections files
POPULATION_PROJECTION_COUNTRY_PATH = paste(BASE_PATH, POPULATION_PROJECTION_COUNTRY_FILE, sep = "/")
df_population_country = read_excel(POPULATION_PROJECTION_COUNTRY_PATH)
df_population_country = dplyr::mutate(df_population_country, ID = row_number())
# INTEGRITY ----
integrityCheck<-checkFieldsCompliantWithTemplate(df_population_template, df_population_country)

# COMPLETENESS ---- 
df_population_country = filter(df_population_country, df_population_country$Country==COUNTRY)
df_completeness = checkRecordsCompliancy(df_population_template,df_population_country, c("Platform", "Country", "Admin 1"))
if(nrow(df_completeness) > 0){
  print("Data not compliant with template, check Platform, Country and Admin1 names")
  write_xlsx(df_completeness,paste(BASE_PATH,paste("Admin1NoCompliant_",POPULATION_PROJECTION_COUNTRY_FILE),sep="/"))
}
df_population_country <- naToZero(df_population_country)

# CONFORMITY ----
#Get columns with numeric data
numCols_pop_country<-names(df_population_country)[sapply(df_population_country, is.numeric)]

numData_pop_country<- df_population_country %>% select(all_of(numCols_pop_country))

IsRound_numData_pop_country <- data.frame(lapply( numData_pop_country, isWholeNumber))

# CONSISTENCY ----
# Horizontal in destination ----
IN_DESTINATION_FIELDS = c("ID",
                          "Girls  In Destination",
                          "Boys In Destination",
                          "Women In Destination",
                          "Men In Destination",
                          "Total 2023 In Destination")

Total2023InDestination <- select(numData_pop_country, IN_DESTINATION_FIELDS )%>%
  rowwise()%>%
  mutate(SUMXX = `Girls  In Destination` +
           `Boys In Destination`+
           `Women In Destination`+
           `Men In Destination`) %>% ungroup() %>%
  mutate(TotalInDestinationSum = ifelse(SUMXX == `Total 2023 In Destination`, "", "Review"))%>%
  select(TotalInDestinationSum)
# Horizontal host community ----
HOST_COMMUNITY_FIELDS = c("ID",
                          "Girls  Host Community",
                          "Boys Host Community",
                          "Women Host Community",
                          "Men Host Community",
                          "Total 2023 Host Community")
Total2023HostComunity <- select(numData_pop_country, HOST_COMMUNITY_FIELDS )%>%
  rowwise()%>%
  mutate(SUMXX = `Girls  Host Community`+
           `Boys Host Community`+
           `Women Host Community`+
           `Men Host Community`) %>% ungroup() %>%
  mutate(TotalInHostCommunitySum = ifelse(SUMXX == `Total 2023 Host Community`, "", "Review"))%>%
  select(TotalInHostCommunitySum)

# Vertical in destination ----

df_verticalTest <- df_population_country %>% rowwise()%>% mutate(Admin0 = ifelse(`Admin 1` == 'Country level', "Country level",`Country` )) %>% ungroup()

# Vertical Dec 2022 in Destination ----

summary_DEC2022Destination <- df_verticalTest %>% group_by(Admin0) %>%  summarise(Freq = sum(`Dec 2022 Population projection In Destination`)) %>% ungroup() %>%
  spread(Admin0, Freq) %>% mutate_if(is.numeric, round, digits=3) %>% ungroup() %>%
rowwise() %>% mutate(checkXX = ifelse(`Country level` == as.name(COUNTRY),"OK","Review")) %>% ungroup() %>%
gather(key="year", value = "val", -checkXX)

# Vertical Dec 2023 in Destination ----
summary_DEC2023Destination <- df_verticalTest %>% group_by(Admin0) %>%  summarise(Freq = sum(`Total 2023 In Destination`)) %>% ungroup() %>%
  spread(Admin0, Freq) %>% mutate_if(is.numeric, round, digits=3) %>% ungroup() %>%
  rowwise() %>% mutate(checkXX = ifelse(`Country level` == as.name(COUNTRY),"OK","Review")) %>% ungroup() %>%
  gather(key="year", value = "val", -checkXX)


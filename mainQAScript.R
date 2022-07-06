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

df_completeness = checkNumberOfRecords(df_population_template,df_population_country, c("Platform", "Country", "Admin 1"))
if(nrow(df_completeness) > 0){
  print("Data not compliant with template, check Platform, Country and Admin1 names")

  write_xlsx(df_completeness,paste(BASE_PATH,paste("Admin1NoCompliant_",POPULATION_PROJECTION_COUNTRY_FILE),sep="/"))
}
df_population_country <- naToZero(df_population_country)
sapply(df_population_country, class)
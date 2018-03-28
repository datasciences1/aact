########################################################
## 
## parse ? website
## 
########################################################
library(rvest)
# https://clinicaltrials.gov/ct2/show?cond=cancer&rank=7
# https://clinicaltrials.gov/ct2/show?rank=1
#########################################################################################
# Retrieve the first page in order to get the number of studies
file <- "https://clinicaltrials.gov/ct2/show?rank=1"
page <- read_html(file)

# Number of pages to retrieve
code <- html_nodes(page,xpath = '//*[@class="results-summary"]') 
n <- html_text(code, trim = TRUE)
start <- gregexpr(pattern ='of',n)
end <- gregexpr(pattern ='for',n)

nb <- as.numeric(substr(n,start[[1]][1]+3, end[[1]][1]-1))

# List of websites to scrap
# boucle de 1 a nb ? stocker dans un tableau

code <- html_nodes(page,xpath = '//*[@id="sponsor"]')
sponsor <- html_text(code, trim = TRUE)

code <- html_nodes(page,xpath = '//*[@class="body3 indent2"]')
desc <- html_text(code, trim = TRUE)

code <- html_nodes(page,xpath = '//*[@headers="studyInfoColData"]')
design <- html_text(code, trim = TRUE)

########################################################
## 
## parse json file: Open FDA org
## 
########################################################  
#install.packages("rjson")
suppressWarnings(library("rjson"))

setwd("D:/4. learning/05. STAT/04. DS-Training/1. Project/")
json_file <- "drug-event-0008-of-0008.json"
json_data <- fromJSON(paste(readLines(json_file), collapse=""))



## Number of event in the file
length(json_data$results)

## Number of drug per event
# for(i in 1:length(json_data$results)) print(json_data$results[[i]]$patient$drug)

product  <- rep( list(list()), length(json_data$results) )
reaction <- rep( list(list()), length(json_data$results) )
weight   <- rep( list(list()), length(json_data$results) )
sex      <- rep( list(list()), length(json_data$results) )
company  <- rep( list(list()), length(json_data$results) )
#name <- list()
#subst <- list()
## Number of sponsor per drug per event
for(i in 1:length(json_data$results)){
  for (j in 1:length(json_data$results[[i]]$patient$reaction)){
    reaction[[i]][j] <- json_data$results[[i]]$patient$reaction[[j]]$reactionmeddrapt
    weight[[i]][j]   <- json_data$results[[i]]$patient$patientweight
    sex[[i]][j]      <- json_data$results[[i]]$patient$patientsex
  }
  for (k in 1:length(json_data$results[[i]]$patient$drug))      product[[i]][k] <- json_data$results[[i]]$patient$drug[[k]]$medicinalproduct
}


########################################################
## 
## AACT clinical data base query
## 
########################################################

#install.packages("RPostgreSQL")
suppressWarnings(library(RPostgreSQL))
drv <- dbDriver('PostgreSQL')
con <- dbConnect(drv, dbname="aact",host="aact-db.ctti-clinicaltrials.org", port=5432, user="aact", password="aact" )
aact_sample <- dbGetQuery(con, "select * from studies where nct_id='NCT00920790'")


dbGetQuery(con, "select nct_id, study_type, official_title, overall_status, phase, source, has_dmc from studies where lower(source) like '%sanofi%'")
dbGetQuery(con, "select count(*) from studies")
dbGetQuery(con, "select nct_id, title, units, param_type, param_value, dispersion_type from outcome_measurements where nct_id='NCT00920790'")


#From txt file uploaded
setwd('E:/4. learning/05. STAT/04. DS-Training/1. Project/database/20170811_pipe-delimited-export')

suppressWarnings(library(data.table))

for(i in 1:length(list.files())){
  assign(substr(list.files()[1],1,nchar(list.files()[1])-4), fread(list.files()[i]))
  
}

# database creation
library(RSQLite)
db <- dbConnect(SQLite(), dbname="essai.sqlite")

result_contacts <- fread("result_contacts.txt")
dbWriteTable(conn = db, name = "result_contacts", value = result_contacts, row.names = FALSE, header = TRUE)


#for(i in 1:2){
#  assign(substr(list.files()[1],1,nchar(list.files()[1])-4), fread(list.files()[i]))  
#}




########################################################
## 
## Kaggle AE 
## 
########################################################


setwd("E:/4. learning/05. STAT/04. DS-Training/1. Project/adverse-pharmaceuticals-events/aeolus_v1")
require(data.table)
library(sqldf)
concept <- fread("concept.tsv")
voca <- fread("vocabulary.tsv")
voca

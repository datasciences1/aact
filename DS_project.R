########################################################
## 
## parse à website
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
# boucle de 1 a nb à stocker dans un tableau

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
## Kaggle AE 
## 
########################################################


setwd("E:/4. learning/05. STAT/04. DS-Training/1. Project/adverse-pharmaceuticals-events/aeolus_v1")
require(data.table)
library(sqldf)
concept <- fread("concept.tsv")
voca <- fread("vocabulary.tsv")
voca

################################################################################################################################################################################################################################
################################################################################################################################################################################################################################

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

#***************************************************************************************************************************************************
#***************************************************************************************************************************************************
#***************************************************************************************************************************************************
#***************************************************************************************************************************************************

#From txt file uploaded
setwd('D:/4. learning/05. STAT/04. DS-Training/0. Project/database/20170811_pipe-delimited-export')
#setwd('E:/4. learning/05. STAT/04. DS-Training/0. Project/database/20170811_pipe-delimited-export')

suppressWarnings(library(data.table))
suppressWarnings(library(readxl))

file <- "0.summary.xlsx"
tbl <- read_excel(path=file, col_names= TRUE, na="", n_max= Inf)

aact <- data.frame(name = tbl[which(tbl$Status=='Y'),]$Name,column = tbl[which(tbl$Status=='Y'),]$Columns)



for(i in 1:nrow(aact)){
      nm    <- paste("'",aact$name[i],"'",sep="")
      input <- paste("'",aact$name[i],".txt'",sep="")
      xpr   <- paste("c('",gsub(",","','",gsub(" ","",aact$column[i])),"')",sep="")
      nm   <- eval(parse(text=nm))
      input<- eval(parse(text=input))
      col   <- eval(parse(text=xpr))
      
      assign(nm, fread(input, header=TRUE, select= col ))
}

#for(i in 1:length(list.files())){
#  assign(substr(list.files()[i],1,nchar(list.files()[i])-4), fread(list.files()[i]))  
#}

########################################################
## SQLLite DB creation
########################################################

# creation and connection to the sqlite db
library(sqldf)
db <- dbConnect(SQLite(), dbname="aact.sqlite")

#dbSendQuery(conn = db, "CREATE TABLE baseline_counts(nct_id TEXT, count INTEGER)")
dbWriteTable(conn = db, name = "baseline_counts", value = baseline_counts, row.names = FALSE)
dbWriteTable(conn = db, name = "baseline_measurements", value = baseline_measurements, row.names = FALSE)
dbWriteTable(conn = db, name = "calculated_values", value = calculated_values, row.names = FALSE)
dbWriteTable(conn = db, name = "conditions", value = conditions, row.names = FALSE)
dbWriteTable(conn = db, name = "countries", value = countries, row.names = FALSE)

dbWriteTable(conn = db, name = "design_group_interventions", value = design_group_interventions, row.names = FALSE)
dbWriteTable(conn = db, name = "design_groups", value = design_groups, row.names = FALSE)
dbWriteTable(conn = db, name = "designs", value = designs, row.names = FALSE)

dbWriteTable(conn = db, name = "drop_withdrawals", value = drop_withdrawals, row.names = FALSE)
dbWriteTable(conn = db, name = "eligibilities", value = eligibilities, row.names = FALSE)

dbWriteTable(conn = db, name = "intervention_other_names", value = intervention_other_names, row.names = FALSE)
dbWriteTable(conn = db, name = "interventions", value = interventions, row.names = FALSE)

dbWriteTable(conn = db, name = "milestones", value = milestones, row.names = FALSE)
dbWriteTable(conn = db, name = "outcome_counts", value = outcome_counts, row.names = FALSE)

dbWriteTable(conn = db, name = "outcome_measurements", value = outcome_measurements, row.names = FALSE)
dbWriteTable(conn = db, name = "outcomes", value = outcomes, row.names = FALSE)

dbWriteTable(conn = db, name = "reported_events", value = reported_events, row.names = FALSE)
dbWriteTable(conn = db, name = "result_groups", value = result_groups, row.names = FALSE)

dbWriteTable(conn = db, name = "sponsors", value = sponsors, row.names = FALSE)
dbWriteTable(conn = db, name = "studies", value = studies, row.names = FALSE)


## Display on a map
# install.packages("ggmap")
# install.packages("maptools")
# install.packages("maps")

suppressWarnings(library(ggmap))
suppressWarnings(library(maptools))
suppressWarnings(library(ggplot2))
suppressWarnings(library(maps))
suppressWarnings(library(sqldf))

# Regroup countries

cntrs <- sqldf("select nct_id, 
                       case when name in ('American Samoa ','Guam','Northern Mariana Islands','United States','United States Minor Outlying Islands','Virgin Islands (U.S.)') then 'USA'
                            when name in ('Aruba','Netherlands Antilles')                                                                                                     then 'Netherland'
                            when name in ('Bermuda','Cayman Islands','Gibraltar','Montserrat','United Kingdom')                                                               then 'UK'
                            when name in ('Congo, The Democratic Republic of the','The Democratic Republic of the Congo')                                                     then 'Congo'
                            when name in ('Czechia','Czech Republic')                                                                                                         then 'Congo'
                            when name in ('Faroe Islands','Greenland')                                                                                                        then 'Denmark'
                            when name in ('French Polynesia','Guadeloupe','Martinique','New Caledonia','RÃ©union')                                                            then 'France'
                            when name in ('Holy See (Vatican City State)')                                                                                                    then 'Vatican'
                            when name in ('Hong Kong','Macau','Taiwan')                                                                                                       then 'China'
                            when name like '%Korea%'                                                                                                                          then 'North Korea'
                            when name in ('Former Yugoslavia and Macedonia','The Former Yugoslav Republic of')                                                                then 'Macedonia'
                            when name in ('Federated States of Micronesia','Nauru','Palau')                                                                                   then 'Macedonia'
                            when name in ('Palestinian Territories','Occupied and Palestinian Territory, Occupied')                                                           then 'Palestine'
                            when name in ('Syrian Arab Republic')                                                                                                             then 'Syria'
                            when name in ('Russian Federation')                                                                                                               then 'Russia'
                            else name end as country
                from countries ")



mdat <- map_data('world')
ct <- as.data.frame(table(cntrs$country))
colnames(ct) <- c("country", "value")

mpdt <- sqldf("select m.region, avg(m.lat) as lat, avg(m.long) as long from mdat as m group by region")

dat <- sqldf("select c.*, m.lat, m.long from ct as c left join mpdt as m on c.country=m.region ")

ggplot() + 
  geom_polygon(dat=mdat, aes(long, lat, group=group), fill="blue") +
  geom_point(data=dat, 
             aes(y=lat,x=long, map_id=country, size=value), col="red")


##############################################################################################
##############################################################################################

map("world", fill=TRUE, col="white", bg="lightblue", ylim=c(-120, 120), mar=c(0,0,0,0)) 

points(ct$Var1[1], cex = ct$Freq[1], pch = 20)


## start a model based on 5 variables (sponsor, nb on countries, in criteria, ex criteria, therapeutic area)

#derive in study ==> variable outcome as such

# 3 - completed	    ["Completed", "Approved for marketing"]
# 2 - ongoing		    ["Recruiting", "Not yet recruiting", "Active, not recruiting","Enrolling by invitation","Suspended"]
# 1 - unknown       ["Unknown status", "Available","No longer available","Temporarily not available"]
# 0 - not completed	["Terminated", "Withdrawn"]

suppressWarnings(library(sqldf))

trial <- sqldf("select nct_id, study_type, overall_status, phase, number_of_arms, has_dmc,
                       case when overall_status in ('Terminated', 'Withdrawn') then 0
                            when overall_status in ('Approved for marketing','Completed') then 1
                            when overall_status in ('Recruiting', 'Not yet recruiting', 'Active, not recruiting','Enrolling by invitation','Suspended') then 2
                            when overall_status in ('Unknown status', 'Available','No longer available','Temporarily not available','') then 3 end as outcome,

                       case when study_type in ('Expanded Access') then 1
                            when study_type in ('Interventional') then 2
                            when study_type in ('Observational', 'Observational [Patient Registry]') then 3
                            when study_type in ('Unknown','') then 4 end as type
                from studies ")

region <- sqldf("select nct_id, count(*) as nb_rg from cntrs group by nct_id")
trial <- sqldf("select t.*, r.nb_rg from trial as t left join region as r on t.nct_id = r.nct_id ")

ae <- sqldf("select nct_id, count(*) as nb_ae from reported_events group by nct_id")
trial <- sqldf("select t.*, a.nb_ae from trial as t left join ae as a on t.nct_id = a.nct_id ")

trial <- sqldf("select * 
                from trial 
                where outcome is not null and nb_rg is not null and nb_ae is not null and outcome in (0,1)")


# studies
# nct_id, study_type, overall_status, phase, number_of_arms, has_dmc
table(trial$outcome,useNA = "always")
table(trial$type,useNA = "always")



# Calculation nb of inclusion criteria and exclusion criteria
temp <- strsplit(eligibilities$criteria,split = 'Exclusion Criteria:')



set.seed(123)
library(caret)
trainnew<-createDataPartition(y=trial$outcome,p = .8,list = F)
rms_train <- trial[trainnew,] #80% training set
rms_test <- trial[-trainnew,] #20% test set

dim(rms_train); dim(rms_test)

## Logistic Regression
rms_fit <- glm(formula =outcome~type+nb_rg+nb_ae,data=rms_train)
summary(rms_fit)


## Random Forest
library(randomForest)
set.seed(123)
model.rf <- randomForest(y=rms_train$outcome,x=rms_train[,c("type","nb_rg","nb_ae")],importance=T,keep.forest = T)
plot(model.rf)
legend("topright",colnames(model.rf$err.rate),col=1:3,fill=1:3)
varImpPlot(model.rf,scale = F)
pred_cart<-predict(model.rf,rms_train)
c<-confusionMatrix(table(pred_cart,rms_train$outcome))
c
acc<-round(c$overall[[1]]*100,2)



test_set$Survived<-predict(model.rf,test_set)










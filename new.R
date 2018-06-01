#From txt file uploaded
setwd('C:/Users/Aswen/Desktop/20180314_pipe-delimited-export')
#setwd('E:/4. learning/05. STAT/04. DS-Training/0. Project/database/20170811_pipe-delimited-export')
install.packages("data.table")
library(data.table)
suppressWarnings(library(data.table))
install.packages("readxl")
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
install.packages("sqldf")
library(sqldf)

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


## ici il veut faire une map avec les pays
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

## Ali code
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

# Mouna code of database 
# par rappor au code d'ali j'ai rajouté mes variables d'interet et j'ai mis 
# recruiting dans la catégorie terminated et withdrawn car pour moi ce sont des etudes qui recrutent
# mais pas complètes
trialbase <- sqldf("select s.nct_id, s.study_type, s.overall_status, s.phase, s.number_of_arms, s.has_dmc, s.enrollment, s.number_of_arms, s.has_expanded_access,
                  c.number_of_facilities, c.number_of_nsae_subjects, c.number_of_sae_subjects, C.actual_duration, c.were_results_reported, 
                  C.minimum_age_num, C.maximum_age_num, c.minimum_age_unit, c.maximum_age_unit,
                  case when overall_status in ('Recruiting','Terminated', 'Withdrawn') then 0
                  when overall_status in ('Approved for marketing','Completed') then 1
                  when overall_status in ('Not yet recruiting', 'Active, not recruiting','Enrolling by invitation','Suspended') then 2
                  when overall_status in ('Unknown status', 'Available','No longer available','Temporarily not available','') then 3 end as outcome,
                  case when study_type in ('Expanded Access') then 1
                  when study_type in ('Interventional') then 2
                  when study_type in ('Observational', 'Observational [Patient Registry]') then 3
                  when study_type in ('Unknown','') then 4 end as type
                  from studies s ,calculated_values c
                  where s.nct_id=c.nct_id")

#comme Ali je rajoute ae et region
region <- sqldf("select nct_id, count(*) as nb_rg from cntrs group by nct_id")
trialbase<- sqldf("select t.*, r.nb_rg from trialbase as t left join region as r on t.nct_id = r.nct_id ")

ae <- sqldf("select nct_id, count(*) as nb_ae from reported_events group by nct_id")
trialbase <- sqldf("select t.*, a.nb_ae from trialbase as t left join ae as a on t.nct_id = a.nct_id ")

# d'adbord je garde que les 0,1 var ex
trialbase1 <- sqldf("select * 
               from trialbase
                where outcome in (0,1)")

table(trialbase1$phase,useNA = "always")## je vais supprimer les etudes 
#qui n'ont pas indiqué leur phase soit 98807 lignes

trialbase1$phase <- as.character(trialbase1$phase)

trialbase2<-trialbase1[trialbase1$phase!="N/A",]
        
summary(trialbase2$enrollment) # il y a une valeur abérrante à enelever vu en max
boxplot(trialbase2$enrollment)# visualisation valeur abérrantte
# on supprime les 2 valeurs observées très loin des autres
trialbase2<-trialbase2[trialbase2$enrollment!=99999999,]
trialbase2<-trialbase2[trialbase2$enrollment!=720000.0,]
summary(trialbase2$enrollment)# les données manquantes représentes 2.6 % des donnees
boxplot(trialbase2$enrollment)

table(trialbase2$number_of_arms,useNA = "always") ##12 % de données manquantes à voir
table(trialbase2$has_dmc,useNA = "always")##         f     t      <NA> 
                                            ## 23732 47937 37331  2640 
table(trialbase2$has_expanded_access,useNA = "always") 
#         f      t   <NA> 
#  2725 106041    234   2640 
summary (trialbase2$number_of_facilities)
boxplot(trialbase2$number_of_facilities)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   1.00    1.00    1.00   12.44    6.00 1746.00   12802 
summary(trialbase2$number_of_nsae_subjects)# trop de manquant à voir
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.0    14.0   107.0   608.6   423.0 79635.0   89843 
summary(trialbase2$number_of_sae_subjects)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0.0     0.0     8.0   112.1    43.0 73542.0   89843 
summary(trialbase2$actual_duration)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  0.00    9.00   20.00   27.28   37.00  452.00   32080 
summary(trialbase2$maximum_age_num)
##Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   1.00   45.00   65.00   59.83   75.00 6569.00   53517
summary(trialbase2$nb_ae)
##Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##    2.0    12.0    44.0   151.8   130.0 12803.0   89843 
summary(trialbase2$nb_rg)
##Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##  1.000   1.000   1.000   2.073   1.000  58.000    9255 

# imputation des données manquantes pour has_dmc
table(trialbase2$has_dmc,useNA = "always")##         f     t      <NA> 
                                            ## 23732 47937 37331  2640 
trialbase2$has_dmc <-  as.character(trialbase2$has_dmc)
trialbase2[trialbase2$has_dmc %in% c(""),'has_dmc']="f"
table(trialbase2$has_dmc,useNA = "always")
trialbase2<-trialbase2[!is.na(trialbase2$has_dmc),] ## suppression des lignes manquantes
dim(trialbase2)

table(trialbase2$has_expanded_access,useNA = "always") ## cela a supprimé aussi les données manquantes
trialbase2[trialbase2$has_expanded_access %in% c(""),'has_expanded_access']="f"

##gestion des ages
## je decide de supprimer les colonnes maximum_age_unit et maximum_age_num 
## car beaucoup de données manquantes et peu d'interet
summary(trialbase2$maximum_age_num)# je vais finalement supprimer l'age max 
trialbase2$maximum_age_num<- NULL
trialbase2$maximum_age_unit<- NULL

table(trialbase2$minimum_age_unit)
## je fais des changements sur le units afin que

#       Days  Month Months  Weeks   Year  Years 
#5227    138    214   1685    319    754 100663 

#        Days  Month Months  Weeks  Years 
#5227    138    214   1685    319 101417
? Date
## je recode les unités 
trialbase2$minimum_age_unit[trialbase2$minimum_age_unit %in% c("Year")]<-"Years"
trialbase2$minimum_age_unit[trialbase2$minimum_age_unit %in% c("Month")]<-"Months"


install.packages("date")
library(date)
## conversion (voir comment faire commande ci dessous ne marche pas)

trialbase2$minimum_age_num <-if (trialbase2$minimum_age_unit %in% c("Months","Days","Weeks")){
  year(trialbase2$minimum_age_num)
} else {
  trialbase2$minimum_age_num
}


#imputation donneés manquantes numériques 
install.packages("mice")
library(mice)


imp_data <- mice(trialbase2)
## trop long avec maxit =50 seed 500, mice(data = trialbase2, m = 5, method = "pmm", maxit = 50, seed = 500)
imp_data
test<-complete (imp_data,1)
summary(test$enrollment)
hist(test$enrollment,probability = TRUE)
summary (test$minimum_age_num)
summary (test$number_of_arms)
summary(test$nb_ae)
table(test$has_dmc,useNA = "always")
table(test$has_expanded_access,useNA = "always")


#test Mouna avec imputation données manquantes reg log
reg_log<- glm(formula =outcome~type+nb_rg+nb_ae +phase + number_of_arms + enrollment
               + number_of_facilities + number_of_nsae_subjects + number_of_sae_subjects +actual_duration +
                 + were_results_reported, data=test)


#code Ali
# studies
# nct_id, study_type, overall_status, phase, number_of_arms, has_dmc
table(trial$outcome,useNA = "always")
table(trial$type,useNA = "always")



# Calculation nb of inclusion criteria and exclusion criteria
#temp <- strsplit(eligibilities$criteria,split = 'Exclusion Criteria:')



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
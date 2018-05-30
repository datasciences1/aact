library(data.table)
library(dplyr)
library(plyr)
baseline_counts <- fread("C:/Users/mohamed/Downloads/20180301_clinical_trials/Data/selection/baseline_counts.txt", stringsAsFactors = T, sep="|")
baseline_counts <- fread("C:/Users/pr0osenc09/Downloads/20180301_clinical_trials/Data/selection/baseline_counts.txt", stringsAsFactors = T, sep="|")
baseline_counts <- fread("D:/Users/pr0osenc09/Downloads/20180301_clinical_trials/Data/selection/baseline_counts.txt", stringsAsFactors = T, sep="|")
baseline_measurements <- fread("D:/Users/pr0osenc09/Downloads/20180301_clinical_trials/Data/selection/baseline_measurements.txt", stringsAsFactors = T, sep="|")
baseline_measurements[ ,c("nct_id","result_group_id","category","title","units","param_type","param_value_num","dispersion_type","dispersion_value_num","dispersion_lower_limit","dispersion_upper_limit")]
calculated_values <- fread("D:/Users/pr0osenc09/Downloads/20180301_clinical_trials/Data/selection/calculated_values.txt", stringsAsFactors = T, sep="|")
conditions<- fread("D:/Users/pr0osenc09/Downloads/20180301_clinical_trials/Data/selection/conditions.txt", stringsAsFactors = T, sep="|")
countries<- fread("D:/Users/pr0osenc09/Downloads/20180301_clinical_trials/Data/selection/countries.txt", stringsAsFactors = T, sep="|")
design_group_interventions<- fread("D:/Users/pr0osenc09/Downloads/20180301_clinical_trials/Data/selection/design_group_interventions.txt", stringsAsFactors = T, sep="|")
design_groups<- fread("D:/Users/pr0osenc09/Downloads/20180301_clinical_trials/Data/selection/design_groups.txt", stringsAsFactors = T, sep="|")
designs<- fread("D:/Users/pr0osenc09/Downloads/20180301_clinical_trials/Data/selection/designs.txt", stringsAsFactors = T, sep="|")
drop_withdrawals <- fread("D:/Users/pr0osenc09/Downloads/20180301_clinical_trials/Data/selection/drop_withdrawals.txt", stringsAsFactors = T, sep="|")
eligibilities <- fread("D:/Users/pr0osenc09/Downloads/20180301_clinical_trials/Data/selection/eligibilities.txt", stringsAsFactors = T, sep="|")
intervention_other_names <- fread("D:/Users/pr0osenc09/Downloads/20180301_clinical_trials/Data/selection/intervention_other_names.txt", stringsAsFactors = T, sep="|")
#intervention_other_names[ ,c("nct_id","name")]
interventions <- fread("D:/Users/pr0osenc09/Downloads/20180301_clinical_trials/Data/selection/interventions.txt", stringsAsFactors = T, sep="|")
#interventions[ ,c("nct_id","intervention_type")]
milestones<- fread("D:/Users/pr0osenc09/Downloads/20180301_clinical_trials/Data/selection/milestones.txt", stringsAsFactors = T, sep="|")
#milestones[ ,c("nct_id","result_group_id","title","count")]
outcome_counts<- fread("D:/Users/pr0osenc09/Downloads/20180301_clinical_trials/Data/selection/outcome_counts.txt", stringsAsFactors = T, sep="|")
#outcome_counts[ ,c("nct_id","outcome_id","result_group_id","scope","units","count")]
outcome_measurements <- fread("D:/Users/pr0osenc09/Downloads/20180301_clinical_trials/Data/selection/outcome_measurements.txt", stringsAsFactors = T, sep="|")
#outcome_measurements[ ,c("nct_id","outcome_id","result_group_id","classification","title","units","param_type","param_value_num","dispersion_type","dispersion_value_num","dispersion_lower_limit","dispersion_upper_limit")]
outcomes <- fread("D:/Users/pr0osenc09/Downloads/20180301_clinical_trials/Data/selection/outcomes.txt", stringsAsFactors = T, sep="|")
#outcomes[ ,c("nct_id","outcome_type")]
result_groups <- fread("D:/Users/pr0osenc09/Downloads/20180301_clinical_trials/Data/selection/result_groups.txt", stringsAsFactors = T, sep="|")
#result_groups[ ,c("id","nct_id","result_type")]
sponsors <- fread("D:/Users/pr0osenc09/Downloads/20180301_clinical_trials/Data/selection/sponsors.txt", stringsAsFactors = T, sep="|")
#sponsors[ ,c("nct_id","agency_class","lead_or_collaborator","name")]
studies <- fread("D:/Users/pr0osenc09/Downloads/20180301_clinical_trials/Data/selection/studies.txt", stringsAsFactors = T, sep="|")
#studies[ ,c("nct_id","study_first_submitted_date","results_first_submitted_date","last_update_submitted_date","study_first_submitted_qc_date","study_first_posted_date","results_first_submitted_qc_date","results_first_posted_date","last_update_submitted_qc_date","	last_update_posted_date","start_date","completion_date","target_duration","study_type","acronym","brief_title","overall_status","last_known_status","phase","enrollment","number_of_arms","number_of_groups","why_stopped","has_expanded_access","has_dmc")]
reported_events<- fread("D:/Users/pr0osenc09/Downloads/20180301_clinical_trials/Data/selection/reported_events.txt", stringsAsFactors = T, sep="|")
studies <- studies[, c("nct_id","nlm_download_date_description","study_first_posted_date_type", "last_update_posted_date_type" , "brief_title", "overall_status","source","limitations_and_caveats","number_of_arms","number_of_groups" ,"why_stopped","has_expanded_access","expanded_access_type_individual","expanded_access_type_intermediate","expanded_access_type_treatment","has_dmc","is_fda_regulated_drug","is_fda_regulated_device","is_unapproved_device","is_ppsd","is_us_export","biospec_retention","biospec_description","plan_to_share_ipd","plan_to_share_ipd_description")]
studies <- studies[, c("nct_id","nlm_download_date_description","study_first_posted_date_type", "last_update_posted_date_type" , "brief_title", "overall_status","source","limitations_and_caveats","number_of_arms","number_of_groups" ,"why_stopped","has_expanded_access","expanded_access_type_individual","expanded_access_type_intermediate","expanded_access_type_treatment","has_dmc","is_fda_regulated_drug","is_fda_regulated_device","is_unapproved_device","is_ppsd","is_us_export","biospec_retention","biospec_description","plan_to_share_ipd","plan_to_share_ipd_description")]
nctId_subjects_affected <- aggregate(subjects_affected ~ nct_id, data=reported_events, FUN=sum)
nctId_subjects_at_risk <- aggregate(subjects_at_risk ~ nct_id, data=reported_events, FUN=sum)
nctId_event_count <- aggregate(event_count ~ nct_id, data=reported_events, FUN=sum)
nctId_frequency_threshold <- aggregate(frequency_threshold ~ nct_id, data=reported_events, FUN=sum)
studies <- join(x=studies , y=nctId_subjects_affected , by="nct_id" , type="left", match="first")
studies <- join(x=studies , y=nctId_subjects_at_risk , by="nct_id" , type="left", match="first")
studies <- join(x=studies , y=nctId_event_count , by="nct_id" , type="left", match="first")
studies <- join(x=studies , y=nctId_frequency_threshold , by="nct_id" , type="left", match="first")
remove(nctId_frequency_threshold)
remove(nctId_subjects_affected)
remove(nctId_subjects_at_risk)
remove(nctId_event_count)
remove(reported_events)
studies <- join(x=studies , y=countries[,c(2,3)] , by="nct_id" , type="left", match="first")
studies <- join(x=studies , y=calculated_values[,c(2,3,4,5,11,12)] , by="nct_id" , type="left", match="first")
studies <- join(x=studies , y=designs , by="nct_id" , type="left", match="first")
#baseline_measurements	 	<- fread("baseline_measurements.txt")
#baseline_counts	         	<- fread("baseline_counts.txt")
new_DF <- studies[!(is.na(studies$event_count)),]
new_DF <- studies[!(is.na(studies$frequency_threshold)),]
new_DF <- studies[!(is.na(studies$subjects_affected)),]
new_DF <- studies[!(is.na(studies$subjects_at_risk)),]
new_DF <- join(x=new_DF , y=countries[,c(2,3)] , by="nct_id" , type="left", match="first")
new_DF$outcomes_assessor_masked <- ifelse(new_DF$outcomes_assessor_masked !="t","f", "t")
new_DF$overall_status_1 <- ifelse(new_DF$overall_status== "Completed", 1, 0)
new_DF$last_known_status <- ifelse(new_DF$last_known_status== "", "Not Available", new_DF$last_known_status)
new_DF$phase <- ifelse(new_DF$phase== "", "Not Available", new_DF$phase)
new_DF$has_dmc <- ifelse(new_DF$has_dmc== "", "f", new_DF$has_dmc)
new_DF$is_fda_regulated_drug <- ifelse(new_DF$is_fda_regulated_drug== "", "f", new_DF$is_fda_regulated_drug)
new_DF$is_fda_regulated_device <- ifelse(new_DF$is_fda_regulated_device== "", "f", new_DF$is_fda_regulated_device)
new_DF$name <- ifelse(is.na(new_DF$name), "Not Available", new_DF$name)
new_DF$overall_status_1 <- as.factor(new_DF$overall_status_1)
new_DF$study_first_posted_date_type <- as.factor(new_DF$study_first_posted_date_type)
new_DF$study_last_posted_date_type <- as.factor(new_DF$study_last_posted_date_type)
new_DF$overall_status <- as.factor(new_DF$overall_status)
new_DF$name <- as.factor(new_DF$name)
new_DF$has_expanded_access <- as.factor(new_DF$has_expanded_access)
new_DF$has_dmc <- as.factor(new_DF$has_dmc)
new_DF$has_us_facility <- as.factor(new_DF$has_us_facility)
new_DF$has_single_facility <- as.factor(new_DF$has_single_facility)
new_DF2 <- new_DF[, c("nct_id","study_first_posted_date_type", "study_last_posted_date_type", "overall_status", "name", "has_expanded_access", "has_dmc","has_us_facility","has_single_facility","number_of_arms", "number_of_groups", "why_stopped", "is_fda_regulated_device", "is_unapproved_device", "is_ppsd", "is_us_export", "subjects_affected", "subjects_at_risk","name","frequency_threshold","number_of_facilities","number_of_nsae_subjects","number_of_sae_subjects","overall_status")]
new_DF2$has_dmc <- new_DF$has_dmc
new_DF2 <- new_DF2[, c(1,7,8,9,10,11,12,13,14,15,16,17,19,20,21,22,23,24)]

new_DF2$outcomes_assessor_masked <- ifelse(new_DF2$outcomes_assessor_masked !="t", "f", "t")
new_DF2[new_DF2$gender=="",'gender']=NA
new_DF2$overall_status_1 <- ifelse(new_DF2$overall_status== "Completed", 1, 0)
new_DF2$last_known_status <- as.character(new_DF2$last_known_status)
new_DF2[new_DF2$last_known_status =="",'last_known_status']="Not Available"
new_DF2$last_known_status <- as.factor(new_DF2$last_known_status)
new_DF2$phase <- as.character(new_DF2$phase)
new_DF2[new_DF2$phase=="",'phase']="Not Available"
new_DF2$phase <- as.factor(new_DF2$phase)
new_DF2[new_DF2$has_expanded_access =="",'has_expanded_access']="f"
new_DF2[new_DF2$has_dmc  =="",'has_dmc']="f

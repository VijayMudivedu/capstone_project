# ---
# title: "Hospital Ratings CMS"
# author: "Vijay Mudivedu"
# date: '2018-10-28'

#install.packages("mice")
#install.packages("psych")
#install.packages("doParallel")
#install.packages("cowplot")
#install.packages("tidyverse")
#install.packages("randomForest")
#install.packages("caret")

library(tidyverse)
library(mice)
library(psych)
library(cowplot)
library(randomForest)
library(caret)
library(doParallel)

#setwd("~/Google Drive/_OneDrive_Atimi_Software/Upgrad/_Upgrad/Capstone_project/")

##########################
# General Information
##########################

general_info <- read.csv(file = "ValidFiles/Hospital General Information.csv",header = T,check.names = T,na.strings = c("Not Available",""),
                         stringsAsFactors = T)


#- Filtering the demographic variables, "Hospital.Name","Address","City","State","County.Name","Phone.Number","ZIP.Code" not needed for analysis
#- Filtertime the redundant variables that do not contribute for analysis

zdemographics <- c("Hospital.Name","Address","City","State","County.Name","Phone.Number","ZIP.Code")
zdemogrphic_vars <- which(names(general_info) %in% zdemographics)

zvar1 <- c("Hospital.Type","Hospital.Ownership","Emergency.Services","Meets.criteria.for.meaningful.use.of.EHRs")
zvar2 <- which(names(general_info) %in% zvar1)
general_info_cleaned <- general_info[,-c(zdemogrphic_vars,zvar2)]


# There are missing ratings in the target variable "Hospital.Overall.Rating". Replacing the empty values in the target variable as "Missing"
general_info_cleaned$Hospital.overall.rating[which(is.na(general_info_cleaned$Hospital.overall.rating))] <- "Missing"
# Converting the targe variable to factor
general_info_cleaned$Hospital.overall.rating <- as.factor(general_info_cleaned$Hospital.overall.rating)


## Check if the NAs in the Ratings are missing at Random in the Footnote

# * footnote with levels are inducing the NAs in the dataset: 
#     + "Data are shown only for hospitals that participate in the Inpatient Quality Reporting (IQR) and Outpatient Quality Reporting (OQR) programs" is inducing the NAs across the dataset
#     + "Data suppressed by CMS for one or more quarters"
# * This concludes that the provider ids related this level can be purged from the dataset
# 
# * Similarly, "Data suppressed by CMS for one or more quarters" also cannot be used for analysis as this is inducing the NAs across the datasets, which mandates to purge the dataset with missing NAs

general_info_cleaned %>% filter(general_info_cleaned$Hospital.overall.rating.footnote %in%
           c("Data are shown only for hospitals that participate in the Inpatient Quality Reporting (IQR) and Outpatient Quality Reporting (OQR) programs",
             "Data suppressed by CMS for one or more quarters","Data suppressed by CMS for one or more quarters","Results are not available for this reporting period")) %>% group_by(Mortality.national.comparison,Readmission.national.comparison,Safety.of.care.national.comparison,Efficient.use.of.medical.imaging.national.comparison,Timeliness.of.care.national.comparison,Effectiveness.of.care.national.comparison,Patient.experience.national.comparison) %>%
  summarise(count_rws = n()) %>% t()


# * Filtering the footnotes that are leading to NAs Hospital Ratings

general_info_cleaned <- general_info_cleaned %>% 
  filter(!general_info_cleaned$Hospital.overall.rating.footnote %in% 
           c("Data are shown only for hospitals that participate in the Inpatient Quality Reporting (IQR) and Outpatient Quality Reporting (OQR) programs",
             "Data suppressed by CMS for one or more quarters","Results are not available for this reporting period"))


# * Barplot of the ratings show that the Hospital rating has a Gaussian distribution

ggplot(data = general_info_cleaned,aes(x = Hospital.overall.rating)) + 
  geom_bar(na.rm = T, fill = "steelblue") + geom_text(stat = "count", aes(label = paste0(round(..count../sum(..count..),3)*100)),hjust = 0) + 
  labs(title = "% Distrbution of the Class Variable, Hospital Ratings ") + 
  theme(plot.title = element_text(hjust = 0.5),plot.background = element_blank(),axis.title.y = element_blank(),
        panel.background = element_blank(),axis.text.y = element_text(hjust = 1)) + coord_flip()


# * checking the level footnote level of Hospital Rating once again.
#   - "There are too few measures or measure groups reported to calculate a star rating or measure group score"
#   - "Results are not available for this reporting period"
# 

general_info_cleaned %>% 
  filter(general_info_cleaned$Hospital.overall.rating.footnote %in% 
           c("There are too few measures or measure groups reported to calculate a star rating or measure group score")) %>% summary()

# * The above specific additional footnotes also contribute to the NAs in Hospital Overall Rating with multilevel dependency on the other groups. 
# * For example, specified ones below contribue less to NAs, compared to Mortality, Safety of care, Readmission, Efficient Use of medical Imaging.
#   + "Patient.experience" , "Effectiveness.of.care", "Timeliness.of.care"
# 

general_info_cleaned <- general_info_cleaned %>% 
  filter(!general_info_cleaned$Hospital.overall.rating.footnote %in% 
           c("There are too few measures or measure groups reported to calculate a star rating or measure group score")) 

summary(general_info_cleaned)

### General Info csv file conclusions
# 1. We thus have conclusively arrived at the "general_info_final" dataset with 1-5 hospital ratings imputing the NAs
# 2. Picking the hospital Rating from the general info cleaned dataset 

z_rem_var1 <- which(names(general_info_cleaned) %in% c("Provider.ID","Hospital.overall.rating"))
general_info_final <- general_info_cleaned[is.na(general_info_cleaned$Hospital.overall.rating.footnote),z_rem_var1]
summary(general_info_final)
dim(general_info_final)

####################################
# Analysing the Complications 
####################################

complications_df <- read.csv(file = "ValidFiles//Complications - Hospital.csv",
                             header = T,check.names = T,stringsAsFactors = T,na.strings = c('Not Available',""))
head(complications_df)

#  Cleaning the demographic variables that do not have an impact on the Measure IDs

zdemographics <- c("Hospital.Name","Address","City","State","ZIP.Code","County.Name","Phone.Number","Measure.Start.Date", "Measure.End.Date")
zdemogrphic_vars <- which(names(complications_df) %in% zdemographics)
complications_df_cleaned <- complications_df[,-zdemogrphic_vars]
head(complications_df_cleaned)

# - Measuring the distribution of NAs in the Complications dataset, Compared to National, and Footnote variables.
# - About 35% of the NAs in Compared to National, and 41% of the records in footnote is stale due to reasons specified in the variable.

round(prop.table(summary(factor(complications_df_cleaned$Compared.to.National)))*100,2)  #- Thus NAs are 35% in the Compared.To.National variable
round(prop.table(summary(factor(complications_df_cleaned$Footnote)))*100,2)  #- NAs are 65% in the Footnote variable 

### imputation of NAs in Complications dataset
# - Analysis of "footnote" variable by checking the impact on the score variable
# Distribution of Score and Denominator by Footnote

complications_df_cleaned %>% group_by(Footnote) %>% 
  summarise(cnt_rows = n(),Avg_Score = mean(Score,na.rm = T),Total_Score = sum(Score,na.rm = T)) %>% arrange(Avg_Score)

# - Footnote Variable: Footnote levels which have specific reasons do not contribute to score variable for the specific Provided.ID,
# these rows can be imputed from analysis.

zvar1 <- is.na(complications_df$Footnote) # Blank Footnotes
zvar2 <- which(names(complications_df_cleaned) %in% "Footnote")
complications_df_cleaned_footnote_nas <- complications_df_cleaned[zvar1,-zvar2]
summary(complications_df_cleaned_footnote_nas)

# - Imputing the "NAs" resulted in inducing the variability in each of the variables. 
# which variable is contributing most NAs  in "Score" variable?

library(reshape2)

complications_df_cleaned_footnote_nas %>% group_by(Measure.ID) %>% 
  summarise(cnt_rws = n(), mean_Score = round(mean(Score,na.rm = T))) %>% 
  arrange(desc(cnt_rws)) %>% melt(value.name = c("value")) %>%
  ggplot(aes(x = Measure.ID,y = value)) + geom_col(fill = "steelblue") + geom_text(aes(label = value),hjust = 1,vjust = 0.5,angle = 90) +
  facet_wrap(facets = ~ variable,scales = "free",ncol = 3) +  labs(title = "Complications measure") +
  theme(axis.text.x = element_text(angle = 60,vjust = 1,hjust = 1), plot.title = element_text(hjust = 0.5))


# - Significant Measures from this dataset measures are "PSI_90_SAFETY","PSI_4_SURG_COMP" "COMP_HIP_KNEE"
# - rejecting the Denominator, Lower Estimate, Higher Estimate, Compared.to.National variables

zvar1 <- c("Provider.ID", "Measure.ID", "Score")
zvar2 <- which(names(complications_df_cleaned_footnote_nas) %in% zvar1)

complications_df_final <-  complications_df_cleaned_footnote_nas[,zvar2] %>%  spread(key = Measure.ID,value = Score)
summary(complications_df_final)
# PSI_8_POST_HIP data is stale with no contribution to the overall score and  minimal.
complications_df_final <- complications_df_final[,-which(names(complications_df_final) %in% "PSI_8_POST_HIP")]

# - Some of the variables exhibit that high variability compared to other measures are for example, "PSI_90_SAFETY","COMP_HIP_KNEE", "PSI_4_SURG_COMP" 
# - Considering the signficant variable measures from these Complications Dataset

zvar1 <- which(names(complications_df_final) %in% c("Provider.ID","PSI_90_SAFETY","COMP_HIP_KNEE","PSI_4_SURG_COMP"))
complications_df_final <- complications_df_final[,zvar1]
library(psych)
pairs.panels(x = complications_df_final[2:ncol(complications_df_final)], main = "Complications dataset correlation ",
             bg = rainbow(n = 12),smooth = TRUE, ellipses = TRUE,pch = 21,cex.cor = 0.85,cex.labels = 0.6)


########################################################################
## Analysis for Healthcare Associated Infections - For Hosptial
########################################################################

hai_df <- read.csv(file = "~/Google Drive/_OneDrive_Atimi_Software/Upgrad/_Upgrad/Capstone_project/capstone_project/ValidFiles/Healthcare Associated Infections - Hospital.csv",na.strings = c("Not Available",""))


# - Removing the demographic variables, Address, City, State, ZIP.Code, County.Name,Phone.Number,Footnote

zdemographics <- c("Hospital.Name","Address","City","State","County.Name","Phone.Number","ZIP.Code","Measure.Start.Date","Measure.End.Date")
hai_df_cleaned <- hai_df[,-which(names(hai_df) %in% c(zdemographics))]

# - Imputing the NAs from the Hospital Associated Infections dataset - Variables, Footnote with values

hai_df_cleaned %>% group_by(Footnote) %>% summarise(count_rows = n(),score_total = sum(Score,na.rm = T))

# - Footnote variable that briefs out the imperfections associated with the score for each related level
# - Considering the blank in the footnotes that carry more weightage to the score

zvar1 <- which(names(hai_df_cleaned) %in% c("Footnote"))
hai_df_cleaned <- hai_df_cleaned[is.na(hai_df_cleaned$Footnote),-zvar1]  
summary(hai_df_cleaned) # running the summary on the after removing the invalid footnotes

# determine the outliers in the Score

hai_df_cleaned %>% group_by(Measure.ID) %>% summarise(count_rows = n(), per_mean_score = mean(Score)*100) %>%
ggplot(aes(x = Measure.ID,y = count_rows)) +
  geom_col(fill = "steelblue") + geom_text(aes(label = count_rows),vjust = -0.5,angle = 0) +
  xlab("Measure.ID") + labs(title = "Healthcare Associated Infections") +
  theme(axis.text.x = element_text(angle = 45,hjust = 1,vjust = 1),plot.title = element_text(hjust = 0.5))

# * There are abnormal outliers in the dataset from the measure IDs HAI_1_DOPC_Days, besides this they do not contribute to the overall score
# - Summarising the distribution of the measures. 

# - A central line-associated bloodstream infection (CLABSI) is a serious infection that occurs when germs (usually bacteria or viruses) enter the bloodstream through the central line. Healthcare providers must follow a strict protocol when inserting the line to make sure the line remains sterile and a CLABSI does not occur. 

# -Clostridium difficile (C. difficile) is a bacteria that causes diarrhea and can lead to serious complications. Those at highest risk for C. difficile infection include people who take antibiotics and also receive care in any medical setting, including hospitals. C. difficile bacteria produce spores that can be spread from patient to patient.

# - CAUTI -Catheter Associated Urinary Tract Infections: A urinary tract infection (UTI) is an infection involving any part of the urinary system, including urethra, bladder, ureters, and kidney. UTIs are the most common type of healthcare-associated infection reported to the National Healthcare Safety Network (NHSN).  

# - Considering the SIRs, that Centre for Diesease Control and Prevention uses to calculate, Standard Infection Ratio (SIR) which takes into account patient care location, number of patients with an exisiting infection, lab mehtords, bed size, afficialiton with a medical schools, bed size of the hospital, age of patients.
# 
# - central line-associated bloodstream infections (CLABSI), catheter- associated urinary tract infections (CAUTI), surgical site infection (SSI) from colon surgery or abdominal hysterectomy, methicillin-resistant Staphylococcus Aureus (MRSA) blood laboratory-identified events (bloodstream infections), and Clostridium difficile (C.diff.) laboratory-identified events (intestinal infections). 

# The HAI measures show how often patients in a particular hospital contract certain infections during the couse of their medical treatment:
# HAI_1_SIR, HAI_1a_SIR, HAI_2_SIR, HAI_2a_SIR, HAI_6_SIR,HAI_4_SIR, HAI_5_SIR, HAI_3_SIR
 
# HAI-1 measure tracks central-line associated bloodstream infections (CLABSI) in ICUs and select wards. 
# HAI-2 measure tracks catheter-associated urinary tract infections (CAUTI) in ICUs and select wards. 
# HAI-3 Surgical Site Infection from colon surgery (SSI: Colon)
# HAI-4 Surgical Site Infection from abdominal hysterectomy (SSI: Hysterectomy)
# HAI-5 Methicillin-resistant Staphylococcus Aureus (MRSA) Blood Laboratory-identified Events (Bloodstream infections)
# HAI-6 Clostridium difficile (C.diff.) Laboratory-identified Events (Intestinal infections)


hai_measures <- c("HAI_1_SIR", "HAI_2_SIR", "HAI_3_SIR", "HAI_4_SIR", "HAI_5_SIR", "HAI_6_SIR")
# Filtering the measure.ids useful for analysis
hai_df_cleaned <- hai_df_cleaned[which(hai_df_cleaned$Measure.ID %in% hai_measures),]

# Plotting the measures required for the analysis

ggplot(data = hai_df_cleaned,aes(x = Measure.ID)) + 
  geom_bar(fill = "steelblue",na.rm = TRUE) + xlab("Measure.ID") + 
  geom_text(stat = "count",aes(label = ..count..),vjust = -.1) +
  labs(title = "Hospital Associated Infections by Measure by Hospital Count") +
  theme(axis.text.x = element_text(angle = 45,vjust = 1,hjust = 1))

# Selecting the required variables for analysis
zvar1 <- c( "Provider.ID", "Measure.ID", "Score")
zvar2 <- which(names(hai_df_cleaned) %in% zvar1)

# converting the measure variables from long format to wide format
hai_df_final <- hai_df_cleaned[,zvar2] %>% spread(key = "Measure.ID",value = "Score")
summary(hai_df_final)


# Checking the correlation between the variables. 
pairs.panels(x = hai_df_final[,-1],cex.cor = 0.5,cex.labels = 0.9,ellipses = TRUE,pch = 21,bg = rainbow(length(zvar1)), 
             main = "Hospital Patient Safety Group variable correlation" )

# - From the correlation plot HAI_1, HAI_2, HAI_3, HAI_4, HAI_5 measures have some degree of correlation between them


#----- Merging dataframes--------
# Merging the complications_df final with general information final together w.r.t to the "Provider.ID" gives rise to "Saftey of Care Group"


gen_inf_compli_df <- merge(x = general_info_final,y = complications_df_final,all = TRUE, by = intersect(x = names(general_info_final),y = names(complications_df_final)))

safety_of_care_group <- merge(x = gen_inf_compli_df,y = hai_df_final,all = TRUE,by = intersect(x = names(gen_inf_compli_df),y = names(hai_df_final)))
head(safety_of_care_group)


####################################
# Analysis of HCAPHS dataset
####################################

hcaphs_df <- read.csv(file = "ValidFiles/HCAHPS - Hospital.csv",header = TRUE,check.names = TRUE,na.strings = c("Not Available","Not Applicable",""))
head(hcaphs_df)

# Removing the demographic columns, Mesaure Start Date and measure end data columns
hcaphs_df_cleaned <- hcaphs_df[,-which(names(hcaphs_df) %in% zdemographics)]
head(hcaphs_df_cleaned)

# - Extracting the Measure IDs that are of importance from:
# H_COMP_1_LINEAR_SCORE, H_COMP_2_LINEAR_SCORE, H_COMP_3_LINEAR_SCORE, H_COMP_4_LINEAR_SCORE, H_COMP_5_LINEAR_SCORE, 
# H_COMP_6_LINEAR_SCORE, H_COMP_7_LINEAR_SCORE, H_HSP_RATING_LINEAR_SCORE, H_QUIET_LINEAR_SCORE,H_RECMND_LINEAR_SCORE, H_CLEAN_LINEAR_SCORE, 
# 
# - Selecting the required measures

hcaphs_measures <- c("H_COMP_1_LINEAR_SCORE", "H_COMP_2_LINEAR_SCORE", "H_COMP_3_LINEAR_SCORE", "H_COMP_4_LINEAR_SCORE", "H_COMP_5_LINEAR_SCORE", 
"H_COMP_6_LINEAR_SCORE", "H_COMP_7_LINEAR_SCORE", "H_HSP_RATING_LINEAR_SCORE", "H_QUIET_LINEAR_SCORE","H_RECMND_LINEAR_SCORE", "H_CLEAN_LINEAR_SCORE")

hcaphs_df_measures <- hcaphs_df_cleaned %>% filter(hcaphs_df_cleaned$HCAHPS.Measure.ID %in% hcaphs_measures)
hcaphs_df_measures$HCAHPS.Measure.ID <- as.character(hcaphs_df_measures$HCAHPS.Measure.ID)
hcaphs_df_measures$HCAHPS.Measure.ID <- str_replace(string = hcaphs_df_measures$HCAHPS.Measure.ID,pattern = "_LINEAR_SCORE",replacement = "")
hcaphs_df_measures$HCAHPS.Measure.ID <- as.factor(hcaphs_df_measures$HCAHPS.Measure.ID)

head(hcaphs_df_measures)


# - Selecting only the important Variables 

zdistri_vars <- c("Provider.ID","HCAHPS.Measure.ID","HCAHPS.Linear.Mean.Value","Number.of.Completed.Surveys",
                  "Survey.Response.Rate.Percent.Footnote","Number.of.Completed.Surveys.Footnote")

hcaphs_df_measures_distri <- hcaphs_df_measures[,zdistri_vars] 


# - Eliminating the rows that have NA scores  1. too few cases to report, 2. Results are unavailable for the current reporting period 3. Data Specific to IQR and OQR

hcaphs_df_measures_distri[is.na(hcaphs_df_measures_distri$Survey.Response.Rate.Percent.Footnote),] %>% summary()
hcaphs_df_measures_distri <- hcaphs_df_measures_distri[is.na(hcaphs_df_measures_distri$Survey.Response.Rate.Percent.Footnote),c(1,2,3)]


# * creating the patient experience group and spreading the measure ids
patient_exp_group <- hcaphs_df_measures_distri %>% spread(key = HCAHPS.Measure.ID, value = HCAHPS.Linear.Mean.Value)
head(patient_exp_group)

# - Checking the correlation between the Patient Experience Measure Variables for each hosptial
pairs.panels(patient_exp_group[,-1],cex.labels = 0.6 ,cex.cor = 0.7,main = "Patient Experience Relationship between measures")

# - Patient experience measures are uniformly distributed about their mean. Each of the measureshave a strong correlation between them.
#    +  Cleanliness of Hospital Environment (Q8)  - H_Clean_HSP
#    +  Nurse Communication (Q1, Q2, Q3)          - H_COMP_1
#    +  Doctor Communication (Q5, Q6, Q7)         _ H_COMP_2
#    +  Responsiveness of Hospital Staff (Q4, Q11)- H_COMP_3 
#    +  Pain management (Q13, Q14)                - H_COMP_4
#    +  Communication About Medicines (Q16, Q17)  - H_COMP_5
#    +  Discharge Information (Q19, Q20)          - H_COMP_6
#    +  Overall Rating of Hospital (Q21)          - H_OVERALL_RATING
#    +  Quietness of Hospital Environment (Q9)    - H_QUIET_HSP
#    +  Willingness to Recommend Hospital (Q22)   - H_RECMND
#    +  HCAHPS 3 Item Care Transition Measure (CTM-3) - - H_COMP_7 

#----- Merging dataframes--------
# - Merging the patient experience group dataframe with the master dataframe

master_df  <- merge(x = safety_of_care_group,y = patient_exp_group,by = intersect(names(safety_of_care_group),names(patient_exp_group)),all = TRUE)
head(master_df)


########################################################################
# Analysis of Timeliness and Effectiveness of Care Dataset
########################################################################

time_and_eff_care_df <- read.csv(file = "ValidFiles/Timely and Effective Care - Hospital.csv",header = T,check.names = T,stringsAsFactors = T,na.strings = c("Not Available",""))
head(time_and_eff_care_df)

# Cleaning the demographic variables from the dataset

zdemogrphic_vars <- which(names(time_and_eff_care_df) %in% zdemographics)
time_and_eff_care_cleaned <- time_and_eff_care_df[,-zdemogrphic_vars]
head(time_and_eff_care_cleaned)


# * NA Imputation in the Score Variable
# * Considering the Footnote levels as a reference to impute the NAs, the levels below satisfy the non-stale data category.
#  + 1. Blank Levels (NA) 2.  "2 - Data submitted were based on a sample of cases/patients."

time_and_eff_care_cleaned %>% group_by(Footnote) %>% summary(cnt_rows=n())

# - The footnote level "2 - Data submitted were based on a sample of cases/patients." has valid data the and this affects the overall score of the patients
# - So keeping the Footnote level and removing the dataset that have NAs

time_and_eff_care_cleaned$Score <- as.integer(time_and_eff_care_cleaned$Score)
time_and_eff_care_cleaned <- time_and_eff_care_cleaned[is.na(time_and_eff_care_cleaned$Footnote) | (time_and_eff_care_cleaned$Footnote %in% c("2 - Data submitted were based on a sample of cases/patients.")),] 
time_and_eff_care_cleaned %>% summary()


# * Subsetting the data by removing the NA from "Samples" variable

time_and_eff_care_cleaned <- time_and_eff_care_cleaned[!is.na(time_and_eff_care_cleaned$Sample),]

# - Performing the validation checks after imputing the NAs in the dataset

# Samples Vs Score

time_and_eff_care_cleaned %>% group_by(Measure.ID) %>% summarise(mean_score = round(mean(Score))) %>%
ggplot(aes(Measure.ID,mean_score)) + geom_col(fill = "steelblue") + 
  geom_text(aes(label = mean_score),vjust = 0.5,angle = 90,hjust = 1) +
  labs(title = "Measure.ID Vs Scores") +
  theme(axis.text.x = element_text(angle = 60,vjust = 1,hjust = 1),
        plot.title = element_text(hjust = 0.5))


# - AMI_71, AMI7b though have high mean scores, there aren't many providers


ggplot(time_and_eff_care_cleaned, aes(Measure.ID)) + geom_bar(fill = "steelblue") + 
  geom_text(stat = "count",aes(label = ..count..),vjust = 0.5,angle = 90,hjust = 0) +
  labs(title = "Timeliness and Effectiveness Measure.ID Vs Count of Providers") +
  theme(axis.text.x = element_text(angle = 45,vjust = 1,hjust = 1),
        plot.title = element_text(hjust = 0.5))


# * Measures that contribute lessser to the overall score and that are have lesser providers ids are eliminated from the analysis
# Grouping the Effectiveness Measures and Timeliness Measures:
# - *Timeliness Group*: ED_1b,ED_2b,OP_18b, OP_3, OP_5, OP_20,OP_21
# ED-1b 	Median Time from ED Arrival to ED Departure for Admitted ED Patients 
# ED-2b 	Admit Decision Time to ED Departure Time for Admitted Patients 
# OP-3  	Median Time to Transfer to Another Facility for Acute Coronary Intervention 
# OP-5 	  Median Time to ECG 
# OP-18b 	Median Time from ED Arrival to ED Departure for Discharged ED Patients 
# OP-20 	Door to Diagnostic Evaluation by a Qualified Medical Professional 
# OP-21 	ED-Median Time to Pain Management for Long Bone Fracture 
# 
# - *Effectiveness Group*: CAC_3,IMM_2,IMM_3_OP_27_FAC_ADHPCT,OP_22,OP_23,OP_29,OP_30,OP_4,PC_01,STK_1, STK_4, STK_6,STK_8,VTE_1,VTE_2,VTE_3,VTE_5, VTE_6
# CAC-3 	Home Management Plan of Care (HMPC) Document Given to Patient/Caregiver 
# IMM-2 	Influenza Immunization 
# IMM-3/OP-27 	Healthcare Personnel Influenza Vaccination 
# OP-4 	  Aspirin at Arrival 
# OP-22 	ED-Patient Left Without Being Seen 
# OP-23 	ED-Head CT or MRI Scan Results for Acute Ischemic Stroke or Hemorrhagic Stroke who Received Head CT or MRI Scan Interpretation Within 45 Minutes of Arrival 
# OP-29 	Endoscopy/Polyp Surveillance: Appropriate Follow-Up Interval for Normal Colonoscopy in Average Risk Patients 
# OP-30 	Endoscopy/Polyp Surveillance: Colonoscopy Interval for Patients with a History of Adenomatous Polyps – Avoidance of Inappropriate Use 
# PC-01 	Elective Delivery Prior to 39 Completed Weeks Gestation: Percentage of Babies Electively Delivered Prior to 39 Completed Weeks Gestation 
# STK-1 	Venous Thromboembolism (VTE) Prophylaxis 
# STK-4 	Thrombolytic Therapy 
# STK-6 	Discharged on Statin Medication 
# STK-8 	Stroke Education 
# VTE-3 	Venous Thromboembolism Patients with Anticoagulation Overlap Therapy 
# VTE-1 	Venous Thromboembolism Prophylaxis 
# VTE-2 	Intensive Care Unit Venous Thromboembolism Prophylaxis 
# VTE-5 	Venous Thromboembolism Warfarin Therapy Discharge Instructions 
# VTE-6 	Hospital Acquired Potentially-Preventable Venous Thromboembolism 

 
# * Separating the the measure variables needed for analysis
zvar1 <-  which(names(time_and_eff_care_cleaned) %in% c("Provider.ID","Measure.ID","Score"))
timeliness_group <- time_and_eff_care_cleaned[,zvar1] %>% spread(key = Measure.ID,value = Score)
zvar1 <- which(names(timeliness_group) %in% c("Provider.ID","ED_1b","ED_2b","OP_18b", "OP_3b", "OP_5", "OP_20","OP_21"))
zvar2 <- which(names(timeliness_group) %in% c("Provider.ID","CAC_3","IMM_2","IMM_3_OP_27_FAC_ADHPCT","OP_22","OP_23","OP_29","OP_30","OP_4","PC_01","STK_1", "STK_4", "STK_6","STK_8","VTE_1","VTE_2","VTE_3","VTE_5", "VTE_6"))

effectiveness_group <- timeliness_group[,zvar2]
timeliness_group <- timeliness_group[,zvar1]


# * check the correlation between the measure variables

pairs.panels(x = timeliness_group[2:length(zvar1)],cex.cor = 0.5,cex.labels = 0.9,ellipses = TRUE,pch = 21,bg = rainbow(length(zvar1)), main = "Timeliness Group variable correlation" )

# - most of the measure variables of timeliness group are uncorrelated.
# - ED_1B and ED_2B are the inversely correlated to some extent as the timespent in the emergency room is the common between them. They are inversely correlated as the doctors visit reduces the time spend by the patient before moving to ED to inpatient room is reduced.

# renmaing the measures "IMM_3_OP_27"
zvar1 <- which(names(effectiveness_group) %in% "IMM_3_OP_27_FAC_ADHPCT")
names(effectiveness_group)[zvar1] <- "IMM_3_OP_27" 

# - Plotting the Correlation between the effectiveness group measures

cor.plot(effectiveness_group[,-1],
        stars = FALSE,numbers = TRUE,colors = TRUE,cex = 0.5, show.legend = FALSE,
         xlas = 2,cex.axis = 0.6,main = "Effectiveness group measure correlation")

# - Variables represented by the effectiveness group have small degree of correlation
# - Merging the timliness group and effectiveness group with master dataframe


#----- Merging dataframes--------
# merging master dataframe with timliness group
master_df <- merge(x = master_df,y = timeliness_group,by = intersect(names(master_df),names(timeliness_group)),all = TRUE)

#----- Merging dataframes--------
# merging master dataframe with effectiveness group
master_df <- merge(x = master_df,y = effectiveness_group,by = intersect(names(master_df),names(effectiveness_group)),all = TRUE)
summary(master_df)


########################################################################
# Analysing the Readmission and Mortality groups
########################################################################

readm_mort_df <- read.csv(file = "ValidFiles/Readmissions and Deaths - Hospital.csv",check.names = T,header = T,stringsAsFactors = T,na.strings = c("Not Available",""))

# * Purging the demographic variables not needed in the analysis 

zdemogrphic_vars <- which(names(readm_mort_df) %in% c(zdemographics,"Measure.Name","Compared.to.National"))
readm_mort_cleaned <- readm_mort_df[,-zdemogrphic_vars]
head(readm_mort_cleaned)

# - Analysing the footnote variable

### removing the records filled with defective foot notes.

# * Considering the Footnote variable as the reference variable. Assuming that the levels specified below encapsulate the NAs in the dataset. A summary rollover on the dataset breifs the situation that below levels holds NAs
#  1 - The number of cases/patients is too few to report.
# 19 - Data are shown only for hospitals that participate in the Inpatient Quality Reporting (IQR) and Outpatient Quality Reporting (OQR) programs.
#  4 - Data suppressed by CMS for one or more quarters.
#  5 - Results are not available for this reporting period.
#  7 - No cases met the criteria for this measure.

readm_mort_cleaned[!is.na(readm_mort_cleaned$Footnote),] %>% group_by(Footnote) %>% summarise(cnt_rows = n(),avg_score = mean(Score,na.rm = T))

# * Footnotes that are stale do not have any scores.
# * Removing the rows that are as a reslut of stale data from different levels of footnotes

readm_mort_cleaned <- readm_mort_cleaned[is.na(readm_mort_cleaned$Footnote),]

# - Plotting the Mortality Mean scores values

readm_mort_cleaned %>% group_by(Measure.ID) %>% summarise(Score = sum(Score)) %>%
ggplot(aes(Measure.ID,Score)) + geom_col(fill = "steelblue") + geom_text(aes(label = Score),vjust = -0.2,size = 3) +
  labs(title = "Mortality Readmission Rate Score") + ylab(label = "Provider Count") + 
  theme(axis.text.x = element_text(angle = 45,vjust = 1,hjust = 1),
        plot.title = element_text(hjust = 0.5)) 

# * Plotting the Mortality Measure variables by Count

ggplot(readm_mort_cleaned,aes(Measure.ID)) + geom_bar(fill = "steelblue") + geom_text(stat = "count",aes(label = ..count..),vjust = -.1) +
  labs(title = "Mortality Readmission Rate Count by Providers") + ylab(label = "Provider Count") +
  theme(axis.text.x = element_text(angle = 45,vjust = 1,hjust = 1),axis.text.y = element_blank(),
        plot.title = element_text(hjust = 0.5)) 

# * *Readmission# measure that contribute to the overall count are:

# READM-30-AMI 	Acute Myocardial Infarction (AMI) 30-Day Readmission Rate 
# READM-30-COPD Chronic Obstructive Pulmonary Disease (COPD) 30-Day Readmission Rate 
# READM-30-CABG Coronary Artery Bypass Graft (CABG) 30-Day Readmission Rate 
# READM-30-HF 	Heart Failure (HF) 30-Day Readmission Rate 
# READM-30-Hip-Knee 	Hospital-Level 30-Day All-Cause Risk- Standardized Readmission Rate (RSRR) Following Elective Total Hip Arthroplasty (THA)/ Total Knee Arthroplasty (TKA) 
# READM-30-PN 	Pneumonia (PN) 30-Day Readmission Rate 
# READM-30-STK 	Stroke (STK) 30-Day Readmission Rate 
# READM-30-HOSP-WIDE 	HWR Hospital-Wide All-Cause Unplanned Readmission
# 
# * *Mortality# group that contribute to Overall Hospital Rating are:
# MORT-30-AMI 	Acute Myocardial Infarction (AMI) 30-Day Mortality Rate 
# MORT-30-CABG 	Coronary Artery Bypass Graft (CABG) 30-Day Mortality Rate 
# MORT-30-COPD 	Chronic Obstructive Pulmonary Disease (COPD) 30-Day Mortality Rate 
# MORT-30-HF 	Heart Failure (HF) 30-Day Mortality Rate 
# MORT-30-PN 	Pneumonia (PN) 30-Day Mortality Rate 
# MORT-30-STK 	Acute Ischemic Stroke (STK) 30-Day Mortality Rate 

levels(readm_mort_cleaned$Measure.ID)
zvar1 <- which(names(readm_mort_cleaned) %in% c("Provider.ID","Measure.ID","Score"))
readm_mort_final <- readm_mort_cleaned[,zvar1] %>% spread(key = Measure.ID,value = Score)

# Separating the mortality measure and readmission measure

zvar1 <- which(names(readm_mort_final) %in% c("Provider.ID","MORT_30_AMI","MORT_30_CABG","MORT_30_COPD","MORT_30_HF","MORT_30_PN","MORT_30_STK"))
mortality_grp <- readm_mort_final[,zvar1]

zvar1 <- which(names(readm_mort_final) %in% c("MORT_30_AMI","MORT_30_CABG","MORT_30_COPD","MORT_30_HF","MORT_30_PN","MORT_30_STK"))
readmission_grp <- readm_mort_final[,-zvar1]

dim(readmission_grp)
dim(mortality_grp)

# Checking the correlation between different measures
pairs.panels(mortality_grp[,-1],scale = TRUE,ellipses = TRUE,pch = 21,bg = rainbow(n = ncol(readmission_grp)),cex.labels = 0.6,cex.cor = 2,main = "Mortality Group Correlation Plot")

# - Mortality measures have gaussian distribution
# - Mortality measure have positive correlation between themselves


pairs.panels(readmission_grp[,-1],scale = TRUE,ellipses = TRUE,pch = 21,bg = rainbow(n = ncol(readmission_grp)),cex.labels = 0.6,cex.cor = 2,main = "Readmission Group Correlation Plot")

# * All varaibles in the dataset are significant and thus are being retained into the final dataframe
# * Two variables that are significant and strong correlation between them are: 
# + 1. Readmission_Hospital_Wide and Readm_PN death rate
# + 2. Readmission-after discharge and Heart Failure
# + 3. Readmission after discharge and COPD
# 
# Ailments are significantly correlated.
# 
# * Combining the mortality and readmission group with the master dataframe.

#----- Merging dataframes--------
master_df <- merge(x = master_df,y = readm_mort_final,by = intersect(x = names(master_df),y = names(readm_mort_final)),all = TRUE)
head(master_df)

# Analysis of Imaging dataset
op_imaging_eff_df <- read.csv(file = "ValidFiles/Outpatient Imaging Efficiency - Hospital.csv",header = T,check.names = T,na.strings = c("Not Available",""),stringsAsFactors = T)
head(op_imaging_eff_df)


# Purging the variables that are not useful for analysis
zdemogrphic_vars <- which(names(op_imaging_eff_df) %in% c(zdemographics,"Measure.Start.Date","Measure.End.Date"))
op_imaging_eff_cleaned <- op_imaging_eff_df[,-zdemogrphic_vars]

#head(op_imaging_eff_cleaned)
# - Verifying the distribution of different measures
# summary(op_imaging_eff_cleaned)
# - In order to check the distribution of NAs in Score variable, Footnote is taken as reference to validate the NAs

#op_imaging_eff_cleaned[is.na(op_imaging_eff_cleaned$Footnote),] %>% summary()
op_imaging_eff_cleaned %>% group_by(Footnote) %>% summarise(count_score = sum(Score,na.rm = T))

# * From the summary we can infer that most of the NAs in the score are induced by Stale data represented by all classess of Footnotes except NA/Missing
# * Purging the footnote levels that do not contribute 

op_imaging_eff_cleaned <- op_imaging_eff_cleaned[is.na(op_imaging_eff_cleaned$Footnote),] 

# Plotting the trends of Measure Variables acorss the providers

ggplot(op_imaging_eff_cleaned,aes(Measure.ID)) + geom_bar(fill = "steelblue") + 
  geom_text(stat = "count",aes(label = ..count..),vjust = -0.2,angle = 0,hjust = 0.5) +
  labs(title = "Count of Providers by Imaging Efficiency measures") + 
  theme(plot.title = element_text(hjust = 0.5))

# - The plots suggests that all measures of Imaging efficiency are significant.

op_imaging_eff_cleaned %>% group_by(Measure.ID) %>% summarise(mean_score = mean(Score)) %>%
ggplot(aes(Measure.ID, mean_score)) + geom_col(fill = "steelblue") + 
  geom_text(aes(label = round(mean_score,1)),vjust = -0.1,angle = 0,hjust = 0.5) +
  labs(title = "Mean Score of Measures for Imaging Efficiency") + 
  theme(plot.title = element_text(hjust = 0.5))

# * Considering the imaging parameters OP_8,OP_10,OP_11,OP_13,OP_14

# *Oupatient Imaging Efficiency Group:*
# OP-8 	  MRI Lumbar Spine for Low Back Pain 
# OP-10 	Abdomen CT Use of Contrast Material 
# OP-11 	Thorax CT Use of Contrast Material 
# OP-13 	Cardiac Imaging for Preoperative Risk Assessment for Non-Cardiac Low-Risk Surgery 
# OP-14 	Simultaneous Use of Brain Computed Tomography (CT) and Sinus CT 

zvar1 <- c("Provider.ID","Measure.ID","Score")
zvar2 <- which(names(op_imaging_eff_cleaned) %in% zvar1)
op_imaging_eff_final <- op_imaging_eff_cleaned[,zvar2] %>% spread(key = Measure.ID,value = Score)

# dropping the less singificant OP_9 measure
op_imaging_eff_final <- op_imaging_eff_final[,-ncol(op_imaging_eff_final)]

# head(op_imaging_eff_final)
# * Checking the variability correlation that exist between the imaging measures

pairs.panels(op_imaging_eff_final[,-1],scale = T,smooth = T,density = T,ellipses = T,pch = 21,method = 'pearson',cex.labels = 1,cex.cor = 2,
             main = "Correlation between Imagining Efficiency Measures ")

# * Data variablility is uniform acorss the dataset except the Op_10 and OP-11 measure which are closely correlated with skewed distribution
# * Merging the imaging the dataframe with Master_dataframe

#----- Merging dataframes--------
master_df_final <- merge(x = master_df,y = op_imaging_eff_final,by = intersect(x = names(master_df),y = names(op_imaging_eff_final)),all = TRUE) 

head(master_df_final)

# * removing the provider.id and re arranging the hospital overall rating column
# Next steps:
# 1. check outliers in the measures
# 2. imputing missing values
# which rows have all NAs?

na_indices <- apply(master_df_final[,-c(1,2)], MARGIN = 1, function(x) all(is.na(x)))
sum(na_indices) # there are no columns with all NAs

# Replace NA's in the final master_df with Missing
master_df_final$Hospital.overall.rating[which(is.na(master_df_final$Hospital.overall.rating))] <- "Missing"

# imputing the missing values using the mice library
master_df_imputed <- mice(data = master_df_final[,-c(1,2)],seed = 100,maxit = 5,m = 5)
master_df_imputed_final <-  mice::complete(data = master_df_imputed,action = 5)
master_df_imputed_final <- cbind(Provider.ID = master_df_final$Provider.ID,master_df_imputed_final)

####################################################
# Using Random Forest algorithm for ratings
####################################################

# Spliting the dataset into training and testing 
set.seed(100)

master_df_model <- data.frame(Hospital.overall.rating = master_df_final$Hospital.overall.rating,master_df_imputed_final)

##########################
# Sampling
##########################

indices <- sample(1:nrow(master_df_model),0.7 * nrow(master_df_model))

# training dataset
train_df <- master_df_model[indices,]
# test dataset
test_df <- master_df_model[-indices,]

####################################################
# Model building using RandomForest Algorithm
####################################################

set.seed(100)
doParallel::registerDoParallel(cl = 4,cores = 4)

train_control_rf <- trainControl(method = "repeatedcv",
                                  repeats = 5,
                                  number = 5,
                                  search = "grid",
                                  sampling = "smote",
                                 allowParallel = TRUE)

# Tuning grid parameters of Random Forest
tuning_grid_rf <- expand.grid(.mtry = round(sqrt(ncol(train_df[,-1]))),ntree = seq(100,1000,100)) 

# training the random forest with training dataset
model_rf <- randomForest(Hospital.overall.rating ~.,
                         data = train_df,
                         trControl = train_control_rf,
                         tuneGrid = tuning_grid_rf,
                         metric = "auc",
                         na.action = na.roughfix,
                         scale = TRUE,
                         seed = 100)
model_rf

### Predicting the Hospital Ratings
vars_predict <- setdiff(x = names(train_df[,-1]),y = "Hospital.overall.rating")
predict_rf <- stats::predict(object = model_rf,test_df[vars_predict])

# Confusion Matrix
confusionMatrix(predict_rf,test_df$Hospital.overall.rating)

########################################################################
# Predicting the "Missing" Ratings in Hospital Rating using RandomForest
########################################################################
# Isolating the Hospitals with missing Hospital Ratings and buding a random Forest model for the remaining dataset

set.seed(100)
master_df_model <- data.frame(Hospital.overall.rating = master_df_final$Hospital.overall.rating,master_df_imputed_final)

# Removing the Hospital with Hospital Rating as "Missing"
ratings <- master_df_model$Hospital.overall.rating
ratings[ratings %in% "Missing"] <- NA
ratings <- as.integer(ratings)
master_df_model_no_Missing <- data.frame(Hospital.overall.rating = ratings,master_df_model[,-1])

master_df_model_no_Missing <- master_df_model_no_Missing[!is.na(master_df_model_no_Missing$Hospital.overall.rating),]
master_df_model_no_Missing$Hospital.overall.rating <- as.factor(master_df_model_no_Missing$Hospital.overall.rating)

####################################
# Sampling
####################################
set.seed(999)
indices_no_missing <- sample(1:nrow(master_df_model_no_Missing),0.7 * nrow(master_df_model_no_Missing))

# training dataset
train_df <- master_df_model_no_Missing[indices_no_missing,]
# test dataset
test_df <- master_df_model_no_Missing[-indices_no_missing,]

############################################################################################################
### using RandomForest Algorithm for Hospitals Ratings to predict the  Missing Rating
############################################################################################################
set.seed(100)

doParallel::registerDoParallel(cl = 4,cores = 4)
train_control_rf <- trainControl(method = "repeatedcv",
                                  repeats = 5,
                                  number = 5,
                                  search = "grid",
                                  sampling = "smote",
                                 allowParallel = TRUE)

# Tuning grid parameters of Random Forest
tuning_grid_rf <- expand.grid(.mtry = round(sqrt(ncol(train_df[,-1]))),ntree = seq(100,1000,100)) 

# training the random forest with training dataset
model_rf_no_na <- randomForest(Hospital.overall.rating ~.,
                         data = train_df[,-2],
                         trControl = train_control_rf,
                         tuneGrid = tuning_grid_rf,
                         metric = "auc",
                         na.action = na.roughfix,
                         scale = TRUE,
                         seed = 100)
model_rf_no_na

# Predicting the test_df hosptial ratings and 
vars_predict <- setdiff(x = names(test_df[,-c(2)]),y = "Hospital.overall.rating")
predict_rf_test <- stats::predict(object = model_rf_no_na,test_df[vars_predict])

# Confusion Matrix
confusionMatrix(predict_rf_test,test_df$Hospital.overall.rating)

# * Random Forest Algorithm generated an accuracy of 74%.


# Predicting the rating of missing Ratings using the randomForest

# Dataframe of Missing ratings
master_df_model_Missing <- data.frame(Hospital.overall.rating = ratings,master_df_model[,-c(1)])
master_df_model_Missing <- master_df_model_Missing[is.na(master_df_model_Missing$Hospital.overall.rating),]

predicted_rating_missing_Values <- predict(model_rf_no_na,master_df_model_Missing[,-1])

#assigning the predicted values to missing
master_df_model_Missing$Hospital.overall.rating <- predicted_rating_missing_Values

# combining the missing and no missing rows
predict_master_df <- rbind(master_df_model_no_Missing,master_df_model_Missing) 


############################################################################################################
#  Grouping the measures 
############################################################################################################

# Groups identified so far are:
# Outcome Groups:
# 1. Mortality group:  - This is a negative group
# 2. Saftey of Care +
# 3. Readmission  -
# 4. Patient Experience  +
# Process Groups:
# 5. Effectiveness of Care +
# 6. Timeliness of care - 
# 7. Efficient Use of Imaging +

########################################################################
##                  1 Safety of Care Group
########################################################################
# safety of care has a positive impact on the overall rating
zvar_saftey <- c("Provider.ID","COMP_HIP_KNEE","PSI_90_SAFETY","HAI_1_SIR","HAI_2_SIR","HAI_3_SIR","HAI_4_SIR","HAI_5_SIR","HAI_6_SIR")
zvar1 <- which(names(master_df_imputed_final) %in% zvar_saftey)

# scaled # winsorized 
safety_of_care_group <- master_df_imputed_final[,zvar1][,-1] %>% scale() %>% winsor(trim = 0.0125)

## Applying Latent Variable Model 
# - Factor Analysis technique to identify the underlying factors in the continuous variable
# Latent Variable model is used to summarize the information. 
# For the Star Rating LVM assumes that each measure reflects information about the underlying unobserved dimension of quality. 

# LVM is constructed for each group independently.

# - Different approaches to factor analysis to identify the factors
# - Using the psych library functions to:
#   - 1. identify the factors
#   - 2. compute the factors

# Method to determine the factors
zxz <- nfactors(x=safety_of_care_group,n=20,
         rotate="varimax",diagonal=FALSE,
         fm="ml",title="Number of Factors",pch=16,use="complete")

# Extracting the number of factors
n_factors = sum(!is.na(zxz$vss.stats[,"prob"]))

# From the nfactors, it is clear that the saftey_care variable is to be factored into 4 factors.
# factoring method, fm = "pa" pa- principal axis, rotate = with max variance, minres - min resume

fm = "ml" # maximum likelihood
rotate = "varimax" # varimax rotation to obtain maximum variance between the measons
nfactors = n_factors
scores = "regression"
safety_care_fact_anal <- fa(r = safety_of_care_group,rotate = rotate,fm = fm,scores = scores,nfactors = nfactors)

# using the loadings
safety_loadings <- safety_care_fact_anal$loadings
cor.plot(safety_of_care_group,stars = FALSE,numbers = TRUE,colors = TRUE,cex = 0.5, show.legend = FALSE,upper = F,
         xlas = 2,cex.axis = 0.6,main = "Safety of Care measures correlation")

# computing the mean loading
safety_loading <- as.vector(apply(safety_loadings[,1:nfactors],1,FUN = mean))

# comparing with correlations
cor_matrix <- data.frame(cor(safety_of_care_group))
cor_matrix <- data.frame(sapply(cor_matrix, function(x) round(mean(x), 2)))
cor_matrix <- cbind(cor_matrix, round(safety_loading, 2))
colnames(cor_matrix) <- c("avg_correlation", "mean_factor_loading")
cor_matrix

# comparing the relation between the avg correlation, mean factor loadings, mean weights
mean_weights = round(apply(safety_care_fact_anal$weights,1,mean),3)
cbind(cor_matrix,mean_wghts = mean_weights)

# It is clear that factor loadings are proportional to mean_weights.

# ASSUMPTION: Num of Hospitals with greater than 3 measures having NAs in master_df_final is to be eliminated as per CMS
master_df_final_safety_care_measures <- cbind(master_df_final$Provider.ID,master_df_final[,which(colnames(master_df_final) %in% 
                                                                                                   colnames(safety_of_care_group))])

# filtering the indices with containing NA score measures <= 3 
valid_indices <- apply(master_df_final_safety_care_measures[,-1],1,function(x) sum(is.na(x))) < 3
master_df_final_safety_care_measures <- master_df_final_safety_care_measures[valid_indices == TRUE,] 

# Num of Providers with less than 3 measures having NAs in the master_df_final
table(valid_indices)

# In order to calculate scores,imputing the missing values in the scores variable
impute_na <- function(measure){
  measure[which(is.na(measure))] <- median(measure, na.rm = TRUE)
  return(measure)
}

master_df_final_safety_care_measures <- data.frame(sapply(master_df_final_safety_care_measures, impute_na))

# Computting group score of Saftety of Care
mean_weights
master_df_final_safety_care_measures$score <- 0

# using sweep function to multiply mean_weights with each hospital group measure to evaluate to the total group score from all the measures.
master_df_final_safety_care_measures$score <- apply(sweep(master_df_final_safety_care_measures[,-1],MARGIN = 2, mean_weights/length(mean_weights),'*'),1,sum) %>% round(digits = 3)

# selecting the score measures
saftey_group_score <- master_df_final_safety_care_measures[, c("master_df_final.Provider.ID", "score")]
colnames(saftey_group_score) <- c("Provider.ID", "saftey_score")
head(saftey_group_score)
plot(density(saftey_group_score$saftey_score),main = "Safety Group Score" )

############################################################################################################
### Generic function to convert the remaining groups to group scores
############################################################################################################

# - To evaluate For the 6 other group, let's put this entire code in a function
# - Function takes measure_group as the input and returns the group scores for each hospital

# Initialize the function
f_group_scores <- function(group_measure) {
  
zxz <- nfactors(x=group_measure,n=20,
         rotate="varimax",diagonal=FALSE,
         fm="ml",title="Number of Factors",pch=16,use="complete")

# Extracting the number of factors
n_factors = sum(!is.na(zxz$vss.stats[,"prob"]))

# From the nfactors, it is clear that the saftey_care variable is to be factored into 4 factors.
# factoring method, fm = "pa" pa- principal axis, rotate = with max variance, minres - min resume

fm = "ml" # maximum likelihood
rotate = "varimax" # varimax rotation to obtain maximum variance between the measons
nfactors = n_factors
scores = "regression"
fact_anal <- fa(r = group_measure,rotate = rotate,fm = fm,scores = scores,nfactors = nfactors)

# using the loadings
safety_loadings <- fact_anal$loadings
cor.plot(group_measure,stars = FALSE,numbers = TRUE,colors = TRUE,cex = 0.5, show.legend = FALSE,upper = F,
         xlas = 2,cex.axis = 0.6,main = "Group measure correlation")

# computing the mean loading
loadings <- as.vector(apply(safety_loadings[,1:nfactors],1,FUN = mean))

# comparing with correlations
cor_matrix <- data.frame(cor(group_measure))
cor_matrix <- data.frame(sapply(cor_matrix, function(x) round(mean(x), 2)))
cor_matrix <- cbind(cor_matrix, round(loadings, 2))
colnames(cor_matrix) <- c("avg_correlation", "mean_factor_loading")

cor_matrix

# Comparing the relation between the avg correlation, mean factor loadings, mean weights
mean_weights = round(apply(fact_anal$weights,1,mean),3)
cbind(cor_matrix,mean_wghts = mean_weights)

# It is clear that factor loadings are proportional to mean_weights.
# Num of Hospitals with greater than 3 measures having NAs in master_df_final is to be eliminated as per CMS

master_df_final_group_measure <- cbind(master_df_final$Provider.ID,master_df_final[,which(colnames(master_df_final) %in% colnames(group_measure))])

# filtering the indices with containing NA score measures <= 3 
valid_indices <- apply(master_df_final_group_measure[,-1],1,function(x) sum(is.na(x))) < 3
master_df_final_group_measure <- master_df_final_group_measure[valid_indices == TRUE,] 

# Num of Providers with less than 3 measures having NAs in the master_df_final
table(valid_indices)

# In order to calculate scores,imputing the missing values in the scores variable
impute_na <- function(measure){
  measure[which(is.na(measure))] <- median(measure, na.rm = TRUE)
  return(measure)
}

master_df_final_group_measure <- data.frame(sapply(master_df_final_group_measure, impute_na))

####################################
# Function to multiply mean_weights with each hospital group measure to evaluate to the total group score from all the measures. 
####################################

  n <- ncol(master_df_final_group_measure) 
  master_df_final_group_measure$score <- 0
  
  for (row in 1:nrow(master_df_final_group_measure)){
    master_df_final_group_measure[row, c("score")] <- round(sum(master_df_final_group_measure[row, 1:n]*mean_weights)/length(mean_weights), 3)
  }

# selecting the score measures
group_score <- master_df_final_group_measure[, c("master_df_final.Provider.ID", "score")]
colnames(group_score) <- c("Provider.ID", "saftey_score")
return(group_score)

}

####----------------------- END of USER-DEFINED Function ------------------ ####


########################################################################
##                      2 patient_experience Group
########################################################################

zvar_patient_exp <- c("Provider.ID","H_CLEAN","H_COMP_1","H_COMP_2","H_COMP_3","H_COMP_4","H_COMP_5","H_COMP_6","H_COMP_7","H_HSP_RATING","H_QUIET","H_RECMND")
zvar1 <- which(names(master_df_imputed_final) %in% zvar_patient_exp)

direction <- -1
# scaling the patient experience group and winsoring between +/- 3Sigma
patient_experience_group <- master_df_imputed_final[,zvar1][,-1] %>% scale() %>% winsor(trim = 0.0125) * direction

## Patient experience latent model -
## Calling the user-defined function used to create the scores and latent view modelling

patient_experience_group_score <- f_group_scores(patient_experience_group)
colnames(patient_experience_group_score)[2] <- "patient_exp_score"
str(patient_experience_group_score)

########################################################################
##                      3 readmission group
########################################################################

zvar_readm <- c("Provider.ID","READM_30_AMI","READM_30_CABG","READM_30_COPD","READM_30_HF","READM_30_HIP_KNEE","READM_30_HOSP_WIDE","READM_30_PN","READM_30_STK")
zvar1 <- which(names(master_df_imputed_final) %in% zvar_readm)

# multiply the measure scores with direction to indicate the type of measure. timeliness, readmission, mortality
direction <- -1  

# scaling the patient experience group and winsoring between +/- 3Sigma
readmission_group <- master_df_imputed_final[,zvar1][,-1] %>% scale() %>% winsor(trim = 0.0125) *direction

## Computing readmission group latent model

## Readmission Latent Variable Model
## Calling the user-defined function used to create the scores and latent view modelling

readmission_group_score <- f_group_scores(readmission_group)
colnames(readmission_group_score)[2] <- "readm_score"
head(readmission_group_score)

########################################################################
##                       4 mortality group
########################################################################

zvar_mort <- c("Provider.ID","MORT_30_AMI","MORT_30_CABG","MORT_30_COPD","MORT_30_HF","MORT_30_PN","MORT_30_STK","PSI_4_SURG_COMP")
zvar1 <- which(names(master_df_imputed_final) %in% zvar_mort)

# scaling the patient experience group and winsoring between +/- 3Sigma
mortality_group <- master_df_imputed_final[,zvar1][,-1] %>% scale() %>% winsor(trim = 0.0125) * direction

## Computing mortality group latent model
# Calling the user-defined function used to create the scores and latent view modelling

mortality_group_score <- f_group_scores(mortality_group)
colnames(mortality_group_score)[2] <- "mort_score"
head(mortality_group_score)

########################################################################
##                      5. Process Group 4% - Effectiveness Group
########################################################################

zvar_eff <- c("Provider.ID","CAC_3","IMM_2","IMM_3_OP_27","OP_22","OP_23","OP_29","OP_30","OP_4","PC_01","STK_1","STK_4","STK_6","STK_8","VTE_1","VTE_2","VTE_3","VTE_5","VTE_6")
zvar1 <- which(names(master_df_imputed_final) %in% zvar_eff)

# scaling the patient experience group and winsoring between +/- 3Sigma
effectiveness_group <- master_df_imputed_final[,zvar1][,-1] %>% scale() %>% winsor(trim = 0.0125)

## Computing Effectiveness group latent model
# Calling the user-defined function used to create the scores and latent view modelling

effectiveness_group_score <- f_group_scores(effectiveness_group)
colnames(effectiveness_group_score)[2] <- "eff_score"
head(effectiveness_group_score)

########################################################################
##                      6 timeliness Group
########################################################################

zvar_tim <- c("Provider.ID","ED_1b","ED_2b","OP_18b","OP_20","OP_21","OP_3b","OP_5")
zvar1 <- which(names(master_df_imputed_final) %in% zvar_tim)

# scaling the timeliness between +/1 3Sigma and winsorizing the scores
timeliness_group <- master_df_imputed_final[,zvar1][,-1] %>% scale() %>% winsor(trim = 0.0125) * direction
dim(timeliness_group)

## Computing timeliness group latent variable model
# Calling the user-defined function used to create the scores and latent view modelling

timeliness_group_score <- f_group_scores(timeliness_group)
colnames(timeliness_group_score)[2] <- "timeliness_score"
head(timeliness_group_score)

########################################################################
##                      7 medical imagine efficiency group
########################################################################

zvar_imag <- c("Provider.ID","OP_10","OP_11","OP_13","OP_14","OP_8")
zvar1 <- which(names(master_df_imputed_final) %in% zvar_imag)

# scaling the patient experience group and winsoring between +/- 3Sigma
imaging_efficiency_group <- master_df_imputed_final[,zvar1][,-1] %>% scale() %>% winsor(trim = 0.0125)

## Computing Imagining Efficiency group latent variable model
# Calling the user-defined function used to create the scores and latent view modelling

imaging_eff_group_score <- f_group_scores(imaging_efficiency_group)
colnames(imaging_eff_group_score)[2] <- "imaging_eff_score"
head(imaging_eff_group_score)

##########################################################################
## Binding the all the groups scores and preparing the final scores table.
##########################################################################
m1 <- merge(saftey_group_score, patient_experience_group_score, by="Provider.ID", all=T)
m2 <- merge(m1, readmission_group_score, by="Provider.ID", all=T)
m3 <- merge(m2, mortality_group_score, by="Provider.ID", all=T)
m4 <- merge(m3, timeliness_group_score, by="Provider.ID", all=T)
m5 <- merge(m4, effectiveness_group_score, by="Provider.ID", all=T)

group_scores <- merge(m5, imaging_eff_group_score, by="Provider.ID", all=T) 
head(group_scores)
dim(group_scores)

## Computing the final scores
# Adding the weightages specified by the CMS
# - The outcome groups have 22% weightage, that is Patient Experience,Saftey of Care, Readmission, and Mortality
# - Process groups have 4% weightage, that is timeliness, effectiveness measures and imaging efficiency measures

out_grp_weight = 0.22
process_group_weight = 0.04

weights <- c(safety = out_grp_weight, experience = out_grp_weight, readmission = out_grp_weight,mortality = out_grp_weight, 
             timeliness = process_group_weight,effectivesss = process_group_weight,imaging_eff = process_group_weight)

# procedure for reproproportioning the wieghts with example:
# There are missing scores in group_scores. For example, for Provider.ID=10007, Saftey, readm, mortality, effectiveness and imaging scores are missing
# Therefore, patient_exp = 22%, timeliness = 4% adds to 26%, the remaining weights add up to 74%. Thus, the reproportioned weights will be 
# c(mortality = 0.22/0.74, effectivenss = 0.04/0.74, readmission = 0.22/0.74,Safety = 0.22/0.74, imaging = 0.04/0.7 )

group_scores$final_score <- 100 # Initialise the group score

for (row in 1:nrow(group_scores)) {
  if (sum(is.na(group_scores[row, -1])) > 0) {    # if NAs are found in the group_scores
    invalid_indices = which(is.na(group_scores[row, -1])) # get the indices where NAs are found
    s = sum(weights[-invalid_indices]) # sum the scores' weights where the are scores are found
    reproportioned_weights <- weights[-invalid_indices]/s # reproportion the weights where indices are found
    #print(weights[-invalid_indices])
    #print(reproportioned_weights)
    print(sum(weights[-invalid_indices]*reproportioned_weights)) # adjusting the re-proportioned to indices that have "NAs"
    group_scores$final_score[row] <- sum(group_scores[row, -1][-invalid_indices]*reproportioned_weights)
  }
  
  else {
    group_scores$final_score[row] <- sum(group_scores[row, -1]*weights)
  }
}

# The group scores are the final scores
final_scores <- group_scores[, 1:ncol(group_scores)] 
dim(final_scores)

########################################################################
#                         Clustering the final scores
########################################################################

# - Applying K-means clustering algorithm to the final scores
# clearing the garbage collector
gc()

# with 5 clusters
kmeans_clustering <- kmeans(x = final_scores$final_score,centers = 5,nstart = 100)

# Assigning clusters to the final_scores
final_scores <- as.data.frame(final_scores)
final_scores$rating_clster <- as.factor(kmeans_clustering$cluster)

# summarising the clusters
cluster_summary <-  final_scores %>% group_by(rating_clster) %>% summarise(mean_score = mean(final_score) ,cluster_count = n()) %>% 
  mutate(percent = cluster_count*100/sum(cluster_count))
cluster_summary

# we now need to reassign the cluster ratings according to the average rating
str(cluster_summary)

# swap the 1st element with 5, 2nd with 4, 3rd with 3, 4th with 2, 5th with 1
# In general, in 1:5, swap the ith element with (5-i + 1) 

# Resetting the clusters
for (row in 1:nrow(final_scores)) {
  id = final_scores$rating_clster[row] 
  final_scores$new_cluster_id[row] = 5 - which(cluster_summary$rating_clster == id) + 1 # # swap x with 5 - which(f$cluster_id == x) + 1
}

# check the distribution of the new clusters after the swap
final_scores %>% group_by(new_cluster_id) %>% 
  summarise(avg_score = mean(final_score)) %>%
  arrange(desc(avg_score))

# new cluster id added to the final scores data frame. after adjusting the clusters
final_scores$new_cluster_id <- as.factor(final_scores$new_cluster_id)
summary(final_scores[,c(10,11)])

head(final_scores)

## comparing scores across star ratings ####
ggplot(final_scores, aes(x = factor(as.character(new_cluster_id)), y=final_score*100)) + geom_boxplot(na.rm=TRUE) + 
  xlab("star rating") + ylab("final score") + theme_minimal() + ggtitle("comparing scores across star ratings") +
  scale_y_continuous(breaks=100*seq(-0.4, 0.4, 0.05))

# Distribution of ratings across different groups
melt(data = final_scores[,-1],variable.name = "measure") %>%
  ggplot(aes(new_cluster_id,value*100)) + geom_boxplot(na.rm = T) +
  facet_wrap(facets = ~measure,scales = "free",ncol = 3) + ggtitle("Distribution of ratings across different groups")+
  xlab("Star rating") + ylab("Score") + theme_minimal() 


# 5 - Rating in the final score is contributed by the Timeliness and Imaging Efficiency
# 4 - Rating is contributed by Safety score, readmission, efficectiveness and timeliness.
# 1 - Rating is mainly due to timeliness score, Low imaging efficiency, 


# star_rating_validation
# merging the general_info_final with final scores

final_scores_merged <- merge(final_scores, general_info_final, by="Provider.ID", all=T)
head(final_scores_merged)
final_scores_merged$new_cluster_id <- as.character(final_scores_merged$new_cluster_id)
str(final_scores_merged)
# imputing the missing NA records in the new_cluster_id with "Not Available"
final_scores_merged$new_cluster_id[which(is.na(final_scores_merged$new_cluster_id))] <- "Missing"

# converting the cluster id to factor
final_scores_merged$new_cluster_id <- factor(final_scores_merged$new_cluster_id)
summary(final_scores_merged$new_cluster_id)

# confusion matrix
table(final_scores_merged$new_cluster_id, final_scores_merged$Hospital.overall.rating)
confusionMatrix(final_scores_merged$new_cluster_id, final_scores_merged$Hospital.overall.rating)


########################################################################
##                         Provider Analysis
########################################################################

head(group_scores)
# comapring provider's overall score with
ggplot(group_scores, aes(x=1, y=final_score))+geom_boxplot()

# provider's score
final_scores_merged[which(final_scores_merged$Provider.ID == 140010),]

final_scores_merged %>% group_by(new_cluster_id) %>%
  summarise(avg_final_score = mean(final_score, na.rm = T), avg_mort_scr = mean(mort_score, na.rm = T), 
            avg_saf_scr = mean(saftey_score, na.rm = T),avg_eff_scr = mean(eff_score, na.rm = T), 
            avg_read_scr = mean(readm_score, na.rm = T),avg_tim_scr = mean(timeliness_score, na.rm = T), 
            avg_img_scr = mean(imaging_eff_score, na.rm=T), avg_pat_exp_scr = mean(patient_exp_score, na.rm=T))

# Viewing scores of the given provider
final_scores_merged[which(final_scores_merged$Provider.ID == 140010),]

# plotting group level scores with rating
ggplot(final_scores, aes(x=factor(as.character(new_cluster_id)), y=final_score*100)) + geom_boxplot(na.rm=TRUE) + ylab("final score") + 
  scale_y_continuous(breaks = 100*seq(-0.4, 0.4, 0.05)) + 
  theme(plot.title = element_text(size = 10),
        axis.title.x = element_blank())

# mortality
plot.m <- ggplot(final_scores_merged, aes(x=factor(as.character(new_cluster_id)), y=mort_score*100)) + 
  geom_boxplot(na.rm=TRUE) + ylab(label = "mortality score") + scale_y_continuous(breaks = 100*seq(-0.4, 0.4, 0.05)) + 
  theme(plot.title = element_text(size = 10),
        axis.title.x = element_blank(),axis.title.y = element_text(size = 8),axis.text.x = element_text(size = 8),
        axis.text.y = element_blank(),axis.ticks.y = element_blank())
#plot.m

# readmission
plot.r <- ggplot(final_scores_merged, aes(x=factor(as.character(new_cluster_id)), y=readm_score*100)) + geom_boxplot(na.rm=TRUE) +
   ylab("readmission score") + scale_y_continuous(breaks = 100*seq(-0.4, 0.4, 0.05)) + 
  theme(plot.title = element_text(size = 10),
        axis.title.x = element_blank(),axis.title.y = element_text(size = 8),axis.text.x = element_text(size = 8),
        axis.text.y = element_blank(),axis.ticks.y = element_blank())
#plot.r

# safety
plot.s <- ggplot(final_scores_merged, aes(x=factor(as.character(new_cluster_id)), y=saftey_score*100)) + geom_boxplot(na.rm=TRUE) +
   ylab("safety score") + scale_y_continuous(breaks = 100*seq(-0.4, 0.4, 0.05)) + 
  theme(plot.title = element_text(size = 10),
        axis.title.x = element_blank(),axis.title.y = element_text(size = 8),axis.text.x = element_text(size = 8),
        axis.text.y = element_blank(),axis.ticks.y = element_blank())
#plot.s

# effectiveness
plot.eff <- ggplot(final_scores_merged, aes(x=factor(as.character(new_cluster_id)), y=eff_score*100)) + geom_boxplot(na.rm=TRUE) +
   ylab("effectiveness score") + scale_y_continuous(breaks = 100*seq(-0.4, 0.4, 0.05)) + 
  theme(plot.title = element_text(size = 10),
        axis.title.x = element_blank(),axis.title.y = element_text(size = 8),axis.text.x = element_text(size = 8),
        axis.text.y = element_blank(),axis.ticks.y = element_blank())
#plot.eff

# experience
plot.exp <- ggplot(final_scores_merged, aes(x=factor(as.character(new_cluster_id)), y=patient_exp_score*100)) + geom_boxplot(na.rm=TRUE) +
   ylab("experience score") + 
  scale_y_continuous(breaks = 100*seq(-0.4, 0.4, 0.05)) + 
  theme(plot.title = element_text(size = 10),
        axis.title.x = element_blank(),axis.title.y = element_text(size = 8),axis.text.x = element_text(size = 8),
        axis.text.y = element_blank(),axis.ticks.y = element_blank())
#plot.exp

# timeliness
plot.t <- ggplot(final_scores_merged, aes(x=factor(as.character(new_cluster_id)), y=timeliness_score*100)) + 
  geom_boxplot(na.rm=TRUE) +
   ylab("timeliness score") + 
  scale_y_continuous(breaks = 100*seq(-0.4, 0.4, 0.05)) + 
  theme(plot.title = element_text(size = 10),
        axis.title.x = element_blank(),axis.title.y = element_text(size = 8),axis.text.x = element_text(size = 8),
        axis.text.y = element_blank(),axis.ticks.y = element_blank())
#plot.t

# medical
plot.med <- ggplot(final_scores_merged, aes(x=factor(as.character(new_cluster_id)), y=imaging_eff_score*100)) + 
  geom_boxplot(na.rm=TRUE) + ylab("medical score") + 
  scale_y_continuous(breaks = 100*seq(-0.4, 0.4, 0.05)) + 
  theme(plot.title = element_text(size = 10),
        axis.title.x = element_blank(),axis.title.y = element_text(size = 8),axis.text.x = element_text(size = 8),
        axis.text.y = element_blank(),axis.ticks.y = element_blank())
#plot.med

p.grid <- plot_grid(plot.r, plot.exp, plot.m, plot.s,
          plot.t, plot.eff, plot.med,
          labels=c("readmission", "experience", "mortality", "safety",  "timeliness",
                   "effectiveness", "medical"),
          ncol = 3, nrow = 3,label_size = 8,hjust = -2)

p.grid


#     Provider.ID saftey_score patient_exp_score readm_score mort_score timeliness_score eff_score imaging_eff_score final_score rating_clster new_cluster_id
# 947      140010         0.11          -238.337    1296.652     405.07         7970.751        NA           12100.4    1194.599             4              2
#     Hospital.overall.rating
# 947                       3

############################################################################################################
# Setting the provider yintercepts to compare with the national levels average
############################################################################################################

provider.grid <- plot_grid(plot.r + geom_hline(yintercept=1296.7, col="red"), 
                    plot.s + geom_hline(yintercept=-0.11, col="red"),
                    plot.exp + geom_hline(yintercept=--238, col="red"),
                    plot.m + geom_hline(yintercept=405, col="red"),
                    plot.t + geom_hline(yintercept=7970, col="red"),
                    plot.eff + geom_hline(yintercept=3.8, col="red"),
                    plot.med + geom_hline(yintercept=12100, col="red"),
          labels=c("readmission", "safety", "experience", "mortality", "timeliness", 
                   "effectiveness", "medical"), 
          ncol = 3, nrow = 3)

provider.grid 

# Isolating the measures which are lowering the Hosptial rating
grid_low <- plot_grid(plot.s + geom_hline(yintercept=0.11, col="red")+ggtitle("Safety"),
                    plot.exp + geom_hline(yintercept=-238, col="red")+ggtitle("Experience"))
grid_low # These measures hould be improved in order to increase the rating

grid_med <- plot_grid(plot.r + geom_hline(yintercept=1296.5, col="red")+ggtitle("Readmission"),
                      plot.m + geom_hline(yintercept=405, col="red")+ggtitle("Mortality"),
                      plot.t + geom_hline(yintercept=7970, col="red")+ggtitle("Timeliness"),
                    plot.eff + geom_hline(yintercept=3.8, col="red")+ggtitle("Effectiveness"),
                    plot.med + geom_hline(yintercept=12100, col="red")+ggtitle("Medical"),
          ncol = 3)
grid_med # These measures have to improved to improve the rating

# -  On comparing the average scores of groups with the provider's score, we find that:
#  The final score is higher than the average score for rating=4 but lower than for rating=5
#  Mortality score is better than average for rating=5
#  Safety score is between the avg score for rating = 2 and 1
#  Effectiveness score is better than average for rating=5
#  Readmissions score is between avg of ratings 3 and 4
#  Timeliness score is better than average for rating=5
#  Medical score is better than average for rating=5
#  Experience score is between avg of ratings 2 and 3
# 
# - Overall, the rating is low because of a very low safety score (lesser than avg for rating=2), 
# - a low experience score and (between the avg for ratings 2 and 3) and (both are 22% weightage groups)

# Compare the provider's score with the overall averages
mean_scores <- round(sapply(group_scores[2:9], function(x) mean(x, na.rm = T)), 2)
mean_scores
group_scores[which(group_scores$Provider.ID == 140010),][2:9]

# Can see that the safety and experience scores of the provider are lesser than the respective averages
plot.s + geom_hline(yintercept=-0.11, col="red")
plot.exp + geom_hline(yintercept=-1.5, col="red")

# Exploring further into safety and experience scores (measure wise) safety
safety_scores <- data.frame(Provider.ID = master_df_imputed_final[,1],safety_of_care_group)
head(safety_scores)

# Average safety scores of the provider are
avg_safety = round(safety_scores[which(safety_scores$Provider.ID == 140010),][3:ncol(safety_scores)], 3)

# comparing with the average safety scores
provider_safety = round(sapply(safety_scores[, -1], function(x) mean(x, na.rm = T)), 3)

# Binding the both the variables
head(rbind(avg_safety, provider_safety))

########################################################################
# Hosptial Safety - -- Provider Analysis
########################################################################

# merging safety scores with hospital ratings
safety_scores_merged <- merge(safety_scores, final_scores, by = "Provider.ID")

plot.hai_4 <- ggplot(safety_scores_merged, aes(x = factor(as.character(new_cluster_id)), y=HAI_4_SIR*100)) + geom_boxplot(na.rm=TRUE) + 
  xlab("star rating") + ylab("HAI_4 score") + theme_minimal() 
plot.hai_4

# Observation: HAI_4 score of the provider (0.00) is lower than the median scores for ratings 2, 3, 4, 5
plot.hai_5 <- ggplot(safety_scores_merged, aes(x=factor(as.character(new_cluster_id)), y=HAI_5_SIR*100)) + geom_boxplot(na.rm=TRUE) + 
  xlab("star rating") + ylab("HAI_5 score") + theme_minimal() 
plot.hai_5

# - HAI_2, HAI_1, HAI_3, HAI_6 scores of the provider are better than avg
# - But HAI_4 and HAI_5 are lower than average
# - HAI_4 is Surgical Site Infection from abdominal hysterectomy (SSI: Hysterectomy)
# - HAI_5 is Methicillin-resistant Staphylococcus Aureus (MRSA) Blood Laboratory-identified Events (Bloodstream infections)
# - *Conclusions* - Improviing the HAI 4 and HAI_5 scores may improve the overall score rating.

########################################################################
# 2. Patient experience- -- Provider Analysis
########################################################################

# Exploring further into safety and patient experience scores (measure wise) 
experience_scores <- data.frame(Provider.ID = master_df_imputed_final[,1],patient_experience_group)

# safety scores of the provider
avg_experience = round(experience_scores[which(experience_scores$Provider.ID == 140010),][3:ncol(experience_scores)], 3)

# comparing with the average safety scores
provider_experience = round(sapply(experience_scores[, -1], function(x) mean(x, na.rm = T)), 3)
head(rbind(avg_experience, provider_experience))

#  For patient experience, H_COMP_1_LINEAR_SCORE, H_HSP_RATING_LINEAR_SCORE,
# - H_COMP_7_LINEAR_SCORE, H_COMP_3_LINEAR_SCORE, H_COMP_4_LINEAR_SCORE, 
# - H_COMP_5_LINEAR_SCORE, H_RECMND_LINEAR_SCORE are the most imp measures

# - *Observations:*
# - H_HSP_RATING_LINEAR_SCORE, H_RECMND_LINEAR_SCORE, H_CLEAN_LINEAR_SCORE are better than the National avg
# - H_HSP_RATING is overall hospital rating linear mean score
# - H_RECMND is Recommend hospital - linear mean score
# - H_CLEAN is Cleanliness - linear mean score
# *Conclusions:* - Improving the H_COMP_1 H_COMP_2 H_COMP_3 H_COMP_4 H_COMP_5 H_COMP_6 H_COMP_7 H_QUIET will improve the overall rating

# merging safety scores with hospital ratings
experience_scores_merged <- merge(experience_scores, final_scores, by="Provider.ID")

plot.hsp <- ggplot(experience_scores_merged, aes(x=factor(as.character(new_cluster_id)), y=H_HSP_RATING*100)) + geom_boxplot(na.rm=TRUE) + 
  xlab("star rating") + ylab("H_HSP_RATING score") + theme_minimal() 
plot.hsp

# Observation: HSP_RATING score of the provider (0.00) is lower than the 25th percentile of rating=4
plot.recmd <- ggplot(experience_scores_merged, aes(x=factor(as.character(new_cluster_id)), y=H_RECMND*100)) + geom_boxplot(na.rm=TRUE) + 
  xlab("star rating") + ylab("H_RECMND score") + theme_minimal() 
plot.recmd
# H_RECMND is near the 25th percentile of rating=4

plot.clean <- ggplot(experience_scores_merged, aes(x=factor(as.character(new_cluster_id)), y = H_CLEAN*100)) + geom_boxplot(na.rm=TRUE) + 
  xlab("star rating") + ylab("H_CLEAN score") + theme_minimal() 
plot.clean
# H_CLEAN of 0 is comparable to the median cleanliness score for rating=3 

########################################################################
#               3 Readmission -- Provider Analysis
########################################################################

readm_scores <- data.frame(Provider.ID = master_df_imputed_final[,1],readmission_group)

# readm scores of the provider
avg_readm = round(readm_scores[which(readm_scores$Provider.ID == 140010),][3:ncol(readm_scores)], 3)

# comparing with the average readm scores
provider_readm = round(sapply(readm_scores[, -1], function(x) mean(x, na.rm = T)), 3)
rbind(avg_readm, provider_readm)

# - The measures READM_30_CABG, READM_30_CABG are lower than the national average. However, Readm_30 has scores higher than the national average. 

########################################################################
#               4 Mortality group -- Provider Analysis
########################################################################
mortality_scores <- data.frame(Provider.ID = master_df_imputed_final[,1],mortality_group)

# readm scores of the provider
avg_mort = round(mortality_scores[which(mortality_scores$Provider.ID == 140010),][3:ncol(mortality_scores)], 3)

# comparing with the average readm scores
provider_mort = round(sapply(mortality_scores[, -1], function(x) mean(x, na.rm = T)), 3)

rbind(avg_mort, provider_mort)

# Average score is higher than the national average for the Mortality.

############################################################################################################
# - *Summary of provider analysis:*
############################################################################################################

# - The provider has low scores in the groups safety and patient-experience and readmission and this pulled the over.rating of the hosptial downs.
provider.grid

# The safety score of provider is -4.4 (on the scale of this plot)
plot.s + geom_hline(yintercept=-4.4, col = "red")

# The experience score is -1.5 (on this scale)
plot.exp + geom_hline(yintercept=-1.5, col = "red")

# - within safety, HAI_4 and HAI_5 scores are lower than the average scores of these measures
# - within experience, H_HSP_RATING, H_RECMND, H_CLEAN are worse than avg


 
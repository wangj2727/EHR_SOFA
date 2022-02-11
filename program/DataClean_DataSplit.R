
library(dplyr)
library(tidyr)
library(tibble)
library(rsample)
library(stringr)
library(labelled)
library(readr)
library(readxl)
library(here)
library(lubridate)

set.seed(202105)

# indir <- "C:\\Users\\wangj27\\Desktop\\SOFA\\"
# datacsv <- read_csv(paste0(indir, "data\\Final data\\byday_covid_2020q4.csv"))  ## size=320MB
# saveRDS(datacsv, paste0(indir,"data\\byday_covid_2020q4.Rds"))  ### to speed up data importing, convert .csv to .Rds


#############################
###
### By Day data
###
##############################
dta0 <- readRDS(here("data/Final data\\byday_covid_2020q4.Rds")) 

# # of obs=171290  (N=1036419)
# check # encounters
# length(unique(dta0$encounterid))  ---- (N=109285)
# check # tenant
#length(unique(dta0$tenant))  --- (N=86)

### incorporate labels from data dictionary data
dta0_dict <- read_excel(here("data/Final data\\byday data dictionary.xlsx"))
dta1_dict<-dta0_dict%>%
  filter(variable %in% names(dta0))
dta1 <- dta0%>%
  select(dta1_dict$variable)
var_label(dta1)<-dta1_dict$description


#################################
###
### Outcome data
###
#################################
outcome0 <- read_csv(here("data\\Final data\\covid_enc_variables_2020q4.csv"))
# check # encounters
#length(unique(outcome0$PAT_KEY))  ---- (N=109285)
# check # tenant
#length(unique(outcome0$PROV_ID))  --- (N=86)
# check # medical records
#length(unique(outcome0$MEDREC_KEY))  ---- (N=101985) --- # of participants

### incorporate labels from data dictionary data
outcome0_dict <- read_excel(here("data\\Final data\\data dictionary_cerner_latest_v1_jw.xlsx"),
                            sheet=2)
outcome1_dict<-outcome0_dict%>%filter(Variable %in% names(outcome0))
outcome1 <- outcome0%>%select(outcome1_dict$Variable)
var_label(outcome1)<-outcome1_dict$Description



#################################
###
### update data about paper revision
###
# New variables are:
# encountered: should allow you to merge this with the other data, PAT_KEY
# long_vent: binary variable indicating presence of prolonged ventilation
# or_vent: binary variable indicating mechanical ventilation associated with surgery
# resp_fail: binary variable indicating coding for respiratory failure
# arf_before_vent: binary variable indicating acute respiratory failure occurred before mechanical ventilation

#################################
revisonDta0 <- read_excel(here("data\\Final data\\new_variables_12.20.21.xlsx"),sheet=1)
# check # encounters
#length(unique(revisonDta0$encounterid))  ---- (N=109285)

outcome2 <- outcome1%>%
  full_join(revisonDta0, by=c("PAT_KEY"="encounterid"))

#save(outcome2, file=here("data\\outcome2.RData"))


##########################################
###
### select unique participant
###
###########################################
### patients re-admitted have different encounter id at each of admission. 
### Randomly selected one encounter id, check if MEDREC_KEY is unique for patients
selected_encouter <- outcome1%>%
  select(PAT_KEY, MEDREC_KEY)%>%
  group_by(MEDREC_KEY)%>%
  slice_sample()

outcome2 <-outcome2%>%
  filter(PAT_KEY %in% selected_encouter$PAT_KEY)

ReAdmitted<- outcome1%>%
  select(PAT_KEY, MEDREC_KEY)%>%
  group_by(MEDREC_KEY)%>%
  mutate(num_dups = n(),
         dup_id = row_number()) %>%
  ungroup() %>%
  filter(dup_id > 1)%>%
  distinct(MEDREC_KEY, .keep_all = TRUE)
### check how many pt readmitted 
# sum(ReAdmitted$num_dups)-nrow(ReAdmitted)
# dim(ReAdmitted)
# N=6465 patients had been readmitted; 7300 observations (encounters) had been excluded from 13765 encounters;


##########################################
###
### Split data
###
##########################################
### split data into training and testing sets with 2:1 ratio, stratify by hospitals
### number of patients within each hospitals
hosp_check <- outcome2%>%
  select(PROV_ID, PAT_KEY)%>%
  distinct(PAT_KEY, .keep_all = TRUE)%>%
  group_by(PROV_ID)%>%
  summarise(NumPT = n())

# hosp_check$NumPT[hosp_check$NumPT<=15]

# beeswarm::beeswarm(log10(hosp_check$NumPT), method="center", 
#                    horizontal=TRUE, xlab="# of patients in each hospitals (log10 scale)")

outcome2<-outcome2%>%
  left_join(hosp_check, by="PROV_ID")%>%
  mutate(Hosp_ID = ifelse(NumPT<=15, "Pooled", PROV_ID)) ### 10 hospitals that have #pt <=15

# hosp_check2 <- outcome2%>%
#   select(Hosp_ID, PAT_KEY)%>%
#   distinct(PAT_KEY, .keep_all = TRUE)%>%
#   group_by(Hosp_ID)%>%
#   summarise(NumPT = n()) ### confirmed that 10 hospitals had been combined into 1 "Pooled" category



dta_split <- initial_split(outcome2, strata = PROV_ID,prop = 2/3)
# <Analysis/Assess/Total>
#   <67992/33993/101985>
train_data <- training(dta_split)
test_data <- testing(dta_split)



##########################################
###
### formulate data for patients with multiple admissions
###
##########################################

ReAdmitted_Dta <- outcome1%>%
  filter(MEDREC_KEY %in% ReAdmitted$MEDREC_KEY)%>%
  select(PAT_KEY, MEDREC_KEY, ADM_MON, dnr_poa)  ### N=13765 encounters from 6565 patients

ReAdmitted_Dta1 <- dta1%>%
  filter(encounterid %in% ReAdmitted_Dta$PAT_KEY)%>%
  full_join(ReAdmitted_Dta, by=c("encounterid"="PAT_KEY"))%>%
  mutate(cardiovascular_sofa_points_num= ifelse(cardiovascular_sofa_points=="missing",NA,
                                                as.numeric(cardiovascular_sofa_points)),
         bilirubin_sofa_points_num= ifelse(bilirubin_sofa_points=="missing",NA,
                                           as.numeric(bilirubin_sofa_points)),
         gcs_sofa_points_num= ifelse(gcs_sofa_points=="missing",NA,
                                     as.numeric(gcs_sofa_points)),
         creatinine_sofa_points_num= ifelse(creatinine_sofa_points=="missing",NA,
                                            as.numeric(creatinine_sofa_points)),
         platelet_sofa_points_num= ifelse(platelet_sofa_points=="missing",NA,
                                          as.numeric(platelet_sofa_points)),
         respiratory_sofa_points_num= ifelse(respiratory_sofa_points=="missing",NA,
                                             as.numeric(respiratory_sofa_points)),
         total_score_num=ifelse(total_score=="missing",NA,as.numeric(total_score))
  )%>%
  mutate(bilirubin_sofa_points_num_imp=replace_na(bilirubin_sofa_points_num,0),
         creatinine_sofa_points_num_imp=replace_na(creatinine_sofa_points_num,0),
         platelet_sofa_points_num_imp=replace_na(platelet_sofa_points_num,0),
         gcs_sofa_points_num_imp=replace_na(gcs_sofa_points_num,0),
         cardiovascular_sofa_points_num_imp=replace_na(cardiovascular_sofa_points_num,0),
         respiratory_sofa_points_num_imp=replace_na(respiratory_sofa_points_num,0),
         total_score_num_imp=bilirubin_sofa_points_num_imp+
           creatinine_sofa_points_num_imp+
           platelet_sofa_points_num_imp+
           gcs_sofa_points_num_imp+
           cardiovascular_sofa_points_num_imp+
           respiratory_sofa_points_num_imp)%>%
  mutate(ADM_Year = str_sub(ADM_MON, 1,4),
         ADM_Month = str_sub(ADM_MON, 6,7),
         ADM_Date = as.Date(paste0(ADM_Year,"-",ADM_Month,"-01"),format = "%Y-%m-%d"))

save(ReAdmitted_Dta1, file=here("data\\ReAdmitted.RData"))


#####################################
###
### Merge two data sets
###
#####################################
train_data2 <- dta1%>%
  rename(PAT_KEY=encounterid)%>%
  right_join(train_data, by="PAT_KEY")  ### # of obs=648247; # of subj = 67992

train_data2 <- train_data2%>%
  mutate(cardiovascular_sofa_points_num= ifelse(cardiovascular_sofa_points=="missing",NA,
                                                as.numeric(cardiovascular_sofa_points)),
         bilirubin_sofa_points_num= ifelse(bilirubin_sofa_points=="missing",NA,
                                           as.numeric(bilirubin_sofa_points)),
         gcs_sofa_points_num= ifelse(gcs_sofa_points=="missing",NA,
                                     as.numeric(gcs_sofa_points)),
         creatinine_sofa_points_num= ifelse(creatinine_sofa_points=="missing",NA,
                                            as.numeric(creatinine_sofa_points)),
         platelet_sofa_points_num= ifelse(platelet_sofa_points=="missing",NA,
                                          as.numeric(platelet_sofa_points)),
         respiratory_sofa_points_num= ifelse(respiratory_sofa_points=="missing",NA,
                                             as.numeric(respiratory_sofa_points)),
         total_score_num=ifelse(total_score=="missing",NA,as.numeric(total_score))
  )%>%
  mutate(bilirubin_sofa_points_num_imp=replace_na(bilirubin_sofa_points_num,0),
         creatinine_sofa_points_num_imp=replace_na(creatinine_sofa_points_num,0),
         platelet_sofa_points_num_imp=replace_na(platelet_sofa_points_num,0),
         gcs_sofa_points_num_imp=replace_na(gcs_sofa_points_num,0),
         cardiovascular_sofa_points_num_imp=replace_na(cardiovascular_sofa_points_num,0),
         respiratory_sofa_points_num_imp=replace_na(respiratory_sofa_points_num,0),
         total_score_num_imp=bilirubin_sofa_points_num_imp+
           creatinine_sofa_points_num_imp+
           platelet_sofa_points_num_imp+
           gcs_sofa_points_num_imp+
           cardiovascular_sofa_points_num_imp+
           respiratory_sofa_points_num_imp)


test_data2 <- dta1%>%
  rename(PAT_KEY=encounterid)%>%
  right_join(test_data, by="PAT_KEY")  ### # of obs=323169; # of subj = 33993

test_data2 <- test_data2%>%
  mutate(cardiovascular_sofa_points_num= ifelse(cardiovascular_sofa_points=="missing",NA,
                                                as.numeric(cardiovascular_sofa_points)),
         bilirubin_sofa_points_num= ifelse(bilirubin_sofa_points=="missing",NA,
                                           as.numeric(bilirubin_sofa_points)),
         gcs_sofa_points_num= ifelse(gcs_sofa_points=="missing",NA,
                                     as.numeric(gcs_sofa_points)),
         creatinine_sofa_points_num= ifelse(creatinine_sofa_points=="missing",NA,
                                            as.numeric(creatinine_sofa_points)),
         platelet_sofa_points_num= ifelse(platelet_sofa_points=="missing",NA,
                                          as.numeric(platelet_sofa_points)),
         respiratory_sofa_points_num= ifelse(respiratory_sofa_points=="missing",NA,
                                             as.numeric(respiratory_sofa_points)),
         total_score_num=ifelse(total_score=="missing",NA,as.numeric(total_score))
  )%>%
  mutate(bilirubin_sofa_points_num_imp=replace_na(bilirubin_sofa_points_num,0),
         creatinine_sofa_points_num_imp=replace_na(creatinine_sofa_points_num,0),
         platelet_sofa_points_num_imp=replace_na(platelet_sofa_points_num,0),
         gcs_sofa_points_num_imp=replace_na(gcs_sofa_points_num,0),
         cardiovascular_sofa_points_num_imp=replace_na(cardiovascular_sofa_points_num,0),
         respiratory_sofa_points_num_imp=replace_na(respiratory_sofa_points_num,0),
         total_score_num_imp=bilirubin_sofa_points_num_imp+
           creatinine_sofa_points_num_imp+
           platelet_sofa_points_num_imp+
           gcs_sofa_points_num_imp+
           cardiovascular_sofa_points_num_imp+
           respiratory_sofa_points_num_imp)


train_data<-train_data2
test_data<-test_data2

#save(train_data, test_data, file = here("output\\SOFA_train_test_data_20Dec2021.RData"))



###########################
##
## mv cohort
##
###########################

### train
MV_mortality_MVstart_train<- train_data%>%
  group_by(PAT_KEY)%>%
  mutate(Pre_total_score_num = lag(total_score_num),
         Pre_total_score_num_imp = lag(total_score_num_imp))%>%
  ungroup()%>%
  filter(mv_yn=="Y")%>%
  group_by(PAT_KEY)%>%
  summarise(mv_yn = first(mv_yn),
            Pre_total_score_num = first(Pre_total_score_num),
            Pre_total_score_num_imp = first(Pre_total_score_num_imp))

MV_mortality_train<- train_data%>%
  group_by(PAT_KEY)%>%
  mutate(Pre_total_score_num = lag(total_score_num),
         Pre_total_score_num_imp = lag(total_score_num_imp))%>%
  filter(PAT_KEY %in% MV_mortality_MVstart_train$PAT_KEY)%>%
  select(PAT_KEY, dayoffset, death, total_score_num, total_score_num_imp, Pre_total_score_num,Pre_total_score_num_imp,
         everything())


### test
MV_mortality_MVstart_test<- test_data%>%
  group_by(PAT_KEY)%>%
  mutate(Pre_total_score_num = lag(total_score_num),
         Pre_total_score_num_imp = lag(total_score_num_imp))%>%
  ungroup()%>%
  filter(mv_yn=="Y")%>%
  group_by(PAT_KEY)%>%
  summarise(mv_yn = first(mv_yn),
            Pre_total_score_num = first(Pre_total_score_num),
            Pre_total_score_num_imp = first(Pre_total_score_num_imp))

MV_mortality_test<- test_data%>%
  group_by(PAT_KEY)%>%
  mutate(Pre_total_score_num = lag(total_score_num),
         Pre_total_score_num_imp = lag(total_score_num_imp))%>%
  filter(PAT_KEY %in% MV_mortality_MVstart_test$PAT_KEY)%>%
  select(PAT_KEY, dayoffset, death, total_score_num, total_score_num_imp, Pre_total_score_num,Pre_total_score_num_imp,
         everything())

save(MV_mortality_train, MV_mortality_test, file=paste0(indir, "MV_mortality_20JUL2021.RData"))

#save(MV_mortality_MVstart_train, MV_mortality_test, file=paste0(indir, "MV_mortality_MVstart_20JUL2021.RData"))


save(revisonDta0,file=here("data\\Final data\\revisonDta0.RData"))


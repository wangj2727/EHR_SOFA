### Project: EHR SOFA
### Purpose: create analysis ready data sets for primary and sensitivity analysis (prediction of mortality among ventilated)
### Date: 20JUL2021
### Programmer: Jing Wang

library(dplyr)
library(readr)

#setwd("/data/wangj27/SOFA/Tree/TenPct/")  ### Biowulf dir
setwd("J:\\SOFA\\Tree\\TenPct\\")   ### local dir

### import data containing indicator of ESRD (end stage renal disease) 
# ESRD <- read.csv("esrd_variables_2020q4.csv",header=TRUE, stringsAsFactors = FALSE)%>%
#   select(PAT_KEY=pat_key, esrd_poa)
# save(ESRD, file="ESRD_Ind.RData")  ### save as RData for faster data import 
load("ESRD_Ind.RData")
esrd_PID <- ESRD$PAT_KEY[ESRD$esrd_poa==1]
esrd_PID<-unique(esrd_PID)


### import data to extract indicator variable of CKD + ESRD, where CKD stands for chronic kidney disease
outcome0 <- read_csv(paste0("C:\\Users\\wangj27\\Desktop\\SOFA\\","data\\Final data\\covid_enc_variables_2020q4.csv"))
ckd_esrd_PID <- unique(outcome0$PAT_KEY[outcome0$ckd_poa_cdc==1])


# load the compiled data: MV_mortality_train & MV_mortality_test, which are longitudinal data by subject over days
load("MV_mortality_20JUL2021.RData")

#check data dimension
MV_mortality_train_oppo <- MV_mortality_train%>%filter(dnr_poa==1)
MV_mortality_test_oppo <- MV_mortality_test%>%filter(dnr_poa==1)
length(unique(MV_mortality_train_oppo$PAT_KEY))
length(unique(MV_mortality_test_oppo$PAT_KEY))

### remove participants with dnr at admission
MV_mortality_train <- MV_mortality_train%>%filter(dnr_poa!=1)
MV_mortality_test <- MV_mortality_test%>%filter(dnr_poa!=1)

#check data dimension
length(unique(MV_mortality_train$PAT_KEY))
length(unique(MV_mortality_test$PAT_KEY))


### extracted SOFA score within 24 and 48 hours prior to the start of MV, as well as baseline demographic variables at admission
FirstMVdata.train <- NULL
FirstMVdata.test <- NULL

FirstMVdata.train <-MV_mortality_train%>% 
  arrange(PAT_KEY, dayoffset)%>%
  group_by(PAT_KEY)%>%
  mutate(# SOFA score within 24 hours prior to MV
    Pre1day_total_score_num_imp = lag(total_score_num_imp,1),
    Pre1day_bilirubin_sofa_points_num_imp=lag(bilirubin_sofa_points_num_imp,1),
    Pre1day_creatinine_sofa_points_num_imp=lag(creatinine_sofa_points_num_imp,1),
    Pre1day_platelet_sofa_points_num_imp=lag(platelet_sofa_points_num_imp,1),
    Pre1day_gcs_sofa_points_num_imp=lag(gcs_sofa_points_num_imp,1),
    Pre1day_cardiovascular_sofa_points_num_imp=lag(cardiovascular_sofa_points_num_imp,1),
    Pre1day_respiratory_sofa_points_num_imp=lag(respiratory_sofa_points_num_imp,1),
    
    Pre1day_total_score_num = lag(total_score_num,1),
    Pre1day_bilirubin_sofa_points_num=lag(bilirubin_sofa_points_num,1),
    Pre1day_creatinine_sofa_points_num=lag(creatinine_sofa_points_num,1),
    Pre1day_platelet_sofa_points_num=lag(platelet_sofa_points_num,1),
    Pre1day_gcs_sofa_points_num=lag(gcs_sofa_points_num,1),
    Pre1day_cardiovascular_sofa_points_num=lag(cardiovascular_sofa_points_num,1),
    Pre1day_respiratory_sofa_points_num=lag(respiratory_sofa_points_num,1),
    # SOFA score within 48 hours prior to MV
    Pre2day_total_score_num_imp = lag(total_score_num_imp,2),
    Pre2day_bilirubin_sofa_points_num_imp=lag(bilirubin_sofa_points_num_imp,2),
    Pre2day_creatinine_sofa_points_num_imp=lag(creatinine_sofa_points_num_imp,2),
    Pre2day_platelet_sofa_points_num_imp=lag(platelet_sofa_points_num_imp,2),
    Pre2day_gcs_sofa_points_num_imp=lag(gcs_sofa_points_num_imp,2),
    Pre2day_cardiovascular_sofa_points_num_imp=lag(cardiovascular_sofa_points_num_imp,2),
    Pre2day_respiratory_sofa_points_num_imp=lag(respiratory_sofa_points_num_imp,2)
  )%>%
  ungroup()%>%
  filter(mv_yn=="Y")%>%
  group_by(PAT_KEY) %>% 
  slice(1)%>%
  rename(Hyptertension = HTN, Diabetes = diabetes_poa_cdc, ElixhauserScore = score)%>%
  mutate(FourGrp_Pre1day_total_score_num_imp = case_when(
    is.na(Pre1day_total_score_num_imp) ~ NA_character_,
    Pre1day_total_score_num_imp<6 ~ "sofa <6",
    Pre1day_total_score_num_imp>=6 & Pre1day_total_score_num_imp<9 ~ "6 >= sofa <9",
    Pre1day_total_score_num_imp>=9 & Pre1day_total_score_num_imp<12 ~ "9 >= sofa <12",
    Pre1day_total_score_num_imp>=12 ~ "sofa >= 12"
  ),
  FourGrp_Pre1day_total_score_num_imp = factor(FourGrp_Pre1day_total_score_num_imp, levels=c("sofa <6","6 >= sofa <9","9 >= sofa <12","sofa >= 12")),
  death = factor(death, levels = c("discharged", "deceased")))%>%
  ungroup()  # discharged or alive serves as the reference group




FirstMVdata.test <-MV_mortality_test%>% 
  arrange(PAT_KEY, dayoffset)%>%
  group_by(PAT_KEY)%>%
  mutate(# SOFA score within 24 hours prior to MV
    Pre1day_total_score_num_imp = lag(total_score_num_imp,1),
    Pre1day_bilirubin_sofa_points_num_imp=lag(bilirubin_sofa_points_num_imp,1),
    Pre1day_creatinine_sofa_points_num_imp=lag(creatinine_sofa_points_num_imp,1),
    Pre1day_platelet_sofa_points_num_imp=lag(platelet_sofa_points_num_imp,1),
    Pre1day_gcs_sofa_points_num_imp=lag(gcs_sofa_points_num_imp,1),
    Pre1day_cardiovascular_sofa_points_num_imp=lag(cardiovascular_sofa_points_num_imp,1),
    Pre1day_respiratory_sofa_points_num_imp=lag(respiratory_sofa_points_num_imp,1),
    
    Pre1day_total_score_num = lag(total_score_num,1),
    Pre1day_bilirubin_sofa_points_num=lag(bilirubin_sofa_points_num,1),
    Pre1day_creatinine_sofa_points_num=lag(creatinine_sofa_points_num,1),
    Pre1day_platelet_sofa_points_num=lag(platelet_sofa_points_num,1),
    Pre1day_gcs_sofa_points_num=lag(gcs_sofa_points_num,1),
    Pre1day_cardiovascular_sofa_points_num=lag(cardiovascular_sofa_points_num,1),
    Pre1day_respiratory_sofa_points_num=lag(respiratory_sofa_points_num,1),
    
    # SOFA score within 48 hours prior to MV
    Pre2day_total_score_num_imp = lag(total_score_num_imp,2),
    Pre2day_bilirubin_sofa_points_num_imp=lag(bilirubin_sofa_points_num_imp,2),
    Pre2day_creatinine_sofa_points_num_imp=lag(creatinine_sofa_points_num_imp,2),
    Pre2day_platelet_sofa_points_num_imp=lag(platelet_sofa_points_num_imp,2),
    Pre2day_gcs_sofa_points_num_imp=lag(gcs_sofa_points_num_imp,2),
    Pre2day_cardiovascular_sofa_points_num_imp=lag(cardiovascular_sofa_points_num_imp,2),
    Pre2day_respiratory_sofa_points_num_imp=lag(respiratory_sofa_points_num_imp,2)
  )%>%
  ungroup()%>%
  filter(mv_yn=="Y")%>%
  group_by(PAT_KEY) %>% 
  slice(1)%>%
  rename(Hyptertension = HTN, Diabetes = diabetes_poa_cdc, ElixhauserScore = score)%>%
  mutate(FourGrp_Pre1day_total_score_num_imp = case_when(
    is.na(Pre1day_total_score_num_imp) ~ NA_character_,
    Pre1day_total_score_num_imp<6 ~ "sofa <6",
    Pre1day_total_score_num_imp>=6 & Pre1day_total_score_num_imp<9 ~ "6 >= sofa <9",
    Pre1day_total_score_num_imp>=9 & Pre1day_total_score_num_imp<12 ~ "9 >= sofa <12",
    Pre1day_total_score_num_imp>=12 ~ "sofa >= 12"
  ),
  FourGrp_Pre1day_total_score_num_imp = factor(FourGrp_Pre1day_total_score_num_imp, levels=c("sofa <6","6 >= sofa <9","9 >= sofa <12","sofa >= 12")),
  death = factor(death, levels = c("discharged", "deceased")))%>%
  ungroup()



FirstMV.select <- FirstMVdata.train %>% 
  select(PAT_KEY, AGE, GENDER, contains("num_imp"), contains("_num"),Hyptertension , Obesity, Diabetes, ElixhauserScore, death)%>%
  ungroup()


### remove participants who had missing SOFA score that are within 24 hours prior to MV
#table(FirstMVdata.train$dayoffset[is.na(FirstMVdata.train$Pre1day_total_score_num_imp)])
FirstMVdata.test<-filter(FirstMVdata.test, !is.na(Pre1day_total_score_num_imp))
FirstMVdata.train<-filter(FirstMVdata.train, !is.na(Pre1day_total_score_num_imp))
FirstMV.select<-filter(FirstMV.select, !is.na(Pre1day_total_score_num_imp))

dim(FirstMVdata.train)
dim(FirstMVdata.test)


### create data for sensitivity analysis by removing patients with ESRD
FirstMVdata.train.NotESRD <- FirstMVdata.train%>%filter(!PAT_KEY %in% esrd_PID)
FirstMVdata.test.NotESRD <- FirstMVdata.test%>%filter(!PAT_KEY %in% esrd_PID)
FirstMV.select.NotESRD <- FirstMV.select%>%filter(!PAT_KEY %in% esrd_PID)

dim(FirstMVdata.train.NotESRD)
dim(FirstMVdata.test.NotESRD)

### create data for sensitivity analysis by removing patients with ESRD
FirstMVdata.train.NotCKDESRD <- FirstMVdata.train%>%filter(!PAT_KEY %in% ckd_esrd_PID)
FirstMVdata.test.NotCKDESRD <- FirstMVdata.test%>%filter(!PAT_KEY %in% ckd_esrd_PID)
FirstMV.select.NotCKDESRD <- FirstMV.select%>%filter(!PAT_KEY %in% ckd_esrd_PID)

dim(FirstMVdata.train.NotCKDESRD)
dim(FirstMVdata.test.NotCKDESRD)

obj.list <- ls()[stringr::str_detect(ls(), "FirstMV")]
rm(list=setdiff(ls(), obj.list))

save.image(file="FirstMV_20JUL2021_final.RData")


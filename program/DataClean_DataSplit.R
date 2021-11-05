library(dplyr)
library(tidyr)
library(tibble)
library(rsample)
library(stringr)
library(labelled)
library(readr)
library(readxl)

set.seed(202105)

indir <- "C:\\Users\\wangj27\\Desktop\\SOFA\\"

# datacsv <- read_csv(paste0(indir, "data\\Final data\\byday_covid_2020q4.csv"))  ## size=320MB
# saveRDS(datacsv, paste0(indir,"data\\byday_covid_2020q4.Rds"))  ### to speed up data importing, convert .csv to .Rds


#############################
###
### By Day data
###
##############################
dta0 <- readRDS(paste0(indir,"data\\Final data\\byday_covid_2020q4.Rds")) 

# # of obs=171290  (new N=1036419 -- 6 times)
# check # encounters
# length(unique(dta0$encounterid))  ---- N=15954 (new N=109285 - 7 times)
# check # tenant
#length(unique(dta0$tenant))  --- N=57 (new N=86)

### incorporate labels from data dictionary data
dta0_dict <- read_excel(paste0(indir,"data\\Final data\\byday data dictionary.xlsx"))
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
outcome0 <- read_csv(paste0(indir,"data\\Final data\\covid_enc_variables_2020q4.csv"))
# check # encounters
#length(unique(outcome0$PAT_KEY))  ---- N=15954 (new N=109285)
# check # tenant
#length(unique(outcome0$PROV_ID))  --- N=57 (new N=86)
# check # medical records
#length(unique(outcome0$MEDREC_KEY))  ---- N=15161 (new N=101985) --- # of participants

### incorporate labels from data dictionary data
outcome0_dict <- read_excel(paste0(indir,"data\\Final data\\data dictionary_cerner_latest_v1_jw.xlsx"),
                            sheet=2)
outcome1_dict<-outcome0_dict%>%filter(Variable %in% names(outcome0))
outcome1 <- outcome0%>%select(outcome1_dict$Variable)
var_label(outcome1)<-outcome1_dict$Description


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

outcome1 <-outcome1%>%
  filter(PAT_KEY %in% selected_encouter$PAT_KEY)

### check how many pt readmitted ---- N=7300 patients had been readmitted
ReAdmitted<- outcome0%>%
  select(PAT_KEY, MEDREC_KEY)%>%
  group_by(MEDREC_KEY)%>%
  mutate(num_dups = n(),
         dup_id = row_number()) %>%
  ungroup() %>%
  filter(dup_id > 1)%>%
  distinct(MEDREC_KEY)


##########################################
###
### Split data
###
##########################################
### split data into training and testing sets with 2:1 ratio, stratify by hospitals
### number of patients within each hospitals
hosp_check <- outcome1%>%
  select(PROV_ID, PAT_KEY)%>%
  distinct(PAT_KEY, .keep_all = TRUE)%>%
  group_by(PROV_ID)%>%
  summarise(NumPT = n())

# hosp_check$NumPT[hosp_check$NumPT<=15]
#  15  5  2  1  3 11  2 10  1 15
# beeswarm::beeswarm(hosp_check$NumPT, method="center", 
#                    horizontal=TRUE, xlab="# of patients in each hospitals")

outcome2<-outcome1%>%
  left_join(hosp_check, by="PROV_ID")%>%
  mutate(Hosp_ID = ifelse(NumPT<=15, "Pooled", PROV_ID)) ### 10 hospital that have #pt <=15

# hosp_check2 <- outcome2%>%
#   select(Hosp_ID, PAT_KEY)%>%
#   distinct(PAT_KEY, .keep_all = TRUE)%>%
#   group_by(Hosp_ID)%>%
#   summarise(NumPT = n())



dta_split <- initial_split(outcome2, strata = PROV_ID,prop = 2/3)
# <Analysis/Assess/Total>
#   <67992/33993/101985>
train_data <- training(dta_split)
test_data <- testing(dta_split)





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

#save(train_data, test_data, file = paste0(indir,"output\\SOFA_train_test_data_20JUL2021.RData"))



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





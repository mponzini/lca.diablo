#Clear existing data and graphics
rm(list=ls())
graphics.off()
#Load Hmisc library
library(Hmisc)
#Read Data
data=read.csv('H:/Kyoungmi/Hagerman/Metformin_DIA-MS/Research/Data/CTSC4328ADoubleBlind-MetforminLongitudina_DATA_2023-06-02_1015.csv')
#Setting Labels

label(data$fxs_sts_id)="FXS/STS ID #"
label(data$redcap_event_name)="Event Name"
label(data$profile_gender)="Patients Gender"
label(data$profile_race___1)="Patients Race (Please check all that apply) (choice=White/Caucasian)"
label(data$profile_race___2)="Patients Race (Please check all that apply) (choice=Native Hawaiian or Other Pacific Islander)"
label(data$profile_race___3)="Patients Race (Please check all that apply) (choice=Black/ African American)"
label(data$profile_race___4)="Patients Race (Please check all that apply) (choice=Asian)"
label(data$profile_race___5)="Patients Race (Please check all that apply) (choice=American Indian or Alaska native)"
label(data$profile_ethnic)="Ethnicity (Please check one)"
label(data$cgis_soi)="Clincal Global Impression-- Severity of Illness (CGIS_SOI)"
label(data$cgi_i)="Clincal Global Impression-- Improvement (CGI-I)"
label(data$abc_comp_rev)="ABC Composite Score-- REVISED"
label(data$ss_i_irrit_rev)="Total - Subscale I (Irritability)-- REVISED"
label(data$ss_ii_leth_rev)="Total - Subscale II (Lethargy)-- REVISED"
label(data$ss_iii_stereo_rev)="Total - Subscale III (Stereotypy)--REVISED"
label(data$ss_iv_hyper_rev)="Total - Subscale IV (Hyperactivity)--REVISED"
label(data$ss_v_inapp_rev)="Total - Subscale V (Inappropriate Speech)--REVISED"
label(data$ss_vi_socavoid_rev)="Total - Subscale VI (Social Avoidance)--REVISED"
label(data$adams_manic)="Manic/Hyperactive Behavior Total"
label(data$adams_depress)="Depressed Mood Total"
label(data$adams_avoid)="Social Avoidance Total"
label(data$adams_anxious)="General Anxiety Total"
label(data$adams_obsess)="Obsessive/Compulsive Behavior Total"
label(data$pql_pf_tot)="Physical Functioning Total"
label(data$pql_ef_tot)="Emotional Functioning Total"
label(data$pql_socf_tot)="Social Functioning Total"
label(data$pql_schf_tot)="School Functioning Total"
label(data$pql_pf_tot_cor)="Physical Functioning Total Corrected"
label(data$pql_socf_tot_cor)="Social Functioning Total Corrected"
label(data$pql_schf_tot_cor)="School functioning Total Corrected"
label(data$pql_ef_tot_cor)="Emotional functioning"
label(data$fmrp_rhmc)="FMRP (relative to history mean of controls)"
label(data$tassone_cgg)="CGG repeat number"
label(data$tassone_cat)="Molecular category"
label(data$tassone_fmr1)="FMR1 mRNA levels"
label(data$tassone_cyfip1)="CYFIP1 mRNA levels"
label(data$tassone_mmp9)="MMP9 normalized ratio"
label(data$tassone_meth)="% Methylation"
#Setting Units


#Setting Factors(will create new variable for factors)
data$redcap_event_name.factor = factor(data$redcap_event_name,levels=c("baseline_visit_scr_arm_1","baseline_visit_vis_arm_1","week_1_phone_call_arm_1","week_2_phone_call_arm_1","week_3_phone_call_arm_1","week_4_phone_call_arm_1","week_8_visit_visit_arm_1","week_12_phone_call_arm_1","week_16_visit_3ear_arm_1","ongoing_arm_1"))
data$profile_gender.factor = factor(data$profile_gender,levels=c("1","2"))
data$profile_race___1.factor = factor(data$profile_race___1,levels=c("0","1"))
data$profile_race___2.factor = factor(data$profile_race___2,levels=c("0","1"))
data$profile_race___3.factor = factor(data$profile_race___3,levels=c("0","1"))
data$profile_race___4.factor = factor(data$profile_race___4,levels=c("0","1"))
data$profile_race___5.factor = factor(data$profile_race___5,levels=c("0","1"))
data$profile_ethnic.factor = factor(data$profile_ethnic,levels=c("1","2"))
data$cgis_soi.factor = factor(data$cgis_soi,levels=c("0","1","2","3","4","5","6","7"))
data$cgi_i.factor = factor(data$cgi_i,levels=c("1","2","3","4","5","6","7","999","888"))

levels(data$redcap_event_name.factor)=c("Baseline Visit (Screen Fail)","Baseline Visit (Visit 1)","Week 1 Phone Call","Week 2 Phone Call","Week 3 Phone Call","Week 4 Phone Call","Week 8 Visit (Visit 2)","Week 12 Phone Call","Week 16 (Visit 3/Early Term)","Ongoing")
levels(data$profile_gender.factor)=c("Female (1)","Male (2)")
levels(data$profile_race___1.factor)=c("Unchecked","Checked")
levels(data$profile_race___2.factor)=c("Unchecked","Checked")
levels(data$profile_race___3.factor)=c("Unchecked","Checked")
levels(data$profile_race___4.factor)=c("Unchecked","Checked")
levels(data$profile_race___5.factor)=c("Unchecked","Checked")
levels(data$profile_ethnic.factor)=c("Hispanic or Latino","Not Hispanic or Latino")
levels(data$cgis_soi.factor)=c("0 = Not Assessed","1 = Normal","2 = Borderline Ill","3 = Mildly Ill","4 = Moderately Ill","5 = Markedly Ill","6 = Severely Ill","7 = Among the Most Extremely Ill Patients")
levels(data$cgi_i.factor)=c("1=Very Much Improved","2=Much Improved","3=Minimally Improved","4=No Change","5=Minimally Worse","6=Much Worse","7=Very Much Worse","999=No Data","888=NA")

# create extra copy
raw_data <- data
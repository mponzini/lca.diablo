#Load Hmisc library
library(Hmisc)
#Read Data
data=read.csv('H:/Kyoungmi/Hagerman/Metformin_DIA-MS/Research/Data/CTSC4328ADoubleBlind-MetforminLongitudina_DATA_2023-05-16_1549.csv')
#Setting Labels

label(data$fxs_sts_id)="FXS/STS ID #"
label(data$redcap_event_name)="Event Name"
label(data$sex)="Gender"
label(data$demo_age)="Age at Visit"
label(data$race___1)="Race (choice=Caucasian)"
label(data$race___2)="Race (choice=Black or African American)"
label(data$race___3)="Race (choice=Asian)"
label(data$race___4)="Race (choice=Native American/Alaska Native)"
label(data$race___5)="Race (choice=Native Hawaiian or Other Pacific Islander)"
label(data$race___6)="Race (choice=Other)"
label(data$race___7)="Race (choice=Unknown / Not Reported)"
label(data$ethn___1)="Ethnicity (choice=Hispanic or Latino)"
label(data$ethn___2)="Ethnicity (choice=Chinese)"
label(data$ethn___3)="Ethnicity (choice=Indian)"
label(data$ethn___4)="Ethnicity (choice=Japanese)"
label(data$ethn___5)="Ethnicity (choice=Mixed Ethnicity (Specify))"
label(data$ethn___6)="Ethnicity (choice=Other)"
label(data$ethn___7)="Ethnicity (choice=Unknown / Not Reported)"
label(data$snap_adhd_combtot)="ADHD Combined Total"
label(data$snap_odd_tot)="ODD Total"
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
label(data$vabs3_ac_ss)="Adaptive Behavior Composite Standard Score"
label(data$vabs3_c_ss)="Communication Standard Score"
label(data$vabs3_dl_ss)="Daily Living Skills Standard Score"
label(data$vabs3_s_ss)="Socialization Standard Score"
label(data$nar_ndw50_a)="Number of Different Words 50 - Analysis Set"
label(data$con_ndw50_a)="Number of Different Words 50 - Analysis Set"
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
data$sex.factor = factor(data$sex,levels=c("1","2"))
data$race___1.factor = factor(data$race___1,levels=c("0","1"))
data$race___2.factor = factor(data$race___2,levels=c("0","1"))
data$race___3.factor = factor(data$race___3,levels=c("0","1"))
data$race___4.factor = factor(data$race___4,levels=c("0","1"))
data$race___5.factor = factor(data$race___5,levels=c("0","1"))
data$race___6.factor = factor(data$race___6,levels=c("0","1"))
data$race___7.factor = factor(data$race___7,levels=c("0","1"))
data$ethn___1.factor = factor(data$ethn___1,levels=c("0","1"))
data$ethn___2.factor = factor(data$ethn___2,levels=c("0","1"))
data$ethn___3.factor = factor(data$ethn___3,levels=c("0","1"))
data$ethn___4.factor = factor(data$ethn___4,levels=c("0","1"))
data$ethn___5.factor = factor(data$ethn___5,levels=c("0","1"))
data$ethn___6.factor = factor(data$ethn___6,levels=c("0","1"))
data$ethn___7.factor = factor(data$ethn___7,levels=c("0","1"))

levels(data$redcap_event_name.factor)=c("Baseline Visit (Screen Fail)","Baseline Visit (Visit 1)","Week 1 Phone Call","Week 2 Phone Call","Week 3 Phone Call","Week 4 Phone Call","Week 8 Visit (Visit 2)","Week 12 Phone Call","Week 16 (Visit 3/Early Term)","Ongoing")
levels(data$sex.factor)=c("Male","Female")
levels(data$race___1.factor)=c("Unchecked","Checked")
levels(data$race___2.factor)=c("Unchecked","Checked")
levels(data$race___3.factor)=c("Unchecked","Checked")
levels(data$race___4.factor)=c("Unchecked","Checked")
levels(data$race___5.factor)=c("Unchecked","Checked")
levels(data$race___6.factor)=c("Unchecked","Checked")
levels(data$race___7.factor)=c("Unchecked","Checked")
levels(data$ethn___1.factor)=c("Unchecked","Checked")
levels(data$ethn___2.factor)=c("Unchecked","Checked")
levels(data$ethn___3.factor)=c("Unchecked","Checked")
levels(data$ethn___4.factor)=c("Unchecked","Checked")
levels(data$ethn___5.factor)=c("Unchecked","Checked")
levels(data$ethn___6.factor)=c("Unchecked","Checked")
levels(data$ethn___7.factor)=c("Unchecked","Checked")

# create extra copy
raw_data <- data
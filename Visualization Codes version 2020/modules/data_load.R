#upload example datasets
accord_data_analysis<-read.csv("modules/data/accord_count_data.csv")
accord_data_analysis$X<-NULL
allhat_data_analysis<-read.csv("modules/data/allhat_count_data.csv")
allhat_data_analysis$X<-NULL
sprint_data_analysis<-read.csv("modules/data/sprint_count_data.csv")
sprint_data_analysis$X<-NULL


#upload background datasets
NHANES_diabetes<-read.csv("modules/data/nhanes_diabetes.csv")
NHANES_diabetes$X<-NULL
NHANES_diabetes_lab<-read.csv("modules/data/nhanes_diabetes_lab.csv")
NHANES_diabetes_lab$X<-NULL
NHANES_diabetes_combined<-read.csv("modules/data/nhanes_diabetes_combined.csv")
NHANES_diabetes_combined$X<-NULL

NHANES_hypertension<-read.csv("modules/data/nhanes_hypertension.csv")
NHANES_hypertension$X<-NULL
NHANES_hypertension_lab<-read.csv("modules/data/nhanes_hypertension_lab.csv")
NHANES_hypertension_lab$X<-NULL
NHANES_hypertension_combined<-read.csv("modules/data/nhanes_hypertension_combined.csv")
NHANES_hypertension_combined$X<-NULL


accord_data_analysis$Race_or_Ethnicity_D <- factor(accord_data_analysis$Race_or_Ethnicity_D, levels = c("NH White", "NH Black", "Hispanic", "Other"))
accord_data_analysis$Education_D <- factor(accord_data_analysis$Education_D, levels = c("<HSG", "HSG/GED", "Some college/TS", ">=College grad"))
accord_data_analysis$BMI_R <- factor(accord_data_analysis$BMI_R, levels = c("Underweight", "Normal weight", "Overweight", "Obese"))
accord_data_analysis$SBP_R <- factor(accord_data_analysis$SBP_R, levels = c("SBP<120", "SBP 120-129", "SBP 130-139", "SBP>=140"))
accord_data_analysis$FPG_L <- factor(accord_data_analysis$FPG_L, levels = c("Glucose<100", "Glucose 100-125", "Glucose>=126"))

allhat_data_analysis$Race_or_Ethnicity_D <- factor(allhat_data_analysis$Race_or_Ethnicity_D, levels = c("NH White", "NH Black", "NH Asian", "Hispanic", "Other"))
allhat_data_analysis$Education_D <- factor(allhat_data_analysis$Education_D, levels = c("<HSG","HSG/GED", "Some college/TS", ">=College grad"))
allhat_data_analysis$BMI_R <- factor(allhat_data_analysis$BMI_R, levels = c("Underweight", "Normal weight", "Overweight", "Obese"))
allhat_data_analysis$SBP_R <- factor(allhat_data_analysis$SBP_R, levels = c("SBP<120", "SBP 120-129", "SBP 130-139", "SBP>=140"))
allhat_data_analysis$FPG_L <- factor(allhat_data_analysis$FPG_L, levels = c("Glucose<100", "Glucose 100-125", "Glucose>=126"))

sprint_data_analysis$Race_or_Ethnicity_D <- factor(sprint_data_analysis$Race_or_Ethnicity_D, levels = c("NH White", "NH Black", "NH Asian", "Hispanic", "Other"))
sprint_data_analysis$Education_D <- factor(sprint_data_analysis$Education_D, levels = c("<HSG","HSG/GED", "Some college/TS", ">=College grad"))
sprint_data_analysis$BMI_R <- factor(sprint_data_analysis$BMI_R, levels = c("Underweight", "Normal weight", "Overweight", "Obese"))
sprint_data_analysis$SBP_R <- factor(sprint_data_analysis$SBP_R, levels = c("SBP<120", "SBP 120-129", "SBP 130-139", "SBP>=140"))
sprint_data_analysis$FPG_L <- factor(sprint_data_analysis$FPG_L, levels = c("Glucose<100", "Glucose 100-125", "Glucose>=126"))

NHANES_diabetes$Gender_D <- factor(NHANES_diabetes$Gender_D, levels = c("Male","Female"))
NHANES_diabetes$Race_or_Ethnicity_D <- factor(NHANES_diabetes$Race_or_Ethnicity_D, levels = c("NH White", "NH Black", "Hispanic", "Other"))
NHANES_diabetes$Education_D <- factor(NHANES_diabetes$Education_D, levels = c("<HSG", "HSG/GED", "Some college/TS", ">=College grad"))
NHANES_diabetes$BMI_R <- factor(NHANES_diabetes$BMI_R, levels = c("Underweight", "Normal weight", "Overweight", "Obese"))
NHANES_diabetes$SBP_R <- factor(NHANES_diabetes$SBP_R, levels = c("SBP<120", "SBP 120-129", "SBP 130-139", "SBP>=140"))
NHANES_diabetes_lab$FPG_L <- factor(NHANES_diabetes_lab$FPG_L, levels = c("Glucose<100", "Glucose 100-125", "Glucose>=126"))

NHANES_hypertension$Gender_D <- factor(NHANES_hypertension$Gender_D, levels = c("Male","Female"))
NHANES_hypertension$Race_or_Ethnicity_D <- factor(NHANES_hypertension$Race_or_Ethnicity_D, levels = c("NH White", "NH Black", "NH Asian", "Hispanic", "Other"))
NHANES_hypertension$Education_D <- factor(NHANES_hypertension$Education_D, levels = c("<HSG","HSG/GED", "Some college/TS", ">=College grad"))
NHANES_hypertension$BMI_R <- factor(NHANES_hypertension$BMI_R, levels = c("Underweight", "Normal weight", "Overweight", "Obese"))
NHANES_hypertension$SBP_R <- factor(NHANES_hypertension$SBP_R, levels = c("SBP<120", "SBP 120-129", "SBP 130-139", "SBP>=140"))
NHANES_hypertension_lab$FPG_L <- factor(NHANES_hypertension_lab$FPG_L, levels = c("Glucose<100", "Glucose 100-125", "Glucose>=126"))

NHANES_diabetes_combined$Race_or_Ethnicity_D <- factor(NHANES_diabetes_combined$Race_or_Ethnicity_D, levels = c("NH White", "NH Black", "NH Asian", "Hispanic", "Other"))
NHANES_diabetes_combined$Education_D <- factor(NHANES_diabetes_combined$Education_D, levels = c("<HSG","HSG/GED", "Some college/TS", ">=College grad"))
NHANES_diabetes_combined$BMI_R <- factor(NHANES_diabetes_combined$BMI_R, levels = c("Underweight", "Normal weight", "Overweight", "Obese"))
NHANES_diabetes_combined$SBP_R <- factor(NHANES_diabetes_combined$SBP_R, levels = c("SBP<120", "SBP 120-129", "SBP 130-139", "SBP>=140"))
NHANES_diabetes_combined$FPG_L <- factor(NHANES_diabetes_combined$FPG_L, levels = c("Glucose<100", "Glucose 100-125", "Glucose>=126"))


NHANES_hypertension_combined$Race_or_Ethnicity_D <- factor(NHANES_hypertension_combined$Race_or_Ethnicity_D, levels = c("NH White", "NH Black", "NH Asian", "Hispanic", "Other"))
NHANES_hypertension_combined$Education_D <- factor(NHANES_hypertension_combined$Education_D, levels = c("<HSG","HSG/GED", "Some college/TS", ">=College grad"))
NHANES_hypertension_combined$BMI_R <- factor(NHANES_hypertension_combined$BMI_R, levels = c("Underweight", "Normal weight", "Overweight", "Obese"))
NHANES_hypertension_combined$SBP_R <- factor(NHANES_hypertension_combined$SBP_R, levels = c("SBP<120", "SBP 120-129", "SBP 130-139", "SBP>=140"))
NHANES_hypertension_combined$FPG_L <- factor(NHANES_hypertension_combined$FPG_L, levels = c("Glucose<100", "Glucose 100-125", "Glucose>=126"))

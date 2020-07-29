make_table_combine<-function(background_info,RCT_info,background_info_lab,x, sig_t, cutoff1,cutoff2) {
  nonlab_factors<-colnames(select(RCT_info, ends_with("_D")|ends_with( "_R")))
  lab_factors<-colnames(select(RCT_info, ends_with("_L")))
  
  num_nonlab<- length(nonlab_factors)
  num_lab<-length(lab_factors)
  
  background_info_df<-background_info %>%
    group_by(.dots = nonlab_factors[1]) %>% 
    summarise(background_n = sum(background_n))
  names(background_info_df)[1] <- "Group_Name"
  background_info_df<-na.omit(background_info_df)
  total_population_background<-sum(as.integer(background_info_df$background_n))
  background_info_df$total_background<-rep(total_population_background, nrow(background_info_df))

  user_info_df<-RCT_info%>%
    group_by(.dots = nonlab_factors[1]) %>% 
    summarise(user_n = sum(user_n))
  names(user_info_df)[1] <- "Group_Name"
  user_info_df<-na.omit(user_info_df)
  total_population_user<-sum(as.integer(user_info_df$user_n))
  user_info_df$total_user<-rep(total_population_user, nrow(user_info_df))
  
  for (i in 2:num_nonlab){
    background_info_df_var<-background_info %>%
      group_by(.dots = nonlab_factors[i]) %>% 
      summarise(background_n = sum(background_n))
    names(background_info_df_var)[1] <- "Group_Name"
    background_info_df_var<-na.omit(background_info_df_var)
    total_population_background_var<-sum(as.integer(background_info_df_var$background_n))
    background_info_df_var$total_background<-rep(total_population_background_var, nrow(background_info_df_var))
    
    user_info_df_var<-RCT_info%>%
      group_by(.dots = nonlab_factors[i]) %>% 
      summarise(user_n = sum(user_n))
    names(user_info_df_var)[1] <- "Group_Name"
    user_info_df_var<-na.omit(user_info_df_var)
    total_population_user_var<-sum(as.integer(user_info_df_var$user_n))
    user_info_df_var$total_user<-rep(total_population_user, nrow(user_info_df_var))
    
    background_info_df<-plyr::rbind.fill(background_info_df, background_info_df_var) 
    user_info_df<-plyr::rbind.fill(user_info_df, user_info_df_var) 
  }
  
  for (ii in 1:num_lab){
    background_info_df_var<-background_info_lab %>%
      group_by(.dots = lab_factors[ii]) %>% 
      summarise(background_n = sum(background_n))
    names(background_info_df_var)[1] <- "Group_Name"
    background_info_df_var<-na.omit(background_info_df_var)
    total_population_background_var<-sum(as.integer(background_info_df_var$background_n))
    background_info_df_var$total_background<-rep(total_population_background_var, nrow(background_info_df_var))
    
    user_info_df_var<-RCT_info%>%
      group_by(.dots = lab_factors[ii]) %>% 
      summarise(user_n = sum(user_n))
    names(user_info_df_var)[1] <- "Group_Name"
    user_info_df_var<-na.omit(user_info_df_var)
    total_population_user_var<-sum(as.integer(user_info_df_var$user_n))
    user_info_df_var$total_user<-rep(total_population_user, nrow(user_info_df_var))
    
    background_info_df<-plyr::rbind.fill(background_info_df, background_info_df_var) 
    user_info_df<-plyr::rbind.fill(user_info_df, user_info_df_var) 
  }
  
  
  merged_df<-merge(background_info_df, user_info_df, by="Group_Name", all=TRUE)
  
  
  merged_df$user_n<-merged_df$user_n %>% replace_na(0)
  merged_df$background_n<-merged_df$background_n %>% replace_na(0)
  
  merged_df$total_user<-merged_df$total_user %>% replace_na(0)
  merged_df$total_background<-merged_df$total_background %>% replace_na(0)
  
  
  merged_df$participant_rate<- mapply(Rate_Calculation,as.integer(merged_df$total_user),as.integer(merged_df$total_background))
  
  #calculate the observed & background rates 
  merged_df$Observed_Rate<- mapply(Rate_Calculation,as.integer(merged_df$user_n),as.integer(merged_df$total_user))
  merged_df$Background_Rate<- mapply(Rate_Calculation,as.integer(merged_df$background_n),as.integer(merged_df$total_background))
  
  merged_df$pValue<- mapply(compare_population_proportion,as.numeric(merged_df$Background_Rate),as.integer(merged_df$user_n),as.integer(merged_df$total_user))
  merged_df$BH_p<- p.adjust(merged_df$pValue,method = "BH")
  
  merged_df$whether_significant<- mapply(whether_significant,as.numeric(merged_df$BH_p),sig_t)
  
  merged_df$AEOEquityValue<- mapply(Adjusted_Equal_Opportunity,as.numeric(merged_df$Background_Rate),as.numeric(merged_df$Observed_Rate), for_plot = FALSE)

  merged_df$LDIEquityValue<- mapply(Log_Disparate_Impact,as.numeric(merged_df$Background_Rate),as.numeric(merged_df$Observed_Rate), for_plot = FALSE)
  merged_df$LDIEquityLable<-mapply(whether_biased_label, as.numeric(merged_df$LDIEquityValue),as.numeric(merged_df$BH_p),sig_t,cutoff1,cutoff2,cutoff2)
  
  merged_df$QMEquityValue<- mapply(Quality_Metric,as.numeric(merged_df$Background_Rate),as.numeric(merged_df$Observed_Rate), for_plot = FALSE)

  merged_df<-merged_df %>%mutate(Group_Name =  factor(Group_Name, levels = x)) %>%arrange(Group_Name)
  
  
  
  new_df_univariable<- data.frame(Group_Name = merged_df$Group_Name,
                                  BH_p =  merged_df$BH_p,
                                  Significant_Level =  merged_df$whether_significant,
                                  LDI_Value= merged_df$LDIEquityValue,
                                  LDI_Level= merged_df$LDIEquityLable,
                                  stringsAsFactors=FALSE)
  
  new_df_univariable<-new_df_univariable%>% mutate_if(is.numeric, round, digits=5)
  
  return(new_df_univariable)
  
}

preprocess_comparison_df<-function(sig_t, cut1,cut2){
  y1_attributes<-c("Female", "Male","18-44", "45-64","64+","NH White","NH Black" ,"Hispanic","Other","<HSG" ,"HSG/GED","Some college/TS",">=College grad","Smoke","No smoke", "Underweight","Normal weight","Overweight","Obese","SBP<120","SBP 120-129","SBP 130-139","SBP>=140","Normal TC","High TC","Glucose<100","Glucose 100-125","Glucose>=126")
  
  y2_attributes<-c("Female", "Male","18-39", "40-59","59+","NH White","NH Black","NH Asian","Hispanic","Other","<HSG" ,"HSG/GED","Some college/TS",">=College grad","Smoke","No smoke", "Underweight","Normal weight","Overweight","Obese","SBP<120","SBP 120-129","SBP 130-139","SBP>=140","Normal TC","High TC","Glucose<100","Glucose 100-125","Glucose>=126")
  
  accord_univariable_combine<-make_table_combine(NHANES_diabetes, accord_data_analysis, NHANES_diabetes_lab,y1_attributes,sig_t,-log(1-cut1), -log(1-cut2) )
  allhat_univariable_combine<-make_table_combine(NHANES_hypertension, allhat_data_analysis, NHANES_hypertension_lab,y2_attributes,sig_t,-log(1-cut1), -log(1-cut2))
  sprint_univariable_combine<-make_table_combine(NHANES_hypertension, sprint_data_analysis, NHANES_hypertension_lab,y2_attributes, sig_t,-log(1-cut1), -log(1-cut2))
  
  
  
  
  levels(accord_univariable_combine$Group_Name)[levels(accord_univariable_combine$Group_Name)=="18-44"] <- "18-44/18-39"
  levels(accord_univariable_combine$Group_Name)[levels(accord_univariable_combine$Group_Name)=="45-64"] <- "45-64/40-59"
  levels(accord_univariable_combine$Group_Name)[levels(accord_univariable_combine$Group_Name)=="64+"] <- "64+/59+"
  levels(accord_univariable_combine$Group_Name) <- c(levels(accord_univariable_combine$Group_Name), "NH Asian")
  New <- c("NH Asian",  NA,NA,NA,NA )
  accord_univariable_combine <- InsertRow(accord_univariable_combine, NewRow = New, RowNum = 8)
  
  levels(allhat_univariable_combine$Group_Name)[levels(allhat_univariable_combine$Group_Name)=="18-39"] <- "18-44/18-39"
  levels(allhat_univariable_combine$Group_Name)[levels(allhat_univariable_combine$Group_Name)=="40-59"] <- "45-64/40-59"
  levels(allhat_univariable_combine$Group_Name)[levels(allhat_univariable_combine$Group_Name)=="59+"] <- "64+/59+"
  
  levels(sprint_univariable_combine$Group_Name)[levels(sprint_univariable_combine$Group_Name)=="18-39"] <- "18-44/18-39"
  levels(sprint_univariable_combine$Group_Name)[levels(sprint_univariable_combine$Group_Name)=="40-59"] <- "45-64/40-59"
  levels(sprint_univariable_combine$Group_Name)[levels(sprint_univariable_combine$Group_Name)=="59+"] <- "64+/59+"
  
  colnames(accord_univariable_combine)[2:5] <- paste(colnames(accord_univariable_combine)[2:5],"(1)",  sep = "")
  colnames(allhat_univariable_combine)[2:5] <- paste(colnames(allhat_univariable_combine)[2:5],"(2)",  sep = "")
  colnames(sprint_univariable_combine)[2:5] <- paste(colnames(sprint_univariable_combine)[2:5],"(3)",  sep = "")
  
  df_comparison_studies<-merge(accord_univariable_combine,allhat_univariable_combine, by = "Group_Name")
  df_comparison_studies<-merge(df_comparison_studies,sprint_univariable_combine, by = "Group_Name")
  
  y3_attributes<-c("Female", "Male","18-44/18-39", "45-64/40-59","64+/59+","NH White","NH Black","NH Asian","Hispanic","Other","<HSG" ,"HSG/GED","Some college/TS",">=College grad","Smoke","No smoke", "Underweight","Normal weight","Overweight","Obese","SBP<120","SBP 120-129","SBP 130-139","SBP>=140","Normal TC","High TC","Glucose<100","Glucose 100-125","Glucose>=126")
  
  df_comparison_studies_new<-df_comparison_studies %>%mutate(Group_Name =  factor(Group_Name, levels = y3_attributes)) %>%arrange(Group_Name)
  
  df_comparison_studies_new$Group_Name<-c("Female","Male","18-44/18-39","45-64/40-59","64+/59+","Non-Hispanic White","Non-Hispanic Black","Non-Hispanic Asian","Hispanic","Other","Less than high school","High-school graduate","Some college/Technical school","College degree or higher","Current smoker" ,"Not smoke","Underweight","Normal weight","Overweight","Obese","less than 120","120-129","130-139","greater than 139","Normal","High","less than 100","100-125","greater than 125")
  
  df_comparison_studies_new<-df_comparison_studies_new[c(1,4,8,12)]
  return(df_comparison_studies_new)
}




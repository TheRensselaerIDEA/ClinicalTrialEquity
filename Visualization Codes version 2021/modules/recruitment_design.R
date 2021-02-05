# find the lower bound using significance threshold
find_LCI_new<-function(num_g,total_g, sig_t, method_name){
  if (total_g == 0 | num_g == 0){return(NA)}
  return(prop.test(x=num_g, n=total_g, alternative = method_name,
                   conf.level = 1-sig_t)$conf.int[1])
}

# find the upper bound using significance threshold
find_HCI_new<-function(num_g,total_g, sig_t,method_name){
  if (total_g == 0 | num_g == 0){return(NA)}
  return(prop.test(x=num_g, n=total_g, alternative = method_name,
                   conf.level = 1-sig_t)$conf.int[2])
}

# find the observed rate based on the metric threshold
Log_Disparate_Impact_reverse<-function (background_rate_di, threshold_di){
  b_L<-threshold_di+log(background_rate_di/(1-background_rate_di))
  e_L<-exp(b_L)
  observed_bound_L<- e_L/(1+e_L)
  return(observed_bound_L)
}


Adjusted_Equal_Opportunity_reverse<-function (background_rate_aeo, threshold_aeo){
  observed_bound_L<-threshold_aeo * background_rate_aeo*(1-background_rate_aeo)+background_rate_aeo
  return(observed_bound_L)
}


Quality_Metric_reverse<-function (background_rate_qm, threshold_qm){
  b_L<-threshold_qm+log(background_rate_qm)
  observed_bound_L<-exp(b_L)
  return(observed_bound_L)
}


Log_Disparate_Impact_plan<-function (alpha_di, x,n ){
  if (alpha_di == 0){
    return (7777)
  }
  else if (x == 0){
    return(9999)
  }
  
  if (!(x%%1==0)){
    return(8888)
  }
  
  beta_di<-x/n
  left_di <- beta_di/(1-beta_di)
  right_di <- alpha_di/(1-alpha_di)
  result_di <- log(left_di)-log(right_di)
  return (abs(result_di))
}


obtain_RCT_info<-function(old_RCT, var_list){
  cols<-var_list
  df_processed<-old_RCT
  df_processed[cols] <- lapply(df_processed[cols], factor)
  
  df_selected<-subset(df_processed, select=c(var_list,"user_n"))
  df_selected<-na.omit(df_selected)
  
  
  num_vars<-length(var_list)
  df_cleaned<- df_selected %>%
    group_by(.dots = var_list,.drop = FALSE) %>% 
    summarise(user_n = sum(user_n))
  
  total_n<- sum(df_cleaned$user_n)
  
  if (num_vars>1){
    for (index_var in 1:(num_vars-1)){
      current_vars<-combinations(num_vars,index_var,var_list)
      for (index_var2 in 1:nrow(current_vars)){
        df_current<-df_selected %>%
          group_by(.dots = current_vars[index_var2,],.drop = FALSE) %>% 
          summarise(user_n = sum(user_n))
        df_cleaned <- dplyr::bind_rows(df_cleaned, df_current)
      }
    }
  }
  
  df_cleaned$total_RCT<-rep(total_n, nrow(df_cleaned))
  df_cleaned$RCT_rate<-df_cleaned$user_n/total_n
  df_cleaned$all_count <- apply(df_cleaned[,1:num_vars,drop=F], 1, function(x) sum(is.na(x)))
  
  return(df_cleaned)
}




obtain_background_info<-function(bakcgroud_info, var_list){
  cols<-var_list
  df_processed<-bakcgroud_info
  df_processed[cols] <- lapply(df_processed[cols], factor)
  
  df_selected<-subset(df_processed, select=c(var_list,"background_n"))
  df_selected<-na.omit(df_selected)
  
  
  num_vars<-length(var_list)
  df_cleaned<- df_selected %>%
    group_by(.dots = var_list,.drop = FALSE) %>% 
    summarise(background_n = sum(background_n))
  
  total_n<- sum(df_cleaned$background_n)
  
  if (num_vars>1){
    for (index_var in 1:(num_vars-1)){
      current_vars<-combinations(num_vars,index_var,var_list)
      for (index_var2 in 1:nrow(current_vars)){
        df_current<-df_selected %>%
          group_by(.dots = current_vars[index_var2,],.drop = FALSE) %>% 
          summarise(background_n = sum(background_n))
        df_cleaned <- dplyr::bind_rows(df_cleaned, df_current)
      }
    }
  }
  
  df_cleaned$total_ideal<-rep(total_n, nrow(df_cleaned))
  df_cleaned$ideal_rate<-df_cleaned$background_n/total_n
  df_cleaned$all_count <- apply(df_cleaned[,1:num_vars,drop=F], 1, function(x) sum(is.na(x)))
  
  return(df_cleaned)
}





reprocess_info<-function(bakcgroud_info, var_list){
  cols<-var_list
  df_processed<-bakcgroud_info
  df_processed[cols] <- lapply(df_processed[cols], factor)
  
  df_selected<-subset(df_processed, select=c(var_list,"background_n", "user_n"))
  df_selected<-na.omit(df_selected)
  
  num_vars<-length(var_list)
  df_cleaned<- df_selected %>%
    group_by(.dots = var_list,.drop = FALSE) %>% 
    summarise(background_n = sum(background_n),
              user_n = sum(user_n))
  
  
  total_n_b<- sum(df_cleaned$background_n)
  total_n_rct<- sum(df_cleaned$user_n)
  
  if (num_vars>1){
    for (index_var in 1:(num_vars-1)){
      current_vars<-combinations(num_vars,index_var,var_list)
      for (index_var2 in 1:nrow(current_vars)){
        df_current<-df_selected %>%
          group_by(.dots = current_vars[index_var2,],.drop = FALSE) %>% 
          summarise(background_n = sum(background_n),
                    user_n = sum(user_n))
        df_cleaned <- dplyr::bind_rows(df_cleaned, df_current)
      }
    }
  }
  
  
  df_cleaned$total_ideal<-rep(total_n_b, nrow(df_cleaned))
  df_cleaned$ideal_rate<-df_cleaned$background_n/total_n_b
  df_cleaned$total_RCT<-rep(total_n_rct, nrow(df_cleaned))
  df_cleaned$RCT_rate<-df_cleaned$user_n/total_n_rct
  df_cleaned$all_count <- apply(df_cleaned[,1:num_vars,drop=F], 1, function(x) sum(is.na(x)))
  return(df_cleaned)
}



assign_confidence_interval_executed<-function(given_df_with_background,given_significant,given_test_method,given_metric, given_metric_t,given_n){
  given_df_with_background[,"L_CI"] <- 0
  given_df_with_background[,"H_CI"] <- 0
  given_df_with_background[,"L_N"] <- 0
  given_df_with_background[,"H_N"] <- 0
  given_df_with_background[,"ideal_N"] <- 0
  
  
  for (i in 1:nrow(given_df_with_background)) {
    row <- given_df_with_background[i,]
    
    rate_L<- find_LCI_new(as.integer(row$background_n),as.integer(row$total_ideal),as.numeric(given_significant),given_test_method)
    rate_H<- find_HCI_new(as.integer(row$background_n),as.integer(row$total_ideal),as.numeric(given_significant),given_test_method)
    
    rate_di_1<-given_metric(row$ideal_rate,given_metric_t)
    rate_di_2<-given_metric(row$ideal_rate,-given_metric_t)
    
    CI_L<- min(rate_L,rate_H,rate_di_1,rate_di_2)
    CI_H<- max(rate_L,rate_H,rate_di_1,rate_di_2)
    
    if (is.na(CI_L)){CI_L<-0}
    if (is.na(CI_H)){CI_H<-0}
    
    given_df_with_background[i,"L_CI"]<-CI_L
    given_df_with_background[i,"H_CI"]<-CI_H
    
    interger_L<-ceiling(given_n*CI_L)
    interger_H<-floor(given_n*CI_H)
    interger_ideal<-ceiling(given_n*row$ideal_rate)
    
    
    
    
    if (interger_H-interger_L>0){
      given_df_with_background[i,"L_N"]<-interger_L
      given_df_with_background[i,"ideal_N"]<-interger_ideal
      given_df_with_background[i,"H_N"]<-interger_H
    }
    else if (interger_H == 0 |interger_L == 0 ){
      given_df_with_background[i,"L_N"]<-0
      given_df_with_background[i,"ideal_N"]<-0
      given_df_with_background[i,"H_N"]<-0
    }
    else{
      given_df_with_background[i,"L_N"]<-floor(given_n*CI_L)
      given_df_with_background[i,"ideal_N"]<-interger_ideal
      given_df_with_background[i,"H_N"]<-ceiling(given_n*CI_H)
    }
  }
  
  return(given_df_with_background) 
}

add_instructions<-function(given_df, num_small, num_sub, num_vars){
  given_df$inclusion<-NA
  
  
  smallest_subgroups_df<-given_df[given_df$all_count == 0, ]
  
  for (i in 1:num_sub){
    row<-given_df[i,]
    vars_matched<-num_vars-row$all_count
    
    #smallest subgroups
    if (row$all_count==0){
      if (is.na(row$inclusion)){
        given_df[i,"inclusion"]<- paste(i)
      }
    }
    #upper level subgroups
    else if (row$all_count > 0){
      row1_compare<-given_df[i,1:num_vars]
      for (i2 in 1:num_small){
        row2_compare<-smallest_subgroups_df[i2,1:num_vars]
        compare_result<- (row1_compare==row2_compare)
        if (length(compare_result)>0){
          n_intersect<-length(which(compare_result == TRUE))
        }
        else{
          n_intersect<--1
        }
        
        
        if (n_intersect==vars_matched){
          if (is.na(given_df[i,"inclusion"])){
            given_df[i,"inclusion"]<-paste(i2)
          }
          else{
            given_df[i,"inclusion"]<-paste(given_df[i,"inclusion"], ',',i2)
          }
        }
      }
    }
    
  }
  return(given_df)
  
}


study_recruitment_plan<-function(Background_NHANES, additional_N, variable_list, Previous_RCT,metric_name,significant_t, metric_t,test_method,equity_metric_selected){
  ideal_df<-obtain_background_info(Background_NHANES, variable_list)
  
  if (is.numeric(Previous_RCT)){
    RCT_df<-ideal_df
    RCT_df<-dplyr::rename(RCT_df, user_n = background_n, total_RCT = total_ideal, RCT_rate = ideal_rate )
    RCT_df$user_n<-0
    RCT_df$total_RCT<-0
    RCT_df$RCT_rate<-0
    
  } 
  else{
    RCT_df<-obtain_RCT_info(Previous_RCT, variable_list)
  }
  
  
  combined_info_old<- merge(RCT_df, ideal_df, by = c(variable_list, "all_count"))
  combined_info_old <- combined_info_old[combined_info_old$background_n>0,]
  df_subgroups_total<-combined_info_old[combined_info_old$all_count == 0, ]
  combined_info<-as.data.frame(reprocess_info(df_subgroups_total, variable_list))
  combined_info <- combined_info[combined_info$background_n>0,]
  
  total_n<-sum(df_subgroups_total$user_n)
  total_n_new<-total_n+additional_N
  
  
  df_info_new<-assign_confidence_interval_executed(combined_info,significant_t,test_method,metric_name,metric_t,total_n_new)
  df_info_new_ordered<-df_info_new[order(df_info_new$all_count),]
  
  
  num_vars<-length(variable_list)
  num_smallest_subgroup<- sum(df_info_new_ordered$all_count == 0)
  num_subgroups<-nrow(df_info_new_ordered)
  
  df_info_new_ordered<-add_instructions(df_info_new_ordered,num_smallest_subgroup,num_subgroups,num_vars)
  
  
  df_info_new_ordered$bound_L<- df_info_new_ordered$L_N-df_info_new_ordered$user_n
  df_info_new_ordered$bound_H<- df_info_new_ordered$H_N-df_info_new_ordered$user_n
  
  L<-df_info_new_ordered$bound_L
  H<-df_info_new_ordered$bound_H
  ideal_n<-df_info_new_ordered$ideal_N
  D1<- df_info_new_ordered$ideal_N-df_info_new_ordered$user_n
  
  obj <- c(rep(0,num_smallest_subgroup), rep(1,num_subgroups), rep(1,num_subgroups))
  
  
  num_unknowns<- num_smallest_subgroup + num_subgroups + num_subgroups
  num_eqns<-num_smallest_subgroup + num_subgroups*5+1
  
  num_x<-num_smallest_subgroup
  num_z<- num_subgroups
  num_d<-num_subgroups
  
  
  mat<- matrix(rep(0, num_eqns*num_unknowns), nrow = num_eqns, ncol = num_unknowns)
  
  for (i in 1: num_eqns){
    #lower bound check for smallest subgroups
    if (i <= num_smallest_subgroup ){
      #ith column
      mat[i,] <- c(rep(0,(i-1)), 1, rep(0,(num_smallest_subgroup-i)),rep(0,(i-1)), 1, rep(0,(num_subgroups-i)), rep(0, num_d))
    }
    
    #lower bound check for upper-level subgroups
    if (i > num_smallest_subgroup & i <= num_subgroups){
      index_list<-as.numeric(unlist(strsplit(df_info_new_ordered[i,"inclusion"],",")))
      mat[i,index_list]<-1  #all subgroups x
      mat[i,i+num_smallest_subgroup]<-1  #upper z
    }
    
    #upper bound check for smallest subgroups
    if (i > num_subgroups & i <= num_subgroups+num_smallest_subgroup){
      mat[i,] <- c(rep(0,(i-num_subgroups-1)), 1, rep(0,(num_smallest_subgroup+num_subgroups-i)),rep(0,(i-num_subgroups-1)), -1, rep(0,(num_subgroups+num_subgroups-i)),rep(0, num_d))
    }
    
    #upper bound check for upper-level subgroups
    if (i >  num_subgroups+num_smallest_subgroup & i <= num_subgroups*2){
      index_list<-as.numeric(unlist(strsplit(df_info_new_ordered[i-num_subgroups,"inclusion"],",")))
      mat[i,index_list]<-1
      mat[i,i-num_subgroups+num_smallest_subgroup]<--1
    }
    
    #x+d for smallest subgroups
    if (i > num_subgroups*2 & i <= num_subgroups*2+num_smallest_subgroup){
      mat[i,] <- c(rep(0,(i-num_subgroups*2-1)), 1, rep(0,(num_smallest_subgroup+num_subgroups*2-i)), rep(0,num_subgroups),rep(0,(i-num_subgroups*2-1)), 1, rep(0,(num_subgroups*3-i)))
    }
    
    
    if (i > num_subgroups*2+num_smallest_subgroup & i <= num_subgroups*3){
      index_list<-as.numeric(unlist(strsplit(df_info_new_ordered[i-num_subgroups*2,"inclusion"],",")))
      mat[i,index_list]<-1 #all subgroups
      mat[i,i-num_subgroups+num_smallest_subgroup]<-1 #upper d
    }
    
    if (i > num_subgroups*3 & i <= num_subgroups*3+num_smallest_subgroup){
      mat[i,] <- c(rep(0,(i-num_subgroups*3-1)), 1, rep(0,(num_smallest_subgroup+num_subgroups*3-i)),rep(0, num_subgroups),rep(0,(i-num_subgroups*3-1)), -1, rep(0,(num_subgroups*4-i)))
    }
    
    if (i > num_subgroups*3+num_smallest_subgroup & i <= num_subgroups*4){
      index_list<-as.numeric(unlist(strsplit(df_info_new_ordered[i-num_subgroups*3,"inclusion"],",")))
      mat[i,index_list]<-1
      mat[i,i-2*num_subgroups+num_smallest_subgroup]<--1
    }
    
    
    #all x >=0 and all d>=0
    if (i > num_subgroups*4 & i <= num_subgroups*4+num_smallest_subgroup){
      mat[i, i-num_subgroups*4]<-1
    }
    
    if (i > num_subgroups*4+num_smallest_subgroup & i <= num_subgroups*5+num_smallest_subgroup){
      mat[i, i-num_subgroups*4+num_subgroups]<-1
    }
    
    if (i == num_eqns){
      mat[i, c(1:num_smallest_subgroup)]<-1
    }
    
  }

  dir <- c(rep(">=",num_subgroups), rep("<=",num_subgroups),rep(">=",num_subgroups),rep("<=",num_subgroups),rep(">=",num_smallest_subgroup), rep(">=",num_subgroups),"<=" )
  
  rhs <- c(L,H,D1,D1,rep(0,num_smallest_subgroup),rep(0,num_subgroups),additional_N)
  
  types <- rep("I", num_smallest_subgroup+num_subgroups+num_subgroups)
  
  
  new_rct_equity<-lpsymphony_solve_LP(obj, mat, dir, rhs, types = types, max = FALSE)
  new_rct_solution<-new_rct_equity$solution[1:num_smallest_subgroup]
  new_relaxation<-new_rct_equity$solution[num_smallest_subgroup+1:num_subgroups]
  new_d<-new_rct_equity$solution[num_smallest_subgroup+num_subgroups+1:num_subgroups]
  
  print(sum(new_relaxation))
  print(sum(new_d))
  
  df_with_background_info_new<-df_info_new_ordered[df_info_new_ordered$all_count == 0, ]
  df_with_background_info_new$Assigned_Participants<-new_rct_solution
  df_with_background_info_new$new_subgroup_n<-df_with_background_info_new$Assigned_Participants+df_with_background_info_new$user_n
  total_rct<- sum(df_with_background_info_new$Assigned_Participants)+total_n
  
  df_upper_levels<-df_info_new_ordered
  df_upper_levels$relaxation<-new_relaxation
  

  df_cleaned<- df_with_background_info_new %>%
    group_by(.dots = variable_list,.drop = TRUE) %>% 
    summarise(Assigned_Participants = sum(Assigned_Participants),
              user_n = sum(user_n),
              new_subgroup_n=sum(new_subgroup_n) )
  
  if (num_vars>1){
    for (index_var in 1:(num_vars-1)){
      current_vars<-combinations(num_vars,index_var,variable_list)
      for (index_var2 in 1:nrow(current_vars)){
        df_current<-df_with_background_info_new %>%
          group_by(.dots = current_vars[index_var2,],.drop = TRUE) %>% 
          summarise(Assigned_Participants = sum(Assigned_Participants),
                    user_n = sum(user_n),
                    new_subgroup_n=sum(new_subgroup_n) )
        df_cleaned <- dplyr::bind_rows(df_cleaned, df_current)
      }
    }
  }
  
  design_df<-df_cleaned[,c(variable_list, "user_n", "Assigned_Participants","new_subgroup_n")]
  df_upper_levels<-df_upper_levels[,c(variable_list, "relaxation", "ideal_N","ideal_rate", "L_N","H_N")]
  
  results<- merge(design_df,df_upper_levels,by=variable_list, all= TRUE)
  results$total_RCT_new<-rep(total_rct, nrow(results))
  
  if (is.numeric(Previous_RCT)){
    results$new_Equity<- mapply(equity_metric_selected,results$ideal_rate,(results$new_subgroup_n)/results$total_RCT_new, for_plot = FALSE)
    results<-results[,c(variable_list,"Assigned_Participants","new_Equity")]
    results$new_Equity<- mapply(signif,results$new_Equity,digits = 3)
    names(results)[(num_vars+1):ncol(results)]<-c("Recruitment Number","Equity")
  }
  else{
    results$total_RCT_old<-rep(total_n, nrow(results))
    results$old_Equity<- mapply(equity_metric_selected,results$ideal_rate,(results$user_n)/results$total_RCT_old, for_plot = FALSE)
    results$new_Equity<- mapply(equity_metric_selected,results$ideal_rate,(results$new_subgroup_n)/results$total_RCT_new, for_plot = FALSE)
    results<-results[,c(variable_list,"user_n", "Assigned_Participants", "new_subgroup_n","old_Equity","new_Equity")]
    results$old_Equity<- mapply(signif,results$old_Equity,digits = 3)
    results$new_Equity<- mapply(signif,results$new_Equity,digits = 3)
    names(results)[(num_vars+1):ncol(results)]<-c("Previous n", "Additional n", "New n", "Previous Equity", "Current Equity")
    
  }
  
  
  
  return(results)
  
}


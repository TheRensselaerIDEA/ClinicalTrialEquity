#convert numbers to ratios
Rate_Calculation<-function(subgroup_n,total_n){
  if(total_n == 0){
    return(0)
  }
  return(subgroup_n/total_n)
}
# Equity Metric derived from Equal Opportunity
Adjusted_Equal_Opportunity<-function (alpha_aeo, beta_aeo, for_plot = FALSE){
  if (!for_plot){
    if (beta_aeo == 0 & alpha_aeo == 0){
      return (-9999999)
    }
    else if (beta_aeo == 0){
      return (-Inf)
    }
    else if (alpha_aeo == 0){
      return (-8888888)
    }
    
  }
  top_aeo <- (beta_aeo-alpha_aeo)
  bottom_aeo <- alpha_aeo*(1-alpha_aeo)
  result_aeo <- top_aeo/bottom_aeo
  return (result_aeo)
}
# Equity Metric derived from Disparate Impact
Log_Disparate_Impact<-function (alpha_di, beta_di, for_plot = FALSE){
  if (!for_plot){
    if (beta_di == 0 & alpha_di == 0){
      return (-9999999)
    }
    else if (beta_di == 0){
      return (-Inf)
    }
    else if (alpha_di == 0){
      return (-8888888)
    }
    
  }
  
  left_di <- beta_di/(1-beta_di)
  right_di <- alpha_di/(1-alpha_di)
  result_di <- log(left_di)-log(right_di)
  return (result_di)
}
# Equity Metric derived from observed rate/ ideal rate
Quality_Metric<-function (alpha_di2, beta_di2, for_plot = FALSE){
  if (!for_plot){
    if (beta_di2 == 0 & alpha_di2 == 0){
      return (-9999999)
    }
    else if (beta_di2 == 0){
      return (-Inf)
    }
    else if (alpha_di2 == 0){
      return (-8888888)
    }
    
  }
  return(log(beta_di2/alpha_di2))
}
#  Equity Metric derived from Normalized Mutual Information
Normalized_Mutual_Information<-function (pg1, pg1_a1, pa1, for_plot = FALSE){
  if (pg1 == 0 & pg1_a1 == 0){
    return (-9999999)
  }
  else if (pg1_a1 == 0  & pg1 != 0){
    return (-Inf)
  }
  else if (pg1 == 0& pg1_a1 != 0){
    return (-8888888)
  }
  else{
    pg1a1<-pa1 * pg1_a1
    pa0<-1-pa1
    pg0<-1-pg1
    pa1_g1 <- pg1a1/pg1
    pg1_a0 <- pg1*(1-pa1_g1)/(1-pa1)
    pa0_g1 <- pa0*(pg1_a0)/(pg1)
    bottom_nmi <-sqrt((pa1*log(pa1)+pa0*log(pa0))*(pg1*log(pg1)+pg0*log(pg0)))
    pg1a1<-pg1_a1*(pa1)
    pg1a0<-pg1_a0*(pa0)
    pg0a0<-(1-pg1_a0)*(pa0)
    pg0a1<-(1-pg1_a1)*(pa1)
    top_nmi <- pg0a0*(log(pg0a0)-log(pg0)-log(pa0))+pg0a1*(log(pg0a1)-log(pg0)-log(pa1))+pg1a0*(log(pg1a0)-log(pg1)-log(pa0))+pg1a1*(log(pg1a1)-log(pg1)-log(pa1))
    result_nmi<- top_nmi/bottom_nmi
    return (result_nmi)
  }
}
# Significance test (one sided)
compare_population_proportion_one_side<-function(pb,pa,Nb,Na){
  if (pa == 0 & pb == 0){
    return (-9999999)
  }
  else if (pa == 0){
    return (-Inf)
  }
  else if (pb == 0){
    return (-8888888)
  }
  Xa<- pa*Na
  Xb<- pb*Nb
  pc<- (Xa+Xb)/(Na+Nb)
  z<-(pa-pb)/sqrt(pc*(1-pc)*(1/Na+1/Nb))
  return(pnorm(z))
  
}
# Significance test (two sided)
compare_population_proportion<-function(pb,Xa,Na){
  if (Xa == 0 & pb == 0){
    return (-9999999)
  }
  else if (Xa == 0){
    return (-Inf)
  }
  else if (pb == 0){
    return (-8888888)
  }
  
  X_not<- Na-Xa
  if (Xa>=5 & X_not>=5){
    p_value<-prop.test(x = Xa , n = Na, p = pb,
                       alternative = c("two.sided"),
                       conf.level = 0.95,
                       correct = FALSE)$p.value
  }
  else{
    p_value<-binom.test(x=Xa, Na, p = pb,
                        alternative = c("two.sided"),
                        conf.level = 0.95)$p.value
  }
  
  return(p_value)
}

# transform the significance number into text
whether_significant<-function (p_num, threshold){
  if (p_num == -Inf){
    return ("Absent")
  }
  else if(p_num == -9999999){
    return ("No Info")
  }
  else if(p_num == -8888888){
    return ("No Base Data")
  }
  else if (p_num<threshold) 
  {
    return ("Significant")
  }
  else{
    return ("Not Significant")
  }
}

# transform the equity value to text
whether_biased_label<-function (value, significance_value,significance_threshold, threshold, neg_break, pos_break){
  if (significance_value>significance_threshold){
    return("Equitable(p)")
  }
  
  if (value == -Inf){
    return ("Absent")
  }
  else if(value == -9999999){
    return ("No Info")
  }
  else if(value == -8888888){
    return ("No Base Data")
  }
  else if (value < -threshold) 
  {
    if (value < -neg_break){
      return ("Highly Underrepresented")
    }
    else{
      return ("Underrepresented")
    }
  }
  else if(value > threshold) {
    if (value > pos_break){
      return ("Highly Overrepresented")
    }
    else{
      return ("Overrepresented")
    }
  }
  else{
    return("Equitable")
  }
  
}
# assign the colors to different equity values
add_colors<-function(value, significance_value,significance_threshold, threshold, neg_break, pos_break){
  if (significance_value>significance_threshold){
    return("#f7f7f7")
  }
  
  if (value == -Inf){
    return ("#b2182b")
  }
  else if(value == -9999999){
    return ("#000000")
  }
  else if(value == -8888888){
    return ("#696969")
  }
  else if (value < -threshold) 
  {
    if (value < -neg_break){
      return ("#ef8a62")
    }
    else{
      return ("#FDAE61")
    }
  }
  else if(value > threshold) {
    if (value > pos_break){
      return ("#253494")
    }
    else{
      return ("#ABD9E9")
    }
  }
  else{
    return("#f7f7f7")
  }
}


whether_biased_label_NMI<-function (value, significance_value,significance_threshold, threshold, pos_break){
  if (significance_value>significance_threshold){
    return("Equitable(p)")
  }
  
  if (value == -Inf){
    return ("Absent")
  }
  else if(value == -9999999){
    return ("No Info")
  }
  else if(value == -8888888){
    return ("No Base Data")
  }
  else if(value > threshold) {
    if (value > pos_break){
      return ("Highly Interdependent ")
    }
    else{
      return ("Interdependent")
    }
  }
  else{
    return("Equitable")
  }
  
}

add_colors_NMI<-function(value, significance_value,significance_threshold, threshold, pos_break){
  if (significance_value>significance_threshold){
    return("#f7f7f7")
  }
  if (value == -Inf){
    return ("#b2182b")
  }
  else if(value == -9999999){
    return ("#000000")
  }
  else if(value == -8888888){
    return ("#696969")
  }
  else if(value > threshold) {
    if (value > pos_break){
      return ("#ef8a62")
    }
    else{
      return ("#FDAE61")
    }
  }
  else{
    return("#f7f7f7")
  }
}
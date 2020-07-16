#### Library and Data Imports ####
source("modules/Source.R")
source("modules/data_load.R")
source("modules/Equity_metrics.R")
source("modules/sunburst_process.R")
source("modules/studies_comparison.R")

ui <- dashboardPage(
  skin = "black",
  title = "Equity Browser",
  
  # HEADER ------------------------------------------------------------------
  
  dashboardHeader(title =  span(a("Equity Visualization Tool", href="https://github.com/TheRensselaerIDEA/ClinicalTrialEquity"), 
                                style = "color: red; font-size: 20px; font-weight: bold"),
                  
                  tags$li(a(href = 'https://idea.rpi.edu/',
                            img(src = 'rpi.jpg',
                                title = "Affiliation2", height = "30px"),
                            style = "padding-top:10px; padding-bottom:10px;"),
                          class = "dropdown")),
  
  # SIDEBAR -----------------------------------------------------------------
  
  dashboardSidebar(width = 200,
                   sidebarMenu(
                     selectInput("rctExample", "Which RCT example:",
                                 c("ACCORD (Type-2 Diabetes)" = "ACCORD",
                                   "ALLHAT (Hypertension)" = "ALLHAT",
                                   "SPRINT (Hypertension)" = "SPRINT")),
                     menuItem("RCT vs Target Population", icon = icon("bar-chart-o"),
                              menuSubItem("Patients Distribution ", tabName = "subgroupDistribution")
                     ),
                     menuItem("Equity Metrics Evaluation", icon = icon("clipboard-check"),
                              menuSubItem("Equity Results Visualization", tabName = "metrics_visualization")
                     ),
                     menuItem("Equity Metrics Evaluation", icon = icon("clipboard-check"),
                              menuSubItem("Studies Comparison", tabName = "metrics_table")
                              
                     )
                   )),
  
  # MAIN BODY -----------------------------------------------------------------
  dashboardBody(
    tabItems(
      # Data Visualization -----------------------------------------------------------------
      tabItem(tabName = "subgroupDistribution",
              fluidRow(
                box(width=4,background = "black",
                    #checkboxInput("age_cat_user", "Categorize Age", TRUE),
                    uiOutput("user_selected")),
                box(width = 4, title= "Clinical Trial Rates of Disease",solidHeader = TRUE,status = "primary",
                    div(style = 'overflow-y: scroll;height: 300px;', DT::dataTableOutput("summary_ct"))),
                box(width = 4, title= "Population Rates of Disease",solidHeader = TRUE,status = "primary",
                    div(style = 'overflow-y: scroll;height: 300px;', DT::dataTableOutput("summary_br")))),
                plotOutput(outputId = "comparePlot")

      ),
      # Evaluation -----------------------------------------------------------------
      # 1. Equity Evaluation -----------------------------------------------------------------
      tabItem(tabName = "metrics_table",
              fluidRow(
                box(width = 5,title = "Equity Threshold & Significanc Level Selection",solidHeader = TRUE,background = "navy",
                    # Input: Select equity cutoff ----
                    numericInput("equity_cutoff1_analysis", "Equity Lower Threshold:", 0.2, min = 0.0, max = 1.0, step = 0.01),
                    numericInput("equity_cutoff2_analysis", "Equity Upper Threshold:", 0.4, min = 0.0, max = 1.0, step = 0.01),
                    numericInput("significance_cutoff_analysis", "Significance Threshold:", 0.05, min = 0.0, max = 1.0, step = 0.01)
                )
              ),
              fluidRow(
                box(width = 5,title = "Equity Analysis Results (Demographic Characteristics)",solidHeader = TRUE,
                    div(style = 'overflow-y: scroll;height: 300px;', htmlOutput("study_comparison_demo"))
                ),
                box(width = 5,title = "Equity Analysis Results (Clinical Characteristics)",solidHeader = TRUE,
                    div(style = 'overflow-y: scroll;height: 300px;', htmlOutput("study_comparison_clinical"))
                )
              )
              
      ),
      tabItem(tabName = "metrics_visualization",
              fluidRow(
                box(width = 5,height= 500,title = "Metrics & Visualization Type Selection",background = "navy",solidHeader = TRUE,
                    # Input: Select equity metrics ----
                    selectInput(inputId = "equity_metrics", label = "Select an Equity Metric",
                                c("Log Disparate Impact" = "DI_metric",
                                  "Adjusted Equal Opportunity" = "AEO_metric",
                                  "Normalized Mutual Information" = "NMI_metric",
                                  "Quality Metric" = "QM_metric")),
                    uiOutput("user_var_display"),
                    uiOutput("user_var_order")
                    ),
                box(width = 7,title = "Equity Threshold & Significanc Level Selection",solidHeader = TRUE,background = "navy",
                    # Input: Select equity cutoff ----
                    numericInput("equity_cutoff1", "Equity Lower Threshold:", 0.2, min = 0.0, max = 1.0, step = 0.01),
                    numericInput("equity_cutoff2", "Equity Upper Threshold:", 0.4, min = 0.0, max = 1.0, step = 0.01),
                    numericInput("significance_cutoff", "Significance Threshold:", 0.05, min = 0.0, max = 1.0, step = 0.01)
                    )
              ),
        #Log Disparate Impact
                conditionalPanel(
                  condition = "input.equity_metrics == 'DI_metric'" ,                          
                  fluidRow(
                    box(width = 5,height= 500,title = "How do the protected characteristics influence the equity level?",solidHeader = TRUE,status="info",
                      plotlyOutput("sun_ldi")
                    ),
                    box(width = 7,height= 500,title = "How far is the patient from the equitable level?",solidHeader = TRUE,status="info",
                      #"Box content here", br(), "More box content",
                      plotOutput("sun_plot_di",width = 700, height =400),
                    )
                  )
                ),
        #Adjusted Equal Opportunity
                conditionalPanel(
                  condition = "input.equity_metrics == 'AEO_metric'"   ,                          
                  fluidRow(
                    box(width = 5,height= 500,title = "How do the protected characteristics influence the equity level?",solidHeader = TRUE,status="info",
                        plotlyOutput("sun_aeo")
                    ),
                    box(width = 7,height= 500,title = "How the protected characteristics influence the equity level?",solidHeader = TRUE,status="info",
                        #"Box content here", br(), "More box content",
                        plotOutput("sun_plot_aeo",width = 700, height =400)
                    )
                  )
                ),
        #Normalized Mutual Information
                conditionalPanel(
                  condition = "input.equity_metrics == 'NMI_metric'"   ,                          
                  fluidRow(
                    box(width = 5,height= 500,title = "How do the protected characteristics influence the equity level?",solidHeader = TRUE,status="info",
                        plotlyOutput("sun_nmi")
                    ),
                    box(width = 7,height= 500,title = "How the protected characteristics influence the equity level?",solidHeader = TRUE,status="info",
                        #"Box content here", br(), "More box content",
                        plotOutput("sun_plot_nmi",width = 700, height =400)
                    )
                  )
                ),
        #Quality Metric
                conditionalPanel(
                  condition = "input.equity_metrics == 'QM_metric'"   ,                          
                  fluidRow(
                    box(width = 5,height= 500,title = "How do the protected characteristics influence the equity level?",solidHeader = TRUE,status="info",
                        plotlyOutput("sun_qm")
                    ),
                    box(width = 7,height= 500,title = "How the protected characteristics influence the equity level?",solidHeader = TRUE,status="info",
                        #"Box content here", br(), "More box content",
                        plotOutput("sun_plot_qm",width = 700, height =400)
                    )
                  )
                )
      ),
      tabItem(tabName = "newStudy",
              fluidRow(
                column(width = 4,
                  box(width = NULL,title = "Protected Variables & New Study Population Selection",solidHeader = TRUE,background = "navy",
                    uiOutput("newstudy_var_display"),
                    numericInput("new_study_n", "New study population:", 10000, min =1, step = 1)
                  ),
                  box(width = NULL,title = "Lower Equity Threshold & Significanc Level Selection",solidHeader = TRUE,background = "navy",
                      # Input: Select equity cutoff ----
                      numericInput("equity_cutoff_new_study", "Equity Lower Threshold:", 0.2, min = 0.0, max = 1.0, step = 0.01),
                      numericInput("significance_cutoff_new_study", "Significance Threshold:", 0.05, min = 0.0, max = 1.0, step = 0.01)
                  )
                ),
                column(width = 8,
                       box(width = NULL,title = "New Study Recruitment Plan",solidHeader = TRUE,
                           div(style = 'overflow-y: scroll;height: 500px;', DT::dataTableOutput("new_study_recruitment_plan"))
                       )
                )
              )
      ),
      tabItem(tabName = "additionalRcruitment",
              fluidRow(
                column(width = 4,
                       box(width = NULL,title = "Protected Variables & Additional Population Selection",solidHeader = TRUE,background = "navy",
                           uiOutput("additionalRcruitment_var_display"),
                           numericInput("additionalRcruitment_n", "New study population:", 10000, min =1, step = 1)
                       ),
                       box(width = NULL,title = "Lower Equity Threshold & Significanc Level Selection",solidHeader = TRUE,background = "navy",
                           # Input: Select equity cutoff ----
                           numericInput("equity_cutoff_additionalRcruitment", "Equity Lower Threshold:", 0.2, min = 0.0, max = 1.0, step = 0.01),
                           numericInput("significance_cutoff_additionalRcruitment", "Significance Threshold:", 0.05, min = 0.0, max = 1.0, step = 0.01)
                       )
                ),
                column(width = 8,
                       box(width = NULL,title = "Additional Recruitment Plan",solidHeader = TRUE,
                           div(style = 'overflow-y: scroll;height: 500px;', DT::dataTableOutput("addtional_recruitment_plan"))
                       )
                )
              )
      )      
      
      
      
      
      
    )
  )
  
)




server <- function(input, output, session){

  ######################################## Upload Datasets ########################################################
  df_upload <- reactive({
    if(input$rctExample == "ACCORD"){df_load<- accord_data_analysis}
    else if(input$rctExample == "ALLHAT"){df_load<- allhat_data_analysis}
    else if(input$rctExample == "SPRINT"){df_load<- sprint_data_analysis}
    return(df_load)
  })
  
  ########################################## Background Info ###########################################
  #process the background information
  df_upload_background <- reactive({
    if (input$rctExample == "ACCORD"){
      df_background <- NHANES_diabetes
    }
    else if (input$rctExample == "ALLHAT" |input$rctExample == "SPRINT" ){
      df_background <- NHANES_hypertension
    }
    return(df_background)
  })
  
  df_upload_background_lab <- reactive({
    if (input$rctExample == "ACCORD"){
      df_background_lab <- NHANES_diabetes_lab
    }
    else if (input$rctExample == "ALLHAT" |input$rctExample == "SPRINT"){
      df_background_lab <- NHANES_hypertension_lab
    }
    
    return(df_background_lab)
  })
  
  df_upload_background_combined <- reactive({
    if (input$rctExample == "ACCORD"){
      df_background_combined <- NHANES_diabetes_combined
    }
    else if (input$rctExample == "ALLHAT" |input$rctExample == "SPRINT"){
      df_background_combined <- NHANES_hypertension_combined
    }
    
    return(df_background_combined)
  })
  
  
 
  #########################################################        Distribution        ########################################################
  #User pick the protected attributes that they want to analyze (univariate, bivariate, or multivariate)
  output$user_selected <- renderUI({
    # Get the data set with the appropriate name
    dat <- df_upload()
    variable_choices <- colnames(dat[1:ncol(dat)-1])
    
    # Create the checkboxes
    checkboxGroupInput("columns_distribution", "Choose protected attributes for further equity analysis", 
                       choices  = variable_choices,
                       selected = c("Gender_D","Age_D","Race_or_Ethnicity_D","Education_D"))
  })
  
  
  df_user_compare<- reactive({
    df_compare <- df_upload()
    df_group<- df_compare %>%
      group_by(.dots = input$columns_distribution) %>% 
      summarise(user_n = sum(user_n))
    df_cleaned<-na.omit(df_group)
    total_n<- sum(df_cleaned$user_n)
    df_cleaned$Percentage<-df_cleaned$user_n/total_n
    df_cleaned$user_n<-NULL
    return(df_cleaned)
  })
  
  
  select_appropriate_background_df<-reactive({
    TC_exist<- "TC_L" %in% input$columns_distribution 
    FPG_exist<- "FPG_L" %in% input$columns_distribution
    n<-length(input$columns_distribution)
    
    if ((!TC_exist) & (!FPG_exist) ){
      df_background<-df_upload_background()
    }
    else if((TC_exist & !FPG_exist & n==1)|(!TC_exist & FPG_exist & n==1)|(TC_exist & FPG_exist & n==2)){
      df_background<-df_upload_background_lab()
    }
    else{
      df_background<-df_upload_background_combined()
    }
    
    return(df_background)
  })
  
  #corresponding background subgroup selection
  df_background_compare <- reactive({
    df_background<-select_appropriate_background_df()
    df_new<- df_background %>%
      group_by(.dots = input$columns_distribution) %>% 
      summarise(background_n = sum(background_n))
    df_cleaned<-na.omit(df_new)
    total_n<- sum(df_cleaned$background_n)
    df_cleaned$Percentage<-df_cleaned$background_n/total_n
    df_cleaned$background_n<-NULL
    return(df_cleaned)
  })
  
  df_match_two_df<-reactive({
    if (is.null(input$columns_distribution)){
      return()
    }
    df_compare_user<-df_user_compare()
    df_compare_base<-df_background_compare()
    df_merged<-merge(df_compare_user, df_compare_base, by=1:(ncol(df_compare_base)-1), all=TRUE)
    df_merged[is.na(df_merged)] <- 0
    return(df_merged)
  })
  
  #user input: subgroup summary table
  output$summary_ct <- DT::renderDataTable({
    df_compare_merged<-df_match_two_df()
    n_v<-ncol(df_compare_merged)
    if (is.null(df_compare_merged)){return()}
    df_user<-df_compare_merged %>%
      dplyr::select(1:(n_v-1))
    
    names(df_user)[n_v-1] <-"Percentage"
    return(datatable(df_user)%>%formatPercentage('Percentage', 2)  )
    
  })
  
  #background: corresponding subgroup summary table
  output$summary_br <- DT::renderDataTable({
    df_compare_merged<-df_match_two_df()
    if (is.null(df_compare_merged)){return()}
    n_v<-ncol(df_compare_merged)
    df_base<-df_compare_merged %>%
      dplyr::select(-(n_v-1))
    
    names(df_base)[n_v-1] <-"Percentage"
    return(datatable(df_base)%>%formatPercentage('Percentage', 2) )
  })
  
  #comparison barplot 
  output$comparePlot <- renderPlot({
    if (is.null(input$columns_distribution)){
      return()
    }
    df_user<-df_user_compare()
    df_background<- df_background_compare()
    n_used<-ncol(df_background)-1
    
    df_user$Category<-rep("Observed",nrow(df_user))
    df_background$Category<-rep("Ideal",nrow(df_background))
    
    
    df_merged<-rbind(df_background,df_user)
    if (n_used>1){
      df_merged$Subgroup <- apply( df_merged[ , 1:n_used] , 1 , paste , collapse = "-" )
    }
    else{
      names(df_merged)[1]<-"Subgroup"
    }
    
    ggplot(df_merged, aes(Subgroup, Percentage, fill=Category))+ 
      geom_bar(stat = "identity", position = position_dodge(preserve= "single"))+ 
      theme(axis.text.x = element_text(angle = 75, hjust = 1))+ 
      ggtitle("U.S. Patients Distribution vs Clinical Trial Patients Distribution ")
    
  })
  ######################################################### Evaluation ########################################################
  #User pick the protected attributes that they want to analyze (univariate, bivariate, or multivariate)
  output$user_var_display <- renderUI({
    # Get the data set with the appropriate name
    dat <- df_upload()
    colnames<-names(dat[1:ncol(dat)-1])
    checkboxGroupInput("vars_display_sundb", "Choose variables for equiuty valuation", 
                       choices  = colnames,
                       selected = c("Gender_D","Age_D","Race_or_Ethnicity_D","Education_D"))
  })

  #User pick the protected attributes that they want to analyze (univariate, bivariate, or multivariate)
  output$user_var_order <- renderUI({
    # Get the data set with the appropriate name
    colnames <- input$vars_display_sundb
    orderInput(inputId = "vars_sundb_order", label= "Variable Order: inner - outer (Drag and drop the strings to reorder)", items  = colnames)
  })
  
  select_appropriate_background_df_sunburst<-reactive({
    TC_exist<- "TC_L" %in% input$vars_display_sundb 
    FPG_exist<- "FPG_L" %in% input$vars_display_sundb
    n<-length(input$vars_display_sundb)
    
    if ((!TC_exist) & (!FPG_exist) ){
      df_background<-df_upload_background()
    }
    else if((TC_exist & !FPG_exist & n==1)|(!TC_exist & FPG_exist & n==1)|(TC_exist & FPG_exist & n==2)){
      df_background<-df_upload_background_lab()
    }
    else{
      df_background<-df_upload_background_combined()
    }
    
    return(df_background)
  })
  
  df_generate_sunburst <- reactive({
    user_data <- df_upload()
    background_data <- select_appropriate_background_df_sunburst()
    if (input$equity_metrics == "DI_metric"){
      df_processed <-generate_sunburst_df(background_data,as.list(input$vars_sundb_order),user_data,input$significance_cutoff,-log(1-input$equity_cutoff1),-log(1-input$equity_cutoff2),Log_Disparate_Impact, FALSE)

    }
    else if (input$equity_metrics == "AEO_metric"){
      df_processed <-generate_sunburst_df(background_data,as.list(input$vars_sundb_order),user_data,input$significance_cutoff,input$equity_cutoff1,input$equity_cutoff2,Adjusted_Equal_Opportunity, FALSE)
      
    }
    else if (input$equity_metrics == "NMI_metric"){
      df_processed <-generate_sunburst_df(background_data,as.list(input$vars_sundb_order),user_data,input$significance_cutoff,input$equity_cutoff1,input$equity_cutoff2,Normalized_Mutual_Information,whether_NMI=TRUE)
      
    }
    else if (input$equity_metrics == "QM_metric"){
      df_processed <-generate_sunburst_df(background_data,as.list(input$vars_sundb_order),user_data,input$significance_cutoff,-log(1-input$equity_cutoff1),-log(1-input$equity_cutoff2),Quality_Metric, FALSE)
      
    }
    return(df_processed)
  })
  
  
  
  
  output$sun_ldi <- renderPlotly({
    df_returned<-df_generate_sunburst()
    generate_sunburst_plotly(df_returned)
  })
  
  output$sun_aeo <- renderPlotly({
    df_returned<-df_generate_sunburst()
    generate_sunburst_plotly(df_returned)
  })
  
  output$sun_nmi <- renderPlotly({
    df_returned<-df_generate_sunburst()
    generate_sunburst_plotly(df_returned)
  })
  
  output$sun_qm <- renderPlotly({
    df_returned<-df_generate_sunburst()
    generate_sunburst_plotly(df_returned)
  })

  
  
  
  generate_hover_subgroup<- reactive({
    # if there is no click data, render nothing!
    hoverData<-event_data(event="plotly_hover", source = "sunSource")
    if (is.null(hoverData)) {
      print("Select a sunbgroup on the sunburst figure")
      return(NULL)
    } 
    else {
      df_returned<-df_generate_sunburst()
      index_row<-as.integer(hoverData[["pointNumber"]])+1
      sungroup_info<-df_returned[index_row,] 
      return(sungroup_info)
    }
  })
  
  

  
  output$sun_plot_di <-renderPlot({
    returned_info<-generate_hover_subgroup()
    if (is.null(returned_info)){
      return(NULL)
    }
    sun_plot_process("Log Disparate Impact",returned_info$ids,Log_Disparate_Impact,log(1-input$equity_cutoff2) ,log(1-input$equity_cutoff1),
                     -log(1-input$equity_cutoff1),-log(1-input$equity_cutoff2),input$significance_cutoff,returned_info$Ideal_Rate,returned_info$Observed_Rate,returned_info$EquityLable,seg_length=0.0001)
  })
  
  
  output$sun_plot_qm <-renderPlot({
    returned_info<-generate_hover_subgroup()
    if (is.null(returned_info)){
      return(NULL)
    }
    sun_plot_process("Clinical Quality Metric",returned_info$ids, Quality_Metric,log(1-input$equity_cutoff2) ,log(1-input$equity_cutoff1),
                     -log(1-input$equity_cutoff1),-log(1-input$equity_cutoff2),input$significance_cutoff,returned_info$Ideal_Rate,returned_info$Observed_Rate,returned_info$EquityLable,seg_length=0.0001)
  })
  
  
  output$sun_plot_aeo <-renderPlot({
    returned_info<-generate_hover_subgroup()
    if (is.null(returned_info)){
      return(NULL)
    }
    sun_plot_process("Adjusted Equal Opportunity",returned_info$ids, Adjusted_Equal_Opportunity,
                     -input$equity_cutoff2,-input$equity_cutoff1 ,input$equity_cutoff1,input$equity_cutoff2,input$significance_cutoff,returned_info$Ideal_Rate,returned_info$Observed_Rate,returned_info$EquityLable,seg_length=0.0001,ylimit=FALSE)
  })
 
  output$sun_plot_nmi <-renderPlot({
    returned_info<-generate_hover_subgroup()
    if (is.null(returned_info)){
      return(NULL)
    }
    sun_plot_process_2("Normalized Mutual Information",returned_info$ids, Normalized_Mutual_Information,
                     input$equity_cutoff2,input$equity_cutoff1 ,input$equity_cutoff1,input$equity_cutoff2,input$significance_cutoff,returned_info$Ideal_Rate,returned_info$Observed_Rate,returned_info$ParticipantRate,returned_info$EquityLable,seg_length=0.0001)
  }) 

  
  colorpicker <- function(z,cut1 = log(1-input$equity_cutoff2_analysis),cut2=log(1-input$equity_cutoff1_analysis),cut3=-log(1-input$equity_cutoff1_analysis),cut4=-log(1-input$equity_cutoff2_analysis)){
    if(is.na(z)){return("white")}
    else if(z>cut4){return("white")}
    else if(z == -Inf){return("white")}
    else {return("black")}
  }
  
  bgpicker <- function(z,cut1 = log(1-input$equity_cutoff2_analysis),cut2=log(1-input$equity_cutoff1_analysis),cut3=-log(1-input$equity_cutoff1_analysis),cut4=-log(1-input$equity_cutoff2_analysis)){
    if(is.na(z)){return("black")}
    else if(z == -Inf){return("#b2182b")}
    else if(z <= cut1){return("#ef8a62")}
    else if( z > cut1 & z <= cut2){return("#FDAE61")}
    else if( z > cut2 & z <= cut3){return("#f7f7f7")}
    else if( z > cut3 & z <= cut4){return("#ABD9E9")}
    else if (z>cut4){return("#253494")}
  }
  
  
  
  output$study_comparison_demo <- renderPrint({
    df_demo_comparison<-preprocess_comparison_df(input$significance_cutoff_analysis,input$equity_cutoff1_analysis,input$equity_cutoff2_analysis )
    colnames(df_demo_comparison)<-c("Demographic Characteristics","ACCORD Equity" ,"ALLHAT Equity","SPRINT Equity")
    
    df_demo_comparison<-df_demo_comparison[c(1:14),]
    rownames(df_demo_comparison) <- NULL
    
    df_demo_comparison$`ACCORD Equity`<-as.numeric(df_demo_comparison$`ACCORD Equity`)
    df_demo_comparison$`ALLHAT Equity`<-as.numeric(df_demo_comparison$`ALLHAT Equity`)
    df_demo_comparison$`SPRINT Equity`<-as.numeric(df_demo_comparison$`SPRINT Equity`)
    
    format_table (df_demo_comparison, 
                  align =c("l","c","c","c"), 
                  list(
                    `ACCORD Equity`= formatter("span",
                                               style = x ~ style(display = "block",
                                                                 "border-radius" = "4px",
                                                                 "padding-right" = "4px",
                                                                 color = sapply(x,colorpicker),
                                                                 "background-color" = sapply(x,bgpicker)),
                                               x ~ sprintf("%.2f", x)),
                    `ALLHAT Equity`= formatter("span",
                                               x ~icontext(ifelse(x== 0.03511, "star","")), 
                                               style = x ~ style(display = "block",
                                                                 "border-radius" = "4px",
                                                                 "padding-right" = "4px",
                                                                 color = sapply(x,colorpicker),
                                                                 "background-color" = sapply(x,bgpicker)),
                                               x ~ sprintf("%.2f", x)),
                    `SPRINT Equity`= formatter("span",
                                               x ~icontext(ifelse(x== 0.03895, "star","")), 
                                               style = x ~ style(display = "block",
                                                                 "border-radius" = "4px",
                                                                 "padding-right" = "4px",
                                                                 color = sapply(x,colorpicker),
                                                                 "background-color" = sapply(x,bgpicker)),
                                               x ~ sprintf("%.2f", x))
                  ))%>%
      kable_styling("striped", full_width = F) %>%
      pack_rows("Gender", 1,2) %>%
      pack_rows("Age group (Diabetes/Hypertension, years)", 3,5)%>%
      pack_rows("Race/Ethnicity", 6,10) %>%
      pack_rows("Education", 11,14)
  })
  
  

  
  output$study_comparison_clinical <- renderPrint({
    df_clinical_comparison<-preprocess_comparison_df(input$significance_cutoff_analysis,input$equity_cutoff1_analysis,input$equity_cutoff2_analysis )
    colnames(df_clinical_comparison)<-c("Clinical Characteristics ","ACCORD Equity" ,"ALLHAT Equity","SPRINT Equity")
    
    df_clinical_comparison<-df_clinical_comparison[c(15:29),]
    rownames(df_clinical_comparison) <- NULL
    
    
    df_clinical_comparison$`ACCORD Equity`<-as.numeric(df_clinical_comparison$`ACCORD Equity`)
    df_clinical_comparison$`ALLHAT Equity`<-as.numeric(df_clinical_comparison$`ALLHAT Equity`)
    df_clinical_comparison$`SPRINT Equity`<-as.numeric(df_clinical_comparison$`SPRINT Equity`)
    
    format_table (df_clinical_comparison, 
                  align =c("l","c","c","c"), 
                  list(
                    `ACCORD Equity`= formatter("span",
                                               style = x ~ style(display = "block",
                                                                 "border-radius" = "4px",
                                                                 "padding-right" = "4px",
                                                                 color = sapply(x,colorpicker),
                                                                 "background-color" = sapply(x,bgpicker)),
                                               x ~ sprintf("%.2f", x)),
                    `ALLHAT Equity`= formatter("span",
                                               x ~icontext(ifelse(x== 0.03511, "star","")), 
                                               style = x ~ style(display = "block",
                                                                 "border-radius" = "4px",
                                                                 "padding-right" = "4px",
                                                                 color = sapply(x,colorpicker),
                                                                 "background-color" = sapply(x,bgpicker)),
                                               x ~ sprintf("%.2f", x)),
                    `SPRINT Equity`= formatter("span",
                                               x ~icontext(ifelse(x== 0.03895, "star","")), 
                                               style = x ~ style(display = "block",
                                                                 "border-radius" = "4px",
                                                                 "padding-right" = "4px",
                                                                 color = sapply(x,colorpicker),
                                                                 "background-color" = sapply(x,bgpicker)),
                                               x ~ sprintf("%.2f", x))
                  ))%>%
      kable_styling("striped", full_width = F) %>%
      pack_rows("Cigarette-smoking status", 1,2)%>%
      pack_rows("Body-mass index group", 3,6)%>%
      pack_rows("Systolic blood pressure (mm Hg)", 7,10)%>%
      pack_rows("Total cholesterol", 11,12)%>%
      pack_rows("Fasting glucose (mg/dl) ", 13,15)
  })
  
  
  
  
  
  
}


























# Run the application 
shinyApp(ui = ui, server = server)


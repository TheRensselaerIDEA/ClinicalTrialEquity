#### Library and Data Imports ####
source("modules/Source.R")
source("modules/data_load.R")
source("modules/Equity_metrics.R")
source("modules/sunburst_process.R")
source("modules/studies_comparison.R")

ui <- dashboardPage(
  skin = "black",
  title = "RCT Equity Visualization Supplement",
  
  # HEADER ------------------------------------------------------------------
  dashboardHeader(title =  span(img(src="RPI.png", width = 50, height=50),a("RCT Equity Visualization Supplement",href="https://github.com/TheRensselaerIDEA/ClinicalTrialEquity/blob/master/README.md#visualization-tool", target="_blank"), 
                                style = "color: #000000; font-size: 20px; font-weight: bold"),titleWidth = 450),
  
  # SIDEBAR -----------------------------------------------------------------
  
  dashboardSidebar(width = 270,
                   sidebarMenu(
                     menuItem("About", icon = icon("home"),tabName = "aboutEquityBrowser"),
                     selectInput("rctExample", "Which RCT example:",
                                 c("ACCORD (Type-2 Diabetes)" = "ACCORD",
                                   "ALLHAT (Hypertension)" = "ALLHAT",
                                   "SPRINT (Hypertension)" = "SPRINT")),
                     menuItem("RCT vs Target Populations", icon = icon("bar-chart-o"),tabName = "subgroupDistribution"),
                     menuItem("Studies Comparison", icon = icon("group"),tabName = "metrics_table"),
                     menuItem("Equity Metrics Evaluation", icon = icon("balance-scale"), tabName = "metrics_visualization"),
                     HTML(paste0(
                       "<br><br><br><br><br><br><br><br><br>",
                       "<table style='margin-left:auto; margin-right:auto;'>",
                       "<tr>",
                       "<td style='padding: 5px;'><a href='https://idea.rpi.edu/' target='_blank'>RPI-IDEA | </a></td>",
                       "<td style='padding: 5px;'><a href='https://github.com/TheRensselaerIDEA/ClinicalTrialEquity/tree/master/Visualization%20Codes' target='_blank'>Open Source Code | </a></td>",
                       "<td style='padding: 5px;'><a href='https://github.com/TheRensselaerIDEA/ClinicalTrialEquity' target='_blank'>Project</a></td>",
                       "</tr>",
                       "</table>",
                       "<br>")
                     )
                  
                   )),
  
  # MAIN BODY -----------------------------------------------------------------
  dashboardBody(
    tags$head(tags$style(HTML('
                  h1 {
                      margin: 25px;
                      text-align:center;
                      font-family: Source Sans Pro;
                      font-weight: 300;
                      font-size: 2.0em;
                      color: #222222;
                      text-decoration: none solid rgb(158, 162, 162);
                      line-height: 80px;
                    }
                    
                    h2 {
                      font-family: Source Sans Pro;
                      font-size: 24px;
                      color: #222222;
                      text-decoration: none solid rgb(171, 35, 40);
                      text-transform: uppercase;
                      text-align: center;
                    }
                    h3 {
                      line-height: 1.5em;
                      font-size: 1.3em;
                      font-family: "Source Sans Pro", sans-serif;
                      text-decoration: none solid rgb(0, 32, 91);
                      color: #00205b;
                      text-transform: uppercase;
                    }
                  p {
                          padding-bottom: 6.9px;
                              font-size: 1.2em;
                              line-height: 1.6em;
                              font-weight: 400;
                  }
                  li {
                          padding-bottom: 6.9px;
                              font-size: 1.2em;
                              line-height: 1.6em;
                              font-weight: 400;
                          }
                  li span {
                          padding-bottom: 6.9px;
                              font-size: 1.2em;
                              line-height: 1.6em;
                              font-weight: 400;
                          }
    .selectize-input {
                          background: #FAFAFA;
                          font-family: SourceSansPro-Regular;
                          max-width: 100%;
                          font-size: 15px;
                          color: #333366;
                          letter-spacing: 0;
                          border: none;
                      }
    .main-header  a{
                              background-color: #FFFFFF;
                              color: #000000;
                              font-weight:bold;
                              font-family: Source Sans Pro;
                              }
        .superbigimage{
                            overflow-x:scroll;
                            overflow-y:scroll;
                            white-space: nowrap;
                        }
      
        .superbigimage img{
                            max-width: none;
                        }

    /* logo */
        .skin-black .main-header  {
                              background-color: #FFFFFF;
                              color: #000000;
                              font-family: Source Sans Pro;
                              }
        .skin-black .main-header .logo {
                              background-color: #FFFFFF;
                              color: #000000;
                              font-family: Source Sans Pro;
                              }

        /* logo when hovered */
        .skin-black .main-header .logo:hover {
                              background-color: #FFFFFF;
                              color: #000000;
                              font-family: Source Sans Pro;
                              }

        /* navbar (rest of the header) */
        .skin-black .main-header .navbar {
                              background-color: #FFFFFF;
                              color: #000000;
                              font-family: Source Sans Pro;
                              }        

        /* main sidebar */
        .skin-black .main-sidebar {
                              background-color: #FFFFFF;
                              color: #000000;
                              font-family: Source Sans Pro;
        }
                              
        .skin-black .main-sidebar .sidebar {
                              background-color: #FFFFFF;
                              color: #000000;
                              font-family: Source Sans Pro;
                              }
                              
        /* active selected tab in the sidebarmenu */
        .skin-black .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #b3d3d5;
                              color: #000000;
                              font-family: Source Sans Pro;
                              }

        /* other links in the sidebarmenu */
        .skin-black .main-sidebar .sidebar .sidebar-menu a{
                              background-color: #FFFFFF;
                              color: #7fa9ae;
                              font-weight:bold;
                              font-family: Source Sans Pro;
                              }

        /* other links in the sidebarmenu when hovered */
         .skin-black .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-color: #d4e6e8;
                              color: #000000;
                              font-family: Source Sans Pro;
                              }
        /* toggle button when hovered  */                    
         .skin-black .main-header .navbar .sidebar-toggle:hover{
                              background-color: #9ea2a2;
                              color: #000000;
                              font-family: Source Sans Pro;
                              }
        /* body */
         .content-wrapper, .right-side {
                              background-color: #ededed;
                              }
        .box.box-solid.box-primary>.box-header  {
                              border-top-left-radius: 7px;
                              border-top-right-radius: 7px;
                              background: #B4B4B4;
                              text-align: center;
                            }
        .box.box-solid.box-primary>.box-title {
                              font-family: Source Sans Pro;
                              text-decoration: none solid rgb(240, 240, 240);
                              text-transform: uppercase;
                              color: white;
                            }
        .box.box-solid.box-primary{
                              border-bottom-color:#ffffff;
                              border-left-color:#ffffff;
                              border-right-color:#ffffff;
                              border-top-color:#ffffff;
                              background:#ffffff
                            }
                              ')))
  ,
    tabItems(
      tabItem(tabName = "aboutEquityBrowser",
              tags$h1("WELCOME TO RCT EQUITY VISUALIZATION SUPPLEMENT"),
              tags$h2("Introduction"),
              tags$p("This ",
                tags$span("RCT Equity Visualization Supplement", style = "color:teal"),
                " has been designed to aid in the quantification of the inequities in existing clinical trials 
              and provide insights to improve the clinical trial equity and health equity. It is part of the supplementary materials for the paper", tags$em(" Quantifying Inequities in Randomized Clinical Trials using Fairness Metrics. "),
              "This tool allows the user to select a sample randomized clinical trial (RCT) for inequity analysis, and then visualize the equity levels of diffreent 
              subgroups by selecting different protected attributes such as age and gender, different RCT inequity metrics including Log Disparate Impact, 
              Adjusted Equal Opportunity, and Clinical Quality Metric, and thresholds including the significance level, lower and upper metric thresholds."),
              br(),
              tags$h2("Features"),
              tags$div(tags$ul(
                tags$li(tags$span("Compare the patients distributions in the randomized clinical trial and the target population"),tags$span("(RCT vs Target Populations)", style = "color:teal")),
                tags$li(tags$span("Measure inequity in randomized clinical trials"),tags$span("(Equity Metrics Evaluation)", style = "color:teal")),
                tags$li(tags$span("Visualize inequity for subgroups"),tags$span("(Equity Metrics Evaluation)", style = "color:teal")),
                tags$li(tags$span("Compare inequities among studies"),tags$span("(Studies Comparison)", style = "color:teal")))),
              br(),
              tags$h3("Sources"),  
              tags$ul(
                tags$li(tags$a(href="https://www.cdc.gov/nchs/nhanes/index.htm", "Population Statistics: National Health and Nutrition Examination Survey Data", target="_blank")),
                tags$li(tags$a(href="https://biolincc.nhlbi.nih.gov/studies/accord/","Action to Control Cardiovascular Risk in Diabetes (ACCORD)", target="_blank")),
                tags$li(tags$a(href="https://biolincc.nhlbi.nih.gov/studies/allhat/","Antihypertensive and Lipid-Lowering Treatment to Prevent Heart Attack Trial (ALLHAT)", target="_blank")),
                tags$li(tags$a(href="https://biolincc.nhlbi.nih.gov/studies/sprint/","Systolic Blood Pressure Intervention Trial (SPRINT)", target="_blank"))
              )
              
      ),
      # Data Visualization -----------------------------------------------------------------
      tabItem(tabName = "subgroupDistribution",
              fluidRow(
              box(width=12,title = "U.S. Patients Distribution vs Clinical Trial Patients Distribution", status = "primary", solidHeader = TRUE,
              div(class="superbigimage",plotOutput(outputId = "comparePlot", width= 5000, height = 600)))),
              hr(),
              fluidRow(
                column(2,
                    uiOutput("user_selected")),
                
                column(10,
                       box(title= "Clinical Trial Rates of Disease",solidHeader = TRUE,status = "primary",
                   DT::dataTableOutput("summary_ct")),
            
                       box(title= "Population Rates of Disease",solidHeader = TRUE,status = "primary",
                   DT::dataTableOutput("summary_br"))))
                

      ),
      # Evaluation -----------------------------------------------------------------
      # 1. Equity Evaluation -----------------------------------------------------------------
      tabItem(tabName = "metrics_table",
              fluidRow(
                column(3,
                    numericInput("equity_cutoff1_analysis", "Equity Lower Threshold:", 0.2, min = 0.0, max = 1.0, step = 0.01)),
                column(3,
                    numericInput("equity_cutoff2_analysis", "Equity Upper Threshold:", 0.4, min = 0.0, max = 1.0, step = 0.01)),
                column(3,
                    numericInput("significance_cutoff_analysis", "Significance Threshold:", 0.05, min = 0.0, max = 1.0, step = 0.01)),
                tags$h5("Note: black with 'NA' means the RCTs do not contain this category value; white with 'star' indicates subgroups that are treated equitably due to significance tests.")
                ),

              fluidRow(
                column(12,
                  box(width = 6,title = "Equity Analysis Results (Demographic Characteristics) for Log Disparate Impact",status = "primary", solidHeader = TRUE,
                      htmlOutput("study_comparison_demo")
                  ),
                
                  box(width = 6,title = "Equity Analysis Results (Clinical Characteristics) for Log Disparate Impact",status = "primary", solidHeader = TRUE,
                    htmlOutput("study_comparison_clinical"))
                )
              )
              
      ),
      tabItem(tabName = "metrics_visualization",
              fluidRow(
                column(8,
                  box(width=12,title = "Metrics & Visualization Type Selection",status = "primary", solidHeader = TRUE,
                      # Input: Select equity metrics ----
                      selectInput(inputId = "equity_metrics", label = "Select an Equity Metric",
                                  c("Log Disparate Impact" = "DI_metric",
                                    "Adjusted Equal Opportunity" = "AEO_metric",
                                    "Quality Metric" = "QM_metric")),
                      uiOutput("user_var_display"),
                      uiOutput("user_var_order")
                      )),
                  column(4,
                         numericInput("equity_cutoff1", "Equity Lower Threshold:", 0.2, min = 0.0, max = 1.0, step = 0.01),
                         numericInput("equity_cutoff2", "Equity Upper Threshold:", 0.4, min = 0.0, max = 1.0, step = 0.01),
                         numericInput("significance_cutoff", "Significance Threshold:", 0.05, min = 0.0, max = 1.0, step = 0.01)
                  )),
                  fluidRow(
                    column(2,
                         numericInput("zoom_w", "Sunburst Size:", 450, min = 0, max = 10000, step = 50)),
                    tags$h5("Notes: TC= total cholesterol; FPG = fasting glucose; SBP = systolic blood pressure;
                            '_D' = demographic characteristics; '_R' = risk factors ; '_L' = lab results")
                    
                    
                    ),
                  

        #Log Disparate Impact
                conditionalPanel(
                  condition = "input.equity_metrics == 'DI_metric'" ,                          
                  fluidRow(
                    box(width = 6, height = 650,title = "How do the protected characteristics influence the equity level?",status = "primary", solidHeader = TRUE,
                      h3("Hover or click a subgroup to get detials"),
                      div(class="superbigimage",plotlyOutput("sun_ldi",width = 600, height = 500 ))
                    ),
                    box(width = 6, height = 650, title = "How far is a patient of the selected subgroup from the equitable level?",status = "primary", solidHeader = TRUE,
                      #plotOutput("sun_plot_di")
                      h3("Brush and double-click to zoom"),
                      actionButton("reset_di", "reset"),
                      plotOutput("sun_plot_di",
                                 dblclick = "sun_plot_di_dblclick",
                                 brush = brushOpts(
                                   id = "sun_plot_di_brush",
                                   resetOnNew = TRUE
                                  )
                      )
                    )
                  )
                ),
        #Adjusted Equal Opportunity
                conditionalPanel(
                  condition = "input.equity_metrics == 'AEO_metric'"   ,                          
                  fluidRow(
                    box(width = 6, height = 650,title = "How do the protected characteristics influence the equity level?",status = "primary", solidHeader = TRUE,
                        h3("Hover or click a subgroup to get detials"),
                        div(class="superbigimage",plotlyOutput("sun_aeo",width = 600, height = 500 ))
                        
                    ),
                    box(width = 6,height= 650,title = "How far is a patient of the selected subgroup from the equitable level?",status = "primary", solidHeader = TRUE,
                        #plotOutput("sun_plot_aeo")
                        h3("Brush and double-click to zoom"),
                        actionButton("reset_aeo", "reset"),
                        plotOutput("sun_plot_aeo",
                                   dblclick = "sun_plot_aeo_dblclick",
                                   brush = brushOpts(
                                     id = "sun_plot_aeo_brush",
                                     resetOnNew = TRUE
                                   )
                        )
                    )
                  )
                ),
        #Quality Metric
                conditionalPanel(
                  condition = "input.equity_metrics == 'QM_metric'"   ,                          
                  fluidRow(
                    box(width = 6, height = 650,title = "How do the protected characteristics influence the equity level?",status = "primary", solidHeader = TRUE,
                        h3("Hover or click a subgroup to get detials"),
                        div(class="superbigimage",plotlyOutput("sun_qm",width = 600, height = 500 ))
                        
                    ),
                    box(width = 6,height= 650,title = "How far is a patient of the selected subgroup from the equitable level?",status = "primary", solidHeader = TRUE,
                        #plotOutput("sun_plot_qm")
                        h3("Brush and double-click to zoom"),
                        actionButton("reset_qm", "reset"),
                        plotOutput("sun_plot_qm",
                                   dblclick = "sun_plot_qm_dblclick",
                                   brush = brushOpts(
                                     id = "sun_plot_qm_brush",
                                     resetOnNew = TRUE
                                   )
                        )
                    )
                  )
                )
      )   
      
      
      
      
      
    )
  )
  
)




server <- function(input, output, session){
  options(dplyr.summarise.inform = FALSE)

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
    return(datatable(df_user,class = 'cell-border stripe',extensions = 'Scroller', options = list(
      deferRender = TRUE,
      scrollY = 200,
      scrollX = TRUE,
      scroller = TRUE,  
      autoWidth = TRUE,
      columnDefs = list(list(width = '10px', targets = "_all"))
    ))%>%formatPercentage('Percentage', 2)  )
    
  })
  
  #background: corresponding subgroup summary table
  output$summary_br <- DT::renderDataTable({
    df_compare_merged<-df_match_two_df()
    if (is.null(df_compare_merged)){return()}
    n_v<-ncol(df_compare_merged)
    df_base<-df_compare_merged %>%
      dplyr::select(-(n_v-1))
    
    names(df_base)[n_v-1] <-"Percentage"
    return(datatable(df_base,class = 'cell-border stripe',extensions = 'Scroller', options = list(
      deferRender = TRUE,
      scrollY = 200,
      scrollX = TRUE,
      scroller = TRUE,
      columnDefs = list(list(width = '10px', targets = "_all"))
    ))%>%formatPercentage('Percentage', 2) )
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
    
    ggplot(df_merged, aes(Subgroup, Percentage, fill=Category, width = 0.9))+ 
      geom_col(position = "dodge") +
      #geom_bar(stat = "identity", position = position_dodge(preserve= "single"))+ 
      scale_color_manual(values=c("#7fa9ae","#ab2328"))+
      scale_fill_manual(values=c("#7fa9ae","#ab2328"))+
      theme(axis.text.x = element_text(size=12, face = "bold",angle = 75, hjust = 1),
            legend.position="left",plot.title = element_text(size = 14, face = "bold"),
            legend.title=element_text(size=14, face = "bold"), 
            legend.text=element_text(size=13, face = "bold"),
            axis.title=element_text(size=14,face="bold"),
            axis.text=element_text(size=12,face="bold"),
            plot.margin = margin(1, 2, 2, 1, "cm"))
    
  })
  ######################################################### Evaluation ########################################################
  #User pick the protected attributes that they want to analyze (univariate, bivariate, or multivariate)
  output$user_var_display <- renderUI({
    # Get the data set with the appropriate name
    dat <- df_upload()
    colnames<-names(dat[1:ncol(dat)-1])
    checkboxGroupInput("vars_display_sundb", "Choose Variables for Equity Evaluation", 
                       inline = TRUE,
                       width = '100%',
                       choices  = colnames,
                       selected = c("Gender_D","Age_D","Race_or_Ethnicity_D","Education_D"))
  })

  #User pick the protected attributes that they want to analyze (univariate, bivariate, or multivariate)
  output$user_var_order <- renderUI({
    # Get the data set with the appropriate name
    colnames <- input$vars_display_sundb
    #print("colnames")
    #print(colnames)
    orderInput(inputId = "vars_sundb", label= "Variable Order: inner - outer (Drag and drop the strings to reorder)", items  = colnames)
  })
  
  select_appropriate_background_df_sunburst<-reactive({
    TC_exist<- "TC_L" %in% input$vars_display_sundb 
    FPG_exist<- "FPG_L" %in% input$vars_display_sundb
    n<-length(input$vars_display_sundb)
    
    #print(input$vars_display_sundb)
    
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
    else if (input$equity_metrics == "QM_metric"){
      df_processed <-generate_sunburst_df(background_data,as.list(input$vars_sundb_order),user_data,input$significance_cutoff,-log(1-input$equity_cutoff1),-log(1-input$equity_cutoff2),Quality_Metric, FALSE)
      
    }
    
    return(df_processed)
  })
  
  
  
  
  output$sun_ldi <- renderPlotly({
    df_returned<-df_generate_sunburst()
    generate_sunburst_plotly(df_returned, given_width = input$zoom_w)
  })
  
  output$sun_aeo <- renderPlotly({
    df_returned<-df_generate_sunburst()
    generate_sunburst_plotly(df_returned, given_width = input$zoom_w)
  })

  output$sun_qm <- renderPlotly({
    df_returned<-df_generate_sunburst()
    generate_sunburst_plotly(df_returned, given_width = input$zoom_w)
  })

  
  
  
  generate_hover_subgroup<- reactive({
    # if there is no click data, render nothing!
    hoverData<-event_data(event="plotly_hover", source = "sunSource")
    if (is.null(hoverData)) {
      #print("Select a sunbgroup on the sunburst figure")
      return(NULL)
    } 
    else {
      df_returned<-df_generate_sunburst()
      index_row<-as.integer(hoverData[["pointNumber"]])+1
      sungroup_info<-df_returned[index_row,]
      return(sungroup_info)
    }
  })
  
  
  generate_click_subgroup<- reactive({
    # if there is no click data, render nothing!
    clickData<-event_data(event="plotly_click", source = "sunSource")
    if (is.null(clickData)) {
      #print("Click a sunbgroup on the sunburst figure")
      return(NULL)
    } 
    else {
      df_returned<-df_generate_sunburst()
      index_row<-as.integer(clickData[["pointNumber"]])+1
      sungroup_info<-df_returned[index_row,]
      return(sungroup_info)
    }
  })
  
  ranges <- reactiveValues(x = c(0,1), y = c(0,0))
  
  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  
  observeEvent(input$reset_di, {
      ranges$x <- c(0,1)
      ranges$y <- c(0,0)
  })

  observeEvent(input$sun_plot_di_dblclick, {
    brush <- input$sun_plot_di_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)

    } else {
      ranges$x <- c(0,1)
      ranges$y <- c(0,0)
    }
  })
  
  
  observeEvent(input$reset_aeo, {
    ranges$x <- c(0,1)
    ranges$y <- c(0,0)
  })
  
  observeEvent(input$sun_plot_aeo_dblclick, {
    brush <- input$sun_plot_aeo_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- c(0,1)
      ranges$y <- c(0,0)
    }
  })
  
  observeEvent(input$reset_qm, {
    ranges$x <- c(0,1)
    ranges$y <- c(0,0)
  })
  
  observeEvent(input$sun_plot_qm_dblclick, {
    brush <- input$sun_plot_qm_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- c(0,1)
      ranges$y <- c(0,0)
    }
  })
  
  
  
  
  
  

  
  output$sun_plot_di <-renderPlot({
    returned_info<-generate_hover_subgroup()
    returned_info_click<-generate_click_subgroup()
    
    if (is.null(returned_info) & is.null(returned_info_click)){
      return(NULL)
    }
    
    else if (!is.null(returned_info)){
      sun_plot_process("Log Disparate Impact",returned_info$ids,Log_Disparate_Impact,log(1-input$equity_cutoff2) ,log(1-input$equity_cutoff1),
                     -log(1-input$equity_cutoff1),-log(1-input$equity_cutoff2),input$significance_cutoff,returned_info$Ideal_Rate,returned_info$Observed_Rate,returned_info$EquityLable,seg_length=0.0001,TRUE,ranges$x, ranges$y)
    }
    
    else{
      sun_plot_process("Log Disparate Impact",returned_info_click$ids,Log_Disparate_Impact,log(1-input$equity_cutoff2) ,log(1-input$equity_cutoff1),
                     -log(1-input$equity_cutoff1),-log(1-input$equity_cutoff2),input$significance_cutoff,returned_info_click$Ideal_Rate,returned_info_click$Observed_Rate,returned_info_click$EquityLable,seg_length=0.0001,TRUE,ranges$x, ranges$y)
    }
    
    
  })
  
  
  
  output$sun_plot_qm <-renderPlot({
    returned_info<-generate_hover_subgroup()
    returned_info_click<-generate_click_subgroup()
    
    if (is.null(returned_info) & is.null(returned_info_click)){
      return(NULL)
    }
    
    else if (!is.null(returned_info)){
      sun_plot_process("Clinical Quality Metric",returned_info$ids,Quality_Metric,log(1-input$equity_cutoff2) ,log(1-input$equity_cutoff1),
                       -log(1-input$equity_cutoff1),-log(1-input$equity_cutoff2),input$significance_cutoff,returned_info$Ideal_Rate,returned_info$Observed_Rate,returned_info$EquityLable,seg_length=0.0001,TRUE,ranges$x, ranges$y)
    }
    
    else{
      sun_plot_process("Clinical Quality Metric",returned_info_click$ids,Quality_Metric,log(1-input$equity_cutoff2) ,log(1-input$equity_cutoff1),
                       -log(1-input$equity_cutoff1),-log(1-input$equity_cutoff2),input$significance_cutoff,returned_info_click$Ideal_Rate,returned_info_click$Observed_Rate,returned_info_click$EquityLable,seg_length=0.0001,TRUE,ranges$x, ranges$y)
    }
  })
  
  
  output$sun_plot_aeo <-renderPlot({
    returned_info<-generate_hover_subgroup()
    returned_info_click<-generate_click_subgroup()
    
    if (is.null(returned_info) & is.null(returned_info_click)){
      return(NULL)
    }
    
    else if (!is.null(returned_info)){
    sun_plot_process("Adjusted Equal Opportunity",returned_info$ids, Adjusted_Equal_Opportunity,
                     -input$equity_cutoff2,-input$equity_cutoff1 ,input$equity_cutoff1,input$equity_cutoff2,input$significance_cutoff,returned_info$Ideal_Rate,returned_info$Observed_Rate,returned_info$EquityLable,seg_length=0.0001,ylimit=FALSE,ranges$x, ranges$y)
    }
    
    else{
      sun_plot_process("Adjusted Equal Opportunity",returned_info_click$ids, Adjusted_Equal_Opportunity,
                       -input$equity_cutoff2,-input$equity_cutoff1 ,input$equity_cutoff1,input$equity_cutoff2,input$significance_cutoff,returned_info_click$Ideal_Rate,returned_info_click$Observed_Rate,returned_info_click$EquityLable,seg_length=0.0001,ylimit=FALSE,ranges$x, ranges$y)
      
    }
  })
 
 
  
  colorpicker <- function(z,cut1 = log(1-input$equity_cutoff2_analysis),cut2=log(1-input$equity_cutoff1_analysis),cut3=-log(1-input$equity_cutoff1_analysis),cut4=-log(1-input$equity_cutoff2_analysis)){
    if(is.na(z)){return("white")}
    else if(z>cut4){return("white")}
    else if(z == -Inf){return("white")}
    else {return("black")}
  }
  
  bgpicker <- function(z,cut1 = log(1-input$equity_cutoff2_analysis),cut2=log(1-input$equity_cutoff1_analysis),cut3=-log(1-input$equity_cutoff1_analysis),cut4=-log(1-input$equity_cutoff2_analysis)){
    if(is.na(z)){return("black")}
    else if(z == -Inf){return("#ab2328")}
    else if(z <= cut1){return("#d58570")}
    else if( z > cut1 & z <= cut2){return("#eabcad")}
    else if( z > cut2 & z <= cut3){return("#ffffff")}
    else if( z > cut3 & z <= cut4){return("#a5b0cb")}
    else if (z>cut4){return("#00205b")}
  }
  
  
  
  output$study_comparison_demo <- renderPrint({
    df_demo_comparison<-preprocess_comparison_df(input$significance_cutoff_analysis,input$equity_cutoff1_analysis,input$equity_cutoff2_analysis )
    colnames(df_demo_comparison)<-c("Demographic Characteristics","ACCORD Equity" ,"ALLHAT Equity","SPRINT Equity")
    
    df_demo_comparison<-df_demo_comparison[c(1:14),]
    rownames(df_demo_comparison) <- NULL
    
    df_demo_comparison$`ACCORD Equity`<-as.numeric(df_demo_comparison$`ACCORD Equity`)
    df_demo_comparison$`ALLHAT Equity`<-as.numeric(df_demo_comparison$`ALLHAT Equity`)
    df_demo_comparison$`SPRINT Equity`<-as.numeric(df_demo_comparison$`SPRINT Equity`)
    
    demo_result<-format_table (df_demo_comparison, 
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
      kable_styling("striped", full_width = TRUE,fixed_thead = TRUE) %>%
      pack_rows("Gender", 1,2) %>%
      pack_rows("Age group (Diabetes/Hypertension, years)", 3,5)%>%
      pack_rows("Race/Ethnicity", 6,10) %>%
      pack_rows("Education", 11,14)
    
    
      gt::html(demo_result)
  })
  
  

  
  output$study_comparison_clinical <- renderPrint({
    df_clinical_comparison<-preprocess_comparison_df(input$significance_cutoff_analysis,input$equity_cutoff1_analysis,input$equity_cutoff2_analysis )
    colnames(df_clinical_comparison)<-c("Clinical Characteristics ","ACCORD Equity" ,"ALLHAT Equity","SPRINT Equity")
    
    df_clinical_comparison<-df_clinical_comparison[c(15:29),]
    rownames(df_clinical_comparison) <- NULL
    
    
    df_clinical_comparison$`ACCORD Equity`<-as.numeric(df_clinical_comparison$`ACCORD Equity`)
    df_clinical_comparison$`ALLHAT Equity`<-as.numeric(df_clinical_comparison$`ALLHAT Equity`)
    df_clinical_comparison$`SPRINT Equity`<-as.numeric(df_clinical_comparison$`SPRINT Equity`)
    
    clinical_result<-format_table (df_clinical_comparison, 
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
      kable_styling("striped", full_width = TRUE,fixed_thead = TRUE) %>%
      pack_rows("Cigarette-smoking status", 1,2)%>%
      pack_rows("Body-mass index group", 3,6)%>%
      pack_rows("Systolic blood pressure (mm Hg)", 7,10)%>%
      pack_rows("Total cholesterol", 11,12)%>%
      pack_rows("Fasting glucose (mg/dl) ", 13,15)
    
    gt::html(clinical_result)
  })
  
  
  
  
  
  
}


























# Run the application 
shinyApp(ui = ui, server = server)


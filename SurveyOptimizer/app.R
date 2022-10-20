#
# This is a Shiny web application designed to show optimized survey sizes. You can run the application by clicking
# the 'Run App' button above.
#
# For any questions, feel free to email Lily at lmmcgill@ucsd.edu


## Load in required libraries/ R packages 
library(shiny) 
library(dplyr)
library(ggplot2)
library(raster)
library(elastic)
library(cowplot)
library(tidyr)
library(stringr)
library(DT)
library(reactable)
library(NlcOptim)
require(openxlsx)
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")

#setwd("/Users/lillianmcgill/Documents/SurveyOptimizer")

render_dt = function(data, editable = 'cell', server = TRUE, ...) {
  renderDT(data, selection = 'none', server = server, editable = editable, ...)
}

dt_output = function(title, id) {
  fluidRow(column(
    12, h1("Objective Weights"),
    hr(), DTOutput(id)
  ))
}


# Define UI for application that draws a histogram
ui = bootstrapPage(
  navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
             HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">Survey Optimizer</a>'), id="nav",
             # Specify titles and that we want multiple panels 
             windowTitle = "Survey Optimizer",
             
             tabPanel("Optimizer Details", 
                      sidebarLayout(
                        sidebarPanel(
                          id = "maininfo",
                          tags$h2(HTML(paste0("The <b>Survey Optimizer Model</b> allocates sampling effort 
                          across the following Gulf of Mexico fisheries surveys to maximize insights and meet management objectives:")), style={'color:#045a8d'}), 
                          tags$h4(HTML(paste0(
                            "<ul><li>Camera/ Reef</li><li>SEAMAP Spring Trawl</li><li>SEAMAP Fall Trawl</li><li>SEAMAP Bottom Longline</li><li>SEAMAP Summer Plankton Bongo</li>
                            <li>SEAMAP Summer Plankton Neuston</li><li>SEAMAP Fall Plankton Bongo</li><li>SEAMAP Fall Plankton Neuston</li>
                            <li>NMFS Bottom Longline</li><li>NMFS Small Pelagics</li></ul>"
                          )), style={'color:#045a8d'}), #
                          width = 5
                          
                        ), 
                        mainPanel(
                          tags$h3(HTML(paste0("<b>How it works (this needs to be modified)</b>"))),
                          HTML(paste0(
                            "<p>The optimization routine seeks to maximize the <b>Enterprise Score</b>, which is a combination of multiple, weighted objective criteria, 
                                     by modifying the number of each type of survey. Each survey has a specific value according to the frequency which it encounters a fish 
                                     and the CV (a function of sample size). Low frequencies and high CVs lead to a lower valuation. Constraints are the total cost, total 
                                     capacity, and the capacity for each specific type of survey. 
                                     The <b>Objective Criteria</b> consist of management objectives that are weighted to reflect high level prioritization for the agency. It works by first assigning
                                     a value to each species representing their importance to each of the weighted management criteria, which then factors into a
                                     valuation of each survey. The species-survey values are discounted by frequency of occurrence and the CV in each fleet, which
                                     is related to sample size using a simple power function.  This allows changes in sample size to affect the CV, and therefore
                                     factors into the species and survey valuation and optimization. </p><p>
                                     Values are summed across species and fleets for a combined
                                     enterprise value, or score, for which to optimize. Optimization is done by adjusting survey sample sizes to maximize the enterprise
                                     score, subject to logistic constraints (ship days available) and financial constraints (costs).  This SOM highlights the tradeoffs
                                     associated with achieving different management goals, namely  commercial, recreational, ecosystem,  management importance and
                                     uniqueness(of the species specific data) criteria.</p><p>
                                     The logistical and financial constraints and can be set at any levels, median values
                                     for a range of years are included, as well as calculated % increases of 10% 25% and 50%.
                                     Overall survey values are based on the inclusion of the various species, which can be excluded, either as adults, juveniles or both.</p><p>
                                     Currently all values are in draft stage, and technical documentation on the provenance of the values is under development.  </p>"
                          )),
                          #tags$br(),
                          tags$h3(HTML(paste0("<b>Example</b>"))),
                          width = 7
                          
                          
                        )
                      )
             ),
             
             tabPanel("Data Options", 
                      
                      sidebarLayout(
                        sidebarPanel(
                          id = "sidebar",
                          tags$h1("Modify Data Inputs"),
                          tags$br(),
                          HTML(paste0(
                            "<p>Below are the inputs for the survey optimization model that can be modified with user input. Use options below to:</p>",
                            "<ul><li>View current parameters or constraints</li><li>Modify existing parameters or constraints</li><li>Upload your own sheet</li></ul>", 
                            "<p>Note that if you choose to upload your own data sheets, please ensure are .csv files with identical columns and values  provided for every 
                            species.</p>"
                            
                          )),
                          tags$br(),
                          tabsetPanel(
                            id = "selected_tab",
                            type = "tabs",
                            selected = "parameters",
                            tabPanel("Parameters", value = "parameters",
                                     tags$br(),
                                     selectInput("parameter_name", NULL, c("Objective Criteria","Species to Include","Species Valuation"), selected = "Objective Criteria")                            ),
                            tabPanel("Constraints", value = "constraints",
                                     tags$br(),
                                     selectInput("constraint_name", NULL, c("Survey Size","Totals"), selected = "Survey Size")                            )
                          ),
                          width = 5
                        ),
                        mainPanel(
                          conditionalPanel(
                            condition = "input.selected_tab == 'parameters' & input.parameter_name == 'Objective Criteria'",
                            DT::dataTableOutput('x3'),
                          ),
                          conditionalPanel(
                            condition = "input.selected_tab == 'parameters' & input.parameter_name == 'Species Valuation'",
                            DT::dataTableOutput('x1'),
                          ),
                          
                          conditionalPanel(
                            condition = "input.selected_tab == 'parameters' & input.parameter_name == 'Species to Include'",
                            DT::dataTableOutput('x5'),
                          ),
                          conditionalPanel(
                            condition = "input.selected_tab == 'constraints' & input.constraint_name == 'Survey Size'",
                            DT::dataTableOutput('x2')
                          ),
                          
                          conditionalPanel(
                            condition = "input.selected_tab == 'constraints' & input.constraint_name == 'Totals'",
                            DT::dataTableOutput('x4')
                          ),
                          
                          width = 7
                          
                          
                        )
                      )),
             tabPanel("Run the Optimizer", 
                      sidebarLayout(
                        sidebarPanel(
                          id = "sidebar",
                          tags$h1("Optimizer Results"),
                          tags$br(),
                          HTML(paste0("Below are comparions between current survey values and the optimized values given 
                                         parameters specified on the 'Data Options' tab. Use the buttons below to choose what metric 
                                      to visualize and to save your data for later comparions between scenarios.")),
                          tags$br(),
                          tags$br(),
                          radioButtons(inputId = "plot_select", 
                                       label = "Metric Options:",
                                       choices = 
                                         c("Survey Size and Cost Comparison" = "survy_size",
                                           "Species Group Totals" = "group_total", 
                                           "Survey Valuation by Species Group" = "group_value")),
                          tags$br(),
                          downloadButton(
                            outputId = "download_btn",
                            label = "Download Results"),
                          width = 5
                          
                        ), 
                        mainPanel(
                          conditionalPanel(
                            condition = "input.plot_select == 'survy_size'",
                            HTML(paste0("<b>Optimized Solution Plot</b>")),
                            plotOutput("optimized.solution.plot", height=600),
                            HTML(paste0("<b>Optimized Solution Table</b>")),
                            DT::dataTableOutput("optimized.solution"),
                          ),
                          conditionalPanel(
                            condition = "input.plot_select == 'group_value'",
                            HTML(paste0("<b>Optimized Solution Plot</b>")),
                            plotOutput("optimized.enterprise.score.plot", height=600),
                            HTML(paste0("<b>Optimized Solution Table</b>")),
                            DT::dataTableOutput("optimized.enterprise.score"),
                          ),
                          conditionalPanel(
                            condition = "input.plot_select == 'group_total'",
                            HTML(paste0("<b>Optimized Solution Plot</b>")),
                            plotOutput("optimized.enterprise.totals.plot", height=600),
                            HTML(paste0("<b>Optimized Solution Table</b>")),
                            DT::dataTableOutput("optimized.enterprise.totals"),
                          ),
                          
                          
                          width = 7
                        )
                        
                        
                      ),
             ), 
             
             
             
             tabPanel("Compare Scenarios", 
                      sidebarLayout(
                        sidebarPanel(
                          id = "sidebar",
                          tags$h1("Compare Saved Scenarios"),
                          tags$br(),
                          HTML(paste0("Here you can upload your saved simulation results to compare key features of each.")),
                          tags$br(),

                          width = 5
                          
                        ), 
                        mainPanel(width = 7)
                        )), 
             
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  ## This will create the original objective weights data frame that is rendered on the table to start 
  objective.weights = data.frame(values = c("commercial_value","recreational_value","ecosystem_value",
                                            "management_importantce",	"uniqueness"), 
                                 weights = c(1,1,1,1,1))
  
  ## This will create the original upper/ lower bounds data frame that is rendered on the table to start 
  bounds = data.frame(values =c("spring_trawl", "fall_trawl", "seamap_bll", "nmfs_bll", "camera_reef", "sum_plank_bongo",
                                "sum_plan_neuston", "fall_plank_bongo", "fall_plank_neust", "nmfs_small_pelagics"), 
                      lower.bound =  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                      upper.bound = c(346, 302, 160, 165, 1494, 56, 56, 72, 72, 122))
  
  ## This will create the original species valuation data frame that is rendered on the table to start 
  species.value = read.csv("data/species_value.csv") %>% 
    dplyr::select(-include)
  
  ## This will create the total parameter $$ / amount 
  totals = data.frame(values = c("Total Cost","Total Survey Number"), 
                      totals = c(6866963, 2590))
  
  ## This is %Freq_{species, survey} data - it does not change 
  species.survey.freq = read.csv("data/species_pct_frequency.csv") %>% 
    dplyr::mutate(species = tolower(species)) 
  # Replace all NA values with 0 (NAs are the empty cells from the excel sheet)
  species.survey.freq[is.na(species.survey.freq)] = 0
  col_order <- c("Group", "species", "life_stage",
                 "spring_trawl", "fall_trawl", "seamap_bll","nmfs_bll", "camera_reef", "sum_plank_bongo", "sum_plan_neuston", "fall_plank_bongo", 
                 "fall_plank_neust", "nmfs_small_pelagics")
  species.survey.freq = species.survey.freq[, col_order]
  
  ## This will create the include species data frame
  species.include = species.survey.freq %>% mutate_if(is.numeric, ~1 * (. > 0)) 
  
  ##################################################################################################################################################
  # Reactive values for input 
  ##################################################################################################################################################
  
  # New, reactive elements 
  bounds.new <- reactiveValues(data = bounds)
  bounds.original <- bounds
  
  objective.weights.new <- reactiveValues(data = objective.weights)
  objective.weights.original <- objective.weights
  
  species.value.new = reactiveValues(data=species.value)
  species.value.original = species.value
  
  totals.new = reactiveValues(data=totals)
  totals.original = totals
  
  species.include.new = reactiveValues(data=species.include)
  species.include.original = species.include
  
  # This renders the datatable on the first page
  output$x2<-renderDT(
    bounds.new$data,
    selection = 'none', 
    editable = TRUE,
    rownames = TRUE)
  
  # This updates and saves the new data 
  observeEvent(input$x2_cell_edit, {
    req(input$x2_cell_edit)
    
    # If you have a non-numeric edit, it messes up the optimization routine unless this is added (switches empties/character to zeros)
    cell.edit.value = input$x2_cell_edit$value
    if(is.numeric(cell.edit.value) == FALSE){cell.edit.value = 0}
    
    # Replace values with user input
    bounds[input$x2_cell_edit$row,input$x2_cell_edit$col] <<- cell.edit.value
    bounds.new$data <- bounds
  })
  
  # This renders the datatable on the first page
  output$x3 <- renderDT(
    objective.weights.new$data,
    selection = 'none',
    editable = TRUE,
    rownames = TRUE)
  
  # This updates and saves the new data 
  observeEvent(input$x3_cell_edit, {
    req(input$x3_cell_edit)
    
    # If you have a non-numeric edit, it messes up the optimization routine unless this is added (switches empties/character to zeros)
    cell.edit.value = input$x3_cell_edit$value
    if(is.numeric(cell.edit.value) == FALSE){cell.edit.value = 0}
    
    # Replace values with user input
    objective.weights[input$x3_cell_edit$row,input$x3_cell_edit$col] <<- cell.edit.value
    objective.weights.new$data <- objective.weights
  })
  
  # This renders the datatable on the first page
  output$x1 <- renderDT(
    species.value.new$data,
    selection = 'none',
    editable = TRUE,
    rownames = TRUE)
  
  # This updates and saves the new data 
  observeEvent(input$x1_cell_edit, {
    req(input$x1_cell_edit)
    
    # If you have a non-numeric edit, it messes up the optimization routine unless this is added (switches empties/character to zeros)
    cell.edit.value = input$x1_cell_edit$value
    if(is.numeric(cell.edit.value) == FALSE){cell.edit.value = 0}
    
    # Replace values with user input
    species.value[input$x1_cell_edit$row,input$x1_cell_edit$col] <<- cell.edit.value
    species.value.new$data <- species.value
  })
  
  
  # This renders the datatable on the first page
  output$x4 <- renderDT(
    totals.new$data,
    selection = 'none',
    editable = TRUE,
    rownames = TRUE)
  
  # This updates and saves the new data 
  observeEvent(input$x4_cell_edit, {
    req(input$x4_cell_edit)
    
    # If you have a non-numeric edit, it messes up the optimization routine unless this is added (switches empties/character to zeros)
    cell.edit.value = input$x4_cell_edit$value
    if(is.numeric(cell.edit.value) == FALSE){cell.edit.value = 0}
    
    # Replace values with user input
    totals[input$x4_cell_edit$row,input$x4_cell_edit$col] <<- cell.edit.value
    totals.new$data <- totals
  })
  
  # This renders the datatable on the first page
  output$x5 <- renderDT(
    species.include.new$data,
    selection = 'none',
    editable = TRUE,
    rownames = TRUE)
  
  # This updates and saves the new data 
  observeEvent(input$x5_cell_edit, {
    req(input$x5_cell_edit)
    
    # If you have a non-numeric edit, it messes up the optimization routine unless this is added (switches empties/character to zeros)
    cell.edit.value = input$x5_cell_edit$value
    if(is.numeric(cell.edit.value) == FALSE){cell.edit.value = 0}
    
    # Replace values with user input
    species.include[input$x5_cell_edit$row,input$x5_cell_edit$col] <<- cell.edit.value
    species.include.new$data <- species.include
  })
  
  
  ##################################################################################################################################################
  # Getting data/ running optimization 
  ##################################################################################################################################################
  
  frequency.data <- reactive({
    
    # Get the necessary parameters from reactive data 
    objective.weights.new = data.frame(objective.weights.new$data) 
    upper.bound = data.frame(bounds.new$data)$upper.bound
    lower.bound = data.frame(bounds.new$data)$lower.bound
    species.value.new = data.frame(species.value.new$data) 
    max.cost =  data.frame(totals.new$data)[1,2]
    max.capacity = data.frame(totals.new$data)[2,2]
    species.include.new = data.frame(species.include.new$data) 
      
    # Calculate the weighted average for each species 
    survey.value = species.value.new %>% 
      dplyr::mutate(species = tolower(species)) %>% 
      dplyr::mutate(commercial_value = commercial_value * objective.weights.new$weights[objective.weights.new$values == "commercial_value"], 
                    recreational_value = recreational_value * objective.weights.new$weights[objective.weights.new$values == "recreational_value"], 
                    ecosystem_value = ecosystem_value * objective.weights.new$weights[objective.weights.new$values == "ecosystem_value"], 
                    management_importantce = management_importantce * objective.weights.new$weights[objective.weights.new$values == "management_importantce"], 
                    uniqueness = uniqueness * objective.weights.new$weights[objective.weights.new$values == "uniqueness"]) %>% 
      dplyr::rowwise() %>% 
      dplyr::mutate(total_value = (sum(commercial_value, recreational_value, ecosystem_value, management_importantce, uniqueness))/sum(objective.weights.new$weights)) %>%  
      dplyr::ungroup()
    
    # Open power parameter file 
    species.survey.power = read.csv("data/species_power_parameter.csv") %>% 
      dplyr::mutate(species = tolower(species)) %>% 
      gather("survey","power_param",-c(species, life_stage, Group))
    
    # Gather the include file 
    species.include.new =   species.include.new %>% 
      gather("survey","include",-c(species, life_stage, Group)) 

    # Now we want to calculate the %Freq_{species, survey} * Value_{species}
    species.survey.freq.value = left_join(species.survey.freq, survey.value[, c("species", "life_stage","total_value")], by=c("species", "life_stage")) %>% 
      gather("survey","pct_freq",-c(species, life_stage, Group, total_value)) %>% 
      left_join(., species.survey.power, by=c("species", "life_stage", "Group", "survey")) %>% 
      left_join(., species.include.new, by=c("species", "life_stage", "Group", "survey")) %>% 
      dplyr::mutate(freq_value = total_value * pct_freq * include) 
    
    return(species.survey.freq.value)
  })
  
  optimized.data <- reactive({
    
    # Get the necessary parameters from reactive data 
    upper.bound = data.frame(bounds.new$data)$upper.bound
    lower.bound = data.frame(bounds.new$data)$lower.bound
    max.cost =  data.frame(totals.new$data)[1,2]
    max.capacity = data.frame(totals.new$data)[2,2]
    
    species.survey.freq.value = frequency.data()
    
    survey.names = c("spring_trawl", "fall_trawl", "seamap_bll", "nmfs_bll", "camera_reef", "sum_plank_bongo",
                     "sum_plan_neuston", "fall_plank_bongo", "fall_plank_neust", "nmfs_small_pelagics")
    survey.size.current = c(315, 275, 146, 150, 1359, 51, 51, 66, 66, 111)
    survey.n = rep(10, 10)
    #survey.n = c(1000, 1000, 1000, 1000, 1500, 1000, 1000, 1000, 1000, 1000)-100
    
    cost.per.survey = c(2910, 3414, 2830, 3600, 1871, 1854, 1854, 2555, 2555, 4172)
    
    ## More constraints - this time it's cost constraints. They will be inequality constraints.   
    # NOTE - I'm not sure what the difference between "cost per" and "current cost" is on the sheet 
    
    con <- function(survey.n) {
      f = NULL
      f = rbind(f, sum(survey.n * cost.per.survey) - max.cost)
      f = rbind(f, sum(survey.n) - max.capacity)
      return(list(ceq = NULL, c = f))
    }
    
    ## Objective function 
    # Objective function is %Freq_{survey, species}*Value_{species} * (1-survey.size^{-cv.param_{survey, species}})
    # You need to multiply objective function by 1000 to get a value that's not too sensitive
    
    obj=function(survey.n){
      
      survey.names = c("spring_trawl", "fall_trawl", "seamap_bll", "nmfs_bll", "camera_reef", "sum_plank_bongo",
                       "sum_plan_neuston", "fall_plank_bongo", "fall_plank_neust", "nmfs_small_pelagics")
      
      # Spring trawl 
      spring.trawl.power = (survey.n[1]^(-1 * species.survey.freq.value$power_param[species.survey.freq.value$survey == survey.names[1]]))
      spring.trawl.sum = sum(species.survey.freq.value$freq_value[species.survey.freq.value$survey == survey.names[1]] * (1-spring.trawl.power)* 100000)
      
      # Fall trawl 
      fall.trawl.power = (survey.n[2]^(-1 * species.survey.freq.value$power_param[species.survey.freq.value$survey == survey.names[2]]))
      fall.trawl.sum = sum(species.survey.freq.value$freq_value[species.survey.freq.value$survey == survey.names[2]] * (1-fall.trawl.power)* 100000)
      
      # Seamap BLL 
      seamap.bll.power = (survey.n[3]^(-1 * species.survey.freq.value$power_param[species.survey.freq.value$survey == survey.names[3]]))
      seamap.bll.sum = sum(species.survey.freq.value$freq_value[species.survey.freq.value$survey == survey.names[3]] * (1-seamap.bll.power)* 100000)
      
      # NMFS BLL 
      nmfs.bll.power = (survey.n[4]^(-1 * species.survey.freq.value$power_param[species.survey.freq.value$survey == survey.names[4]]))
      nmfs.bll.sum = sum(species.survey.freq.value$freq_value[species.survey.freq.value$survey == survey.names[4]] * (1-nmfs.bll.power)* 100000)
      
      # Camera Reef 
      camera.reef.power = (survey.n[5]^(-1 * species.survey.freq.value$power_param[species.survey.freq.value$survey == survey.names[5]]))
      camera.reef.sum = sum(species.survey.freq.value$freq_value[species.survey.freq.value$survey == survey.names[5]] * (1-camera.reef.power)* 100000)
      
      # Sum Plank Bongo 
      sum.plank.bongo.power = (survey.n[6]^(-1 * species.survey.freq.value$power_param[species.survey.freq.value$survey == survey.names[6]]))
      sum.plank.bongo.sum = sum(species.survey.freq.value$freq_value[species.survey.freq.value$survey == survey.names[6]] * (1-sum.plank.bongo.power)* 100000)
      
      # Sum Plan Neuston 
      sum.plan.neuston.power = (survey.n[7]^(-1 * species.survey.freq.value$power_param[species.survey.freq.value$survey == survey.names[7]]))
      sum.plan.neuston.sum = sum(species.survey.freq.value$freq_value[species.survey.freq.value$survey == survey.names[7]] * (1-sum.plan.neuston.power)* 100000)
      
      # Fall Plank Bongo
      fall.plank.bongo.power = (survey.n[8]^(-1 * species.survey.freq.value$power_param[species.survey.freq.value$survey == survey.names[8]]))
      fall.plank.bongo.sum = sum(species.survey.freq.value$freq_value[species.survey.freq.value$survey == survey.names[8]] * (1-fall.plank.bongo.power)* 100000)
      
      # Fall Plank Neust
      fall.plank.neust.power = (survey.n[9]^(-1 * species.survey.freq.value$power_param[species.survey.freq.value$survey == survey.names[9]])) 
      fall.plank.neust.sum = sum(species.survey.freq.value$freq_value[species.survey.freq.value$survey == survey.names[9]] * (1-fall.plank.neust.power)* 100000)
      
      # NMFS Small Pelagics 
      nmfs.small.pelagics.power = (survey.n[10]^(-1 * species.survey.freq.value$power_param[species.survey.freq.value$survey == survey.names[10]])) 
      nmfs.small.pelagics.sum = sum(species.survey.freq.value$freq_value[species.survey.freq.value$survey == survey.names[10]] * (1-nmfs.small.pelagics.power)* 100000) 
      
      return(-sum(spring.trawl.sum, fall.trawl.sum, 
                  seamap.bll.sum, nmfs.bll.sum, 
                  camera.reef.sum, 
                  sum.plank.bongo.sum, sum.plan.neuston.sum, 
                  fall.plank.bongo.sum, fall.plank.neust.sum, 
                  nmfs.small.pelagics.sum))
      
    }
    
    result <- solnl(X = survey.n, objfun = obj, confun = con, 
                    lb = lower.bound, ub = upper.bound, 
                    tolFun = 1e-08, tolCon = 1e-08, maxnFun = 1e+08, maxIter = 8000, tolX = 1e-07)
    
    
    final.table = data.frame(Survey=c("Spring Trawl","Fall Trawl","Seamap BLL", "NMFS BLL",
                                      "Camera Reef","Summer Plankton Bongo","Summer Plankton Neust",
                                      "Fall Plankton Bongo","Fall Plankton Neust","NMFS Small Pelagics"), 
                             Optimized.N = round(result$par, digits=0), 
                             Current.N = round(survey.size.current, digits=0), 
                             Optimized.Cost = round(result$par * cost.per.survey, digits=0), 
                             Current.Cost = round(survey.size.current * cost.per.survey, digits=0))
    
    final.table
  })
  
  enterprise.score <- reactive({
    survey.names = c("spring_trawl", "fall_trawl", "seamap_bll", "nmfs_bll", "camera_reef", "sum_plank_bongo",
                     "sum_plan_neuston", "fall_plank_bongo", "fall_plank_neust", "nmfs_small_pelagics")
    
    species.survey.freq.value = frequency.data()
    final.table.n = optimized.data()
    final.table.n$survey = survey.names
    new.values = left_join(species.survey.freq.value, final.table.n[, c("Survey","survey","Optimized.N", "Current.N")], by="survey") %>% 
      rowwise() %>% 
      dplyr::mutate(Optimized.enterprise.score = log(Optimized.N^(-1*power_param))) %>% 
      dplyr::mutate(Optimized.enterprise.score = freq_value * (1-Optimized.enterprise.score)) %>% 
      dplyr::mutate(Current.enterprise.score = log(Current.N^(-1*power_param))) %>% 
      dplyr::mutate(Current.enterprise.score = freq_value * (1-Current.enterprise.score)) %>% 
      dplyr::ungroup() %>% 
      dplyr::select(Group, Survey, Optimized.enterprise.score, Current.enterprise.score) %>% 
      dplyr::group_by(Group, Survey) %>% 
      dplyr::summarize(Optimized.enterprise.score = sum(Optimized.enterprise.score), 
                       Current.enterprise.score = sum(Current.enterprise.score)) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(Optimized.enterprise.score = round(Optimized.enterprise.score, 2)) %>%
      dplyr::mutate(Current.enterprise.score = round(Current.enterprise.score, 2)) 
    
    return(new.values)
  })
  ##################################################################################################################################################
  # Generating plots 
  ##################################################################################################################################################
  
  output$optimized.solution <- DT::renderDataTable({
    
    #bounds.new$data
    optimized.data() %>%
      rbind(., data.frame(Survey="Total Number",
                          Optimized.N=sum(optimized.data()$Optimized.N),
                          Current.N=sum(optimized.data()$Current.N), 
                          Optimized.Cost=sum(optimized.data()$Optimized.Cost),
                          Current.Cost=sum(optimized.data()$Current.Cost))) %>%
      datatable(rownames = T, # set rownames T
                options = list(columnDefs = list(list(visible = F, targets = 0)),
                               pageLength = 15, info = FALSE)) %>%
      formatStyle(
        0, target = "row",
        fontWeight = styleEqual(11, "bold"))
  })
  
  output$optimized.solution.plot <- renderPlot({
    
    optimized.data() %>% 
      gather("Type","Survey Metric", -Survey) %>% 
      dplyr::mutate(Metric = case_when(Type %in% c("Optimized.N", "Current.N") ~ "Number",
                                       Type %in% c("Optimized.Cost", "Current.Cost") ~ "Cost ($)")) %>% 
      dplyr::mutate(Type = case_when(Type %in% c("Optimized.N", "Optimized.Cost") ~ "Optimized",
                                     Type %in% c("Current.N", "Current.Cost") ~ "Current")) %>% 
      ggplot(aes(y=`Survey Metric`, x=Survey, fill=Type)) + 
      geom_bar(stat="identity", position='dodge', color="black") + 
      theme_half_open(12) + 
      xlab("")+
      facet_wrap(~Metric, nrow=2, scales="free_y")+
      scale_fill_manual(values=c("gray","darkgreen"))+
      theme(axis.text.x = element_text(angle = 45,hjust=1)) + 
      theme(legend.position = "top", 
            legend.title = element_blank())
    
  })
  
  output$optimized.enterprise.score <- DT::renderDataTable({
    
    #bounds.new$data
    enterprise.score()  %>%
      datatable(rownames = T, # set rownames T
                options = list(columnDefs = list(list(visible = F, targets = 0)),
                               pageLength = 15, info = FALSE))
  })
  
  output$optimized.enterprise.score.plot <- renderPlot({ 
    
    enterprise.score() %>% 
      ggplot(aes(x=Survey, y=Optimized.enterprise.score, fill=Group)) + 
      geom_bar(position="stack", stat="identity", color="black")+
      theme_half_open(12) + 
      xlab("")+
      ylab("Enterprise Score")+
      #scale_fill_manual(values=c("gray","darkgreen"))+
      theme(axis.text.x = element_text(angle = 45,hjust=1)) + 
      theme(legend.position = "top", 
            legend.title = element_blank())
    
  })
  
  output$optimized.enterprise.totals <- DT::renderDataTable({
    
    #bounds.new$data
    enterprise.score()  %>%
      dplyr::group_by(Group) %>% 
      dplyr::summarize(Optimized.enterprise.score = sum(Optimized.enterprise.score), 
                       Current.enterprise.score = sum(Current.enterprise.score)) %>% 
      dplyr::ungroup() %>% 
      datatable(rownames = T, # set rownames T
                options = list(columnDefs = list(list(visible = F, targets = 0)),
                               pageLength = 15, info = FALSE))
  })
  
  output$optimized.enterprise.totals.plot <- renderPlot({ 
    
    enterprise.score() %>% 
      dplyr::group_by(Group) %>% 
      dplyr::summarize(Optimized.enterprise.score = sum(Optimized.enterprise.score), 
                       Current.enterprise.score = sum(Current.enterprise.score)) %>% 
      dplyr::ungroup() %>% 
      gather("Type","Enterprise Score", -Group) %>% 
      dplyr::mutate(Type = case_when(Type %in% c("Optimized.enterprise.score") ~ "Optimized",
                                     Type %in% c("Current.enterprise.score") ~ "Current")) %>% 
      ggplot(aes(x=Group, y=`Enterprise Score`, fill=Type)) + 
      geom_bar(position="dodge", stat="identity", color="black")+
      theme_half_open(12) + 
      xlab("")+
      ylab("Enterprise Score")+
      scale_fill_manual(values=c("gray","darkgreen"))+
      theme(axis.text.x = element_text(angle = 45,hjust=1)) + 
      theme(legend.position = "top", 
            legend.title = element_blank())
    
    
  })

  
  
  output$download_btn <- downloadHandler(
    #filename = function() {paste0(input$main, " vs ", input$control, " ", input$obRange[1], "-", input$obRange[2], ".xlsx")},
    
    filename = function() {paste("SOM_results_", Sys.Date(), ".xlsx", sep = "")},
    content = function(file) {
      
      fname <- paste(file,"xlsx",sep=".")
      wb <- createWorkbook("Survey_shinyapp", "Survey results")
      addWorksheet(wb, sheetName = "objective_weights")
      addWorksheet(wb, sheetName = "bounds")
      addWorksheet(wb, sheetName = "totals")
      addWorksheet(wb, sheetName = "data")
      writeData(wb, data.frame(objective.weights.new$data), sheet = "objective_weights")
      writeData(wb,  data.frame(bounds.new$data), sheet = "bounds")
      writeData(wb,  data.frame(totals.new$data), sheet = "totals")
      writeData(wb, enterprise.score(), sheet = "data")
      saveWorkbook(wb, file = file)
      
    },
    contentType = "file/xlsx"
  )
  
  output$TEMP <- renderText({
    paste0("x=", "Dumb")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
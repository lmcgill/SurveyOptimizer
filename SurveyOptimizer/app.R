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
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")

setwd("/Users/lillianmcgill/Documents/SurveyOptimizer")

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
             
             tabPanel("Data Options", 
                      
                      sidebarLayout(
                        sidebarPanel(
                          id = "sidebar",
                          tags$h1("Modify Data Inputs"),
                          tags$br(),
                          HTML(paste0(
                            "<p>Below are the inputs for the survey optimization model that can be modified with user input. Use options below to:</p>",
                            "<ul><li>View current parameters or constraints</li><li>Modify existing parameters or constraints</li><li>Upload your own sheet</li></ul>"
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
                                     selectInput("constraint_name", NULL, c("Survey Size","Survey Money","Totals"), selected = "Survey Size")                            )
                          ),
                          width = 5
                        ),
                        
                        
                          mainPanel(
                            
                            conditionalPanel(
                              condition = "input.selected_tab == 'parameters' & input.parameter_name == 'Objective Criteria'",
          
                              DT::dataTableOutput('x3'),
                              
                            ),
                            
                            conditionalPanel(
                              condition = "input.selected_tab == 'constraints' & input.constraint_name == 'Survey Size'",
                              
                              DT::dataTableOutput('x2')
                            ),

                        width = 7
                        
                        
                      )
                      )),
             tabPanel("Run the Optimizer", 
                      fluidRow(
                         span(tags$h3("Below are the optimzed and current survey size and cost, given the
                                         parameters specified on the 'Data Options' tab.")),

                        splitLayout(DT::dataTableOutput("optimized.solution"), plotOutput("optimized.solution.plot", height=600),# dataTableOutput("objective.weights"),
                                    cellArgs = list(style = "padding: 15px"),
                                    cellWidths = c("40%", "60%"))
                      )),
             
                      # mainPanel(
                      #   span(tags$i(h6("Below are the optimzed survey sizes and current survey sizes, given the
                      #                   parameters specified on the 'Data Options' tab.")), style="color:#045a8d"),
                      # 
                      #   DT::dataTableOutput("optimized.solution"),
                      #   plotOutput("optimized.solution.plot", height=800)
                      # 
                      # )),
             tabPanel("Plot and Save"), 
             tabPanel("Compare Scenarios")
             
             )
             )

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
  ## This will create the original data frame that is rendered on the table to start 
  objective.weights = data.frame(values = c("commercial_value","recreational_value","ecosystem_value",
                                            "management_importantce",	"uniqueness"), 
                                 weights = c(1,1,1,1,1))
  
  bounds = data.frame(values =c("spring_trawl", "fall_trawl", "seamap_bll", "nmfs_bll", "camera_reef", "sum_plank_bongo",
                                         "sum_plan_neuston", "fall_plank_bongo", "fall_plank_neust", "nmfs_small_pelagics"), 
                       lower.bound =  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                       upper.bound = c(346, 302, 160, 165, 1494, 56, 56, 72, 72, 122))
  
  bounds.new <- reactiveValues(data = bounds)
  bounds.original <- bounds
  
  objective.weights.new <- reactiveValues(data = objective.weights)
  objective.weights.original <- objective.weights
  
  # This renders the datatable on the first page
  output$x2<-renderDT(
    bounds.new$data,
    selection = 'none', 
    editable = TRUE,
    rownames = TRUE)
  
  # This updates and saves the new data 
  observeEvent(input$x2_cell_edit, {
    req(input$x2_cell_edit)
    bounds[input$x2_cell_edit$row,input$x2_cell_edit$col] <<- input$x2_cell_edit$value
    bounds.new$data <- bounds
  })
  
  
  # This is the output that will show on the App 
  # output$x2<-renderDT(
  #   bounds.new$data,selection = 'none', editable = TRUE,
  #   rownames = TRUE)
  
  output$x3 <- renderDT(
    objective.weights.new$data,
    selection = 'none',
    editable = TRUE,
    rownames = TRUE)
  
  
  # This is trying to save the new dataframe to be used in later optimization routines 
  # This updates and saves the new data 
  observeEvent(input$x3_cell_edit, {
    req(input$x3_cell_edit)
    objective.weights[input$x3_cell_edit$row,input$x3_cell_edit$col] <<- input$x3_cell_edit$value
    objective.weights.new$data <- objective.weights
  })
  
  optimized.data <- reactive({
    
    # Get the objective weights 
    objective.weights.new = data.frame(objective.weights.new$data)
    upper.bound = data.frame(bounds.new$data)$upper.bound
    lower.bound = data.frame(bounds.new$data)$lower.bound
    

    # Read in a table of species value (or use default values provided by Joel) 
    species.value = read.csv("data/species_value.csv")
    # Calculate the weighted average for each species 
    species.value = species.value %>% 
      dplyr::mutate(species = tolower(species)) %>% 
      dplyr::mutate(commercial_value = commercial_value * objective.weights.new$weights[objective.weights.new$values == "commercial_value"], 
                    recreational_value = recreational_value * objective.weights.new$weights[objective.weights.new$values == "recreational_value"], 
                    ecosystem_value = ecosystem_value * objective.weights.new$weights[objective.weights.new$values == "ecosystem_value"], 
                    management_importantce = management_importantce * objective.weights.new$weights[objective.weights.new$values == "management_importantce"], 
                    uniqueness = uniqueness * objective.weights.new$weights[objective.weights.new$values == "uniqueness"]) %>% 
      dplyr::rowwise() %>% 
      dplyr::mutate(total_value = (sum(commercial_value, recreational_value, ecosystem_value, management_importantce, uniqueness) * include)/sum(objective.weights.new$weights)) %>%  
      dplyr::ungroup()
    
    # The %Freq_{species, survey} is determined by data that does not change 
    species.survey.freq = read.csv("data/species_pct_frequency.csv") %>% 
      dplyr::mutate(species = tolower(species)) 
    # Replace all NA values with 0 (NAs are the empty cells from the excel sheet)
    species.survey.freq[is.na(species.survey.freq)] = 0
    
    # Open power parameter folder 
    species.survey.power = read.csv("data/species_power_parameter.csv") %>% 
      dplyr::mutate(species = tolower(species)) %>% 
      gather("survey","power_param",-c(species, life_stage, Group))
    
    # Now we want to calculate the %Freq_{species, survey} * Value_{species}
    species.survey.freq.value = left_join(species.survey.freq, species.value[, c("species", "life_stage","total_value")], by=c("species", "life_stage")) %>% 
      gather("survey","pct_freq",-c(species, life_stage, Group, total_value)) %>% 
      left_join(., species.survey.power, by=c("species", "life_stage", "Group", "survey")) %>% 
      dplyr::mutate(freq_value = total_value * pct_freq) %>% 
      dplyr::select(survey, freq_value, power_param)
    
    
    survey.names = c("spring_trawl", "fall_trawl", "seamap_bll", "nmfs_bll", "camera_reef", "sum_plank_bongo",
                     "sum_plan_neuston", "fall_plank_bongo", "fall_plank_neust", "nmfs_small_pelagics")
    survey.size.current = c(315, 275, 146, 150, 1359, 51, 51, 66, 66, 111)
    survey.n = c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2)
    cost.per.survey = c(2910, 3414, 2830, 3600, 1871, 1854, 1854, 2555, 2555, 4172)
    
    #survey.n = c(215, 175, 46, 50, 1259, 51, 51, 66, 66, 11)
    #upper.bound = c(500, 500, 250, 300, 2000, 150, 150, 200, 200, 200)
    
    ## More constraints - this time it's cost constraints. They will be inequality constraints.   
    # NOTE - I'm not sure what the difference between "cost per" and "current cost" is on the sheet 
    
    con <- function(survey.n) {
      max.cost =  6866963 
      #max.cost =  4866963 
      max.capacity = 2590
      #max.capacity = 2000
      
      f = NULL
      f = rbind(f, sum(survey.n * cost.per.survey) - max.cost)
      f = rbind(f, sum(survey.n) - max.capacity)
      return(list(ceq = NULL, c = f))
    }
    
    ## Objective function 
    # Objective function is %Freq_{survey, species}*Value_{species} * (1-survey.size^{-cv.param_{survey, species}})
    # You need to multply objective function by 1000 to get a value that's not too sensitive
    
    obj=function(survey.n){
      
      survey.names = c("spring_trawl", "fall_trawl", "seamap_bll", "nmfs_bll", "camera_reef", "sum_plank_bongo",
                       "sum_plan_neuston", "fall_plank_bongo", "fall_plank_neust", "nmfs_small_pelagics")
      
      # Spring trawl 
      spring.trawl.power = survey.n[1]^(-1 * species.survey.freq.value$power_param[species.survey.freq.value$survey == survey.names[1]])
      spring.trawl.sum = sum(species.survey.freq.value$freq_value[species.survey.freq.value$survey == survey.names[1]] * (1-spring.trawl.power))* 10
      
      # Fall trawl 
      fall.trawl.power = survey.n[2]^(-1 * species.survey.freq.value$power_param[species.survey.freq.value$survey == survey.names[2]])
      fall.trawl.sum = sum(species.survey.freq.value$freq_value[species.survey.freq.value$survey == survey.names[2]] * (1-fall.trawl.power))* 10
      
      # Seamap BLL 
      seamap.bll.power = survey.n[3]^(-1 * species.survey.freq.value$power_param[species.survey.freq.value$survey == survey.names[3]])
      seamap.bll.sum = sum(species.survey.freq.value$freq_value[species.survey.freq.value$survey == survey.names[3]] * (1-seamap.bll.power))* 10
      
      # NMFS BLL 
      nmfs.bll.power = survey.n[4]^(-1 * species.survey.freq.value$power_param[species.survey.freq.value$survey == survey.names[4]])
      nmfs.bll.sum = sum(species.survey.freq.value$freq_value[species.survey.freq.value$survey == survey.names[4]] * (1-nmfs.bll.power))* 10
      
      # Camera Reef 
      camera.reef.power = survey.n[5]^(-1 * species.survey.freq.value$power_param[species.survey.freq.value$survey == survey.names[5]])
      camera.reef.sum = sum(species.survey.freq.value$freq_value[species.survey.freq.value$survey == survey.names[5]] * (1-camera.reef.power))* 10
      
      # Sum Plank Bongo 
      sum.plank.bongo.power = survey.n[6]^(-1 * species.survey.freq.value$power_param[species.survey.freq.value$survey == survey.names[6]])
      sum.plank.bongo.sum = sum(species.survey.freq.value$freq_value[species.survey.freq.value$survey == survey.names[6]] * (1-sum.plank.bongo.power))* 10
      
      # Sum Plan Neuston 
      sum.plan.neuston.power = survey.n[7]^(-1 * species.survey.freq.value$power_param[species.survey.freq.value$survey == survey.names[7]])
      sum.plan.neuston.sum = sum(species.survey.freq.value$freq_value[species.survey.freq.value$survey == survey.names[7]] * (1-sum.plan.neuston.power))* 10
      
      # Fall Plank Bongo
      fall.plank.bongo.power = survey.n[8]^(-1 * species.survey.freq.value$power_param[species.survey.freq.value$survey == survey.names[8]])
      fall.plank.bongo.sum = sum(species.survey.freq.value$freq_value[species.survey.freq.value$survey == survey.names[8]] * (1-fall.plank.bongo.power))* 10
      
      # Fall Plank Neust
      fall.plank.neust.power = survey.n[9]^(-1 * species.survey.freq.value$power_param[species.survey.freq.value$survey == survey.names[9]])
      fall.plank.neust.sum = sum(species.survey.freq.value$freq_value[species.survey.freq.value$survey == survey.names[9]] * (1-fall.plank.neust.power))* 10
      
      # NMFS Small Pelagics 
      nmfs.small.pelagics.power = survey.n[10]^(-1 * species.survey.freq.value$power_param[species.survey.freq.value$survey == survey.names[10]])
      nmfs.small.pelagics.sum = sum(species.survey.freq.value$freq_value[species.survey.freq.value$survey == survey.names[10]] * (1-nmfs.small.pelagics.power)) * 10
      
      return(-sum(spring.trawl.sum, fall.trawl.sum, 
                  seamap.bll.sum, nmfs.bll.sum, 
                  camera.reef.sum, 
                  sum.plank.bongo.sum, sum.plan.neuston.sum, 
                  fall.plank.bongo.sum, fall.plank.neust.sum, 
                  nmfs.small.pelagics.sum))
      
    }
    
    result <- solnl(X = survey.n, objfun = obj, confun = con, 
                    lb = lower.bound, ub = upper.bound, 
                    tolFun = 1e-08, tolCon = 1e-08, maxnFun = 1e+011, maxIter = 8000)
    
    final.table = data.frame(Survey=c("Spring Trawl","Fall Trawl","Seamap BLL", "NMFS BLL",
                                      "Camera Reef","Summer Plankton Bongo","Summer Plankton Neust",
                                      "Fall Plankton Bongo","Fall Plankton Neust","NMFS Small Pelagics"), 
                             Optimized.N = round(result$par, digits=0), 
                             Current.N = round(survey.size.current, digits=0), 
                             Optimized.Cost = round(result$par * cost.per.survey, digits=0), 
                             Current.Cost = round(survey.size.current * cost.per.survey, digits=0))
    
    final.table
  })
  
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
                                 pageLength = 15, info = FALSE,dom = 't')) %>%
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
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)

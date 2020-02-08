################################
## R Shiny EwE Results
################################
library(readxl)
load('results.RData')
FGs = cost_TL$FG

cost_TL$inputID = c( 'Coastal_Pel',
  rep("MarMam_Turtles", 5), rep('RFish', 9),  rep('Meso_Subphotic_Fish', 5),
  'Coastal_Pel', 'Micronekton', rep('BFish', 3), rep('Coastal_Pel', 3), 
  rep('Macrobenthos', 5), rep('Other_Benthos', 5), rep('StructBen', 6), 
  rep("P", 5), 'Micronekton', rep('Z', 4), 'DET')

Groups = data.frame(
  valname = c('RFish', 'Meso_Subphotic_Fish', 'BFish', 'Coastal_Pel', 
              "MarMam_Turtles", 'Micronekton', 'Macrobenthos', 'Other_Benthos', 
              'StructBen', 'P', 'Z', 'DET'),
  label = c('Coral Reef Fishes', 'Mesopelagic and Subphotic Fishes', 
            'Deepwater Bottomfishes', 'Coastal Pelagic Fishes', 
            'Marine Mammals and Turtles', 'Micronekton', 'Macrobenthos', 
            'Other Benthos', 'Structural Benthic Species', 'Primary Producers', 
            'Zooplankton', 'Detritus'), stringsAsFactors = F)

# Define UI
ui <- navbarPage("EwE Mock Shiny",
                 tabPanel("Biomass by Functional Group",
                          fluidPage(
                            
                            sidebarLayout(
                              # Sidebar with a slider input
                              sidebarPanel(
                                selectInput(
                                  inputId = 'scen',
                                  label = 'Effort Scenario',
                                  choices = list('BRFA Scenario 1' = 'BRFA1',
                                                 'BRFA Scenario 2' = 'BRFA2',
                                                 'Constant Effort' = 'ConstantEffort', 
                                                 'Line Only' = 'LineOnly', 
                                                 'No Spearfishing' = 'NoSpear',
                                                 'No Herbivore Fishing' = 'NoHerb', 
                                                 'No Net Fishing' = 'NoNet'),
                                  selected = 'ConstantEffort"'),
                                
                                actionLink("selectall_RFish",
                                           "Select/Deselect All"),
                                checkboxGroupInput(
                                  inputId = 'RFish',
                                  inline = T,
                                  label = 'Coral Reef Fishes',
                                  selected = NULL,
                                  choiceValues = FGs[7:15],
                                  choiceNames = cost_TL$lab[7:15]),
                                
                                actionLink("selectall_Meso_Subphotic_Fish",
                                           "Select/Deselect All"),
                                checkboxGroupInput(
                                  inputId = 'Meso_Subphotic_Fish',
                                  inline = T,
                                  label = 'Mesopelagic and Subphotic Fishes',
                                  selected = NULL,
                                  choiceValues = FGs[16:20],
                                  choiceNames = cost_TL$lab[16:20]),
                                
                                actionLink("selectall_BFish",
                                           "Select/Deselect All"),
                                checkboxGroupInput(
                                  inputId = 'BFish',
                                  inline = T,
                                  label = 'Deepwater Bottomfishes',
                                  selected = NULL,
                                  choiceValues = FGs[23:25],
                                  choiceNames = cost_TL$lab[23:25]),
                                
                                actionLink("selectall_Coastal_Pel",
                                           "Select/Deselect All"),
                                checkboxGroupInput(
                                  inputId = 'Coastal_Pel',
                                  inline = T,
                                  label = 'Coastal Pelagic Fishes',
                                  selected = NULL,
                                  choiceValues = FGs[c(1,21,26:28)],
                                  choiceNames = cost_TL$lab[c(1,21,26:28)]),
                                
                                actionLink("selectall_MarMam_Turtles",
                                           "Select/Deselect All"),
                                checkboxGroupInput(
                                  inputId = 'MarMam_Turtles',
                                  inline = T,
                                  label = 'Marine Mammals and Turtles',
                                  selected = NULL,
                                  choiceValues = FGs[2:6],
                                  choiceNames = cost_TL$lab[2:6]),
                                
                                actionLink("selectall_Micronekton",
                                           "Select/Deselect All"),
                                checkboxGroupInput(
                                  inputId = 'Micronekton',
                                  inline = T,
                                  label = 'Micronekton',
                                  selected = NULL,
                                  choiceValues =FGs[c(22,50)],
                                  choiceNames = cost_TL$lab[c(22,50)]),
                                
                                actionLink("selectall_Macrobenthos",
                                           "Select/Deselect All"),
                                checkboxGroupInput(
                                  inputId = 'Macrobenthos',
                                  inline = T,
                                  label = 'Macrobenthos',
                                  selected = NULL,
                                  choiceValues = FGs[29:33],
                                  choiceNames = cost_TL$lab[29:33]),
                                
                                actionLink("selectall_Other_Benthos",
                                           "Select/Deselect All"),
                                checkboxGroupInput(
                                  inputId = 'Other_Benthos',
                                  inline = T,
                                  label = 'Other_Benthos',
                                  selected = NULL,
                                  choiceValues = FGs[34:38],
                                  choiceNames = cost_TL$lab[34:38]),
                                
                                actionLink("selectall_StructBen",
                                           "Select/Deselect All"),
                                checkboxGroupInput(
                                  inputId = 'StructBen',
                                  inline = T,
                                  label = 'Structural Benthic Species',
                                  selected = NULL,
                                  choiceValues = FGs[39:44],
                                  choiceNames = cost_TL$lab[39:44]),
                                
                                actionLink("selectall_P",
                                           "Select/Deselect All"),
                                checkboxGroupInput(
                                  inputId = 'P',
                                  inline = T,
                                  label = 'Primary Producers',
                                  selected = NULL,
                                  choiceValues = FGs[45:49],
                                  choiceNames = cost_TL$lab[45:49]),
                                
                                actionLink("selectall_Z",
                                           "Select/Deselect All"),
                                checkboxGroupInput(
                                  inputId = 'Z',
                                  inline = T,
                                  label = 'Zooplankton',
                                  selected = NULL,
                                  choiceValues = FGs[51:54],
                                  choiceNames = cost_TL$lab[51:54]),
                                
                                actionLink("selectall_DET",
                                           "Select/Deselect All"),
                                checkboxGroupInput(
                                  inputId = 'DET',
                                  inline = T,
                                  label = 'Detritus',
                                  selected = NULL,
                                  choiceValues = FGs[55],
                                  choiceNames = cost_TL$lab[55])
                              ),
                              
                              # Show a plot of the generated distribution
                              mainPanel(
                                plotOutput("PlotBiomass")
                              )
                            )
                          )
                          
                 )
)

# Server logic
server <- function(input, output, session) {
  
  observe({
    for(i in 1:nrow(Groups) ){
      temp_name = paste0('selectall_', Groups$valname[i])
      
      if(input[[temp_name]] == 0) return(NULL) 
      else if (input[[temp_name]]%%2 == 0)
      {
        updateCheckboxGroupInput(session,
                                 inputId = Groups$valname[i],
                                 inline = T,
                                 label = Groups$label[i],
                                 selected = NULL,
                                 choiceValues = cost_TL$FG[cost_TL$inputID%in%Groups$valname[i]],
                                 choiceNames = cost_TL$lab[cost_TL$inputID%in%Groups$valname[i]])
      }
      else
      {
        updateCheckboxGroupInput(session,
                                 inputId = Groups$valname[i],
                                 inline = T,
                                 label = Groups$label[i],
                                 selected = cost_TL$FG[cost_TL$inputID %in% Groups$valname[i]],
                                 choiceValues = cost_TL$FG[cost_TL$inputID %in% Groups$valname[i]],
                                 choiceNames = cost_TL$lab[cost_TL$inputID %in% Groups$valname[i]])
      }
    }
  })
  
  output$PlotBiomass <- renderPlot({
    
    plot_these = c(input$RFish, input$Meso_Subphotic_Fish, input$BFish,
                   input$Coastal_Pel, input$MarMam_Turtles, input$Micronekton,
                   input$Macrobenthos, input$Other_Benthos, input$StructBen,
                   input$P, input$Z, input$DET)
    
    if(is.null(plot_these)){
      plot(x = seq(2000,2040, length = nTime), 
           y = seq(2000,2040, length = nTime),
           type = 'n', xlab="Time", ylab="Relative Biomass", las = 1,
           ylim = c(0,2), xlim = c(2000,2045) )
    }
    
    if(!is.null(plot_these)){
      df = bio_ar[input$scen,,plot_these]
      
      plot(x = seq(2000,2040, length = nTime), 
           y = seq(2000,2040, length = nTime),
           type = 'n', xlab="Time", ylab="Relative Biomass", las = 1,
           ylim = c(0,max(df)), xlim = c(2000,2045) )
      polygon(x = c(2019,2019,2040,2040), y = c(0,max(df),max(df),0), 
              col = 'lightgray', border = F)
      matlines(seq(2000,2040, length = nTime), df, lwd = 2, lty = 1)
      legend('topright', legend = plot_these, col = palette(), lwd = 2, lty = 1)
    }
    
  })
}

# Complete app with UI and server components
shinyApp(ui, server)
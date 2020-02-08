# # User Interface
# tabPanel("Indicators by Objective",
#          fluidPage(
#            sidebarLayout(
#              
#              #Sidebar with a slider input
#              sidebarPanel(
#                selectInput(
#                  inputId = 'scen2',
#                  label = 'Effort Scenario',
#                  choices =  list('BRFA Scenario 1' = 'BRFA1',
#                                  'BRFA Scenario 2' = 'BRFA2',
#                                  'Constant Effort' = 'ConstantEffort', 
#                                  'Line Only' = 'LineOnly', 
#                                  'No Spearfishing' = 'No_Spear',
#                                  'No Herbivore Fishing' = 'NoHerb', 
#                                  'No Net Fishing' = 'NoNet'),
#                  selected = 'ConstantEffort"'),
#                
#                actionLink("selectall_ind",
#                           "Select/Deselect All"),
#                checkboxGroupInput(
#                  inputId = 'indicator',
#                  inline = T,
#                  label = 'Indicator:',
#                  choiceValues = c('Rec', 'Rev', 'TL', 'Div',
#                                   'Herb_Bio', 'Pisc_Bio',
#                                   'Tar_Bio', 'RFish_Bio',
#                                   'Turtle_Bio', 'Coral_Bio',
#                                   'Dive'),
#                  choiceNames = c('Non-Commercial Catch',
#                                  'Revenue',
#                                  'Catch Trophic Level (TL)',
#                                  'Ecosystem Diversity',
#                                  'Herbivore Biomass',
#                                  'Piscivore Biomass',
#                                  'Targeted Species Biomass',
#                                  'Reef Fish Biomass',
#                                  'Turtle Biomass',
#                                  'Coral Biomass',
#                                  'Dive Enjoyment?'),
#                  selected = c('Rec', 'Rev', 'TL', 'Div',
#                               'Herb_Bio', 'Pisc_Bio',
#                               'Tar_Bio', 'RFish_Bio',
#                               'Turtle_Bio', 'Coral_Bio',
#                               'Dive'))
#              ),
#              
#              #Show a plot of the generated distribution
#              mainPanel(
#                plotOutput("distPlot2")
#              )
#            )
#          )
# )
# 
# #Server
# observe({
#   if(input$selectall_ind == 0) return(NULL) 
#   else if (input$selectall_ind%%2 == 0)
#   {
#     updateCheckboxGroupInput(session,
#                              inputId = 'indicator',
#                              inline = T,
#                              label = 'Indicator:',
#                              selected = NULL,
#                              choiceValues = c('Rec', 'Rev', 'TL', 'Div',
#                                               'Herb_Bio', 'Pisc_Bio',
#                                               'Tar_Bio', 'RFish_Bio',
#                                               'Turtle_Bio', 'Coral_Bio',
#                                               'Dive'),
#                              choiceNames = c('Non-Commercial Catch',
#                                              'Revenue',
#                                              'Catch Trophic Level (TL)',
#                                              'Ecosystem Diversity',
#                                              'Herbivore Biomass',
#                                              'Piscivore Biomass',
#                                              'Targeted Species Biomass',
#                                              'Reef Fish Biomass',
#                                              'Turtle Biomass',
#                                              'Coral Biomass',
#                                              'Dive Enjoyment?'))
#   }
#   else
#   {
#     updateCheckboxGroupInput(session,
#                              inputId = 'indicator',
#                              inline = T,
#                              label = 'Indicator:',
#                              choiceValues = c('Rec', 'Rev', 'TL', 'Div',
#                                               'Herb_Bio', 'Pisc_Bio',
#                                               'Tar_Bio', 'RFish_Bio',
#                                               'Turtle_Bio', 'Coral_Bio',
#                                               'Dive'),
#                              selected = c('Rec', 'Rev', 'TL', 'Div',
#                                           'Herb_Bio', 'Pisc_Bio',
#                                           'Tar_Bio', 'RFish_Bio',
#                                           'Turtle_Bio', 'Coral_Bio',
#                                           'Dive'),
#                              choiceNames = c('Non-Commercial Catch',
#                                              'Revenue',
#                                              'Catch Trophic Level (TL)',
#                                              'Ecosystem Diversity',
#                                              'Herbivore Biomass',
#                                              'Piscivore Biomass',
#                                              'Targeted Species Biomass',
#                                              'Reef Fish Biomass',
#                                              'Turtle Biomass',
#                                              'Coral Biomass',
#                                              'Dive Enjoyment?'))
#   }
#   
#   
# })
# 
# output$distPlot2 <- renderPlot({
#   par(mar = c(5,5,1,1))
#   obj = input$scen2
#   ind = input$indicator
#   
#   if(is.null(ind)){
#     barplot(rep(0, 11), ylim = c(-100, 100), las = 1)
#   }
#   else{
#     labels = c(Rec = 'Non-Com.\nCatch',
#                Rev = 'Rev.',
#                TL = 'Catch\nTL',
#                Div = 'Eco.\nDiv.',
#                Herb_Bio = 'Herb.\nBio.',
#                Pisc_Bio = 'Pisc.\nBio.',
#                Tar_Bio = 'Targeted\nBio.',
#                RFish_Bio = 'R. Fish\nBio',
#                Turtle_Bio = 'Turtle\nBio',
#                Coral_Bio = 'Coral\nBio',
#                Dive = 'Dive')[ind]
#     
#     barplot(rep(0, 11), ylim = c(-100,105), las = 1)
#     more_than_100 = which(ind_ar[obj, ind]*100 > 100)
#     
#     to_plot = ind_ar[obj, ind]*100
#     to_plot = ifelse(to_plot > 100, 100, to_plot)
#     
#     barplot(to_plot, add = T, axes = F, names.arg = labels,
#             col = ifelse(ind_ar[obj, ind]> 0, 'blue', 'red'))
#     legend('bottomleft', '* >100% change', bty = 'n', cex = 1.5)
#     
#     if(length(more_than_100)>0){
#       barplot_temp = barplot(to_plot, add = T, axes = F, names.arg = labels,
#                              col = ifelse(ind_ar[obj, ind]> 0, 'blue', 'red'))
#       temp = rep(NA, length(ind))
#       temp[more_than_100] = 90
#       points(x = barplot_temp, y = temp, pch = 8, cex = 2, font = 2)
#     }
#     
#     abline(h = 0)
#   }
#   
#   box()
#   mtext(side = 1, 'Indicator', line = 3, cex = 1.25)
#   mtext(side = 2, 'Percent Change', line = 3, cex = 1.25)
# })

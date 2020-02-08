###################################
## Utility Function, Dive Survey
## Zack Oyafuso
###################################

###################################
## Import Libraries
###################################
library(readxl)

###########################################
## Import Data, without comments for now
###########################################
setwd('C:/Users/Zack Oyafuso/Google Drive/EwE/HI_EwE/')

df_dive = as.data.frame(
  readxl::read_xlsx(path = 'Hawaii-Survey-Result-6-17-2019-Filtered-NO.xlsx',
                    range = 'A1:DO810', na = 'NA'))

df_dive = df_dive[,c("HealthyCoral", "IncHealthyCoral", "DecHealthyCoral", 
                     "AbundanceFish", "IncNumFish", "DecNumFish", 
                     "VarietyFish", "IncVarietyFish", "DecVarietyFish", 
                     "SeaTurtles","IncTurtleSightings", "DecTurtleSightings")]

names(df_dive)[c(4, 10)] = c('NumFish', 'TurtleSightings')

df_dive = na.omit(df_dive)

tot_util = function(  HealthyCoral = c("Inc","Dec", "N")[3],
                      NumFish = c("Inc","Dec", "N")[3],
                      VarietyFish =  c("Inc","Dec", "N")[3],
                      TurtleSightings = c("Inc","Dec", "N")[3],
                      temp_df = df_dive){
  
  #Baseline Utility
  baseline_utility = as.numeric(
    rowSums(temp_df[,c("HealthyCoral", 'NumFish', 
                       'TurtleSightings', 'VarietyFish')])
  )
  
  new_utility = 0
  
  for(temp in c("HealthyCoral", 'NumFish', 'TurtleSightings', 'VarietyFish')){
    temp_utility = temp_df[,temp]
    
    temp_mult=1
    
    if(get(temp) == 'Inc'){
      temp_mult = ifelse(temp_df[,paste0("Inc",temp)] == 1, 0.5,
                         ifelse(temp_df[,paste0("Inc",temp)] == 2, 1,
                                ifelse(temp_df[,paste0("Inc",temp)] == 3, 2, 1)))
    }
    
    if(get(temp) == 'Dec'){
      temp_mult = ifelse(temp_df[,paste0("Dec", temp)] == 1, 0.5,
                         ifelse(temp_df[,paste0("Dec", temp)] == 2, 1,
                                ifelse(temp_df[,paste0("Dec", temp)] == 3, 2, 1)))
    }
    
    new_utility = new_utility + temp_utility*temp_mult
  }
  
  return(list(new_utility = new_utility, baseline_utility = baseline_utility))
}

save.image('dive_fn.RData')

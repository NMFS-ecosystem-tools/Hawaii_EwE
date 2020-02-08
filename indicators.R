##################################################
## General Functions to Create Indicators for: 
## 1) Non-commercial catch
## 2) Revenue
## 3) Dive Enjoyment
## 4) Biomass of Targeted Species
## 5) Herbivore Biomass
## 6) Sharks and other Apex Predator Diets
## 7) Weighted Trophic Level
## 
## Zack Oyafuso 
## created 27 November 2019
##################################################
library(readxl)

setwd('/Users/zackoyafuso/Documents/GitHub/Hawaii_EwE')

##################################################
## Settings
##################################################

## Recreational Catch Composition
load('data/Rec_Component/rec_comp.RData')

## Price, Trophic Level and Rec Composition of each FG
cost_TL = read.csv('data/TL_cost.csv', stringsAsFactors = F)
cost_TL$cost = cost_TL$cost
cost_TL$rec_comp = 0
cost_TL$rec_comp[match(names(rec_comp), cost_TL$FG )] = rec_comp
cost_TL$lab = paste0(cost_TL$FG, ': ', cost_TL$nicename)
cost_TL = rbind( c('groupname' = 'StingRay', nicename = 'Sting rays', FG = 'IVR',
                   cost = 0, TL = NA, rec_comp = 0, lab = 'IVR: Sting rays'),
                 cost_TL )
cost_TL$cost = as.numeric(cost_TL$cost)
cost_TL$TL = as.numeric(cost_TL$TL)
cost_TL$rec_comp = as.numeric(cost_TL$rec_comp)

n_FG = nrow(cost_TL)
nTime = 493
scenarios = c('BRFA1', 'BRFA2', 'ConstantEffort', 'LineOnly', 'NoSpear',
              'NoHerb', 'NoNet', 'NoNetwithEffortDisplacement', 
              'NoSpearwithEffortDisplacement', 'LineOnlywithEffortDisplacement')
nscen = length(scenarios)

areas = read.csv(file = paste0('data/habitat_FisheryArea_funcGrp.csv'), 
                 stringsAsFactors = F)[,1:4]

##################################################
## Result Objects
##################################################
bio_ar = array(0,
               dim = c(nscen, nTime, n_FG),
               dimnames = list(
                 scenarios, 
                 NULL, cost_TL$FG
               ))

ind_ar = array(NA, dim = c(nscen, 14),
               dimnames = list(
                 scenarios,
                 c('Rec','Rev','TL','Div','Herb_Bio','Pisc_Bio','Tar_Bio',
                   'BF_Tar_Bio', 'Seal_Dol_Bio',
                   'RFish_Bio','Turtle_Bio','Coral_Bio','CV_catch','Dive')
               ))

##################################################
#1) Biomass Across Functional Groups
#2) Herbivore Biomass
##################################################

for(iobj in scenarios){
  
  filename = paste0('data/', iobj, '_Relative biomass.csv')
  
  if(file.exists(filename)) {
    temp_df = read.csv(filename,skip = 1,header = T)
    temp_df = temp_df[1:nTime,grepl(x = colnames(temp_df), pattern = '^X')]
    colnames(temp_df) = cost_TL$FG
    
    bio_ar[iobj, , ] = as.matrix(temp_df)
  }
}

##################################################
## 2) Recreational Catch
##################################################

for(iobj in scenarios){
  
  #Import FG results
  ewe_res = as.data.frame(read_excel('data/Ecosim_Results_2020.xlsx', 
                                     sheet = 'Ecosim_FG'))
  
  #Extract catch density data (mt/km^2)
  tot_catch = subset(ewe_res, Scenario == iobj,
                     select = c(Group_name, Catch_start, Catch_end))
  tot_catch =  tot_catch[tot_catch$Group_name != 'Total',] #Correct Typo
  
  #Calculate absolute catch in lbs
  total_area = 8905 #km^2
  tot_catch = sweep(x = tot_catch[,-1], MARGIN = 1, 
                    STATS = total_area*areas$Fishery_Area*2204.62, FUN = '*')
  
  ## Extract biomass density data (mt/km^2)
  biomass = subset(ewe_res, Scenario == iobj,
                   select = c(Group_name, Biomass_start, Biomass_end))
  biomass =  biomass[biomass$Group_name != 'Total',] #Correct Typo
  
  #Calculate absolute biomass in lbs
  biomass = cbind(Group_name = biomass$Group_name,
                  sweep(x = biomass[,-1], MARGIN = 1, FUN = '*', 
                        STATS = total_area*areas$habitatArea*2204.62))
  
  #############################
  ## Herbivore Biomass
  #############################
  herb = c('R_Browser', 'R_Grazer', 'Parrotfish')
  Herb_Bio_change = colSums(biomass[biomass$Group_name %in% herb,-1])
  Herb_Bio_change = diff(Herb_Bio_change)/ Herb_Bio_change[1]
  
  #############################
  ## Apex Predator Biomass
  #############################
  pisc = c('MonkSeals', 'SpinnerD', 'BottlenoseD', 
           'R_BenthPisc', 'D_BenthPisc',
           'RovPisc', 'Sharks')
  Pisc_Bio_change = colSums(biomass[biomass$Group_name %in% pisc,-1])
  Pisc_Bio_change = diff(Pisc_Bio_change)/ Pisc_Bio_change[1]
  
  #############################
  ## Targeted Fisheries Biomass
  #############################
  tar = cost_TL$groupname[cost_TL$cost > 3]
  BF_tar = c('BFW_group', 'BFB_group', 'Uku')
  
  BF_Bio_change = colSums(biomass[biomass$Group_name %in% BF_tar,-1])
  BF_Bio_change = diff(BF_Bio_change)/BF_Bio_change[1]
  
  tar_Bio_change = colSums(biomass[biomass$Group_name %in% tar,-1])
  tar_Bio_change = diff(tar_Bio_change)/tar_Bio_change[1]
  
  temp = biomass[biomass$Group_name %in% tar,]
  temp[,-1] = round(temp[,-1]/1e6, 2)
  cbind(temp, 
        change = apply(temp[,-1], MARGIN = 1, diff ),
        rel_change = round(100*apply(temp[,-1], 
                                     MARGIN = 1, 
                                     FUN = function(x) (x[2]-x[1])/x[1]),
                           digits = 1)
        )
  
  #############################
  ## Reef Fish Biomass
  #############################
  RFish = biomass$Group_name[grepl(x = biomass$Group_name, pattern = '^R_')]
  RFish_Bio_change = colSums(biomass[biomass$Group_name %in% RFish,-1])
  RFish_Bio_change = diff(RFish_Bio_change)/ RFish_Bio_change[1]
  
  #############################
  ## Coral Biomass
  #############################
  Coral = c('Porites_mass', 'Porites_branch', 'Montipora', 'Pocillopora',
            'Leptoseris', 'CCA')
  Coral_Bio_change = colSums(biomass[biomass$Group_name %in% Coral,-1])
  Coral_Bio_change = diff(Coral_Bio_change)/Coral_Bio_change[1]
  
  #############################
  ## Monk Seal and Dolphin Biomass
  ##############################
  Seal_Dol = c('MonkSeals', 'SpinnerD', 'BottlenoseD')
  Seal_Dol_Bio_change = colSums(biomass[biomass$Group_name %in% Seal_Dol,-1])
  Seal_Dol_Bio_change = diff(Seal_Dol_Bio_change)/Seal_Dol_Bio_change[1]
  
  #############################
  ## Turtle Biomass
  #############################
  Turtle = c('GreenTurtles', 'HawksbillT')
  Turtle_Bio_change = colSums(biomass[biomass$Group_name %in% Turtle,-1])
  Turtle_Bio_change = (Turtle_Bio_change[2]-Turtle_Bio_change[1])/ Turtle_Bio_change[1]
  
  group_biomass = as.matrix(c(Herb_Bio_change, Pisc_Bio_change, 
                              tar_Bio_change, RFish_Bio_change, 
                              Coral_Bio_change, Turtle_Bio_change))
  rownames(group_biomass) = NULL
  
  ind_ar[iobj, c('Herb_Bio', 'Pisc_Bio', 'Tar_Bio', 'BF_Tar_Bio',
                 'RFish_Bio', 'Coral_Bio', 'Turtle_Bio', 'Seal_Dol_Bio')] = 
    as.matrix(c(Herb_Bio_change, Pisc_Bio_change, tar_Bio_change, BF_Bio_change,
                RFish_Bio_change, Coral_Bio_change, Turtle_Bio_change,
                Seal_Dol_Bio_change))
  
  
  #############################
  ## Recreational Catch 
  #############################
  rec_catch = sweep(x = tot_catch, STATS = cost_TL$rec_comp, 
                    MARGIN = 1, FUN = '*')
  
  tot_rec_catch = colSums(rec_catch, na.rm = T)
  rec_catch_change = (tot_rec_catch[2]-tot_rec_catch[1])/ tot_rec_catch[1]
  names(rec_catch_change) = NULL
  
  ind_ar[iobj, 'Rec'] = rec_catch_change
  
  #############################
  ## Revenue
  #############################
  com_catch = tot_catch - rec_catch
  rev = sweep(x = com_catch, STATS = cost_TL$cost, MARGIN = 1, FUN = '*')
  
  tot_rev = colSums(rev, na.rm = T)
  rev_change =  (tot_rev[2]-tot_rev[1])/ tot_rev[1]
  names(rev_change) = NULL
  ind_ar[iobj,  'Rev'] = rev_change
  
  #############################
  ## Trophic Level of Catch, 
  ## Diversity, Variability of Catch
  #############################
  ewe_res = as.data.frame(read_excel('data/Ecosim_Results_2020.xlsx', 
                                     sheet = 'Ecosim_Indices'))
  ewe_res = ewe_res[,-2]
  
  TL = subset(ewe_res, Scenario == iobj, 
              select = c("TrophicLevelCatch", "KemptonsQ",
                         "TotalCatch"))
  
  TL$year = rep(1:41, each = 12)
  TL_year = aggregate(cbind(TrophicLevelCatch, KemptonsQ) ~ year, 
                      data = TL, FUN = mean)
  
  TL_year$TotalCatch = aggregate(TotalCatch ~ year, 
                                 data = TL, FUN = sum)$TotalCatch
  
  TL_change = (TL_year[41, c('TrophicLevelCatch', 'KemptonsQ')] -  TL_year[20, c('TrophicLevelCatch', 'KemptonsQ')])/ TL_year[1, c('TrophicLevelCatch', 'KemptonsQ')]
  
  yearly_CV = 100*sd(TL_year$TotalCatch[20:41])/mean(TL_year$TotalCatch[20:41])
  
  TL_change = cbind(TL_change, yearly_CV)
  
  names(TL_change) = NULL; rownames(TL_change) = NULL
  ind_ar[iobj, c("TL", 'Div', 'CV_catch')] = as.matrix(TL_change)
  
}

##################################################
## Dive Experience
##################################################
df_dive = as.data.frame(
  readxl::read_xlsx(path = paste0('data/Hawaii-Survey-Result-6-17-',
                                  '2019-Filtered-NO.xlsx'),
                    range = 'A1:DO810', na = 'NA'))

df_dive = df_dive[,c("HealthyCoral", "IncHealthyCoral", "DecHealthyCoral", 
                     "AbundanceFish", "IncNumFish", "DecNumFish", 
                     "VarietyFish", "IncVarietyFish", "DecVarietyFish", 
                     "SeaTurtles","IncTurtleSightings", "DecTurtleSightings")]

names(df_dive)[c(4, 10)] = c('NumFish', 'TurtleSightings')

df_dive = na.omit(df_dive)

#Baseline Utility
baseline_utility = as.numeric(
  rowSums(df_dive[,c("HealthyCoral", 'NumFish', 
                     'TurtleSightings', 'VarietyFish')])
)

dive_aspects = ind_ar[,c('RFish_Bio', 'Coral_Bio', 'Turtle_Bio', 'Div')]
dive_aspects = ifelse(dive_aspects > 0, 'Inc', 'Dec')
dimnames(dive_aspects)[[2]] = c('NumFish', "HealthyCoral", 
                                'TurtleSightings', 'VarietyFish')

for(iobj in scenarios){
  new_utility = 0
  for(temp in c("HealthyCoral", 'NumFish', 'TurtleSightings', 'VarietyFish')){
    temp_utility = df_dive[,temp]
    
    temp_mult=1
    
    temp_change = dive_aspects[iobj, temp]
    
    
    if(temp_change == 'Inc'){
      temp_mult = ifelse(df_dive[,paste0("Inc",temp)] == 1, 0.5,
                         ifelse(df_dive[,paste0("Inc",temp)] == 2, 1,
                                ifelse(df_dive[,paste0("Inc",temp)] == 3, 2, 1)))
    }
    
    if(temp_change == 'Dec'){
      temp_mult = ifelse(df_dive[,paste0("Dec", temp)] == 1, 0.5,
                         ifelse(df_dive[,paste0("Dec", temp)] == 2, 1,
                                ifelse(df_dive[,paste0("Dec", temp)] == 3, 2, 1)))
    }
    new_utility = new_utility + temp_utility*temp_mult
  }
  
  change_utility = ( median(new_utility)-median(baseline_utility) ) / median(baseline_utility)
  ind_ar[iobj, 'Dive'] = change_utility
}

##################################################
## Save Results
##################################################
save(list = c('bio_ar', 'cost_TL', 'ind_ar', 'n_FG', 
              'nTime', 'scenarios', 'areas'), 
     file = 'results.RData')

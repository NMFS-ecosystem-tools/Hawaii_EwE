##################################
## Load Commercial and Recreational Datasets
## No gears
## Created 8/5/2019
##################################
rm(list = ls())
#setwd('C:/Users/Zack Oyafuso/Google Drive/Atlantis/')
source('ZSO_master_scripts/settings.R')

#################################
## Totals with gears
#################################
load('ZSO_master_scripts/com/com_ar.RData')
load('ZSO_master_scripts/rec/rec_ar.RData')

#################################
## Add extra gear type to the recreational dataset
#################################
temp = array(data = 0, dim = dim(com_ar), dimnames = dimnames(com_ar))
temp[,,,-4] = rec_ar
rec_ar = temp
rm(temp)

##################################
## Bottomfish catch is separated from the reef catches by gear
## set BF values for the gear-specific arrays to zero
##################################

##################################
## Remove bf from the commercial reef fish catch arrays
##################################
com_ar[BF_fgs,,, ] = 0

##################################
## Mesophotic, Subphotic, and Invertebrate Recreational catch
## is equal to commercial catch
##################################
rec_ar[c(invert_fgs, meso_fgs, sub_fgs),,,] = 
  com_ar[c(invert_fgs, meso_fgs, sub_fgs),,,]

rec_ar = apply(rec_ar, MARGIN = c(1,2), sum)
com_ar = apply(com_ar, MARGIN = c(1,2), sum)

percent_rec = rec_ar / (rec_ar + com_ar) 
percent_rec[is.nan(percent_rec)] = 0

percent_rec[c('BFB', 'BFW'),] = 0.5

##############################################
## Calculate Mean Proportion of Recreational Catch
## Subset only 2004-2015 data (only years where
## both rec and com data originally exists)
###############################################
rec_comp = rowMeans(percent_rec[,paste(2004:2015)])

save('rec_comp', file = "ZSO_master_scripts/indicators/rec_comp.RData")

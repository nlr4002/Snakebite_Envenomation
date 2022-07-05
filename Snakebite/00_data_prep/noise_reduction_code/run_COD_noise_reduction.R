## Run noise reduction now

# Date: 07/18/2017
# Purpose currently implemented for VR noise reduction
# Soundtrack: "Introitus: Requiem" Mozart
# Mantra: "As long as I have a want, I have a reason for living. Satisfaction is death." - George Bernard Shaw

library(readr)
library(stats)
library(MASS)
library(data.table)
library(parallel)
library(stringr)
library(magrittr)
library(lme4)
library(ggplot2)
library(merTools)

source('[DIRECTORY]/get_age_metadata.R')
source('[DIRECTORY]/get_location_metadata.R')

library(mortcore, lib = "[DIRECTORY]")

model_group_files <- Sys.glob('[DIRECTORY]*.csv')
va_files <- Sys.glob('[DIRECTORY]/VA*.csv')
for(f in model_group_files){
  print(f)
  model_group <- str_split(f, '/')[[1]][7]
  model_group <- str_split(model_group, '[.]')[[1]][1]
  
  qsub(jobname = paste0('NR_', model_group),
       code = '[DIRECTORY]/noise_reduction_worker.R',
       pass = list(f),
       cores = 2,
       mem = 10,
       wallclock = "01:00:00",
       submit = T,
       log = T,
       archive_node = T,
       queue = "long",
       proj = "proj_injuries",
       shell = paste0('[DIRECTORY]/r_shell2.sh'))
  
}


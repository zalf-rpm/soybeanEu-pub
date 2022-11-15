
### Set directory to current one
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# read libraries ----------------------------------------------------------

source("Rbuildapsim.R")
library("XML")

library(tidyverse)

# read data ---------------------------------------------------------------



mastertabelle=readRDS(file = "sim2.rds")
mastertabelle <- subset(mastertabelle, id > 180000 & id < 200001 )



# create sims -------------------------------------------------------------



for (template in unique(mastertabelle$template)){ #iterate through templates
  
  tabelle<-mastertabelle
  
  #generate arguments
  soilnames<-tabelle$soil
  metfiles<-tabelle$metfiles
  outfilenames<-tabelle$OUT
  
  set_nodes<-lapply( 1:nrow(tabelle), function(i) {
    set_node=c("start_date"=tabelle$start_date[i],
               "end_date"=tabelle$end_date[i],
               "cultivar_soybean"=tabelle$cultivar_soy[i],
               "fert_criteria"=tabelle$fert_criteria[i])
  })
  
  #generate individual insertions
  insertions<-lapply( 1:nrow(tabelle), function(i) {
    insertion=c("Soil"=file.path("Sample_file","sample.xml"),
                "Soil"=file.path("Sample_file","initialwater.xml"),
                "//folder[@name='Manager folder']"=file.path("Commander",tabelle$Commander[i]))
  })
  
  lookup<-data.frame(soil=soilnames,
                     met=metfiles,
                     outfilename=outfilenames)
  
  buildapsim(template="fros_Balkan_ms.apsim",
             soilfiles=c("soils/soil_grid.soils"),
             metfiles=metfiles,
             insertions=insertions,
             set_nodes=set_nodes,
             target_dir="apsimfiles_balkan_ms/",
             lookup = lookup,
             overwrite = T,
             metpath="../met",
             metincludepath="/met",
             pathsep="/")
  
  buildapsim(template="fros_Balkan_sm.apsim",
             soilfiles=c("soils/soil_grid.soils"),
             metfiles=metfiles,
             insertions=insertions,
             set_nodes=set_nodes,
             target_dir="apsimfiles_balkan_sm/",
             lookup = lookup,
             overwrite = T,
             metpath="../met",
             metincludepath="/met",
             pathsep="/")
  
  buildapsim(template="fros_Augusta_ms.apsim",
             soilfiles=c("soils/soil_grid.soils"),
             metfiles=metfiles,
             insertions=insertions,
             set_nodes=set_nodes,
             target_dir="apsimfiles_augusta_ms/",
             lookup = lookup,
             overwrite = T,
             metpath="../met",
             metincludepath="/met",
             pathsep="/")
  
  buildapsim(template="fros_Augusta_sm.apsim",
             soilfiles=c("soils/soil_grid.soils"),
             metfiles=metfiles,
             insertions=insertions,
             set_nodes=set_nodes,
             target_dir="apsimfiles_augusta_sm/",
             lookup = lookup,
             overwrite = T,
             metpath="../met",
             metincludepath="/met",
             pathsep="/")
  
  buildapsim(template="fros_Ecudor_ms.apsim",
             soilfiles=c("soils/soil_grid.soils"),
             metfiles=metfiles,
             insertions=insertions,
             set_nodes=set_nodes,
             target_dir="apsimfiles_ecudor_ms/",
             lookup = lookup,
             overwrite = T,
             metpath="../met",
             metincludepath="/met",
             pathsep="/")
  
  buildapsim(template="fros_Ecudor_sm.apsim",
             soilfiles=c("soils/soil_grid.soils"),
             metfiles=metfiles,
             insertions=insertions,
             set_nodes=set_nodes,
             target_dir="apsimfiles_ecudor_sm/",
             lookup = lookup,
             overwrite = T,
             metpath="../met",
             metincludepath="/met",
             pathsep="/")
  
  buildapsim(template="fros_Galina_ms.apsim",
             soilfiles=c("soils/soil_grid.soils"),
             metfiles=metfiles,
             insertions=insertions,
             set_nodes=set_nodes,
             target_dir="apsimfiles_galina_ms/",
             lookup = lookup,
             overwrite = T,
             metpath="../met",
             metincludepath="/met",
             pathsep="/")
  
  buildapsim(template="fros_Galina_sm.apsim",
             soilfiles=c("soils/soil_grid.soils"),
             metfiles=metfiles,
             insertions=insertions,
             set_nodes=set_nodes,
             target_dir="apsimfiles_galina_sm/",
             lookup = lookup,
             overwrite = T,
             metpath="../met",
             metincludepath="/met",
             pathsep="/")
  
  buildapsim(template="fros_Merkur_ms.apsim",
             soilfiles=c("soils/soil_grid.soils"),
             metfiles=metfiles,
             insertions=insertions,
             set_nodes=set_nodes,
             target_dir="apsimfiles_merkur_ms/",
             lookup = lookup,
             overwrite = T,
             metpath="../met",
             metincludepath="/met",
             pathsep="/")
  
  buildapsim(template="fros_Merkur_sm.apsim",
             soilfiles=c("soils/soil_grid.soils"),
             metfiles=metfiles,
             insertions=insertions,
             set_nodes=set_nodes,
             target_dir="apsimfiles_merkur_sm/",
             lookup = lookup,
             overwrite = T,
             metpath="../met",
             metincludepath="/met",
             pathsep="/")
  
  buildapsim(template="fros_Sultana_ms.apsim",
             soilfiles=c("soils/soil_grid.soils"),
             metfiles=metfiles,
             insertions=insertions,
             set_nodes=set_nodes,
             target_dir="apsimfiles_sultana_ms/",
             lookup = lookup,
             overwrite = T,
             metpath="../met",
             metincludepath="/met",
             pathsep="/")
  
  buildapsim(template="fros_Sultana_sm.apsim",
             soilfiles=c("soils/soil_grid.soils"),
             metfiles=metfiles,
             insertions=insertions,
             set_nodes=set_nodes,
             target_dir="apsimfiles_sultana_sm/",
             lookup = lookup,
             overwrite = T,
             metpath="../met",
             metincludepath="/met",
             pathsep="/")
}




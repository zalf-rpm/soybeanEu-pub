# SCRIPT:  scriptRotaClusterN.R
#
# AUTHOR: Helene Raynal - INRAE - Helene.Raynal@inrae.fr
# DATE: Octobre 2020
# LICENSE:  GPL V3 (https://www.gnu.org/licenses/gpl-3.0.html)

# =====================================================================================
# #DESCRIPTION:
# script used to prepare and launch  simulations of rotation soybean / maize at European scale, following the protocol defined
# for  the project Innisoy
# To run the script, you need to install STICS V9 (https://www6.paca.inrae.fr/stics_eng/Download) and STICSOnR (https://github.com/SticsRPacks/SticsOnR)
# Inputs:
# ---------------------
#  - stu_eu_layer_ref.csv: contains all the simulations to run with the followin variables
#  soil_ref,CLocation,latitude,depth,OC_topsoil,OC_subsoil,BD_topsoil,BD_subsoil,Sand_topsoil,Clay_topsoil,Silt_topsoil,Sand_subsoil,Clay_subsoil,Silt_subsoil
#  - climate data: one file by year and by climate scenario, in the format expected by STICS 
#  - sowing dates files: one file by climate scenario and by set soil point (20000 points). Be careful on the name of the file: 
#      "sowingDatesXXX.csv" where XXX corresponds to the name of the directory of climate files 
#        (for a specific climatic scenario example c("00","GFDL", "GISS", "Had","MIR","MPI" ))
#   - CO2 value in ppm. Be careful this value is hardcoded CO2 <- 360
#  - archive of USM: required to run STICS
#
# Outputs:
# ---------------------
#    File of aggregated results (one file by soil ref point): 
#      one row = one result concerning: one climate scenario, one treatment (rainfed / unlimited irrigation), one year, one crop  
#==================================================================================================================

#ALL the native files are on the INRAE repository: https://forgemia.inra.fr/helene.raynal/legumegap, and available on demand. Contact Helene.Raynal@inrae.fr
rm(list = ls())


# Get the number of cores and working directory
#args = commandArgs(trailingOnly = TRUE)
#if (length(args > 0)) {
#  numcores = as.integer(args[1])
#  print(paste("Nb cores=", numcores))
#  setwd(as.character(args[2]))
#  print(paste("Working directory:", args[2]))
#} else {
#  numcores = 1
#}

args = commandArgs(trailingOnly = TRUE)
if (length(args > 0)) {
  argSoilRefIndex = as.integer(args[1])
} else {
  argSoilRefIndex = 450
}
lot <- ceiling(argSoilRefIndex / 1000)



library(readxl)
library(CroptimizR)
library(SticsOnR)
library(SticsRFiles)

###############################################
# Paths cluster
###############################################
#Paths concerning STICS
workspace_path <- "/home/raynalh/scratch"
stics_path <- file.path(workspace_path, "JavaSTICS-1.41-stics-9.0/bin/stics_modulo")
#workspace_path <- getwd()
javastics_path <- file.path(workspace_path, "JavaSTICS-1.41-stics-9.0")

# Paths concerning datas
DirClimate <- ""
#DirClimate <- file.path(workspace_path,"climate/")
#DirClimate <- "../climate/0_0_output/"
#DirClimate <- "../climate/"
DirSoil <- "/home/raynalh/scratch/outputSoil/"
DirStation <- ""
DirPlant <- ""
DirTec <- ""
DirIni <- ""
DirVarMod <- file.path("/home/raynalh/scratch/VarMod")
DirOutputs <- workspace_path

# Paths personnal computer
###########################
# Paths concerning STICS
#javastics_path <-
#  "C:/Users/hraynal/EspaceTravailBadet/MODELES/Modele_STICS/JavaSTICS-1.41-stics-9.1"
#stics_path <- file.path(javastics_path, "bin", "stics_modulo")
#exe_path <-
#  "C:/Users/hraynal/EspaceTravailBadet/MODELES/Modele_STICS/JavaSTICS-1.41-stics-9.1/bin"
#workspace_path <- "D:/LegumeGap/data/"

# Paths concerning data
#DirClimate <- "../climate/macsur_v3_corr_test/00/"
#DirSoil <- "soybeanEUsoil/"
#DirStation <- ""
#DirPlant <- ""
#DirTec <- ""
#DirIni <- ""
#DirVarMod <- file.path(workspace_path, "VarMod", fsep = "")
#DirOutputs <- "Simulation/Baseline/Baresoil"





###############################################
# Read data file concerning points to simulate
###############################################
#InputFile <- "stu_eu_layer_ref.csv"

#Read Raw data on personnal computer
InputFile <-
#file.path(workspace_path, DirSoil, "stu_eu_layer_ref.csv", fsep = "")
file.path(workspace_path, "/stu_eu_layer_ref.csv", fsep = "")


dataGridFull <-
  read.csv(
    InputFile,
    header = TRUE,
    sep = ",",
    quote = "\"",
    dec = "."
  )
#head(dataGridFull)
#dataGrid <- dataGridFull[1:10,]
dataGrid <- dataGridFull
dimDataGrid <- nrow(dataGrid)

###############################################
# Function that
#  -generates STICS input files
#  -runs simulation
#  - post-processes simulation results
# The argument of the function = 1 point (one line of InputFile)
#################################################################
build30USM <- function(ind, indlot) {

  #functions used to initialize a new working directory where USMs of the specific soil point considered will be treated 
  copyInitUSM <- function(x) {
    dir.create(paste(workspace_path, x, sep = "/"))
    xx <- paste(workspace_path, x, sep = "/")
    #xxsols <- paste(xx,"/sols.xml", sep="")
    xxsols <- paste("/home/raynalh/scratch/", x, "/sols.xml", sep = "")
    file.copy(
    #from = paste(workspace_path, "InitCurrentUSMRota/sols.xml", sep = ""),
    #from = paste(workspace_path, "/outputSoil/sols",indlot,".xml", sep = ""),
      from = paste("/home/raynalh/scratch/outputSoil/sols", indlot, ".xml", sep = ""),
    #to = xx,
      to = xxsols,
      overwrite = FALSE
    )
    file.copy(
      from = paste(
        workspace_path,
        "/InitCurrentUSMRota/tempopar.sti",
        sep = ""
      ),
      to = xx,
      overwrite = TRUE
    )
    file.copy(
      from = paste(
        workspace_path,
        "/InitCurrentUSMRota/tempoparv6.sti",
        sep = ""
      ),
      to = xx,
      overwrite = TRUE
    )
    file.copy(
      from = paste(
        workspace_path,
        "/InitCurrentUSMRota/IniLegumeGap_ini.xml",
        sep = ""
      ),
      to = xx,
      overwrite = TRUE
    )
    file.copy(
      from = paste(workspace_path, "/InitCurrentUSMRota/rap.mod", sep = ""),
      to = xx,
      overwrite = TRUE
    )
    file.copy(
      from = paste(workspace_path, "/InitCurrentUSMRota/usms.xml", sep = ""),
      to = xx,
      overwrite = TRUE
    )
    file.copy(from = paste(workspace_path, "/InitCurrentUSMRota/var.mod", sep = ""), to = xx, overwrite = TRUE)
    file.copy(
      from = paste(
        workspace_path,
        "/InitCurrentUSMRota/new_travail.usm",
        sep = ""
      ),
      to = xx,
      overwrite = TRUE
    )
    file.copy(
      from = paste(
        workspace_path,
        "/InitCurrentUSMRota/843480_sta.xml",
        sep = ""
      ),
      to = xx,
      overwrite = TRUE
    )
    file.copy(
      from = paste(workspace_path, "/InitCurrentUSMRota/CLIMAISJ.1996", sep = ""),
      to = xx,
      overwrite = TRUE
    )
    file.copy(
      from = paste(
        workspace_path,
        "/InitCurrentUSMRota/CLIMAISJ_sta.xml",
        sep = ""
      ),
      to = xx,
      overwrite = TRUE
    )
    file.copy(
      from = paste(workspace_path, "/InitCurrentUSMRota/mais.lai", sep = ""),
      to = xx,
      overwrite = TRUE
    )
    file.copy(
      from = paste(workspace_path, "/InitCurrentUSMRota/mais_ini.xml", sep = ""),
      to = xx,
      overwrite = TRUE
    )
    file.copy(
      from = paste(workspace_path, "/InitCurrentUSMRota/Mais_tec.xml", sep = ""),
      to = xx,
      overwrite = TRUE
    )
    file.copy(
      from = paste(workspace_path, "/InitCurrentUSMRota/prof.mod", sep = ""),
      to = xx,
      overwrite = TRUE
    )
    file.copy(
      from = paste(
        workspace_path,
        "/InitCurrentUSMRota/maizeP_grain_plt.xml",
        sep = ""
      ),
      to = xx,
      overwrite = TRUE
    )
    file.copy(
      from = paste(workspace_path, "/InitCurrentUSMRota/Mais_tec.xml", sep = ""),
      to = xx,
      overwrite = TRUE
    )
    file.copy(
      from = paste(
        workspace_path,
        "/InitCurrentUSMRota/soybean_0_plt.xml",
        sep = ""
      ),
      to = xx,
      overwrite = TRUE
    )
    file.copy(
      from = paste(
        workspace_path,
        "/InitCurrentUSMRota/soybean_00_plt.xml",
        sep = ""
      ),
      to = xx,
      overwrite = TRUE
    )
    file.copy(
      from = paste(
        workspace_path,
        "/InitCurrentUSMRota/soybean_000_plt.xml",
        sep = ""
      ),
      to = xx,
      overwrite = TRUE
    )
    file.copy(
      from = paste(
        workspace_path,
        "/InitCurrentUSMRota/soybean_0000_plt.xml",
        sep = ""
      ),
      to = xx,
      overwrite = TRUE
    )
    file.copy(
      from = paste(
        workspace_path,
        "/InitCurrentUSMRota/soybean_I_plt.xml",
        sep = ""
      ),
      to = xx,
      overwrite = TRUE
    )
    file.copy(
      from = paste(
        workspace_path,
        "/InitCurrentUSMRota/soybean_II_plt.xml",
        sep = ""
      ),
      to = xx,
      overwrite = TRUE
    )
    file.copy(
      from = paste(
        workspace_path,
        "/InitCurrentUSMRota/soybean_III_plt.xml",
        sep = ""
      ),
      to = xx,
      overwrite = TRUE
    )
    file.copy(
      from = paste(workspace_path, "/InitCurrentUSMRota/Soja_tec.xml", sep = ""),
      to = xx,
      overwrite = TRUE
    )

  }



  copyInitUSM(paste("CurrentUSM", ind, sep = ""))
  NomDir <- paste("CurrentUSM", ind, sep = "")

  linerefpred <- 0 # initiate the number of lines of output results mod_rapport.sti


  # 1 .Loop over Climate Model
  #=======================
  ListClimateModels <- c("0_0", "GFDL-CM3_85", "GISS-E2-R_85", "HadGEM2-ES_85", "MIROC5_85", "MPI-ESM-MR_85")

  for (iClimModel in 1:length(ListClimateModels)) {
    SowingDates <-
            read.csv(
              paste(
                "/home/raynalh/scratch/SowingDates/",
                ListClimateModels[iClimModel], # now all points are in the same file , provided by Legume Gap
                "_sowing-dates.csv",
                sep = ""
              ),
              sep = ",",
              header = TRUE,
              stringsAsFactors = FALSE,
             row.names = NULL
            )
    SowingDatesPerPoint <- SowingDates[which(SowingDates$refId == ind),]

    flag <- 0
    if (iClimModel == 1) {
      CO2 <- 360 #!!! to change depening on climate scenario, in fact the value is in input climate files
    } else {
      CO2 <- 571
    }
    DirClimate <- paste("../climate/", ListClimateModels[iClimModel], "/", sep = "")

    # Build the first USM for point ind and year 1981
    usm_nom <- paste(dataGrid$soil_ref[ind], ".81A", sep = "")
    datedebut <- 1
    datefin <- 365
    finit <-
    file.path(DirIni, "IniLegumeGap_ini.xml", fsep = "") #  changer ultériurement
    nomsol <- dataGrid$soil_ref[ind]
    #!!!!! find a way for station
    fstation <- file.path(DirStation, "843480_sta.xml", fsep = "")
    fclim1 <-
    paste(DirClimate, dataGrid$CLocation[ind], "_v3test.1981", sep = "")
    fclim2 <-
    paste(DirClimate, dataGrid$CLocation[ind], "_v3test.1981", sep = "")
    culturean <- 1
    nbplantes <- 1
    codesimul <- 0
    fplt_1 <- file.path(DirPlant, "soybean_0_plt.xml", fsep = "")
    ftec_1 <- file.path(DirTec, "Soja_tec.xml", fsep = "")
    flai_1 <- "null"
    fplt_2 <- "null"
    ftec_2 <- "null"
    flai_2 <- "null"

    USM30years <-
    cbind.data.frame(
      usm_nom,
      datedebut,
      datefin,
      finit,
      nomsol,
      fstation,
      fclim1,
      fclim2,
      culturean,
      nbplantes,
      codesimul,
      fplt_1,
      ftec_1,
      flai_1,
      fplt_2,
      ftec_2,
      flai_2,
      stringsAsFactors = FALSE
    )


    # Build the other USM for the same point ind and years 1982-2010
    usm_nom <- paste(dataGrid$soil_ref[ind], ".82A", sep = "")
    fclim1 <-
    paste(DirClimate, dataGrid$CLocation[ind], "_v3test.1982", sep = "")
    fclim2 <-
    paste(DirClimate, dataGrid$CLocation[ind], "_v3test.1982", sep = "")
    fplt_1 <- file.path(DirPlant, "maizeP_grain_plt.xml", fsep = "")
    ftec_1 <- file.path(DirTec, "Mais_tec.xml", fsep = "")
    USMYear1982 <-
    cbind.data.frame(
      usm_nom,
      datedebut,
      datefin,
      finit,
      nomsol,
      fstation,
      fclim1,
      fclim2,
      culturean,
      nbplantes,
      codesimul,
      fplt_1,
      ftec_1,
      flai_1,
      fplt_2,
      ftec_2,
      flai_2,
      stringsAsFactors = FALSE
    )

    USM30years <- rbind(USM30years, USMYear1982)

    usm_nom <- paste(dataGrid$soil_ref[ind], ".83A", sep = "")
    fclim1 <-
    paste(DirClimate, dataGrid$CLocation[ind], "_v3test.1983", sep = "")
    fclim2 <-
    paste(DirClimate, dataGrid$CLocation[ind], "_v3test.1983", sep = "")
    fplt_1 <- file.path(DirPlant, "soybean_0_plt.xml", fsep = "")
    ftec_1 <- file.path(DirTec, "Soja_tec.xml", fsep = "")
    USMYear1983 <-
    cbind.data.frame(
      usm_nom,
      datedebut,
      datefin,
      finit,
      nomsol,
      fstation,
      fclim1,
      fclim2,
      culturean,
      nbplantes,
      codesimul,
      fplt_1,
      ftec_1,
      flai_1,
      fplt_2,
      ftec_2,
      flai_2
    )

    USM30years <- rbind(USM30years, USMYear1983)

    usm_nom <- paste(dataGrid$soil_ref[ind], ".84A", sep = "")
    fclim1 <-
    paste(DirClimate, dataGrid$CLocation[ind], "_v3test.1984", sep = "")
    fclim2 <-
    paste(DirClimate, dataGrid$CLocation[ind], "_v3test.1984", sep = "")
    fplt_1 <- file.path(DirPlant, "maizeP_grain_plt.xml", fsep = "")
    ftec_1 <- file.path(DirTec, "Mais_tec.xml", fsep = "")
    datefin <- 366
    USMYear1984 <-
    cbind.data.frame(
      usm_nom,
      datedebut,
      datefin,
      finit,
      nomsol,
      fstation,
      fclim1,
      fclim2,
      culturean,
      nbplantes,
      codesimul,
      fplt_1,
      ftec_1,
      flai_1,
      fplt_2,
      ftec_2,
      flai_2,
      stringsAsFactors = FALSE
    )

    USM30years <- rbind(USM30years, USMYear1984)

    usm_nom <- paste(dataGrid$soil_ref[ind], ".85A", sep = "")
    fclim1 <-
    paste(DirClimate, dataGrid$CLocation[ind], "_v3test.1985", sep = "")
    fclim2 <-
    paste(DirClimate, dataGrid$CLocation[ind], "_v3test.1985", sep = "")
    fplt_1 <- file.path(DirPlant, "soybean_0_plt.xml", fsep = "")
    ftec_1 <- file.path(DirTec, "Soja_tec.xml", fsep = "")
    USMYear1985 <-
    cbind.data.frame(
      usm_nom,
      datedebut,
      datefin,
      finit,
      nomsol,
      fstation,
      fclim1,
      fclim2,
      culturean,
      nbplantes,
      codesimul,
      fplt_1,
      ftec_1,
      flai_1,
      fplt_2,
      ftec_2,
      flai_2,
      stringsAsFactors = FALSE
    )

    USM30years <- rbind(USM30years, USMYear1985)

    usm_nom <- paste(dataGrid$soil_ref[ind], ".86A", sep = "")
    fclim1 <-
    paste(DirClimate, dataGrid$CLocation[ind], "_v3test.1986", sep = "")
    fclim2 <-
    paste(DirClimate, dataGrid$CLocation[ind], "_v3test.1986", sep = "")
    fplt_1 <- file.path(DirPlant, "maizeP_grain_plt.xml", fsep = "")
    ftec_1 <- file.path(DirTec, "Mais_tec.xml", fsep = "")
    USMYear1986 <-
    cbind.data.frame(
      usm_nom,
      datedebut,
      datefin,
      finit,
      nomsol,
      fstation,
      fclim1,
      fclim2,
      culturean,
      nbplantes,
      codesimul,
      fplt_1,
      ftec_1,
      flai_1,
      fplt_2,
      ftec_2,
      flai_2,
      stringsAsFactors = FALSE
    )

    USM30years <- rbind(USM30years, USMYear1986)

    usm_nom <- paste(dataGrid$soil_ref[ind], ".87A", sep = "")
    fclim1 <-
    paste(DirClimate, dataGrid$CLocation[ind], "_v3test.1987", sep = "")
    fclim2 <-
    paste(DirClimate, dataGrid$CLocation[ind], "_v3test.1987", sep = "")
    fplt_1 <- file.path(DirPlant, "soybean_0_plt.xml", fsep = "")
    ftec_1 <- file.path(DirTec, "Soja_tec.xml", fsep = "")
    USMYear1987 <-
    cbind.data.frame(
      usm_nom,
      datedebut,
      datefin,
      finit,
      nomsol,
      fstation,
      fclim1,
      fclim2,
      culturean,
      nbplantes,
      codesimul,
      fplt_1,
      ftec_1,
      flai_1,
      fplt_2,
      ftec_2,
      flai_2,
      stringsAsFactors = FALSE
    )

    USM30years <- rbind(USM30years, USMYear1987)


    usm_nom <- paste(dataGrid$soil_ref[ind], ".88A", sep = "")
    fclim1 <-
    paste(DirClimate, dataGrid$CLocation[ind], "_v3test.1988", sep = "")
    fclim2 <-
    paste(DirClimate, dataGrid$CLocation[ind], "_v3test.1988", sep = "")
    fplt_1 <- file.path(DirPlant, "maizeP_grain_plt.xml", fsep = "")
    ftec_1 <- file.path(DirTec, "Mais_tec.xml", fsep = "")
    datefin <- 366
    USMYear1988 <-
    cbind.data.frame(
      usm_nom,
      datedebut,
      datefin,
      finit,
      nomsol,
      fstation,
      fclim1,
      fclim2,
      culturean,
      nbplantes,
      codesimul,
      fplt_1,
      ftec_1,
      flai_1,
      fplt_2,
      ftec_2,
      flai_2,
      stringsAsFactors = FALSE
    )

    USM30years <- rbind(USM30years, USMYear1988)

    usm_nom <- paste(dataGrid$soil_ref[ind], ".89A", sep = "")
    fclim1 <-
    paste(DirClimate, dataGrid$CLocation[ind], "_v3test.1989", sep = "")
    fclim2 <-
    paste(DirClimate, dataGrid$CLocation[ind], "_v3test.1989", sep = "")
    fplt_1 <- file.path(DirPlant, "soybean_0_plt.xml", fsep = "")
    ftec_1 <- file.path(DirTec, "Soja_tec.xml", fsep = "")
    USMYear1989 <-
    cbind.data.frame(
      usm_nom,
      datedebut,
      datefin,
      finit,
      nomsol,
      fstation,
      fclim1,
      fclim2,
      culturean,
      nbplantes,
      codesimul,
      fplt_1,
      ftec_1,
      flai_1,
      fplt_2,
      ftec_2,
      flai_2,
      stringsAsFactors = FALSE
    )

    USM30years <- rbind(USM30years, USMYear1989)

    usm_nom <-
    as.character(paste(dataGrid$soil_ref[ind], ".90A", sep = ""))
    fclim1 <-
    paste(DirClimate, dataGrid$CLocation[ind], "_v3test.1990", sep = "")
    fclim2 <-
    paste(DirClimate, dataGrid$CLocation[ind], "_v3test.1990", sep = "")
    fplt_1 <- file.path(DirPlant, "maizeP_grain_plt.xml", fsep = "")
    ftec_1 <- file.path(DirTec, "Mais_tec.xml", fsep = "")
    USMYear1990 <-
    cbind.data.frame(
      usm_nom,
      datedebut,
      datefin,
      finit,
      nomsol,
      fstation,
      fclim1,
      fclim2,
      culturean,
      nbplantes,
      codesimul,
      fplt_1,
      ftec_1,
      flai_1,
      fplt_2,
      ftec_2,
      flai_2,
      stringsAsFactors = FALSE
    )

    USM30years <- rbind(USM30years, USMYear1990)

    usm_nom <- paste(dataGrid$soil_ref[ind], ".91A", sep = "")
    fclim1 <-
    paste(DirClimate, dataGrid$CLocation[ind], "_v3test.1991", sep = "")
    fclim2 <-
    paste(DirClimate, dataGrid$CLocation[ind], "_v3test.1991", sep = "")
    fplt_1 <- file.path(DirPlant, "soybean_0_plt.xml", fsep = "")
    ftec_1 <- file.path(DirTec, "Soja_tec.xml", fsep = "")
    USMYear1991 <-
    cbind.data.frame(
      usm_nom,
      datedebut,
      datefin,
      finit,
      nomsol,
      fstation,
      fclim1,
      fclim2,
      culturean,
      nbplantes,
      codesimul,
      fplt_1,
      ftec_1,
      flai_1,
      fplt_2,
      ftec_2,
      flai_2,
      stringsAsFactors = FALSE
    )

    USM30years <- rbind(USM30years, USMYear1991)

    usm_nom <- paste(dataGrid$soil_ref[ind], ".92A", sep = "")
    fclim1 <-
    paste(DirClimate, dataGrid$CLocation[ind], "_v3test.1992", sep = "")
    fclim2 <-
    paste(DirClimate, dataGrid$CLocation[ind], "_v3test.1992", sep = "")
    fplt_1 <- file.path(DirPlant, "maizeP_grain_plt.xml", fsep = "")
    ftec_1 <- file.path(DirTec, "Mais_tec.xml", fsep = "")
    datefin <- 366
    USMYear1992 <-
    cbind.data.frame(
      usm_nom,
      datedebut,
      datefin,
      finit,
      nomsol,
      fstation,
      fclim1,
      fclim2,
      culturean,
      nbplantes,
      codesimul,
      fplt_1,
      ftec_1,
      flai_1,
      fplt_2,
      ftec_2,
      flai_2,
      stringsAsFactors = FALSE
    )

    USM30years <- rbind(USM30years, USMYear1992)

    usm_nom <- paste(dataGrid$soil_ref[ind], ".93A", sep = "")
    fclim1 <-
    paste(DirClimate, dataGrid$CLocation[ind], "_v3test.1993", sep = "")
    fclim2 <-
    paste(DirClimate, dataGrid$CLocation[ind], "_v3test.1993", sep = "")
    fplt_1 <- file.path(DirPlant, "soybean_0_plt.xml", fsep = "")
    ftec_1 <- file.path(DirTec, "Soja_tec.xml", fsep = "")
    USMYear1993 <-
    cbind.data.frame(
      usm_nom,
      datedebut,
      datefin,
      finit,
      nomsol,
      fstation,
      fclim1,
      fclim2,
      culturean,
      nbplantes,
      codesimul,
      fplt_1,
      ftec_1,
      flai_1,
      fplt_2,
      ftec_2,
      flai_2,
      stringsAsFactors = FALSE
    )

    USM30years <- rbind(USM30years, USMYear1993)

    usm_nom <- paste(dataGrid$soil_ref[ind], ".94A", sep = "")
    fclim1 <-
    paste(DirClimate, dataGrid$CLocation[ind], "_v3test.1994", sep = "")
    fclim2 <-
    paste(DirClimate, dataGrid$CLocation[ind], "_v3test.1994", sep = "")
    fplt_1 <- file.path(DirPlant, "maizeP_grain_plt.xml", fsep = "")
    ftec_1 <- file.path(DirTec, "Mais_tec.xml", fsep = "")
    USMYear1994 <-
    cbind.data.frame(
      usm_nom,
      datedebut,
      datefin,
      finit,
      nomsol,
      fstation,
      fclim1,
      fclim2,
      culturean,
      nbplantes,
      codesimul,
      fplt_1,
      ftec_1,
      flai_1,
      fplt_2,
      ftec_2,
      flai_2,
      stringsAsFactors = FALSE
    )

    USM30years <- rbind(USM30years, USMYear1994)

    usm_nom <- paste(dataGrid$soil_ref[ind], ".95A", sep = "")
    fclim1 <-
    paste(DirClimate, dataGrid$CLocation[ind], "_v3test.1995", sep = "")
    fclim2 <-
    paste(DirClimate, dataGrid$CLocation[ind], "_v3test.1995", sep = "")
    fplt_1 <- file.path(DirPlant, "soybean_0_plt.xml", fsep = "")
    ftec_1 <- file.path(DirTec, "Soja_tec.xml", fsep = "")
    USMYear1995 <-
    cbind.data.frame(
      usm_nom,
      datedebut,
      datefin,
      finit,
      nomsol,
      fstation,
      fclim1,
      fclim2,
      culturean,
      nbplantes,
      codesimul,
      fplt_1,
      ftec_1,
      flai_1,
      fplt_2,
      ftec_2,
      flai_2,
      stringsAsFactors = FALSE
    )

    USM30years <- rbind(USM30years, USMYear1995)

    usm_nom <- paste(dataGrid$soil_ref[ind], ".96A", sep = "")
    fclim1 <-
    paste(DirClimate, dataGrid$CLocation[ind], "_v3test.1996", sep = "")
    fclim2 <-
    paste(DirClimate, dataGrid$CLocation[ind], "_v3test.1996", sep = "")
    fplt_1 <- file.path(DirPlant, "maizeP_grain_plt.xml", fsep = "")
    ftec_1 <- file.path(DirTec, "Mais_tec.xml", fsep = "")
    datefin <- 366
    USMYear1996 <-
    cbind.data.frame(
      usm_nom,
      datedebut,
      datefin,
      finit,
      nomsol,
      fstation,
      fclim1,
      fclim2,
      culturean,
      nbplantes,
      codesimul,
      fplt_1,
      ftec_1,
      flai_1,
      fplt_2,
      ftec_2,
      flai_2,
      stringsAsFactors = FALSE
    )

    USM30years <- rbind(USM30years, USMYear1996)

    usm_nom <- paste(dataGrid$soil_ref[ind], ".97A", sep = "")
    fclim1 <-
    paste(DirClimate, dataGrid$CLocation[ind], "_v3test.1997", sep = "")
    fclim2 <-
    paste(DirClimate, dataGrid$CLocation[ind], "_v3test.1997", sep = "")
    fplt_1 <- file.path(DirPlant, "soybean_0_plt.xml", fsep = "")
    ftec_1 <- file.path(DirTec, "Soja_tec.xml", fsep = "")
    USMYear1997 <-
    cbind.data.frame(
      usm_nom,
      datedebut,
      datefin,
      finit,
      nomsol,
      fstation,
      fclim1,
      fclim2,
      culturean,
      nbplantes,
      codesimul,
      fplt_1,
      ftec_1,
      flai_1,
      fplt_2,
      ftec_2,
      flai_2,
      stringsAsFactors = FALSE
    )

    USM30years <- rbind(USM30years, USMYear1997)

    usm_nom <- paste(dataGrid$soil_ref[ind], ".98A", sep = "")
    fclim1 <-
    paste(DirClimate, dataGrid$CLocation[ind], "_v3test.1998", sep = "")
    fclim2 <-
    paste(DirClimate, dataGrid$CLocation[ind], "_v3test.1998", sep = "")
    fplt_1 <- file.path(DirPlant, "maizeP_grain_plt.xml", fsep = "")
    ftec_1 <- file.path(DirTec, "Mais_tec.xml", fsep = "")
    USMYear1998 <-
    cbind.data.frame(
      usm_nom,
      datedebut,
      datefin,
      finit,
      nomsol,
      fstation,
      fclim1,
      fclim2,
      culturean,
      nbplantes,
      codesimul,
      fplt_1,
      ftec_1,
      flai_1,
      fplt_2,
      ftec_2,
      flai_2,
      stringsAsFactors = FALSE
    )

    USM30years <- rbind(USM30years, USMYear1998)

    usm_nom <- paste(dataGrid$soil_ref[ind], ".99A", sep = "")
    fclim1 <-
    paste(DirClimate, dataGrid$CLocation[ind], "_v3test.1999", sep = "")
    fclim2 <-
    paste(DirClimate, dataGrid$CLocation[ind], "_v3test.1999", sep = "")
    fplt_1 <- file.path(DirPlant, "soybean_0_plt.xml", fsep = "")
    ftec_1 <- file.path(DirTec, "Soja_tec.xml", fsep = "")
    USMYear1999 <-
    cbind.data.frame(
      usm_nom,
      datedebut,
      datefin,
      finit,
      nomsol,
      fstation,
      fclim1,
      fclim2,
      culturean,
      nbplantes,
      codesimul,
      fplt_1,
      ftec_1,
      flai_1,
      fplt_2,
      ftec_2,
      flai_2,
      stringsAsFactors = FALSE
    )

    USM30years <- rbind(USM30years, USMYear1999)

    usm_nom <-
    as.character(paste(dataGrid$soil_ref[ind], ".00A", sep = ""))
    fclim1 <-
    paste(DirClimate, dataGrid$CLocation[ind], "_v3test.2000", sep = "")
    fclim2 <-
    paste(DirClimate, dataGrid$CLocation[ind], "_v3test.2000", sep = "")
    fplt_1 <- file.path(DirPlant, "maizeP_grain_plt.xml", fsep = "")
    ftec_1 <- file.path(DirTec, "Mais_tec.xml", fsep = "")
    USMYear2000 <-
    cbind.data.frame(
      usm_nom,
      datedebut,
      datefin,
      finit,
      nomsol,
      fstation,
      fclim1,
      fclim2,
      culturean,
      nbplantes,
      codesimul,
      fplt_1,
      ftec_1,
      flai_1,
      fplt_2,
      ftec_2,
      flai_2,
      stringsAsFactors = FALSE
    )

    USM30years <- rbind(USM30years, USMYear2000)

    usm_nom <- paste(dataGrid$soil_ref[ind], ".01A", sep = "")
    fclim1 <-
    paste(DirClimate, dataGrid$CLocation[ind], "_v3test.2001", sep = "")
    fclim2 <-
    paste(DirClimate, dataGrid$CLocation[ind], "_v3test.2001", sep = "")
    fplt_1 <- file.path(DirPlant, "soybean_0_plt.xml", fsep = "")
    ftec_1 <- file.path(DirTec, "Soja_tec.xml", fsep = "")
    USMYear2001 <-
    cbind.data.frame(
      usm_nom,
      datedebut,
      datefin,
      finit,
      nomsol,
      fstation,
      fclim1,
      fclim2,
      culturean,
      nbplantes,
      codesimul,
      fplt_1,
      ftec_1,
      flai_1,
      fplt_2,
      ftec_2,
      flai_2,
      stringsAsFactors = FALSE
    )

    USM30years <- rbind(USM30years, USMYear2001)

    usm_nom <- paste(dataGrid$soil_ref[ind], ".02A", sep = "")
    fclim1 <-
    paste(DirClimate, dataGrid$CLocation[ind], "_v3test.2002", sep = "")
    fclim2 <-
    paste(DirClimate, dataGrid$CLocation[ind], "_v3test.2002", sep = "")
    fplt_1 <- file.path(DirPlant, "maizeP_grain_plt.xml", fsep = "")
    ftec_1 <- file.path(DirTec, "Mais_tec.xml", fsep = "")
    USMYear2002 <-
    cbind.data.frame(
      usm_nom,
      datedebut,
      datefin,
      finit,
      nomsol,
      fstation,
      fclim1,
      fclim2,
      culturean,
      nbplantes,
      codesimul,
      fplt_1,
      ftec_1,
      flai_1,
      fplt_2,
      ftec_2,
      flai_2,
      stringsAsFactors = FALSE
    )

    USM30years <- rbind(USM30years, USMYear2002)

    usm_nom <- paste(dataGrid$soil_ref[ind], ".03A", sep = "")
    fclim1 <-
    paste(DirClimate, dataGrid$CLocation[ind], "_v3test.2003", sep = "")
    fclim2 <-
    paste(DirClimate, dataGrid$CLocation[ind], "_v3test.2003", sep = "")
    fplt_1 <- file.path(DirPlant, "soybean_0_plt.xml", fsep = "")
    ftec_1 <- file.path(DirTec, "Soja_tec.xml", fsep = "")
    USMYear2003 <-
    cbind.data.frame(
      usm_nom,
      datedebut,
      datefin,
      finit,
      nomsol,
      fstation,
      fclim1,
      fclim2,
      culturean,
      nbplantes,
      codesimul,
      fplt_1,
      ftec_1,
      flai_1,
      fplt_2,
      ftec_2,
      flai_2,
      stringsAsFactors = FALSE
    )

    USM30years <- rbind(USM30years, USMYear2003)

    usm_nom <- paste(dataGrid$soil_ref[ind], ".04A", sep = "")
    fclim1 <-
    paste(DirClimate, dataGrid$CLocation[ind], "_v3test.2004", sep = "")
    fclim2 <-
    paste(DirClimate, dataGrid$CLocation[ind], "_v3test.2004", sep = "")
    fplt_1 <- file.path(DirPlant, "maizeP_grain_plt.xml", fsep = "")
    ftec_1 <- file.path(DirTec, "Mais_tec.xml", fsep = "")
    datefin <- 366
    USMYear2004 <-
    cbind.data.frame(
      usm_nom,
      datedebut,
      datefin,
      finit,
      nomsol,
      fstation,
      fclim1,
      fclim2,
      culturean,
      nbplantes,
      codesimul,
      fplt_1,
      ftec_1,
      flai_1,
      fplt_2,
      ftec_2,
      flai_2,
      stringsAsFactors = FALSE
    )

    USM30years <- rbind(USM30years, USMYear2004)

    usm_nom <- paste(dataGrid$soil_ref[ind], ".05A", sep = "")
    fclim1 <-
    paste(DirClimate, dataGrid$CLocation[ind], "_v3test.2005", sep = "")
    fclim2 <-
    paste(DirClimate, dataGrid$CLocation[ind], "_v3test.2005", sep = "")
    fplt_1 <- file.path(DirPlant, "soybean_0_plt.xml", fsep = "")
    ftec_1 <- file.path(DirTec, "Soja_tec.xml", fsep = "")
    USMYear2005 <-
    cbind.data.frame(
      usm_nom,
      datedebut,
      datefin,
      finit,
      nomsol,
      fstation,
      fclim1,
      fclim2,
      culturean,
      nbplantes,
      codesimul,
      fplt_1,
      ftec_1,
      flai_1,
      fplt_2,
      ftec_2,
      flai_2,
      stringsAsFactors = FALSE
    )

    USM30years <- rbind(USM30years, USMYear2005)

    usm_nom <- paste(dataGrid$soil_ref[ind], ".06A", sep = "")
    fclim1 <-
    paste(DirClimate, dataGrid$CLocation[ind], "_v3test.2006", sep = "")
    fclim2 <-
    paste(DirClimate, dataGrid$CLocation[ind], "_v3test.2006", sep = "")
    fplt_1 <- file.path(DirPlant, "maizeP_grain_plt.xml", fsep = "")
    ftec_1 <- file.path(DirTec, "Mais_tec.xml", fsep = "")
    USMYear2006 <-
    cbind.data.frame(
      usm_nom,
      datedebut,
      datefin,
      finit,
      nomsol,
      fstation,
      fclim1,
      fclim2,
      culturean,
      nbplantes,
      codesimul,
      fplt_1,
      ftec_1,
      flai_1,
      fplt_2,
      ftec_2,
      flai_2,
      stringsAsFactors = FALSE
    )

    USM30years <- rbind(USM30years, USMYear2006)

    usm_nom <- paste(dataGrid$soil_ref[ind], ".07A", sep = "")
    fclim1 <-
    paste(DirClimate, dataGrid$CLocation[ind], "_v3test.2007", sep = "")
    fclim2 <-
    paste(DirClimate, dataGrid$CLocation[ind], "_v3test.2007", sep = "")
    fplt_1 <- file.path(DirPlant, "soybean_0_plt.xml", fsep = "")
    ftec_1 <- file.path(DirTec, "Soja_tec.xml", fsep = "")
    USMYear2007 <-
    cbind.data.frame(
      usm_nom,
      datedebut,
      datefin,
      finit,
      nomsol,
      fstation,
      fclim1,
      fclim2,
      culturean,
      nbplantes,
      codesimul,
      fplt_1,
      ftec_1,
      flai_1,
      fplt_2,
      ftec_2,
      flai_2,
      stringsAsFactors = FALSE
    )

    USM30years <- rbind(USM30years, USMYear2007)

    usm_nom <- paste(dataGrid$soil_ref[ind], ".08A", sep = "")
    fclim1 <-
    paste(DirClimate, dataGrid$CLocation[ind], "_v3test.2008", sep = "")
    fclim2 <-
    paste(DirClimate, dataGrid$CLocation[ind], "_v3test.2008", sep = "")
    fplt_1 <- file.path(DirPlant, "maizeP_grain_plt.xml", fsep = "")
    ftec_1 <- file.path(DirTec, "Mais_tec.xml", fsep = "")
    datefin <- 366
    USMYear2008 <-
    cbind.data.frame(
      usm_nom,
      datedebut,
      datefin,
      finit,
      nomsol,
      fstation,
      fclim1,
      fclim2,
      culturean,
      nbplantes,
      codesimul,
      fplt_1,
      ftec_1,
      flai_1,
      fplt_2,
      ftec_2,
      flai_2,
      stringsAsFactors = FALSE
    )

    USM30years <- rbind(USM30years, USMYear2008)

    usm_nom <- paste(dataGrid$soil_ref[ind], ".09A", sep = "")
    fclim1 <-
    paste(DirClimate, dataGrid$CLocation[ind], "_v3test.2009", sep = "")
    fclim2 <-
    paste(DirClimate, dataGrid$CLocation[ind], "_v3test.2009", sep = "")
    fplt_1 <- file.path(DirPlant, "soybean_0_plt.xml", fsep = "")
    ftec_1 <- file.path(DirTec, "Soja_tec.xml", fsep = "")
    USMYear2009 <-
    cbind.data.frame(
      usm_nom,
      datedebut,
      datefin,
      finit,
      nomsol,
      fstation,
      fclim1,
      fclim2,
      culturean,
      nbplantes,
      codesimul,
      fplt_1,
      ftec_1,
      flai_1,
      fplt_2,
      ftec_2,
      flai_2,
      stringsAsFactors = FALSE
    )

    USM30years <- rbind(USM30years, USMYear2009)

    usm_nom <- paste(dataGrid$soil_ref[ind], ".10A", sep = "")
    fclim1 <-
    paste(DirClimate, dataGrid$CLocation[ind], "_v3test.2010", sep = "")
    fclim2 <-
    paste(DirClimate, dataGrid$CLocation[ind], "_v3test.2010", sep = "")
    fplt_1 <- file.path(DirPlant, "maizeP_grain_plt.xml", fsep = "")
    ftec_1 <- file.path(DirTec, "Mais_tec.xml", fsep = "")
    USMYear2010 <-
    cbind.data.frame(
      usm_nom,
      datedebut,
      datefin,
      finit,
      nomsol,
      fstation,
      fclim1,
      fclim2,
      culturean,
      nbplantes,
      codesimul,
      fplt_1,
      ftec_1,
      flai_1,
      fplt_2,
      ftec_2,
      flai_2,
      stringsAsFactors = FALSE
    )

    USM30years <- rbind(USM30years, USMYear2010)


    #functions to use if copy configuration files for report
    copyVarMod <-
    function(x) {
      file.copy(
        from = file.path(DirVarMod, "var.mod", fsep = "/"),
        to = x,
        overwrite = TRUE
      )
    }

    #functions to use if dependant USM
    copyNewUSM <-
    function(x) {
      file.copy(
        from = file.path(workspace_path, "/new_travail.usm", fsep = ""),
        to = x,
        overwrite = TRUE
      )
    }


    #Preparing the loop of preprocess of data, simulation, and postprocess of simulation results for the point considered
    # ===================================================================================================================

    #copyInitUSM(paste("1CurrentUSM", ind, sep = ""))
    #NomDir <- paste("1CurrentUSM", ind, sep = "")
    USMsFile <- file.path(workspace_path, "/", NomDir, "/usms.xml", fsep = "")




    # 2 . Loop over Maturity Group
    #=============================
    ListMaturityGroup <- c("0", "00", "000", "0000", "I", "II", "III")
    for (iMatGroup in 1:length(ListMaturityGroup)) {


      # 3 . Loop over Management
      #=============================
      ListProductionCase <- c("Unlimited", "Actual")
      #for (iListProductionCase in 1:length(ListProductionCase)) {
      for (iListProductionCase in 1:2) {

        #4 .Loop over the 30 USM
        #========================
        for (iUSM in 1:nrow(USM30years)) {
          #flag <- 0 # to test if it is the first year of the set of 30 years and change the climate file
          USM30years[iUSM,]$usm_nom <- NomDir

          #read .csv file with calculated SowingDates, if doesn't present in the file, take default value 135
          # SowingDates <-
          #   read.csv(
          #     paste(
          #       workspace_path,
          #       "/SowingDates/",
          #ListClimateModels[iClimModel],indlot,
          #       ListClimateModels[iClimModel], # now all points are in the same file , provided by Legume Gap
          #       "_sowing-dates.csv",
          #       sep = ""
          #     ),
          #     sep = ",",
          #header = FALSE,
          #     header = TRUE,
          #     stringsAsFactors = FALSE,
          #    row.names = NULL
          #   )

          SowingDatesCrop <- 135

          # if (!is.null(SowingDates[which(SowingDates$refId == ind & substr(SowingDates$Date, 1, 4) == iUSM), ])) {
          #   SowingDatesPoint <- SowingDates[which(SowingDates$refId == ind & substr(SowingDates$Date, 1, 4) == iUSM),]
          #   SowingDatesCrop <- SowingDatesPoint$sowDOY #using sowing dates provided by Legume Gap
          #   if (is.na(SowingDatesCrop)) {
          #     SowingDatesCrop <- 135
          #   }
          # } 

          #Generate stics input files usi/ing informations stored in data frame USM30years
          # specific treatment for 1981
          if (grepl("1981", USM30years[iUSM, ]$fclim1) == TRUE) {
            indyear <- 1981
            if (!file.exists(paste(
               workspace_path, NomDir, "param.sol", sep = "/" ))) {
              gen_usms_xml(
                          usms_param = USM30years[iUSM,],
                          usms_out_file = paste(workspace_path, "/", NomDir, "/usms.xml", sep = "")
                           )
              SticsRFiles::gen_usms_xml2txt(
                          javastics_path,
                          workspace_path = paste(workspace_path, NomDir, sep = "/"),
                          dir_per_usm_flag = FALSE
                           )
            }
            #end if file exist  #Sys.sleep(1) 

            #Copy climate file
            file.copy(
              from = paste(
            #workspace_path,
                "../climate/",
                ListClimateModels[iClimModel],
                "/",
                dataGrid$CLocation[ind],
                "_v3test.",
                indyear,
                sep = ""
                ),
                to = paste(workspace_path, NomDir, "climat.txt", sep = "/"),
                overwrite = TRUE
                )
            SticsRFiles::set_usm_txt(
              filepath = paste(workspace_path, NomDir, "new_travail.usm", sep = "/"),
              param = "fclim1",
              value = paste(
                workspace_path,
                "/climate/",
                ListClimateModels[iClimModel],
                "/",
                dataGrid$CLocation[ind],
                "_v3test.",
                indyear,
                sep = ""
              )
            )
            SticsRFiles::set_usm_txt(
              filepath = paste(workspace_path, NomDir, "new_travail.usm", sep = "/"),
              param = "fclim2",
              value = paste(
                workspace_path,
                "/climate/",
            #"climate/macsur_v3_corr_test/",
                ListClimateModels[iClimModel],
                "/",
                dataGrid$CLocation[ind],
                "_v3test.",
                indyear,
                sep = ""
              )
            )

            #Change latitude in station file
            SticsRFiles::set_usm_txt(
              filepath = paste(workspace_path, NomDir, "station.txt", sep = "/"),
              param = "latitude",
              value = dataGrid[ind,]$latitude
            )

            # as 1981 is the first year of the succession, put codesuite=0
            SticsRFiles::set_usm_txt(
              filepath = paste(workspace_path, NomDir, "new_travail.usm", sep = "/"),
              param = "codesuite",
              value = 0
            )

            #Change duration² 
            SticsRFiles::set_usm_txt(
              filepath = paste(workspace_path, NomDir, "new_travail.usm", sep = "/"),
              param = "datefin",
              value = 365
            )

            #Change maturity 
            SticsRFiles::set_usm_txt(
              filepath = paste(workspace_path, NomDir, "new_travail.usm", sep = "/"),
              param = "fplt1",
              value = paste("soybean_", ListMaturityGroup[iMatGroup], "_plt.xml", sep = ""))

     file.copy(
                from = paste(
                  workspace_path,
                  "/InitCurrentUSMSoja/",
                  ListMaturityGroup[iMatGroup],
                  "ficplt1.txt",
                  sep = ""
                ),
                to = paste(workspace_path, NomDir, "ficplt1.txt", sep = "/"),
                overwrite = TRUE
              )

              file.copy(
                from = paste(
                  workspace_path,
                  "/InitCurrentUSMSoja/fictec1.txt",
                  sep = ""
                ),
                to = paste(workspace_path, NomDir, "fictec1.txt", sep = "/"),
                overwrite = TRUE
              )


            #Change SowingDates
            if (!is.null(SowingDatesPerPoint[which(substr(SowingDatesPerPoint$Date, 1, 4) == indyear), ])) {
              SowingDatesPoint <- SowingDatesPerPoint[which(substr(SowingDatesPerPoint$Date, 1, 4) == indyear),]
              SowingDatesCrop <- SowingDatesPoint$sowDOY #using sowing dates provided by Legume Gap
              if (is.na(SowingDatesCrop)) {
                SowingDatesCrop <- 135
              }
            }
            SticsRFiles::set_usm_txt(
                filepath = paste(workspace_path, NomDir, "fictec1.txt", sep = "/"),
                param = "iplt0",
                value = SowingDatesCrop
              )

            #Change if automatic irrigation
            if (iListProductionCase == 2) {
              #Actual
              SticsRFiles::set_usm_txt(
                  filepath = paste(workspace_path, NomDir, "fictec1.txt", sep = "/"),
                  param = "codecalirrig",
                  value = 2
                )
            } else {
              SticsRFiles::set_usm_txt(
                  filepath = paste(workspace_path, NomDir, "fictec1.txt", sep = "/"),
                  param = "codecalirrig",
                  value = 1
                )
            }


          } else {

            # treatment of other years (not  1981)
            indyear = indyear + 1
            #Change climate files
            file.copy(
              from = paste(
                workspace_path,
                "/climate/",
                ListClimateModels[iClimModel],
                "/",
                dataGrid$CLocation[ind],
                "_v3test.",
                indyear,
                sep = ""
              ),
              to = paste(workspace_path, NomDir, "climat.txt", sep = "/"),
              overwrite = TRUE
            )
            SticsRFiles::set_usm_txt(
              filepath = paste(workspace_path, NomDir, "new_travail.usm", sep = "/"),
              param = "fclim1",
              value = paste(
                workspace_path,
                "/climate/",
                ListClimateModels[iClimModel],
                "/",
                dataGrid$CLocation[ind],
                "_v3test.",
                indyear,
                sep = ""
              )
            )
            SticsRFiles::set_usm_txt(
              filepath = paste(workspace_path, NomDir, "new_travail.usm", sep = "/"),
              param = "fclim2",
              value = paste(
                workspace_path,
                "/climate/",
            #"climate/macsur_v3_corr_test/",
                ListClimateModels[iClimModel],
                "/",
                dataGrid$CLocation[ind],
                "_v3test.",
                indyear,
                sep = ""
              )
            )
            SticsRFiles::set_usm_txt(
              filepath = paste(workspace_path, NomDir, "new_travail.usm", sep = "/"),
              param = "codesuite",
              value = 1
            )
            #Change latitude in station file
            SticsRFiles::set_usm_txt(
              filepath = paste(workspace_path, NomDir, "station.txt", sep = "/"),
              param = "latitude",
              value = dataGrid[ind,]$latitude
            )
            #Change duration 
            SticsRFiles::set_usm_txt(
              filepath = paste(workspace_path, NomDir, "new_travail.usm", sep = "/"),
              param = "datefin",
              value = 365
            )
            #Change duration of simulation for bissextile years
            if (is.element(indyear, c(1984, 1988, 1992, 1996, 2000, 2004, 2008))) {
              SticsRFiles::set_usm_txt(
                filepath = paste(workspace_path, NomDir, "new_travail.usm", sep = "/"),
                param = "datefin",
                value = 366
              )
            }
            #Change crop for years with maize
            if (is.element(
              indyear,
              c(
                1982,
                1984,
                1986,
                1988,
                1990,
                1992,
                1994,
                1996,
                1998,
                2000,
                2002,
                2004,
                2006,
                2008,
                2010
              )
            )) {
              SticsRFiles::set_usm_txt(
                filepath = paste(workspace_path, NomDir, "new_travail.usm", sep = "/"),
                param = "fplt1",
                value = "maizeP_grain_plt.xml"
              )
              SticsRFiles::set_usm_txt(
                filepath = paste(workspace_path, NomDir, "new_travail.usm", sep = "/"),
                param = "ftec1",
                value = "MaisDT_tec.xml"
              )
              file.copy(
                from = paste(
                  workspace_path,
                  "/InitCurrentUSMMais/ficplt1.txt",
                  sep = ""
                ),
                to = paste(workspace_path, NomDir, "ficplt1.txt", sep = "/"),
                overwrite = TRUE
              )
              file.copy(
                from = paste(
                  workspace_path,
                  "/InitCurrentUSMMais/fictec1.txt",
                  sep = ""
                ),
                to = paste(workspace_path, NomDir, "fictec1.txt", sep = "/"),
                overwrite = TRUE
              )
            } else {
              SticsRFiles::set_usm_txt(
                filepath = paste(workspace_path, NomDir, "new_travail.usm", sep = "/"),
                param = "fplt1",
                value = paste("soybean_", ListMaturityGroup[iMatGroup], "_plt.xml", sep = "")
              )
              SticsRFiles::set_usm_txt(
                filepath = paste(workspace_path, NomDir, "new_travail.usm", sep = "/"),
                param = "ftec1",
                value = "Soja_tec.xml"
              )
              #Copy file plant corresponding to the maturity group of the simulated crop
              file.copy(
                from = paste(
                  workspace_path,
                  "/InitCurrentUSMSoja/",
                  ListMaturityGroup[iMatGroup],
                  "ficplt1.txt",
                  sep = ""
                ),
                to = paste(workspace_path, NomDir, "ficplt1.txt", sep = "/"),
                overwrite = TRUE
              )

              file.copy(
                from = paste(
                  workspace_path,
                  "/InitCurrentUSMSoja/fictec1.txt",
                  sep = ""
                ),
                to = paste(workspace_path, NomDir, "fictec1.txt", sep = "/"),
                overwrite = TRUE
              )


              #Change SowingDates
            if (!is.null(SowingDatesPerPoint[which(substr(SowingDatesPerPoint$Date, 1, 4) == indyear), ])) {
              SowingDatesPoint <- SowingDatesPerPoint[which(substr(SowingDatesPerPoint$Date, 1, 4) == indyear),]
              SowingDatesCrop <- SowingDatesPoint$sowDOY #using sowing dates provided by Legume Gap
              if (is.na(SowingDatesCrop)) {
                SowingDatesCrop <- 135
              }
            }

              SticsRFiles::set_usm_txt(
                filepath = paste(workspace_path, NomDir, "fictec1.txt", sep = "/"),
                param = "iplt0",
                value = SowingDatesCrop
              )

              #Change if automatic irrigation
              if (iListProductionCase == 2) {
                #Actual
                SticsRFiles::set_usm_txt(
                  filepath = paste(workspace_path, NomDir, "fictec1.txt", sep = "/"),
                  param = "codecalirrig",
                  value = 2
                )
              } else {
                #if (grepl("Unlimited", ListProductionCase[iListProductionCase]) == TRUE) {
                SticsRFiles::set_usm_txt(
                  filepath = paste(workspace_path, NomDir, "fictec1.txt", sep = "/"),
                  param = "codecalirrig",
                  value = 1
                )
              }
            }
          }

          #To copy the template file varmod used to specify the outputs
          #copyVarMod(NomDir)

          # Run STICS
          run_stics(stics_path, paste(workspace_path, NomDir, sep = "/"))

          #Test in case run of stics has failed. If yes, stics is run once more
          if ((!file.exists(paste(
            workspace_path, NomDir, "recup.tmp", sep = "/"
          ))) |
          (file.size(paste(
            workspace_path, NomDir, "recup.tmp", sep = "/"
          )) < 2) == TRUE) {
            #  file.copy(
            #   from = paste(workspace_path,NomDir, "recup2.tmp", sep = "/"),
            #  to = paste(workspace_path, NomDir, "recup.tmp", sep = "/") ,
            #  overwrite = TRUE
            #) #ajout 14oct
            SticsRFiles::gen_usms_xml2txt(
                          javastics_path,
                          workspace_path = paste(workspace_path, NomDir, sep = "/"),
                          dir_per_usm_flag = FALSE
                           )
            SticsRFiles::set_usm_txt(
              filepath = paste(workspace_path, NomDir, "new_travail.usm", sep = "/"),
              param = "codesuite",
              value = 0
            ) #idem
            #Change SowingDates
            SticsRFiles::set_usm_txt(
                filepath = paste(workspace_path, NomDir, "fictec1.txt", sep = "/"),
                param = "iplt0",
                value = 135
              )
            run_stics(stics_path, paste(workspace_path, NomDir, sep = "/"))
            #Sys.sleep(1)
            #SticsRFiles::set_usm_txt(
            #  filepath = paste(workspace_path, NomDir, "new_travail.usm", sep = "/"),
            #  param = "codesuite",
            #  value =1 
            #)  #idem
          }

          #save the recup.tmp file
          #file.copy(
          #  from = paste(workspace_path, NomDir, "recup.tmp", sep = "/"),
          #  to = paste(workspace_path, NomDir, "recup2.tmp", sep = "/") ,
          #  overwrite = TRUE
          #) #ajout 14oct


          # Build result files for LegumGap
          #Read report file: mod_rapport.sti. The file contains all the simulation results for USM30years. By year, there is 6 lines corresponding to the phenological stage chosen: plt emer flo drp mat rec
          while (!file.exists(paste("/home/raynalh/scratch/", NomDir, "/mod_rapport.sti", sep = ""))) {
            #Sys.sleep(1) 
            run_stics(stics_path, paste(workspace_path, NomDir, sep = "/"))
          }

          if (file.exists(paste("/home/raynalh/scratch/", NomDir, "/mod_rapport.sti", sep = ""))) {
            Rapport <-
                   read.csv(
                       paste("/home/raynalh/scratch/", NomDir, "/mod_rapport.sti", sep = ""),
                      sep = ";",
            header = FALSE,
            stringsAsFactors = FALSE,
            row.names = NULL
          )
            linerefcurr <- nrow(Rapport)
          } else {
            linerefcurr <- -1
          }


          #Loop on results file
          #for (iUSM in 1:nrow(USM30years)) {
          #      for (iUSM in 1:30) {


          # Test the number of lines in mod-rapport.sti
          # -------------------------------------------


          if (linerefcurr == (linerefpred + 6)) {
            #treatment of results. Simulation was run
            # -------------------------------------------

            #Keep the variables required by protocol and statistical model. Currently the results are on 6 lines corresponding to the different phenological stages
            #Variables required by Legume Gap protocol
            #lineref <- linerefpred + ((iUSM - 1) * 6) 
            lineref <- linerefpred # lineref corresponds to the last line before new USM
            #lineref <- 0 # !!!!!! attention à changer
            Model <- "ST"
            soil_ref <- ind
            first_crop <- "soybean"
            if (is.element(
              indyear,
              c(
                1982,
                1984,
                1986,
                1988,
                1990,
                1992,
                1994,
                1996,
                1998,
                2000,
                2002,
                2004,
                2006,
                2008,
                2010
              ))) {
              Crop <- "maize"
            } else {
              Crop <- paste("soybean/", ListMaturityGroup[iMatGroup], sep = "")
            }

            if (grepl("0_0", ListClimateModels[iClimModel]) == TRUE) {
              period <- "0"
              sce <- "0_0"
            } else {
              period <- "2"
              if (grepl("GFDL", ListClimateModels[iClimModel]) == TRUE) sce <- "GFDL-CM3_85"
              if (grepl("GISS", ListClimateModels[iClimModel]) == TRUE) sce <- "GISS-E2-R_85"
              if (grepl("Had", ListClimateModels[iClimModel]) == TRUE) sce <- "HadGEM2-ES_85"
              if (grepl("MIR", ListClimateModels[iClimModel]) == TRUE) sce <- "MIROC5_85"
              if (grepl("MPI", ListClimateModels[iClimModel]) == TRUE) sce <- "MPI-ESM-MR_85"
            }

            if (iListProductionCase == 1) {
              ProductionCase <- "Unlimited water"
              TrNo <- "T2"
            } else {
              ProductionCase <- "Actual"
              TrNo <- "T1"
            }

            Year <- 1981 + (iUSM - 1)
            Yield <- round(as.numeric(Rapport[(6 + lineref), 12]), 3) # need to be in kg/ha
            MaxLAI <- round(as.numeric(Rapport[(6 + lineref), 13]), 3)
            SowDOY <- as.numeric(Rapport[(1 + lineref), 14])
            EmergDOY <- as.numeric(Rapport[(2 + lineref), 15])
            AntDOY <- as.numeric(Rapport[(3 + lineref), 16])
            MatDOY <- as.numeric(Rapport[(5 + lineref), 17])
            HarvDOY <- as.numeric(Rapport[(6 + lineref), 18])
            sum_ET <- round(as.numeric(Rapport[(6 + lineref), 19]), 2)
            AWC_30_sow <- round(as.numeric(Rapport[(1 + lineref), 20]), 2)
            AWC_60_sow <- round(as.numeric(Rapport[(1 + lineref), 21]), 2)
            AWC_90_sow <- round(as.numeric(Rapport[(1 + lineref), 22]), 2)
            AWC_30_harv <- round(as.numeric(Rapport[(6 + lineref), 20]), 2)
            AWC_60_harv <- round(as.numeric(Rapport[(6 + lineref), 21]), 2)
            AWC_90_harv <- round(as.numeric(Rapport[(6 + lineref), 22]), 2)
            tradef <-
            round(as.numeric(Rapport[(6 + lineref), 25]) / (as.numeric(Rapport[(6 +
                                                                                   lineref), 26]) - as.numeric(Rapport[(6 + lineref), 27])), 2)
            sum_irri <- round(as.numeric(Rapport[(6 + lineref), 23]), 2) #Remark: totirr doesn't include precipitation (despite STICS documentation)
            sum_Nmin <- round(as.numeric(Rapport[(6 + lineref), 24]), 2)
            #variables for statistical model
            ep_flomat <-
            as.numeric(Rapport[(5 + lineref), 25]) - as.numeric(Rapport[(3 + lineref), 25])
            ep_drpmat <-
            as.numeric(Rapport[(5 + lineref), 25]) - as.numeric(Rapport[(4 + lineref), 25])
            ep_levmat <-
            as.numeric(Rapport[(5 + lineref), 25]) - as.numeric(Rapport[(2 + lineref), 25])
            masec_imat <- as.numeric(Rapport[(5 + lineref), 29])
            masec_idrp <- as.numeric(Rapport[(4 + lineref), 29])
            masec_iflo <- as.numeric(Rapport[(3 + lineref), 29])
            Qnplante_idrp <- as.numeric(Rapport[(4 + lineref), 30])
            Qnplante_imat <- as.numeric(Rapport[(4 + lineref), 30])
            lai.n_idrp <- as.numeric(Rapport[(5 + lineref), 30])
            raint_drpmat <-
            as.numeric(Rapport[(5 + lineref), 31]) - as.numeric(Rapport[(4 + lineref), 31])
            zrac_imat <- as.numeric(Rapport[(5 + lineref), 32])
            zrac_idrp <- as.numeric(Rapport[(4 + lineref), 32])
            Qfix_imat <- as.numeric(Rapport[(4 + lineref), 32])
            lai.n._idrp <- as.numeric(Rapport[(4 + lineref), 13])
            lai.n._imat <- as.numeric(Rapport[(5 + lineref), 13])
            idrps <- as.numeric(Rapport[(4 + lineref), 34])

            #Read report file: mod_rapport.sti. The file has 4 lines corresponding to the phenological stage chosen: plt drp mat rec
            Rapports <-
            read.csv(
              paste(
                "/home/raynalh/scratch",
                "/",
                NomDir,
                paste("mod_s", NomDir, ".sti", sep = ""),
                sep = "/"
              ),
              sep = ";",
              header = TRUE
            )
            #Calculate variables between physiological stages IDRP and MAT
            flagYield <- Rapports[(Rapports$jul > idrps & Rapports$jul < MatDOY),]
            if (nrow(flagYield) > 0) {
              #tmax_drpmat_moy <- round(mean(flagYield$tmax.n.),2)
              tmax_drpmat_moy <- round(mean(flagYield[, 5]), 2)
              tmax_drpmat_max <- round(max(flagYield[, 5]), 2)
              tmoy_drpmat_moy <- round(mean(flagYield[, 6]), 2)
              tmoy_drpmat_moy <- round(mean(flagYield[, 6]), 2)
              flagYieldB <- subset(flagYield, flagYield[, 7] > 0.6)
              swfac_drpmat_06 <- round(sum(flagYieldB[, 7]), 2)
              flagYieldC <- subset(flagYield, flagYield[, 5] > 28)
              tmax_drpmat_28 <- round(sum(flagYieldC[, 5]), 2)
              flagY <- 0
            } else {
              tmax_drpmat_moy <- 0
              tmax_drpmat_max <- 0
              tmoy_drpmat_moy <- 0
              swfac_drpmat_06 <- 0
              tmax_drpmat_28 <- 0
              flagY <- 1 # to put yield to zero
            }
            #Calculate variables between physiological stages FLO and IDRP 
            flagYield3 <- Rapports[(Rapports$jul > AntDOY & Rapports$jul < idrps),]
            if (nrow(flagYield3) > 0) {
              swfac_flodrp_moy <- round(mean(flagYield3[, 7]), 2)
            } else {
              swfac_flodrp_moy <- 0
            }
            #Calculate variables between physiological stages FLO and MAT 
            flagYield2 <- Rapports[(Rapports$jul > AntDOY & Rapports$jul < MatDOY),]
            if (nrow(flagYield2) > 0) {
              inn_flomat_min <- round(min(flagYield2[, 8]), 2)
            } else {
              inn_flomat_min <- 0
            }
            #Calculate variables between physiological stages Emerg and MAT 
            flagYield4 <- Rapports[(Rapports$jul > EmergDOY & Rapports$jul < MatDOY),]
            if (nrow(flagYield4) > 0) {
              precip_levmat <- round(sum(flagYield4[, 9]), 2)
            } else {
              precip_levmat <- 0
            }


            RapportLGi <-
            data.frame(
              Model,
              soil_ref,
              first_crop,
              Crop,
              period,
              sce,
              CO2,
              TrNo,
              ProductionCase,
              Year,
              Yield,
              MaxLAI,
              SowDOY,
              EmergDOY,
              AntDOY,
              MatDOY,
              HarvDOY,
              sum_ET,
              AWC_30_sow,
              AWC_60_sow,
              AWC_90_sow,
              AWC_30_harv,
              AWC_60_harv,
              AWC_90_harv,
              tradef,
              sum_irri,
              sum_Nmin,
              ep_flomat,
              ep_drpmat,
              ep_levmat,
              masec_imat,
              masec_idrp,
              Qnplante_idrp,
              Qnplante_imat,
              raint_drpmat,
              zrac_imat,
              zrac_idrp,
              Qfix_imat,
              lai.n._idrp,
              lai.n._imat,
              idrps,
             tmax_drpmat_moy,
             tmax_drpmat_max,
             tmoy_drpmat_moy,
             swfac_drpmat_06,
             tmax_drpmat_28,
             swfac_flodrp_moy,
             inn_flomat_min,
             precip_levmat
            )

            if (is.element(
            Year,
            c(
              1982,
              1984,
              1986,
              1988,
              1990,
              1992,
              1994,
              1996,
              1998,
              2000,
              2002,
              2004,
              2006,
              2008,
              2010
            )
          )) {
              SVM14Predictions <- Yield * 1000 # stics yield for maize is correct need to be in kg / ha  
              LM6Predictions <- Yield * 1000 #stics yield for maize is correct
            } else {
              ### Getting mafruit predictions from support vector machine model #####
              SVM14Predictions <- 0
              #    round(predict(SVM_14_model, RapportLGi), 3)
              ### Getting mafruit predictions from linear regression model #####
              #LM6Predictions <- 
              #round(predict(LM_6_model, RapportLGi), 3)
              if (as.numeric(Yield) > 0) {
                # si le rdt calculé par STICS est nul, je dis que le rdt sera de toute façon nul
                LM6Predictions <- round(
              0.1479784 * RapportLGi$lai.n._imat +
              0.0059443 * RapportLGi$raint_drpmat -
              0.0038023 * RapportLGi$ep_flomat +
              0.0036927 * RapportLGi$Qfix_imat +
              0.0017713 * RapportLGi$precip_levmat +
              0.1414605 * RapportLGi$masec_idrp
              , 3) * 1000 # yield results have to be in kg/ha
              } else {
                LM6Predictions <- 0
              }
            }


            RapportLGline <- cbind(
              Model,
              soil_ref,
              first_crop,
              Crop,
              period,
              sce,
              CO2,
              TrNo,
              ProductionCase,
              Year,
              LM6Predictions,
              MaxLAI,
              SowDOY,
              EmergDOY,
              AntDOY,
              MatDOY,
              HarvDOY,
              sum_ET,
              AWC_30_sow,
              AWC_60_sow,
              AWC_90_sow,
              AWC_30_harv,
              AWC_60_harv,
              AWC_90_harv,
              tradef,
              sum_irri,
              sum_Nmin
            # ,
            # ep_flomat,
            # ep_drpmat,
            # ep_levmat,
            # masec_imat,
            # masec_idrp,
            # Qnplante_idrp ,
            # Qnplante_imat ,
            # raint_drpmat ,
            # zrac_imat,
            # zrac_idrp ,
            # Qfix_imat ,
            # lai.n._idrp,
            # lai.n._imat  ,
            # idrps ,
            # tmax_drpmat_moy,
            #tmax_drpmat_moy,
            #tmax_drpmat_max, 
            #tmoy_drpmat_moy, 
            #swfac_drpmat_06,
            #tmax_drpmat_28,
            #swfac_flodrp_moy,
            #precip_levmat,
            #, Yield
             )



            #RapportLG <-
            #  rbind(RapportLG, cbind(RapportLGi, SVM14Predictions, LM6Predictions))

            FileLGline <- paste("/home/raynalh/scratch/outputsLG/EU_SOY_ST_", soil_ref, ".csv", sep = "")
            #RapportLGline <- cbind( iClimModel, iMatGroup, iListProductionCase, RapportLGi)
            #colnames(RapportLGline) <- c( "iClimModel", "iMatGroup", "iListProductionCase",
            colnames(RapportLGline) <- c(
               "Model",
               "soil_ref",
               "first_crop",
               "Crop",
               "period",
               "sce",
               "CO2",
               "TrNo",
               "ProductionCase",
               "Year",
               "Yield",
              "MaxLAI",
              "SowDOY",
              "EmergDOY",
              "AntDOY",
              "MatDOY",
              "HarvDOY",
              "sum_ET",
              "AWC_30_sow",
              "AWC_60_sow",
              "AWC_90_sow",
              "AWC_30_harv",
              "AWC_60_harv",
              "AWC_90_harv",
              "tradef",
              "sum_irri",
              "sum_Nmin"
            #,
            #"ep_flomat",
            #"ep_drpmat",
            #"ep_levmat",
            #"masec_imat",
            #"masec_idrp",
            #"Qnplante_idrp",
            #"Qnplante_imat",
            #"raint_drpmat",
            #"zrac_imat",
            #"zrac_idrp",
            #"Qfix_imat",
            #"lai.n._idrp",
            #"lai.n._imat",
            #"idrps",
            #"tmax_drpmat_moy",
            #"tmax_drpmat_moy",
            #"tmax_drpmat_max", 
            #"tmoy_drpmat_moy", 
            #"swfac_drpmat_06",
            #"tmax_drpmat_28",
            #"swfac_flodrp_moy",
            #"precip_levmat",
            # ,"SticsYield"
            )

            if (linerefpred == 0) {
              # 1ere anne de simulation, on mettra les titres des colonnes
              write.table(
    RapportLGline,
              #file = paste(workspace_path, FileLG, sep = ""),
    file = FileLGline,
    append = TRUE,
    row.names = F,
    col.names = T,
    sep = ","
  )
            } else {
              write.table(
    RapportLGline,
              #file = paste(workspace_path, FileLG, sep = ""),
    file = FileLGline,
    append = TRUE,
    row.names = F,
    col.names = F,
    sep = "," # sep needs to be , in Legume Gap protocol
  )
            }

            linerefpred <- linerefpred + 6

          } else {


            # echec simulation
            Model <- "ST"
            soil_ref <- ind
            first_crop <- "soybean"
            if (is.element(
              indyear,
              c(
                1982,
                1984,
                1986,
                1988,
                1990,
                1992,
                1994,
                1996,
                1998,
                2000,
                2002,
                2004,
                2006,
                2008,
                2010
              ))) {
              Crop <- "maize"
            } else {
              Crop <- paste("soybean/", ListMaturityGroup[iMatGroup], sep = "")
            }

            if (grepl("00", ListClimateModels[iClimModel]) == TRUE) {
              period <- "0"
              sce <- "0_0"
            } else {
              period <- "2"
              if (grepl("GFDL", ListClimateModels[iClimModel]) == TRUE) sce <- "GFDL-CM3_85"
              if (grepl("GISS", ListClimateModels[iClimModel]) == TRUE) sce <- "GISS-E2-R_85"
              if (grepl("Had", ListClimateModels[iClimModel]) == TRUE) sce <- "HadGEM2-ES_85"
              if (grepl("MIR", ListClimateModels[iClimModel]) == TRUE) sce <- "MIROC5_85"
              if (grepl("MPI", ListClimateModels[iClimModel]) == TRUE) sce <- "MPI-ESM-MR_85"

            }

            if (iListProductionCase == 1) {
              ProductionCase <- "Unlimited water"
              TrNo <- "T2"
            } else {
              ProductionCase <- "Actual"
              TrNo <- "T1"
            }
            Year <- 1981 + (iUSM - 1)
            Yield <- "na"
            MaxLAI <- "na"
            SowDOY <- SowingDatesCrop
            EmergDOY <- "na"
            AntDOY <- "na"
            MatDOY <- "na"
            HarvDOY <- "na"
            sum_ET <- "na"
            AWC_30_sow <- "na"
            AWC_60_sow <- "na"
            AWC_90_sow <- "na"
            AWC_30_harv <- "na"
            AWC_60_harv <- "na"
            AWC_90_harv <- "na"
            tradef <- "na"
            sum_irri <- "na"
            sum_Nmin <- "na"
            ep_flomat <- "na"
            ep_drpmat <- "na"
            ep_levmat <- "na"
            masec_imat <- "na"
            masec_idrp <- "na"
            Qnplante_idrp <- "na"
            Qnplante_imat <- "na"
            raint_drpmat <- "na"
            zrac_imat <- "na"
            zrac_idrp <- "na"
            Qfix_imat <- "na"
            lai.n._idrp <- "na"
            lai.n._imat <- "na"
            idrps <- "na"
            tmax_drpmat_moy <- "na"
            tmax_drpmat_moy <- "na"
            tmax_drpmat_max <- "na"
            tmoy_drpmat_moy <- "na"
            swfac_drpmat_06 <- "na"
            tmax_drpmat_28 <- "na"
            swfac_flodrp_moy <- "na"
            precip_levmat <- "na"
            #SticsYield <- "na"


            RapportLGline <-
            data.frame(
              Model,
              soil_ref,
              first_crop,
              Crop,
              period,
              sce,
              CO2,
              TrNo,
              ProductionCase,
              Year,
              Yield,
              MaxLAI,
              SowDOY,
              EmergDOY,
              AntDOY,
              MatDOY,
              HarvDOY,
              sum_ET,
              AWC_30_sow,
              AWC_60_sow,
              AWC_90_sow,
              AWC_30_harv,
              AWC_60_harv,
              AWC_90_harv,
              tradef,
              sum_irri,
              sum_Nmin
            # ,
            #ep_flomat,
            #ep_drpmat,
            #ep_levmat,
            #masec_imat,
            #masec_idrp,
            #Qnplante_idrp ,
            #Qnplante_imat ,
            #raint_drpmat ,
            #zrac_imat,
            #zrac_idrp ,
            #Qfix_imat ,
            #lai.n._idrp,
            #lai.n._imat  ,
            #idrps ,
            #tmax_drpmat_moy,
            #tmax_drpmat_moy,
            #tmax_drpmat_max, 
            #tmoy_drpmat_moy, 
            #swfac_drpmat_06,
            #tmax_drpmat_28,
            #swfac_flodrp_moy,
            #precip_levmat,
            # SticsYield
            )


            FileLGline <- paste("/home/raynalh/scratch/outputsLG/EU_SOY_ST_", soil_ref, ".csv", sep = "")
            #RapportLGline <- cbind( iClimModel, iMatGroup, iListProductionCase, RapportLGi)
            #RapportLGline <- cbind(  RapportLGi)
            #colnames(RapportLGline) <- c( "iClimModel", "iMatGroup", "iListProductionCase",
            colnames(RapportLGline) <- c(
               "Model",
               "soil_ref",
               "first_crop",
               "Crop",
               "period",
               "sce",
               "CO2",
               "TrNo",
               "ProductionCase",
               "Year",
               "Yield",
              "MaxLAI",
              "SowDOY",
              "EmergDOY",
              "AntDOY",
              "MatDOY",
              "HarvDOY",
              "sum_ET",
              "AWC_30_sow",
              "AWC_60_sow",
              "AWC_90_sow",
              "AWC_30_harv",
              "AWC_60_harv",
              "AWC_90_harv",
              "tradef",
              "sum_irri",
              "sum_Nmin"
            #,
            #"ep_flomat",
            #"ep_drpmat",
            #"ep_levmat",
            #"masec_imat",
            #"masec_idrp",
            #"Qnplante_idrp",
            #"Qnplante_imat",
            #"raint_drpmat",
            #"zrac_imat",
            #"zrac_idrp",
            #"Qfix_imat",
            #"lai.n._idrp",
            #"lai.n._imat",
            #"idrps",
            #"tmax_drpmat_moy",
            #"tmax_drpmat_moy",
            #"tmax_drpmat_max", 
            #"tmoy_drpmat_moy", 
            #"swfac_drpmat_06",
            #"tmax_drpmat_28",
            #"swfac_flodrp_moy",
            #"precip_levmat",
            # , "SticsYield"
             )
            write.table(
    RapportLGline,
    file = FileLGline,
    append = TRUE,
    row.names = F,
    col.names = F,
    sep = ","
            )
            #file = paste(workspace_path, FileLG, sep = ""),



          }
          # end echec sim          
        }
        #end for loop
        #linerefpred <- lineref + 6


      }
      #end loop 3 over Management

    }
    #end loop 2 over MaturityGRoup

  }
  #end loop 1 over climate model
  # ===============================

  #delete working directory
  unlink(paste("/home/raynalh/scratch", NomDir, sep = "/"), recursive = TRUE)

}
#end of function Buil30USM


#######################################
# Parallelization of simulations
#######################################
# library(doParallel)
# library(foreach)
# cl <- makeForkCluster(40)
# doParallel::registerDoParallel(cl)
# lot <-1 #num of lot of points to simulate. Must be compliant to soilfile (.xml to use)
# indSoil <- 1
# resultat <-
#   foreach (
#     indSoil = 10:50000,
#     .combine = rbind ,
#     .packages = c("readxl", "SticsOnR", "SticsRFiles")
#   ) %dopar% {
#     lot <-   ceiling(indSoil/1000)
#     resu <- build30USM(indSoil,lot)
#   }

# parallel::stopCluster(cl)

# Possible to test on one point
# indSoil <- 450
# lot <- 1
build30USM(argSoilRefIndex, lot)

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

# due to some issues with parallel runs of this Stics version, a scheduler was written to prevent overlapping of the a startup phase (stics_scheduler.go)
# use go build to compile
# Warning: Hardcoded paths, may need adaptation

# the R scripts can also be used without parallelization or scheduler (slow)


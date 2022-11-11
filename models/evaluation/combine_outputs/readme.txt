
# combine resultes of 4 different models
# Monica, Apsim, Hermes2Go, and Stics
# combine outputs will create ascii maps
# these maps can be turned into pictures 

# requires go 1.18 or higher to be installed in path
go build

# combine outputs takes as -source(1-4) a path to the output of a model
# the -cut(1-4) parameter is last day of the last month in the simulation 
#(if simulation runs longer, days after are cut off)
# the -harvest4 determines the last day of the harvest period for Stics 
# -project is the path to big project files (e.g. mapping files and masks)
# -climate is the path to the climate files
# -out is the path to the output folder

./combine_outputs \
-path Cluster \
-source1 /path/to/monica/results \
-source2 /path/to/apsim/results \
-source3 /path/to/hermes/results\
-source4 /path/to/stics/results \
-harvest4 30 \
-cut1 15 \
-cut2 15 \
-cut3 15 \
-cut4 15 \
-project /path/to/projects_data \
-climate /path/to/macsur_european_climate_scenarios_v3/ \
-out ./out_path
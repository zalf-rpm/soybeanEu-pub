compile this folder with go build
convert climate files to apsim met file format 

Example: 

./metConversion \
-source /path/to/climate-data/2/GFDL-CM3_85 \
-output /path/to/climate-data/soybeanEU/met/2/GFDL-CM3_85 \
-project /path/to/resources \
-co2 571 &
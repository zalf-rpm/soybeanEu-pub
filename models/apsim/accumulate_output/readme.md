
Transform apsim output files into a standard format.
Compile with go build


Example:

./accumulate_output \
-in path/to/soybeanEU/out_2_GFDL-CM3_45 \
-out path/to/soybeanEU/out_transformed \
-base ./base.csv \
-period 2 \
-sce GFDL-CM3_45 \
-co2 499 \
-concurrent 40 
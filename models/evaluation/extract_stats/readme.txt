
# Following files generated in combine_outputs
# need to be copied to eval directoy here
# 
# "./extract_stats/eval/dev_max_yield_future_trnoT1.asc.gz"
# "./extract_stats/eval/dev_max_yield_future_trnoT2.asc.gz"
# "./extract_stats/eval/dev_max_yield_historical_trnoT1.asc.gz"
# "./extract_stats/eval/dev_max_yield_historical_trnoT2.asc.gz"
# "./extract_stats/eval/irrgated_areas.asc.gz"
# "./extract_stats/eval/crop_land_mask_historical.asc.gz"
# "./extract_stats/eval/dev_allRisks_5_historical.asc.gz"
# "./extract_stats/eval/dev_allRisks_5_future.asc.gz"
# "./extract_stats/eval/all_historical_stdDev.asc.gz"
# "./extract_stats/eval/all_future_stdDev.asc.gz"
# "./extract_stats/eval/avg_over_models_stdDev.asc.gz"
# "./extract_stats/eval/avg_over_climScen_stdDev.asc.gz"
# "./extract_stats/eval/dev_sowing_dif_historical_future.asc.gz"
# "./extract_stats/eval/dev_compare_mg_yield_future_trnoT1.asc.gz"
# "./extract_stats/eval/dev_compare_mg_yield_future_trnoT2.asc.gz"


python .\extract_stats\extract_stats.py folder=eval > mystats.txt
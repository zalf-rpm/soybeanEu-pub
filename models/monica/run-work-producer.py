#!/usr/bin/python
# -*- coding: UTF-8

# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/. */

# Authors:
# Michael Berg-Mohnicke
# Tommaso Stella 
# Susanne Schulz
#
# Maintainers:
# Currently maintained by the authors.
#
# This file has been created at the Institute of
# Landscape Systems Analysis at the ZALF.
# Copyright (C: Leibniz Centre for Agricultural Landscape Research (ZALF)

import time
import os
import json
import csv
from copy import deepcopy
from datetime import date, timedelta
import sys
import errno
import zmq
import monica_io3

#USER_MODE = "localProducer-localMonica"
#USER_MODE = "remoteProducer-remoteMonica"
USER_MODE = "localProducer-remoteMonica"

PATHS = {
    # adjust the local path to your environment
    "localProducer-localMonica": {
        "monica-parameters-path": "C:/Users/berg.ZALF-AD/GitHub/monica-parameters/", # path to monica-parameters
        #"monica-parameters-path": "C:/Users/stella/Documents/GitHub/monica-parameters/", # path to monica-parameters
        "monica-path-to-climate-dir": "A:/projects/macsur-eu-heat-stress-assessment/climate-data/corrected2/", # mounted path to archive accessable by monica executable
    },
    "localProducer-remoteMonica": {
        "monica-project-data": ".",
        "monica-parameters-path": "D:/zalfrpm/monica-parameters/", # path to monica-parameters
        #"monica-parameters-path": "C:/Users/stella/Documents/GitHub/monica-parameters/", # path to monica-parameters
        "monica-path-to-climate-dir": "/monica_data/climate-data/macsur_european_climate_scenarios_v3/testing/corrected2/", # mounted path to archive accessable by monica executable
    },
    "remoteProducer-remoteMonica": {
        "monica-project-data": "/project/soybeanEU/",
        "monica-parameters-path": "/monica-parameters/", # path to monica-parameters
        "monica-path-to-climate-dir": "/monica_data/climate-data/macsur_european_climate_scenarios_v3/testing/corrected2/" # mounted path to archive accessable by monica executable
    }
}
# local testing: python .\run-work-producer.py server-port=6004 mode=localProducer-remoteMonica > out_producer.txt

server = {
    "localProducer-localMonica": "localhost",
    "localProducer-remoteMonica": "login01.cluster.zalf.de",
    "remoteProducer-remoteMonica": "login01.cluster.zalf.de"
}

CONFIGURATION = {
    "mode": "localProducer-localMonica",
    "server": None,
    "server-port": "6666",
    "start-row": 1, 
    "end-row": -1,
    "run-periods": "[0,2]"
}

script_path = os.path.dirname(os.path.abspath(__file__))

def run_producer(config):
    "main"

    if not config["server"]:
        config["server"] = server[config["mode"]]

    def rotate(crop_rotation):
        "rotate the crops in the rotation"
        crop_rotation.insert(0, crop_rotation.pop())

    print("config:", config)

    context = zmq.Context()
    socket = context.socket(zmq.PUSH) # pylint: disable=no-member
    
    # select paths 
    paths = PATHS[config["mode"]]
    
    # connect to monica proxy (if local, it will try to connect to a locally started monica)
    socket.connect("tcp://" + config["server"] + ":" + str(config["server-port"]))
    
    template_folder = script_path + "/json_templates/"
    with open(template_folder + "sim.json") as _:
        sim = json.load(_)
        sim["include-file-base-path"] = paths["monica-parameters-path"]
        if USER_MODE == "localProducer-localMonica":
            sim["climate.csv-options"]["no-of-climate-file-header-lines"] = 1
        elif USER_MODE == "localProducer-remoteMonica":
            sim["climate.csv-options"]["no-of-climate-file-header-lines"] = 2
        elif USER_MODE == "remoteProducer-remoteMonica":
            sim["climate.csv-options"]["no-of-climate-file-header-lines"] = 2

    with open(template_folder + "site.json") as _:
        site = json.load(_)

    with open(template_folder +"crop.json") as _:
        crop = json.load(_)

    with open(template_folder + "sims.json") as _:
        sims = json.load(_)

    with open(template_folder +"irrigations.json") as _:
        irrigation = json.load(_)

    period_gcm_co2s = [
        {"id": "C1", "period": "0", "gcm": "0_0", "co2_value": 360},
        #{"id": "C26", "period": "2", "gcm": "GFDL-CM3_45", "co2_value": 499},
        {"id": "C28", "period": "2", "gcm": "GFDL-CM3_85", "co2_value": 571},
        #{"id": "C30", "period": "2", "gcm": "GISS-E2-R_45", "co2_value": 499},
        {"id": "C32", "period": "2", "gcm": "GISS-E2-R_85", "co2_value": 571},
        #{"id": "C36", "period": "2", "gcm": "HadGEM2-ES_45", "co2_value": 499},
        {"id": "C38", "period": "2", "gcm": "HadGEM2-ES_85", "co2_value": 571},
        #{"id": "C40", "period": "2", "gcm": "MIROC5_45", "co2_value": 499},
        {"id": "C42", "period": "2", "gcm": "MIROC5_85", "co2_value": 571},
        #{"id": "C46", "period": "2", "gcm": "MPI-ESM-MR_45", "co2_value": 499},
        {"id": "C48", "period": "2", "gcm": "MPI-ESM-MR_85", "co2_value": 571}
    ]

    soil = {}
    row_cols = []
    with open(os.path.join(paths["monica-project-data"], "stu_eu_layer_ref.csv")) as _:
        reader = csv.reader(_)
        next(reader)
        for row in reader:
            #Column_,Row,Grid_Code,Location,CLocation,elevation,latitude,longitude,depth,OC_topsoil,OC_subsoil,BD_topsoil,BD_subsoil,Sand_topsoil,Clay_topsoil,Silt_topsoil,Sand_subsoil,Clay_subsoil,Silt_subsoil
            #soil_ref,CLocation,latitude,depth,OC_topsoil,OC_subsoil,BD_topsoil,BD_subsoil,Sand_topsoil,Clay_topsoil,Silt_topsoil,Sand_subsoil,Clay_subsoil,Silt_subsoil

            soil_ref = row[0]
            #row_col = (int(row[1]), int(row[0]))
            row_cols.append(soil_ref)
            soil[soil_ref] = {
                "climate_location": row[1],
                "latitude": float(row[2]),
                "depth": float(row[3]),
                "oc-topsoil": float(row[4]),
                "oc-subsoil": float(row[5]),
                "bd-topsoil": float(row[6]),
                "bd-subsoil": float(row[7]),
                "sand-topsoil": float(row[8]),
                "clay-topsoil": float(row[9]),
                "silt-topsoil": float(row[10]),
                "sand-subsoil": float(row[11]),
                "clay-subsoil": float(row[12]),
                "silt-subsoil": float(row[13]),
            }
    def get_custom_site(soil_ref):
        "update function"
        cell_soil = soil[soil_ref]
        
        # pwp = cell_soil["pwp"]
        # fc_ = cell_soil["fc"]
        # sm_percent_fc = cell_soil["sw-init"] / fc_ * 100.0
        
        top = {
            "Thickness": [0.3, "m"],
            "SoilOrganicCarbon": [cell_soil["oc-topsoil"], "%"],
            "SoilBulkDensity": [cell_soil["bd-topsoil"] , "kg m-3"],
            #"FieldCapacity": [fc_, "m3 m-3"],
            #"PermanentWiltingPoint": [pwp, "m3 m-3"],
            #"PoreVolume": [cell_soil["sat"], "m3 m-3"],
            #"SoilMoisturePercentFC": [sm_percent_fc, "% [0-100]"],
            "Sand": cell_soil["sand-topsoil"] / 100.0,
            "Clay": cell_soil["clay-topsoil"] / 100.0,
            "Silt": cell_soil["silt-topsoil"] / 100.0
            }
        sub = {
            "Thickness": [1.7, "m"],
            "SoilOrganicCarbon": [cell_soil["oc-subsoil"], "%"],
            "SoilBulkDensity": [cell_soil["bd-subsoil"] , "kg m-3"],
            #"FieldCapacity": [fc_, "m3 m-3"],
            #"PermanentWiltingPoint": [pwp, "m3 m-3"],
            #"PoreVolume": [cell_soil["sat"], "m3 m-3"],
            #"SoilMoisturePercentFC": [sm_percent_fc, "% [0-100]"],
            "Sand": cell_soil["sand-subsoil"] / 100.0,
            "Clay": cell_soil["clay-subsoil"] / 100.0,
            "Silt": cell_soil["silt-subsoil"] / 100.0
        }

        custom_site = {
            "soil-profile": [top, sub],
            "latitude": cell_soil["latitude"],
            #"sw-init": cell_soil["sw-init"],
        }
        
        return custom_site


    #assert len(row_cols) == len(pheno["GM"].keys()) == len(pheno["WW"].keys())
    print("# of rowsCols = ", len(row_cols))

    i = 0
    start_store = time.process_time()
    start = config["start-row"] - 1
    end = config["end-row"] - 1
    if end < 0 :
        row_cols_ = row_cols[start:]
    else :
        row_cols_ = row_cols[start:end+1]
    #row_cols_ = [(108,106), (89,82), (71,89), (58,57), (77,109), (66,117), (46,151), (101,139), (116,78), (144,123)]
    #row_cols_ = ["64500"]
    #row_cols_ = [(3015,2836)] # 2836,3015
    print("running from ", start, "/", row_cols[start], " to ", end, "/", row_cols[end])
    run_periods = list(map(str, json.loads(config["run-periods"])))

    for soil_ref in row_cols_:

        custom_site = get_custom_site(soil_ref)

        site["SiteParameters"]["Latitude"] = custom_site["latitude"]
        site["SiteParameters"]["SoilProfileParameters"] = custom_site["soil-profile"]

        for crop_id in crop["soybean"].keys():
            #if crop_id not in ["0000", "II"]:
            #    continue
            for ws in crop["cropRotation"][0]["worksteps"]:
                if ws["type"] == "AutomaticSowing":
                    #set crop ref
                    ws["crop"][2] = crop_id
                #if ws["type"] == "SetValue":
                #    #set mois
                #    ws["value"] = custom_site["sw-init"]
            
            #force max rooting depth
            #site["SiteParameters"]["ImpenetrableLayerDepth"] = crop["crops"][crop_id]["cropParams"]["cultivar"]["CropSpecificMaxRootingDepth"]

            env = monica_io3.create_env_json_from_json_config({
                "crop": crop,
                "site": site,
                "sim": sim,
                "climate": ""
            })
            
            env["csvViaHeaderOptions"] = sim["climate.csv-options"]

            for pgc in period_gcm_co2s:
                co2_id = pgc["id"]
                co2_value = pgc["co2_value"]
                period = pgc["period"]
                gcm = pgc["gcm"]

                if period not in run_periods:
                    continue

                env["params"]["userEnvironmentParameters"]["AtmosphericCO2"] = co2_value
                
                if USER_MODE == "localProducer-localMonica":
                    climatefile_version = "v1"
                elif USER_MODE == "localProducer-remoteMonica":
                    climatefile_version = "v3test"
                elif USER_MODE == "remoteProducer-remoteMonica":
                    climatefile_version = "v3test"
                climateLocation = soil[soil_ref]["climate_location"]
                climate_filename = "{}_{}.csv".format(climateLocation, climatefile_version)
                #if not os.path.exists(path_to_climate_file):
                #    continue

                #read climate data on the server and send just the path to the climate data csv file
                env["pathToClimateCSV"] = paths["monica-path-to-climate-dir"] + period + "/" + gcm + "/" + climate_filename

                env["events"] = sims["output"]

                for sim_ in sims["treatments"]:
                    env["params"]["simulationParameters"]["UseAutomaticIrrigation"] = False
                    env["params"]["simulationParameters"]["WaterDeficitResponseOn"] = sim_["WaterDeficitResponseOn"]
                    env["params"]["simulationParameters"]["FrostKillOn"] = sim_["FrostKillOn"]

                    n_steps = len(env["cropRotation"][0]["worksteps"])
                    if sim_["Irrigate"]:
                        if n_steps ==2:
                            #add irrigation
                            for irri in irrigation["irristeps"]:
                                env["cropRotation"][0]["worksteps"].append(irri)
                                env["cropRotation"][1]["worksteps"].append(irri)
                    if not sim_["Irrigate"]:
                        if n_steps == 35:
                            #remove irrigation
                            env["cropRotation"][0]["worksteps"] = [env["cropRotation"][0]["worksteps"][0], env["cropRotation"][0]["worksteps"][1]]
                            env["cropRotation"][1]["worksteps"] = [env["cropRotation"][1]["worksteps"][0], env["cropRotation"][1]["worksteps"][1]]
                    

                    for _ in range(len(env["cropRotation"])):
                        rotate(env["cropRotation"])
                        try:
                            first_cp = env["cropRotation"][0]["worksteps"][0]["crop"]["cropParams"]["species"]["="]["SpeciesName"]
                        except:
                            first_cp = env["cropRotation"][0]["worksteps"][0]["crop"]["cropParams"]["species"]["SpeciesName"]
                        
                        env["customId"] = {
                            #"row": row, "col": col,
                            "soil_ref": soil_ref,
                            "period": period,
                            "gcm": gcm,
                            "co2_id": co2_id, "co2_value": co2_value,
                            "trt_no": sim_["TrtNo"],
                            "prod_case": sim_["ProdCase"],
                            "crop_id": crop_id,
                            "first_cp": first_cp
                        }

                         
                        print("sent env ", i, " customId: ", list(env["customId"].values()))
                        #filename = "./V" + str(i) + "_" + soil_ref +"_"+ env["customId"]["trt_no"] +"_"+ env["customId"]["gcm"] +"_"+ env["customId"]["crop_id"] +".json"
                        #WriteEnv(filename, env) 
                        socket.send_json(env)                        
                        i += 1
        #exit()

    stop_store = time.process_time()

    print("sending ", i, " envs took ", (stop_store - start_store), " seconds")
    print("ran from ", start, "/", row_cols[start], " to ", end, "/", row_cols[end])
    return

def WriteEnv(filename, env) :
    if not os.path.exists(os.path.dirname(filename)):
        try:
            os.makedirs(os.path.dirname(filename))
        except OSError as exc: # Guard against race condition
            if exc.errno != errno.EEXIST:
                raise
    with open(filename, 'w') as outfile:
        json.dump(env, outfile)

if __name__ == "__main__":

    config = deepcopy(CONFIGURATION)

    # read commandline args only if script is invoked directly from commandline
    if len(sys.argv) > 1 and __name__ == "__main__":
        for arg in sys.argv[1:]:
            k, v = arg.split("=")
            if k in config:
                config[k] = v

    run_producer(config)
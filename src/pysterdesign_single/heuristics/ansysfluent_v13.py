from typing import Union, Dict

try:
    import tkinter
except ImportError:
    import Tkinter as tkinter

from tktooltip import ToolTip
"""
A recreation of ./src/performance/fluentperf/ansysfluent_v13.pas

This is not a general purpose algorithem, but a basic example.

Following comment is from the original

=============================
This file is part of "FluentPerf", a simple performance model for ANSYS/Fluent software.

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU Affero General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Affero General Public License for more details.

  You should have received a copy of the GNU Affero General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
=============================
"""

KNOWN_BENCHMARK_TYPES:list = ['truck_111m']
KNOWN_NETWORK_TECH:list = ['InfiniBand', '10Gb Ethernet']
ALLOW_THROUGHPUT_DEFAULT:bool = True

K_CPU:float = 3.75
CORES_PER_MACHINE:int = 12
MAX_CORES_INGINIBAND:int = 3072
MAX_CORES_10GB_ETH:int = 384

def __validation(self, params: Dict) -> bool:
    CHOSEN_NETWORK = ""
    ## BEGIN MANDATORY PARAMETERS CHECK
    if("benchmark" not in params):
        #need to use error pop up window for this, implement later
        #same goes for all the others
        print("Benchmark must be specified: "+ ", ".join(KNOWN_BENCHMARK_TYPES))
        return False
    else:
        if(params["benchmark"] not in KNOWN_BENCHMARK_TYPES):
            print("Invalid benchmark, must be one of the following: "+ ", ".join(KNOWN_BENCHMARK_TYPES))
            return False
    
    if("networkTech" not in params):
        print("Network technology must be specified: "+ ", ".join(KNOWN_NETWORK_TECH))
        return False
    else:
        if(params["networkTech"] not in KNOWN_NETWORK_TECH):
            print("Invalid Network, must be one of the following: "+ ", ".join(KNOWN_NETWORK_TECH))
            return False

    if("cpuFrequency" in params):
        try:
            if float(params["cpuFrequency"]) <= 0.0:
                raise ValueError("Not a positive CPU Freq")
            params["cpuFrequency"] = float(params["cpuFrequency"])
        except ValueError:
            print("CPU frequency must be a positive floating-point value")
            return False
            
    else:
        print("CPU frequency must be specified")
        return False
    
    if("AllowThroughput" not in params):
        params["AllowThroughput"] = ALLOW_THROUGHPUT_DEFAULT
    else:
        allowThroughput: bool = params["AllowThroughput"]
        if(isinstance(allowThroughput, str)):
            allowThroughput = allowThroughput.lower()
            if((allowThroughput == "true") or (allowThroughput == "false")):
                params["AllowThroughput"] = (allowThroughput == "true")
            else:
                print('"AllowThroughput" must be a boolean value')
            return False
        
    #END MANDATORY PARAMETERS CHECK

    #Cores XOR Performance

    coresParam: bool = ("Cores" in params)
    performanceParam: bool = ("Performance" in params)

    if(coresParam ^ performanceParam):
        print('"Cores" and "Performance" are contradictory, one (and only one) must be defined')
        return False
    
    #may need to come back and do DirectModel, Ref lines 130 and 143 in ansysfluent_13.pas
    if(coresParam):
        try:
            params["Cores"] = int(params["Cores"])
            if params["Cores"] < 1:
                raise ValueError()
        except:
            print('"Cores" must be a positive integer')
            return False
    elif(performanceParam):
        try:
            params["Performance"] = float(params["Performance"])
            if params["Performance"] < 0.0:
                raise ValueError()
        except:
            print('"Performance" must be a positive floating-point value')
            return False
    else:
        #How the hell did you get here
        print("Not sure how you got here, this should be litterally impossible")
        raise SystemExit(1)

    return True

def efficiency(self, params: Dict) -> float:
    k:float = 0.0
    b:float = 0.0

    if(params["networkTech"].lower() in '"InfiniBand"'.lower()):
        k = -0.00979
        b = 88.42231
    elif(params["networkTech"].lower() in '"10Gb Ethernet"'.lower()):
        k = -0.08270
        b = 101.9848
    else:
        return None
    
    return (k * int(params["Cores"]) * b) / 100

def performance(self, params: Dict) -> float:
    return float(params["cpuFrequency"]) * K_CPU * (int(params["Cores"]) / CORES_PER_MACHINE) * efficiency(self, params)    

def execute(parameters: Dict) -> float:
    print(parameters)
    if __validation(params=parameters):
        return performance(parameters)
    else:
        print("invalid")
    return None



    

    
    
#    print(parameters)
#    print("heur cost perf execute")
#    return "cost perf execution"


def identity() -> str:
    return "ANSYS FLUENT 13.0.0"

def toolTip(host: tkinter) -> None:
    ToolTip(host, msg="Demo model with linear approximation of efficiency, March 2012", delay=1.0)

from typing import Union

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

KNOWN_BENCHMARK_TYPES = ['"truck_111m"']
KNOWN_NETWORK_TECH = ['"InfiniBand"', '"10Gb Ethernet"']
ALLOW_THROUGHPUT_DEFAULT = True

def __validation(parameters: dict[str: Union[str, float, int, bool]]) -> bool:
    
    ## BEGIN MANDATORY PARAMETERS CHECK
    if("benchmark" not in parameters):
        #need to use error pop up window for this, implement later
        #same goes for all the others
        print("Benchmark must be specified: "+ ", ".join(KNOWN_BENCHMARK_TYPES))
        return False
    
    if("networkTech" not in parameters):
        print("Network technology must be specified: "+ ", ".join(KNOWN_NETWORK_TECH))
        return False

    if("cpuFrequency" in parameters):
        try:
            if float(parameters["cpuFrequency"]) <= 0.0:
                raise ValueError("Not a positive CPU Freq")
            parameters["cpuFrequency"] = float(parameters["cpuFrequency"])
        except ValueError:
            print("CPU frequency must be a positive floating-point value")
            return False
            
    else:
        print("CPU frequency must be specified")
        return False
    
    if("AllowThroughput" not in parameters):
        parameters["AllowThroughput"] = ALLOW_THROUGHPUT_DEFAULT
    else:
        allowThroughput: bool = parameters["AllowThroughput"]
        if(isinstance(allowThroughput, str)):
            allowThroughput = allowThroughput.lower()
            if((allowThroughput == "true") or (allowThroughput == "false")):
                parameters["AllowThroughput"] = (allowThroughput == "true")
            else:
                print('"AllowThroughput" must be a boolean value')
            return False
        
    #END MANDATORY PARAMETERS CHECK

    #Cores XOR Performance

    coresParam: bool = ("Cores" in parameters)
    performanceParam: bool = ("Performance" in parameters)

    if(coresParam ^ performanceParam):
        print('"Cores" and "Performance" are contradictory, one (and only one) must be defined')
        return False
    
    #may need to come back and do DirectModel, Ref lines 130 and 143 in ansysfluent_13.pas
    if(coresParam):
        try:
            parameters["Cores"] = int(parameters["Cores"])
            if parameters["Cores"] < 1:
                raise ValueError()
        except:
            print('"Cores" must be a positive integer')
            return False
    elif(performanceParam):
        try:
            parameters["Performance"] = float(parameters["Performance"])
            if parameters["Performance"] < 0.0:
                raise ValueError()
        except:
            print('"Performance" must be a positive floating-point value')
            return False
    else:
        #How the hell did you get here
        print("Not sure how you got here, this should be litterally impossible")
        SystemExit(1)

    return True

def execute(parameters: dict[str: Union[str, float, int, bool]]) -> object:

        if __validation(parameters):
            print("valid")
        else:
            print("invalid")
        return None


    

    
    
    print(parameters)
    print("heur cost perf execute")
    return "cost perf execution"


def identity() -> str:
    return "ANSYS FLUENT 13.0.0"

def toolTip(host: tkinter) -> None:
    ToolTip(host, msg="Demo model with linear approximation of efficiency, March 2012", delay=1.0)

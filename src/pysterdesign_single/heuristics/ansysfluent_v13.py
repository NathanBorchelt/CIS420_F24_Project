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


def execute(parameters: dict[str: Union[str, float, int, bool]]) -> object:
    if("benchmark" not in parameters):
        #need to use error pop up window for this, implement later
        #same goes for all the others
        print("Benchmark must be specified: "+ ", ".join(KNOWN_BENCHMARK_TYPES))
        return
    
    if("networkTech" not in parameters):
        print("Network technology must be specified: "+ ", ".join(KNOWN_NETWORK_TECH))
        return

    if("cpuFrequency" in parameters):
        try:
            if float(parameters["cpuFrequency"]) <= 0.0:
                raise ValueError("Not a positive CPU Freq")
        except ValueError:
            print("CPU frequency must be a positive floating-point value")
            return
            
    else:
        print("CPU frequency must be specified")
        return
    
    if("AllowThroughput" not in parameters):
        parameters["AllowThroughput"] = ALLOW_THROUGHPUT_DEFAULT
    else:
        allowThroughput = parameters["AllowThroughput"]
        if(isinstance(allowThroughput, str)):
            allowThroughput = allowThroughput.lower()
            if((allowThroughput == "true") or (allowThroughput == "false")):
                parameters["AllowThroughput"] = (allowThroughput == "true")
            else:
                print('"AllowThroughput" must be a positive floating-point value')
            return
        


    

    
    
    print(parameters)
    print("heur cost perf execute")
    return "cost perf execution"


def identity() -> str:
    return "ANSYS FLUENT 13.0.0"

def toolTip(host: tkinter) -> None:
    ToolTip(host, msg="Demo model with linear approximation of efficiency, March 2012", delay=1.0)

{
Copyright (C) 2012 Konstantin S. Solnushkin

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
}
unit AnsysFluent_v13;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils,
  FloatStr, perf_common;        // Our own units

const
  // ALWAYS! SET THE FOLLOWING LINE! if you are modifying somebody else's model:
  cPerformanceModelId = 'Demo model with linear approximation of efficiency, March 2012';
  cSoftwareVersionStr = 'ANSYS FLUENT 13.0.0';   // The model is for THIS particular version of software!
  { Names of benchmarks }
  cTruck_111m = 'truck_111m';

type
{ TAnsysFluentPerformance }

TAnsysFluentPerformance = class (TPerformance)
  constructor Create;
  function KnownBenchmarkTypes: String;
  function KnownNetworkTechs: String;
  procedure LoadParameters (Parameters: TStrings); override;
  procedure CalculatePerformance; override;
end;


implementation

{ TAnsysFluentPerformance }

constructor TAnsysFluentPerformance.Create;
// MODIFY THIS FUNCTION
begin
  // This one must be set properly! Or great confusion may occur in the world.
  PerfModelId := cPerformanceModelId;
  // This performance model is for a particular version of software
  Software := cSoftwareVersionStr;
end;

function TAnsysFluentPerformance.KnownBenchmarkTypes: String;
// MODIFY THIS FUNCTION
begin
  Result := '"' + cTruck_111m + '"';
end;

function TAnsysFluentPerformance.KnownNetworkTechs: String;
// MODIFY THIS FUNCTION
begin
  Result := '"' + cInfiniBand + '" or "' + c10GigabitEthernet + '"';
end;

procedure TAnsysFluentPerformance.LoadParameters (Parameters: TStrings);
// MODIFY THIS FUNCTION if you need to load more parameters
var
  TmpCoresStr, TmpPerformanceStr: String;
  AllowThroughputModeStr: String;
  AllowThroughputModeDefault: Boolean = False;      // That's the default value
begin
  // Fetch benchmark name and network type from the URL
  Benchmark := Parameters.Values[cBenchmark];
  if Benchmark = '' then                            // List known benchmark names
    AppendErrorMessage ('"' + cBenchmark + '" must be specified: ' + KnownBenchmarkTypes);

  NetworkTech := Parameters.Values[cNetworkTech];
  if NetworkTech = '' then                          // List known network types
    AppendErrorMessage ('"' + cNetworkTech + '" must be specified: ' + KnownNetworkTechs);

  // Convert "CPU_Frequency" to a floating point format; if it fails, remember this by setting the error message
  try
    CPU_Frequency := GenericStrToFloat (Parameters.Values[cCPU_Frequency]);
    if CPU_Frequency <= 0 then       // Same as above; "CPU_Frequency" can't be negative
      Raise Exception.Create ('');
  except
    AppendErrorMessage ('"' + cCPU_Frequency + '" must be a positive floating-point value');
  end;

  // Check if throughput mode is allowed
  AllowThroughputModeStr := Parameters.Values[cAllowThroughputMode];
  if AllowThroughputModeStr = '' then               // Nothing was specified, use the default
    AllowThroughputMode := AllowThroughputModeDefault
  else                                              // Something was specified, try to parse
    try
      AllowThroughputMode := StrToBool (AllowThroughputModeStr);
    except
      AppendErrorMessage ('"' + cAllowThroughputMode + '" must be a boolean value');
    end;

  // All essential mandatory parameters ("Benchmark", "NetworkTech", "CPU_Frequency", etc.) have been defined by now.
  // We only need one more parameter, which is either "Cores" or "Performance"

  // Were they defined?
  TmpCoresStr := Parameters.Values[cCores];
  TmpPerformanceStr := Parameters.Values[cPerformance];

  // One, and only one must be defined. Having both strings empty or both strings non-empty is erroneous.
  // "XOR" will help here.
  if not ((TmpCoresStr = '') xor (TmpPerformanceStr = '')) then
    AppendErrorMessage ('"' + cCores + '" and "' + cPerformance + '" are contradictory, one (and only one) must be defined');

  // If "Cores" was defined, then the user requested direct performance modelling:
  // given the number of cores, calculate performance.
  //
  // If "Performance" was specified instead, then the user requested
  // inverse performance modelling: given the desired performance, calculate the number
  // of cores required to attain it.
  if TmpCoresStr <> '' then  // "Cores" was defined
  begin
    // Set the attribure of a direct model
    IsDirectModel := True;
    // Convert "Cores" into an integer
    try
      Cores := StrToInt (TmpCoresStr);
      if Cores < 1 then               // "Cores" must be a positive integer, otherwise raise an exception
        Raise Exception.Create ('');  // which will be handled one line below
    except
      AppendErrorMessage ('"' + cCores + '" must be a positive integer');
    end;
  end;

  if TmpPerformanceStr <> '' then             // "Performance" was defined
  begin
    IsDirectModel := False;
    // Convert "Performance" to a floating point value
    // and save in the field "ProjectedPerformance"
    try
      ProjectedPerformance := GenericStrToFloat (TmpPerformanceStr);
      if ProjectedPerformance <= 0 then       // Can't be negative
        Raise Exception.Create ('');
    except
      AppendErrorMessage ('"' + cPerformance + '" must be a positive floating-point value');
    end;
  end;
end;

procedure TAnsysFluentPerformance.CalculatePerformance;
//
// XXX: This function relies on monotone increase of performance with the number of cores.
// This usually holds in real life situations.
//
// MODIFY THIS FUNCTION (rewrite completely as your own model requires)
//
// Follow these guidelines:
// 1. The procedure, upon successful execution, shall calculate and write the correct value of performance
// into "Performance". Use "Cores" and "NetworkTech" for calculating performance.
// 2. If an error occurs, "Performance" must be set to zero to indicate an error, and the error message
// must be specified using "AppendErrorMessage".
// 3. The variable "MaxRatingAtCores" should contain the maximum number of cores which still can be used
// for running this benchmark (with this network type) -- i.e., bigger number of cores results in
// severely degraded efficiency.
const
  // This is the so-called CPU coefficient for our performance model, the higher it is,
  // the better this particular CPU architecture performs at the given task. "kCPU" is multiplied
  // by CPU frequency to attain performance (rating) for serial (only one core) execution.
  // We only have one "kCPU" in this performance model because we didn't have enough measured performance data.
  //
  kCPU = 3.75;

  // This is how many cores we had in the machine where performance measurements were made:
  Cores_per_machine = 12;

  // Measurement results were only available up to this number of cores, for the two network technologies, respectively.
  // We assume that exceeding this number of cores yields unsatisfactory performance. Therefore, if "Cores" is bigger
  // than the maximal numbers below (MaxCores), performance is not calculated directly but rather scaled with regard to the maximum
  // performance attainable at MaxCores.
  MaxCoresInfiniBand = 3072;
  MaxCoresTenGigabitEthernet = 384;

  function InfiniBandEfficiency (Cores: LongInt) : Single;
  const
    k = -0.00979;
    b = 88.42231;
  begin
    // This function returns parallel computing efficiency as a linear function (y=k*Cores+b) from the number of cores
    Result := (k * Cores + b) / 100;
  end; { InfiniBandEfficiency }

  function TenGigabitEthernetEfficiency (Cores: LongInt) : Single;
  const
    k = -0.08270;
    b = 101.9848;
  begin
    // This function returns parallel computing efficiency as a linear function (y=k*Cores+b) from the number of cores
    Result := (k * Cores + b) / 100;
  end; { TenGigabitEthernetEfficiency }

  function InfiniBandPerformance (Cores: LongInt; CPU_Frequency: Single) : Single;
  begin
    Result := CPU_Frequency * kCPU * (Cores / Cores_per_machine) * InfiniBandEfficiency (Cores);
  end; { InfiniBandPerformance }

  function TenGigabitEthernetPerformance (Cores: LongInt; CPU_Frequency: Single) : Single;
  begin
    if Cores < 192 then
      Result := InfiniBandPerformance (Cores, CPU_Frequency)  // Just because we don't have a better approximation... :-(
    else
      Result := CPU_Frequency * kCPU * (Cores / Cores_per_machine) * TenGigabitEthernetEfficiency (Cores);
  end; { TenGigabitEthernetPerformance }

  function SelectPreferredNetworkTech (var NetworkTech: String): Boolean;
   { If a list of comma-separated network technologies was specified,
     then the compute node has several differing network adapters.
     The function must return the network technology from the list
     that would provide the best performance, all other conditions
     being equal. Currently, it is obvious that "InfiniBand" is
     better to use than "10GbE", therefore the function is itself trivial.
     If you are dealing with more advanced cases (such as comparing a 3D-torus
     to a blocking fat-tree), then instead of using a function like this you
     might need to actually evaluate performance in both cases, and return
     to the user the best result.
    }
  begin
    // Suppose we were passed the following list of network technologies available on
    // the compute node: "10GbE,InfiniBand-4X-QDR". It is obvious that, if both are available,
    // we should use "InfiniBand". The function works this way: if a string corresponding
    // to the best available technology is found, then it is returned in the supplied
    // string parameter. Then we check for the presence of the next most-preferred technology,
    // in the same way. If no familiar technologies were found, return an error.

    Result := True;                             // Hope for the better

    // In case any string containing "InfiniBand" is found (such as "10GbE,InfiniBand-4X-QDR"),
    // simply return "InfiniBand" in response
    if AnsiContainsText (NetworkTech, cInfiniBand) then
      NetworkTech := cInfiniBand                // Replace the input parameter with the best technology
    else
    if AnsiContainsText (NetworkTech, c10GigabitEthernet) then
      NetworkTech := c10GigabitEthernet         // Try the next most-preferred technology
    else
      Result := False;                          // If none were found, report an error
  end;

begin
  if IsErrorSet then Exit;                   // If error was already set, exit immediately

  if not SelectPreferredNetworkTech (NetworkTech) then  // Couldn't recognize the technology
    Exit;
  // After "SelectPreferredNetworkTech" is run, the variable "NetworkTech" contains only one technology name.
  // We now act according to this name.

  // If throughput performance mode was explicitly disallowed, we must check for this (for every network technology)
  // and report an error in case of violations.

  if CompareText (NetworkTech, cInfiniBand) = 0 then   // Calculate performance for InfiniBand
  begin
    MaxRatingAtCores := MaxCoresInfiniBand;  // Report the maximal reasonable value of cores for this type of network
    MaxRating := InfiniBandPerformance (MaxRatingAtCores, CPU_Frequency); // Calculate max rating for that number of cores
    if Cores <= MaxRatingAtCores then
    begin
      Performance := InfiniBandPerformance (Cores, CPU_Frequency);
      TimeToSolution := 86400 / Performance;
      ThroughputMode := False;
    end
    else  // "Too many" cores specified for a cluster, throughput performance mode will be required
    begin
      if AllowThroughputMode then
      begin
        Performance :=  MaxRating * (Cores div MaxRatingAtCores);
        Performance += InfiniBandPerformance (Cores - MaxRatingAtCores * (Cores div MaxRatingAtCores), CPU_Frequency);
        TimeToSolution := 86400 / MaxRating;
        ThroughputMode := True;
      end
      else
      begin            // Throughput performance mode explicitly dissalowed
        AppendErrorMessage (cConstrainViolation + ': ' + cThroughputModeNotAllowed);
        // Just to be safe, reset "Performance" to zero to indicate an error:
        Performance := 0;
        Exit;
      end;
    end;
  end
  else
  if CompareText (NetworkTech, c10GigabitEthernet) = 0 then
  // Calculate performance for 10GbE
  // 10GbE is a complex issue. We don't have much performance measurements for it available.
  // Therefore, we make an arbitrary assumption that for "Cores <= 192" parallel efficiency is best approximated
  // by InfiniBand characteristics, while for "192 <= Cores <= MaxCoresTenGigabitEthernet" we use our own efficiency approximation.
  // Running this benchmark on anything more than MaxCoresTenGigabitEthernet, which is 384 cores,
  // makes no sense (performance degrades greatly), so we use throughput performance mode as in the case of InfiniBand.
  begin
    MaxRatingAtCores := MaxCoresTenGigabitEthernet;  // Report the maximal reasonable value of cores for this type of network
    MaxRating := TenGigabitEthernetPerformance (MaxRatingAtCores, CPU_Frequency); // Calculate max rating for that number of cores
    if Cores <= MaxRatingAtCores then
    begin
      Performance := TenGigabitEthernetPerformance (Cores, CPU_Frequency);
      TimeToSolution := 86400 / Performance;
      ThroughputMode := False;
    end
    else // Cores >= 384, calculate throughput performance
    begin
      if AllowThroughputMode then
      begin
        Performance :=  MaxRating * (Cores div MaxRatingAtCores);
        Performance += TenGigabitEthernetPerformance (Cores - MaxRatingAtCores * (Cores div MaxRatingAtCores), CPU_Frequency);
        TimeToSolution := 86400 / MaxRating;
        ThroughputMode := True;
      end
      else
      begin            // Throughput performance mode explicitly dissalowed
        AppendErrorMessage (cConstrainViolation + ': ' + cThroughputModeNotAllowed);
        // Just to be safe, reset "Performance" to zero to indicate an error:
        Performance := 0;
        Exit;
      end;
    end;
  end
  else  // We don't know this network type! Act conservatively and report an error
  begin
    AppendErrorMessage ('no such network known, "' + NetworkTech + '"');
    // Just to be safe, reset "Performance" to zero to indicate an error:
    Performance := 0;
  end;
end;

end.


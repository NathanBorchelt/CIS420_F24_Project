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
unit Perf_Common;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math;

const
  cNVPStatusError = 'status=error';
  cNVPErrorIs = 'error_message=';
  { String constants used in CGI requests & responses }
  // Used in requests:
  cTask = 'task';
  cDesign = 'design';
  cCores = 'cores';
  cProjectedPerformance = 'projected_performance';
  cCPU_Frequency = 'cpu_frequency';
  cBenchmark = 'benchmark';
  cNetworkTech = 'network_tech';
  cAllowThroughputMode = 'allow_throughput_mode';
  // Used in responses:
  cPerformance = 'performance';
  cTimeToSolution = 'time_to_solution';
  cMaxRatingAtCores = 'max_rating_at_cores';
  cMaxRating = 'max_rating';
  cPerformanceThroughputMode = 'performance_throughput_mode';
  // For error messages in responses:
  cConstrainViolation = 'Constraint violation';
  cThroughputModeNotAllowed = 'Throughput performance mode not allowed';
  // Other:
  cSoftware = 'software';
  cPerfModelId = 'perf_model_id';
  { Types of output format }
  cOutputType = 'output_type';
  cOutputText = 'text';
  cOutputCSV = 'csv';
  cOutputNVP = 'nvp';
  { Technologies of cluster interconnection networks }
  cInfiniBand        = 'InfiniBand';
  c10GigabitEthernet = '10GbE';

type

  { TPerformance }

  TPerformance = class (TPersistent)
    protected
      PerformanceError: String;                 // Holds error message, if any
    public
      Software: String;                         // Software whose performance we model
      Benchmark: String;                        // A single software may have several benchmarks
      PerfModelId: String;                      // An ID of this model, used to discern from other similar models, e.g., may hold author's name
      Cores: Integer;                           // Number of cores for which performance is assessed
      MaxRatingAtCores: Integer;                // The maximum number of cores for which running is still reasonable
      MaxRating: Single;                        // Performance rating attained at the above number of cores
      ThroughputMode: Boolean;                  // If "True", desired performance can be achieved only through scaling of "MaxRatingAtCores"
      AllowThroughputMode: Boolean;             // If "False", entering throughput mode is not allowed, and an error is returned
      CPU_Frequency: Single;                    // Clock frequency of CPU cores, in GHz
      NetworkTech: String;                      // Type of cluster interconnection network
      Performance: Single;                      // Performance (tasks performed in unit time)
      TimeToSolution: Single;                   // How much time it takes to perform a single run
      ProjectedPerformance: Single;             // The performance that the user wants to achieve
      IsDirectModel: Boolean;                   // If "True", calculate performance. If "False", calculate the number of cores
      procedure LoadParameters (Parameters: TStrings); virtual; abstract;  // Every child defines these two methods in their own way
      procedure CalculatePerformance; virtual; abstract;
      procedure DoInverseModel;
      function IsErrorSet: Boolean;             // Returns "True" if error message is set
      function NVPErrorDescription: String;     // Error description
      function NVPDescription: String;          // Name-value pairs (machine-readable description)
      function CSVHeader: String;               // Header of the CSV description
      function CSVDescription: String;          // Comma-separated values version of the description
      function CSVDescriptionWithHeader: String;// Header plus CSV
      procedure AppendErrorMessage (S: String); // Append a string to the error message
      procedure Assign (aPerformance: TPersistent); override; // Used to duplicate an object
  end;

implementation

{ TPerformance }

procedure TPerformance.DoInverseModel;
// WARNING! This method heavily depends on the monotone increase of the performance function!
// (Because the bisection method is used).
// If monotone increase does not hold, incorrect (too high) number of cores may be returned.
// In real life performance usually indeed increases monotonically with the number of cores.
// (In other cases, implement some other search method that inspects more points)
const
  MulFactor = 2;
  EpsilonCores = 1;
var
  Cl, Cm, Cr: LongInt;
begin
  if IsErrorSet then Exit;            // If error is set, do nothing

  if IsDirectModel then               // This method should only be called if the user requested the inverse model invocation
  begin
    AppendErrorMessage ('Inverse model called incorrectly, error in program, please file a bug report');
    Exit;
  end;

  // When we enter this method, the object already has almost all necessary properties filled.
  // We just need "MaxRatingAtCores" and "MaxRating". To get them, we perform a dumb performance calculation, say, for 1 core.
  Cores := 1;
  CalculatePerformance;

  // If the user wants even more performance than "MaxRating", we have to inform them that it is only achievable in
  // "throughput (capacity) computing" mode. The number of cores has to be scaled accordingly.
  if ProjectedPerformance > MaxRating then
    if AllowThroughputMode then       // Throughput mode allowed
    begin
      ThroughputMode := True;
      Cores := Round (MaxRatingAtCores * ProjectedPerformance / MaxRating);
      // Calculate performance with the new number of cores
      CalculatePerformance;
      Exit;
    end
    else                              // Throughput mode explicitly dissalowed
    begin
      AppendErrorMessage ('Performance rating '+ FloatToStrF (ProjectedPerformance, ffFixed, 0, 2) +
        ' only available in throughput mode');
      Exit;
    end;

  // If we reached here, then performance requested by the user is _smaller_ than the maximum achievable
  // on a supercomputer with these parameters. The throughput performance mode is not required.
  // Employ the bisection method to search for the exact number of cores
  // that attains the requested performance.
  ThroughputMode := False;

  // The bisection method. Set the right bound, Cr, to just one core, and calculate performance:
  Cr := 1;
  Cores := Cr;
  CalculatePerformance;

  // Sometimes one core is enough to achieve desired performance; if so, exit
  if Performance >= ProjectedPerformance then Exit;

  // Keep increasing the right bound, Cr, until the performance reaches or exceeds "ProjectedPerformance":
  while Performance < ProjectedPerformance do
  begin
    // Multiply by "MulFactor" (default = 2, this goes pretty fast)
    Cr *= MulFactor;

    // But it shall not exceed the maximum reasonable number of cores, nevertheless:
    if Cr > MaxRatingAtCores then
      Cr := MaxRatingAtCores;

    Cores := Cr;
    CalculatePerformance;
  end;
  // When we reach here, performance at "Cr" cores is more than or equal to "ProjectedPerformance"
  // The previous value was not enough to attain "ProjectedPerformance". Set the left bound to that previous value:
  Cl := Floor (Cr / MulFactor);
  // Now, the exact number of cores that brings us the "ProjectedPerformance" is somewhere between "Cl" and "Cr".
  // Search for it. Set the average value, "Cm", and calculate performance at that point:
  Cm := (Cl + Cr) div 2;
  Cores := Cm;
  CalculatePerformance;
  // Keep bisecting the interval until the exact value is found:
  // (in fact, until the interval's length equals "EpsilonCores" or less, the default is 1, which means exact calculation)
  while (Cr - Cl) > EpsilonCores do
  begin
    if Performance > ProjectedPerformance then   // "Cr" too big, reduce it to "Cm"
      Cr := Cm
    else                                         // "Cl" too small, increase it to "Cm"
      Cl := Cm;
    // Bisect again
    Cm := (Cl + Cr) div 2;
    Cores := Cm;
    CalculatePerformance;
  end;
  // Still, choose the right bound of the interval as the final number of required cores
  // (otherwise, the attained performance would be a bit smaller than requested)
  // Then, calculate performance for the last time. This sets final values of important fields.
  Cores := Cr;
  CalculatePerformance;
end;

function TPerformance.IsErrorSet: Boolean;
begin
  Result := (PerformanceError <> '');
end;

function TPerformance.NVPErrorDescription: String;
begin
  if PerformanceError = '' then
    Result := ''
  else
    Result := cNVPStatusError + LineEnding + cNVPErrorIs + PerformanceError;   // This is, actually, two strings
end;

function TPerformance.NVPDescription: String;
begin
  if Performance = 0 then   // If "Performance" was not calculated yet, do it now
    CalculatePerformance;

  // Error might have occurred during performance calculation (or might have been there before)
  // Report the error and exit
  if IsErrorSet then
  begin
    Result := NVPErrorDescription;
    Exit;
  end;

  with TStringList.Create do
    try
      Add (cSoftware + '=' + Software);
      Add (cBenchmark + '=' + Benchmark);
      Add (cPerfModelId + '=' + PerfModelId);
      Add (cCores + '=' + IntToStr (Cores));
      Add (cNetworkTech + '=' + NetworkTech);
      Add (cPerformanceThroughputMode + '=' + BoolToStr (ThroughputMode, True));   // Prints "True" of "False"
      Add (cTimeToSolution + '=' + FloatToStrF (TimeToSolution, ffFixed, 0, 1));
      Add (cMaxRatingAtCores + '=' + IntToStr (MaxRatingAtCores));
      Add (cMaxRating + '=' + FloatToStrF (MaxRating, ffFixed, 0, 1));
      Add (cPerformance + '=' + FloatToStrF (Performance, ffFixed, 0, 1)); // Fixed float format, 1 digit after decimal point
      if not IsDirectModel then                                        // In case of inverse modelling, add more info
        Add (cProjectedPerformance + '=' + FloatToStrF (ProjectedPerformance, ffFixed, 0, 1));
    finally
      Result := Text;      // Return result is the contents of the TStringList
      Free;                // Free the object
    end;
end;

function TPerformance.CSVHeader: String;
begin
  Result := cSoftware + ', ' + cBenchmark + ', ' + cPerfModelId + ', ' + cCores + ', ' +
    cNetworkTech + ', ' + cPerformanceThroughputMode + ', ' + cTimeToSolution + ', ' +
    cMaxRatingAtCores + ', ' +
    cMaxRating + ', ' + cPerformance;
  if not IsDirectModel then                                            // In case of inverse modelling, add more info
    Result += ', ' + cProjectedPerformance;
end;

function TPerformance.CSVDescription: String;
begin
  if Performance = 0 then   // If "Performance" was not calculated yet, do it now
    CalculatePerformance;

  // Error might have occurred during performance calculation (or might have been there before)
  // Report the error and exit
  if IsErrorSet then
  begin
    Result := NVPErrorDescription;
    Exit;
  end;

  Result := '"' + Software + '","' + Benchmark + '","' + PerfModelId + '",' + IntToStr (Cores) + ',' +
    NetworkTech + ',' + BoolToStr (ThroughputMode, True) + ',' +
    '"' + FloatToStrF (TimeToSolution, ffFixed, 0, 1) + '", ' + IntToStr (MaxRatingAtCores) + ',' +
    '"' + FloatToStrF (MaxRating, ffFixed, 0, 1) + '","' + FloatToStrF (Performance, ffFixed, 0, 1) + '"';
  if not IsDirectModel then                                            // In case of inverse modelling, add more info
    Result += ',"' + FloatToStrF (ProjectedPerformance, ffFixed, 0, 1) + '"';
  (*    Don't forget to edit the sister function, "CSVHeader", as well!    *)
end;

function TPerformance.CSVDescriptionWithHeader: String;
begin
  // Consists of two lines: the header and the description itself
  Result := CSVHeader + LineEnding + CSVDescription;
end;

procedure TPerformance.AppendErrorMessage(S: String);
begin
  if PerformanceError <> '' then   // If a previous message exists, add a delimiter first
    PerformanceError += '; ';

  PerformanceError += S;           // Now, append the message
end;

procedure TPerformance.Assign (aPerformance: TPersistent);
// Used to duplicate an object
begin
  if aPerformance is TPerformance then
  begin
    PerformanceError := TPerformance (aPerformance).PerformanceError;
    Software := TPerformance (aPerformance).Software;
    Benchmark := TPerformance (aPerformance).Benchmark;
    PerfModelId := TPerformance (aPerformance).PerfModelId;
    Cores := TPerformance (aPerformance).Cores;
    MaxRatingAtCores := TPerformance (aPerformance).MaxRatingAtCores;
    CPU_Frequency := TPerformance (aPerformance).CPU_Frequency;
    NetworkTech := TPerformance (aPerformance).NetworkTech;
    Performance := TPerformance (aPerformance).Performance;
    ProjectedPerformance := TPerformance (aPerformance).ProjectedPerformance;
    IsDirectModel := TPerformance (aPerformance).IsDirectModel;
  end
  else
    inherited Assign (aPerformance);
end;

end.


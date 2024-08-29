{
Copyright (C) 2012 Konstantin S. Solnushkin

This file is part of "clusterdesign", the GUI tool to design high-performance
computer clusters.

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
}
unit ClusterConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math,
  httpsend, synacode,                       // "Synapse" package by Lukas Gebauer
  Config, Common, FloatStr;                 // Our own units

const
  cNodes = 'nodes';
  cCpu = 'cpu';               // A prefix used for the other characteristics below
  cCpuModel = cCpu + '_model';
  cCpuCores = cCpu + '_cores';
  cCpuFrequency = cCpu + '_frequency';
  cCpuFlopsPerCycle = cCpu + '_flops_per_cycle';
  cCores = 'cores';
  cCapex = 'capex';
  cOpex = 'opex';
  cTco = 'tco';
  // Per-node characteristics:
  cNode = 'node';             // A prefix used for the other characteristics below
  cNodeModel = cNode + '_model';
  cNodePeakPerformance = cNode + '_peak_performance';
  cNodeMainMemoryPerCore = cNode + '_main_memory_per_core';
  cNodeMainMemorySize = cNode + '_main_memory_size';
  cNodeCpuCount = cNode + '_cpu_count';
  cNodeCost = cNode + '_cost';
  cNodePower = cNode + '_power';
  cNodeWeight = cNode + '_weight';
  cNodeEquipmentSize = cNode + '_equipment_size';
  cEnclosureSize = 'enclosure_size';
  // Network characteristics:
  cNetwork = 'network';       // A prefix used for the other characteristics below
  cNetworkTech = cNetwork + '_tech';
  cNetworkCost = cNetwork + '_cost';
  cNetworkPower = cNetwork + '_power';
  cNetworkWeight = cNetwork + '_weight';
  cNetworkEquipmentSize = cNetwork + '_equipment_size';
  // UPS characteristics
  cUps = 'ups';               // A prefix used for the other characteristics below
  cUpsCost = cUps + '_cost';
  cUpsHeat = cUps + '_heat';
  cUpsWeight = cUps + '_weight';
  cUpsSize = cUps + '_size';
  cUpsSizeRacks = cUps + '_size_racks';
  // Per-cluster characteristics:
  cPower = 'power';
  cWeight = 'weight';
  cPerformance = 'performance';
  cEquipmentSize = 'equipment_size';
  cEquipmentSizeRacks = cEquipmentSize + '_racks';
  // Strings related to settings
  cRackHeight = 'rack_height';
  cSystemLifetimeYears = 'system_lifetime_years';
  cKwhPrice = 'kwh_price';
  cRackOpExPerYear = 'rack_opex_per_year';
  // Related to built-in models
  cPeakPerformance = 'Peak performance';

var
  // Add other heuristics here if you wish
  ArraySimpleHeuristicType: array [0..0] of String = ('Node cost / Peak perf.');
  ArrayObjFuncType: array [0..1] of String = ('TCO / Performance', 'CapEx / Performance');

type

  { TClusterConfig }

  TClusterConfig = class (TConfig)
    private
      function GetCores: Integer;
      function GetNodes: Integer;
      function GetPerformance: Single;
      function GetNodePeakPerformance: Single;
      function GetNodeMainMemoryPerCore: Single;
      procedure SetCores (const AValue: Integer);
      procedure SetNodes (const NodeCount: Integer);
      procedure SetPerformance (const AValue: Single);
      procedure RoundupNodeCount;
      // Simple objective function(s) used for heuristics:
      function NodeCostToPeakPerformance: Single;
      // Complex objective functions
      function CapexToPerformance: Single;
      function TcoToPerformance: Single;
    public
      property Cores: Integer read GetCores write SetCores;
      property Nodes: Integer read GetNodes write SetNodes;
      property Performance: Single read GetPerformance write SetPerformance;
      property NodePeakPerformance: Single read GetNodePeakPerformance;
      property NodeMainMemoryPerCore: Single read GetNodeMainMemoryPerCore;
      procedure ComputeTechnicalCharacteristics;
      procedure ComputeSimpleObjFunc (id: String);
      procedure ComputeObjFunc (id: String);
      function EssentialCharacteristicsText: String;
      function RunInversePerformanceModel (var AModel: TGenericModel): Boolean;
      function RunInversePeakPerformanceModel (var AModel: TGenericModel): Boolean;
      function ComputePerformance (var AModel: TGenericModel): Boolean;
      function DesignNetwork (var AModel: TGenericModel): Boolean;
      function DesignUps (var AModel: TGenericModel): Boolean;
      function CallModel (var AModel: TGenericModel): Boolean;
  end;

  { TClusterConfigList }

  TClusterConfigList = class (TConfigList)
  // This class manages its own memory
    protected
      function GetItem(Index: Integer): TClusterConfig;
    public
      function CreateListItem: TClusterConfig; override;
      property Items[Index: Integer]: TClusterConfig read GetItem; default;
      function ApplyHeuristic (id: String): Boolean;
  end;


implementation

{ TClusterConfig }

function TClusterConfig.GetCores: Integer;
// Minimal wrapping to help find errors. Same trick is used in the functions below.
begin
  try
    Result := StrToInt (Values [cCores]);
  except
    On EConvertError do MyShowMessageFmt ('Error in function %s.GetCores', [ClassName]);
  end;
end;

function TClusterConfig.GetNodes: Integer;
begin
  try
    Result := StrToInt (Values [cNodes]);
  except
    On EConvertError do MyShowMessageFmt ('Error in function %s.GetNodes', [ClassName]);
  end;
end;

function TClusterConfig.GetPerformance: Single;
begin
  try
    Result := GenericStrToFloat (Values [cPerformance]);
  except
    On EConvertError do MyShowMessageFmt ('Error in function %s.GetPerformance', [ClassName]);
  end;
end;

function TClusterConfig.GetNodePeakPerformance: Single;
begin
  try
    Result := GenericStrToFloat (Values [cNodePeakPerformance]);
  except
    On EConvertError do MyShowMessageFmt ('Error in function %s.GetNodePeakPerformance', [ClassName]);
  end;
end;

function TClusterConfig.GetNodeMainMemoryPerCore: Single;
begin
  try
    Result := GenericStrToFloat (Values [cNodeMainMemoryPerCore]);
  except
    On EConvertError do MyShowMessageFmt ('Error in function %s.GetNodeMainMemoryPerCore', [ClassName]);
  end;
end;

procedure TClusterConfig.SetCores(const AValue: Integer);
begin
  Values [cCores] := IntToStr (AValue);
end;

procedure TClusterConfig.SetNodes (const NodeCount: Integer);
begin
  Values [cNodes] := IntToStr (NodeCount);
end;

procedure TClusterConfig.SetPerformance(const AValue: Single);
begin
  // XXX: Hard-wired: one decimal digit (but who needs more?)
  Values [cPerformance] := FloatToStrF (AValue, ffFixed, 0, 1);
end;

procedure TClusterConfig.RoundupNodeCount;
begin
  // The current number of cores may correspond to a fractional number of nodes (say, 3.8).
  // Compute the new number of nodes, then round it upwards to the nearest integer number
  Nodes := Ceil (Cores / (GenericStrToFloat (Values [cCpuCores]) * GenericStrToFloat (Values [cNodeCpuCount])));

  // The number of nodes might have increased due to the above operation, recompute the number of cores,
  // based on the new number of nodes.
  // "Round" is used to convert from Float to Integer
  Cores := Round (Nodes * GenericStrToFloat (Values [cCpuCores]) * GenericStrToFloat (Values [cNodeCpuCount]));
end;

function TClusterConfig.NodeCostToPeakPerformance: Single;
begin
  // Divide cost by peak performance
  Result := GenericStrToFloat (Values [cNodeCost]) / NodePeakPerformance;
end;

function TClusterConfig.CapexToPerformance: Single;
begin
  Result := GenericStrToFloat (Values [cCapex]) / GenericStrToFloat (Values [cPerformance]);
end;

function TClusterConfig.TcoToPerformance: Single;
begin
  Result := GenericStrToFloat (Values [cTco]) / GenericStrToFloat (Values [cPerformance]);
end;

procedure TClusterConfig.ComputeTechnicalCharacteristics;
// Computes some important technical characteristics.
// Not very flexible, because some configurations may not have all the characteristics.
// For example, what happens if compute node weight is not defined for the current configuration --
// shall we just not compute the cluster weight then?

// This function is called many, many times for each configuration -- and every time it recomputes
// technical and economic characteristics. It is more convenient to have a single function so that
// all calculations can be easily tracked. Besides, calling this function every time the
// next subsystem is designed allows to immediately check for constraint violation, which may
// disable the configuration, thereby skipping unnecessary calls to web services responsible
// for other subsystems.
const
  HoursPerYear = 365 * 24;
var
  CapEx, OpEx, Tco, Power, EnclosureSize, RackHeight, SystemLifetimeYears, RackOpExPerYear: Integer;
  EqSize, EqSizeRacks, Weight, KwhPrice: Single;
begin
  if not Enabled then Exit;         // Only process enabled configurations

  // Calcaluate CapEx:
  //   Cost of compute nodes:
  CapEx := Nodes * StrToInt (Values [cNodeCost]);
  //   Cost of network:
  if Values [cNetworkCost] <> '' then
    CapEx += StrToInt (Values [cNetworkCost]);
  //   Cost of UPS:
  if Values [cUpsCost] <> '' then
    CapEx += StrToInt (Values [cUpsCost]);
  // Final assignment:
  Values [cCapex] := IntToStr (CapEx);

  // Calculate "Power", "Weight" and "Equipment size"
  // XXX: Need to add similar figures from other models (storage subsystem, cooling equipment) when they become available
  // Note: we can have nice properties in TClusterConfig class to access there metrics, such as "property Power: Integer", etc.
  // Unsure if the sprawl is needed, though.
  // XXX: For blade systems, power and weight of enclosures is NOT taken into account, while equipment size is.

  // Power from compute nodes:
  Power := Nodes * StrToInt (Values [cNodePower]);
  // Power from network equipment:
  if Values [cNetworkPower] <> '' then
    Power += StrToInt (Values [cNetworkPower]);
  // Here comes a tricky moment. We need to understand cooling requirements for the configuration,
  // as well as choose the rated current of circuit breakers. As the UPS subsystem is not 100% efficient,
  // some of the electricity it consumes is dissipated as heat and is not passed to the load. Therefore,
  // we need to add that "heat" value to the current value of consumed power. The resulting value of "power"
  // is then good for cooling system and circuit breaker sizing.
  // Heat dissipated by the UPS:
  if Values [cUpsHeat] <> '' then
    Power += StrToInt (Values [cUpsHeat]);
  // Final assignment of power metric
  Values [cPower] := IntToStr (Power);

  // The same with weight
  //   Weight of compute nodes:
  Weight := Nodes * GenericStrToFloat (Values [cNodeWeight]);
  //   Weight of network:
  if Values [cNetworkWeight] <> '' then
    Weight += GenericStrToFloat (Values [cNetworkWeight]);
  //   Weight of UPS:
  if Values [cUpsWeight] <> '' then
    Weight += GenericStrToFloat (Values [cUpsWeight]);
  // Final assignment of weight
  Values [cWeight] := FloatToStrF (Weight, ffFixed, 0, 0);

  // (Almost) the same with equipment size
  EqSize := Nodes * GenericStrToFloat (Values [cNodeEquipmentSize]);
  // If enclosure size is defined, than we are dealing with blade servers, and shall
  // round the equipment size up to the nearest number of units the enclosure(s) occupy
  if Values [cEnclosureSize] <> '' then
  begin
    EnclosureSize := StrToInt (Values [cEnclosureSize]);
    EqSize := Ceil (EqSize / EnclosureSize) * EnclosureSize;
  end;

  // Size of network equipment
  if Values [cNetworkEquipmentSize] <> '' then
    EqSize += GenericStrToFloat (Values [cNetworkEquipmentSize]);

  // Size of the UPS subsystem.
  // Some UPS systems are located in separate racks. We assume
  // that web service will return size in "cUpsSizeRacks".
  // If, however, the UPS is shipped as ordinary blocks designed for usual installation into
  // racks, then we assume the size will be returned in "cUpsSize", and then we add this value
  // to the overall equipment size.

  // Initialise with zero; it will be used if nothing is returned by web services:
  EqSizeRacks := 0;
  // If UPS size is returned in racks, save it:
  if Values [cUpsSizeRacks] <> '' then
    EqSizeRacks := GenericStrToFloat (Values [cUpsSizeRacks])
  else // If it is returned in units, add to other equipment size:
    if Values [cUpsSize] <> '' then
      EqSize += GenericStrToFloat (Values [cUpsSize]);

  // Read rack height
  RackHeight := StrToInt (Values [cRackHeight]);
  // Calculate the number of racks using the user-supplied rack height
  // Assume that compute nodes and network equipment can be densely packaged into racks.
  // Round the required number of racks upwards, and add it to the
  // already present racks of the UPS system.
  EqSizeRacks += Ceil (EqSize / RackHeight);

  // Final assignments
  //   Cumulative size of separate blocks of equipment:
  Values [cEquipmentSize] := FloatToStrF (EqSize, ffFixed, 0, 0);
  Values [cEquipmentSizeRacks] := FloatToStrF (EqSizeRacks, ffFixed, 0, 0);

  // Calculate OpEx and then TCO
  Power := StrToInt (Values [cPower]);
  KwhPrice := GenericStrToFloat (Values [cKwhPrice]);                            // This can be zero due to user request
  RackOpExPerYear := StrToInt (Values [cRackOpExPerYear]);                       // And this, too.
  SystemLifetimeYears := StrToInt (Values [cSystemLifetimeYears]);               // And even this. And if it is, OpEx=0
  OpEx := Round (
    SystemLifetimeYears * (HoursPerYear * KwhPrice * Power / 1000 +              // Power OpEx
    EqSizeRacks * RackOpExPerYear));                                             // Rack space rental OpEx
  Values [cOpex] := IntToStr (OpEx);
  Tco := OpEx + CapEx;
  Values [cTco] := IntToStr (Tco);
end;

procedure TClusterConfig.ComputeSimpleObjFunc (id: String);
// Implements a simple objective function -- the one that can be inferred from the node's characteristics.
// Currently, the objective function "node cost divided by node peak performance" is implemented.
begin
  try
    if Enabled then                 // Only proceed with computations if a configuration is enabled
    begin
      if id = ArraySimpleHeuristicType [0] then     // If we know the specified heuristic,
        SimpleObjFunc := NodeCostToPeakPerformance  // calculate it.
      // (Add more heuristics if you wish)
      else
        MyShowMessageFmt ('No such heuristic had been programmed: "%s", sorry.', [id]);
    end;
  except
    // In case of any floating point exception (or other error), disable this configuration for safety
    Disable (cFloatingPointException);
  end;
end;

procedure TClusterConfig.ComputeObjFunc(id: String);
// Implements a complex objective function -- the one that requires calculating cluster performance first, etc.,
// and therefore cannot be inferred from the characteristics of a compute node alone.
// Currently, objective functions "CapEx / Performance" and "TCO / Performance" are implemented.
begin
  try
    if Enabled then                               // Only proceed with computations if a configuration is enabled
    begin
      if id = ArrayObjFuncType [0] then           // If we know the specified heuristic,
        ObjFunc := TcoToPerformance               // calculate it.
      else
      if id = ArrayObjFuncType [1] then
        ObjFunc := CapexToPerformance
      // (Add more objective functions if you wish)
      else
        MyShowMessageFmt ('No such objective function had been programmed: "%s", sorry.', [id]);
    end;
  except
    // In case of any floating point exception (or other error), disable this configuration for safety
    Disable (cFloatingPointException);
  end;
end;

function TClusterConfig.EssentialCharacteristicsText: String;
// Returns the essential technical and economic characteristics of the configuration
// in a formatted text string
const
  cTemplate = 'Essential characteristics: ' + LineEnding + LineEnding +
    'Compute node model: %s' + LineEnding +
    'Number of compute nodes: %s' + LineEnding + 'Total number of cores: %s' + LineEnding +
    'Performance: %s' + LineEnding + 'Equipment size: %s' + LineEnding + 'Power: %s' +
    LineEnding + 'Weight: %s' + LineEnding + 'Capital expenditures: %s' + LineEnding +
    'Operating expenditures: %s' + LineEnding + 'TCO: %s' + LineEnding +
    LineEnding + 'CPU:' + LineEnding + cSpace + cSpace + 'Model: %s' +
    LineEnding + cSpace + cSpace + 'Cores: %s' + LineEnding + cSpace +
    cSpace + 'Frequency: %s' + LineEnding + LineEnding + 'Network technology: %s';
var
  sNodeModel, sNodes, sCores, sPerformance, sEquipmentSize, sPower, sWeight,
    sCapex, sOpex, sTco, sCpuModel, sCpuCores, sCpuFrequency, sNetworkTech: String;
begin
  // Model of compute nodes
  sNodeModel := Values [cNodeModel];
  if sNodeModel = '' then sNodeModel := cNA;
  // Number of compute nodes
  sNodes := Values [cNodes];
  if sNodes = '' then sNodes := cNA;
  // Total number of cores
  sCores := Values [cCores];
  if sCores = '' then sCores := cNA;
  // Performance (units of measurement depend on what the web service is using)
  sPerformance := Values [cPerformance];
  if sPerformance = '' then sPerformance := cNA;
  // Equipment size
  sEquipmentSize := Values [cEquipmentSize];
  if sEquipmentSize = '' then sEquipmentSize := cNA;
  // Power
  sPower := Values [cPower];
  if sPower = '' then sPower := cNA;
  // Weight
  sWeight := Values [cWeight];
  if sWeight = '' then sWeight := cNA;
  // Capital expenditures
  sCapex := Values [cCapex];
  if sCapex = '' then sCapex := cNA;
  // Operating expenditures
  sOpex := Values [cOpex];
  if sOpex = '' then sOpex := cNA;
  // TCO
  sTco := Values [cTco];
  if sTco = '' then sTco := cNA;
  // CPU Model
  sCpuModel := Values [cCpuModel];
  if sCpuModel = '' then sCpuModel := cNA;
  // Number of cores per CPU
  sCpuCores := Values [cCpuCores];
  if sCpuCores = '' then sCpuCores := cNA;
  // CPU frequency
  sCpuFrequency := Values [cCpuFrequency];
  if sCpuFrequency = '' then sCpuFrequency := cNA;
  // Network technology
  sNetworkTech := Values [cNetworkTech];
  if sNetworkTech = '' then sNetworkTech := cNA;
  // Output the result as a formatted string
  Result := Format (cTemplate, [sNodeModel, sNodes, sCores, sPerformance,
    sEquipmentSize, sPower, sWeight, sCapex, sOpex, sTco, sCpuModel, sCpuCores,
    sCpuFrequency, sNetworkTech]);
end;

function TClusterConfig.RunInversePerformanceModel (var AModel: TGenericModel): Boolean;
// Queries the web service, provides it with the projected performance and
// receives the number of cores (in the current implementation) required to
// attain the requested performance
begin
  if not Enabled then Exit;           // Only deal with enabled configurations

  Result := False;                    // The default is failure

  // "Send" contains the name of the variable where the web service expects to receive the projected performance.
  AModel.Send := NVPName (AModel.Send) + '=' + AModel.Constraints.Values [cMinPerformance];

  // Actually call the model; if it failed, exit
  Result := CallModel (AModel);
  if not Result then Exit;

  // If we reached here, the result was positive, and the contents of "AlsoReceive" -- the auxiliary
  // characteristics -- were already stored in the current configuration. Just the last step remains:
  // save the value of the essential parameter.

  // "Receive" contains the name-value pair where web service reports the number of cores
  Values [cCores] := NVPValue (AModel.Receive);

  // Now, the number of cores has changed. The value returned by the web service can lead to the fractional number of nodes,
  // so we need to perform adjustments.
  RoundupNodeCount;

  // Now that the number of nodes and cores could have increased, it is best to compute the performance again, because
  // it has also possibly increased. That's why the call to this method is often followed by the call to "ComputePerformance".
end;

function TClusterConfig.RunInversePeakPerformanceModel (var AModel: TGenericModel): Boolean;
var
  MinPeakPerformance: Single;
begin
  if not Enabled then Exit;           // Only deal with enabled configurations

  Result := True;                     // We don't call web services, so we always return "True"

  // That's our desired peak performance
  MinPeakPerformance := GenericStrToFloat (AModel.Constraints.Values [cMinPerformance]);
  // That's how many nodes we must use to achieve it. Note that "Ceil" is used to round up the value.
  Nodes := Ceil (MinPeakPerformance / NodePeakPerformance);
  // After rounding up, the performance can be slightly higher than requested. Compute it and store.
  Values [cPerformance] := FloatToStrF (Nodes * NodePeakPerformance, ffFixed, 0, 1);

  // We also need to set the number of cores.
  // That's how many cores our nodes will contain: Nodes * Cores in one CPU * CPUs in one node
  Cores := Nodes * StrToInt (Values [cCpuCores]) * StrToInt (Values [cNodeCpuCount]);
end;

function TClusterConfig.ComputePerformance (var AModel: TGenericModel): Boolean;
// Computes performance by querying the web service
begin
  if not Enabled then Exit;           // Only deal with enabled configurations

  Result := False;                    // The default is failure

  // Make sure the number of cores for this configuration is defined, otherwise we cannot calculate performance
  if Values [cCores] = '' then
  begin
    MyShowMessageFmt ('Error when trying to calculate performance for the configuration: ' +
      'value of "%s" is not assigned yet', [cCores]);
    Exit;
  end;

  // "Send" contains the name of the variable where the web service expects to receive the number of cores.
  AModel.Send := NVPName (AModel.Send) + '=' + Values [cCores];

  // Actually call the model; if it failed, exit
  Result := CallModel (AModel);
  if not Result then Exit;

  // If we reached here, the result was positive, and the contents of "AlsoReceive" -- the auxiliary
  // characteristics -- were already stored in the current configuration. Just the last step remains:
  // save the value of the essential parameter.

  // "Receive" contains the name-value pair where web service reports the performance
  Values [cPerformance] := NVPValue (AModel.Receive);
end;

function TClusterConfig.DesignNetwork (var AModel: TGenericModel): Boolean;
begin
  if not Enabled then Exit;           // Only deal with enabled configurations

  Result := False;                    // The default is failure

  // Make sure the number of nodes for this configuration is defined, otherwise we cannot design a network
  if Values [cNodes] = '' then
  begin
    MyShowMessageFmt ('Error when trying to design a network for the configuration: ' +
      'value of "%s" is not assigned yet', [cNodes]);
    Exit;
  end;

  // "Send" contains the name of the variable where the web service expects to receive the number of nodes.
  AModel.Send := NVPName (AModel.Send) + '=' + Values [cNodes];

  // Actually call the model; if it failed, exit
  Result := CallModel (AModel);
  if not Result then Exit;

  // If we reached here, the result was positive, and the contents of "AlsoReceive" -- the auxiliary
  // characteristics -- were already stored in the current configuration. Just the last step remains:
  // save the value of the essential parameter.

  // "Receive" contains the name-value pair where web service reports the network cost
  Values [cNetworkCost] := NVPValue (AModel.Receive);
end;

function TClusterConfig.DesignUps(var AModel: TGenericModel): Boolean;
begin
  // Almost an identical copy of "DesignNetwork"

  if not Enabled then Exit;           // Only deal with enabled configurations

  Result := False;                    // The default is failure

  // Make sure the "power" metric for this configuration is defined, otherwise we cannot design an UPS subsystem
  if Values [cPower] = '' then
  begin
    MyShowMessageFmt ('Error when trying to design an UPS subsystem for the configuration: ' +
      'value of "%s" is not assigned yet', [cPower]);
    Exit;
  end;

  // "Send" contains the name of the variable where the web service expects to receive the number of nodes.
  AModel.Send := NVPName (AModel.Send) + '=' + Values [cPower];

  // Actually call the model; if it failed, exit
  Result := CallModel (AModel);
  if not Result then Exit;

  // If we reached here, the result was positive, and the contents of "AlsoReceive" -- the auxiliary
  // characteristics -- were already stored in the current configuration. Just the last step remains:
  // save the value of the essential parameter.

  // "Receive" contains the name-value pair where web service reports the network cost
  Values [cUpsCost] := NVPValue (AModel.Receive);
end;

function TClusterConfig.CallModel (var AModel: TGenericModel): Boolean;
// A generic procedure to call a model
var
  AHTTP: THTTPSend;
  QueryURL: String;
  ErrorMsg: String;
  TempStr: String; // A temporary string
  I, Idx: Integer;
  S: TStringList;  // Holds a web service reply
  T: TStringList;  // A temporary list of strings
  U: TConfig;      // A temporary object
begin

  Result := False;                                     // Be prepared for failure

  if not Enabled then Exit;                            // Only deal with enabled configurations

  S := TStringList.Create;
  T := TStringList.Create;
  AHTTP := THTTPSend.Create;
  with AModel do
  try
    // Construct a query URL
    QueryURL := URL;
    // If the "URL" does not yet contain a "?" sign, add it to the tail of "QueryURL"
    if AnsiPos ('?', URL) = 0 then
      QueryURL += '?'
    else                  // But if the '?' was already there, add the '&'
      QueryURL += '&';

    // The essential part that we need to send
    QueryURL += Send;

    // Also send additional parameters to the web service
    T.Text := AlsoSend;                   // "T" contains the list of names
    FillValues (T);                       // In the current config, try to find corresponding values
    for I := 0 to T.Count-1 do            // Send them all together
      QueryURL += '&' + T[I];

    // It is best to encode URL according to RFC. This makes HTTP server logs less human-readable,
    // but allows to send queries that contain spaces and other unusual characters.
    QueryURL := EncodeURL (QueryURL);

    // Try to make a request
    Result := AHTTP.HTTPMethod ('GET', QueryURL);
    // If "True" is returned, the connection was successful
    if Result then
    begin
      // If any other code was returned other than "200", assume failure
      if AHTTP.ResultCode <> 200 then
      begin
        MyShowMessageFmt ('Web service (query URL "%s") returned: HTTP code "%d", string "%s"',
          [QueryURL, AHTTP.ResultCode, AHTTP.ResultString]);
        // Return failure status
        Result := False;
        Exit;
      end;

      // Now, it was not a blatant error. Try to load the web service's response
      S.LoadFromStream (AHTTP.Document);
      if S.Count = 0 then                                // Empty response
      begin
        MyShowMessageFmt ('Web service returned empty response', []);
        // Return failure status
        Result := False;
        Exit;
      end;

      // Also, the web service could return an error message
      Idx := S.IndexOfName (cStatus);
      if Idx <> -1 then
      begin
        if CompareText (S.ValueFromIndex[Idx], cError) = 0 then   // Status is Error
        begin
          ErrorMsg := S.Values[cErrorMessage];                    // Retrieve the error message

          if Length (ErrorMsg) <> 0 then                          // Error message was specified
            Disable (ErrorMsg)                                    // Disable the configuration, providing error message as a reason
          else
            MyShowMessageFmt ('Web service reported a generic error', []);

          // Return failure status
          Result := False;
          Exit;
        end;
      end;

      // If we reached here, assume we got a correct response

      // First of all, we need to retrieve our "main parameter"
      // During repetitive runs, the value of "Receive" may contain not just the name of the variable we
      // need to check for, but also its value, in a form of a "Name=Value" pair. In this case, strip the value

      Receive := NVPName (Receive);

      // Now, "Receive" only contains the name of the parameter. Look for it in "S":
      TempStr := S.Values [Receive];
      if Length (TempStr) = 0 then
      begin
        MyShowMessageFmt ('Web service didn''t report the main parameter, "%s"', [Receive]);
        // Return failure status
        Result := False;
        Exit;
      end;
      // If we reached here, the required string is there. This will return the complete "Name=Value" pair:
      Receive := S [S.IndexOfName(Receive)];

      // What other characteristics do we need to receive from the web service? Write them to the list "T":
      T.Text := AlsoReceive;
      // Create a new temporary object for this purpose
      U := TConfig.Create;
      try
        // Add web service output to this object
        U.AddStrings (S);
        // Fill strings in "T" with values from web service output. That is what we wanted to receive
        U.FillValues (T);
        // Add the result to the current configurations
        Self.InjectNameValuePairs (T);
      finally
        U.Free;
      end;
    end;

  finally
    S.Free;
    T.Free;
    AHTTP.Free;
  end;
end;

{ TClusterConfigList }

function TClusterConfigList.GetItem(Index: Integer): TClusterConfig;
begin
  Result := Objects[Index] as TClusterConfig;
end;

function TClusterConfigList.CreateListItem: TClusterConfig;
begin
  Result := TClusterConfig.Create;
end;

function TClusterConfigList.ApplyHeuristic (id: String): Boolean;
// For each enabled cluster configuration, compute a simple objective function.
// Leave good configurations enabled, and disable the rest.
// Returns "True" if some configurations were disabled (true in almost all cases)
var
  I, J, K: Integer;
  AConf: TClusterConfig;
begin
  Result := False;
  // Compute a simple objective function for every enabled configuration in the list
  for I := 0 to Count-1 do
  begin
    AConf := Items[I];
    if AConf.Enabled then
      AConf.ComputeSimpleObjFunc (id);
  end;
  // It might seem non-optimal to compute a SimpleObjFunc every time this procedure
  // is called, but this is done because every time a different heuristic
  // could be used: one for the 1st pass, another for the 2nd, and so on.

  // Sort the list according to the value of the simple objective function
  CustomSort (@SimpleObjFuncCompare);

  // Some configurations might already be disabled (due to the previous application
  // of this or another heuristic, or due to other reasons). We select the top share
  // of best configurations that are still enabled, and disable all the rest.

  // We will only keep "J" (the top 20%) of configurations enabled
  // (XXX: currently hardwired)
  J := Ceil (CountEnabled * 0.20);

  // "K" tracks the number of enabled combinations.
  K := 0;                                        // Set the initial value
  for I := 0 to Count-1 do
  begin
    AConf := Items[I];

    if K < J then                                // Continue keeping configurations enabled
    begin
      // If a combination is enabled, increment "K"
      if AConf.Enabled then
        K := K+1;
    end
    else                                         // Now, K>=J, and we disable all remaining configurations
    begin
      AConf.Disable (cByHeuristic + ': ' + id);
      Result := True;                            // Indicate that there have been deletions
    end;
  end;
  // As a result, exactly "J" configurations are enabled, and the rest is disabled
  // A repeated call to "CountEnabled" would now return a different (smaller) number.
end;

end.


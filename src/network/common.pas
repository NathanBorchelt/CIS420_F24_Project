{
Copyright (C) 2012 Konstantin S. Solnushkin

This file is part of "Network", the tool to design fat-tree and torus networks.

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
unit common;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  floatstr;         // Our own units

const
  // See also "DecimalSeparator" being defined in the very end of this unit!
  CableCost = 80;                               // A truly naive approach to calculate cable costs!

  { String constants used in network characteristics }
  cNetworkTopology = 'network_topology';
  cNetworkVendor = 'network_vendor';
  cNetworkCost = 'network_cost';
  cNetworkPower = 'network_power';
  cNetworkWeight = 'network_weight';
  cNetworkObjectiveFunction = 'network_objective_function';
  cNetworkEquipmentSize = 'network_equipment_size';
  cNetworkBlockingFactor = 'network_blocking_factor';
  cNetworkLinkCount = 'network_link_count';
  cNetworkSparePorts = 'network_spare_ports';
  cNetworkExpandableTo = 'network_expandable_to';
  cNetworkEdgeSwitchSize = 'network_edge_switch_size';
  cNetworkCoreSwitchSize = 'network_core_switch_size';

  { Names of possible design constraints }
  cCommentCharacter = '#';
  cNodes = 'nodes';
  cNodesFutureMax = 'nodes_future_max';
  cMinPrefix = 'min_';
  cMaxPrefix = 'max_';
  cMaxNetworkCost = cMaxPrefix + cNetworkCost;
  cMaxNetworkPower = cMaxPrefix + cNetworkPower;
  cMaxNetworkWeight = cMaxPrefix + cNetworkWeight;
  cMaxNetworkEquipmentSize = cMaxPrefix + cNetworkEquipmentSize;
  cMaxNetworkBlockingFactor = cMaxPrefix + cNetworkBlockingFactor;
  cPreferExpandable = 'network_prefer_expandable';

  { String constants used in switch characteristics }
  cSwitchPortCount = 'network_switch_port_count';
  cSwitchSize = 'network_switch_size';
  cSwitchModel = 'network_switch_model';
  cSwitchCost = 'network_switch_cost';
  cSwitchWeight = 'network_switch_weight';
  cSwitchPower = 'network_switch_power';
  // Topology name
  cStar = 'star';

  { Path names }
  cDatabaseDir = 'db';
  cDefaultDir = 'default';              // Directory to hold "default" vendor database

type

  { TSwitch }

  TSwitch = class (TStringList)
    public
      function Model: String;
      function P: Integer;                      // Number of ports in a switch
      function Cost: Integer;                   // Cost of the switch
      function Size: Integer;                   // In rack-mount units
      function Weight: Single;                  // In kilograms
      function Power: Integer;                  // In watts
  end;

  { TCable }

  TCable = class                                // Currently not used, sorry.
    public
      Model: String;
      Length: Integer;
      Cost: Integer;
  end;

  { TSwitchList }

  TSwitchList = class (TStringList)             // Manages its own memory
    public
      destructor Destroy; override;
      function BiggestPortCount: Integer;
  end;

  { TNetworkConstraints }

  TNetworkConstraints = class (TStringList)
    protected
      FDescription: String;   // Textual overview of constraints
      FAlreadyChecked: Boolean;
    public
      property Overview: String read FDescription;
      constructor Create;
      function CheckNodes (out Nodes: Integer; var Err: TStringList): Boolean;
      function CheckNodesFutureMax (out NodesFutureMax: Integer; var Err: TStringList): Boolean;
      function CheckBlockingFactor (out Bl: Single; var Err: TStringList): Boolean;
      function CheckMaxNetworkCost (out MaxNetworkCost: Integer; var Err: TStringList): Boolean;
      function CheckMaxNetworkPower (out MaxNetworkPower: Integer; var Err: TStringList): Boolean;
      function CheckMaxNetworkWeight (out MaxNetworkWeight: Single; var Err: TStringList): Boolean;
      function CheckMaxNetworkEquipmentSize (out MaxNetworkEquipmentSize: Integer; var Err: TStringList): Boolean;
      function Check (var Err: TStringList): Boolean; // This function checks them all
      function GetNodes: Integer;
      function GetNodesFutureMax: Integer;
      function GetBlockingFactor: Single;
      function GetMaxNetworkCost: Integer;
      function GetMaxNetworkPower: Integer;
      function GetMaxNetworkWeight: Single;
      function GetMaxNetworkEquipmentSize: Integer;
      procedure AddOverview (S: String);
  end;

  { TNetworkConfig }

  TNetworkConfig = class (TStringList)
    protected
      function GetBlockingFactor: Single; virtual;
      function GetLinkCount: Integer;
      function GetExpandableTo: Integer;
      function GetNodes: Integer;
      function GetNetCost: Integer; virtual; abstract;
      function GetNetPower: Integer; virtual; abstract;
      function GetNetWeight: Single; virtual; abstract;
      function GetNetEquipmentSize: Integer; virtual; abstract;
      function GetNetTopology: String;
      function GetObjectiveFunction: Single;
      function GetSparePorts: Integer;
      procedure SetBlockingFactor(const AValue: Single);
      procedure SetLinkCount(const AValue: Integer);
      procedure SetExpandableTo(const AValue: Integer);
      procedure SetNodes(const AValue: Integer);
      procedure SetNetCost(const AValue: Integer);
      procedure SetNetPower(const AValue: Integer);
      procedure SetNetWeight(const AValue: Single);
      procedure SetNetEquipmentSize(const AValue: Integer);
      procedure SetNetTopology(const AValue: String);
      procedure SetObjectiveFunction(const AValue: Single);
      procedure SetSparePorts(const AValue: Integer);
    public
      property Nodes: Integer read GetNodes write SetNodes;                                  // No. of nodes
      property Bl: Single read GetBlockingFactor write SetBlockingFactor;                    // Blocking factor ("1" means a non-blocking network)
      property L: Integer read GetLinkCount write SetLinkCount;                              // No. of cables
      property SparePorts: Integer read GetSparePorts write SetSparePorts;                   // Spare ports where new nodes can be connected
      property ExpandableTo: Integer read GetExpandableTo write SetExpandableTo;             // How many nodes can accommodate
      property NetCost: Integer read GetNetCost write SetNetCost;                              // Cost of this configuration
      property NetPower: Integer read GetNetPower write SetNetPower;                           // Power, in watts
      property NetWeight: Single read GetNetWeight write SetNetWeight;                         // Weight, in kilograms
      property NetEquipmentSize: Integer read GetNetEquipmentSize write SetNetEquipmentSize;   // Size of this configuration, in units
      property NetTopology: String read GetNetTopology write SetNetTopology;                   // Network topology: star, etc.
      property ObjectiveFunction: Single read GetObjectiveFunction write SetObjectiveFunction; // Objective function
      function CheckForConstraints (Constr: TNetworkConstraints; var DebugMsg: String): Boolean; // Checks if constraints are met
      function Description: String; virtual;                                                 // Human-readable description
      function HtmlDescription: String; virtual; abstract;                                   // Fully-fledged description in HTML
      function CSVHeader: String;                                                            // Header of the CSV description
      function CSVDescription: String;                                                       // Comma-separated values version of the description
      function CSVDescriptionWithHeader: String;                                             // Header plus CSV
      function NVPDescription: String;                                                       // Description in name-value pairs
      procedure Assign (Source: TPersistent); override;                                      // Used to create copies of objects
  end;

  { TConfigList }

  TConfigList = class (TStringList)             // Manages its own memory
    public
      destructor Destroy; override;
      function BestConfig: TNetworkConfig;
  end;

implementation

{ TSwitchList }

destructor TSwitchList.Destroy;
var
  I: Integer;
begin
  for I := Count-1 downto 0 do
    (Objects[I] as TSwitch).Free;
  inherited Destroy;
end;

function TSwitchList.BiggestPortCount: Integer;
var
  I: Integer;
begin
  Result := 0;                                        // Initial value
  for I := 0 to Count-1 do
    if (Objects[I] as TSwitch).P > Result then        // Finds maximum
      Result := (Objects[I] as TSwitch).P;
end;

{ TConfigList }

destructor TConfigList.Destroy;
var
  I: Integer;
begin
  for I := Count-1 downto 0 do
    (Objects[I] as TNetworkConfig).Free;
  inherited Destroy;
end;

function TConfigList.BestConfig: TNetworkConfig;
{ Finds a config with the lowest value of the objective function, and returns a pointer to it.
  In case of errors returns "Nil". }
var
  I: Integer;
  aNetConfig: TNetworkConfig;
begin
  if Count = 0 then                                                         // Nothing to search
  begin
    Result := Nil;                                                          // Return Nil in this case
    Exit;
  end;

  aNetConfig := Objects[0] as TNetworkConfig;                               // Check the zeroth configuration
  Result := aNetConfig;                                                     // Will return this if nothing better is found

  // Step through the list
  for I := 1 to Count-1 do                                                  // Start from "1", not from zero! (We already checked zero)
  begin
    aNetConfig := Objects[I] as TNetworkConfig;
    if aNetConfig.ObjectiveFunction < Result.ObjectiveFunction then         // Found a better alternative (smaller objective function)
      Result := aNetConfig;
  end;
  // Finally, "Result" already contains the best configuration. There is nothing else to do, so we exit happily.
end;

{ TNetworkConfig }

function TNetworkConfig.GetBlockingFactor: Single;
var
  I: Integer;
begin
  I := IndexOfName (cNetworkBlockingFactor);
  Assert (I >= 0);
  Result := GenericStrToFloat (ValueFromIndex [I]);
end;

function TNetworkConfig.GetLinkCount: Integer;
var
  I: Integer;
begin
  I := IndexOfName (cNetworkLinkCount);
  Assert (I >= 0);
  Result := StrToInt (ValueFromIndex [I]);
end;

function TNetworkConfig.GetExpandableTo: Integer;
var
  I: Integer;
begin
  I := IndexOfName (cNetworkExpandableTo);
  Assert (I >= 0);
  Result := StrToInt (ValueFromIndex [I]);
end;

function TNetworkConfig.GetNodes: Integer;
var
  I: Integer;
begin
  I := IndexOfName (cNodes);
  Assert (I >= 0);
  Result := StrToInt (ValueFromIndex [I]);
end;

function TNetworkConfig.GetNetTopology: String;
var
  I: Integer;
begin
  I := IndexOfName (cNetworkTopology);
  Assert (I >= 0);
  Result := ValueFromIndex [I];
end;

function TNetworkConfig.GetSparePorts: Integer;
var
  I: Integer;
begin
  I := IndexOfName (cNetworkSparePorts);
  Assert (I >= 0);
  Result := StrToInt (ValueFromIndex [I]);
end;

procedure TNetworkConfig.SetBlockingFactor(const AValue: Single);
begin
  Values [cNetworkBlockingFactor] := FloatToStrF (AValue, ffFixed, 0, 1);
end;

procedure TNetworkConfig.SetLinkCount(const AValue: Integer);
begin
  Values [cNetworkLinkCount] := IntToStr (AValue);
end;

procedure TNetworkConfig.SetExpandableTo(const AValue: Integer);
begin
  Values [cNetworkExpandableTo] := IntToStr (AValue);
end;

procedure TNetworkConfig.SetNodes(const AValue: Integer);
begin
  Values [cNodes] := IntToStr (AValue);
end;

procedure TNetworkConfig.SetNetCost(const AValue: Integer);
begin
  Values [cNetworkCost] := IntToStr (AValue);
end;

procedure TNetworkConfig.SetNetEquipmentSize(const AValue: Integer);
begin
  Values [cNetworkEquipmentSize] := IntToStr (AValue);
end;

procedure TNetworkConfig.SetNetPower(const AValue: Integer);
begin
  Values [cNetworkPower] := IntToStr (AValue);
end;

procedure TNetworkConfig.SetNetWeight(const AValue: Single);
begin
  Values [cNetworkWeight] := FloatToStrF (AValue, ffFixed, 0, 1);
end;

procedure TNetworkConfig.SetNetTopology(const AValue: String);
begin
  Values [cNetworkTopology] := AValue;
end;

procedure TNetworkConfig.SetObjectiveFunction(const AValue: Single);
begin
  Values [cNetworkObjectiveFunction] := FloatToStrF (AValue, ffFixed, 0, 1);
end;

procedure TNetworkConfig.SetSparePorts(const AValue: Integer);
begin
  Values [cNetworkSparePorts] := IntToStr (AValue);
end;

function TNetworkConfig.CheckForConstraints (Constr: TNetworkConstraints; var DebugMsg: String): Boolean;
{ Accepts the list of constraints and checks if the current configuration satisfies them.
  To do this, analyzes names of constraints and tries to find characteristics of the current config
  that must be checked against constraints. Returns "True" if no constraints were violated.
  Code mostly borrowed from the "ClusterDesign" project. }
type
  TConstrType = (MaxConstraint, MinConstraint);
const
  cViolatedMessage = 'Constraint violated: ';
  Sep = ', '; // Separator
var
  I, ConIdx: Integer;
  CType: TConstrType;
  ConstrName, CName, CValue: String;
  CurValue, NeedValue: Single;
begin
  // The debug message explaining which constraints were violated
  DebugMsg := cViolatedMessage;

  // For each constraint in the list
  for ConIdx := 0 to Constr.Count-1 do
  begin
    // Fetch the constraint name and value
    Constr.GetNameValue (ConIdx, ConstrName, CValue);

    // Empty string got in the way
    if ConstrName = '' then Continue;

    // If constraint starts with a comment character, then skip it
    if ConstrName [1] = cCommentCharacter then
      Continue;

    // We will work with the copy of the original variable
    CName := ConstrName;

    // Is it a "Max" or a "Min" constraint?
    if AnsiPos (cMaxPrefix, CName) = 1 then     // Starts with "MaxPrefix"
    begin
      // It's a "Max" constraint
      CType := MaxConstraint;

      // Delete "MaxPrefix" in the beginning of the line
      System.Delete (CName, 1, Length (cMaxPrefix));
    end
    else
    if AnsiPos (cMinPrefix, CName) = 1 then     // Starts with "MinPrefix"
    begin
      // It's a "Min" constraint
      CType := MinConstraint;

      // Delete "MinPrefix" in the beginning of the line
      System.Delete (CName, 1, Length (cMinPrefix));
    end
    else
      Continue; // Unknown constraint type -- doesn't start with either "MaxPrefix" of "MinPrefix"

    // Now, it is either a Max or a Min constraint.
    // Try to find the corresponding characteristic in the current config.
    I := IndexOfName (CName);
    if I <> -1 then                  // If a characteristic with this name is found
    begin
      // What value is stored in the config
      CurValue := GenericStrToFloat (Values [CName]);

      // What value is specified in the constraint
      NeedValue := GenericStrToFloat (CValue);

      // If it is zero, we do not evaluate that constraint
      if NeedValue = 0 then
        Continue;

      if CType = MaxConstraint then  // It's a Max constraint
      begin
        if CurValue > NeedValue then // '>', constraint violated
          DebugMsg += ConstrName + Sep;
      end
      else                           // It's a Min constraint
      begin
        if CurValue < NeedValue then // '<', constraint violated
          DebugMsg += ConstrName + Sep;
      end;
    end;
  end;

  // Now, all constraints have been processed. If the "DebugMsg" is not equal to its initial value,
  // then something was added to it, which is also an indication of constraint violation.
  if DebugMsg <> cViolatedMessage then
  begin
    // Delete the trailing separator
    System.Delete (DebugMsg, Length (DebugMsg) - Length (Sep) + 1, Length (Sep));
    Result := False;
  end
  else
    Result := True; // All went well
end;

function TNetworkConfig.Description: String;
begin
  // Info on costs and equipment power, weight and size
  Result := ', total network cost is ' + IntToStr (NetCost);
  Result := Result + ', power consumption of equipment is ' + IntToStr (NetPower) + ' watts';
  Result := Result + ', weight of equipment is ' + FloatToStrF (NetWeight, ffFixed, 0, 1) + ' kilograms';
  Result := Result + ', size of equipment is ' + IntToStr (NetEquipmentSize) + ' units';

  // Info on objective function
  Result := Result + ', objective function is ' + FloatToStrF (ObjectiveFunction, ffFixed, 0, 1);
end;

function TNetworkConfig.GetObjectiveFunction: Single;
var
  I: Integer;
begin
  I := IndexOfName (cNetworkObjectiveFunction);
  if I >= 0 then
    begin
      Result := GenericStrToFloat (ValueFromIndex [I]);  // Already calculated, exit
      Exit;
    end;

  // Otherwise, it was not calculated yet
  {
        CURRENTLY, OBJECTIVE FUNCTION IS NETWORK COST!
  }
  Result := NetCost;

  SetObjectiveFunction (Result);                         // Save result for future reuse
end;

function TNetworkConfig.CSVHeader: String;
const
  Sep = ', ';
var
  I: Integer;
begin
  Result := '';
  if Count > 0 then
  begin
    // Add names, one by one
    for I := 0 to Count-1 do
      Result := Result + Names[I] + Sep;
    // Delete the trailing separator
    System.Delete (Result, Length (Result) - Length (Sep) + 1, Length (Sep));
  end;
end;

function TNetworkConfig.CSVDescription: String;
const
  Sep = ', ';
var
  I: Integer;
begin
  Result := '';
  if Count > 0 then
  begin
    // Add values, one by one
    for I := 0 to Count-1 do
      Result := Result + ValueFromIndex[I] + Sep;
    // Delete the trailing separator
    System.Delete (Result, Length (Result) - Length (Sep) + 1, Length (Sep));
  end;
end;

function TNetworkConfig.CSVDescriptionWithHeader: String;
begin
  // Consists of two lines: the header and the description itself
  Result := CSVHeader + LineEnding + CSVDescription;
end;

function TNetworkConfig.NVPDescription: String;
begin
  Sort;
  Result := Text;
end;

procedure TNetworkConfig.Assign (Source: TPersistent);
// Used to duplicate an object
begin
  if Source is TNetworkConfig then
    AddStrings (TNetworkConfig (Source))
  else
    inherited Assign (Source);
end;

{ TNetworkConstraints }

constructor TNetworkConstraints.Create;
begin
  inherited Create;
  // Initially, constraints are not yet checked. Will be set to "True" after checking
  FAlreadyChecked := False;
end;

function TNetworkConstraints.CheckNodes(out Nodes: Integer; var Err: TStringList): Boolean;
// Get the number of nodes, "N"
var
  I: Integer;
begin
  Result := False;   // Prepare for the worst
  I := IndexOfName (cNodes);
  if I = -1 then
  begin
    Err.Add ('Error: ' + cNodes + ' (the number of nodes) must be assigned!');
    Exit;
  end;
  try  // Try to convert
    Nodes := StrToInt (ValueFromIndex [I]);
  except
    on EConvertError do
    begin
      Err.Add ('Error: Invalid number encountered when parsing "' + cNodes + '"');
      Exit;
    end;
  end; { try }
  if (Nodes < 1) then
  begin
    Err.Add ('Error: ' + cNodes + ' (the number of nodes) must be >= than 1!');
    Exit;
  end;

  // If we reached here, then "Nodes" is valid. Return success.
  Result := True;
end;

function TNetworkConstraints.CheckNodesFutureMax(out NodesFutureMax: Integer; var Err: TStringList): Boolean;
// Get the maximum future number of nodes, "Nmax". If not specified, assume Nmax=N
var
  I: Integer;
  Nodes: Integer;
begin
  Result := False;                          // Prepare for the worst

  CheckNodes (Nodes, Err);                  // First of all, we need to get "Nodes": it will be used as a default value for NodesFutureMax

  I := IndexOfName (cNodesFutureMax);
  if I = -1 then                            // If not specified, then equals to Nodes
    NodesFutureMax := Nodes
  else
  if ValueFromIndex [I] = '' then           // Was specified, but holds an empty value
    NodesFutureMax := Nodes
  else
  begin                                     // Try to convert
    try
      NodesFutureMax := StrToInt (ValueFromIndex [I]);
    except
      on EConvertError do
      begin
        Err.Add ('Error: Invalid number encountered when parsing "' + cNodesFutureMax + '"');
        Exit;
      end;
    end; { try }

    // If zero is specified, then also equals to Nodes
    if NodesFutureMax = 0 then
      NodesFutureMax := Nodes;

    // Further check: NodesFutureMax must be >= Nodes
    if not (NodesFutureMax >= Nodes) then
    begin
      Err.Add ('Error: ' + cNodesFutureMax + ' (the maximum future number of nodes) must be bigger than or equal to ' + cNodes + '!');
      Exit;
    end;
  end;

  // If we reached here, then "NodesFutureMax" is valid. Save it and return success.
  Values [cNodesFutureMax] := IntToStr (NodesFutureMax);
  Result := True;
end;

function TNetworkConstraints.CheckBlockingFactor (out Bl: Single;
  var Err: TStringList): Boolean;
// Get the blocking factor, "Bl"
var
  I: Integer;
begin
  Result := False;                                 // Prepare for the worst
  I := IndexOfName (cMaxNetworkBlockingFactor);
  if I = -1 then
    Bl := 1                                        // If not assigned, equals to "1" (non-blocking network)
  else
  if ValueFromIndex [I] = '' then                  // Was specified, but holds an empty value
    Bl := 1
  else
    try                                            // Try to convert
      Bl := GenericStrToFloat (ValueFromIndex [I]);
    except
      on EConvertError do
      begin
        Err.Add ('Error: Invalid number encountered when parsing "' + cMaxNetworkBlockingFactor + '"');
        Exit;
      end;
    end; { try }
  if (Bl < 1) then
  begin
    Err.Add ('Error: ' + cMaxNetworkBlockingFactor + ' (the blocking factor) must be >= than 1!');
    Exit;
  end;

  // If we reached here, then "Bl" is valid. Save it and return success.
  Values [cMaxNetworkBlockingFactor] := FloatToStrF (Bl, ffFixed, 0, 1);
  Result := True;
end;

function TNetworkConstraints.CheckMaxNetworkCost (out MaxNetworkCost: Integer;
  var Err: TStringList): Boolean;
// Get the maximum network cost
var
  I: Integer;

begin
  Result := False;                                 // Prepare for the worst
  I := IndexOfName (cMaxNetworkCost);
  if (I = -1) or (ValueFromIndex [I] = '') then    // If not specified (or value is empty)
  begin
    MaxNetworkCost := 0;
    // Save "0" in the list of constraints
    Values [cMaxNetworkCost] := '0';
  end
  else                                             // Was specified and non-empty
  begin
    try                                            // Try to convert and save the result
      MaxNetworkCost := StrToInt (ValueFromIndex [I]);
      Values [cMaxNetworkCost] := ValueFromIndex [I];
    except
      on EConvertError do
      begin
        Err.Add ('Error: Invalid number encountered when parsing "' + cMaxNetworkCost + '"');
        Exit;
      end;
    end; { try }
    // Check after conversion
    if MaxNetworkCost < 0 then
    begin
      Err.Add ('Error: ' + cMaxNetworkCost + ' (maximum network cost) must be >= than 0!');
      Exit;
    end;
  end;

  // If we reached here, then "MaxNetworkCost" is valid. Return success.
  Result := True;
end;

function TNetworkConstraints.CheckMaxNetworkPower(out MaxNetworkPower: Integer;
  var Err: TStringList): Boolean;
// Get the maximum network equipment power, in watts
var
  I: Integer;

begin
  Result := False;                                 // Prepare for the worst
  I := IndexOfName (cMaxNetworkPower);
  if (I = -1) or (ValueFromIndex [I] = '') then    // If not specified (or value is empty)
  begin
    MaxNetworkPower := 0;
    // Save "0" in the list of constraints
    Values [cMaxNetworkPower] := '0';
  end
  else                                             // Was specified and non-empty
  begin
    try                                            // Try to convert and save the result
      MaxNetworkPower := StrToInt (ValueFromIndex [I]);
      Values [cMaxNetworkPower] := ValueFromIndex [I];
    except
      on EConvertError do
      begin
        Err.Add ('Error: Invalid number encountered when parsing "' + cMaxNetworkPower + '"');
        Exit;
      end;
    end; { try }

    // Check after conversion
    if MaxNetworkPower < 0 then
    begin
      Err.Add ('Error: ' + cMaxNetworkPower + ' (maximum network power) must be >= than 0!');
      Exit;
    end;
  end;

  // If we reached here, then "MaxNetworkPower" is valid. Return success.
  Result := True;
end;

function TNetworkConstraints.CheckMaxNetworkWeight(out MaxNetworkWeight: Single;
  var Err: TStringList): Boolean;
// Get the maximum network equipment weight, in kilograms
var
  I: Integer;

begin
  Result := False;                                 // Prepare for the worst
  I := IndexOfName (cMaxNetworkWeight);
  if (I = -1) or (ValueFromIndex [I] = '') then    // If not specified (or value is empty)
  begin
    MaxNetworkWeight := 0;
    // Save "0" in the list of constraints
    Values [cMaxNetworkWeight] := '0';
  end
  else                                             // Was specified and non-empty
  begin
    try                                            // Try to convert and save the result
      MaxNetworkWeight := GenericStrToFloat (ValueFromIndex [I]);
      Values [cMaxNetworkWeight] := ValueFromIndex [I];
    except
      on EConvertError do
      begin
        Err.Add ('Error: Invalid number encountered when parsing "' + cMaxNetworkWeight + '"');
        Exit;
      end;
    end; { try }

    // Check after conversion
    if MaxNetworkWeight < 0 then
    begin
      Err.Add ('Error: ' + cMaxNetworkWeight + ' (maximum network equipment weight) must be >= than 0!');
      Exit;
    end;
  end;

  // If we reached here, then "MaxNetworkWeight" is valid. Return success.
  Result := True;
end;

function TNetworkConstraints.CheckMaxNetworkEquipmentSize (out
  MaxNetworkEquipmentSize: Integer; var Err: TStringList): Boolean;
// Get the maximum network equipment size, in units
var
  I: Integer;

begin
  Result := False;                                 // Prepare for the worst
  I := IndexOfName (cMaxNetworkEquipmentSize);
  if (I = -1) or (ValueFromIndex [I] = '') then    // If not specified (or value is empty)
  begin
    MaxNetworkEquipmentSize := 0;
    // Save "0" in the list of constraints
    Values [cMaxNetworkEquipmentSize] := '0';
  end
  else                                             // Was specified and non-empty
  begin
    try                                            // Try to convert and save the result
      MaxNetworkEquipmentSize := StrToInt (ValueFromIndex [I]);
      Values [cMaxNetworkEquipmentSize] := ValueFromIndex [I];
    except
      on EConvertError do
      begin
        Err.Add ('Error: Invalid number encountered when parsing "' + cMaxNetworkEquipmentSize + '"');
        Exit;
      end;
    end; { try }

    // Check after conversion
    if MaxNetworkEquipmentSize < 0 then
    begin
      Err.Add ('Error: ' + cMaxNetworkEquipmentSize + ' (maximum network equipment size) must be >= than 0!');
      Exit;
    end;
  end;

  // If we reached here, then "MaxNetworkEquipmentSize" is valid. Return success.
  Result := True;
end;

function TNetworkConstraints.Check (var Err: TStringList): Boolean;
{ Checks constraints one by one for validity -- that is, that numbers are supplied,
  and their values are non-negative, and so on.
  If any erroneous input is found, the error message is automatically saved in "DebugOutput". }
var
  Nodes, NodesFutureMax: Integer;        // Number of nodes in a network; the current one and the future maximum
  RequestedBlockingFactor: Single;       // Blocking factor requested by the user
  MaxNetworkCost: Integer;               // Maximum network cost
  MaxNetworkPower: Integer;              // Maximum network equipment power, in watts
  MaxNetworkWeight: Single;              // Maximum network equipment weight, in kilograms
  MaxNetworkEquipmentSize: Integer;      // Maximum network equipment size, in rack-mount units
begin
  // The following statement:
  // (1) Sets the values of all variables (Nodes, ...)
  // (2) If erroneous input is detected, saves the error message to "Err"
  Result := (
    CheckNodes (Nodes, Err) and
    CheckNodesFutureMax (NodesFutureMax, Err) and
    CheckBlockingFactor (RequestedBlockingFactor, Err) and
    CheckMaxNetworkCost (MaxNetworkCost, Err) and
    CheckMaxNetworkPower (MaxNetworkPower, Err) and
    CheckMaxNetworkWeight (MaxNetworkWeight, Err) and
    CheckMaxNetworkEquipmentSize (MaxNetworkEquipmentSize, Err));

  // If unsuccessful, exit (error output was already saved to "Err")
  if not Result then
    Exit;

  // If we reached here, the check was successful. Save this in a flag
  FAlreadyChecked := True;

  // Now, all values of variables were set. Produce the textual overview of constraints
  AddOverview ('Building a network of ' + IntToStr (Nodes) + ' nodes, expandable to ' +
    IntToStr (NodesFutureMax) + ' nodes');
  AddOverview ('Requested blocking factor is ' + FloatToStrF (RequestedBlockingFactor, ffFixed, 0, 1));

  if MaxNetworkCost = 0 then
    AddOverview ('Maximum cost of network is not limited')
  else
    AddOverview ('Maximum cost of network is ' + IntToStr (MaxNetworkCost));

  if MaxNetworkPower = 0 then
    AddOverview ('Maximum power of network equipment is not limited')
  else
    AddOverview ('Maximum power of network equipment is ' + IntToStr (MaxNetworkPower));

  if MaxNetworkWeight = 0 then  // XXX: Remember, the type is "Single", but we use "MaxLongInt", because there is no "MaxSingle"
    AddOverview ('Maximum weight of network equipment is not limited')
  else
    AddOverview ('Maximum weight of network equipment is ' + FloatToStrF (MaxNetworkWeight, ffFixed, 0, 1));

  if MaxNetworkEquipmentSize = 0 then
    AddOverview ('Maximum size of network equipment is not limited')
  else
    AddOverview ('Maximum size of network equipment is ' + IntToStr (MaxNetworkEquipmentSize) + ' units');
end;

function TNetworkConstraints.GetNodes: Integer;
begin
  Assert (FAlreadyChecked); // Make sure that checks were already done!
  Result := StrToInt (Values [cNodes]);
end;

function TNetworkConstraints.GetNodesFutureMax: Integer;
begin
  Assert (FAlreadyChecked);
  Result := StrToInt (Values [cNodesFutureMax]);
end;

function TNetworkConstraints.GetBlockingFactor: Single;
begin
  Assert (FAlreadyChecked);
  Result := GenericStrToFloat (Values [cMaxNetworkBlockingFactor]);
end;

function TNetworkConstraints.GetMaxNetworkCost: Integer;
begin
  Assert (FAlreadyChecked);
  Result := StrToInt (Values [cMaxNetworkCost]);
end;

function TNetworkConstraints.GetMaxNetworkPower: Integer;
begin
  Assert (FAlreadyChecked);
  Result := StrToInt (Values [cMaxNetworkPower]);
end;

function TNetworkConstraints.GetMaxNetworkWeight: Single;
begin
  Assert (FAlreadyChecked);
  Result := GenericStrToFloat (Values [cMaxNetworkWeight]);
end;

function TNetworkConstraints.GetMaxNetworkEquipmentSize: Integer;
begin
  Assert (FAlreadyChecked);
  Result := StrToInt (Values [cMaxNetworkEquipmentSize]);
end;

procedure TNetworkConstraints.AddOverview (S: String);
// Adds to the current textual overview of constraints a specified string and a line ending
begin
  FDescription += S + LineEnding;
end;

{ TSwitch }

function TSwitch.Model: String;
var
  I: Integer;
begin
  I := IndexOfName (cSwitchModel);
  Assert (I >= 0);
  Result := ValueFromIndex [I];
end;

function TSwitch.P: Integer;
var
  I: Integer;
begin
  I := IndexOfName (cSwitchPortCount);
  Assert (I >= 0);
  Result := StrToInt (ValueFromIndex [I]);
end;

function TSwitch.Cost: Integer;
var
  I: Integer;
begin
  I := IndexOfName (cSwitchCost);
  Assert (I >= 0);
  Result := StrToInt (ValueFromIndex [I]);
end;

function TSwitch.Size: Integer;
var
  I: Integer;
begin
  I := IndexOfName (cSwitchSize);
  Assert (I >= 0);
  Result := StrToInt (ValueFromIndex [I]);
end;

function TSwitch.Weight: Single;
var
  I: Integer;
begin
  I := IndexOfName (cSwitchWeight);
  Assert (I >= 0);
  Result := GenericStrToFloat (ValueFromIndex [I]);
end;

function TSwitch.Power: Integer;
var
  I: Integer;
begin
  I := IndexOfName (cSwitchPower);
  Assert (I >= 0);
  Result := StrToInt (ValueFromIndex [I]);
end;

initialization
  DefaultFormatSettings.DecimalSeparator := '.';

end.


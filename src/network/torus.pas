{
Copyright (C) 2012, 2013 Konstantin S. Solnushkin

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
unit torus;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math,
  common, database, floatstr;               // Our own units

const
  { Topology names }
  cTorus = 'torus';
  cRing = 'ring';

  { File names }

  // Generic files
  cFileShowDatabaseRuler = 'show-database-ruler.html';
  cFileShowDatabaseItem = 'show-database-item.html';
  cFileShowDatabaseFooter = 'show-database-footer.html';
  // Files specific to tori
  cFileShowDatabaseHeader = 'torus-show-database-header.html';
  cFileShowDatabaseTitle = 'torus-show-database-title.html';
  cFileResults = 'torus-results.html';
  // Switch databases
  cFileSwitchDB = 'torus.csv';

  { String constants for network characteristics }
  cNetworkSwitchCount = 'network_switch_count';
  cNetworkSwitchModel = 'network_switch_model';
  cNetworkTorusDimensionsCount = 'network_torus_dimensions_count';
  cNetworkTorusDimensions = 'network_torus_dimensions';
  cNetworkPortsToNodes = 'network_ports_to_nodes';
  cNetworkPortsToNeighbours = 'network_ports_to_neighbours';

type

  { TTorusNetworkConfig }

  TTorusNetworkConfig = class (TNetworkConfig)
    protected
      function GetDim: Integer;
      function GetDimStr: String;
      function GetPortsToNodes: Integer;
      function GetPortsToNeighbours: Integer;
      function GetBlockingFactor: Single; override;
      function GetNetCost: Integer; override;
      function GetNetPower: Integer; override;
      function GetNetWeight: Single; override;
      function GetNetEquipmentSize: Integer; override;
      function GetSwitchCount: Integer;
      function GetSwModelStr: String;
      procedure SetDim (const AValue: Integer);
      procedure SetDimStr(const AValue: String);
      procedure SetPortsToNodes(const AValue: Integer);
      procedure SetPortsToNeighbours(const AValue: Integer);
      procedure SetSwitchCount (const AValue: Integer);
      procedure SetSwModelStr(const AValue: String);
    public
      SwModel: TSwitch;                                                           // Model of the switch
      property SwCount: Integer read GetSwitchCount;                              // No. of switches (read-only, set automatically when you read it)
      property PortsToNodes: Integer read GetPortsToNodes write SetPortsToNodes;
      property PortsToNeighbours: Integer read GetPortsToNeighbours write SetPortsToNeighbours;
      property Dim: Integer read GetDim;                                          // The number of torus dimensions (read-only, use AddDim to add)
      property DimStr: String read GetDimStr write SetDimStr;                     // String that represents dimensions
      property SwModelStr: String read GetSwModelStr write SetSwModelStr;         // Model of the switch
      procedure AddDim (Side: Integer);                                           // Adds a dimension to the torus
      procedure ClearDim;                                                         // Resets the number of dimensions to zero
      function Description: String; override;                                     // Human-readable description
      function HtmlDescription: String; override;                                 // Fully-fledged description in HTML
      procedure Assign (Source: TPersistent); override;                           // Used to create copies of objects
  end;

procedure LoadTorusDatabase (const Vendor: String; var L: TSwitchList);

function FormatTorusDatabase(const Vendor: String): String;

procedure DesignTorus (const Vendor: String; var Constraints: TStringList; Debug: Boolean;
  var DebugOutput: TStringList; out BestNetConfig: TNetworkConfig);

implementation

procedure LoadTorusDatabase (const Vendor: String; var L: TSwitchList);
var
  Dir: String;
begin
  // Loads the database (same directory as the executable file + subdirectory for topology + subdirectory for vendor)
  Dir := ExtractFilePath (ParamStr(0)) + cDatabaseDir + DirectorySeparator +
    cTorus + DirectorySeparator + Vendor + DirectorySeparator;
  LoadSwitchDBFromCSVFile (L, Dir + cFileSwitchDB);
end;

function FormatTorusDatabase(const Vendor: String): String;
var
  MyTemplate: TStringList;
  L: TSwitchList;
  Sw: TSwitch;
  I: Integer;
  HorizontalRuler, SwitchItemStr: String;   // String to hold HTML templates loaded from disk
begin
  MyTemplate := TStringList.Create;
  L := TSwitchList.Create;

  // Load the database of switches
  LoadTorusDatabase(Vendor, L);

  // Load a horizontal ruler template, to be used later
  MyTemplate.LoadFromFile (ExtractFilePath(ParamStr(0)) + cFileShowDatabaseRuler);
  HorizontalRuler := MyTemplate.Text;

  // Load a switch item template, to be used later
  MyTemplate.LoadFromFile (ExtractFilePath(ParamStr(0)) + cFileShowDatabaseItem);
  SwitchItemStr := MyTemplate.Text;

  // Start to form the response
  // Load header and output it
  MyTemplate.LoadFromFile (ExtractFilePath(ParamStr(0)) + cFileShowDatabaseHeader);
  Result := MyTemplate.Text;                                                      // No elements to format in the header; add "as is"

  // Load title template and output it
  MyTemplate.LoadFromFile (ExtractFilePath(ParamStr(0)) + cFileShowDatabaseTitle);
  Result += Format (MyTemplate.Text, [L.Count]);                                  // Number of switches

  // Add a horizontal ruler
  Result += HorizontalRuler;

  // Output switches
  for I := 0 to L.Count-1 do
  begin
    Sw := L.Objects[I] as TSwitch;
    Result += Format (SwitchItemStr, [Sw.Model, Sw.P, Sw.Size, Sw.Weight, Sw.Power, Sw.Cost]) + HorizontalRuler;
  end;

  // Load footer and output it
  MyTemplate.LoadFromFile (ExtractFilePath(ParamStr(0)) + cFileShowDatabaseFooter);
  Result += MyTemplate.Text;                                                      // No elements to format in the footer; add "as is"

  // Free all objects
  L.Free;
  MyTemplate.Free;
end;

procedure DesignTorus (const Vendor: String; var Constraints: TStringList; Debug: Boolean;
  var DebugOutput: TStringList; out BestNetConfig: TNetworkConfig);
(* Design torus networks (possibly blocking).
   "Constraints" defines design constraints, such as "nodes" (the number of nodes to be interconnected), "nodes-future-max" (the maximum
   number of nodes the network has to support in the future), "max-network-cost" (the maximum allowed cost), etc.
   If "Debug" is set, then debug messages are saved in "DebugOutput".
   Error messages go there anyway.
   If successful, then "BestNetConfig" is assigned the best network configuration. Otherwise, "Nil" is returned in "BestNetConfig"*)

var
  L: TSwitchList;
  NetConfig: TTorusNetworkConfig;      // A pointer to a network configuration
  NetConstraints: TNetworkConstraints; // List to hold constraints
  ConfigList: TConfigList;               // List to hold pointers to network configurations
  Description: String;                   // Textual description of a network configuration
  WhyDiscarded: String;                  // Textual reason why the config was discarded
  // Variables to hold constraints:
  RequestedBlockingFactor: Single;       // Blocking factor requested by the user

  procedure DoDebugOutput (const S: String);
  // Adds debug output, but only if "Debug" is set
  begin
    if Debug then
      DebugOutput.Add (S);
  end;

  procedure DoErrorOutput (const S: String);
  // Adds error output, no matter if "Debug" is set or not
  begin
    DebugOutput.Add (S);
  end;

  function GetConstraints: Boolean;
  // Extracts values of design constraints from the list and checks them for consistency:
  // numerical values must be convertible into numbers, and negative values must not be specified, etc.
  // If all checks passed, returns "True"; otherwise, an error message gets recorded to "DebugOutput".
  // In either case, if debug output was requested, the overview of constraints is produced in "DebugOutput".
  begin
    // Hope for the best, but prepare for the worst
    Result := False;

    // Populate the list with the constraints that we were passed
    NetConstraints.Assign (Constraints);

    try
      // Perform the check; "DebugOutput" will be set if necessary
      if not NetConstraints.Check (DebugOutput) then
        Exit;
      // Produce debug output with the review of design constraints
      if Debug then
        DoDebugOutput (NetConstraints.Overview);
      // Get values of those constraints
      with NetConstraints do
      begin
        RequestedBlockingFactor := GetBlockingFactor;
      end;
    finally
      // Save the processed constraints
      Constraints.Assign (NetConstraints);
    end;

    // If we reached here, then we passed all checks
    Result := True;
  end;

  procedure BuildTorusNetwork;
  // Actually designs a torus network
  var
    I, J: Integer;
    Sw: TSwitch;
    SuggestedDims: Integer;     // How many dimensions the heuristic suggests to have
    MinSwCount: Integer;        // The minimum number of switches required (but which results in unbalanced tori)
    Side: Integer;              // Number of switches along a particular dimension of a torus
    RemainingSwCount: Integer;  // How many switches remain to be distributed in other dimensions

    function GetDimCount (Switches: Integer): Integer;
    { Receives the number of switches in the torus network, returns the recommended number of dimensions.
      This is essentially a heuristics. If you want to change the behaviour of the entire module,
      change this function. }
    begin
      if Switches <= 3 then
        Result := 1                     // 1D torus is a ring. Max: 1x3
      else
      if Switches <= 36 then
        Result := 2                     // 2D torus is, ideally, a square. Max: 6x6
      else
      if Switches <= 125 then
        Result := 3                     // 3D torus is, ideally, a cube. Max: 5x5x5 ("Gordon" in SDSC uses 4x4x4=64 switches)
      else
      if Switches <= 2401 then
        Result := 4                     // 4D torus. Max: 7x7x7x7
      else
        Result := 5;                    // 5D torus, such as one used in "IBM Sequoia" (their torus is 16x16x16x12x2)
    end;

  begin
    DoDebugOutput (LineEnding + 'Module for torus networks:');

    // Search through the list of switches
    for I := 0 to L.Count-1 do
    begin
      // Fetch the switch
      Sw := L.Objects[I] as TSwitch;

      // Create an object to store the network config
      NetConfig := TTorusNetworkConfig.Create;
      NetConfig.AddStrings (NetConstraints);     // Add constraints to the new config

      // Two alternatives exist:
      // 1. If a switch contains P>=Nodes ports, it is usable to build a star network, which is a trivial case
      if Sw.P >= NetConfig.Nodes then            // A switch usable for a star network is found
      begin
        with NetConfig do                        // Fill out network characteristics
        begin
          NetTopology := cStar;                  // Set the topology
          AddDim (1);                            // In a star network there is only one switch
          SwModel := Sw;                         // Its model

          DoDebugOutput (LineEnding + 'Trying switch "' + Sw.Model + '" with ' + IntToStr (Sw.P) + ' ports');
          DoDebugOutput ('Need ' + IntToStr (SwCount) + ' switches');
          DoDebugOutput ('Will use "' + NetTopology + '" topology');

          // Need to set some values, since they are used in HTML output template
          Bl := 1;                               // Star networks are non-blocking, hence "1" here
          PortsToNodes := Nodes;
          PortsToNeighbours := 0;
          L := Nodes;                            // As many cables as we have nodes
          SparePorts := SwModel.P - Nodes;       // These ports are spare
          ExpandableTo := SwModel.P;             // Can have this many ports
          // Perform dummy calls of these properties just to calculate their values:
          SwModelStr;
          NetCost;
          NetPower;
          NetWeight;
          NetEquipmentSize;
        end;
        Description := '> Trivial case (' + NetConfig.Description + ')';
      end
      else
      // 2. Second alternative: not enough ports to form a star network, so will build a true torus network instead
      begin
        with NetConfig do                        // Fill out network characteristics
        begin
          SwModel := Sw;                         // Switch model
          // Calculate the number of ports on every switch going to nodes and to neighbouring switches.
          // Borrowed from the fat-tree design procedure.
          PortsToNodes := Trunc (Sw.P * (RequestedBlockingFactor / (1 + RequestedBlockingFactor)));
          PortsToNeighbours := Sw.P - PortsToNodes;
          Bl := PortsToNodes / PortsToNeighbours;
          // Calculate the number of switches required to accommodate all nodes
          MinSwCount := Ceil (Nodes / PortsToNodes);
          // Dimensions required
          SuggestedDims := GetDimCount (MinSwCount);

          if SuggestedDims = 1 then { A ring, which is a degenerate torus }
          begin
            NetTopology := cRing;
            AddDim (MinSwCount);   // Only one dimension, with this number of switches
            DoDebugOutput (LineEnding + 'Trying switch "' + Sw.Model + '" with ' + IntToStr (Sw.P) + ' ports');
            DoDebugOutput ('Need ' + IntToStr (MinSwCount) + ' switches');
            DoDebugOutput ('Will use "' + NetTopology + '" topology');
          end
          else
          if SuggestedDims >= 2 then { A real torus }
          begin
            NetTopology := cTorus;                 // Set the topology
            // First dimension of a torus has this length (we take the root of "SuggestedDims"-th degree)
            Side := Round (MinSwCount ** (1 / SuggestedDims));

            DoDebugOutput (LineEnding + 'Trying switch "' + Sw.Model + '" with ' + IntToStr (Sw.P) + ' ports');
            DoDebugOutput ('Minimum required number of switches is ' + IntToStr (MinSwCount));
            DoDebugOutput ('Will use "' + NetTopology + '" topology');
            DoDebugOutput ('Using ' + IntToStr (SuggestedDims) + ' dimensions.');
            // All dimensions, up to the last one, are the same
            for J := 1 to SuggestedDims - 1 do
              AddDim (Side);
            // The last dimension is whatever remains
            RemainingSwCount := Ceil (MinSwCount / Power (Side, SuggestedDims - 1));
            AddDim (RemainingSwCount);
            DoDebugOutput ('Dimensions: ' + DimStr);
            DoDebugOutput ('Actual number of switches is ' + IntToStr (SwCount));
          end { Torus }
          else // SuggestedDims <= 0, should never reach here
            Raise Exception.Create ('Problem calculating the number of torus dimensions');

          { Switches in a torus network can be interconnected in an arbitrary way. Every switch has "PortsToNeighbours"
            ports connected to other switches. Therefore, there are "SwCount * PortsToNeighbours" internal ports in this
            network. But each two ports need one cable to interconnect them. Therefore the number of cables required
            to connect switches together is "SwCount * PortsToNeighbours / 2". Also, nodes need "Nodes" cables to connect
            to the switches. Hence the formula for "L" below: }
          L := Nodes + SwCount * PortsToNeighbours div 2;

          SparePorts := SwCount * PortsToNodes - Nodes;       // These ports are spare
          ExpandableTo := SwCount * PortsToNodes;             // Can have this many ports
          // Perform dummy calls of these properties just to calculate their values:
          SwModelStr;
          NetCost;
          NetPower;
          NetWeight;
          NetEquipmentSize;
        end;
        Description := '> Torus (' + NetConfig.Description + ')';
      end;

      // Check the resulting configuration for constraints
      if NetConfig.CheckForConstraints (NetConstraints, WhyDiscarded) then
      begin
        DoDebugOutput (Description + ', added');
        ConfigList.AddObject ('A network, with ' + Sw.Model, NetConfig);  // Add to the list of good configs
      end
      else
      begin
        DoDebugOutput (Description + ', discarded, due to: ' + WhyDiscarded + '.');
        NetConfig.Free;                                                   // This config failed; free its memory
      end;
    end;
  end;

  begin
    Assert (Assigned (DebugOutput));
    DebugOutput.Clear;                     // Debug output will be saved here

    BestNetConfig := Nil;                  // Prepare for the worst: if we are unsuccessful, Nil is returned, indicating an error

    NetConstraints := TNetworkConstraints.Create; // We will store network constraints here

    if not GetConstraints then             // Extract design constraints from the list; if failed then exit
      Exit;                                // Error output should have been already prerformed in "GetConstraints"

    ConfigList := TConfigList.Create;      // We will store network configurations here

    L := TSwitchList.Create;
    LoadTorusDatabase (Vendor, L);         // Load the database of switches

    BuildTorusNetwork;                     // Try to design a torus network

    if Debug then
      DoDebugOutput (LineEnding + 'Searching through ' + IntToStr (ConfigList.Count) + ' configurations' + LineEnding);

    NetConfig :=  ConfigList.BestConfig as TTorusNetworkConfig;   // Find the best configuration
    if Assigned (NetConfig) then           // If found, return its description by creating a clone of that object (to release all other memory)
    begin
      BestNetConfig := TTorusNetworkConfig.Create;    // Create a new, clean object
      BestNetConfig.Assign (NetConfig);               // and assign its fields
    end
    else
      DoErrorOutput ('Error: No torus network configurations found matching your constraints.');

    // Free all objects
    ConfigList.Free;                       // Free the list of configurations _AND_ free the configurations, too
    NetConstraints.Free;
    L.Free;
end;

{ TTorusNetworkConfig }

function TTorusNetworkConfig.GetDim: Integer;
var
  I: Integer;
begin
  I := IndexOfName (cNetworkTorusDimensionsCount);
  Assert (I >= 0);
  Result := StrToInt (ValueFromIndex [I]);
end;

function TTorusNetworkConfig.GetDimStr: String;
var
  I: Integer;
begin
  I := IndexOfName (cNetworkTorusDimensions);
  if I >= 0 then
    Result := ValueFromIndex [I]
  else
    Result := ''; // Not calculated yet
end;

function TTorusNetworkConfig.GetPortsToNodes: Integer;
var
  I: Integer;
begin
  I := IndexOfName (cNetworkPortsToNodes);
  Assert (I >= 0);
  Result := StrToInt (ValueFromIndex [I]);
end;

function TTorusNetworkConfig.GetPortsToNeighbours: Integer;
var
  I: Integer;
begin
  I := IndexOfName (cNetworkPortsToNeighbours);
  Assert (I >= 0);
  Result := StrToInt (ValueFromIndex [I]);
end;

function TTorusNetworkConfig.GetBlockingFactor: Single;
var
  I: Integer;
begin
  I := IndexOfName (cNetworkBlockingFactor);
  if I >= 0 then
    begin
      Result := GenericStrToFloat (ValueFromIndex [I]);  // Already calculated, exit
      Exit;
    end;

  // Otherwise, it was not calculated yet
  if Dim > 0 then
    Result := PortsToNodes / PortsToNeighbours
  else
    Result := 1;               // Otherwise, this is a star network, and we return "1" to indicate that it is always non-blocking

  SetBlockingFactor (Result);  // Save result for future reuse
end;

function TTorusNetworkConfig.GetNetCost: Integer;
var
  I: Integer;
begin
  I := IndexOfName (cNetworkCost);
  if I >= 0 then
    begin
      Result := StrToInt (ValueFromIndex [I]);  // Already calculated, exit
      Exit;
    end;

  // Otherwise, it was not calculated yet
  Result := SwCount * SwModel.Cost + L * CableCost;    // Switches and cables

  SetNetCost(Result);                           // Save result for future reuse
end;

function TTorusNetworkConfig.GetNetPower: Integer;
var
  I: Integer;
begin
  I := IndexOfName (cNetworkPower);
  if I >= 0 then
    begin
      Result := StrToInt (ValueFromIndex [I]);  // Already calculated, exit
      Exit;
    end;

  // Otherwise, it was not calculated yet
  Result := SwCount * SwModel.Power;            // Switches

  SetNetPower (Result);                         // Save result for future reuse
end;

function TTorusNetworkConfig.GetNetWeight: Single;
var
  I: Integer;
begin
  I := IndexOfName (cNetworkWeight);
  if I >= 0 then
    begin
      Result := GenericStrToFloat (ValueFromIndex [I]);  // Already calculated, exit
      Exit;
    end;

  // Otherwise, it was not calculated yet
  Result := SwCount * SwModel.Weight;           // Switches

  SetNetWeight (Result);                        // Save result for future reuse
end;

function TTorusNetworkConfig.GetNetEquipmentSize: Integer;
// Size of network hardware, in rack mount units
var
  I: Integer;
begin
  I := IndexOfName (cNetworkEquipmentSize);
  if I >= 0 then
    begin
      Result := StrToInt (ValueFromIndex [I]);  // Already calculated, exit
      Exit;
    end;

  // Otherwise, it was not calculated yet
  Result := SwCount * SwModel.Size;             // Size of switches

  SetNetEquipmentSize (Result);                 // Save result for future reuse

  // Without much ado and OOP paradigms, methods and properties,
  // add one more metric with switch size:
  Values [cNetworkEdgeSwitchSize] := IntToStr (SwModel.Size);
end;

function TTorusNetworkConfig.GetSwitchCount: Integer;
{ Uses internal torus representation (AxBxC) to calculate the number of switches.
  Relies on a fact that torus dimensions are separated with 'x' }
var
  I, PrevSep: Integer;
  S, NumStr: String;
begin
  // Fetch the string that represents torus dimensions (such as "5x5x6")
  S := DimStr;
  Result := 1;
  // A trick is to add an extra separator in the beginning of the string, so that it is always found
  S := 'x' + S;
  PrevSep := Length (S) + 1;              // Initial value of the "previously found" separator
  for I := Length (S) downto 1 do         // Step through characters in the string, starting from the tail
  begin
    if S[I] = 'x' then                    // Separator is found
    begin
      // Get the number between two separators: current (I) and previous (PrevSep)
      NumStr := Copy (S, I + 1, PrevSep - I - 1);
      // This is the side of the torus, multiply result by it
      Result *= StrToInt (NumStr);
      // Previous separator is now the current one
      PrevSep := I;
    end;
  end;
  // Before exiting, save the value
  SetSwitchCount (Result);
end;

function TTorusNetworkConfig.GetSwModelStr: String;
{ During execution of the program, the object "Sw" -- the switch -- might not
  already exist in memory. Still we need to have a string representation of the
  switch's model name. Here is when this function comes to rescue. We remember the
  name of the switch model for future use. }
var
  I: Integer;
begin
  I := IndexOfName (cNetworkSwitchModel);
  if I >= 0 then
    begin
      Result := ValueFromIndex [I];  // Already calculated, exit
      Exit;
    end;

  // Otherwise, it was not calculated yet
  Result := SwModel.Model;

  SetSwModelStr (Result);                           // Save result for future reuse
end;

procedure TTorusNetworkConfig.SetDim (const AValue: Integer);
begin
  Values [cNetworkTorusDimensionsCount] := IntToStr (AValue);
end;

procedure TTorusNetworkConfig.SetDimStr (const AValue: String);
var
  S: String;
  I, Dims: Integer;
begin
  if AValue = '' then  // Clear
    SetDim (0)
  else                 // Non-empty value specified
  begin
    // Make sure we only receive numbers separated by an 'x' character
    S := AValue;
    Dims := 1;
    // Leading and trailing characters must not be equal to 'x'
    Assert (S[1] <> 'x');
    Assert (S[Length (S)] <> 'x');
    // Check other characters
    for I := 1 to Length (S) do
    begin
      Assert (S[I] in ['0','1','2','3','4','5','6','7','8','9','x']);
      if S[I] = 'x' then
        Dims += 1; // Every 'x' indicates a new dimension
    end;
    SetDim (Dims);
  end;
  // Checks complete, can use the input
  Values [cNetworkTorusDimensions] := AValue;
end;

procedure TTorusNetworkConfig.SetPortsToNodes (const AValue: Integer);
begin
  Values [cNetworkPortsToNodes] := IntToStr (AValue);
end;

procedure TTorusNetworkConfig.SetPortsToNeighbours (const AValue: Integer);
begin
  Values [cNetworkPortsToNeighbours] := IntToStr (AValue);
end;

procedure TTorusNetworkConfig.SetSwitchCount (const AValue: Integer);
begin
  Values [cNetworkSwitchCount] := IntToStr (AValue);
end;

procedure TTorusNetworkConfig.SetSwModelStr (const AValue: String);
begin
  Values [cNetworkSwitchModel] := AValue;
end;

procedure TTorusNetworkConfig.AddDim (Side: Integer);
// Adds a new torus dimension to the internal string representation. Example: "5x4" --> "5x4x6"
var
  S: String;
begin
  S := GetDimStr;
  if S = '' then                              // This is the first dimension that we are adding
    SetDimStr (IntToStr (Side))               // Add a single dimension only
  else
    SetDimStr (S + 'x' + IntToStr (Side));    // Add all previous dimensions and this one
end;

procedure TTorusNetworkConfig.ClearDim;
// Clears the string that represents dimensions
begin
  SetDimStr ('');
end;

function TTorusNetworkConfig.Description: String;
begin
  // Info on switches
  Result := 'Torus: ' + IntToStr (SwCount) + ' switch(es), model "' + SwModel.Model + '"';

  // And also add info from the parent method
  Result += inherited Description;
end;

function TTorusNetworkConfig.HtmlDescription: String;
var
  MyTemplate: TStringList;
begin
  MyTemplate := TStringList.Create;
  MyTemplate.LoadFromFile (ExtractFilePath (ParamStr(0)) + cFileResults); // Load the template

  try // Try conversion
    Result := Format (MyTemplate.Text, [NetTopology, DimStr,
      Nodes, SparePorts, ExpandableTo, PortsToNodes, PortsToNeighbours, Bl, SwModelStr, SwCount, L,
      NetPower, NetWeight, NetEquipmentSize, NetCost]);
  except
    On Excpt: Exception do
      Result += 'Exception caught : ' + Excpt.Message;
  end;

  MyTemplate.Free;
end;

procedure TTorusNetworkConfig.Assign (Source: TPersistent);
// Used to duplicate an object
var
  S: TTorusNetworkConfig;
begin
  if Source is TTorusNetworkConfig then
  begin
    S := TTorusNetworkConfig (Source);
    // Perform a basic copy first
    inherited Assign (S);
    // Now add the fields that are missing in the parent class:
    SwModel := S.SwModel;
  end
  else
    // This statement allows to assign "TNetworkConfig" to "TTorusNetworkConfig". Not all fields will be filled, but it is better than nothing.
    inherited Assign (Source);
end;

end.


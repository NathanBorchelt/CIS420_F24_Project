{
Copyright (C) 2012-2014 Konstantin S. Solnushkin

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
unit fattree;

{$mode objfpc}{$H+}

{

  The recommended strategy for creating switch databases for fat-tree networks is as follows:

  1. For standard servers:
    1.1. Edge switches: monolithic switches + modular switches in their maximal configurations (to create
         the biggest possible blocks of servers that are connected to a single switch and therefore have
         the minimal possible latency)
    1.2. Core switches: monolithic switches + modular switches in all intermediate (partially populated)
         configurations (for flexibility)

  2. For blade servers:
    2.1. Edge switches: blade switches stipulated by the blade enclosure manufacturer
    2.2. Core switches: monolithic switches + modular switches in all intermediate (partially populated)
         configurations (for flexibility)
}

interface

uses
  Classes, SysUtils, Math,
  common, database, floatstr;                       // Our own units

const
  { File names }
  cFileMainForm = 'main-form.html';
  // Generic files
  cFileShowDatabaseRuler = 'show-database-ruler.html';
  cFileShowDatabaseItem = 'show-database-item.html';
  cFileShowDatabaseFooter = 'show-database-footer.html';
  // Files specific to fat-trees
  cFileShowDatabaseHeader = 'fattree-show-database-header.html';
  cFileShowDatabaseEdge = 'fattree-show-database-edge-title.html';
  cFileShowDatabaseCore = 'fattree-show-database-core-title.html';
  cFileResults = 'fattree-results.html';
  // Switch databases
  cFileEdgeDB = 'edge.csv';
  cFileCoreDB = 'core.csv';
  { Constants below are specific to fat-tree networks }
  cNetworkCoreLevelUtilization = 'network_core_level_utilization';
  cNetworkCoreSwitchModel = 'network_core_switch_model';
  cNetworkCoreSwitchCount = 'network_core_switch_count';
  cNetworkEdgeSwitchModel = 'network_edge_switch_model';
  cNetworkEdgeSwitchCount = 'network_edge_switch_count';
  cNetworkEptc = 'network_edge_ports_to_core_level';
  cNetworkEptn = 'network_edge_ports_to_nodes';
  cNetworkCorePorts = 'network_core_ports';
  cNetworkLinksRunInBundles = 'network_links_run_in_bundles';
  cNetworkEdgeUniformDistribution = 'network_edge_uniform_distribution';
  cNotAvailableInStars = 'N/A in star networks';
  // Topology name
  cFatTree = 'fat-tree';

type

  { TFatTreeNetworkConstraints }

  TFatTreeNetworkConstraints = class (TNetworkConstraints)
    public
      function CheckPreferExpandable (out PreferExpandable: Boolean; var Err: TStringList): Boolean;
      function Check (var Err: TStringList): Boolean;  // This function checks all constraints
      function GetPreferExpandable: Boolean;
  end;

  { TFatTreeNetworkConfig }

  TFatTreeNetworkConfig = class (TNetworkConfig)
  private
    protected
      function GetBlockingFactor: Single; override;
      function GetBundles: Integer;
      function GetCoreSwitchCount: Integer;
      function GetCoreLevelUtilization: Single;
      function GetCoreModelStr: String;
      function GetEdgeSwitchCount: Integer;
      function GetEdgeModelStr: String;
      function GetEptc: Integer;
      function GetEptn: Integer;
      function GetCorePorts: Integer;
      function GetNetCost: Integer; override;
      function GetNetPower: Integer; override;
      function GetNetWeight: Single; override;
      function GetNetEquipmentSize: Integer; override;
      function GetUniform: Boolean;
      procedure SetBundles(const AValue: Integer);
      procedure SetCoreSwitchCount(const AValue: Integer);
      procedure SetCoreLevelUtilization(const AValue: Single);
      procedure SetCoreModelStr(const AValue: String);
      procedure SetEdgeSwitchCount(const AValue: Integer);
      procedure SetEdgeModelStr(const AValue: String);
      procedure SetEptc(const AValue: Integer);
      procedure SetEptn(const AValue: Integer);
      procedure SetCorePorts(const AValue: Integer);
      procedure SetUniform(const AValue: Boolean);
    public
      EModel: TSwitch;                                                           // Model of edge switches
      CModel: TSwitch;                                                           // Model of core switches
      property E: Integer read GetEdgeSwitchCount write SetEdgeSwitchCount;      // No. of edge switches
      property C: Integer read GetCoreSwitchCount write SetCoreSwitchCount;      // No. of core switches
      property CoreLevelUtilization: Single read GetCoreLevelUtilization write SetCoreLevelUtilization;        // Core level utilization
      property EdgeModelStr: String read GetEdgeModelStr write SetEdgeModelStr;  // Model of the edge switch
      property CoreModelStr: String read GetCoreModelStr write SetCoreModelStr;  // Model of the core switch, if it exists
      property Eptn: Integer read GetEptn write SetEptn;                         // Number of ports on edge switches going to nodes...
      property Eptc: Integer read GetEptc write SetEptc;                         // ...and to the core level (for two-level topologies)
      property CorePorts: Integer read GetCorePorts write SetCorePorts;          // Number of ports on core switches
      property B: Integer read GetBundles write SetBundles;                      // Links between layers run in bundles of "B"
      property EdgeUniformDistribution: Boolean read GetUniform write SetUniform;// If nodes are evenly distributed between edge switches
      function BStr: String;                                                     // Formats "B" as a string, if not a star
      function Description: String; override;                                    // Human-readable description
      function HtmlDescription: String; override;                                // Fully-fledged description in HTML
      procedure Assign (Source: TPersistent); override;                           // Used to create copies of objects
  end;

procedure LoadFatTreeDatabase (const Vendor: String; var E, C: TSwitchList; out BiggestN: LongInt);

function FormatFatTreeDatabase(const Vendor: String): String;

procedure DesignFatTree (const Vendor: String; var Constraints: TStringList; Debug: Boolean;
  var DebugOutput: TStringList; out BestNetConfig: TNetworkConfig);

implementation

procedure LoadFatTreeDatabase (const Vendor: String; var E, C: TSwitchList; out BiggestN: LongInt);
var
  Dir: String;
begin
  // Load both databases (same directory as the executable file + subdirectory for topology + subdirectory for vendor)
  Dir := ExtractFilePath (ParamStr(0)) + cDatabaseDir + DirectorySeparator +
    cFatTree + DirectorySeparator + Vendor + DirectorySeparator;
  LoadSwitchDBFromCSVFile (E, Dir + cFileEdgeDB);
  LoadSwitchDBFromCSVFile (C, Dir + cFileCoreDB);

  // Now, calculate the biggest number of nodes our database can support.
  BiggestN := E.BiggestPortCount;

  // If we also have core switches in the database, then two-level networks
  // can be constructed. In this case, the number of supported nodes is increased.
  if C.Count <> 0 then
    BiggestN := (BiggestN * C.BiggestPortCount) div 2;
end;

function FormatFatTreeDatabase(const Vendor: String): String;
var
  MyTemplate: TStringList;
  E, C: TSwitchList;
  Sw: TSwitch;
  BiggestN: Integer;
  I: Integer;
  HorizontalRuler, SwitchItemStr: String;   // String to hold HTML templates loaded from disk
begin
  MyTemplate := TStringList.Create;
  E := TSwitchList.Create;
  C := TSwitchList.Create;

  // Load the database of switches
  LoadFatTreeDatabase (Vendor, E, C, BiggestN);

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

  // Load edge switches count template and output it
  MyTemplate.LoadFromFile (ExtractFilePath(ParamStr(0)) + cFileShowDatabaseEdge);
  Result += Format (MyTemplate.Text, [E.Count]);                                  // Number of edge switches

  // Add a horizontal ruler
  Result += HorizontalRuler;

  // Output edge switches
  for I := 0 to E.Count-1 do
  begin
    Sw := E.Objects[I] as TSwitch;
    Result += Format (SwitchItemStr, [Sw.Model, Sw.P, Sw.Size, Sw.Weight, Sw.Power, Sw.Cost]) + HorizontalRuler;
  end;

  // Load core switches count template and output it
  MyTemplate.LoadFromFile (ExtractFilePath(ParamStr(0)) + cFileShowDatabaseCore);
  Result += Format (MyTemplate.Text, [C.Count]);                                  // Number of core switches

  // Add a horizontal ruler
  Result += HorizontalRuler;

  // Output core switches
  for I := 0 to C.Count-1 do
  begin
    Sw := C.Objects[I] as TSwitch;
    Result += Format (SwitchItemStr, [Sw.Model, Sw.P, Sw.Size, Sw.Weight, Sw.Power, Sw.Cost]) + HorizontalRuler;
  end;

  // Load footer and output it
  MyTemplate.LoadFromFile (ExtractFilePath(ParamStr(0)) + cFileShowDatabaseFooter);
  Result += MyTemplate.Text;                                                      // No elements to format in the footer; add "as is"

  // Free all objects
  E.Free;
  C.Free;
  MyTemplate.Free;
end;

procedure DesignFatTree (const Vendor: String; var Constraints: TStringList; Debug: Boolean;
  var DebugOutput: TStringList; out BestNetConfig: TNetworkConfig);
(* Design two-level fat-tree networks (possibly blocking).
   "Constraints" defines design constraints, such as "nodes" (the number of nodes to be interconnected), "nodes-future-max" (the maximum
   number of nodes the network has to support in the future), "max-network-cost" (the maximum allowed cost), etc.
   If "Debug" is set, then debug messages are saved in "DebugOutput".
   Error messages go there anyway.
   If successful, then "BestNetConfig" is assigned the best network configuration. Otherwise, "Nil" is returned in "BestNetConfig"*)

var
  E, C: TSwitchList;
  BiggestN: Integer;
  NetConfig: TFatTreeNetworkConfig;      // A pointer to a network configuration
  NetConstraints: TFatTreeNetworkConstraints; // List to hold constraints
  ConfigList: TConfigList;               // List to hold pointers to network configurations
  Description: String;                   // Textual description of a network configuration
  WhyDiscarded: String;                  // Textual reason why the config was discarded
  // Variables to hold constraints:
  NodesFutureMax: Integer;               // The maximum future number of nodes in a network
  RequestedBlockingFactor: Single;       // Blocking factor requested by the user
  PreferExpandable: Boolean;             // Whether the user prefers easy expandability to lower cost

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
        NodesFutureMax := GetNodesFutureMax;
        RequestedBlockingFactor := GetBlockingFactor;
        PreferExpandable := GetPreferExpandable;
      end;
    finally
      // Save the processed constraints
      Constraints.Assign (NetConstraints);
    end;

    // If we reached here, then we passed all checks
    Result := True;
  end;

  procedure CheckStarNetwork;
  // Checks for a special case of star network
  var
    I: Integer;
    Sw: TSwitch;
  begin
    DoDebugOutput (LineEnding + 'Trying star networks:' + LineEnding);

    { Switches for star networks are only sought for in the list of edge switches. It is because
      installations that use blade servers _always_ require edge switches that are installed within
      enclosures. We cannot take a large core switch and form a star network as we could for
      standard servers, because blade servers still require edge switches.
    }

    // Search through the list of edge switches.
    // If a switch contains P>=NodesFutureMax ports, it is usable to build a star network
    for I := 0 to E.Count-1 do
    begin
      Sw := E.Objects[I] as TSwitch;
      if Sw.P >= NodesFutureMax then             // A switch usable for a star network is found
      begin
        NetConfig := TFatTreeNetworkConfig.Create;
        NetConfig.AddStrings (NetConstraints);   // Add constraints to the new config
        with NetConfig do                        // Fill other values
        begin
          NetTopology := cStar;                  // Set the topology
          E := 1;                                // In a star network there is only one edge switch
          EModel := Sw;                          // Its model
          B := 1;                                // Links run singly
          Bl := 1;                               // Star networks are non-blocking, hence "1" here
          Eptn := EModel.P;                      // In star topologies, all available ports go to nodes
          Eptc := 0;
          C := 0;                                // No core switches
          CModel := Nil;
          L := Nodes;                            // As many cables as we have nodes
          SparePorts := EModel.P - Nodes;        // These ports are spare
          ExpandableTo := EModel.P;              // Can have this many ports
          // Perform dummy calls of these properties just to calculate their values:
          EdgeModelStr;
          CoreModelStr;
          NetCost;
          NetPower;
          NetWeight;
          NetEquipmentSize;
        end;
        Description := '> Trivial case (' + NetConfig.Description + ')';

        if NetConfig.CheckForConstraints (NetConstraints, WhyDiscarded) then
        begin
          DoDebugOutput (Description + ', added');
          ConfigList.AddObject ('Star, with ' + Sw.Model, NetConfig);  // Add to the list of good configs
        end
        else
        begin
          DoDebugOutput (Description + ', discarded, due to: ' + WhyDiscarded + '.');
          NetConfig.Free;                                              // This config failed; free its memory
        end;
      end
      else                                         // Switch is unsuitable: not enough ports to form a star network
        DoDebugOutput ('Switch ' + Sw.Model + ' unsuitable, not enough ports: ' +
          IntToStr (Sw.P) + ' < ' + IntToStr (NodesFutureMax));
    end;
  end;

  procedure CheckGeneralCase;
  { Checks for the general case. Steps through edge switches. For every edge switch:
    1. Find the number of edge switches required to accommodate all NodesFutureMax compute nodes
       (taking the blocking factor into account)
    2. Step through core switches. For every core switch (if port count of allows):
      2.1. Calculate B, the number of links in a bundle towards edge level
      2.2. Calculate C, the number of core switches
      2.3. Check for constraints and calculate objective function

    (Kindly request details if necessary)

    The result of this procedure is a set of suitable configurations, saved to "ConfigList".
  }
  var
    IdxE, IdxC: Integer;                              // Indices for edge and core switches
    ESw, CSw: TSwitch;                                // Edge and core switches to try
    EMax: Integer;                                    // Number of edge switches in a configuration with NodesFutureMax
    EdgePortsToNodes, EdgePortsToCore: Integer;       // Number of ports on edge switches that will be connected to nodes and to the core level
    UniformPortsToNodes, UniformPortsToCore: Integer; // Same, but in case of even distribution of nodes between edge switches
    CUniform: Integer;                                // The number of core switches if even allocation is used
    MaximumNodes: Integer;                            // The number of nodes a network can support
  begin
    DoDebugOutput (LineEnding + 'Trying general case:');

    for IdxE := 0 to E.Count-1 do                     // Iterate through edge switches
    begin
      ESw := E.Objects[IdxE] as TSwitch;
      // How many ports on edge switches will be dedicated to connecting nodes?
      // The use of the "Trunc" function ensures that blocking will be less than or equal to the value
      // specified by the user. This is important in order not to accidentally produce worse designs than requested.
      EdgePortsToNodes := Trunc (ESw.P * (RequestedBlockingFactor / (1 + RequestedBlockingFactor)));
      EdgePortsToCore := ESw.P - EdgePortsToNodes;
      // Calculate "E", the number of edge switches
      EMax := Ceil (NodesFutureMax / EdgePortsToNodes);     // The number of edge switches required to connect NodesFutureMax nodes
      DoDebugOutput (LineEnding + '> Trying edge switch "' + ESw.Model + '", need ' + IntToStr (EMax) +
        ' switches for the maximal capacity configuration');
      DoDebugOutput ('> Requested blocking factor is "' + FloatToStrF (RequestedBlockingFactor, ffFixed, 0, 1) +
        '", ' + IntToStr (EdgePortsToNodes) + ' ports go to nodes, ' + IntToStr (EdgePortsToCore) +
        ' go to the core level');

      for IdxC := 0 to C.Count-1 do               // Iterate through core switches
      begin
        CSw := C.Objects[IdxC] as TSwitch;

        // Now, edge and core switch models have been determined. Check that the chosen switches can support this number of nodes.
        // This is the maximum number of nodes that the network with these particular
        // edge and core switches (and the blocking factor) can support.
        MaximumNodes := EdgePortsToNodes * CSw.P;

        if Debug then
          DoDebugOutput (LineEnding + '>>> Trying core switch "' + CSw.Model + '". The network can support up to ' +
            IntToStr (MaximumNodes) + ' nodes.');

        if MaximumNodes < NodesFutureMax then     // This network cannot support NodesFutureMax nodes, reject this core switch
        begin
          if Debug then
            DoDebugOutput ('>>> ' + IntToStr (MaximumNodes) + ' < ' + IntToStr (NodesFutureMax) + ', core switch unsuitable, rejected.');
          Continue;                               // Continue with the next core switch
        end
        else
          if Debug then
            DoDebugOutput ('>>> Core switch suitable.');  // Otherwise, joyfully inform that it is suitable.

        // Create a new fat-tree network configuration and fill its fields
        NetConfig := TFatTreeNetworkConfig.Create;
        NetConfig.AddStrings (Constraints);      // Add constraints to the new config
        with NetConfig do                        // Fill other values
        begin
          NetTopology := cFatTree;               // Set the topology
          CModel := CSw;                         // Model of a core switch
          E := Ceil (Nodes / EdgePortsToNodes);  // Initially, this number of edge switches is required (in contrast to EMax)
          EModel := ESw;                         // Which model
          Eptn := EdgePortsToNodes;              // Number of ports dedicated to connecting nodes
          Eptc := EdgePortsToCore;               // And to the core level
          // Calculate "B", the number of links in a bundle going towards edge level:
          // B = P_C div EMax, where P_C is port count of core switches, and EMax is the number of edge switches in the maximal configuration
          // In rare cases (when using blocking), only one core switch is enough to connect the few edge switches; hence "Min"
          B := Min (CSw.P div EMax, Eptc);

        (* Calculate "C", the number of core switches, to accommodate all NodesFutureMax nodes.
           Sometimes (very rarely), if the network will not be further expanded, a lower number of core switches can be
           attained by distributing nodes evenly between edge level switches. Example: Nodes=127 (or 128), P_E=P_C=36.
           E=8 edge switches are required, and B=4.
           If distributing evenly (16 nodes to each edge switch), then C=Ceil(16/B)=4.
           If distributing as closely as possible (18 nodes per each of 7 switches, and 2 nodes for the 8th switch),
           then C=Ceil(18/B)=Ceil(4,5)=5.
           On the other side, distributing evenly, although marginally better in rare cases, means problems for future expansion:
           adding more nodes to the existing edge level means initially underpopulated racks with empty slots where
           new nodes would be inserted.
           However, even if the user didn't ask for expansion opportunities, we only accept even distribution if it provides savings
           in the number of core switches. *)

          // First, try the usual allocation (as densely as possible)
          C := Ceil (Eptc / B);  // In all other cases, connect nodes
          EdgeUniformDistribution := False;

          // Also try the even allocation which might yield better results
          UniformPortsToNodes := Ceil (NodesFutureMax / Emax); // How many ports go to nodes if distributed evenly between edge switches
          UniformPortsToCore := Ceil (UniformPortsToNodes / RequestedBlockingFactor); // And how many go to the core level
          CUniform := Ceil (UniformPortsToCore / B);   // How many core switches are needed in case of even distribution

          // Only accept this solution if: (1) there are savings -AND- (2) the user explicitly stated they don't want
          // easily expandable networks
          if (CUniform < C) and (not PreferExpandable) then  // (In all other cases, previously set values of these variables are used)
          begin
            EdgeUniformDistribution := True;
            Eptn := UniformPortsToNodes;
            Eptc := UniformPortsToCore;
            C := CUniform;
          end;

          // Set remaining fields of the configuration
          L := Nodes + E * Eptc;  // Number of cables, of them "Nodes" are for nodes, the rest is between the edge and core layers
          SparePorts := E * Eptn - Nodes; // These ports are spare on the edge level after "Nodes" nodes are installed
          ExpandableTo := EMax * Eptn;    // Can connect this many nodes without requiring more core switches (just by adding edge switches)
          CorePorts := CSw.P;
          // Perform dummy calls of these properties just to calculate their values:
          EdgeModelStr;
          CoreModelStr;
          NetCost;
          NetPower;
          NetWeight;
          NetEquipmentSize;
        end;
        Description := 'Two-level, links in bundles of ' + IntToStr (NetConfig.B) + ' (' + NetConfig.Description + ')';

        // Check against constraints
        if NetConfig.CheckForConstraints (NetConstraints, WhyDiscarded) then
        begin
          DoDebugOutput ('>>>>>> ' + Description + ', added');
          ConfigList.AddObject (Description, NetConfig);  // Add to the list of good configs
        end
        else
        begin
          DoDebugOutput ('>>>>>> ' + Description + ', discarded, due to: ' + WhyDiscarded + '.');
          NetConfig.Free;                                 // This config failed; free its memory
        end;

      end; { End of iteration through core switches }

    end;  { End of iteration through edge switches }

  end;

begin
  Assert (Assigned (DebugOutput));
  DebugOutput.Clear;                     // Debug output will be saved here

  BestNetConfig := Nil;                  // Prepare for the worst: if we are unsuccessful, Nil is returned, indicating an error

  NetConstraints := TFatTreeNetworkConstraints.Create; // We will store network constraints here

  if not GetConstraints then             // Extract design constraints from the list; if failed then exit
    Exit;                                // Error output should have been already prerformed in "GetConstraints"

  ConfigList := TConfigList.Create;      // We will store network configurations here

  E := TSwitchList.Create;
  C := TSwitchList.Create;
  LoadFatTreeDatabase (Vendor, E, C, BiggestN);  // Load the database of switches

  CheckStarNetwork;                      // Check for the special case of the "Star" network

  CheckGeneralCase;                      // Check for the general case (try all edge and core switches)

  if Debug then
    DoDebugOutput (LineEnding + 'Searching through ' + IntToStr (ConfigList.Count) + ' configurations' + LineEnding);

  NetConfig :=  ConfigList.BestConfig as TFatTreeNetworkConfig;   // Find the best configuration
  if Assigned (NetConfig) then           // If found, return its description by creating a clone of that object (to release all other memory)
  begin
    BestNetConfig := TFatTreeNetworkConfig.Create;  // Create a new, clean object
    BestNetConfig.Assign (NetConfig);               // and assign its fields
  end
  else
  begin
    DoErrorOutput ('Error: No fat-tree configurations found matching your constraints.');
    DoErrorOutput ('Most likely reason: Too many nodes specified. Try to reduce the number of nodes, or increase the blocking factor.');
  end;

  // Free all objects
  ConfigList.Free;                       // Free the list of configurations _AND_ free the configurations, too
  NetConstraints.Free;
  E.Free;
  C.Free;
end;

{ TFatTreeNetworkConstraints }

function TFatTreeNetworkConstraints.CheckPreferExpandable(out PreferExpandable: Boolean;
  var Err: TStringList): Boolean;
// Get the preference of expandable networks
// If assigned, we'll try to design networks expandable in the future, even if they cost more
var
  I: Integer;
begin
  Result := False;                                         // Prepare for the worst
  I := IndexOfName (cPreferExpandable);
  if (I = -1) or                                           // Not specified
    (ValueFromIndex [I] = '') then                         // Was specified, but holds an empty value
      PreferExpandable := False                            // Set default value
  else
    try
      PreferExpandable := StrToBool (ValueFromIndex [I]);  // Was specified, and is non-empty
    except
      on EConvertError do
      begin
        Err.Add ('Error: Boolean value expected when parsing "' + cPreferExpandable + '"');
        Exit;
      end;
    end; { try }

  // If we reached here, then "PreferExpandable" is valid. Save it and return success.
  Values [cPreferExpandable] := BoolToStr (PreferExpandable, True);
  Result := True;
end;

function TFatTreeNetworkConstraints.Check(var Err: TStringList): Boolean;
var
  PreferExpandable: Boolean;             // Whether the user prefers easy expandability to lower cost
begin
  // Result is the logical "AND" of the inherited method (that does many checks)
  // and the checks pertinent to fat-tree networks (i.e., preference for expandability)
  Result := (
    inherited Check (Err) and
    CheckPreferExpandable (PreferExpandable, Err));

  // If checks are unsuccessful, exit
  if not Result then
    Exit;

  // Add to the textual overview of constraints
  AddOverview ('User preference for expandable networks: ' + BoolToStr (PreferExpandable, True));
end;

function TFatTreeNetworkConstraints.GetPreferExpandable: Boolean;
begin
  Assert (FAlreadyChecked);
  Result := StrToBool (Values [cPreferExpandable]);
end;

{ TFatTreeNetworkConfig }

function TFatTreeNetworkConfig.BStr: String;
begin
  if not Assigned (CModel) then                 // It is a star network
    Result := cNotAvailableInStars
  else
    Result := IntToStr (B);
end;

function TFatTreeNetworkConfig.GetCoreLevelUtilization: Single;
var
  I: Integer;
begin
  I := IndexOfName (cNetworkCoreLevelUtilization);
  if I >= 0 then
    begin
      Result := GenericStrToFloat (ValueFromIndex [I]);  // Already calculated, exit
      Exit;
    end;

  // Otherwise, it was not calculated yet
  if Assigned (CModel) then  // Only available if core level exists
    Result := E * B / CModel.P * 100
  else
    Result := 0;

  SetCoreLevelUtilization (Result);                      // Save result for future reuse
end;

function TFatTreeNetworkConfig.GetBlockingFactor: Single;
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
  if Assigned (CModel) then    // Only available if core level exists
    Result := Eptn / Eptc
  else
    Result := 1;               // Otherwise, this is a star network, and we return "1" to indicate that it is always non-blocking

  SetBlockingFactor (Result);  // Save result for future reuse
end;

function TFatTreeNetworkConfig.GetBundles: Integer;
var
  I: Integer;
begin
  I := IndexOfName (cNetworkLinksRunInBundles);
  Assert (I >= 0);
  Result := StrToInt (ValueFromIndex [I]);
end;

function TFatTreeNetworkConfig.GetCoreSwitchCount: Integer;
var
  I: Integer;
begin
  I := IndexOfName (cNetworkCoreSwitchCount);
  Assert (I >= 0);
  Result := StrToInt (ValueFromIndex [I]);
end;

function TFatTreeNetworkConfig.GetCoreModelStr: String;
var
  I: Integer;
begin
  I := IndexOfName (cNetworkCoreSwitchModel);
  if I >= 0 then
    begin
      Result := ValueFromIndex [I];  // Already calculated, exit
      Exit;
    end;

  // Otherwise, it was not calculated yet
  if not Assigned (CModel) then                 // It is a star network
    Result := cNotAvailableInStars
  else
    Result := CModel.Model;

  SetCoreModelStr (Result);                           // Save result for future reuse
end;

function TFatTreeNetworkConfig.GetEdgeSwitchCount: Integer;
var
  I: Integer;
begin
  I := IndexOfName (cNetworkEdgeSwitchCount);
  Assert (I >= 0);
  Result := StrToInt (ValueFromIndex [I]);
end;

function TFatTreeNetworkConfig.GetEdgeModelStr: String;
var
  I: Integer;
begin
  I := IndexOfName (cNetworkEdgeSwitchModel);
  if I >= 0 then
    begin
      Result := ValueFromIndex [I];  // Already calculated, exit
      Exit;
    end;

  // Otherwise, it was not calculated yet
  Result := EModel.Model;

  SetEdgeModelStr (Result);                           // Save result for future reuse
end;

function TFatTreeNetworkConfig.GetEptc: Integer;
var
  I: Integer;
begin
  I := IndexOfName (cNetworkEptc);
  Assert (I >= 0);
  Result := StrToInt (ValueFromIndex [I]);
end;

function TFatTreeNetworkConfig.GetEptn: Integer;
var
  I: Integer;
begin
  I := IndexOfName (cNetworkEptn);
  Assert (I >= 0);
  Result := StrToInt (ValueFromIndex [I]);
end;

function TFatTreeNetworkConfig.GetCorePorts: Integer;
var
  I: Integer;
begin
  I := IndexOfName (cNetworkCorePorts);
  Assert (I >= 0);
  Result := StrToInt (ValueFromIndex [I]);
end;

function TFatTreeNetworkConfig.GetNetCost: Integer;
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
  Result := E * EModel.Cost + L * CableCost;    // Edge switches and cables

  if Assigned (CModel) then                     // If not a trivial case, where core level is not used
    Result := Result + C * CModel.Cost;         // then add core level costs as well

  SetNetCost(Result);                           // Save result for future reuse
end;

function TFatTreeNetworkConfig.GetNetPower: Integer;
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
  Result := E * EModel.Power;                   // Edge switches

  if Assigned (CModel) then                     // If not a trivial case, where core level is not used
    Result := Result + C * CModel.Power;        // then add core level power as well

  SetNetPower (Result);                         // Save result for future reuse
end;

function TFatTreeNetworkConfig.GetNetWeight: Single;
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
  Result := E * EModel.Weight;                  // Edge switches

  if Assigned (CModel) then                     // If not a trivial case, where core level is not used
    Result := Result + C * CModel.Weight;       // then add core level weight as well

  SetNetWeight (Result);                        // Save result for future reuse
end;

function TFatTreeNetworkConfig.GetNetEquipmentSize: Integer;
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
  Result := E * EModel.Size;                    // Size of edge switches

  if Assigned (CModel) then                     // If not a trivial case, where core level is not used
    Result := Result + C * CModel.Size;         // then add core level size as well

  SetNetEquipmentSize (Result);                 // Save result for future reuse

  // Without much ado and OOP paradigms, methods and properties,
  // add two more metrics with switch sizes:
  Values [cNetworkEdgeSwitchSize] := IntToStr (EModel.Size);
  if Assigned (CModel) then
    Values [cNetworkCoreSwitchSize] := IntToStr (CModel.Size);
end;

function TFatTreeNetworkConfig.GetUniform: Boolean;
var
  I: Integer;
begin
  I := IndexOfName (cNetworkEdgeUniformDistribution);
  Assert (I >= 0);
  Result := StrToBool (ValueFromIndex [I]);
end;

procedure TFatTreeNetworkConfig.SetBundles(const AValue: Integer);
begin
  Values [cNetworkLinksRunInBundles] := IntToStr (AValue);
end;

procedure TFatTreeNetworkConfig.SetCoreSwitchCount(const AValue: Integer);
begin
  Values [cNetworkCoreSwitchCount] := IntToStr (AValue);
end;

procedure TFatTreeNetworkConfig.SetCoreLevelUtilization (const AValue: Single);
begin
  // XXX: Hard-wired: integer value
  Values [cNetworkCoreLevelUtilization] := IntToStr (Round (AValue));
end;

procedure TFatTreeNetworkConfig.SetCoreModelStr(const AValue: String);
begin
  Values [cNetworkCoreSwitchModel] := AValue;
end;

procedure TFatTreeNetworkConfig.SetEdgeSwitchCount(const AValue: Integer);
begin
  Values [cNetworkEdgeSwitchCount] := IntToStr (AValue);
end;

procedure TFatTreeNetworkConfig.SetEdgeModelStr(const AValue: String);
begin
  Values [cNetworkEdgeSwitchModel] := AValue;
end;

procedure TFatTreeNetworkConfig.SetEptc(const AValue: Integer);
begin
  Values [cNetworkEptc] := IntToStr (AValue);
end;

procedure TFatTreeNetworkConfig.SetEptn(const AValue: Integer);
begin
  Values [cNetworkEptn] := IntToStr (AValue);
end;

procedure TFatTreeNetworkConfig.SetCorePorts(const AValue: Integer);
begin
  Values [cNetworkCorePorts] := IntToStr (AValue);
end;

procedure TFatTreeNetworkConfig.SetUniform(const AValue: Boolean);
begin
  Values [cNetworkEdgeUniformDistribution] := BoolToStr (AValue, True);
end;

function TFatTreeNetworkConfig.Description: String;
begin
  // Info on edge level
  if (C = 0) and (E = 1) then   // If no core level, then it is a "Star" network
    Result := 'Star network, one switch, model "' + EModel.Model + '"'
  else
    Result := 'Edge level: ' + IntToStr (E) + ' switch(es), model "' + EModel.Model + '"';

  // Info on core level
  if Assigned (CModel) then  // Core level also exists
    Result := Result + ', core level: ' + IntToStr (C) + ' switches, model "' + CModel.Model + '"';

  // Info on link bundles
  if B = 1 then
    Result := Result + ', links run singly'
  else
    Result := Result + ', links run in bundles of ' + IntToStr (B);

  // Info on core level
  if Assigned (CModel) then  // Only available if core level exists
  begin
    Result := Result + ', core level switches have ' + IntToStr (CorePorts);
    // Core level port utilization
    Result := Result + ', core level port utilization is ' + IntToStr (Round (CoreLevelUtilization))
      + ' percent';
  end;

  // Info on cables and expandability
  Result := Result + ', ' + IntToStr (L) + ' cables, expandable to ' + IntToStr (ExpandableTo) + ' nodes';

  // Info on ports distribution and blocking factor (only applicable for non-star networks)
  if Assigned (CModel) then
    Result := Result + ', ' + IntToStr (Eptn) + ' ports go to nodes, ' + IntToStr (Eptc) + ' go to core level,' +
      ' blocking factor is "' + FloatToStrF (Bl, ffFixed, 0, 1) + '"';

  // And also add info from the parent method
  Result += inherited Description;
end;

function TFatTreeNetworkConfig.HtmlDescription: String;
var
  MyTemplate: TStringList;
begin
  MyTemplate := TStringList.Create;
  MyTemplate.LoadFromFile (ExtractFilePath (ParamStr(0)) + cFileResults); // Load the template

  try // Try conversion
    Result := Format (MyTemplate.Text, [NetTopology,
      Nodes, SparePorts, ExpandableTo, Eptn, Eptc, Bl, EdgeModelStr, E, CoreModelStr, C, L,
      BStr, IntToStr (Round (CoreLevelUtilization)), NetPower, NetWeight, NetEquipmentSize, NetCost]);
  except
    On Excpt: Exception do
      Result += 'Exception caught : ' + Excpt.Message;
  end;

  MyTemplate.Free;
end;

procedure TFatTreeNetworkConfig.Assign (Source: TPersistent);
// Used to duplicate an object
var
  S: TFatTreeNetworkConfig;
begin
  if Source is TFatTreeNetworkConfig then
  begin
    S := TFatTreeNetworkConfig (Source);
    // Perform a basic copy first
    inherited Assign (S);
    // Now add the fields that are missing in the parent class:
    EModel := S.EModel;
    CModel := S.CModel;
  end
  else
    // This statement allows to assign "TNetworkConfig" to "TFatTreeNetworkConfig". Not all fields will be filled, but it is better than nothing.
    inherited Assign (Source);
end;

end.


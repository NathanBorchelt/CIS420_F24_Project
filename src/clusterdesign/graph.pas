{
Copyright (C) 2012 Konstantin S. Solnushkin

This file is part of "dbcli", the tool to query graph-based databases.

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
unit Graph;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, XMLRead, DOM,
  Common, Evaluate, FloatStr;                                   // Our own units

const
  NameOfStartVertex = 'Start';
  NameOfEndVertex = 'End';

type

  { TVertexColor }

  TVertexColor = (White, Gray, Black);

  { TParameters }

  TParameters = class (TStringList)
  end;

  { TVertex }

  TVertex = class (TParameters)
    private
      FName: String;
      FPartitionName: String;
    public
      Color: TVertexColor;
      Predecessor: TVertex;
      DTimeStamp, FTimeStamp: Longword;
      constructor Create;
      function ColorName: String;
      function PredecessorName: String;
      procedure SetDefaults;
      property Name: String read FName write FName;
      property PartitionName: String read FPartitionName write FPartitionName;
  end;

  { TVertexList }

  TVertexList = class(TStringList)
    private
      FFreeObjects: Boolean;
    public
      constructor Create(FreeObjects : Boolean);
      procedure Clear; override;
      destructor Destroy; override;
      function AddVertexByName (const VertexName : String): TVertex;
      function CopyVertexByName (const SourceVertexName: String; const TargetVertexName: String): TVertex;
      procedure AddVertex (const V: TVertex);
      procedure GetVertexListing (var L: TStringList);
      property OwnsObjects: Boolean read FFreeObjects; // Read-only, specified at the time of creation
  end;

  { TPath }

  TPath = class(TStringList)
  // In the path, vertices are stored in the TStringList: vertices' names are strings,
  // and associated objects are pointer to vertices. See "AddVertex" method for details.
    private
      // And this one holds parameters for the whole path. Following vertices in a path,
      // one by one, we calculate the path's parameters (see the "CalculateParameters" method).
      FParameters: TParameters;
    public
      constructor Create;
      destructor Destroy; override;
      procedure AddVertex (const V: TVertex);
      procedure RemoveVertexFromTail;
      procedure EvaluateFormula (var Formula: String);
      procedure CalculateParameters;
      procedure InjectParameters (const ParamNames: TParameters);
      property Parameters: TParameters read FParameters; // Read-only; for writing, use procedure "CalculateParameters"
  end;

  { TVertexRecord }

  TVertexRecord = class
    public
      Vertex: TVertex;
      AdjacentVertexList: TVertexList;
  end;

  { TVertexRecordList }

  TVertexRecordList = class(TStringList)
    public
      constructor Create;
  end;

  { TEdge }

  TEdge = class(TParameters)
    private
      FFrom, FTo: TVertex;
    public
      constructor Create(VFrom, VTo: TVertex);
      property FromVertex: TVertex read FFrom;
      property ToVertex: TVertex read FTo;
  end;

  { TEdgeList }

  TEdgeList = class(TStringList)
  end;

  { TPathList }

  TPathList = class(TStringList)
    public
      procedure Clear; override;
      destructor Destroy; override;
  end;

  { TGraph }

  TGraph = class
    private
      FVertexRecordList: TVertexRecordList; // Contains pointers to each vertex's adjacency list
      FPathList: TPathList;                 // Contains pointers to paths
      FEdgeList: TEdgeList;                 // Contains pointers to edges
      FPartitionList: TStringList;          // Contains name and pointers to graph partitions
      function InternalEdgeName (const VFromName, VToName: String): String;
      function GetVertexCount: Integer;
      function GetPathCount: Longword;
      function GetPartitionCount: Longword;
      procedure AddVertex (const V: TVertex);
      procedure AddVertexToPartition (const V: TVertex; const PartitionName: String);
      function MakeVertexName (const SourceVertexName: String; const PartitionName: String) : String;
    public
      constructor Create;
      destructor Destroy; override;
      function CreateVertexByName (const VertexName: String; const PartitionName: String): TVertex;
      function CopyVertexToPartition (const VertexName: String; const PartitionName: String): TVertex;
      function GetVertexByName (const VName: String): TVertex;
      function GetVertexByNameAndPartition (const VName: String; const PartitionName: String): TVertex;
      function AddEdge (const VFrom, VTo: TVertex): TEdge;
      function AddEdgeByNames (const VFromName, VToName, PartFromName, PartToName: String): TEdge;
      procedure ConnectPartitionsByName (const PartFromName, PartToName: String);
      procedure ConnectPartitionToVertex (const PartName, VertexName: String);
      procedure ConnectVertexToPartition (const VertexName, PartName: String);
      procedure GetVertexListing (var L: TStringList);
      procedure GetPathListing (var L: TStringList);
      function GetPathByIndex (I: Integer): TPath;
      property VertexCount: Integer read GetVertexCount;
      property PathCount: Longword read GetPathCount;
      property PartitionCount: Longword read GetPartitionCount;
      procedure RemoveMarkings;
      procedure NormalizePaths;
      procedure FindAllPaths;
      procedure LoadDatabase (const FileName: String);
      procedure Clear;
  end;

implementation

{ TPath }

constructor TPath.Create;
begin
  inherited Create;
  Sorted := False;  // Vertex list is NOT sorted, because we need to preserve order
  FParameters := TParameters.Create;
  FParameters.Sorted := True;  // But parameters list IS sorted.
end;

destructor TPath.Destroy;
begin
  FParameters.Free;
  inherited Destroy;
end;

procedure TPath.AddVertex(const V: TVertex);
begin
  AddObject (V.Name, V);
end;

procedure TPath.RemoveVertexFromTail;
begin
  if Count > 0 then   // There is at least one vertex to delete
    Delete(Count-1)   // ..delete it
  else
    ;                 // Else, currently do nothing
end;

procedure TPath.EvaluateFormula (var Formula: String);
{ Types of expression we might encounter and must be able to evaluate:

 b + 2,3
 2,3 + b
 2,3 + 4,7
 b + c + 0,1
 power * 2 + 100
 power * ((2 + 4 * ((5 * 5)))) / (14 - (weight * (4)))
}

  function FindLR (const Formula: String; const TermChars: TCharSet; const StartWith: Integer; out L, R: Integer): String;
  // Scans the string "Formula" for literals, starting from position "StartWith", using "TermChars"
  // as terminating characters that enclose the literal.
  // If the literal is found, returns it as a function result, also returning left and right
  // indices of string "Formula" in "L" and "R", respectively.
  var
    Found: Boolean;
  begin
    // Default value
    Result := '';

    // Check the value of "StartWith" for correctness
    if StartWith > Length (Formula) then Exit;

    // Scan the "Formula" until the index "L" reaches first character that belongs to a parameter name
    // Then, start increasing the "R" index until the parameter name ends
    L := StartWith;
    Found := False;
    while L <= Length (Formula) do
    begin
      if not (Formula[L] in TermChars) then
      begin
        Found := True;
        Break;
      end;
      L += 1;
    end;

    if not Found then Exit;                   // If nothing is found, return empty string

    // If we reached here, then "Found" is True, and "L" points to the first character of the parameter name
    // Start "R" from here, increasing until we meet either the terminal character or end of line
    // Possibly, we could already reach the end of line
    if L = Length (Formula) then            // It's a one-char parameter name
      R := L
    else
    begin
      R := L + 1;                           // Start with the next character
      while R <= Length (Formula) do
      begin
        if Formula[R] in TermChars then
          Break;
        R += 1;
      end;
      if R <> Length (Formula) then         // If it is not the end of line, then the parameters actually ends one character earlier
        R -= 1;
    end;

    // If we reached here, then the substring between indices "L" and "R" is the parameter name
    Result := Copy (Formula, L, R - L + 1);
  end;

var
  L, R, Idx: Integer;
  TermChars: TCharSet;
  ParamName, ParamValue: String;
  SubstitutionFailed: Boolean;
  StartPos: Integer;

begin
  // If the string is empty, then exit immediately
  if Formula = '' then Exit;

  // All these characters cannot belong to the parameter name
  TermChars := Operators + Digits + DecimalSeps + Braces + Space;

  // Perform substitutions of parameters until there is nothing more to substitute
  // We substitute whatever we can, but the strings that cannot be substituted
  // (their values are undefined) are left as they are. If some substitutions
  // fail, we will exit.
  StartPos := 1;                   // Initially, start searching from the beginning of the string
  SubstitutionFailed := False;     // Hope for the best
  while True do
  begin
    ParamName := FindLR (Formula, TermChars, StartPos, L, R);
    if ParamName = '' then Break;  // Nothing to substitute

    // If we reached here, there is some substitution work to do

    // Try to find a parameter of this name on the current path
    Idx := FParameters.IndexOfName (ParamName);
    if Idx = -1 then
    begin
      // Substitution of this parameter failed, set the flag
      SubstitutionFailed := True;
      // Continue searching after this parameter -- there can be something else
      // we still can substitute
      StartPos := R + 1;
    end
    else
    begin
      // Parameter value was found, substitution will be successful
      ParamValue := FParameters.ValueFromIndex[Idx];
      // Substitute the parameter name with its value
      System.Delete (Formula, L, R - L + 1);
      System.Insert (ParamValue, Formula, L);
      // Skip to the right
      StartPos := L + Length (ParamValue);
    end;

    // No need to search more if we reached the right edge of the string
    if StartPos > Length (Formula) then Break;
  end;

  // If at least one substitution failed, leave the function with a semi-processed string
  if SubstitutionFailed then Exit;

  // Now that all parameters have been substituted, we can evaluate the expression
  Formula := EvaluateExpression (Formula);
end;

procedure TPath.CalculateParameters;
var
  I, J, Idx: Integer;
  V: TVertex;
  ParamName, ParamValue, ExistingParamValue: String;
  EvaluationDone: Boolean;
begin
  FParameters.Clear;                                 // Start with an empty list of parameters
  for I := 0 to Count-1 do                           // Step through all vertices in the path
  begin
    V := Objects[I] as TVertex;
    for J := 0 to V.Count-1 do                       // Parse all parameters of each vertex
    begin
      EvaluationDone := False;                       // Initial value of the flag

      ParamName := V.Names[J];                       // Find the J-th parameter's name ...
      ParamValue := V.ValueFromIndex[J];             // ... and a value

      // If parameter value is empty, immediately continue with the next parameter
      if ParamValue = '' then
        Continue;

      // Handle the case of simplified syntax, when the value of the parameter starts right with the
      // operation sign (such as in power=+10)
      if ParamValue [1] in Operators then
      begin
        // Is there a parameter with this name already on the path?
        if FParameters.IndexOfName (ParamName) = -1 then
        begin
          // Value not defined yet. This is only allowed with the "+" operator, used to indicate
          // summation and string concatenation. If it is any other operator, this probably indicates
          // an error!
          if ParamValue [1] = cPlus then
            System.Delete (ParamValue, 1, 1)   // Strip the operator sign
          else
          begin
            MyShowMessageFmt ('Evaluation failed, offending formula: "%s=%s" ' +
              '(value of "%s" is not defined yet, and operator is not "%s")', [ParamName, ParamValue, ParamName, cPlus]);
            Halt (1);
          end;
        end
        else
        begin
          // Value IS defined; but is it a string or a number?
          ExistingParamValue := FParameters.Values[ParamName];  // This one is already on the path
          if not IsNumber (ExistingParamValue) then             // It's a string
          begin
            // Then, we perform a string concatenation. But the only allowed operator is a "+"
            if ParamValue [1] = cPlus then
            begin
              // Strip the operator sign
              System.Delete (ParamValue, 1, 1);
              // Concatenate strings
              ParamValue := ExistingParamValue + ParamValue;
              // Set the flag that we don't need to evaluate strings
              EvaluationDone := True;
            end
            else
            begin
              // Wrong operator specified -- not "+"
              MyShowMessageFmt ('Evaluation failed, offending formula: "%s=%s" ' +
                '(wrong operator "%s" for string concatenations, because "%s" is a ' +
                'string with value "%s")', [ParamName, ParamValue, ParamValue[1],
                ParamName, ExistingParamValue]);
              Halt (1);
            end;
          end
          else
          begin
            // Finally, existing value is a number, we put it in the beginning of the expression
            // to be evaluated, right before the operator sign. Then the general case evaluation
            // will take care of it.
            // If we reached here, the value is defined; continue
            ParamValue := ExistingParamValue + ParamValue;
          end;
        end;
      end;

      if not EvaluationDone then
      begin
        // One more step
        try
          EvaluateFormula (ParamValue);                  // If value contains a formula, evaluate it
        except
          MyShowMessageFmt ('Evaluation failed, offending formula: "%s=%s"', [ParamName, ParamValue]);
          Halt (1);
        end;
      end;

      // Save the results
      // Strip leading and trailing spaces for beauty
      ParamValue := Trim (ParamValue);

      // If a parameter with such a name already exists on the path, it will need updating, so we delete it from the path
      // (That's because we can't use method "TStrings.Values" below on sorted lists if the item is already there)
      Idx := FParameters.IndexOfName (ParamName);
      if Idx <> -1 then                              // Found
        FParameters.Delete (Idx);

      FParameters.Values[ParamName] := ParamValue;   // Save the new "parameter=value" pair in the list
    end;
  end;

end;

procedure TPath.InjectParameters (const ParamNames: TParameters);
var
  I: Integer;
begin
  // Accepts the list of parameter names.
  // If a particular parameter is not found in the current path, add it with an empty value.

  for I := 0 to ParamNames.Count-1 do                                           // Step through the names that we were passed
    if FParameters.IndexOfName (ParamNames.Strings[I]) = -1 then                // If no parameter with that name exists,
      FParameters.Add (ParamNames.Strings[I] + FParameters.NameValueSeparator); // then add it with an empty value
end;

{ TPathList }

procedure TPathList.Clear;
var
  I: Integer;
begin
  // The specific of TPathList is that it owns its objects, and frees their memory
  for I := Count-1 downto 0 do
    (Objects[I] as TPath).Free;
  inherited Clear;
end;

destructor TPathList.Destroy;
begin
  Self.Clear;  // First free the objects that we own
  inherited Destroy;
end;

{ TVertexRecordList }

constructor TVertexRecordList.Create;
begin
  inherited Create;
  Sorted := True;
end;

{ TEdge }

constructor TEdge.Create(VFrom, VTo: TVertex);
begin
  inherited Create;
  Assert (Assigned (VFrom), 'TEdge: Source vertex not created yet');
  Assert (Assigned (VTo), 'TEdge: Destination vertex not created yet');
  FFrom := VFrom;
  FTo := VTo;
end;

{ TVertex }

constructor TVertex.Create;
begin
  inherited Create;
  SetDefaults;     // Set important default properties upon creation
end;

function TVertex.ColorName: String;
begin
  if Color = White then
    Result := 'White' else
  if Color = Gray then
    Result := 'Gray' else
  if Color = Black then
    Result := 'Black' else
  Assert (True, ClassName + ': Internal inconsistency');
end;

function TVertex.PredecessorName: String;
begin
  if Assigned (Predecessor) then
    Result := Predecessor.Name
  else
    Result := 'Nil';
end;

procedure TVertex.SetDefaults;
begin
  // Sets important defaults
  Color := White;
  Predecessor := nil;
  DTimeStamp := 0;
  FTimeStamp := 0;
end;

{ TVertexList }

constructor TVertexList.Create (FreeObjects : Boolean);
begin
  inherited Create;
  Sorted := True;              // Sort objects to optimize access
  FFreeObjects := FreeObjects; // Callers can specify if they want us to free objects after deleting them
end;

procedure TVertexList.Clear;
var I: Integer;
begin
  if OwnsObjects then   // Free the memory of each object (the vertex).
    for I := Count-1 downto 0 do
      (Objects[I] as TVertex).Free;
  // (Now the list only contains their names)

  inherited Clear; // Delete all items from the list in one go
end;

destructor TVertexList.Destroy;
begin
  Self.Clear; // Remove all vertices referenced by list and free their memory (if required), then remove all names from the list
  inherited Destroy;
end;

function TVertexList.AddVertexByName(const VertexName: String): TVertex;
var
  V: TVertex;
begin
  V := TVertex.Create;
  V.Name := VertexName;
  AddObject (V.Name, V);
  Result := V;
end;

function TVertexList.CopyVertexByName(const SourceVertexName: String;
  const TargetVertexName: String): TVertex;
var
  VSource, VTarget: TVertex;
  Idx: Integer;
begin
  Idx := IndexOf (SourceVertexName);              // Make sure that a source vertex exists
  Assert (Idx <> -1, ClassName + ': Source vertex "' + SourceVertexName + '" not found');
  Assert (SourceVertexName <> TargetVertexName, ClassName + ': Source and target names cannot be equal when making a copy');
  VSource := Objects[Idx] as TVertex;
  VTarget := AddVertexByName (TargetVertexName);  // Create another vertex, with the target name
  VTarget.AddStrings (VSource);                   // Copy parameters of the source vertex to the target
  Result := VTarget;                              // Return the target as a result
end;

procedure TVertexList.AddVertex(const V: TVertex);
begin
  Assert (Assigned (V), ClassName + ': Vertex does not exist yet');
  AddObject (V.Name, V);
end;

procedure TVertexList.GetVertexListing(var L: TStringList);
var
  I: Integer;
  Vertex: TVertex;
  S: String;
begin
  L.Clear;  // Clear the string list that we were passed
  if Count = 0 then
    L.Add ('(The vertex list is empty)')
  else
    begin
      S := '';
      for I := 0 to Count-1 do
      begin
        Vertex := Objects[I] as TVertex;
        S := S + Vertex.Name;
        if Vertex.PartitionName <> '' then
          S := S + ' (Partition: "' + Vertex.PartitionName + '")';
        S := S + ', ';
      end;
      L.Add (S);
    end;
end;

{ TGraph }

function TGraph.GetVertexCount: Integer;
begin
  Result := FVertexRecordList.Count;
end;

function TGraph.GetPathCount: Longword;
begin
  Result := FPathList.Count;
end;

function TGraph.GetPartitionCount: Longword;
begin
  Result := FPartitionList.Count;
end;

function TGraph.InternalEdgeName(const VFromName, VToName: String): String;
// Private function, not public
begin
  // Edges have names of a specific format: "<from>=<to>"
  Result := VFromName + '=' + VToName;
end;

constructor TGraph.Create;
begin
  inherited Create;
  FVertexRecordList := TVertexRecordList.Create;
  FPathList := TPathList.Create;
  FEdgeList := TEdgeList.Create;
  FPartitionList := TStringList.Create;
  FPartitionList.Sorted := True;           // List of partitions is sorted
  FPartitionList.Duplicates := dupError;   // No duplicate partitions
end;

destructor TGraph.Destroy;
begin
  Self.Clear; // Free all memory used by auxiliary structures
  FPathList.Free;
  FEdgeList.Free;
  FPartitionList.Free;
  FVertexRecordList.Free;
  inherited Destroy;
end;

function TGraph.MakeVertexName(const SourceVertexName: String;
  const PartitionName: String): String;
// When vertices are added by copying them into a partition, we need to invent a new name for them,
// because vertex names across the graph must be unique.
begin
  Result := PartitionName + '_' + SourceVertexName;
end;

procedure TGraph.AddVertex(const V: TVertex);
var
  VertexRecord: TVertexRecord;
begin
  Assert (Assigned (V), ClassName + ': Trying to add vertex that was not created yet');
  Assert (FVertexRecordList.IndexOf(V.Name) = -1, ClassName + ': Trying to replace an already existing vertex "'
    + V.Name + '" in a graph');
  VertexRecord := TVertexRecord.Create;                   // Create an object to hold info about a vertex
  with VertexRecord do
  begin
    Vertex := V;                                          // Fill the first part
    AdjacentVertexList := TVertexList.Create(False);      // Create an adjacency list
  end;
  FVertexRecordList.AddObject (V.Name, VertexRecord);     // Store info
end;

procedure TGraph.AddVertexToPartition(const V: TVertex; const PartitionName: String);
var
  Partition: TVertexList;                                   // Partition just contains a list of vertices
  Idx: Integer;
begin

  AddVertex (V);                                            // Add vertex to the graph using the more general method
  if PartitionName = '' then Exit;                          // Nothing else to do

  // Otherwise (partition was specified)
  Assert (V.PartitionName = PartitionName, ClassName + ': When adding vertex "' + V.Name + '" to a partition, it''s partition ' +
    'field must already be set');

  Idx := FPartitionList.IndexOf (PartitionName);
  if Idx = -1 then                                          // No such partition exists, create it
  begin
    Partition := TVertexList.Create (False);
    FPartitionList.AddObject (PartitionName, Partition);
  end
  else
    Partition := FPartitionList.Objects[Idx] as TVertexList;

  Partition.AddVertex (V);                                  // Add vertex to the partition
end;

function TGraph.CreateVertexByName(const VertexName: String; const PartitionName: String): TVertex;
var
  V: TVertex;
begin
  V := TVertex.Create;                               // Create
  V.Name := VertexName;                              // Set fields
  if PartitionName <> '' then                        // If a partition was specified
  begin
    V.PartitionName := PartitionName;
    AddVertexToPartition (V, PartitionName);         // Add to a specific partition of the graph
  end
  else
    AddVertex (V);                                   // Otherwise, simply add
  Result := V;
end;

function TGraph.CopyVertexToPartition(const VertexName: String; const PartitionName: String): TVertex;
// Copies the specified vertex to the specified partition
var
  V: TVertex;
  NewName: String;
begin
  V := GetVertexByName (VertexName);                       // Does it exist in the graph?
  // It must exist and not already be in that partition
  Assert (Assigned (V), ClassName + ': No such vertex "' + VertexName + '" to copy to partition "' + PartitionName + '"');
  Assert (V.PartitionName <> PartitionName, ClassName + ': Vertex "' + VertexName + '" is already in partition "'+ PartitionName + '"');
  NewName := MakeVertexName (VertexName, PartitionName);   // Make a new name for it
  Result := CreateVertexByName (NewName, PartitionName);    // Create a new vertex
  Result.AddStrings (V);                                   // Copy parameters as well
end;

function TGraph.GetVertexByName(const VName: String): TVertex;
var
  I: Integer;
begin
  I := FVertexRecordList.IndexOf(VName);
  if I = -1 then
    Result := Nil
  else
    Result := (FVertexRecordList.Objects[I] as TVertexRecord).Vertex;
end;

function TGraph.GetVertexByNameAndPartition(const VName: String; const PartitionName: String): TVertex;
begin
  if PartitionName = '' then
    Result := GetVertexByName (VName)
  else
    Result := GetVertexByName (MakeVertexName (VName, PartitionName));
end;

function TGraph.AddEdge(const VFrom, VTo: TVertex): TEdge;
var
  E: TEdge;
  EdgeName: String;
begin
  // Vertices must exist
  Assert (Assigned (VFrom), ClassName + ': Source vertex does not exist when adding edge');
  Assert (Assigned (VTo), ClassName + ': Destination vertex does not exist when adding edge');
  // Multiple edges are not allowed
  EdgeName := InternalEdgeName (VFrom.Name, VTo.Name);
  Assert (FEdgeList.IndexOf (EdgeName) = -1, ClassName + ': Attempting to add a second edge from "' +
    VFrom.Name + '" to"' + VTo.Name + '"');
  // If all is well, create the edge
  E := TEdge.Create (VFrom, VTo);
  FEdgeList.AddObject (EdgeName, E);
  // Add the second vertex to the adjacency list of the first vertex
  (FVertexRecordList.Objects[FVertexRecordList.IndexOf(VFrom.Name)] as TVertexRecord).AdjacentVertexList.AddVertex(VTo);
  // Return the edge object as a result
  Result := E;
end;

function TGraph.AddEdgeByNames(const VFromName, VToName, PartFromName, PartToName: String): TEdge;
var
  VFrom, VTo: TVertex;
begin
  // Vertices with these names must already exist
  VFrom := GetVertexByNameAndPartition (VFromName, PartFromName);
  Assert (Assigned (VFrom), ClassName + ': Source vertex "' + VFromName + '" does not exist when adding edge');
  VTo := GetVertexByNameAndPartition (VToName, PartToName);
  Assert (Assigned (VTo), ClassName + ': Destination vertex "' + VToName + '" does not exist when adding edge');
  Result := AddEdge (VFrom, VTo);   // Actually add the edge
end;

procedure TGraph.ConnectPartitionsByName(const PartFromName, PartToName: String);
// Takes every vertex from the first partition and connects it with an edge to each vertex in the second partitions,
// thus forming a clique.
var
  PartFromIdx, PartToIdx: Integer;
  I, J: Integer;
  PartFrom, PartTo: TVertexList;
begin
  // Partitions must exist
  PartFromIdx := FPartitionList.IndexOf (PartFromName);
  PartToIdx := FPartitionList.IndexOf (PartToName);
  Assert (PartFromIdx <> -1, ClassName + ': No source partition "' + PartFromName + '" found when connecting partitions');
  Assert (PartToIdx <> -1, ClassName + ': No target partition "' + PartToName + '" found when connecting partitions');

  // Partitions are vertex lists
  PartFrom := FPartitionList.Objects[PartFromIdx] as TVertexList;
  PartTo := FPartitionList.Objects[PartToIdx] as TVertexList;

  // Nested loop to connect vertices
  for I := 0 to PartFrom.Count-1 do
    for J := 0 to PartTo.Count-1 do
      AddEdge (PartFrom.Objects[I] as TVertex, PartTo.Objects[J] as TVertex);

end;

procedure TGraph.ConnectPartitionToVertex(const PartName, VertexName: String);
var
  PartIdx: Integer;
  Vertex: TVertex;
  Partition: TVertexList;
  I: Integer;
begin
  PartIdx := FPartitionList.IndexOf (PartName);
  Assert (PartIdx <> -1, ClassName + ': No source partition "' + PartName + '" found when connecting from partition "' + PartName + '"');
  Partition := FPartitionList.Objects[PartIdx] as TVertexList;

  Vertex := GetVertexByName (VertexName);
  Assert (Assigned (Vertex), ClassName + ': No target vertex "' + VertexName + '" found when connecting from partition "' + PartName + '"');

  for I := 0 to Partition.Count-1 do
    AddEdge (Partition.Objects[I] as TVertex, Vertex);
end;

procedure TGraph.ConnectVertexToPartition(const VertexName, PartName: String);
var
  PartIdx: Integer;
  Vertex: TVertex;
  Partition: TVertexList;
  I: Integer;
begin
  PartIdx := FPartitionList.IndexOf (PartName);
  Assert (PartIdx <> -1, ClassName + ': No target partition "' + PartName + '" found when connecting to partition "' + PartName + '"');
  Partition := FPartitionList.Objects[PartIdx] as TVertexList;

  Vertex := GetVertexByName (VertexName);
  Assert (Assigned (Vertex), ClassName + ': No source vertex "' + VertexName + '" found when connecting to partition "' + PartName + '"');

  for I := 0 to Partition.Count-1 do
    AddEdge (Vertex, Partition.Objects[I] as TVertex);
end;

procedure TGraph.GetVertexListing(var L: TStringList);
var
  I: Integer;
begin
  L.Clear;   // Clear the string list that we were passed
  if FVertexRecordList.Count = 0 then
    L.Add ('(The vertex list in a graph is empty)')
  else
    for I := 0 to FVertexRecordList.Count-1 do
    begin
      with FVertexRecordList.Objects[I] as TVertexRecord do
      begin
        L.Add (Vertex.Name + ', color=' + Vertex.ColorName +
          ', predecessor=' + Vertex.PredecessorName + ', dtime=' + IntToStr(Vertex.DTimeStamp) +
          ', ftime=' + IntToStr(Vertex.FTimeStamp) + ', "' + Vertex.CommaText + '"');
        L.Add ('  >> Partition: "' + Vertex.PartitionName + '"');
        L.Add ('  >> Adjacent to: "' + AdjacentVertexList.CommaText + '"');
      end;
    end;
end;

procedure TGraph.GetPathListing(var L: TStringList);
// Provides a human-readable listing of paths, in a CSV format
var
  I, J: Integer;
  P: TPath;
  S: String;
begin
  L.Clear;   // Clear the string list that we were passed
  if FPathList.Count = 0 then
    L.Add ('No paths found yet')
  else
    begin                                    // There are paths, print the title first
      S := '"Path name", ';
      P := FPathList.Objects[0] as TPath;    // Take the first path as an example
      for I := 0 to P.Parameters.Count-1 do  // Step through parameters and extract their names
        S := S + '"' + P.Parameters.Names[I] + '", ';
      System.Delete (S, Length (S) -1, 2);   // Remove the trailing comma and a space
      L.Add (S);                             // Print the header

      for I := 0 to FPathList.Count-1 do     // Header is printed, now print the paths
      begin
        P := FPathList.Objects[I] as TPath;
        S := '"' + FPathList.Strings[I] + '", ';                      // Name of the path
        for J := 0 to P.Parameters.Count-1 do
          S := S + '"' + P.Parameters.ValueFromIndex[J] + '", ';      // Add the parameter value
        System.Delete (S, Length (S) - 1, 2);                         // Remove the trailing comma and a space
        L.Add (S);                                                    // Print the string with values
      end;
    end;
end;

function TGraph.GetPathByIndex(I: Integer): TPath;
// Returns a specified path, referenced by its index
begin
  Assert (I >= 0, ClassName + ': Path index "' + IntToStr (I) + '" less than zero was specified');
  Assert (I < FPathList.Count, ClassName + ': Path index "' + IntToStr (I) + '" out of bounds');
  Result := FPathList.Objects[I] as TPath;
end;

procedure TGraph.RemoveMarkings;
var
  I: Integer;
begin
  for I := 0 to FVertexRecordList.Count-1 do
    (FVertexRecordList.Objects[I] as TVertexRecord).Vertex.SetDefaults;
end;

procedure TGraph.NormalizePaths;
var
  ParamNames: TParameters;
  P: TPath;
  I, J: Integer;
begin
  if FPathList.Count = 0 then Exit;  // Nothing to do, exit

  // There are two tasks:
  // 1. Step through all paths and find which parameter names they have. Collect all parameter names.
  // 2. Step through them again, "injecting" the full list of parameters, so that all paths have the same
  //    parameter list.

  ParamNames := TParameters.Create;
  ParamNames.Sorted := True;                     // Must be sorted, otherwise the value of "Duplicates" is ignored
  ParamNames.Duplicates := dupIgnore;            // Accept duplicate values without error, but don't store them
  for I := 0 to FPathList.Count-1 do
  begin
    P := FPathList.Objects[I] as TPath;          // For each path in the graph...
    for J := 0 to P.Parameters.Count-1 do
      ParamNames.Add (P.Parameters.Names[J]);    // ...add names of parameters
  end;

  // Now "ParamNames" contains the full list of all parameter names, used by every path in the graph
  for I := 0 to FPathList.Count-1 do
    (FPathList.Objects[I] as TPath).InjectParameters (ParamNames);  // Inject the names

  ParamNames.Free;                               // Free the temporary object
end;

procedure TGraph.FindAllPaths;  // Traverse the graph using depth-first search

var
  Time: Longword;
  Path: TPath;
  PathNumber: Longword;

  procedure DFSVisit (UName: String);          // A sub procedure that implements depth-first search
  var
    I: Integer;
    U, V: TVertex;
    UAdjList: TVertexList;
    NewPath: TPath;
  begin
    I := FVertexRecordList.IndexOf (UName);                       // Check that a vertex with that name exists
    Assert (I <> -1, ClassName + ': Vertex with name "' + UName + '" not found');
    U := (FVertexRecordList.Objects[I] as TVertexRecord).Vertex;  // Find it

    Path.AddVertex (U);                                           // Add input vertex to the end of path

    U.Color := Gray;                                              // Mark as discovered
    Time := Time + 1;
    U.DTimeStamp := Time;                                         // Discovery time
    UAdjList := (FVertexRecordList.Objects[I] as TVertexRecord).AdjacentVertexList; // Find its adjacency list

    // Step through all neighbours on the adjacency list
    for I := 0 to UAdjList.Count-1 do
      begin
        V := UAdjList.Objects[I] as TVertex;  // Find each ancestor
        if V.Name = NameOfEndVertex then      // End found, add the current path to the list of found paths
          begin
            PathNumber := PathNumber + 1;     // Increment the path number
            NewPath := TPath.Create;          // Create a new path
            NewPath.Assign (Path);            // Copy current contents into it
            NewPath.AddVertex (V);            // Add the end vertex to the new path
            NewPath.CalculateParameters;      // Calculate parameters along the path
            FPathList.AddObject('Path_' + IntToStr (PathNumber), NewPath);  // Save the new path
          end;
 //       if V.Color = White then               // Traverse only if not traversed yet
 // This line is what makes it a depth-first search. It finds a spanning tree of the graph.
 // Since we want to find all paths, we need to explore the vertices even if they we already traversed.
          begin
            V.Predecessor := U;               // Save how we came here
            DFSVisit (V.Name);                // And explore recursively from here
          end;
      end;

    Path.RemoveVertexFromTail;                // Delete input vertex from the end of path

    U.Color := Black;                         // Now we finished with 'U'
    Time := Time + 1;
    U.FTimeStamp := Time;                     // Time when 'U' was finished
  end;

begin
  RemoveMarkings;                             // Need the graph to be in default state
  Time := 0;
  PathNumber := 0;

  FPathList.Clear;                            // Start anew

  Path := TPath.Create;
  DFSVisit (NameOfStartVertex);               // Perform the depth-first search
  Path.Free;

  NormalizePaths;
end;

procedure TGraph.LoadDatabase(const FileName: String);
// A rather complex procedure that parses an XML database
const
  idItem         = 'item';
  idPlace        = 'place';
  idConnect      = 'connect';
  idInclude      = 'include';
  idEdge         = 'edge';
  idFromAttr     = 'from';
  idToAttr       = 'to';
  idFromPartAttr = 'from-partition';
  idToPartAttr   = 'to-partition';
  // This one is Lazarus-specific; this string is returned by XML parsing library when it encounters a comment
  idComment = '#comment';

var
  V, FromVertex, ToVertex: TVertex;
  DB: TXMLDocument;
  XMLNode: TDOMNode;
  I: Integer;
  FromVertexName, ToVertexName, ToPartition: String;
  FromPartNode, ToPartNode, ToVertexNode: TDOMNode;
  Dir: String;

begin
  try
    try
      // Remember directory where our file is located
      Dir := ExtractFilePath (FileName);

      // Read XML file from disk
      // If a "file not found" exception occurs, it will be caught
      // by the outer "try..except" block.
      ReadXMLFile (DB, FileName);

      // If successful, proceed with the first node
      XMLNode := DB.DocumentElement.FirstChild;

      while Assigned (XMLNode) do
      begin

        // Comment detected; skip it
        if XMLNode.NodeName = idComment then
        begin
          XMLNode := XMLNode.NextSibling;
          Continue;
        end;

        // Include directive detected; include an external file by calling ourselves recursively
        if XMLNode.NodeName = idInclude then
        begin
          LoadDatabase (Dir + XMLNode.FirstChild.NodeValue);
          XMLNode := XMLNode.NextSibling;
          Continue;
        end;

        // Edge directive detected. It has the following structure:
        // <edge from=<from-vertex> [from-partition=<part>] [to=<to-vertex>] [to-partition=<part>]></edge>
        //
        // It connects <from-vertex> to <to-vertex>. If "to-partition" is specified as an optional argument,
        // then a copy of a vertex specified in "to" is created first in "to-partition", and it is this copy that is connected.
        // If "from-partition" is specified, a copy of "from" is connected, too; but it must exist beforehand.
        // If "to" is not specified, it is considered to be equal to "from".
        if XMLNode.NodeName = idEdge then
        begin
          FromVertexName := XMLNode.Attributes.GetNamedItem(idFromAttr).NodeValue;      // "from=..."

          ToVertexNode := XMLNode.Attributes.GetNamedItem(idToAttr);                    // Supposedly the "to=..." attribute
          if Assigned (ToVertexNode) then                                               // If it was specified
            ToVertexName := ToVertexNode.NodeValue
          else                                                                          // Was not specified
            ToVertexName := FromVertexName;                                             // Defaults to "from=..."

          FromPartNode := XMLNode.Attributes.GetNamedItem(idFromPartAttr);              // "from-partition=..."
          // If "from-partition" is specified, then we need to create an edge NOT from that particular vertex,
          // but from its EXISTING copy in the specified partition.
          if Assigned (FromPartNode) then
            FromVertex := GetVertexByNameAndPartition (FromVertexName, FromPartNode.NodeValue)   // Find a copy of the vertex
          else
            FromVertex := GetVertexByName (FromVertexName);

          Assert (Assigned (FromVertex), 'Vertex "' + FromVertexName + '" not yet defined.');

          ToPartNode := XMLNode.Attributes.GetNamedItem(idToPartAttr);                  // "to-partition=..."
          // Analogously, if "to-partition" is specified, then we need to create an edge NOT to that particular vertex,
          // but to its copy in the specified partition.
          if Assigned (ToPartNode) then
            ToVertex := CopyVertexToPartition (ToVertexName, ToPartNode.NodeValue)    // Create a new vertex by copying
          else
            ToVertex := GetVertexByName (ToVertexName);

          Assert (Assigned (ToVertex), 'Vertex "' + ToVertexName + '" not yet defined.');

          AddEdge (FromVertex, ToVertex);                                             // Add the edge

          XMLNode := XMLNode.NextSibling;
          Continue;
        end;

        // Place a copy of an item to a specified partition
        if XMLNode.NodeName = idPlace then
        begin
          ToPartition := XMLNode.Attributes.GetNamedItem(idToPartAttr).NodeValue;       // "to-partition=..."
          CopyVertexToPartition (XMLNode.FirstChild.NodeValue, ToPartition);

          XMLNode := XMLNode.NextSibling;
          Continue;
        end;

        // Connect two partitions together, each vertex to each, forming a clique
        if XMLNode.NodeName = idConnect then
        begin

          if Assigned (XMLNode.Attributes.GetNamedItem(idFromPartAttr)) then
          begin
            if Assigned (XMLNode.Attributes.GetNamedItem(idToPartAttr)) then                // Typical: two partitions specified
              ConnectPartitionsByName (XMLNode.Attributes.GetNamedItem(idFromPartAttr).NodeValue,
                XMLNode.Attributes.GetNamedItem(idToPartAttr).NodeValue)
            else
            if Assigned (XMLNode.Attributes.GetNamedItem(idToAttr)) then                    // Partition --> vertex
              ConnectPartitionToVertex (XMLNode.Attributes.GetNamedItem(idFromPartAttr).NodeValue,
                XMLNode.Attributes.GetNamedItem(idToAttr).NodeValue);
          end
          else
          if Assigned (XMLNode.Attributes.GetNamedItem(idFromAttr)) then
          begin
            if Assigned (XMLNode.Attributes.GetNamedItem(idToPartAttr)) then                // Vertex --> partition
              ConnectVertexToPartition (XMLNode.Attributes.GetNamedItem(idFromAttr).NodeValue,
                XMLNode.Attributes.GetNamedItem(idToPartAttr).NodeValue)
            else
            if Assigned (XMLNode.Attributes.GetNamedItem(idToAttr)) then                    // Vertex --> vertex; not supported
              Assert (False, 'When reading XML database: Cannot use "connect" for two vertices');
          end
          else                                                                              // Something went wrong
          Assert (False, 'When reading XML database: Incorrect syntax for connecting partitions');

          XMLNode := XMLNode.NextSibling;
          Continue;
        end;

        // Found a simple item that will be represented by a vertex
        if XMLNode.NodeName = idItem then
        begin
          V := CreateVertexByName (XMLNode.FirstChild.NodeValue, '');                 // Create vertex in a graph
          for I := 0 to XMLNode.Attributes.Length-1 do                                // Add attributes
            V.Add (XMLNode.Attributes.Item[I].NodeName + '=' +
                  XMLNode.Attributes.Item[I].NodeValue);
          XMLNode := XMLNode.NextSibling;
          Continue;
        end;

        // If we reached here, then we found an unknown construct in the input file.
        // Inform the user and proceed
        MyShowMessageFmt ('Unknown construct found while reading XML database: "%s", ignoring.', [XMLNode.NodeName]);
        XMLNode := XMLNode.NextSibling;

      end;
    finally
      DB.Free;
    end;
  except
    On EFOpenError do
      MyShowMessageFmt ('Unable to read the database file "%s"', [FileName]);
  end;
end;

procedure TGraph.Clear;
var
  I: Integer;
  VertexRecord: TVertexRecord;
begin
  // Clear edges
  for I := FEdgeList.Count-1 downto 0 do
    (FEdgeList.Objects[I] as TEdge).Free; // Free each edge object
  FEdgeList.Clear;                        // Delete all edges at once

  // Clear paths
  FPathList.Clear;

  // Clear partitions
  for I := FPartitionList.Count-1 downto 0 do
    (FPartitionList.Objects[I] as TVertexList).Free;
  FPartitionList.Clear;

  // Clear vertices
  for I := FVertexRecordList.Count-1 downto 0 do
  begin
    VertexRecord := (FVertexRecordList.Objects[I]) as TVertexRecord;
    with VertexRecord do
    begin
      AdjacentVertexList.Free;            // Free the adjacency list
      Vertex.Free;                        // Free the vertex
    end;
    VertexRecord.Free;                    // Free the vertex record itself
  end;
  FVertexRecordList.Clear;                // Delete all vertices' records (vertices and their adjacency lists) at once
end;

end.


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
unit Config;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, Math,
  Graph, FloatStr;                  // Our own units

const
  cEnabled = 'enabled';
  cSimpleObjFunc = 'simpleobjfunc';
  cObjFunc = 'objfunc';
  // Miscellaneous:
  cCommentCharacter = '#';
  cReasonDisabled = 'reason_disabled';
  cUntestedConstraints = 'untested_constraints';
  cByHeuristic = 'By heuristic';
  cFloatingPointException = 'Floating point exception';
  cStatus = 'status';
  cError = 'error';
  cErrorMessage = 'error_message';
  cNotAvailable = '(Not available)';
  cNA = '(N/A)';
  // String constants for design constraints:
  cMinPerformance = 'min_performance';
  cMaxPrefix = 'max_';
  cMinPrefix = 'min_';
  // Related to built-in models
  cBuiltIn = '(built-in)';

type

  { TDesignConstraints }

  TDesignConstraints = class (TStringList)
  end;

  { TGenericModel }

  // It is a successful compomise between generality and specificity.
  // Different design modules can interpret fields as they find appropriate. The "main" parameter is
  // passed in "Send", and web service's response is received in "Receive". The list of
  // "Constraints" may or may not be utilized by any particular design module.
  TGenericModel = class
    URL: String;
    Send: String;
    Receive: String;
    AlsoSend: String;
    AlsoReceive: String;
    Constraints: TDesignConstraints;
  end;

  { TConfig }

  TConfig = class (TStringList)
    private
      function GetEnabled: Boolean;
      function GetObjFunc: Single;
      function GetReasonDisabled: String;
      function GetSimpleObjFunc: Single;
      function GetUntestedConstraints: String;
      procedure SetEnabled(const AValue: Boolean);
      procedure SetObjFunc(const AValue: Single);
      procedure SetSimpleObjFunc(const AValue: Single);
    public
      constructor Create;
      procedure Disable (S: String);
      function CheckConstraints (Constr: TDesignConstraints): Boolean;
      procedure FillValues (var T: TStringList);
      procedure InjectNameValuePairs (const T: TStringList);
      property Enabled: Boolean read GetEnabled write SetEnabled;
      property ReasonDisabled: String read GetReasonDisabled;
      property UntestedConstraints: String read GetUntestedConstraints;
      property SimpleObjFunc: Single read GetSimpleObjFunc write SetSimpleObjFunc;
      property ObjFunc: Single read GetObjFunc write SetObjFunc;
  end;

  { TConfigList }

  TConfigList = class (TStringList)
  // This class manages its own memory
    protected
      FRequiresNormalize: Boolean;
      FSortBy: String;
      FSettings: TStringList;
      function GetItem(Index: Integer): TConfig;
      procedure ChangeHandler (Sender: TObject);
    public
      procedure Clear; override;
      constructor Create;
      destructor Destroy; override;
      procedure Assign (Source: TPersistent); override;
      function CreateListItem: TConfig; virtual;
      procedure GetConfigListing (var L: TStringList);
      function CountEnabled: Integer;
      function DeleteDisabled: Boolean;
      procedure InjectNameValuePairs (const T: TStringList);
      procedure ApplySettings;
      procedure LoadDatabase (const FileName: String); virtual;
      procedure LoadFromCSV (const FileName: String);
      procedure Normalize;
      procedure SortByObjFunc;
      procedure SortByName (const aName: String);
      procedure ReverseSortByName (const aName: String);
      function CheckConstraints (AConstraints: TDesignConstraints): Boolean;
      function StatusBarText: String;
      property Items[Index: Integer]: TConfig read GetItem; default;    // As in "TFPObjectList"
      property SortBy: String read FSortBy;
      property Settings: TStringList read FSettings write FSettings;
  end;

  // Diverse useful functions
  function SimpleObjFuncCompare (List: TStringList; Index1, Index2: Integer): Integer;
  function ObjFuncCompare (List: TStringList; Index1, Index2: Integer): Integer;
  function NameFuncCompare (List: TStringList; Index1, Index2: Integer): Integer;
  function ReverseNameFuncCompare (List: TStringList; Index1, Index2: Integer): Integer;
  function NVPName (S: String): String;
  function NVPValue (S: String): String;

implementation

// The following three functions are not methods of any class but are standalone functions instead.
// Only so the function can be passes as a parameter to the "CustomSort" method of "TSTringList".

function SimpleObjFuncCompare (List: TStringList; Index1, Index2: Integer): Integer;
// Used for sorting the list of configurations based on the value of a simple objective function.
// Returns "-1" if the value of SimpleObjFunc for item with Index1 is smaller (i.e., better) than
// the value for item with Index2.
var
  v1, v2: Single;
begin
  // Get values of SimpleObjFunc
  v1 := (List.Objects[Index1] as TConfig).SimpleObjFunc;
  v2 := (List.Objects[Index2] as TConfig).SimpleObjFunc;

  if v1 < v2 then
    Result := -1
  else
  if v1 > v2 then
    Result := 1
  else           // else, v1 = v2
    Result := 0;
end;

function ObjFuncCompare (List: TStringList; Index1, Index2: Integer): Integer;
// Used for sorting the list of configurations based on the value of an objective function.
// Returns "-1" if the value of ObjFunc for item with Index1 is smaller (i.e., better) than
// the value for item with Index2.
var
  v1, v2: Single;
begin
  // Get values of ObjFunc
  v1 := (List.Objects[Index1] as TConfig).ObjFunc;
  v2 := (List.Objects[Index2] as TConfig).ObjFunc;

  if v1 < v2 then
    Result := -1
  else
  if v1 > v2 then
    Result := 1
  else           // else, v1 = v2
    Result := 0;
end;

function NameFuncCompare (List: TStringList; Index1, Index2: Integer): Integer;
// The most general custom sort. Fetches the name of the metric we want to sort by,
// and does comparisons of values of that metric in the two configurations.
// Returns "-1" if the value of SimpleObjFunc for item with Index1 is smaller (i.e., better) than
// the value for item with Index2.
var
  AConfList: TConfigList;
  v1, v2: String;
  v1_float, v2_float: Single;
  BothAreNumbers: Boolean;
begin
  // Perform a typecast for easier references
  AConfList := List as TConfigList;

  // "AConfList.SortBy" must already contain the metric name by the time we are called.
  // Fetch values of the metric from both configurations
  v1 := AConfList [Index1].Values [AConfList.SortBy];
  v2 := AConfList [Index2].Values [AConfList.SortBy];

  // They could be numbers! We need to check this, because string comparison would give incorrect
  // results (e.g., "1000" < "700")
  // Hope for the best:
  BothAreNumbers := True;
  try
    // Try to convert the first string
    if Pos('.', v1) > 0 then
      v1_float := StrToFloat (v1, FPointSeparator)
    else
      v1_float := StrToFloat (v1, FCommaSeparator);
    // Now, the second string
    if Pos('.', v2) > 0 then
      v2_float := StrToFloat (v2, FPointSeparator)
    else
      v2_float := StrToFloat (v2, FCommaSeparator);
  except
    // If conversion failed, then at least one string was not a true number
    on EConvertError do
      BothAreNumbers := False;
  end;

  if BothAreNumbers then
  // Compare as numbers
  begin
    if v1_float < v2_float then
      Result := -1
    else
    if v1_float > v2_float then
      Result := 1
    else           // else, they are equal
      Result := 0;
  end
  else
  // Compare as strings
  begin
    if v1 < v2 then
      Result := -1
    else
    if v1 > v2 then
      Result := 1
    else           // else, v1 = v2
      Result := 0;
  end;
end;

function ReverseNameFuncCompare (List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result := -NameFuncCompare (List, Index1, Index2);
end;

function NVPName (S: String): String;
// Returns a name in a "Name=Value" pair
var
  I: Integer;
begin
  I := AnsiPos ('=', S);
  if I = 0 then         // '=' not found
    Result := S         // Return the whole string
  else
    Result := Copy (S, 1, I - 1);
end;

function NVPValue (S: String): String;
// Returns a value in a "Name=Value" pair
var
  I: Integer;
begin
  I := AnsiPos ('=', S);
  if I = 0 then         // Value not found
    Result := ''
  else
    Result := Copy (S, I + 1, Length (S) - I);
end;

{ TConfig }

function TConfig.GetEnabled: Boolean;
begin
  Result := StrToBool (Values [cEnabled]);
end;

function TConfig.GetObjFunc: Single;
var
  S: String;
begin
  // Not all configurations have their "ObjFunc" set. A particular example is a disabled configuration
  // that doesn't participate in the design procedures.
  // When the value is not set, we return "a big number", which symbolizes an inferior configuration
  // (because the smaller the objective function, the better the design).
  S := Values [cObjFunc];
  if S <> '' then
    Result := GenericStrToFloat (S)
  else
    Result := MaxSingle;
end;

function TConfig.GetReasonDisabled: String;
begin
  Result := Values [cReasonDisabled];
end;

function TConfig.GetSimpleObjFunc: Single;
// Acts similarly to "GetObjFunc" method
var
  S: String;
begin
  S := Values [cSimpleObjFunc];
  if S <> '' then
    Result := GenericStrToFloat (S)
  else
    Result := MaxSingle;
end;

function TConfig.GetUntestedConstraints: String;
begin
  Result := Values [cUntestedConstraints];
end;

procedure TConfig.SetEnabled(const AValue: Boolean);
begin
  Values [cEnabled] := BoolToStr (AValue, True);
end;

procedure TConfig.SetObjFunc(const AValue: Single);
begin
  // XXX: Hard-wired: 2 decimal digits
  Values [cObjFunc] := FloatToStrF (AValue, ffFixed, 0, 2);
end;

procedure TConfig.SetSimpleObjFunc(const AValue: Single);
begin
  // XXX: Hard-wired: 2 decimal digits
  Values [cSimpleObjFunc] := FloatToStrF (AValue, ffFixed, 0, 2);
end;

constructor TConfig.Create;
begin
  inherited Create;
  // By default, the configuration is enabled at the time of creation:
  Enabled := True;
end;

procedure TConfig.Disable (S: String);
// Disables the configuration, providing a reason
var
  R: String;
begin
  // Only process enabled configurations
  if (not Enabled) then Exit;

  // Disable the configuration
  Enabled := False;

  // Is any other reason currently specified?
  R := Values [cReasonDisabled];
  if R <> '' then
    R += '; ';

  // Append the specified reason
  Values [cReasonDisabled] := R + S;
end;

function TConfig.CheckConstraints (Constr: TDesignConstraints): Boolean;
// Check the current config for constraints. In case of violation, disable the config with an explanation.
// Returns "False" if everything is fine, or "True" if constraints were violated, and the config was disabled
// (or if it was already disabled when the function was called)
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
  Msg: String;
  UntestedConstr: String;
begin
  // Default is failure: constraints were violated
  Result := True;

  // Only proceed with enabled constraints
  if not Enabled then Exit;

  // The debug messages explaining which constraints were violated or not tested
  Msg := cViolatedMessage;
  UntestedConstr := '';

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
    if I <> -1 then                  // If found
    begin
      // What value is stored in the config
      CurValue := GenericStrToFloat (Values [CName]);
      // What value we need
      NeedValue := GenericStrToFloat (CValue);

      if CType = MaxConstraint then  // It's a Max constraint
      begin
        if CurValue > NeedValue then // '>', constraint violated
          Msg += CName + Sep;
      end
      else                           // It's a Min constraint
      begin
        if CurValue < NeedValue then // '<', constraint violated
          Msg += CName + Sep;
      end;
    end
    else
      UntestedConstr += ConstrName + Sep;
  end;

  // Now, all constraints have been processed. If the "Msg" is not equal to its initial value,
  // then something was added to it, which is also an indication of constraint violation.
  // Remove the trailing separator, then disable the config, providing the message
  // as the explanation.
  if Msg <> cViolatedMessage then
  begin
    // Delete the trailing separator
    System.Delete (Msg, Length (Msg) - Length (Sep) + 1, Length (Sep));
    Disable (Msg);
  end
  else
    Result := False;    // All went well, no constraints violated

  // If all constraints were tested successfully, try to find and delete the corresponding field
  // in our configuration
  if UntestedConstr = '' then
  begin
    I := IndexOfName (cUntestedConstraints);
    if I <> -1 then
      Delete (I);
  end
  else
  begin
    // Alternatively, if some constraints could not be tested (characteristics were unavailable),
    // we save that information, too. The configuration stays enabled.
    // Delete the trailing separator
    System.Delete (UntestedConstr, Length (UntestedConstr) - Length (Sep) + 1, Length (Sep));
    Values [cUntestedConstraints] := UntestedConstr;
  end;
end;

procedure TConfig.FillValues (var T: TStringList);
// Inspects the list "T", which consists of strings that are either a "Name=Value" pair, or just "Name".
// Leaves "Name=Value" pairs intact, and for the "Name" strings tries to find a corresponding value in the
// current config, and if found, returns it together with a name, in another "Name-Value" pair.
var
  I, Idx: Integer;
begin
  for I := 0 to T.Count-1 do
    if T.Names[I] = '' then        // If the string does not contain a name-value separator, then TStrings.Names would return an empty value.
    begin                          // This indicates that the entire string T[I] is the name.
      Idx := IndexOfName (T [I]);  // Try to find its value in the current configuration
      if Idx <> -1 then
        T [I] := T [I] + T.NameValueSeparator + ValueFromIndex [Idx];  // If found, add '=' and a value
    end;
end;

procedure TConfig.InjectNameValuePairs (const T: TStringList);
// Inspects the list "T" and finds strings that are "Name=Value" pairs, then adds them to the configuration.
// If a pair with this name aready exists, its value is updated.
var
  I, Idx: Integer;
begin
  for I := 0 to T.Count-1 do
  begin
    if T.Names [I] = '' then Continue;   // Ignore strings that are not "Name=Value" pairs
    Idx := IndexOfName (T.Names [I]);    // Try to find this name in the current config
    if Idx <> -1 then
      Strings [Idx] := T [I]             // This name was found, update the entire string
    else
      Add (T [I]);                       // No such name found, add the string
  end;
end;

{ TConfigList }

function TConfigList.GetItem(Index: Integer): TConfig;
begin
  Result := Objects[Index] as TConfig;
end;

procedure TConfigList.ChangeHandler(Sender: TObject);
// This one is called when the list has been changed
begin
  // Set the flag that the list requires normalization
  FRequiresNormalize := True;

  { XXX: To be completely precise, normalization of the list is required not on _every_ change,
    but rather only when new items are added (or an item gets new fields added). When items are deleted,
    repeated normalization is not required. However, to implement this cleanly, we need to re-implement
    the "Add" method to send a new type of notification event, i.e., "OnAdd", that would be connected
    to this procedure. Seems not worth the trouble, at least now, when normalization is fairly fast. }
end;

procedure TConfigList.Clear;
var
  I: Integer;
begin
  for I := Count-1 downto 0 do
    Items[I].Free;
  // (Now the list only contains config names)

  inherited Clear; // Delete all items from the list in one go
end;

constructor TConfigList.Create;
begin
  inherited Create;
  // Set a handler that is invoked when the list is changed
  OnChange := @ChangeHandler;
  // Create a list of settings ("Name=Value" pairs) that can be filled
  // and then added to all configurations in the list
  FSettings := TStringList.Create;
end;

destructor TConfigList.Destroy;
begin
  Self.Clear; // Remove all configs referenced by list and free their memory, then remove all names from the list
  FSettings.Free;
  inherited Destroy;
end;

procedure TConfigList.Assign (Source: TPersistent);
var
  S : TConfigList;
  AConfig: TConfig;
  I: Integer;
begin
  if Source is TConfigList then
  begin
    S := TConfigList (Source);                 // An explicit typecast
    for I := 0 to S.Count-1 do                 // Loop through items in the source list
    begin
      AConfig := Self.CreateListItem;          // Create a new item
      AConfig.Assign (S[I]);                   // Fill it
      Self.AddObject (S.Strings[I], AConfig);  // Add it with the same name
    end;
  end
  else
    inherited Assign (Source);
end;

function TConfigList.CreateListItem: TConfig;
begin
  Result := TConfig.Create;
end;

procedure TConfigList.GetConfigListing (var L: TStringList);
// Provides a human-readable listing of configurations, in a CSV format
var
  I, J: Integer;
  C: TConfig;
  S: String;
begin
  L.Clear;   // Clear the string list that we were passed
  if Count = 0 then
    L.Add ('No configurations exist')
  else
    begin                                    // There are configurations, print the title first
      S := '"Configuration ID",';
      C := Items[0];                         // Take the first configuration as an example
      for I := 0 to C.Count-1 do             // Step through parameters and extract their names
        S := S + '"' + C.Names[I] + '",';
      System.Delete (S, Length (S), 1);      // Remove the trailing comma
      L.Add (S);                             // Print the header

      for I := 0 to Count-1 do               // Header is printed, now print the configurations
      begin
        C := Items[I];
        S := '"' + Strings[I] + '",';                                 // ID of the configuration
        for J := 0 to C.Count-1 do
          S := S + '"' + C.ValueFromIndex[J] + '",';                  // Add the parameter value
        System.Delete (S, Length (S), 1);                             // Remove the trailing comma
        L.Add (S);                                                    // Print the string with values
      end;
    end;
end;

function TConfigList.CountEnabled: Integer;
// Return the number of configurations currently marked as "Enabled"
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count-1 do
    if Items[I].Enabled then
      Result += 1;
end;

function TConfigList.DeleteDisabled: Boolean;
// Deletes items marked as "disabled", returns "True" if there have been deletions
var
  I: Integer;
begin
  Result := False;
  for I := Count-1 downto 0 do
    if (not Items[I].Enabled) then       // Found an item that is disabled
    begin
      Items[I].Free;                     // Free the item's memory
      Delete (I);                        // Delete the name of the item from our list
      Result := True;                    // Set the function's result to indicate the change
    end;
end;

procedure TConfigList.InjectNameValuePairs(const T: TStringList);
// Inject "Name=Value" pairs into all enabled items
var
  I: Integer;
begin
  for I := 0 to Count-1 do
    if Items[I].Enabled then
      Items[I].InjectNameValuePairs(T);
end;

procedure TConfigList.ApplySettings;
// Takes "Name=Value" pairs stored in "Settings" and adds them to all configs in the list
// Note: you cannot (easily) reverse this action.
begin
  InjectNameValuePairs(Settings);
end;

procedure TConfigList.LoadDatabase(const FileName: String);
var
  G: TGraph;
  AConf: TConfig;
  APath: TPath;
  I: Integer;
begin
  // Create a new graph object
  G := TGraph.Create;

  try
    // Load the database specified in "FileName" into graph "G"
    G.LoadDatabase (FileName);

    // Finds all combinations (all paths in the graph)
    G.FindAllPaths;

    // Now follows a transition from a graph representation of hardware combinations to a set of "configurations",
    // with each of configurations being described by a set of parameters and their values (name-value pairs).

    // Clear ourselves before loading any new data
    Clear;

    // For all possible combinations (all paths in the graph)
    for I := 0 to G.PathCount-1 do
    begin
      AConf := CreateListItem;                   // Create a new object to store a configuration
      APath := G.GetPathByIndex (I);             // Get the link to the path object. A path contains a list of parameters and their values
      AConf.AddStrings (APath.Parameters);       // Save those parameters in a configuration
      // NB: Below is an important part: configuration numbering starts from _one_!
      AddObject (IntToStr (I+1), AConf);         // Add to ourselves the current configuration
    end;
  finally
    // Free the graph object
    G.Free;
  end;
end;

procedure TConfigList.LoadFromCSV (const FileName: String);
{ If the generative graph was used to generate configurations (see method "LoadDatabase"), and those were saved in a CSV file,
  then we can read them back anytime. This process is guaranteed to arrive to the same results as using the generative graph.
  But consistency is guaranteed if the user didn't manually change the contents of the CSV file.
  No checking is performed when loading this data (because it is not quite clear what exactly should be checked). The ways
  the data could be mangled manually are just too numerous.

  Child classes (such as TClusterConfigList) that implement their own enhanced version of "LoadDatabase", by computing
  some additional characteristics of the configuration (such as peak performance, for example), don't need to
  provide a corresponding "enhanced" version of the "LoadFromCSV" method. The reason is as follows. The only way to
  get CSV representation is by using the generative graph with "LoadDatabase". Using "LoadDatabase" from TClusterConfigList
  already computes additional characteristics such as peak performance. These data are already in CSV. No need to compute them
  in "LoadFromCSV". Hopefully this long explanation makes sense.
}
var
  S: TStringList;      // Here we will read the data from a CSV file
  C: TStringList;      // Here we will store characteristics of each consecutive configuration
  Params: TStringList; // Here we will store names of parameters
  AConf: TConfig;
  I, J, K: Integer;
begin
  S := TStringList.Create;
  C := TStringList.Create;
  Params := TStringList.Create;

  try
    S.LoadFromFile (FileName);  // Read strings from the CSV file

    Clear;  // Clear ourselves before loading any new data

    // Skip leading comments, if any
    K := 0;
    while K < S.Count-1 do
    begin
      if S[K][1] <> cCommentCharacter then Break;  // Break the loop as the first non-comment is found
      K := K + 1;
    end;

    if K = S.Count then Exit; // We reached the end of the file, but there were only comments

    // If we reached here, then the K'th line is a non-comment line
    // The first non-comment line stores parameter names
    Params.CommaText := S[K];
    // Start the loop from "K+1", as the K'th line has been analyzed
    for I := K + 1 to S.Count-1 do
    begin
      if S[I][1] = cCommentCharacter then Continue;  // Ignore comments
      C.CommaText := S[I];      // Add each string read from the CSV file to a temporary container
      AConf := CreateListItem;  // Create a new configuration
      // Parameter #0 is a Configuration ID, therefore we start from parameter #1
      for J := 1 to Params.Count-1 do             // Loop through all parameters
        AConf.Values [Params[J]] := C[J];         // Add (or replace) "parameter=value" strings to the current configuration
      // The name is a configuration ID, as it was read from the CSV file
      AddObject (C[0], AConf);                    // Add to ourselves the current configuration
    end;
  finally
    S.Free;
    C.Free;
    Params.Free;
  end;
end;

procedure TConfigList.Normalize;
// Different configurations in the list may have different sets of characteristics.
// The task of this procedure is to find a union of all possible characteristics,
// and populate each configuration with the missing fields.
// Works similarly to "TGraph.NormalizePaths"
var
  ChrNames: TStringList;                     // List to hold the names of characteristics
  I, J: Integer;
  AItem: TConfig;
begin
  // Check if the list requires to be normalized, or if it is empty
  if (FRequiresNormalize = False) or (Count = 0) then Exit;   // Nothing to do

  // Two tasks:
  // 1. Step through all configurations, collecting names of characteristics
  // 2. Step again, injecting missing names into configurations

  ChrNames := TStringList.Create;
  ChrNames.Sorted := True;                   // Must be sorted, otherwise the value of "Duplicates" is ignored
  ChrNames.Duplicates := dupIgnore;          // Accept duplicate values without error, but don't store them

  // We need to ignore duplicates. We could use an unsorted list for that, and manually check if an item
  // already exists before adding it. But this is long (O(N) complexity). A sorted list uses binary search to check for
  // duplicates before adding items, that's why it is faster (O(log2(N)) complexity). Or least it should be.

  // Collect names of characteristics
  for I := 0 to Count-1 do                   // For each configuration
  begin
    AItem := Items[I];
    for J := 0 to AItem.Count-1 do           // ("J" steps through characteristics within a configuration)
      ChrNames.Add (AItem.Names[J]);         // Add names of characteristics
  end;

  // Now "ChrNames" contains the full sorted list of all characteristics, used by every configuration
  // Step again, injecting the names
  for I := 0 to Count-1 do
  begin
    AItem := Items[I];

    for J := 0 to ChrNames.Count-1 do                        // (Now, "J" steps through entries in "ChrNames")
      if AItem.IndexOfName (ChrNames[J]) = -1 then           // Name not found
        AItem.Add (ChrNames[J] + AItem.NameValueSeparator);  // So we need to add it with an empty value

    AItem.Sort;  // Only by sorting characteristics inside a configuration we can ensure a consistent look of all configurations
    // (Vitally important for export to spreadsheets, etc.)
    AItem.Sorted := False;  // No need to maintain sorted order after that
  end;

  // Free the temporary object
  ChrNames.Free;

  // The list has been normalized, set the flag accordingly
  FRequiresNormalize := False;
end;

procedure TConfigList.SortByObjFunc;
begin
  CustomSort (@ObjFuncCompare);
end;

procedure TConfigList.SortByName (const aName: String);
begin
  // First we must assign the metric name by whose values we will sort
  FSortBy := aName;
  CustomSort (@NameFuncCompare);
end;

procedure TConfigList.ReverseSortByName(const aName: String);
// Same as above, but diametrically different
begin
  // Sort by this metric
  FSortBy := aName;
  CustomSort (@ReverseNameFuncCompare);
end;

function TConfigList.CheckConstraints (AConstraints: TDesignConstraints): Boolean;
// Steps through configs that are still enabled and checks if constraints are met.
// Returns "True" if some configs were disabled on the way.
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Count-1 do
    with Items[I] do
      if Enabled then                                   // Only deal with enabled items
        if CheckConstraints (AConstraints) then
          Result := True;                               // Indicate that at least one config was disabled
end;

function TConfigList.StatusBarText: String;
begin
  // Returns text to be displayed in the status bar.
  // And don't forget to add a number of spaces in the end of the line, or the tail of
  // your string will be covered with the visual control that allows to drag
  // the bottom right corner of the form.
  Result := 'Loaded: ' + IntToStr (Count) + ', enabled: ' + IntToStr (CountEnabled) + '      ';
end;

end.


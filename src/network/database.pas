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
unit database;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, XMLRead, DOM,
  common;                                       // Our own units

const
  // XML entries
  idItem         = 'item';
  idInclude      = 'include';
  // This XML id is Lazarus-specific; this string is returned by XML parsing library when it encounters a comment
  idComment      = '#comment';

procedure LoadSwitchDBFromCSVFile (var L: TSwitchList; const FileName: String);

implementation

procedure LoadSwitchDBFromCSVFile(var L: TSwitchList; const FileName: String);
// Resembles method "TConfigList.LoadFromCSV" from the "dbcli" tool
var
  S: TStringList;      // Here we will read the data from a CSV file
  C: TStringList;      // Here we will store characteristics of each consecutive configuration
  Params: TStringList; // Here we will store names of parameters
  I, J, K: Integer;
  Sw: TSwitch;
  ModelStr: String;
  ModelIndex: Integer;
begin
  S := TStringList.Create;
  C := TStringList.Create;
  Params := TStringList.Create;

  try
    S.LoadFromFile (FileName);  // Read strings from the CSV file

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
    // What is the index of a parameter that holds the switch model?
    ModelIndex := Params.IndexOf(cSwitchModel);
    // Start the loop from "K+1", as the K'th line has been analyzed
    for I := K + 1 to S.Count-1 do
    begin
      if S[I][1] = cCommentCharacter then Continue;  // Ignore comments
      C.CommaText := S[I];                           // Add each string read from the CSV file to a temporary container
      Sw := TSwitch.Create;                          // Create an object for a new switch
      ModelStr := C[ModelIndex];                     // Get the model name
      // Parameter #0 is a Configuration ID, therefore we start from parameter #1
      for J := 1 to Params.Count-1 do                // Loop through all parameters
        Sw.Values [Params[J]] := C[J];               // Add (or replace) "parameter=value" strings to the current switch configuration

      L.AddObject (ModelStr, Sw);                    // Add switch to the list
    end;
  finally
    S.Free;
    C.Free;
    Params.Free;
  end;
end;

end.


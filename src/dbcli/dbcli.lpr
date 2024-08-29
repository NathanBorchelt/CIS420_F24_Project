{
Copyright (C) 2012 Konstantin S. Solnushkin

This file is part of "dbcli", the tool to query graph-based databases.

    "dbcli" is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    "dbcli" is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with "dbcli".  If not, see <http://www.gnu.org/licenses/>.
}
program dbcli;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp,
  config, FloatStr, evaluate, common;                          // Our own units

const
  cInputFileNameOption = 'i';
  cInputFileTypeOption = 't';
  cConstraintsOption = 'c';
  cDeleteOption = 'd';
  cSortByOption = 's';
  cReverseSortOption = 'r';
  cNumLinesOption = 'n';
  cFirstConfigOption = '1';
  cXML = 'xml';
  cCSV = 'csv';

type

  TInputFileType = (tXML, tCSV);

  { TCliApplication }

  TCliApplication = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TCliApplication }

procedure TCliApplication.DoRun;
var
  Opts, NonOpts: TStringList;
  ErrorMsg: String;
  E: Exception;
  I: Integer;
  AConfList: TConfigList;
  AConstr: TDesignConstraints;
  TempStrings: TStringList;
  FileName: String;
  FileType: TInputFileType;
  FileTypeStr: String;
  ShouldImposeConstraints, ShouldDelete,
    ShouldSort, ReverseSort,
    ShouldLimitNumLines,
    ShouldPrintFirstConfig: Boolean;
  SortByStr: String;
  LinesOfOutput: Integer;
begin

  // Check command-line parameters
  Opts := TStringList.Create;
  NonOpts := TStringList.Create;
  ErrorMsg := CheckOptions ('h' +             // Help option
    cInputFileNameOption + ':' +              // Input option requires a file name, hence ":"
    cInputFileTypeOption + ':' +              // File type option, if specified, also requires a value
    cConstraintsOption + ':' +                // The value of this option is the long quoted string of constraints
    cDeleteOption +                           // The option for deletion of disabled comments
    cSortByOption + ':' +                     // By which metric we should sort; requires a value
    cReverseSortOption +                      // Whether we should sort in reverse order
    cNumLinesOption + ':' +                   // The number of lines in the output
    cFirstConfigOption,                       // Whether we should only report the first configuration
    Nil, Opts, NonOpts);
  if ErrorMsg <> '' then
  begin
    // Invalid options
    E := Exception.Create (ErrorMsg);
    ShowException (E);
    E.Free;
    // Clear the two lists before exiting
    Opts.Free;
    NonOpts.Free;
    Terminate;
    Exit;
  end;

  // If we reached here, the options were valid.
  // We can even examine them:
  {Writeln ('==================================');
  Writeln ('You passed options:');
  for I := 0 to Opts.Count-1 do
    Writeln (Opts [I]);
  Writeln ('You passed non-options:');
  for I := 0 to NonOpts.Count-1 do
    Writeln (NonOpts [I]);
  Writeln ('==================================');}

  // Free the memory for the debug output
  Opts.Free;
  NonOpts.Free;


  // If help was requested
  if HasOption ('h', 'help') then
  begin
    WriteHelp;
    Exit;
  end;

  // Input file name must be specified
  if not HasOption (cInputFileNameOption) then
  begin
    WriteHelp;
    Exit;
  end
  else
    // Get the file name from command line
    FileName := GetOptionValue (cInputFileNameOption);

  // Now, which type is it? If extension is specified, use it
  if Pos ('.' + cXML, FileName) <> 0 then
    FileType := tXML
  else
    if Pos ('.' + cCSV, FileName) <> 0 then
      FileType := tCSV
  else
    // Extension not found automatically, use the option that specifies it
    begin
      if not HasOption (cInputFileTypeOption) then  // But no such option was specified
      begin
        WriteHelp;
        Exit;
      end;
      // If we reached here, the value for this option was specified. Analyze it.
      FileTypeStr := GetOptionValue (cInputFileTypeOption);
      if FileTypeStr = cXML then
        FileType := tXML
      else
        if FileTypeStr = cCSV then
          FileType := tCSV
      else  // The value of option is invalid, tell the user
        begin
          WriteHelp;
          Exit;
        end;
    end;

  // By this point, "FileName" and "FileType" are both set.
  // Check the constraints option

  if HasOption (cConstraintsOption) then
  begin
    ShouldImposeConstraints := True;
    // Get the constraints string
    AConstr := TDesignConstraints.Create;
    AConstr.CommaText := GetOptionValue (cConstraintsOption);
  end
  else
    ShouldImposeConstraints := False;

  // Shall we delete disabled configurations?
  if HasOption (cDeleteOption) then
    ShouldDelete := True
  else
    ShouldDelete := False;

  // Shall we sort the list?
  if HasOption (cSortByOption) then
  begin
    ShouldSort := True;
    SortByStr := GetOptionValue (cSortByOption);
  end
  else
    ShouldSort := False;

  // Shall we do the reverse sort?
  if HasOption (cReverseSortOption) then
  begin
    // The sorting option must itself be present, too:
    if not HasOption (cSortByOption) then
    begin
      WriteHelp;
      Exit;
    end;
    ReverseSort := True;
  end
  else
    ReverseSort := False;

  // Shall we limit the number of output lines?
  if HasOption (cNumLinesOption) then
  begin
    ShouldLimitNumLines := True;
    LinesOfOutput := StrToInt (GetOptionValue (cNumLinesOption));
  end
  else
    ShouldLimitNumLines := False;

  // Shall we only print the first configuration?
  ShouldPrintFirstConfig := HasOption (cFirstConfigOption);

  // The options to print a certain number of configurations and to print
  // only the first configuration are contradictory: only one must be present, or none at all.
  // Both cannot be present, so the logical "AND" will help to determine that condition:
  if ShouldLimitNumLines and ShouldPrintFirstConfig then
  begin
    WriteHelp;
    Exit;
  end;

  // Finally, all options were analyzed, and we are ready to do the main task
  AConfList := TConfigList.Create;
  TempStrings := TStringList.Create;
  try
    // Try loading the files
    try
      // Determine which type of file we shall load
      if FileType = tXML then
        AConfList.LoadDatabase (FileName)
      else // it is "tCSV"
        AConfList.LoadFromCSV (FileName);
    except
      // Probably a file was not found, or there was a syntax error; exit with the error code
      // Re-raise the current exception to let the user know what exactly happened
      Raise;
      Terminate;
      Halt (2);
    end;

    // Impose design constraints if necessary
    if ShouldImposeConstraints then
      AConfList.CheckConstraints (AConstr);

    // Delete disabled configurations if necessary
    if ShouldDelete then
      AConfList.DeleteDisabled;

    // By now, the list could become empty!
    // If there is nothing to print, exit immediately, indicating this condition through the exit code
    if AConfList.Count = 0 then
    begin
      Terminate;
      Halt (1);
    end;

    // If we reached here, there is still something in the list
    // Sort if necessary
    if ShouldSort then
    begin
      // Sorting can be in normal or in reverse order
      if ReverseSort then
        AConfList.ReverseSortByName (SortByStr)
      else
        AConfList.SortByName (SortByStr);
    end;

    // Prepare the output
    AConfList.Normalize;
    AConfList.GetConfigListing (TempStrings);

    // If we are only to print the first config
    if ShouldPrintFirstConfig then
      Writeln (AConfList[0].Text)   // In "key=value" format
    else
    begin
      // No, it is not just the first config.
      // NB: The first line in "TempStrings" is the HEADER!
      // Therefore, we always print at least the first line of "TempStrings" (its index is "0")

      // If the user didn't limit the number of configs to output, print everything.
      if not ShouldLimitNumLines then
        LinesOfOutput := TempStrings.Count-1
      else
        // The user did specify the number, but could be too optimistic.
        // Set the actual value as the upper limit.
        if LinesOfOutput > TempStrings.Count-1 then
          LinesOfOutput := TempStrings.Count-1;

      // Print the output to the console
      for I := 0 to LinesOfOutput do
        Writeln (TempStrings [I]);
    end;

  finally
    // Memory of these two is released unconditionally
    AConfList.Free;
    TempStrings.Free;
    // And this one only if the corresponding object had been created
    if ShouldImposeConstraints then
      AConstr.Free;
  end;


  // stop program loop
  Terminate;
end;

constructor TCliApplication.Create(TheOwner: TComponent);
begin
  inherited Create (TheOwner);
  StopOnException := True;
end;

destructor TCliApplication.Destroy;
begin
  inherited Destroy;
end;

procedure TCliApplication.WriteHelp;
begin
  Writeln ('');
  Writeln ('Loads the list of configurations, optionally imposes constraints and sorts, and prints the output in CSV format');
  Writeln ('');
  Writeln('Usage: ' + ExtractFileName (ExeName) + ' -i <filename.xml|filename.csv> [-t <xml|csv>] [-c "<constraints>"] [-d] [-s <metric> [-r]] [-n N | -1]');
  Writeln ('');
  Writeln (' -i   Input file name (in XML of CSV format)');
  Writeln (' -t   (Optional) File type, if cannot be determined from the file extension');
  Writeln (' -c   (Optional) A quoted, comma-separated list of constraints to be imposed on the list');
  Writeln (' -d   (Optional) Delete disabled configurations after imposing constraints');
  Writeln (' -s   (Optional) Sort the list by the specified metric name');
  Writeln (' -r   (Optional) Used with ''-s'' to indicate reverse sort order');
  Writeln (' -n   (Optional) Limit output to N configurations only');
  Writeln (' -1   (Optional) Only print the first configuration, in key-value pairs format');
  Terminate;
end;

var
  Application: TCliApplication;

{$R *.res}

begin
  Application:=TCliApplication.Create(nil);
  Application.Run;
  Application.Free;
end.


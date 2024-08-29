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
unit cgimain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, HTTPDefs, fpHTTP, fpWeb,
  fattree, torus, common, checkruns;                   // Own our units

const
  { String constants used in CGI requests }
  cTask = 'task';
  cShowDatabase = 'showdatabase';
  cDesign = 'design';
  cHelp = 'help';
  cOutputType = 'output_type';
  cOutputText = 'text';
  cOutputCSV = 'csv';
  cOutputNVP = 'nvp';
  // The default output type is currently NVP
  DefaultOutputType = cOutputNVP;

  { File names }
  cFileHelp = 'help.html';

  { Miscellaneous }
  cNVPStatusError = 'Status=Error';
  cNVPErrorMessage = 'Error-message';
  cDebugOutput = '#' + LineEnding + '# Debug output follows:' + LineEnding + '#' + LineEnding;

type

  { TFPWebModule1 }

  TFPWebModule1 = class(TFPWebModule)
    procedure DataModuleRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  FPWebModule1: TFPWebModule1; 
  RunsRemaining: Integer;
  RunsWarning: String;

implementation

{$R *.lfm}

{ TFPWebModule1 }

function FormatMainForm (const Args: Array of Const): String;
var
  MyTemplate: TStringList;
begin
  MyTemplate := TStringList.Create;

  // Load the HTML template of the main form
  MyTemplate.LoadFromFile (ExtractFilePath (ParamStr(0)) + cFileMainForm);

  // Format the template using the supplied arguments
  Result := Format (MyTemplate.Text, Args);

  // Free the template object
  MyTemplate.Free;
end;

procedure TFPWebModule1.DataModuleRequest (Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
type
  TTopology = (tFatTree, tTorus);         // Network topologies recognized by this web service
const
  DefaultTopology: TTopology = tFatTree;  // Default network topology
  DefaultVendor = cDefaultDir;            // Default name of network equipment vendor
var
  Constraints: TStringList;
  DebugOutput: TStringList;
  BestNetConfig: TNetworkConfig;
  Topology: TTopology;
  Vendor: String;
  Resp: String;
  I: Integer;
  OutputType, TopologyStr: String;
  Debug: Boolean;
begin

  // We don't generally want debug output in the Web application, only errors.
  // If you need it, set it to "True" and recompile, then use NVP (name-value pairs) output type to examine it.
  Debug := False;

  // Determine what topology the user requested, if any.
  TopologyStr := ARequest.QueryFields.Values [cNetworkTopology];
  if TopologyStr = '' then                    // Unspecified, use the default
    Topology := DefaultTopology
  else
  if TopologyStr = cFatTree then
    Topology := tFatTree
  else
  if TopologyStr = cTorus then
    Topology := tTorus
  else                                        // Unrecognized
    Raise Exception.Create ('Error: unrecognized network topology: "' + TopologyStr + '"');

  // Remove the topology specifier from the list of query fields (if it is there), as it has been analyzed.
  with ARequest.QueryFields do
    if IndexOfName (cNetworkTopology) <> -1 then
      Delete (IndexOfName (cNetworkTopology));

  // Determine what network equipment vendor the user requested, if any
  Vendor := ARequest.QueryFields.Values [cNetworkVendor];
  if Vendor = '' then                    // Unspecified, use the default
    Vendor := DefaultVendor;
  // Since it is user input, we need to strip unsafe characters
  // (directory separators) that could be used to maliciously
  // traverse directory structure:
  Vendor := ExtractFileName (Vendor);

  // Remove the vendor specifier from the list of query fields (if it is there), as it has been analyzed.
  with ARequest.QueryFields do
    if IndexOfName (cNetworkVendor) <> -1 then
      Delete (IndexOfName (cNetworkVendor));

  if ARequest.QueryFields.Count = 0 then      // No CGI request specified, print the main form
  begin
    AResponse.ContentType := 'text/html;charset=iso-8859-1';

    // Retrieve the number of allowed runs
    CheckRemainingRuns (RunsRemaining, RunsWarning);

    // Format and output the template
    Resp := FormatMainForm ([RunsRemaining, RunsWarning]);
    AResponse.Contents.Add (Resp);
  end
  else
  if ARequest.QueryFields.Values[cTask] = cShowDatabase then // A request to show database
  begin
    AResponse.ContentType := 'text/html;charset=iso-8859-1';

    // Check topology type
    case Topology of
      tFatTree: Resp := FormatFatTreeDatabase (Vendor);
      tTorus:   Resp := FormatTorusDatabase (Vendor);
    else
      // Should never reach here
      Raise Exception.Create ('Error: unrecognized network topology');
    end;

    AResponse.Contents.Add (Resp);
  end
  else
  if ARequest.QueryFields.Values[cTask] = cDesign then       // Design the network
  begin

    // Determine whether we check for remaining runs (anti-abuse measure of your CGI application)
    // "Project Options" --> "Compiler options" --> "Other" --> "Custom options" --> "-dCHECK_REMAINING_RUNS"
    {$IFDEF CHECK_REMAINING_RUNS}
      // Make sure we have some runs remaining. If no runs left, report an error and exit
      CheckRemainingRuns (RunsRemaining, RunsWarning);
      if RunsRemaining <= 0 then
      begin
        AResponse.ContentType := 'text/html;charset=iso-8859-1';
        AResponse.Code := 403; // Try to emit HTTP/1.1 "Access Forbidden" status code (HTTP server may not support it)
        AResponse.Contents.Add ('<html><title>Error: Server temporarily throttled</title><body>The server has been temporarily throttled to avoid abuse : ' + RunsWarning);
        AResponse.Contents.Add ('<br><br>Or go back using this button:<br>' +
          '<form method="get" action=""><input value="Go back" type="submit"></form></body></html>');
        Handled := True;
        Exit;
      end;
    {$ENDIF}

    // Otherwise, proceed
    Constraints := TStringList.Create;
    DebugOutput := TStringList.Create;

    // Simply copy constraints from the browser's request (the URL).
    // If rubbish is supplied, it will be parsed and rejected later.
    with Constraints do
    begin
      AddStrings (ARequest.QueryFields);
      // Remove strings that are known to be part of URL:
      Delete (IndexOfName (cTask));
      // The output type may not be specified in the URL, so check its index before deleting:
      I := IndexOfName (cOutputType);
      if I <> -1 then
        Delete (I);
    end;

    // Check topology type
    case Topology of
      tFatTree: DesignFatTree (Vendor, Constraints, Debug, DebugOutput, BestNetConfig);
      tTorus:   DesignTorus   (Vendor, Constraints, Debug, DebugOutput, BestNetConfig);
    else
      // Should never reach here
      Raise Exception.Create ('Error: unrecognized network topology');
    end;

    {$IFDEF CHECK_REMAINING_RUNS}
      // So, one run attempt was used. Decrement the number of remaining runs and save it in the file
      RunsRemaining := RunsRemaining - 1;
      SaveRemainingRuns (RunsRemaining);
    {$ENDIF}

    // Report the results of the design procedure to the user
    OutputType := ARequest.QueryFields.Values[cOutputType];
    // If not specified, enforce the default
    if OutputType = '' then
      OutputType := DefaultOutputType;

    if Assigned (BestNetConfig) then   // If assigned, then a solution was found
    begin

      // Act depending on what output format was specified: comma-separated values (CSV), or name-value pairs (NVP),
      // or human-readable text format. (Default is NVP).
      if OutputType = cOutputCSV then
      begin
        AResponse.ContentType := 'text/plain;charset=iso-8859-1';
        Resp := BestNetConfig.CSVDescriptionWithHeader;
      end
      else
      if OutputType = cOutputText then  // Human-readable format was specified
      begin
        AResponse.ContentType := 'text/html;charset=iso-8859-1';
        Resp := BestNetConfig.HtmlDescription;
      end
      else  // Either output format was not assigned at all, or NVP format was specified (the default), or anything else
      begin
        AResponse.ContentType := 'text/plain;charset=iso-8859-1';
        Resp := BestNetConfig.NVPDescription;

        // Additionally, if debug output is necessary, append it
        if Debug then
          Resp += cDebugOutput + DebugOutput.Text;
      end;

      // No matter what output type was requested, now the "Resp" variable contains a response, so we can output it:
      AResponse.Contents.Add (Resp); // Add the formatted document to the response

      FreeAndNil (BestNetConfig);    // Free the object
    end
    else // There was an error, report it
    begin
      if OutputType = cOutputNVP then // In case of name-value pairs, report error in a specific way
      begin
        AResponse.ContentType := 'text/plain;charset=iso-8859-1';
        AResponse.Contents.Add (cNVPStatusError);                    // Indicate the status in the first line
        // Make sure that error output is not lost in the debug output: if there
        // was an error, it is usually the _last_ line of the debug output
        // XXX: Could be implemented more reliably
        AResponse.Contents.Add (cNVPErrorMessage + '=' + DebugOutput [DebugOutput.Count-1]);
        // Now we can safely append the lengthly debug output
        if Debug then
          AResponse.Contents.Add (cDebugOutput + DebugOutput.Text);
      end
      else // In other cases, use generic error message
      begin
        AResponse.ContentType := 'text/html;charset=iso-8859-1';
        AResponse.Contents.Add ('<html><title>Error detected</title><body><pre>');
        AResponse.Contents.Add (DebugOutput.Text);
        AResponse.Contents.Add ('<pre></body></html>');
      end;
    end;

    DebugOutput.Free;
    Constraints.Free;
  end
  else                                                       // If anything else, print short help
  begin
    AResponse.ContentType := 'text/html;charset=iso-8859-1';
    AResponse.Contents.LoadFromFile (ExtractFilePath (ParamStr(0)) + cFileHelp);
  end;

  Handled := True;     // Handling complete

end;

initialization
  RegisterHTTPModule('TFPWebModule1', TFPWebModule1); 
end.


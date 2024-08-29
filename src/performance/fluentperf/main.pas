{
Copyright (C) 2012-2014 Konstantin S. Solnushkin

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
unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, HTTPDefs, fpHTTP, fpWeb,
  Perf_Common, CheckRuns, AnsysFluent_v13;           // Our own units

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

procedure TFPWebModule1.DataModuleRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
var
  MyTemplate: TStringList;
  Resp, OutputType: String;
  FluentPerf: TAnsysFluentPerformance;
begin

  // Template for HTML output
  MyTemplate := TStringList.Create;

  if ARequest.QueryFields.Count = 0 then      // No request specified, print the main form
  begin
    AResponse.ContentType := 'text/html;charset=iso-8859-1';

    // Load the template of the main form
    MyTemplate.LoadFromFile (ExtractFilePath(ParamStr(0)) + 'main-form.html');

    // Check if we still can run the script
    CheckRemainingRuns (RunsRemaining, RunsWarning);

    // Format the template and output it
    Resp := Format (MyTemplate.Text, [RunsRemaining, RunsWarning]);
    AResponse.Contents.Add (Resp);
  end
  else                                        // A request _was_ specified, so parse it and reply
  if ARequest.QueryFields.Values[cTask] = cDesign then
  begin

    // Determine whether we check for remaining runs (anti-abuse measure of your CGI application)
    // "Project Options" --> "Compiler options" --> "Other" --> "Custom options" --> "-dCHECK_REMAINING_RUNS"
    {$IFDEF CHECK_REMAINING_RUNS}
    begin
      // Make sure we have some runs remaining. If no runs left, report an error and exit
      CheckRemainingRuns (RunsRemaining, RunsWarning);
      if RunsRemaining <= 0 then
      begin
        AResponse.ContentType := 'text/html;charset=iso-8859-1';
        AResponse.Code := 403; // Try to emit HTTP/1.1 "Access Forbidden" status code (HTTP server may not support it)
        AResponse.Contents.Add ('<html><title>Error: No runs left</title><body>No runs left : ' + RunsWarning);
        AResponse.Contents.Add ('<br><br>Or go back using this button:<br>' +
          '<form method="get" action=""><input value="Go back" type="submit"></form></body></html>');
        Handled := True;
        Exit;
      end;
      // Decrement the number of remaining runs and save it in the file
      RunsRemaining := RunsRemaining - 1;
      SaveRemainingRuns (RunsRemaining);
    end;
    {$ENDIF}

    FluentPerf := TAnsysFluentPerformance.Create;
    FluentPerf.LoadParameters (ARequest.QueryFields);   // Load parameters from the URL string

    // If error was encountered after loading parameters, print a message and exit
    if FluentPerf.IsErrorSet then
    begin
      AResponse.ContentType := 'text/plain;charset=iso-8859-1';
      AResponse.Contents.Add (FluentPerf.NVPErrorDescription);
      Handled := True;
      Exit;
    end;

    // Perform calculations
    if FluentPerf.IsDirectModel then
      FluentPerf.CalculatePerformance
    else
      FluentPerf.DoInverseModel;           // If inverse model was requested, perform iterative calculations

    // Again, if error was encountered after calculations, print a message and exit
    if FluentPerf.IsErrorSet then
    begin
      AResponse.ContentType := 'text/plain;charset=iso-8859-1';
      AResponse.Contents.Add (FluentPerf.NVPErrorDescription);
      Handled := True;
      Exit;
    end;

    // Act depending on what output format was specified: comma-separated values (CSV), or name-value pairs (NVP),
    // or human-readable text format. (Default is NVP).
    OutputType := ARequest.QueryFields.Values[cOutputType];
    if OutputType = cOutputCSV then
    begin
      AResponse.ContentType := 'text/plain;charset=iso-8859-1';
      Resp := FluentPerf.CSVDescriptionWithHeader;
    end
    else
    if OutputType = cOutputText then  // Human-readable format was specified
    begin
      AResponse.ContentType := 'text/html;charset=iso-8859-1';

      with TStringList.Create do
      try
        LoadFromFile (ExtractFilePath(ParamStr(0)) + 'results.html'); // Load the HTML template

        try // Try conversion
          with FluentPerf do
            Resp := Format (Text, [RunsRemaining, RunsWarning, Software, Benchmark,
              PerfModelId, NetworkTech, MaxRating, MaxRatingAtCores,
              Cores, Performance, BoolToStr (ThroughputMode, True), TimeToSolution]);
        except
          On Excpt: Exception do
          begin
            AResponse.Contents.Add ('Exception caught : ' + Excpt.Message);
            Handled := True;
            Exit;
          end;
        end;

      finally
        Free;
      end;
    end
    else  // Either output format was not assigned at all, or NVP format was specified (the default), or anything else
    begin
      AResponse.ContentType := 'text/plain;charset=iso-8859-1';
      Resp := FluentPerf.NVPDescription;
    end;

    // No matter what output type was requested, now the "Resp" variable contains a response, so we can output it:
    AResponse.Contents.Add (Resp); // Add the formatted document to the response

    // Free the now unnecessary object
    FluentPerf.Free;
  end
  else                                                  // If anything else, print short help
  begin
    AResponse.ContentType := 'text/html;charset=iso-8859-1';
    AResponse.Contents.LoadFromFile (ExtractFilePath(ParamStr(0)) + 'help.html');
  end; { End of request parsing }

  Handled := True;
  MyTemplate.Free;
end;

initialization
  RegisterHTTPModule('TFPWebModule1', TFPWebModule1); 
end.


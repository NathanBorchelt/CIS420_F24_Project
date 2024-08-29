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
unit checkruns;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  cRunsRemainingFileName = 'runs-remaining.txt';

var
  RunsFileName: String;

procedure CheckRemainingRuns (out RunsRemaining: Integer; out RunsWarning: String);

procedure SaveRemainingRuns (RunsToSave: Integer);

implementation

procedure CheckRemainingRuns (out RunsRemaining: Integer; out RunsWarning: String);
{ Opens a file from disk and reads from there how many runs we have remaining.
  It is supposed that this file must be recreated by a "cron" script every 10 minutes or every hour.
  If the file doesn't exist, it is created, with '0' inside.
  This procedure outputs there variables:
    RunsRemaining (Integer, a number of runs), RunsWarning (String, holds a warning message, or empty otherwise)
}
begin
  {$IFDEF CHECK_REMAINING_RUNS}
    // Check if we have more runs remaining (an anti-abuse measure)
    RunsRemaining := 0;
    with TStringList.Create do
      try
        if FileExists (RunsFileName) then
        begin
          LoadFromFile (RunsFileName);            // Load a file from disk
          if Count <> 0 then                      // If there is something in the file
          begin
            try                                   // Try to convert the first line
              RunsRemaining := StrToInt (Strings[0]);
            except
              on EConvertError do                 // If there was an error when converting, act as if '0' was specified
                RunsRemaining := 0;
            end; { try }
          end; { Count <> 0 }
        end
        else                                      // No such file exists on disk
        begin
          // Create this file and put '0' inside
          Add ('0');
          SaveToFile (RunsFileName);
        end;

        // Now, whether the file exists or not, contained a valid number or garbage, the variable
        // "RunsRemaining" is set and can be analyzed.
        if RunsRemaining = 0 then               // If '0' was in the file, ask the user to wait
          RunsWarning := 'It renews automatically each hour; check back in a while and simply reload the page!'
        else
          RunsWarning := '';                    // No warning

      finally
        Free;                                   // Free the TStringList, it is not needed anymore
      end;
  {$ELSE}
    RunsRemaining := MaxInt;
    RunsWarning := '';
  {$ENDIF}
end; { CheckRemainingRuns }

procedure SaveRemainingRuns (RunsToSave: Integer);
begin
  {$IFDEF CHECK_REMAINING_RUNS}
    with TStringList.Create do
      try
        if RunsToSave < 0 then // Supplying a number less than zero indicated an error, but we must handle it somehow
          RunsToSave := 0;
        Add (IntToStr (RunsToSave));
        SaveToFile (RunsFileName);
      finally
        Free;
      end;
  {$ENDIF}
end; { SaveRemainingRuns }

initialization
  // The file where the number of remaining runs is stored. The same directory as the executable file itself.
  RunsFileName := ExtractFilePath(ParamStr(0)) + cRunsRemainingFileName;

end.


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
unit FloatStr;

// A nice trick, as per "http://wiki.freepascal.org/Multiplatform_Programming_Guide":
// Defines a new "GenericStrToFloat" function that can use either '.' or ',' as a DecimalSeparator.

{$mode objfpc}{$H+}

interface

uses
  Classes, Dialogs, SysUtils,
  Common;           // Own our units

var
  FPointSeparator, FCommaSeparator: TFormatSettings;

function GenericStrToFloat (AStr: String): Single;

function IsNumber (AStr: String): Boolean;

function GenericFloatToStr (AFloat: Single): String;

function GenericFloatToStr (AFloat: Single; DecPlaces: Integer): String;

implementation

// This function works like StrToFloat, but simply tries two possible decimal separator
// This will avoid an exception when the string format doesn't match the locale
function GenericStrToFloat (AStr: String): Single;
begin
  try
    if Pos('.', AStr) > 0 then
      Result := StrToFloat (AStr, FPointSeparator)
    else
      Result := StrToFloat (AStr, FCommaSeparator);
  except
    on EConvertError do
      MyShowMessageFmt ('Conversion of "%s" to a floating point value failed', [AStr]);
  end;
end;

function IsNumber (AStr: String): Boolean;
// Returns "True" is "AStr" contains a number. In contrast with "TryStrToFloat", tries
// both possible variations of a floating point separator.
begin
  Result := True; // Hope for the best
  try
    if Pos('.', AStr) > 0 then
      StrToFloat (AStr, FPointSeparator)        // We ignore the result of the function
    else
      StrToFloat (AStr, FCommaSeparator);
  except
    // If conversion failed, it was not a number: return "False"
    on EConvertError do
      Result := False;
  end;
end;

function GenericFloatToStr (AFloat: Single): String;
begin
  // XXX: If the number of decimal places is not specified, default to "1"
  Result := GenericFloatToStr (AFloat, 1);
end;

function GenericFloatToStr (AFloat: Single; DecPlaces: Integer): String;
begin
  if Frac (AFloat) = 0 then                            // If fractional part is zero,
    Result := FloatToStrF (AFloat, ffFixed, 0, 0)      // then format as the integer number
  else
    Result := FloatToStrF (AFloat, ffFixed, 0, DecPlaces);
end;

initialization

// Format seetings to convert a string to a float
  FPointSeparator := DefaultFormatSettings;
  FPointSeparator.DecimalSeparator := '.';
  FPointSeparator.ThousandSeparator := '#';// disable the thousand separator
  FCommaSeparator := DefaultFormatSettings;
  FCommaSeparator.DecimalSeparator := ',';
  FCommaSeparator.ThousandSeparator := '#';// disable the thousand separator

end.


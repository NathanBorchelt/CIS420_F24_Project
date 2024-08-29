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
unit floatstr;

// A nice trick, as per "http://wiki.freepascal.org/Multiplatform_Programming_Guide"
//
// Defines a new "GenericStrToFloat" function that can use either '.' or ',' as a DecimalSeparator.

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

var
  FPointSeparator, FCommaSeparator: TFormatSettings;

function GenericStrToFloat(AStr: String): Single;

implementation

// This function works like StrToFloat, but simply tries two possible decimal separator
// This will avoid an exception when the string format doesn't match the locale
function GenericStrToFloat(AStr: String): Single;
begin
  try
    if Pos('.', AStr) > 0 then
      Result := StrToFloat(AStr, FPointSeparator)
    else
      Result := StrToFloat(AStr, FCommaSeparator);
  except
    Raise EConvertError.Create ('Conversion of "' + AStr + '" to a floating point value failed');
  end;
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


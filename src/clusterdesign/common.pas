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
unit Common;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs;

const
  cComma = ',';      // Stored here; used by other units
  cSpace = ' ';

procedure MyShowMessageFmt (const aMsg: String; Params: array of const);

implementation

procedure MyShowMessageFmt (const aMsg: String; Params: array of const);
begin
  {$IFDEF AppTypeConsole}
  Writeln (Format(aMsg, Params));
  {$ELSE}
  ShowMessageFmt (aMsg, Params);
  {$ENDIF}
end;

end.


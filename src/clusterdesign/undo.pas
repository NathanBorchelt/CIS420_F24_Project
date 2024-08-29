{
Copyright (C) 2012 Konstantin S. Solnushkin

This file is part of "clusterdesign", the GUI tool to design high-performance
computer clusters.

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
unit Undo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs,
  Config;                    // Our own units

const
  cUndo = 'Undo';

type

  { TUndoStack }

  TUndoStack = class (TObjectStack)
    public
      destructor Destroy; override;
      procedure Clear;
  end;

implementation

{ TUndoStack }

destructor TUndoStack.Destroy;
begin
  // Free items' memory first
  Clear;
  // Then destroy as usual
  inherited Destroy;
end;

procedure TUndoStack.Clear;
begin
  // Free items' memory
  while Count > 0 do
    (Pop as TConfigList).Free;
end;

end.


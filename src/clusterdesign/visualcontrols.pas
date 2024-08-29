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
unit VisualControls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs, Controls, StdCtrls, Forms, Dialogs, httpsend;

const
  // Border spacing around dynamically created visual components
  vcHSpacing = 15;
  vcVSpacing = vcHSpacing;

type

  { A weird but necessary thing }

  TCustomCheckBoxClass = Class of TCustomCheckBox;

  { TModelItem }

  TModelItem = class
  private
    function GetChecked: Boolean;
    procedure SetChecked(const Value: Boolean);
  public
    C: TCustomCheckbox;
    L: TLabel;
    E: TEdit;
    constructor Create (AOwner: TWinControl; CheckBoxType: TCustomCheckBoxClass);
    property Checked: Boolean read GetChecked write SetChecked;
  end;

  { TModelList }

  TModelList = class (TScrollBox)
  // This class is a happy marriage of "TScrollBox" and "TFPObjectList"
  private
    ItemList: TFPObjectList;
    fLocatorURL: String;
    fCheckBoxType: TCustomCheckBoxClass;
  protected
    function GetItem(Index: Integer): TModelItem;
    function GetCount: Integer;
  public
    // The user could simply call "Create", or specify a check box type to be used for list entries (TRadioButton or TCheckBox)
    constructor Create (AOwner: TWinControl); reintroduce;
    constructor Create (AOwner: TWinControl; CheckBoxType: TCustomCheckBoxClass); reintroduce;
    destructor Destroy; override;
    procedure PutBelow (AControl: TWinControl);
    procedure AnchorRightSide (AControl: TWinControl);
    function AddItem: Integer;
    function LoadFromURL (aLocatorURL: String): Boolean;
    function CountEnabled: Integer;
    function GetFirstEnabledItem: TModelItem;
    property Items[Index: Integer]: TModelItem read GetItem; default;    // As in "TFPObjectList"
    property Count: Integer read GetCount;
  end;

implementation

{ TModelItem }

function TModelItem.GetChecked: Boolean;
begin
  Result := (C.State = cbChecked);
end;

procedure TModelItem.SetChecked(const Value: Boolean);
begin
  if Value then
    C.State := cbChecked
  else
    C.State := cbUnchecked;
end;

constructor TModelItem.Create (AOwner: TWinControl; CheckBoxType: TCustomCheckBoxClass);
begin
  // Only these two types of "checkboxes" make sense
  Assert ((CheckBoxType = TRadioButton) or (CheckBoxType = TCheckBox));

  inherited Create;

  // Create objects
  if CheckBoxType = TRadioButton then
    C := TRadioButton.Create (AOwner)
  else
    if CheckBoxType = TCheckBox then
      C := TCheckBox.Create (AOwner);
  C.Parent := AOwner; // Setting the "Parent" property allows the control to be visible -- an obscure feature that hinders visibility (pun intended)
  L := TLabel.Create (AOwner);
  L.Parent := AOwner;
  E := TEdit.Create (AOwner);
  E.Parent := AOwner;
end;

{ TModelList }

function TModelList.GetItem (Index: Integer): TModelItem;
begin
  Result := ItemList[Index] as TModelItem;
end;

function TModelList.GetCount: Integer;
begin
  Result := ItemList.Count;
end;

constructor TModelList.Create (AOwner: TWinControl);
begin
  // If we were created without specifying a "checkbox" type, use radio buttons as a default,
  // so that only one model can be selected.
  Create (AOwner, TRadioButton);
end;

constructor TModelList.Create (AOwner: TWinControl; CheckBoxType: TCustomCheckBoxClass);
begin
  // Only these two types of "checkboxes" make sense
  Assert ((CheckBoxType = TRadioButton) or (CheckBoxType = TCheckBox));

  // Save for future reference
  fCheckBoxType := CheckBoxType;

  inherited Create (AOwner);

  // Set the top and bottom spacings
  BorderSpacing.Top := vcVSpacing;
  BorderSpacing.Bottom := vcVSpacing;

  // Maximize the borders
  Anchors := [akLeft, akRight, akTop, akBottom];

  // Anchor the bottom side
  AnchorSideBottom.Control := AOwner;
  AnchorSideBottom.Side := asrBottom;

  // Allows to make the object visible
  Parent := AOwner;

  // Create lists to store information about items
  ItemList := TFPObjectList.Create;
end;

destructor TModelList.Destroy;
begin
  ItemList.Free;
  inherited Destroy;
end;

procedure TModelList.PutBelow(AControl: TWinControl);
begin
  // Put the visual control below the specified one, with their left sides aligned
  AnchorSide[akLeft].Control := AControl;
  AnchorSide[akLeft].Side := asrLeft;
  AnchorSide[akTop].Control := AControl;
  AnchorSide[akTop].Side := asrBottom;
end;

procedure TModelList.AnchorRightSide(AControl: TWinControl);
// Anchor the right side to the specified control
begin
  AnchorSideRight.Control := AControl;
  AnchorSideRight.Side := asrRight;
end;

function TModelList.AddItem: Integer;
var
  AItem: TModelItem;
begin
  // Create an item and add it to the list. Set ourselves as the "owner" (required for drawing on the screen).
  AItem := TModelItem.Create (Self, fCheckBoxType);
  // Return its number
  Result := ItemList.Add (AItem);

  // Set properties of elements: checkbox, label, edit field
  with AItem do
  begin
    // Checkbox (or a radio button)
    // Initially unchecked
    Checked := False;
    // Anchor to the left
    C.BorderSpacing.Left := vcHSpacing;
    C.AnchorSide[akLeft].Side := asrLeft;
    C.AnchorSide[akLeft].Control := Self;
    C.Anchors := C.Anchors + [akLeft];
    // Label; immediately after the checkbox
    L.AnchorToNeighbour (akLeft, vcHSpacing div 3, C);
    // Edit field; anchored both to the left and to the right
    E.AnchorToNeighbour (akLeft, 170, C);                                         // XXX: Distance is currently fixed.
    E.BorderSpacing.Right := vcHSpacing;
    E.AnchorSide[akRight].Side := asrRight;
    E.AnchorSide[akRight].Control := Self;
    E.Anchors := E.Anchors + [akRight];
  end;

  with AItem do
  begin
    if Result = 0 then                                                            // It is the first item
    begin
      // Anchor the checkbox of the first item to the top of our visual control
      C.BorderSpacing.Top := vcVSpacing;
      C.AnchorSide[akTop].Side := asrTop;
      C.AnchorSide[akTop].Control := Self;
      C.Anchors := C.Anchors + [akTop];
    end
    else                                                                          // Not the first item
    begin
      // Anchor this item's checkbox to the checkbox of the previous item
      Assert (Assigned (Items [Result-1]));                                       // Make sure the previous item exists
      C.AnchorToNeighbour (akTop, vcVSpacing, (Items [Result-1]).C);
    end;

    // For any item -- first or not -- anchor other elements to the checkbox (center them vertically)
    L.AnchorSide[akBottom].Side := asrCenter;
    L.AnchorSide[akBottom].Control := C;
    L.Anchors := L.Anchors + [akBottom];
    E.AnchorSide[akBottom].Side := asrCenter;
    E.AnchorSide[akBottom].Control := C;
    E.Anchors := E.Anchors + [akBottom];
  end;
end;

function TModelList.LoadFromURL (aLocatorURL: String): Boolean;
var
  S: TStringList;
  I, J: Integer;
  AItem: TModelItem;
  AHTTP: THTTPSend;
begin
  fLocatorUrl := aLocatorURL;                          // Save for future use

  Result := False;                                     // Be prepared for failure

  S := TStringList.Create;
  AHTTP := THTTPSend.Create;
  try
    // Try to make a request
    Result := AHTTP.HTTPMethod ('GET', aLocatorURL);
    // If "True" is returned, the connection was successful
    if Result then
    begin
      // If any code other than "200" was returned, assume failure
      if AHTTP.ResultCode <> 200 then
      begin
        ShowMessage ('Web service returned: ' +
          IntToStr (AHTTP.ResultCode) + ' ' + AHTTP.ResultString);
        // Return failure status
        Result := False;
        Exit;
      end;

      // Now, it was not a blatant error. Try to load the web service's response
      S.LoadFromStream (AHTTP.Document);
      if S.Count = 0 then                                // Empty response
      begin
        ShowMessage ('Web service returned empty response');
        // Return failure status
        Result := False;
        Exit;
      end;

      // If we reached here, assume we got a correct response
      for I := 0 to S.Count-1 do                       // Parse strings one by one
      begin
        J := AddItem;
        AItem := Items[J];
        AItem.L.Caption := S.Names[I];                 // Fill caption and edit field
        AItem.L.Hint := AItem.L.Caption;               // Set hint (useful if the line is too long)
        AItem.E.Text := S.ValueFromIndex[I];
      end;
    end;

  finally
    S.Free;
    AHTTP.Free;
  end;
end;

function TModelList.CountEnabled: Integer;
var
  I: Integer;
begin
  Result := 0;

  // If the checkbox is checked, increment "Result"
  for I := 0 to ItemList.Count-1 do
    if Items[I].Checked then
      Result += 1;
end;

function TModelList.GetFirstEnabledItem: TModelItem;
// For some purposes, getting just the first item that has its checkbox enabled can be enough
var
  I: Integer;
begin
  // Step through the items. When the first enabled item is found, return it and exit.
  for I := 0 to ItemList.Count-1 do
    if Items[I].Checked then
    begin
      Result := Items[I];
      Exit;
    end;

  // If we reached here, no item was enabled. Return "Nil"
  Result := Nil;
end;

end.


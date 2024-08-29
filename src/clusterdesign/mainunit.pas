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
unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, LCLIntf, LCLType,
  ExtCtrls, ComCtrls, Spin, Math,
  VisualControls, Common, Config, ClusterConfig, Undo, FloatStr; { Our own units }

type

  { TMainForm }

  TMainForm = class(TForm)
    // Page control, for multiple tabs
    APageControl: TPageControl;
    // Toolbar
    ToolBar: TToolBar;
    ToolBarImageList: TImageList;
    LoadDBToolButton: TToolButton;
    LoadCSVToolButton: TToolButton;
    SaveCSVToolButton: TToolButton;
    UndoToolButton: TToolButton;
    Div1ToolButton: TToolButton;
    Div2ToolButton: TToolButton;
    // Status bar
    AStatusBar: TStatusBar;
    // "Nodes" tab
    NodeTabSheet: TTabSheet;
    NodeLevelDesignConstraintsLabel: TLabel;
    NodeLevelDesignConstraintsMemo: TMemo;
    NodeImposeDesignConstraintsButton: TButton;
    OpenXMLFileDialog: TOpenDialog;
    OpenCSVFileDialog: TOpenDialog;
    SaveCSVFileDialog: TSaveDialog;
    NodeOutputMemo: TMemo;
    NodeCleanupDisabledConfigsButton: TButton;
    NodeDeleteDisabledConfigsCheckBox: TCheckBox;
    NodeHeuristicTypeComboBox: TComboBox;
    NodeHeuristicsButton: TButton;
    // "Network" tab
    NetworkTabSheet: TTabSheet;
    NetworkLocatorButton: TButton;
    NetworkLocatorEdit: TLabeledEdit;
    NetworkGroupBox: TGroupBox;
    NetworkSendEdit: TEdit;
    NetworkReceiveEdit: TEdit;
    NetworkAlsoSendLabel: TLabel;
    NetworkAlsoReceiveLabel: TLabel;
    NetworkSendLabel: TLabel;
    NetworkReceiveLabel: TLabel;
    NetworkAlsoSendMemo: TMemo;
    NetworkAlsoReceiveMemo: TMemo;
    NetworkModelList: TModelList;
    // "Performance" tab
    PerformanceTabSheet: TTabSheet;
    PerformanceLocatorButton: TButton;
    PerformanceLocatorEdit: TLabeledEdit;
    PerformanceDirectGroupBox: TGroupBox;
    PerformanceDirectSendLabel: TLabel;
    PerformanceDirectSendEdit: TEdit;
    PerformanceDirectReceiveLabel: TLabel;
    PerformanceDirectReceiveEdit: TEdit;
    PerformanceInverseGroupBox: TGroupBox;
    PerformanceInverseSendLabel: TLabel;
    PerformanceInverseSendEdit: TEdit;
    PerformanceInverseReceiveLabel: TLabel;
    PerformanceInverseReceiveEdit: TEdit;
    PerformanceAlsoSendLabel: TLabel;
    PerformanceAlsoReceiveLabel: TLabel;
    PerformanceAlsoSendMemo: TMemo;
    PerformanceAlsoReceiveMemo: TMemo;
    PerformanceModelList: TModelList;
    // "UPS" tab
    UpsTabSheet: TTabSheet;
    UpsAlsoReceiveLabel: TLabel;
    UpsAlsoReceiveMemo: TMemo;
    UpsAlsoSendLabel: TLabel;
    UpsAlsoSendMemo: TMemo;
    UpsSendEdit: TEdit;
    UpsReceiveEdit: TEdit;
    UpsSendLabel: TLabel;
    UpsReceiveLabel: TLabel;
    UpsGroupBox: TGroupBox;
    UpsLocatorEdit: TLabeledEdit;
    UpsLocatorButton: TButton;
    UpsModelList: TModelList;
    // "Settings" tab
    SettingsTabSheet: TTabSheet;
    RackHeightLabel: TLabel;
    RackHeightSpinEdit: TSpinEdit;
    SystemLifetimeLabel: TLabel;
    SystemLifetimeSpinEdit: TSpinEdit;
    KwhPriceLabel: TLabel;
    KwhPriceEdit: TEdit;
    RackOpExPerYearLabel: TLabel;
    RackOpExPerYearEdit: TEdit;
    // "Design" tab
    DesignTabSheet: TTabSheet;
    DesignButton: TButton;
    DesignCancelButton: TButton;
    DesignConstraintsGroupBox: TGroupBox;
    DesignConstraintsMemo: TMemo;
    DesignObjFuncLabel: TLabel;
    DesignObjFuncComboBox: TComboBox;
    DesignProgressBar: TProgressBar;
    DesignCurrentConfigMemo: TMemo;
    DesignEssentialCharsMemo: TMemo;
    DesignCurrentConfigSpinEdit: TSpinEdit;
    DesignCurrentConfigDisabledLabel: TLabel;
    DesignCurrentConfigUntestedLabel: TLabel;
    DesignConfigRankLabel: TLabel;
    // "About" tab
    AboutTabSheet: TTabSheet;
    AboutLogoImage: TImage;
    AboutProgramTitleLabel: TLabel;
    AboutAuthorLabel: TLabel;
    AboutMoreInfoLabel: TLabel;
    AboutURLLabel: TLabel;
    // Methods
    procedure AboutURLLabelClick(Sender: TObject);
    procedure DesignButtonClick(Sender: TObject);
    procedure DesignCancelButtonClick(Sender: TObject);
    procedure DesignCurrentConfigSpinEditChange(Sender: TObject);
    procedure DesignObjFuncComboBoxEditingDone(Sender: TObject);
    procedure FormClose(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure KwhPriceEditEditingDone(Sender: TObject);
    procedure LoadCSVToolButtonClick(Sender: TObject);
    procedure LoadDBToolButtonClick(Sender: TObject);
    procedure NodeCleanupDisabledConfigsButtonClick(Sender: TObject);
    procedure NodeDeleteDisabledConfigsCheckBoxClick(Sender: TObject);
    procedure NodeHeuristicsButtonClick(Sender: TObject);
    procedure NodeHeuristicTypeComboBoxEditingDone(Sender: TObject);
    procedure NodeImposeDesignConstraintsButtonClick(Sender: TObject);
    procedure NetworkLocatorButtonClick(Sender: TObject);
    procedure PerformanceLocatorButtonClick(Sender: TObject);
    procedure RackOpExPerYearEditEditingDone(Sender: TObject);
    procedure SaveCSVToolButtonClick(Sender: TObject);
    procedure UndoToolButtonClick(Sender: TObject);
    procedure UpsLocatorButtonClick(Sender: TObject);
  private
    { private declarations }
    // Variables:
    //   Configurations are stored here
    NodeConfList: TClusterConfigList;
    // This one is used for undo operations
    UndoStack: TUndoStack;
    //   Node-level and global constraints
    NodeLevelConstraints: TDesignConstraints;
    // A flag indicating whether the design procedure should be cancelled
    DesignCancel: Boolean;
    procedure DesignCurrentConfigUpdateLabels (const AConfig: TClusterConfig);
    procedure DesignCurrentConfigRemoveLabels;
    procedure PrintConfigList;
    procedure PrintCurrentConfig (I: Longint);
    procedure UpdateDesignTabSpinEdit;
    function CheckNodesTab: Boolean;
    function CheckPerformanceTab (var ADirectPerfModel, AInversePerfModel: TGenericModel;
      const GlobalConstraints: TDesignConstraints): Boolean;
    function CheckNetworkTab (var ANetworkModel: TGenericModel;
      const GlobalConstraints: TDesignConstraints; out DesignNetwork: Boolean): Boolean;
    function CheckUpsTab (var AUpsModel: TGenericModel;
      const GlobalConstraints: TDesignConstraints; out DesignUps: Boolean): Boolean;
    function CheckDesignTab (var GlobalConstraints: TDesignConstraints): Boolean;
    function CheckSettingsTab (var GlobalSettings: TStringList): Boolean;
    procedure UpdateStatusBar;
    procedure UpdateUndoButton;
    procedure UpdatesAfterLoad;
    procedure RedrawConfigList;
    function DeleteDisabledConfigurations: Boolean;
    procedure AutomaticallyDeleteDisabledConfigurations;
  public
    { public declarations }
  end; 

const
  cProgramURL = 'http://ClusterDesign.org/tools';
  cProgramAuthors = '(C) Konstantin S. Solnushkin, 2012-2013';
  cXMLFileFilter = 'XML files|*.xml|All Files|*.*';
  cCSVFileFilter = 'CSV Files|*.csv|TXT Files|*.txt|All Files|*.*';
  cCSVDefaultExt = 'csv';
  cStatusLoadingDatabase = 'Loading database...';
  cStatusLoadingCSV = 'Loading CSV...';
  cStatusSorting = 'Sorting...';
  cStatusSaving = 'Saving...';
  cStatusApplyingHeuristic = 'Applying heuristic...';
  cStatusImposingNodeLevelConstraints = 'Imposing node-level constraints...';
  cCircle = '●';
  cNoConfigsMatchConstraints = '(No configurations match your constraints)';
  cErrorOpenFile = 'Couldn''t open file "%s"' + LineEnding + '(is it already in use by another program?)';

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  // Create a list to store cluster configurations
  NodeConfList := TClusterConfigList.Create;

  // Create object to store node-level constraints
  NodeLevelConstraints := TDesignConstraints.Create;

  // Lists of configurations are stored on the special undo stack
  UndoStack := TUndoStack.Create;

  // Set a default simple heuristic type
  with NodeHeuristicTypeComboBox do
  begin
    // Add items from the predefined array
    for I := Low (ArraySimpleHeuristicType) to High (ArraySimpleHeuristicType) do
      Items.Add (ArraySimpleHeuristicType [I]);
    // The default item to display (property "Text") is the first item
    Text := Items.Strings [0];
  end;

  // Set a default object function type
  with DesignObjFuncComboBox do
  begin
    // Add items from the predefined array
    for I := Low (ArrayObjFuncType) to High (ArrayObjFuncType) do
      Items.Add (ArrayObjFuncType [I]);
    // The default item to display:
    Text := Items.Strings [0];
  end;

  // Print things on the "About" tab.
  // The name of the program  is taken from the main form's title
  AboutProgramTitleLabel.Caption := Self.Caption;
  // Then come authors:
  AboutAuthorLabel.Caption := cProgramAuthors;
  // Same with URL:
  AboutURLLabel.Caption := cProgramURL;



  // XXX: does some actions automatically that would be otherwise completed by the user manually:
//  LoadDBToolButtonClick (Self);
//  LoadCSVToolButtonClick (Self);
(*  NodeConfList.LoadDatabase ('hp.xml');*)
//  NodeHeuristicsButtonClick(Self);   NodeHeuristicsButtonClick(Self);
(*  PerformanceLocatorButtonClick (Self);
  PerformanceModelList[0].C.Checked := True;
  NetworkLocatorButtonClick (Self);
  NetworkModelList[0].C.Checked := True;
  UpsLocatorButtonClick (Self);
  UpsModelList[0].C.Checked := True;*)



end;

procedure TMainForm.KwhPriceEditEditingDone(Sender: TObject);
begin
  // Only numbers are allowed, otherwise set the field to zero
  with KwhPriceEdit do
    if not IsNumber (Text) then
      Text := '0';
end;

procedure TMainForm.LoadCSVToolButtonClick(Sender: TObject);
begin
  OpenCSVFileDialog.Filter := cCSVFileFilter;
  OpenCSVFileDialog.Options := OpenCSVFileDialog.Options + [ofFileMustExist];
  OpenCSVFileDialog.InitialDir := '.';
  if not OpenCSVFileDialog.Execute then Exit;     // "Escape" was pressed, nothing to do, exit.

  try
    try
      AStatusBar.Panels[0].Text := cStatusLoadingCSV;         // Inform the user via a status bar message
      Application.ProcessMessages;                            // Otherwise the status might be displayed too late
      NodeConfList.LoadFromCSV (OpenCSVFileDialog.FileName);  // Load the only file that was selected by the user
      UpdatesAfterLoad;                                       // Update form's visual controls
    except
      On EFOpenError do
        ShowMessageFmt (cErrorOpenFile, [OpenCSVFileDialog.FileName]);
    end;
  finally
    AStatusBar.Panels[0].Text := '';
  end;
end;

procedure TMainForm.LoadDBToolButtonClick(Sender: TObject);
begin
  OpenXMLFileDialog.Filter := cXMLFileFilter;
  OpenXMLFileDialog.Options := OpenXMLFileDialog.Options + [ofFileMustExist];
  OpenXMLFileDialog.InitialDir := '.';
  if not OpenXMLFileDialog.Execute then Exit;     // "Escape" was pressed, nothing to do, exit.

  try
    try
      AStatusBar.Panels[0].Text := cStatusLoadingDatabase;        // Inform the user via a status bar message
      Application.ProcessMessages;                                // Otherwise the status might be displayed too late
      NodeConfList.LoadDatabase (OpenXMLFileDialog.FileName);     // Load the only file that was selected by the user
      UpdatesAfterLoad;                                           // Update form's visual controls
    except
      On EFOpenError do
        ShowMessageFmt (cErrorOpenFile, [OpenXMLFileDialog.FileName]);
    end;
  finally
    AStatusBar.Panels[0].Text := '';
  end;
end;

procedure TMainForm.NodeCleanupDisabledConfigsButtonClick(Sender: TObject);
// When this button is clicked, we unconditionally delete disabled configurations
var
  TempList: TClusterConfigList;
begin
  if Assigned (NodeConfList) then  // Because if nothing was loaded, there would be no config list in memory
  begin
    // Create a copy of the list of configuration in case undo is needed
    TempList := TClusterConfigList.Create;
    TempList.Assign (NodeConfList);
    // Continue with the original list: modify it
    if DeleteDisabledConfigurations then
    begin
      // Some elements have been deleted, so "TempList" and "NodeConfList" are now different
      // Copy to the undo stack
      UndoStack.Push (TempList);
      // Update the caption and the enabled state of the "Undo" button
      UpdateUndoButton;
      // Redraw the list of configurations and update the status bar
      RedrawConfigList;
    end
    else
      // No changes made, dispose of copy
      TempList.Free;
  end;
end;

procedure TMainForm.NodeDeleteDisabledConfigsCheckBoxClick (Sender: TObject);
// The user clicked on the check box
begin
  if NodeDeleteDisabledConfigsCheckBox.Checked then   // The user set the check box
  begin
    // Make the corresponding button greyed out, because clean-ups will now be done automatically
    NodeCleanupDisabledConfigsButton.Enabled := False;
    // Delete disabled configs, if any, and redraw the list if necessary
    DeleteDisabledConfigurations;
  end
  else                                                // The user unset the check box
    NodeCleanupDisabledConfigsButton.Enabled := True; // Re-enable the button for manual clean-up
end;

procedure TMainForm.NodeHeuristicsButtonClick (Sender: TObject);
var
  TempList: TClusterConfigList;
begin
  // Inform the user via a status bar message
  AStatusBar.Panels[0].Text := cStatusApplyingHeuristic;
  Application.ProcessMessages;  // Otherwise the status might be displayed too late

  // Create a copy of the list of configuration in case undo is needed
  TempList := TClusterConfigList.Create;
  TempList.Assign (NodeConfList);

  // Apply the heuristic, marking unsuitable configurations as "disabled"
  if NodeConfList.ApplyHeuristic (NodeHeuristicTypeComboBox.Text) then
  begin
    // There have been deletions, "TempList" and "NodeConfList" are now different
    // Copy to the undo stack
    UndoStack.Push (TempList);
    // Update the caption and the enabled state of the "Undo" button
    UpdateUndoButton;

    // If automatic clean-up was requested, do it
    if NodeDeleteDisabledConfigsCheckBox.Checked then
      NodeConfList.DeleteDisabled;

    // In any case, something has changed -- redraw the list and update the status bar
    RedrawConfigList;
  end
  else
    // No deletions were made, dispose of copy
    TempList.Free;

  // Indicate in the status bar that we are done
  AStatusBar.Panels[0].Text := '';
end;

procedure TMainForm.NodeHeuristicTypeComboBoxEditingDone (Sender: TObject);
begin
  // If the user tried to type his own expression into the ComboBox
  if NodeHeuristicTypeComboBox.ItemIndex = -1 then
    ShowMessage ('Only pre-programmed heuristics are currently supported, sorry.');
end;

procedure TMainForm.NodeImposeDesignConstraintsButtonClick(Sender: TObject);
var
  TempList: TClusterConfigList;
begin
  // Fill the node-level constraints with values taken from the form
  with NodeLevelConstraints do
  begin
    Clear;
    AddStrings (NodeLevelDesignConstraintsMemo.Lines);
  end;

  // Inform the user via a status bar message
  AStatusBar.Panels[0].Text := cStatusImposingNodeLevelConstraints;
  Application.ProcessMessages;  // Otherwise the status might be displayed too late

  // Create a copy of the list of configuration in case undo is needed
  TempList := TClusterConfigList.Create;
  TempList.Assign (NodeConfList);

  // Check for constraints, disabling the violating configs
  if NodeConfList.CheckConstraints (NodeLevelConstraints) then
  begin
    // There have been deletions, "TempList" and "NodeConfList" are now different
    // Copy to the undo stack
    UndoStack.Push (TempList);
    // Update the caption and the enabled state of the "Undo" button
    UpdateUndoButton;

    // If automatic clean-up was requested, do it
    if NodeDeleteDisabledConfigsCheckBox.Checked then
      NodeConfList.DeleteDisabled;

    // In any case, something has changed -- redraw the list and update the status bar
    RedrawConfigList;
  end
  else
    // No deletions were made, dispose of copy
    TempList.Free;

  // Indicate in the status bar that we are done
  AStatusBar.Panels[0].Text := '';
end;

procedure TMainForm.NetworkLocatorButtonClick(Sender: TObject);
begin
  // Disables the whole form's autosizing to avoid flicker
  DisableAutoSizing;

  try
    // If already exists, start anew
    if Assigned (NetworkModelList) then
      NetworkModelList.Free;

    // Create the object and position it on the form
    NetworkModelList := TModelList.Create (NetworkTabSheet);
    NetworkModelList.PutBelow (NetworkAlsoSendMemo);
    NetworkModelList.AnchorRightSide (NetworkLocatorButton);

    // Load network models
    if not NetworkModelList.LoadFromURL (NetworkLocatorEdit.Text) then
      FreeAndNil (NetworkModelList);
  finally
    EnableAutoSizing; // Recompute and redraw the form elements
  end;
end;

procedure TMainForm.PerformanceLocatorButtonClick(Sender: TObject);
// Almost an identical copy of "NetworkLocatorButtonClick", with the exception that we
// add a built-in performance model for peak performance.
var
  I: Integer;
  AModelItem: TModelItem;
begin
  // Disables the whole form's autosizing to avoid flicker
  DisableAutoSizing;

  try
    if Assigned (PerformanceModelList) then
      PerformanceModelList.Free;

    // Create the object and position it on the form
    PerformanceModelList := TModelList.Create (PerformanceTabSheet);
    PerformanceModelList.PutBelow (PerformanceAlsoSendMemo);
    PerformanceModelList.AnchorRightSide (PerformanceInverseGroupBox);

    // Add a built-in performance model
    I := PerformanceModelList.AddItem;
    AModelItem := PerformanceModelList.Items[I];
    AModelItem.L.Caption := cPeakPerformance;
    AModelItem.E.Text := cBuiltIn;
    AModelItem.E.Enabled := False;

    // Load performance models. It is OK if it fails, because there is already one entry for a built-in model,
    // so we do not check the return value
    PerformanceModelList.LoadFromURL (PerformanceLocatorEdit.Text);
  finally
    EnableAutoSizing; // Recompute and redraw the form elements
  end;
end;

procedure TMainForm.RackOpExPerYearEditEditingDone(Sender: TObject);
begin
  // Only numbers are allowed, otherwise set the field to zero
  with RackOpExPerYearEdit do
    if not IsNumber (Text) then
      Text := '0';
end;

procedure TMainForm.SaveCSVToolButtonClick(Sender: TObject);
var
  TempStrings: TStringList;
begin
  SaveCSVFileDialog.Filter := cCSVFileFilter;
  SaveCSVFileDialog.DefaultExt := cCSVDefaultExt;
  SaveCSVFileDialog.Options := SaveCSVFileDialog.Options + [ofOverwritePrompt];  // Warn user when overwriting
  SaveCSVFileDialog.InitialDir:='.';
  if not SaveCSVFileDialog.Execute then Exit;   // "Escape" was pressed, nothing to do, exit.

  // If we reached here, then the filename has been determined, and we can prepare the list to be saved

  // Inform the user via a status bar message
  AStatusBar.Panels[0].Text := cStatusSaving;
  Application.ProcessMessages;  // Otherwise the status might be displayed too late

  // Create a temporary object
  TempStrings := TStringList.Create;

  // Normalize first
  NodeConfList.Normalize;

  // Write the listing into the temporary object
  NodeConfList.GetConfigListing (TempStrings);

  // Disk operations may fail, hence the "try..finally" construct
  try
    TempStrings.SaveToFile (UTF8ToSys (SaveCSVFileDialog.FileName)); // Save the memo contents to the file
  finally
    // Free the temporary object
    TempStrings.Free;
    // Remove the status bar notification
    AStatusBar.Panels[0].Text := '';
  end;
end;

procedure TMainForm.UndoToolButtonClick(Sender: TObject);
var
  TempList: TClusterConfigList;
begin
  // Perform an undo step
  TempList := NodeConfList;
  NodeConfList := UndoStack.Pop as TClusterConfigList;
  TempList.Free;

  // Update the caption and the enabled state
  UpdateUndoButton;

  // Redraw the screen
  RedrawConfigList;
end;

procedure TMainForm.UpsLocatorButtonClick(Sender: TObject);
begin
  // Nothing different from "NetworkLocatorButtonClick"...

  // Disables the whole form's autosizing to avoid flicker
  DisableAutoSizing;

  try
    // If already exists, start anew
    if Assigned (UpsModelList) then
      UpsModelList.Free;

    // Create the object and position it on the form
    UpsModelList := TModelList.Create (UpsTabSheet);
    UpsModelList.PutBelow (UpsAlsoSendMemo);
    UpsModelList.AnchorRightSide (UpsLocatorButton);

    // Load UPS models
    if not UpsModelList.LoadFromURL (UpsLocatorEdit.Text) then
      FreeAndNil (UpsModelList);
  finally
    EnableAutoSizing; // Recompute and redraw the form elements
  end;
end;

procedure TMainForm.PrintConfigList;
// Prints the list of configurations in the "NodeOutputMemo" pane
var
  TempStrings: TStringList;
begin
  // Create a temporary object
  TempStrings := TStringList.Create;

  // Normalize first
  NodeConfList.Normalize;

  // Write the listing into the temporary object
  NodeConfList.GetConfigListing (TempStrings);

  // Output the contents of the object
  with NodeOutputMemo do
  begin
    Clear;
    Lines.AddStrings (TempStrings);
  end;
  TempStrings.Free;
end;

procedure TMainForm.DesignCurrentConfigUpdateLabels (const AConfig: TClusterConfig);
// XXX: Refactoring could be done. E.g., declare a new class, a descendant of TLabel, in "VisualControls" unit,
// and govern it with "Show"/"Hide" methods. However, works fine in this state, too.
begin
  // If this configuration is enabled ("AConfig.Enabled=true"), then remove the red label
  if AConfig.Enabled then
    with DesignCurrentConfigDisabledLabel do
    begin
      Caption := ' ';
      Font.Color := clDefault;
      Hint := '';
    end
  else
  // Otherwise, print it and assign a hint
    with DesignCurrentConfigDisabledLabel do
    begin
      Caption := cCircle;
      Font.Color := clRed;
      Hint := 'Disabled: ' + AConfig.ReasonDisabled;
    end;

  // If there are no untested constraints, remove the yellow label
  if AConfig.UntestedConstraints = '' then
    with DesignCurrentConfigUntestedLabel do
    begin
      Caption := ' ';
      Font.Color := clDefault;
      Hint := '';
    end
  else
    with DesignCurrentConfigUntestedLabel do
    begin
      Caption := cCircle;
      Font.Color := clYellow;
      Hint := 'Untested constraints: ' + AConfig.UntestedConstraints;
    end;
end;

procedure TMainForm.DesignCurrentConfigRemoveLabels;
begin
  with DesignCurrentConfigDisabledLabel do
  begin
    Caption := ' ';
    Font.Color := clDefault;
    Hint := '';
  end;
  with DesignCurrentConfigUntestedLabel do
  begin
    Caption := ' ';
    Font.Color := clDefault;
    Hint := '';
  end
end;

procedure TMainForm.PrintCurrentConfig (I: Longint);
var
  AConfig: TClusterConfig;
begin
  // A negative value specifies we need to print a message that no configs are found
  if I < 0 then
  begin
    // Print that there are no configs
    DesignCurrentConfigMemo.Text := cNoConfigsMatchConstraints;
    // And there are no details
    DesignEssentialCharsMemo.Enabled := False;
    // Remove the stale labels (if any) from the screen
    DesignCurrentConfigRemoveLabels;
    // Nothing more to do
    Exit;
  end;

  // If we reached here, we were passed a positive value. Check if it is valid.
  Assert (I < NodeConfList.Count);

  // Validity check was passed. Now we can refer to the configuration by its index
  AConfig := NodeConfList[I];

  // Prints the given configuration in the "DesignCurrentConfigMemo"
  DesignCurrentConfigMemo.Lines := AConfig;

  // Print essential characteristics of the configurations
  DesignEssentialCharsMemo.Enabled := True;
  DesignEssentialCharsMemo.Lines.Text := AConfig.EssentialCharacteristicsText;

  // Set status of the labels corresponding to the state of the configuration
  DesignCurrentConfigUpdateLabels (AConfig);
end;

procedure TMainForm.UpdateDesignTabSpinEdit;
begin
  // Set the limits for the "DesignCurrentConfigSpinEdit" visual control
  // NB: We count starting with "1", like real people do, not machines
  with DesignCurrentConfigSpinEdit do
  begin
    // It is obligatory to set "MinValue" to zero first, or we would be unable to set "MaxValue" to zero
    MinValue := 0;
    // MaxValue can be set unconditionally to the number of configurations that we have (even if zero)
    MaxValue := NodeConfList.Count;

    // In the rare but still possible case when no configurations were loaded, we need to check
    // if MaxValue is "0". In this case, the SpinEdit control should always stay at zero,
    // and we also disable it for a clear visual indication
    if MaxValue = 0 then
    begin
      MinValue := 0;
      Value := 0;
      Enabled := False;  // Will be re-enabled automatically upon the next load operation
    end
    else
    // In all other cases, it can be set to at least one (and re-enabled if necessary):
    begin
      MinValue := 1;
      Value := 1;
      Enabled := True;
    end;
  end;
end;

function TMainForm.CheckNodesTab: Boolean;
// Most importantly, check that at least one configuration is present and enabled
begin
  // Return "False" by default
  Result := False;

  // Was the database of nodes ever loaded?
  if not Assigned (NodeConfList) then
  begin
    APageControl.ActivePage := NodeTabSheet;
    ShowMessage ('Please load the database with some enabled node configurations first.' +
      LineEnding + ' (Use the "' + LoadDBToolButton.Caption + '" or "' +
      LoadCSVToolButton.Caption + '" buttons)');
    Exit;
  end;

  if NodeConfList.CountEnabled < 1 then
  begin
    APageControl.ActivePage := NodeTabSheet;
    ShowMessage ('No configurations are marked as enabled. (By some reason, all are marked as disabled).' +
      LineEnding + 'Perhaps you need to reload the database?');
    Exit;
  end;

  // If we reached here, all checks were passed
  Result := True;
end;

function TMainForm.CheckPerformanceTab (var ADirectPerfModel,
  AInversePerfModel: TGenericModel; const GlobalConstraints: TDesignConstraints): Boolean;
// Check the values on the "Performance" tab of the GUI
begin
  // Return "False" by default
  Result := False;

  // Was the list of models ever loaded?
  if not Assigned (PerformanceModelList) then
  begin
    APageControl.ActivePage := PerformanceTabSheet;
    ShowMessage ('Please load the list of performance models first.' +
      LineEnding + ' (Use the "' + PerformanceLocatorButton.Caption + '" button)' +
      LineEnding + 'Then select the performance model of your choice');
    Exit;
  end;

  // XXX: We only ask for one performance model to be selected. The reason is the following: it is not agreed upon
  // what to do if _two_ or more models were specified. Some possible resolutions:
  // (1) Use logical "AND" and design computer clusters that satisfy the requirements of _all_ performance models.
  // (Example: ANSYS performance > 100, Linpack performance > 200).
  //
  // (2) Use an "average" metric (which?) Example: harmonic average, weighted properly to represent the amount of tasks
  // to be run on the future computer cluster.

  if PerformanceModelList.CountEnabled <> 1 then
  begin
    APageControl.ActivePage := PerformanceTabSheet;
    ShowMessage ('Please enable one performance model. ' +
      LineEnding + '(Select only one checkbox in the list of models)');
    Exit;
  end;

  // Now, one performance model is enabled. Use it to build descriptions of direct and inverse performance models

  // Define the direct performance model to be used
  Assert (Assigned (ADirectPerfModel));
  with ADirectPerfModel do
  begin
    // Pass design constraints
    Constraints := GlobalConstraints;
    // XXX: Even if multiple performance models have their checkboxes ticked, we only refer to the first one enabled:
    URL := PerformanceModelList.GetFirstEnabledItem.E.Text;
    // Our "main" parameter that we send is the number of cores
    Send := PerformanceDirectSendEdit.Text;
    // And we receive performance
    Receive := PerformanceDirectReceiveEdit.Text;
    // Fields in this Memo are required to determine performance accurately
    AlsoSend := PerformanceAlsoSendMemo.Text;
    // And these fields are returned by the web service
    AlsoReceive := PerformanceAlsoReceiveMemo.Text;
  end;

  // Similarly, define the inverse performance model to be used
  Assert (Assigned (ADirectPerfModel));
  with AInversePerfModel do
  begin
    // Pass design constraints
    Constraints := GlobalConstraints;
    // XXX: Even if multiple performance models have their checkboxes ticked, we only refer to the first one enabled:
    URL := PerformanceModelList.GetFirstEnabledItem.E.Text;
    // Here, the "main" parameter is the projected performance
    Send := PerformanceInverseSendEdit.Text;
    // And we receive the number of cores
    Receive := PerformanceInverseReceiveEdit.Text;
    // Same as in the direct performance model
    AlsoSend := PerformanceAlsoSendMemo.Text;
    AlsoReceive := PerformanceAlsoReceiveMemo.Text;
  end;

  // If we reached here, all checks were passed
  Result := True;
end;

function TMainForm.CheckNetworkTab (var ANetworkModel: TGenericModel;
      const GlobalConstraints: TDesignConstraints; out DesignNetwork: Boolean): Boolean;
// Check the values on the "Network" tab of the GUI
var
  Reply: Integer;
begin
  // Return "False" by default
  Result := False;

  // If the user didn't load the list of models or didn't tick checkboxes, then
  // ask them if we should proceed without the network design stage.

  if (not Assigned (NetworkModelList)) or                                 // Was the list of models ever loaded?
    (Assigned (NetworkModelList) and (NetworkModelList.CountEnabled = 0)) // Or list loaded, but no checkboxes ticked?
  then
  begin
    Reply := Application.MessageBox ('No network models were selected. Design without networks?', '', MB_OKCANCEL);
    if Reply = IDOK then            // "OK": User agreed to proceed without designing networks
    begin
      DesignNetwork := False;       // Indicate that we don't want to design networks
      Result := True;               // Checks passed, exit with success
      Exit;
    end;

    // If we reached here, then the user pressed "CANCEL", indicating that they wish to correct the situation by loading an
    // appropriate network model. Also exit, but this time with the default value of failure
    Exit;
  end;

  // If we reached here, then the user has successfully loaded the list of network models, and selected one or more items.
  // Make sure only one item is selected.
  //
  (* XXX: This is an artificial limitation. Nothing prevents from designing with many network types. However, until
     performance models are created that can take into account network properties that influence performance (such as
     blocking in fat-trees, or torus networks that are cheaper than fat-trees but provide worse performance), the results
     of the design process will be unfairly skewed towards cheaper networks -- tori and blocking fat-trees. (Because the
     software will think all network types have the same performance). To prevent this situation with unaware users, we
     use only one network model at a time. Remember, the clue to this problem is to create performance models that would
     analyze "network-topology" and "network-blocking" properties of a configuration, and take them into account when
     calculating performance. When such performance models are available, some changes to this software will be required
     as well. More precisely, before starting the design procedure, we'll have to query every network model once (by their
     URLs), and ask them what values of "network-topology" and "network-blocking" they use. Sometimes that can be inferred
     from the URL of a network model without querying it; but one query will not hurt anyway.
  *)

  if NetworkModelList.CountEnabled <> 1 then
  begin
    APageControl.ActivePage := NetworkTabSheet;
    ShowMessage ('Please enable one network model. ' +
      LineEnding + '(Select only one checkbox in the list of models)');
    Exit;
  end;

  // Define the network model to be used
  Assert (Assigned (ANetworkModel));
  with ANetworkModel do
  begin
    // XXX: Even if multiple network models have their checkboxes ticked, we only refer to the first one enabled:
    URL := NetworkModelList.GetFirstEnabledItem.E.Text;
    // Our "main" parameter that we send is the number of compute nodes
    Send := NetworkSendEdit.Text;
    // And we receive network cost
    Receive := NetworkReceiveEdit.Text;
    // Also send fields in this Memo
    AlsoSend := NetworkAlsoSendMemo.Text;
    // And receive these
    AlsoReceive := NetworkAlsoReceiveMemo.Text;
    // Pass design constraints
    Constraints := GlobalConstraints;
  end;

  // If we reached here, all checks were passed
  DesignNetwork := True;
  Result := True;
end;

function TMainForm.CheckUpsTab(var AUpsModel: TGenericModel;
  const GlobalConstraints: TDesignConstraints; out DesignUps: Boolean): Boolean;
// Check the values on the "UPS" tab of the GUI
var
  Reply: Integer;
begin
  // Almost an identical copy of "CheckNetworkTab"

  // Return "False" by default
  Result := False;

  // If the user didn't load the list of models or didn't tick checkboxes, then
  // ask them if we should proceed without the UPS design stage.

  if (not Assigned (UpsModelList)) or                                 // Was the list of models ever loaded?
    (Assigned (UpsModelList) and (UpsModelList.CountEnabled = 0))     // Or list loaded, but no checkboxes ticked?
  then
  begin
    Reply := Application.MessageBox ('No UPS models were selected. Design without UPS?', '', MB_OKCANCEL);
    if Reply = IDOK then            // "OK": User agreed to proceed without designing UPS
    begin
      DesignUps := False;           // Indicate that we don't want to design networks
      Result := True;               // Checks passed, exit with success
      Exit;
    end;

    // If we reached here, then the user pressed "CANCEL", indicating that they wish to correct the situation by loading an
    // appropriate UPS model. Also exit, but this time with the default value of failure
    Exit;
  end;

  // If we reached here, then the user has successfully loaded the list of UPS models, and selected one or more items.
  // Make sure only one item is selected.
  //
  (* XXX: This is an artificial limitation. We can, in fact, design with many UPS models. This means, however, that
     for N UPS models we will need to create N-1 copies of each configuration, and perform design of the UPS subsystem
     N times. This also means that UPS sizing web service will be queried N times, and that the number of configurations
     in the tool's output will be N times bigger (say, not 200 but 600).
     In other words, this is possible, but to avoid confusion during the early adoption of this software, let us
     restrict ourselves to only one UPS model for now.
  *)

  if UpsModelList.CountEnabled <> 1 then
  begin
    APageControl.ActivePage := UpsTabSheet;
    ShowMessage ('Please enable one UPS model. ' +
      LineEnding + '(Select only one checkbox in the list of models)');
    Exit;
  end;

  // Define the UPS model to be used
  Assert (Assigned (AUpsModel));
  with AUpsModel do
  begin
    // XXX: Even if multiple UPS models have their checkboxes ticked, we only refer to the first one enabled:
    URL := UpsModelList.GetFirstEnabledItem.E.Text;
    // Our "main" parameter that we send is the required power rating of the UPS
    Send := UpsSendEdit.Text;
    // And we receive UPS cost
    Receive := UpsReceiveEdit.Text;
    // Also send fields in this Memo
    AlsoSend := UpsAlsoSendMemo.Text;
    // And receive these
    AlsoReceive := UpsAlsoReceiveMemo.Text;
    // Pass design constraints
    Constraints := GlobalConstraints;
  end;

  // If we reached here, all checks were passed
  DesignUps := True;
  Result := True;
end;

function TMainForm.CheckDesignTab (var GlobalConstraints: TDesignConstraints): Boolean;
// Check the values on the "Design" tab of the GUI
begin
  // Return "False" by default
  Result := False;

  // Was a minimum performance specified? This is crucial, because we calculate the number of compute nodes
  // (using the inverse performance model) based on this constraint.
  if DesignConstraintsMemo.Lines.Values [cMinPerformance] = '' then
  begin
    MyShowMessageFmt ('The minimum performance that you want to achieve ("%s") ' +
      LineEnding + 'must be specified as a global design constraint.', [cMinPerformance]);
    Exit;
  end;

  // Fill the global design constraints with values taken from the form
  with GlobalConstraints do
  begin
    Clear;
    AddStrings (DesignConstraintsMemo.Lines);
  end;

  // If we reached here, all checks were passed
  Result := True;
end;

function TMainForm.CheckSettingsTab(var GlobalSettings: TStringList): Boolean;
// Check the values on the "Settings" tab of the GUI
var
  KwhPrice: Single;
  RackOpExPerYear: Integer;
begin
  // Return "False" by default
  Result := False;

  // Clear the list of settings before adding anything
  GlobalSettings.Clear;

  // Fill rack height. Properties of this SpinEdit control ensure
  // that the returned value is always a positive number or zero,
  // so no additional checks are necessary.
  GlobalSettings.Values [cRackHeight] := RackHeightSpinEdit.Text;

  // Fill system lifetime. As with rack height above, no checks
  // are necessary.
  GlobalSettings.Values [cSystemLifetimeYears] := SystemLifetimeSpinEdit.Text;

  // Fill electricity price
  KwhPrice := GenericStrToFloat (KwhPriceEdit.Text);
  if KwhPrice < 0 then
  begin
    APageControl.ActivePage := SettingsTabSheet;
    MyShowMessageFmt ('Electricity price cannot be negative.', []);
    Exit;
  end;
  // Save
  GlobalSettings.Values [cKwhPrice] := KwhPriceEdit.Text;

  // Fill yearly rack operating expenses (OpEx)
  RackOpExPerYear := StrToInt (RackOpExPerYearEdit.Text);
  if RackOpExPerYear < 0 then
  begin
    APageControl.ActivePage := SettingsTabSheet;
    MyShowMessageFmt ('Yearly operating expense per rack cannot be negative.', []);
    Exit;
  end;
  // Save
  GlobalSettings.Values [cRackOpExPerYear] := RackOpExPerYearEdit.Text;

  // If we reached here, all checks were passed
  Result := True;
end;

procedure TMainForm.UpdateStatusBar;
begin
  // Fill the panel on the status bar with information on the number of loaded
  // and enabled configurations
  AStatusBar.Panels[1].Text := NodeConfList.StatusBarText;
end;

procedure TMainForm.UpdateUndoButton;
begin
  if UndoStack.Count = 0 then
  begin
    UndoToolButton.Enabled := False;        // If nothing to undo, disable the button
    UndoToolButton.Caption := cUndo;        // and revert the caption to default
  end
  else
  begin
    UndoToolButton.Enabled := True;         // Print the number of available undo steps
    UndoToolButton.Caption := cUndo + ' (' + IntToStr (UndoStack.Count) + ')';
  end;
end;

procedure TMainForm.UpdatesAfterLoad;
// After loading data (either from a database or a CSV file), some visual control must be updated,
// and other relevant stuff done.
begin
  // Don't let the configurations from the previous load persist in the undo stack
  UndoStack.Clear;
  UpdateUndoButton;

  // Enable visual controls
  NodeHeuristicsButton.Enabled := True;
  NodeHeuristicTypeComboBox.Enabled := True;
  NodeLevelDesignConstraintsMemo.Enabled := True;
  NodeLevelDesignConstraintsLabel.Enabled := True;
  NodeImposeDesignConstraintsButton.Enabled := True;
  NodeDeleteDisabledConfigsCheckBox.Enabled := True;
  SaveCSVToolButton.Enabled := True;

  // The enabled status of this button is the opposite of whether the check box is checked
  NodeCleanupDisabledConfigsButton.Enabled := not NodeDeleteDisabledConfigsCheckBox.Checked;

  // The "Design" button is enabled if and only if there are enabled configurations
  DesignButton.Enabled := (NodeConfList.CountEnabled <> 0);

  // Some newly loaded configurations could already be disabled and might need to be deleted.
  // If the corresponding check box is set, do the deletion.
  if NodeDeleteDisabledConfigsCheckBox.Checked then
    NodeConfList.DeleteDisabled;

  // In any case, print the list of configs
  RedrawConfigList;
end;

procedure TMainForm.RedrawConfigList;
begin
  // Update the "Nodes" tab
  PrintConfigList;

  // And the status bar
  UpdateStatusBar;

  // Update the "Design" tab as well:

  // If there are no configurations in the list
  if NodeConfList.Count = 0 then
    PrintCurrentConfig (-1)  // Negative value signifies to clear the visual controls
  else
    // If there are configurations in the list, print the 1st one on the "Design" tab
    PrintCurrentConfig (0);

  // Update SpinEdit Max and Min values on the "Design" tab
  UpdateDesignTabSpinEdit;
end;

function TMainForm.DeleteDisabledConfigurations: Boolean;
// Makes the necessary call to delete disabled configurations
begin
  if Assigned (NodeConfList) then         // Because if nothing was loaded, there would be no config list in memory
  begin
    Result := NodeConfList.DeleteDisabled;
    if Result then                        // Only if something was actually deleted...
      RedrawConfigList;                   // ..Redraw the list of configurations and update the status bar
  end;
end;

procedure TMainForm.AutomaticallyDeleteDisabledConfigurations;
// If the check box for deletion of disabled configurations is set, then
// delete (and, if necessary, redraw the list)
begin
  if NodeDeleteDisabledConfigsCheckBox.Checked then
    DeleteDisabledConfigurations;
end;

procedure TMainForm.FormClose(Sender: TObject);
begin
  // Destroy the objects
  NodeLevelConstraints.Free;
  NodeConfList.Free;
  UndoStack.Free;
end;

procedure TMainForm.DesignButtonClick(Sender: TObject);
var
  AConf: TClusterConfig;
  I, J, K: Integer;
  ADirectPerfModel, AInversePerfModel: TGenericModel; // Descriptions of performance models: direct and inverse
  ANetworkModel: TGenericModel;                       // A network model
  AUpsModel: TGenericModel;                           // An UPS model
  GlobalConstraints: TDesignConstraints;              // Global constraints
  GlobalSettings: TStringList;                        // Simply gathered from the "Settings" tab
  DesignNetwork: Boolean;                             // Indicates whether a user needs a network to be designed
  DesignUps: Boolean;                                 // Same, but with regard to UPS subsystem
  TempList: TClusterConfigList;

begin

  // XXX: You see, it is not very flexible. We expect the user to select only one performance model,
  // only one network model and only one UPS model. That's why we create single objects to hold those
  // models, not lists of models.

  ADirectPerfModel := TGenericModel.Create;
  AInversePerfModel := TGenericModel.Create;
  ANetworkModel := TGenericModel.Create;
  AUpsModel := TGenericModel.Create;
  GlobalConstraints := TDesignConstraints.Create;
  GlobalSettings := TStringList.Create;

  try
    // Check if tabs on the graphical user interface (GUI) contain sensible values
    // The procedures below MUST inform the users what exactly they should correct in the GUI,
    // otherwise it will not be evident to the user what needs to be corrected.
    //
    // To these "Check..." procedures we pass: (1) objects to hold models, that these procedures must fill in,
    // and (2) the list of "GlobalConstraints" that these procedures add as a field into the above object.
    //
    // "CheckDesignTab" is special, because it fills the list of global constraints with actual data from the form,
    // therefore we need to call it first, as other functions may rely on "GlobalConstraints".
    if not CheckDesignTab (GlobalConstraints) then Exit;
    // Now, "GlobalConstraints" has been filled from the form, proceed with the other tabs
    if not
      (CheckSettingsTab (GlobalSettings) and CheckNodesTab and
       CheckPerformanceTab (ADirectPerfModel, AInversePerfModel, GlobalConstraints) and
       CheckNetworkTab (ANetworkModel, GlobalConstraints, DesignNetwork) and
       CheckUpsTab (AUpsModel, GlobalConstraints, DesignUps))
         then Exit;

    // Clear the output memo
    DesignCurrentConfigMemo.Clear;
    // Remove the labels that may remain there from the previous run
    DesignCurrentConfigRemoveLabels;

    // Save values gathered from the "Settings" tab to the config list
    NodeConfList.Settings.Assign (GlobalSettings);
    NodeConfList.ApplySettings;

    // Create a copy of the list of configurations in case undo is needed
    TempList := TClusterConfigList.Create;
    TempList.Assign (NodeConfList);
    UndoStack.Push (TempList);

    // Disable the undo button so that the user doesn't accidentally press it during the design procedure
    UndoToolButton.Enabled := False;

    // Enable the "Cancel" button
    DesignCancelButton.Enabled := True;
    // Pressing the "Cancel" button in the middle of the design procedure will set this flag to true
    DesignCancel := False;

    // Step only through the enabled configurations
    I := -1; // "I" holds the current configuration number
    J := 0;  // The number of enabled configurations processed so far
    K := NodeConfList.CountEnabled; // The total number of configurations we need to process

    // While not all enabled configurations have been checked, do:
    while J < K do
    begin
      // Exit the loop immediately if the user pressed the "Cancel" button
      if DesignCancel then Break;

      // Increment the configuration's index
      // Need to do this in the beginning of the loop, because the same statement in the end
      // of the loop would not be effective due to multiple "...then Continue" statements
      // that would jump to the beginning.
      I += 1;

      // Fetch a new configuration
      AConf := NodeConfList[I];

      // Only proceed with configurations that are marked as enabled
      if AConf.Enabled then
      begin
        // Update the progress bar with the current number of processed configurations
        DesignProgressBar.Position := Ceil (100 * J / K);

        // For every 10 configurations, ask the window manager to process messages, e.g.,
        // redraw the progress bar so that it doesn't stall.
        if J mod 10 = 0 then Application.ProcessMessages;

        // An new enabled configuration was found, update the counter
        // (the progress bar will be updated on the next call)
        J += 1;

        // Below are calls to web services. If some of them fail, then probably the configuration was disabled
        // (most likely, it violates some constraints). We don't need to process it anymore, so we continue the
        // loop from a new iteration -- hence the "if not ... then Continue" constructs below.
        // But even if we didn't continue with the next iteration of the loop immediately, the next-in-sequence
        // functions would determine that a configuration was disabled, and would exit immediately.

        // Determine performance
        if PerformanceModelList.GetFirstEnabledItem.E.Text = cBuiltIn then
        // It is one of the built-in models
        begin
          if PerformanceModelList.GetFirstEnabledItem.L.Caption = cPeakPerformance then  // A built-in model for peak performance
            if not AConf.RunInversePeakPerformanceModel (AInversePerfModel) then         // Run the built-in peak performance model
              Continue;                                                                  // If it failed, continue with the next config
        end
        else
        // It is not a built-in model, but rather a pointer to a web service
        begin
          // Supply the projected performance, receive the number of cores.
          // The number of nodes will be automatically adjusted to the nearest (bigger than or equal)
          // integer value, which slightly increases the number of cores.
          if not AConf.RunInversePerformanceModel (AInversePerfModel) then Continue;

          // After we call an inverse performance model, we determine the numer of cores, and therefore the number
          // of compute nodes. It is now the right time to calculate technical characteristics, because it is quite possible
          // that some of the constraints may be violated at this early stage (such as power, cost, or size).
          AConf.ComputeTechnicalCharacteristics;
          if AConf.CheckConstraints (GlobalConstraints) then Continue;

          { Attention!
            After the previous call ("RunInversePerformanceModel"), some configurations
            might get disabled -- for example, when the requested performance simply cannot be attained
            using that hardware. In this case, the calls below ("ComputePerformance", "ComputeObjFunc", ...)
            WILL NOT be executed (because if a configuration is disabled, even if these functions are called,
            they exit immediately).
            Therefore, DON'T GET CONFUSED if the number of calls to the web service implementing the inverse
            performance model is bigger than the number of calls to the direct performance model: for
            configurations that got disabled, no direct performance modeling is necessary. Analogously,
            if a configuration was disabled based on results of performance modeling (e.g., could not attain
            a required performance level, or violates cost or power constraints), it will not be used for
            the network design stage, and so on.
            }

          // Recompute the performance. As the number of cores has probably increased due to the above operation,
          // the resulting performance is generally slightly bigger than the projected performance.
          if not AConf.ComputePerformance (ADirectPerfModel) then Continue;
          // (No need to recompute technical characteristics after this procedure. It only slightly increases the
          // performance, everything else stays as it is).
        end;

        // If the user asked to design a network, do that
        if DesignNetwork then
          if not AConf.DesignNetwork (ANetworkModel) then Continue;

        // Again, compute technical characteristics. The configuration might get disabled now
        AConf.ComputeTechnicalCharacteristics;
        if AConf.CheckConstraints (GlobalConstraints) then Continue;

        // If the user asked to design an UPS subsytem, do that, too
        if DesignUps then
          if not AConf.DesignUps (AUpsModel) then Continue;

        // Yet again, compute technical characteristics.
        AConf.ComputeTechnicalCharacteristics;
        if AConf.CheckConstraints (GlobalConstraints) then Continue;

        // Compute the objective function
        AConf.ComputeObjFunc (DesignObjFuncComboBox.Text);
      end;
    end;

    // The cycle has finished, indicate this by clearing the progress bar
    DesignProgressBar.Position := 0;

    // Disable the "Cancel" button
    DesignCancelButton.Enabled := False;
    // If the procedure was cancelled prematurely, inform the user
    if DesignCancel then
      ShowMessageFmt ('Design procedure cancelled by the user. ' +
        LineEnding + '%d of %d enabled configurations were analyzed', [J, K]);

    // Enable the undo button, possibly updating its caption as well
    UpdateUndoButton;

    // Quite possibly, some configs were disabled. Delete them if necessary.
    AutomaticallyDeleteDisabledConfigurations;

    // Sort by value of the objective function
    AStatusBar.Panels[0].Text := cStatusSorting;
    Application.ProcessMessages;  // Otherwise the status might be displayed too late
    NodeConfList.SortByObjFunc;
    AStatusBar.Panels[0].Text := '';

    // Print the list of configurations and update the status bar
    RedrawConfigList;

  finally
    // Free the objects
    ADirectPerfModel.Free;
    AInversePerfModel.Free;
    ANetworkModel.Free;
    AUpsModel.Free;
    GlobalConstraints.Free;
    GlobalSettings.Free;
  end;
end;

procedure TMainForm.DesignCancelButtonClick(Sender: TObject);
begin
  DesignCancel := True;
end;

procedure TMainForm.AboutURLLabelClick (Sender: TObject);
begin
  OpenURL (cProgramURL);
end;

procedure TMainForm.DesignCurrentConfigSpinEditChange (Sender: TObject);
begin
  // When the user presses arrows on the SpinEdit control, we update the current config in the memo
  // NB: We count configs starting with "1"
  PrintCurrentConfig (DesignCurrentConfigSpinEdit.Value - 1);
end;

procedure TMainForm.DesignObjFuncComboBoxEditingDone (Sender: TObject);
begin
  // If the user tried to type his own expression into the ComboBox
  if DesignObjFuncComboBox.ItemIndex = -1 then
    ShowMessage ('Only pre-programmed objective functions are currently supported, sorry.');
end;

end.


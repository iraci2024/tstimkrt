unit DemoBasicMain;

interface

{$I cxVer.inc}

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ComCtrls, StdCtrls,
  dxCore, cxContainer, cxEdit, cxTextEdit, cxDropDownEdit, cxMaskEdit, cxSpinEdit,
  cxGraphics, cxClasses, cxControls,
{$IFDEF EXPRESSSKINS}
  dxSkinsdxRibbonPainter,
  dxSkinsdxBarPainter,
{$ENDIF}
{$IFDEF EXPRESSBARS}
  dxBar, dxStatusBar,
{$ENDIF}
  cxLookAndFeels, cxLookAndFeelPainters, dxLayoutControl,
  dxLayoutContainer, dxLayoutLookAndFeels, dxLayoutcxEditAdapters, dxBarBuiltInMenu,
  dxGanttControlCustomClasses, dxGanttControl, dxGanttControlCustomSheet, dxGanttControlViewChart,
  dxGanttControlViewResourceSheet, dxGanttControlViewTimeline, dxGanttControlTasks, dxGanttControlAssignments,
  dxGanttControlResources, cxGroupBox;

type
  TDemoBasicMainForm = class(TForm)
    mmMain: TMainMenu;
    miAbout: TMenuItem;
    miFile: TMenuItem;
    miExit: TMenuItem;
    Separator1: TMenuItem;
    lfController: TcxLookAndFeelController;
    cxGroupBox1: TcxGroupBox;
    lcMainGroup_Root: TdxLayoutGroup;
    lcMain: TdxLayoutControl;
    GanttControl: TdxGanttControl;
    dxLayoutItem1: TdxLayoutItem;
    Export1: TMenuItem;
    Import1: TMenuItem;
    SaveDialog: TSaveDialog;
    OpenDialog: TOpenDialog;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    dxLayoutGroup1: TdxLayoutGroup;
    lgOptions: TdxLayoutGroup;
    lgActiveView: TdxLayoutGroup;
    lchbDeleteConfirm: TdxLayoutCheckBoxItem;
    lchbDirectX: TdxLayoutCheckBoxItem;
    lchbChartCellAutoHeight: TdxLayoutCheckBoxItem;
    lchbChartColumnHide: TdxLayoutCheckBoxItem;
    lchbChartColumnMove: TdxLayoutCheckBoxItem;
    lchbChartColumnSize: TdxLayoutCheckBoxItem;
    lchbChartColumnQuickCustomization: TdxLayoutCheckBoxItem;
    lchbChartVisible: TdxLayoutCheckBoxItem;
    dxLayoutGroup2: TdxLayoutGroup;
    dxLayoutGroup3: TdxLayoutGroup;
    dxLayoutGroup4: TdxLayoutGroup;
    dxLayoutGroup5: TdxLayoutGroup;
    lchbResourceSheetCellAutoHeight: TdxLayoutCheckBoxItem;
    lchbResourceSheetColumnHide: TdxLayoutCheckBoxItem;
    lchbResourceSheetColumnMove: TdxLayoutCheckBoxItem;
    lchbResourceSheetColumnSize: TdxLayoutCheckBoxItem;
    lchbResourceSheetColumnQuickCustomization: TdxLayoutCheckBoxItem;
    lchbShowOnlyExplicitlyAddedTasks: TdxLayoutCheckBoxItem;
    dxLayoutItem4: TdxLayoutItem;
    seTimelineUnitMinWidth: TcxSpinEdit;
    dxLayoutItem3: TdxLayoutItem;
    cmbTimelineScale: TcxComboBox;
    dxLayoutSeparatorItem1: TdxLayoutSeparatorItem;
    dxLayoutEmptySpaceItem1: TdxLayoutEmptySpaceItem;
    dxLayoutItem5: TdxLayoutItem;
    cbSrcrollBars: TcxComboBox;
    dxLayoutGroup6: TdxLayoutGroup;
    dxLayoutGroup7: TdxLayoutGroup;
    dxLayoutItem2: TdxLayoutItem;
    cmbChartTimescale: TcxComboBox;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    New1: TMenuItem;
    Separator2: TMenuItem;
    lbDescription: TLabel;
    procedure FileExitExecute(Sender: TObject);
    procedure miAboutClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Export1Click(Sender: TObject);
    procedure Import1Click(Sender: TObject);
    procedure lchbDeleteConfirmClick(Sender: TObject);
    procedure lchbDirectXClick(Sender: TObject);
    procedure cbSrcrollBarsPropertiesEditValueChanged(Sender: TObject);
    procedure lgActiveViewTabChanged(Sender: TObject);
    procedure lchbChartCellAutoHeightClick(Sender: TObject);
    procedure lchbChartVisibleClick(Sender: TObject);
    procedure cmbChartTimescalePropertiesChange(Sender: TObject);
    procedure lchbResourceSheetCellAutoHeightClick(Sender: TObject);
    procedure lchbShowOnlyExplicitlyAddedTasksClick(Sender: TObject);
    procedure cmbTimelineScalePropertiesChange(Sender: TObject);
    procedure seTimelineUnitMinWidthPropertiesChange(Sender: TObject);
    procedure GanttControlActiveViewChanged(Sender: TObject);
    procedure New1Click(Sender: TObject);
    procedure GanttControlDataModelLoaded(Sender: TObject);
    procedure GanttControlAssignmentChanged(Sender: TObject; AAssignment: TdxGanttControlAssignment);
    procedure GanttControlResourceChanged(Sender: TObject; AResource: TdxGanttControlResource);
    procedure GanttControlTaskChanged(Sender: TObject; ATask: TdxGanttControlTask);
  protected
    FModified: Boolean;
    procedure AddLookAndFeelMenu; virtual;
    function EnableDocumentOpen: Boolean; virtual;
    function GetDefaultLookAndFeelKind: TcxLookAndFeelKind; virtual;
    function IsNativeDefaultStyle: Boolean; virtual;
    procedure SetDefaultLookAndFeel; virtual;
  public
  {$IFDEF EXPRESSBARS}
    BarManager: TdxBarManager;
  {$ENDIF}
  end;

var
  DemoBasicMainForm: TDemoBasicMainForm;

implementation

uses
  Variants, DateUtils, AboutDemoForm, SkinDemoUtils;

{$R *.dfm}

procedure TDemoBasicMainForm.FormCreate(Sender: TObject);
begin
  SetDefaultLookAndFeel;
  AddLookAndFeelMenu;
end;

procedure TDemoBasicMainForm.AddLookAndFeelMenu;
begin
{$IFDEF EXPRESSBARS}
  BarManager := TdxBarManager.Create(Self);
  dxBarConvertMainMenu(mmMain, BarManager);
  BarManager.Style := bmsUseLookAndFeel;
{$ENDIF}
{$IFDEF EXPRESSBARS}
  CreateSkinsMenuItem(BarManager);
{$ELSE}
  CreateSkinsMenuItem(mmMain);
{$ENDIF}
end;

function TDemoBasicMainForm.GetDefaultLookAndFeelKind: TcxLookAndFeelKind;
begin
  Result := lfUltraFlat;
end;

function TDemoBasicMainForm.IsNativeDefaultStyle: Boolean;
begin
  Result := False;
end;

procedure TDemoBasicMainForm.SetDefaultLookAndFeel;
begin
  lfController.NativeStyle := IsNativeDefaultStyle;
  lfController.Kind := GetDefaultLookAndFeelKind;
end;

procedure TDemoBasicMainForm.miAboutClick(Sender: TObject);
begin
  ShowAboutDemoForm;
end;

function TDemoBasicMainForm.EnableDocumentOpen: Boolean;
var
   mr: Integer;
begin
  Result := not FModified;
  if not Result then
  begin
    mr := MessageDlg('Do you want to save your changes?', mtConfirmation, mbYesNoCancel, 0);
    if mr = mrCancel then
      Exit;
    Result := mr = mrNo;
    if not Result then
    begin
      Result := SaveDialog.Execute;
      if Result then
        GanttControl.DataModel.SaveToFile(SaveDialog.FileName);
    end;
  end;
end;

procedure TDemoBasicMainForm.New1Click(Sender: TObject);
begin
  if EnableDocumentOpen then
  begin
    GanttControl.DataModel.Reset;
    FModified := False;
  end
end;

procedure TDemoBasicMainForm.Import1Click(Sender: TObject);
begin
  if EnableDocumentOpen and OpenDialog.Execute then
    GanttControl.DataModel.LoadFromFile(OpenDialog.FileName);
end;

procedure TDemoBasicMainForm.Export1Click(Sender: TObject);
begin
  if SaveDialog.Execute then
  begin
    GanttControl.DataModel.SaveToFile(SaveDialog.FileName);
    FModified := False;
  end;
end;

procedure TDemoBasicMainForm.FileExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TDemoBasicMainForm.GanttControlActiveViewChanged(Sender: TObject);
begin
  if GanttControl.ViewChart.Active then
    lgActiveView.ItemIndex := 0
  else
  if GanttControl.ViewResourceSheet.Active then
    lgActiveView.ItemIndex := 1
  else
    lgActiveView.ItemIndex := 2;
end;

procedure TDemoBasicMainForm.GanttControlDataModelLoaded(Sender: TObject);
begin
  FModified := False;
end;

procedure TDemoBasicMainForm.GanttControlAssignmentChanged(Sender: TObject; AAssignment: TdxGanttControlAssignment);
begin
  FModified := True;
end;

procedure TDemoBasicMainForm.GanttControlResourceChanged(Sender: TObject; AResource: TdxGanttControlResource);
begin
  FModified := True;
end;

procedure TDemoBasicMainForm.GanttControlTaskChanged(Sender: TObject; ATask: TdxGanttControlTask);
begin
  FModified := True;
end;

procedure TDemoBasicMainForm.lgActiveViewTabChanged(Sender: TObject);
begin
  case lgActiveView.ItemIndex of
    0:
      GanttControl.ViewChart.Active := True;
    1:
      GanttControl.ViewResourceSheet.Active := True;
    2:
      GanttControl.ViewTimeline.Active := True;
  end;
end;

procedure TDemoBasicMainForm.cmbChartTimescalePropertiesChange(Sender: TObject);
begin
  GanttControl.ViewChart.TimescaleUnit := TdxGanttControlChartViewTimescaleUnit(cmbChartTimescale.ItemIndex);
end;

procedure TDemoBasicMainForm.lchbChartCellAutoHeightClick(Sender: TObject);
begin
  GanttControl.ViewChart.OptionsSheet.CellAutoHeight := lchbChartCellAutoHeight.Checked;
  GanttControl.ViewChart.OptionsSheet.AllowColumnHide := lchbChartColumnHide.Checked;
  GanttControl.ViewChart.OptionsSheet.AllowColumnMove := lchbChartColumnMove.Checked;
  GanttControl.ViewChart.OptionsSheet.AllowColumnSize := lchbChartColumnSize.Checked;
  GanttControl.ViewChart.OptionsSheet.ColumnQuickCustomization := lchbChartColumnQuickCustomization.Checked;
end;

procedure TDemoBasicMainForm.lchbChartVisibleClick(Sender: TObject);
begin
  GanttControl.ViewChart.OptionsSheet.Visible := lchbChartVisible.Checked;
  lchbChartCellAutoHeight.Enabled := lchbChartVisible.Checked;
  lchbChartColumnHide.Enabled := lchbChartVisible.Checked;
  lchbChartColumnMove.Enabled := lchbChartVisible.Checked;
  lchbChartColumnSize.Enabled := lchbChartVisible.Checked;
  lchbChartColumnQuickCustomization.Enabled := lchbChartVisible.Checked;
end;

procedure TDemoBasicMainForm.lchbResourceSheetCellAutoHeightClick(Sender: TObject);
begin
  GanttControl.ViewResourceSheet.OptionsSheet.CellAutoHeight := lchbResourceSheetCellAutoHeight.Checked;
  GanttControl.ViewResourceSheet.OptionsSheet.AllowColumnHide := lchbResourceSheetColumnHide.Checked;
  GanttControl.ViewResourceSheet.OptionsSheet.AllowColumnMove := lchbResourceSheetColumnMove.Checked;
  GanttControl.ViewResourceSheet.OptionsSheet.AllowColumnSize := lchbResourceSheetColumnSize.Checked;
  GanttControl.ViewResourceSheet.OptionsSheet.ColumnQuickCustomization := lchbResourceSheetColumnQuickCustomization.Checked;
end;

procedure TDemoBasicMainForm.lchbShowOnlyExplicitlyAddedTasksClick(Sender: TObject);
begin
  GanttControl.ViewTimeLine.ShowOnlyExplicitlyAddedTasks := lchbShowOnlyExplicitlyAddedTasks.Checked;
end;

procedure TDemoBasicMainForm.cmbTimelineScalePropertiesChange(Sender: TObject);
begin
  GanttControl.ViewTimeLine.TimescaleUnit := TdxGanttControlTimeLineViewTimescaleUnit(cmbTimelineScale.ItemIndex);
end;

procedure TDemoBasicMainForm.cbSrcrollBarsPropertiesEditValueChanged(Sender: TObject);
begin
  GanttControl.LookAndFeel.ScrollbarMode := TdxScrollbarMode(cbSrcrollBars.ItemIndex);
end;

procedure TDemoBasicMainForm.seTimelineUnitMinWidthPropertiesChange(Sender: TObject);
begin
  GanttControl.ViewTimeLine.TimescaleUnitMinWidth := seTimelineUnitMinWidth.Value;
end;

procedure TDemoBasicMainForm.lchbDeleteConfirmClick(Sender: TObject);
begin
  GanttControl.OptionsBehavior.ConfirmDelete := lchbDeleteConfirm.Checked;
end;

procedure TDemoBasicMainForm.lchbDirectXClick(Sender: TObject);
const
  AMode: array[Boolean] of TdxRenderMode = (rmGDI, rmDirectX);
begin
  lfController.RenderMode := AMode[lchbDirectX.Checked];
end;

initialization
  UseLatestCommonDialogs := False;

end.


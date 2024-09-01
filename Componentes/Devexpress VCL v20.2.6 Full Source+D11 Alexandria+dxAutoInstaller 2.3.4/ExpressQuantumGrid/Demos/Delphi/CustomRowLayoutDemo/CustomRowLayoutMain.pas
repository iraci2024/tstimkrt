unit CustomRowLayoutMain;

interface

{$I cxVer.inc}

uses
  Types, Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, ActnList,
  Dialogs, cxStyles, cxCustomData, cxGraphics, cxFilter, cxData, cxDataStorage,
  cxEdit, DB, cxDBData, BaseForm, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, cxNavigator, dxDateRanges, dxScrollbarAnnotations,
  dxLayoutLookAndFeels, ImgList, DevAVData, dxRatingControl,
  cxImageList, cxEditRepositoryItems, cxGridLevel, dxLayoutContainer,
  cxGridInplaceEditForm, cxGridCustomView, cxGridCustomTableView, Math,
  cxGridTableView, cxGridDBTableView, cxGrid, cxGridCardView, cxClasses,
  Menus, ComCtrls, StdCtrls, cxTextEdit, dxmdaset, cxDrawTextUtils,
  cxGridBandedTableView, cxGridDBBandedTableView, Variants, cxGeometry;

type
  TfrmMain = class(TfmBaseForm)
    Grid: TcxGrid;
    MasterLevel: TcxGridLevel;
    MasterTableView: TcxGridDBTableView;
    MasterTableViewDepartment: TcxGridDBColumn;
    MasterTableViewTitle: TcxGridDBColumn;
    MasterTableViewStatus: TcxGridDBColumn;
    MasterTableViewFullName: TcxGridDBColumn;
    MasterTableViewPrefix: TcxGridDBColumn;
    MasterTableViewMobilePhone: TcxGridDBColumn;
    MasterTableViewEmail: TcxGridDBColumn;
    MasterTableViewSkype: TcxGridDBColumn;
    MasterTableViewAddress_Line: TcxGridDBColumn;
    MasterTableViewAddress_City: TcxGridDBColumn;
    MasterTableViewAddress_State: TcxGridDBColumn;
    MasterTableViewPicture: TcxGridDBColumn;
    MasterTableViewPerson: TcxGridDBColumn;
    MasterTableViewHireDate: TcxGridDBColumn;
    MasterTableViewPersonalProfile: TcxGridDBColumn;
    MasterTableViewFirstName: TcxGridDBColumn;
    MasterTableViewLastName: TcxGridDBColumn;
    MasterTableViewHomePhone: TcxGridDBColumn;
    MasterTableViewBirthDate: TcxGridDBColumn;
    MasterTableViewAddress_ZipCode: TcxGridDBColumn;
    MasterTableViewAddress_Latitude: TcxGridDBColumn;
    MasterTableViewAddress_Longitude: TcxGridDBColumn;
    MasterTableViewAge: TcxGridDBColumn;
    MasterTableViewWorked: TcxGridDBColumn;
    MasterTableViewLevel: TcxGridDBColumn;
    DetailLevel: TcxGridLevel;
    DetailTableView: TcxGridDBTableView;
    DetailTableViewSubject: TcxGridDBColumn;
    DetailTableViewStartDate: TcxGridDBColumn;
    DetailTableViewDueDate: TcxGridDBColumn;
    DetailTableViewCompletion: TcxGridDBColumn;
    DetailTableViewAssignedEmployeeId: TcxGridDBColumn;
    DetailTableViewOwnerId: TcxGridDBColumn;
    ilImages: TcxImageList;
    miOptions: TMenuItem;
    miItemHotTrack: TMenuItem;
    miSeparator: TMenuItem;
    miCustomizeLayout: TMenuItem;
    miCustomizeEditForm: TMenuItem;
    alAction: TActionList;
    actItemHotTrack: TAction;
    actCustomizeLayout: TAction;
    actCustomizeEditForm: TAction;
    procedure actCustomizeLayoutExecute(Sender: TObject);
    procedure actCustomizeEditFormExecute(Sender: TObject);
    procedure actItemHotTrackExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MasterTableViewLevelCustomDrawCell(Sender: TcxCustomGridTableView;
      ACanvas: TcxCanvas; AViewInfo: TcxGridTableDataCellViewInfo; var ADone: Boolean);
    procedure MasterTableViewMouseLeave(Sender: TObject);
    procedure MasterTableViewMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure MasterTableViewCellClick(Sender: TcxCustomGridTableView;
      ACellViewInfo: TcxGridTableDataCellViewInfo; AButton: TMouseButton;
      AShift: TShiftState; var AHandled: Boolean);
    procedure MasterTableViewCustomDrawTextEditCell(Sender: TcxCustomGridTableView; 
      ACanvas: TcxCanvas; AViewInfo: TcxGridTableDataCellViewInfo; var ADone: Boolean);
    procedure MasterTableViewDetachedEditFormInitialize(
      Sender: TcxGridTableView; AForm: TForm);
    procedure MasterTableViewInitEdit(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem; AEdit: TcxCustomEdit);
  strict private
    FHotRecordIndex: Integer;

    procedure SetHotRecordIndex(AValue: Integer);
  public
    function GetEditIndicatorBounds(AViewInfo: TcxGridTableDataCellViewInfo): TRect;

    property HotRecordIndex: Integer read FHotRecordIndex write SetHotRecordIndex;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

uses
  cxGridCommon;

function TfrmMain.GetEditIndicatorBounds(AViewInfo: TcxGridTableDataCellViewInfo): TRect;
const
  AIndent = 2;
var
  ATextWidth: Integer;
  AEditViewInfo: TcxCustomTextEditViewInfo;
begin
  AEditViewInfo := TcxCustomTextEditViewInfo(AViewInfo.EditViewInfo);
  Result := AEditViewInfo.TextRect;
  ATextWidth := cxTextWidth(AEditViewInfo.Font, AEditViewInfo.Text);
  Result.Left := Result.Left + ATextWidth + AIndent;
  Result.Right := Result.Left + ilImages.Width;
  Result := cxRectCenterVertically(Result, ilImages.Height);
end;

procedure TfrmMain.SetHotRecordIndex(AValue: Integer);
begin
  if HotRecordIndex <> AValue then
  begin
    if MasterTableView.ViewData.IsRecordIndexValid(HotRecordIndex) then
      MasterTableView.ViewData.Records[HotRecordIndex].Invalidate(MasterTableViewPerson);
    FHotRecordIndex := AValue;
    if MasterTableView.ViewData.IsRecordIndexValid(HotRecordIndex) then
      MasterTableView.ViewData.Records[HotRecordIndex].Invalidate(MasterTableViewPerson);
  end;
end;

procedure TfrmMain.actCustomizeEditFormExecute(Sender: TObject);
begin
  MasterTableView.OptionsBehavior.EditMode := emModalEditForm;
  try
    MasterTableView.Controller.ShowEditFormCustomizationDialog;
  finally
    MasterTableView.OptionsBehavior.EditMode := emInplace;
  end;
end;

procedure TfrmMain.actCustomizeLayoutExecute(Sender: TObject);
begin
  MasterTableView.Controller.ShowRowLayoutCustomizationDialog;
end;

procedure TfrmMain.actItemHotTrackExecute(Sender: TObject);
begin
  MasterTableView.RowLayout.ItemHotTrack := actItemHotTrack.Checked;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  inherited;
  dmDevAV.mdTasks.SortedField := dmDevAV.mdTasksAssignedEmployeeId.FieldName;
  MasterTableView.DataController.Groups.FullExpand;
  MasterTableView.Controller.FocusedRowIndex := 2;
  MasterTableView.Controller.FocusFirstAvailableItem;
end;

procedure TfrmMain.MasterTableViewCellClick(Sender: TcxCustomGridTableView;
  ACellViewInfo: TcxGridTableDataCellViewInfo; AButton: TMouseButton; AShift: TShiftState; var AHandled: Boolean);
var
  APos: TPoint;
  ABounds: TRect;
  ARow: TcxGridDataRow;
  ARecordIndex: Integer;
begin
  if (AButton = mbLeft) and (AShift = []) and (ACellViewInfo.Item = MasterTableViewPerson) then
  begin
    APos := dxMapWindowPoint(0, MasterTableView.Site.Handle, GetMouseCursorPos);
    ABounds := GetEditIndicatorBounds(ACellViewInfo);
    if PtInRect(ABounds, APos) then
    begin
      ARow := TcxGridDataRow(ACellViewInfo.GridRecord);
      if MasterTableView.DataController.IsEditing then
      begin
        HotRecordIndex := -1;
        ARecordIndex := ARow.RecordIndex;
        MasterTableView.DataController.Post(True);
        ARow := TcxGridDataRow(MasterTableView.ViewData.GetRecordByRecordIndex(ARecordIndex));
      end;
      if ARow <> nil then
      begin
        MasterTableViewPicture.Options.Focusing := True;
        MasterTableViewPicture.Options.Editing := True;
        MasterTableViewLevel.Options.Focusing := True;
        MasterTableViewLevel.Options.Editing := True;
        TdxRatingControlProperties(MasterTableViewLevel.Properties).AllowHover := True;
        MasterTableView.OptionsBehavior.EditMode := emModalEditForm;
        try
          ARow.EditFormVisible := True;
        finally
          MasterTableViewPicture.Options.Focusing := False;
          MasterTableViewPicture.Options.Editing := False;
          MasterTableViewLevel.Options.Focusing := False;
          MasterTableViewLevel.Options.Editing := False;
          TdxRatingControlProperties(MasterTableViewLevel.Properties).AllowHover := False;
          MasterTableView.OptionsBehavior.EditMode := emInplace;
          AHandled := True;
        end;
      end;
    end;
  end;
end;

procedure TfrmMain.MasterTableViewCustomDrawTextEditCell(Sender: TcxCustomGridTableView;
  ACanvas: TcxCanvas; AViewInfo: TcxGridTableDataCellViewInfo; var ADone: Boolean);
const
  ATextIndent = 2;
var
  ABounds: TRect;
begin
  AViewInfo.EditViewInfo.Transparent := True;
  AViewInfo.EditViewInfo.Offset(-ATextIndent, 0);
  try
    AViewInfo.EditViewInfo.Paint(ACanvas);
    if (AViewInfo.Item = MasterTableViewPerson) and (AViewInfo.GridRecord.Index = HotRecordIndex) then
    begin
      ABounds := GetEditIndicatorBounds(AViewInfo);
      ilImages.Draw(ACanvas.Canvas, ABounds, 0);
    end;
  finally
    AViewInfo.EditViewInfo.Offset(ATextIndent, 0);
  end;
  ACanvas.ExcludeClipRect(AViewInfo.Bounds);
  ADone := True;
end;

procedure TfrmMain.MasterTableViewDetachedEditFormInitialize(Sender: TcxGridTableView; AForm: TForm);
begin
  AForm.BorderStyle := bsDialog;
end;

procedure TfrmMain.MasterTableViewInitEdit(Sender: TcxCustomGridTableView; AItem: TcxCustomGridTableItem; AEdit: TcxCustomEdit);
begin
  if AItem = MasterTableViewLevel then
    TdxRatingControl(AEdit).Transparent := True;
end;

procedure TfrmMain.MasterTableViewLevelCustomDrawCell(Sender: TcxCustomGridTableView;
  ACanvas: TcxCanvas; AViewInfo: TcxGridTableDataCellViewInfo; var ADone: Boolean);
begin
  AViewInfo.EditViewInfo.Transparent := True;
  if AViewInfo.State = gcsSelected then
    AViewInfo.State := gcsNone;
  if not MasterTableView.RowLayout.ItemHotTrack then
    ACanvas.ExcludeFrameRect(cxRectInflate(AViewInfo.EditViewInfo.Bounds, 1));
end;

procedure TfrmMain.MasterTableViewMouseLeave(Sender: TObject);
begin
  HotRecordIndex := -1;
end;

procedure TfrmMain.MasterTableViewMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  AHitTest: TcxCustomGridHitTest;
begin
  AHitTest := MasterTableView.ViewInfo.GetHitTest(Point(X, Y));
  if (AHitTest <> nil) and (AHitTest.HitTestCode in [htRecord, htCell]) then
    HotRecordIndex := TcxGridRecordHitTest(AHitTest).GridRecord.Index
  else
    HotRecordIndex := -1;
end;

end.

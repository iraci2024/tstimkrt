unit ScrollbarAnnotationsDemoMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, DemoBasicMain, Menus, cxClasses, cxLookAndFeels,
  ActnList, ImgList, ComCtrls, StdCtrls, dxCore,
  cxGraphics, cxControls, cxLookAndFeelPainters, cxStyles, cxEdit, cxTextEdit,
  cxVGrid, cxDBVGrid, cxInplaceContainer, cxCustomData, dxScrollbarAnnotations;

type
  TScrollbarAnnotationsDemoMainForm = class(TDemoBasicMainForm)
    ActionList1: TActionList;
    actScrollAnnotationsActive: TAction;
    actDoorCount: TAction;
    actPrice: TAction;
    actCylinderCount: TAction;
    actShowErrors: TAction;
    actShowSearchResults: TAction;
    actShowFocusedRow: TAction;
    actCustomAnnotationSettings: TAction;
    cxDBVerticalGrid1: TcxDBVerticalGrid;
    clName: TcxDBEditorRow;
    clModification: TcxDBEditorRow;
    clPrice: TcxDBEditorRow;
    cxDBVerticalGrid1MPGCity: TcxDBEditorRow;
    cxDBVerticalGrid1MPGHighway: TcxDBEditorRow;
    clDoorCount: TcxDBEditorRow;
    clCilinderCount: TcxDBEditorRow;
    cxDBVerticalGrid1Horsepower: TcxDBEditorRow;
    cxDBVerticalGrid1Torque: TcxDBEditorRow;
    cxDBVerticalGrid1TransmissionSpeeds: TcxDBEditorRow;
    cxDBVerticalGrid1TransmissionType: TcxDBEditorRow;
    cxDBVerticalGrid1Description: TcxDBEditorRow;
    cxDBVerticalGrid1DeliveryDate: TcxDBEditorRow;
    Scrollannotations1: TMenuItem;
    Active1: TMenuItem;
    N1: TMenuItem;
    ShowErrors1: TMenuItem;
    ShowSearchResults1: TMenuItem;
    ShowFocusedRow1: TMenuItem;
    N2: TMenuItem;
    Doorcountislessthen41: TMenuItem;
    N6cylindercars1: TMenuItem;
    Pricemorethen1000001: TMenuItem;
    Customannotationssettings1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure actCustomAnnotationSettingsExecute(Sender: TObject);
    procedure actDoorCountExecute(Sender: TObject);
    procedure actScrollAnnotationsActiveExecute(Sender: TObject);
    procedure actShowErrorsExecute(Sender: TObject);
    procedure actShowSearchResultsExecute(Sender: TObject);
    procedure actShowFocusedRowExecute(Sender: TObject);
    procedure cxDBVerticalGrid1GetScrollbarAnnotationHint(Sender: TObject; AAnnotationRowIndexLists: TdxScrollbarAnnotationRowIndexLists; var AHint: string);
    procedure cxDBVerticalGrid1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure cxDBVerticalGrid1MPGCityPropertiesValidateDrawValue(
      Sender: TcxCustomEditorRowProperties; ARecordIndex: Integer;
      const AValue: Variant; AData: TcxEditValidateInfo);
    procedure cxDBVerticalGrid1PopulateCustomScrollbarAnnotationRowIndexList(Sender: TObject; AAnnotationIndex: Integer; ARowIndexList: TdxScrollbarAnnotationRowIndexList);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ScrollbarAnnotationsDemoMainForm: TScrollbarAnnotationsDemoMainForm;

implementation

{$R *.dfm}

uses
  Math, StrUtils, CarsData, CustomAnnotationSettings;

procedure TScrollbarAnnotationsDemoMainForm.FormCreate(Sender: TObject);
begin
  inherited;
  SCustomAnnotationstrs[0] := '2-Door Cars';
  SCustomAnnotationstrs[1] := 'Price is Greater Than ' + CurrToStrF(100000, ffCurrency, 0);
  SCustomAnnotationstrs[2] := '6-Cylinder Cars';
  cxDBVerticalGrid1.DataController.FindCriteria.Text := 'V8';
  actDoorCount.Caption := SCustomAnnotationstrs[0];
  actPrice.Caption := SCustomAnnotationstrs[1];
  actCylinderCount.Caption := SCustomAnnotationstrs[2];
end;

procedure TScrollbarAnnotationsDemoMainForm.actCustomAnnotationSettingsExecute(Sender: TObject);
begin
  if frmCustomAnnotationSettings = nil then
  begin
    Application.CreateForm(TfrmCustomAnnotationSettings, frmCustomAnnotationSettings);
    frmCustomAnnotationSettings.Initialize(cxDBVerticalGrid1.ScrollbarAnnotations.CustomAnnotations);
  end;
  frmCustomAnnotationSettings.Show;
end;

procedure TScrollbarAnnotationsDemoMainForm.actDoorCountExecute(Sender: TObject);
begin
  cxDBVerticalGrid1.ScrollbarAnnotations.CustomAnnotations[(Sender as TAction).Tag].Visible := (Sender as TAction).Checked;
end;

procedure TScrollbarAnnotationsDemoMainForm.actScrollAnnotationsActiveExecute(Sender: TObject);
begin
  cxDBVerticalGrid1.ScrollbarAnnotations.Active := (Sender as TAction).Checked;
end;

procedure TScrollbarAnnotationsDemoMainForm.actShowErrorsExecute(Sender: TObject);
begin
  cxDBVerticalGrid1.ScrollbarAnnotations.ShowErrors := (Sender as TAction).Checked;
end;

procedure TScrollbarAnnotationsDemoMainForm.actShowSearchResultsExecute(Sender: TObject);
begin
  cxDBVerticalGrid1.ScrollbarAnnotations.ShowSearchResults := (Sender as TAction).Checked;
end;

procedure TScrollbarAnnotationsDemoMainForm.actShowFocusedRowExecute(Sender: TObject);
begin
  cxDBVerticalGrid1.ScrollbarAnnotations.ShowFocusedRow := (Sender as TAction).Checked;
end;

procedure TScrollbarAnnotationsDemoMainForm.cxDBVerticalGrid1GetScrollbarAnnotationHint(Sender: TObject; AAnnotationRowIndexLists: TdxScrollbarAnnotationRowIndexLists; var AHint: string);

  function GetValue(ARecordIndex: Integer; AColumn: TcxDBEditorRow): Variant;
  begin
    Result := AColumn.Properties.Values[ARecordIndex];
  end;

var
  AKind: TdxScrollbarAnnotationKind;
  AList: TdxScrollbarAnnotationRowIndexList;
  I: Integer;
  ACount: Integer;
  AAnnotationHint: string;
begin
  for AKind in AAnnotationRowIndexLists.Keys do
  begin
    AList := AAnnotationRowIndexLists[AKind];
    ACount := Min(5, AList.Count);
    for I := 0 to ACount - 1 do
    begin
      case AKind of
        0:
          begin
            AAnnotationHint := Format(' %s doors', [IntToStr(GetValue(AList[I], clDoorCount))]);
          end;
        1:
          begin
            AAnnotationHint := Format('(price: %s)', [FormatFloat('$,0;($,0)', GetValue(AList[I], clPrice))]);
          end;
        2:
          begin
            AAnnotationHint := Format(' %s cylinders', [IntToStr(GetValue(AList[I], clCilinderCount))]);
          end;
        dxErrorScrollbarAnnotationID:
          begin
            AAnnotationHint := ' - MPG City is not specified!';
          end;
      else
        AAnnotationHint := '';
      end;
      if AAnnotationHint <> '' then
      begin
        AAnnotationHint := Format('%s %s', [GetValue(AList[I], clName), GetValue(AList[I], clModification)]) + AAnnotationHint;
        AHint := AHint + IfThen(AHint <> '', dxCRLF) + AAnnotationHint;
      end;
    end;
    if AList.Count > ACount then
      AHint := AHint + dxCRLF + Format('And %d more records...', [AList.Count - ACount]);
  end;
end;

procedure TScrollbarAnnotationsDemoMainForm.cxDBVerticalGrid1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_F4 then
    cxDBVerticalGrid1.GoToNextScrollbarAnnotation([dxErrorScrollbarAnnotationID], not (ssShift in Shift))
end;

procedure TScrollbarAnnotationsDemoMainForm.cxDBVerticalGrid1MPGCityPropertiesValidateDrawValue(
  Sender: TcxCustomEditorRowProperties; ARecordIndex: Integer;
  const AValue: Variant; AData: TcxEditValidateInfo);
begin
  if VarIsStr(AValue) and (AValue = '') or (AValue = Null) then
    AData.ErrorType := eetError;
end;

procedure TScrollbarAnnotationsDemoMainForm.cxDBVerticalGrid1PopulateCustomScrollbarAnnotationRowIndexList(Sender: TObject; AAnnotationIndex: Integer; ARowIndexList: TdxScrollbarAnnotationRowIndexList);

  function GetValue(ARecordIndex: Integer; AColumn: TcxDBEditorRow): Variant;
  begin
    Result := AColumn.Properties.Values[ARecordIndex];
  end;

var
  I: Integer;
  ACondition: Boolean;
  AValue: Variant;
begin
  for I := 0 to cxDBVerticalGrid1.RecordCount - 1 do
  begin
    case AAnnotationIndex of
      0:
        begin
          AValue := GetValue(I, clDoorCount);
          ACondition := not VarIsNull(AValue) and (AValue = 2);
        end;
      1:
        begin
          ACondition := GetValue(I, clPrice) > 100000;
        end;
      2:
        ACondition := GetValue(I, clCilinderCount) = 6
    else
      ACondition := False;
    end;
    if ACondition then
      ARowIndexList.Add(I);
  end;
end;

end.

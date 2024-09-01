unit ScrollbarAnnotationsDemoMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, BaseForm, cxLookAndFeels, cxGridCardView, cxStyles, cxGridTableView,
  cxClasses, Menus, ComCtrls, StdCtrls, cxGraphics, cxControls, dxCore,
  cxLookAndFeelPainters, cxCustomData, cxFilter, cxData, cxDataStorage, cxEdit,
  cxNavigator, dxDateRanges, DB, cxDBData, cxTextEdit, cxGridLevel,
  cxGridCustomTableView, cxGridDBTableView, cxGridCustomView, cxGrid, dxScrollbarAnnotations,
  ActnList, cxCurrencyEdit;

type
  TScrollbarAnnotationsDemoMainForm = class(TfmBaseForm)
    cxGrid1: TcxGrid;
    cxGrid1DBTableView1: TcxGridDBTableView;
    clName: TcxGridDBColumn;
    clModification: TcxGridDBColumn;
    clPrice: TcxGridDBColumn;
    cxGrid1DBTableView1MPGCity: TcxGridDBColumn;
    cxGrid1DBTableView1MPGHighway: TcxGridDBColumn;
    clDoorCount: TcxGridDBColumn;
    clCylinderCount: TcxGridDBColumn;
    cxGrid1DBTableView1Horsepower: TcxGridDBColumn;
    cxGrid1DBTableView1TransmissionSpeeds: TcxGridDBColumn;
    clDescription: TcxGridDBColumn;
    cxGrid1Level1: TcxGridLevel;
    Scrollannotations1: TMenuItem;
    ActionList1: TActionList;
    actScrollAnnotationsActive: TAction;
    Active1: TMenuItem;
    actDoorCount: TAction;
    actPrice: TAction;
    actCylinderCount: TAction;
    Doorcountislessthan41: TMenuItem;
    Pricemorethan1000001: TMenuItem;
    Active2: TMenuItem;
    actShowErrors: TAction;
    actShowSearchResults: TAction;
    actShowFocusedRow: TAction;
    actShowSelectedRows: TAction;
    ShowErrors1: TMenuItem;
    ShowSearchResults1: TMenuItem;
    ShowFocusedRow1: TMenuItem;
    ShowSelectedRows1: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    actCustomAnnotationSettings: TAction;
    Customannotationssettings1: TMenuItem;
    procedure actCustomAnnotationSettingsExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure actCustomScrollAnnotationExecute(Sender: TObject);
    procedure actScrollAnnotationsActiveExecute(Sender: TObject);
    procedure actShowErrorsExecute(Sender: TObject);
    procedure actShowSearchResultsExecute(Sender: TObject);
    procedure actShowFocusedRowExecute(Sender: TObject);
    procedure actShowSelectedRowsExecute(Sender: TObject);
    procedure cxGrid1DBTableView1GetScrollbarAnnotationHint(Sender: TcxCustomGridTableView; AScrollbarAnnotationRows: TdxScrollbarAnnotationRowIndexLists; var AHint: string);
    procedure cxGrid1DBTableView1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure cxGrid1DBTableView1MPGCityValidateDrawValue(Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord; const AValue: Variant; AData: TcxEditValidateInfo);
    procedure cxGrid1DBTableView1PopulateCustomScrollbarAnnotationRowIndexList(Sender: TcxCustomGridTableView; AAnnotationIndex: Integer; ARowIndexList: TdxScrollbarAnnotationRowIndexList);
  private
    { Private declarations }
  protected
  public
    { Public declarations }
  end;

var
  ScrollbarAnnotationsDemoMainForm: TScrollbarAnnotationsDemoMainForm;

implementation

uses
  Math, StrUtils, CarsData, AboutDemoForm, CustomAnnotationSettings;

{$R *.dfm}

procedure TScrollbarAnnotationsDemoMainForm.actCustomAnnotationSettingsExecute(Sender: TObject);
begin
  if frmCustomAnnotationSettings = nil then
  begin
    Application.CreateForm(TfrmCustomAnnotationSettings, frmCustomAnnotationSettings);
    frmCustomAnnotationSettings.Initialize(cxGrid1DBTableView1.ScrollbarAnnotations.CustomAnnotations);
  end;
  frmCustomAnnotationSettings.Show;
end;

procedure TScrollbarAnnotationsDemoMainForm.FormCreate(Sender: TObject);
begin
  inherited;
  SCustomAnnotationStrs[0] := '2-Door Cars';
  SCustomAnnotationStrs[1] := 'Price is Greater Than ' + CurrToStrF(100000, ffCurrency, 0);
  SCustomAnnotationStrs[2] := '6-Cylinder Cars';
  cxGrid1DBTableView1.DataController.FindCriteria.Text := 'V8';
  actDoorCount.Caption := SCustomAnnotationStrs[0];
  actPrice.Caption := SCustomAnnotationStrs[1];
  actCylinderCount.Caption := SCustomAnnotationStrs[2];
end;

procedure TScrollbarAnnotationsDemoMainForm.actCustomScrollAnnotationExecute(Sender: TObject);
begin
  cxGrid1DBTableView1.ScrollbarAnnotations.CustomAnnotations[(Sender as TAction).Tag].Visible := (Sender as TAction).Checked;
end;

procedure TScrollbarAnnotationsDemoMainForm.actScrollAnnotationsActiveExecute(Sender: TObject);
begin
  cxGrid1DBTableView1.ScrollbarAnnotations.Active := (Sender as TAction).Checked;
end;

procedure TScrollbarAnnotationsDemoMainForm.actShowErrorsExecute(Sender: TObject);
begin
  cxGrid1DBTableView1.ScrollbarAnnotations.ShowErrors := (Sender as TAction).Checked;
end;

procedure TScrollbarAnnotationsDemoMainForm.actShowSearchResultsExecute(Sender: TObject);
begin
  cxGrid1DBTableView1.ScrollbarAnnotations.ShowSearchResults := (Sender as TAction).Checked;
end;

procedure TScrollbarAnnotationsDemoMainForm.actShowFocusedRowExecute(Sender: TObject);
begin
  cxGrid1DBTableView1.ScrollbarAnnotations.ShowFocusedRow := (Sender as TAction).Checked;
end;

procedure TScrollbarAnnotationsDemoMainForm.actShowSelectedRowsExecute(Sender: TObject);
begin
  cxGrid1DBTableView1.ScrollbarAnnotations.ShowSelectedRows := (Sender as TAction).Checked;
end;

procedure TScrollbarAnnotationsDemoMainForm.cxGrid1DBTableView1GetScrollbarAnnotationHint(Sender: TcxCustomGridTableView; AScrollbarAnnotationRows: TdxScrollbarAnnotationRowIndexLists; var AHint: string);

  function GetValue(ARecordIndex: Integer; AColumn: TcxGridDBColumn): Variant;
  begin
    Result := cxGrid1DBTableView1.ViewData.Records[ARecordIndex].Values[AColumn.Index];
  end;

var
  AKind: TdxScrollbarAnnotationKind;
  AList: TdxScrollbarAnnotationRowIndexList;
  I: Integer;
  ACount: Integer;
  AAnnotationHint: string;
begin
  for AKind in AScrollbarAnnotationRows.Keys do
  begin
    AList := AScrollbarAnnotationRows[AKind];
    ACount := Min(5, AList.Count);
    for I := 0 to ACount - 1 do
    begin
      case AKind of
        0:
          begin
            AAnnotationHint := Format('(%s doors)', [IntToStr(GetValue(AList[I], clDoorCount))]);
          end;
        1:
          begin
            AAnnotationHint := Format('(price: %s)', [FormatFloat('$,0;($,0)', GetValue(AList[I], clPrice))]);
          end;
        2:
          begin
            AAnnotationHint := Format('(%s cylinders)', [IntToStr(GetValue(AList[I], clCylinderCount))]);
          end;
        dxErrorScrollbarAnnotationID:
          begin
            AAnnotationHint := '(MPG City is not specified!)';
          end;
      else
        AAnnotationHint := '';
      end;
      if AAnnotationHint <> '' then
      begin
        AAnnotationHint := Format('%s %s ', [GetValue(AList[I], clName), GetValue(AList[I], clModification)]) + AAnnotationHint;
        AHint := AHint + IfThen(AHint <> '', dxCRLF) + AAnnotationHint;
      end;
    end;
    if AList.Count > ACount then
      AHint := AHint + dxCRLF + Format('...and %d more car(s)', [AList.Count - ACount]);
  end;
end;

procedure TScrollbarAnnotationsDemoMainForm.cxGrid1DBTableView1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_F4 then
    cxGrid1DBTableView1.GoToNextScrollbarAnnotation([dxErrorScrollbarAnnotationID], not (ssShift in Shift))
end;

procedure TScrollbarAnnotationsDemoMainForm.cxGrid1DBTableView1MPGCityValidateDrawValue(Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord; const AValue: Variant; AData: TcxEditValidateInfo);
begin
  if VarIsStr(AValue) and (AValue = '') or (AValue = Null) then
    AData.ErrorType := eetError;
end;

procedure TScrollbarAnnotationsDemoMainForm.cxGrid1DBTableView1PopulateCustomScrollbarAnnotationRowIndexList(Sender: TcxCustomGridTableView; AAnnotationIndex: Integer; ARowIndexList: TdxScrollbarAnnotationRowIndexList);

  function GetValue(ARecordIndex: Integer; AColumn: TcxGridDBColumn): Variant;
  begin
    if cxGrid1DBTableView1.ViewData.Records[ARecordIndex].IsData then
      Result := cxGrid1DBTableView1.ViewData.Records[ARecordIndex].Values[AColumn.Index]
    else
      Result := Null;
  end;

var
  I: Integer;
  ACondition: Boolean;
  AValue: Variant;
begin
  for I := 0 to cxGrid1DBTableView1.ViewData.RecordCount - 1 do
  begin
    case AAnnotationIndex of
      0:
        begin
          AValue := GetValue(I, clDoorCount);
          ACondition := not VarIsNull(AValue) and (AValue = 2);
        end;
      1: ACondition := GetValue(I, clPrice) > 100000;
      2: ACondition := GetValue(I, clCylinderCount) = 6
    else
      ACondition := False;
    end;
    if ACondition then
      ARowIndexList.Add(I);
  end;
end;

end.

unit ScrollbarAnnotationsDemoMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, DemoBasicMain, Menus, cxClasses, cxLookAndFeels,
  ActnList, ImgList, ComCtrls, StdCtrls, cxGraphics, cxControls, dxCore,
  cxLookAndFeelPainters, cxCustomData, cxStyles, cxTL, cxMaskEdit, cxTextEdit,
  cxCheckBox, cxInplaceContainer, cxDBTL, cxTLData, dxScrollbarAnnotations, cxEdit;

type
  TScrollbarAnnotationsDemoMainForm = class(TDemoBasicMainForm)
    cxDBTreeList1: TcxDBTreeList;
    cxDBTreeListID: TcxDBTreeListColumn;
    cxDBTreeListPARENTID: TcxDBTreeListColumn;
    cxDBTreeListNAME: TcxDBTreeListColumn;
    clBUDGET: TcxDBTreeListColumn;
    clPHONE: TcxDBTreeListColumn;
    cxDBTreeListFAX: TcxDBTreeListColumn;
    cxDBTreeListEMAIL: TcxDBTreeListColumn;
    clVACANCY: TcxDBTreeListColumn;
    ActionList1: TActionList;
    actScrollAnnotationsActive: TAction;
    actVacancy: TAction;
    actBudget: TAction;
    actPhoneNumber: TAction;
    actShowErrors: TAction;
    actShowSearchResults: TAction;
    actShowFocusedRow: TAction;
    actShowSelectedRows: TAction;
    actCustomAnnotationSettings: TAction;
    Scrollannotations1: TMenuItem;
    Active1: TMenuItem;
    N1: TMenuItem;
    Vacancy1: TMenuItem;
    Budgetmorethen300001: TMenuItem;
    Phonenumberstartswith81: TMenuItem;
    Customannotationssettings1: TMenuItem;
    N2: TMenuItem;
    ShowErrors1: TMenuItem;
    ShowSearchResults1: TMenuItem;
    ShowFocusedRow1: TMenuItem;
    ShowSelectedRows1: TMenuItem;
    procedure actCustomAnnotationSettingsExecute(Sender: TObject);
    procedure actScrollAnnotationsActiveExecute(Sender: TObject);
    procedure actShowErrorsExecute(Sender: TObject);
    procedure actShowSearchResultsExecute(Sender: TObject);
    procedure actShowFocusedRowExecute(Sender: TObject);
    procedure actShowSelectedRowsExecute(Sender: TObject);
    procedure actVacancyExecute(Sender: TObject);
    procedure clBUDGETValidateDrawValue(Sender: TcxTreeListColumn; ANode: TcxTreeListNode; const AValue: Variant; AData: TcxEditValidateInfo);
    procedure cxDBTreeList1GetScrollbarAnnotationHint(Sender: TObject; AAnnotationRowIndexLists: TdxScrollbarAnnotationRowIndexLists; var AHint: string);
    procedure cxDBTreeList1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure cxDBTreeList1PopulateCustomScrollbarAnnotationRowIndexList(Sender: TObject; AAnnotationIndex: Integer; ARowIndexList: TdxScrollbarAnnotationRowIndexList);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ScrollbarAnnotationsDemoMainForm: TScrollbarAnnotationsDemoMainForm;

implementation

uses
  Math, StrUtils, ScrollbarAnnotationsDemoData, CustomAnnotationSettings;

{$R *.dfm}

procedure TScrollbarAnnotationsDemoMainForm.actCustomAnnotationSettingsExecute(Sender: TObject);
begin
  if frmCustomAnnotationSettings = nil then
  begin
    Application.CreateForm(TfrmCustomAnnotationsettings, frmCustomAnnotationsettings);
    frmCustomAnnotationsettings.Initialize(cxDBTreeList1.ScrollbarAnnotations.CustomAnnotations);
  end;
  frmCustomAnnotationSettings.Show;
end;

procedure TScrollbarAnnotationsDemoMainForm.actScrollAnnotationsActiveExecute(Sender: TObject);
begin
  cxDBTreeList1.ScrollbarAnnotations.Active := (Sender as TAction).Checked;
end;

procedure TScrollbarAnnotationsDemoMainForm.actShowErrorsExecute(Sender: TObject);
begin
  cxDBTreeList1.ScrollbarAnnotations.ShowErrors := (Sender as TAction).Checked;
end;

procedure TScrollbarAnnotationsDemoMainForm.actShowSearchResultsExecute(Sender: TObject);
begin
  cxDBTreeList1.ScrollbarAnnotations.ShowSearchResults := (Sender as TAction).Checked;
end;

procedure TScrollbarAnnotationsDemoMainForm.actShowFocusedRowExecute(Sender: TObject);
begin
  cxDBTreeList1.ScrollbarAnnotations.ShowFocusedRow := (Sender as TAction).Checked;
end;

procedure TScrollbarAnnotationsDemoMainForm.actShowSelectedRowsExecute(Sender: TObject);
begin
  cxDBTreeList1.ScrollbarAnnotations.ShowSelectedRows := (Sender as TAction).Checked;
end;

procedure TScrollbarAnnotationsDemoMainForm.actVacancyExecute(Sender: TObject);
begin
  cxDBTreeList1.ScrollbarAnnotations.CustomAnnotations[(Sender as TAction).Tag].Visible := (Sender as TAction).Checked;
end;

procedure TScrollbarAnnotationsDemoMainForm.clBUDGETValidateDrawValue(Sender: TcxTreeListColumn; ANode: TcxTreeListNode; const AValue: Variant; AData: TcxEditValidateInfo);
begin
  if AValue < 15000 then
    AData.ErrorType := eetWarning;
end;

procedure TScrollbarAnnotationsDemoMainForm.cxDBTreeList1GetScrollbarAnnotationHint(Sender: TObject; AAnnotationRowIndexLists: TdxScrollbarAnnotationRowIndexLists; var AHint: string);

  function GetValue(ARecordIndex: Integer; AColumn: TcxTreeListColumn): Variant;
  begin
    Result := cxDBTreeList1.AbsoluteVisibleItems[ARecordIndex].Values[AColumn.ItemIndex]
  end;

var
  AKind: TdxScrollbarAnnotationKind;
  AList: TdxScrollbarAnnotationRowIndexList;
  I: Integer;
  ACount: Integer;
  AAnnotationHint, AAdditionalHint: string;
begin
  for AKind in AAnnotationRowIndexLists.Keys do
  begin
    if AKind in (dxAllCustomScrollbarAnnotationKinds + [dxErrorScrollbarAnnotationID]) then
    begin
      AList := AAnnotationRowIndexLists[AKind];
      ACount := Min(5, AList.Count);
      for I := 0 to ACount - 1 do
      begin
        case AKind of
          0:
            AAdditionalHint := 'has vacancy';
          1, dxErrorScrollbarAnnotationID:
            AAdditionalHint := Format('budget is %s', [FormatFloat('$,0;($,0)', GetValue(AList[I], clBUDGET))]);
          2:
            AAdditionalHint := Format('phone number is %s', [GetValue(AList[I], clPHONE)]);
        end;
        AAnnotationHint := Format('%s %s', [GetValue(AList[I], cxDBTreeListNAME), AAdditionalHint]);
        AHint := AHint + IfThen(AHint <> '', dxCRLF) + AAnnotationHint;
      end;
      if AList.Count > ACount then
        AHint := AHint + dxCRLF + Format('And %d more records...', [AList.Count - ACount]);
    end;
  end;
end;

procedure TScrollbarAnnotationsDemoMainForm.cxDBTreeList1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_F4 then
    cxDBTreeList1.GoToNextScrollbarAnnotation([dxErrorScrollbarAnnotationID], not (ssShift in Shift))
end;

procedure TScrollbarAnnotationsDemoMainForm.cxDBTreeList1PopulateCustomScrollbarAnnotationRowIndexList(Sender: TObject; AAnnotationIndex: Integer; ARowIndexList: TdxScrollbarAnnotationRowIndexList);

  function GetValue(ARecordIndex: Integer; AColumn: TcxTreeListColumn): Variant;
  begin
    Result := cxDBTreeList1.AbsoluteVisibleItems[ARecordIndex].Values[AColumn.ItemIndex]
  end;

var
  I: Integer;
  ACondition: Boolean;
  AValue: Variant;
  AStr: string;
begin
  for I := 0 to cxDBTreeList1.AbsoluteVisibleCount - 1 do
  begin
    case AAnnotationIndex of
      0:
        begin
          AValue := GetValue(I, clVACANCY);
          ACondition := not VarIsNull(AValue) and AValue;
        end;
      1:
        begin
          ACondition := GetValue(I, clBUDGET) > 30000;
        end;
      2:
        begin
          AValue := GetValue(I, clPHONE);
          if VarIsStr(AValue) then
          begin
            AStr := AValue;
            ACondition := (Length(AStr) > 1) and (AStr[2] = '8');
          end
          else
            ACondition := False;
        end
    else
      ACondition := False;
    end;
    if ACondition then
      ARowIndexList.Add(I);
  end;
end;

procedure TScrollbarAnnotationsDemoMainForm.FormCreate(Sender: TObject);
begin
  inherited;
  SCustomAnnotationstrs[0] := 'Has open vacancy';
  SCustomAnnotationstrs[1] := 'Budget is Greater Than ' + CurrToStrF(30000, ffCurrency, 0);
  SCustomAnnotationstrs[2] := 'Phone number starts with 8';
  cxDBTreeList1.DataController.FindCriteria.Text := '00000';
  actVacancy.Caption := SCustomAnnotationstrs[0];
  actBudget.Caption := SCustomAnnotationstrs[1];
  actPhoneNumber.Caption := SCustomAnnotationstrs[2];
  cxDBTreeList1.AbsoluteVisibleItems[0].Expand(False);
end;

end.

unit FindPanelDemoMain;

{$I cxVer.inc}

interface

uses
  SysUtils, Classes, Controls, Menus, DB, StdCtrls, BaseForm, ComCtrls, DBClient,
  dxCore, cxStyles, cxCustomData, cxGraphics, cxFilter, cxData, cxDataStorage,
  cxEdit, cxGridLevel, cxClasses, cxControls, cxGridCustomView, cxGridCustomTableView,
  cxGridTableView, cxGrid, cxEditRepositoryItems, cxLookAndFeels, cxLookAndFeelPainters,
  cxGridCardView, cxNavigator, cxContainer, cxGroupBox, ActnList, cxCheckBox, cxLabel,
  cxTextEdit, cxMaskEdit, cxSpinEdit, cxDropDownEdit, cxImageComboBox, cxDBData,
  cxGridDBTableView, XPMan, CarsDataForGrid, MidasLib, dxDateRanges;

type
  TfrmMain = class(TfmBaseForm)
    erMain: TcxEditRepository;
    erMainFlag: TcxEditRepositoryImageItem;
    cxGroupBox1: TcxGroupBox;
    cbClearFindOnClose: TcxCheckBox;
    cbShowClearButton: TcxCheckBox;
    cbShowCloseButton: TcxCheckBox;
    cbShowFindButton: TcxCheckBox;
    cbHighlightSearchResults: TcxCheckBox;
    alAction: TActionList;
    actClearFindOnClose: TAction;
    actShowClearButton: TAction;
    actShowCloseButton: TAction;
    actShowFindButton: TAction;
    actHighlightSearchResults: TAction;
    miFindPanelOptions: TMenuItem;
    ClearFindOnClose1: TMenuItem;
    HighlightFindResult1: TMenuItem;
    miVisibleButtons: TMenuItem;
    ShowClearButton2: TMenuItem;
    ShowCloseButton2: TMenuItem;
    ShowFindButton2: TMenuItem;
    seFindDelay: TcxSpinEdit;
    lbSearchDelay: TcxLabel;
    icbFindFilterColumns: TcxImageComboBox;
    lbSearchableColumns: TcxLabel;
    cbeFindPanelPosition: TcxComboBox;
    lbFindPanelPosition: TcxLabel;
    cbeDisplayMode: TcxComboBox;
    lbDisplayMode: TcxLabel;
    cbUseDelayedSearch: TcxCheckBox;
    actUseDelayedSearch: TAction;
    UseDelayedFind1: TMenuItem;
    dsCustomers: TDataSource;
    cdsCustomers: TClientDataSet;
    actUseExtendedSyntax: TAction;
    cbUseExtendedSyntax: TcxCheckBox;
    UseExtendedSyntax1: TMenuItem;
    Grid: TcxGrid;
    TableView: TcxGridDBTableView;
    GridLevel1: TcxGridLevel;
    cbShowPrevAnNextButtons: TcxCheckBox;
    actShowPrevAndNextButtons: TAction;
    cxLabel1: TcxLabel;
    cbeBehavior: TcxComboBox;
    cbSearchInGroupRows: TcxCheckBox;
    cbSearchPreview: TcxCheckBox;
    actSearchInGroupRows: TAction;
    actSearchInPreview: TAction;
    cdsCustomersCustomerID: TStringField;
    cdsCustomersCompanyName: TStringField;
    cdsCustomersContactName: TStringField;
    cdsCustomersContactTitle: TStringField;
    cdsCustomersAddress: TStringField;
    cdsCustomersCity: TStringField;
    cdsCustomersPostalCode: TStringField;
    cdsCustomersCountry: TStringField;
    cdsCustomersPhone: TStringField;
    cdsCustomersFax: TStringField;
    cdsCustomersRegion: TStringField;
    TableViewCompanyName: TcxGridDBColumn;
    TableViewContactName: TcxGridDBColumn;
    TableViewContactTitle: TcxGridDBColumn;
    TableViewCity: TcxGridDBColumn;
    TableViewCountry: TcxGridDBColumn;
    TableViewAddress: TcxGridDBColumn;
    cbLocation: TcxComboBox;
    cxLabel2: TcxLabel;
    cbLayout: TcxComboBox;
    cxLabel3: TcxLabel;
    procedure FormCreate(Sender: TObject);
    procedure actClearFindOnCloseChange(Sender: TObject);
    procedure actShowClearButtonChange(Sender: TObject);
    procedure actShowCloseButtonChange(Sender: TObject);
    procedure actShowFindButtonEChange(Sender: TObject);
    procedure actHighlightFindResultChange(Sender: TObject);
    procedure seFindDelayPropertiesChange(Sender: TObject);
    procedure icbFindFilterColumnsPropertiesChange(Sender: TObject);
    procedure cbFindPanelPositionPropertiesChange(Sender: TObject);
    procedure cbDisplayModePropertiesChange(Sender: TObject);
    procedure actUseDelayedSearchExecute(Sender: TObject);
    procedure actUseExtendedSyntaxExecute(Sender: TObject);
    procedure actShowPrevAndNextButtonsExecute(Sender: TObject);
    procedure actSearchInGroupRowsExecute(Sender: TObject);
    procedure actSearchInPreviewExecute(Sender: TObject);
    procedure cbeBehaviorPropertiesChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cbLocationPropertiesEditValueChanged(Sender: TObject);
    procedure cbLayoutPropertiesEditValueChanged(Sender: TObject);
  protected
    procedure UpdateFindFilterColumns;
  end;

var
  frmMain: TfrmMain;

implementation

uses
  cxFindPanel, Variants, AboutDemoForm;

{$R *.dfm}

procedure TfrmMain.icbFindFilterColumnsPropertiesChange(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to TableView.ColumnCount - 1 do
    if (icbFindFilterColumns.EditValue = 'All') or
      (Pos(TableView.Columns[I].Name, icbFindFilterColumns.EditValue) > 0) then
      TableView.Columns[I].Options.FilteringWithFindPanel := True
    else
      TableView.Columns[I].Options.FilteringWithFindPanel := False;
end;

procedure TfrmMain.UpdateFindFilterColumns;
var
  I, J: Integer;
  AFindFilterColumnsDescription: string;
  AFindFilterColumnsValue: string;
  AImageComboBoxItem: TcxImageComboBoxItem;
  AColumn: TcxGridColumn;
begin
  icbFindFilterColumns.Properties.Items.Clear;
  AImageComboBoxItem := icbFindFilterColumns.Properties.Items.Add;
  AImageComboBoxItem.Description := 'All';
  AImageComboBoxItem.Value := 'All';
  for I := 0 to TableView.ColumnCount - 1 do
  begin
    AColumn := TableView.Columns[I];
    if not AColumn.Visible then
      Continue;
    AFindFilterColumnsDescription := TableView.Columns[I].Caption;
    AFindFilterColumnsValue := TableView.Columns[I].Name;
    for J := I to TableView.ColumnCount - 1 do
    begin
      AColumn := TableView.Columns[J];
      if not AColumn.Visible then
        Continue;
      if J <> I then
      begin
        AFindFilterColumnsDescription := AFindFilterColumnsDescription + '; ' + TableView.Columns[J].Caption;
        AFindFilterColumnsValue := AFindFilterColumnsValue + ';' + TableView.Columns[J].Name;
      end;
      AImageComboBoxItem := icbFindFilterColumns.Properties.Items.Add;
      AImageComboBoxItem.Description := AFindFilterColumnsDescription;
      AImageComboBoxItem.Value := AFindFilterColumnsValue;
    end;
  end;
  icbFindFilterColumns.ItemIndex := 0;
end;

procedure TfrmMain.seFindDelayPropertiesChange(Sender: TObject);
begin
  TableView.FindPanel.ApplyInputDelay := seFindDelay.Value;
end;

procedure TfrmMain.actClearFindOnCloseChange(Sender: TObject);
begin
  TableView.FindPanel.ClearFindFilterTextOnClose := actClearFindOnClose.Checked;
end;

procedure TfrmMain.actHighlightFindResultChange(Sender: TObject);
begin
  TableView.FindPanel.HighlightSearchResults := actHighlightSearchResults.Checked;
end;

procedure TfrmMain.actSearchInGroupRowsExecute(Sender: TObject);
begin
  TableView.FindPanel.SearchInGroupRows := actSearchInGroupRows.Checked;
  if TableView.FindPanel.SearchInGroupRows and (TableView.GroupedItemCount = 0) then
  begin
    TableViewCountry.GroupIndex := 0;
    TableViewCountry.Visible := False;
    TableView.DataController.Groups.FullExpand;
    TableView.DataController.FindCriteria.Text := 'tr';
  end;
end;

procedure TfrmMain.actSearchInPreviewExecute(Sender: TObject);
begin
  TableView.Preview.Visible := actSearchInPreview.Checked;
  TableView.FindPanel.SearchInPreview := actSearchInPreview.Checked;
end;

procedure TfrmMain.actShowClearButtonChange(Sender: TObject);
begin
  TableView.FindPanel.ShowClearButton := actShowClearButton.Checked;
end;

procedure TfrmMain.actShowCloseButtonChange(Sender: TObject);
begin
  TableView.FindPanel.ShowCloseButton := actShowCloseButton.Checked;
end;

procedure TfrmMain.actShowFindButtonEChange(Sender: TObject);
begin
  TableView.FindPanel.ShowFindButton := actShowFindButton.Checked;
end;

procedure TfrmMain.actShowPrevAndNextButtonsExecute(Sender: TObject);
begin
  TableView.FindPanel.ShowPreviousButton := actShowPrevAndNextButtons.Checked;
  TableView.FindPanel.ShowNextButton := actShowPrevAndNextButtons.Checked;
end;

procedure TfrmMain.actUseDelayedSearchExecute(Sender: TObject);
begin
  TableView.FindPanel.UseDelayedFind := actUseDelayedSearch.Checked;
end;

procedure TfrmMain.actUseExtendedSyntaxExecute(Sender: TObject);
begin
  TableView.FindPanel.UseExtendedSyntax := actUseExtendedSyntax.Checked;
end;

procedure TfrmMain.cbDisplayModePropertiesChange(Sender: TObject);
begin
  if cbeDisplayMode.ItemIndex = 0 then
    TableView.FindPanel.DisplayMode := fpdmNever
  else
    if cbeDisplayMode.ItemIndex = 1 then
      TableView.FindPanel.DisplayMode := fpdmManual
    else
      TableView.FindPanel.DisplayMode := fpdmAlways;
  actShowCloseButton.Enabled := not (TableView.FindPanel.DisplayMode = fpdmAlways);
end;

procedure TfrmMain.cbeBehaviorPropertiesChange(Sender: TObject);
begin
  TableView.FindPanel.Behavior := TcxDataFindCriteriaBehavior(cbeBehavior.ItemIndex + 1);
  TableView.ScrollbarAnnotations.Active := TableView.FindPanel.Behavior = fcbSearch;
  actShowClearButton.Enabled := cbeBehavior.ItemIndex = 0;
  actShowFindButton.Enabled := actShowClearButton.Enabled;
  actShowPrevAndNextButtons.Enabled := not actShowClearButton.Enabled;
end;

procedure TfrmMain.cbFindPanelPositionPropertiesChange(Sender: TObject);
begin
  if cbeFindPanelPosition.Text = 'Top' then
    TableView.FindPanel.Position := fppTop
  else
    TableView.FindPanel.Position := fppBottom;
end;

procedure TfrmMain.cbLayoutPropertiesEditValueChanged(Sender: TObject);
begin
  if cbLayout.ItemIndex = 1 then
    TableView.FindPanel.Layout := fplCompact
  else
    TableView.FindPanel.Layout := fplDefault;
end;

procedure TfrmMain.cbLocationPropertiesEditValueChanged(Sender: TObject);
begin
  if cbLocation.ItemIndex = 1 then
  begin
    TableView.FindPanel.Location := fplGroupByBox;
    cbLayout.ItemIndex := 1;
  end
  else
  begin
    TableView.FindPanel.Location := fplSeparatePanel;
    cbLayout.ItemIndex := 0;
  end;
  cbLayout.Enabled := TableView.FindPanel.Location = fplSeparatePanel;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  cdsCustomers.Open;
  UpdateFindFilterColumns;
  TableView.Controller.ShowFindPanel;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  cbeBehaviorPropertiesChange(cbeBehavior);
  TableView.DataController.FindCriteria.Text := 'ana +tr';
end;

end.

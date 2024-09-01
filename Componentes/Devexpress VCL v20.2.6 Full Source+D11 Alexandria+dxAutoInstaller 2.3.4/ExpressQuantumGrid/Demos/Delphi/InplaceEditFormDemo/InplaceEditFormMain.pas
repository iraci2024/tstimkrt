unit InplaceEditFormMain;

interface

{$I cxVer.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, ActnList,
  Dialogs, cxStyles, cxCustomData, cxGraphics, cxFilter, cxData, cxDataStorage,
  cxEdit, DB, cxDBData, cxControls, cxGridCustomView, cxGridCustomTableView,
  cxClasses, cxGridLevel, cxGrid, StdCtrls, Menus, cxMemo, cxImage, cxCurrencyEdit,
  cxHyperLinkEdit, cxTextEdit, cxEditRepositoryItems, cxLookAndFeels, cxLookAndFeelPainters,
  dxLayoutContainer, cxGridLayoutView, cxGridDBLayoutView, cxGridCustomLayoutView,
  cxContainer, cxGroupBox, dxLayoutLookAndFeels, ExtCtrls, cxButtons, 
  dxmdaset, BaseForm, cxGridTableView, cxRadioGroup, cxCheckBox, cxGridCardView,
  ComCtrls, ImgList, cxLabel, cxMaskEdit, cxDropDownEdit, cxNavigator, cxGridDBTableView,
  cxSpinEdit, cxGridViewLayoutContainer, cxGridInplaceEditForm, CarsDataForGrid,
  dxLayoutControlAdapters, dxLayoutControl;

type
  TfrmMain = class(TfmBaseForm)
    Grid: TcxGrid;
    EditRepository: TcxEditRepository;
    EditRepositoryImage: TcxEditRepositoryImageItem;
    EditRepositoryMemo: TcxEditRepositoryMemoItem;
    EditRepositoryHyperLink: TcxEditRepositoryHyperLinkItem;
    EditRepositoryPrice: TcxEditRepositoryCurrencyItem;
    EditRepositoryAutomatic: TcxEditRepositoryCheckBoxItem;
    miCustomize: TMenuItem;
    GridLevel1: TcxGridLevel;
    stValues: TcxStyle;
    stItems: TcxStyle;
    stHeader: TcxStyle;
    stRecordCaption: TcxStyle;
    Images: TcxImageList;
    stRecordSelected: TcxStyle;
    TableView: TcxGridDBTableView;
    TableViewRecId: TcxGridDBColumn;
    TableViewID: TcxGridDBColumn;
    TableViewTrademark: TcxGridDBColumn;
    TableViewModel: TcxGridDBColumn;
    TableViewHP: TcxGridDBColumn;
    TableViewCyl: TcxGridDBColumn;
    TableViewTransmissSpeedCount: TcxGridDBColumn;
    TableViewTransmissAutomatic: TcxGridDBColumn;
    TableViewMPG_City: TcxGridDBColumn;
    TableViewMPG_Highway: TcxGridDBColumn;
    TableViewCategory: TcxGridDBColumn;
    TableViewDescription: TcxGridDBColumn;
    TableViewHyperlink: TcxGridDBColumn;
    TableViewPicture: TcxGridDBColumn;
    TableViewPrice: TcxGridDBColumn;
    miEditMode: TMenuItem;
    miInplace: TMenuItem;
    miInplaceEditForm: TMenuItem;
    miInplaceEditFormHideCurrentRow: TMenuItem;
    alAction: TActionList;
    actCustomizeEditForm: TAction;
    btnCustomizeEditForm: TcxButton;
    actInplace: TAction;
    actInplaceEditForm: TAction;
    actInplaceEditFormHCR: TAction;
    rbInplace: TcxRadioButton;
    rbInplaceEditForm: TcxRadioButton;
    rbInplaceEditFormHideCurrentRow: TcxRadioButton;
    miHotTrack: TMenuItem;
    actHotTrack: TAction;
    rbDetachedEditForm: TcxRadioButton;
    actDetachedEditForm: TAction;
    lcOptionsGroup_Root: TdxLayoutGroup;
    lcOptions: TdxLayoutControl;
    lgEditMode: TdxLayoutGroup;
    liInplace: TdxLayoutItem;
    liModalEditForm: TdxLayoutItem;
    dxLayoutGroup1: TdxLayoutGroup;
    liInplaceEditForm: TdxLayoutItem;
    liInplaceEditFromHCR: TdxLayoutItem;
    dxLayoutGroup2: TdxLayoutGroup;
    liCustomizeEditFrom: TdxLayoutItem;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    procedure actCustomizeEditFormExecute(Sender: TObject);
    procedure actEditModeChange(Sender: TObject);
    procedure actHotTrackExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TableViewDetachedEditFormInitialize(Sender: TcxGridTableView; AForm: TForm);
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.actCustomizeEditFormExecute(Sender: TObject);
begin
  inherited;
  TableView.Controller.ShowEditFormCustomizationDialog;
end;

procedure TfrmMain.actEditModeChange(Sender: TObject);
begin
  if Sender = actInplace then
    TableView.OptionsBehavior.EditMode := emInplace
  else
    if Sender = actInplaceEditForm then
      TableView.OptionsBehavior.EditMode := emInplaceEditForm
    else
      if Sender = actInplaceEditFormHCR then
        TableView.OptionsBehavior.EditMode := emInplaceEditFormHideCurrentRow
      else
        TableView.OptionsBehavior.EditMode := emModalEditForm;
  actCustomizeEditForm.Enabled := Sender <> actInplace;
end;

procedure TfrmMain.actHotTrackExecute(Sender: TObject);
begin
  inherited;
  TableView.EditForm.ItemHotTrack := actHotTrack.Checked;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  TableView.Controller.FocusedRowIndex := 0;
  TableView.Controller.FocusedRow.Expand(False);
  TableView.Controller.FocusedRowIndex := 1;
  (TableView.Controller.FocusedRow as TcxGridDataRow).EditFormVisible := True;
end;

procedure TfrmMain.TableViewDetachedEditFormInitialize(Sender: TcxGridTableView; AForm: TForm);
begin
  AForm.BorderStyle := bsDialog;
end;

end.

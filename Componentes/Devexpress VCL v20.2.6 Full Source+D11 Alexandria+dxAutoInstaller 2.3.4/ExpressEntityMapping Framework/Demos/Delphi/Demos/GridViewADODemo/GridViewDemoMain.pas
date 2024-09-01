unit GridViewDemoMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  BaseForm, GridViewDemoData, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, cxStyles, cxCustomData, cxFilter, cxData, cxDataStorage,
  cxEdit, cxCalendar, cxGridCustomPopupMenu, cxGridPopupMenu, cxGridLevel,
  cxGridCustomTableView, cxGridTableView, cxGridServerModeTableView, cxClasses,
  cxGridCustomView, cxGrid, cxImageComboBox, ImgList, cxGridCardView, Menus,
  ComCtrls, StdCtrls, cxNavigator, cxGridBandedTableView, cxGridServerModeBandedTableView, dxDateRanges,
  dxBarBuiltInMenu, cxImageList, cxGridEMFTableView, dxScrollbarAnnotations;

type
  TGridViewDemoMainForm = class(TfmBaseForm)
    cxGridPopupMenu1: TcxGridPopupMenu;
    cxGrid1: TcxGrid;
    cxGrid1EMFTableView1: TcxGridEMFTableView;
    cxGrid1EMFTableView1OID: TcxGridEMFColumn;
    cxGrid1EMFTableView1Subject: TcxGridEMFColumn;
    cxGrid1EMFTableView1From: TcxGridEMFColumn;
    cxGrid1EMFTableView1Sent: TcxGridEMFColumn;
    cxGrid1EMFTableView1Size: TcxGridEMFColumn;
    cxGrid1EMFTableView1HasAttachment: TcxGridEMFColumn;
    cxGrid1EMFTableView1Priority: TcxGridEMFColumn;
    cxGrid1Level1: TcxGridLevel;
    ilImages: TcxImageList;
    mOptions: TMenuItem;
    mCancelOnExit: TMenuItem;
    mDeleting: TMenuItem;
    mDeletingConfirmation: TMenuItem;
    mEditing: TMenuItem;
    mInserting: TMenuItem;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure UpdateOptionsDataView(Sender: TObject);
    procedure cxGrid1ActiveTabChanged(Sender: TcxCustomGrid; ALevel: TcxGridLevel);
  public
    procedure Initialize;
  end;

var
  GridViewDemoMainForm: TGridViewDemoMainForm;

implementation

{$R *.dfm}

{ TGridViewDemoMainForm }

procedure TGridViewDemoMainForm.cxGrid1ActiveTabChanged(Sender: TcxCustomGrid; ALevel: TcxGridLevel);
begin
  if ALevel.GridView is TcxGridEMFTableView then
  begin
    cxGrid1EMFTableView1.DataController.DataSource.Active := True;
  end;
end;

procedure TGridViewDemoMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Application.Terminate;
end;

procedure TGridViewDemoMainForm.Initialize;
begin
  Caption := TGridViewDemoDataDM.GetCaption;
  lbDescription.Caption := TGridViewDemoDataDM.GetDescription;
  cxGrid1EMFTableView1.DataController.DataSource := GridViewDemoDataDM.EMFDataSource;
  GridViewDemoDataDM.EMFDataSource.Active := True;
end;

procedure TGridViewDemoMainForm.UpdateOptionsDataView(Sender: TObject);
begin
  with cxGrid1EMFTableView1.OptionsData do
  begin
    CancelOnExit := GetMenuItemChecked(mCancelOnExit);
    Deleting := GetMenuItemChecked(mDeleting);
    DeletingConfirmation := GetMenuItemChecked(mDeletingConfirmation);
    Editing := GetMenuItemChecked(mEditing);
    Inserting := GetMenuItemChecked(mInserting);
  end;
end;

end.

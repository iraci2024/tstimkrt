unit UContextMenuDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.TMSFNCTypes, FMX.TMSFNCUtils, FMX.TMSFNCGraphics, FMX.TMSFNCGraphicsTypes,
  FMX.TMSFNCCustomControl, FMX.TMSFNCWebBrowser, FMX.TMSFNCEdgeWebBrowser,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Memo.Types, FMX.Edit,
  FMX.ScrollBox, FMX.Memo, System.Rtti, FMX.Grid.Style, FMX.Grid, FMX.Menus, Generics.Collections;

type
  TContextMenuForm = class(TForm)
    TMSFNCEdgeWebBrowser1: TTMSFNCEdgeWebBrowser;
    SelectedMemo: TMemo;
    NavigateBtn: TButton;
    URIEdit: TEdit;
    DefaultRadio: TRadioButton;
    NativeRadio: TRadioButton;
    CustomRadio: TRadioButton;
    Label1: TLabel;
    ContextMenu: TPopupMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    NormalRadio: TRadioButton;
    procedure FormCreate(Sender: TObject);
    procedure NavigateBtnClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure DefaultRadioChange(Sender: TObject);
    procedure NativeRadioChange(Sender: TObject);
    procedure CustomRadioChange(Sender: TObject);
    procedure TMSFNCEdgeWebBrowser1GetContextMenu(Sender: TObject; ATarget: TTMSFNCWebBrowserTargetItem;
      AContextMenu: TObjectList<FMX.TMSFNCWebBrowser.TTMSFNCWebBrowserContextMenuItem>);
    procedure TMSFNCEdgeWebBrowser1CustomContextMenuItemSelected(Sender: TObject;
      ASelectedItem: TTMSFNCWebBrowserCustomContextMenuItem);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure TMSFNCEdgeWebBrowser1Initialized(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ContextMenuForm: TContextMenuForm;

implementation

{$R *.fmx}

procedure TContextMenuForm.MenuItem1Click(Sender: TObject);
begin
  TTMSFNCUtils.OpenURL('https://www.youtube.com/@tmssoftwareTV')
end;

procedure TContextMenuForm.MenuItem4Click(Sender: TObject);
begin
  MenuItem4.IsChecked := True;
  MenuItem5.IsChecked := False;
end;

procedure TContextMenuForm.MenuItem5Click(Sender: TObject);
begin
  MenuItem4.IsChecked := False;
  MenuItem5.IsChecked := True;
end;

procedure TContextMenuForm.NativeRadioChange(Sender: TObject);
begin
  if NativeRadio.IsChecked then
  begin
    TMSFNCEdgeWebBrowser1.PopupMenu := ContextMenu;
    TMSFNCEdgeWebBrowser1.Settings.UsePopupMenuAsContextMenu := True;
  end;
end;

procedure TContextMenuForm.NavigateBtnClick(Sender: TObject);
begin
  TMSFNCEdgeWebBrowser1.Navigate(URIEdit.Text);
end;

procedure TContextMenuForm.TMSFNCEdgeWebBrowser1CustomContextMenuItemSelected(Sender: TObject; ASelectedItem: TTMSFNCWebBrowserCustomContextMenuItem);
var
  s: string;
begin
  if (ASelectedItem.DataInteger > 0) or Assigned(TMSFNCEdgeWebBrowser1.PopupMenu) then
  begin
    s := ASelectedItem.Name;
    if Assigned(ASelectedItem.ParentItem) then
      s := s + ' (' + ASelectedItem.ParentItem.AsCustom.Name + ')';
    SelectedMemo.Lines.Add(s);

    if ASelectedItem.AsCustom.Name = 'Go to TMS' then
    begin
      TMSFNCEdgeWebBrowser1.Navigate('https://www.tmssoftware.com');
    end;
  end;
end;

procedure TContextMenuForm.TMSFNCEdgeWebBrowser1GetContextMenu(Sender: TObject; ATarget: TTMSFNCWebBrowserTargetItem;
  AContextMenu: TObjectList<FMX.TMSFNCWebBrowser.TTMSFNCWebBrowserContextMenuItem>);
var
  I: Integer;
  mi: TTMSFNCWebBrowserCustomContextMenuItem;
begin
  if ATarget.SelectionText <> '' then
    SelectedMemo.Lines.Add('Selected Text: ' + ATarget.SelectionText);

  if CustomRadio.IsChecked then
  begin
    I := 0;

    while I < AContextMenu.Count do
    begin
      if AContextMenu[I].AsSystem.Name.ToLower <> 'reload' then
      begin
        AContextMenu.Delete(I);
      end
      else
        Inc(I);
    end;

    mi := TTMSFNCWebBrowserCustomContextMenuItem.Create;
    mi.Name := 'Go to TMS';
    mi.DataInteger := 1;

    AContextMenu.Add(mi);
  end;
end;

procedure TContextMenuForm.TMSFNCEdgeWebBrowser1Initialized(Sender: TObject);
begin
  if Pos('http', TMSFNCEdgeWebBrowser1.URL) > 0 then
    URIEdit.Text := TMSFNCEdgeWebBrowser1.URL;
end;

procedure TContextMenuForm.CustomRadioChange(Sender: TObject);
begin
  if CustomRadio.IsChecked then
  begin
    TMSFNCEdgeWebBrowser1.PopupMenu := nil;
  end;
end;

procedure TContextMenuForm.DefaultRadioChange(Sender: TObject);
begin
  if DefaultRadio.IsChecked then
  begin
    TMSFNCEdgeWebBrowser1.PopupMenu := ContextMenu;
    TMSFNCEdgeWebBrowser1.Settings.UsePopupMenuAsContextMenu := False;
  end;
end;

procedure TContextMenuForm.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := True;
end;

end.

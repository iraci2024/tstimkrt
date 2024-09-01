{$I ..\Base\SBDemo.inc}

unit SSHServerMain;

interface

uses
  SysUtils, Classes,
{$IFDEF CLR}
  System.ComponentModel,
{$ENDIF}
  Windows, Messages, Graphics, Controls, Forms,
  Dialogs, Menus, ImgList, StdCtrls, ComCtrls, Buttons, ExtCtrls,
  ShellApi, SecureBridgeAbout, DemoForm, DemoBase;

const
  WM_NOTIFYTRAYICON = WM_USER + 1;

type
  TSSHServerForm = class(TDemoForm)
    PopupMenu: TPopupMenu;
    miSettings: TMenuItem;
    miClose: TMenuItem;
    procedure lbAboutClick(Sender: TObject); override;
    procedure FormCreate(Sender: TObject); override;
    procedure FormDestroy(Sender: TObject); override;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure miSettingsClick(Sender: TObject);
    procedure miCloseClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FIconData: TNotifyIconData;
    procedure HideForm(Sender: TObject);
    procedure WMNOTIFYTRAYICON(var msg: TMessage); message WM_NOTIFYTRAYICON;
  protected
    //Product customization
    function ApplicationTitle: string; override;
    procedure RegisterDemos; override;
  public
    function ProductColor: TColor; override;
  end;

var
  fmSSHServerForm: TSSHServerForm;

implementation

uses
  SSHServerFrame;

{$IFDEF CLR}
{$R *.nfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

function TSSHServerForm.ProductColor: TColor;
begin
  Result := $0000BBFF;
end;

function TSSHServerForm.ApplicationTitle: string;
begin
  Result := 'SecureBridge demos';
end;

procedure TSSHServerForm.RegisterDemos;
begin
  Demos.RegisterDemo('SSHServerFrame', 'SSH Server', 'SSHServerFrame', '', TSSHServerFrame, 3);
end;

procedure TSSHServerForm.lbAboutClick(Sender: TObject);
begin
  inherited;
  SecureBridgeAboutForm.ShowModal;
end;

procedure TSSHServerForm.FormCreate(Sender: TObject);
begin
  inherited;

{$IFNDEF CLR}
  FillChar(FIconData, SizeOf(FIconData), 0);
{$ENDIF}

  with FIconData do begin
    cbSize := {$IFNDEF CLR}System.{$ENDIF}SizeOf(TNotifyIconData);
    Wnd := Self.Handle;
    uID := 1;
    uFlags := NIF_ICON or NIF_MESSAGE or NIF_TIP;
    uCallBackMessage := WM_NOTIFYTRAYICON;
    hIcon := Application.Icon.Handle;
  {$IFDEF CLR}
    FIconData.szTip := Application.Title;
  {$ELSE}
    StrLCopy(FIconData.szTip, PChar(Application.Title), System.SizeOf(FIconData.szTip)-1);
    FIconData.szTip[System.SizeOf(FIconData.szTip) div System.SizeOf(Char) - 1] := #0;
  {$ENDIF}
  end;
  Shell_NotifyIcon(NIM_ADD, {$IFNDEF CLR}Addr{$ENDIF}(FIconData));

  Application.OnMinimize := HideForm;
end;

procedure TSSHServerForm.FormDestroy(Sender: TObject);
begin
  inherited;

  Shell_NotifyIcon(NIM_DELETE, {$IFNDEF CLR}Addr{$ENDIF}(FIconData));
end;

procedure TSSHServerForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caNone;
  HideForm(Sender);

  inherited;
end;

procedure TSSHServerForm.HideForm(Sender: TObject);
begin
  Application.ShowMainForm := False;
  Visible := False;
  ShowWindow(Application.Handle, SW_HIDE);
end;

procedure TSSHServerForm.WMNOTIFYTRAYICON(var msg: TMessage);
var
  CursorPos: TPoint;
begin
  case integer(msg.LParam) of
    WM_RBUTTONUP: begin
      GetCursorPos(CursorPos);
      SetForegroundWindow(Handle);
      PopupMenu.Popup(CursorPos.X, CursorPos.Y);
    end;
    WM_LBUTTONDBLCLK: begin
      if Application.ShowMainForm then
        HideForm(Self)
      else
        miSettingsClick(Self);
    end;
  end;
end;

procedure TSSHServerForm.miSettingsClick(Sender: TObject);
begin
  Application.ShowMainForm := True;
  Application.Restore;
  if WindowState = wsMinimized then
    WindowState := wsNormal;
  Visible := True;
  SetForegroundWindow(Application.Handle);
end;

procedure TSSHServerForm.miCloseClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TSSHServerForm.FormShow(Sender: TObject);
begin
  Demos.SelectDemo(1).Frame.Activate;
end;

end.

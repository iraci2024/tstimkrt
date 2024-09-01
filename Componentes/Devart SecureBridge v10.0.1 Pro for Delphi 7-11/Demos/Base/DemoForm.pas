{$I SBDemo.inc}

unit DemoForm;

interface

uses
{$IFNDEF WIN32}
  Types,
{$ENDIF}
  SysUtils, Classes,
  Windows, Forms, Messages, Controls, StdCtrls,
  Graphics, ImgList,
  ComCtrls, Dialogs, ExtCtrls, Tabs, Menus, DBCtrls, Buttons,
{$IFNDEF VER130}
  Variants,
{$ENDIF}
{$IFDEF CLR}
  System.ComponentModel,
{$ENDIF}
  DemoBase, DemoFrame, ScBridge, ScSSHUtils, ScUtils
  {$IFDEF XPMAN}, UxTheme{$ENDIF}
  {$IFDEF USE_SYNEDIT}, SynMemo, SynEdit, SynEditHighlighter, SynHighlighterPas{$ENDIF}
  ;

type
  TDemoForm = class(TForm)
    MainPanel: TPanel;
    shTopShape: TShape;
    pnTopLabel: TPanel;
    lbAbout: TLabel;
    pnTopPanel: TPanel;
    pnSource: TPanel;
    Panel6: TPanel;
    Panel3: TPanel;
    sbOpenDemoDir: TSpeedButton;
    pnDemo: TPanel;
    sbDemo: TSpeedButton;
    pnShowSource: TPanel;
    lbTitle: TLabel;
    Panel1: TPanel;
    btRandom: TSpeedButton;

    procedure FormCreate(Sender: TObject); virtual;
    procedure lbAboutClick(Sender: TObject); virtual;
    procedure lbAboutMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure lbTitleMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure sbOpenDemoDirClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject); virtual;
    procedure sbDemoClick(Sender: TObject);
    procedure btRandomClick(Sender: TObject);
    procedure FormResize(Sender: TObject);

  protected
    Demos: TDemos;
    FRandomized: boolean;

    //Product customization
    function ApplicationTitle: string; virtual; abstract; //This function should return DAC product specific title
    procedure RegisterDemos; virtual; abstract;          //This procedure should regiter DAC product specific demos
    //Demo selection
    procedure InitializeDemoFrame(Frame: TDemoFrame; DemoType: TDemoType); virtual;
    procedure UpdateDemo;
  public
{$IFDEF USE_SYNEDIT}
    SourceBrowser: TSynMemo;
{$ELSE}
    SourceBrowser: TMemo;
{$ENDIF}
    function ProductColor: TColor; virtual; abstract;     //This function should return DAC product specific color
    //XP manifest
    function GetIsXPMan: boolean;
  {$IFDEF XPMAN}
    procedure ReplaceFlatStyle(Control: TWinControl; Flat: boolean);
  {$ENDIF}

    procedure GenerateClientKey(FileStorage: TScFileStorage; const KeyName: string);
    procedure ImportServerKey(FileStorage: TScFileStorage; var KeyName: string);
    procedure DoServerKeyValidate(FileStorage: TScFileStorage;
      const HostKeyName: string; NewServerKey: TScKey; var Accept: Boolean);

    procedure Randomize;
    property Randomized: boolean read FRandomized;
  end;

implementation

{$IFDEF CLR}
{$R *.nfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

{$IFDEF XPMAN}
  {$R WindowsXP.res}
{$ENDIF}

uses
  RandomForm;

procedure TDemoForm.FormCreate(Sender: TObject);
begin
  Demos := TDemos.Create;
  RegisterDemos;
{$IFDEF XPMAN}
  if GetIsXPMan then begin
    ReplaceFlatStyle(Self, False);
    pnTopLabel.Color := ProductColor;
  end;
{$ENDIF}

{$IFDEF APP}
  pnTopPanel.Visible := False;
  shTopShape.Visible := False;
{$ENDIF}

{$IFDEF USE_SYNEDIT}
  SourceBrowser := TSynMemo.Create(pnSource);

  SourceBrowser.Highlighter := TSynPasSyn.Create(SourceBrowser);
  SourceBrowser.Options := [eoAltSetsColumnMode, eoAutoIndent, eoAutoSizeMaxScrollWidth, eoDisableScrollArrows, eoDragDropEditing, eoDropFiles, eoEnhanceEndKey, eoGroupUndo, eoHideShowScrollbars, eoKeepCaretX, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabIndent, eoTabsToSpaces];
  with SourceBrowser.Gutter do begin
    Visible := True;
    AutoSize := True;
    DigitCount := 3;
    LeftOffset := 0;
    RightOffset := 0;
    ShowLineNumbers := True;
    Width := 1;
 end;
{$ELSE}
  SourceBrowser := TMemo.Create(pnSource);
  SourceBrowser.ScrollBars := ssVertical;
  SourceBrowser.Font.Name := 'Courier New';
  SourceBrowser.Font.Size := 10;
{$ENDIF}
  with SourceBrowser do begin
    Parent := pnSource;
    Align := alClient;
    ReadOnly := True;
  end;
  Resize;
  UpdateDemo;
end;

procedure TDemoForm.FormDestroy(Sender: TObject);
begin
  Demos.Free;
end;

procedure TDemoForm.FormResize(Sender: TObject);
begin
  lbAbout.Left := lbAbout.Parent.ClientWidth - 100;
  pnShowSource.Left := lbAbout.Left + lbAbout.Width - (pnShowSource.Width);
end;

//Sizing constraints
function TDemoForm.GetIsXPMan: boolean;
begin
  Result := {$IFDEF XPMAN}UseThemes; {$ELSE} False;{$ENDIF}
end;

{$IFDEF XPMAN}
procedure TDemoForm.ReplaceFlatStyle(Control: TWinControl; Flat: boolean);
var
  i: integer;
begin
  for i := 0 to Control.ControlCount - 1 do
    if Control.Controls[i] is TSpeedButton then
      TSpeedButton(Control.Controls[i]).Flat := Flat
    else
    if Control.Controls[i] is TDBNavigator then
      TDBNavigator(Control.Controls[i]).Flat := Flat
    else
      if Control.Controls[i] is TWinControl then begin
        if Control.Controls[i] is TPanel then begin
          TPanel(Control.Controls[i]).ParentBackground := False;
          if Control.Controls[i].Tag <> 0 then
            TPanel(Control.Controls[i]).Color := ProductColor
          else
            TPanel(Control.Controls[i]).Color := clBtnFace;
        end;
        ReplaceFlatStyle(TWinControl(Control.Controls[i]), Flat);
      end;

end;
{$ENDIF}

//Demo Change
procedure TDemoForm.InitializeDemoFrame(Frame: TDemoFrame; DemoType: TDemoType);
begin
  Frame.Parent := pnDemo;
  Frame.Initialize;
{$IFDEF XPMAN}
  if GetIsXPMan then
    ReplaceFlatStyle(Frame, False);
{$ENDIF}
end;

procedure TDemoForm.UpdateDemo;
begin
//  for i := 1 to StatusBar.Panels.Count - 1 do
//    StatusBar.Panels[i].Text := '';

  with Demos.SelectDemo(1) do begin
    InitializeDemoFrame(Frame, DemoType);
    LoadDemoCode(SourceBrowser.Lines);            //Load demo sources
    if DemoType = dtCategory then begin
      pnSource.Visible := False;
      pnDemo.Visible := True;
      sbDemo.Enabled := False;
    end
    else begin
      sbDemo.Enabled := True;
      if sbDemo.Down then begin
        pnSource.Visible := True;
        pnDemo.Visible := False;
      end;
    end;

    if Hint <> '' then
      Self.Caption := ApplicationTitle + ' - ' + Hint
    else
      Self.Caption := ApplicationTitle;
    Application.Title := Self.Caption;
  end;
//  StatusBar.Repaint;
end;

procedure TDemoForm.sbOpenDemoDirClick(Sender: TObject);
begin
  Demos.SelectedDemo.OpenDemoFolder;
end;

procedure TDemoForm.lbAboutClick(Sender: TObject);
begin
  lbAbout.Font.Color := $FFFFFF;
  lbAbout.Cursor := crDefault;
end;

//About highlite
procedure TDemoForm.lbAboutMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  lbAbout.Font.Color := TColor($FF00001A); //clHotLight
  lbAbout.Cursor := crHandPoint;
end;

procedure TDemoForm.lbTitleMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  lbAbout.Font.Color := $FFFFFF;
  lbAbout.Cursor := crDefault;
end;

procedure TDemoForm.sbDemoClick(Sender: TObject);
begin
  with sbDemo do
    if Down then begin
      pnSource.Visible := True;
      pnDemo.Visible := False;
      Caption := 'Show demo'
    end
    else begin
      pnSource.Visible := False;
      pnDemo.Visible := True;
      Caption := 'Show source'
    end;
end;

procedure TDemoForm.Randomize;
begin
  fmRandom := TfmRandom.Create(Self);
  try
    if fmRandom.ShowModal = mrOk then begin
      Random.Randomize(fmRandom.Data);
      FRandomized := True;
    end;
  finally
    fmRandom.Free;
  end;
end;

procedure TDemoForm.btRandomClick(Sender: TObject);
begin
  Self.Randomize;
end;

procedure TDemoForm.GenerateClientKey(FileStorage: TScFileStorage; const KeyName: string);
var
  msg: string;
  OldCursor: TCursor;
  Key: TScKey;
  Algorithm: TScAsymmetricAlgorithm;
  BitCount: integer;
begin
  OldCursor := Screen.Cursor;
  try
    Screen.Cursor := crHourGlass;

    Key := FileStorage.Keys.FindKey(KeyName);

    if Key = nil then begin
      Key := TScKey.Create(FileStorage.Keys);
      Key.KeyName := KeyName;
      Algorithm := aaRSA;
      BitCount := 2048;
    end
    else begin
      Key.Ready := True;
      Algorithm := Key.Algorithm;
      BitCount := Key.BitCount;
    end;

    try
      Key.Generate(Algorithm, BitCount);
      Key.ExportTo(Key.KeyName + '.pub', True, '');

      msg := 'The client key file has been generated in the current application directory.'#13#10 +
             'To connect with authentication by key, you should pass the "' + Key.KeyName +
             '.pub" file to the server and set the server to work with this file.';
      MessageDlg(msg, mtInformation, [mbOk], 0);
    except
      on E: Exception do
        MessageDlg('Cannot generate key: ' + E.Message, mtWarning, [mbOk], 0);
    end;
  finally
    Screen.Cursor := OldCursor;
  end;
end;

procedure TDemoForm.ImportServerKey(FileStorage: TScFileStorage; var KeyName: string);
var
  OpenDialog: TOpenDialog;
  msg, fp, Comment: string;
  Key, StorageKey: TScKey;
begin
  OpenDialog := TOpenDialog.Create(nil);
  try
    OpenDialog.Options := OpenDialog.Options + [ofPathMustExist, ofFileMustExist];
    OpenDialog.Title := 'Import key';
    OpenDialog.Filter := 'All formats |*.ssl;*.pem;*.ietf;*.pub;*.ietfpub|' +
      'OpenSSL format (*.ssl)|*.ssl|PKCS8 format (*.pem)|*.pem|IETF format (*.ietf)|*.ietf|' +
      'Public key (*.pub)|*.pub|Public IETF key (*.ietfpub)|*.ietfpub|All files (*.*)|*.*';
    OpenDialog.InitialDir := '.';

    if OpenDialog.Execute then begin
      Key := TScKey.Create(nil);
      try
        Key.ImportFrom(OpenDialog.Filename, '', Comment);
        Key.GetFingerPrint(haMD5, fp);
        msg := 'Fingerprint for the key imported from server: ' + fp +'.'#13#10 +
               'Key length: ' + IntToStr(Key.BitCount) +' bits.'#13#10 +
               'Do you want to save the server key?';

        if MessageDlg(msg, mtConfirmation, [mbOk, mbCancel], 0) = mrOk then begin
          if Trim(KeyName) = '' then
            KeyName := ChangeFileExt(ExtractFileName(OpenDialog.Filename), '');

          StorageKey := FileStorage.Keys.FindKey(KeyName);
          if StorageKey <> nil then
            StorageKey.Assign(Key)
          else begin
            Key.KeyName := KeyName;
            FileStorage.Keys.Add(Key);
            Key := nil;
          end;
        end;
      finally
        Key.Free;
      end;
    end;
  finally
    OpenDialog.Free;
  end;
end;

procedure TDemoForm.DoServerKeyValidate(FileStorage: TScFileStorage;
  const HostKeyName: string; NewServerKey: TScKey; var Accept: Boolean);
var
  Key: TScKey;
  fp, msg: string;
begin
  Key := FileStorage.Keys.FindKey(HostKeyName);
  if (Key = nil) or not Key.Ready then begin
    NewServerKey.GetFingerPrint(haMD5, fp);
    msg := 'The authenticity of server can not be verified.'#13#10 +
           'Fingerprint for the key received from server: ' + fp + '.'#13#10 +
           'Key length: ' + IntToStr(NewServerKey.BitCount) + ' bits.'#13#10 +
           'Are you sure you want to continue connecting?';

    if MessageDlg(msg, mtConfirmation, [mbOk, mbCancel], 0) = mrOk then begin
      Key := TScKey.Create(nil);
      try
        Key.Assign(NewServerKey);
        Key.KeyName := HostKeyName;
        FileStorage.Keys.Add(Key);
      except
        Key.Free;
        raise;
      end;

      Accept := True;
    end;
  end;
end;

end.

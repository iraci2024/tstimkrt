{*************************************************************************}
{ TMS TAdvRichEditor                                                      }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2014 - 2022                                       }
{           Email : info@tmssoftware.com                                  }
{           Web : http://www.tmssoftware.com                              }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit AdvRichEditorIO;

{$I TMSDEFS.INC}

interface

uses
  Classes, SysUtils, AdvRichEditor, AdvRichEditorBase, StrUtils
  {$IFDEF FMXLIB}
  , FMX.Forms
  {$ENDIF}
  {$IFNDEF FMXLIB}
  , Forms
  {$ENDIF}
  {$IFDEF FNCLIB}
  , AdvCustomComponent
  {$ENDIF}
  {$IFNDEF LCLLIB}
  , AdvTypes
  {$ENDIF}
  ;

type
  TAdvRichEditorBaseEx = class(TAdvRichEditorBase);

  {$IFDEF FNCLIB}
  TNVComponent = class(TTMSFNCCustomComponent)
  protected
    function GetInstance: NativeUInt; override;
  end;
  {$ENDIF}

  {$IFNDEF FNCLIB}
  TNVComponent = class(TComponent);
  {$ENDIF}

  TAdvRichEditorIO = class(TNVComponent)
  private
    FRichEditor: TAdvRichEditorBase;
  protected
    procedure SetRichEditor(AValue: TAdvRichEditorBase); virtual;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    property RichEditor: TAdvRichEditorBase read FRichEditor write SetRichEditor;
    procedure Save(AStream: TStream); overload; virtual; abstract;
  end;

  {$IFDEF FNCLIB}
  {$IFNDEF LCLLIB}
  [ComponentPlatformsAttribute(TMSPlatforms)]
  {$ENDIF}
  {$ENDIF}
  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvRichEditorHTMLIO = class(TAdvRichEditorIO)
  private
    FOnImageNotFound: TImageNotFoundEvent;
    FSpaceAsNbSp: boolean;
    FPlainHTML: boolean;
    FBodyClass: string;
    FHead: TStrings;
    FTarget: string;
    procedure SetHead(const Value: TStrings);
  protected
    procedure ImageNotFound(Sender: TObject; ImageName: string; var ReplaceImage: string);
  public
    FInlineImages: boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Save(const FileName: string); overload; virtual;
    procedure Save(const FileName: string; ImgPath: string); overload; virtual;
    {$IFNDEF LCLLIB}
    procedure Save(const FileName: string; AEncoding: TEncoding); overload; virtual;
    procedure Save(const FileName: string; ImgPath: string; AEncoding: TEncoding); overload; virtual;
    {$ENDIF}
    procedure Save(AStream: TStream); override;
    procedure Load(const FileName: string {$IFNDEF LCLLIB}; Encoding: TEncoding = nil {$ENDIF}); overload;
    procedure Load(AStream: TStream {$IFNDEF LCLLIB}; Encoding: TEncoding = nil{$ENDIF}); overload;
    procedure LoadHTML(HtmlValue: string);
    procedure InsertHTML({%H-}HtmlValue: string);
  published
    property RichEditor;
    property Head: TStrings read FHead write SetHead;
    property BodyClass: string read FBodyClass write FBodyClass;
    property HyperlinkTarget: string read FTarget write FTarget;
    property InlineImages: boolean read FInlineImages write FInlineImages default False;
    property PlainHTML: boolean read FPlainHTML write FPlainHTML default False;
    property SpaceAsNbSp: boolean read FSpaceAsNbSp write FSpaceAsNbSp default True;
    property OnImageNotFound: TImageNotFoundEvent read FOnImageNotFound write FOnImageNotFound;
  end;

  {$IFDEF FNCLIB}
  {$IFNDEF LCLLIB}
  [ComponentPlatformsAttribute(TMSPlatforms)]
  {$ENDIF}
  {$ENDIF}
  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvRichEditorRTFIO = class(TAdvRichEditorIO)
  public
    procedure Load(const FileName: string); overload;
    procedure Load(AStream: TStream); overload;
    procedure Save(const FileName: string); overload; virtual;
    procedure Save(AStream: TStream); override;
  published
    property RichEditor;
  end;

implementation

{$IFDEF FNCLIB}
{$R TMSFNCRichEditorIO.res}
{$ENDIF}

{$IFNDEF LCLLIB}
uses
  Character
  ;
{$ENDIF}

type
  TAdvRichEditorEx = class(TAdvRichEditor);


{$IFDEF FNCLIB}
function TNVComponent.GetInstance: NativeUInt;
begin
  Result := HInstance;
end;
{$ENDIF}


{ TAdvRichEditorHTMLIO }

{$IFNDEF LCLLIB}
procedure TAdvRichEditorHTMLIO.Save(const FileName: string; ImgPath: string; AEncoding: TEncoding);
var
  sl: TStringList;
begin
  if not Assigned(FRichEditor) then
    Exit;

  sl := TStringList.Create;

  try
    if ImgPath = '' then
      ImgPath := ExtractFilePath(FileName);

    if InlineImages then
      TAdvRichEditorBaseEx(FRichEditor).HTMLImages := igInline;

    sl.Text := TAdvRichEditorBaseEx(FRichEditor).GetContentAsHTML(False, ImgPath, FPlainHTML, BodyClass, Head.Text, FTarget);

    if not FSpaceAsNbSp then
      sl.Text := ReplaceStr(sl.Text, '&nbsp;', ' ');

    {$IFNDEF LCLLIB}
    {$IF COMPILERVERSION > 24}
    sl.SaveToFile(FileName, AEncoding);
    {$ELSE}
    sl.SaveToFile(FileName);
    {$IFEND}
    {$ENDIF}
    {$IFDEF LCLLIB}
    sl.SaveToFile(FileName);
    {$ENDIF}

  finally
    sl.Free;
  end;
end;

procedure TAdvRichEditorHTMLIO.Save(const FileName: string; AEncoding: TEncoding);
begin
  Save(FileName, '', AEncoding);
end;
{$ENDIF}

procedure TAdvRichEditorHTMLIO.Save(const FileName: string; ImgPath: string);
var
  sl: TStringList;
begin
  if not Assigned(FRichEditor) then
    Exit;

  sl := TStringList.Create;

  try
    if ImgPath = '' then
      ImgPath := ExtractFilePath(FileName);

    if InlineImages then
      TAdvRichEditorBaseEx(FRichEditor).HTMLImages := igInline;

    sl.Text := TAdvRichEditorBaseEx(FRichEditor).GetContentAsHTML(False, ImgPath, FPlainHTML, BodyClass, Head.Text, FTarget);

    if not FSpaceAsNbSp then
      sl.Text := ReplaceStr(sl.Text, '&nbsp;', ' ');

    {$IFNDEF LCLLIB}
    {$IF COMPILERVERSION > 24}
    sl.SaveToFile(FileName, TEncoding.UTF8);
    {$ELSE}
    sl.SaveToFile(FileName);
    {$IFEND}
    {$ENDIF}
    {$IFDEF LCLLIB}
    sl.SaveToFile(FileName);
    {$ENDIF}

  finally
    sl.Free;
  end;
end;

procedure TAdvRichEditorHTMLIO.Save(const FileName: string);
begin
  Save(FileName, '');
end;

constructor TAdvRichEditorHTMLIO.Create(AOwner: TComponent);
begin
  inherited;
  FSpaceAsNbSp := True;
  FHead := TStringList.Create;
end;

destructor TAdvRichEditorHTMLIO.Destroy;
begin
  FHead.Free;
  inherited;
end;

procedure TAdvRichEditorHTMLIO.ImageNotFound(Sender: TObject;
  ImageName: string; var ReplaceImage: string);
begin
  if Assigned(FOnImageNotFound) then
    FOnImageNotFound(Self, ImageName, ReplaceImage);
end;

procedure TAdvRichEditorHTMLIO.InsertHTML(HtmlValue: string);
var
  tempRE: TAdvRichEditorEx;
begin
  if Assigned(RichEditor) then
  begin
    tempRE := TAdvRichEditorEx.Create(Self);
    tempRE.Width := 0;
    tempRE.Parent := Application.MainForm;
    tempRE.ParseHTML(HTMLValue);

    tempRE.SelectAll;
    tempRE.CopyToClipboard;

    RichEditor.PasteFromClipboard;
    tempRE.Free;
  end;
end;

procedure TAdvRichEditorHTMLIO.Load(const FileName: string{$IFNDEF LCLLIB}; Encoding: TEncoding = nil{$ENDIF});
var
  sl: TStringList;
begin
  if not Assigned(RichEditor) then
    raise Exception.Create('No rich editor assigned');

  sl := TStringList.Create;
  try
    sl.LoadFromFile(FileName{$IFNDEF LCLLIB}, Encoding {$ENDIF});
    LoadHTML(sl.Text);
  finally
    sl.Free;
  end;
end;

procedure TAdvRichEditorHTMLIO.Load(AStream: TStream{$IFNDEF LCLLIB}; Encoding: TEncoding = nil{$ENDIF});
var
  ss: TStringStream;
  hasBOM: boolean;
  s: string;
  {$IFNDEF LCLLIB}
  w: word;
  {$ENDIF}
begin
  {$IFNDEF LCLLIB}
  if Assigned(Encoding) then
    ss := TStringStream.Create('', Encoding)
  else
  {$ENDIF}
    ss := TStringStream.Create('');
  try
    hasBOM := false;

    {$IFNDEF LCLLIB}
    if AStream.Size >= 2  then
    begin
      AStream.Read(w,2);
      hasBom := w = $FEFF;
    end;

    ss.LoadFromStream(AStream);
    {$ENDIF}

    {$IFDEF LCLLIB}
    ss.CopyFrom(AStream, AStream.Size);
    {$ENDIF}

    s := ss.DataString;

    if hasBOM then
      s := Copy(s,2,Length(s));

    LoadHTML(s);
  finally
    ss.Free;
  end;
end;

procedure TAdvRichEditorHTMLIO.LoadHTML(HtmlValue: string);
begin
  if (HtmlValue = '') then
    Exit;

  if not Assigned(RichEditor) then
    raise Exception.Create('No rich editor assigned');

  RichEditor.Clear;

  TAdvRichEditorEx(RichEditor).OnImageNotFound := ImageNotFound;

  TAdvRichEditorEx(RichEditor).ParseHTML(HTMLValue);
  TAdvRichEditorEx(RichEditor).Resize;
end;

procedure TAdvRichEditorHTMLIO.Save(AStream: TStream);
var
  sl: TStringList;
begin
  if not Assigned(FRichEditor) then
    Exit;

  sl := TStringList.Create;

  try
    if InlineImages then
      TAdvRichEditorBaseEx(FRichEditor).HTMLImages := igInline;

    sl.Text := TAdvRichEditorBaseEx(FRichEditor).GetContentAsHTML(False, '', FPlainHTML, BodyClass, Head.Text, FTarget);

    if not FSpaceAsNbSp then
      sl.Text := ReplaceStr(sl.Text, '&nbsp;', ' ');

    sl.SaveToStream(AStream);
  finally
    sl.Free;
  end;
end;

procedure TAdvRichEditorHTMLIO.SetHead(const Value: TStrings);
begin
  FHead.Assign(Value);
end;

{ TAdvRichEditorRTFIO }

procedure TAdvRichEditorRTFIO.Save(const FileName: string);
begin
  if not Assigned(FRichEditor) then
    Exit;

  TAdvRichEditorBaseEx(FRichEditor).GetContentAsRTF(False);

  TAdvRichEditorBaseEx(FRichEditor).RTFEngine.SaveToFile(FileName);
end;

procedure TAdvRichEditorRTFIO.Load(AStream: TStream);
var
  ts: TStringStream;
begin
  if not Assigned(FRichEditor) then
    raise Exception.Create('No richeditor assigned');

  if not Assigned(AStream) then
    raise Exception.Create('Stream not assigned');

  AStream.Position := 0;

  ts := TStringStream.Create;
  try
    ts.CopyFrom(AStream, AStream.Size);
    RichEditor.InsertAsRTF(ts.DataString);
  finally
    ts.Free;
  end;
end;

procedure TAdvRichEditorRTFIO.Load(const FileName: string);
var
  ts: TStringStream;
  s,su: string;
  i, lp: integer;
  sl: TStringList;
begin
  if not Assigned(FRichEditor) then
    raise Exception.Create('No richeditor assigned');

  sl := TStringList.Create;
  {$IFNDEF LCLLIB}
  {$IF COMPILERVERSION > 24}
  sl.LoadFromFile(FileName, TEncoding.ANSI);
  {$ELSE}
  sl.LoadFromFile(FileName);
  {$IFEND}
  {$ENDIF}
  {$IFDEF LCLLIB}
  sl.LoadFromFile(FileName);
  {$ENDIF}

  ts := TStringStream.Create;

  for i := 0 to sl.Count - 1  do
  begin
    s := sl.Strings[i];
    if (Length(s) > 0) and (CharInStr(s,1) <> '\') then
      ts.WriteString(#13);

    ts.WriteString(s);

    lp := Length(s);
    su := '';
    if lp >= 1 then
    begin
      su := Copy(s, lp ,1);
    end;

    if su = '\' then
      ts.WriteString('line');
  end;

  s := ts.DataString;

  ts.Free;
  sl.Free;

  RichEditor.InsertAsRTF(s);
end;

procedure TAdvRichEditorRTFIO.Save(AStream: TStream);
begin
  TAdvRichEditorBaseEx(FRichEditor).GetContentAsRTF(False);
  TAdvRichEditorBaseEx(FRichEditor).RTFEngine.SaveToStream(AStream);
end;

{ TAdvRichEditorIO }

constructor TAdvRichEditorIO.Create(AOwner: TComponent);
var
  i: integer;

begin
  inherited;

  if Assigned(AOwner) and (AOwner is TCustomForm) then
  begin
    for i := 0 to AOwner.ComponentCount - 1 do
      if (AOwner.Components[i] is TAdvRichEditor) then
      begin
        FRichEditor := AOwner.Components[i] as TAdvRichEditor;
        break;
      end;
  end;
end;

procedure TAdvRichEditorIO.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  if (AComponent = FRichEditor) and (AOperation = opRemove) then
    FRichEditor := nil;

  inherited;
end;

procedure TAdvRichEditorIO.SetRichEditor(AValue: TAdvRichEditorBase);
begin
  FRichEditor := AValue;
end;

end.

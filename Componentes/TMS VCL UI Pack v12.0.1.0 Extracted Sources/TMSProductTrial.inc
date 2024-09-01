//Registry Keys Registered version
//Root: HKCU; Subkey: "Software\tmssoftware";
//Root: HKCU; Subkey: "Software\tmssoftware\TMS Component Pack";
//Root: HKCU; Subkey: "Software\tmssoftware\TMS Component Pack"; ValueType: string; ValueName: "InstallDir"; ValueData: "{code:GetInstallDir|''}"; Flags: uninsdeletevalue;
//Root: HKCU; Subkey: "Software\tmssoftware\TMS Component Pack"; ValueType: string; ValueName: "Username"; ValueData: ""; Flags:createvalueifdoesntexist;
//Root: HKCU; Subkey: "Software\tmssoftware\TMS Component Pack"; ValueType: string; ValueName: "Code"; ValueData: ""; Flags:createvalueifdoesntexist;
//Root: HKCU; Subkey: "Software\tmssoftware\TMS Component Pack"; ValueType: string; ValueName: "Version"; ValueData: "{code:GetVersionNr|''}"; Flags: uninsdeletevalue;
//Root: HKCU; Subkey: "Software\tmssoftware\TMS Component Pack"; ValueType: string; ValueName: "Date"; ValueData: "{code:GetInstallDate|''}"; Flags: uninsdeletevalue;

//Registry Keys trial version
//Root: HKCU; Subkey: "Software\tmssoftware";
//Root: HKCU; Subkey: "Software\tmssoftware\TMS Component Pack";
//Root: HKCU; Subkey: "Software\tmssoftware\TMS Component Pack"; ValueType: string; ValueName: "InstallDir"; ValueData: "{code:GetInstallDir|''}"; Flags: uninsdeletevalue;
//Root: HKCU; Subkey: "Software\tmssoftware\TMS Component Pack"; ValueType: string; ValueName: "Version"; ValueData: "{code:GetVersionNr|''}"; Flags: uninsdeletevalue;
//Root: HKCU; Subkey: "Software\tmssoftware\TMS Component Pack"; ValueType: string; ValueName: "Date"; ValueData: "{code:GetInstallDate|''}"; Flags: uninsdeletevalue;

{$I TMSProductInfo.inc}

type
  TInteractionHandler = class
    class procedure LabelClick(Sender: TObject);
    class procedure FormClick(Sender: TObject);
    class procedure FormClose(Sender: TObject; var ACloseAction: TCloseAction);
  end;

  TPaintObject = class
    {$IFDEF VCLLIB}
    class procedure FormPaint(Sender: TObject);
    class procedure CloseButtonPaint(Sender: TObject);
    {$ENDIF}
    {$IFDEF FMXLIB}
    class procedure FormPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    class procedure CloseButtonPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    {$ENDIF}
  end;

  TTrialForm = class(TForm)
  private
    FProductName: string;
    FVersionNumber: string;
    FInstallDate: string;
    FProductCategory: TProductCategory;
    FURLNames: TProductInfoArray;
    FURLs: TProductInfoArray;
    FProductKind: TProductKind;
  protected
    property ProductName: string read FProductName write FProductName;
    property VersionNumber: string read FVersionNumber write FVersionNumber;
    property ProductCategory: TProductCategory read FProductCategory write FProductCategory;
    property ProductKind: TProductKind read FProductKind write FProductKind;
    property URLNames: TProductInfoArray read FURLNames write FURLNames;
    property URLs: TProductInfoArray read FURLS write FURLs;
    property InstallDate: string read FInstalldate write FInstallDate;
    procedure Init(AProductName: string; AVersionNumber: string; AInstallDate: String; AProductCategory: TProductCategory; AProductKind: TProductKind; AURLNames: TProductInfoArray; AURLs: TProductInfoArray);
  end;

  TLinkLabel = class(TLabel)
  private
    FURL: String;
  protected
    property URL: string read FURL write FURL;
  end;

  {$IFDEF FMXLIB}
  TCloseButton = class(TRectangle)
  {$ENDIF}
  {$IFDEF VCLLIB}
  TCloseButton = class(TPaintBox)
  {$ENDIF}
  private
    FProductCategory: TProductCategory;
  protected
    property ProductCategory: TProductCategory read FProductCategory write FProductCategory;
  end;

function Scramble(s:string): string;
var
  r:string;
  i: integer;
  c: char;
  b: byte;
begin
  r := '';
  for i := 1 to length(s) do
  begin
    b := ord(s[i]);
    b := (b and $E0) + ((b and $1F) xor 5);
    c := chr(b);
    r := r + c;
  end;
  Result := r;
end;

function NoIDERunning: boolean;
begin
  {$IFDEF MSWINDOWS}
  Result := (FindWindow(PChar(Scramble('QDuuilfdqljk')), nil) = 0) OR
       (FindWindow(PChar(Scramble('QDuuGplia`w')), nil) = 0);
  {$ELSE}
  Result := True;
  {$ENDIF}
end;

procedure ShowTrialMessage(AProductName, AProductVersion: String; AProductKind: TProductKind; AProductCategory: TProductCategory; AURLNames: TProductInfoArray; AURLs: TProductInfoArray);
var
  frm: TTrialForm;
begin
  try
    frm := TTrialForm.CreateNew(Application.MainForm);
    frm.Init(AProductName, AProductVersion, '', AProductCategory, AProductKind, AURLNames, AURLs);
    frm.Show;
  finally
  end;
end;

{$IFDEF MSWINDOWS}
procedure ShowTrialMessageRegistry(AProductName: String; AProductCategory: TProductCategory; AURLNames: TProductInfoArray; AURLs: TProductInfoArray);
var
  frm: TTrialForm;
  reg: TRegistry;
  vn, un, cd, dt, k, dr: string;
  pk: TProductKind;
begin
  reg := TRegistry.Create;
  reg.RootKey := HKEY_CURRENT_USER;
  k := 'Software\tmssoftware\' + AProductName;
  vn := '';
  un := '';
  cd := '';
  dt := '';
  dr := '';
  pk := pkTrial;
  if reg.KeyExists(k) then
  begin
    if reg.OpenKey(k, False) then
    begin
      vn := reg.ReadString('Version');
      un := reg.ReadString('Username');
      cd := reg.ReadString('Code');
      dt := reg.ReadString('Date');
      dr := reg.ReadString('InstallDir');
      if (un <> '') and (cd <> '') then
        pk := pkRegistered;
    end;
  end;

  try
    frm := TTrialForm.CreateNew(Application.MainForm);
    frm.Init(AProductName, vn, dt, AProductCategory, pk, AURLNames, AURLs);
    frm.Show;
  finally
    reg.Free;
  end;
end;
{$ENDIF}

procedure TTrialForm.Init(AProductName: string; AVersionNumber: string; AInstallDate: string; AProductCategory: TProductCategory; AProductKind: TProductKind; AURLNames: TProductInfoArray; AURLs: TProductInfoArray);
var
  lbl: TLinkLabel;
  I: Integer;
  btn: TCloseButton;
begin
  FProductName := AProductName;
  FProductKind := AProductKind;
  FVersionNumber := AVersionNumber;
  FProductCategory := AProductCategory;
  FURLNames := AURLNames;
  FInstallDate := AInstallDate;
  FURLs := AURLs;
  {$IFDEF VCLLIB}
  Position := poScreenCenter;
  BorderStyle := TFormBorderStyle.bsNone;
  Color := HTMLColorToTColor(TMSProductCategoryBackgroundColors[Integer(AProductCategory)]);
  FormStyle := fsStayOnTop;
  OnClick := TInteractionHandler.FormClick;
  {$ENDIF}
  {$IFDEF FMXLIB}
  Position := TFormPosition.ScreenCenter;
  BorderStyle := TFMXFormBorderStyle.None;
  Fill.Kind := TBrushKind.Solid;
  Fill.Color := HTMLColorToTColor(TMSProductCategoryBackgroundColors[Integer(AProductCategory)]);
  FormStyle := TFormStyle.StayOnTop;
  {$ENDIF}
  OnPaint := TPaintObject.FormPaint;
  Width := 500;
  Height := 200;
  OnClose := TInteractionHandler.FormClose;

  for I := 0 to Length(AURLNames) - 1 do
  begin
    lbl := TLinkLabel.Create(Self);
    lbl.Parent := Self;
    {$IFDEF FMXLIB}
    lbl.HitTest := True;
    lbl.Text := AURLNames[I];
    lbl.Font.Family := 'Tahoma';
    lbl.StyledSettings := [];
    lbl.TextSettings.FontColor := claWhite;
    lbl.Font.Size := 16;
    lbl.Position.X := 105;
    lbl.Position.Y := 100 + (I * 25);
    {$ENDIF}
    {$IFDEF VCLLIB}
    lbl.Caption := AURLNames[I];
    lbl.Font.Name := 'Tahoma';
    lbl.Font.Color := clWhite;
    lbl.Font.Size := 12;
    lbl.Left := 105;
    lbl.Top := 100 + (I * 25);
    {$ENDIF}
    lbl.AutoSize := True;
    lbl.Font.Style := [TFontStyle.fsUnderline];
    lbl.Cursor := crHandPoint;
    lbl.OnClick := TInteractionHandler.LabelClick;
    if (I >= 0) and (I <= Length(AURLs) - 1) then
      lbl.URL := AURLs[I];
  end;

  btn := TCloseButton.Create(Self);
  btn.Parent := Self;
  btn.Width := 25;
  btn.Height := 25;
  {$IFDEF VCLLIB}
  btn.Left := ClientWidth - 30;
  btn.Top := 5;
  btn.Anchors := [akRight, akTop];
  btn.Color := Color;
  {$ENDIF}
  {$IFDEF FMXLIB}
  btn.HitTest := True;
  btn.Position.X := ClientWidth - 30;
  btn.Position.Y := 5;
  btn.Anchors := [TAnchorKind.akRight, TAnchorKind.akTop];
  btn.Fill.Color := Fill.Color;
  {$ENDIF}
  btn.OnClick := TInteractionHandler.FormClick;
  btn.OnPaint := TPaintObject.CloseButtonPaint;
  btn.ProductCategory := AProductCategory;
  btn.Cursor := crHandPoint;
end;

class procedure TInteractionHandler.FormClose(Sender: TObject; var ACloseAction: TCloseAction);
begin
  {$IFDEF VCLLIB}
  ACloseAction := caFree;
  {$ENDIF}
  {$IFDEF FMXLIB}
  ACloseAction := TCloseAction.caFree;
  {$ENDIF}
end;

class procedure TInteractionHandler.FormClick(Sender: TObject);
var
  frm: TTrialForm;
begin
  frm := nil;
  if Sender is TTrialForm then
    frm := (Sender as TTrialForm)
  else if Sender is TCloseButton then
    frm := ((Sender as TCloseButton).Owner as TTrialForm);

  if Assigned(frm) then
    frm.Close;
end;

class procedure TInteractionHandler.LabelClick(Sender: TObject);
var
  lbl: TLinkLabel;
begin
  lbl := (Sender as TLinkLabel);
  if Assigned(lbl) then
  begin
    {$IFDEF MSWINDOWS}
    ShellExecute(0,'open', PChar(lbl.URL), nil, nil, SW_NORMAL);
    {$ENDIF}
    {$IFDEF MACOS}
    TNSWorkSpace.Wrap(TNSWorkSpace.OCClass.sharedWorkSpace).openURL(TNSURL.Wrap(TNSURL.OCClass.URLWithString(StrToNSStr(lbl.URL))));
    {$ENDIF}
  end;
end;

{$IFDEF VCLLIB}
class procedure TPaintObject.CloseButtonPaint(Sender: TObject);
{$ENDIF}
{$IFDEF FMXLIB}
class procedure TPaintObject.CloseButtonPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
{$ENDIF}
var
  lbl: TCloseButton;
  c: TCanvas;
  {$IFDEF FMXLIB}
  rct: TRectF;
  {$ENDIF}
  {$IFDEF VCLLIB}
  rct: TRect;
  {$ENDIF}
begin
  lbl := (Sender as TCloseButton);
  if Assigned(lbl) then
  begin
    c := lbl.Canvas;
    {$IFDEF FMXLIB}
    c.Stroke.Kind := TBrushKind.None;
    c.Fill.Kind := TBrushKind.Solid;
    c.Fill.Color := HTMLColorToTColor(TMSProductCategoryColors[Integer(lbl.ProductCategory)]);
    rct := lbl.LocalRect;
    c.FillRect(rct, 0, 0, AllCorners, 1);
    c.Stroke.Kind := TBrushKind.Solid;
    c.Stroke.Thickness := 2;
    c.Stroke.Color := claWhite;
    rct := RectF(rct.Left + 4, rct.Top + 4, rct.Right - 6, rct.Bottom - 6);
    c.DrawLine(PointF(Int(rct.Left) + 0.5, Int(rct.Top) + 0.5), PointF(Int(rct.Right) + 0.5, Int(rct.Bottom) + 0.5), 1);
    c.DrawLine(PointF(Int(rct.Right) + 0.5, Int(rct.Top) + 0.5), PointF(Int(rct.Left) + 0.5, Int(rct.Bottom) + 0.5), 1);
    {$ENDIF}
    {$IFDEF VCLLIB}
    c.Pen.Style := psClear;
    c.Brush.Color := HTMLColorToTColor(TMSProductCategoryColors[Integer(lbl.ProductCategory)]);
    rct := lbl.ClientRect;
    c.Rectangle(rct);
    c.Pen.Color := clWhite;
    c.Pen.Width := 2;
    c.Pen.Style := psSolid;
    rct := Rect(rct.Left + 4, rct.Top + 4, rct.Right - 6, rct.Bottom - 6);
    c.MoveTo(rct.Left, rct.Top);
    c.LineTo(rct.Right, rct.Bottom);
    c.MoveTo(rct.Right, rct.Top);
    c.LineTo(rct.Left, rct.Bottom);
    {$ENDIF}
  end;
end;

{$IFDEF VCLLIB}
class procedure TPaintObject.FormPaint(Sender: TObject);
{$ENDIF}
{$IFDEF FMXLIB}
class procedure TPaintObject.FormPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
{$ENDIF}
var
  frm: TTrialForm;
  c: TCanvas;
  ct, pk: string;
  {$IFDEF VCLLIB}
  rct, rctx: TRect;
  {$ENDIF}
  {$IFDEF FMXLIB}
  rct, rctx: TRectF;
  {$ENDIF}
begin
  frm := (Sender as TTrialForm);
  if Assigned(frm) then
  begin
    c := frm.Canvas;
    {$IFDEF VCLLIB}
    c.Pen.Style := psClear;
    c.Brush.Color := HTMLColorToTColor(TMSProductCategoryColors[Integer(frm.ProductCategory)]);
    rct := Rect(10, 10, 90, 90);
    c.Rectangle(rct);

    if frm.ProductCategory = pcFNC then
    begin
      c.Brush.Color := HTMLColorToTColor(TMSProductCategoryColors[Integer(pcVCL)]);
      rctx := Rect(10, 70, 30, 90);
      c.Rectangle(rctx);

      c.Brush.Color := HTMLColorToTColor(TMSProductCategoryColors[Integer(pcWEB)]);
      rctx := Rect(30, 70, 50, 90);
      c.Rectangle(rctx);

      c.Brush.Color := HTMLColorToTColor(TMSProductCategoryColors[Integer(pcFMX)]);
      rctx := Rect(50, 70, 70, 90);
      c.Rectangle(rctx);

      c.Brush.Color := HTMLColorToTColor(TMSProductCategoryColors[Integer(pcLCL)]);
      rctx := Rect(70, 70, 90, 90);
      c.Rectangle(rctx);
    end;

    c.Brush.Style := bsClear;
    c.Font.Name := 'Tahoma';
    c.Font.Size := 18;
    c.Font.Style := [TFontStyle.fsBold];
    c.Font.Color := clWhite;

    pk := GetProductKindName(frm.ProductKind);
    ct := GetProductCategoryName(frm.ProductCategory);
    DrawText(c.Handle, PChar(ct), Length(ct), rct, DT_CENTER or DT_VCENTER or DT_SINGLELINE);
    c.TextOut(105, 10, frm.ProductName);
    c.Font.Size := 10;
    if frm.InstallDate <> '' then
      c.TextOut(105, 45, pk + ' version ' + frm.VersionNumber + ' installed on ' + frm.InstallDate)
    else
      c.TextOut(105, 45, pk + ' version ' + frm.VersionNumber);
    {$ENDIF}
    {$IFDEF FMXLIB}
    c.Stroke.Kind := TBrushKind.None;
    c.Fill.Kind := TBrushKind.Solid;
    c.Fill.Color := HTMLColorToTColor(TMSProductCategoryColors[Integer(frm.ProductCategory)]);
    rct := RectF(10, 10, 90, 90);
    c.FillRect(rct, 0, 0, AllCorners, 1);

    if frm.ProductCategory = pcFNC then
    begin
      c.Fill.Color := HTMLColorToTColor(TMSProductCategoryColors[Integer(pcVCL)]);
      rctx := RectF(10, 70, 30, 90);
      c.FillRect(rctx, 0, 0, AllCorners, 1);

      c.Fill.Color := HTMLColorToTColor(TMSProductCategoryColors[Integer(pcWEB)]);
      rctx := RectF(30, 70, 50, 90);
      c.FillRect(rctx, 0, 0, AllCorners, 1);

      c.Fill.Color := HTMLColorToTColor(TMSProductCategoryColors[Integer(pcFMX)]);
      rctx := RectF(50, 70, 70, 90);
      c.FillRect(rctx, 0, 0, AllCorners, 1);

      c.Fill.Color := HTMLColorToTColor(TMSProductCategoryColors[Integer(pcLCL)]);
      rctx := RectF(70, 70, 90, 90);
      c.FillRect(rctx, 0, 0, AllCorners, 1);
    end;

    c.Font.Family := 'Tahoma';
    c.Font.Size := 24;
    c.Font.Style := [TFontStyle.fsBold];
    c.Fill.Color := claWhite;

    pk := GetProductKindName(frm.ProductKind);
    ct := GetProductCategoryName(frm.ProductCategory);
    c.FillText(rct, ct, False, 1, [], TTextAlign.Center, TTextAlign.Center);
    rct.Left := 105;
    rct.Top := 10;
    rct.Width := 1000;
    rct.Height := 1000;
    c.FillText(rct, frm.ProductName, False, 1, [], TTextAlign.Leading, TTextAlign.Leading);
    rct.Top := 45;
    c.Font.Size := 14;
    if frm.InstallDate <> '' then
      c.FillText(rct, pk + ' version ' + frm.VersionNumber + ' installed on ' + frm.InstallDate, False, 1, [], TTextAlign.Leading, TTextAlign.Leading)
    else
      c.FillText(rct, pk + ' version ' + frm.VersionNumber, False, 1, [], TTextAlign.Leading, TTextAlign.Leading);
    {$ENDIF}
  end;
end;

initialization
  if NoIDERunning then
    ShowTrialMessage(GetProductName, GetProductVersion, GetProductKind, GetProductCategory, GetProductInfoNames, GetProductInfoURLs);

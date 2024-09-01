{*************************************************************************}
{                                                                         }
{ written by TMS Software                                                 }
{           copyright (c)  2020                                           }
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

unit VCL.TMSFNCGeneralDE;

{$I VCL.TMSFNCDefines.inc}

{$IFDEF WEBLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}
{$IFDEF LCLLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}

interface

uses
  Classes
  {$IFNDEF LCLWEBLIB}
  ,DesignEditors, DesignIntf, DesignMenus, VCL.Menus
  {$ENDIF}
  {$IFDEF WEBLIB}
  ,WEBLib.DesignIntf, WEBLib.Forms, web
  {$ENDIF}
  {$IFDEF LCLLIB}
  ,ComponentEditors, PropEdits, Menus
  {$ENDIF}
  {$IFDEF VCLLIB}
  ,VCLEditors, VCL.Graphics, Types, VCL.Consts
  {$ENDIF}
  ;

type
  TTMSFNCDefaultEditor = class(TDefaultEditor)
  public
    function GetVerb(Index: Integer):string; override;
    function GetVerbCount: Integer; override;
    procedure ExecuteVerb(Index: Integer{$IFDEF WEBLIB}; AProc: TModalResultProc{$ENDIF}); override;
    {$IFDEF LCLLIB}
    procedure PrepareItem(Index: Integer; const AItem: TMenuItem); override;
    {$ENDIF}
    {$IFNDEF LCLLIB}
    procedure PrepareItem(Index: Integer; const AItem: IMenuItem); override;
    {$ENDIF}
    procedure MenuItemExecute(Sender: TObject);
    procedure LoadStyle(AComponent: TComponent); {$IFDEF WEBLIB}async;{$ENDIF}
  end;

  {$IFDEF FNCLIB}
  TTMSFNCStylesManagerStyleResourceProperty = class(TStringProperty)
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;
  {$ENDIF}

  {$IFDEF VCLLIB}
  TTMSFNCColorProperty = class(TIntegerProperty, ICustomPropertyDrawing,
    ICustomPropertyListDrawing, ICustomPropertyDrawing80)
  protected
    function PaintColorBox(const Value: string; ACanvas: TCanvas;
      const ARect: TRect; ASelected: Boolean): TRect;
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;

    { ICustomPropertyListDrawing }
    procedure ListMeasureHeight(const Value: string; ACanvas: TCanvas;
      var AHeight: Integer);
    procedure ListMeasureWidth(const Value: string; ACanvas: TCanvas;
      var AWidth: Integer);
    procedure ListDrawValue(const Value: string; ACanvas: TCanvas;
      const ARect: TRect; ASelected: Boolean);

    { ICustomPropertyDrawing }
    procedure PropDrawName(ACanvas: TCanvas; const ARect: TRect;
      ASelected: Boolean);
    procedure PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
      ASelected: Boolean);
    { ICustomPropertyDrawing80 }
    function PropDrawNameRect(const ARect: TRect): TRect;
    function PropDrawValueRect(const ARect: TRect): TRect;
  end;

  {$ENDIF}

  {$IFNDEF LCLLIB}
  TTMSFNCGeneralSelectionEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;
  {$ENDIF}

  TTMSFNCItemPickerEditor = class(TTMSFNCDefaultEditor)
  protected
    {$IFNDEF WEBLIB}
    {$IFNDEF LCLLIB}
    procedure EditProperty(const PropertyEditor: IProperty; var Continue: Boolean); override;
    {$ENDIF}
    {$IFDEF LCLLIB}
    procedure EditProperty(const PropertyEditor: TPropertyEditor; var Continue: Boolean); override;
    {$ENDIF}
    {$ENDIF}
  end;

  TTMSFNCItemSelectorEditor = class(TTMSFNCDefaultEditor)
  protected
    {$IFNDEF WEBLIB}
    {$IFNDEF LCLLIB}
    procedure EditProperty(const PropertyEditor: IProperty; var Continue: Boolean); override;
    {$ENDIF}
    {$IFDEF LCLLIB}
    procedure EditProperty(const PropertyEditor: TPropertyEditor; var Continue: Boolean); override;
    {$ENDIF}
    {$ENDIF}
  end;

  {$IFNDEF WEBLIB}
  TTMSFNCAPIKeyProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;
  {$ENDIF}

  TTMSFNCGlobalFontNamePropertyEditor = class(TStringProperty)
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

implementation

uses
  {$IFDEF MSWINDOWS}
  Registry,
  {$ENDIF}
  SysUtils
  {$IFDEF FNCLIB}
  ,VCL.TMSFNCStylesEditor
  {$ENDIF}
  ,VCL.TMSFNCStyles, VCL.TMSFNCCustomComponent, VCL.TMSFNCTypes,
  VCL.TMSFNCPersistence, VCL.TMSFNCUtils, DateUtils, VCL.Dialogs
  {$IFNDEF LCLWEBLIB}
  {$HINTS OFF}
  {$IF COMPILERVERSION > 22}
  , UITypes
  {$IFEND}
  {$HINTS ON}
  {$ENDIF}
  {$IFDEF VCLLIB}
  ,VCL.TMSFNCGraphics, VCL.TMSFNCGraphicsTypes, VCL.Forms
  {$ENDIF}
  {$IFDEF FMXLIB}
  ,VCL.Forms
  {$ENDIF}
  {$IFDEF LCLLIB}
  ,Forms
  {$ENDIF}
  {$IFDEF WEBLIB}
  ,js
  {$ENDIF}
  ;

type
  {$IFNDEF WEBLIB}
  TTMSFNCStylesDEOpenDialog = TOpenDialog;
  {$ENDIF}
  {$IFDEF WEBLIB}
  TTMSFNCStylesDEOpenDialog = TWebOpenDialog;
  {$ENDIF}

{ TTMSFNCItemSelectorEditor }

{$IFNDEF WEBLIB}
{$IFNDEF LCLLIB}
procedure TTMSFNCItemSelectorEditor.EditProperty(const PropertyEditor: IProperty; var Continue: Boolean);
{$ENDIF}
{$IFDEF LCLLIB}
procedure TTMSFNCItemSelectorEditor.EditProperty(const PropertyEditor: TPropertyEditor; var Continue: Boolean);
{$ENDIF}
var
  PropName: string;
begin
  PropName := PropertyEditor.GetName;
  if (CompareText(PropName, 'ITEMS') = 0) then
  begin
    PropertyEditor.Edit;
    Continue := False;
  end;
end;
{$ENDIF}

{ TTMSFNCItemPickerEditor }

{$IFNDEF WEBLIB}
{$IFNDEF LCLLIB}
procedure TTMSFNCItemPickerEditor.EditProperty(const PropertyEditor: IProperty; var Continue: Boolean);
{$ENDIF}
{$IFDEF LCLLIB}
procedure TTMSFNCItemPickerEditor.EditProperty(const PropertyEditor: TPropertyEditor; var Continue: Boolean);
{$ENDIF}
var
  PropName: string;
begin
  PropName := PropertyEditor.GetName;
  if (CompareText(PropName, 'ITEMS') = 0) then
  begin
    PropertyEditor.Edit;
    Continue := False;
  end;
end;
{$ENDIF}

{$IFDEF VCLLIB}

{ TTMSFNCColorProperty }

function FNCColorToString(Color: TColor): string;
var
  I: Integer;
begin
  if not ColorToIdent(Color, Result) then begin
    for I := Low(TMSFNCGraphicsColors) to High(TMSFNCGraphicsColors) do
      if TMSFNCGraphicsColors[I].Value = Color then
      begin
        Result := StrPas(TMSFNCGraphicsColors[I].Name);
        Exit;
      end;
    FmtStr(Result, '$%.8x', [Color]);
  end;
end;

function FNCIdentToColor(const Ident: string; var Color: Longint): Boolean;
var
  I: Integer;
  Text: array[0..63] of Char;
begin
  StrPLCopy(Text, Ident, SizeOf(Text) - 1);
  for I := Low(TMSFNCGraphicsColors) to High(TMSFNCGraphicsColors) do
    if StrIComp(TMSFNCGraphicsColors[I].Name, Text) = 0 then begin
      Color := TMSFNCGraphicsColors[I].Value;
      Result := True;
      Exit;
    end;
  Result := IdentToColor(Ident, Color);
end;

function FNCStringToColor(S: string): TColor;
begin
  if not FNCIdentToColor(S, Longint(Result)) then
    Result := StringToColor(S);
end;

procedure FNCGetColorValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  GetColorValues(Proc);
  for I := Low(TMSFNCGraphicsColors) to High(TMSFNCGraphicsColors) do
    Proc(StrPas(TMSFNCGraphicsColors[I].Name));
end;

procedure TTMSFNCColorProperty.Edit;
var
  ColorDialog: TColorDialog;
  IniFile: TRegIniFile;

  procedure GetCustomColors;
  begin
    if BaseRegistryKey = '' then Exit;
    IniFile := TRegIniFile.Create(BaseRegistryKey);
    try
      IniFile.ReadSectionValues(SCustomColors, ColorDialog.CustomColors);
    except
      { Ignore errors reading values }
    end;
  end;

  procedure SaveCustomColors;
  var
    I, P: Integer;
    S: string;
  begin
    if IniFile <> nil then
      with ColorDialog do
        for I := 0 to CustomColors.Count - 1 do
        begin
          S := CustomColors.Strings[I];
          P := Pos('=', S);
          if P <> 0 then
          begin
            S := Copy(S, 1, P - 1);
            IniFile.WriteString(SCustomColors, S,
              CustomColors.Values[S]);
          end;
        end;
  end;

begin
  IniFile := nil;
  ColorDialog := TColorDialog.Create(Application);
  try
    GetCustomColors;
    ColorDialog.Color := GetOrdValue;
    ColorDialog.Options := [cdShowHelp];
    if ColorDialog.Execute then SetOrdValue(ColorDialog.Color);
    SaveCustomColors;
  finally
    IniFile.Free;
    ColorDialog.Free;
  end;
end;

function TTMSFNCColorProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paDialog, paValueList, paRevertable];
end;

function TTMSFNCColorProperty.GetValue: string;
begin
  Result := FNCColorToString(TTMSFNCGraphicsColor(GetOrdValue));
end;

procedure TTMSFNCColorProperty.GetValues(Proc: TGetStrProc);
begin
  FNCGetColorValues(Proc);
end;

procedure TTMSFNCColorProperty.PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
  ASelected: Boolean);
begin
  if GetVisualValue <> '' then
    PaintColorBox(GetVisualValue, ACanvas, ARect, ASelected)
//    ListDrawValue(GetVisualValue, ACanvas, ARect, True{ASelected})
  else
    DefaultPropertyDrawValue(Self, ACanvas, ARect);
end;

procedure TTMSFNCColorProperty.ListDrawValue(const Value: string; ACanvas: TCanvas;
  const ARect: TRect; ASelected: Boolean);
var
  ValueRect: TRect;
begin
  ValueRect := PaintColorBox(Value, ACanvas, Rect(ARect.Left + 8,
    ARect.Top, ARect.Right, ARect.Bottom), ASelected);
  DefaultPropertyListDrawValue(Value, ACanvas, ValueRect, ASelected);
end;

procedure TTMSFNCColorProperty.ListMeasureWidth(const Value: string;
  ACanvas: TCanvas; var AWidth: Integer);
begin
  AWidth := AWidth + ACanvas.TextHeight('M') {* 2};
end;

procedure TTMSFNCColorProperty.SetValue(const Value: string);
var
  NewValue: Longint;
begin
  if FNCIdentToColor(Value, NewValue) then
    SetOrdValue(NewValue)
  else
    inherited SetValue(Value);
end;

procedure TTMSFNCColorProperty.ListMeasureHeight(const Value: string;
  ACanvas: TCanvas; var AHeight: Integer);
begin
  // No implemenation necessary
end;

procedure TTMSFNCColorProperty.PropDrawName(ACanvas: TCanvas; const ARect: TRect;
  ASelected: Boolean);
begin
  DefaultPropertyDrawName(Self, ACanvas, ARect);
end;

function TTMSFNCColorProperty.PropDrawNameRect(const ARect: TRect): TRect;
begin
  Result := ARect;
end;

function TTMSFNCColorProperty.PropDrawValueRect(const ARect: TRect): TRect;
begin
  Result := Rect(ARect.Left, ARect.Top, (ARect.Bottom - ARect.Top) + ARect.Left, ARect.Bottom);
end;

function TTMSFNCColorProperty.PaintColorBox(const Value: string; ACanvas: TCanvas;
  const ARect: TRect; ASelected: Boolean): TRect;

  function ColorToBorderColor(AColor: TColor): TColor;
  type
    TColorQuad = record
      Red,
      Green,
      Blue,
      Alpha: Byte;
    end;
  begin
    if (TColorQuad(AColor).Red > 192) or
       (TColorQuad(AColor).Green > 192) or
       (TColorQuad(AColor).Blue > 192) then
      Result := clBlack
    else if ASelected then
      Result := clWhite
    else
      Result := AColor;
  end;

var
  Right: Integer;
  OldPenColor, OldBrushColor: TColor;
begin
  Right := (ARect.Bottom - ARect.Top) + ARect.Left;
  with ACanvas do
  begin
    // save off things
    OldPenColor := Pen.Color;
    OldBrushColor := Brush.Color;

    if UpperCase(Value) <> 'GCNULL' then
    begin
      Pen.Color := Brush.Color;
      Rectangle(ARect.Left, ARect.Top, Right, ARect.Bottom);

      Brush.Color := FNCStringToColor(Value);
      Pen.Color := ColorToBorderColor(ColorToRGB(Brush.Color));
      Rectangle(ARect.Left + 1, ARect.Top + 1, Right - 1, ARect.Bottom - 1);
    end
    else
    begin
      Pen.Color := clBlack;
      Rectangle(ARect.Left, ARect.Top, Right, ARect.Bottom);

      Pen.Color := clRed;
      MoveTo(ARect.Left + 1, ARect.Top + 1);
      LineTo(Right - 1, ARect.Bottom - 1);
    end;

    // restore the things we twiddled with
    Brush.Color := OldBrushColor;
    Pen.Color := OldPenColor;
    Result := Rect(Right, ARect.Top, ARect.Right, ARect.Bottom);
{    DefaultPropertyListDrawValue(Value, ACanvas, Rect(Right, ARect.Top, ARect.Right,
      ARect.Bottom), ASelected);}
  end;
end;

{$ENDIF}

{$IFNDEF LCLLIB}

{ TTMSFNCGeneralSelectionEditor }

procedure TTMSFNCGeneralSelectionEditor.RequiresUnits(Proc: TGetStrProc);
begin
  inherited;
  Proc('VCL.TMSFNCTypes');
  Proc('VCL.TMSFNCUtils');
  Proc('VCL.TMSFNCGraphics');
  Proc('VCL.TMSFNCGraphicsTypes');
end;
{$ENDIF}

{ TTMSFNCDefaultEditor }

procedure TTMSFNCDefaultEditor.ExecuteVerb(Index: Integer{$IFDEF WEBLIB}; AProc: TModalResultProc{$ENDIF});
var
  pi: ITMSFNCProductInfo;
  idx, s: Integer;
begin
  s := inherited GetVerbCount;

  idx := Index - s;

  if Supports(Component, ITMSFNCProductInfo, pi) then
  begin
    case idx of
      0: {$IFDEF WEBLIB}Application.IDEMessage{$ELSE}TTMSFNCUtils.&Message{$ENDIF}(Component.ClassName + ' ' + pi.GetVersion + #13#10'(c) ' + IntToStr(YearOf(Now)) + ' TMS Software'{$IFNDEF WEBLIB}, TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], 0{$ENDIF});
      1:
      begin
        if pi.GetDocURL <> '' then
          {$IFDEF WEBLIB}Application.IDEOpenURL{$ELSE}TTMSFNCUtils.OpenURL{$ENDIF}(pi.GetDocURL);
      end;
      2:
      begin
        if pi.GetTipsURL <> '' then
          {$IFDEF WEBLIB}Application.IDEOpenURL{$ELSE}TTMSFNCUtils.OpenURL{$ENDIF}(pi.GetTipsURL);
      end;
    end;
  end;
end;

procedure TTMSFNCDefaultEditor.MenuItemExecute(Sender: TObject);
var
  idx: Integer;
  MenuItem: {$IFDEF WEBLIB}TObject{$ELSE}TMenuItem{$ENDIF};
  {$IFNDEF WEBLIB}
  sd: TSaveDialog;
  fn, fe: string;
  {$ENDIF}
  io: ITMSFNCPersistence;
  {$IFDEF FNCLIB}
  ed: TTMSFNCStylesEditor;
  {$ENDIF}
begin
  {$IFDEF WEBLIB}
  idx := -1;
  MenuItem := Sender;
  asm
    idx = MenuItem.tag;
  end;
  {$ELSE}
  MenuItem := Sender as TMenuItem;
  idx := MenuItem.Tag;
  {$ENDIF}

  if idx >= 0 then
  begin
    case idx of
      0:
      begin
        LoadStyle(Component);
      end;
      1:
      begin
        {$IFNDEF WEBLIB}
        sd := TSaveDialog.Create(Component);
        sd.Filter := 'All (*.*)|*.*|Style file (*.style)|*.style';
        sd.DefaultExt := 'style';
        sd.Options := sd.Options + [TOpenOption.ofOverwritePrompt];
        sd.FilterIndex := 2;

        try
          if sd.Execute then
          begin
            fn := sd.FileName;

            fe := Uppercase(ExtractFileExt(sd.FileName));

            if (fe = '') then
            begin
              if sd.FilterIndex = 2 then
                fn := fn + '.style';
            end
            else
            begin
              if fe = '.STYLE' then
                sd.FilterIndex := 2;
            end;

            if Supports(Component, ITMSFNCPersistence, io) then
              io.SaveSettingsToFile(fn, True);
          end;
        finally
          sd.Free;
        end;
        {$ENDIF}
      end;
      2:
      begin
        {$IFDEF FNCLIB}
        ed := TTMSFNCStylesEditor.Create(Component);
        try
          ed.StylesForm := TTMSFNCUtils.GetOwnerForm(Component);
          ed.Execute
          {$IFDEF WEBLIB}
          (procedure(AResult: TModalResult)
          begin
            ed.Free;
          end)
          {$ENDIF}
          ;
        finally
          {$IFNDEF WEBLIB}
          ed.Free;
          {$ENDIF}
        end;
        {$ENDIF}
      end;
    end;
  end;
end;

{$IFDEF LCLLIB}
procedure TTMSFNCDefaultEditor.PrepareItem(Index: Integer;
  const AItem: TMenuItem);
{$ENDIF}
{$IFNDEF LCLLIB}
procedure TTMSFNCDefaultEditor.PrepareItem(Index: Integer;
  const AItem: IMenuItem);
{$ENDIF}
var
  {$IFDEF LCLLIB}
  MenuItem: TMenuItem;
  {$ENDIF}
  {$IFNDEF LCLLIB}
  MenuItem: IMenuItem;
  {$ENDIF}
  pi: ITMSFNCProductInfo;
  {$IFDEF FNCLIB}
  io: ITMSFNCPersistence;
  {$ENDIF}
  i, k, idx, s: Integer;
begin
  inherited;
  s := inherited GetVerbCount;

  i := 0;
  k := 0;

  if Supports(Component, ITMSFNCProductInfo, pi) then
  begin
    Inc(i);
    if pi.GetDocURL <> '' then
      Inc(i);

    if pi.GetTipsURL <> '' then
      Inc(i);
  end;

 {$IFDEF FNCLIB}
  if Supports(Component, ITMSFNCPersistence, io) and not (Component is TTMSFNCCustomComponent) then
    k := 1;
 {$ENDIF}

  idx := Index - s;

  if k > 0 then
  begin
    if i = 1 then
    begin
      case Index of
        2: idx := 4;
      end;
    end
    else if i = 2 then
    begin
      case Index of
        3: idx := 4;
      end;
    end;

    case idx of
      4:
      begin
        {$IFNDEF LCLLIB}
        MenuItem := AItem.AddItem('Load Style', 0, False, True, {$IFDEF WEBLIB}@{$ENDIF}MenuItemExecute);
        {$ENDIF}
        {$IFDEF LCLLIB}
        MenuItem := TMenuItem.Create(AItem);
        MenuItem.Caption := 'Load Style';
        MenuItem.OnClick := MenuItemExecute;
        AItem.Add(MenuItem);
        {$ENDIF}
        MenuItem.Tag := 0;

        {$IFNDEF WEBLIB}
        {$IFNDEF LCLLIB}
        MenuItem := AItem.AddItem('Save Style', 0, False, True, {$IFDEF WEBLIB}@{$ENDIF}MenuItemExecute);
        {$ENDIF}
        {$IFDEF LCLLIB}
        MenuItem := TMenuItem.Create(AItem);
        MenuItem.Caption := 'Save Style';
        MenuItem.OnClick := MenuItemExecute;
        AItem.Add(MenuItem);
        {$ENDIF}
        MenuItem.Tag := 1;
        {$ENDIF}

        {$IFNDEF LCLLIB}
        MenuItem := AItem.AddItem('Manager', 0, False, True, {$IFDEF WEBLIB}@{$ENDIF}MenuItemExecute);
        {$ENDIF}
        {$IFDEF LCLLIB}
        MenuItem := TMenuItem.Create(AItem);
        MenuItem.Caption := 'Manager';
        MenuItem.OnClick := MenuItemExecute;
        AItem.Add(MenuItem);
        {$ENDIF}
        MenuItem.Tag := 2;
      end;
    end;
  end;
end;

function TTMSFNCDefaultEditor.GetVerb(Index: Integer): string;
var
  pi: ITMSFNCProductInfo;
  {$IFDEF FNCLIB}
  io: ITMSFNCPersistence;
  {$ENDIF}
  i, k, idx, s: Integer;
begin
  Result := inherited;
  s := inherited GetVerbCount;

  i := 0;
  k := 0;

  if Supports(Component, ITMSFNCProductInfo, pi) then
  begin
    Inc(i);
    if pi.GetDocURL <> '' then
      Inc(i);

    if pi.GetTipsURL <> '' then
      Inc(i);
  end;

  {$IFDEF FNCLIB}
  if Supports(Component, ITMSFNCPersistence, io) and not (Component is TTMSFNCCustomComponent) then
    k := 1;
  {$ENDIF}

  idx := Index - s;

  if k > 0 then
  begin
    if i = 1 then
    begin
      case Index of
        0: idx := 0;
        1: idx := 3;
        2: idx := 4;
        3: idx := 5;
      end;
    end
    else if i = 2 then
    begin
      case Index of
        0: idx := 0;
        1: idx := 1;
        2: idx := 3;
        3: idx := 4;
        4: idx := 5;
      end;
    end;
  end;

  case idx of
    0: Result := '&About';
    1: Result := '&Documentation';
    2: Result := '&Tips && FAQ';
    3: Result := '-';
    4: Result := '&Style';
    5: Result := '-';
  end;
end;

function TTMSFNCDefaultEditor.GetVerbCount: Integer;
var
  pi: ITMSFNCProductInfo;
  {$IFDEF FNCLIB}
  io: ITMSFNCPersistence;
  {$ENDIF}
begin
  Result := 0;
  if Supports(Component, ITMSFNCProductInfo, pi) then
  begin
    Inc(Result);
    if pi.GetDocURL <> '' then
      Inc(Result);

    if pi.GetTipsURL <> '' then
      Inc(Result);
  end;

  {$IFDEF FNCLIB}
  if Supports(Component, ITMSFNCPersistence, io) and not (Component is TTMSFNCCustomComponent) then
  begin
    Result := Result + 2;
  end;
  {$ENDIF}

  if Result > 0 then
    Result := Result + 1;
end;

procedure TTMSFNCDefaultEditor.LoadStyle(AComponent: TComponent);
{$IFDEF FNCLIB}
var
  I: Integer;
  {$IFDEF WEBLIB}
  res: boolean;
  str: string;
  {$ENDIF}
  m: TTMSFNCStylesManager;
  arr: TTMSFNCStylesManagerComponentArray;
  od: TTMSFNCStylesDEOpenDialog;
  io: ITMSFNCPersistence;
{$ENDIF}
begin
  {$IFDEF FNCLIB}
  od := TTMSFNCStylesDEOpenDialog.Create(AComponent);
  od.Filter := 'All (*.*)|*.*|Style file (*.style)|*.style';
  od.FilterIndex := 0;
  try
    {$IFNDEF WEBLIB}
    if od.Execute then
    {$ENDIF}
    {$IFDEF WEBLIB}
    res := await(Boolean, od.Perform);
    if res then
    {$ENDIF}
    begin
      for I := 0 to od.Files.Count - 1 do
      begin
        if Supports(AComponent, ITMSFNCPersistence, io) then
        begin
          m := TTMSFNCStylesManager.Create(TTMSFNCUtils.GetOwnerForm(AComponent));
          try
            SetLength(arr, 1);
            arr[0] := AComponent;
            {$IFDEF WEBLIB}
            str := await(string, od.Files[0].FileAsText);
            m.LoadStyleFromText(str, arr);
            {$ELSE}
            m.LoadStyleFromFile(od.Files[I], arr);
            {$ENDIF}
            finally
              m.Free;
            end;
        end;
      end;
    end;
  finally
    od.Free;
  end;
  {$ENDIF}
end;

{$IFDEF FNCLIB}

{ TTMSFNCStylesManagerStyleResourceProperty }

function TTMSFNCStylesManagerStyleResourceProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList];
end;

procedure TTMSFNCStylesManagerStyleResourceProperty.GetValues(
  Proc: TGetStrProc);
var
  sl: TStringList;
  I: Integer;
  f: string;
begin
  Proc('');
  sl := TStringList.Create;
  try
    TTMSFNCStylesEditor.LoadStylesFromResources(sl);
    for I := 0 to sl.Count - 1 do
    begin
      f := sl[I];
      f := StringReplace(f, 'TTMSFNCSTYLE_', '', [rfReplaceAll]);
      Proc(f);
    end;
  finally
    sl.Free;
  end;
end;
{$ENDIF}

{$IFNDEF WEBLIB}

{ TTMSFNCAPIKeyProperty }

function TTMSFNCAPIKeyProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList{$IFNDEF LCLLIB}, paValueEditable{$ENDIF}];
end;

procedure TTMSFNCAPIKeyProperty.GetValues(Proc: TGetStrProc);
var
  p: TPersistent;
  {$IFDEF MSWINDOWS}
  f, fn: string;
  RegIniFile: TRegIniFile;
  sl: TStringList;
  I: Integer;
  {$ENDIF}
begin
  p := GetComponent(0);
  if not Assigned(p) then
    Exit;

  {$IFDEF MSWINDOWS}
  sl := TStringList.Create;
  try
    RegIniFile := TRegIniFile.Create('TMSFNCAPIKeys');
    try
      RegIniFile.ReadSectionValues('', sl);
      for I := 0 to sl.Count - 1 do
      begin
        f := sl.Names[I];
        fn := sl.Values[f];
        Proc(f + ' - ' + fn);
      end;
    finally
      RegIniFile.Free;
    end;
  finally
    sl.Free;
  end;
  {$ENDIF}
end;

procedure TTMSFNCAPIKeyProperty.SetValue(const Value: string);
var
  v: string;
  p: Integer;
begin
  v := Value;
  p := Pos(' - ', v);
  if p > 0 then
    v := Trim(Copy(v, p + 3, Length(v) - p));

  inherited SetValue(v);
end;

{$ENDIF}

{ TTMSFNCGlobalFontNamePropertyEditor }

function TTMSFNCGlobalFontNamePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList];
end;

procedure TTMSFNCGlobalFontNamePropertyEditor.GetValues(Proc: TGetStrProc);
var
  st: TStringList;
  i: Integer;
begin
  st := TStringList.Create;
  TTMSFNCUtils.GetFonts(st);
  for i := 1 to st.Count do
  begin
    Proc(st.Strings[i - 1]);
  end;
  st.Free;
end;

end.


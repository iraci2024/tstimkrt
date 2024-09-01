{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{            copyright (c) 2016 - 2021                               }
{            Email : info@tmssoftware.com                            }
{            Web : https://www.tmssoftware.com                       }
{                                                                    }
{ The source code is given as is. The author is not responsible      }
{ for any possible damage done due to the use of this code.          }
{ The complete source code remains property of the author and may    }
{ not be distributed, published, given or sold in any form as such.  }
{ No parts of the source code can be included in any other component }
{ or application without written authorization of the author.        }
{********************************************************************}

unit LCLTMSFNCEditorsTools;

{$I LCLTMSFNCDefines.inc}
{$IFDEF LCLLIB}
  {$DEFINE LCLWEBLIB}
{$ENDIF}
{$IFDEF CMNLIB}
  {$DEFINE CMNWEBLIB}
{$ENDIF}
{$IFDEF WEBLIB}
  {$DEFINE CMNWEBLIB}
  {$DEFINE LCLWEBLIB}
{$ENDIF}
{$IFDEF VCLLIB}
  {$DEFINE FMXVCLLIB}
{$ENDIF}
{$IFDEF FMXLIB}
  {$DEFINE FMXVCLLIB}
{$ENDIF}
interface

uses
  Classes, Types, LCLTMSFNCTypes, LCLTMSFNCGraphicsTypes, Graphics,
  LCLTMSFNCEditorButton, LCLTMSFNCEditorListView, LCLTMSFNCEditorPanel,
  StdCtrls, Controls
  {$IFDEF FMXLIB}
  ,FMX.Edit, FMX.Memo
  {$ENDIF}
  {$IFDEF WEBLIB}
  ,JS, Web
  {$ENDIF}
  {$IFNDEF LCLWEBLIB}
  {$HINTS OFF}
  {$IF COMPILERVERSION > 22}
  ,UITypes
  {$IFEND}
  {$HINTS ON}
  ,Generics.Collections
  {$ENDIF}
  {$IFDEF LCLLIB}
  ,fgl, LCLType, LCLIntF
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // v1.0.0.0 : First Release


  {$IFDEF FMXLIB}
  EDITORBUTTONROUNDING = 5;
  EDITORPRIMCOLOR = $FF0077be;
  EDITORCOUNTERCOLOR = $FFbe7700;
  EDITOROKFONTCOLOR = gcWhite;
  EDITORSTROKECOLORDARK =$FF4F5E6B;
  EDITORSTROKECOLORLIGHT =$FFCEDEEC;
  EDITORAUTOCOLOR = gcNull;
  EDITORFONTCOLORLIGHT = $FF555555;
  EDITORFONTCOLORDARK = $FFAAAAAA;
  EDITORFONTCOLORDISABLED = $FF787878;
  EDITORMAINBACKCOLORLIGHT = gcWhite;
  EDITORMAINBACKCOLORDARK = $FF2A2A2C;
  EDITORSUBBACKCOLORLIGHT = gcWhitesmoke;
  EDITORSUBBACKCOLORDARK = $FF3E3E43;
  {$ENDIF}
  {$IFNDEF FMXLIB}
  EDITORBUTTONROUNDING = 5;
  EDITORPRIMCOLOR = $be7700;
  EDITORSTROKECOLORLIGHT = $ECDECE;
  EDITORSTROKECOLORDARK = $6B5E4F;
  EDITORCOUNTERCOLOR = $0077be;
  EDITOROKFONTCOLOR = gcWhite;
  EDITORAUTOCOLOR = gcNull;
  EDITORFONTCOLORLIGHT = $555555;
  EDITORFONTCOLORDARK = $AAAAAA;
  EDITORFONTCOLORDISABLED = $787878;
  EDITORMAINBACKCOLORLIGHT = gcWhite;
  EDITORMAINBACKCOLORDARK = $2C2A2A;
  EDITORSUBBACKCOLORLIGHT = gcWhitesmoke;
  EDITORSUBBACKCOLORDARK = $433E3E;
  {$ENDIF}

  // Color selection VSC (Bruno) 094771
  //0077be


type
  TTMSFNCEditorDesignerTheme = (edtDefault, edtLight, edtDark);


function IsLightTheme: boolean;
function EditorDarkerShadeColor(AColor: TTMSFNCGraphicsColor; AFactor: Double = 10; ATimes: Integer = 1): TTMSFNCGraphicsColor;
function EditorLighterShadeColor(AColor: TTMSFNCGraphicsColor; AFactor: Double = 10; ATimes: Integer = 1): TTMSFNCGraphicsColor;
procedure SetTMSFNCDesignerTheme(ATheme: TTMSFNCEditorDesignerTheme);
function GetTMSFNCDesignerTheme: TTMSFNCEditorDesignerTheme;

procedure SetEditorOKButtonAppearance(AButton: TTMSFNCEditorButton; APrimaryColor: TTMSFNCGraphicsColor = EDITORPRIMCOLOR; ARounding: Integer = EDITORBUTTONROUNDING; AFontColor: TTMSFNCGraphicsColor = EDITOROKFONTCOLOR);
procedure SetEditorCancelButtonAppearance(AButton: TTMSFNCEditorButton; APrimaryColor: TTMSFNCGraphicsColor = EDITORPRIMCOLOR; ARounding: Integer = EDITORBUTTONROUNDING; AFontColor: TTMSFNCGraphicsColor = EDITORAUTOCOLOR; ABackGroundColor: TTMSFNCGraphicsColor = EDITORAUTOCOLOR);
procedure SetEditorTabButtonAppearance(AButton: TTMSFNCEditorButton; APrimaryColor: TTMSFNCGraphicsColor = EDITORPRIMCOLOR; ARounding: Integer = EDITORBUTTONROUNDING; AFontColor: TTMSFNCGraphicsColor = EDITORAUTOCOLOR; ABackGroundColor: TTMSFNCGraphicsColor = EDITORAUTOCOLOR);
procedure SetEditorSubTabButtonAppearance(AButton: TTMSFNCEditorButton; APrimaryColor: TTMSFNCGraphicsColor = EDITORPRIMCOLOR; ARounding: Integer = EDITORBUTTONROUNDING; AFontColor: TTMSFNCGraphicsColor = EDITORAUTOCOLOR; ABackGroundColor: TTMSFNCGraphicsColor = EDITORAUTOCOLOR);
procedure SetEditorIconButtonAppearance(AButton: TTMSFNCEditorButton; APrimaryColor: TTMSFNCGraphicsColor = EDITORPRIMCOLOR; ARounding: Integer = EDITORBUTTONROUNDING; AFontColor: TTMSFNCGraphicsColor = EDITORAUTOCOLOR; ABackGroundColor: TTMSFNCGraphicsColor = EDITORAUTOCOLOR);
procedure SetEditorBitmapListAppearance(ABitmapListView: TTMSFNCBitmapEditorListView; APrimaryColor: TTMSFNCGraphicsColor = EDITORPRIMCOLOR; ARounding: integer = EDITORBUTTONROUNDING; AFontColor: TTMSFNCGraphicsColor = EDITORAUTOCOLOR; ABackGroundColor: TTMSFNCGraphicsColor = EDITORAUTOCOLOR);
procedure SetEditorListAppearance(AEditorList: TTMSFNCEditorList; APrimaryColor: TTMSFNCGraphicsColor = EDITORPRIMCOLOR; ARounding: integer = EDITORBUTTONROUNDING; AFontColor: TTMSFNCGraphicsColor = EDITORAUTOCOLOR; ABackGroundColor: TTMSFNCGraphicsColor = EDITORAUTOCOLOR);
procedure SetEditorBackPanelAppearance(APanel: TTMSFNCEditorPanel; ABackGroundColor: TTMSFNCGraphicsColor = EDITORAUTOCOLOR; AStrokeColor: TTMSFNCGraphicsColor = EDITORAUTOCOLOR; ARounding: Integer = EDITORBUTTONROUNDING);
procedure SetEditorSubPanelAppearance(APanel: TTMSFNCEditorPanel; ABackGroundColor: TTMSFNCGraphicsColor = EDITORAUTOCOLOR; AStrokeColor: TTMSFNCGraphicsColor = EDITORAUTOCOLOR; ARounding: integer = 0);
procedure SetEditorLabelAppearance(ALabel: TLabel; AFontColor: TTMSFNCGraphicsColor = EDITORAUTOCOLOR);
procedure SetEditorCheckBoxAppearance(ACheckBox: TCheckBox; AFontColor: TTMSFNCGraphicsColor = EDITORAUTOCOLOR);
procedure SetEditorEditAppearance(AEdit: TEdit; AFontColor: TTMSFNCGraphicsColor = EDITORAUTOCOLOR; ABackGroundColor: TTMSFNCGraphicsColor = EDITORAUTOCOLOR; AStrokeColor: TTMSFNCGraphicsColor = EDITORAUTOCOLOR);
procedure SetEditorMemoAppearance(AMemo: TMemo; AFontColor: TTMSFNCGraphicsColor = EDITORAUTOCOLOR; ABackGroundColor: TTMSFNCGraphicsColor = EDITORAUTOCOLOR; AStrokeColor: TTMSFNCGraphicsColor = EDITORAUTOCOLOR);

var
  TMSFNCDesignerTheme: TTMSFNCEditorDesignerTheme = edtDefault;

implementation
uses
  LCLTMSFNCGraphics, LCLTMSFNCUtils
  , Math, SysUtils
  {$IFDEF FMXLIB}
  , FMX.Types, FMX.Objects
  {$ENDIF}
  {$IFDEF WEBLIB}
  , WEBLib.Forms
  {$ENDIF}
  {$IFDEF FMXVCLLIB}
  {$IFDEF MSWINDOWS}
  , System.Win.Registry, Winapi.Windows
  {$ENDIF}
  {$ENDIF}
  ;

procedure SetTMSFNCDesignerTheme(ATheme: TTMSFNCEditorDesignerTheme);
begin
  TMSFNCDesignerTheme := ATheme;
end;

function GetTMSFNCDesignerTheme: TTMSFNCEditorDesignerTheme;
begin
  Result := TMSFNCDesignerTheme;
end;

function NoIDERunning: boolean;
begin
  {$IFDEF WEBLIB}
  if Application.IDETheme <> '' then
  begin
    Result := False;
    Exit;
  end;
  {$ENDIF}

  {$IFNDEF LCLLIB}
  {$IFDEF MSWINDOWS}
  Result := (FindWindow(PChar('TApplication'), nil) = 0) OR
     (FindWindow(PChar('TAppBuilder'), nil) = 0);
  {$ELSE}
  Result := True;
  {$ENDIF}
  {$ENDIF}
  {$IFDEF LCLLIB}
  Result := True;
  {$ENDIF}
end;

function IsLightTheme: boolean;
{$IFDEF FMXVCLLIB}
{$IFDEF MSWINDOWS}
var
  reg: TRegistry;
  key: string;
  s: string;
{$ENDIF}
{$ENDIF}
{$IFDEF WEBLIB}
var
  s: string;
{$ENDIF}
begin
  if TMSFNCDesignerTheme <> edtDefault then
  begin
    Result := TMSFNCDesignerTheme = edtLight;
    Exit;
  end;

  Result := True;

  if NoIDERunning then
    Exit;

  {$IFDEF WEBLIB}
  s := Application.IDETheme;
  if (s = 'vscode-light') then
      Result := True
  else
    Result := False;
  {$ENDIF}
  {$IFDEF FMXVCLLIB}
  {$IFDEF MSWINDOWS}
  reg := TRegistry.Create;
  reg.RootKey := HKEY_CURRENT_USER;
  key := '';
  try
    {$IF COMPILERVERSION = 32}
    key := 'SOFTWARE\Embarcadero\BDS\19.0\Theme';
    {$ENDIF}
    {$IF COMPILERVERSION = 33}
    key := 'SOFTWARE\Embarcadero\BDS\20.0\Theme';
    {$ENDIF}
    {$IF COMPILERVERSION = 34}
    key := 'SOFTWARE\Embarcadero\BDS\21.0\Theme';
    {$ENDIF}
    {$IF COMPILERVERSION >= 35}
    key := 'SOFTWARE\Embarcadero\BDS\22.0\Theme';
    {$ENDIF}

    if (key <> '') and reg.KeyExists(key) then
    begin
      reg.OpenKey(key, false);
      if reg.ValueExists('Theme') then
      begin
        s := reg.GetDataAsString('Theme');
        if (s = 'Dark') then
          Result := False;
      end;
      reg.CloseKey;
    end
  finally
    reg.Free;
  end;
  {$ENDIF}
  {$ENDIF}
end;

function EditorDarkerShadeColor(AColor: TTMSFNCGraphicsColor; AFactor: Double; ATimes: Integer): TTMSFNCGraphicsColor;
var
  r,g,b: integer;
  I: Integer;
begin
  r := TTMSFNCGraphics.GetColorRed(AColor);
  g := TTMSFNCGraphics.GetColorGreen(AColor);
  b :=TTMSFNCGraphics.GetColorBlue(AColor);

  for I := 0 to ATimes do
  begin
    r := Max(0, Round(r - (r/255)* AFactor));
    g := Max(0, Round(g - (g/255)* AFactor));
    b := Max(0, Round(b - (b/255)* AFactor));
  end;

  Result := TTMSFNCGraphics.HTMLToColor('#' + IntToHex(r, 2) + IntToHex(g, 2) + IntToHex(b, 2));
end;

function EditorLighterShadeColor(AColor: TTMSFNCGraphicsColor; AFactor: Double; ATimes: Integer): TTMSFNCGraphicsColor;
var
  r,g,b: integer;
  I: Integer;
begin
  r := TTMSFNCGraphics.GetColorRed(AColor);
  g := TTMSFNCGraphics.GetColorGreen(AColor);
  b :=TTMSFNCGraphics.GetColorBlue(AColor);

  for I := 0 to ATimes do
  begin
    r := Min(255, Round(r + (r/255)* AFactor));
    g := Min(255, Round(g + (g/255)* AFactor));
    b := Min(255, Round(b + (b/255)* AFactor));
  end;

  Result := TTMSFNCGraphics.HTMLToColor('#' + IntToHex(r, 2) + IntToHex(g, 2) + IntToHex(b, 2));
end;

procedure SetEditorOKButtonAppearance(AButton: TTMSFNCEditorButton; APrimaryColor: TTMSFNCGraphicsColor; ARounding: Integer; AFontColor: TTMSFNCGraphicsColor);
begin
  if Assigned(AButton) then
  begin
    AButton.Appearance.Stroke.Kind := gskNone;
    AButton.Appearance.HoverStroke.Color := APrimaryColor;
    AButton.Appearance.DownStroke.Color := APrimaryColor;

    AButton.Appearance.Fill.Color := APrimaryColor;
    AButton.Appearance.HoverFill.Kind := gfkGradient;
    AButton.Appearance.HoverFill.Color := APrimaryColor;

    AButton.Appearance.DisabledStroke.Color := APrimaryColor;
    if IsLightTheme then
    begin
      AButton.Appearance.DisabledFill.Color := EDITORSUBBACKCOLORLIGHT;
      AButton.Appearance.DisabledFont.Color := EDITORFONTCOLORLIGHT;
    end
    else
    begin
      AButton.Appearance.DisabledFill.Color := EDITORSUBBACKCOLORDARK;
      AButton.Appearance.DisabledFont.Color := EDITORFONTCOLORDARK;
    end;

    AButton.Appearance.HoverFill.ColorTo := EditorDarkerShadeColor(APrimaryColor);
    AButton.Appearance.DownFill.Color := EditorLighterShadeColor(APrimaryColor);

    AButton.Appearance.Font.Style := [TFontStyle.fsBold];
    AButton.Appearance.HoverFont.Style := [TFontStyle.fsBold];
    AButton.Appearance.DownFont.Style := [TFontStyle.fsBold];

    AButton.Appearance.Font.Color := AFontColor;
    AButton.Appearance.HoverFont.Color := AFontColor;
    AButton.Appearance.DownFont.Color := AFontColor;

    AButton.Appearance.Rounding := ARounding;
  end;
end;

procedure SetEditorCancelButtonAppearance(AButton: TTMSFNCEditorButton; APrimaryColor: TTMSFNCGraphicsColor; ARounding: Integer; AFontColor: TTMSFNCGraphicsColor; ABackGroundColor: TTMSFNCGraphicsColor);
var
  fc: TTMSFNCGraphicsColor;

begin
  if Assigned(AButton) then
  begin
    if AFontColor = EDITORAUTOCOLOR then
    begin
      if IsLightTheme then
        fc := EDITORFONTCOLORLIGHT
      else
        fc := EDITORFONTCOLORDARK;
    end
    else
      fc := AFontColor;

    AButton.Appearance.HoverFont.Color := APrimaryColor;
    AButton.Appearance.DownFont.Color := gcWhite;

    AButton.Appearance.HoverStroke.Color := APrimaryColor;
    AButton.Appearance.DownStroke.Color := APrimaryColor;

    AButton.Appearance.DownFill.Color := EditorLighterShadeColor(APrimaryColor);

    AButton.Appearance.Font.Color := fc;
    AButton.Appearance.Fill.Color := ABackGroundColor;
    AButton.Appearance.HoverFill.Color := ABackGroundColor;

    AButton.Appearance.Rounding := ARounding;
  end;
end;

procedure SetEditorBitmapListAppearance(ABitmapListView: TTMSFNCBitmapEditorListView; APrimaryColor: TTMSFNCGraphicsColor; ARounding: integer; AFontColor: TTMSFNCGraphicsColor; ABackGroundColor: TTMSFNCGraphicsColor);
var
  dc, fc: TTMSFNCGraphicsColor;

begin
  if Assigned(ABitmapListView) then
  begin
    if AFontColor = EDITORAUTOCOLOR then
    begin
      if IsLightTheme then
        fc := EDITORFONTCOLORLIGHT
      else
        fc := EDITORFONTCOLORDARK;
    end
    else
      fc := AFontColor;

    ABitmapListView.Appearance.ItemVerticalSpacing := ARounding;
    ABitmapListView.Appearance.ItemHorizontalSpacing := ARounding;

    ABitmapListView.Appearance.ItemRounding := ARounding;
    ABitmapListView.Appearance.ItemImageRounding := ARounding;

    ABitmapListView.Appearance.ItemStroke.Kind := gskNone;
    ABitmapListView.Appearance.ItemHoverFill.Kind := gfkNone;
    ABitmapListView.Appearance.ItemHoverStroke.Color := APrimaryColor;
    ABitmapListView.Appearance.ItemHoverFont.Color := APrimaryColor;

    ABitmapListView.Appearance.ItemDownStroke.Color := APrimaryColor;
    dc := EditorLighterShadeColor(APrimaryColor);
    ABitmapListView.Appearance.ItemDownFill.Color := dc;
    ABitmapListView.Appearance.ItemSelectedStroke.Color := EditorLighterShadeColor(dc);

    ABitmapListView.Appearance.ItemSelectedFill.Kind := gfkGradient;
    ABitmapListView.Appearance.ItemSelectedFill.Color := APrimaryColor;
    ABitmapListView.Appearance.ItemSelectedFill.ColorTo := EditorDarkerShadeColor(APrimaryColor);

    ABitmapListView.Appearance.ItemSelectedFont.Color := gcWhite;

    ABitmapListView.Appearance.ItemFill.Color := ABackGroundColor;
    ABitmapListView.Appearance.ItemFont.Color := fc;

    {$IFNDEF VCLLIB}
    ABitmapListView.Appearance.Fill.Kind := gfkNone;
    ABitmapListView.Appearance.Stroke.Kind := gskNone;
    {$ENDIF}
    {$IFDEF VCLLIB}
     ABitmapListView.Appearance.Fill.Kind := gfkSolid;
    ABitmapListView.Appearance.Stroke.Kind := gskSolid;

    if ABackGroundColor = EDITORAUTOCOLOR then
    begin
      if IsLightTheme then
      begin
        ABitmapListView.Appearance.Fill.Color := EDITORSUBBACKCOLORLIGHT;
        ABitmapListView.Appearance.Stroke.Color := EDITORSUBBACKCOLORLIGHT;
      end
      else
      begin
        ABitmapListView.Appearance.Fill.Color := EDITORSUBBACKCOLORDARK;
        ABitmapListView.Appearance.Stroke.Color := EDITORSUBBACKCOLORDARK;
      end;
    end
    else
    begin
      ABitmapListView.Appearance.Fill.Color := ABackGroundColor;
      ABitmapListView.Appearance.Stroke.Color := ABackGroundColor;
    end;
    {$ENDIF}
  end;
end;

procedure SetEditorListAppearance(AEditorList: TTMSFNCEditorList; APrimaryColor: TTMSFNCGraphicsColor; ARounding: integer; AFontColor: TTMSFNCGraphicsColor; ABackGroundColor: TTMSFNCGraphicsColor);
var
  dc, fc: TTMSFNCGraphicsColor;
begin
  if Assigned(AEditorList) then
  begin
    if AFontColor = EDITORAUTOCOLOR then
    begin
      if IsLightTheme then
        fc := EDITORFONTCOLORLIGHT
      else
        fc := EDITORFONTCOLORDARK;
    end
    else
      fc := AFontColor;

    AEditorList.Appearance.ItemVerticalSpacing := ARounding;
    AEditorList.Appearance.ItemHorizontalSpacing := ARounding;

    AEditorList.Appearance.ItemRounding := ARounding;
    AEditorList.Appearance.ItemImageRounding := ARounding;

    AEditorList.Appearance.ItemStroke.Kind := gskNone;
    AEditorList.Appearance.ItemHoverFill.Kind := gfkNone;
    AEditorList.Appearance.ItemHoverStroke.Color := APrimaryColor;
    AEditorList.Appearance.ItemHoverFont.Color := APrimaryColor;

    AEditorList.Appearance.ItemDownStroke.Color := APrimaryColor;
    dc := EditorLighterShadeColor(APrimaryColor);
    AEditorList.Appearance.ItemDownFill.Color := dc;
    AEditorList.Appearance.ItemSelectedStroke.Color := EditorLighterShadeColor(dc);

    AEditorList.Appearance.ItemSelectedFill.Kind := gfkGradient;
    AEditorList.Appearance.ItemSelectedFill.Color := APrimaryColor;
    AEditorList.Appearance.ItemSelectedFill.ColorTo := EditorDarkerShadeColor(APrimaryColor);

    AEditorList.Appearance.ItemSelectedFont.Color := gcWhite;

    AEditorList.Appearance.ItemFill.Color := ABackGroundColor;
    AEditorList.Appearance.ItemFont.Color := fc;

    {$IFNDEF VCLLIB}
    AEditorList.Appearance.Fill.Kind := gfkNone;
    AEditorList.Appearance.Stroke.Kind := gskNone;
    {$ENDIF}
    {$IFDEF VCLLIB}
     AEditorList.Appearance.Fill.Kind := gfkSolid;
    AEditorList.Appearance.Stroke.Kind := gskSolid;

    if ABackGroundColor = EDITORAUTOCOLOR then
    begin
      if IsLightTheme then
      begin
        AEditorList.Appearance.Fill.Color := EDITORSUBBACKCOLORLIGHT;
        AEditorList.Appearance.Stroke.Color := EDITORSUBBACKCOLORLIGHT;
      end
      else
      begin
        AEditorList.Appearance.Fill.Color := EDITORSUBBACKCOLORDARK;
        AEditorList.Appearance.Stroke.Color := EDITORSUBBACKCOLORDARK;
      end;
    end
    else
    begin
      AEditorList.Appearance.Fill.Color := ABackGroundColor;
      AEditorList.Appearance.Stroke.Color := ABackGroundColor;
    end;
    {$ENDIF}
  end;
end;

procedure SetEditorIconButtonAppearance(AButton: TTMSFNCEditorButton; APrimaryColor: TTMSFNCGraphicsColor; ARounding: Integer; AFontColor: TTMSFNCGraphicsColor; ABackGroundColor: TTMSFNCGraphicsColor);
var
  fc: TTMSFNCGraphicsColor;
begin
  if Assigned(AButton) then
  begin
    if AFontColor = EDITORAUTOCOLOR then
    begin
      if IsLightTheme then
        fc := EDITORFONTCOLORLIGHT
      else
        fc := EDITORFONTCOLORDARK;
    end
    else
      fc := AFontColor;
//    AButton.Appearance.Rounding := Round(AButton.Width / 2);
    AButton.Appearance.Rounding := ARounding;
    AButton.Appearance.Stroke.Kind := gskNone;
    AButton.Appearance.DisabledFont.Color := EDITORFONTCOLORDISABLED;
    AButton.Appearance.DisabledStroke.Kind := gskNone;
    AButton.Appearance.DisabledFill.Kind := gfkNone;

    AButton.Appearance.HoverStroke.Color := APrimaryColor;
    AButton.Appearance.DownStroke.Color := APrimaryColor;
    AButton.Appearance.HoverFill.Kind := gfkNone;
    AButton.Appearance.HoverFill. Color:= ABackGroundColor;
    AButton.Appearance.DownFill.Color := EditorDarkerShadeColor(APrimaryColor);

    AButton.Appearance.Fill.Color := ABackGroundColor;

    AButton.Appearance.Font.Color := fc;
    AButton.Appearance.HoverFont.Color := APrimaryColor;


//    if not IsLightTheme then
//    begin
//      AButton.Appearance.Fill.Color := TTMSFNCGraphics.HTMLToColor('#2a2a2c');
//    end
//    else
//    begin
//      AButton.Appearance.Fill.Color := gcWhite;
//    end;
  end;
end;

procedure SetEditorTabButtonAppearance(AButton: TTMSFNCEditorButton; APrimaryColor: TTMSFNCGraphicsColor; ARounding: Integer; AFontColor: TTMSFNCGraphicsColor; ABackGroundColor: TTMSFNCGraphicsColor);
var
  lc: TTMSFNCGraphicsColor;
begin
  AButton.Appearance.SelectionLine := true;
  AButton.Appearance.Stroke.Kind := gskNone;
  AButton.Appearance.HoverStroke.Kind := gskNone;
  AButton.Appearance.DownStroke.Kind := gskNone;

  AButton.Appearance.DisabledFill.Kind := gfkNone;
  AButton.Appearance.DisabledStroke.Kind := gskNone;

  AButton.Appearance.HoverFont.Color := APrimaryColor;
  AButton.Appearance.HoverFill.Color := APrimaryColor;

  lc := EditorLighterShadeColor(APrimaryColor, 20);
  AButton.Appearance.DownFont.Color := lc;
  AButton.Appearance.DownFill.Color := lc;

  AButton.Appearance.SelectedFont.Color := lc;
  AButton.Appearance.SelectedFill.Color := lc;
  AButton.Appearance.SelectedStroke.Kind := gskNone;

  AButton.Appearance.DisabledFont.Color := EDITORFONTCOLORDISABLED;

  AButton.Appearance.Rounding := ARounding;

  if AFontColor = EDITORAUTOCOLOR then
  begin
    if IsLightTheme then
      AButton.Appearance.Font.Color := EDITORFONTCOLORLIGHT
    else
      AButton.Appearance.Font.Color := EDITORFONTCOLORDARK;
  end
  else
  begin
    AButton.Appearance.Font.Color := AFontColor;
  end;

  if ABackGroundColor = EDITORAUTOCOLOR then
  begin
    if IsLightTheme then
      AButton.Appearance.Fill.Color := EDITORMAINBACKCOLORLIGHT
    else
      AButton.Appearance.Fill.Color := EDITORMAINBACKCOLORDARK;
  end
  else
  begin
    AButton.Appearance.Fill.Color := ABackGroundColor;
  end;
end;

procedure SetEditorSubTabButtonAppearance(AButton: TTMSFNCEditorButton; APrimaryColor: TTMSFNCGraphicsColor; ARounding: Integer; AFontColor: TTMSFNCGraphicsColor; ABackGroundColor: TTMSFNCGraphicsColor);
var
  bc: TTMSFNCGraphicsColor;
begin
  if ABackGroundColor = EDITORAUTOCOLOR then
  begin
    if IsLightTheme then
      bc := EDITORSUBBACKCOLORLIGHT
    else
      bc := EDITORSUBBACKCOLORDARK;

    SetEditorTabButtonAppearance(AButton, APrimaryColor, ARounding, AFontColor, bc);
  end
  else
    SetEditorTabButtonAppearance(AButton, APrimaryColor, ARounding, AFontColor, ABackGroundColor);
end;

procedure SetEditorBackPanelAppearance(APanel: TTMSFNCEditorPanel; ABackGroundColor: TTMSFNCGraphicsColor; AStrokeColor: TTMSFNCGraphicsColor; ARounding: integer);
begin
  if Assigned(APanel) then
  begin
    APanel.Appearance.Rounding := ARounding;

    if ABackGroundColor = EDITORAUTOCOLOR then
    begin
      if IsLightTheme then
      begin
        APanel.Appearance.Fill.Color := EDITORMAINBACKCOLORLIGHT;
      end
      else
      begin
        APanel.Appearance.Fill.Color := EDITORMAINBACKCOLORDARK;
      end;
    end;

    if AStrokeColor = EDITORAUTOCOLOR then
    begin
      if IsLightTheme then
        APanel.Appearance.Stroke.Color := EDITORSTROKECOLORLIGHT
      else
        APanel.Appearance.Stroke.Color := EDITORSTROKECOLORDARK;
    end
    else
      APanel.Appearance.Stroke.Color := AStrokeColor;
  end;
end;

procedure SetEditorSubPanelAppearance(APanel: TTMSFNCEditorPanel; ABackGroundColor: TTMSFNCGraphicsColor; AStrokeColor: TTMSFNCGraphicsColor; ARounding: integer);
begin
  if Assigned(APanel) then
  begin
    APanel.Appearance.Rounding := ARounding;

    if ABackGroundColor = EDITORAUTOCOLOR then
    begin
      if IsLightTheme then
        APanel.Appearance.Fill.Color := EDITORSUBBACKCOLORLIGHT
      else
        APanel.Appearance.Fill.Color := EDITORSUBBACKCOLORDARK;
    end
    else
      APanel.Appearance.Fill.Color := ABackGroundColor;

    if AStrokeColor = EDITORAUTOCOLOR then
    begin
      if IsLightTheme then
        APanel.Appearance.Stroke.Color := EDITORSUBBACKCOLORLIGHT
      else
        APanel.Appearance.Stroke.Color := EDITORSUBBACKCOLORDARK;
    end
    else
      APanel.Appearance.Stroke.Color := AStrokeColor;
  end;
end;

procedure SetEditorLabelAppearance(ALabel: TLabel; AFontColor: TTMSFNCGraphicsColor = EDITORAUTOCOLOR);
begin
  if Assigned(ALabel) then
  begin
    {$IFDEF FMXLIB}
    ALabel.StyledSettings := ALabel.StyledSettings - [TStyledSetting.FontColor];
    {$ENDIF}
    if AFontColor = EDITORAUTOCOLOR then
    begin
      if IsLightTheme then
      begin
        {$IFDEF CMNWEBLIB}
        ALabel.Font.Color := EDITORFONTCOLORLIGHT;
        {$ENDIF}
        {$IFDEF FMXLIB}
        ALabel.TextSettings.FontColor := EDITORFONTCOLORLIGHT;
        {$ENDIF}
      end
      else
      begin
        {$IFDEF CMNWEBLIB}
        ALabel.Font.Color := EDITORFONTCOLORDARK;
        {$ENDIF}
        {$IFDEF FMXLIB}
        ALabel.TextSettings.FontColor := EDITORFONTCOLORDARK;
        {$ENDIF}
      end;
    end
    else
    begin
      {$IFDEF CMNWEBLIB}
      ALabel.Font.Color := EDITORFONTCOLORDARK;
      {$ENDIF}
      {$IFDEF FMXLIB}
      ALabel.TextSettings.FontColor := AFontColor;
      {$ENDIF}
    end;
  end;
end;

procedure SetEditorCheckBoxAppearance(ACheckBox: TCheckBox; AFontColor: TTMSFNCGraphicsColor = EDITORAUTOCOLOR);
begin
  if Assigned(ACheckBox) then
  begin
    {$IFDEF FMXLIB}
    ACheckBox.StyledSettings := ACheckBox.StyledSettings - [TStyledSetting.FontColor];
    {$ENDIF}
    if AFontColor = EDITORAUTOCOLOR then
    begin
      if IsLightTheme then
      begin
        {$IFDEF CMNWEBLIB}
        ACheckBox.Font.Color := EDITORFONTCOLORLIGHT;
        {$ENDIF}
        {$IFDEF FMXLIB}
        ACheckBox.TextSettings.FontColor := EDITORFONTCOLORLIGHT;
        {$ENDIF}
      end
      else
      begin
        {$IFDEF CMNWEBLIB}
        ACheckBox.Font.Color := EDITORFONTCOLORDARK;
        {$ENDIF}
        {$IFDEF FMXLIB}
        ACheckBox.TextSettings.FontColor := EDITORFONTCOLORDARK;
        {$ENDIF}
      end;
    end
    else
    begin
      {$IFDEF CMNWEBLIB}
      ACheckBox.Font.Color := AFontColor;
      {$ENDIF}
      {$IFDEF FMXLIB}
      ACheckBox.TextSettings.FontColor := AFontColor;
      {$ENDIF}
    end;
  end;
end;

procedure SetEditorEditAppearance(AEdit: TEdit; AFontColor: TTMSFNCGraphicsColor = EDITORAUTOCOLOR; ABackGroundColor: TTMSFNCGraphicsColor = EDITORAUTOCOLOR; AStrokeColor: TTMSFNCGraphicsColor = EDITORAUTOCOLOR);
var
  fc, bc: TTMSFNCGraphicsColor;
  {$IFDEF FMXLIB}
  sc: TTMSFNCGraphicsColor;
  r: TRectangle;
  b: TControl;
  {$ENDIF}
begin
  if AFontColor = EDITORAUTOCOLOR then
  begin
    if IsLightTheme then
      fc := EDITORFONTCOLORLIGHT
    else
      fc := EDITORFONTCOLORDARK
  end
  else
    fc := AFontColor;

  if ABackGroundColor = EDITORAUTOCOLOR then
  begin
    if IsLightTheme then
      bc := EDITORMAINBACKCOLORLIGHT
    else
      bc := EDITORMAINBACKCOLORDARK;
  end
  else
    bc := ABackGroundColor;

  {$IFDEF FMXLIB}
  if AStrokeColor = EDITORAUTOCOLOR then
  begin
    if IsLightTheme then
      sc := EDITORFONTCOLORDARK
    else
      sc := EDITORSTROKECOLORDARK;
  end
  else
    sc := AStrokeColor;

  AEdit.NeedStyleLookup;
  AEdit.ApplyStyleLookup;

  b := (AEdit.FindStyleResource('background') as TControl);

  AEdit.StyledSettings := AEdit.StyledSettings - [TStyledSetting.FontColor];
  AEdit.FontColor := fc;

  r := TRectangle.Create(b);
  r.Align := TAlignLayout.Client;
  r.HitTest := False;
  r.Parent := b;
  r.Fill.Color := bc;
  r.Stroke.Color := sc;
  {$ENDIF}
  {$IFNDEF FMXLIB}
  AEdit.Color := bc;
  AEdit.Font.Color := fc;
  {$ENDIF}
end;

procedure SetEditorMemoAppearance(AMemo: TMemo; AFontColor: TTMSFNCGraphicsColor = EDITORAUTOCOLOR; ABackGroundColor: TTMSFNCGraphicsColor = EDITORAUTOCOLOR; AStrokeColor: TTMSFNCGraphicsColor = EDITORAUTOCOLOR);
var
  fc, bc: TTMSFNCGraphicsColor;
  {$IFDEF FMXLIB}
  sc: TTMSFNCGraphicsColor;
  r: TRectangle;
  b: TControl;
  {$ENDIF}
begin
  if AFontColor = EDITORAUTOCOLOR then
  begin
    if IsLightTheme then
      fc := EDITORFONTCOLORLIGHT
    else
      fc := EDITORFONTCOLORDARK
  end
  else
    fc := AFontColor;

  if ABackGroundColor = EDITORAUTOCOLOR then
  begin
    if IsLightTheme then
      bc := EDITORMAINBACKCOLORLIGHT
    else
      bc := EDITORMAINBACKCOLORDARK;
  end
  else
    bc := ABackGroundColor;

  {$IFDEF FMXLIB}
  if AStrokeColor = EDITORAUTOCOLOR then
  begin
    if IsLightTheme then
      sc := EDITORFONTCOLORDARK
    else
      sc := EDITORSTROKECOLORDARK;
  end
  else
    sc := AStrokeColor;

  AMemo.NeedStyleLookup;
  AMemo.ApplyStyleLookup;

  b := (AMemo.FindStyleResource('background') as TControl);

  AMemo.StyledSettings := AMemo.StyledSettings - [TStyledSetting.FontColor];
  AMemo.FontColor := fc;

  r := TRectangle.Create(b);
  r.Align := TAlignLayout.Client;
  r.HitTest := False;
  r.Parent := b;
  r.Fill.Color := bc;
  r.Stroke.Color := sc;
  {$ENDIF}
  {$IFNDEF FMXLIB}
  AMemo.Color := bc;
  AMemo.Font.Color := fc;
  {$ENDIF}
end;

end.

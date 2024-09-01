{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{            copyright (c) 2016 - 2021                               }
{            Email : info@tmssoftware.com                            }
{            Web : http://www.tmssoftware.com                        }
{                                                                    }
{ The source code is given as is. The author is not responsible      }
{ for any possible damage done due to the use of this code.          }
{ The complete source code remains property of the author and may    }
{ not be distributed, published, given or sold in any form as such.  }
{ No parts of the source code can be included in any other component }
{ or application without written authorization of the author.        }
{********************************************************************}

unit VCL.TMSFNCStyles;

{$I VCL.TMSFNCDefines.inc}

{$IFDEF VCLLIB}
{$HINTS OFF}
{$IF COMPILERVERSION > 22}
{$DEFINE VCLSTYLESENABLED}
{$IFEND}
{$ENDIF}

interface

uses
  Classes, VCL.Graphics, VCL.Controls, VCL.Forms, VCL.TMSFNCTypes, VCL.TMSFNCCustomComponent, VCL.TMSFNCGraphicsTypes
  {$IFDEF FMXLIB}
  ,FMX.Types, UITypes
  {$ENDIF}
  {$IFDEF VCLSTYLESENABLED}
  ,VCL.Themes
  {$ENDIF}
  {$IFDEF WEBLIB}
  ,js
  {$ENDIF}
  {$IFDEF FNCLIB}
  {$IFDEF WEBLIB}
  ,WEBLIB.JSON
  {$ENDIF}
  {$IFNDEF WEBLIB}
  {$IFNDEF LCLLIB}
  {$HINTS OFF}
  {$IF COMPILERVERSION > 26}
  ,JSON
  {$ELSE}
  ,DBXJSON
  {$IFEND}
  {$HINTS ON}
  {$ELSE}
  ,fpjson, jsparser
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
  ;

type
  ITMSFNCAdaptToStyle = interface
  ['{3EFF288D-3927-4E86-8E9D-EF684B501C9E}']
    function GetAdaptToStyle: Boolean;
    procedure SetAdaptToStyle(const Value: Boolean);
    property AdaptToStyle: Boolean read GetAdaptToStyle write SetAdaptToStyle;
  end;

  TTMSFNCStyles = class
  private class var
    {$IFDEF FMXLIB}
    FScene: IScene;
    {$ENDIF}
  protected
    {$IFDEF FMXLIB}
    class function ExtractColor(ABitmap: TBitmap): TTMSFNCGraphicsColor; virtual;
    class function ExtractColorTo(ABitmap: TBitmap): TTMSFNCGraphicsColor; virtual;
    {$ENDIF}
    {$IFDEF WEBLIB}
    class function ExtractColor(AValue: string): TTMSFNCGraphicsColor; virtual;
    class function ExtractColorTo(AValue: string): TTMSFNCGraphicsColor; virtual;
    class function ExtractFontName(AValue: string): string; virtual;
    {$ENDIF}
    {$IFDEF VCLSTYLESENABLED}
    class function ExtractColor(AElement: TThemedElementDetails): TTMSFNCGraphicsColor; virtual;
    class function ExtractColorTo(AElement: TThemedElementDetails): TTMSFNCGraphicsColor; virtual;
    {$ENDIF}
    {$IFDEF FMXLIB}
    class function ParseBrush(ABrush: TBrush; ASecondColor: Boolean): TTMSFNCGraphicsColor; virtual;
    class function GetStyleSceneObject: TFmxObject; virtual;
    class function GetStyleBackgroundFill(AStyle: TFmxObject): TBrush; virtual;
    class function GetStyleBackgroundStroke(AStyle: TFmxObject): TStrokeBrush; virtual;
    class function GetStyleDefaultButtonFill(AStyle: TFmxObject): TBrush; virtual;
    class function GetStyleDefaultButtonStroke(AStyle: TFmxObject): TStrokeBrush; virtual;
    class function GetStyleEditTextColor(AStyle: TFmxObject): TAlphaColor; virtual;
    class function GetStyleEditFill(AStyle: TFmxObject): TBrush; virtual;
    class function GetStyleEditStroke(AStyle: TFmxObject): TStrokeBrush; virtual;
    class function GetStyleHeaderFill(AStyle: TFmxObject): TBrush; virtual;
    class function GetStyleHeaderStroke(AStyle: TFmxObject): TStrokeBrush; virtual;
    class function GetStyleSelectionFill(AStyle: TFmxObject): TBrush; virtual;
    class function GetStyleAlternativeSelectionFill(AStyle: TFmxObject): TBrush; virtual;
    class function GetStyleAlternativeBackgroundFill(AStyle: TFmxObject): TBrush; virtual;
    class function GetStyleAlternativeBackgroundStroke(AStyle: TFmxObject): TStrokeBrush; virtual;
    class function GetStyleFocusFill(AStyle: TFmxObject): TBrush; virtual;
    class function GetStyleLineFill(AStyle: TFmxObject): TBrush; virtual;
    class function GetStyleTextFont(AStyle: TFmxObject): TFont; virtual;
    class function GetStyleTextColor(AStyle: TFmxObject): TAlphaColor; virtual;
    class function GetStyleAlternateBackgroundFill(AStyle: TFmxObject): TBrush; virtual;
    class function IsTransparentStyle(AStyle: TFmxObject): Boolean; virtual;
    {$ENDIF}
    {$IFDEF WEBLIB}
    class function GetStyleBackgroundFill: string; virtual;
    class function GetStyleBackgroundStroke: string; virtual;
    class function GetStyleDefaultButtonFill: string; virtual;
    class function GetStyleDefaultButtonStroke: string; virtual;
    class function GetStyleEditTextColor: string; virtual;
    class function GetStyleEditFill: string; virtual;
    class function GetStyleEditStroke: string; virtual;
    class function GetStyleHeaderFill: string; virtual;
    class function GetStyleHeaderStroke: string; virtual;
    class function GetStyleSelectionFill: string; virtual;
    class function GetStyleAlternativeSelectionFill: string; virtual;
    class function GetStyleAlternativeBackgroundFill: string; virtual;
    class function GetStyleAlternativeBackgroundStroke: string; virtual;
    class function GetStyleFocusFill: string; virtual;
    class function GetStyleLineFill: string; virtual;
    class function GetStyleTextFont: string; virtual;
    class function GetStyleTextColor: string; virtual;
    class function GetStyleAlternateBackgroundFill: string; virtual;
    {$ENDIF}
  public
    {$IFDEF FMXLIB}
    class procedure SetActiveScene(AScene: IScene); virtual;
    {$ENDIF}
    {$IFDEF WEBLIB}
    class function FindCSSStyleRule(ARuleName: string): TJSObject; virtual;
    class function FindCSSStyleProperty(ARuleName, APropertyName: string): string; virtual;
    {$ENDIF}
    class function StyleServicesEnabled: Boolean; virtual;
    class function GetStyleLineFillColor(var {%H-}AColor: TTMSFNCGraphicsColor): Boolean; virtual;
    class function GetStyleBackgroundFillColor(var {%H-}AColor: TTMSFNCGraphicsColor): Boolean; virtual;
    class function GetStyleBackgroundFillColorTo(var {%H-}AColor: TTMSFNCGraphicsColor): Boolean; virtual;
    class function GetStyleAlternativeBackgroundFillColor(var {%H-}AColor: TTMSFNCGraphicsColor): Boolean; virtual;
    class function GetStyleAlternativeBackgroundFillColorTo(var {%H-}AColor: TTMSFNCGraphicsColor): Boolean; virtual;
    class function GetStyleBackgroundStrokeColor(var {%H-}AColor: TTMSFNCGraphicsColor): Boolean; virtual;
    class function GetStyleHeaderFillColor(var {%H-}AColor: TTMSFNCGraphicsColor): Boolean; virtual;
    class function GetStyleHeaderFillColorTo(var {%H-}AColor: TTMSFNCGraphicsColor): Boolean; virtual;
    class function GetStyleHeaderStrokeColor(var {%H-}AColor: TTMSFNCGraphicsColor): Boolean; virtual;
    class function GetStyleSelectionFillColor(var {%H-}AColor: TTMSFNCGraphicsColor): Boolean; virtual;
    class function GetStyleSelectionFillColorTo(var {%H-}AColor: TTMSFNCGraphicsColor): Boolean; virtual;
    class function GetStyleTextFontColor(var {%H-}AColor: TTMSFNCGraphicsColor): Boolean; virtual;
    class function GetStyleAlternativeTextFontColor(var {%H-}AColor: TTMSFNCGraphicsColor): Boolean; virtual;
    class function GetStyleDefaultButtonFillColor(var {%H-}AColor: TTMSFNCGraphicsColor): Boolean; virtual;
    class function GetStyleDefaultButtonStrokeColor(var {%H-}AColor: TTMSFNCGraphicsColor): Boolean; virtual;
  end;

  {$IFDEF FNCLIB}
  TTMSFNCStylesManagerCanLoadStyleEvent = procedure(Sender: TObject; AStyle: string; AComponent: TComponent; var ACanLoadStyle: Boolean) of object;
  TTMSFNCStylesManagerStyleLoadedEvent = procedure(Sender: TObject; AStyle: string; AComponent: TComponent) of object;

  TTMSFNCStylesManagerFileArray = array of string;

  TTMSFNCStylesManagerComponentArray = array of TComponent;

  ITMSFNCStylesManager = interface
    ['{88852C7F-B7B5-4FFA-BB47-6D95600CB1F3}']
    function GetSubComponentArray: TTMSFNCStylesManagerComponentArray;
  end;

  TTMSFNCStylesManagerOptions = class(TPersistent)
  private
    FAdaptFormColor: Boolean;
  public
    constructor Create; virtual;
  published
    property AdaptFormColor: Boolean read FAdaptFormColor write FAdaptFormColor default True;
  end;

  {$IFNDEF LCLLIB}
  [ComponentPlatformsAttribute(TMSPlatformsWeb)]
  {$ENDIF}
  TTMSFNCStylesManager = class(TTMSFNCCustomComponent)
  private
    {$IFDEF WEBLIB}
    FComponents: TTMSFNCStylesManagerComponentArray;
    {$ENDIF}
    FStyle: string;
    FStyleResource: string;
    FStyleForm: TCustomForm;
    FOnCanLoadStyle: TTMSFNCStylesManagerCanLoadStyleEvent;
    FOnStyleLoaded: TTMSFNCStylesManagerStyleLoadedEvent;
    FOptions: TTMSFNCStylesManagerOptions;
    procedure SetStyle(const Value: string);
    procedure SetStyleResource(const Value: string);
    procedure SetOptions(const Value: TTMSFNCStylesManagerOptions);
  protected
    function GetDocURL: string; override;
    procedure InternalLoadStyleFromJSONValue(AJSONValue: TJSONValue; AComponents: TTMSFNCStylesManagerComponentArray);
    procedure InternalLoadStyle(AValue: string; AComponents: TTMSFNCStylesManagerComponentArray);
    procedure DoCanLoadStyle(AStyle: string; AComponent: TComponent; var ACanLoadStyle: Boolean); virtual;
    procedure DoStyleLoaded(AStyle: string; AComponent: TComponent); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadStyleFromText(AText: string); overload; virtual;
    procedure LoadStyleFromStream(AStream: TStream); overload; virtual;
    procedure LoadStyleFromFile(AFile: string); overload; virtual;
    procedure LoadStyleFromText(AText: string; AComponents: TTMSFNCStylesManagerComponentArray); overload; virtual;
    procedure LoadStyleFromStream(AStream: TStream; AComponents: TTMSFNCStylesManagerComponentArray); overload; virtual;
    procedure LoadStyleFromFile(AFile: string; AComponents: TTMSFNCStylesManagerComponentArray); overload; virtual;
    {$IFNDEF WEBLIB}
    procedure LoadStyleFromResource(AResourceName: string; AComponents: TTMSFNCStylesManagerComponentArray); overload; virtual;
    procedure LoadStyleFromResource(AResourceName: string); overload; virtual;
    function GetStyleFromResource(AResourceName: string): string; virtual;
    function GetStyleFromFile(AFile: string): string; virtual;
    function CombineStyles(AFiles: TTMSFNCStylesManagerFileArray): string; virtual;
    {$ELSE}
    procedure LoadStyleFromURL(AURL: string; AComponents: TTMSFNCStylesManagerComponentArray); overload; virtual;
    procedure LoadStyleFromURL(AURL: string); overload; virtual;
    {$ENDIF}
    property StyleForm: TCustomForm read FStyleForm write FStyleForm;
  published
    property Options: TTMSFNCStylesManagerOptions read FOptions write SetOptions;
    property Style: string read FStyle write SetStyle;
    property StyleResource: string read FStyleResource write SetStyleResource;
    property OnCanLoadStyle: TTMSFNCStylesManagerCanLoadStyleEvent read FOnCanLoadStyle write FOnCanLoadStyle;
    property OnStyleLoaded: TTMSFNCStylesManagerStyleLoadedEvent read FOnStyleLoaded write FOnStyleLoaded;
  end;
  {$ENDIF}

var
  CSSStyleFileName: string = '';

implementation

uses
  VCL.TMSFNCUtils, SysUtils, VCL.TMSFNCPersistence, VCL.TMSFNCGraphics
  {$IFDEF FMXLIB}
  ,UIConsts, FMX.Objects, FMX.Styles, FMX.Styles.Objects, Types
  {$ENDIF}
  ;

{$IFDEF FNCLIB}
{$R 'TMSFNCStyles.res'}
{$ENDIF}

{$IFDEF FMXLIB}

class function TTMSFNCStyles.ParseBrush(ABrush: TBrush; ASecondColor: Boolean): TTMSFNCGraphicsColor;
begin
  case ABrush.Kind of
    TBrushKind.None, TBrushKind.Resource: Result := gcNull;
    TBrushKind.Solid: Result := ABrush.Color;
    TBrushKind.Gradient:
    begin
      if ABrush.Gradient.Points.Count > 0 then
      begin
        if ASecondColor then
          Result := ABrush.Gradient.Points[1].Color
        else
          Result := ABrush.Gradient.Points[0].Color;
      end
      else
        Result := ABrush.Color;
    end;
    TBrushKind.Bitmap:
    begin
      if ASecondColor then
        Result := ExtractColorTo(ABrush.Bitmap.Bitmap)
      else
        Result := ExtractColor(ABrush.Bitmap.Bitmap);
    end;
    else
      Result := gcNull;
  end;
end;

class function TTMSFNCStyles.GetStyleSceneObject: TFmxObject;
var
  frm: TCommonCustomForm;
begin
  if Assigned(TTMSFNCStyles.FScene) and Assigned(TTMSFNCStyles.FScene.GetObject) and (TTMSFNCStyles.FScene.GetObject.ClassName <> 'TTMSFNCCustomPopupForm') then
  begin
    if Assigned(TTMSFNCStyles.FScene.StyleBook) then
      Result := TTMSFNCStyles.FScene.StyleBook.Style
    else
      Result := TStyleManager.ActiveStyleForScene(TTMSFNCStyles.FScene);
  end
  else
  begin
    frm := Screen.ActiveForm;
    if not Assigned(frm) then
      frm := Application.MainForm;

    if Assigned(frm) and Assigned(frm.StyleBook) then
      Result := frm.StyleBook.Style
    else
      Result := TStyleManager.ActiveStyle(nil);
  end;
end;

class function TTMSFNCStyles.IsTransparentStyle(
  AStyle: TFmxObject): Boolean;
var
  st: TStyleDescription;
begin
  Result := False;
  st := TStyleManager.FindStyleDescriptor(AStyle);
  if Assigned(st) then
    Result := LowerCase(st.Title) = 'transparent';
end;

class function TTMSFNCStyles.GetStyleAlternateBackgroundFill(
  AStyle: TFmxObject): TBrush;
var
  st, stobj: TFmxObject;
  f: TBrush;
begin
  f := TBrush.Create(TBrushKind.Solid, claNull);
  Result := f;
  if Assigned(AStyle) then
  begin
    st := AStyle.FindStyleResource('gridstyle');
    if Assigned(st) then
    begin
      stobj := st.FindStyleResource('alternatingrowbackground');
      if (Assigned(stobj) and (stobj is TBrushObject)) then
        f.Assign((stobj as TBrushObject).Brush);
    end;
  end;
end;

class function TTMSFNCStyles.GetStyleAlternativeSelectionFill(
  AStyle: TFmxObject): TBrush;
var
  st, stobj: TFmxObject;
  f: TBrush;
  bmp: TBitmap;
begin
  f := TBrush.Create(TBrushKind.Solid, claNull);
  Result := f;
  if Assigned(AStyle) then
  begin
    st := AStyle.FindStyleResource('memostyle');
    if Assigned(st) then
    begin
      stobj := st.FindStyleResource('selection');
      if Assigned(stobj) then
      begin
        if (stobj is TRectangle) then
          f.Assign((stobj as TRectangle).Fill)
        else if (stobj is TBrushObject) then
          f.Assign((stobj as TBrushObject).Brush)
        else if stobj is TCustomStyleObject then
        begin
          f.Kind := TBrushKind.Bitmap;
          f.Bitmap.WrapMode := TWrapMode.TileStretch;
          bmp := TBitmap.Create(Round(200), Round(200));
          if bmp.Canvas.BeginScene then
          begin
            (stobj as TCustomStyleObject).DrawToCanvas(bmp.Canvas, RectF(-10, -10, bmp.Width + 10, bmp.Height + 10));
            bmp.Canvas.EndScene;
          end;
          f.Bitmap.Bitmap.Assign(bmp);
          bmp.Free;
        end;
      end;
    end;
  end;
end;

class function TTMSFNCStyles.GetStyleDefaultButtonFill(
  AStyle: TFmxObject): TBrush;
var
  st, stobj: TFmxObject;
  f: TBrush;
  bmp: TBitmap;
begin
  f := TBrush.Create(TBrushKind.Solid, claNull);
  Result := f;
  if Assigned(AStyle) then
  begin
    st := AStyle.FindStyleResource('buttonstyle');
    if Assigned(st) then
    begin
      stobj := st.FindStyleResource('background');
      if Assigned(stobj) then
      begin
        if (stobj is TRectangle) then
          f.Assign((stobj as TRectangle).Fill)
        else if stobj is TCustomStyleObject then
        begin
          f.Kind := TBrushKind.Bitmap;
          f.Bitmap.WrapMode := TWrapMode.TileStretch;
          bmp := TBitmap.Create(Round(200), Round(200));
          if bmp.Canvas.BeginScene then
          begin
            (stobj as TCustomStyleObject).DrawToCanvas(bmp.Canvas, RectF(-10, -10, bmp.Width + 10, bmp.Height + 10));
            bmp.Canvas.EndScene;
          end;
          f.Bitmap.Bitmap.Assign(bmp);
          bmp.Free;
        end;
      end;
    end;
  end;
end;

class function TTMSFNCStyles.GetStyleBackgroundFill(
  AStyle: TFmxObject): TBrush;
var
  st, stobj: TFmxObject;
  f: TBrush;
  bmp: TBitmap;
begin
  f := TBrush.Create(TBrushKind.Solid, claNull);
  Result := f;
  if Assigned(AStyle) then
  begin
    st := AStyle.FindStyleResource('gridstyle');
    if Assigned(st) then
    begin
      stobj := st.FindStyleResource('background');
      if Assigned(stobj) then
      begin
        if (stobj is TRectangle) then
          f.Assign((stobj as TRectangle).Fill)
        else if stobj is TCustomStyleObject then
        begin
          f.Kind := TBrushKind.Bitmap;
          f.Bitmap.WrapMode := TWrapMode.TileStretch;
          bmp := TBitmap.Create(Round(200), Round(200));
          if bmp.Canvas.BeginScene then
          begin
            (stobj as TCustomStyleObject).DrawToCanvas(bmp.Canvas, RectF(-10, -10, bmp.Width + 10, bmp.Height + 10));
            bmp.Canvas.EndScene;
          end;
          f.Bitmap.Bitmap.Assign(bmp);
          bmp.Free;
        end;
      end;
    end;
  end;
end;

class function TTMSFNCStyles.GetStyleDefaultButtonStroke(
  AStyle: TFmxObject): TStrokeBrush;
var
  st, stobj: TFmxObject;
  s: TStrokeBrush;
begin
  s := TStrokeBrush.Create(TBrushKind.Solid, claDarkGray);
  Result := s;
  if Assigned(AStyle) then
  begin
    st := AStyle.FindStyleResource('buttonstyle');
    if Assigned(st) then
    begin
      stobj := st.FindStyleResource('background');
      if Assigned(stobj) then
      begin
        if (stobj is TRectangle) then
          s.Assign((stobj as TRectangle).Stroke);
      end;
    end;
  end;
end;

class function TTMSFNCStyles.GetStyleBackgroundStroke(
  AStyle: TFmxObject): TStrokeBrush;
var
  st, stobj: TFmxObject;
  s: TStrokeBrush;
begin
  s := TStrokeBrush.Create(TBrushKind.Solid, claDarkGray);
  Result := s;
  if Assigned(AStyle) then
  begin
    st := AStyle.FindStyleResource('gridstyle');
    if Assigned(st) then
    begin
      stobj := st.FindStyleResource('background');
      if Assigned(stobj) then
      begin
        if (stobj is TRectangle) then
          s.Assign((stobj as TRectangle).Stroke);
      end;
    end;
  end;
end;

class function TTMSFNCStyles.GetStyleAlternativeBackgroundFill(
  AStyle: TFmxObject): TBrush;
var
  st, stobj: TFmxObject;
  f: TBrush;
  bmp: TBitmap;
begin
  f := TBrush.Create(TBrushKind.Solid, claNull);
  Result := f;
  if Assigned(AStyle) then
  begin
    st := AStyle.FindStyleResource('memostyle');
    if Assigned(st) then
    begin
      stobj := st.FindStyleResource('background');
      if Assigned(stobj) then
      begin
        if (stobj is TRectangle) then
          f.Assign((stobj as TRectangle).Fill)
        else if stobj is TCustomStyleObject then
        begin
          f.Kind := TBrushKind.Bitmap;
          f.Bitmap.WrapMode := TWrapMode.TileStretch;
          bmp := TBitmap.Create(Round(200), Round(200));
          if bmp.Canvas.BeginScene then
          begin
            (stobj as TCustomStyleObject).DrawToCanvas(bmp.Canvas, RectF(-10, -10, bmp.Width + 10, bmp.Height + 10));
            bmp.Canvas.EndScene;
          end;
          f.Bitmap.Bitmap.Assign(bmp);
          bmp.Free;
        end;
      end;
    end;
  end;
end;

class function TTMSFNCStyles.GetStyleAlternativeBackgroundStroke(
  AStyle: TFmxObject): TStrokeBrush;
var
  st, stobj: TFmxObject;
  s: TStrokeBrush;
begin
  s := TStrokeBrush.Create(TBrushKind.Solid, claDarkGray);
  Result := s;
  if Assigned(AStyle) then
  begin
    st := AStyle.FindStyleResource('memostyle');
    if Assigned(st) then
    begin
      stobj := st.FindStyleResource('foreground');
      if Assigned(stobj) then
      begin
        if (stobj is TBrushObject) then
          s.Assign((stobj as TBrushObject).Brush);
      end;
    end;
  end;
end;

class function TTMSFNCStyles.GetStyleEditTextColor(AStyle: TFmxObject): TAlphaColor;
var
  st, stobj: TFMXObject;
begin
  Result := claBlack;
  if Assigned(AStyle) then
  begin
    st := AStyle.FindStyleResource('editstyle');
    if Assigned(st) then
    begin
      stobj := st.FindStyleResource('foreground');
      if (Assigned(stobj) and (stobj is TBrushObject)) then
      begin
        if (Assigned(stobj) and (stobj is TBrushObject)) then
          Result := (stobj as TBrushObject).Brush.Color;
      end;
    end;
  end;
end;

class function TTMSFNCStyles.GetStyleEditFill(
  AStyle: TFmxObject): TBrush;
var
  st, stobj: TFmxObject;
  f: TBrush;
  bmp: TBitmap;
begin
  f := TBrush.Create(TBrushKind.Solid, claNull);
  Result := f;
  if Assigned(AStyle) then
  begin
    st := AStyle.FindStyleResource('editstyle');
    if Assigned(st) then
    begin
      stobj := st.FindStyleResource('background');
      if Assigned(stobj) then
      begin
        if (stobj is TRectangle) then
          f.Assign((stobj as TRectangle).Fill)
        else if stobj is TCustomStyleObject then
        begin
          f.Kind := TBrushKind.Bitmap;
          f.Bitmap.WrapMode := TWrapMode.TileStretch;
          bmp := TBitmap.Create(Round(200), Round(200));
          if bmp.Canvas.BeginScene then
          begin
            (stobj as TCustomStyleObject).DrawToCanvas(bmp.Canvas, RectF(-10, -10, bmp.Width + 10, bmp.Height + 10));
            bmp.Canvas.EndScene;
          end;
          f.Bitmap.Bitmap.Assign(bmp);
          bmp.Free;
        end;
      end;
    end;
  end;
end;

class function TTMSFNCStyles.GetStyleEditStroke(
  AStyle: TFmxObject): TStrokeBrush;
var
  st, stobj: TFmxObject;
  s: TStrokeBrush;
begin
  s := TStrokeBrush.Create(TBrushKind.Solid, claDarkGray);
  Result := s;
  if Assigned(AStyle) then
  begin
    st := AStyle.FindStyleResource('editstyle');
    if Assigned(st) then
    begin
      stobj := st.FindStyleResource('background');
      if Assigned(stobj) then
      begin
        if (stobj is TRectangle) then
          s.Assign((stobj as TRectangle).Stroke);
      end;
    end;
  end;
end;

class function TTMSFNCStyles.GetStyleFocusFill(AStyle: TFmxObject): TBrush;
var
  st, stobj: TFmxObject;
  f: TBrush;
  bmp: TBitmap;
begin
  f := TBrush.Create(TBrushKind.Solid, claNull);
  Result := f;
  if Assigned(AStyle) then
  begin
    st := AStyle.FindStyleResource('gridstyle');
    if Assigned(st) then
    begin
      stobj := st.FindStyleResource('focus');
      if Assigned(stobj) then
      begin
        if (stobj is TRectangle) then
          f.Assign((stobj as TRectangle).Fill)
        else if stobj is TCustomStyleObject then
        begin
          f.Kind := TBrushKind.Bitmap;
          f.Bitmap.WrapMode := TWrapMode.TileStretch;
          bmp := TBitmap.Create(Round(200), Round(200));
          if bmp.Canvas.BeginScene then
          begin
            (stobj as TCustomStyleObject).DrawToCanvas(bmp.Canvas, RectF(-10, -10, bmp.Width + 10, bmp.Height + 10));
            bmp.Canvas.EndScene;
          end;
          f.Bitmap.Bitmap.Assign(bmp);
          bmp.Free;
        end;
      end;
    end;
  end;
end;

class function TTMSFNCStyles.GetStyleHeaderFill(
  AStyle: TFmxObject): TBrush;
var
  st, stobj: TFmxObject;
  f: TBrush;
  bmp: TBitmap;
begin
  f := TBrush.Create(TBrushKind.Solid, claNull);
  Result := f;
  if Assigned(AStyle) then
  begin
    st := AStyle.FindStyleResource('headeritemstyle');
    if Assigned(st) then
    begin
      stobj := st.FindStyleResource('background');
      if Assigned(stobj) then
      begin
        if (stobj is TRectangle) then
          f.Assign((stobj as TRectangle).Fill)
        else if stobj is TButtonStyleObject then
        begin
          f.Kind := TBrushKind.Bitmap;
          f.Bitmap.WrapMode := TWrapMode.TileStretch;
          bmp := TBitmap.Create(Round(200), Round(200));
          if bmp.Canvas.BeginScene then
          begin
            (stobj as TButtonStyleObject).DrawToCanvas(bmp.Canvas, RectF(-10, -10, bmp.Width + 10, bmp.Height + 10));
            bmp.Canvas.EndScene;
          end;
          f.Bitmap.Bitmap.Assign(bmp);
          bmp.Free;
        end;
      end;
    end;
  end;
end;

class function TTMSFNCStyles.GetStyleHeaderStroke(
  AStyle: TFmxObject): TStrokeBrush;
var
  st, stobj: TFmxObject;
  s: TStrokeBrush;
begin
  s := TStrokeBrush.Create(TBrushKind.Solid, claDarkGray);
  Result := s;
  if Assigned(AStyle) then
  begin
    st := AStyle.FindStyleResource('headeritemstyle');
    if Assigned(st) then
    begin
      stobj := st.FindStyleResource('background');
      if Assigned(stobj) then
      begin
        if (stobj is TRectangle) then
          s.Assign((stobj as TRectangle).Stroke);
      end;
    end;
  end;
end;

class function TTMSFNCStyles.GetStyleLineFill(AStyle: TFmxObject): TBrush;
var
  st, stobj: TFmxObject;
  f: TBrush;
  bmp: TBitmap;
begin
  f := TBrush.Create(TBrushKind.Solid, claNull);
  Result := f;
  if Assigned(AStyle) then
  begin
    st := AStyle.FindStyleResource('gridstyle');
    if Assigned(st) then
    begin
      stobj := st.FindStyleResource('linefill');
      if Assigned(stobj) then
      begin
        if (stobj is TBrushObject) then
          f.Assign((stobj as TBrushObject).Brush)
        else if stobj is TCustomStyleObject then
        begin
          f.Kind := TBrushKind.Bitmap;
          f.Bitmap.WrapMode := TWrapMode.TileStretch;
          bmp := TBitmap.Create(Round(200), Round(200));
          if bmp.Canvas.BeginScene then
          begin
            (stobj as TCustomStyleObject).DrawToCanvas(bmp.Canvas, RectF(-10, -10, bmp.Width + 10, bmp.Height + 10));
            bmp.Canvas.EndScene;
          end;
          f.Bitmap.Bitmap.Assign(bmp);
          bmp.Free;
        end;
      end;
    end;
  end;
end;

class function TTMSFNCStyles.GetStyleSelectionFill(
  AStyle: TFmxObject): TBrush;
var
  st, stobj: TFmxObject;
  f: TBrush;
  bmp: TBitmap;
begin
  f := TBrush.Create(TBrushKind.Solid, claNull);
  Result := f;
  if Assigned(AStyle) then
  begin
    st := AStyle.FindStyleResource('gridstyle');
    if Assigned(st) then
    begin
      stobj := st.FindStyleResource('selection');
      if Assigned(stobj) then
      begin
        if (stobj is TRectangle) then
          f.Assign((stobj as TRectangle).Fill)
        else if (stobj is TBrushObject) then
          f.Assign((stobj as TBrushObject).Brush)
        else if stobj is TCustomStyleObject then
        begin
          f.Kind := TBrushKind.Bitmap;
          f.Bitmap.WrapMode := TWrapMode.TileStretch;
          bmp := TBitmap.Create(Round(200), Round(200));
          if bmp.Canvas.BeginScene then
          begin
            (stobj as TCustomStyleObject).DrawToCanvas(bmp.Canvas, RectF(-10, -10, bmp.Width + 10, bmp.Height + 10));
            bmp.Canvas.EndScene;
          end;
          f.Bitmap.Bitmap.Assign(bmp);
          bmp.Free;
        end;
      end;
    end;
  end;
end;

class function TTMSFNCStyles.GetStyleTextColor(
  AStyle: TFmxObject): TAlphaColor;
var
  st, stobj: TFmxObject;
begin
  Result := claBlack;
  if Assigned(AStyle) then
  begin
    st := AStyle.FindStyleResource('headeritemstyle');
    if Assigned(st) then
    begin
      stobj := st.FindStyleResource('text');
      if (Assigned(stobj) and (stobj is TText)) then
        Result := (stobj as TText).TextSettings.FontColor;
    end;
  end;
end;

class function TTMSFNCStyles.GetStyleTextFont(AStyle: TFmxObject): TFont;
var
  st, stobj: TFmxObject;
begin
  Result := TFont.Create;
  if Assigned(AStyle) then
  begin
    st := AStyle.FindStyleResource('headeritemstyle');
    if Assigned(st) then
    begin
      stobj := st.FindStyleResource('text');
      if (Assigned(stobj) and (stobj is TText)) then
        Result.Assign((stobj as TText).TextSettings.Font);
    end;
  end;
end;
{$ENDIF}

{ TTMSFNCStyles }

{$IFDEF FMXLIB}
class function TTMSFNCStyles.ExtractColor(
  ABitmap: TBitmap): TTMSFNCGraphicsColor;
var
  dt: TBitmapData;
begin
  Result := gcNull;
  if not Assigned(ABitmap) then
    Exit;
  if ABitmap.Map(TMapAccess.Read, dt) then
  begin
    Result := dt.GetPixel(dt.Width div 2, 1);
    ABitmap.Unmap(dt);
  end;
end;

class function TTMSFNCStyles.ExtractColorTo(
  ABitmap: TBitmap): TTMSFNCGraphicsColor;
var
  dt: TBitmapData;
begin
  Result := gcNull;
  if not Assigned(ABitmap) then
    Exit;
  if ABitmap.Map(TMapAccess.Read, dt) then
  begin
    Result := dt.GetPixel(dt.Width div 2, dt.Height - 1);
    ABitmap.Unmap(dt);
  end;
end;
{$ENDIF}

{$IFDEF WEBLIB}

class function TTMSFNCStyles.GetStyleBackgroundFill: string;
begin
  Result := FindCSSStyleProperty('.TMSFNCGridStyle', 'background');
end;

class function TTMSFNCStyles.GetStyleBackgroundStroke: string;
begin
  Result := FindCSSStyleProperty('.TMSFNCGridStyle', 'border-color');
end;

class function TTMSFNCStyles.GetStyleDefaultButtonFill: string;
begin
  Result := FindCSSStyleProperty('.TMSFNCButtonStyle', 'background');
end;

class function TTMSFNCStyles.GetStyleDefaultButtonStroke: string;
begin
  Result := FindCSSStyleProperty('.TMSFNCButtonStyle', 'border-color');
end;

class function TTMSFNCStyles.GetStyleEditTextColor: string;
begin
  Result := FindCSSStyleProperty('.TMSFNCEditStyle', '--foreground');
end;

class function TTMSFNCStyles.GetStyleEditFill: string;
begin
  Result := FindCSSStyleProperty('.TMSFNCEditStyle', 'background');
end;

class function TTMSFNCStyles.GetStyleEditStroke: string;
begin
  Result := FindCSSStyleProperty('.TMSFNCStyle', 'border-color');
end;

class function TTMSFNCStyles.GetStyleHeaderFill: string;
begin
  Result := FindCSSStyleProperty('.TMSFNCHeaderItemStyle', 'background');
end;

class function TTMSFNCStyles.GetStyleHeaderStroke: string;
begin
  Result := FindCSSStyleProperty('.TMSFNCHeaderItemStyle', 'border-color');
end;

class function TTMSFNCStyles.GetStyleSelectionFill: string;
begin
  Result := FindCSSStyleProperty('.TMSFNCGridStyle', '--selection');
end;

class function TTMSFNCStyles.GetStyleAlternativeSelectionFill: string;
begin
  Result := FindCSSStyleProperty('.TMSFNCMemoStyle', '--selection');
end;

class function TTMSFNCStyles.GetStyleAlternativeBackgroundFill: string;
begin
  Result := FindCSSStyleProperty('.TMSFNCMemoStyle', 'background');
end;

class function TTMSFNCStyles.GetStyleAlternativeBackgroundStroke: string;
begin
  Result := FindCSSStyleProperty('.TMSFNCMemoStyle', 'border-color');
end;

class function TTMSFNCStyles.GetStyleFocusFill: string;
begin
  Result := FindCSSStyleProperty('.TMSFNCGridStyle', '--focus');
end;

class function TTMSFNCStyles.GetStyleLineFill: string;
begin
  Result := FindCSSStyleProperty('.TMSFNCGridStyle', '--linefill');
end;

class function TTMSFNCStyles.GetStyleTextFont: string;
begin
  Result := FindCSSStyleProperty('.TMSFNCHeaderItemStyle', 'font-family');
end;

class function TTMSFNCStyles.GetStyleTextColor: string;
begin
  Result := FindCSSStyleProperty('.TMSFNCHeaderItemStyle', 'color');
end;

class function TTMSFNCStyles.GetStyleAlternateBackgroundFill: string;
begin
  Result := FindCSSStyleProperty('.TMSFNCGridStyle', '--alternatingrowbackground');
end;

class function TTMSFNCStyles.ExtractColor(AValue: string): TTMSFNCGraphicsColor;
var
  s, s2: string;
  sl: TStringList;
  p, p2: Integer;

  function ParseValue(AValue: string): Integer;
  begin
    Result := StrToInt(Trim(StringReplace(AValue, 'rgb(', '', [rfReplaceAll])))
  end;
begin
  Result := gcNull;
  if AValue <> '' then
  begin
    s := LowerCase(StringReplace(AValue, ' ', '', [rfReplaceAll]));
    if Pos('linear-gradient', s) = 1 then
    begin
      p := Pos('rgb', s);
      if p = 0 then
        p := Pos('#', s);
      p2 := Pos(')', s, p);
      s2 := Copy(s, p, p2 - p + 1);
      p := Pos('rgb', s, p2);
      if p = 0 then
        p := Pos('#', s);
      p2 := Pos(')', s, p);
      Result := ExtractColor(s2);
    end
    else if Pos('rgb', s) = 1 then
    begin
      sl := TStringList.Create;
      try
        sl.Delimiter := ',';
        sl.DelimitedText := s;
        Result := MakeGraphicsColor(ParseValue(sl[0]), ParseValue(sl[1]), ParseValue(sl[2]));
      finally
        sl.Free;
      end;
    end
    else if Pos('#', s) = 1 then
      Result := HexToColor(s);
  end;
end;

class function TTMSFNCStyles.ExtractColorTo(AValue: string): TTMSFNCGraphicsColor;
var
  s, s3: string;
  sl: TStringList;
  p, p2: Integer;

  function ParseValue(AValue: string): Integer;
  begin
    Result := StrToInt(Trim(StringReplace(AValue, 'rgb(', '', [rfReplaceAll])))
  end;
begin
  Result := gcNull;
  if AValue <> '' then
  begin
    s := LowerCase(StringReplace(AValue, ' ', '', [rfReplaceAll]));
    if Pos('linear-gradient', s) = 1 then
    begin
      p := Pos('rgb', s);
      if p = 0 then
        p := Pos('#', s);
      p2 := Pos(')', s, p);
      //s2 := Copy(s, p, p2 - p + 1);
      p := Pos('rgb', s, p2);
      if p = 0 then
        p := Pos('#', s);
      p2 := Pos(')', s, p);
      s3 := Copy(s, p, p2 - p + 1);
      Result := ExtractColor(s3);
    end
    else if Pos('rgb', s) = 1 then
    begin
      sl := TStringList.Create;
      try
        sl.Delimiter := ',';
        sl.DelimitedText := s;
        Result := MakeGraphicsColor(ParseValue(sl[0]), ParseValue(sl[1]), ParseValue(sl[2]));
      finally
        sl.Free;
      end;
    end
    else if Pos('#', s) = 1 then
      Result := HexToColor(s);
  end;
end;

class function TTMSFNCStyles.ExtractFontName(AValue: string): string;
begin
  Result := AValue;
end;

{$HINTS OFF}
class function TTMSFNCStyles.FindCSSStyleProperty(ARuleName, APropertyName: string): string;
var
  j: TJSObject;
  s: string;
  v: string;
begin
  Result := '';
  j := FindCSSStyleRule(ARuleName);
  if Assigned(j) then
  begin
    s := APropertyName;
    asm
      v = j.style.getPropertyValue(s);
    end;
    Result := v;
  end;
end;

class function TTMSFNCStyles.FindCSSStyleRule(ARuleName: string): TJSObject;
var
  s, f: string;
begin
  Result := nil;
  s := ARuleName;
  f := CSSStyleFileName;
  asm
    for (var i = 0; i < document.styleSheets.length; i++){
      var fn = document.styleSheets[i].href;
      if (fn != null) {
      if (f == fn.substring(fn.lastIndexOf('/')+1)){
        var classes = document.styleSheets[i].rules || document.styleSheets[i].cssRules;
        for (var x = 0; x < classes.length; x++) {
            if (classes[x].selectorText.startsWith(s)) {
                return classes[x];
            }
        }
       }
      }
    }
  end;
end;
{$HINTS ON}
{$ENDIF}

{$IFDEF VCLSTYLESENABLED}
class function TTMSFNCStyles.ExtractColor(AElement: TThemedElementDetails): TTMSFNCGraphicsColor;
var
  bmp: TBitmap;
begin
  bmp := TBitmap.Create;
  try
    bmp.SetSize(200, 200);
    StyleServices.DrawElement(bmp.Canvas.Handle, AElement, Rect(0, 0, 200, 200));
    Result := bmp.Canvas.Pixels[bmp.Width div 2, 2];
  finally
    bmp.Free;
  end;
end;

class function TTMSFNCStyles.ExtractColorTo(AElement: TThemedElementDetails): TTMSFNCGraphicsColor;
var
  bmp: TBitmap;
begin
  bmp := TBitmap.Create;
  try
    bmp.SetSize(200, 200);
    StyleServices.DrawElement(bmp.Canvas.Handle, AElement, Rect(0, 0, 200, 200));
    Result := bmp.Canvas.Pixels[bmp.Width div 2, bmp.Height - 2];
  finally
    bmp.Free;
  end;
end;
{$ENDIF}

class function TTMSFNCStyles.GetStyleBackgroundFillColor(var AColor: TTMSFNCGraphicsColor): Boolean;
{$IFDEF FMXLIB}
var
  f: TBrush;
{$ENDIF}
{$IFDEF VCLSTYLESENABLED}
var
  l: TThemedElementDetails;
  c: TColor;
{$ENDIF}
{$IFDEF WEBLIB}
var
  f: string;
  c: TTMSFNCGraphicsColor;
{$ENDIF}
begin
  Result := False;
  if StyleServicesEnabled then
  begin
    {$IFDEF FMXLIB}
    f := GetStyleBackgroundFill(GetStyleSceneObject);
    if Assigned(f) then
    begin
      AColor := ParseBrush(f, False);
      Result := AColor <> claNull;
      f.Free;
    end;
    {$ENDIF}
    {$IFDEF VCLSTYLESENABLED}
    l := StyleServices.GetElementDetails(tgCellNormal);
    c := clNone;
    StyleServices.GetElementColor(l, ecFillColor, c);
    if c = clNone then
      c := ExtractColor(l);

    AColor := c;
    Result := c <> clNone;
    {$ENDIF}
    {$IFDEF WEBLIB}
    f := GetStyleBackgroundFill;
    if f <> '' then
    begin
      c := ExtractColor(f);
      AColor := c;
      Result := c <> gcNull;
    end;
    {$ENDIF}
  end;
end;

class function TTMSFNCStyles.GetStyleBackgroundFillColorTo(var AColor: TTMSFNCGraphicsColor): Boolean;
{$IFDEF FMXLIB}
var
  f: TBrush;
{$ENDIF}
{$IFDEF VCLSTYLESENABLED}
var
  l: TThemedElementDetails;
  c: TColor;
{$ENDIF}
{$IFDEF WEBLIB}
var
  f: string;
  c: TTMSFNCGraphicsColor;
{$ENDIF}
begin
  Result := False;
  if StyleServicesEnabled then
  begin
    {$IFDEF FMXLIB}
    f := GetStyleBackgroundFill(GetStyleSceneObject);
    if Assigned(f) then
    begin
      AColor := ParseBrush(f, True);
      Result := AColor <> claNull;
      f.Free;
    end;
    {$ENDIF}
    {$IFDEF VCLSTYLESENABLED}
    l := StyleServices.GetElementDetails(tgGradientCellNormal);
    c := clNone;
    StyleServices.GetElementColor(l, ecFillColor, c);
    if c = clNone then
      c := ExtractColorTo(l);

    AColor := c;
    Result := c <> clNone;
    {$ENDIF}
    {$IFDEF WEBLIB}
    f := GetStyleBackgroundFill;
    if f <> '' then
    begin
      c := ExtractColorTo(f);
      AColor := c;
      Result := c <> gcNull;
    end;
    {$ENDIF}
  end;
end;

class function TTMSFNCStyles.GetStyleAlternativeBackgroundFillColor(var AColor: TTMSFNCGraphicsColor): Boolean;
{$IFDEF FMXLIB}
var
  f: TBrush;
{$ENDIF}
{$IFDEF VCLSTYLESENABLED}
var
  l: TThemedElementDetails;
  c: TColor;
{$ENDIF}
{$IFDEF WEBLIB}
var
  f: string;
  c: TTMSFNCGraphicsColor;
{$ENDIF}
begin
  Result := False;
  if StyleServicesEnabled then
  begin
    {$IFDEF FMXLIB}
    f := GetStyleAlternativeBackgroundFill(GetStyleSceneObject);
    if Assigned(f) then
    begin
      AColor := ParseBrush(f, False);
      Result := AColor <> claNull;
      f.Free;
    end;
    {$ENDIF}
    {$IFDEF VCLSTYLESENABLED}
    l := StyleServices.GetElementDetails(teBackgroundNormal);
    c := ExtractColor(l);
    AColor := c;
    Result := c <> clNone;
    {$ENDIF}
    {$IFDEF WEBLIB}
    f := GetStyleAlternativeBackgroundFill;
    if f <> '' then
    begin
      c := ExtractColor(f);
      AColor := c;
      Result := c <> gcNull;
    end;
    {$ENDIF}
  end;
end;

class function TTMSFNCStyles.GetStyleAlternativeBackgroundFillColorTo(var AColor: TTMSFNCGraphicsColor): Boolean;
{$IFDEF FMXLIB}
var
  f: TBrush;
{$ENDIF}
{$IFDEF VCLSTYLESENABLED}
var
  l: TThemedElementDetails;
  c: TColor;
{$ENDIF}
{$IFDEF WEBLIB}
var
  f: string;
  c: TTMSFNCGraphicsColor;
{$ENDIF}
begin
  Result := False;
  if StyleServicesEnabled then
  begin
    {$IFDEF FMXLIB}
    f := GetStyleAlternativeBackgroundFill(GetStyleSceneObject);
    if Assigned(f) then
    begin
      AColor := ParseBrush(f, True);
      Result := AColor <> claNull;
      f.Free;
    end;
    {$ENDIF}
    {$IFDEF VCLSTYLESENABLED}
    l := StyleServices.GetElementDetails(teBackgroundNormal);
    c := clNone;
    StyleServices.GetElementColor(l, ecGradientColor1, c);
    AColor := c;
    Result := c <> clNone;
    {$ENDIF}
    {$IFDEF WEBLIB}
    f := GetStyleAlternativeBackgroundFill;
    if f <> '' then
    begin
      c := ExtractColorTo(f);
      AColor := c;
      Result := c <> gcNull;
    end;
    {$ENDIF}
  end;
end;

class function TTMSFNCStyles.GetStyleBackgroundStrokeColor(var AColor: TTMSFNCGraphicsColor): Boolean;
{$IFDEF FMXLIB}
var
  s: TStrokeBrush;
{$ENDIF}
{$IFDEF VCLSTYLESENABLED}
var
  l: TThemedElementDetails;
  c: TColor;
{$ENDIF}
{$IFDEF WEBLIB}
var
  f: string;
  c: TTMSFNCGraphicsColor;
{$ENDIF}
begin
  Result := False;
  if StyleServicesEnabled then
  begin
    {$IFDEF FMXLIB}
    s := GetStyleBackgroundStroke(GetStyleSceneObject);
    if Assigned(s) then
    begin
      AColor := ParseBrush(s, False);
      Result := AColor <> claNull;
      s.Free;
    end;
    {$ENDIF}
    {$IFDEF VCLSTYLESENABLED}
    l := StyleServices.GetElementDetails(tgCellNormal);
    c := clNone;
    StyleServices.GetElementColor(l, ecBorderColor, c);
    AColor := c;
    Result := c <> clNone;
    {$ENDIF}
    {$IFDEF WEBLIB}
    f := GetStyleBackgroundStroke;
    if f <> '' then
    begin
      c := ExtractColor(f);
      AColor := c;
      Result := c <> gcNull;
    end;
    {$ENDIF}
  end;
end;

class function TTMSFNCStyles.GetStyleDefaultButtonFillColor(var AColor: TTMSFNCGraphicsColor): Boolean;
{$IFDEF FMXLIB}
var
  f: TBrush;
{$ENDIF}
{$IFDEF VCLSTYLESENABLED}
var
  l: TThemedElementDetails;
  c: TColor;
{$ENDIF}
{$IFDEF WEBLIB}
var
  f: string;
  c: TTMSFNCGraphicsColor;
{$ENDIF}
begin
  Result := False;
  if StyleServicesEnabled then
  begin
    {$IFDEF FMXLIB}
    f := GetStyleDefaultButtonFill(GetStyleSceneObject);
    if Assigned(f) then
    begin
      AColor := ParseBrush(f, False);
      Result := AColor <> claNull;
      f.Free;
    end;
    {$ENDIF}
    {$IFDEF VCLSTYLESENABLED}
    l := StyleServices.GetElementDetails(tcbButtonNormal);
    c := ExtractColor(l);
    AColor := c;
    Result := c <> clNone;
    {$ENDIF}
    {$IFDEF WEBLIB}
    f := GetStyleDefaultButtonFill;
    if f <> '' then
    begin
      c := ExtractColor(f);
      AColor := c;
      Result := c <> gcNull;
    end;
    {$ENDIF}
  end;
end;

class function TTMSFNCStyles.GetStyleDefaultButtonStrokeColor(var AColor: TTMSFNCGraphicsColor): Boolean;
{$IFDEF FMXLIB}
var
  s: TStrokeBrush;
{$ENDIF}
{$IFDEF VCLSTYLESENABLED}
var
  l: TThemedElementDetails;
  c: TColor;
{$ENDIF}
{$IFDEF WEBLIB}
var
  f: string;
  c: TTMSFNCGraphicsColor;
{$ENDIF}
begin
  Result := False;
  if StyleServicesEnabled then
  begin
    {$IFDEF FMXLIB}
    s := GetStyleDefaultButtonStroke(GetStyleSceneObject);
    if Assigned(s) then
    begin
      AColor := ParseBrush(s, False);
      Result := AColor <> claNull;
      s.Free;
    end;
    {$ENDIF}
    {$IFDEF VCLSTYLESENABLED}
    l := StyleServices.GetElementDetails(tcbButtonNormal);
    c := clNone;
    StyleServices.GetElementColor(l, ecBorderColor, c);
    AColor := c;
    Result := c <> clNone;
    {$ENDIF}
    {$IFDEF WEBLIB}
    f := GetStyleDefaultButtonStroke;
    if f <> '' then
    begin
      c := ExtractColor(f);
      AColor := c;
      Result := c <> gcNull;
    end;
    {$ENDIF}
  end;
end;

class function TTMSFNCStyles.GetStyleHeaderFillColor(var AColor: TTMSFNCGraphicsColor): Boolean;
{$IFDEF FMXLIB}
var
  f: TBrush;
{$ENDIF}
{$IFDEF VCLSTYLESENABLED}
var
  l: TThemedElementDetails;
  c: TColor;
{$ENDIF}
{$IFDEF WEBLIB}
var
  f: string;
  c: TTMSFNCGraphicsColor;
{$ENDIF}
begin
  Result := False;
  if StyleServicesEnabled then
  begin
    {$IFDEF FMXLIB}
    f := GetStyleHeaderFill(GetStyleSceneObject);
    if Assigned(f) then
    begin
      AColor := ParseBrush(f, False);
      Result := AColor <> claNull;
      f.Free;
    end;
    {$ENDIF}
    {$IFDEF VCLSTYLESENABLED}
    l := StyleServices.GetElementDetails(tgFixedCellNormal);
    c := ExtractColor(l);
    AColor := c;
    Result := c <> clNone;
    {$ENDIF}
    {$IFDEF WEBLIB}
    f := GetStyleHeaderFill;
    if f <> '' then
    begin
      c := ExtractColor(f);
      AColor := c;
      Result := c <> gcNull;
    end;
    {$ENDIF}
  end;
end;

class function TTMSFNCStyles.GetStyleHeaderFillColorTo(var AColor: TTMSFNCGraphicsColor): Boolean;
{$IFDEF FMXLIB}
var
  f: TBrush;
{$ENDIF}
{$IFDEF VCLSTYLESENABLED}
var
  l: TThemedElementDetails;
  c: TColor;
{$ENDIF}
{$IFDEF WEBLIB}
var
  f: string;
  c: TTMSFNCGraphicsColor;
{$ENDIF}
begin
  Result := False;
  if StyleServicesEnabled then
  begin
    {$IFDEF FMXLIB}
    f := GetStyleHeaderFill(GetStyleSceneObject);
    if Assigned(f) then
    begin
      AColor := ParseBrush(f, True);
      Result := AColor <> claNull;
      f.Free;
    end;
    {$ENDIF}
    {$IFDEF VCLSTYLESENABLED}
    l := StyleServices.GetElementDetails(tgFixedCellNormal);
    c := ExtractColorTo(l);
    AColor := c;
    Result := c <> clNone;
    {$ENDIF}
    {$IFDEF WEBLIB}
    f := GetStyleHeaderFill;
    if f <> '' then
    begin
      c := ExtractColorTo(f);
      AColor := c;
      Result := c <> gcNull;
    end;
    {$ENDIF}
  end;
end;

class function TTMSFNCStyles.GetStyleHeaderStrokeColor(var AColor: TTMSFNCGraphicsColor): Boolean;
{$IFDEF FMXLIB}
var
  s: TStrokeBrush;
{$ENDIF}
{$IFDEF VCLSTYLESENABLED}
var
  l: TThemedElementDetails;
  c: TColor;
{$ENDIF}
{$IFDEF WEBLIB}
var
  f: string;
  c: TTMSFNCGraphicsColor;
{$ENDIF}
begin
  Result := False;
  if StyleServicesEnabled then
  begin
    {$IFDEF FMXLIB}
    s := GetStyleHeaderStroke(GetStyleSceneObject);
    if Assigned(s) then
    begin
      AColor := ParseBrush(s, False);
      Result := AColor <> claNull;
      s.Free;
    end;
    {$ENDIF}
    {$IFDEF VCLSTYLESENABLED}
    l := StyleServices.GetElementDetails(tgFixedCellNormal);
    c := clNone;
    StyleServices.GetElementColor(l, ecBorderColor, c);
    AColor := c;
    Result := c <> clNone;
    {$ENDIF}
    {$IFDEF WEBLIB}
    f := GetStyleHeaderStroke;
    if f <> '' then
    begin
      c := ExtractColor(f);
      AColor := c;
      Result := c <> gcNull;
    end;
    {$ENDIF}
  end;
end;

class function TTMSFNCStyles.GetStyleLineFillColor(
  var AColor: TTMSFNCGraphicsColor): Boolean;
{$IFDEF FMXLIB}
var
  f: TBrush;
{$ENDIF}
{$IFDEF VCLSTYLESENABLED}
var
  l: TThemedElementDetails;
  c: TColor;
{$ENDIF}
{$IFDEF WEBLIB}
var
  f: string;
  c: TTMSFNCGraphicsColor;
{$ENDIF}
begin
  Result := False;
  if StyleServicesEnabled then
  begin
    {$IFDEF FMXLIB}
    f := GetStyleLineFill(GetStyleSceneObject);
    if Assigned(f) then
    begin
      AColor := ParseBrush(f, False);
      Result := AColor <> claNull;
      f.Free;
    end;
    {$ENDIF}
    {$IFDEF VCLSTYLESENABLED}
    l := StyleServices.GetElementDetails(tgCellNormal);
    c := clNone;
    StyleServices.GetElementColor(l, ecBorderColor, c);
    AColor := c;
    Result := c <> clNone;
    {$ENDIF}
    {$IFDEF WEBLIB}
    f := GetStyleLineFill;
    if f <> '' then
    begin
      c := ExtractColor(f);
      AColor := c;
      Result := c <> gcNull;
    end;
    {$ENDIF}
  end;
end;

class function TTMSFNCStyles.GetStyleSelectionFillColor(var AColor: TTMSFNCGraphicsColor): Boolean;
{$IFDEF FMXLIB}
var
  f: TBrush;
{$ENDIF}
{$IFDEF VCLSTYLESENABLED}
var
  l: TThemedElementDetails;
  c: TColor;
{$ENDIF}
{$IFDEF WEBLIB}
var
  f: string;
  c: TTMSFNCGraphicsColor;
{$ENDIF}
begin
  Result := False;
  if StyleServicesEnabled then
  begin
    {$IFDEF FMXLIB}
    f := GetStyleSelectionFill(GetStyleSceneObject);
    if Assigned(f) then
    begin
      AColor := ParseBrush(f, False);
      Result := AColor <> claNull;
      f.Free;
    end;
    {$ENDIF}
    {$IFDEF VCLSTYLESENABLED}
    l := StyleServices.GetElementDetails(tgCellSelected);
    c := clNone;
    StyleServices.GetElementColor(l, ecFillColor, c);
    c := ExtractColor(l);
    AColor := c;
    Result := c <> clNone;
    {$ENDIF}
    {$IFDEF WEBLIB}
    f := GetStyleSelectionFill;
    if f <> '' then
    begin
      c := ExtractColor(f);
      AColor := c;
      Result := c <> gcNull;
    end;
    {$ENDIF}
  end;
end;

class function TTMSFNCStyles.GetStyleSelectionFillColorTo(var AColor: TTMSFNCGraphicsColor): Boolean;
{$IFDEF FMXLIB}
var
  f: TBrush;
{$ENDIF}
{$IFDEF VCLSTYLESENABLED}
var
  l: TThemedElementDetails;
  c: TColor;
{$ENDIF}
{$IFDEF WEBLIB}
var
  f: string;
  c: TTMSFNCGraphicsColor;
{$ENDIF}
begin
  Result := False;
  if StyleServicesEnabled then
  begin
    {$IFDEF FMXLIB}
    f := GetStyleSelectionFill(GetStyleSceneObject);
    if Assigned(f) then
    begin
      AColor := ParseBrush(f, True);
      Result := AColor <> claNull;
      f.Free;
    end;
    {$ENDIF}
    {$IFDEF VCLSTYLESENABLED}
    l := StyleServices.GetElementDetails(tgCellSelected);
    c := clNone;
    StyleServices.GetElementColor(l, ecFillColor, c);
    c := ExtractColorTo(l);
    AColor := c;
    Result := c <> clNone;
    {$ENDIF}
    {$IFDEF WEBLIB}
    f := GetStyleSelectionFill;
    if f <> '' then
    begin
      c := ExtractColorTo(f);
      AColor := c;
      Result := c <> gcNull;
    end;
    {$ENDIF}
  end;
end;

class function TTMSFNCStyles.GetStyleTextFontColor(var AColor: TTMSFNCGraphicsColor): Boolean;
{$IFDEF VCLSTYLESENABLED}
var
  l: TThemedElementDetails;
  c: TColor;
{$ENDIF}
{$IFDEF WEBLIB}
var
  f: string;
  c: TTMSFNCGraphicsColor;
{$ENDIF}
begin
  Result := False;
  if StyleServicesEnabled then
  begin
    {$IFDEF FMXLIB}
    AColor := GetStyleTextColor(GetStyleSceneObject);
    Result := AColor <> claNull;
    {$ENDIF}
    {$IFDEF VCLSTYLESENABLED}
    AColor := gcBlack;
    l := StyleServices.GetElementDetails(tgCellNormal);
    c := clNone;
    StyleServices.GetElementColor(l, ecTextColor, c);
    AColor := c;
    Result := c <> clNone;
    {$ENDIF}
    {$IFDEF WEBLIB}
    f := GetStyleTextColor;
    if f <> '' then
    begin
      c := ExtractColor(f);
      AColor := c;
      Result := c <> gcNull;
    end;
    {$ENDIF}
  end;
end;

class function TTMSFNCStyles.GetStyleAlternativeTextFontColor(var AColor: TTMSFNCGraphicsColor): Boolean;
{$IFDEF VCLSTYLESENABLED}
var
  l: TThemedElementDetails;
  c: TColor;
{$ENDIF}
{$IFDEF WEBLIB}
var
  f: string;
  c: TTMSFNCGraphicsColor;
{$ENDIF}
begin
  Result := False;
  if StyleServicesEnabled then
  begin
    {$IFDEF FMXLIB}
    AColor := GetStyleEditTextColor(GetStyleSceneObject);
    Result := AColor <> claNull;
    {$ENDIF}
    {$IFDEF VCLSTYLESENABLED}
    AColor := gcBlack;
    l := StyleServices.GetElementDetails(tgCellSelected);
    c := clNone;
    StyleServices.GetElementColor(l, ecTextColor, c);
    AColor := c;
    Result := c <> clNone;
    {$ENDIF}
    {$IFDEF WEBLIB}
    f := GetStyleEditTextColor;
    if f <> '' then
    begin
      c := ExtractColor(f);
      AColor := c;
      Result := c <> gcNull;
    end;
    {$ENDIF}
  end;
end;

{$IFDEF FMXLIB}
class procedure TTMSFNCStyles.SetActiveScene(AScene: IScene);
begin
  if Assigned(AScene) and Assigned(AScene.GetObject) and (AScene.GetObject.ClassName <> 'TTMSFNCCustomPopupForm') then
    FScene := AScene
  else if not Assigned(AScene) then
    FScene := nil;
end;
{$ENDIF}

class function TTMSFNCStyles.StyleServicesEnabled: Boolean;
{$IFDEF FMXLIB}
var
  s: TFmxObject;
{$ENDIF}
begin
  {$IFDEF WEBLIB}
  Result := True;
  {$ENDIF}
  {$IFDEF LCLLIB}
  Result := False;
  {$ENDIF}
  {$IFDEF FMXLIB}
  s := GetStyleSceneObject;
  Result := Assigned(s);
  {$ENDIF}
  {$IFDEF VCLLIB}
  {$IFDEF VCLSTYLESENABLED}
  {$HINTS OFF}
  {$IF COMPILERVERSION >= 34}
  Result := StyleServices.Enabled and (StyleServices.Name <> 'Windows') and (StyleServices.Name <> 'Windows10');
  {$IFEND}
  {$IF COMPILERVERSION < 34}
  Result := (StyleServices.Enabled) and (StyleServices.Name <> 'Windows');
  {$IFEND}
  {$HINTS ON}
  {$ENDIF}
  {$IFNDEF VCLSTYLESENABLED}
  Result := False;
  {$ENDIF}
  {$ENDIF}
end;

{$IFDEF FNCLIB}

{ TTMSFNCStyleManager }

function TTMSFNCStylesManager.GetDocURL: string;
begin
  Result := TTMSFNCBaseDocURL + 'tmsfnccore/components/ttmsfncstyling';
end;

procedure TTMSFNCStylesManager.DoCanLoadStyle(AStyle: string;
  AComponent: TComponent; var ACanLoadStyle: Boolean);
begin
  if Assigned(OnCanLoadStyle) then
    OnCanLoadStyle(Self, AStyle, AComponent, ACanLoadStyle);
end;

constructor TTMSFNCStylesManager.Create(AOwner: TComponent);
begin
  inherited;
  FOptions := TTMSFNCStylesManagerOptions.Create;
end;

destructor TTMSFNCStylesManager.Destroy;
begin
  FOptions.Free;
  inherited;
end;

procedure TTMSFNCStylesManager.DoStyleLoaded(AStyle: string;
  AComponent: TComponent);
begin
  if Assigned(OnStyleLoaded) then
    OnStyleLoaded(Self, AStyle, AComponent);
end;

procedure TTMSFNCStylesManager.InternalLoadStyle(AValue: string; AComponents: TTMSFNCStylesManagerComponentArray);
var
  v, jv, va, fc: TJSONValue;
  fcs: string;
  ja: TJSONArray;
  I: Integer;
  f: TCustomForm;
  c: TTMSFNCGraphicsColor;
begin
  v := TTMSFNCUtils.ParseJSON(AValue);
  if Assigned(v) then
  begin
    try
      if Options.AdaptFormColor then
      begin
        fc := TTMSFNCUtils.GetJSONValue(v, 'FormColor');
        if Assigned(fc) then
        begin
          fcs := TTMSFNCUtils.GetJSONProp(v, 'FormColor');
          f := StyleForm;
          if not Assigned(f) then
            f := TTMSFNCUtils.GetOwnerForm(Self);

          if Assigned(f) then
          begin
            c := TTMSFNCGraphics.HTMLToColor(fcs);
            {$IFDEF FMXLIB}
            f.Fill.Kind := TBrushKind.Solid;
            f.Fill.Color := c;
            {$ELSE}
            f.Color := c;
            {$ENDIF}
          end;
        end;
      end;

      va := TTMSFNCUtils.GetJSONValue(v, 'Styles');
      if va is TJSONArray then
      begin
        ja := va as TJSONArray;
        for I := 0 to TTMSFNCUtils.GetJSONArraySize(ja) - 1 do
        begin
          jv := TTMSFNCUtils.GetJSONArrayItem(ja, I);
          if Assigned(jv) then
            InternalLoadStyleFromJSONValue(jv, AComponents);
        end;
      end
      else
      begin
        if Assigned(v) then
          InternalLoadStyleFromJSONValue(v, AComponents);
      end;
    finally
      v.Free;
    end;
  end;
end;

procedure TTMSFNCStylesManager.InternalLoadStyleFromJSONValue(
  AJSONValue: TJSONValue; AComponents: TTMSFNCStylesManagerComponentArray);
var
  ct: string;
  I, K: Integer;
  f: TCustomForm;
  io: ITMSFNCPersistence;
  s: TStringStream;
  st: string;
  b: Boolean;
  sm: ITMSFNCStylesManager;
  arr: TTMSFNCStylesManagerComponentArray;

  function IsComponentInArray(AComponent: TComponent): Boolean;
  var
    K: Integer;
  begin
    if Assigned(AComponents) and (Length(AComponents) = 0) or not Assigned(AComponents) then
      Result := True
    else
    begin
      Result := False;
      for K := 0 to Length(AComponents) - 1 do
      begin
        if AComponents[K] = AComponent then
        begin
          Result := True;
          Break;
        end;
      end;
    end;
  end;

  procedure ApplyStyleToComponent(AComponent: TComponent);
  begin
    if not Assigned(AComponent) then
      Exit;

    if (AComponent.ClassName = ct) and IsComponentInArray(AComponent) then
    begin
      if Supports(AComponent, ITMSFNCPersistence, io) then
      begin
        st := TTMSFNCUtils.GetJSONValueAsString(AJSONValue);
        b := True;
        DoCanLoadStyle(st, AComponent, b);
        if b then
        begin
          s := TStringStream.Create(st);
          try
            io.LoadSettingsFromStream(s);
          finally
            s.Free;
          end;

          DoStyleLoaded(st, AComponent);
        end;
      end;
    end;
  end;
begin
  f := StyleForm;
  if not Assigned(f) then
    f := TTMSFNCUtils.GetOwnerForm(Self);

  if not Assigned(f) then
    Exit;

  if Assigned(AJSONValue) then
  begin
    ct := TTMSFNCUtils.GetJSONProp(AJSONValue, TTMSFNCPersistence.ClassTypeVariable);
    if ct <> '' then
    begin
      for I := f.ComponentCount - 1 downto 0 do
      begin
        ApplyStyleToComponent(f.Components[I]);
        if Supports(f.Components[I], ITMSFNCStylesManager, sm) then
        begin
          arr := sm.GetSubComponentArray;
          for K := 0 to Length(arr) - 1 do
            ApplyStyleToComponent(arr[K]);
        end;
      end;
    end;
  end;
end;

{$IFNDEF WEBLIB}
function TTMSFNCStylesManager.CombineStyles(AFiles: TTMSFNCStylesManagerFileArray): string;
var
  I: Integer;
  sl: TStringList;
begin
  Result := '';
  if Length(AFiles) = 0 then
    Exit;

  Result := '{';
  Result := Result + '"Styles": [';
  sl := TStringList.Create;
  try
    for I := 0 to Length(AFiles) - 1 do
    begin
      if FileExists(AFiles[I]) then
        sl.LoadFromFile(AFiles[I])
      else
        sl.Text := AFiles[I];

      Result := Result + sl.Text;
      if I < Length(AFiles) - 1 then
        Result := Result + ',';
    end;
  finally
    sl.Free;
  end;
  Result := Result + ']}';
end;

function TTMSFNCStylesManager.GetStyleFromFile(AFile: string): string;
var
  ss: {$IFDEF LCLLIB}TStringList{$ELSE}TStringStream{$ENDIF};
begin
  Result := '';
  {$IFNDEF WEBLIB}
  if not FileExists(AFile) then
    Exit;
  {$ENDIF}

  ss := {$IFDEF LCLLIB}TStringList{$ELSE}TStringStream{$ENDIF}.Create;
  try
    {$IFNDEF WEBLIB}
    ss.LoadFromFile(AFile);
    Result := {$IFDEF LCLLIB}ss.Text{$ELSE}ss.DataString{$ENDIF};
    {$ENDIF}
  finally
    {$IFNDEF WEBLIB}
    ss.Free;
    {$ENDIF}
  end;
end;

function TTMSFNCStylesManager.GetStyleFromResource(AResourceName: string): string;
var
  r: TResourceStream;
  ss: {$IFDEF LCLLIB}TStringList{$ELSE}TStringStream{$ENDIF};
begin
  Result := '';
  r := nil;
  try
    r := TTMSFNCUtils.GetResourceStream(AResourceName, HInstance);
    if Assigned(r) then
    begin
      ss := {$IFDEF LCLLIB}TStringList{$ELSE}TStringStream{$ENDIF}.Create;
      try
        ss.LoadFromStream(r);
        Result := {$IFDEF LCLLIB}ss.Text{$ELSE}ss.DataString{$ENDIF};
      finally
        ss.Free;
      end;
    end;
  finally
    if Assigned(r) then
      r.Free;
  end;
end;

procedure TTMSFNCStylesManager.LoadStyleFromResource(AResourceName: string; AComponents: TTMSFNCStylesManagerComponentArray);
var
  r: TResourceStream;
begin
  r := nil;
  try
    r := TTMSFNCUtils.GetResourceStream(AResourceName, HInstance);
    if Assigned(r) then
      LoadStyleFromStream(r, AComponents);
  finally
    if Assigned(r) then
      r.Free;
  end;
end;

procedure TTMSFNCStylesManager.LoadStyleFromResource(AResourceName: string);
var
  a: TTMSFNCStylesManagerComponentArray;
begin
  SetLength(a, 0);
  LoadStyleFromResource(AResourceName, a);
end;
{$ENDIF}

{$IFDEF WEBLIB}
procedure TTMSFNCStylesManager.LoadStyleFromURL(AURL: string);
var
  a: TTMSFNCStylesManagerComponentArray;
begin
  SetLength(a, 0);
  LoadStyleFromFile(AURL, a);
end;

procedure TTMSFNCStylesManager.LoadStyleFromURL(AURL: string; AComponents: TTMSFNCStylesManagerComponentArray);
begin
  LoadStyleFromFile(AURL, AComponents);
end;
{$ENDIF}

procedure TTMSFNCStylesManager.LoadStyleFromFile(AFile: string);
var
  a: TTMSFNCStylesManagerComponentArray;
begin
  SetLength(a, 0);
  LoadStyleFromFile(AFile, a);
end;

procedure TTMSFNCStylesManager.LoadStyleFromFile(AFile: string; AComponents: TTMSFNCStylesManagerComponentArray);
var
  ss: {$IFDEF LCLLIB}TStringList{$ELSE}TStringStream{$ENDIF};
begin
  {$IFNDEF WEBLIB}
  if not FileExists(AFile) then
    Exit;
  {$ENDIF}

  ss := {$IFDEF LCLLIB}TStringList{$ELSE}TStringStream{$ENDIF}.Create;
  try
    {$IFDEF WEBLIB}
    FComponents := AComponents;
    {$ENDIF}
    ss.LoadFromFile(AFile {$IFDEF WEBLIB}, procedure begin{$ELSE});{$ENDIF}
    InternalLoadStyle({$IFDEF LCLLIB}ss.Text{$ELSE}ss.DataString{$ENDIF}, AComponents);
    {$IFDEF WEBLIB}
    ss.Free;
    end);
    {$ENDIF}
  finally
    {$IFNDEF WEBLIB}
    ss.Free;
    {$ENDIF}
  end;
end;

procedure TTMSFNCStylesManager.LoadStyleFromStream(AStream: TStream);
var
  a: TTMSFNCStylesManagerComponentArray;
begin
  SetLength(a, 0);
  LoadStyleFromStream(AStream, a);
end;

procedure TTMSFNCStylesManager.LoadStyleFromStream(AStream: TStream; AComponents: TTMSFNCStylesManagerComponentArray);
var
  ss: {$IFDEF LCLLIB}TStringList{$ELSE}TStringStream{$ENDIF};
begin
  ss := {$IFDEF LCLLIB}TStringList{$ELSE}TStringStream{$ENDIF}.Create;
  try
    ss.LoadFromStream(AStream);
    InternalLoadStyle({$IFDEF LCLLIB}ss.Text{$ELSE}ss.DataString{$ENDIF}, AComponents);
  finally
    ss.Free;
  end;
end;

procedure TTMSFNCStylesManager.LoadStyleFromText(AText: string);
var
  a: TTMSFNCStylesManagerComponentArray;
begin
  SetLength(a, 0);
  InternalLoadStyle(AText, a);
end;

procedure TTMSFNCStylesManager.LoadStyleFromText(AText: string; AComponents: TTMSFNCStylesManagerComponentArray);
begin
  InternalLoadStyle(AText, AComponents);
end;

procedure TTMSFNCStylesManager.SetStyle(const Value: string);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    LoadStyleFromFile(FStyle);
  end;
end;

procedure TTMSFNCStylesManager.SetOptions(const Value: TTMSFNCStylesManagerOptions);
begin
  FOptions.Assign(Value);
end;

procedure TTMSFNCStylesManager.SetStyleResource(const Value: string);
begin
  if FStyleResource <> Value then
  begin
    FStyleResource := Value;
    if FStyleResource <> '' then
    begin
      {$IFNDEF WEBLIB}
      LoadStyleFromResource('TTMSFNCSTYLE_' + FStyleResource);
      {$ENDIF}
    end;
  end;
end;

constructor TTMSFNCStylesManagerOptions.Create;
begin
  FAdaptFormColor := True;
end;

{$ENDIF}

end.

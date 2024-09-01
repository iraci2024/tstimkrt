{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{           copyright (c)  2016 - 2021                               }
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

unit FMX.TMSFNCPDFGraphicsLibHTMLEngine;

{$I FMX.TMSFNCDefines.inc}

{$IFDEF WEBLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}
{$IFDEF LCLLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}

//{$DEFINE USEGRAPHICSHTMLENGINE}

interface

uses
  FMX.TMSFNCTypes, FMX.Graphics, FMX.TMSFNCBitmapContainer, FMX.TMSFNCGraphics, FMX.TMSFNCPDFLib,
  FMX.TMSFNCGraphicsTypes
  {$IFDEF WEBLIB}
  ,Contnrs
  {$ENDIF}
  {$IFNDEF LCLWEBLIB}
  ,Types, Generics.Collections
  {$HINTS OFF}
  {$IF COMPILERVERSION > 22}
  , UITypes
  {$IFEND}
  {$HINTS ON}
  {$ENDIF}
  {$IFDEF LCLLIB}
  ,fgl
  {$ENDIF}
  ;

type
  TTMSFNCPDFGraphicsLibHTMLLine = class
  private
    FTextColor: TTMSFNCGraphicsColor;
    FFontSize: Integer;
    FFontStyle: TFontStyles;
    FLineBreak: Boolean;
    FBitmap: TTMSFNCBitmap;
    FHasImage: Boolean;
    FFontName: string;
    FIsURL: Boolean;
    FText: UnicodeString;
    FURL: UnicodeString;
    FBackgroundColor: TTMSFNCGraphicsColor;
    FBitmapHeight: Single;
    FBitmapWidth: Single;
    FSuperscript: Boolean;
    FSubscript: Boolean;
    FTextAlign: TTMSFNCGraphicsTextAlign;
    FOffset: Single;
    FBullet: Boolean;
    FLineColor: TTMSFNCGraphicsColor;
    FIsLine: Boolean;
    procedure SetBitmap(const Value: TTMSFNCBitmap);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Reset;
    property LineBreak: Boolean read FLineBreak write FLineBreak;
    property LineColor: TTMSFNCGraphicsColor read FLineColor write FLineColor;
    property BackgroundColor: TTMSFNCGraphicsColor read FBackgroundColor write FBackgroundColor;
    property TextColor: TTMSFNCGraphicsColor read FTextColor write FTextColor;
    property HasImage: Boolean read FHasImage write FHasImage;
    property IsUrl: Boolean read FIsURL write FIsURL;
    property Bitmap: TTMSFNCBitmap read FBitmap write SetBitmap;
    property BitmapWidth: Single read FBitmapWidth write FBitmapWidth;
    property BitmapHeight: Single read FBitmapHeight write FBitmapHeight;
    property FontName: string read FFontName write FFontName;
    property FontStyle: TFontStyles read FFontStyle write FFontStyle;
    property FontSize: Integer read FFontSize write FFontSize;
    property Text: UnicodeString read FText write FText;
    property URL: UnicodeString read FURL write FURL;
    property Bullet: Boolean read FBullet write FBullet;
    property Offset: Single read FOffset write FOffset;
    property TextAlign: TTMSFNCGraphicsTextAlign read FTextAlign write FTextAlign;
    property Subscript: Boolean read FSubscript write FSubscript;
    property Superscript: Boolean read FSuperscript write FSuperscript;
    property IsLine: Boolean read FIsLine write FIsLine;
  end;

  TTMSFNCPDFGraphicsLibHTMLLineBreak = class(TTMSFNCPDFGraphicsLibHTMLLine)
  public
    constructor Create; override;
  end;

  TTMSFNCPDFGraphicsLibHTMLImage = class(TTMSFNCPDFGraphicsLibHTMLLine)
  public
    constructor Create; override;
  end;

  {$IFDEF WEBLIB}
  TTMSFNCPDFGraphicsLibHTMLLines = class(TObjectList)
  private
    function GetItem(Index: Integer): TTMSFNCPDFGraphicsLibHTMLLine;
    procedure SetItem(Index: Integer; const Value: TTMSFNCPDFGraphicsLibHTMLLine);
  public
    property Items[Index: Integer]: TTMSFNCPDFGraphicsLibHTMLLine read GetItem write SetItem; default;
  end;
  {$ENDIF}
  {$IFNDEF WEBLIB}
  TTMSFNCPDFGraphicsLibHTMLLines = class(TObjectList<TTMSFNCPDFGraphicsLibHTMLLine>);
  {$ENDIF}

  TTMSFNCPDFGraphicsLibHTMLEngine = class
  public
    class function ParseHTML(AHTML: string; ABitmapContainer: TTMSFNCBitmapContainer = nil): TTMSFNCPDFGraphicsLibHTMLLines;
    class function DrawHTMLText(APDFLib: ITMSFNCCustomPDFLib; Text: UnicodeString; Rect: TRectF; ABitmapContainer: TTMSFNCBitmapContainer = nil; Paging: Boolean = False; AScale: Single = 1.0; Calculate: Boolean = False): TRectF; overload;
    class function DrawHTMLText(APDFLib: ITMSFNCCustomPDFLib; Text: UnicodeString; Point: TPointF; ABitmapContainer: TTMSFNCBitmapContainer = nil; AScale: Single = 1.0; Calculate: Boolean = False): TRectF; overload;
  end;

implementation

uses
  Classes, SysUtils, FMX.TMSFNCPDFCoreLibBase,
  FMX.TMSFNCUtils, Math
  {$IFDEF USEGRAPHICSHTMLENGINE}
  , FMX.TMSFNCGraphicsPDFEngine
  {$ENDIF}
  ;

var
  FColorLookup: TStringList;

type
  TTMSFNCPDFCoreLibBaseColor = class
  private
    FColor: TTMSFNCGraphicsColor;
  public
    constructor Create(AColor: TTMSFNCGraphicsColor);
    property Color: TTMSFNCGraphicsColor read FColor write FColor;
  end;

{ TTMSFNCPDFGraphicsLibHTMLEngine }

class function TTMSFNCPDFGraphicsLibHTMLEngine.DrawHTMLText(APDFLib: ITMSFNCCustomPDFLib; Text: UnicodeString; Rect: TRectF;
  ABitmapContainer: TTMSFNCBitmapContainer; Paging: Boolean; AScale: Single;
  Calculate: Boolean): TRectF;
{$IFDEF USEGRAPHICSHTMLENGINE}
var
  g: TTMSFNCGraphicsPDFEngine;
  tr: TRectF;
begin
  Result := RectF(0, 0, 0, 0);
  if not Assigned(APDFLib) then
    Exit;

  g := TTMSFNCGraphicsPDFEngine.Create(APDFLib);
  try
    g.BitmapContainer := ABitmapContainer;
    tr := g.CalculateText(Text, Rect, True);
    if not Calculate then
      g.DrawText(Rect, Text, True, gtaLeading, gtaLeading);

    Result := RectF(Rect.Left, Rect.Top, Rect.Left + tr.Right - tr.Left, Rect.Top + tr.Bottom - tr.Top);
  finally
    g.Free;
  end;
end;

{$ENDIF}
{$IFNDEF USEGRAPHICSHTMLENGINE}
var
  lst: TTMSFNCPDFGraphicsLibHTMLLines;
  l: TTMSFNCPDFGraphicsLibHTMLLine;
  tr: TRectF;
  {$IFDEF WEBLIB}
  s: string;
  {$ELSE}
  s: UnicodeString;
  {$ENDIF}
  x, y, yp: Single;
  fts, ftsurl: TTMSFNCPDFGraphicsLibFont;
  mx, my: Single;
  su: UnicodeString;
  rs: array of UnicodeString;
  rsr: TTMSFNCPDFGraphicsLibRectArray;
  I, K: Integer;
  th: Single;
  trbg, rb: TRectF;
  fc, fcto, stc: TTMSFNCGraphicsColor;
  trd, trc: TRectF;
  lbm: TTMSFNCPDFGraphicsLibLineBreakMode;
  applyoffset: Boolean;
  lw: Single;
  {$IFDEF WEBLIB}
  v: JSValue;
  {$ENDIF}
begin
  Result := RectF(0, 0, 0, 0);
  if not Assigned(APDFLib) then
    Exit;

  lbm := APDFLib.Graphics.LineBreakMode;
  APDFLib.Graphics.LineBreakMode := bmLineBreakModeWordWrap;

  {$IFDEF LCLLIB}
  lst := ParseHTML(UTF8Encode(Text), ABitmapContainer);
  {$ENDIF}
  {$IFNDEF LCLLIB}
  lst := ParseHTML(Text, ABitmapContainer);
  {$ENDIF}
  x := Rect.Left;
  y := Rect.Top;
  mx := 0;
  my := 0;
  stc := APDFLib.Graphics.Stroke.Color;
  fc := APDFLib.Graphics.Fill.Color;
  fcto := APDFLib.Graphics.Fill.ColorTo;
  lw := APDFLib.Graphics.Stroke.Width;

  applyoffset := True;
  fts := TTMSFNCPDFGraphicsLibFont.Create;
  ftsurl := TTMSFNCPDFGraphicsLibFont.Create;
  try
    fts.Assign(APDFLib.Graphics.Font);
    ftsurl.Assign(APDFLib.Graphics.Font);
    if Assigned(lst) then
    begin
      th := APDFLib.Graphics.Font.Size * PDFLHFACTOR;

      {$IFDEF WEBLIB}
      for v in lst do
      begin
        l := TTMSFNCPDFGraphicsLibHTMLLine(v);
      {$ELSE}
      for l in lst do
      begin
      {$ENDIF}
        if l is TTMSFNCPDFGraphicsLibHTMLImage then
        begin
          if Assigned(l.Bitmap) then
          begin
            if (y + l.BitmapHeight * AScale > Rect.Bottom) and Paging and not Calculate then
            begin
              Y := Rect.Top;
              APDFLib.&Initialization.NotifyNewPage;
            end;

            if not Calculate then
            begin
              case l.TextAlign of
                gtaCenter: APDFLib.Graphics.DrawImage(l.Bitmap, RectF(Rect.Left + ((Rect.Right - Rect.Left) - l.BitmapWidth * AScale) / 2, y, Rect.Left + ((Rect.Right - Rect.Left) - l.BitmapWidth * AScale) / 2 + l.BitmapWidth * AScale, y + l.BitmapHeight * AScale));
                gtaLeading: APDFLib.Graphics.DrawImage(l.Bitmap, RectF(x, y, x + l.BitmapWidth * AScale, y + l.BitmapHeight * AScale));
                gtaTrailing: APDFLib.Graphics.DrawImage(l.Bitmap, RectF(Rect.Right - l.BitmapWidth * AScale, y, Rect.Right, y + l.BitmapHeight * AScale));
              end;
            end;

            x := x + l.BitmapWidth * AScale;
            th := l.BitmapHeight * AScale;
            my := Max(my, th);
          end;
        end
        else
        begin
          APDFLib.Graphics.Font.BeginUpdate;
          APDFLib.Graphics.Font.Color := l.TextColor;
          if l.FontSize > 0 then
            APDFLib.Graphics.Font.Size := l.FontSize * AScale
          else
            APDFLib.Graphics.Font.SizeNoScale := fts.Size;

          if l.Subscript or l.Superscript then
            APDFLib.Graphics.Font.SizeNoScale := Round(APDFLib.Graphics.Font.SizeNoScale * 0.7);

          APDFLib.Graphics.Font.Style := l.FontStyle;
          if l.FontName <> '' then
            APDFLib.Graphics.Font.Name := l.FontName
          else
            APDFLib.Graphics.Font.Name := fts.Name;
          APDFLib.Graphics.Font.EndUpdate;

          APDFLib.Graphics.URLFont.BeginUpdate;
          APDFLib.Graphics.URLFont.Assign(APDFLib.Graphics.Font);
          APDFLib.Graphics.URLFont.EndUpdate;

          if l.LineBreak then
          begin
            if l.IsLine then
            begin
              APDFLib.Graphics.Stroke.Color := l.LineColor;
              APDFLib.Graphics.Stroke.Width := 1;
              APDFLib.Graphics.DrawLine(PointF(Rect.Left, y), PointF(mx, y));
              my := my + 2;
            end;

            Y := Y + th;

            th := APDFLib.Graphics.Font.Size * PDFLHFACTOR;

            x := Rect.Left;

            applyoffset := True;
          end
          else
          begin
            if applyoffset then
            begin
              x := x + l.Offset;
              if l.Bullet then
              begin
                rb := APDFLib.Graphics.DrawText(l.Text, PointF(0, 0), True);
                x := x - (rb.Right - rb.Left);
              end;

              applyoffset := False;
            end;

            s := l.Text;
            SetLength(rs, 1);
            SetLength(rsr, 1);
            rsr[0] := RectF(X, Y, Rect.Right, Y + APDFLib.Graphics.Font.Size * PDFLHFACTOR);
            trc := APDFLib.Graphics.DrawText(s, RectF(0, 0, 10000, 10000), True);
            rsr[0] := RectF(X, Y, Rect.Right, Y + (trc.Bottom - trc.Top));
            yp := y + (trc.Bottom - trc.Top);
            k := APDFLib.Graphics.DrawText(s, rsr, 0, True);
            su := Copy(s, 1, Length(s) - k);
            rs[Length(rs) - 1] := su;
            Delete(s, 1, Length(su));

            th := Max(th, (rsr[0].Bottom - rsr[0].Top));

            while k > 0 do
            begin
              SetLength(rsr, Length(rsr) + 1);
              rsr[Length(rsr) - 1] := RectF(Rect.Left + l.Offset, yp, Rect.Right, yp + APDFLib.Graphics.Font.Size * PDFLHFACTOR);
              trc := APDFLib.Graphics.DrawText(s, RectF(0, 0, 10000, 10000), True);
              rsr[Length(rsr) - 1] := RectF(Rect.Left + l.Offset, yp, Rect.Right, yp + (trc.Bottom - trc.Top));
              yp := yp + (trc.Bottom - trc.Top);
              th := Max(th, (trc.Bottom - trc.Top));
              k := APDFLib.Graphics.DrawText(s, rsr[Length(rsr) - 1], 1, 0, True);
              su := Copy(s, 1, Length(s) - k);
              SetLength(rs, Length(rs) + 1);
              rs[Length(rs) - 1] := su;
              Delete(s, 1, Length(su));
            end;

            for I := 0 to Length(rs) - 1 do
            begin
              if (rsr[I].Bottom > Rect.Bottom) and Paging and not Calculate then
              begin
                Y := Rect.Top;
                for K := I to Length(rsr) - 1 do
                begin
                  rsr[K] := RectF(rsr[K].Left, Y, rsr[K].Right, Y + (rsr[K].Bottom - rsr[K].Top));
                  Y := Y + (rsr[K].Bottom - rsr[K].Top);
                end;

                APDFLib.&Initialization.NotifyNewPage;
              end;

              tr := APDFLib.Graphics.DrawText(rs[I], rsr[I], True);

              case l.TextAlign of
                gtaLeading: trbg := RectF(rsr[I].Left, rsr[I].Top, rsr[I].Left + (tr.Right - tr.Left), rsr[I].Top + (tr.Bottom - tr.Top));
                gtaCenter: trbg := RectF(rsr[I].Left + ((rsr[I].Right - rsr[I].Left) - (tr.Right - tr.Left)) / 2, rsr[I].Top, rsr[I].Left + ((rsr[I].Right - rsr[I].Left) - (tr.Right - tr.Left)) / 2 + (tr.Right - tr.Left), rsr[I].Top + (tr.Bottom - tr.Top));
                gtaTrailing: trbg := RectF(rsr[I].Right - (tr.Right - tr.Left), rsr[I].Top, rsr[I].Right, rsr[I].Top + (tr.Bottom - tr.Top));
              end;

              if not Calculate then
              begin
                if (l.BackgroundColor <> gcNull) then
                begin
                  APDFLib.Graphics.Stroke.Color := gcNull;
                  APDFLib.Graphics.Fill.Color := l.BackgroundColor;
                  APDFLib.Graphics.Fill.ColorTo := gcNull;
                  APDFLib.Graphics.DrawRectangle(trbg);
                end;

                if l.Subscript then
                  trd := RectF(rsr[I].Left, rsr[I].Bottom - (rsr[I].Bottom - rsr[I].Top) * 0.3, rsr[I].Right, rsr[I].Bottom - (rsr[I].Bottom - rsr[I].Top) * 0.3 + (rsr[I].Bottom - rsr[I].Top))
                else if l.Superscript then
                  trd := RectF(rsr[I].Left, rsr[I].Top - (rsr[I].Bottom - rsr[I].Top) * 0.3, rsr[I].Right, rsr[I].Top - (rsr[I].Bottom - rsr[I].Top) * 0.3 + (rsr[I].Bottom - rsr[I].Top))
                else
                  trd := rsr[I];

                case l.TextAlign of
                  gtaCenter: APDFLib.Graphics.Alignment := gtaCenter;
                  gtaLeading: APDFLib.Graphics.Alignment := gtaLeading;
                  gtaTrailing: APDFLib.Graphics.Alignment := gtaTrailing;
                end;

                if l.IsUrl then
                  APDFLib.Graphics.AddURL(rs[I], l.URL, trd)
                else
                  APDFLib.Graphics.DrawText(rs[I], trd);
              end;

              X := rsr[I].Left + (tr.Right - tr.Left);
              Y := rsr[I].Top;

              mx := Max(mx, x);
              my := Max(my, y + (tr.Bottom - tr.Top));
              th := Max(th, (tr.Bottom - tr.Top));
            end;
          end;
        end;
      end;
      lst.Free;
    end;
  finally
    APDFLib.Graphics.LineBreakMode := lbm;
    APDFLib.Graphics.Font.BeginUpdate;
    APDFLib.Graphics.Font.Assign(fts);
    APDFLib.Graphics.URLFont.Assign(ftsurl);
    APDFLib.Graphics.Font.EndUpdate;
    APDFLib.Graphics.Fill.Color := fc;
    APDFLib.Graphics.Fill.ColorTo := fcto;
    APDFLib.Graphics.Stroke.Color := stc;
    APDFLib.Graphics.Stroke.Width := lw;
    fts.Free;
    ftsurl.Free;
  end;

  Result := RectF(Rect.Left, Rect.Top, mx, my);
end;
{$ENDIF}

class function TTMSFNCPDFGraphicsLibHTMLEngine.DrawHTMLText(APDFLib: ITMSFNCCustomPDFLib; Text: UnicodeString;
  Point: TPointF; ABitmapContainer: TTMSFNCBitmapContainer; AScale: Single;
  Calculate: Boolean): TRectF;
begin
  Result := DrawHTMLText(APDFLib, Text, RectF(Point.X, Point.Y, Point.X + 10000, Point.Y + 10000), ABitmapContainer, False, AScale, Calculate);
end;

class function TTMSFNCPDFGraphicsLibHTMLEngine.ParseHTML(AHTML: string; ABitmapContainer: TTMSFNCBitmapContainer = nil): TTMSFNCPDFGraphicsLibHTMLLines;
var
  Su: string;
  FoundTag: Boolean;
  FLines: TTMSFNCPDFGraphicsLibHTMLLines;

  function HTML2Color(s: string): TTMSFNCGraphicsColor;
  begin
    if Pos('#',s) = 1 then
      Result := TTMSFNCGraphics.HTMLToColor(s)
    else
      Result := TTMSFNCGraphics.TextToColor(s);
  end;

  function IStrToInt(S: string): Integer;
  var
    {%H-}Err, Res: Integer;
  begin
    Val(S, Res, Err);
    Result := Res;
  end;

  function ConvertHTMLLine(var S: string): string;
  var
    Su, Res, TagProp, Prop, Tagp, LineText, LType: string;
    Linebreak, Imgbreak: Boolean;
    TagPos,SpacePos: Integer;
    WordLen: Integer;
    TagChar: Char;
    LengthFits: Boolean;
    Anchor: Boolean;
    LastAnchor: string;
    IsImg: Boolean;
    ListIndex: Integer;
    Indent, err: integer;
    Invisible: Boolean;
//    FrstBullet: Boolean;
    HTML: TTMSFNCPDFGraphicsLibHTMLLine;
    HTMLIMG: TTMSFNCPDFGraphicsLibHTMLImage;
    Bmp: TTMSFNCBitmap;
    imgw, imgh: Single;
    TagWidth, TagHeight: Integer;
    X: Integer;
    bmpcreated: Boolean;
    imgal: TTMSFNCGraphicsTextAlign;
    c: TTMSFNCGraphicsColor;
    lb: TTMSFNCPDFGraphicsLibHTMLLineBreak;
    bl: String;
  begin
    Anchor := False;
//    FrstBullet := False;
    Invisible := False;
    ListIndex := 0;
    SpacePos := 0;
    LastAnchor := '';
    bl := '';

    Result := '';
    LineText := '';

    Linebreak := False;
    Imgbreak := False;
    Res := '';

    HTML := TTMSFNCPDFGraphicsLibHTMLLine.Create;

    while (Length(S) > 0) and not linebreak and not imgbreak do
    begin
      IsImg := False;
      TagPos := Pos('<', S);

      if (TagPos > 0) and ((SpacePos > TagPos) or (SpacePos = 0)) then
      begin
        Su := Copy(S, 1, TagPos - 1);
      end
      else
      begin
        if SpacePos > 0 then
          Su := Copy(S, 1, SpacePos)
        else
          Su := S;
      end;

      WordLen := Length(Su);

      while Pos('&nbsp;', Su) > 0 do
      begin
        TTMSFNCUtils.TagReplaceString('&nbsp;', ' ', Su);
      end;

      while Pos('&lt;', Su) > 0 do
      begin
        TTMSFNCUtils.TagReplaceString('&lt;', '<', Su);
      end;

      while Pos('&gt;', Su) > 0 do
      begin
        TTMSFNCUtils.TagReplaceString('&gt;', '>', Su);
      end;

      if WordLen > 0 then
      begin
        if Invisible then
          Delete(S, 1, WordLen);

        if not Invisible then
        begin
          HTML.IsUrl := Anchor;
          {$IFDEF LCLLIB}
          HTML.Text := UTF8Decode(bl + Su);
          {$ENDIF}
          {$IFNDEF LCLLIB}
          HTML.Text := bl + Su;
          {$ENDIF}
          HTML.HasImage := IsImg;

          if Anchor then
          begin
            HTML.TextColor := gcBlue;
            {$IFDEF LCLLIB}
            HTML.Url := UTF8Decode(LastAnchor);
            {$ENDIF}
            {$IFNDEF LCLLIB}
            HTML.Url := LastAnchor;
            {$ENDIF}
            HTML.FontStyle := HTML.FontStyle + [TFontStyle.fsUnderline];
          end;

          if (HTML.Text <> '') or (HTML.HasImage) then
          begin
            FLines.Add(HTML);
            HTML := TTMSFNCPDFGraphicsLibHTMLLine.Create;
          end;

          if bl <> '' then
          begin
            FLines.Add(TTMSFNCPDFGraphicsLibHTMLLineBreak.Create);
            bl := '';
          end;

          LengthFits := True;
          LineText := LineText + Su;

          if LengthFits then
          begin
            Res := Res + Copy(S, 1, WordLen);

            if not LengthFits and (LineText <> Su) then
              S := '';

            Delete(S, 1, WordLen);
          end
          else
          begin
            Linebreak := True;
            FLines.Add(TTMSFNCPDFGraphicsLibHTMLLine.Create);
          end;
        end;
      end;

      TagPos := Pos('<', S);

      if (TagPos = 1) and (Length(S) <= 2) then
        S := '';

      {$IFDEF ZEROSTRINGINDEX}
      X := 1;
      {$ELSE}
      X := 0;
      {$ENDIF}
      if not Linebreak and (TagPos = 1) and (Length(S) > 2) then
      begin
        if (S[2 - X] = '/') and (Length(S) > 3) then
        begin
          case UpCase(S[3 - X]) of
            'A':
              begin
                if Anchor then
                  Anchor := False;
              end;
            'B':
             begin
              if s[4 - X] <> '>' then
              else
                HTML.FontStyle := HTML.FontStyle - [TFontStyle.fsBold];
             end;
            'S':
             begin
              TagChar := UpCase(s[4 - X]);

              if (TagChar = 'U') then
              begin
                HTML.Superscript := False;
                HTML.Subscript := False;
              end
              else
              begin
               if (TagChar = 'H') then
               begin

               end
               else
                HTML.FontStyle := HTML.FontStyle - [TFontStyle.fsStrikeOut];
              end;
             end;
            'F':
              begin
                HTML.Reset;
              end;
            'I':
              begin
                if S[3 - X] <> '>' then
                  Linebreak := True
                else
                  HTML.FontStyle := HTML.FontStyle + [TFontStyle.fsItalic];
              end;
            'L', 'P':
              begin
                Linebreak := True;
                FLines.Add(TTMSFNCPDFGraphicsLibHTMLLineBreak.Create);
              end;
            'U':
              begin
                if (S[4 - X] <> '>') and (ListIndex > 0) then
                begin
                end
                else
                  HTML.FontStyle := HTML.FontStyle - [TFontStyle.fsUnderline, TFontStyle.fsStrikeOut];
              end;
            'Z': Invisible := False;
          end;
        end
        else
        begin
          case UpCase(S[2 - X]) of
            'A':
              begin
                TagProp := Uppercase(Copy(S, 3, Pos('>', S) - 1));
                if (TTMSFNCUtils.VarPos('HREF', TagProp, TagPos) > 0) then
                begin
                  TagProp := Copy(S, 3, Pos('>', S) - 1);
                  Prop := Copy(TagProp, TagPos + 4, Length(TagProp));
                  Prop := Copy(Prop, Pos('"', Prop) + 1, Length(Prop));
                  Prop := Copy(Prop, 1, Pos('"', Prop) - 1);
                  LastAnchor := Prop;
                  Anchor := True;
                end;
              end;
            'B':
              begin
                TagChar := UpCase(S[3 - X]);
                if TagChar = '>' then
                begin
                  HTML.FontStyle := HTML.FontStyle + [TFontStyle.fsBold];
                end
                else if TagChar = 'R' then
                begin
                  Linebreak := True;
                  FLines.Add(TTMSFNCPDFGraphicsLibHTMLLineBreak.Create);
                end
                else
                begin
                  if TagChar = 'O' then
                  begin
                    Res := Res + Copy(S, 1, Pos('>', S));
                    TagProp := Uppercase(Copy(S, 6, Pos('>', S) - 1));

                    if (Pos('BGCOLOR', TagProp) > 0) then
                    begin
                      Prop := Copy(TagProp, Pos('BGCOLOR', TagProp) + 7,
                        Length(TagProp));
                      Prop := Copy(Prop, Pos('"', Prop) + 1, Length(Prop));
                      Prop := Copy(Prop, 1, Pos('"', Prop) - 1);
                    end;
                  end;
                end;
              end;
            'H':
              begin
                case UpCase(S[3 - X]) of
                 'R':
                  begin
                    Linebreak := True;
                    TagProp := Copy(s,4,Pos('>',s) - 1);

                    c := gcBlack;
                    if TTMSFNCUtils.VarPos('COLOR',Uppercase(TagProp),TagPos) > 0 then
                    begin
                      Prop := Copy(TagProp,TagPos+5,Length(TagProp));
                      Prop := Copy(Prop,Pos('"',prop)+1,Length(Prop));
                      Prop := Copy(Prop,1,Pos('"',prop)-1);
                      c := HTML2Color(Prop);
                    end;

                    lb := TTMSFNCPDFGraphicsLibHTMLLineBreak.Create;
                    lb.LineColor := c;
                    lb.IsLine := True;
                    FLines.Add(lb);
                  end;
                end;
              end;
          'I':begin
                TagChar := Upcase(s[3 - X]);

                if TagChar = '>' then // <I> tag
                  HTML.FontStyle := HTML.FontStyle + [TFontStyle.fsItalic]
                else
                if TagChar = 'N' then  // <IND> tag
                begin
                  TagProp := Copy(s,3,pos('>',s) - 1);

                  Prop := Copy(TagProp,TTMSFNCUtils.ipos('x',TagProp) + 2,Length(TagProp));
                  Prop := Copy(Prop,Pos('"',Prop) + 1,Length(prop));
                  Prop := Copy(Prop,1,Pos('"',Prop) - 1);

                  val(Prop, indent, err);
                  if err = 0 then
                    HTML.Offset := Indent;
                end
                else
                  if TagChar = 'M' then
                  begin
                    TagProp := Copy(s,3,Pos('>',s) - 1);
                    Prop := Copy(TagProp,Pos('SRC',Uppercase(TagProp)) + 4,Length(TagProp));
                    Prop := Copy(Prop,Pos('"',Prop) + 1,Length(Prop));
                    Prop := Copy(Prop,1,Pos('"',Prop) - 1);

                    TagProp := Uppercase(TagProp);

                    TagWidth := 0;
                    TagHeight := 0;

                    imgal := gtaLeading;

                    if TTMSFNCUtils.VarPos('ALIGN', TagProp, TagPos) > 0 then
                    begin
                      Tagp := Copy(TagProp, TagPos + 5, Length(TagProp));
                      Tagp := Copy(Tagp, Pos('"', Tagp) + 1, Length(Tagp));
                      Tagp := Copy(Tagp, 1, Pos('"', Tagp) - 1);
                      if Pos('RIGHT',Tagp) > 0 then
                        imgal := gtaTrailing;
                      if Pos('LEFT',Tagp) > 0 then
                        imgal := gtaLeading;
                      if Pos('CENTER',Tagp) > 0 then
                        imgal := gtaCenter;
                    end;

                    if Pos('WIDTH',TagProp) > 0 then
                    begin
                      Tagp := Copy(TagProp,Pos('WIDTH',TagProp) + 6,Length(TagProp));
                      Tagp := Copy(Tagp,Pos('"',tagp) + 1,Length(Tagp));
                      Tagp := Copy(Tagp,1,Pos('"',tagp) - 1);
                      Val(Tagp,TagWidth,Err);
                    end;

                    if Pos('HEIGHT',TagProp) > 0 then
                    begin
                      Tagp := Copy(TagProp,TTMSFNCUtils.ipos('HEIGHT',TagProp) + 7,Length(TagProp));
                      Tagp := Copy(Tagp,pos('"',Tagp) + 1,Length(Tagp));
                      Tagp := Copy(Tagp,1,pos('"',Tagp) - 1);
                      Val(Tagp,TagHeight,Err);
                    end;

                    bmp := nil;
                    bmpcreated := false;

                    {$IFNDEF WEBLIB}
                    if (Pos('FILE://',Uppercase(Prop)) > 0)  then
                    begin
                      Delete(Prop,1,7);

                      if FileExists(Prop) then
                      begin
                        bmp := TTMSFNCBitmap.Create;
                        bmp.LoadFromFile(Prop);
                        bmpcreated := true;
                      end;
                    end;
                    {$ENDIF}

                    if (Pos(':',Prop) = 0) and Assigned(ABitmapContainer) then
                    begin
                      bmp := TTMSFNCBitmap(ABitmapContainer.FindBitmap(Prop));
                    end;

                    imgw := 0;
                    imgh := 0;

                    if Assigned(bmp) then
                    begin
                      if not IsBitmapEmpty(bmp) then
                      begin
                        if (TagWidth > 0) and (TagHeight > 0) then
                        begin
                          imgw := TagWidth;
                          imgh := TagHeight;
                        end;
                        if (TagWidth > 0) and (TagHeight = 0) then
                        begin
                          imgw := TagWidth;
                          imgh := Round(TagWidth/bmp.Width * bmp.Height);
                        end;
                        if (TagWidth = 0) and (TagHeight > 0) then
                        begin
                          imgw := Round(TagHeight/bmp.Height * bmp.Width);
                          imgh := TagHeight;
                        end;
                        if (TagWidth = 0) and (TagHeight = 0) then
                        begin
                          imgw := bmp.Width;
                          imgh := bmp.Height;
                        end;

                        HTMLIMG := TTMSFNCPDFGraphicsLibHTMLImage.Create;
                        HTMLIMG.Bitmap.Assign(Bmp);
                        HTMLIMG.BitmapWidth := imgw;
                        HTMLIMG.BitmapHeight := imgh;
                        HTMLIMG.TextAlign := imgal;
                        FLines.Add(HTMLIMG);
                      end;

                      if bmpcreated then
                        bmp.Free;
                    end;
                  end;
                end;
            'L':
              begin
                bl := '';

                if (TTMSFNCUtils.VarPos('>',s,TagPos)>0) then
                begin
                  TagProp := Uppercase(Copy(s,3,TagPos-1));

                  if TTMSFNCUtils.VarPos('TYPE',TagProp,TagPos) > 0 then
                  begin
                    Prop := Copy(TagProp,TagPos+4,Length(TagProp));
                    Prop := Copy(Prop,Pos('"',prop)+1,Length(Prop));
                    Prop := Copy(Prop,1,Pos('"',prop)-1);
                    LType := Prop;
                  end;
                end;

                {$IFDEF LCLLIB}
                if LType = 'CIRCLE' then
                  bl := UTF8Encode(UnicodeString(#$2022))
                else if LType = 'SQUARE' then
                  bl := UTF8Encode(UnicodeString(#$25AA))
                else if LType = 'STAR' then
                  bl := UTF8Encode(UnicodeString(#$2605))
                else if LType = 'ARROW' then
                  bl := UTF8Encode(UnicodeString(#$2192))
                else if LType = 'TICK' then
                  bl := UTF8Encode(UnicodeString(#$2713))
                {$ENDIF}
                {$IFNDEF LCLLIB}
                if LType = 'CIRCLE' then
                  bl := #$2022
                else if LType = 'SQUARE' then
                  bl := #$25AA
                else if LType = 'STAR' then
                  bl := #$2605
                else if LType = 'ARROW' then
                  bl := #$2192
                else if LType = 'TICK' then
                  bl := #$2713
                {$ENDIF}
                else
                  bl := '-';

                bl := bl + ' ';
              end;
            'U':
              begin
                if S[3 - X] <> '>' then
                begin
                  Inc(ListIndex);
                  Linebreak := True;
                end
                else
                  HTML.FontStyle := HTML.FontStyle + [TFontStyle.fsUnderline];
              end;
            'P':
              begin
                if (TTMSFNCUtils.VarPos('>', S, TagPos) > 0) then
                begin
                  TagProp := Uppercase(Copy(S, 3, TagPos - 1));

                  if TTMSFNCUtils.VarPos('ALIGN', TagProp, TagPos) > 0 then
                  begin
                    Prop := Copy(TagProp, TagPos + 5, Length(TagProp));
                    Prop := Copy(Prop, Pos('"', Prop) + 1, Length(Prop));
                    Prop := Copy(Prop, 1, Pos('"', Prop) - 1);
                    if Pos('RIGHT',Prop) > 0 then
                      HTML.TextAlign := gtaTrailing;
                    if Pos('LEFT',Prop) > 0 then
                      HTML.TextAlign := gtaLeading;
                    if Pos('CENTER',Prop) > 0 then
                      HTML.TextAlign := gtaCenter;
                  end;

                  if TTMSFNCUtils.VarPos('INDENT', TagProp, TagPos) > 0 then
                  begin
                    Prop := Copy(TagProp, TagPos + 6, Length(TagProp));
                    Prop := Copy(Prop, Pos('"', Prop) + 1, Length(Prop));
                    Prop := Copy(Prop, 1, Pos('"', Prop) - 1);
                    HTML.Offset := IStrToInt(Prop);
                  end;

                  if TTMSFNCUtils.VarPos('BGCOLOR', TagProp, TagPos) > 0 then
                  begin
                    Prop := Copy(TagProp, TagPos + 5, Length(TagProp));
                    Prop := Copy(Prop, Pos('"', Prop) + 1, Length(Prop));
                    Prop := Copy(Prop, 1, Pos('"', Prop) - 1);
                    HTML.BackgroundColor := HTML2Color(Prop);
                  end;
                end;
              end;
            'F':
              begin
                if (TTMSFNCUtils.VarPos('>', S, TagPos) > 0) then
                begin
                  TagProp := UpperCase(Copy(s,6,TagPos-6));

                  if TTMSFNCUtils.VarPos('ALIGN', TagProp, TagPos) > 0 then
                  begin
                    Prop := Copy(TagProp, TagPos + 5, Length(TagProp));
                    Prop := Copy(Prop, Pos('"', Prop) + 1, Length(Prop));
                    Prop := Copy(Prop, 1, Pos('"', Prop) - 1);
                    if Pos('RIGHT',Prop) > 0 then
                      HTML.TextAlign := gtaTrailing;
                    if Pos('LEFT',Prop) > 0 then
                      HTML.TextAlign := gtaLeading;
                    if Pos('CENTER',Prop) > 0 then
                      HTML.TextAlign := gtaCenter;
                  end;

                  if (TTMSFNCUtils.VarPos('FACE',TagProp,TagPos) > 0) then
                  begin
                    Prop := Copy(s,TagPos+4,Length(TagProp));
                    Prop := Copy(prop,pos('"',prop)+1,Length(prop));
                    Prop := Copy(prop,1,pos('"',prop)-1);
                    HTML.FontName := Prop;
                  end;

                  if TTMSFNCUtils.VarPos('INDENT', TagProp, TagPos) > 0 then
                  begin
                    Prop := Copy(TagProp, TagPos + 6, Length(TagProp));
                    Prop := Copy(Prop, Pos('"', Prop) + 1, Length(Prop));
                    Prop := Copy(Prop, 1, Pos('"', Prop) - 1);
                    HTML.Offset := IStrToInt(Prop);
                  end;

                  if TTMSFNCUtils.VarPos('BULLET', TagProp, TagPos) > 0 then
                    HTML.Bullet := True;

                  if (TTMSFNCUtils.VarPos(' COLOR',TagProp,TagPos) > 0) then
                  begin
                    Prop := Copy(TagProp,TagPos+6,Length(TagProp));
                    Prop := Copy(Prop,Pos('"',prop)+1,Length(prop));
                    Prop := Copy(Prop,1,Pos('"',prop)-1);

                    if Length(Prop) > 0 then
                      HTML.TextColor := HTML2Color(Prop)
                  end;

                  if (TTMSFNCUtils.VarPos('BGCOLOR',TagProp,TagPos)>0) then
                  begin
                    Prop := Copy(TagProp,TagPos+7,Length(TagProp));
                    Prop := Copy(prop,pos('"',prop)+1,Length(prop));
                    Prop := Copy(prop,1,pos('"',prop)-1);

                    if Length(Prop) > 0 then
                      HTML.BackgroundColor := HTML2Color(Prop)
                  end;

                  if (TTMSFNCUtils.VarPos('SIZE',TagProp,TagPos)>0) then
                  begin
                    Prop := Copy(TagProp,TagPos+4,Length(TagProp));
                    Prop := Copy(Prop,Pos('=',Prop)+1,Length(Prop));
                    Prop := Copy(Prop,Pos('"',Prop)+1,Length(Prop));
                    Prop := Copy(Prop,1,Pos('"',Prop) - 1);

                    case IStrToInt(Prop) of
                    1: HTML.FontSize := 8;
                    2: HTML.FontSize := 10;
                    3: HTML.FontSize := 12;
                    4: HTML.FontSize := 14;
                    5: HTML.FontSize := 16;
                    else
                      HTML.FontSize := IStrToInt(Prop);
                    end;
                  end;
                end;
              end;
          'S':begin
                TagChar := Upcase(s[3 - X]);

                if TagChar = '>' then
                  HTML.FontStyle := HTML.FontStyle + [TFontStyle.fsStrikeOut]
                else
                begin
                  if TagChar = 'H' then
                  else
                  begin
                    if TTMSFNCUtils.ipos('<SUB>',s)=1 then
                      HTML.Subscript := True
                    else
                      if TTMSFNCUtils.ipos('<SUP>',s)=1 then
                        HTML.Superscript := True;
                  end;
                end;
              end;
            'Z': Invisible := True;
          end;
        end;
        if (TTMSFNCUtils.VarPos('>', S, TagPos) > 0) and not Imgbreak then
        begin
          Res := Res + Copy(S, 1, TagPos);
          Delete(S, 1, TagPos);
        end
        else
        begin
          if not Imgbreak then
            Delete(S, 1, Length(S));
        end;
      end;
    end;

    Result := Res;

    if FLines.IndexOf(HTML) = -1 then
      HTML.Free;
  end;

  function DBTagStrip(s:string):string;
  var
    i,j: Integer;
  begin
    i := Pos('<#',s);
    if i > 0 then
    begin
      Result := Copy(s,1,i - 1);
      Delete(s,1,i);
      j := Pos('>',s);
      if j > 0 then
        Delete(s,j,1);
      Result := Result + s;
    end
    else
      Result := s;
  end;

  function CRLFStrip(s:string;break:boolean):string;
  var
    i: Integer;
  begin
    Result := '';
    {$IFDEF ZEROSTRINGINDEX}
    for i := 0 to Length(s) - 1 do
    {$ELSE}
    for i := 1 to Length(s) do
    {$ENDIF}
    begin
      if not ( (s[i] = #13) or (s[i] = #10)) then
        Result := Result + s[i]
      else
        if (s[i] = #13) and break then
          Result := Result + '<BR>';
    end;
  end;

begin
  FLines := TTMSFNCPDFGraphicsLibHTMLLines.Create;

  if Pos('&', AHTML) > 0 then
  begin
    repeat
      Foundtag := False;
      if TTMSFNCUtils.TagReplacestring('&amp;', '&', AHTML) then Foundtag := True;
      if TTMSFNCUtils.TagReplacestring('&quot;', '"', AHTML) then Foundtag := True;
      if TTMSFNCUtils.TagReplacestring('&sect;', '§', AHTML) then Foundtag := True;
      if TTMSFNCUtils.TagReplacestring('&permil;', '®‰', AHTML) then Foundtag := True;
      if TTMSFNCUtils.TagReplacestring('&reg;', '®', AHTML) then Foundtag := True;
      if TTMSFNCUtils.TagReplacestring('&copy;', '©', AHTML) then Foundtag := True;
      if TTMSFNCUtils.TagReplacestring('&para;', '¶', AHTML) then Foundtag := True;
      if TTMSFNCUtils.TagReplacestring('&trade;', '™', AHTML) then Foundtag := True;
      if TTMSFNCUtils.TagReplacestring('&euro;', '€', AHTML) then Foundtag := True;
      if TTMSFNCUtils.TagReplacestring('&pound;', '£', AHTML) then Foundtag := True;
      if TTMSFNCUtils.TagReplacestring('&dollar;', '$', AHTML) then Foundtag := True;


    until not Foundtag;
  end;

  AHTML := DBTagStrip(AHTML);
  AHTML := CRLFStrip(AHTML, True);

  Su := '';
  while Length(AHTML) > 0 do
    Su := Su + ConvertHTMLLine(AHTML);

  Result := FLines;
end;

{ TTMSFNCPDFGraphicsLibHTMLLine }

constructor TTMSFNCPDFGraphicsLibHTMLLine.Create;
begin
  FBitmap := TTMSFNCBitmap.Create;
  Reset;
end;

destructor TTMSFNCPDFGraphicsLibHTMLLine.Destroy;
begin
  FBitmap.Free;
  inherited;
end;

procedure TTMSFNCPDFGraphicsLibHTMLLine.Reset;
begin
  FLineBreak := False;
  FBackgroundColor := gcNull;
  FTextColor := gcBlack;
  FHasImage := False;
  FTextAlign := gtaLeading;
  FSubscript := False;
  FBullet := False;
  FSuperscript := False;
  FIsUrl := False;
  FFontName := '';
  FFontStyle := [];
  FFontSize := -1;
  FText := '';
  FURL := '';
  FOffset := 0;
  FBitmapWidth := 0;
  FBitmapHeight := 0;
end;

procedure TTMSFNCPDFGraphicsLibHTMLLine.SetBitmap(const Value: TTMSFNCBitmap);
begin
  FBitmap.Assign(Value);
end;

{ TTMSFNCPDFGraphicsLibHTMLLineBreak }

constructor TTMSFNCPDFGraphicsLibHTMLLineBreak.Create;
begin
  inherited;
  LineBreak := True;
end;

{ TTMSFNCPDFGraphicsLibHTMLImage }

constructor TTMSFNCPDFGraphicsLibHTMLImage.Create;
begin
  inherited;
  HasImage := True;
end;

procedure DestroyColorLookup;
var
  I: Integer;
  {$IFNDEF FMXLIB}
  obj: TTMSFNCGraphicsColorObject;
  {$ENDIF}
begin
  {$IFDEF FMXLIB}
  for I := 0 to FColorLookup.Count - 1 do
    FColorLookup.Objects[I].DisposeOf;
  {$ELSE}
  for I := 0 to FColorLookup.Count - 1 do
  begin
    obj := TTMSFNCGraphicsColorObject(FColorLookup.Objects[I]);
    obj.Free;
  end;
  {$ENDIF}

  FColorLookup.Free;
end;

{ TTMSFNCPDFCoreLibBaseColor }

constructor TTMSFNCPDFCoreLibBaseColor.Create(AColor: TTMSFNCGraphicsColor);
begin
  FColor := AColor;
end;

{$IFDEF WEBLIB}
function TTMSFNCPDFGraphicsLibHTMLLines.GetItem(Index: Integer): TTMSFNCPDFGraphicsLibHTMLLine;
begin
  Result := TTMSFNCPDFGraphicsLibHTMLLine(inherited Items[Index]);
end;

procedure TTMSFNCPDFGraphicsLibHTMLLines.SetItem(Index: Integer; const Value: TTMSFNCPDFGraphicsLibHTMLLine);
begin
  inherited Items[Index] := Value;
end;
{$ENDIF}

initialization
begin
  fcolorlookup := TStringList.Create;
  fcolorlookup.addobject('aliceblue', TTMSFNCPDFCoreLibBaseColor.Create(gcaliceblue));
  fcolorlookup.addobject('antiquewhite', TTMSFNCPDFCoreLibBaseColor.Create(gcantiquewhite));
  fcolorlookup.addobject('aqua', TTMSFNCPDFCoreLibBaseColor.Create(gcaqua));
  fcolorlookup.addobject('aquamarine', TTMSFNCPDFCoreLibBaseColor.Create(gcaquamarine));
  fcolorlookup.addobject('azure', TTMSFNCPDFCoreLibBaseColor.Create(gcazure));
  fcolorlookup.addobject('beige', TTMSFNCPDFCoreLibBaseColor.Create(gcbeige));
  fcolorlookup.addobject('bisque', TTMSFNCPDFCoreLibBaseColor.Create(gcbisque));
  fcolorlookup.addobject('black', TTMSFNCPDFCoreLibBaseColor.Create(gcblack));
  fcolorlookup.addobject('blanchedalmond', TTMSFNCPDFCoreLibBaseColor.Create(gcblanchedalmond));
  fcolorlookup.addobject('blue', TTMSFNCPDFCoreLibBaseColor.Create(gcblue));
  fcolorlookup.addobject('blueviolet', TTMSFNCPDFCoreLibBaseColor.Create(gcblueviolet));
  fcolorlookup.addobject('brown', TTMSFNCPDFCoreLibBaseColor.Create(gcbrown));
  fcolorlookup.addobject('burlywood', TTMSFNCPDFCoreLibBaseColor.Create(gcburlywood));
  fcolorlookup.addobject('cadetblue', TTMSFNCPDFCoreLibBaseColor.Create(gccadetblue));
  fcolorlookup.addobject('chartreuse', TTMSFNCPDFCoreLibBaseColor.Create(gcchartreuse));
  fcolorlookup.addobject('chocolate', TTMSFNCPDFCoreLibBaseColor.Create(gcchocolate));
  fcolorlookup.addobject('coral', TTMSFNCPDFCoreLibBaseColor.Create(gccoral));
  fcolorlookup.addobject('cornflowerblue', TTMSFNCPDFCoreLibBaseColor.Create(gccornflowerblue));
  fcolorlookup.addobject('cornsilk', TTMSFNCPDFCoreLibBaseColor.Create(gccornsilk));
  fcolorlookup.addobject('crimson', TTMSFNCPDFCoreLibBaseColor.Create(gccrimson));
  fcolorlookup.addobject('cyan', TTMSFNCPDFCoreLibBaseColor.Create(gccyan));
  fcolorlookup.addobject('darkblue', TTMSFNCPDFCoreLibBaseColor.Create(gcdarkblue));
  fcolorlookup.addobject('darkcyan', TTMSFNCPDFCoreLibBaseColor.Create(gcdarkcyan));
  fcolorlookup.addobject('darkgoldenrod', TTMSFNCPDFCoreLibBaseColor.Create(gcdarkgoldenrod));
  fcolorlookup.addobject('darkgray', TTMSFNCPDFCoreLibBaseColor.Create(gcdarkgray));
  fcolorlookup.addobject('darkgreen', TTMSFNCPDFCoreLibBaseColor.Create(gcdarkgreen));
  fcolorlookup.addobject('darkgrey', TTMSFNCPDFCoreLibBaseColor.Create(gcdarkgrey));
  fcolorlookup.addobject('darkkhaki', TTMSFNCPDFCoreLibBaseColor.Create(gcdarkkhaki));
  fcolorlookup.addobject('darkmagenta', TTMSFNCPDFCoreLibBaseColor.Create(gcdarkmagenta));
  fcolorlookup.addobject('darkolivegreen', TTMSFNCPDFCoreLibBaseColor.Create(gcdarkolivegreen));
  fcolorlookup.addobject('darkorange', TTMSFNCPDFCoreLibBaseColor.Create(gcdarkorange));
  fcolorlookup.addobject('darkorchid', TTMSFNCPDFCoreLibBaseColor.Create(gcdarkorchid));
  fcolorlookup.addobject('darkred', TTMSFNCPDFCoreLibBaseColor.Create(gcdarkred));
  fcolorlookup.addobject('darksalmon', TTMSFNCPDFCoreLibBaseColor.Create(gcdarksalmon));
  fcolorlookup.addobject('darkseagreen', TTMSFNCPDFCoreLibBaseColor.Create(gcdarkseagreen));
  fcolorlookup.addobject('darkslateblue', TTMSFNCPDFCoreLibBaseColor.Create(gcdarkslateblue));
  fcolorlookup.addobject('darkslategray', TTMSFNCPDFCoreLibBaseColor.Create(gcdarkslategray));
  fcolorlookup.addobject('darkslategrey', TTMSFNCPDFCoreLibBaseColor.Create(gcdarkslategrey));
  fcolorlookup.addobject('darkturquoise', TTMSFNCPDFCoreLibBaseColor.Create(gcdarkturquoise));
  fcolorlookup.addobject('darkviolet', TTMSFNCPDFCoreLibBaseColor.Create(gcdarkviolet));
  fcolorlookup.addobject('deeppink', TTMSFNCPDFCoreLibBaseColor.Create(gcdeeppink));
  fcolorlookup.addobject('deepskyblue', TTMSFNCPDFCoreLibBaseColor.Create(gcdeepskyblue));
  fcolorlookup.addobject('dimgray', TTMSFNCPDFCoreLibBaseColor.Create(gcdimgray));
  fcolorlookup.addobject('dimgrey', TTMSFNCPDFCoreLibBaseColor.Create(gcdimgrey));
  fcolorlookup.addobject('dodgerblue', TTMSFNCPDFCoreLibBaseColor.Create(gcdodgerblue));
  fcolorlookup.addobject('firebrick', TTMSFNCPDFCoreLibBaseColor.Create(gcfirebrick));
  fcolorlookup.addobject('floralwhite', TTMSFNCPDFCoreLibBaseColor.Create(gcfloralwhite));
  fcolorlookup.addobject('forestgreen', TTMSFNCPDFCoreLibBaseColor.Create(gcforestgreen));
  fcolorlookup.addobject('fuchsia', TTMSFNCPDFCoreLibBaseColor.Create(gcfuchsia));
  fcolorlookup.addobject('gainsboro', TTMSFNCPDFCoreLibBaseColor.Create(gcgainsboro));
  fcolorlookup.addobject('ghostwhite', TTMSFNCPDFCoreLibBaseColor.Create(gcghostwhite));
  fcolorlookup.addobject('gold', TTMSFNCPDFCoreLibBaseColor.Create(gcgold));
  fcolorlookup.addobject('goldenrod', TTMSFNCPDFCoreLibBaseColor.Create(gcgoldenrod));
  fcolorlookup.addobject('gray', TTMSFNCPDFCoreLibBaseColor.Create(gcgray));
  fcolorlookup.addobject('green', TTMSFNCPDFCoreLibBaseColor.Create(gcgreen));
  fcolorlookup.addobject('greenyellow', TTMSFNCPDFCoreLibBaseColor.Create(gcgreenyellow));
  fcolorlookup.addobject('grey', TTMSFNCPDFCoreLibBaseColor.Create(gcgrey));
  fcolorlookup.addobject('honeydew', TTMSFNCPDFCoreLibBaseColor.Create(gchoneydew));
  fcolorlookup.addobject('hotpink', TTMSFNCPDFCoreLibBaseColor.Create(gchotpink));
  fcolorlookup.addobject('indianred', TTMSFNCPDFCoreLibBaseColor.Create(gcindianred));
  fcolorlookup.addobject('indigo', TTMSFNCPDFCoreLibBaseColor.Create(gcindigo));
  fcolorlookup.addobject('ivory', TTMSFNCPDFCoreLibBaseColor.Create(gcivory));
  fcolorlookup.addobject('khaki', TTMSFNCPDFCoreLibBaseColor.Create(gckhaki));
  fcolorlookup.addobject('lavender', TTMSFNCPDFCoreLibBaseColor.Create(gcLavender));
  fcolorlookup.addobject('lavenderblush', TTMSFNCPDFCoreLibBaseColor.Create(gcLavenderblush));
  fcolorlookup.addobject('lawngreen', TTMSFNCPDFCoreLibBaseColor.Create(gcLawngreen));
  fcolorlookup.addobject('lemonchiffon', TTMSFNCPDFCoreLibBaseColor.Create(gclemonchiffon));
  fcolorlookup.addobject('lightblue', TTMSFNCPDFCoreLibBaseColor.Create(gclightblue));
  fcolorlookup.addobject('lightcoral', TTMSFNCPDFCoreLibBaseColor.Create(gclightcoral));
  fcolorlookup.addobject('lightcyan', TTMSFNCPDFCoreLibBaseColor.Create(gclightcyan));
  fcolorlookup.addobject('lightgoldenrodyellow', TTMSFNCPDFCoreLibBaseColor.Create(gclightgoldenrodyellow));
  fcolorlookup.addobject('lightgray', TTMSFNCPDFCoreLibBaseColor.Create(gclightgray));
  fcolorlookup.addobject('lightgreen', TTMSFNCPDFCoreLibBaseColor.Create(gclightgreen));
  fcolorlookup.addobject('lightgrey', TTMSFNCPDFCoreLibBaseColor.Create(gclightgrey));
  fcolorlookup.addobject('lightpink', TTMSFNCPDFCoreLibBaseColor.Create(gclightpink));
  fcolorlookup.addobject('lightsalmon', TTMSFNCPDFCoreLibBaseColor.Create(gclightsalmon));
  fcolorlookup.addobject('lightseagreen', TTMSFNCPDFCoreLibBaseColor.Create(gclightseagreen));
  fcolorlookup.addobject('lightskyblue', TTMSFNCPDFCoreLibBaseColor.Create(gclightskyblue));
  fcolorlookup.addobject('lightslategray', TTMSFNCPDFCoreLibBaseColor.Create(gclightslategray));
  fcolorlookup.addobject('lightslategrey', TTMSFNCPDFCoreLibBaseColor.Create(gclightslategrey));
  fcolorlookup.addobject('lightsteelblue', TTMSFNCPDFCoreLibBaseColor.Create(gclightsteelblue));
  fcolorlookup.addobject('lightyellow', TTMSFNCPDFCoreLibBaseColor.Create(gclightyellow));
  fcolorlookup.addobject('lime', TTMSFNCPDFCoreLibBaseColor.Create(gclime));
  fcolorlookup.addobject('limegreen', TTMSFNCPDFCoreLibBaseColor.Create(gclimegreen));
  fcolorlookup.addobject('linen', TTMSFNCPDFCoreLibBaseColor.Create(gclinen));
  fcolorlookup.addobject('magenta', TTMSFNCPDFCoreLibBaseColor.Create(gcmagenta));
  fcolorlookup.addobject('maroon', TTMSFNCPDFCoreLibBaseColor.Create(gcmaroon));
  fcolorlookup.addobject('mediumaquamarine', TTMSFNCPDFCoreLibBaseColor.Create(gcmediumaquamarine));
  fcolorlookup.addobject('mediumblue', TTMSFNCPDFCoreLibBaseColor.Create(gcmediumblue));
  fcolorlookup.addobject('mediumorchid', TTMSFNCPDFCoreLibBaseColor.Create(gcmediumorchid));
  fcolorlookup.addobject('mediumpurple', TTMSFNCPDFCoreLibBaseColor.Create(gcmediumpurple));
  fcolorlookup.addobject('mediumseagreen', TTMSFNCPDFCoreLibBaseColor.Create(gcmediumseagreen));
  fcolorlookup.addobject('mediumslateblue', TTMSFNCPDFCoreLibBaseColor.Create(gcmediumslateblue));
  fcolorlookup.addobject('mediumspringgreen', TTMSFNCPDFCoreLibBaseColor.Create(gcmediumspringgreen));
  fcolorlookup.addobject('mediumturquoise', TTMSFNCPDFCoreLibBaseColor.Create(gcmediumturquoise));
  fcolorlookup.addobject('mediumvioletred', TTMSFNCPDFCoreLibBaseColor.Create(gcmediumvioletred));
  fcolorlookup.addobject('midnightblue', TTMSFNCPDFCoreLibBaseColor.Create(gcmidnightblue));
  fcolorlookup.addobject('mintcream', TTMSFNCPDFCoreLibBaseColor.Create(gcmintcream));
  fcolorlookup.addobject('mistyrose', TTMSFNCPDFCoreLibBaseColor.Create(gcmistyrose));
  fcolorlookup.addobject('moccasin', TTMSFNCPDFCoreLibBaseColor.Create(gcmoccasin));
  fcolorlookup.addobject('navajowhite', TTMSFNCPDFCoreLibBaseColor.Create(gcnavajowhite));
  fcolorlookup.addobject('navy', TTMSFNCPDFCoreLibBaseColor.Create(gcnavy));
  fcolorlookup.addobject('oldlace', TTMSFNCPDFCoreLibBaseColor.Create(gcoldlace));
  fcolorlookup.addobject('olive', TTMSFNCPDFCoreLibBaseColor.Create(gcolive));
  fcolorlookup.addobject('olivedrab', TTMSFNCPDFCoreLibBaseColor.Create(gcolivedrab));
  fcolorlookup.addobject('orange', TTMSFNCPDFCoreLibBaseColor.Create(gcorange));
  fcolorlookup.addobject('orangered', TTMSFNCPDFCoreLibBaseColor.Create(gcorangered));
  fcolorlookup.addobject('orchid', TTMSFNCPDFCoreLibBaseColor.Create(gcorchid));
  fcolorlookup.addobject('palegoldenrod', TTMSFNCPDFCoreLibBaseColor.Create(gcpalegoldenrod));
  fcolorlookup.addobject('palegreen', TTMSFNCPDFCoreLibBaseColor.Create(gcpalegreen));
  fcolorlookup.addobject('paleturquoise', TTMSFNCPDFCoreLibBaseColor.Create(gcpaleturquoise));
  fcolorlookup.addobject('palevioletred', TTMSFNCPDFCoreLibBaseColor.Create(gcpalevioletred));
  fcolorlookup.addobject('papayawhip', TTMSFNCPDFCoreLibBaseColor.Create(gcpapayawhip));
  fcolorlookup.addobject('peachpuff', TTMSFNCPDFCoreLibBaseColor.Create(gcpeachpuff));
  fcolorlookup.addobject('peru', TTMSFNCPDFCoreLibBaseColor.Create(gcperu));
  fcolorlookup.addobject('pink', TTMSFNCPDFCoreLibBaseColor.Create(gcpink));
  fcolorlookup.addobject('plum', TTMSFNCPDFCoreLibBaseColor.Create(gcplum));
  fcolorlookup.addobject('powderblue', TTMSFNCPDFCoreLibBaseColor.Create(gcpowderblue));
  fcolorlookup.addobject('purple', TTMSFNCPDFCoreLibBaseColor.Create(gcpurple));
  fcolorlookup.addobject('red', TTMSFNCPDFCoreLibBaseColor.Create(gcred));
  fcolorlookup.addobject('rosybrown', TTMSFNCPDFCoreLibBaseColor.Create(gcrosybrown));
  fcolorlookup.addobject('royalblue', TTMSFNCPDFCoreLibBaseColor.Create(gcroyalblue));
  fcolorlookup.addobject('saddlebrown', TTMSFNCPDFCoreLibBaseColor.Create(gcsaddlebrown));
  fcolorlookup.addobject('salmon', TTMSFNCPDFCoreLibBaseColor.Create(gcsalmon));
  fcolorlookup.addobject('sandybrown', TTMSFNCPDFCoreLibBaseColor.Create(gcsandybrown));
  fcolorlookup.addobject('seagreen', TTMSFNCPDFCoreLibBaseColor.Create(gcseagreen));
  fcolorlookup.addobject('seashell', TTMSFNCPDFCoreLibBaseColor.Create(gcseashell));
  fcolorlookup.addobject('sienna', TTMSFNCPDFCoreLibBaseColor.Create(gcsienna));
  fcolorlookup.addobject('skyblue', TTMSFNCPDFCoreLibBaseColor.Create(gcskyblue));
  fcolorlookup.addobject('slateblue', TTMSFNCPDFCoreLibBaseColor.Create(gcslateblue));
  fcolorlookup.addobject('slategray', TTMSFNCPDFCoreLibBaseColor.Create(gcslategray));
  fcolorlookup.addobject('slategrey', TTMSFNCPDFCoreLibBaseColor.Create(gcslategrey));
  fcolorlookup.addobject('snow', TTMSFNCPDFCoreLibBaseColor.Create(gcsnow));
  fcolorlookup.addobject('springgreen', TTMSFNCPDFCoreLibBaseColor.Create(gcspringgreen));
  fcolorlookup.addobject('steelblue', TTMSFNCPDFCoreLibBaseColor.Create(gcsteelblue));
  fcolorlookup.addobject('violet', TTMSFNCPDFCoreLibBaseColor.Create(gcviolet));
  fcolorlookup.addobject('thistle', TTMSFNCPDFCoreLibBaseColor.Create(gcthistle));
  fcolorlookup.addobject('tan', TTMSFNCPDFCoreLibBaseColor.Create(gctan));
  fcolorlookup.addobject('tomato', TTMSFNCPDFCoreLibBaseColor.Create(gctomato));
  fcolorlookup.addobject('turquoise', TTMSFNCPDFCoreLibBaseColor.Create(gcturquoise));
  fcolorlookup.addobject('wheat', TTMSFNCPDFCoreLibBaseColor.Create(gcwheat));
  fcolorlookup.addobject('whitesmoke', TTMSFNCPDFCoreLibBaseColor.Create(gcwhitesmoke));
  fcolorlookup.addobject('yellowgreen', TTMSFNCPDFCoreLibBaseColor.Create(gcyellowgreen));
  fcolorlookup.addobject('red', TTMSFNCPDFCoreLibBaseColor.Create(gcred));
  fcolorlookup.addobject('black', TTMSFNCPDFCoreLibBaseColor.Create(gcblack));
  fcolorlookup.addobject('blue', TTMSFNCPDFCoreLibBaseColor.Create(gcblue));
  fcolorlookup.addobject('green', TTMSFNCPDFCoreLibBaseColor.Create(gcgreen));
  fcolorlookup.addobject('aqua', TTMSFNCPDFCoreLibBaseColor.Create(gcaqua));
  fcolorlookup.addobject('yellow', TTMSFNCPDFCoreLibBaseColor.Create(gcyellow));
  fcolorlookup.addobject('fuchsia', TTMSFNCPDFCoreLibBaseColor.Create(gcfuchsia));
  fcolorlookup.addobject('white', TTMSFNCPDFCoreLibBaseColor.Create(gcwhite));
  fcolorlookup.addobject('lime', TTMSFNCPDFCoreLibBaseColor.Create(gclime));
  fcolorlookup.addobject('silver', TTMSFNCPDFCoreLibBaseColor.Create(gcsilver));
  fcolorlookup.addobject('gray', TTMSFNCPDFCoreLibBaseColor.Create(gcgray));
  fcolorlookup.addobject('olive', TTMSFNCPDFCoreLibBaseColor.Create(gcolive));
  fcolorlookup.addobject('navy', TTMSFNCPDFCoreLibBaseColor.Create(gcnavy));
  fcolorlookup.addobject('purple', TTMSFNCPDFCoreLibBaseColor.Create(gcpurple));
  fcolorlookup.addobject('teal', TTMSFNCPDFCoreLibBaseColor.Create(gcteal));
  fcolorlookup.addobject('orange', TTMSFNCPDFCoreLibBaseColor.Create(gcorange));
  fcolorlookup.addobject('maroon', TTMSFNCPDFCoreLibBaseColor.Create(gcmaroon));
  FColorLookup.Sort;
end;

{$IFNDEF WEBLIB}
finalization
begin
  DestroyColorLookup;
end;
{$ENDIF}


end.



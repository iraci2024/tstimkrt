unit sAlphaGraph;
{$I sDefs.inc}

interface

uses
  Windows, SysUtils, Graphics, Controls, math, Classes,
  {$IFNDEF DELPHI5} Types, {$ENDIF}
  {$IFDEF DELPHI_XE2} UItypes, {$ENDIF}
  {$IFNDEF ACHINTS} sMaskData, sCommonData, {$ENDIF}
  sConst, acntTypes;


{$IFNDEF NOTFORHELP}
type
  TsCorner = (scLeftTop, scLeftBottom, scRightTop, scRightBottom);
  TsCorners = set of TsCorner;


{$ENDIF} // NOTFORHELP
function SwapColor(const Color: TColor): TColor;
function SwapRedBlue(const Color: TColor): TColor;
procedure BlendColorRect(Bmp: TBitmap; R: TRect; Blend: byte; Color: TColor); overload;
procedure FillRect32(const Bmp: TBItmap; R: TRect; const Color: TColor; Alpha: byte = MaxByte);
procedure FillRect32Excl(const Bmp: TBItmap; R, ExcludeR: TRect; const Color: TColor; Alpha: byte = MaxByte);
procedure FillAlphaRect(const Bmp: TBItmap; R: TRect; Value: byte = MaxByte; TransparentColor: TColor = clNone; ExcludeRects: TRects = nil);
procedure CheckEmptyAChannel(Bmp: TBitmap);
procedure PrepareLightMask(var maskBmp: TBitmap; AWidth, AHeight: integer; AColor: TColor);
{$IFNDEF ACHINTS}
// Copy with AlphaMask from MasterBmp (get transp. pixels from parent)
procedure CopyMasterRect(R1, R2: TRect; const aBmp: TBitmap; const CI: TCacheInfo; const MaskData: TsMaskData);
procedure DrawSmallSkinRect(Bmp: TBitmap; aRect: TRect; const ci: TCacheInfo; const MaskData: TsMaskData; State: integer);
// Skinned rectangle with transparent corners
procedure DrawSkinRect    (aBmp: TBitmap; const aRect: TRect; const ci: TCacheInfo; const MaskData: TsMaskData; const State: integer; const UpdateCorners: boolean);
procedure DrawSkinRect32  (aBmp: TBitmap; const aRect: TRect; const ci: TCacheInfo; const MaskData: TsMaskData; const State: integer);
procedure DrawSkinRect32Ex(aBmp: TBitmap; const aRect: TRect; const ci: TCacheInfo; const MaskData: TsMaskData; const State: integer; MaxWidths: TRect; Opacity: integer);
// Skip transparent part of corners
procedure DrawSkinGlyph(Bmp: TBitmap; P: TPoint; State, AddedTransparency: integer; const MaskData: TsMaskData; const CI: TCacheInfo);
procedure StretchBltMask    (DstX1, DstY1, Dst_Width, Dst_Height, SrcX1, SrcY1, Src_Width, Src_Height: integer; BmpDst, BmpSrc: TBitmap; MaskOffset: integer; NoStdStretch: boolean = False);
procedure StretchBltMask32  (DstX1, DstY1, Dst_Width, Dst_Height, SrcX1, SrcY1, Src_Width, Src_Height: integer; BmpDst, BmpSrc: TBitmap; MaskOffset: integer; NoStdStretch: boolean = False);
procedure StretchBltMask32Ex(DstX1, DstY1, Dst_Width, Dst_Height, SrcX1, SrcY1, Src_Width, Src_Height: integer; BmpDst, BmpSrc: TBitmap; MaskOffset: integer; Opacity: integer; NoStdStretch: boolean = False);
// Transparency - percent of the bmp visibility
procedure BmpDisabledKind(const Bmp: TBitmap; const DisabledKind: TsDisabledKind; Parent: TControl; const CI: TCacheInfo; Offset: TPoint);
  {$IFNDEF NOTFORHELP}
// Skip transparent pixels
procedure CopyByMask32(R1, R2: TRect; const Bmp1, Bmp2: TBitmap; const CI: TCacheInfo; const MaskData: TsMaskData);
procedure CopyByMask32Ex(R1, R2: TRect; Bmp1, Bmp2: TBitmap; const CI: TCacheInfo; const MaskData: TsMaskData; Opacity: integer);
//procedure BlendColorByMask(DstBmp, MaskBmp: TBitmap; DstPoint: TPoint; SrcRect: TRect; Color: TsColor);
procedure BlendGlyphByMask(R1, R2: TRect; Bmp1, Bmp2: TBitmap; TransparencyMode: integer; const MaskData: TsMaskData);

procedure UpdateCorners(SkinData: TsCommonData; State: integer; Corners: TsCorners = [scLeftTop, scLeftBottom, scRightTop, scRightBottom]); overload;
procedure UpdateCorners(gd: TsGeneralData; Bmp: TBitmap; R: TRect; CI: TCacheInfo; PRadiuses: PacIntValues = nil; ExtSource: boolean = False); overload;

procedure FillTransPixels32(const dstBmp, srcBmp: TBitmap; const dstRect: TRect; const SrcPoint: TPoint; MaskIndex: integer; SkinManager: TComponent; MaskPos: Cardinal = HTTOPLEFT); overload; {$IFDEF WARN_DEPRECATED} deprecated; {$ENDIF}
procedure FillTransPixels32(const dstBmp, srcBmp: TBitmap; dstRect: TRect; SrcPoint: TPoint; MaskIndex: integer; CommonSkinData: TacObject; MaskPos: Cardinal = HTTOPLEFT; SrcColor: TColor = clFuchsia); overload;
procedure DiffBmp32(dstBmp, subBmp: TBitmap);
    {$IFNDEF WIN64}
procedure FillLongword(var X; Count: Integer; Value: Longword);
    {$ENDIF}
procedure WriteColor(const Bmp: TBitmap; Text: PacChar; Enabled: boolean; lRect: TRect; Flags: Cardinal; Color: TColor; IsAnsi: boolean = False);
procedure WriteColorW(const Bmp: TBitmap; const Text: WideString; Enabled: boolean; lRect: TRect; Flags: Cardinal; Color: TColor);
procedure WriteText32(const Bmp: TBitmap; Text: PacChar; Enabled: boolean; var aRect: TRect; Flags: Cardinal; SkinIndex: integer; State: integer; SkinManager: TComponent; IsAnsi: boolean = False);
procedure FillDCBorder32(const Bmp: TBitmap; const aRect: TRect; const wl, wt, wr, wb: integer; const Color: TColor);
procedure UpdateTransPixels(Img: TBitmap);
function MakeIcon32(Img: TBitmap; UpdateAlphaChannell: boolean = False): HICON;
function SwapLong(Value: Cardinal): Cardinal; overload;
procedure SwapLong(P: PInteger; Count: Cardinal); overload;
function GetAPixel(const Bmp: TBitmap; x, y: integer): TsColor;
procedure SetAPixel(const Bmp: TBitmap; x, y: integer; const Value: TsColor); overload;
procedure SetAPixel(const Bmp: TBitmap; x, y: integer; const Value: TColor); overload;
procedure PaintItem32(SkinIndex: integer; Filling: boolean; State: integer; R: TRect; pP: TPoint; ItemBmp: TBitmap; CommonSkinData: TObject); overload;
procedure PaintItemBG32(SkinIndex: integer; State: integer; R: TRect; ItemBmp: TBitmap; CommonSkinData: TObject; CustomColor: TColor = clFuchsia);
procedure UpdateAlpha(Img: TBitmap); overload;
procedure UpdateAlpha(Img: TBitmap; const R: TRect); overload;

function MakeTranspCorners(Bmp: TBitmap; R: TRect; TL, TR, BL, BR: integer): boolean;
function GetRoundMask(Radius: integer): TBitmap;
  {$ENDIF} // NOTFORHELP
{$ENDIF}

implementation

uses sSkinProvider, acntUtils, sGraphUtils{$IFNDEF ACHINTS}, sDefaults, sVclUtils, sSkinManager{$ENDIF},
  sGradient, acgpUtils;


procedure UpdateTransPixels(Img: TBitmap);
var
  x, y, w, h, DeltaS: integer;
  S0, S: PRGBAArray_;
begin
  w := Img.Height - 1;
  h := Img.Width - 1;
  if InitLine(Img, Pointer(S0), DeltaS) then
    for Y := 0 to h do begin
      S := Pointer(PAnsiChar(S0) + DeltaS * Y);
      for X := 0 to w do
        with S[X] do
          if C = clWhite then
            I := 0;
    end;
end;


function MakeIcon32(Img: TBitmap; UpdateAlphaChannell: boolean = False): HICON;
var
  D0, D: PByteArray;
  IconInfo: TIconInfo;
  MaskBitmap: TBitmap;
  S0, S: PRGBAArray_S;
  X, Y, w, h, DeltaS, DeltaD: integer;
begin
  MaskBitmap := TBitmap.Create;
  MaskBitmap.PixelFormat := pf8bit;
  MaskBitmap.Width := Img.Width;
  MaskBitmap.Height := Img.Height;

  Img.PixelFormat := pf32bit;

  w := Img.Width - 1;
  h := Img.Height - 1;
  if InitLine(Img, Pointer(S0), DeltaS) and InitLine(MaskBitmap, Pointer(D0), DeltaD) then
    for Y := 0 to h do begin
      S := Pointer(PAnsiChar(S0) + DeltaS * Y);
      D := Pointer(PAnsiChar(D0) + DeltaD * Y);
      for X := 0 to w do
        D[X] := MaxByte - S[X].SA;
    end;

  try
    IconInfo.fIcon := True;
    IconInfo.hbmColor := Img.Handle;
    IconInfo.hbmMask := MaskBitmap.Handle;
    Result := CreateIconIndirect(IconInfo);
  finally
    FreeAndNil(MaskBitmap);
  end;
end;


function SwapLong(Value: Cardinal): Cardinal; overload;
{$IFDEF WIN64}
begin
  PByte(@Result)[0] := PByte(@Value)[3];
  PByte(@Result)[1] := PByte(@Value)[2];
  PByte(@Result)[2] := PByte(@Value)[1];
  PByte(@Result)[3] := PByte(@Value)[0];
{$ELSE}
asm
  BSWAP EAX
{$ENDIF}
end;


procedure SwapLong(P: PInteger; Count: Cardinal); overload;
{$IFDEF WIN64}
begin
{$R-}
  while Count > 0 do begin
    p^ := SwapLong(p^);
  {$POINTERMATH ON}
    inc(p);
  {$POINTERMATH OFF}
    dec(Count);
  end;
{$R+}
{$ELSE}
asm
@@Loop:
  MOV ECX, [EAX]
  BSWAP ECX
  MOV [EAX], ECX
  ADD EAX, 4
  DEC EDX
  JNZ @@Loop
{$ENDIF}
end;


procedure UpdateAlpha(Img: TBitmap);
begin
  UpdateAlpha(Img, MkRect(Img));
end;


const
  bLimit = $F4;


procedure UpdateAlpha(Img: TBitmap; const R: TRect);
var
  S0, S: PRGBAArray_S;
  x, y, Delta: integer;
begin
  if InitLine(Img, Pointer(S0), Delta) then
    if AeroIsEnabled then
      for Y := R.Top to R.Bottom - 1 do begin
        S := Pointer(PAnsiChar(S0) + Delta * Y);
        for X := R.Left to R.Right - 1 do
          with S[X] do
            if (SI = clWhite) then
              SI := 0
            else begin
              SR := SR * SA shr 8;
              SG := SG * SA shr 8;
              SB := SB * SA shr 8;
            end;
      end
    else
      for Y := R.Top to R.Bottom - 1 do begin
        S := Pointer(PAnsiChar(S0) + Delta * Y);
        for X := R.Left to R.Right - 1 do
          with S[X] do
            if (SI = clWhite) then
              SI := 0
            else
              if (SR > bLimit) and (SG > bLimit) and (SR > bLimit) then begin
                SR := min(bLimit, SR * SA shr 8);
                SG := min(bLimit, SG * SA shr 8);
                SB := min(bLimit, SB * SA shr 8);
              end
              else begin
                SR := SR * SA shr 8;
                SG := SG * SA shr 8;
                SB := SB * SA shr 8;
              end;
      end;
end;


function GetAPixel(const Bmp: TBitmap; x, y: integer): TsColor;
begin
  Result.I := PRGBAArray_(Bmp.ScanLine[Y])^[X].I;
end;


procedure SetAPixel(const Bmp: TBitmap; x, y: integer; const Value: TsColor);
begin
  PRGBAArray_(Bmp.ScanLine[Y])^[X].I := Value.I;
end;


procedure SetAPixel(const Bmp: TBitmap; x, y: integer; const Value: TColor);
begin
  PRGBAArray_(Bmp.ScanLine[Y])^[X].C := Value;
end;


procedure FillTransPixels32(const dstBmp, srcBmp: TBitmap; const dstRect: TRect; const SrcPoint: TPoint; MaskIndex: integer; SkinManager: TComponent; MaskPos: Cardinal = HTTOPLEFT);
begin
  if SkinManager = nil then
    SkinManager := DefaultManager;

  if SkinManager <> nil then
    FillTransPixels32(dstBmp, srcBmp, dstRect, SrcPoint, MaskIndex, TsSkinManager(SkinManager).CommonSkinData, MaskPos);
end;


procedure FillTransPixels32(const dstBmp, srcBmp: TBitmap; dstRect: TRect; SrcPoint: TPoint; MaskIndex: integer; CommonSkinData: TacObject; MaskPos: Cardinal = HTTOPLEFT; SrcColor: TColor = clFuchsia);
var
  C: TsColor_;
  mOffset: TPoint;
  maskBmp: TBitmap;
  M0, M: PRGBAArray_M;
  S0, S: PRGBAArray_S;
  D0, D: PRGBAArray_D;
  X, Y, w, h, wi, hi: integer;
  DeltaS, DeltaD, DeltaM: integer;
begin
  if MaskIndex < 0 then begin // Mask is not defined, check for Fuchsia pixels or semitransparent AlphaLayer
    if srcBmp <> nil then begin
      if SrcPoint.Y < 0 then begin
        dstRect.Top := dstRect.Top - SrcPoint.Y;
        SrcPoint.Y := 0;
      end;
      if SrcPoint.X < 0 then begin
        dstRect.Left := dstRect.Left - SrcPoint.X;
        SrcPoint.X := 0;
      end;
      w := min(WidthOf(dstRect,  True) - 1, srcBmp.Width  - SrcPoint.X - 1);
      h := min(HeightOf(dstRect, True) - 1, srcBmp.Height - SrcPoint.Y - 1);
      if InitLine(srcBmp, Pointer(S0), DeltaS) and InitLine(dstBmp, Pointer(D0), DeltaD) then
        for Y := 0 to h do begin
          S := Pointer(PAnsiChar(S0) + DeltaS * (SrcPoint.Y  + Y));
          D := Pointer(PAnsiChar(D0) + DeltaD * (dstRect.Top + Y));
          for X := 0 to w do
            with D[dstRect.Left + X], S[SrcPoint.X + X] do
              if DC = clFuchsia then
                DC := SC
              else
                if DA < MaxByte then begin
                  DR := ((SR - DR) * (MaxByte - DA) + DR shl 8) shr 8;
                  DG := ((SG - DG) * (MaxByte - DA) + DG shl 8) shr 8;
                  DB := ((SB - DB) * (MaxByte - DA) + DB shl 8) shr 8;
                  DA := MaxByte;
                end;
        end;
    end
    else
      if InitLine(dstBmp, Pointer(D0), DeltaD) then begin // Fill by SrcColor
        w := WidthOf(dstRect,  True) - 1;
        h := HeightOf(dstRect, True) - 1;
        C.C := SrcColor;
        for Y := 0 to h do begin
          D := Pointer(PAnsiChar(D0) + DeltaD * (dstRect.Top + Y));
          for X := 0 to w do
            with D[dstRect.Left + X], C do
              if DC = clFuchsia then
                D[dstRect.Left + X].DC := SrcColor
              else
                if D[dstRect.Left + X].DA < MaxByte then begin
                  DR := ((R - DR) * (MaxByte - DA) + DR shl 8) shr 8;
                  DG := ((G - DG) * (MaxByte - DA) + DG shl 8) shr 8;
                  DB := ((B - DB) * (MaxByte - DA) + DB shl 8) shr 8;
                  DA := MaxByte;
                end;
        end;
      end;
  end
  else
    if not (CommonSkinData is TacSkinData) then begin
  {$IFDEF DEBUG}
      if IsIDERunning then
        Alert('Check parameters of the FillTransPixels32 procedure.');
  {$ENDIF}
      Exit;
    end
    else
      with TacSkinData(CommonSkinData) do begin
        maskBmp := ma[MaskIndex].Bmp;
        if maskBmp = nil then
          maskBmp := FOwner.MasterBitmap;

        with ma[MaskIndex] do begin
          wi := Width;
          hi := Height;
          case MaskPos of
            HTTOPRIGHT    : mOffset := Point(R.Left + wi - WR, R.Top);
            HTBOTTOMLEFT  : mOffset := Point(R.Left, R.Top + hi - WB);
            HTBOTTOMRIGHT : mOffset := Point(R.Left + wi - WR, R.Top + hi - WB);
            else{HTTOPLEFT} mOffset := R.TopLeft;
          end;
        end;
        w := min(WidthOf(dstRect,  True) - 1, srcBmp.Width  - SrcPoint.X - 1);
        h := min(HeightOf(dstRect, True) - 1, srcBmp.Height - SrcPoint.Y - 1);

        if h + mOffset.Y >= maskBmp.Height then
          h := maskBmp.Height - 1 - mOffset.Y;

        if InitLine(srcBmp, Pointer(S0), DeltaS) and InitLine(dstBmp, Pointer(D0), DeltaD) and InitLine(maskBmp, Pointer(M0), DeltaM) then
          for Y := 0 to h do begin
            S := Pointer(PAnsiChar(S0) + DeltaS * (SrcPoint.Y  + Y));
            D := Pointer(PAnsiChar(D0) + DeltaD * (dstRect.Top + Y));
            M := Pointer(PAnsiChar(M0) + DeltaM * (mOffset.Y   + Y));
            for X := 0 to w do begin
              C.C := M[mOffset.X + X].MC;
              C.A := 0;
              if C.C = clFuchsia then
                D[dstRect.Left + X].DC := S[SrcPoint.X + X].SC;
            end;
          end;
      end;
end;


procedure DiffBmp32(dstBmp, subBmp: TBitmap);
var
  S0, S: PRGBAArray_S;
  D0, D: PRGBAArray_D;
  X, Y, w, h: integer;
  DeltaS, DeltaD: integer;
begin
  w := dstBmp.Width;
  h := dstBmp.Height;
  if (w = subBmp.Width) and (h = subBmp.Height) then
    if InitLine(subBmp, Pointer(S0), DeltaS) and InitLine(dstBmp, Pointer(D0), DeltaD) then
      for Y := 0 to h - 1 do begin
        S := Pointer(PAnsiChar(S0) + DeltaS * Y);
        D := Pointer(PAnsiChar(D0) + DeltaD * Y);
        for X := 0 to w - 1 do
          with D[X] do
            if (D[X].DR = S[X].SR) and (D[X].DG = S[X].SG) and (D[X].DB = S[X].SB) then
              D[X].DI := 0;
      end;
end;


procedure WriteColor(const Bmp: TBitmap; Text: PacChar; Enabled: boolean; lRect: TRect; Flags: Cardinal; Color: TColor; IsAnsi: boolean = False);
var
  DeltaS, DeltaD, x, y, h, w, miny, minx: integer;
  S0, S: PRGBAArray_S;
  D0, D: PRGBAArray_D;
  SrcColor: TsColor;
  TmpBmp: TBitmap;
  CC: TsColor_;
  bC: byte;
  R: TRect;
begin
  TmpBmp := CreateBmp32(lRect);
  TmpBmp.Canvas.Font.Assign(Bmp.Canvas.Font);
  TmpBmp.Canvas.Font.Height := Bmp.Canvas.Font.Height;

  SetTextColor(TmpBmp.Canvas.Handle, 0);
  SetBkColor(TmpBmp.Canvas.Handle, clWhite);

  R := MkRect(TmpBmp);
  if IsAnsi then
    DrawText(TmpBmp.Canvas.Handle, PAnsiChar(Text), StrLen(PChar(Text)), R, Flags)
  else
    AcDrawText(TmpBmp.Canvas.Handle, Text, R, Flags);

  h := TmpBmp.Height - 1;
  w := TmpBmp.Width  - 1;

  if lRect.Top + h >= Bmp.Height then
    h := Bmp.Height - 1 - lRect.Top;

  if lRect.Left + w >= Bmp.Width then
    w := Bmp.Width - 1 - lRect.Left;

  if lRect.Top < 0 then
    miny := -lRect.Top
  else
    miny := 0;

  if lRect.Left < 0 then
    minx := -lRect.Left
  else
    minx := 0;

  CC.A := MaxByte;
  SrcColor.C := Color;
  if InitLine(TmpBmp, Pointer(S0), DeltaS) and InitLine(Bmp, Pointer(D0), DeltaD) then
    with SrcColor do
      if Enabled then
        for y := miny to h do begin
          S := Pointer(PAnsiChar(S0) + DeltaS * Y);
          D := Pointer(PAnsiChar(D0) + DeltaD * (y + lRect.Top));
          for x := minx to w do
            with S[x] do
              if (SC <> clWhite) and (SI <> -1) then
                with D[x + lRect.Left] do
                  if SC = 0 then begin
                    DA := max(DA, MaxByte - 1);
                    DR := (DR - R + R shl 8) * DA shr 16;
                    DG := (DG - G + G shl 8) * DA shr 16;
                    DB := (DB - B + B shl 8) * DA shr 16;
                  end
                  else begin
                    bC := Round(0.299 * SR + 0.587 * SG + 0.114 * SB);
                    DA := max(DA, MaxByte - bC);
                    DR := ((DR - R) * bC + R shl 8) * DA shr 16;
                    DG := ((DG - G) * bC + G shl 8) * DA shr 16;
                    DB := ((DB - B) * bC + B shl 8) * DA shr 16;
                  end;
        end
      else
        for y := miny to h do begin
          S := Pointer(PAnsiChar(S0) + DeltaS * Y);
          D := Pointer(PAnsiChar(D0) + DeltaD * (y + lRect.Top));
          for x := minx to w do
            with S[x] do
              if (SC <> clWhite) and (SI <> -1) then
                with D[x + lRect.Left] do begin
                  DA := MaxByte;
                  DR := ((DR - R) * SR + R shl 8) shr 8;
                  DG := ((DG - G) * SG + G shl 8) shr 8;
                  DB := ((DB - B) * SB + B shl 8) shr 8;
                end;
        end;

  FreeAndNil(TmpBmp);
end;


procedure WriteColorW(const Bmp: TBitmap; const Text: WideString; Enabled: boolean; lRect: TRect; Flags: Cardinal; Color: TColor);
var
  DeltaS, DeltaD, x, y, h, w, miny, minx: integer;
  S0, S: PRGBAArray_S;
  D0, D: PRGBAArray_D;
  SrcColor: TsColor;
  TmpBmp: TBitmap;
  CC: TsColor_;
  bC: byte;
  R: TRect;
begin
  TmpBmp := CreateBmp32(lRect);
  TmpBmp.Canvas.Font.Assign(Bmp.Canvas.Font);

  SetTextColor(TmpBmp.Canvas.Handle, 0);
  SetBkColor(TmpBmp.Canvas.Handle, clWhite);

  R := MkRect(TmpBmp);
  DrawTextW(TmpBmp.Canvas.Handle, PWideChar(Text), Length(Text), R, Flags);

  h := TmpBmp.Height - 1;
  w := TmpBmp.Width  - 1;

  if lRect.Top + h >= Bmp.Height then
    h := Bmp.Height - 1 - lRect.Top;

  if lRect.Left + w >= Bmp.Width then
    w := Bmp.Width - 1 - lRect.Left;

  if lRect.Top < 0 then
    miny := -lRect.Top
  else
    miny := 0;

  if lRect.Left < 0 then
    minx := -lRect.Left
  else
    minx := 0;

  CC.A := MaxByte;
  SrcColor.C := Color;
  if InitLine(TmpBmp, Pointer(S0), DeltaS) and InitLine(Bmp, Pointer(D0), DeltaD) then
    with SrcColor do
      if Enabled then
        for y := miny to h do begin
          S := Pointer(PAnsiChar(S0) + DeltaS * Y);
          D := Pointer(PAnsiChar(D0) + DeltaD * (y + lRect.Top));
          for x := minx to w do
            with S[x] do
              if (SC <> clWhite) and (SI <> -1) then
                with D[x + lRect.Left] do
                  if SC = 0 then begin
                    DA := max(DA, MaxByte - 1);
                    DR := (DR - R + R shl 8) * DA shr 16;
                    DG := (DG - G + G shl 8) * DA shr 16;
                    DB := (DB - B + B shl 8) * DA shr 16;
                  end
                  else begin
                    bC := Round(0.299 * SR + 0.587 * SG + 0.114 * SB);
                    DA := max(DA, MaxByte - bC);
                    DR := ((DR - R) * bC + R shl 8) * DA shr 16;
                    DG := ((DG - G) * bC + G shl 8) * DA shr 16;
                    DB := ((DB - B) * bC + B shl 8) * DA shr 16;
                  end;
        end
      else
        for y := miny to h do begin
          S := Pointer(PAnsiChar(S0) + DeltaS * Y);
          D := Pointer(PAnsiChar(D0) + DeltaD * (y + lRect.Top));
          for x := minx to w do
            with S[x] do
              if (SC <> clWhite) and (SI <> -1) then
                with D[x + lRect.Left] do begin
                  DA := MaxByte;
                  DR := ((DR - R) * SR + R shl 8) shr 8;
                  DG := ((DG - G) * SG + G shl 8) shr 8;
                  DB := ((DB - B) * SB + B shl 8) shr 8;
                end;
        end;

  FreeAndNil(TmpBmp);
end;


procedure WriteText32(const Bmp: TBitmap; Text: PacChar; Enabled: boolean; var aRect: TRect; Flags: Cardinal; SkinIndex: integer; State: integer; SkinManager: TComponent; IsAnsi: boolean = False);
var
  R, Rd: TRect;
begin
 if (Text <> '') and not IsRectEmpty(aRect) then begin
  if SkinManager = nil then
    SkinManager := DefaultManager;

  if Assigned(SkinManager) then
    with TsSkinManager(SkinManager) do begin
      R := aRect;
      if IsValidSkinIndex(SkinIndex) then
        with CommonSkinData.gd[SkinIndex].Props[State], R do begin
          if Enabled then begin
            // Left
            if FontColor.Left <> -1 then begin
              Rd := Rect(Left - 1, Top, Right - 1, Bottom);
              WriteColor(Bmp, Text, True, Rd, Flags, FontColor.Left);
            end;
            // Top
            if FontColor.Top <> -1 then begin
              Rd := Rect(Left, Top - 1, Right, Bottom - 1);
              WriteColor(Bmp, Text, True, Rd, Flags, FontColor.Top);
            end;
            // Right
            if FontColor.Right <> -1 then begin
              Rd := Rect(Left + 1, Top, Right + 1, Bottom);
              WriteColor(Bmp, Text, True, Rd, Flags, FontColor.Right);
            end;
            // Bottom
            if FontColor.Bottom <> -1 then begin
              Rd := Rect(Left, Top + 1, Right, Bottom + 1);
              WriteColor(Bmp, Text, True, Rd, Flags, FontColor.Bottom);
            end;
          end;
          // Center
          WriteColor(Bmp, Text, Enabled, R, Flags, FontColor.Color);
        end
        else
          WriteColor(Bmp, Text, Enabled, R, Flags, Bmp.Canvas.Font.Color);
    end
  else
    WriteColor(Bmp, Text, Enabled, aRect, Flags, Bmp.Canvas.Font.Color);
 end;
end;


procedure FillDCBorder32(const Bmp: TBitmap; const aRect: TRect; const wl, wt, wr, wb: integer; const Color: TColor);
begin
  with aRect do begin
    FillRect32(Bmp, Rect(Left,       Top,         Right,     Top + wt),    Color);
    FillRect32(Bmp, Rect(Left,       Top + wt,    Left + wl, Bottom),      Color);
    FillRect32(Bmp, Rect(Left + wl,  Bottom - wb, Right,     Bottom),      Color);
    FillRect32(Bmp, Rect(Right - wr, Top + wt,    Right,     Bottom - wb), Color);
  end;
end;


procedure BlendColorRect(Bmp: TBitmap; R: TRect; Blend: byte; Color: TColor);
var
  S0, S: PRGBAArray_S;
  BlendColor: TsColor;
  bR, bG, bB, Delta, x, y: integer;
  BlendR: byte;
begin
  with R do begin
    if Left < 0 then
      Left := 0;

    if Top < 0 then
      Top := 0;

    if Right >= Bmp.Width then
      Right := Bmp.Width - 1;

    if Bottom >= Bmp.Height then
      Bottom := Bmp.Height - 1;

    BlendR := MaxByte - Blend;
    with BlendColor do begin
      C := ColorToRGB(Color);
      bR := R * BlendR;
      bG := G * BlendR;
      bB := B * BlendR;
    end;
    if InitLine(Bmp, Pointer(S0), Delta) then
      for y := Top to Bottom do begin
        S := Pointer(PAnsiChar(S0) + Delta * Y);
        for x := Left to Right do
          with S[X] do begin
            SR := (SR * Blend + bR) shr 8;
            SG := (SG * Blend + bG) shr 8;
            SB := (SB * Blend + bB) shr 8;
          end;
      end;
  end;
end;


function SwapColor(const Color: TColor): TColor;
{$IFDEF WIN64}
begin
  Result := Color;
{$ELSE}
asm
  BSWAP EAX
  ROR   EAX, 8
{$ENDIF}
end;


function SwapRedBlue(const Color: TColor): TColor;
begin
  with TsColor(Result), TsColor_RGB_(Color) do begin
    A := Alpha;
    R := Red;
    G := Green;
    B := Blue;
  end;
end;


procedure FillRect32(const Bmp: TBItmap; R: TRect; const Color: TColor; Alpha: byte = MaxByte);
var
  C: TsColor_;
  S0, S: PRGBAArray_;
  Delta, h, w, X, Y: integer;
begin
  with TdstRect(R), Bmp do begin
    h := Height;
    w := Width;
    if Height < R.Bottom then
      R.Bottom := Height;

    C.C := SwapRedBlue(Color);
    C.A := Alpha;
    if InitLine(Bmp, Pointer(S0), Delta) then // ToDo fast fill of line
      for Y := max(0, DTop) to min(h, DBottom) - 1 do begin
        S := Pointer(PAnsiChar(S0) + Delta * Y);
//        FillMemory(@D[0], @S[0], Result.Width * 4);

        for X := max(0, DLeft) to min(w, DRight) - 1 do
          S[X] := C;
      end;
  end;
end;


procedure FillRect32Excl(const Bmp: TBItmap; R, ExcludeR: TRect; const Color: TColor; Alpha: byte = MaxByte);
begin
  // Top
  FillRect32(Bmp, Rect(R.Left, R.Top, R.Right, ExcludeR.Top), Color, Alpha);
  // Left
  FillRect32(Bmp, Rect(R.Left, ExcludeR.Top, ExcludeR.Left, ExcludeR.Bottom), Color, Alpha);
  // Right
  FillRect32(Bmp, Rect(ExcludeR.Right, ExcludeR.Top, R.Right, ExcludeR.Bottom), Color, Alpha);
  // Bottom
  FillRect32(Bmp, Rect(R.Left, ExcludeR.Bottom, R.Right, R.Bottom), Color, Alpha);
end;


procedure BlendRect32(const Bmp: TBItmap; R: TRect; Color: TColor; BlendValue: byte; Alpha: byte = MaxByte);
var
  Col: TsColor_;
  S0, S: PRGBAArray_S;
  Delta, h, w, X, Y: integer;
begin
  w := Bmp.Width;
  h := Bmp.Height;
  if h < R.Bottom then
    R.Bottom := h;

  Col.C := SwapRedBlue(Color);
  Col.A := Alpha;
  if InitLine(Bmp, Pointer(S0), Delta) then
    with TdstRect(R), Col do
      for Y := max(0, DTop) to min(h, DBottom) - 1 do begin
        S := Pointer(PAnsiChar(S0) + Delta * Y);
        for X := max(0, DLeft) to min(w, DRight) - 1 do
          with S[X] do begin
            SR := SR - BlendValue * (SR - R) div 100;
            SG := SG - BlendValue * (SG - G) div 100;
            SB := SB - BlendValue * (SB - B) div 100;
            SA := Alpha;
          end;
      end;
end;


procedure FillAlphaRect(const Bmp: TBItmap; R: TRect; Value: byte = MaxByte; TransparentColor: TColor = clNone; ExcludeRects: TRects = nil);
var
  bAllowed: boolean;
  S0, S: PRGBAArray_;
  Delta, X, Y, w, h, i: integer;
begin
  if R.Left < 0 then
    R.Left := 0;

  if R.Top < 0 then
    R.Top := 0;

  w := min(WidthOf(R, True), Bmp.Width - R.Left) - 1;
  h := min(HeightOf(R, True), Bmp.Height - R.Top) - 1;
  if InitLine(Bmp, Pointer(S0), Delta) then
    if Assigned(ExcludeRects) and (Length(ExcludeRects) > 0) then begin
      if TransparentColor <> clNone then
        for Y := 0 to h do begin
          S := Pointer(PAnsiChar(S0) + Delta * (R.Top + Y));
          for X := 0 to w do begin
            bAllowed := True;
            for i := 0 to Length(ExcludeRects) - 1 do
              if PtInRect(ExcludeRects[i], Point(R.Left + X, R.Top + Y)) then begin
                bAllowed := False;
                Break;
              end;

            with S[R.Left + X] do
              if bAllowed then
                if C <> TransparentColor then
                  A := Value;
          end;
        end
      else
        for Y := 0 to h do begin
          S := Pointer(PAnsiChar(S0) + Delta * (R.Top + Y));
          for X := 0 to w do begin
            bAllowed := True;
            for i := 0 to Length(ExcludeRects) - 1 do
              if PtInRect(ExcludeRects[i], Point(R.Left + X, R.Top + Y)) then begin
                bAllowed := False;
                Break;
              end;

            if bAllowed then
              TsColor_(S[R.Left + X]).A := Value;
          end;
        end;
    end
    else
      if TransparentColor <> clNone then
        for Y := 0 to h do begin
          S := Pointer(PAnsiChar(S0) + Delta * (R.Top + Y));
          for X := 0 to w do
            with S[R.Left + X] do
              if C <> TransparentColor then
                A := Value;
        end
      else
        for Y := 0 to h do begin
          S := Pointer(PAnsiChar(S0) + Delta * (R.Top + Y));
          for X := 0 to w do
            TsColor_(S[R.Left + X]).A := Value;
        end;
end;


procedure PrepareLightMask(var maskBmp: TBitmap; AWidth, AHeight: integer; AColor: TColor);
var
  m, iSize: integer;
begin
  m := min(max(AWidth, ScaleInt(100)), ScaleInt(200));
  iSize := 2 * m + 1;
  if (maskBmp = nil) or (maskBmp.Width <> iSize) or (maskBmp.Height <> iSize) or (maskBmp.Canvas.Brush.Color <> AColor) then begin
    if maskBmp <> nil then
      maskBmp.Free;

    maskBmp := MakeShadow(AColor, m, 0, 0, 0, m, False, False);
    maskBmp.Canvas.Brush.Color := AColor;
  end;
end;


procedure CheckEmptyAChannel(Bmp: TBitmap);
var
  w, h, x, y, DeltaS: integer;
  TransColor: TColor;
  S0, S: PRGBAArray_;
  Color: TsColor;
begin
  w := Bmp.Width - 1;
  h := Bmp.Height - 1;
  TransColor := clFuchsia;
  if InitLine(Bmp, Pointer(S0), DeltaS) then
    for Y := 0 to h do begin // Check if alphachannel is fully clear
      S := Pointer(PAnsiChar(S0) + DeltaS * Y);
      for X := 0 to w do begin
        if S[X].A <> 0 then begin
          TransColor := clNone;
          Break;
        end;
      end;
      if TransColor = clNone then
        Break;
    end;

  if TransColor = clFuchsia then begin
    TransColor := SwapRedBlue(Bmp.Canvas.Pixels[0, h]);
    for X := 0 to w do
      for Y := 0 to h do begin
        Color.C := GetAPixel(Bmp, X, Y).C;
        if Color.C <> TransColor then begin
          Color.A := MaxByte;
          SetAPixel(Bmp, X, Y, Color);
        end;
      end;
  end;
end;


{$IFNDEF ACHINTS}
procedure BmpDisabledKind(const Bmp: TBitmap; const DisabledKind: TsDisabledKind; Parent: TControl; const CI: TCacheInfo; Offset: TPoint);
var
  C: TColor;
  R: TRect;
begin
  if dkGrayed in DisabledKind then
    if DefDisabledSaturation = -100 then
      GrayScale(Bmp)
    else
      ChangeBitmapPixels(Bmp, ChangeColorSaturation, DefDisabledSaturation * MaxByte div 100, clFuchsia);

  if dkBlended in DisabledKind then begin
    R := MkRect(Bmp);
    if CI.Ready then begin
      OffsetRect(R, CI.X + Offset.X, CI.Y + Offset.Y);
      BlendTransRectangle(Bmp, 0, 0, CI.Bmp, R, DefBlendDisabled);
    end
    else begin
      if CI.FillColor <> clFuchsia then
        C := CI.FillColor
      else
        C := GetControlColor(Parent);

      BlendColorRect(Bmp, R, byte(DefBlendDisabled), C);
    end;
  end;
end;


procedure DrawSmallSkinRect(Bmp: TBitmap; aRect: TRect; const ci: TCacheInfo; const MaskData: TsMaskData; State: integer);
var
  x, y, w, h, h2, w2, w3, dw, dh, mw, mh, minhp, minwp, minh, minw: integer;
begin
  if (MaskData.Bmp = nil) and (MaskData.Manager <> nil) then
    with TsSkinManager(MaskData.Manager) do begin
      if (WidthOf(aRect) < 2) or (HeightOf(aRect) < 2) then
        Exit;

      if MaskData.ImageCount = 0 then
        Exit;

      if State >= MaskData.ImageCount then
        State := MaskData.ImageCount - 1;

      dw := State * WidthOf(MaskData.R) div (MaskData.ImageCount); // Width of mask
      dh := HeightOf(MaskData.R) div (1 + MaskData.MaskType);      // Height of mask
      w := WidthOf (MaskData.R) div (3 * MaskData.ImageCount);
      h := HeightOf(MaskData.R) div (3 * (1 + MaskData.MaskType));
      if WidthOf(aRect) < w * 2 then
        mw := WidthOf(aRect) div 2
      else
        mw := 0;

      if HeightOf(aRect) < h * 2 then
        mh := HeightOf(aRect) div 2
      else
        mh := 0;

      if mh > 0 then begin
        minh := mh;
        minhp := minh;
        if HeightOf(aRect) mod 2 <> 0 then
          inc(minhp);
      end
      else begin
        minh := h;
        minhp := h;
      end;

      if mw > 0 then begin
        minw := mw;
        minwp := minw;
        if WidthOf(aRect) mod 2 <> 0 then
          inc(minwp);
      end
      else begin
        minw := w;
        minwp := w;
      end;

      w3 := w * 3;
      with TdstRect(aRect), MaskData, TSrcRect(R), MasterBitmap.Canvas do
        if MaskType = 0 then begin
          // left - top
          CopyTransRect(Bmp, MasterBitmap, DLeft, DTop, Rect(SLeft + dw, STop, SLeft + dw + minw, STop + minh), clFuchsia, CI, True);
          // left - middle
          y := DTop + h;
          if DrawMode and BDM_STRETCH = 0 then begin
            while y < DBottom - h do begin
              BitBlt(Bmp.Canvas.Handle, DLeft, y, minw, h, Handle, SLeft + dw, STop + h, SRCCOPY);
              inc(y, h);
            end;
            if y < DBottom - h then
              BitBlt(Bmp.Canvas.Handle, DLeft, y, minw, h, Handle, SLeft + dw, STop + h, SRCCOPY);
          end
          else
            StretchBlt(Bmp.Canvas.Handle, DLeft, y, DLeft + minw, DBottom - minh - y, Handle, SLeft + dw, STop + h, minw, h, SRCCOPY);

          // top - middle
          x := DLeft + minw;
          if DrawMode and BDM_STRETCH = 0 then begin
            while x < DRight - w - minw do begin
              BitBlt(Bmp.Canvas.Handle, x, DTop, w, minh, Handle, SLeft + dw + w, STop, SRCCOPY);
              inc(x, w);
            end;
            if x < DRight - minw then
              BitBlt(Bmp.Canvas.Handle, x, DTop, DRight - minw - x, minh, Handle, SLeft + dw + w, STop, SRCCOPY);
          end
          else
            StretchBlt(Bmp.Canvas.Handle, x, DTop, DRight - minw - x, minh, Handle, SLeft + dw + w, STop, w, minh, SRCCOPY);

          // left - bottom
          CopyTransRect(Bmp, MasterBitmap, DLeft, DBottom - minhp, Rect(SLeft + dw, SBottom - minhp, SLeft + dw + minw - 1,
                        SBottom - 1), clFuchsia, CI, True);
          // bottom - middle
          x := DLeft + minw;
          if DrawMode and BDM_STRETCH = 0 then begin
            while x < DRight - w - minw do begin
              BitBlt(Bmp.Canvas.Handle, x, DBottom - minh, w, minh, Handle, SLeft + dw + w, SBottom - minh, SRCCOPY);
              inc(x, w);
            end;
            if x < DRight - minw then
              BitBlt(Bmp.Canvas.Handle, x, DBottom - minh, DRight - minw - x, minh, Handle, SLeft + dw + w,  SBottom - minh, SRCCOPY);
          end
          else
            StretchBlt(Bmp.Canvas.Handle, x, DBottom - minh, DRight - minw - x, minh, Handle, SLeft + dw + w, SBottom - minh, w, minh, SRCCOPY);
          // right - bottom
          CopyTransRect(Bmp, MasterBitmap, DRight - minwp, DBottom - minhp,
                        Rect(SLeft + dw + w3 - minwp, SBottom - minhp, SLeft + dw + w3 - 1, SBottom - 1), clFuchsia, CI, True);
          // right - top
          CopyTransRect(Bmp, MasterBitmap, DRight - minwp, DTop,
                        Rect(SLeft + dw + w3 - minwp, STop, SLeft + dw + w3 - 1, STop + minh - 1), clFuchsia, CI, True);
          // right - middle
          y := DTop + h;
          while y < DBottom - h do begin
            BitBlt(Bmp.Canvas.Handle, DRight - minwp, y, minwp, h, Handle,  SLeft + dw + w3 - minwp, STop + h, SRCCOPY);
            inc(y, h);
          end;
          if y < DBottom - h then
            BitBlt(Bmp.Canvas.Handle, DRight - minwp, y, minwp, DBottom - h - y, Handle,  SLeft + dw + w3 - minwp, STop + h, SRCCOPY);

          // Fill
          if DrawMode and BDM_FILL <> 0 then begin
            y := DTop + h;
            while y < DBottom - 2 * h do begin
              x := DLeft + w;
              while x < DRight - 2 * w do begin
                BitBlt(Bmp.Canvas.Handle, x, y, w, h, Handle, SLeft + dw + w, STop + h, SRCCOPY);
                inc(x, w);
              end;
              if x < DRight - w then
                BitBlt(Bmp.Canvas.Handle, x, y, DRight - w - x, DBottom - h - y, Handle, SLeft + dw + w, STop + h, SRCCOPY);

              inc(y, h);
            end;
            x := DLeft + w;
            if y < DBottom - h then begin
              while x < DRight - 2 * w do begin
                BitBlt(Bmp.Canvas.Handle, x, y, w, DBottom - h - y, Handle, SLeft + dw + w, STop + h, SRCCOPY);
                inc(x, w);
              end;
              if x < DRight - w then
                BitBlt(Bmp.Canvas.Handle, x, y, DRight - w - x, DBottom - h - y, Handle, SLeft + dw + w, STop + h, SRCCOPY);
            end;
          end;
        end
        else begin
          h2 := h * 2;
          w2 := w * 2;
          CopyMasterRect(Rect(DLeft, DTop, DLeft + minw + 1, DTop + minh + 1), Rect(SLeft + dw, STop, SLeft + dw + minw, STop + minh), Bmp, ci, MaskData);
          // left - middle
          y := DTop + h;
          while y < DBottom - h do begin
            CopyMasterRect(Rect(DLeft, y, DLeft + minw + 1, y + h + 1), Rect(SLeft + dw, STop + h, SLeft + dw + minw, STop + h2), Bmp, CI, MaskData);
            inc(y, h);
          end;
          if y < DBottom - h then
            CopyMasterRect(Rect(DLeft, y, DLeft + minw, DBottom - h), Rect(SLeft + dw, STop + h, SLeft + dw + minw, STop + dh - h), Bmp, CI, MaskData);
          // top - middle
          x := DLeft + w;
          while x < DRight - w2 do begin
            CopyMasterRect(Rect(x, DTop, x + w, DTop + minh), Rect(SLeft + dw + w, STop, SLeft + dw + w2, STop + minh), Bmp, CI, MaskData);
            inc(x, w);
          end;
          if x < DRight - w then
            CopyMasterRect(Rect(x, DTop, DRight - w, DTop + minh), Rect(SLeft + dw + w, STop, SLeft + dw + w2, STop + minh), Bmp, CI, MaskData);
          // left - bottom
          CopyMasterRect(Rect(DLeft, DBottom - minhp, DLeft + minw, DBottom), Rect(SLeft + dw, STop + dh - minhp, SLeft + dw + minw, STop + dh), Bmp, CI, MaskData);
          // bottom - middle
          x := DLeft + w;
          while x < DRight - w2 do begin
            CopyMasterRect(Rect(x, DBottom - minhp, x + w, DBottom), Rect(SLeft + dw + w, STop + dh - minhp, SLeft + dw + w2, STop + dh), Bmp, CI, MaskData);
            inc(x, w);
          end;
          if x < DRight - w then
            CopyMasterRect(Rect(x, DBottom - minhp, DRight - w, DBottom), Rect(SLeft + dw + w, STop + dh - minhp, SLeft + dw + w2, STop + dh), Bmp, CI, MaskData);
          // right - bottom
          CopyMasterRect(Rect(DRight - minwp, DBottom - minhp, DRight, DBottom), Rect(SLeft + dw + w3 - minwp, STop + dh - minhp, SLeft + dw + w3, STop + dh), Bmp, CI, MaskData);
          // right - top
          CopyMasterRect(Rect(DRight - minwp, DTop, DRight, DTop + minh), Rect(SLeft + dw + w3 - minwp, STop, SLeft + dw + w3, STop + minh), Bmp, CI, MaskData);
          // right - middle
          y := DTop + h;
          while y < DBottom - h do begin
            CopyMasterRect(Rect(DRight - minwp, y, DRight, y + h), Rect(SLeft + dw + w3 - minwp, STop + h, SLeft + dw + w3, STop + h2), Bmp, CI, MaskData);
            inc(y, h);
          end;
          if y < DBottom - h then
            CopyMasterRect(Rect(DRight - minwp, y, DRight, DBottom - h), Rect(SLeft + dw + w3 - minwp, STop + h, SLeft + dw + w3, STop + h2), Bmp, CI, MaskData);
          // Fill
          if DrawMode and BDM_FILL <> 0 then begin
            y := DTop + h;
            while y < DBottom - h do begin
              x := DLeft + w;
              while x < DRight - w2 do begin
                CopyMasterRect(Rect(x, y, x + w, y + h), Rect(SLeft + dw + w, STop + h, SLeft + dw + w2, STop + h2), Bmp, EmptyCI, MaskData);
                inc(x, w);
              end;
              if x < DRight - w then
                CopyMasterRect(Rect(x, y, DRight - w, y + h), Rect(SLeft + dw + w, STop + h, SLeft + dw + w2, STop + h2), Bmp, EmptyCI, MaskData);
              inc(y, h);
            end;
            x := DLeft + w;
            if y < DBottom - h then begin
              while x < DRight - w2 do begin
                CopyMasterRect(Rect(x, y, x + w, DBottom - h), Rect(SLeft + dw + w, STop + h, SLeft + dw + w2, STop + h2), Bmp, EmptyCI, MaskData);
                inc(x, w);
              end;
              if x < DRight - w then
                CopyMasterRect(Rect(x, y, DRight - w, DBottom - h), Rect(R.Left + dw + w, R.Top + h, R.Left + dw + w2, R.Top + h2), Bmp, EmptyCI, MaskData);
            end;
          end;
        end;
    end;
end;


procedure DrawSkinRect(aBmp: TBitmap; const aRect: TRect; const ci: TCacheInfo; const MaskData: TsMaskData; const State: integer; const UpdateCorners: boolean);
var
  BmpSrc: TBitmap;
  Stretch: boolean;
  x, y, w, h, dw, dh, dhm, NewState, lWidth, tWidth, rWidth, bWidth: integer;
begin
  if (State <> 0) or (MaskData.DrawMode and BDM_ACTIVEONLY = 0) then begin
    if (WidthOf(aRect) > 1) and (HeightOf(aRect) > 1) and (MaskData.Manager <> nil) then begin
      lWidth := MaskData.WL;
      tWidth := MaskData.WT;
      rWidth := MaskData.WR;
      bWidth := MaskData.WB;
      if lWidth + rWidth > WidthOf(aRect) then begin
        y := lWidth + rWidth - WidthOf(aRect);
        x := y div 2;
        dec(lWidth, x);
        dec(rWidth, x);
        if y mod 2 > 0 then
          dec(rWidth);

        if lWidth < 0 then
          lWidth := 0;

        if rWidth < 0 then
          rWidth := 0;
      end;
      if tWidth + bWidth > HeightOf(aRect) then begin
        y := tWidth + bWidth - HeightOf(aRect);
        x := y div 2;
        dec(tWidth, x);
        dec(bWidth, x);
        if y mod 2 > 0 then
          dec(bWidth);

        if tWidth < 0 then
          tWidth := 0;

        if bWidth < 0 then
          bWidth := 0;
      end;
      if State >= MaskData.ImageCount then
        NewState := MaskData.ImageCount - 1
      else
        NewState := State;

      dw := MaskData.Width;
      dh := MaskData.Height;
      if MaskData.MaskType = 0 then
        dhm := 0
      else
        dhm := dh;

      if MaskData.DrawMode and BDM_STRETCH <> 0 then begin
        Stretch := True;
        if SetStretchBltMode(aBmp.Canvas.Handle, COLORONCOLOR) = 0 then
          CheckLastError
        else
          ResetLastError;
      end
      else
        Stretch := False;

      w := dw - lWidth - rWidth;
      if w >= 0 then begin // Width of middle piece must be > 0
        h := dh - tWidth - bWidth;
        if h >= 0 then begin // Height of middle piece must be > 0
          dw := dw * NewState; // Offset of mask
          if MaskData.Bmp <> nil then
            BmpSrc := MaskData.Bmp
          else
            BmpSrc := TsSkinManager(MaskData.Manager).MasterBitmap;

          with aRect, MaskData, TSrcRect(R) do
            if MaskType = 0 then begin // Copy without mask
              if MaskData.DrawMode and BDM_ALPHA = 0 then begin
                // left - top
                CopyTransRect(aBmp, BmpSrc, Left, Top, Rect(SLeft + dw, STop, SLeft + dw + lWidth - 1, STop + tWidth - 1), clFuchsia, CI, UpdateCorners and (Cfg and 3 > 0));
                y := Top + tWidth;
                // left - middle
                if not Stretch then begin
                  if h > 0 then
                    while y < Bottom - h - bWidth do begin
                      BitBlt(aBmp.Canvas.Handle, Left, y, lWidth, h, BmpSrc.Canvas.Handle, SLeft + dw, STop + tWidth, SRCCOPY);
                      inc(y, h);
                    end;

                  if y < Bottom - bWidth then
                    BitBlt(aBmp.Canvas.Handle, Left, y, lWidth, Bottom - bWidth - y, BmpSrc.Canvas.Handle, SLeft + dw, STop + tWidth, SRCCOPY);
                end
                else
                  StretchBlt(aBmp.Canvas.Handle, Left, y, lWidth, Bottom - bWidth - y, BmpSrc.Canvas.Handle, SLeft + dw, STop + tWidth, lWidth, h, SRCCOPY);
                // top - middle
                x := Left + lWidth;
                if not Stretch then begin
                  if w > 0 then
                    while x < Right - w - rWidth do begin
                      BitBlt(aBmp.Canvas.Handle, x, Top, w, tWidth, BmpSrc.Canvas.Handle, SLeft + dw + lWidth, STop, SRCCOPY);
                      inc(x, w);
                    end;

                  if x < Right - rWidth then
                    BitBlt(aBmp.Canvas.Handle, x, Top, Right - rWidth - x, tWidth, BmpSrc.Canvas.Handle, SLeft + dw + lWidth, STop, SRCCOPY);
                end
                else
                  StretchBlt(aBmp.Canvas.Handle, x, Top, Right - rWidth - x, tWidth, BmpSrc.Canvas.Handle, SLeft + dw + lWidth, STop, w, tWidth, SRCCOPY);

                // left - bottom
                CopyTransRect(aBmp, BmpSrc, Left, Bottom - bWidth, Rect(SLeft + dw, SBottom - bWidth, SLeft + dw + lWidth - 1, SBottom - 1), clFuchsia, CI, UpdateCorners and (Cfg and 3 > 0));
                // bottom - middle
                x := Left + lWidth;
                if not Stretch then begin
                  if w > 0 then
                    while x < Right - w - rWidth do begin
                      BitBlt(aBmp.Canvas.Handle, x, Bottom - bWidth, w, bWidth, BmpSrc.Canvas.Handle, SLeft + dw + lWidth, SBottom - bWidth, SRCCOPY);
                      inc(x, w);
                    end;

                  if x < Right - rWidth then
                    BitBlt(aBmp.Canvas.Handle, x, Bottom - bWidth, Right - rWidth - x, bWidth, BmpSrc.Canvas.Handle, SLeft + dw + lWidth, SBottom - bWidth, SRCCOPY);
                end
                else
                  StretchBlt(aBmp.Canvas.Handle, x, Bottom - bWidth, Right - rWidth - x, bWidth, BmpSrc.Canvas.Handle, SLeft + dw + lWidth, SBottom - bWidth, w, bWidth, SRCCOPY);

                // right - bottom
                CopyTransRect(aBmp, BmpSrc, Right - rWidth, Bottom - bWidth, Rect(SLeft + dw + w + lWidth, SBottom - bWidth, SLeft + dw + w + lWidth + rWidth - 1, SBottom - 1), clFuchsia, CI, UpdateCorners and (Cfg and 3 > 0));
                // right - top
                CopyTransRect(aBmp, BmpSrc, Right - rWidth, Top, Rect(SLeft + dw + w + lWidth, STop, SLeft + dw + w + lWidth + rWidth - 1, STop + tWidth - 1), clFuchsia, CI, UpdateCorners and (Cfg and 3 > 0));
                y := Top + tWidth;
                // right - middle
                if not Stretch then begin
                  if h > 0 then
                    while y < Bottom - h - bWidth do begin
                      BitBlt(aBmp.Canvas.Handle, Right - rWidth, y, rWidth, h, BmpSrc.Canvas.Handle, SLeft + dw + lWidth + w, STop + tWidth, SRCCOPY);
                      inc(y, h);
                    end;

                  if y < Bottom - bWidth then
                    BitBlt(aBmp.Canvas.Handle, Right - rWidth, y, rWidth, Bottom - bWidth - y, BmpSrc.Canvas.Handle, SLeft + dw + w + lWidth, STop + tWidth, SRCCOPY);
                end
                else
                  StretchBlt(aBmp.Canvas.Handle, Right - rWidth, y, rWidth, Bottom - bWidth - y, BmpSrc.Canvas.Handle, SLeft + dw + lWidth + w, STop + tWidth, rWidth, h, SRCCOPY);
                // Fill
                if DrawMode and BDM_FILL <> 0 then begin
                  if not Stretch then begin
                    y := Top + tWidth;
                    if h > 0 then
                      while y < Bottom - h - bWidth do begin
                        x := Left + lWidth;
                        if w > 0 then
                          while x < Right - w - rWidth do begin
                            BitBlt(aBmp.Canvas.Handle, x, y, w, h, BmpSrc.Canvas.Handle, SLeft + dw + lWidth, STop + tWidth, SRCCOPY);
                            inc(x, w);
                          end;

                        if x < Right - rWidth then
                          BitBlt(aBmp.Canvas.Handle, x, y, Right - rWidth - x,  Bottom - bWidth - y, BmpSrc.Canvas.Handle, SLeft + dw + lWidth, STop + tWidth, SRCCOPY);

                        inc(y, h);
                      end;

                    x := Left + lWidth;
                    if y < Bottom - bWidth then begin
                      if w > 0 then
                        while x < Right - w - rWidth do begin
                          BitBlt(aBmp.Canvas.Handle, x, y, w, Bottom - bWidth - y, BmpSrc.Canvas.Handle, SLeft + dw + lWidth, STop + tWidth, SRCCOPY);
                          inc(x, w);
                        end;

                      if x < Right - rWidth then
                        BitBlt(aBmp.Canvas.Handle, x, y, Right - rWidth - x, Bottom - bWidth - y, BmpSrc.Canvas.Handle, SLeft + dw + lWidth, STop + tWidth, SRCCOPY);
                    end;
                  end
                  else begin
                    y := Top + tWidth;
                    x := Left + lWidth;
                    StretchBlt(aBmp.Canvas.Handle, x, y, Right - rWidth - x, Bottom - bWidth - y, BmpSrc.Canvas.Handle, SLeft + dw + lWidth, STop + tWidth, w, h, SRCCOPY);
                  end;
                end;
              end
              else begin
                // left - top
                PaintBmpRect32(aBmp, BmpSrc, Rect(SLeft + dw, STop, SLeft + dw + lWidth - 1, STop + tWidth - 1), Point(Left, Top));
                y := Top + tWidth;
                // left - middle
                if not Stretch then begin
                  if h > 0 then
                    while y < Bottom - h - bWidth do begin
                      PaintBmpRect32(aBmp, BmpSrc, Rect(SLeft + dw, STop + tWidth, SLeft + dw + lWidth, STop + tWidth + h), Point(Left, y));
                      inc(y, h);
                    end;

                  if y < Bottom - bWidth then
                    PaintBmpRect32(aBmp, BmpSrc, Rect(SLeft + dw, STop + tWidth, SLeft + dw + lWidth, STop + tWidth + Bottom - bWidth - y), Point(Left, y));
                end
                else
                  StretchBmpRect32(aBmp,   aRect.Left, y, lWidth, aRect.Bottom - bWidth - y,
                                   BmpSrc, SLeft + dw, STop + tWidth, lWidth, h);
                // top - middle
                x := Left + lWidth;
                if not Stretch then begin
                  if w > 0 then
                    while x < Right - w - rWidth do begin
                      PaintBmpRect32(aBmp, BmpSrc, Rect(SLeft + dw + lWidth, STop, SLeft + dw + lWidth + w, STop + tWidth), Point(x, Top));
                      inc(x, w);
                    end;

                  if x < Right - rWidth then
                    PaintBmpRect32(aBmp, BmpSrc, Rect(SLeft + dw + lWidth, STop, SLeft + dw + lWidth + Right - rWidth - x, STop + tWidth), Point(x, Top));
                end
                else
                  StretchBmpRect32(aBmp, x, Top, Right - rWidth - x, tWidth, BmpSrc, SLeft + dw + lWidth, STop, w, tWidth);

                // left - bottom
                PaintBmpRect32(aBmp, BmpSrc, Rect(SLeft + dw, SBottom - bWidth, SLeft + dw + lWidth - 1, SBottom - 1), Point(Left, Bottom - bWidth));
                // bottom - middle
                x := Left + lWidth;
                if not Stretch then begin
                  if w > 0 then
                    while x < Right - w - rWidth do begin
                      PaintBmpRect32(aBmp, BmpSrc, Rect(SLeft + dw + lWidth, SBottom - bWidth, SLeft + dw + lWidth + w, SBottom), Point(x, Bottom - bWidth));
                      inc(x, w);
                    end;

                  if x < Right - rWidth then
                    PaintBmpRect32(aBmp, BmpSrc, Rect(SLeft + dw + lWidth, SBottom - bWidth, SLeft + dw + lWidth + Right - rWidth - x, SBottom), Point(x, Bottom - bWidth));
                end
                else
                  StretchBmpRect32(aBmp, x, Bottom - bWidth, Right - rWidth - x, bWidth,
                                   BmpSrc, SLeft + dw + lWidth, SBottom - bWidth, w, bWidth);

                // right - bottom
                PaintBmpRect32(aBmp, BmpSrc, Rect(SLeft + dw + w + lWidth, SBottom - bWidth, SLeft + dw + w + lWidth + rWidth - 1, SBottom - 1), Point(Right - rWidth, Bottom - bWidth));
                // right - top
                PaintBmpRect32(aBmp, BmpSrc, Rect(SLeft + dw + w + lWidth, STop, SLeft + dw + w + lWidth + rWidth - 1, STop + tWidth - 1), Point(Right - rWidth, Top));
                y := Top + tWidth;
                // right - middle
                if not Stretch then begin
                  if h > 0 then
                    while y < Bottom - h - bWidth do begin
                      PaintBmpRect32(aBmp, BmpSrc, Rect(SLeft + dw + lWidth + w, STop + tWidth, SLeft + dw + lWidth + w + rWidth, STop + tWidth + h), Point(Right - rWidth, y));
                      inc(y, h);
                    end;

                  if y < Bottom - bWidth then
                    PaintBmpRect32(aBmp, BmpSrc, Rect(SLeft + dw + lWidth + w, STop + tWidth, SLeft + dw + lWidth + w + rWidth, STop + tWidth + Bottom - bWidth - y), Point(Right - rWidth, y));
                end
                else
                  StretchBmpRect32(aBmp,   Right - rWidth, y, rWidth, Bottom - bWidth - y,
                                   BmpSrc, SLeft + dw + lWidth + w, STop + tWidth, rWidth, h);

                // Fill
                if DrawMode and BDM_FILL <> 0 then begin
                  if not Stretch then begin
                    y := Top + tWidth;
                    if h > 0 then
                      while y < Bottom - h - bWidth do begin
                        x := Left + lWidth;
                        if w > 0 then
                          while x < Right - w - rWidth do begin
                            PaintBmpRect32(aBmp, BmpSrc, Rect(SLeft + dw + lWidth, STop + tWidth, SLeft + dw + lWidth + w, STop + tWidth + y), Point(x, y));
                            inc(x, w);
                          end;

                        if x < Right - rWidth then
                          PaintBmpRect32(aBmp, BmpSrc, Rect(SLeft + dw + lWidth, STop + tWidth, SLeft + dw + lWidth + Right - rWidth - x, STop + tWidth + Bottom - bWidth - y), Point(x, y));

                        inc(y, h);
                      end;

                    x := Left + lWidth;
                    if y < Bottom - bWidth then begin
                      if w > 0 then
                        while x < Right - w - rWidth do begin
                          PaintBmpRect32(aBmp, BmpSrc, Rect(SLeft + dw + lWidth, STop + tWidth, SLeft + dw + lWidth + w, STop + tWidth + Bottom - bWidth - y), Point(x, y));
                          inc(x, w);
                        end;

                      if x < Right - rWidth then
                        PaintBmpRect32(aBmp, BmpSrc, Rect(SLeft + dw + lWidth, STop + tWidth, SLeft + dw + lWidth + Right - rWidth - x, STop + tWidth + Bottom - bWidth - y), Point(x, y));
                    end;
                  end
                  else begin
                    y := Top + tWidth;
                    x := Left + lWidth;
                    StretchBmpRect32(aBmp,
                                     x, y, aRect.Right - rWidth - x, aRect.Bottom - bWidth - y,
                                     BmpSrc,
                                     SLeft + dw + lWidth, STop + tWidth, w, SBottom - bWidth - STop - tWidth);
                  end;
                end;
              end;
            end
            else begin
              // left - top
              CopyByMask(Rect(Left, Top, Left + lWidth, Top + tWidth), Rect(SLeft + dw, STop, SLeft + dw + lWidth, STop + tWidth), aBmp, BmpSrc, CI, UpdateCorners and (Cfg and 3 > 0), MaskData);
              y := Top + tWidth;
              // left - middle
              if not Stretch then begin
                while y < Bottom - h - bWidth do begin
                  CopyByMask(Rect(Left, y, Left + lWidth, y + h), Rect(SLeft + dw, STop + tWidth, SLeft + dw + lWidth, STop + h + tWidth), aBmp, BmpSrc, EmptyCI, False, MaskData);
                  inc(y, h);
                end;
                if y < Bottom - bWidth then
                  CopyByMask(Rect(Left, y, Left + lWidth, Bottom - bWidth), Rect(SLeft + dw, STop + tWidth, SLeft + dw + lWidth, STop + tWidth + h), aBmp, BmpSrc, EmptyCI, False, MaskData);
              end
              else
                StretchBltMask(Left, y, lWidth, Bottom - bWidth - y, SLeft + dw, STop + tWidth, lWidth, h, aBmp, BmpSrc, dhm);

              // top - middle
              x := Left + lWidth;
              if not Stretch then begin
                while x < Right - w - rWidth do begin
                  CopyByMask(Rect(x, Top, x + w, Top + tWidth), Rect(SLeft + dw + lWidth, STop, SLeft + dw + w + lWidth, STop + tWidth), aBmp, BmpSrc, EmptyCI, False, MaskData);
                  inc(x, w);
                end;
                if x < Right - rWidth then
                  CopyByMask(Rect(x, Top, Right - rWidth, Top + tWidth), Rect(SLeft + dw + lWidth, STop, SLeft + dw + w + lWidth, STop + tWidth), aBmp, BmpSrc, EmptyCI, False, MaskData);
              end
              else
                StretchBltMask(x, Top, Right - rWidth - x, tWidth, SLeft + dw + lWidth, STop, w, tWidth, aBmp, BmpSrc, dhm);

              // left - bottom
              CopyByMask(Rect(Left, Bottom - bWidth, Left + lWidth, Bottom), Rect(SLeft + dw, STop + dh - bWidth, SLeft + dw + lWidth, STop + dh), aBmp, BmpSrc, CI, UpdateCorners and (Cfg and 3 > 0), MaskData);
              // bottom - middle
              x := Left + lWidth;
              if not Stretch then begin
                while x < Right - w - rWidth do begin
                  CopyByMask(Rect(x, Bottom - bWidth, x + w, Bottom), Rect(SLeft + dw + lWidth, STop + dh - bWidth, SLeft + dw + w + lWidth, STop + dh), aBmp, bmpSrc, EmptyCI, False, MaskData);
                  inc(x, w);
                end;
                if x < Right - rWidth then
                  CopyByMask(Rect(x, Bottom - bWidth, Right - rWidth, Bottom), Rect(SLeft + dw + lWidth, STop + dh - bWidth, SLeft + dw + w + lWidth, STop + dh), aBmp, BmpSrc, EmptyCI, False, MaskData);
              end
              else
                StretchBltMask(x, Bottom - bWidth, Right - rWidth - x, bWidth, SLeft + dw + lWidth, STop + dh - bWidth, w, bWidth, aBmp, BmpSrc, dhm);

              // right - bottom
              CopyByMask(Rect(Right - rWidth, Bottom - bWidth, Right, Bottom), Rect(SLeft + dw + w + lWidth, STop + dh - bWidth, SLeft + dw + w + lWidth + rWidth, STop + dh), aBmp, BmpSrc, CI, UpdateCorners and (Cfg and 3 > 0), MaskData);
              // right - top
              CopyByMask(Rect(Right - rWidth, Top, Right, Top + tWidth), Rect(SLeft + dw + w + lWidth, STop, SLeft + dw + w + lWidth + rWidth, STop + tWidth), aBmp, BmpSrc, CI, UpdateCorners and (Cfg and 3 > 0), MaskData);
              // right - middle
              y := Top + tWidth;
              if not Stretch then begin
                while y < Bottom - h - bWidth do begin
                  CopyByMask(Rect(Right - rWidth, y, Right, y + h), Rect(SLeft + dw + w + lWidth, STop + tWidth, SLeft + dw + w + lWidth + rWidth, STop + h + tWidth), aBmp, BmpSrc, EmptyCI, False, MaskData);
                  inc(y, h);
                end;
                if y < Bottom - bWidth then
                  CopyByMask(Rect(Right - rWidth, y, Right, Bottom - bWidth), Rect(SLeft + dw + w + lWidth, STop + tWidth, SLeft + dw + w + lWidth + rWidth, STop + h + tWidth), aBmp, BmpSrc, EmptyCI, False, MaskData);
              end
              else
                StretchBltMask(Right - rWidth, y, rWidth, Bottom - bWidth - y, SLeft + dw + lWidth + w, STop + tWidth, rWidth, h, aBmp, BmpSrc, dhm);

              // Fill
              if DrawMode and BDM_FILL <> 0 then
                if Stretch then begin
                  y := Top + tWidth;
                  x := Left + lWidth;
                  StretchBltMask(x, y, Right - rWidth - x, Bottom - bWidth - y, SLeft + dw + lWidth, STop + tWidth, w, h, aBmp, BmpSrc, dhm, True)
                end
                else begin
                  y := Top + tWidth;
                  while y < Bottom - h - bWidth do begin
                    x := Left + lWidth;
                    while x < Right - w - rWidth do begin
                      CopyByMask(Rect(x, y, x + w, y + h), Rect(SLeft + dw + lWidth, STop + tWidth, SLeft + dw + w + lWidth, STop + h + tWidth), aBmp, BmpSrc, EmptyCI, False, MaskData);
                      inc(x, w);
                    end;
                    if x < Right - rWidth then
                      CopyByMask(Rect(x, y, Right - rWidth, y + h), Rect(SLeft + dw + lWidth, STop + tWidth, SLeft + dw + w + lWidth, STop + h + tWidth), aBmp, BmpSrc, EmptyCI, False, MaskData);

                    inc(y, h);
                  end;
                  x := Left + lWidth;
                  if y < Bottom - bWidth then begin
                    while x < Right - w - rWidth do begin
                      CopyByMask(Rect(x, y, x + w, Bottom - bWidth), Rect(SLeft + dw + lWidth, STop + tWidth, SLeft + dw + w + lWidth, STop + h + tWidth), aBmp, BmpSrc, EmptyCI, False, MaskData);
                      inc(x, w);
                    end;
                    if x < Right - rWidth then
                      CopyByMask(Rect(x, y, Right - rWidth, Bottom - bWidth), Rect(SLeft + dw + lWidth, STop + tWidth, SLeft + dw + w + lWidth, STop + h + tWidth), aBmp, BmpSrc, EmptyCI, False, MaskData);
                  end;
                end;
            end;
          end;
      end;
    end;
  end;
end;


procedure CopyMasterRect(R1, R2: TRect; const aBmp: TBitmap; const CI: TCacheInfo; const MaskData: TsMaskData);
var
  col_: TsColor_;
  S0, D0: Pointer;
  D: PRGBAArray_D;
  M: PRGBAArray_M;
  S: PRGBAArray_S;
  DeltaS, DeltaD, hd2, PosX, PosY, h, w, dx: Integer;
begin
  if MaskData.Manager <> nil then
    with TsSkinManager(MaskData.Manager) do begin
      h := Min(HeightOf(R1), HeightOf(R2));
      h := Min(h, aBmp.Height - R1.Top);
      h := Min(h, MasterBitmap.Height - R2.Top) - 1;
      if h >= 0 then begin
        w := Min(WidthOf(R1), WidthOf(R2));
        w := Min(w, aBmp.Width - R1.Left);
        w := Min(w, MasterBitmap.Width - R2.Left) - 1;
        if w >= 0 then begin
          if R1.Left < R2.Left then begin
            if R1.Left < 0 then begin
              inc(R2.Left, - R1.Left);
              dec(w, - R1.Left);
              R1.Left := 0;
            end;
          end
          else
            if R2.Left < 0 then begin
              inc(R1.Left, - R2.Left);
              dec(w, - R2.Left);
              R2.Left := 0;
            end;

          if R1.Top < R2.Top then begin
            if R1.Top < 0 then begin
              inc(R2.Top, - R1.Top);
              dec(h, - R1.Top);
              R1.Top := 0;
            end;
          end
          else
            if R2.Top < 0 then begin
              inc(R1.Top, - R2.Top);
              dec(h, - R2.Top);
              R2.Top := 0;
            end;

          hd2 := HeightOf(MaskData.R) div 2;
          if InitLine(MasterBitmap, Pointer(S0), DeltaS) and InitLine(aBmp, Pointer(D0), DeltaD) then begin
            col_.C := SwapRedBlue(CI.FillColor);
            if not CI.Ready then
              for PosY := 0 to h do begin
                D := Pointer(integer(D0) + DeltaD * (R1.Top + PosY));
                S := Pointer(integer(S0) + DeltaS * (R2.Top + PosY));
                M := Pointer(integer(S0) + DeltaS * (R2.Top + hd2 + PosY));
                for PosX := 0 to w do begin
                  dx := R2.Left + PosX;
                  with S[dx] do
                    if SC <> clFuchsia then
                      with D[R1.Left + PosX], M[dx] do begin
                        DR := ((DR - SR) * MR + SR shl 8) shr 8;
                        DG := ((DG - SG) * MG + SG shl 8) shr 8;
                        DB := ((DB - SB) * MB + SB shl 8) shr 8;
                      end
                    else
                      if CI.FillColor <> clFuchsia then
                        D[R1.Left + PosX].DC := col_.C;
                end;
              end
            else
              for PosY := 0 to h do begin
                D := Pointer(integer(D0) + DeltaD * (R1.Top + PosY));
                S := Pointer(integer(S0) + DeltaS * (R2.Top + PosY));
                M := Pointer(integer(S0) + DeltaS * (R2.Top + hd2 + PosY));
                for PosX := 0 to w do begin
                  dx := R2.Left + PosX;
                  with S[dx] do
                    if SC <> clFuchsia then
                      with D[R1.Left + PosX], M[dx] do begin
                        DR := ((DR - SR) * MR + SR shl 8) shr 8;
                        DG := ((DG - SG) * MG + SG shl 8) shr 8;
                        DB := ((DB - SB) * MB + SB shl 8) shr 8;
                      end
                    else
                      with CI do
                        if FillColor <> clFuchsia then
                          D[R1.Left + PosX].DC := col_.C
                        else
                          if (Bmp.Height > R1.Top + Y + PosY) and (R1.Top + Y + PosY >= 0) and (R1.Left + X + PosX >= 0) then
                            if Bmp.Width <= R1.Left + X + PosX then
                              Break
                            else
                              D[R1.Left + PosX].DC := GetAPixel(Bmp, R1.Left + X + PosX, R1.Top + Y + PosY).C;
                end;
              end;
          end;
        end;
      end;
    end;
end;


procedure CopyByMask32(R1, R2: TRect; const Bmp1, Bmp2: TBitmap; const CI: TCacheInfo; const MaskData: TsMaskData);
var
  S: PRGBAArray_S;
  M: PRGBAArray_M;
  S0, D0, D, CacheBMP: PRGBAArray_;
  PosX, PosY, h, w, dX1, dX2, hdiv2, k1, a_, DeltaS, DeltaD: integer;
begin
{$IFDEF TESTSLOWDETAILS} Exit;{$ENDIF}
  h := R1.Bottom - R1.Top;
  if h > R2.Bottom - R2.Top then
    h := R2.Bottom - R2.Top;

  if h > Bmp1.Height - R1.Top then
    h := Bmp1.Height - R1.Top;

  if h > Bmp2.Height - R2.Top then
    h := Bmp2.Height - R2.Top - 1
  else
    h := h - 1;

  if h >= 0 then begin
    w := R1.Right - R1.Left;
    if w > R2.Right - R2.Left then
      w := R2.Right - R2.Left;

    if w > Bmp1.Width - R1.Left then
      w := Bmp1.Width - R1.Left;

    if w > Bmp2.Width - R2.Left then
      w := Bmp2.Width - R2.Left - 1
    else
      w := w - 1;

    if w >= 0 then begin
      if R1.Left < R2.Left then begin
        if R1.Left < 0 then begin
          inc(R2.Left, - R1.Left);
          dec(w, - R1.Left);
          R1.Left := 0;
        end;
      end
      else
        if R2.Left < 0 then begin
          inc(R1.Left, - R2.Left);
          dec(w, - R2.Left);
          R2.Left := 0;
        end;

      if R1.Top < R2.Top then begin
        if R1.Top < 0 then begin
          inc(R2.Top, - R1.Top);
          dec(h, - R1.Top);
          R1.Top := 0;
        end;
      end
      else
        if R2.Top < 0 then begin
          inc(R1.Top, - R2.Top);
          dec(h, - R2.Top);
          R2.Top := 0;
        end;

      C1.A := 0;
      hdiv2 := (MaskData.R.Bottom - MaskData.R.Top) div 2;
      k1 := min(R2.Top + hdiv2, Bmp2.Height - h - 1); // Mask offset
      if InitLine(Bmp2, Pointer(S0), DeltaS) and InitLine(Bmp1, Pointer(D0), DeltaD) then begin
        if CI.Ready and (CI.Bmp <> nil) then begin
          if h >= CI.Bmp.Height then
            h := CI.Bmp.Height - 1;

          if R1.Top + CI.Y < 0 then begin
            inc(h, R1.Top + CI.Y);
            R1.Top := -CI.Y;
          end;

          with CI do
            for PosY := 0 to h do begin
              S := Pointer(PAnsiChar(S0) + DeltaS * (PosY + R2.Top));
              D := Pointer(PAnsiChar(D0) + DeltaD * (PosY + R1.Top));
              M := Pointer(PAnsiChar(S0) + DeltaS * (PosY + k1));
              CacheBMP := Bmp.ScanLine[R1.Top + PosY + Y];
              dX1 := R1.Left;
              dX2 := R2.Left;
              for PosX := 0 to w do begin
                with S[dX2] do
                  if SC <> clFuchsia then
                    with D[dX1], M[dX2] do begin
                      a_ := MaxByte - MR;
                      R := (R * MR + SR * a_) shr 8;
                      G := (G * MR + SG * a_) shr 8;
                      B := (B * MR + SB * a_) shr 8;
                      case A of
                        MaxByte: ;             // Do not change
                        $00: A := MaxByte - MR // Full transparency
                        else A := max(A, MaxByte - MR);
                      end;
                    end
                  else
                    D[dX1] := CacheBMP[dX1 + X];

                inc(dX1);
                inc(dX2);
              end;
            end;
        end
        else
          for PosY := 0 to h do begin
            S := Pointer(PAnsiChar(S0) + DeltaS * (PosY + R2.Top));
            D := Pointer(PAnsiChar(D0) + DeltaD * (PosY + R1.Top));
            M := Pointer(PAnsiChar(S0) + DeltaS * (PosY + k1));
            dX1 := R1.Left;
            dX2 := R2.Left;
            for PosX := 0 to w do begin
              with D[dX1], S[dX2] do
                if SC <> clFuchsia then
                  with M[dX2] do begin
                    a_ := MaxByte - MR;
                    R := (R * MR + SR * a_) shr 8;
                    G := (G * MR + SG * a_) shr 8;
                    B := (B * MR + SB * a_) shr 8;
                    case A of
                      MaxByte: ;             // Do not change
                      $00: A := MaxByte - MR // Full transparency
                      else A := max(A, MaxByte - MR);
                    end;
                  end
                else
                  A := 0;

              inc(dX1);
              inc(dX2);
            end;
          end;
      end;
    end;
  end;
end;


procedure CopyByMask32Ex(R1, R2: TRect; Bmp1, Bmp2: TBitmap; const CI: TCacheInfo; const MaskData: TsMaskData; Opacity: integer);
var
  S: PRGBAArray_S;
  M: PRGBAArray_M;
  D: PRGBAArray_D;
  S0, D0, CacheBMP: PRGBAArray_;
  aa, a_, DeltaS, DeltaD, PosX, PosY, h, w, dX1, dX2, hdiv2, k1: integer;
begin
{$IFDEF TESTSLOWDETAILS} Exit;{$ENDIF}
  h := R1.Bottom - R1.Top;
  if h > R2.Bottom - R2.Top then
    h := R2.Bottom - R2.Top;

  if h > Bmp1.Height - R1.Top then
    h := Bmp1.Height - R1.Top;

  if h > Bmp2.Height - R2.Top then
    h := Bmp2.Height - R2.Top - 1
  else
    h := h - 1;

  if h >= 0 then begin
    w := R1.Right - R1.Left;
    if w > R2.Right - R2.Left then
      w := R2.Right - R2.Left;

    if w > Bmp1.Width - R1.Left then
      w := Bmp1.Width - R1.Left;

    if w > Bmp2.Width - R2.Left then
      w := Bmp2.Width - R2.Left - 1
    else
      w := w - 1;

    if w >= 0 then begin
      if R1.Left < R2.Left then begin
        if R1.Left < 0 then begin
          inc(R2.Left, - R1.Left);
          dec(w, - R1.Left);
          R1.Left := 0;
        end;
      end
      else
        if R2.Left < 0 then begin
          inc(R1.Left, - R2.Left);
          dec(w, - R2.Left);
          R2.Left := 0;
        end;

      if R1.Top < R2.Top then begin
        if R1.Top < 0 then begin
          inc(R2.Top, - R1.Top);
          dec(h, - R1.Top);
          R1.Top := 0;
        end;
      end
      else
        if R2.Top < 0 then begin
          inc(R1.Top, - R2.Top);
          dec(h, - R2.Top);
          R2.Top := 0;
        end;

      C1.A := 0;
      hdiv2 := (MaskData.R.Bottom - MaskData.R.Top) div 2;
      k1 := min(R2.Top + hdiv2, Bmp2.Height - h - 1); // Mask offset
      if InitLine(Bmp2, Pointer(S0), DeltaS) and InitLine(Bmp1, Pointer(D0), DeltaD) then
        if CI.Ready and (CI.Bmp <> nil) then begin
          if h >= CI.Bmp.Height then
            h := CI.Bmp.Height - 1;

          for PosY := 0 to h do begin
            S := Pointer(PAnsiChar(S0) + DeltaS * (PosY + R2.Top));
            D := Pointer(PAnsiChar(D0) + DeltaD * (PosY + R1.Top));
            M := Pointer(PAnsiChar(S0) + DeltaS * (PosY + k1));
            CacheBMP := CI.Bmp.ScanLine[PosY + R1.Top + CI.Y];
            dX1 := R1.Left;
            dX2 := R2.Left;
            for PosX := 0 to w do begin
              with S[dX2] do
                if SC <> clFuchsia then
                  with D[dX1], M[dX2] do begin
                    a_ := (MaxByte - MR) * Opacity shr 8;
                    aa := (MaxByte - a_);
                    DR := (DR * aa + SR * a_) shr 8;
                    DG := (DG * aa + SG * a_) shr 8;
                    DB := (DB * aa + SB * a_) shr 8;
                    case DA of
                      0:   DA := MaxByte - MR; // Full transparency
                      MaxByte:                 // Do not change
                      else DA := max(DA, MaxByte - MR);
                    end;
                  end
                else
                  D[dX1].DC := CacheBMP[dX1 + CI.X].C;

              inc(dX1);
              inc(dX2);
            end;
          end;
        end
        else
          for PosY := 0 to h do begin
            S := Pointer(PAnsiChar(S0) + DeltaS * (PosY + R2.Top));
            D := Pointer(PAnsiChar(D0) + DeltaD * (PosY + R1.Top));
            M := Pointer(PAnsiChar(S0) + DeltaS * (PosY + k1));
            dX1 := R1.Left;
            dX2 := R2.Left;
            for PosX := 0 to w do begin
              with S[dX2], D[dX1] do begin
                if SC <> clFuchsia then
                  with M[dX2] do begin
                    a_ := (MaxByte - MR) * Opacity shr 8;
                    aa := MaxByte - a_;
                    DR := (DR * aa + SR * a_) shr 8;
                    DG := (DG * aa + SG * a_) shr 8;
                    DB := (DB * aa + SB * a_) shr 8;
                    case DA of
                      0: DA := a_; // Full transparency
//                      0:   DA := MaxByte - MR; // Full transparency
                      MaxByte:                 // Do not change
                      else DA := max(DA, MaxByte - MR);
                    end;
                  end
                else
                  DA := 0;
              end;
              inc(dX1);
              inc(dX2);
            end;
          end;
    end;
  end;
end;


procedure DrawSkinGlyph(Bmp: TBitmap; P: TPoint; State, AddedTransparency: integer; const MaskData: TsMaskData; const CI: TCacheInfo);
var
  w, h, cy, cx, dw: integer;
begin
  w := MaskData.Width;
  h := MaskData.Height;
  if State >= MaskData.ImageCount then
    State := MaskData.ImageCount - 1;

  dw := State * w;
  cy := iff(p.y < 0, -p.y, 0);
  cx := iff(p.x < 0, -p.x, 0);
  with MaskData.R do
    if MaskData.Bmp = nil then
      with TsSkinManager(MaskData.Manager) do begin
        if (MaskData.Manager <> nil) and (MaskData.ImageCount > 0) then
          if MaskData.MaskType > 0 then
            BlendGlyphByMask(Rect(p.x + cx, p.y + cy, p.x + w - 1 + cx, p.y + h + cy),
                             Rect(dw + cx + Left, cy + Top, dw + w - 1 + cx + Left, h - 1 + cy + Top),
                             Bmp, MasterBitmap, AddedTransparency, MaskData)
          else
            if AddedTransparency <> 1 then
              if MaskData.DrawMode and BDM_ALPHA = 0 then
                BlendTransRectangle(Bmp, p.x + cx, p.y + cy, MasterBitmap,
                                    Rect(dw + cx + Left, cy + Top, dw + cx + Left + w - 1, cy + Top + h - 1),
                                    127)
              else
                PaintBmpRect32(Bmp, MasterBitmap, Rect(dw + cx + Left, cy + Top, dw + cx + Left + w - 1, cy + Top + h - 1), Point(p.x + cx, p.y + cy), 127)
            else
              if MaskData.DrawMode and BDM_ALPHA = 0 then
                CopyTransRect(Bmp, MasterBitmap, p.x + cx, p.y + cy,
                              Rect(dw + cx + Left, cy + Top, dw + Left + w - 1, cy + Top + h - 1),
                              clFuchsia, CI, True)
              else
                PaintBmpRect32(Bmp, MasterBitmap,
                               Rect(dw + cx + Left, cy + Top, dw + cx + Left + w - 1, cy + Top + h - 1),
                               Point(p.x + cx, p.y + cy));
      end
    else
      if (Right <= MaskData.Bmp.Width) and (Bottom <= MaskData.Bmp.Height) then
        BlendGlyphByMask(Rect(p.x + cx, p.y + cy, p.x + w - 1 + cx, p.y + h + cy - 1),
                         Rect(dw + cx + Left, cy + Top, dw + w - 1 + cx + Left, h - 1 + cy + Top),
                         Bmp, MaskData.Bmp, AddedTransparency, MaskData);
end;


procedure StretchBltMask(DstX1, DstY1, Dst_Width, Dst_Height, SrcX1, SrcY1, Src_Width, Src_Height: integer; BmpDst, BmpSrc: TBitmap; MaskOffset: integer; NoStdStretch: boolean = False);
var
  CC: TsColor_;
  S0, S: PRGBAArray_S;
  D0, D: PRGBAArray_D;
  M0, M: PRGBAArray_M;
  maskBmp, Bmp, TmpBmp: TBitmap;
  DeltaM, DeltaS, DeltaD, X, Y, iY, iX: Integer;
begin
  if (DstX1 >= 0) and (DstY1 >= 0) and (Dst_Width >= 0) and (Dst_Height > 0) and (Dst_Width > 0) and (BmpDst.Height > 1) and (BmpDst.Width > 1) then
    if MaskOffset <> 0 then begin // If masked
      if (BmpSrc.Width >= SrcX1 + Src_Width) and (BmpSrc.Height >= SrcY1 + Src_Height) then
        if NoStdStretch and (Src_Width = 1) and (Src_Height = 1) then begin
          if SrcY1 + MaskOffset < BmpSrc.Height then
            if InitLine(BmpSrc, Pointer(S0), DeltaS) and InitLine(BmpDst, Pointer(D0), DeltaD) then begin
              S := Pointer(PAnsiChar(S0) + DeltaS * SrcY1);
              CC.C := S[SrcX1].SC;
              M := Pointer(PAnsiChar(S0) + DeltaS * (SrcY1 + MaskOffset));
              CC.A := M[SrcX1].MR; // Get mask value
              case CC.A of
                0: begin // If not transparent pixel
                  CC.C := SwapRedBlue(CC.C);
                  FillRect32(BmpDst, Rect(DstX1, DstY1, DstX1 + Dst_Width, DstY1 + Dst_Height), CC.C);
                end;

                $FF: // Dst Changed

                else
                  for Y := DstY1 to DstY1 + Dst_Height - 1 do begin
                    D := Pointer(PAnsiChar(D0) + DeltaD * Y);
                    for X := DstX1 to DstX1 + Dst_Width - 1 do
                      with D[X], CC do begin
                        DR := ((DR - R) * A + R shl 8) shr 8;
                        DG := ((DG - G) * A + G shl 8) shr 8;
                        DB := ((DB - B) * A + B shl 8) shr 8;
                      end;
                  end;
              end;
            end;
        end
        else begin
          Bmp := CreateBmp32(Dst_Width, Dst_Height);
          maskBmp := CreateBmp32(Dst_Width, Dst_Height);
          if ((Src_Width > Dst_Width) and (Src_Height = Dst_Height) or (Src_Width = Dst_Width) and (Src_Height > Dst_Height) or NoStdStretch) and (Dst_Width > 1) and (Dst_Height > 1) then begin
            TmpBmp := CreateBmp32(max(Src_Width, 2), max(Src_Height, 2));
            BitBlt(TmpBmp.Canvas.Handle, 0, 0, Src_Width, Src_Height, BmpSrc.Canvas.Handle, SrcX1, SrcY1, SRCCOPY);
            if Src_Width = 1 then
              BitBlt(TmpBmp.Canvas.Handle, 1, 0, 1, Src_Height, TmpBmp.Canvas.Handle, 0, 0, SRCCOPY);

            if Src_Height = 1 then
              BitBlt(TmpBmp.Canvas.Handle, 0, 1, Src_Width, 1, TmpBmp.Canvas.Handle, 0, 0, SRCCOPY);

            Stretch(TmpBmp, Bmp, Dst_Width, Dst_Height, ftMitchell);
            BitBlt(TmpBmp.Canvas.Handle, 0, 0, Src_Width, Src_Height, BmpSrc.Canvas.Handle, SrcX1, SrcY1 + MaskOffset, SRCCOPY);
            if Src_Width = 1 then
              BitBlt(TmpBmp.Canvas.Handle, 1, 0, 1, Src_Height, TmpBmp.Canvas.Handle, 0, 0, SRCCOPY);

            if Src_Height = 1 then
              BitBlt(TmpBmp.Canvas.Handle, 0, 1, Src_Width, 1, TmpBmp.Canvas.Handle, 0, 0, SRCCOPY);

            Stretch(TmpBmp, maskBmp, Dst_Width, Dst_Height, ftMitchell);
            TmpBmp.Free;
          end
          else begin
            Bmp.Canvas.Lock;
            maskBmp.Canvas.Lock;
            try
              StretchBlt(Bmp.Canvas.Handle, 0, 0, Dst_Width, Dst_Height, BmpSrc.Canvas.Handle, SrcX1, SrcY1, Src_Width, Src_Height, SRCCOPY);
              StretchBlt(maskBmp.Canvas.Handle, 0, 0, Dst_Width, Dst_Height, BmpSrc.Canvas.Handle, SrcX1, SrcY1 + MaskOffset, Src_Width, Src_Height, SRCCOPY);
            finally
              Bmp.Canvas.Unlock;
              maskBmp.Canvas.Unlock;
            end;
          end;
          iY := 0;
          if DstY1 + Dst_Height > BmpDst.Height then
            Dst_Height := BmpDst.Height - DstY1;

          if DstX1 + Dst_Width > BmpDst.Width then
            Dst_Width := BmpDst.Width - DstX1;

          if InitLine(Bmp, Pointer(S0), DeltaS) and InitLine(maskBmp, Pointer(M0), DeltaM) and InitLine(BmpDst, Pointer(D0), DeltaD) then
            for Y := DstY1 to DstY1 + Dst_Height - 1 do begin
              D := Pointer(PAnsiChar(D0) + DeltaD * Y);
              S := Pointer(PAnsiChar(S0) + DeltaS * iY);
              M := Pointer(PAnsiChar(M0) + DeltaM * iY);
              iX := 0;
              for X := DstX1 to DstX1 + Dst_Width - 1 do
                with D[X], S[iX], M[iX] do begin
                  DR := ((DR - SR) * MR + SR shl 8) shr 8;
                  DG := ((DG - SG) * MG + SG shl 8) shr 8;
                  DB := ((DB - SB) * MB + SB shl 8) shr 8;
                  DA := MaxByte;
                  inc(iX);
                end;

              inc(iY);
            end;

          maskBmp.Free;
          Bmp.Free;
        end
    end
    else
      StretchBlt(BmpDst.Canvas.Handle, DstX1, DstY1, Dst_Width, Dst_Height, BmpSrc.Canvas.Handle, SrcX1, SrcY1, Src_Width, Src_Height, SRCCOPY);
end;


procedure StretchBltMask32(DstX1, DstY1, Dst_Width, Dst_Height, SrcX1, SrcY1, Src_Width, Src_Height: integer; BmpDst, BmpSrc: TBitmap; MaskOffset: integer; NoStdStretch: boolean = False);
var
  aa, a_: byte;
  CC: TsColor_;
  S0, S: PRGBAArray_S;
  D0, D: PRGBAArray_D;
  M0, M: PRGBAArray_M;
  maskBmp, Bmp, TmpBmp: TBitmap;
  DeltaM, DeltaS, DeltaD, X, Y, iY, iX: Integer;
begin
  if (DstX1 >= 0) and (DstY1 >= 0) and (Dst_Width >= 0) and (Dst_Height > 0) and (Dst_Width > 0) and (BmpDst.Height > 1) and (BmpDst.Width > 1) then
    if MaskOffset <> 0 then begin // If masked
      if BmpSrc.Width < SrcX1 + Src_Width then
        Exit;

      if BmpSrc.Height < SrcY1 + Src_Height then
        Exit;

      if NoStdStretch and (Src_Width = 1) and (Src_Height = 1) then begin
        if SrcY1 + MaskOffset >= BmpSrc.Height then
          Exit;

        if InitLine(BmpSrc, Pointer(S0), DeltaS) and InitLine(BmpDst, Pointer(D0), DeltaD) then begin
          S := Pointer(PAnsiChar(S0) + DeltaS * SrcY1);
          M := Pointer(PAnsiChar(S0) + DeltaS * (SrcY1 + MaskOffset));
          CC.C := S[SrcX1].SC;
          CC.A := M[SrcX1].MR; // Get mask value
          with CC do
            case A of
              MaxByte:
                ; // Dst Changed
              0: begin // If not transparent pixel
                C := SwapRedBlue(C);
                FillRect32(BmpDst, Rect(DstX1, DstY1, DstX1 + Dst_Width, DstY1 + Dst_Height), C);
              end
              else
                for Y := DstY1 to DstY1 + Dst_Height - 1 do begin
                  D := Pointer(PAnsiChar(D0) + DeltaD * Y);
                  for X := DstX1 to DstX1 + Dst_Width - 1 do
                    with D[X] do begin
                      aa := min(A, DA);
                      a_ := MaxByte - aa;
                      DR := (DR * aa + R * a_) shr 8;
                      DG := (DG * aa + G * a_) shr 8;
                      DB := (DB * aa + B * a_) shr 8;
                      DA := max(DA, MaxByte - A);
                    end;
                end;
            end;
        End;
        Exit;
      end;

      Bmp     := CreateBmp32(Dst_Width, Dst_Height);
      maskBmp := CreateBmp32(Dst_Width, Dst_Height);
      if ((Src_Width > Dst_Width) and (Src_Height = Dst_Height) or (Src_Width = Dst_Width) and (Src_Height > Dst_Height) or NoStdStretch) and (Dst_Width > 1) and (Dst_Height > 1) then begin
        TmpBmp := CreateBmp32(max(Src_Width, 2), max(Src_Height, 2));
        BitBlt(TmpBmp.Canvas.Handle, 0, 0, Src_Width, Src_Height, BmpSrc.Canvas.Handle, SrcX1, SrcY1, SRCCOPY);
        if Src_Width = 1 then
          BitBlt(TmpBmp.Canvas.Handle, 1, 0, 1, Src_Height, TmpBmp.Canvas.Handle, 0, 0, SRCCOPY);

        if Src_Height = 1 then
          BitBlt(TmpBmp.Canvas.Handle, 0, 1, Src_Width, 1, TmpBmp.Canvas.Handle, 0, 0, SRCCOPY);

        Stretch(TmpBmp, Bmp, Dst_Width, Dst_Height, ftMitchell);
        BitBlt(TmpBmp.Canvas.Handle, 0, 0, Src_Width, Src_Height, BmpSrc.Canvas.Handle, SrcX1, SrcY1 + MaskOffset, SRCCOPY);
        if Src_Width = 1 then
          BitBlt(TmpBmp.Canvas.Handle, 1, 0, 1, Src_Height, TmpBmp.Canvas.Handle, 0, 0, SRCCOPY);

        if Src_Height = 1 then
          BitBlt(TmpBmp.Canvas.Handle, 0, 1, Src_Width, 1, TmpBmp.Canvas.Handle, 0, 0, SRCCOPY);

        Stretch(TmpBmp, maskBmp, Dst_Width, Dst_Height, ftHermite);
        TmpBmp.Free;
      end
      else begin
        StretchBlt(Bmp.Canvas.Handle,     0, 0, Dst_Width, Dst_Height, BmpSrc.Canvas.Handle, SrcX1, SrcY1,              Src_Width, Src_Height, SRCCOPY);
        StretchBlt(maskBmp.Canvas.Handle, 0, 0, Dst_Width, Dst_Height, BmpSrc.Canvas.Handle, SrcX1, SrcY1 + MaskOffset, Src_Width, Src_Height, SRCCOPY);
      end;

      iY := 0;
      if DstY1 + Dst_Height > BmpDst.Height then
        Dst_Height := BmpDst.Height - DstY1;

      if InitLine(Bmp, Pointer(S0), DeltaS) and InitLine(maskBmp, Pointer(M0), DeltaM) and InitLine(BmpDst, Pointer(D0), DeltaD) then
        for Y := DstY1 to DstY1 + Dst_Height - 1 do begin
          D := Pointer(PAnsiChar(D0) + DeltaD * Y);
          S := Pointer(PAnsiChar(S0) + DeltaS * iY);
          M := Pointer(PAnsiChar(M0) + DeltaM * iY);
          iX := 0;
          for X := DstX1 to DstX1 + Dst_Width - 1 do begin
            with D[X], S[iX], M[iX] do
              case MR of
                MaxByte:
                  ; // Continue;

                0: begin
                  DI := SI;
                  DA := MaxByte;
                end

                else begin
                  aa := MR;
//                  aa := min(MR, DA);
                  a_ := MaxByte - aa;
                  DR := (DR * aa + SR * a_) shr 8;
                  DG := (DG * aa + SG * a_) shr 8;
                  DB := (DB * aa + SB * a_) shr 8;
                  DA := max(DA, MaxByte - MR);
                end;
              end;

            inc(iX);
          end;
          inc(iY);
        end;

      maskBmp.Free;
      Bmp.Free;
    end
    else
      StretchBlt(BmpDst.Canvas.Handle, DstX1, DstY1, Dst_Width, Dst_Height, BmpSrc.Canvas.Handle, SrcX1, SrcY1, Src_Width, Src_Height, SRCCOPY);
end;


procedure StretchBltMask32Ex(DstX1, DstY1, Dst_Width, Dst_Height, SrcX1, SrcY1, Src_Width, Src_Height: integer; BmpDst, BmpSrc: TBitmap; MaskOffset: integer; Opacity: integer; NoStdStretch: boolean = False);
var
  DeltaM, DeltaS, DeltaD, X, Y, iY, iX: Integer;
  maskBmp, Bmp, TmpBmp: TBitmap;
  D0, D: PRGBAArray_D;
  M0, M: PRGBAArray_M;
  S0, S: PRGBAArray_S;
  CC: TsColor_RGB_;
  aa, a_: byte;
begin
  if (DstX1 >= 0) and (DstY1 >= 0) and (Dst_Width >= 0) and (Dst_Height > 0) and (Dst_Width > 0) and (BmpDst.Height > 1) and (BmpDst.Width > 1) then
    if MaskOffset <> 0 then begin // If masked
      if (BmpSrc.Width < SrcX1 + Src_Width) or (BmpSrc.Height < SrcY1 + Src_Height) then
        Exit;

      if NoStdStretch and (Src_Width = 1) and (Src_Height = 1) then begin
        if SrcY1 + MaskOffset >= BmpSrc.Height then
          Exit;

        if InitLine(BmpSrc, Pointer(S0), DeltaS) and InitLine(BmpDst, Pointer(D0), DeltaD) then begin
          S := Pointer(PAnsiChar(S0) + DeltaS * SrcY1);
          CC.Col := S[SrcX1].SC;
          M := Pointer(PAnsiChar(S0) + DeltaS * (SrcY1 + MaskOffset));
          CC.Alpha := M[SrcX1].MR; // Get mask value
          CC.Col := S[SrcX1].SC;
          CC.Alpha := M[SrcX1].MR; // Get mask value
          case CC.Alpha of
            MaxByte:
              ; // Dst Changed

            0: begin // If not transparent pixel
              CC.Col := SwapRedBlue(CC.Col);
              FillRect32(BmpDst, Rect(DstX1, DstY1, DstX1 + Dst_Width, DstY1 + Dst_Height), CC.Col);
            end

            else
              for Y := DstY1 to DstY1 + Dst_Height - 1 do begin
                D := Pointer(PAnsiChar(D0) + DeltaD * Y);
                for X := DstX1 to DstX1 + Dst_Width - 1 do
                  with D[X], CC do begin
                    a_ := (MaxByte - CC.Alpha) * Opacity shr 8;
                    aa := (MaxByte - a_);
                    DR := (DR * aa + Red   * a_) shr 8;
                    DG := (DG * aa + Green * a_) shr 8;
                    DB := (DB * aa + Blue  * a_) shr 8;
                    case DA of
                      0: DA := MaxByte - Alpha; // Full transparency
                      $FF:                      // Do not change
                      else begin
                        aa := MaxByte - Alpha;
                        if aa > DA then
                          DA := aa;
                      end;
                    end;
                  end;
              end;
          end;
        end;
      end
      else begin
        Bmp := CreateBmp32(Dst_Width, Dst_Height);
        maskBmp := CreateBmp32(Dst_Width, Dst_Height);
        if ((Src_Width > Dst_Width) and
              (Src_Height = Dst_Height) or (Src_Width = Dst_Width) and
                (Src_Height > Dst_Height) or NoStdStretch) and
                  (Dst_Width > 1) and
                    (Dst_Height > 1) then begin
          TmpBmp := CreateBmp32(max(Src_Width, 2), max(Src_Height, 2));
          BitBlt(TmpBmp.Canvas.Handle, 0, 0, Src_Width, Src_Height, BmpSrc.Canvas.Handle, SrcX1, SrcY1, SRCCOPY);
          if Src_Width = 1 then
            BitBlt(TmpBmp.Canvas.Handle, 1, 0, 1, Src_Height, TmpBmp.Canvas.Handle, 0, 0, SRCCOPY);

          if Src_Height = 1 then
            BitBlt(TmpBmp.Canvas.Handle, 0, 1, Src_Width, 1, TmpBmp.Canvas.Handle, 0, 0, SRCCOPY);

          Stretch(TmpBmp, Bmp, Dst_Width, Dst_Height, ftMitchell);
          BitBlt(TmpBmp.Canvas.Handle, 0, 0, Src_Width, Src_Height, BmpSrc.Canvas.Handle, SrcX1, SrcY1 + MaskOffset, SRCCOPY);
          if Src_Width = 1 then
            BitBlt(TmpBmp.Canvas.Handle, 1, 0, 1, Src_Height, TmpBmp.Canvas.Handle, 0, 0, SRCCOPY);

          if Src_Height = 1 then
            BitBlt(TmpBmp.Canvas.Handle, 0, 1, Src_Width, 1, TmpBmp.Canvas.Handle, 0, 0, SRCCOPY);

          Stretch(TmpBmp, maskBmp, Dst_Width, Dst_Height, ftHermite);
          TmpBmp.Free;

        end
        else begin
          StretchBlt(Bmp.Canvas.Handle, 0, 0, Dst_Width, Dst_Height, BmpSrc.Canvas.Handle, SrcX1, SrcY1, Src_Width, Src_Height, SRCCOPY);
          StretchBlt(maskBmp.Canvas.Handle, 0, 0, Dst_Width, Dst_Height, BmpSrc.Canvas.Handle, SrcX1, SrcY1 + MaskOffset, Src_Width, Src_Height, SRCCOPY);
        end;

        iY := 0;
        if DstY1 + Dst_Height > BmpDst.Height then
          Dst_Height := BmpDst.Height - DstY1;

        if DstX1 + Dst_Width > BmpDst.Width then
          Dst_Width := BmpDst.Width - DstX1;

        if InitLine(Bmp, Pointer(S0), DeltaS) and InitLine(maskBmp, Pointer(M0), DeltaM) and InitLine(BmpDst, Pointer(D0), DeltaD) then
          for Y := DstY1 to DstY1 + Dst_Height - 1 do begin
            D := Pointer(PAnsiChar(D0) + DeltaD * Y);
            S := Pointer(PAnsiChar(S0) + DeltaS * iY);
            M := Pointer(PAnsiChar(M0) + DeltaM * iY);
            iX := 0;
            for X := DstX1 to DstX1 + Dst_Width - 1 do begin
              with D[X], S[iX], M[iX] do
                case MR of
                  MaxByte: ; // Continue;

                  0: begin
                    a_ := Opacity;
                    aa := MaxByte - a_;
                    DR := (DR * aa + SR * a_) shr 8;
                    DG := (DG * aa + SG * a_) shr 8;
                    DB := (DB * aa + SB * a_) shr 8;
                    DA := max(DA, MA * a_ shr 8);
//                    DA := MaxByte;
                  end

                  else begin
                    a_ := (MaxByte - MR) * Opacity shr 8;
                    aa := (MaxByte - a_);
                    DR := (DR * aa + SR * a_) shr 8;
                    DG := (DG * aa + SG * a_) shr 8;
                    DB := (DB * aa + SB * a_) shr 8;
                    case DA of
                      0: DA := a_; // Full transparency
//                      0: DA := MaxByte - MR; // Full transparency

                      $FF:                   // Do not change

                      else begin
                        aa := MaxByte - MR;
                        if aa > DA then
                          DA := aa;
                      end;
                    end;
                  end;
                end;

              inc(iX);
            end;
            inc(iY);
          end;

        maskBmp.Free;
        Bmp.Free;
      end
    end
    else
      StretchBlt(BmpDst.Canvas.Handle, DstX1, DstY1, Dst_Width, Dst_Height, BmpSrc.Canvas.Handle, SrcX1, SrcY1, Src_Width, Src_Height, SRCCOPY);
end;


{
procedure BlendColorByMask(DstBmp, MaskBmp: TBitmap; DstPoint: TPoint; SrcRect: TRect; Color: TsColor);
var
  M0, M: PByteArray;
  D0, D: PRGBAArray_D;
  bMask, bMask2: byte;
  X, Y, DeltaD, DeltaM: integer;
begin
  // Warning! Here is no bounds checking, maskBmp should be always smaller or equal dstBmp
  if InitLine(maskBmp, Pointer(M0), DeltaM) and InitLine(dstBmp, Pointer(D0), DeltaD) then
    for Y := 0 to maskBmp.Height - 1 do begin
      D := Pointer(PAnsiChar(D0) + DeltaD * (dstPoint.Y + Y));
      M := Pointer(PAnsiChar(M0) + DeltaM * Y);
      for X := 0 to maskBmp.Width - 1 do
        if M[X] <> MaxByte then begin
          bMask := M[X];
          bMask2 := MaxByte - bMask;
          with D[dstPoint.X + X], Color do begin
            DR := (R * bMask2 + DR * bMask) shr 8;
            DG := (G * bMask2 + DG * bMask) shr 8;
            DB := (B * bMask2 + DB * bMask) shr 8;
            DA := max(DA, bMask2 * A div 256);
          end;
        end;
      end;
end;
}

procedure BlendGlyphByMask(R1, R2: TRect; Bmp1, Bmp2: TBitmap; TransparencyMode: integer; const MaskData: TsMaskData);
var
  b, b1: byte;
  M: PRGBAArray_M;
  D0, D: PRGBAArray_D;
  S0, S: PRGBAArray_S;
  DeltaS, DeltaD, X, Y, h, w, hdiv2, dx1, dy1, dx2, dy2: integer;
begin
  h := min(HeightOf(R1), HeightOf(R2));
  h := min(h, Bmp1.Height - R1.Top - 1);
  w := min(WidthOf(R1), WidthOf(R2));
  w := min(w, Bmp1.Width - R1.Left - 1);
  if MaskData.MaskType = 0 then
    if MaskData.DrawMode and BDM_ALPHA = 0 then
      if TransparencyMode <> 1 then
        CopyTransRectA(Bmp1, Bmp2, R1.Left, R1.Top, R2, clFuchsia, EmptyCI, 180)//28)
      else
        CopyTransRectA(Bmp1, Bmp2, R1.Left, R1.Top, R2, clFuchsia, EmptyCI, 0)
    else begin
      h := min(h, Bmp2.Height - R2.Top - 1);
//      if R2.Top + h <= Bmp2.Height then
      begin
        dy1 := R1.Top;
        dy2 := R2.Top;
        if Bmp1.PixelFormat = pf32bit then
          if InitLine(Bmp2, Pointer(S0), DeltaS) and InitLine(Bmp1, Pointer(D0), DeltaD) then
            for Y := 0 to h do begin
              S := Pointer(PAnsiChar(S0) + DeltaS * dy2);
              D := Pointer(PAnsiChar(D0) + DeltaD * dy1);
              dx1 := R1.Left;
              dx2 := R2.Left;
              for X := 0 to w do begin
                with D[dx1], S[dx2] do begin
                  b := MaxByte - SA;
                  if TransparencyMode <> 1 then begin
                    b1 := min(b + 100, MaxByte);
                    DR := ((DR - SR) * b1 + SR shl 8) shr 8;
                    DG := ((DG - SG) * b1 + SG shl 8) shr 8;
                    DB := ((DB - SB) * b1 + SB shl 8) shr 8;
                  end
                  else begin
                    DR := ((DR - SR) * b + SR shl 8) shr 8;
                    DG := ((DG - SG) * b + SG shl 8) shr 8;
                    DB := ((DB - SB) * b + SB shl 8) shr 8;
                  end;
                  DA := DA + ((MaxByte - DA) * (MaxByte - b)) shr 8;
                end;
                inc(dx1);
                inc(dx2);
              end;
              inc(dy1);
              inc(dy2);
            end
      end;
    end
  else begin
    hdiv2 := HeightOf(MaskData.R) div (MaskData.MaskType + 1);
    h := min(h, Bmp2.Height - R2.Top - hdiv2 - 1);
    if R2.Top + h <= Bmp2.Height then begin
      dy1 := R1.Top;
      dy2 := R2.Top;
      if Bmp1.PixelFormat = pf32bit then
        if InitLine(Bmp2, Pointer(S0), DeltaS) and InitLine(Bmp1, Pointer(D0), DeltaD) then
          for Y := 0 to h do begin
            S := Pointer(PAnsiChar(S0) + DeltaS * dy2);
            D := Pointer(PAnsiChar(D0) + DeltaD * dy1);
            M := Pointer(PAnsiChar(S0) + DeltaS * (dy2 + hdiv2));
            dx1 := R1.Left;
            dx2 := R2.Left;
            for X := 0 to w do begin
              with D[dx1], S[dx2], M[dx2] do begin
                if (SC <> clFuchsia) and (MR <> MaxByte) then
                  if TransparencyMode <> 1 then begin
                    DR := ((DR - SR) * min(MR + 100, MaxByte) + SR shl 8) shr 8;
                    DG := ((DG - SG) * min(MG + 100, MaxByte) + SG shl 8) shr 8;
                    DB := ((DB - SB) * min(MB + 100, MaxByte) + SB shl 8) shr 8;
                  end
                  else begin
                    DR := ((DR - SR) * MR + SR shl 8) shr 8;
                    DG := ((DG - SG) * MG + SG shl 8) shr 8;
                    DB := ((DB - SB) * MB + SB shl 8) shr 8;
                  end;

                DA := DA + ((MaxByte - DA) * (MaxByte - MR)) shr 8;
              end;
              inc(dx1);
              inc(dx2);
            end;
            inc(dy1);
            inc(dy2);
          end
    end;
  end;
end;


procedure CopyTransCorner(const srcBmp: Graphics.TBitMap; const PosX, PosY: integer; SrcRect: TRect; const BGInfo: TacBGInfo; const SkinData: TsCommonData);
var
  S0, Src: PRGBAArray_S;
  D0, Dst: PRGBAArray_D;
  bWidth1, bHeight1, bWidth2, bHeight2, h, w, DeltaS, DeltaD, sX, sY, SrcX, DstX, DstY: integer;
begin
  if SrcRect.Top < 0 then
    SrcRect.Top := 0;

  if SrcRect.Bottom >= srcBmp.Height then
    SrcRect.Bottom := srcBmp.Height - 1;

  if SrcRect.Left < 0 then
    SrcRect.Left := 0;

  if SrcRect.Right >= srcBmp.Width then
    SrcRect.Right := srcBmp.Width - 1;

  h := HeightOf(SrcRect);
  w := WidthOf (SrcRect);
  bHeight1 := SkinData.FCacheBmp.Height;
  bWidth1 := SkinData.FCacheBmp.Width;
  if InitLine(srcBmp, Pointer(S0), DeltaS) and InitLine(SkinData.FCacheBmp, Pointer(D0), DeltaD) then
    with BGInfo do begin
      if (BgType = btCache) and (Bmp <> nil) then begin
        bHeight2 := Bmp.Height;
        bWidth2 := Bmp.Width;
        for sY := 0 to h do begin
          DstY := sY + PosY;
          if (Offset.Y + DstY < bHeight2) and (Offset.Y + DstY >= 0) then
            if IsValidIndex(DstY, bHeight1) then begin
              Src := Pointer(PAnsiChar(S0) + DeltaS * (sY + SrcRect.Top));
              Dst := Pointer(PAnsiChar(D0) + DeltaD * DstY);
              for sX := 0 to w do begin
                DstX := sX + PosX;
                if IsValidIndex(DstX, bWidth1) then begin
                  SrcX := sX + SrcRect.Left;
                  if Src[SrcX].SC = clFuchsia {if pixel is transparent} then 
                    if (Offset.X + DstX < bWidth2) and (Offset.X + DstX >= 0) then
                      Dst[DstX].DC := GetAPixel(Bmp, Offset.X + DstX, Offset.Y + DstY).C;
                end;
              end
            end;
        end
      end
      else
        if Color <> clFuchsia then
          for sY := 0 to h do begin
            DstY := sY + PosY;
            if IsValidIndex(DstY, bHeight1) then begin
              Src := Pointer(PAnsiChar(S0) + DeltaS * (sY + SrcRect.Top));
              Dst := Pointer(PAnsiChar(D0) + DeltaD * DstY);
              for sX := 0 to w do begin
                DstX := sX + PosX;
                if IsValidIndex(DstX, bWidth1) then
                  if Src[sX + SrcRect.Left].SC = clFuchsia then
                    Dst[DstX].DC := SwapRedBlue(Color);
              end
            end;
          end;
    end;
end;


procedure CopyMasterCorner(R1, R2: TRect; const aBmp: TBitmap; const BGInfo: TacBGInfo; const SkinData: TsCommonData; UseAlpha: boolean);
var
  PosX, PosY, h, w, DeltaS, DeltaD: integer;
  S0, S: PRGBAArray_S;
  D0, D: PRGBAArray_D;
  col_: TsColor_;
  md: TsMaskData;
  bMasked: boolean;
begin
  h := Min(HeightOf(R1), HeightOf(R2));
  h := Min(h, SkinData.FCacheBmp.Height - R1.Top);
  h := Min(h, aBmp.Height - R2.Top) - 1;
  if h >= 0 then begin
    w := Min(WidthOf(R1), WidthOf(R2));
    w := Min(w, SkinData.FCacheBmp.Width - R1.Left);
    w := Min(w, aBmp.Width - R2.Left) - 1;
    if w >= 0 then begin
      if R1.Left < R2.Left then begin
        if R1.Left < 0 then begin
          inc(R2.Left, - R1.Left);
          dec(w, - R1.Left);
          R1.Left := 0;
        end;
      end
      else
        if R2.Left < 0 then begin
          inc(R1.Left, - R2.Left);
          dec(w, - R2.Left);
          R2.Left := 0;
        end;

      if R1.Top < R2.Top then begin
        if R1.Top < 0 then begin
          inc(R2.Top, - R1.Top);
          dec(h, - R1.Top);
          R1.Top := 0;
        end;
      end
      else
        if R2.Top < 0 then begin
          inc(R1.Top, - R2.Top);
          dec(h, - R2.Top);
          R2.Top := 0;
        end;

      with R1, BGInfo do begin
        col_.C := Color;
        col_.B := TsColor(Color).B;
        col_.R := TsColor(Color).R;
        if InitLine(aBmp, Pointer(S0), DeltaS) and InitLine(SkinData.FCacheBmp, Pointer(D0), DeltaD) then
          if BgType <> btCache then
            for PosY := 0 to h do begin
              S := Pointer(PAnsiChar(S0) + DeltaS * (R2.Top + PosY));
              D := Pointer(PAnsiChar(D0) + DeltaD * (Top + PosY));
              for PosX := 0 to w do
                if S[R2.Left + PosX].SC = clFuchsia then
                  D[Left + PosX].DC := col_.C;
            end
          else
            if UseAlpha then
              for PosY := 0 to h do begin
                S := Pointer(PAnsiChar(S0) + DeltaS * (R2.Top + PosY));
                D := Pointer(PAnsiChar(D0) + DeltaD * (Top + PosY));
                if BgType <> btCache then begin
                  for PosX := 0 to w do
                    if S[R2.Left + PosX].SC = clFuchsia then
                      D[Left + PosX].DC := col_.C;
                end
                else
                  for PosX := 0 to w do
                    if S[R2.Left + PosX].SA = 0 then
                      if (Bmp.Height > Top + Offset.Y + PosY) and (Left + Offset.X + PosX >= 0) then
                        if (Bmp.Width <= Left + Offset.X + PosX) or (Top + Offset.Y + PosY < 0) then
                          Break
                        else
                          D[Left + PosX].DC := GetAPixel(Bmp, Left + Offset.X + PosX, Top + Offset.Y + PosY).C;
              end
            else
              if SkinData.SkinManager.gd[SkinData.SkinIndex].BorderIndex >= 0 then begin
                md := SkinData.SkinManager.ma[SkinData.SkinManager.gd[SkinData.SkinIndex].BorderIndex];
                bMasked := False;
                for PosY := 0 to h do begin
                  S := Pointer(PAnsiChar(S0) + DeltaS * (R2.Top + PosY + integer(bMasked) * md.Height));
                  D := Pointer(PAnsiChar(D0) + DeltaD * (Top + PosY));
                  if BgType <> btCache then begin
                    for PosX := 0 to w do
                      if S[R2.Left + PosX].SC = clFuchsia then
                        D[Left + PosX].DC := col_.C;
                  end
                  else
                    if not bMasked then begin
                      for PosX := 0 to w do
                        if S[R2.Left + PosX].SC = clFuchsia then
                          if (Bmp.Height > Top + Offset.Y + PosY) and (Left + Offset.X + PosX >= 0) then
                            if (Bmp.Width <= Left + Offset.X + PosX) or (Top + Offset.Y + PosY < 0) then
                              Break
                            else
                              D[Left + PosX].DC := GetAPixel(Bmp, Left + Offset.X + PosX, Top + Offset.Y + PosY).C;
                    end
                    else
                      for PosX := 0 to w do
                        if S[R2.Left + PosX].SC = -1 {White} then
                          if (Bmp.Height > Top + Offset.Y + PosY) and (Left + Offset.X + PosX >= 0) then
                            if (Bmp.Width <= Left + Offset.X + PosX) or (Top + Offset.Y + PosY < 0) then
                              Break
                            else
                              D[Left + PosX].DC := GetAPixel(Bmp, Left + Offset.X + PosX, Top + Offset.Y + PosY).C;
                end;
              end;
      end;
    end;
  end;
end;


procedure UpdateCorners(SkinData: TsCommonData; State: integer; Corners: TsCorners = [scLeftTop, scLeftBottom, scRightTop, scRightBottom]);
var
  w, iWidth, iHeight, dw, dh, wl, wt, wr, wb, rad, wRad, hRad: integer;
  MaskData: TsMaskData;
  BGInfo: TacBGInfo;
  srcBmp: TBitmap;
  p, pOffset: TPoint;
  R: TRect;
begin
  if (SkinData.SkinManager <> nil) and SkinData.SkinManager.IsActive then
    with SkinData do
      if not SkinData.CustomBorder.IsChanged then begin
        with CommonSkinData do
          if (SkinData.SkinIndex >= 0) and Assigned(FCacheBmp) then
            if SkinManager.IsValidImgIndex(gd[SkinData.SkinIndex].BorderIndex) then begin
              MaskData := ma[gd[SkinData.SkinIndex].BorderIndex];
              if MaskData.Cfg and 1 <> 0 then begin // Corners has transparent pixels
                BGInfo.PleaseDraw := False;
                BGInfo.DrawDC := 0;
                if (FOwnerControl = nil) or (FOwnerControl.Parent = nil) then
                  GetBGInfo(@BGInfo, GetParent(FSWHandle))
                else
                  GetBGInfo(@BGInfo, FOwnerControl.Parent);

                iWidth  := FCacheBmp.Width;
                iHeight := FCacheBmp.Height;
                wl := MaskData.WL;
                wt := MaskData.WT;
                wr := MaskData.WR;
                wb := MaskData.WB;
                if (MaskData.ImageCount = 0) and (MaskData.Bmp <> nil) then begin // If external
                  MaskData.MaskType := 1;
                  MaskData.ImageCount := 3;
                  MaskData.R := MkRect(MaskData.Bmp);
                end;
                if BGInfo.BgType = btCache then
                  if FOwnerControl <> nil then begin
                    inc(BGInfo.Offset.X, FOwnerControl.Left);
                    inc(BGInfo.Offset.Y, FOwnerControl.Top);
                  end
                  else begin
                    GetWindowRect(FSWHandle, R);
                    ScreenToClient(GetParent(FSWHandle), R.TopLeft);
                    inc(BGInfo.Offset.X, R.Left);
                    inc(BGInfo.Offset.Y, R.Top);
                  end;

                if State >= MaskData.ImageCount then
                  State := MaskData.ImageCount - 1;

                dw := State * MaskData.Width;
                dh := MaskData.Height;

                w := MaskData.Width - wl - wr;             // Width of piece of mask

                if MaskData.Bmp <> nil then
                  srcBmp := MaskData.Bmp
                else
                  srcBmp := SkinManager.MasterBitmap;

                with MaskData.R do
                  if MaskData.MaskType = 0 then // Copy without mask
                    if MaskData.DrawMode and BDM_ALPHA = 0 then begin
                      if scLeftTop in Corners then
                        CopyTransCorner(srcBmp, 0, 0, Rect(Left + dw, Top, Left + dw + wl - 1, Top + wt - 1), BGInfo, SkinData);

                      if scLeftBottom in Corners then
                        CopyTransCorner(srcBmp, 0, iHeight - wb, Rect(Left + dw, Bottom - wb, Left + dw + wl - 1, Bottom - 1), BGInfo, SkinData);

                      if scRightBottom in Corners then
                        CopyTransCorner(srcBmp, iWidth - wr, iHeight - wb, Rect(Left + dw + wl + w, Bottom - wb, Left + dw + wl + w + wr - 1, Bottom - 1), BGInfo, SkinData);

                      if scRightTop in Corners then
                        CopyTransCorner(srcBmp, iWidth - wr, 0, Rect(Left + dw + wl + w, Top, Left + dw + wl + w + wr - 1, Top + wt - 1), BGInfo, SkinData);
                    end
                    else begin
                      if scLeftTop in Corners then
                        CopyMasterCorner(MkRect(wl + 1, wt + 1), Rect(Left + dw, Top, Left + dw + wl, Top + wt), srcBmp, BGInfo, SkinData, True);

                      if scLeftBottom in Corners then
                        CopyMasterCorner(Rect(0, iHeight - wb, wl, iHeight), Rect(Left + dw, Top + dh - wb, Left + dw + wl, Top + dh), srcBmp, BGInfo, SkinData, True);

                      if scRightBottom in Corners then
                        CopyMasterCorner(Rect(iWidth - wr, iHeight - wb, iWidth, iHeight), Rect(Left + dw + wl + w, Top + dh - wb, Left + dw + wl + w + wr, Top + dh), srcBmp, BGInfo, SkinData, True);

                      if scRightTop in Corners then
                        CopyMasterCorner(Rect(iWidth - wr, 0, iWidth, wt), Rect(Left + dw + wl + w, Top, Left + dw + wl + w + wr, Top + wt), srcBmp, BGInfo, SkinData, True);
                    end
                  else begin
                    if scLeftTop in Corners then
                      CopyMasterCorner(MkRect(wl + 1, wt + 1), Rect(Left + dw, Top, Left + dw + wl, Top + wt), srcBmp, BGInfo, SkinData, False);

                    if scLeftBottom in Corners then
                      CopyMasterCorner(Rect(0, iHeight - wb, wl, iHeight), Rect(Left + dw, Top + dh - wb, Left + dw + wl, Top + dh), srcBmp, BGInfo, SkinData, False);

                    if scRightBottom in Corners then
                      CopyMasterCorner(Rect(iWidth - wr, iHeight - wb, iWidth, iHeight), Rect(Left + dw + wl + w, Top + dh - wb, Left + dw + wl + w + wr, Top + dh), srcBmp, BGInfo, SkinData, False);

                    if scRightTop in Corners then
                      CopyMasterCorner(Rect(iWidth - wr, 0, iWidth, wt), Rect(Left + dw + wl + w, Top, Left + dw + wl + w + wr, Top + wt), srcBmp, BGInfo, SkinData, False);
                  end;
              end;
            end
            else begin // Calculated border
              rad := gd[SkinData.SkinIndex].BorderRadius;
              if rad > 0 then begin
                BGInfo.PleaseDraw := False;
                BGInfo.DrawDC := 0;
                if (FOwnerControl = nil) or (FOwnerControl.Parent = nil) then
                  GetBGInfo(@BGInfo, GetParent(FSWHandle))
                else
                  GetBGInfo(@BGInfo, FOwnerControl.Parent);

                iWidth  := FCacheBmp.Width;
                iHeight := FCacheBmp.Height;
                p := BGInfo.Offset;
                if SkinData.FOwnerControl <> nil then begin
                  inc(p.X, SkinData.FOwnerControl.Left);
                  inc(p.Y, SkinData.FOwnerControl.Top);
                end;

                wRad := min(iWidth, rad);
                hRad := min(iHeight, rad);
                if scLeftTop in Corners then
                  FillTransPixels32(SkinData.FCacheBmp, BGInfo.Bmp,
                    Rect(0, 0, wRad, hRad), p, -1, SkinData.CommonSkinData, HTTOPLEFT, BGInfo.Color);

                if scRightTop in Corners then begin
                  pOffset := Point(iWidth - wRad, 0);
                  FillTransPixels32(SkinData.FCacheBmp, BGInfo.Bmp,
                    Rect(pOffset.X, 0, iWidth, hRad), Point(p.x + pOffset.X, p.Y), -1, SkinData.CommonSkinData, HTTOPLEFT, BGInfo.Color);
                end;

                if scLeftBottom in Corners then begin
                  pOffset := Point(0, iHeight - hRad);
                  FillTransPixels32(SkinData.FCacheBmp, BGInfo.Bmp,
                    Rect(0, pOffset.Y, wRad, iHeight), Point(p.x + pOffset.X, p.Y + pOffset.Y), -1, SkinData.CommonSkinData, HTTOPLEFT, BGInfo.Color);
                end;

                if scRightBottom in Corners then begin
                  pOffset := Point(iWidth - wRad, iHeight - hRad);
                  FillTransPixels32(SkinData.FCacheBmp, BGInfo.Bmp,
                    Rect(pOffset.X, pOffset.Y, iWidth, iHeight), Point(p.x + pOffset.X, p.Y + pOffset.Y), -1, SkinData.CommonSkinData, HTTOPLEFT, BGInfo.Color);
                end;
              end;
            end;
      end
      else
        begin
          BGInfo.PleaseDraw := False;
          BGInfo.DrawDC := 0;
          if (FOwnerControl = nil) or (FOwnerControl.Parent = nil) then
            GetBGInfo(@BGInfo, GetParent(FSWHandle))
          else
            GetBGInfo(@BGInfo, FOwnerControl.Parent);

          iWidth  := FCacheBmp.Width;
          iHeight := FCacheBmp.Height;
          p := BGInfo.Offset;
          inc(p.X, SkinData.FOwnerControl.Left);
          inc(p.Y, SkinData.FOwnerControl.Top);

          if (scLeftTop in Corners) and (SkinData.CustomBorder.RadiusTopLeft > 0) then
            FillTransPixels32(SkinData.FCacheBmp, BGInfo.Bmp,
              Rect(0, 0, SkinData.CustomBorder.RadiusTopLeft, min(iHeight, SkinData.CustomBorder.RadiusTopLeft)),
              p, -1, SkinData.CommonSkinData, HTTOPLEFT, BGInfo.Color);

          if (scRightTop in Corners) and (SkinData.CustomBorder.RadiusTopRight > 0) then begin
            pOffset := Point(iWidth - min(iWidth, SkinData.CustomBorder.RadiusTopRight), 0);
            FillTransPixels32(SkinData.FCacheBmp, BGInfo.Bmp,
              Rect(pOffset.X, 0, iWidth, min(iHeight, SkinData.CustomBorder.RadiusTopRight)),
              Point(p.x + pOffset.X, p.Y), -1, SkinData.CommonSkinData, HTTOPLEFT, BGInfo.Color);
          end;

          if (scLeftBottom in Corners) and (SkinData.CustomBorder.RadiusBottomLeft > 0) then begin
            pOffset := Point(0, iHeight - min(iHeight, SkinData.CustomBorder.RadiusBottomLeft));
            FillTransPixels32(SkinData.FCacheBmp, BGInfo.Bmp,
              Rect(0, pOffset.Y, SkinData.CustomBorder.RadiusBottomLeft, iHeight),
              Point(p.x + pOffset.X, p.Y + pOffset.Y), -1, SkinData.CommonSkinData, HTTOPLEFT, BGInfo.Color);
          end;

          if (scRightBottom in Corners) and (SkinData.CustomBorder.RadiusBottomRight > 0) then begin
            pOffset := Point(iWidth - min(iWidth, SkinData.CustomBorder.RadiusTopRight), iHeight - min(iHeight, SkinData.CustomBorder.RadiusBottomLeft));
            FillTransPixels32(SkinData.FCacheBmp, BGInfo.Bmp,
              Rect(pOffset.X, pOffset.Y, iWidth, iHeight),
              Point(p.x + pOffset.X, p.Y + pOffset.Y), -1, SkinData.CommonSkinData, HTTOPLEFT, BGInfo.Color);
          end;
        end;
end;


var
  aRoundMasks: array of TBitmap;

function MakeTranspCorners(Bmp: TBitmap; R: TRect; TL, TR, BL, BR: integer): boolean;
var
  MaskBmp: TBitmap;
  bWidth, bHeight: integer;

  procedure MakeTranspAlpha(Radius: integer; DstRect, MaskRect: TRect);
  var
    X, Y, DeltaD, DeltaM: integer;
    M0, M: PRGBAArray_M;
    D0, D: PRGBAArray_D;
  begin
    MaskBmp := GetRoundMask(Radius);
    if InitLine(Bmp, Pointer(D0), Deltad) and InitLine(MaskBmp, Pointer(M0), DeltaM) then
      // Warning! Bounds are not verified, values must be correct
      for Y := 0 to Radius - 1 do begin
        M := Pointer(PAnsiChar(M0) + DeltaM * (MaskRect.Top + Y));
        D := Pointer(PAnsiChar(D0) + DeltaD * (DstRect.Top + Y));
        for X := 0 to Radius - 1 do
          with D[DstRect.Left + X], M[MaskRect.Left + X] do
            case MC of
              -1 {White}: DA := 0; // Fuly transparent
              TColor($FF000000) : // Skip
              else DA := (DA + (MaxByte - MA)) div 2; // Combine alpha value
            end;
      end;
      Result := True;
  end;

  procedure CheckSizes(var Value1, Value2, FullValue: integer);
  var
    bDiv2: integer;
  begin
    bDiv2 := FullValue div 2;
    if Value1 > FullValue - Value2 then
      if Value1 < bDiv2 then
        Value2 := FullValue - Value1 - 1
      else
        if Value2 < bDiv2 then
          Value1 := FullValue - Value2 - 1
        else begin
          Value1 := bDiv2;
          Value2 := bDiv2;
        end;
  end;

begin
  bWidth := Bmp.Width;
  bHeight := Bmp.Height;
  CheckSizes(TL, BL, bHeight);
  CheckSizes(TR, BR, bHeight);
  CheckSizes(TL, TR, bWidth);
  CheckSizes(BL, BR, bWidth);
  Result := False;
  if TL > 0 then
    MakeTranspAlpha(TL, Rect(0, 0, TL, TL), Rect(0, 0, TL, TL));

  if TR > 0 then
    MakeTranspAlpha(TR, Rect(bWidth - TR, 0, bWidth, TR), Rect(TR, 0, TR + TR, TR));

  if BL > 0 then
    MakeTranspAlpha(BL, Rect(0, bHeight - BL, BL, bHeight), Rect(0, BL, BL, BL + BR));

  if BR > 0 then
    MakeTranspAlpha(BR, Rect(bWidth - BR, bHeight - BR, bWidth, bHeight), Rect(BR, BR, BR + BR, BR + BR));
end;


function GetRoundMask(Radius: integer): TBitmap;
var
  i, l, Size: integer;

  function GetBmp(Ndx: integer): TBitmap;
  begin
    if Length(aRoundMasks) <= Ndx then
      SetLength(aRoundMasks, Ndx + 1);

    if aRoundMasks[Ndx] = nil then begin
      aRoundMasks[Ndx] := CreateBmp32(Size, Size);
      FillRect32(aRoundMasks[Ndx], MkRect(aRoundMasks[Ndx]), $FFFFFF, MaxByte);
      acgpFillEllipse(aRoundMasks[Ndx].Canvas.Handle, 0, 0, Size - 1, Size - 1, clBlack);
    end;
    Result := aRoundMasks[Ndx];
  end;

begin
  l := Length(aRoundMasks);
  if l < 5 then
    SetLength(aRoundMasks, 5);

  Size := Radius * 2 + 1;
  if Radius < 5 then
    Result := GetBmp(Radius - 1)
  else begin
    Result := nil;
    for i := 5 to l - 1 do
      if (aRoundMasks[i] <> nil) and (aRoundMasks[i].Width = Size) then begin
        Result := aRoundMasks[i];
        Break;
      end;

    if Result = nil then
      Result := GetBmp(l);
  end;
end;


procedure UpdateCorners(gd: TsGeneralData; Bmp: TBitmap; R: TRect; CI: TCacheInfo; PRadiuses: PacIntValues = nil; ExtSource: boolean = False);
var
  C: TsColor;
  rd: integer;
  sRect: TRect;
  MaskBmp: TBitmap;

  procedure UpdateCorner(DstPoint, MskPoint, SrcPoint: TPoint);
  var
    DeltaD, DeltaM, DeltaS: integer;
    M0, M: PRGBAArray_M;
    D0, D: PRGBAArray_D;
    S0, S: PRGBAArray_S;
    w, h, X, Y: integer;
  begin
    Assert(MaskBmp <> nil);
    Assert(Bmp <> nil);
    h := rd;
    w := rd;

    if DstPoint.Y + h >= Bmp.Height then
      h := max(-1, Bmp.Height - DstPoint.Y - 1);

    if DstPoint.X < 0 then begin
      inc(w, DstPoint.X);
      dec(MskPoint.X, DstPoint.X);
      DstPoint.X := 0;
    end;

    // Warning! SrcPoint is not verified, values must be correct
    if InitLine(MaskBmp, Pointer(M0), DeltaM) and InitLine(Bmp, Pointer(D0), DeltaD) then
      if CI.Ready and (CI.Bmp <> nil) and InitLine(CI.Bmp, Pointer(S0), DeltaS) then begin

        if DstPoint.X + w >= Bmp.Width then
          w := max(-1, Bmp.Width - DstPoint.X - 1);

        if CI.Y + SrcPoint.Y + h >= CI.Bmp.Height then
          h := max(-1, CI.Bmp.Height - CI.Y - SrcPoint.Y - 1);

        if CI.Y < 0 then begin
          inc(h, CI.Y);
          dec(DstPoint.Y, CI.Y);
          CI.Y := 0;
        end;

        if (DstPoint.X < 0) or (MskPoint.X < 0) or (CI.X + SrcPoint.X < 0) then
          exit;

        for Y := 0 to h do begin
          M := Pointer(PAnsiChar(M0) + DeltaM * (MskPoint.Y + Y));
          D := Pointer(PAnsiChar(D0) + DeltaD * (DstPoint.Y + Y));
          S := Pointer(PAnsiChar(S0) + DeltaS * (SrcPoint.Y + CI.Y + Y));
          for X := 0 to w do
            with D[DstPoint.X + X], M[MskPoint.X + X], S[CI.X + SrcPoint.X + X] do
              if MC = -1 then begin // $FFFFFFFF
                DC := SC;
                if not ExtSource then
                  DA := 0
                else
                  // DA copied from shadow template for transparent corners in rounded forms
              end
              else begin
                DR := (DR shl 8 + (SR - DR) * MR) shr 8;
                DG := (DG shl 8 + (SG - DG) * MG) shr 8;
                DB := (DB shl 8 + (SB - DB) * MB) shr 8;
                DA := (DA shl 8 + (SA - DA) * MB) shr 8;
              end;
        end;
      end
      else begin
        C.C := SwapRedBlue(CI.FillColor);
        with C do
          for Y := 0 to h do begin
            M := Pointer(PAnsiChar(M0) + DeltaM * (MskPoint.Y + Y));
            D := Pointer(PAnsiChar(D0) + DeltaD * (DstPoint.Y + Y));
            for X := 0 to rd do
              with D[DstPoint.X + X], M[MskPoint.X + X] do
                if MC = -1 then // $FFFFFFFF
                  DC := C
                else begin
                  DR := (DR shl 8 + (R - DR) * MR) shr 8;
                  DG := (DG shl 8 + (G - DG) * MG) shr 8;
                  DB := (DB shl 8 + (B - DB) * MB) shr 8;
                  DA := (DA shl 8 + (A - DA) * MB) shr 8;
                end;
          end;
      end;
  end;

begin
  if PRadiuses <> nil then begin
    if CI.Bmp <> nil then begin
      if ExtSource then begin
        sRect := MkRect(CI.Bmp);
        inc(sRect.Left, R.Left);
        inc(sRect.Top, R.Top);
        dec(sRect.Right, Bmp.Width - R.Right);
        dec(sRect.Bottom, Bmp.Height - R.Bottom);
      end
      else
        sRect := R;
    end
    else
      sRect := MkRect;

    // LT
    if PRadiuses.Value0 > 0 then begin
      rd := PRadiuses.Value0;
      MaskBmp := GetRoundMask(rd);
      UpdateCorner(Point(R.Left, R.Top), Point(0, 0), Point(sRect.Left, sRect.Top));
    end;
    // RT
    if PRadiuses.Value1 > 0 then begin
      rd := PRadiuses.Value1;
      MaskBmp := GetRoundMask(rd);
      UpdateCorner(Point(R.Right - rd - 1, R.Top), Point(rd, 0), Point(sRect.Right - rd - 1, sRect.Top));
    end;
    // LB
    if PRadiuses.Value2 > 0 then begin
      rd := PRadiuses.Value2;
      if R.Bottom - rd - 1 < 0 then
        rd := R.Bottom - 1;

      MaskBmp := GetRoundMask(rd);
      UpdateCorner(Point(R.Left, R.Bottom - rd - 1), Point(0, rd), Point(sRect.Left, sRect.Bottom - rd - 1));
    end;
    // RB
    if PRadiuses.Value3 > 0 then begin
      rd := PRadiuses.Value3;
      if R.Bottom - rd - 1 < 0 then
        rd := R.Bottom - 1;

      MaskBmp := GetRoundMask(rd);
//      UpdateCorner(Point(R.Right - rd - 1, R.Bottom - rd - 1), Point(rd, rd), Point(sRect.Right - rd - 1, sRect.Bottom - rd - 1));
      UpdateCorner(Point(R.Right - rd - 1, R.Bottom - rd - 1), Point(rd, rd), Point(max(0, sRect.Right - rd - 1), max(0, sRect.Bottom - rd - 1)));
    end;
  end
  else
    if gd.BorderRadius > 0 then begin
      rd := gd.BorderRadius;
      MaskBmp := GetRoundMask(rd);
      // LT
      UpdateCorner(Point(R.Left,                       R.Top), Point(0, 0), MkPoint);
      // RT
      UpdateCorner(Point(R.Right - rd - 1,             R.Top), Point(rd, 0), MkPoint);
      // LB
      UpdateCorner(Point(R.Left,           R.Bottom - rd - 1), Point(0, rd), MkPoint);
      // RB
      UpdateCorner(Point(R.Right - rd - 1, R.Bottom - rd - 1), Point(rd, rd), MkPoint);
    end;
end;
{$ENDIF}


{$IFNDEF WIN64}
procedure FillLongword(var X; Count: Integer; Value: Longword);
asm
// EAX = X
// EDX = Count
// ECX = Value
    PUSH  EDI
    MOV   EDI, EAX // Point EDI to destination
    MOV   EAX, ECX
    MOV   ECX, EDX
    TEST  ECX, ECX
    JS    @exit
    REP   STOSD    // Fill count dwords
@exit:
    POP   EDI
end;
{$ENDIF}


procedure PaintItem32(SkinIndex: integer; Filling: boolean; State: integer; R: TRect; pP: TPoint; ItemBmp: TBitmap; CommonSkinData: TObject);
var
  CI: TCacheInfo;
  mState: integer;
  CacheBmp: TBitmap;
begin
  if (ItemBmp <> nil) and not IsRectEmpty(R) then begin
    if not (CommonSkinData is TacSkinData) then begin
{$IFDEF DEBUG}
      if IsIDERunning then
        Alert('Check parameters of the PaintItem32 procedure.');
{$ENDIF}
      Exit;
    end
    else
      with TacSkinData(CommonSkinData) do
        if FOwner.IsValidSkinIndex(SkinIndex) and (R.Bottom <= ItemBmp.Height) and (R.Right <= ItemBmp.Width) and (R.Left >= 0) and (R.Top >= 0) then begin
          // Count of allowed states is limited in skin
          if gd[SkinIndex].States <= State then
            State := gd[SkinIndex].States - 1;

          mState := min(State, ac_MaxPropsIndex);
          CacheBmp := CreateBmp32(WidthOf(R) + 1, HeightOf(R) + 1);
          BitBlt(CacheBmp.Canvas.Handle, 0, 0, CacheBmp.Width, CacheBmp.Height, ItemBmp.Canvas.Handle, R.Left, R.Top, SRCCOPY);
          PaintItemBG32(SkinIndex, mState {normal or hot}, R, ItemBmp, CommonSkinData);
          CI := MakeCacheInfo(CacheBmp, -R.Left, -R.Top);
          if FOwner.IsValidImgIndex(gd[SkinIndex].BorderIndex) then
            DrawSkinRect32(ItemBmp, R, CI, ma[gd[SkinIndex].BorderIndex], State)
          else
            TryPaintBorder(ItemBmp, R, gd[SkinIndex], State, CI, nil, PPI, nil);

          CacheBmp.Free;
          if (ItemBmp.Width = WidthOf(R)) and (ItemBmp.Height = HeightOf(R)) then
            UpdateAlpha(ItemBmp, R) // Updating for using of bitmap in layered windows
        end;
  end;
end;


procedure PaintItemBG32(SkinIndex: integer; State: integer; R: TRect; ItemBmp: TBitmap; CommonSkinData: TObject; CustomColor: TColor = clFuchsia);
var
  C: TsColor;
  aRect: TRect;
  Color: TColor;
  iDrawed: boolean;
  sMan: TsSkinManager;
  GradientArray: TsGradArray;
  ImagePercent, GradientPercent, PatternIndex, Transparency: integer;

  procedure PaintAddons(var aBmp: TBitmap);
  var
    bmp: TBitmap;
    i: integer;
    R: TRect;
  begin
    iDrawed := False;
    with TacSkinData(CommonSkinData) do
      if (ImagePercent + GradientPercent = 100) and
           (GradientPercent in [1..99]) and
             (ma[PatternIndex].DrawMode = 0) and
               (GradientArray[0].Mode < 2) and
                 sMan.IsValidImgIndex(PatternIndex) then // Optimized drawing
        if ma[PatternIndex].Bmp <> nil then
          PaintGradTxt(aBmp, aRect, GradientArray, ma[PatternIndex].Bmp, ma[PatternIndex].R, MaxByte * ImagePercent div 100, Round(1 - Transparency / 100) * MaxByte)
        else
          PaintGradTxt(aBmp, aRect, GradientArray, sMan.MasterBitmap, ma[PatternIndex].R, MaxByte * ImagePercent div 100, Round(1 - Transparency / 100) * MaxByte)
      else begin
        R := aRect;
        // BGImage painting
        if ImagePercent > 0 then
          {with sMan do }begin
            if IsValidIndex(PatternIndex, Length(ma)) then begin
              if boolean(ma[PatternIndex].MaskType) then
                TileMasked(aBmp, R, EmptyCI {!}, ma[PatternIndex], acFillModes[ma[PatternIndex].DrawMode])
              else
                TileBitmap(aBmp.Canvas, R, ma[PatternIndex].Bmp, ma[PatternIndex], acFillModes[ma[PatternIndex].DrawMode]);

              iDrawed := True;
            end;
            if R.Right >= 0 then
              FillRect32(aBmp, R, Color); // If not all rect was filled by texture
          end;
        // BGGradient painting
        if GradientPercent > 0 then
          if iDrawed then begin // If texture exists
            Bmp := CreateBmp32(WidthOf(aRect, True), HeightOf(aRect, True));
            try
              if Length(GradientArray) > 0 then
                PaintGrad(Bmp, MkRect(Bmp), GradientArray)
              else
                FillRect32(Bmp, aRect, Color);

              SumBmpRect(aBmp, Bmp, max(min((ImagePercent * integer(MaxByte)) div 100, MaxByte), 0), MkRect(Bmp), aRect.TopLeft);
            finally
              FreeAndNil(Bmp);
            end;
          end
          else
            if Length(GradientArray) > 0 then begin
              if GradientArray[0].Mode and PM_TRANSPARENT = 0 then begin
                GradientArray[0].Mode := GradientArray[0].Mode or PM_TRANSPARENT; // Add alphachannel
                for i := 0 to Length(GradientArray) - 1 do
                  GradientArray[i].Color.A := (100 - Transparency) * MaxByte div 100;
              end;
              PaintGrad(aBmp, aRect, GradientArray)
            end
            else begin
              FillRect32(aBmp, aRect, Color);
              Exit;
            end;

        case GradientPercent + ImagePercent of
          1..99:
            BlendColorRect(aBmp, aRect, byte((GradientPercent + ImagePercent) * MaxByte div 100), Color); // Mix with color

          0: begin
            // Just fill by color
            C := GetAPixel(aBmp, R.Left, R.Top);
            if C.I <> 0 then
              case Transparency of
                0:   FillRect32 (aBmp, aRect, Color, MaxByte);
                100: FillRect32 (aBmp, aRect, Color, C.A);
                else BlendRect32(aBmp, aRect, Color, 100 - Transparency, max(C.A, (100 - Transparency) * MaxByte div 100));
              end
            else
              FillRect32(aBmp, aRect, Color, (100 - Transparency) * MaxByte div 100);

            Exit; // AlphaChannel is ready
          end;
        end;
      end;
  end;

begin
  if not (CommonSkinData is TacSkinData) then begin
{$IFDEF DEBUG}
    if IsIDERunning then
      Alert('Check parameters of the PaintItemBG32 procedure.');
{$ENDIF}
    Exit;
  end
  else
    with TacSkinData(CommonSkinData) do begin
      sMan := FOwner;
      if sMan.IsValidSkinIndex(SkinIndex) then
        with gd[SkinIndex] do begin
          State := min(State, ac_MaxPropsIndex);
          Transparency := Props[State].Transparency;
          if Transparency = 100 then begin
            if (ItemBmp.Width = WidthOf(R)) and (ItemBmp.Height = HeightOf(R)) then // Make
              FillRect32(ItemBmp, R, clBlack, 0);
          end // Exit without changes
          else
            {with sMan do }begin
              aRect := R;
              PatternIndex := -1;
              if CustomColor = clFuchsia then begin // Get skin props
                Color := Props[State].Color;
                ImagePercent := Props[State].ImagePercent;
                GradientPercent := Props[State].GradientPercent;
                if GradientPercent > 0 then
                  GradientArray := Props[State].GradientArray;

                if ImagePercent > 0 then
                  PatternIndex := Props[State].TextureIndex
              end
              else begin // Filling by custom color only
                ImagePercent := 0;
                GradientPercent := 0;
                Color := CustomColor;
                Transparency := 0;
              end;
              PaintAddons(ItemBmp);
            end;
        end;
    end;
end;


procedure DrawSkinRect32(aBmp: TBitmap; const aRect: TRect; const ci: TCacheInfo; const MaskData: TsMaskData; const State: integer);
var
  BmpSrc: TBitmap;
  Stretch: boolean;
  x, y, w, h, dw, dh, dhm, lWidth, tWidth, rWidth, bWidth, NewState: integer;
begin
  if (State <> 0) or (MaskData.DrawMode and BDM_ACTIVEONLY = 0) then
    if (WidthOf(aRect) > 1) and (HeightOf(aRect) > 1) and (MaskData.Manager <> nil) then begin
      lWidth := MaskData.WL;
      tWidth := MaskData.WT;
      rWidth := MaskData.WR;
      bWidth := MaskData.WB;
      if lWidth + rWidth > WidthOf(aRect) then begin
        y := lWidth + rWidth - WidthOf(aRect);
        x := y div 2;
        dec(lWidth, x);
        dec(rWidth, x);
        if y mod 2 > 0 then
          dec(rWidth);

        if lWidth < 0 then
          lWidth := 0;

        if rWidth < 0 then
          rWidth := 0;
      end;
      if tWidth + bWidth > HeightOf(aRect) then begin
        y := tWidth + bWidth - HeightOf(aRect);
        x := y div 2;
        dec(tWidth, x);
        dec(bWidth, x);
        if y mod 2 > 0 then
          dec(bWidth);

        if tWidth < 0 then
          tWidth := 0;

        if bWidth < 0 then
          bWidth := 0;
      end;
      if State >= MaskData.ImageCount then
        NewState := MaskData.ImageCount - 1
      else
        NewState := State;

      dw := MaskData.Width;                  // Width of mask
      dh := MaskData.Height;                 // Height of mask
      if MaskData.MaskType = 0 then
        dhm := 0
      else
        dhm := dh;

      if MaskData.DrawMode and BDM_STRETCH <> 0 then begin
        Stretch := True;
        SetStretchBltMode(aBmp.Canvas.Handle, COLORONCOLOR);
      end
      else
        Stretch := False;

      w := dw - lWidth - rWidth;
      if w < 0 then
        Exit; // Width of middle piece must be > 0

      h := dh - tWidth - bWidth;
      if h < 0 then
        Exit; // Height of middle piece must be > 0

      dw := dw * NewState; // Offset of mask

      if MaskData.Bmp <> nil then
        BmpSrc := MaskData.Bmp
      else
        BmpSrc := TsSkinManager(MaskData.Manager).MasterBitmap;

      with aRect, MaskData do
        if MaskType = 0 then begin // Copy without mask
          if MaskData.DrawMode and BDM_ALPHA = 0 then begin
            // left - top
            CopyTransRect(aBmp, BmpSrc, Left, Top, Rect(R.Left + dw, R.Top, R.Left + dw + lWidth - 1, R.Top + tWidth - 1), clFuchsia, CI, True);
            y := Top + tWidth;
            // left - middle
            if not Stretch then begin
              if h > 0 then
                while y < Bottom - h - bWidth do begin
                  BitBlt(aBmp.Canvas.Handle, Left, y, lWidth, h, BmpSrc.Canvas.Handle, R.Left + dw, R.Top + tWidth, SRCCOPY);
                  inc(y, h);
                end;

              if y < Bottom - bWidth then
                BitBlt(aBmp.Canvas.Handle, Left, y, lWidth, Bottom - bWidth - y, BmpSrc.Canvas.Handle, R.Left + dw, R.Top + tWidth, SRCCOPY);
            end
            else
              StretchBlt(aBmp.Canvas.Handle, Left, y, lWidth, Bottom - bWidth - y, BmpSrc.Canvas.Handle, R.Left + dw, R.Top + tWidth, lWidth, h, SRCCOPY);

            // top - middle
            x := Left + lWidth;
            if not Stretch then begin
              if w > 0 then
                while x < Right - w - rWidth do begin
                  BitBlt(aBmp.Canvas.Handle, x, Top, w, tWidth, BmpSrc.Canvas.Handle, R.Left + dw + lWidth, R.Top, SRCCOPY);
                  inc(x, w);
                end;

              if x < Right - rWidth then
                BitBlt(aBmp.Canvas.Handle, x, Top, Right - rWidth - x, tWidth, BmpSrc.Canvas.Handle, R.Left + dw + lWidth, R.Top, SRCCOPY);
            end
            else
              StretchBlt(aBmp.Canvas.Handle, x, Top, Right - rWidth - x, tWidth, BmpSrc.Canvas.Handle, R.Left + dw + lWidth, R.Top, w, tWidth, SRCCOPY);

            // left - bottom
            CopyTransRect(aBmp, BmpSrc, Left, Bottom - bWidth, Rect(R.Left + dw, R.Bottom - bWidth, R.Left + dw + lWidth - 1, R.Bottom - 1), clFuchsia, CI, True);

            // bottom - middle
            x := Left + lWidth;
            if not Stretch then begin
              if w > 0 then
                while x < Right - w - rWidth do begin
                  BitBlt(aBmp.Canvas.Handle, x, Bottom - bWidth, w, bWidth, BmpSrc.Canvas.Handle, R.Left + dw + lWidth, R.Bottom - bWidth, SRCCOPY);
                  inc(x, w);
                end;

              if x < Right - rWidth then
                BitBlt(aBmp.Canvas.Handle, x, Bottom - bWidth, Right - rWidth - x, bWidth, BmpSrc.Canvas.Handle, R.Left + dw + lWidth, R.Bottom - bWidth, SRCCOPY);
            end
            else
              StretchBlt(aBmp.Canvas.Handle, x, Bottom - bWidth, Right - rWidth - x, bWidth, BmpSrc.Canvas.Handle, R.Left + dw + lWidth, R.Bottom - bWidth, w, bWidth, SRCCOPY);

            // right - bottom
            CopyTransRect(aBmp, BmpSrc, Right - rWidth, Bottom - bWidth, Rect(R.Left + dw + w + lWidth, R.Bottom - bWidth, R.Left + dw + w + lWidth + rWidth - 1, R.Bottom - 1), clFuchsia, CI, True);
            // right - top
            CopyTransRect(aBmp, BmpSrc, Right - rWidth, Top, Rect(R.Left + dw + w + lWidth, R.Top, R.Left + dw + w + lWidth + rWidth - 1, R.Top + tWidth - 1), clFuchsia, CI, True);
            y := Top + tWidth;
            // right - middle
            if not Stretch then begin
              if h > 0 then
                while y < Bottom - h - bWidth do begin
                  BitBlt(aBmp.Canvas.Handle, Right - rWidth, y, rWidth, h, BmpSrc.Canvas.Handle, R.Left + dw + lWidth + w, R.Top + tWidth, SRCCOPY);
                  inc(y, h);
                end;

              if y < Bottom - bWidth then
                BitBlt(aBmp.Canvas.Handle, Right - rWidth, y, rWidth, Bottom - bWidth - y, BmpSrc.Canvas.Handle, R.Left + dw + w + lWidth, R.Top + tWidth, SRCCOPY);
            end
            else
              StretchBlt(aBmp.Canvas.Handle, Right - rWidth, y, rWidth, Bottom - bWidth - y, BmpSrc.Canvas.Handle, R.Left + dw + lWidth + w, R.Top + tWidth, rWidth, h, SRCCOPY);

            // Fill
            if DrawMode and BDM_FILL <> 0 then
              if not Stretch then begin
                y := Top + tWidth;
                if h > 0 then
                  while y < Bottom - h - bWidth do begin
                    x := Left + lWidth;
                    if w > 0 then
                      while x < Right - w - rWidth do begin
                        BitBlt(aBmp.Canvas.Handle, x, y, w, h, BmpSrc.Canvas.Handle, R.Left + dw + lWidth, R.Top + tWidth, SRCCOPY);
                        inc(x, w);
                      end;

                    if x < Right - rWidth then
                      BitBlt(aBmp.Canvas.Handle, x, y, Right - rWidth - x,  Bottom - bWidth - y, BmpSrc.Canvas.Handle, R.Left + dw + lWidth, R.Top + tWidth, SRCCOPY);

                    inc(y, h);
                  end;

                x := Left + lWidth;
                if y < Bottom - bWidth then begin
                  if w > 0 then
                    while x < Right - w - rWidth do begin
                      BitBlt(aBmp.Canvas.Handle, x, y, w, Bottom - bWidth - y, BmpSrc.Canvas.Handle, R.Left + dw + lWidth, R.Top + tWidth, SRCCOPY);
                      inc(x, w);
                    end;

                  if x < Right - rWidth then
                    BitBlt(aBmp.Canvas.Handle, x, y, Right - rWidth - x, Bottom - bWidth - y, BmpSrc.Canvas.Handle, R.Left + dw + lWidth, R.Top + tWidth, SRCCOPY);
                end;
              end
              else begin
                y := Top + tWidth;
                x := Left + lWidth;
                StretchBlt(aBmp.Canvas.Handle, x, y, Right - rWidth - x, Bottom - bWidth - y, BmpSrc.Canvas.Handle, R.Left + dw + lWidth, R.Top + tWidth, w, h, SRCCOPY);
              end;
          end
          else begin // Paint with alpha-channel
            // left - top
            PaintBmpRect32(aBmp, BmpSrc, Rect(R.Left + dw, R.Top, R.Left + dw + lWidth - 1, R.Top + tWidth - 1), Point(Left, Top));
            y := Top + tWidth;
            // left - middle
            if not Stretch then begin
              if h > 0 then
                while y < Bottom - h - bWidth do begin
                  BitBlt(aBmp.Canvas.Handle, Left, y, lWidth, h, BmpSrc.Canvas.Handle, R.Left + dw, R.Top + tWidth, SRCCOPY);
                  inc(y, h);
                end;

              if y < Bottom - bWidth then
                BitBlt(aBmp.Canvas.Handle, Left, y, lWidth, Bottom - bWidth - y, BmpSrc.Canvas.Handle, R.Left + dw, R.Top + tWidth, SRCCOPY);
            end
            else
              StretchBmpRect32(aBmp, Left, y, lWidth, Bottom - bWidth - y, BmpSrc, R.Left + dw, R.Top + tWidth, lWidth, h);

            // top - middle
            x := Left + lWidth;
            if not Stretch then begin
              if w > 0 then
                while x < Right - w - rWidth do begin
                  BitBlt(aBmp.Canvas.Handle, x, Top, w, tWidth, BmpSrc.Canvas.Handle, R.Left + dw + lWidth, R.Top, SRCCOPY);
                  inc(x, w);
                end;

              if x < Right - rWidth then
                BitBlt(aBmp.Canvas.Handle, x, Top, Right - rWidth - x, tWidth, BmpSrc.Canvas.Handle, R.Left + dw + lWidth, R.Top, SRCCOPY);
            end
            else
              StretchBmpRect32(aBmp, x, Top, Right - rWidth - x, tWidth, BmpSrc, R.Left + dw + lWidth, R.Top, w, tWidth);

            // left - bottom
            PaintBmpRect32(aBmp, BmpSrc, Rect(R.Left + dw, R.Bottom - bWidth, R.Left + dw + lWidth - 1, R.Bottom - 1), Point(Left, Bottom - bWidth));

            // bottom - middle
            x := Left + lWidth;
            if not Stretch then begin
              if w > 0 then
                while x < Right - w - rWidth do begin
                  BitBlt(aBmp.Canvas.Handle, x, Bottom - bWidth, w, bWidth, BmpSrc.Canvas.Handle, R.Left + dw + lWidth, R.Bottom - bWidth, SRCCOPY);
                  inc(x, w);
                end;

              if x < Right - rWidth then
                BitBlt(aBmp.Canvas.Handle, x, Bottom - bWidth, Right - rWidth - x, bWidth, BmpSrc.Canvas.Handle, R.Left + dw + lWidth, R.Bottom - bWidth, SRCCOPY);
            end
            else
              StretchBmpRect32(aBmp, x, Bottom - bWidth, Right - rWidth - X, bWidth, BmpSrc, R.Left + dw + lWidth, R.Bottom - bWidth, w, bWidth);

            // right - bottom
            PaintBmpRect32(aBmp, BmpSrc, Rect(R.Left + dw + w + lWidth, R.Bottom - bWidth, R.Left + dw + w + lWidth + rWidth - 1, R.Bottom - 1), Point(Right - rWidth, Bottom - bWidth));
            // right - top
            PaintBmpRect32(aBmp, BmpSrc, Rect(R.Left + dw + w + lWidth, R.Top, R.Left + dw + w + lWidth + rWidth - 1, R.Top + tWidth - 1), Point(Right - rWidth, Top));
            y := Top + tWidth;
            // right - middle
            if not Stretch then begin
              if h > 0 then
                while y < Bottom - h - bWidth do begin
                  BitBlt(aBmp.Canvas.Handle, Right - rWidth, y, rWidth, h, BmpSrc.Canvas.Handle, R.Left + dw + lWidth + w, R.Top + tWidth, SRCCOPY);
                  inc(y, h);
                end;

              if y < Bottom - bWidth then
                BitBlt(aBmp.Canvas.Handle, Right - rWidth, y, rWidth, Bottom - bWidth - y, BmpSrc.Canvas.Handle, R.Left + dw + w + lWidth, R.Top + tWidth, SRCCOPY);
            end
            else
              StretchBmpRect32(aBmp, Right - rWidth, y, rWidth, Bottom - bWidth - y, BmpSrc, R.Left + dw + lWidth + w, R.Top + tWidth, rWidth, h);

            // Fill
            if DrawMode and BDM_FILL <> 0 then
              if not Stretch then begin
                y := Top + tWidth;
                if h > 0 then
                  while y < Bottom - h - bWidth do begin
                    x := Left + lWidth;
                    if w > 0 then
                      while x < Right - w - rWidth do begin
                        BitBlt(aBmp.Canvas.Handle, x, y, w, h, BmpSrc.Canvas.Handle, R.Left + dw + lWidth, R.Top + tWidth, SRCCOPY);
                        inc(x, w);
                      end;

                    if x < Right - rWidth then
                      BitBlt(aBmp.Canvas.Handle, x, y, Right - rWidth - x,  Bottom - bWidth - y, BmpSrc.Canvas.Handle, R.Left + dw + lWidth, R.Top + tWidth, SRCCOPY);

                    inc(y, h);
                  end;

                x := Left + lWidth;
                if y < Bottom - bWidth then begin
                  if w > 0 then
                    while x < Right - w - rWidth do begin
                      BitBlt(aBmp.Canvas.Handle, x, y, w, Bottom - bWidth - y, BmpSrc.Canvas.Handle, R.Left + dw + lWidth, R.Top + tWidth, SRCCOPY);
                      inc(x, w);
                    end;

                  if x < Right - rWidth then
                    BitBlt(aBmp.Canvas.Handle, x, y, Right - rWidth - x, Bottom - bWidth - y, BmpSrc.Canvas.Handle, R.Left + dw + lWidth, R.Top + tWidth, SRCCOPY);
                end;
              end
              else begin
                y := Top + tWidth;
                x := Left + lWidth;
                StretchBmpRect32(aBmp, x, y, Right - rWidth - x, Bottom - bWidth - y, BmpSrc, R.Left + dw + lWidth, R.Top + tWidth, w, h);
              end;
          end;
        end
        else begin
          // left - top
          CopyByMask32(Rect(Left, Top, Left + lWidth, Top + tWidth), Rect(R.Left + dw, R.Top, R.Left + dw + lWidth, R.Top + tWidth), aBmp, BmpSrc, CI, MaskData);
          y := Top + tWidth;
          // left - middle
          if not Stretch then begin
            while y < Bottom - h - bWidth do begin
              CopyByMask32(Rect(Left, y, Left + lWidth, y + h), Rect(R.Left + dw, R.Top + tWidth, R.Left + dw + lWidth, R.Top + h + tWidth), aBmp, BmpSrc, EmptyCI, MaskData);
              inc(y, h);
            end;
            if y < Bottom - bWidth then
              CopyByMask32(Rect(Left, y, Left + lWidth, Bottom - bWidth), Rect(R.Left + dw, R.Top + tWidth, R.Left + dw + lWidth, R.Top + tWidth + h), aBmp, BmpSrc, EmptyCI, MaskData);
          end
          else
            StretchBltMask32(Left, y, lWidth, Bottom - bWidth - y, R.Left + dw, R.Top + tWidth, lWidth, h, aBmp, BmpSrc, dhm);
          // top - middle
          x := Left + lWidth;
          if not Stretch then begin
            while x < Right - w - rWidth do begin
              CopyByMask32(Rect(x, Top, x + w, Top + tWidth), Rect(R.Left + dw + lWidth, R.Top, R.Left + dw + w + lWidth, R.Top + tWidth), aBmp, BmpSrc, EmptyCI, MaskData);
              inc(x, w);
            end;
            if x < Right - rWidth then
              CopyByMask32(Rect(x, Top, Right - rWidth, Top + tWidth), Rect(R.Left + dw + lWidth, R.Top, R.Left + dw + w + lWidth, R.Top + tWidth), aBmp, BmpSrc, EmptyCI, MaskData);
          end
          else
            StretchBltMask32(x, Top, Right - rWidth - x, tWidth, R.Left + dw + lWidth, R.Top, w, tWidth, aBmp, BmpSrc, dhm);

          // left - bottom
          CopyByMask32(Rect(Left, Bottom - bWidth, Left + lWidth, Bottom), Rect(R.Left + dw, R.Top + dh - bWidth, R.Left + dw + lWidth, R.Top + dh), aBmp, BmpSrc, CI, MaskData);
          // bottom - middle
          x := Left + lWidth;
          if not Stretch then begin
            while x < Right - w - rWidth do begin
              CopyByMask32(Rect(x, Bottom - bWidth, x + w, Bottom), Rect(R.Left + dw + lWidth, R.Top + dh - bWidth, R.Left + dw + w + lWidth, R.Top + dh), aBmp, BmpSrc, EmptyCI, MaskData);
              inc(x, w);
            end;
            if x < Right - rWidth then
              CopyByMask32(Rect(x, Bottom - bWidth, Right - rWidth, Bottom), Rect(R.Left + dw + lWidth, R.Top + dh - bWidth, R.Left + dw + w + lWidth, R.Top + dh), aBmp, BmpSrc, EmptyCI, MaskData);
          end
          else
            StretchBltMask32(x, Bottom - bWidth, Right - rWidth - x, bWidth, R.Left + dw + lWidth, R.Top + dh - bWidth, w, bWidth, aBmp, BmpSrc, dhm);

          // right - bottom
          CopyByMask32(Rect(Right - rWidth, Bottom - bWidth, Right, Bottom), Rect(R.Left + dw + w + lWidth, R.Top + dh - bWidth, R.Left + dw + w + lWidth + rWidth, R.Top + dh), aBmp, BmpSrc, CI, MaskData);
          // right - top
          CopyByMask32(Rect(Right - rWidth, Top, Right, Top + tWidth), Rect(R.Left + dw + w + lWidth, R.Top, R.Left + dw + w + lWidth + rWidth, R.Top + tWidth), aBmp, BmpSrc, CI, MaskData);
          // right - middle
          y := Top + tWidth;
          if not Stretch then begin
            while y < Bottom - h - bWidth do begin
              CopyByMask32(Rect(Right - rWidth, y, Right, y + h), Rect(R.Left + dw + w + lWidth, R.Top + tWidth, R.Left + dw + w + lWidth + rWidth, R.Top + h + tWidth), aBmp, BmpSrc, EmptyCI, MaskData);
              inc(y, h);
            end;
            if y < Bottom - bWidth then
              CopyByMask32(Rect(Right - rWidth, y, Right, Bottom - bWidth), Rect(R.Left + dw + w + lWidth, R.Top + tWidth, R.Left + dw + w + lWidth + rWidth, R.Top + h + tWidth), aBmp, BmpSrc, EmptyCI, MaskData);
          end
          else
            StretchBltMask32(Right - rWidth, y, rWidth, Bottom - bWidth - y, R.Left + dw + lWidth + w, R.Top + tWidth, rWidth, h, aBmp, BmpSrc, dhm);
          // Fill
          if DrawMode and BDM_FILL <> 0 then
            if Stretch then begin
              y := Top + tWidth;
              x := Left + lWidth;
              StretchBltMask32(x, y, Right - rWidth - x, Bottom - bWidth - y, R.Left + dw + lWidth, R.Top + tWidth, w, h, aBmp, BmpSrc, dhm, True)
            end
            else begin
              y := Top + tWidth;
              while y < Bottom - h - bWidth do begin
                x := Left + lWidth;
                while x < Right - w - rWidth do begin
                  CopyByMask32(Rect(x, y, x + w, y + h), Rect(R.Left + dw + lWidth, R.Top + tWidth, R.Left + dw + w + lWidth, R.Top + h + tWidth), aBmp, BmpSrc, EmptyCI, MaskData);
                  inc(x, w);
                end;
                if x < Right - rWidth then
                  CopyByMask32(Rect(x, y, Right - rWidth, y + h), Rect(R.Left + dw + lWidth, R.Top + tWidth, R.Left + dw + w + lWidth, R.Top + h + tWidth), aBmp, BmpSrc, EmptyCI, MaskData);

                inc(y, h);
              end;
              x := Left + lWidth;
              if y < Bottom - bWidth then begin
                while x < Right - w - rWidth do begin
                  CopyByMask32(Rect(x, y, x + w, Bottom - bWidth), Rect(R.Left + dw + lWidth, R.Top + tWidth, R.Left + dw + w + lWidth, R.Top + h + tWidth), aBmp, BmpSrc, EmptyCI, MaskData);
                  inc(x, w);
                end;
                if x < Right - rWidth then
                  CopyByMask32(Rect(x, y, Right - rWidth, Bottom - bWidth), Rect(R.Left + dw + lWidth, R.Top + tWidth, R.Left + dw + w + lWidth, R.Top + h + tWidth), aBmp, BmpSrc, EmptyCI, MaskData);
              end;
            end;
        end;
    end;
end;


procedure DrawSkinRect32Ex(aBmp: TBitmap; const aRect: TRect; const ci: TCacheInfo; const MaskData: TsMaskData; const State: integer; MaxWidths: TRect; Opacity: integer);
var
  x, y, w, h, dw, dh, dhm, NewState, lWidth, tWidth, rWidth, bWidth, maxl, maxt, maxr, maxb: integer;
  Stretch: boolean;
  BmpSrc: TBitmap;
begin
  if (State <> 0) or (MaskData.DrawMode and BDM_ACTIVEONLY = 0) then
    if (WidthOf(aRect) > 1) and (HeightOf(aRect) > 1) and (MaskData.Manager <> nil) then begin
      lWidth := MaskData.WL;
      tWidth := MaskData.WT;
      rWidth := MaskData.WR;
      bWidth := MaskData.WB;
      if lWidth + rWidth > WidthOf(aRect) then begin
        y := lWidth + rWidth - WidthOf(aRect);
        x := y div 2;
        dec(lWidth, x);
        dec(rWidth, x);
        if y mod 2 > 0 then
          dec(rWidth);

        if lWidth < 0 then
          lWidth := 0;

        if rWidth < 0 then
          rWidth := 0;
      end;
      if tWidth + bWidth > HeightOf(aRect) then begin
        y := tWidth + bWidth - HeightOf(aRect);
        x := y div 2;
        dec(tWidth, x);
        dec(bWidth, x);
        if y mod 2 > 0 then
          dec(bWidth);

        if tWidth < 0 then
          tWidth := 0;

        if bWidth < 0 then
          bWidth := 0;
      end;
      if State >= MaskData.ImageCount then
        NewState := MaskData.ImageCount - 1
      else
        NewState := State;

      dw := MaskData.Width;                  // Width of mask
      dh := MaskData.Height;                 // Height of mask
      if MaskData.MaskType = 0 then
        dhm := 0
      else
        dhm := dh;

      if MaskData.DrawMode and BDM_STRETCH <> 0 then begin
        Stretch := True;
        SetStretchBltMode(aBmp.Canvas.Handle, COLORONCOLOR);
      end
      else
        Stretch := False;

      w := dw - lWidth - rWidth;
      if w < 0 then
        Exit; // Width of middle piece must be > 0

      h := dh - tWidth - bWidth;
      if h < 0 then
        Exit; // Height of middle piece must be > 0

      dw := dw * NewState; // Offset of mask

      if MaskData.Bmp <> nil then
        BmpSrc := MaskData.Bmp
      else
        BmpSrc := TsSkinManager(MaskData.Manager).MasterBitmap;

      MaxT := min(tWidth, MaxWidths.Top);
      MaxL := min(lWidth, MaxWidths.Left);
      MaxR := min(rWidth, MaxWidths.Right);
      MaxB := min(bWidth, MaxWidths.Bottom);

      with aRect, MaskData do
        if MaskType = 0 then begin // Copy without mask
          if MaskData.DrawMode and BDM_ALPHA = 0 then begin
            // left - top
            CopyTransRect(aBmp, BmpSrc, Left, Top, Rect(R.Left + dw, R.Top, R.Left + dw + lWidth - 1, R.Top + tWidth - 1), clFuchsia, CI, True);
            y := Top + tWidth;
            // left - middle
            if not Stretch then begin
              if h > 0 then
                while y < Bottom - h - bWidth do begin
                  BitBlt(aBmp.Canvas.Handle, Left, y, lWidth, h, BmpSrc.Canvas.Handle, R.Left + dw, R.Top + tWidth, SRCCOPY);
                  inc(y, h);
                end;

              if y < Bottom - bWidth then
                BitBlt(aBmp.Canvas.Handle, Left, y, lWidth, Bottom - bWidth - y, BmpSrc.Canvas.Handle, R.Left + dw, R.Top + tWidth, SRCCOPY);
            end
            else
              StretchBlt(aBmp.Canvas.Handle, Left, y, lWidth, Bottom - bWidth - y, BmpSrc.Canvas.Handle, R.Left + dw, R.Top + tWidth, lWidth, h, SRCCOPY);

            // top - middle
            x := Left + lWidth;
            if not Stretch then begin
              if w > 0 then
                while x < Right - w - rWidth do begin
                  BitBlt(aBmp.Canvas.Handle, x, Top, w, tWidth, BmpSrc.Canvas.Handle, R.Left + dw + lWidth, R.Top, SRCCOPY);
                  inc(x, w);
                end;

              if x < Right - rWidth then
                BitBlt(aBmp.Canvas.Handle, x, Top, Right - rWidth - x, tWidth, BmpSrc.Canvas.Handle, R.Left + dw + lWidth, R.Top, SRCCOPY);
            end
            else
              StretchBlt(aBmp.Canvas.Handle, x, Top, Right - rWidth - x, tWidth, BmpSrc.Canvas.Handle, R.Left + dw + lWidth, R.Top, w, tWidth, SRCCOPY);

            // left - bottom
            CopyTransRect(aBmp, BmpSrc, Left, Bottom - bWidth, Rect(R.Left + dw, R.Bottom - bWidth, R.Left + dw + lWidth - 1, R.Bottom - 1), clFuchsia, CI, True);

            // bottom - middle
            x := Left + lWidth;
            if not Stretch then begin
              if w > 0 then
                while x < Right - w - rWidth do begin
                  BitBlt(aBmp.Canvas.Handle, x, Bottom - bWidth, w, bWidth, BmpSrc.Canvas.Handle, R.Left + dw + lWidth, R.Bottom - bWidth, SRCCOPY);
                  inc(x, w);
                end;

              if x < Right - rWidth then
                BitBlt(aBmp.Canvas.Handle, x, Bottom - bWidth, Right - rWidth - x, bWidth, BmpSrc.Canvas.Handle, R.Left + dw + lWidth, R.Bottom - bWidth, SRCCOPY);
            end
            else
              StretchBlt(aBmp.Canvas.Handle, x, Bottom - bWidth, Right - rWidth - x, bWidth, BmpSrc.Canvas.Handle, R.Left + dw + lWidth, R.Bottom - bWidth, w, bWidth, SRCCOPY);

            // right - bottom
            CopyTransRect(aBmp, BmpSrc, Right - rWidth, Bottom - bWidth, Rect(R.Left + dw + w + lWidth, R.Bottom - bWidth, R.Left + dw + w + lWidth + rWidth - 1, R.Bottom - 1), clFuchsia, CI, True);
            // right - top
            CopyTransRect(aBmp, BmpSrc, Right - rWidth, Top, Rect(R.Left + dw + w + lWidth, R.Top, R.Left + dw + w + lWidth + rWidth - 1, R.Top + tWidth - 1), clFuchsia, CI, True);
            y := Top + tWidth;
            // right - middle
            if not Stretch then begin
              if h > 0 then
                while y < Bottom - h - bWidth do begin
                  BitBlt(aBmp.Canvas.Handle, Right - rWidth, y, rWidth, h, BmpSrc.Canvas.Handle, R.Left + dw + lWidth + w, R.Top + tWidth, SRCCOPY);
                  inc(y, h);
                end;

              if y < Bottom - bWidth then
                BitBlt(aBmp.Canvas.Handle, Right - rWidth, y, rWidth, Bottom - bWidth - y, BmpSrc.Canvas.Handle, R.Left + dw + w + lWidth, R.Top + tWidth, SRCCOPY);
            end
            else
              StretchBlt(aBmp.Canvas.Handle, Right - rWidth, y, rWidth, Bottom - bWidth - y, BmpSrc.Canvas.Handle, R.Left + dw + lWidth + w, R.Top + tWidth, rWidth, h, SRCCOPY);

            // Fill
            if DrawMode and BDM_FILL <> 0 then
              if not Stretch then begin
                y := Top + tWidth;
                if h > 0 then
                  while y < Bottom - h - bWidth do begin
                    x := Left + lWidth;
                    if w > 0 then
                      while x < Right - w - rWidth do begin
                        BitBlt(aBmp.Canvas.Handle, x, y, w, h, BmpSrc.Canvas.Handle, R.Left + dw + lWidth, R.Top + tWidth, SRCCOPY);
                        inc(x, w);
                      end;

                    if x < Right - rWidth then
                      BitBlt(aBmp.Canvas.Handle, x, y, Right - rWidth - x,  Bottom - bWidth - y, BmpSrc.Canvas.Handle, R.Left + dw + lWidth, R.Top + tWidth, SRCCOPY);

                    inc(y, h);
                  end;

                x := Left + lWidth;
                if y < Bottom - bWidth then begin
                  if w > 0 then
                    while x < Right - w - rWidth do begin
                      BitBlt(aBmp.Canvas.Handle, x, y, w, Bottom - bWidth - y, BmpSrc.Canvas.Handle, R.Left + dw + lWidth, R.Top + tWidth, SRCCOPY);
                      inc(x, w);
                    end;

                  if x < Right - rWidth then
                    BitBlt(aBmp.Canvas.Handle, x, y, Right - rWidth - x, Bottom - bWidth - y, BmpSrc.Canvas.Handle, R.Left + dw + lWidth, R.Top + tWidth, SRCCOPY);
                end;
              end
              else begin
                y := Top + tWidth;
                x := Left + lWidth;
                StretchBlt(aBmp.Canvas.Handle, x, y, Right - rWidth - x, Bottom - bWidth - y, BmpSrc.Canvas.Handle, R.Left + dw + lWidth, R.Top + tWidth, w, h, SRCCOPY);
              end;
          end
          // Paint with AlphaChannel
          else begin
            // left - top
            PaintBmpRect32(aBmp, BmpSrc, Rect(R.Left + dw, R.Top, R.Left + dw + lWidth - 1, R.Top + tWidth - 1), Point(Left, Top), Opacity);
            y := Top + tWidth;
            // left - middle
            if not Stretch then begin
              if h > 0 then
                while y < Bottom - h - bWidth do begin
                  PaintBmpRect32(aBmp, BmpSrc, Rect(R.Left + dw, R.Top + tWidth, R.Left + dw + lWidth - 1, R.Top + tWidth + h - 1), Point(Left, y), Opacity);
                  inc(y, h);
                end;

              if y < Bottom - bWidth then
                PaintBmpRect32(aBmp, BmpSrc, Rect(R.Left + dw, R.Top + tWidth, R.Left + dw + lWidth - 1, R.Top + tWidth + Bottom - bWidth), Point(Left, y), Opacity);
            end
            else
              StretchBmpRect32(aBmp, Left, y, MaxL, Bottom - bWidth - y, BmpSrc, R.Left + dw, R.Top + tWidth, MaxL, h, Opacity);

            // top - middle
            x := Left + lWidth;
            if not Stretch then begin
              if w > 0 then
                while x < Right - w - rWidth do begin
                  PaintBmpRect32(aBmp, BmpSrc, Rect(R.Left + dw + lWidth, R.Top, R.Left + dw + lWidth + lWidth - 1, R.Top + tWidth + h - 1), Point(x, Top), Opacity);
                  inc(x, w);
                end;

              if x < Right - rWidth then
                BitBlt(aBmp.Canvas.Handle, x, Top, Right - rWidth - x, tWidth, BmpSrc.Canvas.Handle, R.Left + dw + lWidth, R.Top, SRCCOPY);
            end
            else
              StretchBmpRect32(aBmp, x, Top, Right - rWidth - x, tWidth, BmpSrc, R.Left + dw + lWidth, R.Top, w, tWidth, Opacity);

            // left - bottom
            PaintBmpRect32(aBmp, BmpSrc, Rect(R.Left + dw, R.Bottom - bWidth, R.Left + dw + lWidth - 1, R.Bottom - 1), Point(Left, Bottom - bWidth), Opacity);

            // bottom - middle
            x := Left + lWidth;
            if not Stretch then begin
              if w > 0 then
                while x < Right - w - rWidth do begin
                  BitBlt(aBmp.Canvas.Handle, x, Bottom - bWidth, w, bWidth, BmpSrc.Canvas.Handle, R.Left + dw + lWidth, R.Bottom - bWidth, SRCCOPY);
                  inc(x, w);
                end;

              if x < Right - rWidth then
                BitBlt(aBmp.Canvas.Handle, x, Bottom - bWidth, Right - rWidth - x, bWidth, BmpSrc.Canvas.Handle, R.Left + dw + lWidth, R.Bottom - bWidth, SRCCOPY);
            end
            else
              StretchBmpRect32(aBmp, x, Bottom - bWidth, Right - rWidth - x, bWidth, BmpSrc, R.Left + dw + lWidth, R.Bottom - bWidth, w, bWidth, Opacity);

            // right - bottom
            PaintBmpRect32(aBmp, BmpSrc, Rect(R.Left + dw + w + lWidth, R.Bottom - bWidth, R.Left + dw + w + lWidth + rWidth - 1, R.Bottom - 1), Point(Right - rWidth, Bottom - bWidth), Opacity);
            // right - top
            PaintBmpRect32(aBmp, BmpSrc, Rect(R.Left + dw + w + lWidth, R.Top, R.Left + dw + w + lWidth + rWidth - 1, R.Top + tWidth - 1), Point(Right - rWidth, Top), Opacity);
            y := Top + tWidth;
            // right - middle
            if not Stretch then begin
              if h > 0 then
                while y < Bottom - h - bWidth do begin
                  BitBlt(aBmp.Canvas.Handle, Right - rWidth, y, rWidth, h, BmpSrc.Canvas.Handle, R.Left + dw + lWidth + w, R.Top + tWidth, SRCCOPY);
                  inc(y, h);
                end;

              if y < Bottom - bWidth then
                BitBlt(aBmp.Canvas.Handle, Right - rWidth, y, rWidth, Bottom - bWidth - y, BmpSrc.Canvas.Handle, R.Left + dw + w + lWidth, R.Top + tWidth, SRCCOPY);
            end
            else
              StretchBmpRect32(aBmp, Right - rWidth, y, rWidth, Bottom - bWidth - y, BmpSrc, R.Left + dw + lWidth + w, R.Top + tWidth, rWidth, h, Opacity);

            // Fill
            if DrawMode and BDM_FILL <> 0 then
              if not Stretch then begin
                y := Top + tWidth;
                if h > 0 then
                  while y < Bottom - h - bWidth do begin
                    x := Left + lWidth;
                    if w > 0 then
                      while x < Right - w - rWidth do begin
                        BitBlt(aBmp.Canvas.Handle, x, y, w, h, BmpSrc.Canvas.Handle, R.Left + dw + lWidth, R.Top + tWidth, SRCCOPY);
                        inc(x, w);
                      end;

                    if x < Right - rWidth then
                      BitBlt(aBmp.Canvas.Handle, x, y, Right - rWidth - x,  Bottom - bWidth - y, BmpSrc.Canvas.Handle, R.Left + dw + lWidth, R.Top + tWidth, SRCCOPY);

                    inc(y, h);
                  end;

                x := Left + lWidth;
                if y < Bottom - bWidth then begin
                  if w > 0 then
                    while x < Right - w - rWidth do begin
                      BitBlt(aBmp.Canvas.Handle, x, y, w, Bottom - bWidth - y, BmpSrc.Canvas.Handle, R.Left + dw + lWidth, R.Top + tWidth, SRCCOPY);
                      inc(x, w);
                    end;

                  if x < Right - rWidth then
                    BitBlt(aBmp.Canvas.Handle, x, y, Right - rWidth - x, Bottom - bWidth - y, BmpSrc.Canvas.Handle, R.Left + dw + lWidth, R.Top + tWidth, SRCCOPY);
                end;
              end
              else begin
                y := Top + tWidth;
                x := Left + lWidth;
                StretchBmpRect32(aBmp, x, y, Right - rWidth - x, Bottom - bWidth - y, BmpSrc, R.Left + dw + lWidth, R.Top + tWidth, w, h, Opacity);
              end;
          end;
        end
        else begin
          // left - top
          CopyByMask32Ex(Rect(Left, Top, Left + maxL{lWidth}, Top + tWidth),
                         Rect(R.Left + dw, R.Top, R.Left + dw + maxL{lWidth}, R.Top + tWidth), aBmp, BmpSrc, CI, MaskData, Opacity);

          CopyByMask32Ex(Rect(Left + maxL, Top, Left + lWidth, Top + maxT{tWidth}),
                         Rect(R.Left + dw + maxL, R.Top, R.Left + dw + lWidth, R.Top + maxT{tWidth}), aBmp, BmpSrc, CI, MaskData, Opacity);

          y := Top + tWidth;
          // left - middle
          if not Stretch then begin
            while y < Bottom - h - bWidth do begin
              CopyByMask32Ex(Rect(Left, y, Left + MaxL, y + h), Rect(R.Left + dw, R.Top + tWidth, R.Left + dw + MaxL, R.Top + h + tWidth), aBmp, BmpSrc, EmptyCI, MaskData, Opacity);
              inc(y, h);
            end;
            if y < Bottom - bWidth then
              CopyByMask32Ex(Rect(Left, y, Left + MaxL, Bottom - bWidth), Rect(R.Left + dw, R.Top + tWidth, R.Left + dw + MaxL, R.Top + tWidth + h), aBmp, BmpSrc, EmptyCI, MaskData, Opacity);
          end
          else
            StretchBltMask32Ex(Left, y, MaxL, Bottom - bWidth - y, R.Left + dw, R.Top + tWidth, MaxL, h, aBmp, BmpSrc, dhm, Opacity);

          // top - middle
          x := Left + lWidth;
          if not Stretch then begin
            while x < Right - w - rWidth do begin
              CopyByMask32Ex(Rect(x, Top, x + w, Top + MaxT), Rect(R.Left + dw + lWidth, R.Top, R.Left + dw + w + lWidth, R.Top + MaxT), aBmp, BmpSrc, EmptyCI, MaskData, Opacity);
              inc(x, w);
            end;
            if x < Right - rWidth then
              CopyByMask32Ex(Rect(x, Top, Right - rWidth, Top + MaxT), Rect(R.Left + dw + lWidth, R.Top, R.Left + dw + w + lWidth, R.Top + MaxT), aBmp, BmpSrc, EmptyCI, MaskData, Opacity);
          end
          else
            StretchBltMask32Ex(x, Top, Right - rWidth - x, MaxT, R.Left + dw + lWidth, R.Top, w, MaxT, aBmp, BmpSrc, dhm, Opacity);

          // left - bottom
          CopyByMask32Ex(Rect(Left, Bottom - bWidth, Left + maxL{lWidth}, Bottom),
                         Rect(R.Left + dw, R.Top + dh - bWidth, R.Left + dw + maxL{lWidth}, R.Top + dh), aBmp, BmpSrc, CI, MaskData, Opacity);

          CopyByMask32Ex(Rect(Left + maxL, Bottom - maxB, Left + lWidth, Bottom),
                         Rect(R.Left + dw + maxL, R.Top + dh - maxB, R.Left + dw + lWidth, R.Top + dh), aBmp, BmpSrc, CI, MaskData, Opacity);

          // bottom - middle
          x := Left + lWidth;
          if not Stretch then begin
            while x < Right - w - rWidth do begin
              CopyByMask32Ex(Rect(x, Bottom - MaxB, x + w, Bottom), Rect(R.Left + dw + lWidth, R.Top + dh - MaxB, R.Left + dw + w + lWidth, R.Top + dh), aBmp, BmpSrc, EmptyCI, MaskData, Opacity);
              inc(x, w);
            end;
            if x < Right - rWidth then
              CopyByMask32Ex(Rect(x, Bottom - MaxB, Right - rWidth, Bottom), Rect(R.Left + dw + lWidth, R.Top + dh - MaxB, R.Left + dw + w + lWidth, R.Top + dh), aBmp, BmpSrc, EmptyCI, MaskData, Opacity);
          end
          else
            StretchBltMask32Ex(x, Bottom - MaxB, Right - rWidth - x, MaxB, R.Left + dw + lWidth, R.Top + dh - MaxB, w, MaxB, aBmp, BmpSrc, dhm, Opacity);

          // right - bottom
          CopyByMask32Ex(Rect(Right - maxR{rWidth}, Bottom - bWidth, Right, Bottom),
                         Rect(R.Left + dw + w + lWidth + rWidth - maxR{lWidth}, R.Top + dh - bWidth, R.Left + dw + w + lWidth + rWidth,
                              R.Top + dh), aBmp, BmpSrc, CI, MaskData, Opacity);

          CopyByMask32Ex(Rect(Right - rWidth, Bottom - maxB{bWidth}, Right - maxR, Bottom),
                         Rect(R.Left + dw + MaskData.Width - rWidth, R.Top + dh - maxB, R.Left + dw + MaskData.Width - maxR, R.Top + dh),
                         aBmp, BmpSrc, CI, MaskData, Opacity);

          // right - top
          CopyByMask32Ex(Rect(Right - maxR{rWidth}, Top, Right, Top + tWidth),
                       Rect(R.Left + dw + w + lWidth + rWidth - maxR{lWidth}, R.Top, R.Left + dw + w + lWidth + rWidth, R.Top + tWidth),
                       aBmp, BmpSrc, CI, MaskData, Opacity);

          CopyByMask32Ex(Rect(Right - rWidth, Top, Right - maxR, Top + maxT{tWidth}),
                       Rect(R.Left + dw + MaskData.Width - lWidth, R.Top, R.Left + dw + MaskData.Width - MaxR, R.Top + maxT{tWidth}),
                       aBmp, BmpSrc, CI, MaskData, Opacity);

          // right - middle
          y := Top + tWidth;
          if not Stretch then begin
            while y < Bottom - h - bWidth do begin
              CopyByMask32Ex(Rect(Right - MaxR, y, Right, y + h), Rect(R.Left + dw + w + lWidth + rWidth - MaxR, R.Top + tWidth, R.Left + dw + w + lWidth + rWidth, R.Top + h + tWidth), aBmp, BmpSrc, EmptyCI, MaskData, Opacity);
              inc(y, h);
            end;
            if y < Bottom - bWidth then
              CopyByMask32Ex(Rect(Right - MaxR, y, Right, Bottom - bWidth), Rect(R.Left + dw + w + lWidth + rWidth - MaxR, R.Top + tWidth, R.Left + dw + w + lWidth + rWidth, R.Top + h + tWidth), aBmp, BmpSrc, EmptyCI, MaskData, Opacity);
          end
          else
            StretchBltMask32Ex(Right - MaxR, y, MaxR, Bottom - bWidth - y, R.Left + dw + lWidth + w + rWidth - MaxR, R.Top + tWidth, MaxR, h, aBmp, BmpSrc, dhm, Opacity);
        end;
      end;
end;


procedure ClearRoundMasks;
var
  i: integer;
begin
  for i := 0 to Length(aRoundMasks) - 1 do
    FreeAndNil(aRoundMasks[i]);
end;


initialization

finalization
  ClearRoundMasks;

end.





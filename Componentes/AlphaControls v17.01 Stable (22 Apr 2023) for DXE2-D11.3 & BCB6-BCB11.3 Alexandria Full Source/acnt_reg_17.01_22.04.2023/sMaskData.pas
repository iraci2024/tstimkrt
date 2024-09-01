unit sMaskData;
{$I sDefs.inc}

interface

uses
  Windows, Graphics,
  sConst;

type
  TacOutEffData = record
    ColorL,
    ColorT,
    ColorR,
    ColorB: TColor;

    OffsetL,
    OffsetT,
    OffsetR,
    OffsetB,

    WidthL,
    WidthT,
    WidthR,
    WidthB,
    Blur,
    Radius,

    Mask,
    Source, // 0 - None, 2 - Generated, 1 - Mask
    Opacity: integer;
  end;


  TacOutEffArray = array of TacOutEffData; // 0 item - Shadow, 1 - Lowered


  TsMaskData = record
    Bmp: TBitmap;

    ClassName,
    PropertyName: string;

    R: TRect;            // Rectangle of the image piece in MasterBitmap

    ImageCount,          // Count of States, allowed for control (count of images in piece)
    MaskType,            // Type of used mask (0 - not used, 1 - AlphaMask, 2... - reserved)
    DrawMode: smallint;  // Fill type if ImgType is texture (0-tiled, 1-stretched, 2-tiled horizontal top, 3-tiled vertical left, 4-stretched horizontal top,
                         //   5-stretched vertical left, 6-tiled hor. bottom, 7-tiled vert. right, 8-stretched hor. bottom, 9-stretched vert. right, A-discrete tiled,
                         //   B-stretch horz, C-stretch vert) (BDM_ACTIVEONLY used in borders)

    ImgType: TacImgType; // itisaBorder, itisaTexture, itisaGlyph //, itisanOldType (default)
    Manager: TObject;

    WL,                  // Border width / Left
    WT,                  // Top
    WR,                  // Right
    WB: smallint;        // Bottom

    Cfg,                 // 1 - has transparent pixels, 2 - control region must be changed, 4 - external
    MulPPi,
    DivPPI,
    SkinIndex,
    Width,
    Height: integer;
  end;
  PsMaskData = ^TsMaskData;

  TsFontColor = record
    Color,          // Text color
    Left,           // Colors
    Top,            //   of
    Right,          //   text
    Bottom: TColor; //   contours
  end;


  TsGenState = record
    GlowSize,                   // Size of text glowing
    ImagePercent,               // Percent of texture in BG
    TextureIndex,
    Transparency,               // Transparency of control
    GradientPercent: integer;   // Percent of gradient in BG
    Color,                      // Color of background
    GlowColor,                  // Color of text glowing
    LightColor,                 // Color of lighting
    BorderColor:   TColor;      // Color of border
    FontColor:     TsFontColor; // Text color structure
    GradientArray: TsGradArray;
  end;


  TsProps = array [0..ac_MaxPropsIndex] of TsGenState; // Array of properties for different states of control (0 - normal, 1 - active)


  TsGeneralData = record
    ParentClass,                 // Name of parent skin section (if exists)
    ClassName: string;           // Name of skin section
    Props: TsProps;              // Array of properties for different control states

    UseState2,
    GiveOwnFont,                 // Gives own font color for transparent child
    ShiftOnClick,
    ReservedBoolean: boolean;    // Reserved

    States,                      // Count of defined control states
    ScrollBorderOffset,          // Internal value which used for calculation of border width
    // Text Glow
    GlowCount,                   // Reserved
    GlowMargin,                  // Margin for glowing effect
    GlowSize,
    HotGlowSize,
    // Initialized values
    BorderWidth,                 // Width of painted border
    BorderRadius,                // Radius of painted border
    BorderIndex,                 // Index of border mask
    ImgTL,                       // Indexes
    ImgTR,                       //   of
    ImgBL,                       //     corner
    ImgBR: integer;              //       images

    HotGlowColor,
    GlowColor: TColor;

    // Outer effect
    OuterMode,
    OuterMask,
    OuterOpacity: integer;
    OuterOffset: TRect;

    OuterBlur,
    OuterRadius: integer;
    OuterColorL,
    OuterColorT,
    OuterColorR,
    OuterColorB: TColor;
  end;

  PsGeneralData = ^TsGeneralData;


  TsMaskArray = array of TsMaskData;
  TsGeneralDataArray = array of TsGeneralData;


function MkSize(const md: TsMaskData): TSize; overload;
function ScaleMaskData(var md: TsMaskData; const MasterBmp: TBitmap; IgnoreFuchsia: boolean): boolean;
function PtIsTransparent(X, Y: integer; const Mask: TsMaskData; Width, Height: integer): boolean;

implementation

uses Classes, SysUtils,
  sSkinManager, acntUtils, sGraphUtils, sVclUtils, math, sAlphaGraph;


function PtIsTransparent(X, Y: integer; const Mask: TsMaskData; Width, Height: integer): boolean;
begin
  Result := False;
  if Mask.Bmp <> nil then
    if Mask.Cfg and 1 = 1 then begin
      if X <= Mask.WL then begin
        if Y <= Mask.WT then
          Result := GetAPixel(Mask.Bmp, X, Y).C = clFuchsia
        else
          if Y >= Height - Mask.WB then
            Result := GetAPixel(Mask.Bmp, X, Mask.Height - Height + Y).C = clFuchsia;
      end
      else
        if X >= Width - Mask.WR then
          if Y <= Mask.WT then
            Result := GetAPixel(Mask.Bmp, Mask.Width - Width + X, Y).C = clFuchsia
          else
            if Y >= Height - Mask.WB then
              Result := GetAPixel(Mask.Bmp, Mask.Width - Width + X, Mask.Height - Height + Y).C = clFuchsia;
    end
    else
      if X <= Mask.WL then begin
        if Y <= Mask.WT then
          Result := GetAPixel(Mask.Bmp, X, Y).A = 0
        else
          if Y >= Height - Mask.WB then
            Result := GetAPixel(Mask.Bmp, X, Mask.Height - Height + Y).A = 0;
      end
      else
        if X >= Width - Mask.WR then
          if Y <= Mask.WT then
            Result := GetAPixel(Mask.Bmp, Mask.Width - Width + X, Y).A = 0
          else
            if Y >= Height - Mask.WB then
              Result := GetAPixel(Mask.Bmp, Mask.Width - Width + X, Mask.Height - Height + Y).A = 0;
end;


function MkSize(const md: TsMaskData): TSize;
begin
  Result.cx := md.Width;
  Result.cy := md.Height;
end;


function ScaleMaskData(var md: TsMaskData; const MasterBmp: TBitmap; IgnoreFuchsia: boolean): boolean;
var
  C: TColor;
  Bmp1, Bmp2, SrcBmp, MaskBmp, Source: TBitmap;
  ImgHeight, ImgWidth: integer;
  FreeSource: boolean;
  D0, D: PRGBAArray_D;
  S0, S: PRGBAArray_S;
  x, y, DeltaS, DeltaD: integer;

  procedure FuchsiaToMain(const R: TRect);
  var
    y, x: integer;
    D0, D: PRGBAArray_D;
    S0, S: PRGBAArray_S;
    DeltaS, DeltaD: integer;
  begin
    if InitLine(SrcBmp, Pointer(S0), DeltaS) and InitLine(MaskBmp, Pointer(D0), DeltaD) then
      for y := R.Top to R.Bottom - 1 do begin
        S := Pointer(PAnsiChar(S0) + DeltaS * Y);
        D := Pointer(PAnsiChar(D0) + DeltaD * Y);
        for X := R.Left to R.Right - 1 do
          with S[X], D[X] do
            if SC = sFuchsia.C then begin
              DC := $FFFFFF; // Update white pixels in mask
              SC := C;       // Change Fuchsia pixels to main color
              SA := 0;
            end
            else
              if DC = sFuchsia.C then
                DC := $FFFFFF; // Update white pixels in mask
      end;
  end;

begin
  Result := False;
  if md.MulPPI <> md.DivPPI then
    with md do begin
      Height := HeightOf(R);
      Width := WidthOf(R);
      ImgWidth := Width div ImageCount;
      if MaskType <> 0 then
        ImgHeight := Height div 2
      else
        ImgHeight := Height;
      // if not too small then
      if (WL + WR <> 2) and (WT + WB <> 2) and (ImgHeight >= 3) and (ImgWidth >= 3) then begin
        C := acColorToRGB(slMainColor);
        FreeSource := Bmp <> nil;
        if FreeSource then
          Source := Bmp
        else
          Source := MasterBmp;
        // Prepare SrcBmp
        SrcBmp := CreateBmp32(Width, ImgHeight);
        BitBlt(SrcBmp.Canvas.Handle, 0, 0, SrcBmp.Width, SrcBmp.Height, Source.Canvas.Handle, R.Left, R.Top, SRCCOPY);
        if MaskType <> 0 then begin
          MaskBmp := CreateBmpLike(SrcBmp);
          BitBlt(MaskBmp.Canvas.Handle, 0, 0, MaskBmp.Width, MaskBmp.Height, Source.Canvas.Handle, R.Left, R.Top + ImgHeight, SRCCOPY);
          FuchsiaToMain(Rect(0, 0, md.WL, md.WT));
          FuchsiaToMain(Rect(0, ImgHeight - md.WB, md.WL, ImgHeight));
          FuchsiaToMain(Rect(ImgWidth - md.WR, 0, ImgWidth, md.WT));
          FuchsiaToMain(Rect(ImgWidth - md.WR, ImgHeight - md.WB, ImgWidth, ImgHeight));
          Height := ((Height div 2) * md.MulPPI div md.DivPPI) * 2;
        end
        else begin
          if DrawMode and BDM_ALPHA = 0 then
            if not IgnoreFuchsia then
              if InitLine(SrcBmp, Pointer(S0), DeltaS) then
                for y := 0 to ImgHeight - 1 do begin
                  S := Pointer(PAnsiChar(S0) + DeltaS * Y);
                  for X := 0 to SrcBmp.Width - 1 do
                    with S[X] do
                      if SC = sFuchsia.C then begin
                        SC := C; // Change Fuchsia pixels to main color
                        SA := 0;
                      end;
                end;

          MaskBmp := nil;
          Height := Height * md.MulPPI div md.DivPPI;
        end;

        Width := (ImgWidth * md.MulPPI div md.DivPPI) * ImageCount;
        md.Bmp := CreateBmp32(Width, Height);
        if MaskType <> 0 then begin
          Bmp1 := CreateBmp32(Width, Height div 2);
          Stretch(SrcBmp, Bmp1, Bmp1.Width, Bmp1.Height, ftMitchell);
          Bmp2 := CreateBmpLike(Bmp1);
          Stretch(MaskBmp, Bmp2, Bmp2.Width, Bmp2.Height, ftMitchell);
          if InitLine(Bmp1, Pointer(D0), DeltaD) then
            for y := 0 to Bmp1.Height - 1 do begin
              D  := Pointer(PAnsiChar(D0) + DeltaD * Y);
              for X := 0 to Bmp1.Width - 1 do
                with D[X] do
                  if DA < 100 then begin
                    DC := sFuchsia.C; // Get Fuchsia pixels back
//                    DA := MaxByte;
                  end;
            end;

          BitBlt(Bmp.Canvas.Handle, 0, 0, Bmp1.Width, Bmp1.Height, Bmp1.Canvas.Handle, 0, 0, SRCCOPY);
          BitBlt(Bmp.Canvas.Handle, 0, Bmp1.Height, Bmp2.Width, Bmp2.Height, Bmp2.Canvas.Handle, 0, 0, SRCCOPY);
          Bmp1.Free;
          Bmp2.Free;
        end
        else begin
{          if md.MulPPI < md.DivPPI then
            Stretch(SrcBmp, Bmp, Bmp.Width, Bmp.Height, ftLanczos3)
          else    }
            Stretch(SrcBmp, Bmp, Bmp.Width, Bmp.Height, ftMitchell);

          if DrawMode and BDM_ALPHA = 0 then
            if not IgnoreFuchsia then
              if InitLine(Bmp, Pointer(S0), DeltaS) then
                for y := 0 to Bmp.Height - 1 do begin
                  S := Pointer(PAnsiChar(S0) + DeltaS * Y);
                  for X := 0 to Bmp.Width - 1 do
                    with S[X] do
                      if SA < 90 then
                        SC := sFuchsia.C // Change Fuchsia pixels to main color
                      else
                        SA := MaxByte;
                end;
        end;

        if FreeSource then
          Source.Free;

        R := MkRect(Bmp);
        Width := Width div ImageCount;
        if DrawMode and BDM_ALPHA = 0 then
          Height := Height div (1 + MaskType)
        else
          Height := Height;

        WL := MulDiv(WL, md.MulPPI, md.DivPPI);
        WT := MulDiv(WT, md.MulPPI, md.DivPPI);
        WR := min(MulDiv(WR, md.MulPPI, md.DivPPI), Width - WL - 1);
        WB := min(MulDiv(WB, md.MulPPI, md.DivPPI), Height - WT - 1);
        SrcBmp.Free;
        Result := True;
        if MaskBmp <> nil then
          FreeAndNil(MaskBmp);
      end;
    end;
end;

end.

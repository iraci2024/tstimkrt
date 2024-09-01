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

unit WEBLib.TMSFNCGraphicsTools;

{$I WEBLib.TMSFNCDefines.inc}
{$IFDEF LCLLIB}
  {$DEFINE LCLWEBLIB}
{$ENDIF}
{$IFDEF WEBLIB}
  {$DEFINE LCLWEBLIB}
  {$DEFINE FMXWEBLIB}
{$ENDIF}
{$IFDEF FMXLIB}
  {$DEFINE FMXWEBLIB}
{$ENDIF}
interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  Classes, Types, WEBLib.TMSFNCTypes, WEBLib.TMSFNCGraphicsTypes, WEBLib.Graphics
  {$IFDEF WEBLIB}
  ,JS, Web, SysUtils
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

type
  {$IFDEF CMNLIB}
  PRGBTripleArray = ^TRGBTripleArray;
  TRGBTripleArray = array[0..4095] of TRGBTriple;
  {$ENDIF}
  TBool2DArray = array of array of Boolean;

function GetMagicWandPolygon(ABitmap: TTMSFNCBitmap; X, Y: Single; Tolerance: Single = 0.1; DensityValue: Single = 50; DensityMax: Single = 50): TTMSFNCGraphicsPathPolygon;
function GetPixel(ABitmap: TTMSFNCBitmap; AX, AY: Single): TTMSFNCGraphicsColor;
function GetPolygonBoundsRect(APolygon: TTMSFNCGraphicsPathPolygon): TRectF;
function InsertPointInPolygon(APolygon: TTMSFNCGraphicsPathPolygon; APoint: TPointF; AIndex: Integer): TTMSFNCGraphicsPathPolygon;
function RemovePointInPolygon(APolygon: TTMSFNCGraphicsPathPolygon; AIndex: Integer): TTMSFNCGraphicsPathPolygon;

implementation


function CalcWandPoly( {$IFDEF FMXLIB}var d: TBitmapData; {$ENDIF}{$IFDEF WEBLIB}var d: TJSImageData; {$ENDIF} bmp: TBitmap; var mask: TBool2DArray; DensityValue, DensityMax: Single): TTMSFNCGraphicsPathPolygon;
var
  i,j,xi,yi,xc,yc,nx,ny: Integer;
  modified: Boolean;
  nPoints: Integer;
  tt,modifiable: array of boolean;
  a,c,Distance: Real;
  odir,dir: Integer;
  {$IFDEF WEBLIB}
  FOffs: Integer;
  {$ENDIF}

//------------------------------------------------------------------------------
  function newx(x,dir:Integer):Integer;//calculate new x in the direction specified
  begin
    case (dir mod 8) of
    0:Result := x;                    //N
    1:Result := x + 1;                //NE
    2:Result := x + 1;                //E
    3:Result := x + 1;                //SE
    4:Result := x;                    //S
    5:Result := x - 1;                //SW
    6:Result := x - 1;                //W
    7:Result := x - 1;                //NW
    else
      Result := 0; //to stop compiler warnings
    end;
  end;

//------------------------------------------------------------------------------
  function newy(y,dir:Integer):Integer;
  begin
    case (dir mod 8) of
    0:Result := y - 1;                //N
    1:Result := y - 1;		      //NE
    2:Result := y;		      //E
    3:Result := y + 1;		      //SE
    4:Result := y + 1;		      //S
    5:Result := y + 1;		      //SW
    6:Result := y;		      //W
    7:Result := y - 1;		      //NW
    else
      Result := 0; //to stop compiler warnings
    end;
  end;

//------------------------------------------------------------------------------
begin
  nPoints := 0;
  Result := nil;
  SetLength(Result,20);//grow 20 points at a time
  SetLength(modifiable,20);

  xi := -1;
  yi := -1;

  //find the first point of the region
  for j := 0 to bmp.Height - 1 do
  begin
    for i := 0 to bmp.Width - 1 do
      if mask[i,j] then
      begin
        xi := i;
        yi := j;
        Break;
      end;
    if mask[i,j] then Break;
  end;

  //if there is no point then Exit
  if (xi < 0) or (yi < 0) then
  begin
    Result := nil;
    Exit;
  end;

  xc := xi;
  yc := yi;
  dir := 8; //the first direction is North, orthogonal with current direction
            //(which is East since we were scanning left to right)

  odir := -1;
  j := 0; //"fire escape" variable

  repeat
    inc(j);
    for i := 0 to 7 do //for all 8 directions
    begin
      nx := newx(xc,dir);
      ny := newy(yc,dir);
      try
        if (nx >= bmp.Width) or (ny >= bmp.Height) or (nx < 0) or (ny < 0) then
          Continue;
        if mask[nx,ny] then //if it found a point then add it
        begin
          //if the direction changed (if not, it is useless to add another point)
          if (odir mod 8) <> (dir mod 8) then
          begin
            if nPoints >= Length(Result) then
            begin
              SetLength(Result,nPoints + 20);
              SetLength(modifiable,nPoints + 20);
            end;
            Result[nPoints].x := xc;
            Result[nPoints].y := yc;

            {$IFDEF FMXLIB}
            D.SetPixel(xc, yc, gcBlue);
            {$ENDIF}
            {$IFDEF CMNLIB}
            bmp.Canvas.Pixels[xc,yc] := gcBlue;
            {$ENDIF}
            {$IFDEF WEBLIB}
             FOffS := (Round(xc) + (Round(yc) * Round(bmp.Width))) * 4;
             d.data[FOffS + 0] := 0;
             d.data[FOffS + 1] := 0;
             d.data[FOffS + 2] := 255;
            {$ENDIF}

            modifiable[nPoints] := True;
            inc(nPoints);
          end;
          //if it is a corner, mark it as non eraseable
          if ((8 + dir - odir) mod 8) > 1 then
            modifiable[nPoints-1] := False;
          xc := nx;
          yc := ny;
          odir := dir;
          //new direction is orthogonal on current direction (dir := dir - 3 + 1 -from finally)
          dir := ((dir + 5) mod 8) + 8;
          Break;
        end;
      finally
        inc(dir);
      end;
    end;
    if j > 1000000 then
      Break;  //in case of fire break window
  until (xc = xi) and (yc = yi);

  //simple algorithm to reduce the number of points:
  //repeat
  // for all points
  //  if the distance between i+1 and (i,i+3) line is smaller than Distance
  //  then delete i+1
  //until no point was deleted

  // for high accuracy, do not reduce points
  Setlength(Result,nPoints);
  Setlength(tt,nPoints);

  Distance := 3 * ((DensityMax - DensityValue + 5) / DensityMax);

  repeat
    Setlength(Result,nPoints);
    Setlength(tt,nPoints);
    Modified := False;

    for i := 0 to High(Result) do
      tt[i] := True;

    for i := 0 to High(Result)-2 do
    begin
      if (not tt[i])or(not modifiable[i+1]) then
        Continue;

      if Abs(Result[i+2].x - Result[i].x) < 0.0000001 then
      begin
        if Abs(Result[i+1].x-Result[i].x) < Distance then
        begin
          tt[i+1] := False;
          modified := True;
        end;
      end
      else
      begin
        a := -(Result[i+2].y-Result[i].y)/(Result[i+2].x-Result[i].x);
        c := -Result[i].y-a*Result[i].x;
        if((abs(a*Result[i+1].x+Result[i+1].y+c)/sqrt(a*a+1))<Distance) then
        begin
          tt[i+1] := False;
          modified := True;
        end;
      end;
    end;

    nPoints := 0;
    for i := 0 to High(Result) do
    begin
      if tt[i] then
      begin
        if nPoints <> i then
          Result[nPoints] := Result[i];
        inc(nPoints);
      end;
    end;
  until modified = False;
end;

{$HINTS OFF}
function GetMagicWandPolygon(ABitmap: TTMSFNCBitmap; X, Y: Single; Tolerance: Single = 0.1; DensityValue: Single = 50; DensityMax: Single = 50): TTMSFNCGraphicsPathPolygon;
var
  nf: Integer;
  i,j,ir: Integer;
  xf,yf: Single;
  mask: TBool2DArray;
  outermask: TBool2DArray;
  oldcolor: TTMSFNCGraphicsColor;
  fillx:array[0..80000]of Single;
  filly:array[0..80000]of Single;
  {$IFDEF FMXLIB}
  d: TBitmapData;
  {$ENDIF}
  {$IFDEF CMNLIB}
  lPixels: PRGBTripleArray;
  {$ENDIF}
  {$IFDEF WEBLIB}
  d: TJSImageData;
  FOffS:Integer;
  img: TJSObject;
  {$ENDIF}
  bmp: TBitmap;

  procedure grow_region;
  var
    pixr,pixg,pixb,oldr,oldg,oldb: Byte;
    {$IFDEF FMXLIB}
    pix: TAlphaColor;
    {$ENDIF}
    jj: Byte;
    xr,yr: Longint;
  begin
    oldr := Byte(oldcolor);
    oldg := Byte(oldcolor shr 8);
    oldb := Byte(oldcolor shr 16);

    xr := Round(xf + 1);
    yr := Round(yf);

    for jj := 1 to 4 do
    begin
      //Growing the Region
      if jj = 1 then
      begin
        xr := Round(xf + 1);
        yr := Round(yf);
      end;
      if jj = 2 then
      begin
        xr := Round(xf - 1);
        yr := Round(yf);
      end;
      if jj = 3 then
      begin
        xr := Round(xf);
        yr := Round(yf + 1);
      end;
      if jj = 4 then
      begin
        xr := Round(xf);
        yr := Round(yf - 1);
      end;

      //if it's not outside the bitmap
      if ((xr < ABitmap.Width) and (jj = 1)) or ((xr >= 0) and (jj = 2))
        or ((yr < ABitmap.Height) and (jj = 3)) or ((yr >= 0)and (jj = 4)) then
      begin
        {$IFDEF FMXLIB}
        pix := d.GetPixel(xr, yr);
        pixr := Byte(pix);
        pixg := Byte(pix shr 8);
        pixb := Byte(pix shr 16);
        {$ENDIF}
        {$IFDEF VCLLIB}
        lPixels := bmp.Scanline[Round(yr)];
        pixr := lPixels[xr].rgbtRed;
        pixg := lPixels[xr].rgbtGreen;
        pixb := lPixels[xr].rgbtBlue;
        {$ENDIF}
        {$IFDEF LCLLIB}
        lPixels := bmp.Scanline[Round(yr)];
        pixr := lPixels^[xr].rgbtRed;
        pixg := lPixels^[xr].rgbtGreen;
        pixb := lPixels^[xr].rgbtBlue;
        {$ENDIF}
        {$IFDEF WEBLIB}
        FOffS := (Round(xr) + (Round(yr) * Round(ABitmap.Width))) * 4;
        pixr := d.data[FOffS + 0];
        pixg := d.data[FOffS + 1];
        pixb := d.data[FOffS + 2];
        {$ENDIF}
        // if the current pixel should be part of the region then add it to it
        if (not Mask[xr,yr]) and (Abs(pixr - oldr) <= Tolerance * 150) and (Abs(pixg-oldg) <= Tolerance * 150) and (Abs(pixb-oldb) <= tolerance*150)then
        begin
          mask[xr,yr] := true;

          nf := nf + 1;
          //add to queue
          fillx[nf] := xr;
          filly[nf] := yr;
        end;
      end;
    end;
  end;

//------------------------------------------------------------------------------
begin
  Result := nil;
  //Image initialization
  if (x > ABitmap.Width) or (x < 0) or
   (y < 0) or (y > ABitmap.Height) then
    Exit;

  {$IFDEF FMXLIB}
  bmp := TBitmap.Create(ABitmap.Width, ABitmap.Height);
  bmp.Assign(ABitmap);
  bmp.Map(TMapAccess.Read, d);
  {$ENDIF}
  {$IFDEF CMNLIB}
  bmp := TBitmap.Create;
  bmp.Width := ABitmap.Graphic.Width;
  bmp.Height := ABitmap.Graphic.Height;
  bmp.PixelFormat := pf24bit;
  bmp.Assign(ABitmap.Graphic);
  {$ENDIF}
  {$IFDEF WEBLIB}
  if not ABitmap.Empty then
  begin
    img := ABitmap.Image;
    asm
      var canvas = document.createElement("canvas");
      canvas.width = img.width;
      canvas.height = img.height;
      var ctx = canvas.getContext("2d");
      ctx.drawImage(img, 0, 0);
      var dataURL = canvas.toDataURL("image/png");
      d = canvas.getContext('2d').getImageData(0, 0, canvas.width, canvas.height);
    end;
  end;

  bmp := TBitmap.Create;
  bmp.Width := ABitmap.Width;
  bmp.Height := ABitmap.Height;
  bmp.Assign(ABitmap);
  {$ENDIF}

  try
    // initialize boolean array
    SetLength(mask, ABitmap.Width + 5, ABitmap.Height + 5);
    SetLength(outermask, ABitmap.Width + 5, ABitmap.Height + 5);

    // start color
    {$IFDEF FMXLIB}
    if Integer(d.PixelFormat) <> 0 then
    {$ENDIF}
    begin
      {$IFDEF FMXLIB}
      oldcolor := d.GetPixel(Round(x), Round(y));
      {$ENDIF}
      {$IFDEF VCLLIB}
      lPixels := bmp.Scanline[Round(y)];
      oldcolor := RGB(lPixels[Round(x)].rgbtRed, lPixels[Round(x)].rgbtGreen, lPixels[Round(x)].rgbtBlue);
      {$ENDIF}
      {$IFDEF LCLLIB}
      lPixels := bmp.Scanline[Round(y)];
      oldcolor := RGB(lPixels^[Round(x)].rgbtRed, lPixels^[Round(x)].rgbtGreen, lPixels^[Round(x)].rgbtBlue);
      {$ENDIF}
      {$IFDEF WEBLIB}
      FOffS := (Round(x) + (Round(y) * Round(ABitmap.Width))) * 4;
      oldColor := RGB(d.data[FOffS + 0], d.data[FOffS + 1], d.data[FOffS + 2]);
      {$ENDIF}

      nf := 1;
      ir := 1;
      fillx[nf] := x;
      filly[nf] := y;
      xf := x;
      yf := y;
      grow_region;

      while (nf > ir) do
      begin
        //extract point & grow_region(point)
        ir := ir + 1;
        xf := fillx[ir];
        yf := filly[ir];
        grow_region;
        if (nf > 75000)  then
        begin
          for i := 1 to nf-ir do
          begin
            fillx[i] := fillx[ir + i];
            filly[i] := filly[ir + i];
          end;
          nf := nf - ir ;
          ir := 0;
        end;
      end;

     for i := 0 to bmp.Width - 1 do
     begin
       for j := 0 to bmp.Height - 1 do
       begin
         if mask[i,j] then
         begin
           outermask[i,j] := mask[i,j];

           if (i > 0) then
             outermask[i - 1, j] := true;

           if (j > 0) then
             outermask[i, j - 1] := true;

           if (i < bmp.Width) then
             outermask[i + 1, j] := true;

           if (j < bmp.Height) then
             outermask[i, j + 1] := true;
         end;

       end;
     end;

      Result := CalcWandPoly({$IFDEF FMXWEBLIB}d, {$ENDIF}bmp, outermask, DensityValue, DensityMax);
    end;
  finally
    {$IFDEF FMXLIB}
    bmp.Unmap(d);
    {$ENDIF}
    bmp.Free;
  end;
end;

function GetPixel(ABitmap: TTMSFNCBitmap; AX, AY: Single): TTMSFNCGraphicsColor;
var
  clr: TTMSFNCGraphicsColor;
  {$IFNDEF WEBLIB}
  bmp: TBitmap;
  {$ENDIF}
  {$IFDEF FMXLIB}
  d: TBitmapData;
  {$ENDIF}
  {$IFDEF CMNLIB}
  lPixels: PRGBTripleArray;
  {$ENDIF}
  {$IFDEF WEBLIB}
  d: TJSImageData;
  FOffS:Integer;
  img: TJSObject;
  {$ENDIF}
begin
  clr := gcNull;

  {$IFDEF FMXLIB}
  bmp := TBitmap.Create(ABitmap.Width, ABitmap.Height);
  bmp.Assign(ABitmap);
  bmp.Map(TMapAccess.Read, d);
  try
    clr := d.GetPixel(Round(AX), Round(AY));
  finally
    bmp.Unmap(d);
    bmp.Free;
  end;
  {$ENDIF}

  {$IFDEF CMNLIB}
  bmp := TBitmap.Create;
  bmp.Width := ABitmap.Graphic.Width;
  bmp.Height := ABitmap.Graphic.Height;
  bmp.PixelFormat := pf24bit;
  bmp.Assign(ABitmap.Graphic);
  try
    lPixels := bmp.Scanline[Round(AY)];
    {$IFDEF VCLLIB}
    clr := RGB(lPixels[Round(AX)].rgbtRed, lPixels[Round(AX)].rgbtGreen, lPixels[Round(AX)].rgbtBlue);
    {$ENDIF}
    {$IFDEF LCLLIB}
    clr := RGB(lPixels^[Round(AX)].rgbtRed, lPixels^[Round(AX)].rgbtGreen, lPixels^[Round(AX)].rgbtBlue);
    {$ENDIF}
  finally
    bmp.Free;
  end;
  {$ENDIF}

  {$IFDEF WEBLIB}
  if not ABitmap.Empty then
  begin
    img := ABitmap.Image;
    asm
      var canvas = document.createElement("canvas");
      canvas.width = img.width;
      canvas.height = img.height;
      var ctx = canvas.getContext("2d");
      ctx.drawImage(img, 0, 0);
      var dataURL = canvas.toDataURL("image/png");
      d = canvas.getContext('2d').getImageData(0, 0, canvas.width, canvas.height);
    end;
  end;

  FOffS := (Round(AX) + (Round(AY) * Round(ABitmap.Width))) * 4;
  clr := RGB(d.data[FOffS + 0], d.data[FOffS + 1], d.data[FOffS + 2]);
  {$ENDIF}

  Result := clr;
end;
{$HINTS ON}

function GetPolygonBoundsRect(APolygon: TTMSFNCGraphicsPathPolygon): TRectF;
var
  xmin, xmax, ymin, ymax: Single;
  I: Integer;
begin
  if Length(APolygon) > 0 then
  begin
    xmin := 100000;
    ymin := 100000;
    xmax := -100000;
    ymax := -100000;
    for I := 0 to Length(APolygon) - 1 do
    begin
      if (APolygon[I].X < xmin) then
        xmin := APolygon[I].X;
      if (APolygon[I].Y < ymin) then
        ymin := APolygon[I].Y;
      if (APolygon[I].X > xmax) then
        xmax := APolygon[I].X;
      if (APolygon[I].Y > ymax) then
        ymax := APolygon[I].Y;

//      xmin := (APolygon[I].X < xmin) * APolygon[I].X;
//      ymin := (APolygon[I].Y < ymin) * APolygon[I].Y;
//      xmax := (APolygon[I].X > xmax) * APolygon[I].X;
//      ymax := (APolygon[I].Y > ymax) * APolygon[I].Y;
    end;

    Result := RectF(xmin, ymin, xmax, ymax);
  end
  else
    Result := RectF(-1, -1, -1, -1);
end;

function InsertPointInPolygon(APolygon: TTMSFNCGraphicsPathPolygon; APoint: TPointF; AIndex: Integer): TTMSFNCGraphicsPathPolygon;
var
  tempArr: TTMSFNCGraphicsPathPolygon;
  {$IFDEF LCLLIB}
  I, idx: Integer;
  {$ENDIF}
begin
  {$IFNDEF LCLWEBLIB}
  tempArr := APolygon;
  {$HINTS OFF}
  {$IF COMPILERVERSION > 26}
  Insert([APoint], tempArr, AIndex);
  {$IFEND}
  {$HINTS ON}
  {$ENDIF}
  {$IFDEF WEBLIB}
  tempArr := APolygon;
  System.Insert(APoint, tempArr, AIndex);
  {$ENDIF}
  {$IFDEF LCLLIB}
  if (AIndex >= Length(APolygon)) then
  begin
    tempArr := APolygon;
    SetLength(tempArr, Length(APolygon) + 1);
    tempArr[Length(tempArr) - 1] := APoint;
  end
  else
  begin
    SetLength(tempArr, Length(APolygon) + 1);

    idx := 0;
    for I := 0 to Length(APolygon) - 1 do
    begin
      if idx = AIndex then
      begin
        tempArr[idx] := APoint;
        Inc(Idx);
      end;
      tempArr[idx] := APolygon[I];
      Inc(Idx)
    end;
  end;
  {$ENDIF}

  Result := tempArr;
end;

function RemovePointInPolygon(APolygon: TTMSFNCGraphicsPathPolygon; AIndex: Integer): TTMSFNCGraphicsPathPolygon;
var
  tempArr: TTMSFNCGraphicsPathPolygon;
  {$IFDEF LCLLIB}
  I, idx: Integer;
  {$ENDIF}
begin
  {$IFNDEF LCLLIB}
  tempArr := APolygon;
  {$HINTS OFF}
  {$IF COMPILERVERSION > 26}
  Delete(tempArr, AIndex, 1);
  {$IFEND}
  {$HINTS ON}
  {$ENDIF}
  {$IFDEF LCLLIB}
  SetLength(tempArr, Length(APolygon) - 1);
  idx := 0;
  for I := 0 to Length(APolygon) - 1 do
  begin
    if I <> AIndex then
    begin
      tempArr[idx] := APolygon[I];
      Inc(Idx)
    end;
  end;
  {$ENDIF}

  Result := tempArr;
end;

end.

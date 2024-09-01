// SSV

unit acImage;
{$I sDefs.inc}
// {$DEFINE LOGGED}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Imglist,
{$IFNDEF DELPHI5} Types, {$ENDIF}
  sConst, ExtCtrls, acAlphaImageList, sCommonData,
  // SSV
{$IFDEF USE_INET}
  WinINet,
{$ENDIF}
//  PNGImage,
{$IFDEF D2007}
  GIFImg,
{$ENDIF}
  Jpeg;


type
  // SSV
  TDownloadCompleteEvent = procedure(Sender: TObject) of object;
  TDownloadCancelEvent = procedure(Sender: TObject; var Cancel: boolean) of object;
  TDownloadErrorEvent = procedure(Sender: TObject; ErrorMsg: string) of object;
  // ...

{$IFNDEF NOTFORHELP}


  TsCustomImage = class(TImage)
  private
    FGrayed, FReflected, FUseFullSize: boolean;

    FBlend: TPercent;
    FImageIndex: integer;
    FImages: TCustomImageList;
    FCommonData: TsCtrlSkinData;
    FImageChangeLink: TChangeLink;
    FTransparentForMouse: boolean;
    //
    FIsDownloading: boolean;
    FAbortDownload: boolean;
    // SSV
    FDownloadErrorEvent: TDownloadErrorEvent;
    FOnDownloadComplete: TDownloadCompleteEvent;
    //
{$IFNDEF D2010}
    FOnMouseLeave, FOnMouseEnter: TNotifyEvent;
{$ENDIF}
    procedure SetBlend(const Value: TPercent);
    procedure SetImageIndex(const Value: integer);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetBoolean(const Index: integer; const Value: boolean);
    procedure ImageListChange(Sender: TObject);
  protected
    ImageChanged: boolean;
    FOnDownloadCancel: TDownloadCancelEvent;
    FOnDownloadCompleteThread: TDownloadCompleteEvent;
    function OwnDrawing: boolean;
    function PrepareCache(DC: HDC): boolean;
    function CanAutoSize(var NewWidth, NewHeight: integer): boolean; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function GetCanvas: TCanvas;
  public
    procedure AfterConstruction; override;
    procedure Loaded; override;
    constructor Create(AOwner: TComponent); override;
    procedure UpdateImage;
    property Canvas: TCanvas read GetCanvas;
    // SSV
    property AbortDownload: boolean write FAbortDownload;
    property IsDownloading: boolean read FIsDownloading;
    //

    function Empty: boolean;
    destructor Destroy; override;
    procedure acWM_Paint(var Message: TWMPaint); message WM_PAINT;
    procedure WndProc(var Message: TMessage); override;
    // SSV
    function LoadFromStream(aStream: TStream): boolean;
    function LoadFromURL(aURL: string; CanCache: boolean = true): boolean;
    function LoadFromURLThread(aURL: string; CanCache: boolean = true): boolean;
    // ...
    property Blend: TPercent read FBlend write SetBlend default 0;

    property UseFullSize: boolean index 0 read FUseFullSize write SetBoolean default False;
    property Reflected: boolean index 1 read FReflected write SetBoolean default False;
    property Grayed: boolean index 2 read FGrayed write SetBoolean default False;

    property TransparentForMouse: boolean read FTransparentForMouse write FTransparentForMouse default False;

    property ImageIndex: integer read FImageIndex write SetImageIndex default -1;
    property Images: TCustomImageList read FImages write SetImages;
    property SkinData: TsCtrlSkinData read FCommonData write FCommonData;
{$IFNDEF D2010}
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
{$ENDIF}
    // SSV
    property OnDownloadComplete: TDownloadCompleteEvent read FOnDownloadComplete write FOnDownloadComplete;
    property OnDownloadCancel: TDownloadCancelEvent read FOnDownloadCancel write FOnDownloadCancel;
    property OnDownloadError: TDownloadErrorEvent read FDownloadErrorEvent write FDownloadErrorEvent;
    //
  end;
{$ENDIF}
{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}


  TsImage = class(TsCustomImage)
  published
{$IFNDEF NOTFORHELP}
    property Picture;
    property OnMouseEnter;
    property OnMouseLeave;
{$ENDIF}
    property Blend;
    property ImageIndex;
    property Images;
    property Grayed;
    property Reflected;
    property SkinData;
    property UseFullSize;
    property TransparentForMouse;
    // SSV
    // property OnDownloadComplete: TDownloadCompleteEvent read FOnDownloadComplete write FOnDownloadComplete;
    property OnDownloadCompleteThread: TDownloadCompleteEvent read FOnDownloadCompleteThread write FOnDownloadCompleteThread;
    property OnDownloadCancel: TDownloadCancelEvent read FOnDownloadCancel write FOnDownloadCancel;
  end;


  TImageDownloadThread = class(TThread)
  private
    FURL: string;
    FPicture: TsCustomImage;
    FCanCache: boolean;
  protected
    procedure Execute; override;
    procedure DownloadCompleteThread;
  public
    constructor Create(URL: string; Cached: boolean; item: TsCustomImage);
  end;

implementation

uses
  math,
{$IFDEF DELPHI_XE4}AnsiStrings, {$ENDIF}
{$IFDEF LOGGED} sDebugMsgs, {$ENDIF}
  sGraphUtils, acntUtils, sAlphaGraph, sSkinManager, sThirdParty, acPNG, sVCLUtils, sMessages, sStyleSimply;


procedure TsCustomImage.AfterConstruction;
begin
  inherited;
  FCommonData.Loaded;
end;


constructor TsCustomImage.Create(AOwner: TComponent);
begin
  FCommonData := TsCtrlSkinData.Create(Self, true);
  FCommonData.COC := COC_TsImage;
  inherited;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
  ImageChanged := true;
  FTransparentForMouse := False;
  FImageIndex := -1;
  // SSV
  FIsDownloading := False;
end;


destructor TsCustomImage.Destroy;
begin
  FreeAndNil(FCommonData);
  FreeAndNil(FImageChangeLink);
  inherited;
end;


procedure TsCustomImage.SetBlend(const Value: TPercent);
begin
  if FBlend <> Value then begin
    FBlend := Value;
    SkinData.Invalidate;
  end;
end;


procedure TsCustomImage.SetBoolean(const Index: integer; const Value: boolean);

  procedure ChangeProp(var Prop: boolean; Value: boolean);
  begin
    if Prop <> Value then begin
      Prop := Value;
      SkinData.Invalidate;
    end;
  end;


begin
  case index of
    0:
      ChangeProp(FUseFullSize, Value);
    1:
      ChangeProp(FReflected, Value);
    2:
      ChangeProp(FGrayed, Value);
  end;
end;


procedure TsCustomImage.SetImageIndex(const Value: integer);
begin
  if FImageIndex <> Value then begin
    FImageIndex := Value;
    if AutoSize then
      AdjustSize;

    SkinData.Invalidate;
  end;
end;


procedure TsCustomImage.SetImages(const Value: TCustomImageList);
begin
  if FImages <> Value then begin
    if FImages <> nil then
      Images.UnRegisterChanges(FImageChangeLink);

    FImages := Value;
    if Images <> nil then begin
      Images.RegisterChanges(FImageChangeLink);
      Images.FreeNotification(Self);
    end;
    UpdateImage;
  end;
end;


procedure TsCustomImage.ImageListChange(Sender: TObject);
begin
  SkinData.Invalidate;
end;


function TsCustomImage.OwnDrawing: boolean;
begin
  Result := Reflected or Grayed or (Blend > 0);
end;


function TsCustomImage.PrepareCache(DC: HDC): boolean;
var
  R: TRect;
  C: TsColor;
  CI: TCacheInfo;
  BGInfo: TacBGInfo;

  function GetSrcSize(CalcStretch: boolean = true): TSize;
  var
    SrcHeight: integer;
{$IFNDEF DELPHI5}
    xyaspect: real;
{$ENDIF}
  begin
    if (ImageIndex >= 0) and (Images <> nil) then
      if Stretch then begin
        if Images is TsAlphaImageList then
          Result := MkSize(TsAlphaImageList(Images).Items[ImageIndex].OrigWidth, TsAlphaImageList(Images).Items[ImageIndex].OrigHeight)
        else
          if (Images is TsVirtualImageList) then begin
            if TsVirtualImageList(Images).AlphaImageList <> nil then
              with TsAlphaImageList(TsVirtualImageList(Images).AlphaImageList).Items[ImageIndex] do
                Result := MkSize(OrigWidth, OrigHeight);
          end
          else
            Result := MkSize(Width, Height);
      end
      else
        Result := MkSize(GetImageWidth(Images, ImageIndex, GetPPI(SkinData)), GetImageHeight(Images, ImageIndex, GetPPI(SkinData)))
    else
      Result := MkSize(Picture.Width, Picture.Height);

    if CalcStretch and (Result.cx <> 0) and (Result.cy <> 0) then begin // Picture is not empty?
      SrcHeight := Height;
      if Reflected then
        SrcHeight := SrcHeight * 2 div 3;
{$IFNDEF DELPHI5}
      if Proportional then begin
        xyaspect := Result.cx / Result.cy;
        if Result.cx > Result.cy then begin
          Result.cx := Width;
          Result.cy := Round(Width / xyaspect);
          if Result.cy > SrcHeight then begin
            Result.cy := SrcHeight;
            Result.cx := Round(Result.cy * xyaspect);
          end;
        end
        else begin
          Result.cy := SrcHeight;
          Result.cx := Round(Result.cy * xyaspect);
          if Result.cx > Width then begin
            Result.cx := Width;
            Result.cy := Round(Result.cx / xyaspect);
          end;
        end;
      end
      else
{$ENDIF}
        Result := MkSize(Width, SrcHeight);
    end;
  end;

  procedure DrawImage { (p: PacBGInfo) };
  var
    l, t: integer;
    StretchSize, Size: TSize;
    TmpBmp, StretchSrc: TBitmap;

  begin
    Size := GetSrcSize(False);
    if Size.cx <> 0 then begin
      StretchSize := GetSrcSize(Stretch);
      TmpBmp := CreateBmp32(Size);
      TmpBmp.Transparent := Transparent;
      if (ImageIndex >= 0) and (Images <> nil) then begin
        // Get bitmap from imagelist
        TmpBmp.Width := Size.cx;
        TmpBmp.Height := Size.cy;
        TmpBmp.Canvas.Font.Color := Font.Color;
        if Images is TacImageList then
          TacImageList(Images).GetBitmap32(ImageIndex, TmpBmp)
        else
          if Images is TsVirtualImageList then
            TsVirtualImageList(Images).GetBitmap32(ImageIndex, TmpBmp)
          else
            Images.GetBitmap(ImageIndex, TmpBmp);

        if Stretch then begin
          StretchSrc := TmpBmp;
          TmpBmp := CreateBmp32(StretchSize);
          sGraphUtils.Stretch(StretchSrc, TmpBmp, StretchSize.cx, StretchSize.cy, ftMitchell);
          StretchSrc.Free;
        end;
      end
      else
        if not(Picture.Graphic is TBitmap) and not(Picture.Graphic is TPNGGraphic) (* {$IFDEF D2010}or not Picture.Graphic.SupportsPartialTransparency {$ENDIF} *) then begin
          TmpBmp.Width := StretchSize.cx;
          TmpBmp.Height := StretchSize.cy;
          BitBlt(TmpBmp.Canvas.Handle, 0, 0, Width, Height, FCommonData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);
          R := MkRect(TmpBmp);
          if Stretch then begin
            R.Right := min(Width, R.Left + StretchSize.cx);
            R.Bottom := min(Height, R.Top + StretchSize.cy);
          end;
          TmpBmp.Canvas.StretchDraw(R, Picture.Graphic);
          FillAlphaRect(TmpBmp, R, MaxByte);
        end
        else
          if not Picture.Bitmap.Empty then begin
            TmpBmp.Assign(Picture.Bitmap);
            if TmpBmp.PixelFormat <> pf32bit then begin
              TmpBmp.PixelFormat := pf32bit;
              if Transparent then begin
                C.C := TmpBmp.Canvas.Pixels[0, Height - 1];
                C.A := 0;
                FillAlphaRect(TmpBmp, MkRect(TmpBmp), MaxByte, C.C);
              end
              else
                FillAlphaRect(TmpBmp, MkRect(TmpBmp), MaxByte);
            end
            else
              if (DefaultManager = nil) or DefaultManager.Options.CheckEmptyAlpha then
                CheckEmptyAChannel(Picture.Bitmap);

            if Stretch then begin
              StretchSrc := TmpBmp;
              TmpBmp := CreateBmp32(StretchSize);
              sGraphUtils.Stretch(StretchSrc, TmpBmp, StretchSize.cx, StretchSize.cy, ftMitchell);
              StretchSrc.Free;
            end;
          end;

      if Stretch {$IFDEF DELPHI6UP} and not Proportional{$ENDIF} or not Center then begin
        l := 0;
        t := 0;
      end
      else begin
        l := (Width - TmpBmp.Width) div 2;
        t := (Height - TmpBmp.Height - integer(FUseFullSize) * (TmpBmp.Height div 2)) div 2;
      end;
      // UpdateCorners
      if MakeTranspCorners(TmpBmp, MkRect(TmpBmp), SkinData.CustomBorder.RadiusTopLeft, SkinData.CustomBorder.RadiusTopRight, SkinData.CustomBorder.RadiusBottomLeft,
        SkinData.CustomBorder.RadiusBottomRight) then begin
        {
          if p <> nil then
          case p.BgType of
          btFill: FillRect32(FCommonData.FCacheBmp, MkRect(FCommonData.FCacheBmp), p.Color, MaxByte);
          btCache: BitBlt(FCommonData.FCacheBmp.Canvas.Handle, 0, 0, FCommonData.FCacheBmp.Width, FCommonData.FCacheBmp.Height,
          p.Bmp.Canvas.Handle, p.Offset.X + Left, p.Offset.Y + Top, SRCCOPY);
          end
        }
      end;
      CopyBmp32(Rect(l, t, l + TmpBmp.Width, t + TmpBmp.Height), MkRect(TmpBmp), FCommonData.FCacheBmp, TmpBmp, CI, False, iff(Grayed, $FFFFFF, clNone), Blend, Reflected);
      TmpBmp.Free;
    end;

  end;


begin
  Result := true;

  InitCacheBmp(SkinData);
  BGInfo.Bmp := nil;
  BGInfo.BgType := btUnknown;
  BGInfo.PleaseDraw := False;
  BGInfo.FillRect := MkRect;
  if not((SkinData.SkinManager <> nil) and SkinData.SkinManager.Options.StdImgTransparency) then
    SendAMessage(Parent, AC_GETBG, LPARAM(@BGInfo));

  if (BGInfo.BgType = btUnknown) or (SkinData.SkinManager <> nil) and SkinData.SkinManager.Options.StdImgTransparency then begin
    BitBlt(FCommonData.FCacheBmp.Canvas.Handle, 0, 0, Width, Height, DC, 0, 0, SRCCOPY);
    CI := BGInfoToCI(@BGInfo);
    DrawImage;
    FCommonData.BGChanged := False;
  end
  else
    if BGInfo.BgType = btNotReady then begin
      FCommonData.FUpdating := true;
      Result := False;
    end
    else begin
      CI := BGInfoToCI(@BGInfo);
      if not CI.Ready or not CI.Bmp.Empty then begin
        if Transparent or (GetSrcSize(False).cx = 0) or (SkinData.CustomBorder.RadiusTopLeft > 0) or (SkinData.CustomBorder.RadiusTopRight > 0) or (SkinData.CustomBorder.RadiusBottomLeft > 0) or
          (SkinData.CustomBorder.RadiusBottomRight > 0) then
          if not CI.Ready and (CI.FillColor = sFuchsia.C) then
            BitBlt(FCommonData.FCacheBmp.Canvas.Handle, 0, 0, Width, Height, DC, 0, 0, SRCCOPY)
          else
            if not CI.Ready or (CI.Bmp = nil) then
              FillDC(FCommonData.FCacheBmp.Canvas.Handle, MkRect(Self), CI.FillColor)
            else
              BitBlt(FCommonData.FCacheBmp.Canvas.Handle, 0, 0, Width, Height, CI.Bmp.Canvas.Handle, CI.X + Left, CI.Y + Top, SRCCOPY);

        if FCommonData.SkinSection <> '' then
          PaintItem(FCommonData, CI, true, 0, MkRect(Self), MkPoint(Left, Top), FCommonData.FCacheBmp, true, 0, 0);

        DrawImage;
        FCommonData.BGChanged := False;
      end;
    end;
end;


procedure TsCustomImage.WndProc(var Message: TMessage);
begin
{$IFDEF LOGGED}
  AddToLog(message);
{$ENDIF}
  if FCommonData <> nil then
    case message.Msg of
      SM_ALPHACMD: begin
          case message.WParamHi of
            AC_GETDEFSECTION:
              message.Result := 2 + 1;

            AC_GETDEFINDEX:
              if FCommonData.SkinManager <> nil then
                message.Result := FCommonData.SkinManager.SkinCommonInfo.Sections[ssTransparent] + 1;

            AC_ISOPAQUE:
              if (SkinData.SkinManager <> nil) and SkinData.SkinManager.IsActive then
                message.Result := 1
              else
                message.Result := integer(not(csOpaque in ControlStyle))

          else
            CommonMessage(message, SkinData);
          end;
          Exit;
        end;

      CM_HITTEST:
        if FTransparentForMouse then begin
          message.Result := HTNOWHERE;
          Exit;
        end;

      WM_ERASEBKGND:
        Exit;

      WM_WINDOWPOSCHANGED, WM_SIZE:
        if Visible then
          FCommonData.BGChanged := true;

      CM_VISIBLECHANGED: begin
          FCommonData.BGChanged := true;
          SkinData.FMouseAbove := False
        end;
    end;

  inherited;
{$IFNDEF D2010}
  case message.Msg of
    CM_MOUSEENTER:
      if Assigned(FOnMouseEnter) then
        FOnMouseEnter(Self);
    CM_MOUSELEAVE:
      if Assigned(FOnMouseLeave) then
        FOnMouseLeave(Self);
  end;
{$ENDIF}
end;


// SSV
function IsJPGImage(FDataStream: TStream): boolean;
var
  buf: array [0 .. 4] of ansichar;
begin
  Result := False;
  try
    if Assigned(FDataStream) and (FDataStream.Size > 11) then begin
      FDataStream.Position := 6;
      FDataStream.Read(buf, 4);
      buf[4] := #0;
{$IFDEF DELPHI_XE4}
      Result := AnsiStrings.Strpas(buf) = 'JFIF';
{$ELSE}
      Result := Strpas(buf) = 'JFIF';
{$ENDIF}
      FDataStream.Position := 0;
    end;
  except
  end;
end;


function IsPNGImage(FDataStream: TStream): boolean;
var
  buf: array [0 .. 4] of ansichar;
begin
  Result := False;
  try
    if Assigned(FDataStream) and (FDataStream.Size > 5) then begin
      FDataStream.Position := 1;
      FDataStream.Read(buf, 4);
      buf[3] := #0;
      buf[4] := #0;
{$IFDEF DELPHI_XE4}
      Result := AnsiStrings.Strpas(buf) = 'PNG';
{$ELSE}
      Result := Strpas(buf) = 'PNG';
{$ENDIF}
      FDataStream.Position := 0;
    end;
  except
  end;
end;


{$IFDEF D2007}
function IsGIFImage(FDataStream: TStream): boolean;
var
  buf: array [0 .. 4] of ansichar;
begin
  Result := False;
  try
    if (FDataStream.Size > 4) then begin
      FDataStream.Position := 0;
      FDataStream.Read(buf, 4);
      buf[4] := #0;
{$IFDEF DELPHI_XE4}
      Result := AnsiStrings.Strpas(buf) = 'GIF8';
{$ELSE}
      Result := Strpas(buf) = 'GIF8';
{$ENDIF}
      FDataStream.Position := 0;
    end;
  except
  end;
end;
{$ENDIF}

function IsBMPImage(FDataStream: TStream): boolean;
var
  buf: array [0 .. 4] of ansichar;
begin
  Result := False;
  try
    if (FDataStream.Size > 4) then begin
      FDataStream.Position := 0;
      FDataStream.Read(buf, 2);
      buf[2] := #0;
{$IFDEF DELPHI_XE4}
      Result := AnsiStrings.Strpas(buf) = 'BM';
{$ELSE}
      Result := Strpas(buf) = 'BM';
{$ENDIF}
      FDataStream.Position := 0;
    end;
  except
  end;
end;


function TsCustomImage.LoadFromStream(aStream: TStream): boolean;
var
  aGraphic: TGraphic;
begin
  Result := False;
  if aStream <> nil then begin
    if IsPNGImage(aStream) then
      aGraphic := TPNGGraphic.Create
//      aGraphic := TPngImage.Create
    else
{$IFDEF D2007}
      if IsGIFImage(aStream) then
        aGraphic := TGifImage.Create
      else
{$ENDIF}
        if IsJPGImage(aStream) then
          aGraphic := TJpegImage.Create
        else
          if IsBMPImage(aStream) then
            aGraphic := TBitmap.Create
          else
            Exit;

    if Assigned(aGraphic) then begin
      aStream.Position := 0;
      aGraphic.LoadFromStream(aStream);
      Picture.Graphic := nil;
      Picture.Graphic := aGraphic;

      // Assign to Bitmap if it's one
      if (Lowercase(Picture.Graphic.ClassName) = 'tbitmap') then begin
        if Picture.Bitmap = nil then
          Picture.Bitmap := TBitmap.Create;
        Picture.Bitmap.Width := aGraphic.Width;
        Picture.Bitmap.Height := aGraphic.Height;
        Picture.Bitmap.Canvas.Draw(0, 0, aGraphic);
      end;
    end;
  end;
end;


function TsCustomImage.LoadFromURL(aURL: string; CanCache: boolean = true): boolean;
{$IFDEF USE_INET}
var
  RBSIZE: dword;
  httpstatus, httpsize, err: integer;
  dwIdx: dword;
  dwBufSize: dword;
  len: dword;
  cbuf: array [0 .. 255] of char;
  rb: array [0 .. 4095] of byte;
  fISession: Pointer;
  fIHttp: Pointer;
  fOptions: Cardinal;
  Error: boolean;
  stream: TMemoryStream;
{$ENDIF}
begin
{$IFDEF USE_INET}
  Result := False;
  if (Trim(aURL) <> '') and not FIsDownloading then begin
    stream := nil;
    Error := true;
    FIsDownloading := true;
    FAbortDownload := False;
    try
      fISession := InternetOpen('acImage', INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
      if (fISession = nil) then
        Exit; // Cannot open internet session

      if (not CanCache) then
        fOptions := INTERNET_FLAG_PRAGMA_NOCACHE or INTERNET_FLAG_NO_CACHE_WRITE or INTERNET_FLAG_RELOAD
      else
        fOptions := INTERNET_FLAG_RESYNCHRONIZE;

      fIHttp := InternetOpenURL(fISession, pchar(aURL), nil, 0, fOptions, 0);

      if (fIHttp = nil) then begin // Cannot open http connection
        InternetCloseHandle(fISession);
        Exit;
      end;

      // Query last modifified
      {
        dwBufSize := sizeof(stime);
        dwidx:=0;
        if httpQueryInfo(fIHttp, HTTP_QUERY_LAST_MODIFIED or HTTP_QUERY_FLAG_SYSTEMTIME, @stime, dwBufSize,dwIdx) then begin
        LastModified := SystemTimeToDateTime( stime );
        end;
      }

      // Query Status. Is it possible?
      dwBufSize := SizeOf(cbuf);
      dwIdx:=0;
      HttpQueryInfo(fIHttp, HTTP_QUERY_STATUS_CODE, @cbuf, dwBufSize, dwIdx);

      val(cbuf, httpstatus, err);
      if (httpstatus <> 200) or (err <> 0) then begin // Cannot open URL
        InternetCloseHandle(fISession);
        InternetCloseHandle(fIHttp);
        if Assigned(FDownloadErrorEvent) then
          FDownloadErrorEvent(Self, IntToStr(httpstatus));

        Exit;
      end;

      // Query Content-Type
      dwBufSize:=SizeOf(cbuf);
      dwIdx:=0;
      HttpQueryInfo(fIHttp, HTTP_QUERY_CONTENT_TYPE, @cbuf, dwBufSize, dwIdx);

      if (pos('IMAGE', uppercase(Strpas(cbuf))) = 0) then begin // Resource is not of image type
        InternetCloseHandle(fISession);
        InternetCloseHandle(fIHttp);
        if Assigned(FDownloadErrorEvent) then
          FDownloadErrorEvent(Self, 'Not an image');

        Exit;
      end;

      dwBufSize := SizeOf(cbuf);
      dwIdx := 0;
      HttpQueryInfo(fIHttp, HTTP_QUERY_CONTENT_LENGTH, @cbuf, dwBufSize, dwIdx);
      val(cbuf, httpsize, err);

      if ((httpsize = 0) or (err <> 0)) then begin // Image size is 0
        InternetCloseHandle(fISession);
        InternetCloseHandle(fIHttp);
        if Assigned(FDownloadErrorEvent) then
          FDownloadErrorEvent(Self, 'Image too small');

        Exit;
      end;

      len := 4096;
      RBSIZE := 4096;
      stream := TMemoryStream.Create;
      while (len > 0) do begin
        InternetReadFile(fIHttp, @rb, RBSIZE, len);
        if (len > 0) then begin
          stream.WriteBuffer(rb, len);
          if Assigned(FOnDownloadCancel) then
            FOnDownloadCancel(Self, FAbortDownload);

          if FAbortDownload then
            Break;
        end;
      end;

      if (not FAbortDownload) then
        Error := False;

      InternetCloseHandle(fIHttp);
      InternetCloseHandle(fISession);

      if FAbortDownload then
        stream.Clear
      else
        Result := LoadFromStream(stream);
    finally
      if Error then
        Result := False;

      FIsDownloading := False;
      if Assigned(stream) then
        FreeAndNil(stream);

      if (not Error) and Assigned(FOnDownloadComplete) then
        FOnDownloadComplete(Self);
    end;
  end;
{$ELSE}
  Result := False;
  if Assigned(FDownloadErrorEvent) then
    FDownloadErrorEvent(Self, 'The `USE_INET` key should be uncommented in the sDefs.inc file.'{$IFDEF BCB} + ' The `wininet.lib` library should be added in C++ Builder projects.' {$ENDIF});
{$ENDIF}
end;


constructor TImageDownloadThread.Create(URL: string; Cached: boolean; item: TsCustomImage);
begin
  inherited Create(False);
  FURL := URL;
  FPicture := item;
  FCanCache := Cached;
  FreeOnTerminate := true;
end;


procedure TImageDownloadThread.Execute;
begin
  if FPicture.LoadFromURL(FURL, FCanCache) then
    Synchronize(DownloadCompleteThread); // Prevent Canvas Deadloop
end;


procedure TImageDownloadThread.DownloadCompleteThread;
begin
  if Assigned(FPicture.FOnDownloadCompleteThread) then
    FPicture.FOnDownloadCompleteThread(Self);
end;


function TsCustomImage.LoadFromURLThread(aURL: string; CanCache: boolean = true): boolean;
begin
  if (aURL <> '') and not FIsDownloading then begin
    TImageDownloadThread.Create(aURL, CanCache, Self);
    Result := true;
  end
  else
    Result := False;
end;

// ...


procedure TsCustomImage.acWM_Paint(var Message: TWMPaint);
var
  SavedDC: HDC;
begin
  if not InUpdating(FCommonData) and not(Empty and (SkinData.SkinIndex < 0)) then begin
    SavedDC := SaveDC(message.DC);
    try
      if FCommonData.BGChanged or ImageChanged then begin
        // FCommonData.UpdateIndexes;
        PrepareCache(message.DC);
      end;
      BitBlt(message.DC, 0, 0, Width, Height, FCommonData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);
    finally
      RestoreDC(message.DC, SavedDC);
    end;
  end
  else
    inherited;
end;


function TsCustomImage.Empty: boolean;
begin
  if (FImages <> nil) and IsValidIndex(FImageIndex, GetImageCount(FImages)) then
    Result := False
  else
    if Picture.Graphic <> nil then
      Result := Picture.Graphic.Empty
    else
      if Picture.Bitmap <> nil then
        Result := Picture.Bitmap.Empty
      else
        Result := true;
end;


function TsCustomImage.GetCanvas: TCanvas;
begin
  if (Picture.Graphic = nil) or not(Picture.Graphic is TBitmap) then
    Result := inherited Canvas
  else begin
    if ((Images = nil) or (ImageIndex < 0)) and TBitmap(Picture.Graphic).Empty then begin
      TBitmap(Picture.Graphic).Width := Width;
      TBitmap(Picture.Graphic).Height := Height;
    end;
    Result := TBitmap(Picture.Graphic).Canvas;
  end;
end;


procedure TsCustomImage.UpdateImage;
begin
  if [csLoading, csDestroying] * ComponentState = [] then begin
    SkinData.BGChanged := true;
    Repaint;
  end;
end;


function TsCustomImage.CanAutoSize(var NewWidth, NewHeight: integer): boolean;
var
  w, h: integer;
begin
  Result := true;
  if not(csDesigning in ComponentState) or not Empty then begin
    if (Images <> nil) and IsValidIndex(ImageIndex, GetImageCount(Images)) then begin
      w := GetImageWidth(Images, ImageIndex, GetPPI(SkinData));
      h := GetImageHeight(Images, ImageIndex, GetPPI(SkinData));
    end
    else begin
      w := Picture.Width;
      h := Picture.Height;
    end;
    if Align in [alNone, alLeft, alRight] then
      NewWidth := w;

    if Align in [alNone, alTop, alBottom] then begin
      NewHeight := h;
      if FUseFullSize and FReflected then
        inc(NewHeight, NewHeight div 2)
    end;
  end
  else
    inherited CanAutoSize(NewWidth, NewHeight);
end;


procedure TsCustomImage.Loaded;
begin
  inherited;
  FCommonData.Loaded;
end;


procedure TsCustomImage.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
    if AComponent = Images then
      Images := nil;
end;

end.

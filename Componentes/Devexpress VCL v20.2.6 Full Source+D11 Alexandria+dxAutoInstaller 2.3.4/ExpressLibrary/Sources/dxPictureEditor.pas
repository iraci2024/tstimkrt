{ ******************************************************************** }
{ }
{ Developer Express Visual Component Library }
{ Express Cross Platform Library classes }
{ }
{ Copyright (c) 2000-2021 Developer Express Inc. }
{ ALL RIGHTS RESERVED }
{ }
{ The entire contents of this file is protected by U.S. and }
{ International Copyright Laws. Unauthorized reproduction, }
{ reverse-engineering, and distribution of all or any portion of }
{ the code contained in this file is strictly prohibited and may }
{ result in severe civil and criminal penalties and will be }
{ prosecuted to the maximum extent possible under the law. }
{ }
{ RESTRICTIONS }
{ }
{ THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES }
{ (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE }
{ SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS }
{ LICENSED TO DISTRIBUTE THE EXPRESSCROSSPLATFORMLIBRARY AND ALL }
{ ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM }
{ ONLY. }
{ }
{ THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED }
{ FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE }
{ COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE }
{ AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT }
{ AND PERMISSION FROM DEVELOPER EXPRESS INC. }
{ }
{ CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON }
{ ADDITIONAL RESTRICTIONS. }
{ }
{ ******************************************************************** }

unit dxPictureEditor;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Dialogs, Menus, ImgList, Forms, Classes, Controls, ActnList, ExtCtrls,
  StdCtrls, Graphics,
  dxCore, cxGraphics, cxControls, cxContainer, cxEdit, cxClasses,
  cxLookAndFeels, cxLookAndFeelPainters, cxPC,
  cxTextEdit, cxMaskEdit, cxButtonEdit, cxButtons, ExtDlgs, dxGDIPlusClasses,
  cxImageList;

type

  { TdxPictureEditorDialog }

  TdxPictureEditorDialogClass = class of TdxPictureEditorDialog;

  TdxPictureEditorDialog = class(TForm)
    actClear: TAction;
    ActionList: TActionList;
    actLoad: TAction;
    actSave: TAction;
    btnCancel: TcxButton;
    btnClear: TcxButton;
    btnImport: TcxButton;
    btnLoad: TcxButton;
    btnOk: TcxButton;
    btnSave: TcxButton;
    EditStyleController: TcxEditStyleController;
    ilImages: TcxImageList;
    ImagePaintBox: TPaintBox;
    miClear: TMenuItem;
    miImageCopy: TMenuItem;
    miImageCut: TMenuItem;
    miImagePaste: TMenuItem;
    miLine2: TMenuItem;
    miLoad: TMenuItem;
    miSave: TMenuItem;
    OpenDialog: TOpenPictureDialog;
    pcMain: TcxPageControl;
    pmImport: TPopupMenu;
    pmPictureEditor: TPopupMenu;
    pnlIconLibraryButtons: TPanel;
    pnlImagePaintBox: TPanel;
    pnlPictureEditorButtons: TPanel;
    SaveDialog: TSavePictureDialog;
    tsPictureEditor: TcxTabSheet;

    procedure actClearExecute(Sender: TObject);
    procedure actClearUpdate(Sender: TObject);
    procedure actLoadExecute(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure actSaveUpdate(Sender: TObject);
    procedure btnImportClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ImagePaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ImagePaintBoxPaint(Sender: TObject);
    procedure miImageCopyClick(Sender: TObject);
    procedure miImagePasteClick(Sender: TObject);
    procedure pmPictureEditorPopup(Sender: TObject);
  strict private
    FGraphicClass: TGraphicClass;
    FImportList: TStrings;
    FPreferSmartImage: Boolean;

    procedure ImportPictureClick(Sender: TObject);
    procedure PictureChangeHandler(Sender: TObject);

    function BuildOpenDialogFilter: string;
    function IsBitmap(const AFileName: string): Boolean;
    function TryLoadAsSmartImage(const AFileName: string): Boolean;
    function TryLoadAsStandardGraphic(AImage: TdxSmartImage): Boolean;
    procedure SetGraphicClass(AValue: TGraphicClass);
    procedure SetImportList(AValue: TStrings);
  protected
    FIsSmartImageRegistered: Boolean;
    FPicture: TPicture;

    procedure Initialize; virtual;
    procedure LoadSettings(APicture: TPicture); virtual;
    procedure SaveSettings(APicture: TPicture); virtual;

    procedure DrawPicture(ACanvas: TCanvas; R: TRect);
    procedure LoadFromFile(const AFileName: string);
    procedure SetImage(AImage: TdxSmartImage);
  public
    function Execute(APicture: TPicture): Boolean;
    //
    property GraphicClass: TGraphicClass read FGraphicClass
      write SetGraphicClass;
    property ImportList: TStrings read FImportList write SetImportList;
    property PreferSmartImage: Boolean read FPreferSmartImage
      write FPreferSmartImage;
  end;

var
  dxPictureEditorDialogClass: TdxPictureEditorDialogClass =
    TdxPictureEditorDialog; // for internal use

function dxExecutePictureEditor(APicture: TPicture;
  AGraphicClass: TGraphicClass = nil; AImportList: TStrings = nil;
  APreferSmartImage: Boolean = True): Boolean;

implementation

uses
  SysUtils, Clipbrd, cxGeometry, dxSmartImage, Consts, dxStringHelper;

{$R *.dfm}

function dxExecutePictureEditor(APicture: TPicture;
  AGraphicClass: TGraphicClass = nil; AImportList: TStrings = nil;
  APreferSmartImage: Boolean = True): Boolean;
var
  ADialog: TdxPictureEditorDialog;
begin
  ADialog := dxPictureEditorDialogClass.Create(nil);
  try
    ADialog.PreferSmartImage := APreferSmartImage;
    ADialog.ImportList := AImportList;
    ADialog.GraphicClass := AGraphicClass;
    Result := ADialog.Execute(APicture);
  finally
    ADialog.Free;
  end;
end;

{ TdxPictureEditorDialog }

function TdxPictureEditorDialog.Execute(APicture: TPicture): Boolean;
begin
  Initialize;
  LoadSettings(APicture);
  Result := ShowModal = mrOk;
  if Result then
    SaveSettings(APicture);
end;

procedure TdxPictureEditorDialog.Initialize;
begin
  PreferSmartImage := PreferSmartImage or GraphicClass.InheritsFrom
    (TdxSmartImage);
  FIsSmartImageRegistered := GraphicExtension(TdxSmartImage) <> '';
  OpenDialog.DefaultExt := cxGraphicExtension(GraphicClass);
  OpenDialog.Filter := BuildOpenDialogFilter;

  btnImport.Visible := (FImportList <> nil) and (FImportList.Count > 0);
  btnImport.Enabled := btnImport.Visible;

end;

procedure TdxPictureEditorDialog.LoadSettings(APicture: TPicture);
begin
  FPicture.Assign(APicture);
end;

procedure TdxPictureEditorDialog.SaveSettings(APicture: TPicture);
begin
  APicture.Assign(FPicture);
end;

procedure TdxPictureEditorDialog.DrawPicture(ACanvas: TCanvas; R: TRect);
begin
  cxDrawTransparencyCheckerboard(ACanvas.Handle, R);
  ACanvas.Brush.Color := Color;
  if (FPicture.Width > 0) and (FPicture.Height > 0) then
  begin
    cxPaintCanvas.BeginPaint(ACanvas);
    try
      cxDrawPicture(cxPaintCanvas, R, FPicture, ifmFit);
    finally
      cxPaintCanvas.EndPaint;
    end;
  end
  else
  begin
    R := cxRectCenter(R, Size(FPicture.Width, FPicture.Height));
    ACanvas.Brush.Style := bsClear;
    ACanvas.TextOut(R.Left, R.Top, 'None');
  end;
end;

procedure TdxPictureEditorDialog.LoadFromFile(const AFileName: string);
begin
  if PreferSmartImage then
  begin
    if not TryLoadAsSmartImage(AFileName) then
      FPicture.LoadFromFile(AFileName);
  end
  else
    try
      FPicture.LoadFromFile(AFileName);
    except
      if FIsSmartImageRegistered or not TryLoadAsSmartImage(AFileName) then
        raise;
    end;
end;

procedure TdxPictureEditorDialog.SetImage(AImage: TdxSmartImage);
var
  AGraphic: TGraphic;
begin
  if PreferSmartImage or not TryLoadAsStandardGraphic(AImage) then
  begin
    if AImage.ClassType <> TdxSmartImage then
    begin
      AGraphic := TdxSmartImage.Create;
      try
        AGraphic.Assign(AImage);
        FPicture.Graphic := AGraphic;
      finally
        AGraphic.Free;
      end;
    end
    else
      FPicture.Graphic := AImage;
  end;
end;

function TdxPictureEditorDialog.BuildOpenDialogFilter: string;

  function GetStrings(const S: string; ADelimiter: Char): TStrings;
  begin
    Result := TStringList.Create;
    Result.Delimiter := ADelimiter;
    ExtractStrings([ADelimiter], [], PChar(S), Result);
  end;

  function StringsToString(AStrings: TStrings): string;
  var
    ABuilder: TStringBuilder;
    I: Integer;
  begin
    ABuilder := TdxStringBuilderManager.Get;
    try
      for I := 0 to AStrings.Count - 1 do
      begin
        if I > 0 then
          ABuilder.Append(AStrings.Delimiter);
        ABuilder.Append(AStrings[I]);
      end;
      Result := ABuilder.ToString;
    finally
      TdxStringBuilderManager.Release(ABuilder);
    end;
  end;

  procedure Merge(AFilter, AExtensions: TStrings;
    ACodec: TdxSmartImageCodecClass);
  var
    ACodecExtensions: TStrings;
    AExtension: string;
    I: Integer;
  begin
    if ACodec.Description <> '' then
    begin
      AFilter.Insert(2, ACodec.Extensions);
      AFilter.Insert(2, ACodec.Description + ' (' + ACodec.Extensions + ')');

      ACodecExtensions := GetStrings(ACodec.Extensions, ';');
      try
        for I := 0 to ACodecExtensions.Count - 1 do
        begin
          AExtension := ACodecExtensions[I];
          if (AExtension <> '') and (AExtensions.IndexOf(AExtension) < 0) then
            AExtensions.Add(AExtension);
        end;
      finally
        ACodecExtensions.Free;
      end;
    end;
  end;

var
  AExtensions: TStrings;
  AFilter: TStrings;
  I: Integer;
begin
  Result := cxGraphicFilter(GraphicClass);
  if not FIsSmartImageRegistered and (GraphicClass <> TdxSmartImage) then
  begin
    AFilter := GetStrings(Result, '|');
    try
      AExtensions := GetStrings(cxGraphicFileMask(GraphicClass), ';');
      try
        for I := 0 to TdxSmartImageCodecsRepository.Count - 1 do
          Merge(AFilter, AExtensions, TdxSmartImageCodecsRepository.Items[I]);
        AFilter[0] := sAllFilter + ' (' + AExtensions.DelimitedText + ')';
        AFilter[1] := StringsToString(AExtensions);
      finally
        AExtensions.Free;
      end;
      Result := StringsToString(AFilter);
    finally
      AFilter.Free;
    end;
  end;
end;

function TdxPictureEditorDialog.IsBitmap(const AFileName: string): Boolean;
begin
  Result := SameText(ExtractFileExt(AFileName), '.bmp');
end;

function TdxPictureEditorDialog.TryLoadAsSmartImage(const AFileName
  : string): Boolean;

  procedure SetSmartImage(AImage: TdxSmartImage);
  begin
    try
      AImage.HandleNeeded;
      FPicture.Graphic := AImage;
    finally
      AImage.Free;
    end;
  end;

var
  ABitmap: TBitmap;
begin
  if (GraphicClass = TBitmap) and IsBitmap(AFileName) then
    Exit(False);

  Result := TdxSmartImageCodecsRepository.CanLoad(AFileName);
  if Result then
  begin
    if IsBitmap(AFileName) then
    begin
      ABitmap := TBitmap.Create;
      try
        ABitmap.LoadFromFile(AFileName);
        case ABitmap.PixelFormat of
          pf8bit .. pf16bit:
            ABitmap.Transparent := True;
          pf24bit:
            begin
              ABitmap.TransparentColor := clFuchsia;
              ABitmap.Transparent := True;
            end;
        end;
        SetSmartImage(TdxSmartImage.CreateFromBitmap(ABitmap));
      finally
        ABitmap.Free;
      end;
    end
    else
      SetSmartImage(TdxSmartImage.CreateFromFile(AFileName));
  end;
end;

function TdxPictureEditorDialog.TryLoadAsStandardGraphic
  (AImage: TdxSmartImage): Boolean;
var
  AExt: string;
  ATempFileName: string;
begin
  Result := False;
  if FIsSmartImageRegistered or (AImage.ImageDataFormat = dxImageUnknown) then
    Exit;

  AExt := '.' + cxGraphicExtension(AImage);
  if Pos('*' + LowerCase(AExt), LowerCase(cxGraphicFileMask(TGraphic))) > 0 then
    try
      ATempFileName := dxCreateTempFile(AExt);
      try
        AImage.SaveToFile(ATempFileName);
        FPicture.LoadFromFile(ATempFileName);
        Result := FPicture.Graphic <> nil;
      finally
        DeleteFile(ATempFileName);
      end;
    except
      Result := False;
    end;
end;

procedure TdxPictureEditorDialog.SetGraphicClass(AValue: TGraphicClass);
begin
  if AValue <> nil then
    FGraphicClass := AValue
  else
    FGraphicClass := TGraphic;
end;

procedure TdxPictureEditorDialog.SetImportList(AValue: TStrings);
var
  I: Integer;
  AMenuItem: TMenuItem;
begin
  if AValue <> nil then
    FImportList.Assign(AValue)
  else
    FImportList.Clear;

  pmImport.Items.Clear;
  for I := 0 to FImportList.Count - 1 do
  begin
    AMenuItem := TMenuItem.Create(Self);
    AMenuItem.Caption := FImportList[I];
    AMenuItem.Tag := I;
    AMenuItem.OnClick := ImportPictureClick;
    pmImport.Items.Add(AMenuItem);
  end;
end;

procedure TdxPictureEditorDialog.ImagePaintBoxMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  // to make shortcuts work
  pnlImagePaintBox.SetFocus;
end;

procedure TdxPictureEditorDialog.ImagePaintBoxPaint(Sender: TObject);
begin
  DrawPicture(ImagePaintBox.Canvas, ImagePaintBox.ClientRect);
end;

procedure TdxPictureEditorDialog.ImportPictureClick(Sender: TObject);
begin
  FPicture.Assign(TPersistent(FImportList.Objects[TMenuItem(Sender).Tag]));
end;

procedure TdxPictureEditorDialog.miImageCopyClick(Sender: TObject);
begin
  if IsPictureAssigned(FPicture) then
  begin
    Clipboard.Assign(FPicture);
    if TComponent(Sender).Tag = 1 then
      btnClear.Click;
  end;
end;

procedure TdxPictureEditorDialog.miImagePasteClick(Sender: TObject);
var
  AImage: TdxSmartImage;
begin
  if TdxSmartImage.HasClipboardFormat then
  begin
    AImage := TdxSmartImage.Create;
    try
      AImage.PasteFromClipboard;
      SetImage(AImage);
    finally
      AImage.Free;
    end;
  end;
end;

procedure TdxPictureEditorDialog.PictureChangeHandler(Sender: TObject);
begin
  ImagePaintBox.Invalidate;
end;

procedure TdxPictureEditorDialog.pmPictureEditorPopup(Sender: TObject);
begin
  miImageCut.Enabled := IsPictureAssigned(FPicture);
  miImageCopy.Enabled := miImageCut.Enabled;
  miImagePaste.Enabled := TdxSmartImage.HasClipboardFormat;
end;

procedure TdxPictureEditorDialog.actClearExecute(Sender: TObject);
begin
  FPicture.Graphic := nil;
end;

procedure TdxPictureEditorDialog.actClearUpdate(Sender: TObject);
begin
  actClear.Enabled := IsPictureAssigned(FPicture);
end;

procedure TdxPictureEditorDialog.actLoadExecute(Sender: TObject);
begin
  if OpenDialog.Execute(Handle) then
    LoadFromFile(OpenDialog.Filename);
end;

procedure TdxPictureEditorDialog.actSaveExecute(Sender: TObject);
begin
  SaveDialog.Filter := cxGraphicFilter(FPicture.Graphic, True);
  SaveDialog.DefaultExt := cxGraphicExtension(FPicture.Graphic);
  if SaveDialog.Execute(Handle) then
    FPicture.SaveToFile(SaveDialog.Filename);
end;

procedure TdxPictureEditorDialog.actSaveUpdate(Sender: TObject);
begin
  actSave.Enabled := IsPictureAssigned(FPicture);
end;

procedure TdxPictureEditorDialog.btnImportClick(Sender: TObject);
begin
  // do nothing
end;

procedure TdxPictureEditorDialog.FormCreate(Sender: TObject);
begin
  PopupMode := pmAuto;
  FPreferSmartImage := True;
  FGraphicClass := TGraphic;
  FPicture := TPicture.Create;
  FPicture.OnChange := PictureChangeHandler;
  FImportList := TStringList.Create;
end;

procedure TdxPictureEditorDialog.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FImportList);
  FreeAndNil(FPicture);
end;

end.

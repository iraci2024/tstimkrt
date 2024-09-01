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

unit VCL.TMSFNCBitmapEditor;

{$I VCL.TMSFNCDefines.inc}

{$IFDEF FMXLIB}
{$WARNINGS OFF}
{$HINTS OFF}
{$IF COMPILERVERSION > 30}
{$DEFINE DELPHIBERLIN}
{$IFEND}
{$HINTS ON}
{$WARNINGS ON}
{$ENDIF}

{$IFDEF VCLLIB}
{$DEFINE VCLWEBLIB}
{$ENDIF}
{$IFDEF CMNLIB}
{$DEFINE CMNWEBLIB}
{$ENDIF}
{$IFDEF WEBLIB}
{$DEFINE CMNWEBLIB}
{$DEFINE VCLWEBLIB}
{$ENDIF}

interface

uses
  Classes, VCL.TMSFNCCustomComponent, VCL.TMSFNCCustomControl, VCL.Controls,
  VCL.TMSFNCTypes, VCL.StdCtrls, VCL.ExtCtrls, VCL.TMSFNCBitmapContainer, VCL.Forms, VCL.TMSFNCGraphics, VCL.TMSFNCGraphicsTypes,
  VCL.TMSFNCEditorButton, VCL.TMSFNCEditorPanel
  {$IFDEF FMXLIB}
  ,FMX.Types, FMX.Memo, FMX.ListBox, FMX.ComboEdit
  {$ENDIF}
  {$IFNDEF LCLLIB}
  {$IFNDEF WEBLIB}
  ,UITypes
  {$ENDIF}
  ,Types
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 1; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // v1.0.0.0 : First Release
  // v1.0.1.0 : New : Updated design

resourcestring
  sTMSFNCBitmapEditorOK = 'OK';
  sTMSFNCBitmapEditorCancel = 'Cancel';
  sTMSFNCBitmapEditorOpen = '&Open';
  sTMSFNCBitmapEditorSave = '&Save';
  sTMSFNCBitmapEditorClear = '&Clear';

type
  TTMSFNCBitmapEditor = class;

  {$IFDEF FMXLIB}
  TTMSFNCBitmapEditorParent = TFmxObject;
  TTMSFNCBitmapEditorComboBox = class(TComboEdit);
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  TTMSFNCBitmapEditorParent = TWinControl;
  TTMSFNCBitmapEditorComboBox = class(TComboBox);
  {$ENDIF}

  {$IFNDEF LCLLIB}
  [ComponentPlatformsAttribute(TMSPlatformsDesktop)]
  {$ENDIF}
  TTMSFNCBitmapEditor = class(TTMSFNCCustomComponent)
  private
    frm: TTMSFNCCustomDesignerForm;
    FPanel, FTopPanel: TTMSFNCEditorPanel;
    FBitmap: TTMSFNCBitmap;
    FCopyBitmap: TTMSFNCBitmap;
    FButtonOk, FButtonCancel: TTMSFNCEditorButton;
    FButtonOpen, {$IFNDEF WEBLIB}FButtonSave, {$ENDIF}FButtonClear: TTMSFNCEditorButton;
    FImage: TTMSFNCCustomControl;
    FBitmapContainer: TTMSFNCBitmapContainer;
    procedure SetBitmap(const Value: TTMSFNCBitmap);
    procedure SetBitmapContainer(const Value: TTMSFNCBitmapContainer);
  protected
    function GetInstance: NativeUInt; override;
    procedure DoCopyBitmapChange(Sender: TObject);
    procedure RegisterRuntimeClasses; override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure BuildEditor(AParent: TTMSFNCBitmapEditorParent); virtual;
    procedure DoAfterDraw(Sender: TObject; AGraphics: TTMSFNCGraphics; ARect: TRectF); virtual;
    procedure DoOpenFile(Sender: TObject); virtual;
    {$IFNDEF FMXLIB}
    procedure DoFormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    {$ENDIF}
    {$IFDEF FMXLIB}
    procedure DoFormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    {$ENDIF}
    {$IFNDEF WEBLIB}
    procedure DoSaveFile(Sender: TObject); virtual;
    {$ELSE}
    procedure ApplyCSS(ACSS: string); virtual;
    procedure DoButtonOKClick(Sender: TObject); virtual;
    procedure DoButtonCancelClick(Sender: TObject); virtual;
    {$ENDIF}
    procedure DoClearFile(Sender: TObject); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$IFDEF WEBLIB}
    procedure Execute(AProc: TModalResultProc = nil);
    {$ELSE}
    function Execute: TModalResult;
    {$ENDIF}
    procedure Assign(Source: TPersistent); override;
  published
    property Bitmap: TTMSFNCBitmap read FBitmap write SetBitmap;
    property BitmapContainer: TTMSFNCBitmapContainer read FBitmapContainer write SetBitmapContainer;
  end;

  TTMSFNCBitmapEditorForm = class(TTMSFNCCustomDesignerForm)
  {$IFDEF WEBLIB}
  private
    FBitmapEditor: TTMSFNCBitmapEditor;
  protected
    procedure Loaded; override;
    procedure LoadDFMValues; override;
  public
    constructor CreateDialogNew(ABitmapEditor: TTMSFNCBitmapEditor; ACaption: string); reintroduce; virtual;
  {$ENDIF}
  end;

implementation

uses
  VCL.TMSFNCUtils, VCL.Graphics, SysUtils, VCL.Dialogs, VCL.TMSFNCEditorsTools
  {$IFDEF WEBLIB}
  ,WEBLib.WebTools
  {$ENDIF}
  {$IFNDEF FMXMOBILE}
  {$IFDEF DELPHIBERLIN}
  ,FMX.DialogService.Sync
  {$ENDIF}
  {$ENDIF}
  ;

{ TTMSFNCBitmapEditor }

function TranslateTextEx(AText: String): String;
begin
  {$IFDEF FMXLIB}
  Result := TranslateText(AText);
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  Result := AText;
  {$ENDIF}
end;

procedure TTMSFNCBitmapEditor.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  if (AOperation = opRemove) and (AComponent = FBitmapContainer) then
    FBitmapContainer := nil;
end;

procedure TTMSFNCBitmapEditor.RegisterRuntimeClasses;
begin
  inherited;
  RegisterClass(TTMSFNCBitmapEditor);
end;

procedure TTMSFNCBitmapEditor.Assign(Source: TPersistent);
begin
  if (Source is TTMSFNCBitmapEditor) then
    FBitmap := (Source as TTMSFNCBitmapEditor).Bitmap;
end;

procedure TTMSFNCBitmapEditor.BuildEditor(AParent: TTMSFNCBitmapEditorParent);
begin
  if Assigned(AParent) then
  begin
    FTopPanel := TTMSFNCEditorPanel.Create(AParent);
    {$IFDEF FMXLIB}
    FTopPanel.Align := TAlignLayout.Top;
    FTopPanel.PanelPosition := eppTop;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    FTopPanel.PanelPosition := eppCenter;
    FTopPanel.Align := alTop;
    FTopPanel.Top := 0;
    {$ENDIF}
    FTopPanel.Parent := AParent;

    SetEditorBackPanelAppearance(FTopPanel);

    FButtonClear := TTMSFNCEditorButton.Create(FTopPanel);
    FButtonClear.Parent := FTopPanel;
    FButtonClear.Text := TranslateTextEx(sTMSFNCBitmapEditorClear);
    {$IFDEF FMXLIB}
    FButtonClear.Align := TAlignLayout.Left;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    FButtonClear.Align := alLeft;
    FButtonClear.Left := 30;
    {$IFDEF VCLLIB}
    FButtonClear.AlignWithMargins := True;
    {$ENDIF}
    {$ENDIF}
    {$IFDEF LCLLIB}
    FButtonClear.BorderSpacing.Right := 5;
    FButtonClear.BorderSpacing.Top := 5;
    FButtonClear.BorderSpacing.Bottom := 5;
    FButtonClear.BorderSpacing.Left := 5;
    {$ENDIF}
    {$IFNDEF LCLLIB}
    FButtonClear.Margins.Right := 5;
    FButtonClear.Margins.Top := 5;
    FButtonClear.Margins.Bottom := 5;
    FButtonClear.Margins.Left := 5;
    {$ENDIF}
    FButtonClear.OnClick := DoClearFile;

    SetEditorTabButtonAppearance(FButtonClear);

    {$IFNDEF WEBLIB}
    FButtonSave := TTMSFNCEditorButton.Create(FTopPanel);
    FButtonSave.Parent := FTopPanel;
    FButtonSave.Text := TranslateTextEx(sTMSFNCBitmapEditorSave);
    {$IFDEF FMXLIB}
    FButtonSave.Align := TAlignLayout.Left;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    FButtonSave.Align := alLeft;
    FButtonSave.Left := 20;
    {$IFDEF VCLLIB}
    FButtonSave.AlignWithMargins := True;
    {$ENDIF}
    {$ENDIF}
    {$IFDEF LCLLIB}
    FButtonSave.BorderSpacing.Right := 0;
    FButtonSave.BorderSpacing.Top := 5;
    FButtonSave.BorderSpacing.Bottom := 5;
    FButtonSave.BorderSpacing.Left := 5;
    {$ENDIF}
    {$IFNDEF LCLLIB}
    FButtonSave.Margins.Right := 0;
    FButtonSave.Margins.Top := 5;
    FButtonSave.Margins.Bottom := 5;
    FButtonSave.Margins.Left := 5;
    {$ENDIF}
    FButtonSave.OnClick := DoSaveFile;
    SetEditorTabButtonAppearance(FButtonSave);
    {$ENDIF}

    FButtonOpen := TTMSFNCEditorButton.Create(FTopPanel);
    FButtonOpen.Parent := FTopPanel;
    FButtonOpen.Text := TranslateTextEx(sTMSFNCBitmapEditorOpen);
    {$IFDEF FMXLIB}
    FButtonOpen.Align := TAlignLayout.Left;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    FButtonOpen.Align := alLeft;
    FButtonOpen.Left := 10;
    {$IFDEF VCLLIB}
    FButtonOpen.AlignWithMargins := True;
    {$ENDIF}
    {$ENDIF}
    {$IFNDEF LCLLIB}
    FButtonOpen.Margins.Right := 0;
    FButtonOpen.Margins.Top := 5;
    FButtonOpen.Margins.Bottom := 5;
    FButtonOpen.Margins.Left := 5;
    {$ENDIF}
    {$IFDEF LCLLIB}
    FButtonOpen.BorderSpacing.Right := 0;
    FButtonOpen.BorderSpacing.Top := 5;
    FButtonOpen.BorderSpacing.Bottom := 5;
    FButtonOpen.BorderSpacing.Left := 5;
    {$ENDIF}
    FButtonOpen.OnClick := DoOpenFile;
    SetEditorTabButtonAppearance(FButtonOpen);

    FTopPanel.Height := 37;

    FImage := TTMSFNCCustomControl.Create(AParent);
    FImage.Parent := AParent;
    FImage.ControlAlignment := caClient;
    {$IFDEF WEBLIB}
    FImage.DisableBackground;
    {$ENDIF}
    FImage.OnClick := DoOpenFile;
    if IsLightTheme then
    begin
      FImage.Color := EDITORSUBBACKCOLORLIGHT;
    end
    else
    begin
      FImage.Color := EDITORSUBBACKCOLORDARK;
    end;
    FImage.DisableStroke;

    FImage.OnAfterDraw := DoAfterDraw;

    FPanel := TTMSFNCEditorPanel.Create(AParent);
    {$IFDEF FMXLIB}
    FPanel.Align := TAlignLayout.Bottom;
    FPanel.PanelPosition := eppBottom;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    FPanel.PanelPosition := eppCenter;
    FPanel.Align := alBottom;
    {$ENDIF}
    FPanel.Parent := AParent;

    FPanel.Height := 37;
    SetEditorBackPanelAppearance(FPanel);

    FButtonCancel := TTMSFNCEditorButton.Create(FPanel);
    FButtonCancel.ModalResult := mrCancel;
    FButtonCancel.Parent := FPanel;
    FButtonCancel.Text := TranslateTextEx(sTMSFNCBitmapEditorCancel);
    {$IFDEF FMXLIB}
    FButtonCancel.Align := TAlignLayout.Right;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    FButtonCancel.Align := alRight;
	FButtonCancel.Left := 99;
    {$IFDEF VCLWEBLIB}
    FButtonCancel.AlignWithMargins := True;
    {$ENDIF}
    {$ENDIF}
    {$IFDEF LCLLIB}
    FButtonCancel.BorderSpacing.Right := 5;
    FButtonCancel.BorderSpacing.Top := 5;
    FButtonCancel.BorderSpacing.Bottom := 5;
    FButtonCancel.BorderSpacing.Left := 5;
    {$ENDIF}
    {$IFNDEF LCLLIB}
    FButtonCancel.Margins.Right := 5;
    FButtonCancel.Margins.Top := 5;
    FButtonCancel.Margins.Bottom := 5;
    FButtonCancel.Margins.Left := 5;
    {$ENDIF}
    {$IFDEF WEBLIB}
    FButtonCancel.OnClick := DoButtonCancelClick;
    {$ENDIF}
    SetEditorCancelButtonAppearance(FButtonCancel);

    FButtonOK := TTMSFNCEditorButton.Create(FPanel);
    FButtonOK.ModalResult := mrOk;
    FButtonOK.Parent := FPanel;
    FButtonOK.Text := TranslateTextEx(sTMSFNCBitmapEditorOK);
    {$IFDEF FMXLIB}
    FButtonOk.Align := TAlignLayout.Right;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    FButtonOk.Align := alRight;
  	FButtonOk.Left := 99;
    {$IFDEF VCLWEBLIB}
    FButtonOK.AlignWithMargins := True;
    {$ENDIF}
    {$ENDIF}
    {$IFNDEF LCLLIB}
    FButtonOK.Margins.Right := 0;
    FButtonOK.Margins.Top := 5;
    FButtonOK.Margins.Bottom := 5;
    FButtonOK.Margins.Left := 5;
    {$ENDIF}
    {$IFDEF LCLLIB}
    FButtonOK.BorderSpacing.Right := 0;
    FButtonOK.BorderSpacing.Top := 5;
    FButtonOK.BorderSpacing.Bottom := 5;
    FButtonOK.BorderSpacing.Left := 5;
    {$ENDIF}
    {$IFDEF WEBLIB}
    FButtonOK.OnClick := DoButtonOKClick;
    {$ENDIF}
    SetEditorOKButtonAppearance(FButtonOk);
  end;
end;

{$IFDEF WEBLIB}
procedure TTMSFNCBitmapEditor.ApplyCSS(ACSS: string);
begin
//  FButtonOK.ElementClassName := 'IDEButton IDEFont';
//  FButtonCancel.ElementClassName := 'IDEButton IDEFont';
//  FButtonOpen.ElementClassName := 'IDEButton IDEFont';
//  FButtonClear.ElementClassName := 'IDEButton IDEFont';
//
//  FTopPanel.ElementClassName := 'IDEBkg';
//  FPanel.ElementClassName := 'IDEBkg';
end;

procedure TTMSFNCBitmapEditor.DoButtonOKClick(Sender: TObject);
begin
  if Assigned(frm) then
    frm.ModalResult := mrOK;
end;

procedure TTMSFNCBitmapEditor.DoButtonCancelClick(Sender: TObject);
begin
  if Assigned(frm) then
    frm.ModalResult := mrCancel;
end;
{$ENDIF}

constructor TTMSFNCBitmapEditor.Create(AOwner: TComponent);
begin
  inherited;
  FCopyBitmap := TTMSFNCBitmap.Create;
  FCopyBitmap.OnChange := DoCopyBitmapChange;
end;

destructor TTMSFNCBitmapEditor.Destroy;
begin
  FreeAndNil(FCopyBitmap);
  inherited;
end;

procedure TTMSFNCBitmapEditor.DoAfterDraw(Sender: TObject;
  AGraphics: TTMSFNCGraphics; ARect: TRectF);
begin
  if Assigned(FCopyBitmap) then
  begin
    InflateRectEx(ARect, -5, -5);
    AGraphics.DrawBitmap(ARect, FCopyBitmap);
  end;
end;

procedure TTMSFNCBitmapEditor.DoClearFile(Sender: TObject);
begin
  FCopyBitmap.Assign(nil);
  FImage.Invalidate;
end;

procedure TTMSFNCBitmapEditor.DoCopyBitmapChange(Sender: TObject);
begin
  if Assigned(FImage) then
    FImage.Invalidate;
end;

{$IFNDEF FMXLIB}
procedure TTMSFNCBitmapEditor.DoFormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
{$ENDIF}
{$IFDEF FMXLIB}
procedure TTMSFNCBitmapEditor.DoFormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
{$ENDIF}
begin
  inherited;

  if Key = KEY_ESCAPE then
  begin
    frm.Close;
  end
  else if Key = KEY_RETURN then
  begin
    frm.ModalResult := mrOk;
    {$IFNDEF FMXLIB}
    frm.Close;
    {$ENDIF}
    {$IFDEF FMXLIB}
    frm.CloseModal;
    {$ENDIF}
  end;
end;

procedure TTMSFNCBitmapEditor.DoOpenFile(Sender: TObject);
var
  od: TOpenDialog;
  I: Integer;
  str: string;
begin
  if not Assigned(FImage) then
    Exit;

  od := TOpenDialog.Create(Self);
  {$IFNDEF WEBLIB}
  od.Filter := 'All (*.gif;*.png;*.jpg;*.jpeg;*.bmp;*.tif;*.tiff;*.ico;*.emf;*.wmf;*.svg)|*.gif;'+
  '*.png;*.jpg;*.jpeg;*.bmp;*.tif;*.tiff;*.ico;*.emf;*.wmf;*.svg|GIF Image (*.gif)|*.gif|Portable Network Graphics (*.png)|*.png|'+
  'JPEG Image File (*.jpg)|*.jpg|JPEG Image File (*.jpeg)|*.jpeg|Bitmaps (*.bmp)|*.bmp|TIFF Images (*.tif)|*.tif|TIFF Images'+
  '(*.tiff)|*.tiff|Icons (*.ico)|*.ico|Enhanced Metafiles (*.emf)|*.emf|Metafiles (*.wmf)|*.wmf|Scalable Vector Graphics (*.svg)|*.svg';
  od.FilterIndex := 0;
  {$ENDIF}
  {$IFNDEF WEBLIB}
  if od.Execute then
  {$ENDIF}
  {$IFDEF WEBLIB}
  od.Execute(
  procedure(AFileName: string)
  {$ENDIF}
  begin
    for I := 0 to od.Files.Count - 1 do
    begin
      {$IFDEF WEBLIB}
      od.Files[I].GetFileAsBase64(
      procedure(AValue: string)
      {$ENDIF}
      begin
        {$IFNDEF WEBLIB}
        str := od.Files[I];
        FCopyBitmap.LoadFromFile(str);
        {$ENDIF}
        {$IFDEF WEBLIB}
        str := 'data:' + od.Files[I].MimeType + ';base64,' + AValue;
        FCopyBitmap.LoadFromResource(str);
        {$ENDIF}
        FImage.Invalidate;
     end{$IFDEF WEBLIB}){$ENDIF};
    end;
  end
  {$IFDEF WEBLIB}){$ENDIF};
  {$IFNDEF WEBLIB}
  od.Free;
  {$ENDIF}
end;

{$IFNDEF WEBLIB}
procedure TTMSFNCBitmapEditor.DoSaveFile(Sender: TObject);
var
  sd: TSaveDialog;
  fe,fn: string;
begin
  if not Assigned(FImage) then
    Exit;

  sd := TSaveDialog.Create(Self);
  sd.Filter := 'All (*.gif;*.png;*.jpg;*.jpeg;*.bmp;*.tif;*.tiff;*.ico;*.emf;*.wmf;*.svg)|*.gif;'+
  '*.png;*.jpg;*.jpeg;*.bmp;*.tif;*.tiff;*.ico;*.emf;*.wmf;*.svg|GIF Image (*.gif)|*.gif|Portable Network Graphics (*.png)|*.png|'+
  'JPEG Image File (*.jpg)|*.jpg|JPEG Image File (*.jpeg)|*.jpeg|Bitmaps (*.bmp)|*.bmp|TIFF Images (*.tif)|*.tif|TIFF Images'+
  '(*.tiff)|*.tiff|Icons (*.ico)|*.ico|Enhanced Metafiles (*.emf)|*.emf|Metafiles (*.wmf)|*.wmf|Scalable Vector Graphics (*.svg)|*.svg';
  sd.DefaultExt := 'png';
  sd.Options := sd.Options + [TOpenOption.ofOverwritePrompt];
  sd.FilterIndex := 0;
  try
    if sd.Execute then
    begin
      fn := sd.FileName;

      fe := Uppercase(ExtractFileExt(sd.FileName));

      if (fe = '') then
      begin
        if sd.FilterIndex = 3 then
          fn := fn + '.png';
      end
      else
      begin
        if fe = '.PNG' then
          sd.FilterIndex := 3;
      end;

      FCopyBitmap.SaveToFile(fn);
    end;
  finally
    sd.Free;
  end;
end;
{$ENDIF}

{$IFDEF WEBLIB}
procedure TTMSFNCBitmapEditor.Execute(AProc: TModalResultProc = nil);
{$ELSE}
function TTMSFNCBitmapEditor.Execute: TModalResult;
{$ENDIF}
begin
  FCopyBitmap.Assign(Bitmap);
  {$IFDEF WEBLIB}
  frm := TTMSFNCBitmapEditorForm.CreateDialogNew(Self, 'Bitmap Editor');
  {$ELSE}
  frm := TTMSFNCBitmapEditorForm.CreateNew(Application);
  frm.Caption := 'Bitmap Editor';
  {$ENDIF}
  frm.Width := 400;
  frm.Height := 500;
  {$IFDEF FMXLIB}
  frm.Position := TFormPosition.ScreenCenter;
  frm.Fill.Kind := TBrushKind.Solid;
  if IsLightTheme then
    frm.Fill.Color := EDITORSUBBACKCOLORLIGHT
  else
    frm.Fill.Color := EDITORSUBBACKCOLORDARK;
  {$ENDIF}
  {$IFDEF CMNLIB}
  frm.KeyPreview := True;
  {$ENDIF}
  frm.OnKeyDown := DoFormKeyDown;
  {$IFDEF CMNWEBLIB}
  frm.Position := poScreenCenter;
  {$ENDIF}
  {$IFNDEF WEBLIB}
  BuildEditor(frm);
  frm.ActiveControl := FTopPanel;
  {$ENDIF}
  TTMSFNCUtils.ScaleForCurrentDPI(frm);
  {$IFNDEF WEBLIB}
  Result := frm.ShowModal;
  if (Result = mrOk) and Assigned(Bitmap) then
  begin
    Bitmap.Assign(nil);
    Bitmap.Assign(FCopyBitmap);
  end;
  frm.Free;
  {$ELSE}
  frm.ShowModal(
  procedure(AResult: TModalResult)
  begin
    if (AResult = mrOk) and Assigned(Bitmap) then
    begin
      Bitmap.Assign(nil);
      Bitmap.Assign(FCopyBitmap);
    end;
    frm := nil;
    if Assigned(AProc) then
      AProc(AResult);
  end);
  {$ENDIF}
end;

function TTMSFNCBitmapEditor.GetInstance: NativeUInt;
begin
  Result := HInstance;
end;

procedure TTMSFNCBitmapEditor.SetBitmap(const Value: TTMSFNCBitmap);
begin
  FBitmap := Value;
end;

procedure TTMSFNCBitmapEditor.SetBitmapContainer(
  const Value: TTMSFNCBitmapContainer);
begin
  FBitmapContainer := Value;
end;

{$IFDEF WEBLIB}

{ TTMSFNCBitmapEditorForm }

constructor TTMSFNCBitmapEditorForm.CreateDialogNew(ABitmapEditor: TTMSFNCBitmapEditor; ACaption: string);
begin
  FBitmapEditor := ABitmapEditor;
  inherited CreateDialogNew(ACaption);
end;

procedure TTMSFNCBitmapEditorForm.LoadDFMValues;
begin
  inherited LoadDFMValues;
  if Assigned(FBitmapEditor) then
  begin
    FBitmapEditor.BuildEditor(Self);
    ActiveControl := FBitmapEditor.FTopPanel;
  end;
end;

procedure TTMSFNCBitmapEditorForm.Loaded;
var
  css: string;
begin
  inherited;

  css := Application.IDECSS;

  if (css <> '') then
  begin
    AddCSS('IDECSS', css);
    ElementClassName := 'IDEBkg';
    CaptionElement['class'] := 'IDECaption IDEFont';
    if Assigned(FBitmapEditor) then
      FBitmapEditor.ApplyCSS(css);
  end;
end;

{$ENDIF}

end.

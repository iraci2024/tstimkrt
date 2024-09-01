{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{            copyright (c) 2021                                      }
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

unit LCLTMSFNCGraphicsAppearanceEditor;

{$I LCLTMSFNCDefines.inc}

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
  Classes, LCLTMSFNCCustomComponent, LCLTMSFNCCustomControl, Controls,
  LCLTMSFNCTypes, StdCtrls, ExtCtrls, LCLTMSFNCBitmapContainer, Forms, LCLTMSFNCGraphics,
  LCLTMSFNCGraphicsTypes, LCLTMSFNCBitmapEditor, LCLTMSFNCEditorButton, LCLTMSFNCEditorPanel, LCLTMSFNCEditorsTools
  {$IFDEF FMXLIB}
  ,FMX.Types, FMX.ComboEdit, FMX.Layouts, FMX.Colors
  {$ENDIF}
  {$IFDEF VCLLIB}
  , VCL.ComCtrls
  {$ENDIF}
  {$IFDEF LCLLIB}
  , ColorBox, ComCtrls
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
  // v1.0.1.0 : New : Updated design fill and stroke editor

resourcestring
  sTMSFNCGraphicsAppearanceOK = 'OK';
  sTMSFNCGraphicsAppearanceCancel = 'Cancel';
  sTMSFNCGraphicsAppearanceColor = 'Color';
  sTMSFNCGraphicsAppearanceColorTo = 'ColorTo';
  sTMSFNCGraphicsAppearanceColorMirror = 'ColorMirror';
  sTMSFNCGraphicsAppearanceColorMirrorTo = 'ColorMirrorTo';
  sTMSFNCGraphicsAppearanceOpacity = 'Opacity';
  sTMSFNCGraphicsAppearanceOrientation = 'Orientation';
  sTMSFNCGraphicsAppearanceTexture = 'Texture';
  sTMSFNCGraphicsAppearanceSolid = 'Solid';
  sTMSFNCGraphicsAppearanceGradient = 'Gradient';
  sTMSFNCGraphicsAppearanceNone = 'None';
  sTMSFNCGraphicsAppearanceMode = 'Mode';
  sTMSFNCGraphicsAppearanceWidth = 'Width';
  sTMSFNCGraphicsAppearanceKind = 'Kind';
  sTMSFNCGraphicsAppearanceDot = 'Dot';
  sTMSFNCGraphicsAppearanceDash = 'Dash';

type
  {$IFDEF FMXLIB}
  TTMSFNCGraphicsAppearanceEditorParent = TFmxObject;
  TTMSFNCGraphicsAppearanceEditorComboBox = class(TComboEdit);
  TTMSFNCGraphicsAppearanceColorPicker = class(TColorComboBox);
  {$ENDIF}
  {$IFDEF CMNLIB}
  TTMSFNCGraphicsAppearanceEditorParent = TWinControl;
  TTMSFNCGraphicsAppearanceEditorComboBox = class(TComboBox);
  TTMSFNCGraphicsAppearanceColorPicker = class(TColorBox);
  {$ENDIF}
  {$IFDEF WEBLIB}
  TTMSFNCGraphicsAppearanceEditorParent = TWinControl;
  TTMSFNCGraphicsAppearanceEditorComboBox = class(TWebComboBox);
  TTMSFNCGraphicsAppearanceColorPicker = class(TWebColorPicker);
  {$ENDIF}

  {$IFNDEF LCLLIB}
  [ComponentPlatformsAttribute(TMSPlatformsDesktop)]
  {$ENDIF}
  TTMSFNCGraphicsFillEditor = class(TTMSFNCCustomComponent)
  private
    FSmall: Boolean;
    frm: TTMSFNCCustomDesignerForm;
    FPanel, FTopPanel: TTMSFNCEditorPanel;
    FNoneBtn, FSolidBtn, FGradientBtn, FTextureBtn: TTMSFNCEditorButton;
    FColP,FColToP, FColMirP, FColMirToP: TTMSFNCGraphicsAppearanceColorPicker;
    FColLbl, FColToLbl, FColMirLbl, FColMirToLbl: TLabel;
    FOpacityLbl, FOrientationLbl, FTextureLbl, FTextureModeLbl: TLabel;
    FOpacityTB: TTrackBar;
    FOrientationCB, FTextureModeCB: TTMSFNCGraphicsAppearanceEditorComboBox;
    {$IFNDEF FMXLIB}
    FVScrlBox: TScrollBox;
    {$ENDIF}
    {$IFDEF FMXLIB}
    FVScrlBox: TVertScrollBox;
    {$ENDIF}
    FFill: TTMSFNCGraphicsFill;
    FCopyFill: TTMSFNCGraphicsFill;
    FButtonOk, FButtonCancel: TTMSFNCEditorButton;
    FImage, FTextureImage: TTMSFNCCustomControl;
    FPreviewStroke: TTMSFNCGraphicsStroke;
    {$IFDEF WEBLIB}
    FBitmapEditor: TTMSFNCBitmapEditor;
    {$ENDIF}
    procedure ClearKindButtons;
    procedure InvalidatePreview;
    procedure UpdateControlPos;
    procedure GetFillKind;
    procedure SetFillValues;
    procedure SetPreviewStroke(const Value: TTMSFNCGraphicsStroke);
  protected
    function GetInstance: NativeUInt; override;
    procedure DoAfterDraw(Sender: TObject; AGraphics: TTMSFNCGraphics; ARect: TRectF);
    procedure DoAfterTextureDraw(Sender: TObject; AGraphics: TTMSFNCGraphics; ARect: TRectF);
    procedure DoCopyFillChanged(Sender: TObject);
    procedure RegisterRuntimeClasses; override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure BuildEditor(AParent: TTMSFNCGraphicsAppearanceEditorParent); virtual;
    procedure HideControls; virtual;
    procedure ShowSolidControls; virtual;
    procedure ShowGradientControls; virtual;
    procedure ShowTextureControls; virtual;
    {$IFDEF WEBLIB}
    procedure AssignWEBTextureBitmap(AValue: TModalResult);
    procedure DoButtonOKClick(Sender: TObject); virtual;
    procedure DoButtonCancelClick(Sender: TObject); virtual;
   {$ENDIF}
    procedure DoFormResize(Sender: TObject);
    procedure DoClearFile(Sender: TObject); virtual;
    procedure DoNoneBtnClick(Sender: TObject); virtual;
    procedure DoSolidBtnClick(Sender: TObject); virtual;
    procedure DoGradientBtnClick(Sender: TObject); virtual;
    procedure DoTextureBtnClick(Sender: TObject); virtual;
    procedure DoColPickChange(Sender: TObject); virtual;
    procedure DoColToPickChange(Sender: TObject); virtual;
    procedure DoColMirPickChange(Sender: TObject); virtual;
    procedure DoColMirToPickChange(Sender: TObject); virtual;
    procedure DoOpacityTrackBarChange(Sender: TObject); virtual;
    procedure DoOrientationComboChange(Sender: TObject); virtual;
    procedure DoTextureModeComboChange(Sender: TObject); virtual;
    procedure DoBitmapAssignClick(Sender: TObject); virtual;
    {$IFNDEF FMXLIB}
    procedure DoFormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    {$ENDIF}
    {$IFDEF FMXLIB}
    procedure DoFormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    {$ENDIF}
    {$IFDEF CMNLIB}
    procedure DoGetColors(Sender: TCustomColorBox; Items: TStrings);
    {$ENDIF}
    procedure SetFill(const Value: TTMSFNCGraphicsFill);
    procedure SetOrientationValue; virtual;
    procedure SetTextureModeValue; virtual;
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
    property Fill: TTMSFNCGraphicsFill read FFill write SetFill;
    property PreviewStroke: TTMSFNCGraphicsStroke read FPreviewStroke write SetPreviewStroke;
  end;

  TTMSFNCGraphicsFillEditorForm = class(TTMSFNCCustomDesignerForm)
  {$IFDEF WEBLIB}
  private
    FFillEditor: TTMSFNCGraphicsFillEditor;
  protected
    procedure Loaded; override;
    procedure LoadDFMValues; override;
  public
    constructor CreateDialogNew(AFillEditor: TTMSFNCGraphicsFillEditor; ACaption: string); reintroduce; virtual;
  {$ENDIF}
  end;

  {$IFNDEF LCLLIB}
  [ComponentPlatformsAttribute(TMSPlatformsDesktop)]
  {$ENDIF}
  TTMSFNCGraphicsStrokeEditor = class(TTMSFNCCustomComponent)
  private
    frm: TTMSFNCCustomDesignerForm;
    FPanel, FTopPanel: TTMSFNCEditorPanel;
    FKindCB: TTMSFNCGraphicsAppearanceEditorComboBox;
    FColP: TTMSFNCGraphicsAppearanceColorPicker;
    FColLbl, FKindLbl: TLabel;
    FOpacityLbl, FWidthLbl, FWidthValLbl: TLabel;
    FOpacityTB, FWidthTB: TTrackBar;
    {$IFNDEF FMXLIB}
    FVScrlBox: TScrollBox;
    {$ENDIF}
    {$IFDEF FMXLIB}
    FVScrlBox: TVertScrollBox;
    {$ENDIF}
    FStroke: TTMSFNCGraphicsStroke;
    FCopyStroke: TTMSFNCGraphicsStroke;
    FButtonOk, FButtonCancel: TTMSFNCEditorButton;
    FImage: TTMSFNCCustomControl;
    FPreviewBackGroundColor: TTMSFNCGraphicsColor;
    procedure InvalidatePreview;
    procedure UpdateControlPos;
    procedure SetStrokeValues;
    procedure SetPreviewBackGroundColor(const Value: TTMSFNCGraphicsColor);
  protected
    function GetInstance: NativeUInt; override;
    procedure DoAfterDraw(Sender: TObject; AGraphics: TTMSFNCGraphics; ARect: TRectF);
    procedure DoCopyStrokeChanged(Sender: TObject);
    procedure RegisterRuntimeClasses; override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure BuildEditor(AParent: TTMSFNCGraphicsAppearanceEditorParent); virtual;
    {$IFDEF WEBLIB}
    procedure DoButtonOKClick(Sender: TObject); virtual;
    procedure DoButtonCancelClick(Sender: TObject); virtual;
   {$ENDIF}
   {$IFNDEF FMXLIB}
    procedure DoFormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    {$ENDIF}
    {$IFDEF FMXLIB}
    procedure DoFormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    {$ENDIF}
    procedure DoFormResize(Sender: TObject);
    procedure DoClearFile(Sender: TObject); virtual;
    procedure DoKindComboChange(Sender: TObject); virtual;
    procedure DoColPickChange(Sender: TObject); virtual;
    procedure DoOpacityTrackBarChange(Sender: TObject); virtual;
    procedure DoWidthTrackBarChange(Sender: TObject); virtual;
    procedure SetKindValue; virtual;
    procedure SetStroke(const Value: TTMSFNCGraphicsStroke);
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
    property Stroke: TTMSFNCGraphicsStroke read FStroke write SetStroke;
    property PreviewBackGroundColor: TTMSFNCGraphicsColor read FPreviewBackGroundColor write SetPreviewBackGroundColor default gcNull;
  end;

  TTMSFNCGraphicsStrokeEditorForm = class(TTMSFNCCustomDesignerForm)
  {$IFDEF WEBLIB}
  private
    FStrokeEditor: TTMSFNCGraphicsStrokeEditor;
  protected
    procedure Loaded; override;
    procedure LoadDFMValues; override;
  public
    constructor CreateDialogNew(AStrokeEditor: TTMSFNCGraphicsStrokeEditor; ACaption: string); reintroduce; virtual;
  {$ENDIF}
  end;

implementation

uses
  LCLTMSFNCUtils, Graphics, SysUtils, Dialogs, Math
  {$IFDEF WEBLIB}
  ,WEBLib.WebTools
  {$ENDIF}
  {$IFNDEF FMXMOBILE}
  {$IFDEF DELPHIBERLIN}
  ,FMX.DialogService.Sync
  {$ENDIF}
  {$ENDIF}
  ;

{ TTMSFNCGraphicsFillEditor }

function TranslateTextEx(AText: String): String;
begin
  {$IFDEF FMXLIB}
  Result := TranslateText(AText);
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  Result := AText;
  {$ENDIF}
end;

{$IFDEF WEBLIB}
procedure TTMSFNCGraphicsFillEditor.AssignWEBTextureBitmap(AValue: TModalResult);
begin
  if AValue = mrOk then
  begin
    FCopyFill.Texture.Assign(FBitmapEditor.Bitmap);
  end;
  FBitmapEditor.Free;
  DoCopyFillChanged(Self);
end;
{$ENDIF}

procedure TTMSFNCGraphicsFillEditor.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
end;

procedure TTMSFNCGraphicsFillEditor.RegisterRuntimeClasses;
begin
  inherited;
  RegisterClass(TTMSFNCGraphicsFillEditor);
end;

procedure TTMSFNCGraphicsFillEditor.InvalidatePreview;
begin
  if Assigned(FImage) then
    FImage.Invalidate;
  if Assigned(FTextureImage) and FTextureImage.Visible then
    FTextureImage.Invalidate;
end;

procedure TTMSFNCGraphicsFillEditor.Assign(Source: TPersistent);
begin
  if (Source is TTMSFNCGraphicsFillEditor) then
    FFill := (Source as TTMSFNCGraphicsFillEditor).Fill;
end;

procedure TTMSFNCGraphicsFillEditor.BuildEditor(AParent: TTMSFNCGraphicsAppearanceEditorParent);
var
  marg: integer;
begin
  if Assigned(AParent) then
  begin
    marg := 5;

    {$IFDEF FMXLIB}
    FVScrlBox := TVertScrollBox.Create(AParent);
    FVScrlBox.Align := TAlignLayout.Client;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    FVScrlBox := TScrollBox.Create(AParent);
    FVScrlBox.Align := alClient;
    FVScrlBox.Top := 0;
    if IsLightTheme then
      FVScrlBox.Color := EDITORSUBBACKCOLORLIGHT
    else
      FVScrlBox.Color := EDITORSUBBACKCOLORDARK;
    {$IFDEF LCLLIB}
    FVScrlBox.HorzScrollBar.Visible:= False;
    {$ENDIF}
    {$ENDIF}
    FVScrlBox.Parent := AParent;

    FTopPanel := TTMSFNCEditorPanel.Create(AParent);
    {$IFDEF FMXLIB}
    FTopPanel.Align := TAlignLayout.Top;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    FTopPanel.Align := alTop;
    FTopPanel.Top := 0;
    {$ENDIF}
    {$IFDEF CMNLIB}
    FTopPanel.PanelPosition := eppCenter;
    {$ENDIF}
    {$IFNDEF CMNLIB}
    FTopPanel.PanelPosition := eppTop;
    {$ENDIF}
    FTopPanel.Parent := FVScrlBox;
    SetEditorBackPanelAppearance(FTopPanel);

    FImage := TTMSFNCCustomControl.Create(FTopPanel);
    FImage.Parent := FTopPanel;
    FImage.Height := 80;
//    FImage.ControlAlignment := caTop;
    {$IFDEF LCLLIB}
    FImage.BorderSpacing.Right := marg;
    FImage.BorderSpacing.Top := marg;
    FImage.BorderSpacing.Bottom := marg;
    FImage.BorderSpacing.Left := marg;
    {$ENDIF}
    {$IFNDEF LCLLIB}
    FImage.Margins.Right := marg;
    FImage.Margins.Top := marg;
    FImage.Margins.Bottom := marg;
    FImage.Margins.Left := marg;
    {$ENDIF}
    {$IFDEF WEBLIB}
    FImage.DisableBackground;
    {$ENDIF}
    if IsLightTheme then
      FImage.Color := EDITORSUBBACKCOLORLIGHT
    else
      FImage.Color := EDITORSUBBACKCOLORDARK;
    FImage.OnAfterDraw := @DoAfterDraw;

    FNoneBtn := TTMSFNCEditorButton.Create(FTopPanel);
    FNoneBtn.Parent := FTopPanel;
    FNoneBtn.Text := '&' + sTMSFNCGraphicsAppearanceNone;
    SetEditorIconButtonAppearance(FNoneBtn);
    FNoneBtn.OnClick := @DoNoneBtnClick;

    FSolidBtn := TTMSFNCEditorButton.Create(FTopPanel);
    FSolidBtn.Parent := FTopPanel;
    FSolidBtn.Text := '&' + sTMSFNCGraphicsAppearanceSolid;
    SetEditorIconButtonAppearance(FSolidBtn);
    FSolidBtn.OnClick := @DoSolidBtnClick;

    FGradientBtn := TTMSFNCEditorButton.Create(FTopPanel);
    FGradientBtn.Parent := FTopPanel;
    FGradientBtn.Text := '&' + sTMSFNCGraphicsAppearanceGradient;
    SetEditorIconButtonAppearance(FGradientBtn);
    FGradientBtn.OnClick := @DoGradientBtnClick;

    FTextureBtn := TTMSFNCEditorButton.Create(FTopPanel);
    FTextureBtn.Parent := FTopPanel;
    FTextureBtn.Text := '&' + sTMSFNCGraphicsAppearanceTexture;
    SetEditorIconButtonAppearance(FTextureBtn);
    FTextureBtn.OnClick := @DoTextureBtnClick;

    FColLbl := TLabel.Create(FTopPanel);
    FColLbl.Parent := FTopPanel;
    {$IFDEF FMXLIB}
    FColLbl.Text := sTMSFNCGraphicsAppearanceColor + ':';
    {$ENDIF}
    {$IFNDEF FMXLIB}
    FColLbl.Caption := sTMSFNCGraphicsAppearanceColor + ':';
    {$ENDIF}

    FColP := TTMSFNCGraphicsAppearanceColorPicker.Create(FTopPanel);
    FColP.Parent := FTopPanel;
    {$IFDEF VCLLIB}
    FColP.OnGetColors := DoGetColors;
    {$ENDIF}
    {$IFDEF LCLLIB}
    FColP.OnGetColors := @DoGetColors;
    {$ENDIF}
    {$IFDEF WEBLIB}
    FColP.OnSelect := @DoColPickChange;
    {$ENDIF}
    {$IFNDEF WEBLIB}
    FColP.OnChange := @DoColPickChange;
    {$ENDIF}

    FColToLbl := TLabel.Create(FTopPanel);
    FColToLbl.Parent := FTopPanel;
    {$IFDEF FMXLIB}
    FColToLbl.Text := sTMSFNCGraphicsAppearanceColorTo + ':';
    {$ENDIF}
    {$IFNDEF FMXLIB}
    FColToLbl.Caption := sTMSFNCGraphicsAppearanceColorTo + ':';
    {$ENDIF}

    FColToP := TTMSFNCGraphicsAppearanceColorPicker.Create(FTopPanel);
    FColToP.Parent := FTopPanel;
    {$IFDEF VCLLIB}
    FColToP.OnGetColors := DoGetColors;
    {$ENDIF}
    {$IFDEF LCLLIB}
    FColToP.OnGetColors := @DoGetColors;
    {$ENDIF}
    {$IFDEF WEBLIB}
    FColToP.OnSelect := @DoColToPickChange;
    {$ENDIF}
    {$IFNDEF WEBLIB}
    FColToP.OnChange := @DoColToPickChange;
    {$ENDIF}

    FColMirLbl := TLabel.Create(FTopPanel);
    FColMirLbl.Parent := FTopPanel;
    {$IFDEF FMXLIB}
    FColMirLbl.Text := sTMSFNCGraphicsAppearanceColorMirror + ':';
    {$ENDIF}
    {$IFNDEF FMXLIB}
    FColMirLbl.Caption := sTMSFNCGraphicsAppearanceColorMirror + ':';
    {$ENDIF}

    FColMirP := TTMSFNCGraphicsAppearanceColorPicker.Create(FTopPanel);
    FColMirP.Parent := FTopPanel;
    {$IFDEF VCLLIB}
    FColMirP.OnGetColors := DoGetColors;
    {$ENDIF}
    {$IFDEF LCLLIB}
    FColMirP.OnGetColors := @DoGetColors;
    {$ENDIF}
    {$IFDEF WEBLIB}
    FColMirP.OnSelect := @DoColMirPickChange;
    {$ENDIF}
    {$IFNDEF WEBLIB}
    FColMirP.OnChange := @DoColMirPickChange;
    {$ENDIF}

    FColMirToLbl := TLabel.Create(FTopPanel);
    FColMirToLbl.Parent := FTopPanel;
    {$IFDEF FMXLIB}
    FColMirToLbl.Text := sTMSFNCGraphicsAppearanceColorMirrorTo + ':';
    {$ENDIF}
    {$IFNDEF FMXLIB}
    FColMirToLbl.Caption := sTMSFNCGraphicsAppearanceColorMirrorTo + ':';
    {$ENDIF}

    FColMirToP := TTMSFNCGraphicsAppearanceColorPicker.Create(FTopPanel);
    FColMirToP.Parent := FTopPanel;
    {$IFDEF VCLLIB}
    FColMirToP.OnGetColors := DoGetColors;
    {$ENDIF}
    {$IFDEF LCLLIB}
    FColMirToP.OnGetColors := @DoGetColors;
    {$ENDIF}
    {$IFDEF WEBLIB}
    FColMirToP.OnSelect := @DoColMirToPickChange;
    {$ENDIF}
    {$IFNDEF WEBLIB}
    FColMirToP.OnChange := @DoColMirToPickChange;
    {$ENDIF}

    FOpacityLbl := TLabel.Create(FTopPanel);
    FOpacityLbl.Parent := FTopPanel;
    {$IFDEF FMXLIB}
    FOpacityLbl.Text := sTMSFNCGraphicsAppearanceOpacity + ':';
    {$ENDIF}
    {$IFNDEF FMXLIB}
    FOpacityLbl.Caption := sTMSFNCGraphicsAppearanceOpacity + ':';
    {$ENDIF}

    FOpacityTB := TTrackBar.Create(FTopPanel);
    FOpacityTB.Parent := FTopPanel;
    FOpacityTB.Min := 0;
    FOpacityTB.Max := 100;
    {$IFNDEF WEBLIB}
    FOpacityTB.Frequency := 5;
    {$ENDIF}
    FOpacityTB.OnChange := @DoOpacityTrackBarChange;

    FOrientationLbl := TLabel.Create(FTopPanel);
    FOrientationLbl.Parent := FTopPanel;
    {$IFDEF FMXLIB}
    FOrientationLbl.Text := sTMSFNCGraphicsAppearanceOrientation + ':';
    {$ENDIF}
    {$IFNDEF FMXLIB}
    FOrientationLbl.Caption := sTMSFNCGraphicsAppearanceOrientation + ':';
    {$ENDIF}

    FOrientationCB := TTMSFNCGraphicsAppearanceEditorComboBox.Create(FTopPanel);
    FOrientationCB.Parent := FTopPanel;
    FOrientationCB.OnChange := @DoOrientationComboChange;

    FTextureLbl := TLabel.Create(FTopPanel);
    FTextureLbl.Parent := FTopPanel;
    {$IFDEF FMXLIB}
    FTextureLbl.Text := sTMSFNCGraphicsAppearanceTexture + ':';
    {$ENDIF}
    {$IFNDEF FMXLIB}
    FTextureLbl.Caption := sTMSFNCGraphicsAppearanceTexture + ':';
    {$ENDIF}

    FTextureImage := TTMSFNCCustomControl.Create(AParent);
    FTextureImage.Parent := FTopPanel;
    FTextureImage.OnAfterDraw := @DoAfterTextureDraw;
    FTextureImage.OnClick := @DoBitmapAssignClick;

    FTextureModeLbl := TLabel.Create(FTopPanel);
    FTextureModeLbl.Parent := FTopPanel;
    {$IFDEF FMXLIB}
    FTextureModeLbl.Text := sTMSFNCGraphicsAppearanceMode + ':';
    {$ENDIF}
    {$IFNDEF FMXLIB}
    FTextureModeLbl.Caption := sTMSFNCGraphicsAppearanceMode + ':';
    {$ENDIF}

    FTextureModeCB := TTMSFNCGraphicsAppearanceEditorComboBox.Create(FTopPanel);
    FTextureModeCB.Parent := FTopPanel;
    FTextureModeCB.OnChange := @DoTextureModeComboChange;

    FPanel := TTMSFNCEditorPanel.Create(AParent);
    {$IFDEF FMXLIB}
    FPanel.Align := TAlignLayout.Bottom;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    FPanel.Align := alBottom;
    {$ENDIF}
    FPanel.Height := 37;
    FPanel.Parent := AParent;
    {$IFNDEF CMNLIB}
    FPanel.PanelPosition := eppBottom;
    {$ENDIF}
    {$IFDEF CMNLIB}
    FPanel.PanelPosition := eppCenter;
    {$ENDIF}

    SetEditorBackPanelAppearance(FPanel);

    FButtonCancel := TTMSFNCEditorButton.Create(FPanel);
    FButtonCancel.ModalResult := mrCancel;
    FButtonCancel.Parent := FPanel;
    FButtonCancel.Text := TranslateTextEx(sTMSFNCGraphicsAppearanceCancel);
    {$IFDEF FMXLIB}
    FButtonCancel.Align := TAlignLayout.Right;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    FButtonCancel.Align := alRight;
	FButtonCancel.Left := 100;
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
    FButtonCancel.OnClick := @DoButtonCancelClick;
    {$ENDIF}
    SetEditorCancelButtonAppearance(FButtonCancel);

    FButtonOK := TTMSFNCEditorButton.Create(FPanel);
    FButtonOK.ModalResult := mrOk;
    FButtonOK.Parent := FPanel;
    FButtonOK.Text := TranslateTextEx(sTMSFNCGraphicsAppearanceOK);
    {$IFDEF FMXLIB}
    FButtonOk.Align := TAlignLayout.Right;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    FButtonOk.Align := alRight;
	FButtonCancel.Left := 99;
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
    FButtonOK.OnClick := @DoButtonOKClick;
    {$ENDIF}
    SetEditorOKButtonAppearance(FButtonOk);

    FOrientationCB.Items.Add('Horizontal');
    FOrientationCB.Items.Add('Vertical');
    FOrientationCB.Items.Add('Custom');

    FTextureModeCB.Items.Add('Center');
    FTextureModeCB.Items.Add('Fit');
    FTextureModeCB.Items.Add('Original');
    FTextureModeCB.Items.Add('Stretch');
    FTextureModeCB.Items.Add('Tile');

    SetEditorLabelAppearance(FColLbl);
    SetEditorLabelAppearance(FColToLbl);
    SetEditorLabelAppearance(FColMirLbl);
    SetEditorLabelAppearance(FColMirToLbl);
    SetEditorLabelAppearance(FOpacityLbl);
    SetEditorLabelAppearance(FOrientationLbl);
    SetEditorLabelAppearance(FTextureLbl);
    SetEditorLabelAppearance(FTextureModeLbl);

    GetFillKind;
    SetFillValues;
  end;
end;

procedure TTMSFNCGraphicsFillEditor.ClearKindButtons;
begin
  FNoneBtn.Selected := False;
  FSolidBtn.Selected := False;
  FGradientBtn.Selected := False;
  FTextureBtn.Selected := False;
end;

{$IFDEF WEBLIB}
procedure TTMSFNCGraphicsFillEditor.DoButtonOKClick(Sender: TObject);
begin
  if Assigned(frm) then
    frm.ModalResult := mrOK;
end;

procedure TTMSFNCGraphicsFillEditor.DoButtonCancelClick(Sender: TObject);
begin
  if Assigned(frm) then
    frm.ModalResult := mrCancel;
end;
{$ENDIF}

constructor TTMSFNCGraphicsFillEditor.Create(AOwner: TComponent);
begin
  inherited;
  FPreviewStroke := TTMSFNCGraphicsStroke.Create(gskNone);
  FCopyFill := TTMSFNCGraphicsFill.Create;
  FCopyFill.OnChanged := @DoCopyFillChanged;
end;

destructor TTMSFNCGraphicsFillEditor.Destroy;
begin
  FreeAndNil(FPreviewStroke);
  FreeAndNil(FCopyFill);
  inherited;
end;

procedure TTMSFNCGraphicsFillEditor.DoAfterDraw(Sender: TObject;
  AGraphics: TTMSFNCGraphics; ARect: TRectF);
begin
  AGraphics.Stroke.Assign(FPreviewStroke);
  AGraphics.Fill.Assign(FCopyFill);
  InflateRectEx(ARect,-2,-2);
  AGraphics.DrawRectangle(ARect);
end;

procedure TTMSFNCGraphicsFillEditor.DoAfterTextureDraw(Sender: TObject;
  AGraphics: TTMSFNCGraphics; ARect: TRectF);
begin                                          
  InflateRectEx(ARect,-2,-2);
  AGraphics.DrawBitmap(ARect, FCopyFill.Texture);
end;


procedure TTMSFNCGraphicsFillEditor.DoBitmapAssignClick(Sender: TObject);
{$IFNDEF WEBLIB}
var
  be: TTMSFNCBitmapEditor;
{$ENDIF}
begin
  {$IFDEF WEBLIB}
  FBitmapEditor := TTMSFNCBitmapEditor.Create(Self);
  FBitmapEditor.Bitmap := FCopyFill.Texture;
  FBitmapEditor.Execute(@AssignWEBTextureBitmap);
  {$ENDIF}
  {$IFNDEF WEBLIB}
  be := TTMSFNCBitmapEditor.Create(frm);
  try
    be.Bitmap := FCopyFill.Texture;
    be.Execute;
  finally
    be.Free;
  end;

  {$IFDEF CMNLIB}
  FTextureImage.Invalidate;
  {$ENDIF}
  {$ENDIF}
end;

procedure TTMSFNCGraphicsFillEditor.DoClearFile(Sender: TObject);
begin
  FCopyFill.Assign(nil);
end;

procedure TTMSFNCGraphicsFillEditor.DoColMirPickChange(Sender: TObject);
var
  c: TTMSFNCGraphicsColor;
begin
  {$IFDEF FMXLIB}
  c := FColMirP.Color;
  {$ENDIF}
  {$IFNDEF FMXLIB}
  c:= FColMirP.Selected;
  {$ENDIF}
  if c <> FCopyFill.ColorMirror then
    FCopyFill.ColorMirror := c;
end;

procedure TTMSFNCGraphicsFillEditor.DoColMirToPickChange(Sender: TObject);
var
  c: TTMSFNCGraphicsColor;
begin
  {$IFDEF FMXLIB}
  c := FColMirToP.Color;
  {$ENDIF}
  {$IFNDEF FMXLIB}
  c:= FColMirToP.Selected;
  {$ENDIF}
  if c <> FCopyFill.ColorMirrorTo then
    FCopyFill.ColorMirrorTo := c;
end;

procedure TTMSFNCGraphicsFillEditor.DoColPickChange(Sender: TObject);
var
  c: TTMSFNCGraphicsColor;
begin
  {$IFDEF FMXLIB}
  c := FColP.Color;
  {$ENDIF}
  {$IFNDEF FMXLIB}
  c:= FColP.Selected;
  {$ENDIF}
  if c <> FCopyFill.Color then
    FCopyFill.Color := c;
end;

procedure TTMSFNCGraphicsFillEditor.DoColToPickChange(Sender: TObject);
var
  c: TTMSFNCGraphicsColor;
begin
  {$IFDEF FMXLIB}
  c := FColToP.Color;
  {$ENDIF}
  {$IFNDEF FMXLIB}
  c:= FColToP.Selected;
  {$ENDIF}
  if c <> FCopyFill.ColorTo then
    FCopyFill.ColorTo := c;
end;

procedure TTMSFNCGraphicsFillEditor.DoCopyFillChanged(Sender: TObject);
begin
//  SetFillValues;
  InvalidatePreview;
end;

{$IFNDEF FMXLIB}
procedure TTMSFNCGraphicsFillEditor.DoFormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
{$ENDIF}
{$IFDEF FMXLIB}
procedure TTMSFNCGraphicsFillEditor.DoFormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
{$ENDIF}
begin
//  inherited;

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

procedure TTMSFNCGraphicsFillEditor.DoFormResize(Sender: TObject);
begin
  FSmall := (frm.Width < (TTMSFNCUtils.GetDPIScale(frm, 96) * 300) );

  UpdateControlPos;
end;

{$IFDEF CMNLIB}
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

procedure TTMSFNCGraphicsFillEditor.DoGetColors(Sender: TCustomColorBox; Items: TStrings);
var
  I: Integer;
  c: LongInt;
begin
  for I := Low(TMSFNCGraphicsColors) to High(TMSFNCGraphicsColors) do
  begin
    FNCIdentToColor(TMSFNCGraphicsColors[I].Name, c);
    {$IFNDEF LCLLIB}
    Items.AddObject(TMSFNCGraphicsColors[I].Name, TObject(c));
    {$ENDIF}
    {$IFDEF LCLLIB}
    Items.AddObject(TMSFNCGraphicsColors[I].Name, TObject(PtrUint(c)));
    {$ENDIF}
  end;
end;
{$ENDIF}

procedure TTMSFNCGraphicsFillEditor.DoGradientBtnClick(Sender: TObject);
begin
  if FCopyFill.Kind <> gfkGradient then
  begin
    FCopyFill.Kind := gfkGradient;
    GetFillKind;

    InvalidatePreview;
  end;
end;

procedure TTMSFNCGraphicsFillEditor.DoNoneBtnClick(Sender: TObject);
begin
  if FCopyFill.Kind <> gfkNone then
  begin
    FCopyFill.Kind := gfkNone;
    GetFillKind;

    InvalidatePreview;
  end;
end;

procedure TTMSFNCGraphicsFillEditor.DoOpacityTrackBarChange(Sender: TObject);
begin
  {$IFDEF FMXLIB}
  if FCopyFill.Opacity <> FOpacityTB.Value / 100 then
    FCopyFill.Opacity := FOpacityTB.Value / 100;
  {$ENDIF}
  {$IFNDEF FMXLIB}
  if FCopyFill.Opacity <> FOpacityTB.Position / 100 then
    FCopyFill.Opacity := FOpacityTB.Position / 100;
  {$ENDIF}
end;

procedure TTMSFNCGraphicsFillEditor.DoOrientationComboChange(Sender: TObject);
var
  o: TTMSFNCGraphicsFillOrientation;
begin
  case FOrientationCB.ItemIndex of
    0: o := gfoHorizontal;
    1: o := gfoVertical;
    else
      o := gfoCustom;
  end;

  if FCopyFill.Orientation <> o then
    FCopyFill.Orientation := o;
end;

procedure TTMSFNCGraphicsFillEditor.DoSolidBtnClick(Sender: TObject);
begin
  if FCopyFill.Kind <> gfkSolid then
  begin
    FCopyFill.Kind := gfkSolid;
    GetFillKind;

    InvalidatePreview;
  end;
end;

procedure TTMSFNCGraphicsFillEditor.DoTextureBtnClick(Sender: TObject);
begin
  if FCopyFill.Kind <> gfkTexture then
  begin
    FCopyFill.Kind := gfkTexture;
    GetFillKind;

    InvalidatePreview;
  end;
end;

procedure TTMSFNCGraphicsFillEditor.DoTextureModeComboChange(Sender: TObject);
var
  m: TTMSFNCGraphicsTextureMode;
begin
  case FTextureModeCB.ItemIndex of
    0: m := gtmCenter;
    1: m := gtmFit;
    2: m := gtmOriginal;
    4: m := gtmTile;
    else
      m := gtmStretch;
  end;

  if FCopyFill.TextureMode <> m then
    FCopyFill.TextureMode := m;
end;

{$IFDEF WEBLIB}
procedure TTMSFNCGraphicsFillEditor.Execute(AProc: TModalResultProc = nil);
{$ELSE}
function TTMSFNCGraphicsFillEditor.Execute: TModalResult;
{$ENDIF}
begin
  FCopyFill.Assign(Fill);
  {$IFDEF WEBLIB}
  frm := TTMSFNCGraphicsFillEditorForm.CreateDialogNew(Self, 'Fill Editor');
  {$ELSE}
  frm := TTMSFNCGraphicsFillEditorForm.CreateNew(Application);
  frm.Caption := 'Fill Editor';
  frm.BorderIcons := [];
  {$ENDIF}
  frm.Width := 240;
  frm.Height := 530;
  {$IFDEF CMNLIB}
  frm.KeyPreview := True;
  {$ENDIF}
  frm.OnKeyDown := @DoFormKeyDown;
  {$IFDEF LCLLIB}
  frm.OnResize := @DoFormResize;
  {$ENDIF}
  {$IFNDEF LCLLIB}
  frm.OnResize := DoFormResize;
  {$ENDIF}
  {$IFDEF FMXLIB}
  frm.Position := TFormPosition.ScreenCenter;

  frm.Fill.Kind := TBrushKind.Solid;
  if IsLightTheme then
    frm.Fill.Color := EDITORSUBBACKCOLORLIGHT
  else
    frm.Fill.Color := EDITORSUBBACKCOLORDARK;
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  frm.Position := poScreenCenter;
  if IsLightTheme then
    frm.Color := EDITORSUBBACKCOLORLIGHT
  else
    frm.Color := EDITORSUBBACKCOLORDARK;
  {$ENDIF}
  {$IFNDEF WEBLIB}
  BuildEditor(frm);
  frm.ActiveControl := FTopPanel;
  {$ENDIF}
  TTMSFNCUtils.ScaleForCurrentDPI(frm);
  {$IFNDEF WEBLIB}
  Result := frm.ShowModal;
  if (Result = mrOk) and Assigned(Fill) then
  begin
    Fill.Assign(nil);
    Fill.Assign(FCopyFill);
  end;
  {$IFDEF LCLLIB}
  FreeAndNil(FImage);
  FreeAndNil(FTextureImage);
  {$ENDIF}
  frm.Free;
  {$ELSE}
  frm.ShowModal(
  procedure(AResult: TModalResult)
  begin
    if (AResult = mrOk) and Assigned(Fill) then
    begin
      Fill.Assign(nil);
      Fill.Assign(FCopyFill);
    end;
    frm := nil;
    if Assigned(AProc) then
      AProc(AResult);
  end);
  {$ENDIF}
end;

procedure TTMSFNCGraphicsFillEditor.GetFillKind;
begin
  ClearKindButtons;
  if Assigned(FCopyFill) then
  begin
    case FCopyFill.Kind of
      gfkNone:
      begin
        FNoneBtn.Selected := True;
        HideControls;
      end;
      gfkSolid:
      begin
        FSolidBtn.Selected := True;
        ShowSolidControls;
      end;
      gfkGradient:
      begin
        FGradientBtn.Selected := True;
        ShowGradientControls;
      end;
      gfkTexture:
      begin
        FTextureBtn.Selected := True;
        ShowTextureControls;
      end;
    end;
  end;

  UpdateControlPos;
end;

function TTMSFNCGraphicsFillEditor.GetInstance: NativeUInt;
begin
  Result := HInstance;
end;

procedure TTMSFNCGraphicsFillEditor.HideControls;
begin
  FColP.Visible := False;
  FColToP.Visible := False;
  FColMirP.Visible := False;
  FColMirToP.Visible := False;

  FColLbl.Visible := False;
  FColToLbl.Visible := False;
  FColMirLbl.Visible := False;
  FColMirToLbl.Visible := False;

  FOpacityLbl.Visible := False;
  FOpacityTB.Visible := False;

  FOrientationLbl.Visible := False;
  FOrientationCB.Visible := False;

  FTextureLbl.Visible := False;
  FTextureImage.Visible := False;
  FTextureModeLbl.Visible := False;
  FTextureModeCB.Visible := False;
end;

procedure TTMSFNCGraphicsFillEditor.SetFill(const Value: TTMSFNCGraphicsFill);
begin
  FFill := Value;
end;

procedure TTMSFNCGraphicsFillEditor.SetFillValues;
begin
  if Assigned(FCopyFill) and Assigned(FNoneBtn) then
  begin
    {$IFDEF FMXLIB}
    FColP.Color := FCopyFill.Color;
    FColToP.Color := FCopyFill.ColorTo;
    FColMirP.Color := FCopyFill.ColorMirror;
    FColMirToP.Color := FCopyFill.ColorMirrorTo;
    FOpacityTB.Value := Round(FCopyFill.Opacity * 100);
    {$ENDIF}
    {$IFNDEF FMXLIB}
    FColP.Selected := FCopyFill.Color;
    FColToP.Selected := FCopyFill.ColorTo;
    FColMirP.Selected := FCopyFill.ColorMirror;
    FColMirToP.Selected := FCopyFill.ColorMirrorTo;
    FOpacityTB.Position := Round(FCopyFill.Opacity * 100);
    {$ENDIF}
    SetOrientationValue;
    SetTextureModeValue;
  end;
end;

procedure TTMSFNCGraphicsFillEditor.SetOrientationValue;
begin
  case FCopyFill.Orientation of
    gfoHorizontal: FOrientationCB.ItemIndex := 0;
    gfoVertical: FOrientationCB.ItemIndex := 1;
    else
      FOrientationCB.ItemIndex := 2;
  end;
end;

procedure TTMSFNCGraphicsFillEditor.SetPreviewStroke(const Value: TTMSFNCGraphicsStroke);
begin
  FPreviewStroke.Assign(Value);
end;

procedure TTMSFNCGraphicsFillEditor.SetTextureModeValue;
begin
  case FCopyFill.TextureMode of
    gtmOriginal: FTextureModeCB.ItemIndex := 2;
    gtmFit: FTextureModeCB.ItemIndex := 1;
    gtmCenter: FTextureModeCB.ItemIndex := 0;
    gtmTile: FTextureModeCB.ItemIndex := 4;
    else
      FTextureModeCB.ItemIndex := 3;
  end;
end;

procedure TTMSFNCGraphicsFillEditor.ShowGradientControls;
begin
  FColP.Visible := True;
  FColToP.Visible := True;
  FColMirP.Visible := True;
  FColMirToP.Visible := True;

  FColLbl.Visible := True;
  FColToLbl.Visible := True;
  FColMirLbl.Visible := True;
  FColMirToLbl.Visible := True;

  FOpacityLbl.Visible := True;
  FOpacityTB.Visible := True;

  FOrientationLbl.Visible := True;
  FOrientationCB.Visible := True;

  FTextureLbl.Visible := False;
  FTextureImage.Visible := False;
  FTextureModeLbl.Visible := False;
  FTextureModeCB.Visible := False;
end;

procedure TTMSFNCGraphicsFillEditor.ShowSolidControls;
begin
  FColP.Visible := True;
  FColToP.Visible := False;
  FColMirP.Visible := False;
  FColMirToP.Visible := False;

  FColLbl.Visible := True;
  FColToLbl.Visible := False;
  FColMirLbl.Visible := False;
  FColMirToLbl.Visible := False;

  FOpacityLbl.Visible := True;
  FOpacityTB.Visible := True;

  FOrientationLbl.Visible := False;
  FOrientationCB.Visible := False;

  FTextureLbl.Visible := False;
  FTextureImage.Visible := False;
  FTextureModeLbl.Visible := False;
  FTextureModeCB.Visible := False;
end;

procedure TTMSFNCGraphicsFillEditor.ShowTextureControls;
begin
  FColP.Visible := False;
  FColToP.Visible := False;
  FColMirP.Visible := False;
  FColMirToP.Visible := False;

  FColLbl.Visible := False;
  FColToLbl.Visible := False;
  FColMirLbl.Visible := False;
  FColMirToLbl.Visible := False;

  FOpacityLbl.Visible := False;
  FOpacityTB.Visible := False;

  FOrientationLbl.Visible := False;
  FOrientationCB.Visible := False;

  FTextureLbl.Visible := True;
  FTextureImage.Visible := True;
  FTextureModeLbl.Visible := True;
  FTextureModeCB.Visible := True;
end;

procedure TTMSFNCGraphicsFillEditor.UpdateControlPos;
var
  marg, low: Integer;
begin
  marg := 5;
  if Assigned(frm) then
  begin
    FTopPanel.Width := FVScrlBox.Width;
    if FSmall then
      FNoneBtn.Width := Round((FTopPanel.Width - 3 * marg) / 2)
    else
      FNoneBtn.Width := Round((FTopPanel.Width - 5 * marg) / 4);

    FImage.Width := FTopPanel.Width - 2 * marg;
    {$IFDEF FMXLIB}
    FImage.Position.X := marg;
    FImage.Position.Y := marg;
    {$ENDIF}
    {$IFNDEF FMXLIB}
    FImage.Left := marg;
    FImage.Top := marg;
    {$ENDIF}


    {$IFDEF FMXLIB}
    FNoneBtn.Position.X := marg ;
    FNoneBtn.Position.Y := marg + FImage.Position.Y + FImage.Height;
    {$ENDIF}
    {$IFNDEF FMXLIB}
    FNoneBtn.Left := marg;
    FNoneBtn.Top := marg + FImage.Top + FImage.Height;
    {$ENDIF}

    FSolidBtn.Width := FNoneBtn.Width;
    {$IFDEF FMXLIB}
    FSolidBtn.Position.X := FNoneBtn.Position.X + FNoneBtn.Width + marg;
    FSolidBtn.Position.Y := FNoneBtn.Position.Y;
    {$ENDIF}
    {$IFNDEF FMXLIB}
    FSolidBtn.Left := FNoneBtn.Left + FNoneBtn.Width + marg ;
    FSolidBtn.Top := FNoneBtn.Top;
    {$ENDIF}

    FGradientBtn.Width := FNoneBtn.Width;
    {$IFDEF FMXLIB}
    if FSmall then
    begin
      FGradientBtn.Position.X := FNoneBtn.Position.X;
      FGradientBtn.Position.Y := FNoneBtn.Position.Y +FNoneBtn.Height + marg;
    end
    else
    begin
      FGradientBtn.Position.X := FSolidBtn.Position.X + FSolidBtn.Width + marg;
      FGradientBtn.Position.Y := FNoneBtn.Position.Y;
    end;
    {$ENDIF}
    {$IFNDEF FMXLIB}
    if FSmall then
    begin
      FGradientBtn.Left := FNoneBtn.Left;
      FGradientBtn.Top := FNoneBtn.Top + FNoneBtn.Height + marg;
    end
    else
    begin
      FGradientBtn.Left := FSolidBtn.Left + FNoneBtn.Width + marg;
      FGradientBtn.Top := FNoneBtn.Top;
    end;
    {$ENDIF}

    FTextureBtn.Width := FNoneBtn.Width;
    {$IFDEF FMXLIB}
    if FSmall then
    begin
      FTextureBtn.Position.X := FSolidBtn.Position.X;
      FTextureBtn.Position.Y := FGradientBtn.Position.Y;
    end
    else
    begin
      FTextureBtn.Position.X := FGradientBtn.Position.X + FGradientBtn.Width + marg;
      FTextureBtn.Position.Y := FNoneBtn.Position.Y;
    end;
    {$ENDIF}
    {$IFNDEF FMXLIB}
    if FSmall then
    begin
      FTextureBtn.Left := FSolidBtn.Left;
      FTextureBtn.Top := FGradientBtn.Top;
    end
    else
    begin
      FTextureBtn.Left := FGradientBtn.Left + FNoneBtn.Width + marg;
      FTextureBtn.Top := FNoneBtn.Top;
    end;
    {$ENDIF}

    {$IFDEF FMXLIB}
    FColLbl.Position.X := FNoneBtn.Position.X;
    FColLbl.Position.Y := FTextureBtn.Position.Y + FTextureBtn.Height + marg;
    {$ENDIF}
    {$IFNDEF FMXLIB}
    FColLbl.Left := FNoneBtn.Left;
    FColLbl.Top := FTextureBtn.Top + FTextureBtn.Height + marg;
    {$ENDIF}

    FColP.Width := Min(200, (FNoneBtn.Width + FSolidBtn.Width - 3 * marg));
    {$IFDEF FMXLIB}
    FColP.Position.X := FSolidBtn.Position.X + FSolidBtn.Width - FColP.Width;
    FColP.Position.Y := FColLbl.Position.Y + FColLbl.Height + marg;
    {$ENDIF}
    {$IFNDEF FMXLIB}
    FColP.Left := FSolidBtn.Left + FSolidBtn.Width - FColP.Width;
    FColP.Top := FColLbl.Top + FColLbl.Height + marg;
    {$ENDIF}
    {$IFDEF CMNLIB}
    FColP.Style := [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColors];
    {$ENDIF}

    {$IFDEF FMXLIB}
    if FSmall then
    begin
      FColToLbl.Position.X := FColLbl.Position.X;
      FColToLbl.Position.Y := FColP.Position.Y + FColP.Height + marg;
    end
    else
    begin
      FColToLbl.Position.X := FGradientBtn.Position.X;
      FColToLbl.Position.Y := FTextureBtn.Position.Y + FTextureBtn.Height + marg;
    end;
    {$ENDIF}
    {$IFNDEF FMXLIB}
    if FSmall then
    begin
      FColToLbl.Left := FColLbl.Left;
      FColToLbl.Top := FColP.Top + FColP.Height + marg;
    end
    else
    begin
      FColToLbl.Left := FGradientBtn.Left;
      FColToLbl.Top := FTextureBtn.Top + FTextureBtn.Height + marg;
    end;
    {$ENDIF}

    FColToP.Width := FColP.Width;
    {$IFDEF FMXLIB}
    FColToP.Position.X := FTextureBtn.Position.X + FTextureBtn.Width - FColToP.Width;
    FColToP.Position.Y := FColToLbl.Position.Y + FColToLbl.Height + marg;
    {$ENDIF}
    {$IFNDEF FMXLIB}
    FColToP.Left := FTextureBtn.Left + FTextureBtn.Width - FColToP.Width;
    FColToP.Top := FColToLbl.Top + FColToLbl.Height + marg;
    {$ENDIF}
    {$IFDEF CMNLIB}
    FColToP.Style := [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColors];
    {$ENDIF}

    {$IFDEF FMXLIB}
    FColMirLbl.Position.X := FColLbl.Position.X;
    FColMirLbl.Position.Y := FColToP.Position.Y + FColToP.Height + marg;
    {$ENDIF}
    {$IFNDEF FMXLIB}
    FColMirLbl.Left := FColLbl.Left;
    FColMirLbl.Top := FColToP.Top + FColToP.Height + marg;
    {$ENDIF}

    FColMirP.Width := FColP.Width;
    {$IFDEF FMXLIB}
    FColMirP.Position.X := FColP.Position.X;
    FColMirP.Position.Y := FColMirLbl.Position.Y + FColMirLbl.Height + marg;
    {$ENDIF}
    {$IFNDEF FMXLIB}
    FColMirP.Left := FColP.Left;
    FColMirP.Top := FColMirLbl.Top + FColMirLbl.Height + marg;
    {$ENDIF}
    {$IFDEF CMNLIB}
    FColMirP.Style := [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColors];
    {$ENDIF}

    {$IFDEF FMXLIB}
    FColMirToLbl.Position.X := FColToLbl.Position.X;
    if FSmall then
      FColMirToLbl.Position.Y := FColMirP.Position.Y + FColMirP.Height + marg
    else
      FColMirToLbl.Position.Y := FColMirLbl.Position.Y;
    {$ENDIF}
    {$IFNDEF FMXLIB}
    FColMirToLbl.Left := FColToLbl.Left;
    if FSmall then
      FColMirToLbl.Top := FColMirP.Top + FColMirP.Height + marg
    else
      FColMirToLbl.Top := FColMirLbl.Top;
    {$ENDIF}

    FColMirToP.Width := FColToP.Width;
    {$IFDEF FMXLIB}
    FColMirToP.Position.X := FColToP.Position.X;
    FColMirToP.Position.Y := FColMirToLbl.Position.Y + FColMirToLbl.Height + marg;
    {$ENDIF}
    {$IFNDEF FMXLIB}
    FColMirToP.Left := FColToP.Left;
    FColMirToP.Top := FColMirToLbl.Top + FColMirToLbl.Height + marg;
    {$ENDIF}
    {$IFDEF CMNLIB}
    FColMirToP.Style := [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColors];
    {$ENDIF}

    {$IFDEF FMXLIB}
    FOpacityLbl.Position.X := FColMirLbl.Position.X;
    FOpacityLbl.Position.Y := FColMirToP.Position.Y + FColMirToP.Height + marg;
    {$ENDIF}
    {$IFNDEF FMXLIB}
    FOpacityLbl.Left := FColMirLbl.Left;
    FOpacityLbl.Top := FColMirToP.Top + FColMirToP.Height + marg;
    {$ENDIF}

    FOpacityTB.Width := FColP.Width;
    {$IFDEF FMXLIB}
    FOpacityTB.Position.X := FColMirP.Position.X;
    FOpacityTB.Position.Y := FOpacityLbl.Position.Y + FOpacityLbl.Height + marg;
    {$ENDIF}
    {$IFNDEF FMXLIB}
    FOpacityTB.Left := FColMirP.Left;
    FOpacityTB.Top := FOpacityLbl.Top + FOpacityLbl.Height + marg;
    {$ENDIF}

    {$IFDEF FMXLIB}
    if FSmall then
    begin
      FOrientationLbl.Position.X := FOpacityLbl.Position.X;
      FOrientationLbl.Position.Y := FOpacityTB.Position.Y + FOpacityLbl.Height + marg;
    end
    else
    begin
      FOrientationLbl.Position.X := FColMirToLbl.Position.X;
      FOrientationLbl.Position.Y := FOpacityLbl.Position.Y;
    end;
    {$ENDIF}
    {$IFNDEF FMXLIB}
    if FSmall then
    begin
      FOrientationLbl.Left := FOpacityLbl.Left;
      FOrientationLbl.Top:= FOpacityTB.Top + FOpacityTB.Height + marg;
    end
    else
    begin
      FOrientationLbl.Left := FColMirToLbl.Left;
      FOrientationLbl.Top := FOpacityLbl.Top;
    end;
    {$ENDIF}

    FOrientationCB.Width := FColP.Width;
    {$IFDEF FMXLIB}
    FOrientationCB.Position.X := FColMirToP.Position.X;
    FOrientationCB.Position.Y := FOrientationLbl.Position.Y + FOrientationLbl.Height + marg;
    {$ENDIF}
    {$IFNDEF FMXLIB}
    FOrientationCB.Left := FColMirToP.Left;
    FOrientationCB.Top := FOrientationLbl.Top + FOrientationLbl.Height + marg;
    {$ENDIF}

    {$IFDEF FMXLIB}
    FTextureLbl.Position.X := FNoneBtn.Position.X;
    FTextureLbl.Position.Y := FTextureBtn.Position.Y + FTextureBtn.Height + marg;
    {$ENDIF}
    {$IFNDEF FMXLIB}
    FTextureLbl.Left := FNoneBtn.Left;
    FTextureLbl.Top := FTextureBtn.Top + FTextureBtn.Height + marg;
    {$ENDIF}

    FTextureImage.Width := FTopPanel.Width - marg * 2;
    {$IFDEF FMXLIB}
    if FSmall then
      FTextureImage.Height := FColMirToP.Position.Y - FColP.Position.Y
    else
      FTextureImage.Height := 150;
    FTextureImage.Position.X := marg;
    FTextureImage.Position.Y := FTextureLbl.Position.Y + FTextureLbl.Height + marg;
    {$ENDIF}
    {$IFNDEF FMXLIB}
    if FSmall then
      FTextureImage.Height := FColMirToP.Top - FColP.Top
    else
      FTextureImage.Height := 150;
    FTextureImage.Left := marg;
    FTextureImage.Top := FTextureLbl.Top + FTextureLbl.Height + marg;
    {$ENDIF}

    {$IFDEF FMXLIB}
    FTextureModeLbl.Position.X := FTextureLbl.Position.X;
    FTextureModeLbl.Position.Y := FTextureImage.Position.Y + FTextureImage.Height + marg;
    {$ENDIF}
    {$IFNDEF FMXLIB}
    FTextureModeLbl.Left := FTextureLbl.Left;
    FTextureModeLbl.Top := FTextureImage.Top + FTextureImage.Height + marg;
    {$ENDIF}

    FTextureModeCB.Width := FColP.Width;
    {$IFDEF FMXLIB}
    FTextureModeCB.Position.X := FTopPanel.Width - marg - FTextureModeCB.Width;
    FTextureModeCB.Position.Y := FTextureModeLbl.Position.Y + FTextureModeLbl.Height + marg;
    {$ENDIF}
    {$IFNDEF FMXLIB}
    FTextureModeCB.Left := FTopPanel.Width - marg - FTextureModeCB.Width;
    FTextureModeCB.Top := FTextureModeLbl.Top + FTextureModeLbl.Height + marg;
    {$ENDIF}

    {$IFDEF FMXLIB}
    case FCopyFill.Kind of
      gfkSolid: low := Round(FOpacityTB.Position.Y + FOpacityTB.Height + marg);
      gfkGradient: low := Round(FOrientationCB.Position.Y + FOrientationCB.Height + marg);
      gfkTexture: low := Round(FTextureModeCB.Position.Y + FTextureModeCB.Height + marg);
      else
        low := Round(FImage.Position.Y + FImage.Height + marg);
    end;
    {$ENDIF}
    {$IFNDEF FMXLIB}
    case FCopyFill.Kind of
      gfkSolid: low := FOpacityTB.Top + FOpacityTB.Height + marg;
      gfkGradient: low := FOrientationCB.Top + FOrientationCB.Height + marg;
      gfkTexture: low := FTextureModeCB.Top + FTextureModeCB.Height + marg;
      else
        low := FImage.Top + FImage.Height + marg;
    end;
    {$ENDIF}
    FTopPanel.Height := Max(low, FVScrlBox.Height - marg);
  end;
end;

{$IFDEF WEBLIB}

{ TTMSFNCGraphicsFillEditorForm }

constructor TTMSFNCGraphicsFillEditorForm.CreateDialogNew(AFillEditor: TTMSFNCGraphicsFillEditor; ACaption: string);
begin
  FFillEditor := AFillEditor;
  inherited CreateDialogNew(ACaption);
end;

procedure TTMSFNCGraphicsFillEditorForm.LoadDFMValues;
begin
  inherited LoadDFMValues;
  if Assigned(FFillEditor) then
  begin
    FFillEditor.BuildEditor(Self);
    ActiveControl := FFillEditor.FTopPanel;
  end;
end;

procedure TTMSFNCGraphicsFillEditorForm.Loaded;
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
  end;
end;

{$ENDIF}


{ TTMSFNCGraphicsStrokeEditor }

procedure TTMSFNCGraphicsStrokeEditor.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
end;

procedure TTMSFNCGraphicsStrokeEditor.RegisterRuntimeClasses;
begin
  inherited;
  RegisterClass(TTMSFNCGraphicsStrokeEditor);
end;

procedure TTMSFNCGraphicsStrokeEditor.InvalidatePreview;
begin
  if Assigned(FImage) then
    FImage.Invalidate;
end;

procedure TTMSFNCGraphicsStrokeEditor.Assign(Source: TPersistent);
begin
  if (Source is TTMSFNCGraphicsStrokeEditor) then
    FStroke := (Source as TTMSFNCGraphicsStrokeEditor).Stroke;
end;

procedure TTMSFNCGraphicsStrokeEditor.BuildEditor(AParent: TTMSFNCGraphicsAppearanceEditorParent);
var
  marg: integer;
begin
  if Assigned(AParent) then
  begin
    marg := 5;

    {$IFDEF FMXLIB}
    FVScrlBox := TVertScrollBox.Create(AParent);
    FVScrlBox.Align := TAlignLayout.Client;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    FVScrlBox := TScrollBox.Create(AParent);
    FVScrlBox.Align := alClient;
    FVScrlBox.Top := 0;
     if IsLightTheme then
       FVScrlBox.Color := EDITORSUBBACKCOLORLIGHT
     else
       FVScrlBox.Color := EDITORSUBBACKCOLORDARK;
    {$IFDEF LCLLIB}
    FVScrlBox.HorzScrollBar.Visible:= False;
    {$ENDIF}
    {$ENDIF}
    FVScrlBox.Parent := AParent;

    FTopPanel := TTMSFNCEditorPanel.Create(AParent);
    {$IFDEF FMXLIB}
    FTopPanel.Align := TAlignLayout.Top;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    FTopPanel.Align := alTop;
    FTopPanel.Top := 0;
    {$ENDIF}
    {$IFDEF CMNLIB}
    FTopPanel.PanelPosition := eppCenter;
    {$ENDIF}
    {$IFNDEF CMNLIB}
    FTopPanel.PanelPosition := eppTop;
    {$ENDIF}
    SetEditorBackPanelAppearance(FTopPanel);
    FTopPanel.Parent := FVScrlBox;

    FImage := TTMSFNCCustomControl.Create(FTopPanel);
    FImage.Parent := FTopPanel;
    FImage.Height := 80;
//    FImage.ControlAlignment := caTop;
    {$IFDEF LCLLIB}
    FImage.BorderSpacing.Right := marg;
    FImage.BorderSpacing.Top := marg;
    FImage.BorderSpacing.Bottom := marg;
    FImage.BorderSpacing.Left := marg;
    {$ENDIF}
    {$IFNDEF LCLLIB}
    FImage.Margins.Right := marg;
    FImage.Margins.Top := marg;
    FImage.Margins.Bottom := marg;
    FImage.Margins.Left := marg;
    {$ENDIF}
    {$IFDEF WEBLIB}
    FImage.DisableBackground;
    {$ENDIF}
    if FPreviewBackGroundColor = gcNull then
    begin
      if IsLightTheme then
        FPreviewBackGroundColor := EDITORSUBBACKCOLORLIGHT
      else
        FPreviewBackGroundColor := EDITORSUBBACKCOLORDARK;
    end;

    FImage.OnAfterDraw := @DoAfterDraw;

    FKindLbl := TLabel.Create(FTopPanel);
    FKindLbl.Parent := FTopPanel;
    {$IFDEF FMXLIB}
    FKindLbl.Text := sTMSFNCGraphicsAppearanceKind + ':';
    {$ENDIF}
    {$IFNDEF FMXLIB}
    FKindLbl.Caption := sTMSFNCGraphicsAppearanceKind + ':';
    {$ENDIF}

    FKindCB := TTMSFNCGraphicsAppearanceEditorComboBox.Create(FTopPanel);
    FKindCB.Parent := FTopPanel;
    FKindCB.OnChange := @DoKindComboChange;

    FColLbl := TLabel.Create(FTopPanel);
    FColLbl.Parent := FTopPanel;
    {$IFDEF FMXLIB}
    FColLbl.Text := sTMSFNCGraphicsAppearanceColor + ':';
    {$ENDIF}
    {$IFNDEF FMXLIB}
    FColLbl.Caption := sTMSFNCGraphicsAppearanceColor + ':';
    {$ENDIF}

    FColP := TTMSFNCGraphicsAppearanceColorPicker.Create(FTopPanel);
    FColP.Parent := FTopPanel;
    {$IFDEF WEBLIB}
    FColP.OnSelect := @DoColPickChange;
    {$ENDIF}
    {$IFNDEF WEBLIB}
    FColP.OnChange := @DoColPickChange;
    {$ENDIF}

    FOpacityLbl := TLabel.Create(FTopPanel);
    FOpacityLbl.Parent := FTopPanel;
    {$IFDEF FMXLIB}
    FOpacityLbl.Text := sTMSFNCGraphicsAppearanceOpacity + ':';
    {$ENDIF}
    {$IFNDEF FMXLIB}
    FOpacityLbl.Caption := sTMSFNCGraphicsAppearanceOpacity + ':';
    {$ENDIF}

    FOpacityTB := TTrackBar.Create(FTopPanel);
    FOpacityTB.Parent := FTopPanel;
    FOpacityTB.Min := 0;
    FOpacityTB.Max := 100;
    {$IFNDEF WEBLIB}
    FOpacityTB.Frequency := 5;
    {$ENDIF}
    FOpacityTB.OnChange := @DoOpacityTrackBarChange;

    FWidthLbl := TLabel.Create(FTopPanel);
    FWidthLbl.Parent := FTopPanel;
    {$IFDEF FMXLIB}
    FWidthLbl.Text := sTMSFNCGraphicsAppearanceWidth + ':';
    {$ENDIF}
    {$IFNDEF FMXLIB}
    FWidthLbl.Caption := sTMSFNCGraphicsAppearanceWidth + ':';
    {$ENDIF}

    FWidthValLbl := TLabel.Create(FTopPanel);
    FWidthValLbl.Parent := FTopPanel;
    {$IFDEF FMXLIB}
    FWidthValLbl.TextSettings.HorzAlign := TTextAlign.Trailing;
    {$ENDIF}
    {$IFNDEF FMXLIB}
    FWidthValLbl.Alignment := taRightJustify;
    {$ENDIF}
    FWidthValLbl.AutoSize := False;
    FWidthValLbl.Width := 25;

    FWidthTB := TTrackBar.Create(FTopPanel);
    FWidthTB.Parent := FTopPanel;
    FWidthTB.Min := 0;
    FWidthTB.Max := 20;
    {$IFNDEF WEBLIB}
    FWidthTB.Frequency := 1;
    {$ENDIF}
    FWidthTB.OnChange := @DoWidthTrackBarChange;

    FPanel := TTMSFNCEditorPanel.Create(AParent);
    {$IFDEF FMXLIB}
    FPanel.Align := TAlignLayout.Bottom;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    FPanel.Align := alBottom;
    {$ENDIF}
    FPanel.Height := 37;
    {$IFDEF CMNLIB}
    FPanel.PanelPosition := eppCenter;
    {$ENDIF}
    {$IFNDEF CMNLIB}
    FPanel.PanelPosition := eppBottom;
    {$ENDIF}
    SetEditorBackPanelAppearance(FPanel);
    FPanel.Parent := AParent;

    FButtonCancel := TTMSFNCEditorButton.Create(FPanel);
    FButtonCancel.ModalResult := mrCancel;
    FButtonCancel.Parent := FPanel;
    FButtonCancel.Text := TranslateTextEx(sTMSFNCGraphicsAppearanceCancel);
    {$IFDEF FMXLIB}
    FButtonCancel.Align := TAlignLayout.Right;
	FButtonCancel.Left := 100;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    FButtonCancel.Align := alRight;
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
    FButtonCancel.OnClick := @DoButtonCancelClick;
    {$ENDIF}
    SetEditorCancelButtonAppearance(FButtonCancel);

    FButtonOK := TTMSFNCEditorButton.Create(FPanel);
    FButtonOK.ModalResult := mrOk;
    FButtonOK.Parent := FPanel;
    FButtonOK.Text := TranslateTextEx(sTMSFNCGraphicsAppearanceOK);
    {$IFDEF FMXLIB}
    FButtonOk.Align := TAlignLayout.Right;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    FButtonOk.Align := alRight;
	FButtonCancel.Left := 99;
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
    FButtonOK.OnClick := @DoButtonOKClick;
    {$ENDIF}
    SetEditorOKButtonAppearance(FButtonOk);

    FKindCB.Items.Add(sTMSFNCGraphicsAppearanceNone);
    FKindCB.Items.Add(sTMSFNCGraphicsAppearanceSolid);
    FKindCB.Items.Add(sTMSFNCGraphicsAppearanceDash);
    FKindCB.Items.Add(sTMSFNCGraphicsAppearanceDot);
    FKindCB.Items.Add(sTMSFNCGraphicsAppearanceDash + sTMSFNCGraphicsAppearanceDot);
    FKindCB.Items.Add(sTMSFNCGraphicsAppearanceDash + sTMSFNCGraphicsAppearanceDot + sTMSFNCGraphicsAppearanceDot);

    SetEditorLabelAppearance(FColLbl);
    SetEditorLabelAppearance(FKindLbl);
    SetEditorLabelAppearance(FOpacityLbl);
    SetEditorLabelAppearance(FWidthLbl);
    SetEditorLabelAppearance(FWidthValLbl);

    SetStrokeValues;
  end;
end;

{$IFDEF WEBLIB}
procedure TTMSFNCGraphicsStrokeEditor.DoButtonOKClick(Sender: TObject);
begin
  if Assigned(frm) then
    frm.ModalResult := mrOK;
end;

procedure TTMSFNCGraphicsStrokeEditor.DoButtonCancelClick(Sender: TObject);
begin
  if Assigned(frm) then
    frm.ModalResult := mrCancel;
end;
{$ENDIF}

constructor TTMSFNCGraphicsStrokeEditor.Create(AOwner: TComponent);
begin
  inherited;
  FPreviewBackGroundColor := gcNull;

  FCopyStroke := TTMSFNCGraphicsStroke.Create;
  FCopyStroke.OnChanged := @DoCopyStrokeChanged;
end;

destructor TTMSFNCGraphicsStrokeEditor.Destroy;
begin
  FreeAndNil(FCopyStroke);
  inherited;
end;

procedure TTMSFNCGraphicsStrokeEditor.DoAfterDraw(Sender: TObject;
  AGraphics: TTMSFNCGraphics; ARect: TRectF);
var
  ch, sw: integer;

begin
  AGraphics.Fill.Kind := gfkSolid;
  AGraphics.Fill.Color := FPreviewBackGroundColor;

  AGraphics.Stroke.Kind := gskNone;

  AGraphics.DrawRectangle(ARect);

  AGraphics.Stroke.Assign(FCopyStroke);

  ch := Round((ARect.Bottom - ARect.Top) / 2);
  sw := Round((ARect.Right - ARect.Left) / 6);

  AGraphics.DrawLine(PointF(sw, ch), PointF(sw * 5, ch));
end;

procedure TTMSFNCGraphicsStrokeEditor.DoClearFile(Sender: TObject);
begin
  FCopyStroke.Assign(nil);
end;

procedure TTMSFNCGraphicsStrokeEditor.DoColPickChange(Sender: TObject);
var
  c: TTMSFNCGraphicsColor;
begin
  {$IFDEF FMXLIB}
  c := FColP.Color;
  {$ENDIF}
  {$IFNDEF FMXLIB}
  c:= FColP.Selected;
  {$ENDIF}
  if c <> FCopyStroke.Color then
    FCopyStroke.Color := c;
end;

procedure TTMSFNCGraphicsStrokeEditor.DoCopyStrokeChanged(Sender: TObject);
begin
  InvalidatePreview;
end;

{$IFNDEF FMXLIB}
procedure TTMSFNCGraphicsStrokeEditor.DoFormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
{$ENDIF}
{$IFDEF FMXLIB}
procedure TTMSFNCGraphicsStrokeEditor.DoFormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
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

procedure TTMSFNCGraphicsStrokeEditor.DoFormResize(Sender: TObject);
begin
  UpdateControlPos;
end;

procedure TTMSFNCGraphicsStrokeEditor.DoOpacityTrackBarChange(Sender: TObject);
begin
  {$IFDEF FMXLIB}
  if FCopyStroke.Opacity <> FOpacityTB.Value / 100 then
    FCopyStroke.Opacity := FOpacityTB.Value / 100;
  {$ENDIF}
  {$IFNDEF FMXLIB}
  if FCopyStroke.Opacity <> FOpacityTB.Position / 100 then
    FCopyStroke.Opacity := FOpacityTB.Position / 100;
  {$ENDIF}
end;

procedure TTMSFNCGraphicsStrokeEditor.DoWidthTrackBarChange(Sender: TObject);
begin
  {$IFDEF FMXLIB}
  if FCopyStroke.Width <> FWidthTB.Value / 2 then
    FCopyStroke.Width := FWidthTB.Value / 2;
  FWidthValLbl.Text := FloatToStr(FCopyStroke.Width);
  {$ENDIF}
  {$IFNDEF FMXLIB}
  if FCopyStroke.Width <> FWidthTB.Position / 2 then
    FCopyStroke.Width := FWidthTB.Position / 2;
  FWidthValLbl.Caption := FloatToStr(FCopyStroke.Width);
  {$ENDIF}
end;

procedure TTMSFNCGraphicsStrokeEditor.DoKindComboChange(Sender: TObject);
var
  k: TTMSFNCGraphicsStrokeKind;
begin
  case FKindCB.ItemIndex of
    1: k := gskSolid;
    2: k := gskDash;
    3: k := gskDot;
    4: k := gskDashDot;
    5: k := gskDashDotDot;
    else
      k := gskNone;
  end;

  if FCopyStroke.Kind <> k then
    FCopyStroke.Kind := k;
end;

{$IFDEF WEBLIB}
procedure TTMSFNCGraphicsStrokeEditor.Execute(AProc: TModalResultProc = nil);
{$ELSE}
function TTMSFNCGraphicsStrokeEditor.Execute: TModalResult;
{$ENDIF}
begin
  FCopyStroke.Assign(Stroke);
  {$IFDEF WEBLIB}
  frm := TTMSFNCGraphicsStrokeEditorForm.CreateDialogNew(Self, 'Stroke Editor');
  {$ELSE}
  frm := TTMSFNCGraphicsStrokeEditorForm.CreateNew(Application);
  frm.Caption := 'Stroke Editor';
  frm.BorderIcons := [];
  {$ENDIF}
  frm.Width := 240;
  frm.Height := 410;
  {$IFDEF CMNLIB}
  frm.KeyPreview := True;
  {$ENDIF}
  frm.OnKeyDown := @DoFormKeyDown;
  {$IFDEF LCLLIB}
  frm.OnResize := @DoFormResize;
  {$ENDIF}
  {$IFNDEF LCLLIB}
  frm.OnResize := DoFormResize;
  {$ENDIF}
  {$IFDEF FMXLIB}
  frm.Position := TFormPosition.ScreenCenter;

  frm.Fill.Kind := TBrushKind.Solid;
  if IsLightTheme then
    frm.Fill.Color := EDITORSUBBACKCOLORLIGHT
  else
    frm.Fill.Color := EDITORSUBBACKCOLORDARK;
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  frm.Position := poScreenCenter;

  if IsLightTheme then
    frm.Color := EDITORSUBBACKCOLORLIGHT
  else
    frm.Color := EDITORSUBBACKCOLORDARK;
  {$ENDIF}
  {$IFNDEF WEBLIB}
  BuildEditor(frm);
  frm.ActiveControl := FTopPanel;
  {$ENDIF}
  TTMSFNCUtils.ScaleForCurrentDPI(frm);
  {$IFNDEF WEBLIB}
  Result := frm.ShowModal;
  if (Result = mrOk) and Assigned(Stroke) then
  begin
    Stroke.Assign(nil);
    Stroke.Assign(FCopyStroke);
  end;
  {$IFDEF LCLLIB}
  FreeAndNil(FImage);
  {$ENDIF}
  frm.Free;
  {$ELSE}
  frm.ShowModal(
  procedure(AResult: TModalResult)
  begin
    if (AResult = mrOk) and Assigned(Stroke) then
    begin
      Stroke.Assign(nil);
      Stroke.Assign(FCopyStroke);
    end;
    frm := nil;
    if Assigned(AProc) then
      AProc(AResult);
  end);
  {$ENDIF}
end;

function TTMSFNCGraphicsStrokeEditor.GetInstance: NativeUInt;
begin
  Result := HInstance;
end;

procedure TTMSFNCGraphicsStrokeEditor.SetStroke(const Value: TTMSFNCGraphicsStroke);
begin
  FStroke := Value;
end;

procedure TTMSFNCGraphicsStrokeEditor.SetStrokeValues;
begin
  if Assigned(FCopyStroke) and Assigned(FKindLbl) then
  begin
    {$IFDEF FMXLIB}
    FColP.Color := FCopyStroke.Color;
    FOpacityTB.Value := Round(FCopyStroke.Opacity * 100);
    FWidthTB.Value := Round(FCopyStroke.Width * 2);
    FWidthValLbl.Text := FloatToStr(FCopyStroke.Width);
    {$ENDIF}
    {$IFNDEF FMXLIB}
    FColP.Selected := FCopyStroke.Color;
    FOpacityTB.Position := Round(FCopyStroke.Opacity * 100);
    FWidthTB.Position := Round(FCopyStroke.Width * 2);
    FWidthValLbl.Caption := FloatToStr(FCopyStroke.Width);
    {$ENDIF}
    SetKindValue;
  end;
end;

procedure TTMSFNCGraphicsStrokeEditor.SetKindValue;
begin
  case FCopyStroke.Kind of
    gskSolid: FKindCB.ItemIndex := 1;
    gskDash: FKindCB.ItemIndex := 2;
    gskDot: FKindCB.ItemIndex := 3;
    gskDashDot: FKindCB.ItemIndex := 4;
    gskDashDotDot: FKindCB.ItemIndex := 5;
    else
      FKindCB.ItemIndex := 0;
  end;
end;

procedure TTMSFNCGraphicsStrokeEditor.SetPreviewBackGroundColor(const Value: TTMSFNCGraphicsColor);
begin
  if FPreviewBackGroundColor <> Value then
    FPreviewBackGroundColor := Value;
end;

procedure TTMSFNCGraphicsStrokeEditor.UpdateControlPos;
var
  marg, low: Integer;
begin
  marg := 5;
  if Assigned(frm) then
  begin
    FTopPanel.Width := FVScrlBox.Width;

    FImage.Width := FTopPanel.Width - 2 * marg;
    {$IFDEF FMXLIB}
    FImage.Position.X := marg;
    FImage.Position.Y := marg;
    {$ENDIF}
    {$IFNDEF FMXLIB}
    FImage.Left := marg;
    FImage.Top := marg;
    {$ENDIF}

    {$IFDEF FMXLIB}
    FKindLbl.Position.X := marg;
    FKindLbl.Position.Y := FImage.Position.Y + FImage.Height + marg;
    {$ENDIF}
    {$IFNDEF FMXLIB}
    FKindLbl.Left := marg;
    FKindLbl.Top := FImage.Top + FImage.Height + marg;
    {$ENDIF}

    FKindCB.Width :=  Min(200, (FImage.Width - 3 * marg));
    {$IFDEF FMXLIB}
    FKindCB.Position.X := FTopPanel.Width - marg - FKindCB.Width;
    FKindCB.Position.Y := FKindLbl.Position.Y + FKindLbl.Height + marg;
    {$ENDIF}
    {$IFNDEF FMXLIB}
    FKindCB.Left := FTopPanel.Width - marg - FKindCB.Width;
    FKindCB.Top := FKindLbl.Top + FKindLbl.Height + marg;
    {$ENDIF}

    {$IFDEF FMXLIB}
    FColLbl.Position.X := FKindLbl.Position.X;
    FColLbl.Position.Y := FKindCB.Position.Y + FKindCB.Height + marg;
    {$ENDIF}
    {$IFNDEF FMXLIB}
    FColLbl.Left := FKindLbl.Left;
    FColLbl.Top := FKindCB.Top + FKindCB.Height + marg;
    {$ENDIF}

    FColP.Width := FKindCB.Width;
    {$IFDEF FMXLIB}
    FColP.Position.X := FKindCB.Position.X;
    FColP.Position.Y := FColLbl.Position.Y + FColLbl.Height + marg;
    {$ENDIF}
    {$IFNDEF FMXLIB}
    FColP.Left := FKindCB.Left;
    FColP.Top := FColLbl.Top + FColLbl.Height + marg;
    {$ENDIF}

    {$IFDEF FMXLIB}
    FOpacityLbl.Position.X := FColLbl.Position.X;
    FOpacityLbl.Position.Y := FColP.Position.Y + FColP.Height + marg;
    {$ENDIF}
    {$IFNDEF FMXLIB}
    FOpacityLbl.Left := FColLbl.Left;
    FOpacityLbl.Top := FColP.Top + FColP.Height + marg;
    {$ENDIF}

    FOpacityTB.Width := FKindCB.Width;
    {$IFDEF FMXLIB}
    FOpacityTB.Position.X := FColP.Position.X;
    FOpacityTB.Position.Y := FOpacityLbl.Position.Y + FOpacityLbl.Height + marg;
    {$ENDIF}
    {$IFNDEF FMXLIB}
    FOpacityTB.Left := FColP.Left;
    FOpacityTB.Top := FOpacityLbl.Top + FOpacityLbl.Height + marg;
    {$ENDIF}

    {$IFDEF FMXLIB}
    FWidthLbl.Position.X := FColLbl.Position.X;
    FWidthLbl.Position.Y := FOpacityTB.Position.Y + FOpacityTB.Height + marg;
    {$ENDIF}
    {$IFNDEF FMXLIB}
    FWidthLbl.Left := FColLbl.Left;
    FWidthLbl.Top := FOpacityTB.Top + FOpacityTB.Height + marg;
    {$ENDIF}

    {$IFDEF FMXLIB}
    FWidthValLbl.Position.X := FOpacityTB.Position.X + FOpacityTB.Width - FWidthValLbl.Width;
    FWidthValLbl.Position.Y := FWidthLbl.Position.Y;
    {$ENDIF}
    {$IFNDEF FMXLIB}
    FWidthValLbl.Left := FOpacityTB.Left + FOpacityTB.Width - FWidthValLbl.Width;
    FWidthValLbl.Top := FWidthLbl.Top;
    {$ENDIF}

    FWidthTB.Width := FOpacityTB.Width;
    {$IFDEF FMXLIB}
    FWidthTB.Position.X := FColP.Position.X;
    FWidthTB.Position.Y := FWidthLbl.Position.Y + FWidthLbl.Height + marg;
    {$ENDIF}
    {$IFNDEF FMXLIB}
    FWidthTB.Left := FColP.Left;
    FWidthTB.Top := FWidthLbl.Top + FWidthLbl.Height + marg;
    {$ENDIF}

    {$IFDEF FMXLIB}
    low := Round(FWidthTB.Position.Y + FWidthTB.Height + marg);
    {$ENDIF}
    {$IFNDEF FMXLIB}
    low := FWidthTB.Top + FWidthTB.Height + marg;
    {$ENDIF}
    FTopPanel.Height := Max(low, FVScrlBox.Height - marg);
  end;
end;

{$IFDEF WEBLIB}

{ TTMSFNCGraphicsStrokeEditorForm }

constructor TTMSFNCGraphicsStrokeEditorForm.CreateDialogNew(AStrokeEditor: TTMSFNCGraphicsStrokeEditor; ACaption: string);
begin
  FStrokeEditor := AStrokeEditor;
  inherited CreateDialogNew(ACaption);
end;

procedure TTMSFNCGraphicsStrokeEditorForm.LoadDFMValues;
begin
  inherited LoadDFMValues;
  if Assigned(FStrokeEditor) then
  begin
    FStrokeEditor.BuildEditor(Self);
    ActiveControl := FStrokeEditor.FTopPanel;
  end;
end;

procedure TTMSFNCGraphicsStrokeEditorForm.Loaded;
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
  end;
end;

{$ENDIF}


end.

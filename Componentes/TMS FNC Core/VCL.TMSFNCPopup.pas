{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{            copyright (c) 2016 - 2023                               }
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

unit VCL.TMSFNCPopup;

{$I VCL.TMSFNCDefines.inc}

{$IFDEF CMNLIB}
{$DEFINE CMNWEBLIB}
{$ENDIF}
{$IFDEF WEBLIB}
{$DEFINE CMNWEBLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}
{$IFDEF LCLLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  Classes, VCL.Controls, VCL.TMSFNCCustomComponent, VCL.TMSFNCGraphics, VCL.Graphics,
  VCL.TMSFNCTypes, VCL.TMSFNCGraphicsTypes, VCL.Forms, VCL.ExtCtrls
  {$IFNDEF LCLLIB}
  ,Types
  {$IFNDEF WEBLIB}
  {$HINTS OFF}
  {$IF COMPILERVERSION > 22}
  ,UITypes
  {$IFEND}
  {$HINTS ON}
  {$ENDIF}
  {$ENDIF}
  {$IFDEF FMXLIB}
  ,FMX.Types
  {$ENDIF}
  {$IFDEF VCLLIB}
  ,Messages
  {$ENDIF}
  {$IFDEF LCLLIB}
  {$IFNDEF MSWINDOWS}
  ,LMessages
  {$ENDIF}
  ,LCLType, LCLIntf
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 5; // Build nr.

  // version history
  // v1.0.0.0 : first release
  // v1.0.0.1 : Fixed: Issue with popup on Android
  // v1.0.0.2 : Fixed: Issue with multi-monitor screen detection
  // v1.0.0.3 : Fixed: Issue with multi-moniutor screen detection on LCL and VCL
  // v1.0.0.4 : Fixed: Issue with BeginScene error invoked when navigating away from the application
  // v1.0.0.5 : Fixed: Issue with retrieving parent in VCL

type
  TTMSFNCPopupPlacement = (ppBottom, ppTop, ppLeft, ppRight, ppCenter, ppBottomCenter, ppTopCenter, ppLeftCenter, ppRightCenter, ppAbsolute,
    ppMouse, ppMouseCenter, ppAboveMouse, ppAboveMouseCenter);

  TTMSFNCPopupPaint = procedure(Sender: TObject; AGraphics: TTMSFNCGraphics; ARect: TRectF) of object;

  TTMSFNCCustomPopupFormClass = class of TTMSFNCCustomPopupForm;

  TTMSFNCCustomPopup = class;

  TTMSFNCCustomPopupForm = class(TCustomForm)
  private
    FPreferedDisplayIndex: Integer;
    FHintWindow: Boolean;
    FTimer: TTimer;
    FFirstShow: Boolean;
    FOwner: TComponent;
    FShowModal: Boolean;
    FPlacement: TTMSFNCPopupPlacement;
    FRealPlacement: TTMSFNCPopupPlacement;
    FPlacementControl: TControl;
    FOffset: TPointF;
    FSize: TSizeF;
    FPlacementRectangle: TTMSFNCMargins;
    FScreenPlacementRect: TRectF;
    FPlacementChanged: Boolean;
    FDragWithParent: Boolean;
    FOnBeforeClose: TNotifyEvent;
    FOnBeforeShow: TNotifyEvent;
    FScreenContentRect: TRectF;
    FContentPadding: TTMSFNCMargins;
    FContentControl: TControl;
    FOnRealPlacementChanged: TNotifyEvent;
    FOnPopupPaint: TTMSFNCPopupPaint;
    procedure SetOffset(const Value: TPointF);
    procedure SetSize(const Value: TSizeF);
    procedure TimerProc(Sender: TObject);
    procedure SetPlacementRectangle(const Value: TTMSFNCMargins);
    procedure SetPlacement(const Value: TTMSFNCPopupPlacement);
    procedure SetPlacementControl(const Value: TControl);
    procedure SetDragWithParent(const Value: Boolean);
    procedure SetContentPadding(const Value: TTMSFNCMargins);
    procedure SetContentControl(const Value: TControl);
    function GetPopup: TTMSFNCCustomPopup;
  protected
    procedure DoBeforeShow; virtual;
    procedure DoBeforeClose; virtual;
    procedure UpdateBounds(LRect: TRectF); virtual;
    procedure DoClose(var CloseAction: TCloseAction); override;
    procedure DoApplyPlacement; virtual;
    procedure Loaded; override;
    procedure Updated; override;
    procedure HandleFocusedControl;
    function GetPopupParent: TControl;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoRealPlacementChanged; virtual;
    property DragWithParent: Boolean read FDragWithParent write SetDragWithParent;
    property Popup: TTMSFNCCustomPopup read GetPopup;
  public
    {$IFDEF CMNWEBLIB}
    constructor CreateNew(AOwner: TComponent; Dummy: Integer  = 0); override;
    {$ENDIF}
    {$IFDEF FMXLIB}
    constructor CreateNew(AOwner: TComponent; Dummy: NativeInt = 0); override;
    {$ENDIF}
    constructor Create(AOwner: TComponent; APlacementControl: TControl = nil); reintroduce;
    destructor Destroy; override;
    procedure ApplyPlacement; virtual;
    procedure KeyDown(var Key: Word; {$IFDEF FMXLIB}var KeyChar: WideChar;{$ENDIF} Shift: TShiftState); override;
    function CloseQuery: Boolean; override;
    property HintWindow: Boolean read FHintWindow write FHintWindow;
    property ContentControl: TControl read FContentControl write SetContentControl;
    property ContentPadding: TTMSFNCMargins read FContentPadding write SetContentPadding;
    property Offset: TPointF read FOffset write SetOffset;
    property Placement: TTMSFNCPopupPlacement read FPlacement write SetPlacement;
    property PlacementRectangle: TTMSFNCMargins read FPlacementRectangle write SetPlacementRectangle;
    property PlacementControl: TControl read FPlacementControl write SetPlacementControl;
    property RealPlacement: TTMSFNCPopupPlacement read FRealPlacement;
    property ScreenContentRect: TRectF read FScreenContentRect;
    property ScreenPlacementRect: TRectF read FScreenPlacementRect;
    property Size: TSizeF read FSize write SetSize;
    property OnBeforeShow: TNotifyEvent read FOnBeforeShow write FOnBeforeShow;
    property OnBeforeClose: TNotifyEvent read FOnBeforeClose write FOnBeforeClose;
    property OnRealPlacementChanged: TNotifyEvent read FOnRealPlacementChanged write FOnRealPlacementChanged;
    property OnPopupPaint: TTMSFNCPopupPaint read FOnPopupPaint write FOnPopupPaint;
  end;

  TTMSFNCCustomPopup = class(TTMSFNCCustomComponent)
  private
    FCloseTime: DWORD;
    FCheckTime: Boolean;
    FDestroyingPopup: Boolean;
    {$IFDEF FMXLIB}
    FPopup: TPopup;
    {$ENDIF}
    FOwner: TComponent;
    FCustomOwner: TComponent;
    FPlacementControl: TControl;
    FPopupForm: TTMSFNCCustomPopupForm;
    FStaysOpen: Boolean;
    FPlacement: TTMSFNCPopupPlacement;
    FPlacementRectangle: TTMSFNCMargins;
    FHorizontalOffset: Single;
    FVerticalOffset: Single;
    FDragWithParent: Boolean;
    FModalResult: TModalResult;
    FModal: Boolean;
    FOnClosePopup: TNotifyEvent;
    FOnPopup: TNotifyEvent;
    FPopupFormSize: TSizeF;
    FContentControl: TControl;
    FDropDownHeight: Single;
    FDropDownWidth: Single;
    FFocusable: Boolean;
    FFocusedControl: TControl;
    FOnPopupPaint: TTMSFNCPopupPaint;
    FOnPopupShown: TNotifyEvent;
    FIsOpen: Boolean;
    FFillColor: TTMSFNCGraphicsColor;
    FStrokeColor: TTMSFNCGraphicsColor;
    procedure SetIsOpen(const Value: Boolean);
    procedure SetPlacementRectangle(const Value: TTMSFNCMargins);
    procedure SetModalResult(const Value: TModalResult);
    procedure SetPlacementControl(const Value: TControl);
    procedure SetPlacement(const Value: TTMSFNCPopupPlacement);
    procedure SetDragWithParent(const Value: Boolean);
    procedure BeforeShowProc(Sender: TObject);
    procedure BeforeCloseProc(Sender: TObject);
    procedure CloseProc(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure ShowProc(Sender: TObject);
    procedure DeactivateProc(Sender: TObject);
    procedure SetPopupFormSize(const Value: TSizeF);
    procedure UpdatePopupSize;
    procedure SetContentControl(const Value: TControl);
    procedure SetDropDownHeight(const Value: Single);
    procedure SetDropDownWidth(const Value: Single);
  protected
    function GetInstance: NativeUInt; override;
    function GetDocURL: string; override;
    function GetVersion: String; override;
    {$IFDEF FNCLIB}
    procedure SetAdaptToStyle(const Value: Boolean); override;
    {$ENDIF}
    procedure ShowPopup(AModal: Boolean); virtual;
    procedure HandleFocusedControl;
    {$IFDEF FMXLIB}
    procedure FormPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    procedure FormPaint(Sender: TObject);
    {$ENDIF}
    procedure DoPopupPaint(Sender: TObject; AGraphics: TTMSFNCGraphics; ARect: TRectF); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure MarginsChanged(Sender: TObject);
    procedure DoClosePopup; virtual;
    procedure DoPopup; virtual;
    procedure DoPopupShown; virtual;
    procedure ClosePopup; virtual;
    procedure DoCreatePopup(const AShowModal: Boolean = False); virtual;
    function GetPopupFormClass: TTMSFNCCustomPopupFormClass; virtual;
    function GetParent: TControl;
    function CreatePopupForm: TTMSFNCCustomPopupForm; virtual;
    property IsOpen: Boolean read FIsOpen write SetIsOpen;
    property ModalResult: TModalResult read FModalResult write SetModalResult;
    property PopupFormSize: TSizeF read FPopupFormSize write SetPopupFormSize;
    property DragWithParent: Boolean read FDragWithParent write SetDragWithParent default False;
    property PopupForm: TTMSFNCCustomPopupForm read FPopupForm;
    property StaysOpen: Boolean read FStaysOpen write FStaysOpen default False;
    property DropDownHeight: Single read FDropDownHeight write SetDropDownHeight;
    property DropDownWidth: Single read FDropDownWidth write SetDropDownWidth;
    property HorizontalOffset: Single read FHorizontalOffset write FHorizontalOffset;
    property Placement: TTMSFNCPopupPlacement read FPlacement write SetPlacement default ppBottom;
    property PlacementRectangle: TTMSFNCMargins read FPlacementRectangle write SetPlacementRectangle;
    property PlacementControl: TControl read FPlacementControl write SetPlacementControl;
    property VerticalOffset: Single read FVerticalOffset write FVerticalOffset;
    property FocusedControl: TControl read FFocusedControl write FFocusedControl;
    property ContentControl: TControl read FContentControl write SetContentControl;
    property OnClosePopup: TNotifyEvent read FOnClosePopup write FOnClosePopup;
    property OnPopupPaint: TTMSFNCPopupPaint read FOnPopupPaint write FOnPopupPaint;
    property OnPopup: TNotifyEvent read FOnPopup write FOnPopup;
    property CustomOwner: TComponent read FCustomOwner write FCustomOwner;
    property OnPopupShown: TNotifyEvent read FOnPopupShown write FOnPopupShown;
    property Version: String read GetVersion;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
    destructor Destroy; override;
    function HasPopupForm: Boolean;
    function PointInPopup(APoint: TPointF): Boolean; virtual;
    property FillColor: TTMSFNCGraphicsColor read FFillColor write FFillColor default gcNull;
    property StrokeColor: TTMSFNCGraphicsColor read FStrokeColor write FStrokeColor default gcNull;
  end;

  {$IFNDEF LCLLIB}
  [ComponentPlatformsAttribute(TMSPlatformsWeb)]
  {$ENDIF}
  TTMSFNCPopup = class(TTMSFNCCustomPopup)
  protected
    procedure RegisterRuntimeClasses; override;
  public
    function PopupModal: TModalResult; virtual;
    procedure Popup(const AShowModal: Boolean = False); virtual;
    property AdaptToStyle;
    property ModalResult;
    property IsOpen;
    property PopupFormSize;
    property DragWithParent;
    property PopupForm;
  published
    property StaysOpen;
    property DropDownHeight;
    property DropDownWidth;
    property HorizontalOffset;
    property Placement;
    property PlacementRectangle;
    property PlacementControl;
    property VerticalOffset;
    property FocusedControl;
    property ContentControl;
    property OnClosePopup;
    property OnPopupPaint;
    property OnPopup;
    property OnPopupShown;
    property Version;
  end;

{$IFDEF CMNLIB}
{$IFDEF MSWINDOWS}
type
  LInteger = LONG_PTR;
  IntPtr = Pointer;
{$ENDIF}
{$ENDIF}

type
  TTMSFNCCustomNonFocusablePopupForm = class(TTMSFNCCustomPopupForm)
  private
    {$IFDEF CMNLIB}
    {$IFDEF MSWINDOWS}
    procedure WMActivate(var Message: TWMActivate); message WM_ACTIVATE;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    {$ELSE}
    procedure WMActivate(var Message: TLMActivate); message LM_ACTIVATE;
    procedure WMNCHitTest(var Message: TLMessage); message LM_NCHITTEST;
    {$ENDIF}
    {$ENDIF}
  protected
    procedure UpdateBounds(LRect: TRectF); override;
    {$IFDEF CMNLIB}
    {$IFDEF MSWINDOWS}
    procedure CreateParams(var Params: TCreateParams); override;
    {$ENDIF}
    {$ENDIF}
  public
    {$IFDEF CMNLIB}
    {$IFDEF MSWINDOWS}
    constructor CreateNew(AOwner: TComponent; Dummy: Integer  = 0); override;
    destructor Destroy; override;
    {$ENDIF}
    {$ENDIF}
  end;

  TTMSFNCCustomNonFocusablePopup = class(TTMSFNCCustomPopup)
  private
    {$IFDEF CMNLIB}
    FActiveWindow: HWND;
    {$ENDIF}
  protected
    procedure ActivatePreviousWindow; virtual;
    procedure ShowPopup({%H-}AModal: Boolean); override;
    function GetPopupFormClass: TTMSFNCCustomPopupFormClass; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TTMSFNCNonFocusablePopup = class(TTMSFNCCustomNonFocusablePopup)
  public
    procedure Deactivate; virtual;
    procedure Activate; virtual;
  published
    property ContentControl;
    property Placement;
    property PlacementRectangle;
    property PlacementControl;
  end;

implementation

uses
  SysUtils, VCL.TMSFNCUtils, Math
  {$IFDEF FNCLIB}
  , VCL.TMSFNCStyles
  {$ENDIF}
  {$IFDEF WEBLIB}
  , Web
  {$ENDIF}
  {$IFDEF FMXLIB}
  , FMX.Platform
  {$ENDIF}
  ;

{$R TMSFNCPopup.res}

{$IFDEF CMNLIB}
{$IFDEF MSWINDOWS}
type
  WndProcRec = record
    FWindowHandle: HWND;
    FProc: LInteger;
  end;

var
  PrevWndProcs: array of WndProcRec;
{$ENDIF}
{$ENDIF}

function GetTickCountX: DWORD;
var
  h, m, s, ms: Word;
begin
  DecodeTime(Now, h, m, s, ms);
  Result := ms + s * 1000 + m * 60 * 1000 + h * 60 * 60 * 1000;
end;

{ TTMSFNCPopup }

constructor TTMSFNCCustomPopup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOwner := AOwner;
  FPlacement := ppBottom;
  FFocusable := True;
  FStaysOpen := False;
  FPlacementRectangle := TTMSFNCMargins.Create;
  FPlacementRectangle.OnChange := MarginsChanged;
  FFillColor := gcNull;
  FStrokeColor := gcNull;
end;

destructor TTMSFNCCustomPopup.Destroy;
begin
  FContentControl := nil;
  ClosePopup;
  if HasPopupForm then
  begin
    FreeAndNil(FPopupForm);
  end;
  FreeAndNil(FPlacementRectangle);
  {$IFDEF FMXLIB}
  FreeAndNil(FPopup);
  {$ENDIF}
  inherited;
end;

procedure TTMSFNCCustomPopup.DeactivateProc(Sender: TObject);
{$IFNDEF LCLWEBLIB}
var
  I: Integer;
  o: TComponent;
  f: TCustomForm;
{$ENDIF}
begin
  if csDestroying in ComponentState then
    Exit;

  if Assigned(FPopupForm) and not FPopupForm.FShowModal and not StaysOpen then
  begin
    {$IFDEF FMXLIB}
    for I := 0 to Screen.FormCount - 1 do
    begin
      if (Screen.Forms[I] is TTMSFNCCustomPopupForm) then
      begin
        o := (Screen.Forms[I] as TTMSFNCCustomPopupForm).Popup.Owner;
        if Assigned(o) and (o is TControl) then
        begin
          f := TTMSFNCUtils.GetParentForm((o as TControl).Parent);

          if f = FPopupForm then
            Exit;
        end;
      end;
    end;

    {$ENDIF}
    {$IFDEF VCLLIB}
    for I := 0 to Screen.CustomFormCount - 1 do
    begin
      if (Screen.CustomForms[I] is TTMSFNCCustomPopupForm) then
      begin
        o := (Screen.CustomForms[I] as TTMSFNCCustomPopupForm).Popup.Owner;
        f := nil;
        if Assigned(o) and (o is TControl) then
        begin
          if o.HasParent then
            f := TTMSFNCUtils.GetParentForm((o as TControl).Parent)
          else if (o is TCustomForm) then
            f := TCustomForm(o);

          if f = FPopupForm then
            Exit;
        end;
      end;
    end;
    {$ENDIF}

    FPopupForm.Visible := False;
    FPopupForm.Close;
    FCloseTime := GetTickCountX;
    FCheckTime := True;
  end;
end;

procedure TTMSFNCCustomPopup.HandleFocusedControl;
var
  frm: TCustomForm;
begin
  if Assigned(FPopupForm) and Assigned(FocusedControl) then
  begin
    if not FPopupForm.FShowModal then
    begin
      frm := TTMSFNCUtils.GetParentForm(FocusedControl);
      if Assigned(frm) and (frm <> FPopupForm) then
      begin
        {$IFDEF FMXLIB}
        frm.Activate;
        {$ENDIF}
        {$IFDEF CMNWEBLIB}
        frm.SetFocus;
        {$ENDIF}
      end;
    end;

    {$IFDEF FMXLIB}
    if FocusedControl.CanFocus then
      FocusedControl.SetFocus;
    {$ENDIF}
    {$IFDEF CMNLIB}
    if FocusedControl is TWinControl then
    begin
      if (FocusedControl as TWinControl).CanFocus then
        (FocusedControl as TWinControl).SetFocus;
    end;
    {$ENDIF}
    {$IFDEF WEBLIB}
    if FocusedControl is TControl then
    begin
      if (FocusedControl as TControl).CanFocus then
        (FocusedControl as TControl).SetFocus;
    end;
    {$ENDIF}
  end;
end;

function TTMSFNCCustomPopup.HasPopupForm: Boolean;
begin
  Result := FPopupForm <> nil;
end;

procedure TTMSFNCCustomPopup.MarginsChanged(Sender: TObject);
begin
  if (FPopupForm <> nil) then
    FPopupForm.PlacementRectangle := PlacementRectangle;
end;

function TTMSFNCCustomPopup.CreatePopupForm: TTMSFNCCustomPopupForm;
var
  NewForm: TTMSFNCCustomPopupForm;
  cls: TTMSFNCCustomPopupFormClass;
begin
  NewForm := nil;
  try
    cls := GetPopupFormClass;
    {$IFDEF FMXLIB}
    FPopup := TPopup.Create(Self);
    NewForm := cls.Create(FPopup, PlacementControl);
    {$ELSE}
    NewForm := cls.Create(Self, PlacementControl);
    {$ENDIF}
  except
    FreeAndNil(NewForm);
    Raise;
  end;
  Result := NewForm;
end;

procedure TTMSFNCPopup.Popup(const AShowModal: Boolean = False);
begin
  DoCreatePopup(AShowModal);
end;

procedure TTMSFNCCustomPopup.SetModalResult(const Value: TModalResult);
begin
  FModalResult := Value;
  if Assigned(FPopupForm) then
    FPopupForm.ModalResult := FModalResult;
end;

function TTMSFNCPopup.PopupModal: TModalResult;
var
  LStaysOpen : Boolean;
begin
  Result := 0;
  if FIsOpen then
    Exit;

  if HasPopupForm then
    FPopupForm.Close;

  LStaysOpen := FStaysOpen;
  try
    FStaysOpen := True;
    Popup(True);
    Result := FModalResult;
    IsOpen := False;
  finally
    FStaysOpen := LStaysOpen;
  end;
end;

procedure TTMSFNCPopup.RegisterRuntimeClasses;
begin
  inherited;
  RegisterClass(TTMSFNCPopup);
end;

procedure TTMSFNCCustomPopup.ClosePopup;
var
  p: TControl;
begin
  if not HasPopupForm or FDestroyingPopup then
    Exit;

  FDestroyingPopup := True;
  if FModal and (FModalResult = 0) then
  begin
    ModalResult := mrCancel;
    Exit;
  end;

  FIsOpen := False;
  if Assigned(FContentControl) then
  begin
    p := GetParent;
    FContentControl.Visible := False;
    {$IFDEF FMXLIB}
    FContentControl.Parent := p;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    if Assigned(p) and (p is TWinControl) then
      FContentControl.Parent := p as TWinControl
    else
      FContentControl.Parent := TTMSFNCUtils.GetParentForm(Self)
    {$ENDIF}
  end;

  if not (csDestroying in ComponentState) then
  begin
    if HasPopupForm then
    begin
      if Self is TTMSFNCNonFocusablePopup then
        FPopupForm.Free;
      DoClosePopup;
      FPopupForm := nil;
    end;
  end;
  FDestroyingPopup := False;
end;

procedure TTMSFNCCustomPopup.DoClosePopup;
begin
  if Assigned(FOnClosePopup) then
    FOnClosePopup(Self);
end;

procedure TTMSFNCCustomPopup.DoCreatePopup(const AShowModal: Boolean = False);
{$IFDEF FNCLIB}
var
  ia: ITMSFNCAdaptToStyle;
{$ENDIF}
begin
  if ((GetTickCountX - FCloseTime) < 200) and FCheckTime then
  begin
    FCheckTime := False;
    Exit;
  end;

  if FIsOpen then
    Exit;

  if HasPopupForm then
    FPopupForm.Close;

  FPopupForm := CreatePopupForm;
  try
    try
      {$IFDEF FMXLIB}
      {$IFDEF FMXMOBILE}
      if AShowModal then
        FPopupForm.FormStyle := TFormStyle.Normal;
      {$ENDIF}
      {$IFNDEF FMXMOBILE}
      {$IFDEF LINUX}
      FPopupForm.FormStyle := TFormStyle.Popup;
      {$ELSE}
      FPopupForm.FormStyle := TFormStyle.Normal;
      {$ENDIF}
      {$ENDIF}
      if Assigned(CustomOwner) and (CustomOwner is TFmxObject) then
        FPopupForm.Parent := (CustomOwner as TFmxObject)
      else if Assigned(Owner) and (Owner is TFmxObject) then
        FPopupForm.Parent := TFmxObject(Owner);
      {$ENDIF}

      {$IFDEF CMNWEBLIB}
      FPopupForm.FormStyle := fsStayOnTop;
      {$ENDIF}

      FPopupForm.OnPaint := FormPaint;
      FPopupForm.OnPopupPaint := DoPopupPaint;

      TComponent(FPopupForm).FreeNotification(Self);
      FPopupForm.Placement := Placement;
      FPopupForm.Offset := PointF(Self.HorizontalOffset, Self.VerticalOffset);
      FPopupForm.PlacementRectangle := Self.PlacementRectangle;
      FPopupForm.DragWithParent := DragWithParent;
      UpdatePopupSize;
      FPopupForm.OnDeactivate := DeactivateProc;
      FPopupForm.OnBeforeShow := BeforeShowProc;
      FPopupForm.OnBeforeClose := BeforeCloseProc;
      FPopupForm.OnClose := CloseProc;
      FPopupForm.OnShow := ShowProc;
      FPopupForm.FShowModal := AShowModal;

      TTMSFNCUtils.ScaleForCurrentDPI(FPopupForm);

      FPopupForm.ContentControl := FContentControl;
      {$IFDEF FNCLIB}
      if Supports(FContentControl, ITMSFNCAdaptToStyle, ia) then
        ia.AdaptToStyle := AdaptToStyle;
      {$ENDIF}
      {$IFDEF FMXLIB}
      if Assigned(FContentControl) then
        FContentControl.Align := TAlignLayout.Client;
      {$ENDIF}
      {$IFDEF CMNWEBLIB}
      if Assigned(FContentControl) then
        FContentControl.Align := alClient;
      {$ENDIF}
    finally
    end;
  except
    FreeAndNil(FPopupForm);
    Raise;
  end;

  if (not (csDestroying in ComponentState)) then
    FPopupForm.DoBeforeShow;

  ShowPopup(AShowModal);
end;

procedure TTMSFNCCustomPopup.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = FContentControl then
      FContentControl := nil;

    if AComponent = FFocusedControl then
      FFocusedControl := nil;

    if (AComponent = FPopupForm) then
      FPopupForm := nil;
    if (AComponent = FPlacementControl) then
    begin
      FPlacementControl := nil;
      if HasPopupForm then
        FPopupForm.PlacementControl := nil;
    end;
  end;
end;

function TTMSFNCCustomPopup.PointInPopup(APoint: TPointF): Boolean;
begin
  Result := False;
  if Assigned(FPopupForm) then
    Result := PtInRectEx(RectF(FPopupForm.Left, FPopupForm.Top, FPopupForm.Left + FPopupForm.Width, FPopupForm.Top + FPopupForm.Height), APoint);
end;

procedure TTMSFNCCustomPopup.SetIsOpen(const Value: Boolean);
var
  b: Boolean;
begin
  if FIsOpen <> Value then
  begin
    b := False;
    {$IFDEF WEBLIB}
    asm
      b = (pas["WEBLib.Forms"].VSIDE != null);
    end;
    {$ENDIF}

    if not b and (csDesigning in ComponentState) then
    begin
      FIsOpen := False;
      Exit;
    end;

    if Value then
      DoCreatePopup
    else
    begin
      if HasPopupForm then
        FPopupForm.Close
      else
        FIsOpen := Value;
    end;
  end;
end;

procedure TTMSFNCCustomPopup.BeforeShowProc(Sender: TObject);
begin
  FIsOpen := True;
  DoPopup;
end;

procedure TTMSFNCCustomPopup.UpdatePopupSize;
var
  LSize: TSizeF;
begin
  if FPopupForm <> nil then
  begin
    if (FPopupFormSize.cx > 0) then
      LSize.cx := FPopupFormSize.cx
    else
      LSize.cx := DropDownWidth;
    if (FPopupFormSize.cy > 0) then
      LSize.cy := FPopupFormSize.cy
    else
      LSize.cy := DropDownHeight;

    FPopupForm.Size := LSize;
  end;
end;

procedure TTMSFNCCustomPopup.Assign(Source: TPersistent);
begin
  if Source is TTMSFNCCustomPopup then
  begin
    FModalResult := (Source as TTMSFNCCustomPopup).ModalResult;
    FPopupFormSize := (Source as TTMSFNCCustomPopup).PopupFormSize;
    FDragWithParent := (Source as TTMSFNCCustomPopup).DragWithParent;
    FStaysOpen := (Source as TTMSFNCCustomPopup).StaysOpen;
    FDropDownHeight := (Source as TTMSFNCCustomPopup).DropDownHeight;
    FDropDownWidth := (Source as TTMSFNCCustomPopup).DropDownWidth;
    FHorizontalOffset := (Source as TTMSFNCCustomPopup).HorizontalOffset;
    FPlacementControl := (Source as TTMSFNCCustomPopup).PlacementControl;
    FPlacement := (Source as TTMSFNCCustomPopup).Placement;
    FPlacementRectangle.Assign((Source as TTMSFNCCustomPopup).PlacementRectangle);
    FVerticalOffset := (Source as TTMSFNCCustomPopup).VerticalOffset;
    FFocusedControl := (Source as TTMSFNCCustomPopup).FocusedControl;
    FContentControl := (Source as TTMSFNCCustomPopup).ContentControl;
  end;
end;

procedure TTMSFNCCustomPopup.BeforeCloseProc(Sender: TObject);
begin
  FIsOpen := False;
end;

procedure TTMSFNCCustomPopup.CloseProc(Sender: TObject; var CloseAction: TCloseAction);
begin
  {$IFDEF FMXLIB}
  if Assigned(PopupForm) and (PopupForm.FormStyle = TFormStyle.Popup) then
  begin
    if not StaysOpen then
      ClosePopup
    else
      CloseAction := TCloseAction.caNone;
  end
  else
    ClosePopup;
  {$ELSE}
  ClosePopup;
  {$ENDIF}
end;

procedure TTMSFNCCustomPopup.DoPopup;
begin
  if Assigned(FOnPopup) then
    FOnPopup(Self);
end;

procedure TTMSFNCCustomPopup.DoPopupPaint(Sender: TObject; AGraphics: TTMSFNCGraphics;
  ARect: TRectF);
begin
  if Assigned(OnPopupPaint) then
    OnPopupPaint(Self, AGraphics, ARect);
end;

procedure TTMSFNCCustomPopup.DoPopupShown;
begin
  if Assigned(FOnPopupShown) then
    FOnPopupShown(Self);
end;

{$IFDEF FMXLIB}
procedure TTMSFNCCustomPopup.FormPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
{$ENDIF}
{$IFDEF CMNWEBLIB}
procedure TTMSFNCCustomPopup.FormPaint(Sender: TObject);
{$ENDIF}
var
  g: TTMSFNCGraphics;
  r: TRectF;
begin
  if Assigned(FPopupForm) then
  begin
    g := TTMSFNCGraphics.Create(FPopupForm.Canvas);
    g.BeginScene;
    try
      g.Fill.Kind := gfkSolid;
      g.Stroke.Kind := gskSolid;
      if (FillColor <> gcNull) and (StrokeColor <> gcNull) then
      begin
        g.Fill.Color := FillColor;
        g.Stroke.Color := StrokeColor;
      end
      else
      begin
        g.Fill.Color := TTMSFNCGraphics.DefaultPopupFillColor;
        g.Stroke.Color := TTMSFNCGraphics.DefaultPopupStrokeColor;
      end;

      r := RectF(0, 0, FPopupForm.Width, FPopupForm.Height);
      g.DrawRectangle(r);

      if Assigned(OnPopupPaint) then
        OnPopupPaint(Self, g, r);
    finally
      g.EndScene;
      g.Free;
    end;
  end;
end;

function TTMSFNCCustomPopup.GetDocURL: string;
begin
  Result := 'https://download.tmssoftware.com/doc/tmsfncuipack/components/ttmsfncpopup';
end;

function TTMSFNCCustomPopup.GetInstance: NativeUInt;
begin
  Result := HInstance;
end;

function TTMSFNCCustomPopup.GetParent: TControl;
begin
  Result := PlacementControl;
  if not Assigned(Result) and (FOwner is TControl) then
    Result := FOwner as TControl;
end;

function TTMSFNCCustomPopup.GetPopupFormClass: TTMSFNCCustomPopupFormClass;
begin
  Result := TTMSFNCCustomPopupForm;
end;

function TTMSFNCCustomPopup.GetVersion: String;
begin
  Result := GetVersionNumber(MAJ_VER, MIN_VER, REL_VER, BLD_VER);
end;

procedure TTMSFNCCustomPopup.SetPlacement(const Value: TTMSFNCPopupPlacement);
begin
  if FPlacement <> Value then
  begin
    FPlacement := Value;
    if HasPopupForm then
      PopupForm.Placement := FPlacement;
  end;
end;

procedure TTMSFNCCustomPopup.SetPlacementRectangle(const Value: TTMSFNCMargins);
begin
  FPlacementRectangle.Assign(Value);
end;

procedure TTMSFNCCustomPopup.SetPlacementControl(const Value: TControl);
begin
  if FPlacementControl <> Value then
  begin
    if FPlacementControl <> nil then
      FPlacementControl.RemoveFreeNotification(self);
    FPlacementControl := Value;
    if HasPopupForm then
      FPopupForm.PlacementControl := FPlacementControl;
    if FPlacementControl <> nil then
      FPlacementControl.FreeNotification(self);
  end;
end;

procedure TTMSFNCCustomPopup.SetPopupFormSize(const Value: TSizeF);
begin
  if (FPopupFormSize.cx <> Value.cx) or (FPopupFormSize.cy <> Value.cy) then
  begin
    FPopupFormSize := Value;
    UpdatePopupSize;
  end;
end;

procedure TTMSFNCCustomPopup.ShowPopup(AModal: Boolean);
begin
  if AModal then
    FModalResult := FPopupForm.ShowModal
  else
  begin
    FPopupForm.Show;
    {$IFDEF CMNLIB}
    {$IFNDEF MSWINDOWS}
    BorderStyle := bsNone;
    BorderStyle := bsSingle;
    {$ENDIF}
    {$ENDIF}
    HandleFocusedControl;
  end;
end;

procedure TTMSFNCCustomPopup.ShowProc(Sender: TObject);
begin
  if Assigned(FPopupForm) and FPopupForm.FShowModal then
    HandleFocusedControl;

  DoPopupShown;
end;

{$IFDEF FNCLIB}
procedure TTMSFNCCustomPopup.SetAdaptToStyle(const Value: Boolean);
var
  ia: ITMSFNCAdaptToStyle;
begin
  inherited;
  if Assigned(ContentControl) then
  begin
    if Supports(ContentControl, ITMSFNCAdaptToStyle, ia) then
      ia.AdaptToStyle := AdaptToStyle;
  end;
end;
{$ENDIF}

procedure TTMSFNCCustomPopup.SetContentControl(const Value: TControl);
begin
  if Value = nil then
  begin
    if Assigned(FContentControl) then
      FContentControl.Visible := True;
  end;

  FContentControl := Value;
  if Assigned(FContentControl) then
  begin
    FContentControl.Visible := False;
    DropDownWidth := FContentControl.Width;
    DropDownHeight := FContentControl.Height;
  end;
end;

procedure TTMSFNCCustomPopup.SetDragWithParent(const Value: Boolean);
begin
  if FDragWithParent <> Value then
  begin
    FDragWithParent := Value;
    if HasPopupForm then
      FPopupForm.DragWithParent := FDragWithParent;
  end;
end;

procedure TTMSFNCCustomPopup.SetDropDownHeight(const Value: Single);
begin
  if FDropDownHeight <> Value then
  begin
    FDropDownHeight := Value;
    if Assigned(PopupForm) then
      PopupForm.Height := Round(Value);
  end;
end;

procedure TTMSFNCCustomPopup.SetDropDownWidth(const Value: Single);
begin
  if FDropDownWidth <> Value then
  begin
    FDropDownWidth := Value;
    if Assigned(PopupForm) then
      PopupForm.Width := Round(Value);
  end;
end;

{ TTMSFNCCustomPopupForm }

constructor TTMSFNCCustomPopupForm.Create(AOwner: TComponent; APlacementControl: TControl = nil);
begin
  CreateNew(AOwner);
  try
    if APlacementControl <> nil then
      FPlacementControl := APlacementControl;
    if FPlacementControl <> nil then
      TComponent(FPlacementControl).FreeNotification(Self);
  finally
  end;
end;

{$IFDEF CMNWEBLIB}
constructor TTMSFNCCustomPopupForm.CreateNew(AOwner: TComponent; Dummy: Integer);
{$ENDIF}
{$IFDEF FMXLIB}
constructor TTMSFNCCustomPopupForm.CreateNew(AOwner: TComponent; Dummy: NativeInt);
{$ENDIF}
  function FindUniqueFormName(const Name: string): string;
  var
    I: Integer;
  begin
    I := 0;
    Result := Name;
    while (FindGlobalComponent(Result) <> nil) or
          ((AOwner <> nil) and (AOwner.FindComponent(Result) <> nil)) do
    begin
      Inc(I);
      Result := Format('%s_%d', [Name, I]);
    end;
  end;
begin
  Name := FindUniqueFormName('CustomPopupForm');
  inherited;
  FPreferedDisplayIndex := -1;
  FOwner := AOwner;
  FDragWithParent := True;
  FPlacementRectangle := TTMSFNCMargins.Create;
  FContentPadding := TTMSFNCMargins.Create;
  FSize.cx := 320;
  FSize.cy := 200;
  FPlacement := ppBottom;
  FRealPlacement := FPlacement;
  Visible := False;
  try
    {$IFDEF FMXLIB}
    BeginUpdate;
    {$IFDEF FMXMOBILE}
    if Assigned(FOwner) and (FOwner is TPopup) and ((FOwner as TPopup).Owner is TTMSFNCCustomPopup) then
      if not ((FOwner as TPopup).Owner as TTMSFNCCustomPopup).StaysOpen then
    {$ENDIF}
        FormStyle := TFormStyle.Popup;

    Position := TFormPosition.Designed;
    BorderStyle := TFmxFormBorderStyle.None;
    {$ENDIF}
    {$IFDEF CMNLIB}
    KeyPreview := True;
    Position := poDesigned;
    BorderStyle := bsNone;
    BorderIcons := [];
    {$IFDEF VCLLIB}
    Ctl3D := False;
    {$ENDIF}
    {$ENDIF}
    {$IFDEF WEBLIB}
    Border := fbNone;
    Position := poDesigned;
    {$ENDIF}
  finally
    {$IFDEF FMXLIB}
    EndUpdate;
    {$ENDIF}
  end;
end;

destructor TTMSFNCCustomPopupForm.Destroy;
begin
  FreeAndNil(FTimer);
  if FPlacementControl <> nil then
  begin
    TComponent(FPlacementControl).RemoveFreeNotification(Self);
    FPlacementControl := nil;
  end;
  FreeAndNil(FContentPadding);
  FreeAndNil(FPlacementRectangle);
  inherited;
end;

procedure TTMSFNCCustomPopupForm.DoClose(var CloseAction: TCloseAction);
begin
  {$IFNDEF FMXMOBILE}
  CloseAction := TCloseAction.caFree;
  {$ENDIF}
  inherited;
  if CloseAction <> TCloseAction.caNone then
  begin
    if Assigned(FTimer) then
      FTimer.Enabled := False;
  end;
end;

procedure TTMSFNCCustomPopupForm.DoRealPlacementChanged;
begin
  if Assigned(FOnRealPlacementChanged) then
    FOnRealPlacementChanged(Self);
end;

procedure TTMSFNCCustomPopupForm.KeyDown(var Key: Word; {$IFDEF FMXLIB}var KeyChar: WideChar;{$ENDIF} Shift: TShiftState);
begin
  inherited;
  if Shift <> [] then
    Exit;

  case Key of
    KEY_ESCAPE, KEY_F4: Close;
  end;
end;

function TTMSFNCCustomPopupForm.GetPopup: TTMSFNCCustomPopup;
{$IFDEF FMXLIB}
var
  p: TPopup;
{$ENDIF}
begin
  Result := nil;
  if FOwner is TTMSFNCCustomPopup then
    Result := FOwner as TTMSFNCCustomPopup
  {$IFDEF FMXLIB}
  else if FOwner is TPopup then
  begin
    p := FOwner as TPopup;
    if p.Owner is TTMSFNCCustomPopup then
      Result := p.Owner as TTMSFNCCustomPopup;
  end;
  {$ENDIF}
end;

function TTMSFNCCustomPopupForm.GetPopupParent: TControl;
begin
  Result := nil;
  if Assigned(FOwner) and (FOwner is TTMSFNCCustomPopup) then
    Result := (FOwner as TTMSFNCCustomPopup).GetParent;
end;

procedure TTMSFNCCustomPopupForm.HandleFocusedControl;
begin
  if Assigned(Popup) then
    Popup.HandleFocusedControl;
end;

function TTMSFNCCustomPopupForm.CloseQuery: Boolean;
begin
  Result := inherited CloseQuery;
  if Result and (not (csDestroying in ComponentState)) then
    DoBeforeClose;
end;

procedure TTMSFNCCustomPopupForm.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = FPlacementControl then
    begin
      FPlacementControl := nil;
      ApplyPlacement;
    end;
    if AComponent = FContentControl then
      FContentControl := nil;
  end;
end;

procedure TTMSFNCCustomPopupForm.SetDragWithParent(const Value: Boolean);
begin
  FDragWithParent := Value;
end;

type
  TDPIControlProtected = class(TControl);

procedure TTMSFNCCustomPopupForm.SetContentControl(const Value: TControl);
{$IFDEF CMNLIB}
var
  h: Integer;
{$ENDIF}
begin
  if FContentControl <> Value then
  begin
    if FContentControl <> nil then
      TComponent(FContentControl).RemoveFreeNotification(Self);
    FContentControl := Value;
    if FContentControl <> nil then
    begin
      TComponent(FContentControl).FreeNotification(Self);
      try
        {$IFDEF CMNLIB}
        h := TDPIControlProtected(FContentControl).Font.Height;
        {$ENDIF}
        FContentControl.Parent := Self;
        {$IFDEF FMXLIB}
        FContentControl.Align := TAlignLayout.None;
        {$ENDIF}
        {$IFDEF CMNWEBLIB}
        FContentControl.Align := alNone;
        {$ENDIF}
        {$IFDEF CMNLIB}
        TDPIControlProtected(FContentControl).Font.Height := h;
        {$ENDIF}
        FContentControl.Visible := True;
      finally
      end;
    end;
  end;
end;

procedure TTMSFNCCustomPopupForm.SetContentPadding(const Value: TTMSFNCMargins);
begin
  FContentPadding.Assign(Value);
end;

procedure TTMSFNCCustomPopupForm.SetOffset(const Value: TPointF);
begin
  if (FOffset.X <> Value.X) or (FOffset.Y <> Value.Y) then
  begin
    FOffset := Value;
    ApplyPlacement;
  end;
end;

procedure TTMSFNCCustomPopupForm.SetPlacement(const Value: TTMSFNCPopupPlacement);
begin
  if FPlacement <> Value then
  begin
    FPlacement := Value;
    ApplyPlacement;
  end;
end;

procedure TTMSFNCCustomPopupForm.SetPlacementRectangle(const Value: TTMSFNCMargins);
begin
  FPlacementRectangle.Assign(Value);
end;

procedure TTMSFNCCustomPopupForm.SetPlacementControl(const Value: TControl);
begin
  if FPlacementControl <> Value then
  begin
    if FPlacementControl <> nil then
      TComponent(FPlacementControl).RemoveFreeNotification(self);
    FPlacementControl := Value;
    if FPlacementControl <> nil then
      TComponent(FPlacementControl).FreeNotification(Self);
  end;
end;

procedure TTMSFNCCustomPopupForm.SetSize(const Value: TSizeF);
begin
  if (FSize.cx <> Value.cx) or (FSize.cy <> Value.cy) then
  begin
    FSize := Value;
    ApplyPlacement;
  end;
end;


procedure TTMSFNCCustomPopupForm.Loaded;
begin
  inherited;
  if FPlacementChanged then
    ApplyPlacement;
end;

procedure TTMSFNCCustomPopupForm.UpdateBounds(LRect: TRectF);
begin
  SetBounds(Round(LRect.Left), Round(LRect.Top), Round(LRect.Right - LRect.Left), Round(LRect.Bottom - LRect.Top));
end;

procedure TTMSFNCCustomPopupForm.Updated;
begin
  inherited;
  if FPlacementChanged then
    ApplyPlacement;
end;

procedure TTMSFNCCustomPopupForm.DoApplyPlacement;
var
  Pos: TPointF;
  {$IFDEF FMXLIB}
  AbsolutePos: TPointF;
  MouseSvc: IFMXMouseService;
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  pt, AbsolutePos: TPoint;
  {$ENDIF}
  LRect: TRectF;
  LPlacement: TTMSFNCPopupPlacement;
  LOffset: TPointF;
  LStep: Byte;
  LSoGood: Boolean;
  SourceSize: TSizeF;
  PlacementByTarget: Boolean;
  function UpdateRectByScreen(var R: TRectF; cs: TComponentState): Boolean;
  var
    WorkareaRect: TRect;
    WorkareaRectF: TRectF;
    W, H: Single;
    {$IFDEF FMXLIB}
    P: TPointF;
    {$ENDIF}
    {$IFNDEF FMXLIB}
    P: TPoint;
    {$ENDIF}
  begin
    Result := True;
    R.Left := Round(R.Left);
    R.Top := Round(R.Top);
    R.Right := Round(R.Right);
    R.Bottom := Round(R.Bottom);
    {$IFDEF FMXLIB}
    {$HINTS OFF}
    {$IF COMPILERVERSION > 27}
    case LPlacement of
      ppAbsolute:
        WorkareaRect := ConvertToRect(Screen.DisplayFromRect(R).WorkareaRect);
      ppMouse, ppMouseCenter, ppAboveMouse, ppAboveMouseCenter:
        WorkareaRect := ConvertToRect(Screen.DisplayFromPoint(Screen.MousePos).WorkareaRect)
    else
      if not InRange(FPreferedDisplayIndex, 0, Screen.DisplayCount - 1) then
      begin
        if (PlacementControl <> nil) and (PlacementControl.Root is TCommonCustomForm) and
          TCommonCustomForm(PlacementControl.Root).Visible then
        begin
          P := PointF(PlacementControl.Width / 2, PlacementControl.Height / 2);
          P := PlacementControl.LocalToAbsolute(P);
          FPreferedDisplayIndex := Screen.DisplayFromForm(TCommonCustomForm(PlacementControl.Root), P).Index;
        end
        else
          FPreferedDisplayIndex := Screen.DisplayFromForm(Self).Index;
      end;
      WorkareaRect := ConvertToRect(Screen.Displays[FPreferedDisplayIndex].WorkareaRect);
    end;
    {$ELSE}
    WorkareaRect := Rect(0, 0, Screen.Size.Width, Screen.Size.Height);
    {$IFEND}
    {$HINTS ON}
    {$ENDIF}
    {$IFDEF WEBLIB}
    WorkareaRect.Left := 0;
    WorkareaRect.Top := 0;
    WorkareaRect.Right := WinWidth;
    WorkareaRect.Bottom := WinHeight;
    {$ENDIF}
    {$IFDEF CMNLIB}
    case LPlacement of
      ppAbsolute:
        WorkareaRect := Screen.MonitorFromRect(ConvertToRect(R)).WorkareaRect;
      ppMouse, ppMouseCenter, ppAboveMouse, ppAboveMouseCenter:
        WorkareaRect := Screen.MonitorFromPoint(ConvertToPoint(TTMSFNCUtils.GetMousePos)).WorkareaRect
    else
      if not InRange(FPreferedDisplayIndex, 0, Screen.MonitorCount - 1) then
      begin
        if (PlacementControl <> nil) and (TTMSFNCUtils.GetParentForm(PlacementControl) is TCustomForm) and
          TCustomForm(TTMSFNCUtils.GetParentForm(PlacementControl)).Visible then
        begin
          P := Point(PlacementControl.Width div 2, PlacementControl.Height div 2);
          P := PlacementControl.ClientToScreen(P);
          FPreferedDisplayIndex := Screen.MonitorFromWindow(TTMSFNCUtils.GetParentForm(PlacementControl).Handle).MonitorNum;
        end
        else
          FPreferedDisplayIndex := Screen.MonitorFromWindow(Self.Handle).MonitorNum;
      end;
      WorkareaRect := Screen.Monitors[FPreferedDisplayIndex].WorkareaRect;
    end;
    {$ENDIF}
    WorkareaRectF := RectF(WorkareaRect.Left - ContentPadding.Left, WorkareaRect.Top - ContentPadding.Top,
      WorkareaRect.Right + ContentPadding.Right, WorkareaRect.Bottom + ContentPadding.Bottom);
    if not (csDesigning in cs) then
    begin
      W := R.Right - R.Left;
      H := R.Bottom - R.Top;
      if R.Left > WorkareaRectF.Left then
      begin
        if R.Left > WorkareaRectF.Right - W then
        begin
          R.Left := WorkareaRectF.Right - W;
          if LPlacement = ppRight then
          begin
            LPlacement := ppLeft;
            Result := False;
          end;
          if LPlacement = ppRightCenter then
          begin
            LPlacement := ppLeftCenter;
            Result := False;
          end;
        end;
      end
      else
      begin
        R.Left := WorkareaRectF.Left;
        if LPlacement = ppLeft then
        begin
          LPlacement := ppRight;
          Result := False;
        end;
        if LPlacement = ppLeftCenter then
        begin
          LPlacement := ppRightCenter;
          Result := False;
        end;
      end;
      if R.Top > WorkareaRectF.Top then
      begin
        if R.Top > WorkareaRectF.Bottom - H then
        begin
          R.Top := WorkareaRectF.Bottom - H;
          if LPlacement = ppBottom then
          begin
            LPlacement := ppTop;
            Result := False;
          end;
          if LPlacement = ppBottomCenter then
          begin
            LPlacement := ppTopCenter;
            Result := False;
          end;
        end;
      end
      else
      begin
        R.Top := WorkareaRectF.Top;
        if LPlacement = ppTop then
        begin
          LPlacement := ppBottom;
          Result := False;
        end;
        if LPlacement = ppTopCenter then
        begin
          LPlacement := ppBottomCenter;
          Result := False;
        end;
      end;
      R.Right := R.Left + W;
      R.Bottom := R.Top + H;
    end;
  end;
begin
  FPlacementChanged := False;
  LOffset := Offset;
  LPlacement := Placement;
  LStep := 0;
  repeat
    LRect := RectF(FPlacementRectangle.Left, FPlacementRectangle.Top, FPlacementRectangle.Right, FPlacementRectangle.Bottom);
    if (PlacementControl <> nil) and FPlacementRectangle.Empty then
      LRect := RectF(0, 0, PlacementControl.Width, PlacementControl.Height);
    if (PlacementControl = nil) and PlacementRectangle.Empty and
       (not (LPlacement in [ppAbsolute, ppMouse, ppMouseCenter, ppAboveMouse, ppAboveMouseCenter])) then
    begin
      {$IFDEF FMXLIB}
      if not TPlatformServices.Current.SupportsPlatformService(IFMXMouseService, IInterface(MouseSvc)) then
        LPlacement := ppAbsolute
      else
        LPlacement := ppMouse;
      {$ENDIF}
    end;
    FScreenPlacementRect := LRect;
    // Vertical Offset
    if LPlacement in [ppTop, ppTopCenter] then
      OffsetRectEx(LRect, 0, ContentPadding.Bottom - LOffset.Y)
    else
      OffsetRectEx(LRect, 0, LOffset.Y - ContentPadding.Top);
    // Horizontal Offset
    if LPlacement in [ppLeft, ppLeftCenter] then
      OffsetRectEx(LRect, ContentPadding.Right - LOffset.X, 0)
    else
      OffsetRectEx(LRect, LOffset.X - ContentPadding.Left, 0);
    // Offset by rect
    PlacementByTarget := not (LPlacement in [ppAbsolute, ppMouse, ppMouseCenter, ppAboveMouse, ppAboveMouseCenter]);
    SourceSize := Size;

    case LPlacement of
      ppBottom:
        OffsetRectEx(LRect, 0, (LRect.Bottom - LRect.Top));
      ppTop:
        OffsetRectEx(LRect, 0, -SourceSize.cy);
      ppLeft:
        OffsetRectEx(LRect, -SourceSize.cx, 0);
      ppRight:
        OffsetRectEx(LRect, (LRect.Right - LRect.Top), 0);
      ppCenter:
        OffsetRectEx(LRect, ((LRect.Right - LRect.Top) - SourceSize.cx) / 2, ((LRect.Bottom - LRect.Top) - SourceSize.cy) / 2);
      ppBottomCenter:
        OffsetRectEx(LRect, ((LRect.Right - LRect.Top) - SourceSize.cx) / 2, (LRect.Bottom - LRect.Top));
      ppTopCenter:
        OffsetRectEx(LRect, ((LRect.Right - LRect.Top) - SourceSize.cx) / 2, -SourceSize.cy);
      ppLeftCenter:
        OffsetRectEx(LRect, -SourceSize.cx, ((LRect.Bottom - LRect.Top) - SourceSize.cy) / 2);
      ppRightCenter:
        OffsetRectEx(LRect, (LRect.Right - LRect.Top), ((LRect.Bottom - LRect.Top) - SourceSize.cy) / 2);
      ppAbsolute:
        begin
          if FPlacementRectangle.Empty then
            LRect := RectF(FPlacementRectangle.Rect.Left, FPlacementRectangle.Rect.Top, FPlacementRectangle.Rect.Left + SourceSize.cx, FPlacementRectangle.Rect.Top + SourceSize.cy)
          else
            LRect := FPlacementRectangle.Rect;
        end;
      ppMouse, ppMouseCenter, ppAboveMouse, ppAboveMouseCenter:
        begin
          Pos := TTMSFNCUtils.GetMousePos;
          LRect := RectF(Pos.X, Pos.Y, Pos.X + SourceSize.cx, Pos.Y + SourceSize.cy);
          if LPlacement = ppMouseCenter then
            OffsetRectEx(LRect, -SourceSize.cx / 2, -SourceSize.cy / 2);
          if LPlacement = ppAboveMouseCenter then
            OffsetRectEx(LRect, -SourceSize.cx / 2, -SourceSize.cy - 5);
          if LPlacement = ppAboveMouse then
            OffsetRectEx(LRect, 0, -SourceSize.cy - 5);
        end;
    end;
    // use border
    if PlacementByTarget then
    begin
      {$IFDEF FMXLIB}
      AbsolutePos := PointF(LRect.Left, LRect.Top);
      {$ENDIF}
      {$IFDEF CMNWEBLIB}
      AbsolutePos := Point(Round(LRect.Left), Round(LRect.Top));
      {$ENDIF}
      if PlacementControl <> nil then
      begin
        {$IFDEF FMXLIB}
        AbsolutePos := PlacementControl.LocalToAbsolute(AbsolutePos);
        FScreenPlacementRect.TopLeft := PlacementControl.LocalToAbsolute(FScreenPlacementRect.TopLeft);
        FScreenPlacementRect.BottomRight := PlacementControl.LocalToAbsolute(FScreenPlacementRect.BottomRight);
        if PlacementControl.Scene <> nil then
        begin
          AbsolutePos := PlacementControl.Scene.LocalToScreen(AbsolutePos);
          FScreenPlacementRect.TopLeft := PlacementControl.Scene.LocalToScreen(FScreenPlacementRect.TopLeft);
          FScreenPlacementRect.BottomRight := PlacementControl.Scene.LocalToScreen(FScreenPlacementRect.BottomRight);
        end;
        {$ENDIF}
        {$IFDEF CMNWEBLIB}
        AbsolutePos := PlacementControl.ClientToScreen(AbsolutePos);
        pt := Point(Round(FScreenPlacementRect.Left), Round(FScreenPlacementRect.Top));
        pt := PlacementControl.ClientToScreen(pt);
        FScreenPlacementRect.Left := pt.X;
        FScreenPlacementRect.Top := pt.Y;
        pt := Point(Round(FScreenPlacementRect.Right), Round(FScreenPlacementRect.Bottom));
        pt := PlacementControl.ClientToScreen(pt);
        FScreenPlacementRect.Right := pt.X;
        FScreenPlacementRect.Bottom := pt.Y;
        {$ENDIF}
      end;
      LRect := RectF(AbsolutePos.X, AbsolutePos.Y, AbsolutePos.X + Size.cx, AbsolutePos.Y + Size.cy);
    end;
    LSoGood := UpdateRectByScreen(LRect, ComponentState);
    Inc(LStep);
  until LSoGood or (LStep > 1);
  FScreenContentRect := LRect;
  FScreenContentRect := RectF(FScreenContentRect.Left + FContentPadding.Left, FScreenContentRect.Top + FContentPadding.Top, FScreenContentRect.Right - FContentPadding.Right, FScreenContentRect.Bottom - FContentPadding.Bottom);
  FRealPlacement := LPlacement;
  UpdateBounds(LRect);
end;

procedure TTMSFNCCustomPopupForm.DoBeforeClose;
begin
  if Assigned(OnBeforeClose) then
    OnBeforeClose(self);
end;

procedure TTMSFNCCustomPopupForm.DoBeforeShow;
begin
  FHintWindow := False;
  if Assigned(OnBeforeShow) then
    OnBeforeShow(self);

  FFirstShow := True;

  if not Assigned(FTimer) then
    FTimer := TTimer.Create(Self);

  FTimer.Interval := 20;
  FTimer.OnTimer := TimerProc;
  FTimer.Enabled := True;
end;

procedure TTMSFNCCustomPopupForm.TimerProc(Sender: TObject);
begin
  if (Visible or FHintWindow) and (FFirstShow or FDragWithParent) then
    ApplyPlacement;
  if (Visible or FHintWindow) and (FFirstShow) then
  begin
    FFirstShow := False;
  end;
end;

procedure TTMSFNCCustomPopupForm.ApplyPlacement;
var
  OldRect, NewRect: TRect;
  OldRealPlacement: TTMSFNCPopupPlacement;
begin
  {$IFNDEF WEBLIB}
  if not (csLoading in ComponentState) and not (csUpdating in ComponentState) and not (csDestroying in ComponentState) then
  {$ENDIF}
  begin
    OldRect := Rect(Left, Top, Left + Width, Top + Height);
    OldRealPlacement := FRealPlacement;
    DoApplyPlacement;
    NewRect := Rect(Left, Top, Left + Width, Top + Height);
    if (NewRect.Left <> OldRect.Left) or (NewRect.Top <> OldRect.Top) or (NewRect.Bottom <> OldRect.Bottom) or (NewRect.Right <> OldRect.Right)
      or (OldRealPlacement <> RealPlacement) then
      DoRealPlacementChanged;
  end
  {$IFNDEF WEBLIB}
  else
    FPlacementChanged := True;
  {$ENDIF}
end;

procedure TTMSFNCCustomNonFocusablePopup.ActivatePreviousWindow;
begin
  {$IFDEF CMNLIB}
  SetActiveWindow(FActiveWindow);
  {$ENDIF}
end;

constructor TTMSFNCCustomNonFocusablePopup.Create(AOwner: TComponent);
begin
  inherited;
  StaysOpen := True;
end;

destructor TTMSFNCCustomNonFocusablePopup.Destroy;
begin
  inherited;
end;

function TTMSFNCCustomNonFocusablePopup.GetPopupFormClass: TTMSFNCCustomPopupFormClass;
begin
  Result := TTMSFNCCustomNonFocusablePopupForm;
end;

procedure TTMSFNCCustomNonFocusablePopup.ShowPopup(AModal: Boolean);
{$IFDEF CMNLIB}
{$IFDEF MSWINDOWS}
var
  hWin: HWND;
{$ENDIF}
{$ENDIF}
begin
  {$IFDEF FMXLIB}
  PopupForm.FormStyle := TFormStyle.Popup;
  inherited;
  {$ENDIF}
  {$IFDEF CMNLIB}
  FActiveWindow := GetActiveWindow;
  {$IFDEF MSWINDOWS}
  PopupForm.HintWindow := True;
  hWin := PopupForm.Handle;
  SetWindowPos(hwin, HWND_TOPMOST, PopupForm.Left, PopupForm.Top, PopupForm.Width, PopupForm.Height, SWP_NOACTIVATE);
  ShowWindow(hWin, SW_SHOWNOACTIVATE);
  PopupForm.Visible := True;
  {$ELSE}
  PopupForm.SetBounds(PopupForm.Left, PopupForm.Top, PopupForm.Width, PopupForm.Height);
  PopupForm.Visible := True;
  PopupForm.BorderStyle := bsSingle;
  PopupForm.BorderStyle := bsNone;
  {$ENDIF}
  SetActiveWindow(FActiveWindow);
  {$ENDIF}
  {$IFDEF WEBLIB}
  inherited;
  {$ENDIF}

  FIsOpen := True;
end;

{ TTMSFNCCustomNonFocusablePopupForm }

procedure TTMSFNCCustomNonFocusablePopupForm.UpdateBounds(LRect: TRectF);
{$IFDEF CMNLIB}
{$IFDEF MSWINDOWS}
var
  hWin: HWND;
{$ENDIF}
{$ENDIF}
begin
  inherited;
  {$IFDEF CMNLIB}
  {$IFDEF MSWINDOWS}
  hWin := Handle;
  SetWindowPos(hwin, HWND_TOPMOST, Round(LRect.Left), Round(LRect.Top), Round((LRect.Right - LRect.Left)), Round((LRect.Bottom - LRect.Top)), SWP_NOACTIVATE);
  {$ELSE}
  SetBounds(Round(LRect.Left), Round(LRect.Top), Round((LRect.Right - LRect.Top)), Round((LRect.Bottom - LRect.Top)));
  {$ENDIF}
  {$ENDIF}
end;

{$IFDEF CMNLIB}
{$IFDEF MSWINDOWS}
function HintWindowProc(hWnd: hWnd; uMsg: Integer; WParam: WParam;
  lParam: lParam): LRESULT; stdcall;
var
  I: Integer;
  pr: LInteger;
begin
  case uMsg of
    WM_MOUSEACTIVATE:
    begin
      Result := MA_NOACTIVATE;
      Exit;
    end;
  end;

  pr := 0;
  for I := 0 to Length(PrevWndProcs) - 1 do
  begin
    if PrevWndProcs[I].FWindowHandle = hWnd then
      pr := PrevWndProcs[I].FProc;
  end;

  Result := CallWindowProc({%H-}IntPtr(pr), hWnd, uMsg, WParam, lParam);
end;

destructor TTMSFNCCustomNonFocusablePopupForm.Destroy;
var
  I: Integer;
begin
  for I := 0 to Length(PrevWndProcs) - 1 do
  begin
    if PrevWndProcs[I].FWindowHandle = Self.Handle then
      SetWindowLongPtr(Self.Handle, GWL_WNDPROC, PrevWndProcs[I].FProc);
  end;
  inherited;
end;

constructor TTMSFNCCustomNonFocusablePopupForm.CreateNew(AOwner: TComponent; Dummy: Integer  = 0);
var
  r: WndProcRec;
begin
  inherited;
  SetLength(PrevWndProcs, Length(PrevWndProcs) + 1);
  r.FWindowHandle := Self.Handle;
  r.FProc := SetWindowLongPtr(Self.Handle, GWL_WNDPROC, {%H-}LInteger(@HintWindowProc));
  PrevWndProcs[Length(PrevWndProcs) - 1] := r;
end;

procedure TTMSFNCCustomNonFocusablePopupForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := WS_POPUP;
    WindowClass.Style := WindowClass.Style or CS_SAVEBITS;
    if NewStyleControls then
      ExStyle := WS_EX_TOOLWINDOW or WS_EX_NOACTIVATE;
  end;
end;

procedure TTMSFNCCustomNonFocusablePopupForm.WMNCHitTest(var Message: TWMNCHitTest);
begin
  Message.Result := HTTRANSPARENT;
end;

procedure TTMSFNCCustomNonFocusablePopupForm.WMActivate(var Message: TWMActivate);
begin
  Message.Result := 1;
end;

{$ELSE}
procedure TTMSFNCCustomNonFocusablePopupForm.WMNCHitTest(var Message: TLMessage);
begin
  Message.Result := HTTRANSPARENT;
end;

procedure TTMSFNCCustomNonFocusablePopupForm.WMActivate(var Message: TLMActivate);
begin
  Message.Result := 1;
  if (Owner is TTMSFNCCustomNonFocusablePopup) then
    (Owner as TTMSFNCCustomNonFocusablePopup).ActivatePreviousWindow;
end;
{$ENDIF}
{$ENDIF}

{ TTMSFNCNonFocusablePopup }

procedure TTMSFNCNonFocusablePopup.Deactivate;
begin
  ClosePopup;
end;

procedure TTMSFNCNonFocusablePopup.Activate;
begin
  DoCreatePopup(False);
end;

end.

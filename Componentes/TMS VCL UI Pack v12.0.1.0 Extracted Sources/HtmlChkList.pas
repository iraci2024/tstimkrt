{**************************************************************************}
{ THTMLCheckList component                                                 )
{ for Delphi & C++Builder                                                  }
{                                                                          }
{ Copyright � 1999 - 2021                                                  }
{   TMS Software                                                           }
{   Email : info@tmssoftware.com                                           }
{   Web : https://www.tmssoftware.com                                      }
{                                                                          }
{ The source code is given as is. The author is not responsible            }
{ for any possible damage done due to the use of this code.                }
{ The component can be freely used in any application. The complete        }
{ source code remains property of the author and may not be distributed,   }
{ published, given or sold in any form as such. No parts of the source     }
{ code can be included in any other component or application without       }
{ written authorization of the author.                                     }
{**************************************************************************}

{$I TMSDEFS.INC}

{$DEFINE REMOVEDRAW}
{$DEFINE HILIGHT}

unit HTMLChkList;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, forms,
  StdCtrls, Dialogs, Menus, ComCtrls, PictureContainer, AdvStyleIF, ImgList,
  HTMLXPVS, AdvGradient,Types
  {$IFDEF DELPHIXE2_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  MAJ_VER = 2; // Major version nr.
  MIN_VER = 4; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // 1.9.0.0 : New styler interface added with Office 2007 gradient selection colors
  //         : New ShowFocus property added
  // 1.9.0.1 : Fixed memory leak issue
  // 1.9.0.2 : Fixed drawing issue with BeginUpdate/EndUpdate

  // 2.0.0.0 : New : SelectionColors property
  //         : New : Windows Vista, Windows 7 color style

  // 2.0.0.1 : Improved : checkbox drawing for non Flat checkboxes
  // 2.0.0.2 : Fixed OnCheckClick event declaration
  // 2.0.0.3 : Fixed issue with painting checkboxes & scrolling

  // 2.0.1.0 : New : support for customizing bullets in HTML UL lists
  // 2.1.0.0 : New : support for Office 2010 color styles added

  // 2.1.1.0 : New : Methods CheckAll/UnCheckAll added
  // 2.1.1.1 : Fixed : Memory leak
  // 2.1.1.2 : Fixed : Issue with Hilight & Mark items
  // 2.2.0.0 : New : Reorder property added
  //         : New : Spacebar to toggle checkbox
  //         : New : Ctrl-A to select all items
  //         : New : CommentColor, CommentFont property added
  // 2.2.0.1 : Fixed : Issue with state saving on recreatewnd
  // 2.3.0.0 : New : Support for PNG images via images in associated PictureContainer
  // 2.3.0.1 : Fixed : Issue with access objects after modifying items
  // 2.3.1.0 : Improved : Centering of text respective to checkbox
  // 2.3.2.0 : New : Property SelectionHTMLColors added
  // 2.3.3.0 : New : Support for LINE-HEIGHT attribute for <P> tag
  // 2.2.3.1 : Improved : Handling for high DPI with Form.Scaled = false
  // 2.3.0.0 : New : Support for VCL styles
  // 2.3.0.1 : Improved : VCL Style handling
  // 2.3.0.2 : Fixed : Issue with use on Win 64bit
  // 2.3.1.0 : New : Selection colors for Office 2019 styles
  // 2.3.2.0 : New : UIStyle property for office look
  //         : Improved : On creation check for enabled AdvFormStyler
  //         : Fixed : ParentFont kept true on initialization
  // 2.3.2.1 : Fixed : VCL styles check updated to work in 10.4 Sydney
  // 2.3.3.0 : Improved : High DPI checkbox drawing
  // 2.3.4.0 : New : Support for high DPI VCL Styles handling
  // 2.4.0.0 : New : HTML Emoji rendering support added


type
  {$IFDEF DELPHI_UNICODE}
  {$EXTERNALSYM THintInfo}
  THintInfo = Controls.THintInfo;
  {$EXTERNALSYM PHintInfo}
  PHintInfo = Controls.PHintInfo;
  {$ENDIF}

  TAnchorClick = procedure(Sender:TObject;index:integer;anchor:string) of object;

  TAnchorHintEvent = procedure(Sender:TObject; Index: Integer; var Anchor:string) of object;

  TCheckBoxClickEvent = procedure(Sender: TObject; Index: integer) of object;

  THTMLCheckList = class;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  THTMLCheckList = class(TCustomListBox, ITMSStyle)
  private
    FMouseDownOnItemX, FMouseDownOnItemY: Integer;
    FDragging: Boolean;
    FItemDragIdx: Integer;
    FAutoFocus: Boolean;
    FOldCursor: TCursor;
    FAllowGrayed: Boolean;
    FDuplicates: Boolean;
    FFlat: Boolean;
    FStandardItemHeight: Integer;
    FOnCheckClick: TCheckBoxClickEvent;
    FSaveStates: TList;
    FShowSelection: Boolean;
    FDuplicateList : TStringList;
    FImages: TCustomImageList;
    FShadowColor: TColor;
    FShadowOffset: Integer;
    FUpdateCount: Integer;
    FContainer: TPictureContainer;
    FImageCache: THTMLPictureCache;
    FMouseDown: Boolean;
    FLineSpacing: Integer;
    FOldAnchor: string;
    FSelectionFontColor: TColor;
    FSelectionColors: TGradientStyle;
    FEllipsis: Boolean;
    FURLColor: TColor;
    FIsWinXP: Boolean;
    FOnAnchorExit: TAnchorClick;
    FOnAnchorClick: TAnchorClick;
    FOnAnchorEnter: TAnchorClick;
    FOnAnchorHint: TAnchorHintEvent;
    FAnchorHint: Boolean;
    FItemHint: Boolean;
    FHTMLHint: Boolean;
    FLastHintPos: Integer;
    FShowFocus: Boolean;
    FEnableCommentSelection: Boolean;
    FReorder: Boolean;
    FReorderComments: Boolean;
    FCommentColor: TColor;
    FCommentFont: TFont;
    FSelectionHTMLColors: boolean;
    FFormScaled: boolean;
    FUseVCLStyles: boolean;
    FTMSStyle: TTMSStyle;
    FCheckWidthForDPI: Integer;
    FCheckHeightForDPI: Integer;
    {$IFDEF DELPHIXE10_LVL}
    procedure GetCheckSizeForDPI;
    {$ENDIF}
    procedure ResetItemHeight;
    procedure SetChecked(Index: Integer; Checked: Boolean);
    function GetChecked(Index: Integer): Boolean;
    procedure SetState(Index: Integer; AState: TCheckBoxState);
    function GetState(Index: Integer): TCheckBoxState;
    procedure SetIndent(Index: Integer; AIndent: integer);
    function GetIndent(Index: Integer): integer;
    procedure ToggleClickCheck(Index: Integer);
    procedure InvalidateCheck(Index: Integer);
    procedure InvalidateItem(Index: Integer);
    function CreateWrapper(Index: Integer): TObject;
    function ExtractWrapper(Index: Integer): TObject;
    function GetWrapper(Index: Integer): TObject;
    function HaveWrapper(Index: Integer): Boolean;
    procedure SetFlat(Value: Boolean);
    procedure SetDuplicates(Value: Boolean);
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMHintShow(Var Msg: TMessage); message CM_HINTSHOW;
    procedure WMDestroy(var Msg : TWMDestroy);message WM_DESTROY;
    procedure WMEraseBkGnd(var Message:TMessage); message WM_ERASEBKGND;
    procedure WMKillFocus(var Message:TMessage); message WM_KILLFOCUS;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    {$IFDEF DELPHIXE2_LVL}
    procedure CMStyleChanged(var Message: TMessage); message CM_STYLECHANGED;
    {$ENDIF}
    procedure InitVCLStyle(init: boolean);
    function GetItemEnabled(Index: Integer): Boolean;
    procedure SetItemEnabled(Index: Integer; const Value: Boolean);
    function GetComment(Index: Integer): Boolean;
    procedure SetComment(Index: Integer; AComment: Boolean);
    function GetSelectCount: Integer;
    function GetSelected(Index: Integer): string;
    procedure QuickSortItems(left,right: Integer);
    procedure SetImageList(const Value: TCustomImageList);
    function GetItemIndent(const Index: Integer): Integer;
    procedure SetShadowColor(const Value: TColor);
    procedure SetShadowOffset(const Value: Integer);
    procedure SetLineSpacing(const Value: Integer);
    procedure SetSelectionColors(const Value: TGradientStyle);
    procedure SetSelectionFontColor(const Value: TColor);
    function IsAnchor(x, y: integer;var Idx: Integer): string;
    procedure SetEllipsis(const Value: Boolean);
    procedure SetURLColor(const Value: TColor);
    function GetTextItem(index: integer): string;
    procedure SetContainer(const Value: TPictureContainer);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    function GetVersionNr: Integer;
    procedure SetEnableCommentSelection(const Value: Boolean);
    procedure CommentFontChanged(Sender: TObject);
  protected
    function GetCheckWidth: Integer; virtual;
    procedure DrawCheck(R: TRect; AState: TCheckBoxState; AEnabled: Boolean); virtual;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure DrawItem(Index: Integer; Rect: TRect; AState: TOwnerDrawState); override;

    {$IFDEF DELPHIXE2_LVL}
    function InternalGetItemData(Index: Integer): TListBoxItemData; override;
    procedure InternalSetItemData(Index: Integer; AData: TListBoxItemData); override;
    procedure SetItemData(Index: Integer; AData: TListBoxItemData); override;
    function GetItemData(Index: Integer): TListBoxItemData; override;
    {$ENDIF}

    {$IFNDEF DELPHIXE2_LVL}
    function InternalGetItemData(Index: Integer): Longint; override;
    procedure InternalSetItemData(Index: Integer; AData: Longint); override;
    procedure SetItemData(Index: Integer; AData: LongInt); override;
    function GetItemData(Index: Integer): LongInt; override;
    {$ENDIF}

    {$IFDEF DELPHIXE10_LVL}
    procedure ChangeScale(M, D: Integer; isDpiChange: Boolean); override;
    {$ENDIF}

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure ResetContent; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure DeleteString(Index: Integer); override;
    procedure ClickCheck(Index: integer); dynamic;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure CheckAllInt(AState: TCheckBoxState); overload;
    procedure CheckAllInt(check: boolean = true); overload;
    procedure Loaded; override;
    property ImageCache: THTMLPictureCache read FImageCache;
    property UpdateCount: integer read FUpdateCount write FUpdateCount;
  public
    procedure DragDrop(Source: TObject; X, Y: integer); override;
    procedure DragOver(Source: TObject; X, Y: integer; State: TDragState;
      var Accept: Boolean); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Checked[Index: Integer]: Boolean read GetChecked write SetChecked;
    property ItemEnabled[Index: Integer]: Boolean read GetItemEnabled write SetItemEnabled;
    property State[Index: Integer]: TCheckBoxState read GetState write SetState;
    property Indent[Index: Integer]: Integer read GetIndent write SetIndent;
    property Comment[Index: Integer]: Boolean read GetComment write SetComment;
    property Selected[Index: Integer]: string read GetSelected;
    property SelectCount: Integer read GetSelectCount;
    procedure SortAllComments;
    procedure SortComment(Index: Integer);
    procedure MoveItem(FromIndex, ToIndex: integer);
    procedure BeginUpdate;
    procedure EndUpdate;
    property TextItems[index:integer]:string read GetTextItem;
    procedure HilightInList(HiText: string; DoCase: Boolean);
    procedure HilightInItem(Index: Integer; HiText: string; DoCase: Boolean);
    procedure UnHilightInList;
    procedure UnHilightInItem(Index: Integer);
    procedure MarkInList(HiText: string; DoCase: Boolean);
    procedure MarkInItem(Index: Integer; HiText: string; DoCase: Boolean);
    procedure UnMarkInList;
    procedure UnMarkInItem(Index: Integer);
    procedure SetComponentStyle(AStyle: TTMSStyle);
    procedure CheckAll;
    procedure UnCheckAll;
    procedure GrayAll;
  published
    property Align;
    property EnableCommentSelection: Boolean read FEnableCommentSelection write SetEnableCommentSelection default True;
    property AllowGrayed: Boolean read FAllowGrayed write FAllowGrayed default False;
    property AnchorHint: Boolean read FAnchorHint write FAnchorHint default False;
    property AutoFocus: Boolean read FAutoFocus write FAutoFocus default False;
    property CommentColor: TColor read FCommentColor write FCommentColor default clNone;
    property CommentFont: TFont read FCommentFont write FCommentFont;
    property Ellipsis: Boolean read FEllipsis write SetEllipsis default False;
    property HTMLHint: Boolean read FHTMLHint write FHTMLHint default False;
    property Images: TCustomImageList read FImages write SetImageList;
    property ItemHint: Boolean read FItemHint write FItemHint default False;
    property LineSpacing: Integer read FLineSpacing write SetLineSpacing default 0;
    property PictureContainer: TPictureContainer read FContainer write SetContainer;
    property Reorder: Boolean read FReorder write FReorder default False;
    property ReorderComments: Boolean read FReorderComments write FReorderComments default False;
    property SelectionColors: TGradientStyle read FSelectionColors write SetSelectionColors;
    property SelectionFontColor: TColor read FSelectionFontColor write SetSelectionFontColor default clHighlightText;
    property SelectionHTMLColors: boolean read FSelectionHTMLColors write FSelectionHTMLColors default False;
    property ShadowColor:TColor read FShadowColor write SetShadowColor default clGray;
    property ShadowOffset: Integer read FShadowOffset write SetShadowOffset default 1;
    property ShowSelection: Boolean read FShowSelection write FShowSelection default true;
    property URLColor: TColor read FURLColor write SetURLColor default clBlue;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property OnContextPopup;
    property OnEndDock;
    property OnStartDock;
    property BorderStyle;
    property Color;
    {property Columns; no multicolumn support}
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Duplicates: Boolean read FDuplicates write SetDuplicates default true;
    property Enabled;
    property Flat: Boolean read FFlat write SetFlat default True;
    //property ExtendedSelect;
    property Font;
    property ImeMode;
    property ImeName;
    property IntegralHeight;
    property ItemHeight;
    property Items;
    //property MultiSelect;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowFocus: Boolean read FShowFocus write FShowFocus default True;
    property ShowHint;
    property Sorted;
    //property Style;
    {$IFDEF DELPHIXE6_LVL}
    property StyleElements;
    {$ENDIF}
    property TabOrder;
    property TabStop;
    property TabWidth;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnCheckClick: TCheckBoxClickEvent read FOnCheckClick write FOnCheckClick;

    property OnAnchorClick:TAnchorClick read FOnAnchorClick write FOnAnchorClick;
    property OnAnchorEnter:TAnchorClick read FOnAnchorEnter write FOnAnchorEnter;
    property OnAnchorExit:TAnchorClick read FOnAnchorExit write FOnAnchorExit;
    property OnAnchorHint:TAnchorHintEvent read FOnAnchorHint write FOnAnchorHint;
    property Version: string read GetVersion write SetVersion;
    property UIStyle: TTMSStyle read FTMSStyle write SetComponentStyle default tsCustom;
  end;

implementation

uses
  Consts, Shellapi, ShlObj, ActiveX, CommCtrl, AdvHTML
  {$IFDEF DELPHIXE2_LVL}
  ,VCL.Themes
{$ENDIF}
  ;

type
  TSaveObject = class(TObject)
  private
    FIndent: Integer;
    FObject: TObject;
  public
    constructor Create(AIndent:Integer;AObject:TObject);
  end;

  THTMLCheckListDataWrapper = class
  private
    {$IFDEF DELPHIXE2_LVL}
    FData: NativeInt;
    {$ENDIF}
    {$IFNDEF DELPHIXE2_LVL}
    FData: LongInt;
    {$ENDIF}
    FState: TCheckBoxState;
    FDisabled: Boolean;
    FIndent : Integer;
    FComment : Boolean;
    procedure SetChecked(Check: Boolean);
    function GetChecked: Boolean;
  public
    class function GetDefaultState: TCheckBoxState;
    property Checked: Boolean read GetChecked write SetChecked;
    property State: TCheckBoxState read FState write FState;
    property Disabled: Boolean read FDisabled write FDisabled;
    property Indent: integer read FIndent write FIndent;
    property Comment: Boolean read FComment write FComment;
  end;

  TStateList = class(TList)
  private
    function GetItem(const Index: Integer): THTMLCheckListDataWrapper;
    procedure SetItem(const Index: Integer; const Value: THTMLCheckListDataWrapper);
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    function Add(const Value: THTMLCheckListDataWrapper): Integer; reintroduce;
    property Items[const Index: Integer]: THTMLCheckListDataWrapper read GetItem write SetItem; default;
  end;

var
  FCheckWidth: Integer;
  FCheckHeight: Integer;

procedure GetCheckSize;
{$IFDEF DELPHIXE2_LVL}
var
  DC: HDC;
  LCheckSize: TSize;
  LStyle: TCustomStyleServices;
{$ENDIF}
begin
  {$IFDEF DELPHIXE2_LVL}
  LStyle := StyleServices;
  if LStyle.Enabled then
  begin
    DC := CreateCompatibleDC(0);
    try
      LStyle.GetElementSize(DC, LStyle.GetElementDetails(tbCheckBoxCheckedNormal), esActual, LCheckSize);
      if (LCheckSize.Width <= 0) or (LCheckSize.Height <= 0) then
      begin
        LStyle := TStyleManager.SystemStyle;
        LStyle.GetElementSize(DC, LStyle.GetElementDetails(tbCheckBoxCheckedNormal), esActual, LCheckSize);
      end;
      FCheckWidth := LCheckSize.Width;
      FCheckHeight := LCheckSize.Height;
    finally
      DeleteDC(DC);
    end;
  end
  else
  {$ENDIF}
    with TBitmap.Create do
      try
        Handle := LoadBitmap(0, PChar(32759));
        FCheckWidth := Width div 4;
        FCheckHeight := Height div 3;
      finally
        Free;
      end;
end;


{ $TSaveObject }

constructor TSaveObject.Create(AIndent: Integer; AObject: TObject);
begin
  inherited Create;
  FIndent := AIndent;
  FObject := AObject;
end;

{ THTMLCheckListDataWrapper }

procedure THTMLCheckListDataWrapper.SetChecked(Check: Boolean);
begin
  if Check then FState := cbChecked else FState := cbUnchecked;
end;

function THTMLCheckListDataWrapper.GetChecked: Boolean;
begin
  Result := (FState = cbChecked) and not FComment;
end;

class function THTMLCheckListDataWrapper.GetDefaultState: TCheckBoxState;
begin
  Result := cbUnchecked;
end;

{ THTMLCheckList }

constructor THTMLCheckList.Create(AOwner: TComponent);
var
  VerInfo: TOSVersionInfo;
begin
  inherited Create(AOwner);
  FItemDragIdx := -1;
  FReorder := False;
  FEnableCommentSelection := True;
  FFlat := True;
  FDuplicates:=True;
  FDuplicateList:=TStringList.Create;
  FSelectionFontColor := clHighLightText;
  FShowSelection := True;
  FAutoFocus := False;

  FOldAnchor := '';
  DoubleBuffered := True;
  Style := lbOwnerDrawFixed;
  FImages := nil;
  FURLColor := clBlue;
  FShadowColor := clGray;
  FShadowOffset := 1;
  FUpdateCount := 0;
  FImageCache :=  THTMLPictureCache.Create;
  FMouseDown := False;
  FCommentColor := clNone;
  FCommentFont := TFont.Create;
  FCommentFont.OnChange := CommentFontChanged;
  FUseVCLStyles := false;

  FSelectionColors := TGradientStyle.Create;

  VerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);

  GetVersionEx(verinfo);

  FIsWinXP := (verinfo.dwMajorVersion > 5) OR
    ((verinfo.dwMajorVersion = 5) AND (verinfo.dwMinorVersion >= 1));

  FShowFocus := true;

  if (csDesigning in ComponentState) and not
    ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState)) then
  begin
    SetComponentStyle(GetDefaultStyle(AOwner));
    if SetParentFontForStyle(FTMSStyle) then
      ParentFont := true;
  end;
  FTMSStyle := tsCustom;

  if (FCheckWidth=0) and (FCheckHeight=0) then
    GetCheckSize;

  FCheckWidthForDPI := FCheckWidth;
  FCheckHeightForDPI := FCheckHeight;
end;

destructor THTMLCheckList.Destroy;
begin
  FSelectionColors.Free;
  FSaveStates.Free;
  FDuplicateList.Free;
  FImageCache.Free;
  FCommentFont.Free;
  inherited;
end;

procedure THTMLCheckList.CreateWnd;
var
  frm: TCustomForm;

begin
  inherited CreateWnd;
  if FSaveStates <> nil then
  begin
    FSaveStates.Free;
    FSaveStates := nil;
  end;

  ResetItemHeight;
  FFormScaled := true;
  frm := GetParentForm(Self);
  if Assigned(frm) and (frm is TForm) then
    FFormScaled := (frm as TForm).Scaled;

end;

procedure THTMLCheckList.DestroyWnd;
var
  I: Integer;
begin
  if Items.Count > 0 then
  begin
    FSaveStates := TStateList.Create;
    for I := 0 to Items.Count - 1 do
      TStateList(FSaveStates).Add(THTMLCheckListDataWrapper(GetWrapper(I)))
  end;
  inherited DestroyWnd;
end;

procedure THTMLCheckList.CreateParams(var Params: TCreateParams);
begin
  inherited;
  with Params do
  Style := Style or LBS_OWNERDRAWFIXED;
end;

function THTMLCheckList.GetCheckWidth: Integer;
begin
  Result := FCheckWidthForDPI + 2;
end;

function THTMLCheckList.GetItemIndent(const Index:integer): Integer;
begin
  Result := (FCheckWidthForDPI + 2) * (Indent[Index] + 1);
  if (Comment[Index]) then Result:=0;
end;

procedure THTMLCheckList.CMFontChanged(var Message: TMessage);
begin
  inherited;
  ResetItemHeight;
end;

procedure THTMLCheckList.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if FAutoFocus then
    SetFocus;
end;


Procedure THTMLCheckList.CMHintShow(Var Msg: TMessage);
var
  CanShow: Boolean;
  hi: PHintInfo;
  Anchor: string;
  Res,Idx: Integer;
  R: TRect;

Begin
  CanShow := True;
  hi := PHintInfo(Msg.LParam);
  Anchor :='';

  if FAnchorHint then
  begin
    Anchor := IsAnchor(hi^.cursorPos.x,hi^.cursorpos.y,res);
    if Anchor <> '' then
    begin
      if Assigned(FOnAnchorHint) then
        FOnAnchorHint(self,res,Anchor);
      hi^.HintPos := ClientToScreen(hi^.CursorPos);
      hi^.Hintpos.y := hi^.Hintpos.Y - 10;
      hi^.Hintpos.x := hi^.Hintpos.X + 10;
      hi^.HintStr := Anchor;
    end;
  end;

  if FItemHint and (Anchor ='') then
  begin
    Idx := SendMessage(Handle,LB_ITEMFROMPOINT,0,MakeLParam(Hi^.CursorPos.X,Hi^.CursorPos.Y));

    SendMessage(Handle,LB_GETITEMRECT,Idx,LParam(@R));

    if PtInRect(R,Point(Hi^.CursorPos.X,Hi^.CursorPos.Y)) then
    begin
      if HTMLHint then
        Hi^.HintStr := Items[Idx]
      else
        Hi^.HintStr := TextItems[Idx];
      Hi^.Hintpos.X := GetItemIndent(Idx);
      Hi^.Hintpos.Y := R.Top;
      Hi^.HintPos := ClientToScreen(Hi^.HintPos);
    end;
  end;

  Msg.Result := Ord(Not CanShow);
end;


procedure THTMLCheckList.ResetItemHeight;
begin
  if HandleAllocated and (Style = lbStandard) then
  begin
    Canvas.Font := Font;
    FStandardItemHeight := Canvas.TextHeight('Wg');
    Perform(LB_SETITEMHEIGHT, 0, FStandardItemHeight);
  end;
end;

procedure THTMLCheckList.DrawItem(Index: Integer; Rect: TRect;
  AState: TOwnerDrawState);
var
  r,hr:TRect;
  ACheckWidth: Integer;
  Enable: Boolean;
  a,s,f:string;
  ml,hl:integer;
  UrlCol: TColor;
  pt:tpoint;
  hrect: TRect;
  XSize, YSize: Integer;
  delta: integer;
  clr: TColor;

begin
  if (FUpdateCount > 0) then
    Exit;

  Canvas.Brush.Color := Color;
  UrlCol := FURLColor;

  r := Rect;
  r.Left := 0;

  if Comment[Index] and (CommentColor <> clNone) then
  begin
    Canvas.Brush.Color := CommentColor;
    Canvas.Pen.Color := CommentColor;
    Canvas.Font.Assign(CommentFont);
    Canvas.Rectangle(r);
  end
  else
  if (odSelected in AState) and ShowSelection then
  begin
    Canvas.Brush.Color := FSelectionColors.ColorFrom;
    Canvas.Pen.Color := FSelectionColors.BorderColor;
    Canvas.Font.Color := FSelectionFontColor;
    UrlCol := FSelectionFontColor;
    SelectionColors.Draw(Canvas, R);
  end
  else
  begin
    Canvas.Brush.Color := Color;
    Canvas.Pen.Color := Color;
    Canvas.Font.Color := Font.Color;
    Canvas.Rectangle(r);
  end;

  clr := Canvas.Font.Color;

  ACheckWidth := GetCheckWidth;

  if (Index < Items.Count) and (Index >= 0) then
  begin
    R := Rect;

    delta := 0;
    if not Ctl3D then
      delta := 2;
    if not Flat then
      delta := 2;

    if SelectionColors.Rounded then
      inc(delta);

    if not UseRightToLeftAlignment then
    begin
      R.Right := Rect.Left + delta;
      R.Left := R.Right - ACheckWidth;
    end
    else
    begin
      R.Left := Rect.Right - delta;
      R.Right := R.Left + ACheckWidth;
    end;

    Enable := Enabled and GetItemEnabled(Index);

    DrawCheck(R, GetState(Index), Enable);

    if not Enable then
      clr := clGrayText;
  end;

  GetCursorPos(pt);
  pt := ScreenToClient(pt);

  hrect := Rect;
  hrect.Left := hrect.Left + 2;

  if not Flat then
    hrect.Left := hrect.Left + 2;

  // force font color update
  Canvas.Font.Color := clNone;
  Canvas.Font.Color := clr;

  HTMLDrawEx(Canvas,Items[Index],hrect,FImages,pt.x,pt.y,-1,-1,FShadowOffset,False,True,False,not FSelectionHTMLColors and (odSelected in AState) and ShowSelection,True,False,not FEllipsis,FFormScaled,1.0,
    urlcol,clNone,clNone,FShadowColor,a,s,f,XSize,YSize,hl,ml,hr,FImageCache,FContainer,FLineSpacing,BidiMode);

  if (YSize < hrect.Bottom - hrect.Top) then
  begin
    hrect.Top := hrect.Top + (hrect.Bottom - hrect.Top - YSize) div 2;
  end
  else
    hrect.Top := hrect.Top + 2;

  HTMLDrawEx(Canvas,Items[Index],hrect,FImages,pt.x,pt.y,-1,-1,FShadowOffset,False,False,False,not FSelectionHTMLColors and (odSelected in AState) and ShowSelection,True,False,not FEllipsis,FFormScaled,1.0,
    urlcol,clNone,clNone,FShadowColor,a,s,f,XSize,YSize,hl,ml,hr,FImageCache,FContainer,FLineSpacing,BidiMode);

  if (odFocused in AState) and FShowFocus and FShowSelection then
  begin
    Rect.Left := 0;
    if SelectionColors.Rounded then
      InflateRect(Rect,-1,-1);
    Canvas.DrawFocusRect(Rect);
    if SelectionColors.Rounded then
      InflateRect(Rect,+1,+1);
  end;

  if (Index = Items.Count - 1) and not FUseVCLStyles then
  begin
    r := GetClientRect;
    if r.Bottom > rect.Bottom then
    begin
      Canvas.Brush.Color := self.Color;
      Canvas.Pen.Color := self.Color;
      Canvas.Rectangle(r.Left,rect.Bottom,rect.right,r.bottom);
    end;
  end;
end;

procedure THTMLCheckList.CNDrawItem(var Message: TWMDrawItem);
var
  State: TOwnerDrawState;
begin
  with Message.DrawItemStruct^ do
  begin
    if Integer(ItemID)<0 then
      Exit;

    if not Comment[ItemID] then
    begin
      if not UseRightToLeftAlignment then
        rcItem.Left := rcItem.Left + GetItemIndent(ItemID)
      else
        rcItem.Right := rcItem.Right - GetItemIndent(ItemID);
    end;

    State := TOwnerDrawState(LongRec(itemState).Lo);
    Canvas.Handle := hDC;
    Canvas.Font := Font;
    Canvas.Brush := Brush;

    if Integer(itemID) >= 0 then
      DrawItem(itemID, rcItem, State);

    Canvas.Handle := 0;
   end;
end;

procedure THTMLCheckList.CommentFontChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure THTMLCheckList.DragDrop(Source: TObject; X, Y: integer);
var
  idx: integer;
begin
  inherited;
  if Reorder then
  begin
    idx := ItemAtPos(Point(X, Y), True);
    if (idx <> -1) and (FItemDragIdx <> -1) and (FItemDragIdx <> idx) then
    begin
      MoveItem(FItemDragIdx, idx);
      FItemDragIdx := -1;
    end;
  end;
end;

procedure THTMLCheckList.DragOver(Source: TObject; X, Y: integer;
  State: TDragState; var Accept: Boolean);
var
  idx,i: integer;
  hascomm: boolean;

begin
  inherited;

  if Reorder then
  begin
    Idx := ItemAtPos(Point(X, Y), True);

    if idx <> -1 then
    begin
      hascomm := false;

      if not ReorderComments then
      begin

        for i := FItemDragIdx to Idx do
        begin
          if Comment[i] then
            hascomm := true;
        end;

        for i := Idx to FItemDragIdx do
        begin
          if Comment[i] then
            hascomm := true;
        end;
      end;

      Accept := Accept or (Source = Self);
      Accept := Accept and not GetComment(idx) and not hascomm;
    end;
  end;
end;

procedure THTMLCheckList.DrawCheck(R: TRect; AState: TCheckBoxState; AEnabled: Boolean);
var
  DrawState: Integer;
  DrawRect: TRect;
  HTHeme: THandle;
  UseWinXP: Boolean;
  sz: TSize;
  {$IFDEF DELPHIXE2_LVL}
  lDetails: TThemedElementDetails;
  {$ENDIF}
begin
//  SaveRgn := 0;
  DrawRect.Left := R.Left + (R.Right - R.Left - FCheckWidthForDPI) div 2;
  DrawRect.Top := R.Top + (R.Bottom - R.Top - FCheckHeightForDPI) div 2;
  DrawRect.Right := DrawRect.Left + FCheckWidthForDPI;
  DrawRect.Bottom := DrawRect.Top + FCheckHeightForDPI;



  case AState of
    cbChecked:
      DrawState := DFCS_BUTTONCHECK or DFCS_CHECKED;
    cbUnchecked:
      DrawState := DFCS_BUTTONCHECK;
    else // cbGrayed
      DrawState := DFCS_BUTTON3STATE or DFCS_CHECKED;
  end;

  if not AEnabled then
    DrawState := DrawState or DFCS_INACTIVE;


  if FUseVCLStyles then
  begin
    {$IFDEF DELPHIXE2_LVL}
    if AState = cbChecked then
      lDetails := StyleServices.GetElementDetails(tbCheckBoxCheckedNormal)
    else if AState = cbUnchecked then
      lDetails := StyleServices.GetElementDetails(tbCheckBoxUncheckedNormal)
    else
      lDetails := StyleServices.GetElementDetails(tbCheckBoxMixedNormal);

    {$IFNDEF DELPHIXE13_LVL}
    StyleServices.DrawElement(Canvas.Handle, lDetails, R);
    {$ENDIF}
    {$IFDEF DELPHIXE13_LVL}
    StyleServices.DrawElement(Canvas.Handle, lDetails, R, Nil, CurrentPPI);
    {$ENDIF}

    {$ENDIF}
  end
  else
  begin
    with Canvas do
    begin

      if FIsWinXP then
        UseWinXP := IsThemeActive
      else
        UseWinXP := False;

      if UseWinXP then
      begin
        HTHeme := OpenThemeData(self.Handle,'button');

        GetThemePartSize(HTheme, Canvas.Handle, BP_CHECKBOX, CBS_CHECKEDNORMAL, nil, TS_DRAW, sz);


        DrawRect := Rect(DrawRect.Left, DrawRect.Top, DrawRect.Left + sz.cx, DrawRect.Top + sz.cy);


        case AState of
        cbChecked:
          if AEnabled then
            DrawThemeBackground(HTheme,Canvas.Handle, BP_CHECKBOX,CBS_CHECKEDNORMAL,@DrawRect,nil)
          else
            DrawThemeBackground(HTheme,Canvas.Handle, BP_CHECKBOX,CBS_CHECKEDDISABLED,@DrawRect,nil);

        cbUnChecked:
          if AEnabled then
            DrawThemeBackground(HTheme,Canvas.Handle, BP_CHECKBOX,CBS_UNCHECKEDNORMAL,@DrawRect,nil)
          else
            DrawThemeBackground(HTheme,Canvas.Handle, BP_CHECKBOX,CBS_UNCHECKEDDISABLED,@DrawRect,nil);

        cbGrayed:
          if AEnabled then
            DrawThemeBackground(HTheme,Canvas.Handle, BP_CHECKBOX,CBS_MIXEDNORMAL,@DrawRect,nil)
          else
            DrawThemeBackground(HTheme,Canvas.Handle, BP_CHECKBOX,CBS_MIXEDDISABLED,@DrawRect,nil);
        end;

        CloseThemeData(HTHeme);
      end
      else
      begin
        DrawFrameControl(Handle, DrawRect, DFC_BUTTON, DrawState);
      end;
    end;
  end;
end;

procedure THTMLCheckList.SetChecked(Index: Integer; Checked: Boolean);
begin
  if Checked <> GetChecked(Index) then
  begin
    THTMLCheckListDataWrapper(GetWrapper(Index)).SetChecked(Checked);
    InvalidateCheck(Index);
  end;
end;

procedure THTMLCheckList.SetItemEnabled(Index: Integer; const Value: Boolean);
begin
  if Value <> GetItemEnabled(Index) then
  begin
    THTMLCheckListDataWrapper(GetWrapper(Index)).Disabled := not Value;
    InvalidateCheck(Index);
  end;
end;

procedure THTMLCheckList.SetState(Index: Integer; AState: TCheckBoxState);
begin
  if AState <> GetState(Index) then
  begin
    THTMLCheckListDataWrapper(GetWrapper(Index)).State := AState;
    InvalidateCheck(Index);
  end;
end;

procedure THTMLCheckList.SetIndent(Index: Integer; AIndent: integer);
begin
  if AIndent <> GetIndent(Index) then
  begin
    THTMLCheckListDataWrapper(GetWrapper(Index)).Indent := AIndent;
    InvalidateItem(Index);
  end;
end;

procedure THTMLCheckList.SetComment(Index: Integer; AComment: Boolean);
begin
  if AComment <> GetComment(Index) then
  begin
    THTMLCheckListDataWrapper(GetWrapper(Index)).Comment := AComment;
    InvalidateItem(Index);
  end;
end;


procedure THTMLCheckList.InvalidateCheck(Index: Integer);
var
  R: TRect;
begin
  R := ItemRect(Index);
  
  if not UseRightToLeftAlignment then
    R.Right := R.Left + GetItemIndent(Index)
  else
    R.Left := R.Right - GetItemIndent(Index);

  {$IFDEF DELPHI2007_LVL}
  Invalidate;
  {$ELSE}
  InvalidateRect(Handle, @R, not (csOpaque in ControlStyle));
  {$ENDIF}
  UpdateWindow(Handle);
end;

procedure THTMLCheckList.InvalidateItem(Index: Integer);
var
  R: TRect;
begin
  R := ItemRect(Index);
  InvalidateRect(Handle, @R, not (csOpaque in ControlStyle));
//  UpdateWindow(Handle);
end;

function THTMLCheckList.GetChecked(Index: Integer): Boolean;
begin
  if HaveWrapper(Index) then
    Result := THTMLCheckListDataWrapper(GetWrapper(Index)).GetChecked
  else
    Result := False;
end;

function THTMLCheckList.GetItemEnabled(Index: Integer): Boolean;
begin
  if HaveWrapper(Index) then
    Result := not THTMLCheckListDataWrapper(GetWrapper(Index)).Disabled
  else
    Result := True;
end;

function THTMLCheckList.GetState(Index: Integer): TCheckBoxState;
begin
  if HaveWrapper(Index) then
    Result := THTMLCheckListDataWrapper(GetWrapper(Index)).State
  else
    Result := THTMLCheckListDataWrapper.GetDefaultState;
end;

function THTMLCheckList.GetIndent(Index: Integer): integer;
begin
  if HaveWrapper(Index) then
    Result := THTMLCheckListDataWrapper(GetWrapper(Index)).Indent
  else
    Result := 0;
end;

function THTMLCheckList.GetComment(Index: Integer): Boolean;
begin
  if HaveWrapper(Index) then
    Result := THTMLCheckListDataWrapper(GetWrapper(Index)).Comment
  else
    Result := False;
end;

{$IFDEF DELPHIXE10_LVL}
procedure THTMLCheckList.ChangeScale(M, D: Integer; isDpiChange: Boolean);
begin
  if isDPIChange then
    GetCheckSizeForDPI;
  inherited;
end;
{$ENDIF}

procedure THTMLCheckList.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Index: Integer;
  anchor: string;
  delta: integer;

begin
  inherited;

  if Button = mbLeft then
  begin
    if not Ctl3D then
      delta := 2
    else
      delta := 0;


    FMouseDownOnItemX := X;
    FMouseDownOnItemY := Y;
    Index := ItemAtPos(Point(X,Y),True);

    if ReorderComments or not GetComment(Index) then
      FItemDragIdx := Index
    else
      FItemDragIdx := -1;

    FMouseDown := True;

    ItemIndex := Index;

    if (Index <> -1) and GetItemEnabled(Index) and not GetComment(Index) then
      if not UseRightToLeftAlignment then
      begin
        if (X - ItemRect(Index).Left - delta < GetItemIndent(Index)) and
	   (X - ItemRect(Index).Left - delta > (GetCheckWidth * (GetIndent(Index)))) then
          ToggleClickCheck(Index)
      end
      else
      begin
        Dec(X, ItemRect(Index).Right - GetCheckWidth);
        if (X > 0) and (X < GetCheckWidth) then
          ToggleClickCheck(Index)
      end;
  end;

  Anchor := IsAnchor(X,Y,index);
  if Anchor <> '' then
  begin
    if (Pos('://',Anchor) > 0) or (Pos('mailto:',Anchor) > 0) then
      ShellExecute(0,'open',PChar(Anchor),nil,nil,SW_NORMAL)
    else
      begin
        if Assigned(FOnAnchorClick) then
          FOnAnchorClick(Self,index,Anchor);
      end;
    Exit;
  end;

end;

procedure THTMLCheckList.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Anchor: string;
  idx:integer;
  R: TRect;
begin
  inherited;

  if ((Abs(FMouseDownOnItemX - X) > 5) or (Abs(FMouseDownOnItemY - Y) > 5)) and (FItemDragIdx <> -1) and FMouseDown and Reorder then
  begin
    FDragging := True;
    FMouseDown := false;
    BeginDrag(true);
    Exit;
  end;

  if FItemHint then
  begin
    Idx := SendMessage(Handle,LB_ITEMFROMPOINT,0,MakeLParam(X,Y));
    SendMessage(Handle,LB_GETITEMRECT,Idx,LParam(@R));

    if PtInRect(R,Point(X,Y)) then
    begin
      if Idx <> FLastHintPos then
      begin
        Application.CancelHint;
        FLastHintPos := Idx;
      end
    end
    else
    begin
      if FLastHintPos >= 0 then
      begin
        Application.CancelHint;
        FLastHintPos := -1;
      end;
    end;
  end;
  

  Anchor := IsAnchor(x,y,idx);

  if Anchor <> '' then
  begin
    if (FOldAnchor <> Anchor) and (FOldAnchor <> '') then
    begin
      Application.Cancelhint;
      if Assigned(FOnAnchorExit) then
        FOnAnchorExit(self,idx,FOldAnchor);
    end;

    if self.Cursor <> crHandPoint then
    begin
      FOldCursor := self.Cursor;
      self.Cursor := crHandPoint;
    end;

    if (Anchor <> FOldAnchor) then
      if Assigned(FOnAnchorEnter) then
        FOnAnchorEnter(self,idx,Anchor);

    FOldAnchor := Anchor;
  end
  else
  begin
    if self.Cursor = crHandPoint then
    begin
      if (FOldAnchor <> '') then
      begin
        Application.CancelHint;
        self.Cursor := FOldCursor;
        if Assigned(FOnAnchorExit) then
          FOnAnchorExit(self,idx,fOldAnchor);
        FOldAnchor := '';
      end;
    end;
  end;

end;

procedure THTMLCheckList.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  FMouseDown := False;
  if FDragging then
  begin
    FDragging := false;
    Exit;
  end;
end;

procedure THTMLCheckList.MoveItem(FromIndex, ToIndex: integer);
var
  chk: boolean;
  cmt: boolean;
  dat: TObject;
begin
  chk := Checked[FromIndex];
  cmt := Comment[FromIndex];
  dat := Items.Objects[FromIndex];

  Items.Move(FromIndex, ToIndex);


  Checked[ToIndex] := chk;
  Comment[ToIndex] := cmt;
  Items.Objects[ToIndex] := dat;
end;

procedure THTMLCheckList.ToggleClickCheck;
var
  State: TCheckBoxState;
begin
  if (Index >= 0) and (Index < Items.Count) and GetItemEnabled(Index) then
  begin

    State := Self.State[Index];
    case State of
      cbUnchecked:
        if AllowGrayed then State := cbGrayed else State := cbChecked;
      cbChecked: State := cbUnchecked;
      cbGrayed: State := cbChecked;
    end;
    Self.State[Index] := State;

    ClickCheck(Index);
  end;
end;

procedure THTMLCheckList.CheckAll;
begin
  CheckAllInt;
end;

procedure THTMLCheckList.CheckAllInt(AState: TCheckBoxState);
var
  i:integer;
begin
  for i := 0 to Items.Count - 1 do
    State[i] := AState;
end;

procedure THTMLCheckList.CheckAllInt(check: boolean = true);
var
  i:integer;
begin
  for i := 0 to Items.Count - 1 do
    Checked[i] := check;
end;

procedure THTMLCheckList.ClickCheck(index: integer);
begin
  if Assigned(FOnCheckClick) then
    FOnCheckClick(Self, index);
end;

{$IFNDEF DELPHIXE2_LVL}
function THTMLCheckList.GetItemData(Index: Integer): LongInt;
{$ENDIF}
{$IFDEF DELPHIXE2_LVL}
function THTMLCheckList.GetItemData(Index: Integer): TListBoxItemData;
{$ENDIF}
begin
  Result := 0;
  if HaveWrapper(Index) then
    Result := THTMLCheckListDataWrapper(GetWrapper(Index)).FData;
end;

function THTMLCheckList.GetWrapper(Index: Integer): TObject;
begin
  Result := ExtractWrapper(Index);
  if Result = nil then
    Result := CreateWrapper(Index);
end;

procedure THTMLCheckList.GrayAll;
begin
  CheckAllInt(cbGrayed);
end;

function THTMLCheckList.ExtractWrapper(Index: Integer): TObject;
begin
  Result := nil;

  if (Index < 0) then
    Exit;

  Result := THTMLCheckListDataWrapper(inherited GetItemData(Index));
  if (Result <> nil) and (not (Result is THTMLCheckListDataWrapper)) then
    Result := nil;
end;

{$IFNDEF DELPHIXE2_LVL}
function THTMLCheckList.InternalGetItemData(Index: Integer): LongInt;
{$ENDIF}
{$IFDEF DELPHIXE2_LVL}
function THTMLCheckList.InternalGetItemData(Index: Integer): TListBoxItemData;
{$ENDIF}
begin
  Result := inherited GetItemData(Index);

  if Result <> 0 then
    Result := THTMLCheckListDataWrapper(Result).FData;
end;

{$IFNDEF DELPHIXE2_LVL}
procedure THTMLCheckList.InternalSetItemData(Index: Integer; AData: LongInt);
{$ENDIF}
{$IFDEF DELPHIXE2_LVL}
procedure THTMLCheckList.InternalSetItemData(Index: Integer; AData: TListBoxItemData);
{$ENDIF}
begin
  SetItemData(Index, AData);
end;

function THTMLCheckList.CreateWrapper(Index: Integer): TObject;
begin
  Result := THTMLCheckListDataWrapper.Create;
  {$IFDEF DELPHIXE2_LVL}
  inherited SetItemData(Index, NativeInt(Result));
  {$ENDIF}
  {$IFNDEF DELPHIXE2_LVL}
  inherited SetItemData(Index, LongInt(Result));
  {$ENDIF}
end;

function THTMLCheckList.HaveWrapper(Index: Integer): Boolean;
begin
  Result := ExtractWrapper(Index) <> nil;
end;

{$IFNDEF DELPHIXE2_LVL}
procedure THTMLCheckList.SetItemData(Index: Integer; AData: LongInt);
{$ENDIF}
{$IFDEF DELPHIXE2_LVL}
procedure THTMLCheckList.SetItemData(Index: Integer; AData: TListBoxItemData);
{$ENDIF}
var
  Wrapper: THTMLCheckListDataWrapper;
  SaveState: THTMLCheckListDataWrapper;

begin
  Wrapper := THTMLCheckListDataWrapper(GetWrapper(Index));
  Wrapper.FData := AData;

  if FSaveStates <> nil then
    if FSaveStates.Count > 0 then
    begin
      SaveState := FSaveStates[0];
      Wrapper.FState := SaveState.State;
      Wrapper.FDisabled := SaveState.Disabled;
      Wrapper.FIndent := SaveState.Indent;
      Wrapper.FComment := SaveState.Comment;
      FSaveStates.Delete(0);
    end;

  {$IFDEF DELPHIXE2_LVL}
  inherited SetItemData(Index, NativeInt(Wrapper));
  {$ENDIF}
  {$IFNDEF DELPHIXE2_LVL}
  inherited SetItemData(Index, LongInt(Wrapper));
  {$ENDIF}
end;

procedure THTMLCheckList.ResetContent;
var
  I: Integer;
begin
  for I := 0 to Items.Count - 1 do
    if HaveWrapper(I) then
      GetWrapper(I).Free;
  inherited;
end;

procedure THTMLCheckList.DeleteString(Index: Integer);
begin
  if HaveWrapper(Index) then
    GetWrapper(Index).Free;
  inherited;
end;

procedure THTMLCheckList.SetFlat(Value: Boolean);
begin
  if Value <> FFlat then
  begin
    FFlat := Value;
    Invalidate;
  end;
end;

procedure THTMLCheckList.SetDuplicates(Value: Boolean);
var
  i,j: Integer;
  FTempList: TStringList;

begin
  if Value <> FDuplicates then
  begin
    FTempList := TStringList.Create;
    FTempList.Sorted := True;

    FDuplicates := Value;
    if not FDuplicates then
    begin
      i := 1;
      FDuplicateList.Clear;
      while (i <= self.Items.Count) do
      begin
        if FTempList.IndexOf(Items[i - 1]) >= 0 then
        begin
          j := FDuplicateList.Add(Items[i - 1]);
          FDuplicateList.Objects[j] := TSaveObject.Create(GetIndent(i - 1),Items.Objects[i - 1]);
          Items.Delete(i - 1);
        end
        else
        begin
          FTempList.Add(Items[i-1]);
          inc(i);
        end;
      end;

      while (i < Items.Count) do
      begin
        if (AnsiCompareText(Items[i-1],Items[i])=0) then
        begin
          j := FDuplicateList.Add(Items[i]);
          FDuplicateList.Objects[j] := TSaveObject.Create(GetIndent(i),Items.Objects[i]);
          Items.Delete(i);
        end
        else inc(i);
      end;
    end
    else
    begin
      for i := 1 to FDuplicateList.Count do
      begin
        j := Items.Add(FDuplicateList.Strings[i - 1]);
        Indent[j] := TSaveObject(FDuplicateList.Objects[i - 1]).FIndent;
        Items.Objects[j] := TSaveObject(FDuplicateList.Objects[i - 1]).FObject;
      end;
    end;

    FTempList.Free;
  end;
end;

function THTMLCheckList.GetSelectCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to Items.Count do
  begin
    if Checked[i-1] then
      Result := Result + 1;
  end;
end;

procedure THTMLCheckList.QuickSortItems(left,right:integer);
var
  i,j: Integer;
  s: string;
  o: TObject;
  w1,w2: Integer;
begin
  i := Left;
  j := Right;

  s := Items[(left+right) shr 1];

  repeat
    while (s > Items[i]) do Inc(i);
    while (s < Items[j]) do Dec(j);

    if (i <= j) then
    begin
      if (i <> j) then
      begin
        s := Items[i];
        o := Items.Objects[i];

        w1 := inherited GetItemData(i);
        w2 := inherited GetItemData(j);
        Items[i] := Items[j];
        Items.Objects[i] := Items.Objects[j];
        Items[j] := s;
        Items.Objects[j] := o;

        inherited SetItemData(i,w2);
        inherited SetItemData(j,w1);
      end;
      inc(i);
      dec(j);
    end;
  until (i > j);

  if (left < j) then QuicksortItems(left,j);
  if (i < right) then QuickSortItems(i,right);
end;

procedure THTMLCheckList.SortComment(Index:integer);
var
  j:Integer;
begin
  if (Items.Count = 0) then
    Exit;

  j := Index + 1;
  while (j <= Items.Count) and not Comment[j] do
    inc(j);

  QuickSortItems(Index + 1,j - 1);
end;

procedure THTMLCheckList.SortAllComments;
var
  i,j: Integer;
begin
  if Items.Count = 0 then
    Exit;

  i := 0;
  j := 0;

  repeat

    if Comment[j] then
    begin
      if (j > i) then
        QuickSortItems(i,j-1);
      i := j + 1;
    end;
    inc(j);

  until (j = Items.Count);

  QuickSortItems(i,j-1);
end;


function THTMLCheckList.GetSelected(Index: Integer): string;
var
  i,j: Integer;
begin
  Result := '';

  j := 0;

  for i := 0 to Items.Count - 1 do
    if Checked[i] then
    begin
      if (j = Index) then
      begin
        Result := Items[i];
        Break;
      end
      else
        inc(j);
    end;
end;

procedure THTMLCheckList.WMDestroy(var Msg: TWMDestroy);
var
  i: Integer;
begin
  for i := 0 to Items.Count -1 do
    ExtractWrapper(i).Free;
  inherited;
end;

function THTMLCheckList.IsAnchor(x,y:integer;var idx: Integer):string;
var
  res: Integer;
  hr, r: TRect;
  a,s,f: string;
  xsize,ysize: Integer;
  ml,hl: Integer;

begin
  Result := '';
  idx := -1;

  res := loword(SendMessage(Handle,LB_ITEMFROMPOINT,0,makelparam(X,Y)));

  if (res >= 0) and (res < Items.Count) then
  begin
    SendMessage(Handle, LB_GETITEMRECT, res, LParam(@r));

    idx := res;

    r.Left := r.Left + GetItemIndent(res) + 2;

    if not Flat then
      r.Left := r.Left + 2;

    r.Top := r.Top + 2;

    if HTMLDrawEx(Canvas,items[res],r,FImages,X,Y,-1,-1,FShadowOffset,True,False,False,true,True,False,not FEllipsis,FFormScaled,1.0,
      clNone,clNone,clNone,FShadowColor,a,s,f,XSize,YSize,hl,ml,hr,FImageCache,FContainer,FLineSpacing,BidiMode) then
      Result := a;
  end;
end;

procedure THTMLCheckList.KeyDown(var Key: Word; Shift: TShiftState);
var
  selidx,i: integer;
  frst,frststate: boolean;
begin
  if Key = VK_SPACE then
  begin
    begin
      frst := false;
      frststate := false;
      for I := 0 to Items.Count - 1 do
      begin
        if TCustomListBox(Self).Selected[I] then
        begin
          if not frst then
          begin
            frststate := not Checked[i];
            frst := true;
          end;
          Checked[I] := frststate;
          ClickCheck(I);
        end;
      end;
    end;
    Key := 0;
  end;

  inherited;

  selidx := itemindex;

  if reorder and (selidx >= 0) and not (comment[selidx]) then
  begin
    selidx := ItemIndex;
    if (Key = VK_DOWN) and (ssCtrl in Shift)  then
    begin
      if (selidx < Items.Count - 1) and not (Comment[selidx + 1]) then
      begin
        MoveItem(selidx, selidx + 1);
        ItemIndex := selidx;
      end;
    end;

    if (Key = VK_UP) and (ssCtrl in Shift)  then
    begin
      if (selidx > 0) and (not Comment[selidx - 1]) then
      begin
        MoveItem(selidx, selidx - 1);
        ItemIndex := selidx;
      end;
    end;
  end;
end;

procedure THTMLCheckList.KeyUp(var Key: Word; Shift: TShiftState);
begin
   case Key of
    Ord('A'):
    begin
      if (ssCtrl in Shift) then
        SelectAll;
    end;
  end;
  inherited;
end;

procedure THTMLCheckList.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) then
  begin
    InitVCLStyle(false);
  end;
end;

procedure THTMLCheckList.SetImageList(const Value: TCustomImageList);
begin
  FImages := Value;
  Invalidate;
end;

procedure THTMLCheckList.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  if (AOperation = opRemove) and (AComponent = FImages) then
    FImages := nil;
  inherited;
end;

procedure THTMLCheckList.SetShadowColor(const Value: TColor);
begin
  FShadowColor := Value;
  Invalidate;
end;

procedure THTMLCheckList.SetShadowOffset(const Value: integer);
begin
  FShadowOffset := Value;
  Invalidate;
end;

procedure THTMLCheckList.BeginUpdate;
begin
  if FUpdateCount = 0 then
    SendMessage(Handle,WM_SETREDRAW,integer(False),0);
  Inc(FUpdateCount);
end;

procedure THTMLCheckList.EndUpdate;
begin
  if FUpdateCount>0 then
  begin
    dec(FUpdateCount);
    if FUpdateCount = 0 then
    begin
      SendMessage(Handle,WM_SETREDRAW,integer(True),0);
      Invalidate;
    end;
  end;
end;

procedure THTMLCheckList.WMEraseBkGnd(var Message: TMessage);
begin
  //if FUpdateCount > 0 then
  //  message.Result := 0
  //else
    inherited;
  //FUpdateCount := 0;
end;

procedure THTMLCheckList.WMKillFocus(var Message: TMessage);
begin
  inherited;
  if Cursor <> FOldCursor then
    Cursor := FOldCursor;
end;

procedure THTMLCheckList.WMLButtonDown(var Message: TWMLButtonDown);
var
  ItemNo : Integer;
  ShiftState: TShiftState;
begin
  ItemNo := ItemAtPos(SmallPointToPoint(Message.Pos), True);
  ShiftState := KeysToShiftState(Message.Keys);
  if (DragMode = dmAutomatic) and FMultiSelect then
  begin
    if not (ssShift in ShiftState) or (ssCtrl in ShiftState) then
    begin
      if (ItemNo >= 0) and (TCustomListBox(Self).Selected[ItemNo]) then
      begin
        BeginDrag (False);
        Exit;
      end;
    end;
  end;

  if ItemNo <> -1 then
  begin
    if not (not EnableCommentSelection and GetComment(ItemNo)) then
      inherited;
  end;
  if (DragMode = dmAutomatic) and not (FMultiSelect and
    ((ssCtrl in ShiftState) or (ssShift in ShiftState))) then
    BeginDrag(False);
end;

{$IFDEF DELPHIXE2_LVL}
procedure THTMLCheckList.CMStyleChanged(var Message: TMessage);
begin
  InitVCLStyle(True);
  {$IFDEF DELPHIXE10_LVL}
  GetCheckSizeForDPI;
  {$ENDIF}
  Invalidate;
end;
{$ENDIF}

procedure THTMLCheckList.InitVCLStyle(init: boolean);
{$IFDEF DELPHIXE2_LVL}
const
  lColorStates: array[Boolean] of TStyleColor = (scListBoxDisabled, scListBox);
  lFontColorStates: array[Boolean] of TStyleFont = (sfListItemTextDisabled, sfListItemTextNormal);
var
  clr: TColor;
{$ENDIF}
begin
  FUseVCLStyles := False;

{$IFDEF DELPHIXE2_LVL}
  if CheckVCLStylesEnabled(StyleServices, (csDesigning in ComponentState)) then
  begin
    FUseVCLStyles := True;

    {$IFDEF DELPHIXE6_LVL}
    if (seClient in StyleElements) then
    begin
    {$ENDIF}
      Color := StyleServices.GetStyleColor(lColorStates[Enabled]);

      if CommentColor <> clNone then
        CommentColor := StyleServices.GetSystemColor(clHighlightText);

      SelectionColors.BorderColor := StyleServices.GetSystemColor(clWindowFrame);

      //clr := StyleServices.GetSystemColor(clHighlight);
      clr := clHighLight;
      SelectionColors.ColorFrom := clr;
      SelectionColors.ColorTo := clr;
      SelectionColors.ColorMirrorFrom := clNone;
      SelectionColors.ColorMirrorTo := clNone;
    {$IFDEF DELPHIXE6_LVL}
    end;
    {$ENDIF}

    {$IFDEF DELPHIXE6_LVL}
    if seFont in StyleElements then
    {$ENDIF}
      Font.Color := StyleServices.GetStyleFontColor(lFontColorStates[Enabled]);
  end;
{$ENDIF}
end;

procedure THTMLCheckList.SetLineSpacing(const Value: Integer);
begin
  FLineSpacing := Value;
  Invalidate;
end;

procedure THTMLCheckList.SetSelectionColors(const Value: TGradientStyle);
begin
  FSelectionColors.Assign(Value);
  Invalidate;
end;

procedure THTMLCheckList.SetSelectionFontColor(const Value: TColor);
begin
  FSelectionFontColor := Value;
  Invalidate;
end;

procedure THTMLCheckList.SetEllipsis(const Value: Boolean);
begin
  FEllipsis := Value;
end;

procedure THTMLCheckList.SetEnableCommentSelection(const Value: Boolean);
begin
  if FEnableCommentSelection <> Value then
  begin
    FEnableCommentSelection := Value;
    Invalidate;
  end;
end;

procedure THTMLCheckList.SetURLColor(const Value: TColor);
begin
  if FURLColor <> Value then
  begin
    FURLColor := Value;
    Invalidate;
  end;   
end;


function THTMLCheckList.GetTextItem(index: integer): string;
begin
  if (Index >= 0) and (Index < Items.Count) then
  begin
    Result := HTMLStrip(self.Items[index]);
  end
  else
    raise Exception.Create('Item index out of range');
end;

procedure THTMLCheckList.HilightInItem(Index: Integer; HiText: string;
  DoCase: Boolean);
begin
  Items[Index] := Hilight(Items[Index],HiText,'hi',DoCase);
end;

procedure THTMLCheckList.HilightInList(HiText: string; DoCase: Boolean);
var
  i: Integer;
begin
  BeginUpdate;
  for i := 1 to Items.Count do
    Items[i - 1] := Hilight(Items[i - 1],HiText,'hi',DoCase);
  EndUpdate;
end;

procedure THTMLCheckList.MarkInItem(Index: Integer; HiText: string;
  DoCase: Boolean);
begin
  Items[Index] := Hilight(Items[Index],HiText,'e',DoCase);
end;

procedure THTMLCheckList.MarkInList(HiText: string; DoCase: Boolean);
var
  i: Integer;
begin
  BeginUpdate;
  for i := 1 to Items.Count do
    Items[i - 1] := Hilight(Items[i - 1],HiText,'e',DoCase);
  EndUpdate;
end;

procedure THTMLCheckList.UnCheckAll;
begin
  CheckAllInt(false);
end;

procedure THTMLCheckList.UnHilightInItem(Index: Integer);
begin
  Items[Index] := UnHilight(Items[Index],'hi');
end;

procedure THTMLCheckList.UnHilightInList;
var
  i: Integer;
begin
  BeginUpdate;
  for i := 1 to Items.Count do
    Items[i - 1] := UnHilight(Items[i - 1],'hi');
  EndUpdate;
end;

procedure THTMLCheckList.UnMarkInItem(Index: Integer);
begin
  Items[Index] := UnHilight(Items[Index],'e');
end;

procedure THTMLCheckList.UnMarkInList;
var
  i: Integer;
begin
  BeginUpdate;
  for i := 1 to Items.Count do
    Items[i - 1] := UnHilight(Items[i - 1],'e');
  EndUpdate;
end;


procedure THTMLCheckList.SetContainer(const Value: TPictureContainer);
begin
  FContainer := Value;
  Invalidate;
end;

procedure THTMLCheckList.SetComponentStyle(AStyle: TTMSStyle);
begin
  FTMSStyle := AStyle;
  if AStyle = tsCustom then
  Exit;

  SelectionColors.SetStyle(AStyle);

  case AStyle of
    tsOffice2019White:
      begin
        if Flat and Assigned(Parent) then
        begin
          Color := (Parent as TWinControl).Brush.Color;
        end
        else
          Color := clWhite;
        SelectionFontColor := $003B3B3B;
        Font.Color := $00444648;
      end;
      tsOffice2019Gray:
      begin
        if Flat and Assigned(Parent) then
        begin
          Color := (Parent as TWinControl).Brush.Color;
          SelectionFontColor := clWhite;
        end
        else
        begin
          Color := $00B8BBBE;
        end;
        SelectionFontColor := $003B3B3B;
        Font.Color := $00232425;
      end;
      tsOffice2019Black:
      begin
        if Flat and Assigned(Parent) then
          Color := (Parent as TWinControl).Brush.Color
        else
          Color := $00444444;
        SelectionFontColor := clWhite;
        Font.Color := clWhite;
      end
      else
      begin
        if Flat and ParentColor and Assigned(Parent) then
          Color := (Parent as TWinControl).Brush.Color
        else
          Color := clWhite;

        if not (AStyle in [tsWindowsXP]) then
          SelectionFontColor := clBlack
        else
          SelectionFontColor := clHighlightText;
        Font.Color := clWindowText;
      end;
  end;
end;

function THTMLCheckList.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function THTMLCheckList.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure THTMLCheckList.SetVersion(const Value: string);
begin

end;

{$IFDEF DELPHIXE10_LVL}
procedure THTMLCheckList.GetCheckSizeForDPI;
var
  DC: HDC;
  LCheckSize: TSize;
  LStyle: TCustomStyleServices;
begin
  {$IFDEF DELPHIXE13_LVL}
  LStyle := StyleServices(Self);
  {$ENDIF}
  {$IFNDEF DELPHIXE13_LVL}
  LStyle := StyleServices;
  {$ENDIF}
  if LStyle.Enabled then
  begin
    DC := CreateCompatibleDC(0);
    try
      {$IFDEF DELPHIXE13_LVL}
      LStyle.GetElementSize(DC, LStyle.GetElementDetails(tbCheckBoxCheckedNormal), esActual, LCheckSize, CurrentPPI);
      if (LCheckSize.Width <= 0) or (LCheckSize.Height <= 0) then
      begin
        LStyle := TStyleManager.SystemStyle;
        LStyle.GetElementSize(DC, LStyle.GetElementDetails(tbCheckBoxCheckedNormal), esActual, LCheckSize, CurrentPPI);
      end;
      {$ENDIF}
      {$IFNDEF DELPHIXE13_LVL}
      LStyle.GetElementSize(DC, LStyle.GetElementDetails(tbCheckBoxCheckedNormal), esActual, LCheckSize);
      if (LCheckSize.Width <= 0) or (LCheckSize.Height <= 0) then
      begin
        LStyle := TStyleManager.SystemStyle;
        LStyle.GetElementSize(DC, LStyle.GetElementDetails(tbCheckBoxCheckedNormal), esActual, LCheckSize);
      end;
      {$ENDIF}

      FCheckWidthForDPI := LCheckSize.Width;
      FCheckHeightForDPI := LCheckSize.Height;
    finally
      DeleteDC(DC);
    end;
  end
  else
    with TBitmap.Create do
      try
        Handle := LoadBitmap(0, PChar(32759));
        FCheckWidth := Width div 4;
        FCheckHeight := Height div 3;
      finally
        Free;
      end;
end;
{$ENDIF}

{ TStateList }

function TStateList.Add(const Value: THTMLCheckListDataWrapper): Integer;
begin
  Result := inherited Add(nil);
  SetItem(Result, Value);
end;

function TStateList.GetItem(const Index: Integer): THTMLCheckListDataWrapper;
begin
  Result := inherited Items[Index];
end;

procedure TStateList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited;
  if (Action = lnDeleted) and (Ptr <> nil) then TObject(Ptr).Free;
end;

procedure TStateList.SetItem(const Index: Integer;
  const Value: THTMLCheckListDataWrapper);
var
  AItem: THTMLCheckListDataWrapper;
begin
  AItem := THTMLCheckListDataWrapper.Create;
  try
    AItem.State := Value.State;
    AItem.Disabled := Value.Disabled;
    AItem.Indent := Value.Indent;
    AItem.Comment := Value.Comment;
  except
    AItem.Free;
    raise;
  end;
  inherited Items[Index] := AItem;
end;


end.

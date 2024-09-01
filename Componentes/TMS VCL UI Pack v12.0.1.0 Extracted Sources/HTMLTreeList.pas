{*************************************************************************}
{ THTMLTreeList component                                                 }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright � 2000 - 2023                                       }
{           Email : info@tmssoftware.com                                  }
{           Web : https://www.tmssoftware.com                             }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit HTMLTreeList;

{$I TMSDEFS.INC}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Clipbrd, Mask, ComCtrls, ExtCtrls, PictureContainer, ImgList,
  AdvStyleIF, AdvGradient, CommCtrl
  , Types
  {$IFDEF DELPHIXE2_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 8; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 2; // Build nr.

  // version history
  // v1.0.0.0 : First release
  // v1.0.0.1 : Fixed issue with HotTrack display
  // v1.0.0.2 : Fixed painting issue when updating nodes
  // v1.0.0.3 : Fixed issue with VCL.NET
  // v1.0.0.4 : Fixed issue with bordersize of header
  // v1.0.0.5 : Fixed painting issue on double click

  // v1.1.0.0 : New : SelectionColors property added, Office2007, Windows Vista, Windows 7 color styles
  //          : New : TAdvFormStyler / ITMSStyle interface compatible
  //          : Improved : DFM property persistence
  // v1.1.0.1 : Fixed : issue with header painting during horizontal scroll
  // v1.1.0.2 : Fixed : issue with GetNodeColumn() to get last column value
  // v1.1.1.0 : New : support for customizing bullets in HTML UL lists
  // v1.2.0.0 : New : Built in support for Office 2010 colors
  // v1.2.1.0 : Improved : vertical centering of HTML text
  // v1.2.2.0 : New : Property WordWrap added
  // v1.2.2.1 : Fixed : Issue with creating component at design time
  // v1.2.2.2 : Fixed : Issue with tree node text longer than 255
  // v1.2.2.3 : Fixed : Issue with inserting items when no columns are defined
  // v1.2.2.4 : Fixed : Issue with customizing header height
  // v1.2.2.5 : Fixed : Issue when using items without columns
  // v1.2.2.6 : Fixed : Issue with painting & scrolling
  // v1.2.2.7 : Fixed : Issue with context menu handling
  // v1.2.2.8 : Fixed : Issue with OnItemClick
  // v1.3.0.0 : New : Support for PNG images via images in associated PictureContainer
  // v1.3.1.0 : New : Added ColumnLineColor property
  // v1.3.1.1 : New : Added support for PictureCache
  // v1.4.0.0 : New : VertAlign property per column added
  // v1.4.0.1 : Fixed : Painting issue in last column on resize
  // v1.4.0.2 : Fixed : Issue with anchor handling & horiz. scroll
  // v1.4.1.0 : New : BeginUpdate/EndUpdate methods added
  // v1.4.1.1 : Fixed : Wordwrapped text in nodes is now always automatically drawn top aligned
  // v1.4.1.2 : Improved : HTML engine drawing in high DPI mode with form.Scaled = false
  // v1.5.0.0 : New : Support for VCL Styles added
  // v1.5.0.1 : Improved : Handling OnCustomDrawItem to change item colors dynamically
  // v1.6.0.0 : New : Support for Office 2019 styles
  // v1.6.0.1 : Fixed : Design-time style painting
  // v1.6.1.0 : New : UIStyle property for Office 2019 look
  //          : Improved : On creation check for enabled AdvFormStyler
  // v1.6.1.1 : Fixed : ParentFont kept true on initialization
  // v1.6.1.2 : Fixed : VCL styles check updated to work in 10.4 Sydney
  // v1.6.1.3 : Fixed : Issue with adapting text color via OnCustomDrawItem
  // v1.7.0.0 : New : Support for rendering Emoji in HTML
  // v1.8.0.0 : New : Support for Bands added
  //          : New : Support for row lines added
  //          : New : Property FullRow added
  // v1.8.0.1 : Fixed : Issue with drawing with only 2 columns
  // v1.8.0.2 : Fixed : Issue with drawing when ShowRoot = false

type
  {$IFDEF DELPHI_UNICODE}
  {$EXTERNALSYM THintInfo}
  THintInfo = Controls.THintInfo;
  {$EXTERNALSYM PHintInfo}
  PHintInfo = Controls.PHintInfo;
  {$ENDIF}

  TAnchorEvent = procedure(Sender: TObject; Node: TTreeNode; anchor: string) of object;

  THTMLTreeList = class;

  {$IFNDEF DELPHI_UNICODE}
  TVerticalAlignment = (taAlignTop, taAlignBottom, taVerticalCenter);
  {$ENDIF}

  TColumnItem = class(TCollectionItem)
  private
    FWidth: integer;
    FWordWrap: boolean;
    FColumnHeader: string;
    FFont: TFont;
    FAlignVert: TVerticalAlignment;
    procedure SetWidth(const value: integer);
    procedure SetColumnHeader(const value: string);
    procedure SetFont(const value: TFont);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(source: TPersistent); override;
  published
    property AlignVert: TVerticalAlignment read FAlignVert write FAlignVert default taVerticalCenter;
    property Header: string read FColumnHeader write SetColumnHeader;
    property Font: TFont read FFont write SetFont;
    property Width: integer read FWidth write SetWidth;
    property WordWrap: boolean read FWordWrap write FWordWrap default false;
  end;

  TColumnCollection = class(TCollection)
  private
    FOwner: THTMLTreeList;
    function GetItem(Index: Integer): TColumnItem;
    procedure SetItem(Index: Integer; const Value: TColumnItem);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(aOwner: THTMLTreeList);
    function GetOwner: tPersistent; override;
    function Add: TColumnItem;
    function Insert(Index: Integer): TColumnItem;
    property Items[Index: Integer]: TColumnItem read GetItem write SetItem; default;
  end;

  TTLHeaderClickEvent = procedure(Sender: TObject; SectionIdx: integer) of object;

  TTLHeader = class(THeader)
  private
    FColor: tColor;
    FOnClick: TTLHeaderClickEvent;
    FOnRightClick: TTLHeaderClickEvent;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMRButtonDown(var Message: TWMLButtonDown); message WM_RBUTTONDOWN;
    procedure WMMouseMove(var Message: TWMMouseMove); message WM_MOUSEMOVE;
    {$IFDEF DELPHIXE2_LVL}
    procedure CMStyleChanged(var Message: TMessage); message CM_STYLECHANGED;
    {$ENDIF}
    procedure SetColor(const Value: TColor);
  protected
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
  public
    constructor Create(aOwner: TComponent); override;
  published
    property Color: TColor read fColor write SetColor;
    property OnClick: TTLHeaderClickEvent read fOnClick write fOnClick;
    property OnRightClick: TTLHeaderClickEvent read fOnRightClick write fOnRightClick;
  end;

  THeaderSettings = class(TPersistent)
  private
    FOwner: THTMLTreeList;
    FHeight: integer;
    FFont : TFont;
    FColor: TColor;
    FColorTo: TColor;
    FOnChange: TNotifyEvent;
    FFlat: Boolean;
    FAllowResize: Boolean;
    procedure SetFont(const Value: TFont);
    procedure SetFlat(const Value: boolean);
    procedure SetAllowResize(const Value: boolean);
    procedure SetColor(const Value: TColor);
    procedure SetHeight(const Value: integer);
    procedure FontChanged(Sender: TObject);
    procedure SetColorTo(const Value: TColor);
  protected
    procedure Changed;
  public
    constructor Create(aOwner: THTMLTreeList);
    destructor Destroy; override;
  published
    property AllowResize: boolean read FAllowResize write SetAllowResize default true;
    property Color: TColor read FColor write SetColor default clBtnFace;
    property ColorTo: TColor read FColorTo write SetColorTo default clNone;
    property Flat: boolean read FFlat write SetFlat default false;
    property Font: TFont read FFont write SetFont;
    property Height: integer read FHeight write SetHeight default 18;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  THTMLTreeListBands = class(TPersistent)
  private
    FSecondaryColor: TColor;
    FPrimaryColor: TColor;
    FActive: boolean;
    FOnChange: TNotifyEvent;
    procedure SetActive(const Value: boolean);
    procedure SetPrimaryColor(const Value: TColor);
    procedure SetSecondaryColor(const Value: TColor);
  protected
    procedure Changed;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Active: boolean read FActive write SetActive default false;
    property PrimaryColor: TColor read FPrimaryColor write SetPrimaryColor default clWindow;
    property SecondaryColor: TColor read FSecondaryColor write SetSecondaryColor default clInfoBk;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  THTMLTreeList = class(TTreeview, ITMSStyle)
  private
  { Private declarations }
    FAnchorHint: boolean;
    FHeader: TTLHeader;
    FHeaderSettings: THeaderSettings;
    FFlatHeader: boolean;
    FColumnCollection: TColumnCollection;
    FColumnLines: boolean;
    FColumnLineColor: TColor;
    FColumnSpace: integer;
    FOldScrollPos: integer;
    FSeparator: string;
    FItemHeight: integer;
    FOldCursor: TCursor;
    FOldAnchor: string;
    FURLColor: TColor;
    FImages: TCustomImageList;
    FOnClick: TTLHeaderClickEvent;
    FOnRightClick: TTLHeaderClickEvent;
    FOnAnchorClick: TAnchorEvent;
    FOnAnchorEnter: TAnchorEvent;
    FOnAnchorExit: TAnchorEvent;
    FSelectionFontColor: TColor;
    FSelectionColors: TGradientStyle;
    FImageCache:THTMLPictureCache;
    FUpdateCount: integer;
    FFormScaled: boolean;
    FUseVCLStyles: boolean;
    FTMSStyle: TTMSStyle;
    FBands: THTMLTreeListBands;
    FRowLines: boolean;
    FRowLineColor: TColor;
    FFullRow: boolean;
    procedure WMContextMenu(var Message: TWMContextMenu); message WM_CONTEXTMENU;
    procedure WMHScroll(var message: TMessage); message WM_HSCROLL;
    procedure WMMouseMove(var message: TWMMouseMove); message WM_MOUSEMOVE;
    procedure WMLButtonDown(var message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMRButtonDown(var message: TWMLButtonDown); message WM_RBUTTONDOWN;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure WMPaint(var message: TWMPaint); message WM_PAINT;
    procedure CNNotify(var message: TWMNotify); message CN_NOTIFY;
    procedure CMHintShow(var Msg: TMessage); message CM_HINTSHOW;
    {$IFDEF DELPHIXE2_LVL}
    procedure CMStyleChanged(var Message: TMessage); message CM_STYLECHANGED;
    {$ENDIF}
    procedure SetColumnCollection(const Value: TColumnCollection);
    procedure SetColumnLines(const Value: boolean);
    procedure UpdateColumns;
    procedure SectionSize(sender: TObject; ASection, AWidth: integer);
    procedure HeaderClick(sender: TObject; ASection: integer); procedure HeaderRightClick(sender: TObject; ASection: integer);
    function GetColWidth(idx: integer): integer;
    function GetColFont(idx: integer): TFont;
    procedure SetSeparator(const Value: string);
    function GetItemHeight: integer;
    procedure SetItemHeight(const Value: integer);
    function GetVisible: boolean;
    procedure SetVisible(const Value: boolean);
    function IsAnchor(node: ttreenode; x, y: integer): string;
    procedure SetURLColor(const value: TColor);
    procedure SetImages(const value: TCustomImageList);
    procedure SetSelectionFontColor(const Value: tcolor);
    function GetColumnText(col: integer; s: string): string;
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    function GetVersionNr: Integer;
    procedure SetSelectionColors(const Value: TGradientStyle);
    procedure HeaderChanged(Sender: TObject);
    procedure SetColumnLineColor(const Value: TColor);
    procedure InitVCLStyle(init: boolean);
    procedure SetBands(const Value: THTMLTreeListBands);
    procedure SetRowLineColor(const Value: TColor);
    procedure SetRowLines(const Value: boolean);
    procedure SetFullRow(const Value: boolean);
  protected
  { Protected declarations }
    procedure Expand(Node: TTreeNode); override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure CreateWnd; override;
    function GetClientRect: TRect; override;
    procedure BandsChanged(Sender: TObject); virtual;
    procedure Loaded; override;
    property ToolTips;
  public
  { Public declarations }
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure SetNodeColumn(tn: TTreeNode; idx: integer; value: string);
    function GetNodeColumn(tn: TTreeNode; idx: integer): string;
    procedure SetComponentStyle(AStyle: TTMSStyle);
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
  published
  { Pubished declarations }
    property AnchorHint: boolean read FAnchorHint write FAnchorHint default False;
    property Bands: THTMLTreeListBands read FBands write SetBands;
    property Columns: TColumnCollection read FColumnCollection write SetColumnCollection;
    property ColumnLines: boolean read FColumnLines write SetColumnLines default true;
    property ColumnLineColor: TColor read FColumnLineColor write SetColumnLineColor default clSilver;
    property FullRow: boolean read FFullRow write SetFullRow default true;
    property Separator: string read FSeparator write SetSeparator;
    property ItemHeight: integer read GetItemHeight write SetItemHeight;
    property RowLines: boolean read FRowLines write SetRowLines default false;
    property RowLineColor: TColor read FRowLineColor write SetRowLineColor default clSilver;
    property Visible: boolean read GetVisible write SetVisible default true;
    property HeaderSettings: THeaderSettings read FHeaderSettings write FHeaderSettings;
    property HTMLImages: TCustomImageList read fImages write SetImages;
    property SelectionColors: TGradientStyle read FSelectionColors write SetSelectionColors;
    property SelectionFontColor: tcolor read fSelectionFontColor write SetSelectionFontColor default clHighlightText;
    property URLColor: TColor read fURLColor write SetURLColor default clBlue;
    property OnHeaderClick: TTLHeaderClickEvent read fOnClick write FOnClick;
    property OnHeaderRightClick: TTLHeaderClickEvent read fOnRightClick write FOnRightClick;
    property OnAnchorClick: TAnchorEvent read FOnAnchorClick write FOnAnchorClick;
    property OnAnchorEnter: TAnchorEvent read FOnAnchorEnter write FOnAnchorEnter;
    property OnAnchorExit: TAnchorEvent read FOnAnchorExit write fOnAnchorExit;
    property Version: string read GetVersion write SetVersion;
    property UIStyle: TTMSStyle read FTMSStyle write SetComponentStyle default tsCustom;
  end;

{$IFDEF VER100}
const
  NM_CUSTOMDRAW = NM_FIRST - 12;

  CDDS_PREPAINT = $00000001;
  CDDS_POSTPAINT = $00000002;
  CDDS_PREERASE = $00000003;
  CDDS_POSTERASE = $00000004;
  CDDS_ITEM = $00010000;
  CDDS_ITEMPREPAINT = CDDS_ITEM or CDDS_PREPAINT;
  CDDS_ITEMPOSTPAINT = CDDS_ITEM or CDDS_POSTPAINT;
  CDDS_ITEMPREERASE = CDDS_ITEM or CDDS_PREERASE;
  CDDS_ITEMPOSTERASE = CDDS_ITEM or CDDS_POSTERASE;
  CDDS_SUBITEM = $00020000;

  // itemState flags
  CDIS_SELECTED = $0001;
  CDIS_GRAYED = $0002;
  CDIS_DISABLED = $0004;
  CDIS_CHECKED = $0008;
  CDIS_FOCUS = $0010;
  CDIS_DEFAULT = $0020;
  CDIS_HOT = $0040;
  CDIS_MARKED = $0080;
  CDIS_INDETERMINATE = $0100;

  CDRF_DODEFAULT = $00000000;
  CDRF_NEWFONT = $00000002;
  CDRF_SKIPDEFAULT = $00000004;
  CDRF_NOTIFYPOSTPAINT = $00000010;
  CDRF_NOTIFYITEMDRAW = $00000020;
  CDRF_NOTIFYSUBITEMDRAW = $00000020; // flags are the same, we can distinguish by context
  CDRF_NOTIFYPOSTERASE = $00000040;

  TVM_GETITEMHEIGHT = TV_FIRST + 28;
  TVM_SETITEMHEIGHT = TV_FIRST + 27;

type
  tagNMCUSTOMDRAWINFO = packed record
    hdr: TNMHdr;
    dwDrawStage: DWORD;
    hdc: HDC;
    rc: TRect;
    dwItemSpec: DWORD; // this is control specific, but it's how to specify an item.  valid only with CDDS_ITEM bit set
    uItemState: UINT;
    lItemlParam: LPARAM;
  end;
  PNMCustomDraw = ^TNMCustomDraw;
  TNMCustomDraw = tagNMCUSTOMDRAWINFO;


  tagNMTVCUSTOMDRAW = packed record
    nmcd: TNMCustomDraw;
    clrText: COLORREF;
    clrTextBk: COLORREF;
    iLevel: Integer;
  end;
  PNMTVCustomDraw = ^TNMTVCustomDraw;
  TNMTVCustomDraw = tagNMTVCUSTOMDRAW;

function TreeView_SetItemHeight(hwnd: HWND; iHeight: Integer): Integer;

function TreeView_GetItemHeight(hwnd: HWND): Integer;

{$ENDIF}

implementation

uses
{$IFDEF DELPHIXE2_LVL}
  VCL.Themes,
{$ENDIF}
  ShellApi, AdvHTML;

type
  TTreeNodesEx = class(TTreeNodes);

{ TColumnItem }

constructor TColumnItem.Create(Collection: TCollection);
begin
  inherited;
  FWidth := 50;
  FWordWrap := false;
  FFont := TFont.Create;
  FFont.Assign((TColumnCollection(Collection).FOwner).Font);
  FAlignVert := taVerticalCenter;
end;

destructor TColumnItem.Destroy;
begin
  FFont.Free;
  inherited Destroy;
end;

procedure TColumnItem.SetWidth(const value: integer);
begin
  FWidth := value;
  TColumnCollection(Collection).Update(self);
end;

procedure TColumnItem.SetColumnHeader(const value: string);
begin
  FColumnHeader := value;
  TColumnCollection(Collection).Update(self);
end;

procedure TColumnItem.SetFont(const value: TFont);
begin
  FFont.Assign(value);
  TColumnCollection(Collection).Update(self);
end;

procedure TColumnItem.Assign(source: TPersistent);
begin
  if (Source is TColumnItem) then
  begin
    FWidth := TColumnItem(Source).Width;
    Header := TColumnItem(Source).Header;
    FFont.Assign(TColumnItem(Source).Font);
    FWordWrap := TColumnItem(Source).WordWrap;
    FAlignVert := TColumnItem(Source).AlignVert;
  end;
end;

{ TColumnCollection }

function TColumnCollection.Add: TColumnItem;
begin
  Result := TColumnItem(inherited Add);
end;

constructor TColumnCollection.Create(aOwner: THTMLTreeList);
begin
  inherited Create(TColumnItem);
  FOwner := AOwner;
end;

function TColumnCollection.GetItem(Index: Integer): TColumnItem;
begin
  Result := TColumnItem(inherited Items[Index]);
end;

function TColumnCollection.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TColumnCollection.Insert(Index: Integer): TColumnItem;
begin
  Result := TColumnItem(inherited Insert(Index));
end;

procedure TColumnCollection.SetItem(Index: Integer;
  const Value: TColumnItem);
begin
  Items[Index] := Value;
end;

procedure TColumnCollection.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  {reflect changes}
  FOwner.UpdateColumns;
end;

{ TTLHeader }

procedure TTLHeader.WMLButtonDown(var Message: TWMLButtonDown);
var
  x, i: integer;
begin
  inherited;
  x := 0;
  i := 0;

  while (x < message.xpos) and (i < sections.Count) do
  begin
    x := x + sectionwidth[i];
    inc(i);
  end;
  dec(i);

  if Assigned(FOnClick) then
    FOnClick(self, i);
end;

procedure TTLHeader.WMMouseMove(var Message: TWMMouseMove);
begin
  inherited;
end;

{$IFDEF DELPHIXE2_LVL}
procedure TTLHeader.CMStyleChanged(var Message: TMessage);
begin
  Invalidate;
end;
{$ENDIF}

procedure TTLHeader.WMRButtonDown(var Message: TWMLButtonDown);
var
  x, i: integer;
begin
  inherited;

  x := 0;
  i := 0;

  while (x < message.xpos) and (i < sections.Count) do
  begin
    x := x + sectionwidth[i];
    inc(i);
  end;

  dec(i);

  if Assigned(FOnRightClick) then
    FOnRightClick(self, i);
end;

procedure TTLHeader.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X,Y);
end;

procedure TTLHeader.Paint;
var
  I, W: Integer;
  S: string;
  R: TRect;
  PR: TRect;
  anchor, stripped: string;
  xsize, ysize: integer;
  urlcol: TColor;
  imagelist: TCustomImageList;

  hyperlinks,mouselink: integer;
  focusanchor: string;
  re: TRect;
  clr,clrto: TColor;
  ww: Boolean;
  {$IFDEF DELPHIXE2_LVL}
  lDetails: TThemedElementDetails;
  {$ENDIF}
begin
  with Canvas do
  begin
    Font := Self.Font;
    Brush.Color := fColor;
    I := 0;
    R := Rect(0, 0, 0, ClientHeight);
    W := 0;
    S := '';

    R.Right := R.Right - GetScrollPos((Owner as THTMLTreeList).Handle, SB_HORZ);

    with Owner as THTMLTreeList do
    begin
      urlcol := URLColor;
      imagelist := HTMLImages;
      clr := HeaderSettings.Color;
      clrto := HeaderSettings.ColorTo;
    end;

    repeat
      if (i < (Owner as THTMLTreeList).Columns.Count) then
        ww := (Owner as THTMLTreeList).Columns[i].WordWrap
      else
        ww := false;

      if I < Sections.Count then
      begin
        W := SectionWidth[i];

        if (i < Sections.Count) then
          S := Sections[i]
        else
          s := Sections[0];

        if (i = 0) and (BorderStyle = bsNone) then
          inc(w);

        Inc(I);
      end;

      R.Left := R.Right;

      Inc(R.Right, W);

      if (ClientWidth - R.Right < 2) or (I = Sections.Count) then
        R.Right := ClientWidth;

      pr := r;

      if (Owner as THTMLTreeList).FUseVCLStyles then
      begin
        {$IFDEF DELPHIXE2_LVL}
        lDetails := StyleServices.GetElementDetails(thHeaderItemNormal);
        StyleServices.DrawElement(Canvas.Handle, lDetails, r);
        {$ENDIF}
      end
      else
      begin
        if clrto <> clNone then
        begin
          DrawGradient(Canvas,clr,clrto,16,r,false);
        end
        else
          FillRect(r);
      end;

      InflateRect(pr, -2, -2);

      if (Owner as THTMLTreeList).FUseVCLStyles {$IFDEF DELPHIXE6_LVL}and (seFont in StyleElements){$ENDIF} then
      begin
        {$IFDEF DELPHIXE2_LVL}
        lDetails := StyleServices.GetElementDetails(tgFixedCellNormal);
        StyleServices.DrawText(Canvas.Handle, lDetails, S, pr, TTextFormatFlags(DT_SINGLELINE or DT_NOPREFIX or DT_VCENTER or DT_LEFT));
        {$ENDIF}
      end
      else
        HTMLDrawEx(Canvas, s, pr, imagelist, 0, 0, -1,-1,1,False, False, False, False, False, False, ww, (Owner as THTMLTreeList).FFormScaled, 1.0, URLCol,
          clNone, clNone, clGray, anchor, stripped, focusanchor, xsize, ysize, hyperlinks, mouselink, re,
          (Owner as THTMLTreeList).FImageCache, nil,0);

      if not (Owner as THTMLTreeList).FUseVCLStyles then
      begin
        DrawEdge(Canvas.Handle, R, BDR_RAISEDINNER, BF_TOPLEFT);
        DrawEdge(Canvas.Handle, R, BDR_RAISEDINNER, BF_BOTTOMRight);
      end;

    until R.Right = ClientWidth;

  end;
  
end;

constructor TTLHeader.Create(aOwner: TComponent);
begin
  inherited;
  fColor := clBtnFace;
end;

procedure TTLHeader.SetColor(const Value: TColor);
begin
  fColor := Value;
  invalidate;
end;

{ THTMLTreeList }

constructor THTMLTreeList.Create(AOwner: TComponent);
begin
  inherited Create(aOwner);
  FBands := THTMLTreeListBands.Create;
  FBands.OnChange := BandsChanged;
  FHeader := nil;
  Tooltips := false;
  FColumnCollection := TColumnCollection.Create(self);
  FHeaderSettings := THeaderSettings.Create(self);
  FHeaderSettings.OnChange := HeaderChanged;
  FSeparator := ';';
  FItemHeight := 16;
  FColumnLines := true;
  FColumnLineColor := clSilver;
  FRowLines := false;
  FRowLineColor := clSilver;
  FColumnSpace := 2;
  FOldScrollPos := -1;
  FHeader := nil;
  FURLColor := clBlue;
  FSelectionColors := TGradientStyle.Create;
  FSelectionFontColor := clHighLightText;
  DoubleBuffered := true;
  FImageCache := THTMLPictureCache.Create;
  FUpdateCount := 0;
  FUseVCLStyles := false;

  if (csDesigning in ComponentState) and not
    ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState)) then
  begin
    SetComponentStyle(GetDefaultStyle(AOwner));
    if SetParentFontForStyle(FTMSStyle) then
      ParentFont := true;
  end;
  FTMSStyle := tsCustom;
end;

procedure THTMLTreeList.CreateWnd;
const
  hdr: array[boolean] of TBorderStyle = (bsSingle, bsNone);
var
  frm: TCustomForm;
begin
  inherited CreateWnd;

  FFormScaled := true;
  frm := GetParentForm(Self);
  if Assigned(frm) and (frm is TForm) then
    FFormScaled := (frm as TForm).Scaled;

  if not Assigned(FHeader) then
  begin
    FHeader := TTLHeader.Create(self);
    FHeader.Top := Top - 16;
    FHeader.Left := Left;
    FHeader.Width := Width - 1;
    FHeader.Height := 18;
    Fheader.Borderstyle := hdr[fFlatHeader];
    FHeader.OnSized := SectionSize;
    FHeader.OnClick := HeaderClick;
    FHeader.OnRightClick := HeaderRightClick;
  end;

  FHeader.Parent := Parent;
  ItemHeight := FItemHeight;

  if not (csDesigning in ComponentState) then
  begin
    if not (csLoading in ComponentState) then
      InitVCLStyle(false);
  end;
end;

procedure THTMLTreeList.SectionSize(sender: TObject; ASection, AWidth: integer);
var
  fIndent: integer;
begin
  FIndent := TreeView_GetIndent(self.handle);
  if Assigned(Images) then
    FIndent := FIndent + Images.Width;

  if (ASection = 0) and (AWidth < fIndent) then
  begin
    AWidth := FIndent;
    if Assigned(FHeader) then
      FHeader.SectionWidth[ASection] := FIndent;
  end;

  TColumnItem(FColumnCollection.Items[ASection]).FWidth := AWidth;
  Invalidate;
end;

procedure THTMLTreeList.HeaderChanged(Sender: TObject);
begin
  if Assigned(FHeader) then
  begin
    if HeaderSettings.Flat then
      FHeader.BorderStyle := bsNone
    else
      FHeader.BorderStyle := bsSingle;

    FHeader.AllowResize := HeaderSettings.AllowResize;
    FHeader.Font.Assign(HeaderSettings.Font);
    FHeader.Color := HeaderSettings.Color;
    FHeader.Height := HeaderSettings.Height;
    FHeader.Top := Top  - (FHeaderSettings.Height - 2);
    FHeader.Width := Width;
  end;
end;

procedure THTMLTreeList.HeaderClick(sender: TObject; ASection: integer);
begin
  if Assigned(OnHeaderClick) then
    OnHeaderClick(self, ASection);
end;

procedure THTMLTreeList.HeaderRightClick(sender: TObject; ASection: integer);
begin
  if Assigned(OnHeaderRightClick) then
    OnHeaderRightClick(self, ASection);
end;

procedure THTMLTreeList.EndUpdate;
begin
  Items.EndUpdate;
  if FUpdateCount > 0 then
  begin
    dec(FUpdateCount);
    Refresh;
  end;
end;

procedure THTMLTreeList.Expand(Node: TTreeNode);
begin
  inherited;
  Invalidate;
end;

destructor THTMLTreeList.Destroy;
begin
  FBands.Free;
  FColumnCollection.Free;
  FHeaderSettings.Free;
  FSelectionColors.Free;
  FImageCache.Free;
  inherited;
end;

procedure THTMLTreeList.SetColumnLineColor(const Value: TColor);
begin
  if (FColumnLineColor <> Value) then
  begin
    FColumnLineColor := Value;
    Invalidate;
  end;
end;

procedure THTMLTreeList.SetColumnLines(const value: boolean);
begin
  if (FColumnLines <> value) then
  begin
    FColumnLines := value;
    if FColumnLines then
      FColumnSpace := 4
    else
      FColumnSpace := 2;
    Invalidate;
  end;
end;


procedure THTMLTreeList.SetComponentStyle(AStyle: TTMSStyle);

  procedure SetHeaderColors(clr1,clr2: TColor);
  begin
    HeaderSettings.Color := clr1;
    HeaderSettings.ColorTo := clr2;
  end;

begin
  FTMSStyle := AStyle;

  if AStyle = tsCustom then
    Exit;

  SelectionColors.SetStyle(AStyle);
  SelectionFontColor := clHighlightText;
  if not (AStyle in [tsCustom, tsWindowsXP]) then
    SelectionFontColor := clBlack;

  HeaderSettings.Flat := not (AStyle = tsWindowsXP);

  case AStyle of
    tsOffice2019White:
      begin
        Color := clWhite;
        HeaderSettings.Flat := True;
        SetHeaderColors(clWhite, clWhite);
        SelectionFontColor := $003B3B3B;
        Font.Color := $00444648;
        ColumnLineColor := $00ABABAB;
      end;
      tsOffice2019Gray:
      begin
        Color := $00B8BBBE;
        HeaderSettings.Flat := True;
        SetHeaderColors($00B8BBBE, $00B8BBBE);
        SelectionFontColor := $003B3B3B;
        Font.Color := $00232425;
        ColumnLineColor := $00808080;
      end;
      tsOffice2019Black:
      begin
        Color := $00444444;
        HeaderSettings.Flat := True;
        SetHeaderColors($00303132, $00303132);
        SelectionFontColor := clWhite;
        Font.Color := clWhite;
        ColumnLineColor := $00686868;
      end
      else
      begin
        case Astyle of
          tsOffice2003Blue: SetHeaderColors($FDEADA, $E4AE88);
          tsOffice2003Silver: SetHeaderColors($ECE2E1, $B39698);
          tsOffice2003Olive: SetHeaderColors($CFF0EA, $8CC0B1);
          tsOffice2003Classic: SetHeaderColors(clBtnFace, clNone);
          tsOffice2007Luna: SetHeaderColors($00FFEFE3, $00FFD2AF);
          tsOffice2007Silver: SetHeaderColors($00F8F7F6, $00E8E0DB);
          tsOffice2007Obsidian: SetHeaderColors($00F2F1F0, $00C9C2BD);
          tsWindowsXP: SetHeaderColors(clBtnFace, clNone);
          tsWhidbey: SetHeaderColors($00EBEEEF, $007E9898);
          tsWindowsVista: SetHeaderColors($00FAF8F7, $00F4F2F1);
          tsWindows7: SetHeaderColors($00F4E6DC, $00F7E9DD);
          tsTerminal: SetHeaderColors(clBtnFace, clNone);
          tsOffice2010Blue: SetHeaderColors($FDF6EF, $F0DAC7);
          tsOffice2010Silver: SetHeaderColors($FFFFFF, $EDE5E0);
          tsOffice2010Black: SetHeaderColors($BFBFBF, $919191);
        end;

        Color := clWindow;
        ColumnLineColor := clSilver;
        if not (AStyle in [tsWindowsXP]) then
          SelectionFontColor := clBlack
        else
          SelectionFontColor := clHighlightText;
        Font.Color := clWindowText;
      end;
  end;
end;

procedure THTMLTreeList.SetFullRow(const Value: boolean);
begin
  if (FFullRow <> Value) then
  begin
    FFullRow := Value;
    Changed;
  end;
end;

function THTMLTreeList.GetNodeColumn(tn: TTreeNode; idx: integer): string;
var
  s: string;
  i: integer;
begin
  Result := '';
  if Assigned(tn) then
    s := tn.Text
  else
    Exit;

  i := 0;
  while (i <= idx) and (s <> '') do
  begin
    if (pos(Separator, s) > 0) then
    begin
      if (idx = i) then
         Result := copy(s, 1, pos(Separator, s) - 1);
      system.delete(s, 1, pos(Separator, s));
      inc(i);
    end
    else
    begin
      Result := s;
      s := '';
    end;
  end;
end;


procedure THTMLTreeList.SetNodeColumn(tn: TTreeNode; idx: integer;
  value: string);
var
  s,su: string;
  i,vp: Integer;
  sel: boolean;
begin
  if Assigned(tn) then
    s := tn.Text
  else
    Exit;

  sel := tn.Selected;

  su := s;
  for i := 1 to Columns.Count do
  begin
    if VarPos(Separator,su,vp) > 0 then
      system.Delete(su,1,vp)
    else
      s := s + Separator;
  end;

  i := 0;
  su := '';
  while (i <= idx) and (s <> '') do
  begin
    if VarPos(Separator,s,vp) > 0 then
    begin
      if i < idx then
        su := su + copy(s,1,vp);
      if i = idx then
        su := su + Value + Separator;
      System.Delete(s,1,vp);
      Inc(i);
    end
    else
    begin
      s := '';
      if i = idx then
        su := su + Value;
      Inc(i);
    end;
  end;

  su := su + s;
  tn.Text := su;
  tn.Selected := sel;
  Invalidate;
end;


procedure THTMLTreeList.SetRowLineColor(const Value: TColor);
begin
  if FRowLineColor <> Value then
  begin
    FRowLineColor := Value;
    Invalidate;
  end;
end;

procedure THTMLTreeList.SetRowLines(const Value: boolean);
begin
  if FRowLines <> Value then
  begin
    FRowLines := Value;
    Invalidate;
  end;
end;

procedure THTMLTreeList.SetColumnCollection(const Value: TColumnCollection);
begin
  FColumnCollection.Assign(Value);
end;

procedure THTMLTreeList.SetBands(const Value: THTMLTreeListBands);
begin
  FBands.Assign(Value);
end;

procedure THTMLTreeList.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  HeaderHeight: Integer;
  ws: boolean;
begin
  ws := AWidth <> Width;
  if not Assigned(FHeaderSettings) then
  begin
    inherited;
    Exit;
  end;

  HeaderHeight := FHeaderSettings.FHeight;

  if Align in [alClient, alTop, alRight, alLeft] then
  begin
    inherited SetBounds(ALeft,ATop + HeaderHeight - 2,AWidth,AHeight - HeaderHeight + 2);

    if Assigned(FHeader) then
    begin
      FHeader.Top := ATop;
      FHeader.Left := ALeft;
      FHeader.Width := AWidth {- 1};
      FHeader.Height := FHeaderSettings.Height;
    end;
  end
  else
  begin
    inherited SetBounds(ALeft,ATop,AWidth,AHeight);

    if Assigned(FHeader) then
    begin
      FHeader.Top := ATop - (FHeaderSettings.Height - 2);
      FHeader.Left := ALeft;
      FHeader.Width := AWidth {- 1};
      FHeader.Height := FHeaderSettings.Height;
    end;
  end;
  if ws then
    Invalidate;
end;

procedure THTMLTreeList.UpdateColumns;
var
  i: integer;
begin
  if Assigned(FHeader) then
  begin
    FHeader.Sections.Clear;
    for i := 1 to FColumnCollection.Count do
    begin
      FHeader.Sections.Add(TColumnItem(FColumnCollection.Items[i - 1]).Header);
      FHeader.SectionWidth[i - 1] := TColumnItem(fColumnCollection.Items[i - 1]).Width;
    end;
    Invalidate;
  end;
end;

function THTMLTreeList.GetColWidth(idx: integer): integer;
begin
  if idx >= fColumnCollection.Count - 1 then result := self.width
  else result := TColumnItem(fColumnCollection.Items[idx]).fWidth;
end;

function THTMLTreeList.GetColFont(idx: integer): TFont;
begin
  if idx >= fColumnCollection.Count then result := self.Font
  else result := TColumnItem(fColumnCollection.Items[idx]).fFont;
end;

procedure THTMLTreeList.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  if (aOperation = opRemove) and (aComponent = fImages) then fImages := nil;
  inherited;
end;

function WindowScrollbars(wnd: HWND): TScrollStyle;
var
  styleflags: DWORD;
begin
  styleflags := GetWindowLong( wnd, GWL_STYLE ) and (WS_VSCROLL or WS_HSCROLL);
  case styleflags Of
  0        : Result := ssNone;
  WS_VSCROLL: Result := ssVertical;
  WS_HSCROLL: Result := ssHorizontal;
  else
    Result := ssBoth;
  end;
end; { WindowScrollbars }

procedure THTMLTreeList.CNNotify(var message: TWMNotify);
var
  TVcd: TNMTVCustomDraw;
  TVdi: TTVDISPINFO;
  ACanvas: TCanvas;
  s, su, anchor, stripped: string;
  tn: ttreenode;
  r,hr: trect;
  fIndent, fIdx, fImgWidth: integer;
  xsize, ysize, th: integer;
  av: TVerticalAlignment;
  hyperlinks,mouselink: Integer;
  focusanchor: string;
  re: TRect;
  ww,isSel: boolean;
  i, scpos: integer;
  ItemsRight: integer;
  defdraw: boolean;
  FSelTxtColor: TColor;
  {$IFDEF DELPHIXE2_LVL}
  clr: TColor;
  usestyle: boolean;
  LStyle: TCustomStyleServices;
  {$ENDIF}

begin
  if FUpdateCount > 0 then
    Exit;

  {$IFDEF DELPHIXE2_LVL}
  usestyle := true;
  {$IFDEF DELPHIXE6_LVL}
  if not (seClient in StyleElements) then
    usestyle := false;
  {$ENDIF}
  {$ENDIF}


  if message.NMHdr^.Code = TVN_GETDISPINFO then
  begin
    TVDi := PTVDispInfo(pointer(message.nmhdr))^;

    if (tvdi.item.mask and TVIF_TEXT = TVIF_TEXT) then
    begin
      inherited;

      tn := items.GetNode(tvdi.item.hitem);

      if Assigned(tn) then
      begin
        s := HTMLStrip(tn.text);
        StrpLCopy(tvdi.item.pszText, s, 255);
        tvdi.item.mask := tvdi.item.mask or TVIF_DI_SETITEM;
        message.Result := 0;
      end;

      Exit;
    end;
  end;

  if message.NMHdr^.code = NM_CUSTOMDRAW then
  begin
    fIndent := TreeView_GetIndent(self.handle);
    TVcd := PNMTVCustomDraw(Pointer(message.NMHdr))^;
    {$IFNDEF DELPHIXE11_LVL}
    FSelTxtColor := clWindowText;
    {$ENDIF}

    case TVcd.nmcd.dwDrawStage of
      CDDS_PREPAINT: message.Result := CDRF_NOTIFYITEMDRAW or CDRF_NOTIFYPOSTPAINT;
      CDDS_ITEMPREPAINT: begin
          if (TVcd.nmcd.uItemState and CDIS_SELECTED = CDIS_SELECTED) then
          begin
            TVcd.nmcd.uItemState := TVcd.nmcd.uItemState and (not CDIS_SELECTED);
            SetTextColor(TVcd.nmcd.hdc, ColorToRGB(self.Color));
            SetBkColor(TVcd.nmcd.hdc, ColorToRGB(self.Color));
            TVcd.clrTextBk := colortorgb(self.Color);
            TVcd.clrText := colortorgb(self.Color);
          end
          else
          begin
            SetTextColor(TVcd.nmcd.hdc, ColorToRGB(self.Color));
            SetBkColor(TVcd.nmcd.hdc, ColorToRGB(self.Color));
          end;
          message.Result := CDRF_NOTIFYPOSTPAINT;
        end;

      CDDS_ITEMPOSTPAINT:
        begin
          ItemsRight := Width;
          if not FFullRow then
          begin
            ItemsRight := 0;
            for i := 1 to FColumnCollection.Count  do
            begin
              ItemsRight := ItemsRight + TColumnItem(FColumnCollection.Items[i - 1]).Width;
            end;
          end;

          ACanvas := TCanvas.Create;
          ACanvas.Handle := TVcd.nmcd.hdc;
          ACanvas.Font.Assign(Font);
          FSelTxtColor := Font.Color;

          tn := Items.GetNode(HTreeItem(TVcd.nmcd.dwitemSpec));
          if not ShowRoot then
            TVcd.nmcd.rc.left := TVcd.nmcd.rc.left - GetScrollPos(handle, SB_HORZ)
          else
            TVcd.nmcd.rc.left := TVcd.nmcd.rc.left + fIndent * (tn.level + 1) - GetScrollPos(handle, SB_HORZ);

          ACanvas.Brush.color := Color;
          ACanvas.Pen.color := Color;

          if (TVcd.nmcd.uItemState and CDIS_SELECTED = CDIS_SELECTED) then
          begin
            ACanvas.Font.Color := FSelectionFontColor;
            FSelTxtColor := FSelectionFontColor;
          end;

          if Assigned(OnCustomDrawItem) then
          begin
            Canvas.Font.Assign(ACanvas.Font);
            Canvas.Pen.Assign(ACanvas.Pen);
            Canvas.Brush.Assign(ACanvas.Brush);
            OnCustomDrawItem(Self, tn, TCustomDrawState(word(TVcd.nmcd.uItemState)), defdraw);
            ACanvas.Font.Assign(Canvas.Font);
            ACanvas.Pen.Assign(Canvas.Pen);
            ACanvas.Brush.Assign(Canvas.Brush);

            if (TVcd.nmcd.uItemState and CDIS_SELECTED = CDIS_SELECTED) then
            begin
              FSelTxtColor := Canvas.Font.Color;
            end;
          end;

          FImgWidth := 0;
          if Assigned(Images) then
          begin
            FImgWidth := Images.Width;

            ACanvas.Rectangle(TVcd.nmcd.rc.left, TVcd.nmcd.rc.top, TVcd.nmcd.rc.left + fImgWidth, TVcd.nmcd.rc.bottom);
            if (TVcd.nmcd.rc.left + fImgWidth < GetColWidth(0)) and (tn.ImageIndex >= 0) then
            begin
              images.Draw(ACanvas, TVcd.nmcd.rc.left, TVcd.nmcd.rc.top, tn.ImageIndex);
            end;
          end;

          TVcd.nmcd.rc.left := TVcd.nmcd.rc.left + fImgWidth;

          r := TVcd.nmcd.rc;

          if (TVcd.nmcd.uItemState and CDIS_SELECTED = CDIS_SELECTED) then
          begin
            with TVcd.nmcd.rc do
              SelectionColors.Draw(ACanvas, Rect(Left, Top, ItemsRight, Bottom));

            ACanvas.Font.Color := FSelTxtColor;

            if (TVcd.nmcd.uItemState and CDIS_FOCUS = CDIS_FOCUS) then
            begin
              ACanvas.Pen.Color := Color;
              ACanvas.Brush.Color := Color;
              if FFullRow then
              begin
                TVcd.nmcd.rc.Right := TVcd.nmcd.rc.Right + 1;
                ACanvas.DrawFocusRect(TVcd.nmcd.rc);
                TVcd.nmcd.rc.Right := TVcd.nmcd.rc.Right - 1;
              end
              else
              begin
                TVcd.nmcd.rc.Right := ItemsRight;
                ACanvas.DrawFocusRect(TVcd.nmcd.rc);
                TVcd.nmcd.rc.Right := TVcd.nmcd.rc.Right - 1;
              end;
            end;
            TVcd.nmcd.rc := r;
            TVcd.nmcd.rc.left := TVcd.nmcd.rc.left + 4;
          end
          else
          begin
            if Bands.Active then
            begin
              if odd(tn.AbsoluteIndex) then
                ACanvas.Brush.Color := Bands.Secondarycolor
              else
                ACanvas.Brush.Color := Bands.PrimaryColor;
//             if tn.ImageIndex = 2 then
               //ACanvas.Brush.Color := self.SelectColor;
            end;

            {$IFDEF DELPHIXE2_LVL}
            LStyle := StyleServices;
            if CheckVCLStylesEnabled(LStyle, (csDesigning in ComponentState)) and usestyle then
            begin
               LStyle.GetElementColor(LStyle.GetElementDetails(tgCellNormal), ecFillColor, clr);

               ACanvas.Brush.Color := clr;
               ACanvas.Pen.Color := ACanvas.Brush.Color;

               LStyle.GetElementColor(LStyle.GetElementDetails(tgCellNormal), ecTextColor, clr);
               ACanvas.Font.Color := clr;
            end;
            {$ENDIF}

            with TVcd.nmcd.rc do
            begin
              ACanvas.Rectangle(Left, Top, ItemsRight, Bottom);

              if RowLines then
              begin
                ACanvas.Pen.Style := psSolid;
                ACanvas.Pen.Color := RowLineColor;
                ACanvas.Pen.Width := 1;
                ACanvas.MoveTo(0, Bottom-1);
                ACanvas.LineTo(ItemsRight, Bottom-1);
              end;
            end;
          end;

          TVcd.nmcd.rc := r;
          TVcd.nmcd.rc.left := TVcd.nmcd.rc.left + 2;

          if (TVcd.nmcd.uItemState and CDIS_SELECTED = CDIS_SELECTED) then
          begin
            ACanvas.Brush.Color := clHighLight;
            ACanvas.Pen.Color := clHighLight;
          end;

          s := tn.Text;
          fIdx := 0;

          Setbkmode(TVcd.nmcd.hdc, TRANSPARENT);

          scpos := GetScrollPos(handle,SB_HORZ);

          repeat
            ACanvas.Font.Assign(GetColFont(fIdx));
            {$IFDEF DELPHIXE2_LVL}
            LStyle := StyleServices;
            if CheckVCLStylesEnabled(LStyle, (csDesigning in ComponentState)) then
              ACanvas.Font.Color := clr;
            {$ENDIF}

            isSel := (TVcd.nmcd.uItemState and CDIS_SELECTED = CDIS_SELECTED);
            if isSel then
              ACanvas.Font.Color := FSelTxtColor;

            ww := false;
            av := taVerticalCenter;
            if FIdx < Columns.Count then
            begin
              ww := Columns[FIdx].WordWrap;
              av := Columns[FIdx].AlignVert;
            end;

            if (FIdx = 0) then
              r.right := GetColWidth(0) - scpos
            else
              r.right := r.left + GetColWidth(fIdx); //+getScrollPos(self.handle,SB_HORZ);

            if fIdx = Columns.Count - 1 then
            begin
              r.Right := Width - 2 * GetSystemMetrics(SM_CXBORDER);

              if (WindowScrollbars(Handle) in [ssVertical, ssBoth]) then
                r.Right := r.Right - GetSystemMetrics(SM_CXVSCROLL);
            end;

            if (pos(FSeparator, s) > 0) then
            begin
              su := copy(s, 1, pos(fSeparator, s) - 1);
              system.Delete(s, 1, pos(fSeparator, s) + length(fSeparator) - 1);
            end
            else
            begin
              su := s;
              s := '';
            end;

            r.right := r.right - FColumnSpace;
            r.Left := r.Left + 2;

            hr := r;
            th := Canvas.TextHeight('gh');

            case av of
            taAlignBottom:
              begin
                if (th < hr.Bottom - hr.Top) and not ww then
                  hr.Top := hr.Bottom - th;
              end;
            taVerticalCenter:
              begin
                if (th < hr.Bottom - hr.Top) and not ww then
                  hr.Top := hr.Top + (hr.Bottom - hr.Top - th) div 2;
              end;
            end;

            if (hr.left < hr.right) then
              HTMLDrawEx(ACanvas, su, hr, fImages, 0, 0, -1, -1, 1, false, false, false, isSel, false, false, ww, FFormScaled,
                1.0, fURLColor, clNone, clNone, clGray, anchor, stripped, focusanchor, xsize, ysize,
                hyperlinks, mouselink, re, FImageCache , nil, 0);

            r.right := r.right + FColumnSpace;

            r.left := r.right;
            inc(fIdx);

          until (length(s) = 0);

          ACanvas.Free;

        end;
    else
      message.Result := 0;
    end;
  end
  else
    inherited;
end;

procedure THTMLTreeList.SetSeparator(const Value: string);
begin
  FSeparator := Value;
  Invalidate;
end;

procedure THTMLTreeList.WMRButtonDown(var message: TWMRButtonDown);
var
  Node: TTreeNode;
begin
  if RightClickSelect then
  begin
    Node := GetNodeAt(message.XPos, message.YPos);
    if Assigned(Node) then
      Node.Selected := True;
  end;
  
  inherited;
end;

procedure THTMLTreeList.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  inherited;
  Invalidate;
end;

procedure THTMLTreeList.WMLButtonDown(var message: TWMLButtonDown);
var
  Node: TTreeNode;
  a: string;
begin
  Node := GetNodeAt(message.XPos, message.YPos);

  if not (csDesigning in ComponentState) then
    inherited
  else
    Exit;

  if Assigned(Node) then
  begin
    Node.Selected := true;
    a := IsAnchor(node, message.XPos, message.YPos);
    if (a <> '') then
      if Assigned(FOnAnchorClick) then
        FOnAnchorClick(self, Node, a);
  end;
end;

procedure THTMLTreeList.WMContextMenu(var Message: TWMContextMenu);
var
  pt: TPoint;
begin
  inherited;

  pt := Point(Message.XPos, Message.YPos);
  pt := ScreenToClient(pt);

  if (pt.y < FHeader.Height) and Assigned(PopupMenu) then
  begin
    Message.Result := 1;
    PopupMenu.Popup(Message.XPos, Message.YPos);
  end;
end;

procedure THTMLTreeList.WMHScroll(var message: TMessage);
begin
  inherited;
  if (FOldScrollPos <> GetScrollPos(Handle, SB_HORZ)) then
  begin
    Invalidate;
    FHeader.Invalidate;
    FOldScrollPos := GetScrollPos(Handle, SB_HORZ);
  end;
end;

procedure THTMLTreeList.WMPaint(var message: TWMPaint);
var
  canvas: tcanvas;
  i: integer;
  xp: integer;
begin
  inherited;

  if (csDestroying in ComponentState) then
    Exit;

  if FColumnLines then
  begin
    Canvas := TCanvas.Create;
    Canvas.Handle := GetDC(Handle);
    try
      xp := 0;
      Canvas.Pen.Color := FColumnLineColor;
      for i := 1 to FColumnCollection.Count - 1 do
      begin
        xp := xp + TColumnItem(FColumnCollection.Items[i - 1]).Width;
        Canvas.MoveTo(xp - 2 - GetScrollPos(self.handle, SB_HORZ), 0);
        Canvas.Lineto(xp - 2 - GetScrollPos(self.handle, SB_HORZ), Height);
      end;
    finally
      ReleaseDC(Handle, Canvas.Handle);
      Canvas.Free;
    end;
  end;
end;

function THTMLTreeList.GetItemHeight: integer;
begin
  Result := TreeView_GetItemHeight(self.Handle);
end;

procedure THTMLTreeList.SetItemHeight(const Value: integer);
begin
  if (Value <> FItemHeight) then
  begin
    FItemHeight := Value;
    TreeView_SetItemHeight(Handle, FItemHeight);
  end;
end;


{$IFDEF VER100}

function TreeView_SetItemHeight(hwnd: HWND; iHeight: Integer): Integer;
begin
  Result := SendMessage(hwnd, TVM_SETITEMHEIGHT, iHeight, 0);
end;

function TreeView_GetItemHeight(hwnd: HWND): Integer;
begin
  Result := SendMessage(hwnd, TVM_GETITEMHEIGHT, 0, 0);
end;
{$ENDIF}



function THTMLTreeList.GetVisible: boolean;
begin
  Result := inherited Visible;
end;

procedure THTMLTreeList.SetVisible(const Value: boolean);
begin
  inherited Visible := value;
  if Assigned(FHeader) then
  begin
    if Value then FHeader.Show else FHeader.Hide;
  end;
end;

function THTMLTreeList.GetClientRect: TRect;
var
  r: TRect;
begin
  r := inherited GetClientRect;
  r.bottom := r.bottom + FHeaderSettings.Height;
  Result := r;
end;

procedure THTMLTreeList.InitVCLStyle(init: boolean);
{$IFDEF DELPHIXE2_LVL}
var
  clr: TColor;
  i: Integer;
{$ENDIF}
begin
  FUseVCLStyles := False;

{$IFDEF DELPHIXE2_LVL}
  if CheckVCLStylesEnabled(StyleServices, (csDesigning in ComponentState)) then
  begin
    FUseVCLStyles := True;

    SelectionColors.ColorTo     := StyleServices.GetSystemColor(clHighlight);
    SelectionColors.ColorFrom   := StyleServices.GetSystemColor(clHighlight);
    SelectionColors.BorderColor := StyleServices.GetSystemColor(clHighlight);

    {$IFDEF DELPHIXE6_LVL}
    if (seClient in StyleElements) then
    begin
    {$ENDIF}
      StyleServices.GetElementColor(StyleServices.GetElementDetails(tgCellNormal), ecFillColor, clr);
    {$IFDEF DELPHIXE6_LVL}
    end;
    {$ENDIF}

    {$IFDEF DELPHIXE6_LVL}
    if (seFont in StyleElements) then
    begin
    {$ENDIF}
      StyleServices.GetElementColor(StyleServices.GetElementDetails(tgCellNormal), ecTextColor, clr);
      for i := 0 to Columns.Count-1 do
      begin
        Columns[i].Font.Color := clr;
      end;
    {$IFDEF DELPHIXE6_LVL}
    end;
    {$ENDIF}

    StyleServices.GetElementColor(StyleServices.GetElementDetails(tgCellNormal), ecBorderColor, clr);
    ColumnLineColor := clr;
  end;
{$ENDIF DELPHIXE2_LVL}
end;

function THTMLTreeList.IsAnchor(node: TTreenode; x, y: integer): string;
var
  r: trect;
  s, a: string;
  xsize, ysize: integer;
  i, w, sp: integer;
  hyperlinks,mouselink:integer;
  focusanchor:string;
  re: TRect;
  ww: boolean;
begin
  r := node.DisplayRect(true);

  a := '';

  Canvas.Font.Assign(Font);

  w := 0;
  i := 0;

  sp := GetScrollPos(Handle, SB_HORZ);

  x := x + sp;

  while not ((x >= w - 2) and (x <= w + GetColWidth(i) - 2)) and (i < Columns.Count) do
  begin
    w := w + GetColWidth(i);
    inc(i);
  end;

  if (i = Columns.Count) then dec(i);
  s := GetColumnText(i, node.Text);

  r.left := w;
  if (i < Columns.Count - 1) then r.right := r.left + GetColWidth(i) else r.right := self.width;

  if (i = 0) then
  begin
    r.left := r.left + integer(TreeView_GetIndent(Handle)) * (node.level + 1);
    if assigned(Images) then r.left := r.left + Images.Width;
  end;

{$IFDEF TMSDEBUG}
  outputdebugstring(pchar('mouse is at pos ' + inttostr(x) + ' in col (' + inttostr(i) + ') @ [' + inttostr(r.left) + ':' + inttostr(r.right) + ']'));
  outputdebugstring(pchar(s));
{$ENDIF}

  ww := false;
  if (i < Columns.Count) and (i >= 0) then
    ww := Columns[i].WordWrap;

  HTMLDrawEx(canvas, s, r, fImages, x, y, -1, -1, 1, true, false, false, true, true, false, ww, FFormScaled,
    1.0, clBlue, clnone, clNone, clGray, a, s, focusanchor, xsize, ysize, hyperlinks, mouselink, re,
    FImageCache, nil, 0);

  Result := a;
end;

procedure THTMLTreeList.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) then
    InitVCLStyle(false);
end;

procedure THTMLTreeList.WMMouseMove(var message: TWMMouseMove);
var
  Node: TTreeNode;
  a: string;

begin
  Node := GetNodeAt(message.XPos, message.YPos);

  if Assigned(Node) then
  begin
    if HotTrack then
      Selected := Node;

    a := IsAnchor(node, message.XPos, message.YPos);
    if (a <> '') then
    begin
      {change from anchor to anchor}
      if (a <> fOldAnchor) and (self.Cursor = crHandPoint) then
      begin
        if FAnchorHint then
          Application.CancelHint;
        if Assigned(FOnAnchorExit) then
          FOnAnchorExit(self, Node, FOldAnchor);
      end;

      if (a <> fOldAnchor) then
      begin
        if Assigned(FOnAnchorEnter) then
          FOnAnchorEnter(self, Node, a);
      end;

      if (Cursor <> crHandPoint) then
      begin
        FOldCursor := Cursor;
        FOldAnchor := a;
        Cursor := crHandPoint;
      end;

    end
    else
    begin
      if FAnchorHint then
        Application.CancelHint;
      if (FOldAnchor <> '') then
      begin
        if Assigned(fOnAnchorExit) then
          FOnAnchorExit(self, Node, fOldAnchor);
        FOldAnchor := '';
      end;

      if (Cursor = crHandPoint) then
        Cursor := FOldCursor;
    end;
  end
  else
    if (Cursor = crHandPoint) then
      Cursor := FOldCursor;
    
  inherited;
end;

function THTMLTreeList.GetColumnText(col: integer; s: string): string;
var
  i: integer;
  su: string;
begin
  i := 0;
  su := s;
  while (i <= col) do
  begin
    if (pos(fSeparator, s) > 0) then
    begin
      su := copy(s, 1, pos(fSeparator, s) - 1);
      system.delete(s, 1, pos(fSeparator, s) + length(fSeparator) - 1);
    end
    else
    begin
      su := s;
      s := '';
    end;

    inc(i);
  end;
  result := su;
end;


procedure THTMLTreeList.SetImages(const Value: TCustomImageList);
begin
  FImages := Value;
  Invalidate;
end;

procedure THTMLTreeList.SetURLColor(const Value: TColor);
begin
  if (FURLColor <> value) then
  begin
    FURLColor := Value;
    Invalidate;
  end;
end;

procedure THTMLTreeList.SetSelectionColors(const Value: TGradientStyle);
begin
  FSelectionColors.Assign(Value);
  Invalidate;
end;

procedure THTMLTreeList.SetSelectionFontColor(const Value: tcolor);
begin
  if (FSelectionFontColor <> Value) then
  begin
    FSelectionFontColor := Value;
    Invalidate;
  end;
end;

procedure THTMLTreeList.BandsChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure THTMLTreeList.BeginUpdate;
begin
  Items.BeginUpdate;
  inc(FUpdateCount);
end;

procedure THTMLTreeList.CMHintShow(var Msg: TMessage);
var
  CanShow: Boolean;
  hi: PHintInfo;
  anchor: string;
  Node: TTreeNode;
begin
  CanShow := True;
  hi := PHintInfo(Msg.LParam);

  Node := GetNodeAt(hi^.cursorPos.x, hi^.cursorpos.y);
  if Assigned(Node) and FAnchorHint then
  begin
    anchor := IsAnchor(Node, hi^.cursorPos.x, hi^.cursorpos.y);
    if (anchor <> '') then
    begin
      hi^.HintPos := clienttoscreen(hi^.CursorPos);
      hi^.hintpos.y := hi^.hintpos.y - 10;
      hi^.hintpos.x := hi^.hintpos.x + 10;
      hi^.HintStr := anchor;
    end;
  end;
  Msg.Result := Ord(not CanShow);
end;

{$IFDEF DELPHIXE2_LVL}
procedure THTMLTreeList.CMStyleChanged(var Message: TMessage);
begin
  InitVCLStyle(True);
  Invalidate;
end;
{$ENDIF}

function THTMLTreeList.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function THTMLTreeList.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure THTMLTreeList.SetVersion(const Value: string);
begin

end;

{ THeaderSettings }

procedure THeaderSettings.Changed;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

constructor THeaderSettings.Create(aOwner: THTMLTreeList);
begin
  inherited Create;
  FOwner := AOwner;
  FColor := clBtnFace;
  FColorTo := clNone;
  FHeight := 18;
  FAllowResize := true;
  FFlat := false;
  FFont := TFont.Create;
  FFont.OnChange := FontChanged;
end;

destructor THeaderSettings.Destroy;
begin
  FFont.Free;
  inherited;
end;

procedure THeaderSettings.FontChanged(Sender: TObject);
begin
  Changed;
end;

procedure THeaderSettings.SetAllowResize(const Value: boolean);
begin
  if (FAllowResize <> Value) then
  begin
    FAllowResize := Value;
    Changed;
  end;
end;

procedure THeaderSettings.SetColor(const Value: tColor);
begin
  if (FColor <> Value) then
  begin
    FColor := Value;
    Changed;
  end;
end;

procedure THeaderSettings.SetColorTo(const Value: TColor);
begin
  if (FColorTo <> Value) then
  begin
    FColorTo := Value;
    Changed;
  end;
end;

procedure THeaderSettings.SetFlat(const Value: boolean);
begin
  if (FFlat <> Value) then
  begin
    FFlat := Value;
    Changed;
  end;
end;

procedure THeaderSettings.SetFont(const Value: tFont);
begin
  FFont.Assign(Value);
  Changed;
end;

procedure THeaderSettings.SetHeight(const Value: integer);
begin
  if (FHeight <> Value) then
  begin
    FHeight := Value;
    Changed;
  end;
end;


{ THTMLTreeListBands }

procedure THTMLTreeListBands.Assign(Source: TPersistent);
begin
  if (Source is THTMLTreeListBands) then
  begin
    FActive := (Source as THTMLTreeListBands).Active;
    FPrimaryColor := (Source as THTMLTreeListBands).PrimaryColor;
    FSecondaryColor := (Source as THTMLTreeListBands).SecondaryColor;
  end;
end;

procedure THTMLTreeListBands.Changed;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

constructor THTMLTreeListBands.Create;
begin
  inherited;
  FActive := false;
  FPrimaryColor := clWindow;
  FSecondaryColor := clInfoBk;
end;

procedure THTMLTreeListBands.SetActive(const Value: boolean);
begin
  if (FActive <> Value) then
  begin
    FActive := Value;
    Changed;
  end;
end;

procedure THTMLTreeListBands.SetPrimaryColor(const Value: TColor);
begin
  if (FPrimaryColor <> Value) then
  begin
    FPrimaryColor := Value;
    Changed;
  end;
end;

procedure THTMLTreeListBands.SetSecondaryColor(const Value: TColor);
begin
  if (FSecondaryColor <> Value) then
  begin
    FSecondaryColor := Value;
    Changed;
  end;
end;


end.

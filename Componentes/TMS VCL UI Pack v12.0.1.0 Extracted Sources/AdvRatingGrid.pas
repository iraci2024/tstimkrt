{***************************************************************************}
{ TAdvRatingGrid component                                                  }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright � 2015 - 2020                                        }
{            Email : info@tmssoftware.com                                   }
{            Web : https://www.tmssoftware.com                              }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of the author.                                      }
{***************************************************************************}

unit AdvRatingGrid;

{$I TMSDEFS.INC}

interface

uses
  Windows, Controls, Classes, Graphics, uxTheme, Messages, JPEG, PNGImage,
  ImgList, Types, Forms, AdvStyleIF;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 5; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 4; // Build nr.

  // version history
  // 1.0.0.0  : First release
  // 1.1.0.0  : New : Support for category images
  //            New : Category text orientation setting
  // 1.1.0.1  : Fixed : Key handling with separator items
  //          : Fixed : Focus indication with custom graphics
  // 1.1.1.0  : New : Property ShowFocus added
  // 1.1.2.0  : New : Public property Checked[ItemIndex, CategoryIndex]: boolean added
  // 1.1.3.0  : New : Event : OnShowItemCategory event added
  // 1.1.4.0  : New : Public property ControlHeight added
  // 1.2.0.0  : New : TRatingCategory.Visible property added
  // 1.3.0.0  : New : BorderStyle, BorderColor property added
  //          : New : AutoSize property added
  // 1.3.1.0  : Improved : Support for High DPI: text and spacing.
  // 1.4.0.0  : New : Support for VCL Styles added
  // 1.5.0.0  : New : TRatingCategory.OnImageIndex,TRatingCategory.OffImageIndex properties added
  //          : New : TAdvRatingGrid.GridLines,TAdvRatingGrid.GridLineColor properties added
  //          : Fixed : Issue with large imagelist image sizes
  // 1.5.0.1  : Fixed : Issue with TRatingCategory.OnImageIndex,TRatingCategory.OffImageIndex initialization
  // 1.5.0.2  : Improved : Drawing precision of images
  // 1.5.0.3  : Fixed : VCL styles check updated to work in 10.4 Sydney
  // 1.5.0.4  : Fixed : Issue with font style settings for horizontal category headers

type
  TGridLines = (glNone, glVert, glHorz, glBoth);

  TValueChangedEvent = procedure(Sender: TObject; ItemIndex, CategoryIndex: integer; Value: boolean) of object;

  TRatingCategory = class(TCollectionItem)
  private
    FTitle: string;
    FTag: integer;
    FHint: string;
    FColor: TColor;
    FImageIndex: integer;
    FStyle: TFontStyles;
    FVisible: boolean;
    FOnImageIndex: integer;
    FOffImageIndex: integer;
    procedure SetColor(const Value: TColor);
    procedure SetImageIndex(const Value: integer);
    procedure SetStyle(const Value: TFontStyles);
    procedure SetTitle(const Value: string);
    procedure SetVisible(const Value: boolean);
    procedure SetOffImageIndex(const Value: integer);
    procedure SetOnImageIndex(const Value: integer);
  protected
    property Hint: string read FHint write FHint;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Color: TColor read FColor write SetColor default clBlack;
    property ImageIndex: integer read FImageIndex write SetImageIndex default -1;
    property OffImageIndex: integer read FOffImageIndex write SetOffImageIndex default -1;
    property OnImageIndex: integer read FOnImageIndex write SetOnImageIndex default -1;
    property Style: TFontStyles read FStyle write SetStyle default [];
    property Title: string read FTitle write SetTitle;
    property Tag: integer read FTag write FTag default 0;
    property Visible: boolean read FVisible write SetVisible default  true;
  end;

  TRatingCategories = class(TOwnedCollection)
  private
    FOnChange: TNotifyEvent;
    function GetItem(i: Integer): TRatingCategory;
    procedure SetItem(i: Integer; const Value: TRatingCategory);
    function GetVisibleCount: integer;
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    function Add: TRatingCategory;
    function Insert(Index: integer): TRatingCategory;
    property Items[i: Integer]: TRatingCategory read GetItem write SetItem; default;
    property VisibleCount: integer read GetVisibleCount;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TRatingItem = class(TCollectionItem)
  private
    FSeparator: boolean;
    FHint: string;
    FColor: TColor;
    FTitle: string;
    FTag: integer;
    FStyle: TFontStyles;
    FImageIndex: integer;
    FValue: int64;
    procedure SetColor(const Value: TColor);
    procedure SetImageIndex(const Value: integer);
    procedure SetStyle(const Value: TFontStyles);
    procedure SetTitle(const Value: string);
    procedure SetValue(const Value: int64);
    procedure SetSeparator(const Value: boolean);
  protected
    property Hint: string read FHint write FHint;
    property ImageIndex: integer read FImageIndex write SetImageIndex;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Color: TColor read FColor write SetColor default clBlack;
    property Style: TFontStyles read FStyle write SetStyle default [];
    property Title: string read FTitle write SetTitle;
    property Separator: Boolean read FSeparator write SetSeparator default false;
    property Tag: Integer read FTag write FTag default 0;
    property Value: int64 read FValue write SetValue default 0;
  end;

  TRatingItems = class(TOwnedCollection)
  private
    FOnChange: TNotifyEvent;
    function GetItem(i: Integer): TRatingItem;
    procedure SetItem(i: Integer; const Value: TRatingItem);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    function Add: TRatingItem;
    function Insert(Index: integer): TRatingItem;
    property Items[i: Integer]: TRatingItem read GetItem write SetItem; default;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TCategoryType = (catRating, catFeature);

  TCatergoryOrientation = (caoVertical, caoHorizontal);

  TShowItemCategoryEvent = procedure(Sender: TObject; ItemIndex, CategoryIndex: integer; var DoShow: boolean) of object;

  TAdvCustomRatingGrid = class(TCustomControl)
  private
    FIsComCtl6: boolean;
    FItems: TRatingItems;
    FCategories: TRatingCategories;
    FCategoryOrientation: TCatergoryOrientation;
    FItemFont: TFont;
    FCategoryFont: TFont;
    FCategoryType: TCategoryType;
    FSpacing: integer;
    FCatIndent: integer;
    FHotItem: TPoint;
    FDownItem: TPoint;
    FKeyItem: TPoint;
    FOnValueChanged: TValueChangedEvent;
    FCheckOnPicture: TPicture;
    FCheckOffPicture: TPicture;
    FRadioOffPicture: TPicture;
    FRadioOnPicture: TPicture;
    FShowFocus: boolean;
    FImages: TCustomImageList;
    FOnShowItemCategory: TShowItemCategoryEvent;
    FBorderColor: TColor;
    FBorderStyle: TBorderStyle;
    FAutoSize: boolean;
    FUseVCLStyles: boolean;
    FGridLines: TGridLines;
    FGridLineColor: TColor;
    procedure InitVCLStyle(init: boolean);
    {$IFDEF DELPHIXE2_LVL}
    procedure CMStyleChanged(var Message: TMessage); message CM_STYLECHANGED;
    {$ENDIF}
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure SetCategories(const Value: TRatingCategories);
    procedure SetItems(const Value: TRatingItems);
    procedure SetCategoryFont(const Value: TFont);
    procedure SetItemFont(const Value: TFont);
    procedure SetCategoryType(const Value: TCategoryType);
    procedure SetSpacing(const Value: integer);
    procedure SetCheckOffPicture(const Value: TPicture);
    procedure SetCheckOnPicture(const Value: TPicture);
    procedure SetRadioOffPicture(const Value: TPicture);
    procedure SetRadioOnPicture(const Value: TPicture);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    procedure SetShowFocus(const Value: boolean);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetCategoryOrientation(const Value: TCatergoryOrientation);
    function GetChecked(ItemIndex, CategoryIndex: integer): boolean;
    procedure SetChecked(ItemIndex, CategoryIndex: integer; const Value: boolean);
    function GetTotalItemHeight: integer;
    procedure SetBorderColor(const Value: TColor);
    procedure SetBorderStyle(const Value: TBorderStyle);
    procedure SetAutoSizeEx(const Value: boolean);
    procedure UpdateSize;
    procedure SetGridLineColor(const Value: TColor);
    procedure SetGridLines(const Value: TGridLines);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    function XYToItem(X,Y: integer): TPoint;
    function ItemCategoryVisible(ItemIndex, CategoryIndex: Integer): boolean; virtual;
    procedure ToggleItem(X,Y: integer);
    procedure DoValueChanged(ItemIndex, CategoryIndex: integer; Value: boolean); virtual;
    procedure DoShowItemCategory(ItemIndex, CategoryIndex: integer; var DoShow: boolean); virtual;
    procedure DataChanged(Sender: TObject);
    procedure Paint; override;
    procedure DoEnter; override;
    procedure DoExit; override;
    function GetVersionNr: Integer; virtual;
    function GetPictureSize: TSize;
    function GetItemHeight: integer;
    function GetCategoriesSize: integer;
    function GetItemsSize: integer;
    function PaintCategories(ACanvas: TCanvas; w,h: integer): integer;
    function PaintItems(ACanvas: TCanvas; h: integer): integer;
    procedure PaintControls(ACanvas: TCanvas; w,h,c: integer);
    procedure DrawCheck(ACanvas: TCanvas; x,y: integer; Checked, Hot, Down, Focus: boolean); virtual;
    procedure DrawRadio(ACanvas: TCanvas; x,y: integer; Checked, Hot, Down, Focus: boolean); virtual;
    procedure DrawImage(ACanvas: TCanvas; x,y: integer; CatIndex: integer; Checked: boolean); virtual;
    procedure Loaded; override;
    procedure CreateWnd; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property AutoSize: boolean read FAutoSize write SetAutoSizeEx default false;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsNone;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clNone;
    property Checked[ItemIndex,CategoryIndex: integer]: boolean read GetChecked write SetChecked;
    property Categories: TRatingCategories read FCategories write SetCategories;
    property CategoryFont: TFont read FCategoryFont write SetCategoryFont;
    property CategoryType: TCategoryType read FCategoryType write SetCategoryType default catRating;
    property CategoryOrientation: TCatergoryOrientation read FCategoryOrientation write SetCategoryOrientation default caoVertical;
    property CheckOffPicture: TPicture read FCheckOffPicture write SetCheckOffPicture;
    property CheckOnPicture: TPicture read FCheckOnPicture write SetCheckOnPicture;
    property ControlHeight: integer read GetTotalItemHeight;
    property GridLines: TGridLines read FGridLines write SetGridLines default glNone;
    property GridLineColor: TColor read FGridLineColor write SetGridLineColor default clSilver;
    property Images: TCustomImageList read FImages write SetImages;
    property Items: TRatingItems read FItems write SetItems;
    property ItemFont: TFont read FItemFont write SetItemFont;
    property RadioOffPicture: TPicture read FRadioOffPicture write SetRadioOffPicture;
    property RadioOnPicture: TPicture read FRadioOnPicture write SetRadioOnPicture;
    property ShowFocus: boolean read FShowFocus write SetShowFocus default true;
    property Spacing: integer read FSpacing write SetSpacing default 6;
    property Version: string read GetVersion write SetVersion;
    property OnShowItemCategory: TShowItemCategoryEvent read FOnShowItemCategory write FOnShowItemCategory;
    property OnValueChanged: TValueChangedEvent read FOnValueChanged write FOnValueChanged;
  end;


  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvRatingGrid = class(TAdvCustomRatingGrid)
  published
    property Align;
    property Anchors;
    property AutoSize;
    property BorderColor;
    property BorderStyle;
    property Constraints;
    property Categories;
    property CategoryFont;
    property CategoryOrientation;
    property CategoryType;
    property CheckOnPicture;
    property CheckOffPicture;
    property Color;
    property DockSite;
    property DragCursor;
    property DragMode;
    property DragKind;
    property Enabled;
    property Images;
    property Items;
    property ItemFont;
    property Padding;
    property RadioOnPicture;
    property RadioOffPicture;
    property ShowFocus;
    property ShowHint;
    property Spacing;
    {$IFDEF DELPHIXE6_LVL}
    property StyleElements;
    {$ENDIF}
    property TabOrder;
    property TabStop;
    property Version;
    property Visible;
    property OnCanResize;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDockOver;
    property OnDockDrop;
    property OnDragOver;
    property OnDragDrop;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyUp;
    property OnKeyPress;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnMouseLeave;
    property OnMouseEnter;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
    property OnShowItemCategory;
    property OnValueChanged;
  end;


implementation

uses
  {$IFDEF DELPHIXE2_LVL}
  VCL.Themes,
  {$ENDIF}
  StrUtils, SysUtils, Math;

function GetFileVersion(FileName:string): Integer;
var
  FileHandle:dword;
  l: Integer;
  pvs: PVSFixedFileInfo;
  lptr: uint;
  querybuf: array[0..255] of char;
  buf: PChar;
begin
  Result := -1;

  StrPCopy(querybuf,FileName);
  l := GetFileVersionInfoSize(querybuf,FileHandle);
  if (l>0) then
  begin
    GetMem(buf,l);
    GetFileVersionInfo(querybuf,FileHandle,l,buf);
    if VerQueryValue(buf,'\',Pointer(pvs),lptr) then
    begin
      if (pvs^.dwSignature = $FEEF04BD) then
      begin
        Result := pvs^.dwFileVersionMS;
      end;
    end;
    FreeMem(buf);
  end;
end;

function GetDPIScale(AControl: TControl; ACanvas: TCanvas): single;
var
  {$IFDEF DELPHIXE10_LVL}
  frm: TCustomForm;
  {$ENDIF}
  {$IFNDEF DELPHIXE10_LVL}
  FHDC: HDC;
  {$ENDIF}
begin
  {$IFDEF DELPHIXE10_LVL}
  Result := 1;
  frm := GetParentForm(AControl);
  if Assigned(frm) and (frm is TForm) and (frm as TForm).Scaled then
  begin
    Result := TForm(frm).Monitor.PixelsPerInch / TForm(frm).PixelsPerInch;
  end;
  {$ENDIF}

  {$IFNDEF DELPHIXE10_LVL}
  if Assigned(ACanvas) then
  begin
    Result := GetDeviceCaps(ACanvas.Handle, LOGPIXELSY) / 96;
  end
  else
  begin
    FHDC := GetDC(0);
    Result := GetDeviceCaps(FHDC, LOGPIXELSX) / 96;
    ReleaseDC(0, FHDC);
  end;
  {$ENDIF}
end;

{ TAdvCustomRatingGrid }

constructor TAdvCustomRatingGrid.Create(AOwner: TComponent);
var
  FDesignTime: boolean;
  i: integer;
begin
  inherited;
  FItems := TRatingItems.Create(Self, TRatingItem);
  FItems.OnChange := DataChanged;
  FItemFont := TFont.Create;
  FItemFont.OnChange := DataChanged;
  FCategories := TRatingCategories.Create(Self, TRatingCategory);
  FCategories.OnChange := DataChanged;
  FCategoryOrientation := caoVertical;
  FCategoryFont := TFont.Create;
  FCategoryFont.OnChange := DataChanged;
  FCheckOnPicture := TPicture.Create;
  FCheckOnPicture.OnChange := DataChanged;
  FCheckOffPicture := TPicture.Create;
  FCheckOffPicture.OnChange := DataChanged;
  FRadioOnPicture := TPicture.Create;
  FRadioOnPicture.OnChange := DataChanged;
  FRadioOffPicture := TPicture.Create;
  FRadioOffPicture.OnChange := DataChanged;

  i := GetFileVersion('COMCTL32.DLL');
  i := (i shr 16) and $FF;
  FIsComCtl6 := (i > 5);

  FSpacing := 6;
  FHotItem := Point(-1,-1);
  FDownItem := Point(-1,-1);
  FKeyItem := Point(0,0);
  FShowFocus := true;
  FBorderStyle := bsNone;
  FBorderColor := clNone;

  Width := 300;
  Height := 220;
  DoubleBuffered := true;
  FUseVCLStyles := False;

  FGridLines := glNone;
  FGridLineColor := clSilver;

  FDesignTime := (csDesigning in ComponentState) and not
                 ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));

  if FDesignTime then
  begin
    FCategories.Add.Title := 'Bad';
    FCategories.Add.Title := 'Average';
    FCategories.Add.Title := 'Good';

    FItems.Add.Title := 'Milk';
    FItems.Add.Title := 'Beer';
    FItems.Add.Title := 'Wine';
  end;

  if not (csDesigning in ComponentState) then
    InitVCLStyle(true);
end;

procedure TAdvCustomRatingGrid.CreateWnd;
begin
  inherited;
  if not (csDesigning in ComponentState) then
    InitVCLStyle(true);
end;

destructor TAdvCustomRatingGrid.Destroy;
begin
  FCheckOnPicture.Free;
  FCheckOffPicture.Free;
  FRadioOffPicture.Free;
  FRadioOnPicture.Free;
  FItems.Free;
  FItemFont.Free;
  FCategories.Free;
  FCategoryFont.Free;
  inherited;
end;

procedure TAdvCustomRatingGrid.DoEnter;
begin
  inherited;

  if FKeyItem.Y < 0 then
    FKeyItem.Y := 0;

  if FKeyItem.X < 0 then
    FKeyItem.X := 0;

  while (FKeyItem.X < Items.Count) and Items[FKeyItem.X].Separator do
    FKeyItem.X := FKeyItem.X + 1;

  Invalidate;
end;

procedure TAdvCustomRatingGrid.DoExit;
begin
  inherited;
  Invalidate;
end;

procedure TAdvCustomRatingGrid.DoShowItemCategory(ItemIndex,
  CategoryIndex: integer; var DoShow: boolean);
begin
  if Assigned(OnShowItemCategory) then
    OnShowItemCategory(Self, ItemIndex, CategoryIndex, DoShow);
end;

procedure TAdvCustomRatingGrid.DoValueChanged(ItemIndex, CategoryIndex: integer;
  Value: boolean);
begin
  if Assigned(OnValueChanged) then
    OnValueChanged(Self, ItemIndex, CategoryIndex, Value);
end;

procedure TAdvCustomRatingGrid.DrawCheck(ACanvas: TCanvas; x, y: integer; Checked, Hot, Down, Focus: boolean);
var
  HTheme: THandle;
  r: TRect;
  DrawThemed: boolean;
  {$IFDEF DELPHIXE2_LVL}
  lDetails: TThemedElementDetails;
  {$ENDIF}
begin
  if FUseVCLStyles then
  begin
    r := Rect(x, y, x + 16, y + 16);
    {$IFDEF DELPHIXE2_LVL}
    if Enabled then
    begin
      if Checked then
        lDetails := StyleServices.GetElementDetails(tbCheckBoxCheckedNormal)
      else
        lDetails := StyleServices.GetElementDetails(tbCheckBoxUncheckedNormal);
    end
    else
    begin
      if Checked then
        lDetails := StyleServices.GetElementDetails(tbCheckBoxCheckedDisabled)
      else
        lDetails := StyleServices.GetElementDetails(tbCheckBoxUncheckedDisabled);
    end;

    StyleServices.DrawElement(ACanvas.Handle, lDetails, r);
    {$ENDIF}
    if Focus then
    begin
      InflateRect(r,2,2);
      DrawFocusRect(ACanvas.Handle, R);
    end;
  end
  else
  begin
    if Assigned(FCheckOnPicture.Graphic) and Assigned(FCheckOffPicture.Graphic) and not FCheckOnPicture.Graphic.Empty and not FCheckOffPicture.Graphic.Empty then
    begin
      r := Rect(x, y, x + FCheckOnPicture.Graphic.Width, y + FCheckOnPicture.Graphic.Height);

      if Checked then
        ACanvas.Draw(x,y,FCheckOnPicture.Graphic)
      else
        ACanvas.Draw(x,y,FCheckOffPicture.Graphic);

      if Focus then
      begin
        InflateRect(r,2,2);
        DrawFocusRect(ACanvas.Handle, R);
      end;
    end
    else
    begin
      r := Rect(x, y, x + 16, y + 16);

      DrawThemed := IsThemeActive and FIsComCtl6;

      if DrawThemed then
      begin
        HTheme := OpenThemeData(Handle,'button');

        if HTheme <> 0 then
        begin
          if Checked then
            begin
              if not Enabled then
                 DrawThemeBackground(HTheme,ACanvas.Handle, BP_CHECKBOX,CBS_CHECKEDDISABLED,r,nil)
              else
                if Down then
                  DrawThemeBackground(HTheme,ACanvas.Handle, BP_CHECKBOX,CBS_CHECKEDNORMAL or CBS_PUSHED,r,nil)
                else
                  if Hot then
                    DrawThemeBackground(HTheme,ACanvas.Handle, BP_CHECKBOX,CBS_CHECKEDHOT or CBS_HOT,r,nil)
                  else
                    DrawThemeBackground(HTheme,ACanvas.Handle, BP_CHECKBOX,CBS_CHECKEDNORMAL,r,nil);
            end
            else
            begin
              if not Enabled then
                DrawThemeBackground(HTheme,ACanvas.Handle, BP_CHECKBOX,CBS_UNCHECKEDDISABLED,r,nil)
              else
              if Down then
                DrawThemeBackground(HTheme,ACanvas.Handle, BP_CHECKBOX,CBS_UNCHECKEDNORMAL or CBS_PUSHED,r,nil)
              else
                if Hot then
                  DrawThemeBackground(HTheme,ACanvas.Handle, BP_CHECKBOX,CBS_HOT,r,nil)
                else
                  DrawThemeBackground(HTheme,ACanvas.Handle, BP_CHECKBOX,CBS_UNCHECKEDNORMAL,r,nil);
            end;
          CloseThemeData(HTheme);

          if Focus then
          begin
            InflateRect(r,2,2);
            DrawFocusRect(ACanvas.Handle, R);
          end;
        end;
      end
      else
      begin
        if Checked then
          DrawFrameControl(ACanvas.Handle, r, DFC_BUTTON, DFCS_BUTTONCHECK or DFCS_FLAT or DFCS_CHECKED)
        else
          DrawFrameControl(ACanvas.Handle, r, DFC_BUTTON, DFCS_BUTTONCHECK or DFCS_FLAT);
      end;
    end;
  end; { if UseVCLStyles... }
end;

procedure TAdvCustomRatingGrid.DrawImage(ACanvas: TCanvas; x, y,
  CatIndex: integer; Checked: boolean);
var
  dx: integer;
begin
  dx := (Images.Width div 2) - 2;
  if Checked then
    Images.Draw(ACanvas, x - dx, y, Categories[CatIndex].OnImageIndex)
  else
    Images.Draw(ACanvas, x - dx, y, Categories[CatIndex].OffImageIndex);
end;

procedure TAdvCustomRatingGrid.DrawRadio(ACanvas: TCanvas; x, y: integer; Checked, Hot, Down, Focus: boolean);
var
  HTheme: THandle;
  r: TRect;
  DrawThemed: boolean;
  dx: integer;
  {$IFDEF DELPHIXE2_LVL}
  lDetails: TThemedElementDetails;
  {$ENDIF}
begin
  if FUseVCLStyles then
  begin
    r := Rect(x, y, x + 16, y + 16);
    {$IFDEF DELPHIXE2_LVL}
    if Enabled then
    begin
      if Checked then
        lDetails := StyleServices.GetElementDetails(tbRadioButtonCheckedNormal)
      else
        lDetails := StyleServices.GetElementDetails(tbRadioButtonUncheckedNormal);
    end
    else
    begin
      if Checked then
        lDetails := StyleServices.GetElementDetails(tbRadioButtonCheckedDisabled)
      else
        lDetails := StyleServices.GetElementDetails(tbRadioButtonUncheckedDisabled);
    end;

    StyleServices.DrawElement(ACanvas.Handle, lDetails, r);
    {$ENDIF}
    if Focus then
    begin
      InflateRect(r, 2, 2);
      DrawFocusRect(ACanvas.Handle, R);
    end;
  end
  else
  begin
    if Assigned(FRadioOnPicture.Graphic) and Assigned(FRadioOffPicture.Graphic) and not FRadioOnPicture.Graphic.Empty and not FRadioOffPicture.Graphic.Empty then
    begin
      dx := FRadioOnPicture.Graphic.Width;
      r := Rect(x, y, x + dx, y + FRadioOnPicture.Graphic.Height);

      if Checked then
        ACanvas.Draw(x,y,FRadioOnPicture.Graphic)
      else
        ACanvas.Draw(x,y,FRadioOffPicture.Graphic);

      if Focus then
      begin
        InflateRect(r, 2, 2);
        DrawFocusRect(ACanvas.Handle, R);
      end;

    end
    else
    begin
      r := Rect(x, y, x + 16, y + 16);

      DrawThemed := FIsComCtl6 and IsThemeActive;

      if DrawThemed then
      begin
        HTheme := OpenThemeData(Handle,'button');

        if HTheme <> 0 then
        begin
          if Checked then
            begin
            if not Enabled then
               DrawThemeBackground(HTheme,ACanvas.Handle, BP_RADIOBUTTON,RBS_CHECKEDDISABLED,r,nil)
            else
              if Down then
                DrawThemeBackground(HTheme,ACanvas.Handle, BP_RADIOBUTTON,RBS_CHECKEDNORMAL or RBS_PUSHED,r,nil)
              else
                if Hot then
                  DrawThemeBackground(HTheme,ACanvas.Handle, BP_RADIOBUTTON,RBS_CHECKEDHOT or RBS_HOT,r,nil)
                else
                  DrawThemeBackground(HTheme,ACanvas.Handle, BP_RADIOBUTTON,RBS_CHECKEDNORMAL,r,nil);
            end
            else
            begin
              if not Enabled then
                DrawThemeBackground(HTheme,ACanvas.Handle, BP_RADIOBUTTON,RBS_UNCHECKEDDISABLED,r,nil)
              else
              if Down then
                DrawThemeBackground(HTheme,ACanvas.Handle, BP_RADIOBUTTON,RBS_UNCHECKEDNORMAL or RBS_PUSHED,r,nil)
              else
                if Hot then
                  DrawThemeBackground(HTheme,ACanvas.Handle, BP_RADIOBUTTON,RBS_UNCHECKEDHOT or RBS_HOT,r,nil)
                else
                  DrawThemeBackground(HTheme,ACanvas.Handle, BP_RADIOBUTTON,RBS_UNCHECKEDNORMAL,r,nil);
            end;
          CloseThemeData(HTheme);

          if Focus then
          begin
            InflateRect(r,2,2);
            DrawFocusRect(ACanvas.Handle, R);
          end;
        end;
      end
      else
      begin
        if Checked then
          DrawFrameControl(ACanvas.Handle, r, DFC_BUTTON, DFCS_BUTTONRADIO or DFCS_FLAT or DFCS_CHECKED)
        else
          DrawFrameControl(ACanvas.Handle, r, DFC_BUTTON, DFCS_BUTTONRADIO or DFCS_FLAT);
      end;

    end;
  end;
end;

procedure TAdvCustomRatingGrid.DataChanged(Sender: TObject);
begin
  if AutoSize then
    UpdateSize;

  Invalidate;
end;

function TAdvCustomRatingGrid.GetCategoriesSize: integer;
var
  i,tw: integer;
  LFont: TLogFont;
  hOldFont,hNewFont: HFont;
  DPIScale: single;
begin
  Result := 0;

  DPIScale := GetDPIScale(Self, Canvas);
  Canvas.Font.Assign(CategoryFont);
  Canvas.Font.Size := Round(CategoryFont.Size * DPIScale);

  if CategoryOrientation = caoVertical then
  begin

    GetObject(Canvas.Font.Handle,SizeOf(LFont),Addr(LFont));
    LFont.lfEscapement := 90 * 10;
    LFont.lfOrientation := 90 * 10;

    hNewFont := CreateFontIndirect(LFont);
    hOldFont := SelectObject(Canvas.Handle, hNewFont);

    for i := 0 to Categories.Count - 1 do
    begin
      if Categories[i].Visible then
      begin
        tw := Canvas.TextWidth(Categories[i].Title);
        if tw > Result then
          Result := tw;
      end;
    end;

    hNewFont := SelectObject(Canvas.Handle,hOldFont);
    DeleteObject(hNewFont);
  end
  else
  begin
    Result := Canvas.TextHeight('gh');
  end;

  if Assigned(Images) then
  begin
    tw := 0;
    for i := 0 to Categories.Count - 1 do
    begin
      if Categories[i].Visible then
      begin
        if Categories[i].ImageIndex >= 0 then
          tw := 1;
      end;
    end;

    Result := Result + tw * (Images.Height + 2);
  end;
end;

function TAdvCustomRatingGrid.GetChecked(ItemIndex,
  CategoryIndex: integer): boolean;
var
  v: int64;
begin
  if (ItemIndex < 0) or (ItemIndex >= Items.Count) then
    raise Exception.Create('Invalid Itemindex');

  if CategoryType = catRating then
  begin
    Result := Items[ItemIndex].Value = CategoryIndex;
  end
  else
  begin
    v := 1 shl CategoryIndex;
    Result := (Items[ItemIndex].Value and v) = v;
  end;
end;

function TAdvCustomRatingGrid.GetItemsSize: integer;
var
  i,tw: integer;
  DPIScale: single;
begin
  Result := 0;

  Canvas.Font.Assign(ItemFont);
  DPIScale := GetDPIScale(Self, Canvas);
  Canvas.Font.Size := Round(DPIScale * ItemFont.Size);

  for i := 0 to Items.Count - 1 do
  begin
    Canvas.Font.Style := Items[i].Style;
    tw := Canvas.TextWidth(Items[i].Title);
    if tw > Result then
      Result := tw;
  end;
end;

function TAdvCustomRatingGrid.GetPictureSize: TSize;
var
  DPIScale: single;
begin
  Result.CX := 16;
  Result.CY := 16;

  DPIScale := GetDPIScale(Self, Canvas);

  if (CategoryType = catFeature) then
  begin
    if Assigned(CheckOnPicture.Graphic) and Assigned(CheckOffPicture.Graphic) then
    begin

      if not CheckOnPicture.Graphic.Empty and not CheckOffPicture.Graphic.Empty then
      begin
        Result.CX := Round(DPIScale * CheckOnPicture.Graphic.Width);
        Result.CY := Round(DPIScale * CheckOnPicture.Graphic.Height);
      end;
    end;
  end
  else
  begin
    if Assigned(RadioOnPicture.Graphic) and Assigned(RadioOffPicture.Graphic) then
    begin

      if not RadioOnPicture.Graphic.Empty and not RadioOffPicture.Graphic.Empty then
      begin
        Result.CX := Round(DPIScale * RadioOnPicture.Graphic.Width);
        Result.CY := Round(DPIScale * RadioOnPicture.Graphic.Height);
      end;
    end;
  end;
end;

function TAdvCustomRatingGrid.GetTotalItemHeight: integer;
begin
  Result := GetCategoriesSize + (GetItemHeight * Items.Count);
end;

function TAdvCustomRatingGrid.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TAdvCustomRatingGrid.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TAdvCustomRatingGrid.InitVCLStyle(init: boolean);
{$IFDEF DELPHIXE2_LVL}
const
  CStates: array[Boolean] of TThemedTextLabel = (ttlTextLabelDisabled, ttlTextLabelNormal);
var
  i: integer;
  clr: TColor;
  lDetails: TThemedElementDetails;
{$ENDIF xe2}
begin
  FUseVCLStyles := False;

  {$IFDEF DELPHIXE2_LVL}
  if CheckVCLStylesEnabled(StyleServices, (csDesigning in ComponentState)) then
  begin
    {$IFDEF DELPHIXE6_LVL}
    FUseVCLStyles := (StyleElements <> []);
    {$ENDIF XE6}
    {$IFNDEF DELPHIXE6_LVL}
    FUseVCLStyles := true;
    {$ENDIF XE6}

    if not FUseVCLStyles then
      Exit;

    FBorderColor := StyleServices.GetSystemColor(clBtnShadow);

    {$IFDEF DELPHIXE6_LVL}
    if (seClient in StyleElements) then
    begin
    {$ENDIF xe6}
      Color := StyleServices.GetSystemColor(clBtnFace);

      lDetails := StyleServices.GetElementDetails(CStates[Enabled]);
      StyleServices.GetElementColor(lDetails, ecTextColor, clr);
      for i := 0 to Categories.Count - 1 do
        Categories[i].Color := clr;
      for i := 0 to Items.Count - 1 do
        Items[i].Color := clr;
    {$IFDEF DELPHIXE6_LVL}
    end;
    {$ENDIF xe6}
  end;
  {$ENDIF xe2}
end;

{$IFDEF DELPHIXE2_LVL}
procedure TAdvCustomRatingGrid.CMStyleChanged(var Message: TMessage);
begin
  InitVCLStyle(true);
  Invalidate;
end;
{$ENDIF}

function TAdvCustomRatingGrid.ItemCategoryVisible(ItemIndex,
  CategoryIndex: Integer): boolean;
begin
  Result := Categories[CategoryIndex].Visible;
  DoShowItemCategory(ItemIndex, CategoryIndex, Result);
end;

function TAdvCustomRatingGrid.GetItemHeight: integer;
var
  dy,i: integer;
  catimg: boolean;
  DPIScale: single;
begin
  DPIScale := GetDPIScale(Self, Canvas);

  Canvas.Font.Assign(ItemFont);
  Canvas.Font.Size := Round(ItemFont.Size * DPIScale);

  dy := Max(Canvas.TextHeight('gh'),GetPictureSize.cy);

  catimg := false;

  if Assigned(Images) then
  begin
    for i := 0 to Categories.Count - 1 do
      begin
        if (Categories[i].OffImageIndex <> -1) and (Categories[i].OnImageIndex <> -1) then
          catimg := true;
      end;

    if catimg then
      if Images.Height > dy then
        dy := Images.Height;
  end;

  Result := dy + 2 + Round(DPIScale * Spacing);
end;

procedure TAdvCustomRatingGrid.KeyDown(var Key: Word; Shift: TShiftState);
var
  FNewKey: TPoint;
  d: integer;
  doshow: boolean;
begin
  inherited;

  if not Enabled then
    Exit;

  FNewKey := FKeyItem;

  case Key of
  VK_UP:
    begin
      if FKeyItem.X > 0 then
      begin
        d := FKeyItem.X;
        repeat
          dec(d);
          if not Items[d].Separator then
            FNewKey.X := d;
        until (d = 0) or ((Items[d].Separator = false) and ItemCategoryVisible(FNewKey.X, FNewKey.Y));
      end;
    end;
  VK_DOWN:
    begin
      if FKeyItem.X < Items.Count - 1 then
      begin
        d := FKeyItem.X;
        repeat
          inc(d);
          if not Items[d].Separator then
            FNewKey.X := d;
        until (d = Items.Count - 1) or ((Items[d].Separator = false) and ItemCategoryVisible(FNewKey.X, FNewKey.Y));
      end;
    end;
  VK_LEFT:
    begin
      while (FNewKey.Y > 0) do
      begin
        FNewKey.Y := FNewKey.Y - 1;
        if (ItemCategoryVisible(FNewKey.X, FNewKey.Y)) then
          break;
      end;
    end;
  VK_RIGHT:
    begin
      while (FNewKey.Y < Categories.Count - 1) do
      begin
        FNewKey.Y := FNewKey.Y + 1;
        if (ItemCategoryVisible(FNewKey.X, FNewKey.Y)) then
          break;
      end;
    end;
  VK_SPACE:
    begin
      ToggleItem(FKeyItem.X, FKeyItem.Y);
      Repaint;
    end;
  end;


  if (FKeyItem.X <> FNewKey.X) or (FKeyItem.Y <> FNewKey.Y) then
  begin
    doshow := true;

    DoShowItemCategory(FNewKey.Y, FNewKey.X, doshow);

    if doshow then
    begin
      FKeyItem := FNewKey;
      Repaint;
    end;
  end;
end;

procedure TAdvCustomRatingGrid.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) then
    InitVCLStyle(true);
end;

procedure TAdvCustomRatingGrid.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  pt: TPoint;
  pnt: boolean;
begin
  inherited;

  SetFocus;

  if not Enabled then
    Exit;

  pt := XYToItem(X,Y);

  pnt := (pt.X <> FDownItem.X) or (pt.Y <> FDownItem.Y);

  FDownItem := pt;

  if (FDownItem.X <> -1) and (FDownItem.Y <> -1) then
  begin
    FKeyItem := FDownItem;
    ToggleItem(FDownItem.X, FDownItem.Y);
  end;

  if pnt then
    Invalidate;
end;

procedure TAdvCustomRatingGrid.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  pt: TPoint;
  pnt: boolean;
begin
  inherited;

  pt := XYToItem(X,Y);

  pnt := (pt.X <> FHotItem.X) or (pt.Y <> FHotItem.Y);

  FHotItem := pt;

  if pnt then
    Invalidate;
end;

procedure TAdvCustomRatingGrid.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  pnt: boolean;
begin
  inherited;

  pnt := (FDownItem.X <> -1) or (FDownItem.Y <> - 1);

  FDownItem := Point(-1,-1);

  if pnt then
    Invalidate;
end;

procedure TAdvCustomRatingGrid.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  if (AOperation = opRemove) and (AComponent = FImages) then
    Images := nil;
end;

procedure TAdvCustomRatingGrid.Paint;
var
  h,w: integer;
begin
  inherited;
  h := GetCategoriesSize;
  w := GetItemsSize;

  FCatIndent := PaintCategories(Canvas,w,h);
  PaintItems(Canvas,h);
  PaintControls(Canvas, w, h, FCatIndent);

  if BorderStyle = bsSingle then
  begin
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := BorderColor;
    Canvas.FrameRect(ClientRect);
  end;
end;

function TAdvCustomRatingGrid.PaintCategories(ACanvas: TCanvas; w, h: integer): integer;
var
  i,x,d,di,ih,th,imgdiff: integer;
  LFont: TLogFont;
  hOldFont,hNewFont: HFont;
  DPIScale: single;
begin
  Result := 0;

  if Categories.VisibleCount = 0 then
    Exit;

  x := w;

  d := (Width - w) div Categories.VisibleCount;

  ACanvas.Brush.Style := bsClear;

  ih := 0;
  if Assigned(Images) then
  begin
    for i := 0 to Categories.Count - 1 do
      if Categories[i].ImageIndex >= 0 then
        ih := Images.Height + 2;
  end;

  DPIScale := GetDPIScale(Self, Canvas);

  for i := 0 to Categories.Count - 1 do
  begin
    imgdiff := 0;

    if Assigned(Images) and (Categories[i].OnImageIndex >= 0) and (Categories[i].OffImageIndex >= 0) then
    begin
//      imgdiff := Round(((Images.Width - 16) div 4) / DPIScale);
    end;

    ACanvas.Font.Assign(CategoryFont);
    ACanvas.Font.Size := Round(DPIScale * CategoryFont.Size);
    th := ACanvas.TextHeight('gh');

    if Categories[i].Visible then
    begin
      if CategoryOrientation = caoVertical then
      begin
        ACanvas.Font.Style := Categories[i].Style;
        ACanvas.Font.Color := Categories[i].Color;

        GetObject(ACanvas.Font.Handle,SizeOf(LFont),Addr(LFont));
        LFont.lfEscapement := 90 * 10;
        LFont.lfOrientation := 90 * 10;

        hNewFont := CreateFontIndirect(LFont);
        hOldFont := SelectObject(ACanvas.Handle, hNewFont);

        di := ACanvas.TextHeight('gh');

        di := (d - di) div 2;

        Result := di;

        ACanvas.TextOut(x + di,h - ih,Categories[i].Title);

        hNewFont := SelectObject(ACanvas.Handle,hOldFont);
        DeleteObject(hNewFont);
      end
      else
      begin
        ACanvas.Font.Style := Categories[i].Style;
        ACanvas.Font.Color := Categories[i].Color;

        di := ACanvas.TextWidth(Categories[i].Title);

        di := (d - di) div 2;

        di := di - imgdiff;

        ACanvas.TextOut(x + di, h - ih - th, Categories[i].Title);

        Result := (d) div 2;
      end;

      if Assigned(Images) and (Categories[i].ImageIndex >= 0) and (Categories[i].ImageIndex < Images.Count) then
      begin
        DrawImage(ACanvas, x + (d div 2), h - ih + 2, Categories[i].ImageIndex, false);
      end;

      x := x + d;
    end;
  end;

  ACanvas.Brush.Style := bsSolid;
end;

procedure TAdvCustomRatingGrid.PaintControls(ACanvas: TCanvas; w, h, c: integer);
var
  i,j,x,y,dx,dy,delta:integer;
  b: int64;
  hot,down,focus,doshow,useimg: boolean;
  DPIScale: single;
begin
  if Categories.Count = 0 then
    Exit;

  DPIScale := GetDPIScale(Self, Canvas);

  dx := (Width - w) div Categories.VisibleCount;

  dy := GetItemHeight;

  y := h + 2 + Round(DPIScale * Spacing);

  for i := 0 to Items.Count - 1 do
  begin
    x := w;
    b := 1;

    if not Items[I].Separator then
    begin
      for j := 0 to Categories.Count - 1 do
      begin

        if Categories[j].Visible then
        begin
          doshow := true;

          DoShowItemCategory(i, j, doshow);

          if doshow then
          begin
            hot := (i = FHotItem.X) and (j = FHotItem.Y);
            down := (i = FDownItem.X) and (j = FDownItem.Y);
            focus := Focused and (i = FKeyItem.X) and (j = FKeyItem.Y) and ShowFocus;

            useimg := Assigned(Images) and (Categories.Items[j].OnImageIndex <> -1) and (Categories.Items[j].OffImageIndex <> -1);

            if CategoryType = catRating then
            begin
              if useimg then
                DrawImage(ACanvas, x + c, y, j, Items[i].Value = j)
              else
                DrawRadio(ACanvas, x + c - 8, y, Items[i].Value = j, hot, down, focus)
            end
            else
            begin
              if useimg then
                DrawImage(ACanvas, x + c, y, j, Items[i].Value and b = b)
              else
                DrawCheck(ACanvas, x + c - 8, y, Items[i].Value and b = b, hot, down, focus);
            end;
          end;

          b := b * 2;

          if (GridLines in [glVert, glBoth]) and (i = 0) then
          begin
            if j = 0 then
              delta := 10
            else
              delta := 0;

            ACanvas.Pen.Color := GridLineColor;
            ACanvas.MoveTo(x + delta, 0);
            ACanvas.LineTo(x + delta, Height);
          end;

          x := x + dx;
        end;
      end;
    end;
    y := y + dy;
  end;
end;

function TAdvCustomRatingGrid.PaintItems(ACanvas: TCanvas; h: integer): integer;
var
  i,d,th: integer;
  DPIScale: single;
begin
  DPIScale := GetDPIScale(Self, Canvas);
  ACanvas.Font.Assign(ItemFont);
  ACanvas.Font.Size := Round(DPIScale * ItemFont.Size);
  ACanvas.Brush.Style := bsClear;

  d := GetItemHeight;

  th := ACanvas.TextHeight('gh');

  if d > th then
    th := (d - 2 - Round(DPIScale * Spacing) - th) div 2
  else
    th := 0;

  h := h + 2 + Round(DPIScale * Spacing);

  for i := 0 to Items.Count - 1 do
  begin
    ACanvas.Font.Color := Items[i].Color;
    ACanvas.Font.Style := Items[i].Style;

    ACanvas.TextOut(2,h + th,Items[i].Title);

    if (GridLines in [glBoth, glHorz]) then
    begin
      ACanvas.Pen.Color := GridLineColor;
      ACanvas.MoveTo(0, h - 3);
      ACanvas.LineTo(Width, h - 3);
    end;

    h := h + d;
  end;

  if (GridLines in [glBoth, glHorz]) then
  begin
    ACanvas.Pen.Color := GridLineColor;
    ACanvas.MoveTo(0, h - 3);
    ACanvas.LineTo(Width, h - 3);
  end;

  ACanvas.Brush.Style := bsSolid;

  Result := h;
end;

procedure TAdvCustomRatingGrid.SetAutoSizeEx(const Value: boolean);
begin
  FAutoSize := Value;

  if FAutoSize then
  begin
    UpdateSize;
  end;
end;

procedure TAdvCustomRatingGrid.SetBorderColor(const Value: TColor);
begin
  if (FBorderColor <> Value) then
  begin
    FBorderColor := Value;
    Invalidate;
  end;
end;

procedure TAdvCustomRatingGrid.SetBorderStyle(const Value: TBorderStyle);
begin
  if (FBorderStyle <> Value) then
  begin
    FBorderStyle := Value;
    Invalidate;
  end;
end;

procedure TAdvCustomRatingGrid.SetCategories(const Value: TRatingCategories);
begin
  FCategories.Assign(Value);
  Invalidate;
end;

procedure TAdvCustomRatingGrid.SetCategoryFont(const Value: TFont);
begin
  FCategoryFont.Assign(Value);
end;

procedure TAdvCustomRatingGrid.SetCategoryOrientation(
  const Value: TCatergoryOrientation);
begin
  if (FCategoryOrientation <> Value) then
  begin
    FCategoryOrientation := Value;
    Invalidate;
  end;

end;

procedure TAdvCustomRatingGrid.SetCategoryType(const Value: TCategoryType);
begin
  if (FCategoryType <> Value) then
  begin
    FCategoryType := Value;
    Invalidate;
  end;
end;

procedure TAdvCustomRatingGrid.SetChecked(ItemIndex, CategoryIndex: integer;
  const Value: boolean);
var
  v: int64;
begin
  if (ItemIndex < 0) or (ItemIndex >= Items.Count) then
    raise Exception.Create('Invalid Itemindex');

  if (CategoryType = catRating) and Value then
    Items[ItemIndex].Value := CategoryIndex
  else
  begin
    v := 1 shl CategoryIndex;
    if Value then
      Items[ItemIndex].Value := Items[ItemIndex].Value or v
    else
      Items[ItemIndex].Value := Items[ItemIndex].Value and not v;
  end;
end;

procedure TAdvCustomRatingGrid.SetCheckOffPicture(const Value: TPicture);
begin
  FCheckOffPicture.Assign(Value);
end;

procedure TAdvCustomRatingGrid.SetCheckOnPicture(const Value: TPicture);
begin
  FCheckOnPicture.Assign(Value);
end;

procedure TAdvCustomRatingGrid.SetGridLineColor(const Value: TColor);
begin
  if (FGridLineColor <> Value) then
  begin
    FGridLineColor := Value;
    Invalidate;
  end;
end;

procedure TAdvCustomRatingGrid.SetGridLines(const Value: TGridLines);
begin
  if (FGridLines <> Value) then
  begin
    FGridLines := Value;
    Invalidate;
  end;
end;

procedure TAdvCustomRatingGrid.SetImages(const Value: TCustomImageList);
begin
  if (FImages <> Value) then
  begin
    FImages := Value;
    Invalidate;
  end;
end;

procedure TAdvCustomRatingGrid.SetItemFont(const Value: TFont);
begin
  FItemFont.Assign(Value);
end;

procedure TAdvCustomRatingGrid.SetItems(const Value: TRatingItems);
begin
  FItems.Assign(Value);
  Invalidate;
end;

procedure TAdvCustomRatingGrid.SetRadioOffPicture(const Value: TPicture);
begin
  FRadioOffPicture.Assign(Value);
  Invalidate;
end;

procedure TAdvCustomRatingGrid.SetRadioOnPicture(const Value: TPicture);
begin
  FRadioOnPicture.Assign(Value);
  Invalidate;
end;

procedure TAdvCustomRatingGrid.SetShowFocus(const Value: boolean);
begin
  if (FShowFocus <> Value) then
  begin
    FShowFocus := Value;
    Invalidate;
  end;
end;

procedure TAdvCustomRatingGrid.SetSpacing(const Value: integer);
begin
  if (FSpacing <> Value) then
  begin
    FSpacing := Value;
    Invalidate;
  end;
end;

procedure TAdvCustomRatingGrid.SetVersion(const Value: string);
begin
  //
end;

procedure TAdvCustomRatingGrid.ToggleItem(X, Y: integer);
var
  d: int64;
  i: integer;

begin
  if CategoryType = catRating then
  begin
    Items[X].Value := Y;

    DoValueChanged(X,Y,true);
  end
  else
  begin
    d := 1;

    for i := 0 to Y - 1 do
      d := d * 2;

    Items[X].Value := Items[X].Value xor d;

    DoValueChanged(X,Y,Items[X].Value and d = d);
  end;
end;

procedure TAdvCustomRatingGrid.UpdateSize;
var
  w,h: integer;
begin
  h := GetCategoriesSize + Items.Count * 24;
  w := GetItemsSize + Categories.Count * 24;

  Width := w;
  Height := h;
end;

procedure TAdvCustomRatingGrid.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS + DLGC_WANTCHARS;
end;

function TAdvCustomRatingGrid.XYToItem(X, Y: integer): TPoint;
var
  i,j,h,w,dx,dy,xp,yp,ps: integer;
  r: TRect;
  pt: TPoint;
  doshow: boolean;
begin
  Result := Point(-1,-1);

  h := GetCategoriesSize;
  w := GetItemsSize;

  pt := Point(X,Y);

  dx := (Width - w) div Categories.VisibleCount;

  dy := GetItemHeight;
  ps := GetPictureSize.cx;

  if Assigned(Images) and (Categories[0].OnImageIndex >= 0) and (Categories[0].OffImageIndex >= 0) then
  begin
    ps := Images.Width;
  end;

  yp := h + 2 + Spacing;

  for i := 0 to Items.Count - 1 do
  begin
    xp := w + FCatIndent - (ps div 2);

    if not Items[i].Separator then
    begin

      for j := 0 to Categories.Count - 1 do
      begin
        if Categories[j].Visible then
        begin
          r := Rect(xp, yp, xp + ps, yp + dy);

          doshow := true;

          DoShowItemCategory(i,j,doshow);

          if doshow and PtInRect(r,pt) then
          begin
            Result := Point(i,j);
            Break;
          end;

          xp := xp + dx;
        end;
      end;
    end;
    yp := yp + dy;
  end;
end;

{ TRatingCategory }

procedure TRatingCategory.Assign(Source: TPersistent);
begin
  if (Source is TRatingCategory) then
  begin
    FTitle := (Source as TRatingCategory).Title;
    FImageIndex := (Source as TRatingCategory).ImageIndex;
    FTag := (Source as TRatingCategory).Tag;
    FStyle := (Source as TRatingCategory).Style;
    FColor := (Source as TRatingCategory).Color;
    FVisible := (Source as TRatingCategory).Visible;
    FOnImageIndex := (Source as TRatingCategory).OnImageIndex;
    FOffImageIndex := (Source as TRatingCategory).OffImageIndex;
  end;
end;

constructor TRatingCategory.Create(Collection: TCollection);
begin
  inherited;
  FColor := clBlack;
  FStyle := [];
  FImageIndex := -1;
  FOffImageIndex := -1;
  FOnImageIndex := -1;
  FVisible := true;
end;

procedure TRatingCategory.SetColor(const Value: TColor);
begin
  if (FColor <> Value) then
  begin
    FColor := Value;
    Changed(false);
  end;
end;

procedure TRatingCategory.SetImageIndex(const Value: integer);
begin
  if (FImageIndex <> Value) then
  begin
    FImageIndex := Value;
    Changed(false);
  end;
end;

procedure TRatingCategory.SetOffImageIndex(const Value: integer);
begin
  if (FOffImageIndex <> Value) then
  begin
    FOffImageIndex := Value;
    Changed(false);
  end;
end;

procedure TRatingCategory.SetOnImageIndex(const Value: integer);
begin
  if (FOnImageIndex <> Value) then
  begin
    FOnImageIndex := Value;
    Changed(false);
  end;
end;

procedure TRatingCategory.SetStyle(const Value: TFontStyles);
begin
  if (FStyle <> Value) then
  begin
    FStyle := Value;
    Changed(false);
  end;
end;

procedure TRatingCategory.SetTitle(const Value: string);
begin
  if (FTitle <> Value) then
  begin
    FTitle := Value;
    Changed(false);
  end;
end;

procedure TRatingCategory.SetVisible(const Value: boolean);
begin
  if (FVisible <> Value) then
  begin
    FVisible := Value;
    Changed(false);
  end;
end;

{ TRatingItem }

procedure TRatingItem.Assign(Source: TPersistent);
begin
  if (Source is TRatingItem) then
  begin
    FSeparator := (Source as TRatingItem).Separator;
    FColor := (Source as TRatingItem).Color;
    FTitle := (Source as TRatingItem).Title;
    FTag := (Source as TRatingItem).Tag;
    FStyle := (Source as TRatingItem).Style;
    FValue := (Source as TRatingItem).Value;
  end;
end;

constructor TRatingItem.Create(Collection: TCollection);
begin
  inherited;
  FColor := clBlack;
  FStyle := [];
  FValue := 0;
  FSeparator := false;
  FTag := 0;
end;

procedure TRatingItem.SetColor(const Value: TColor);
begin
  if (FColor <> Value) then
  begin
    FColor := Value;
    Changed(false);
  end;
end;

procedure TRatingItem.SetImageIndex(const Value: integer);
begin
  if (FImageIndex <> Value) then
  begin
    FImageIndex := Value;
    Changed(false);
  end;
end;

procedure TRatingItem.SetSeparator(const Value: boolean);
begin
  if (FSeparator <> Value) then
  begin
    FSeparator := Value;
    Changed(false);
  end;
end;

procedure TRatingItem.SetStyle(const Value: TFontStyles);
begin
  if (FStyle <> Value) then
  begin
    FStyle := Value;
    Changed(false);
  end;
end;

procedure TRatingItem.SetTitle(const Value: string);
begin
  if (FTitle <> Value) then
  begin
    FTitle := Value;
    Changed(false);
  end;
end;

procedure TRatingItem.SetValue(const Value: int64);
begin
  if (FValue <> Value) then
  begin
    FValue := Value;
    Changed(false);
  end;
end;

{ TRatingCategories }

function TRatingCategories.Add: TRatingCategory;
begin
  Result := TRatingCategory(inherited Add);
end;

function TRatingCategories.GetItem(i: Integer): TRatingCategory;
begin
  Result := TRatingCategory(inherited Items[i]);
end;

function TRatingCategories.GetVisibleCount: integer;
var
  i: integer;
begin
  Result := 0;

  for i := 0 to Count - 1 do
    if Items[i].Visible then
      Inc(Result);
end;

function TRatingCategories.Insert(Index: integer): TRatingCategory;
begin
  Result := TRatingCategory(inherited Insert(Index));
end;

procedure TRatingCategories.SetItem(i: Integer; const Value: TRatingCategory);
begin
  inherited Items[i] := Value;
end;

procedure TRatingCategories.Update(Item: TCollectionItem);
begin
  inherited;
  if Assigned(OnChange) then
    OnChange(Self);
end;

{ TRatingItems }

function TRatingItems.Add: TRatingItem;
begin
  Result := TRatingItem(inherited Add);
end;

function TRatingItems.GetItem(i: Integer): TRatingItem;
begin
  Result := TRatingItem(inherited Items[i]);
end;

function TRatingItems.Insert(Index: integer): TRatingItem;
begin
  Result := TRatingItem(inherited Insert(Index));
end;

procedure TRatingItems.SetItem(i: Integer; const Value: TRatingItem);
begin
  inherited Items[i] := Value;
end;

procedure TRatingItems.Update(Item: TCollectionItem);
begin
  inherited;
  if Assigned(OnChange) then
    OnChange(Self);

end;

end.

{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{            copyright (c) 2023                                      }
{            Email : info@tmssoftware.com                            }
{            Web : http://www.tmssoftware.com                        }
{                                                                    }
{ The source code is given as is. The author is not responsible      }
{ for any possible damage done due to the use of this code.          }
{ The complete source code remains property of the author and may    }
{ not be distributed, published, given or sold in any form as such.  }
{ No parts of the source code can be included in any other component }
{ or application without written authorization of the author.        }
{********************************************************************}

unit AdvPDFViewer;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  WinApi.Windows, WinApi.Messages, System.Win.WinRT, System.Classes, System.SysUtils, VCL.Controls,
  Winapi.ActiveX, Winapi.Winrt, AdvGraphicsTypes, System.Types, VCL.ExtCtrls, AdvCustomControl,
  AdvCustomScrollControl, AdvGraphics, AdvTypes, VCL.StdCtrls, System.Generics.Collections;

const
  SWindows_Data_Pdf_PdfDocument = 'Windows.Data.Pdf.PdfDocument';
  SWindows_Data_Pdf_PdfPage = 'Windows.Data.Pdf.PdfPage';
  SWindows_Data_Pdf_PdfPageRenderOptions = 'Windows.Data.Pdf.PdfPageRenderOptions';
  SWindows_Storage_StorageFile = 'Windows.Storage.StorageFile';

type
  Color = record
    A: Byte;
    R: Byte;
    G: Byte;
    B: Byte;
  end;
  PColor = ^Color;

  Pdf_IPdfDocument = interface;
  Pdf_IPdfDocument_Statics = interface;
  Pdf_IPdfPage = interface;
  Pdf_IPdfPageRenderOptions = interface;

  [WinRTClassNameAttribute(SWindows_Data_Pdf_PdfPageRenderOptions)]
  Pdf_IPdfPageRenderOptions = interface(IInspectable)
    ['{3C98056F-B7CF-4C29-9A04-52D90267F425}']
    function get_SourceRect: TRectF; safecall;
    procedure put_SourceRect(value: TRectF); safecall;
    property SourceRect: TRectF read get_SourceRect write put_SourceRect;
    function get_DestinationWidth: Cardinal; safecall;
    procedure put_DestinationWidth(value: Cardinal); safecall;
    property DestinationWidth: Cardinal read get_DestinationWidth write put_DestinationWidth;
    function get_DestinationHeight: Cardinal; safecall;
    procedure put_DestinationHeight(value: Cardinal); safecall;
    property DestinationHeight: Cardinal read get_DestinationHeight write put_DestinationHeight;
    function get_BackgroundColor: Color; safecall;
    procedure put_BackgroundColor(value: Color); safecall;
    property BackgroundColor: Color read get_BackgroundColor write put_BackgroundColor;
    function get_IsIgnoringHighContrast: Boolean; safecall;
    procedure put_IsIgnoringHighContrast(value: Boolean); safecall;
    property IsIgnoringHighContrast: Boolean read get_IsIgnoringHighContrast write put_IsIgnoringHighContrast;
    function get_BitmapEncoderId: TGUID; safecall;
    procedure put_BitmapEncoderId(value: TGUID); safecall;
    property BitmapEncoderId: TGUID read get_BitmapEncoderId write put_BitmapEncoderId;
  end;

  IInputStream = interface(IInspectable)
  ['{905A0FE2-BC53-11DF-8C49-001E4FC686DA}']
  end;

  IOutputStream = interface(IInspectable)
  ['{905A0FE6-BC53-11DF-8C49-001E4FC686DA}']
  end;

  IRandomAccessStream = interface(IInspectable)
  ['{905A0FE1-BC53-11DF-8C49-001E4FC686DA}']
  end;

  IAsyncAction = interface;

  AsyncStatus = (
    Canceled = 2,
    Completed = 1,
    Error = 3,
    Started = 0
  );
  PAsyncStatus = ^AsyncStatus;

  AsyncActionCompletedHandler = interface(IUnknown)
  ['{A4ED5C81-76C9-40BD-8BE6-B1D90FB20AE7}']
    procedure Invoke(asyncInfo: IAsyncAction; asyncStatus: AsyncStatus); safecall;
  end;

  IAsyncAction = interface(IInspectable)
  ['{5A648006-843A-4DA9-865B-9D26E5DFAD7B}']
    procedure put_Completed(handler: AsyncActionCompletedHandler); safecall;
    function get_Completed: AsyncActionCompletedHandler; safecall;
    procedure GetResults; safecall;
    property Completed: AsyncActionCompletedHandler read get_Completed write put_Completed;
  end;

  [WinRTClassNameAttribute(SWindows_Data_Pdf_PdfPage)]
  Pdf_IPdfPage = interface(IInspectable)
  ['{9DB4B0C8-5320-4CFC-AD76-493FDAD0E594}']
    function RenderToStreamAsync(outputStream: IRandomAccessStream): IAsyncAction; overload; safecall;
    function RenderToStreamAsync(outputStream: IRandomAccessStream; options: Pdf_IPdfPageRenderOptions): IAsyncAction; overload; safecall;
    function PreparePageAsync: IAsyncAction; safecall;
    function get_Index: Cardinal; safecall;
    property &Index: Cardinal read get_Index;
    function get_Size: TSizeF; safecall;
    property Size: TSizeF read get_Size;
  end;

  [WinRTClassNameAttribute(SWindows_Data_Pdf_PdfDocument)]
  Pdf_IPdfDocument = interface(IInspectable)
  ['{AC7EBEDD-80FA-4089-846E-81B77FF5A86C}']
    function GetPage(pageIndex: Cardinal): Pdf_IPdfPage; safecall;
    function get_PageCount: Cardinal; safecall;
    function get_IsPasswordProtected: Boolean;
    property PageCount: Cardinal read get_PageCount;
    property IsPasswordProtected: Boolean read get_IsPasswordProtected;
  end;

  AsyncOperationCompletedHandler_1__Pdf_IPdfDocument = interface;

  IAsyncOperation_1__Pdf_IPdfDocument_Base = interface(IInspectable)
  ['{d6b166ec-099a-5ee2-ad2e-f4c88614aabb}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__Pdf_IPdfDocument); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__Pdf_IPdfDocument; safecall;
    function GetResults: Pdf_IPdfDocument; safecall;
    property Completed: AsyncOperationCompletedHandler_1__Pdf_IPdfDocument read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Data.Xml.Dom.IXmlDocument>
  IAsyncOperation_1__Pdf_IPdfDocument = interface(IAsyncOperation_1__Pdf_IPdfDocument_Base)
  ['{CA3E8F7C-BBE4-5C32-B42F-CBA0C469E42C}']
  end;

  AsyncOperationCompletedHandler_1__Pdf_IPdfDocument_Delegate_Base = interface(IUnknown)
  ['{8d4950b3-629d-5d7d-84cc-04c0dcf7942b}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__Pdf_IPdfDocument; asyncStatus: AsyncStatus); safecall;
  end;

  AsyncOperationCompletedHandler_1__Pdf_IPdfDocument = interface(AsyncOperationCompletedHandler_1__Pdf_IPdfDocument_Delegate_Base)
  ['{4D733BCA-331A-59A6-8361-D703C963C6D1}']
  end;

  [WinRTClassNameAttribute(SWindows_Storage_StorageFile)]
  IStorageFile = interface(IInspectable)
  ['{FA3F6186-4214-428C-A64C-14C9AC7315EA}']
  end;

  [WinRTClassNameAttribute(SWindows_Data_Pdf_PdfDocument)]
  Pdf_IPdfDocument_Statics = interface(IInspectable)
  ['{433A0B5F-C007-4788-90F2-08143D922599}']
    function LoadFromFileAsync(&file: IStorageFile): IAsyncOperation_1__Pdf_IPdfDocument; safecall;
    function LoadFromFileWithPasswordAsync(&file: IStoragefile): IAsyncOperation_1__Pdf_IPdfDocument; safecall;
  end;

  TPdfDocument = class(TWinRTGenericImportSI<Pdf_IPdfDocument_Statics, Pdf_IPdfDocument>)
  public
    class function LoadFromFileAsync(&file: IStorageFile): IAsyncOperation_1__Pdf_IPdfDocument; static; inline;
  end;

  TPdfPageRenderOptions = class(TWinRTGenericImportI<Pdf_IPdfPageRenderOptions>)

  end;

  IAsyncInfo = interface(IInspectable)
  ['{00000036-0000-0000-C000-000000000046}']
    function get_Id: Cardinal; safecall;
    function get_Status: AsyncStatus; safecall;
    function get_ErrorCode: HRESULT; safecall;
    procedure Cancel; safecall;
    procedure Close; safecall;
    property ErrorCode: HRESULT read get_ErrorCode;
    property Id: Cardinal read get_Id;
    property Status: AsyncStatus read get_Status;
  end;

  TAsyncActionCompletedHandler = class(TInterfacedObject, AsyncActionCompletedHandler)
  private
    FPageIndex: Integer;
    FCallBack: TProc<IAsyncInfo, Integer>;
    FAsyncInfo: IAsyncInfo;
  public
    procedure Invoke(asyncInfo: IAsyncAction; asyncStatus: AsyncStatus); safecall;
  end;

  AsyncOperationCompletedHandler_1__IStorageFile = interface;

  IAsyncOperation_1__IStorageFile_Base = interface(IInspectable)
  ['{5E52F8CE-ACED-5A42-95B4-F674DD84885E}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IStorageFile); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IStorageFile; safecall;
    function GetResults: IStorageFile; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IStorageFile read get_Completed write put_Completed;
  end;

  IAsyncOperation_1__IStorageFile = interface(IAsyncOperation_1__IStorageFile_Base)
  ['{31C5C3AB-4BF6-51D1-B590-C6EFC00E9FF2}']
  end;

  AsyncOperationCompletedHandler_1__IStorageFile_Delegate_Base = interface(IUnknown)
  ['{E521C894-2C26-5946-9E61-2B5E188D01ED}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IStorageFile; asyncStatus: AsyncStatus); safecall;
  end;

  AsyncOperationCompletedHandler_1__IStorageFile = interface(AsyncOperationCompletedHandler_1__IStorageFile_Delegate_Base)
  ['{1247300D-7973-53D5-889F-5279D9322114}']
  end;

  TAsyncOperationCompletedHandler_1__IStorageFile = class(TInterfacedObject,
        AsyncOperationCompletedHandler_1__IStorageFile_Delegate_Base,
        AsyncOperationCompletedHandler_1__IStorageFile)
  private
    FCallBack: TProc<IAsyncInfo, Integer>;
    FAsyncInfo: IAsyncInfo;
  public
    procedure Invoke(asyncInfo: IAsyncOperation_1__IStorageFile; asyncStatus: AsyncStatus); safecall;
  end;

  TAsyncOperationCompletedHandler_1__Pdf_IPdfDocument = class(TInterfacedObject,
    AsyncOperationCompletedHandler_1__Pdf_IPdfDocument_Delegate_Base,
    AsyncOperationCompletedHandler_1__Pdf_IPdfDocument)
  private
    FCallBack: TProc<IAsyncInfo, Integer>;
    FAsyncInfo: IAsyncInfo;
  public
    procedure Invoke(asyncInfo: IAsyncOperation_1__Pdf_IPdfDocument; asyncStatus: AsyncStatus); safecall;
  end;

  [WinRTClassNameAttribute(SWindows_Storage_StorageFile)]
  IStorageFileStatics = interface(IInspectable)
  ['{5984C710-DAF2-43C8-8BB4-A4D3EACFD03F}']
    function GetFileFromPathAsync(path: HSTRING): IAsyncOperation_1__IStorageFile; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Storage.IStorageFileStatics2
  [WinRTClassNameAttribute(SWindows_Storage_StorageFile)]
  IStorageFileStatics2 = interface(IInspectable)
  ['{5C76A781-212E-4AF9-8F04-740CAE108974}']
  end;

  TStorageFile = class(TWinRTGenericImportS2<IStorageFileStatics, IStorageFileStatics2>)
  public
    // -> IStorageFileStatics
    class function GetFileFromPathAsync(path: HSTRING): IAsyncOperation_1__IStorageFile; static; inline;
  end;


  TAdvPDFViewerPage = class
  private
    FLoading: Boolean;
    FIndex: Integer;
    FBitmap: TAdvBitmap;
    FStream: TMemoryStream;
    FReload: Boolean;
    function GetBitmap: TAdvBitmap;
    function GetStream: TMemoryStream;
  public
    destructor Destroy; override;
    procedure DestroyStream;
    procedure DestroyBitmap;
    function IsLoaded: Boolean;
    function IsLoadedForPainting: Boolean;
    property Bitmap: TAdvBitmap read GetBitmap;
    property Stream: TMemoryStream read GetStream;
    property &Index: Integer read FIndex write FIndex;
  end;

  TAdvPDFViewerPages = class(TObjectList<TAdvPDFViewerPage>);

  TAdvPDFViewerLoadMode = (lmOnDemand, lmPreload);

  TAdvPDFViewerDisplayMode = (dmSinglePage, dmDoublePage, dmContinuousTopToBottom);
  TAdvPDFViewerPageScrollMode = (smSinglePage, smDoublePage, smDynamic);

  TAdvPDFViewerDrawMode = (drmLeft, drmRight, drmCenter);

  TAdvPDFViewerBeforeDrawThumbnailEvent = procedure(Sender: TObject; APageIndex: Integer; ABitmap: TAdvBitmap; ARect: TRectF; AGraphics: TAdvGraphics; var ADefaultDraw: Boolean) of object;
  TAdvPDFViewerAfterDrawThumbnailEvent = procedure(Sender: TObject; APageIndex: Integer; ABitmap: TAdvBitmap; ARect: TRectF; AGraphics: TAdvGraphics) of object;
  TAdvPDFViewerBeforeDrawPageEvent = procedure(Sender: TObject; APageIndex: Integer; ABitmap: TAdvBitmap; ARect: TRectF; AGraphics: TAdvGraphics; var ADefaultDraw: Boolean) of object;
  TAdvPDFViewerAfterDrawPageEvent = procedure(Sender: TObject; APageIndex: Integer; ABitmap: TAdvBitmap; ARect: TRectF; AGraphics: TAdvGraphics) of object;
  TAdvPDFViewerBeforeDrawHeaderEvent = procedure(Sender: TObject; APageIndex: Integer; AHeader: string; ARect: TRectF; AGraphics: TAdvGraphics; var ADefaultDraw: Boolean) of object;
  TAdvPDFViewerBeforeDrawFooterEvent = procedure(Sender: TObject; APageIndex: Integer; AFooter: string; ARect: TRectF; AGraphics: TAdvGraphics; var ADefaultDraw: Boolean) of object;
  TAdvPDFViewerBeforeDrawPageNumberEvent = procedure(Sender: TObject; APageIndex: Integer; APageNumber: string; ARect: TRectF; AGraphics: TAdvGraphics; var ADefaultDraw: Boolean) of object;
  TAdvPDFViewerAfterDrawHeaderEvent = procedure(Sender: TObject; APageIndex: Integer; AHeader: string; ARect: TRectF; AGraphics: TAdvGraphics) of object;
  TAdvPDFViewerAfterDrawFooterEvent = procedure(Sender: TObject; APageIndex: Integer; AFooter: string; ARect: TRectF; AGraphics: TAdvGraphics) of object;
  TAdvPDFViewerAfterDrawPageNumberEvent = procedure(Sender: TObject; APageIndex: Integer; APageNumber: string; ARect: TRectF; AGraphics: TAdvGraphics) of object;
  TAdvPDFViewerPageLoadedEvent = procedure(Sender: TObject; APageIndex: Integer) of object;
  TAdvPDFViewerPageChangedEvent = procedure(Sender: TObject; APageIndex: Integer) of object;
  TAdvPDFViewerThumbnailClickEvent = procedure(Sender: TObject; APageIndex: Integer) of object;

  TAdvPDFViewerPageNumber = (pnNone, pnHeader, pnFooter);

  TAdvPDFViewerOptions = class(TPersistent)
  private
    FFooter: string;
    FHeader: string;
    FHeaderFont: TAdvGraphicsFont;
    FFooterFont: TAdvGraphicsFont;
    FHeaderSize: Single;
    FFooterMargins: TAdvMargins;
    FFooterAlignment: TAdvGraphicsTextAlign;
    FFooterSize: Single;
    FHeaderMargins: TAdvMargins;
    FHeaderAlignment: TAdvGraphicsTextAlign;
    FPageNumberFormat: string;
    FPageNumberAlignment: TAdvGraphicsTextAlign;
    FPageNumber: TAdvPDFViewerPageNumber;
    FPageNumberFont: TAdvGraphicsFont;
    FPageNumberMargins: TAdvMargins;
    FPageNumberSize: Single;
    FOnChange: TNotifyEvent;
    FPageMargins: TAdvMargins;
    FLoadMode: TAdvPDFViewerLoadMode;
    FDisplayMode: TAdvPDFViewerDisplayMode;
    FTouchScrolling: Boolean;
    FThumbnails: Boolean;
    FThumbnailSize: Integer;
    FThumbnailFont: TAdvGraphicsFont;
    FThumbnailSelectedFill: TAdvGraphicsFill;
    FThumbnailSelectedFont: TAdvGraphicsFont;
    FPageScrollMode: TAdvPDFViewerPageScrollMode;
    FDefaultThumbnail: TAdvBitmap;
    FAllowDropFiles: Boolean;
    FWaitCursor: Boolean;
    procedure SetFooterFont(const Value: TAdvGraphicsFont);
    procedure SetHeaderFont(const Value: TAdvGraphicsFont);
    procedure SetFooterAlignment(
      const Value: TAdvGraphicsTextAlign);
    procedure SetFooterMargins(const Value: TAdvMargins);
    procedure SetFooterSize(const Value: Single);
    procedure SetHeaderAlignment(
      const Value: TAdvGraphicsTextAlign);
    procedure SetHeaderMargins(const Value: TAdvMargins);
    procedure SetHeaderSize(const Value: Single);
    function IsFooterSizeStored: Boolean;
    function IsHeaderSizeStored: Boolean;
    procedure SetPageNumber(const Value: TAdvPDFViewerPageNumber);
    procedure SetPageNumberAlignment(const Value: TAdvGraphicsTextAlign);
    procedure SetPageNumberFont(const Value: TAdvGraphicsFont);
    procedure SetPageNumberFormat(const Value: string);
    procedure SetPageNumberMargins(const Value: TAdvMargins);
    function IsPageNumberSizeStored: Boolean;
    procedure SetPageNumberSize(const Value: Single);
    procedure SetPageMargins(const Value: TAdvMargins);
    procedure SetTouchScrolling(const Value: Boolean);
    procedure SetThumbnails(const Value: Boolean);
    procedure SetThumbnailSize(const Value: Integer);
    procedure SetThumbnailFont(const Value: TAdvGraphicsFont);
    procedure SetThumbnailSelectedFill(const Value: TAdvGraphicsFill);
    procedure SetThumbnailSelectedFont(const Value: TAdvGraphicsFont);
    procedure SetPageScrollMode(const Value: TAdvPDFViewerPageScrollMode);
    procedure SetDefaultThumbnail(const Value: TAdvBitmap);
    procedure SetAllowDropFiles(const Value: Boolean);
    procedure SetWaitCursor(const Value: Boolean);
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    procedure DoChanged(Sender: TObject);
    procedure SetDisplayMode(const Value: TAdvPDFViewerDisplayMode);
  protected
    property LoadMode: TAdvPDFViewerLoadMode read FLoadMode write FLoadMode default lmOnDemand;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create; virtual;
    destructor Destroy; override;
  published
    property AllowDropFiles: Boolean read FAllowDropFiles write SetAllowDropFiles default True;
    property WaitCursor: Boolean read FWaitCursor write SetWaitCursor default True;
    property DisplayMode: TAdvPDFViewerDisplayMode read FDisplayMode write SetDisplayMode default dmSinglePage;
    property Header: string read FHeader write FHeader;
    property Footer: string read FFooter write FFooter;
    property HeaderFont: TAdvGraphicsFont read FHeaderFont write SetHeaderFont;
    property FooterFont: TAdvGraphicsFont read FFooterFont write SetFooterFont;
    property HeaderSize: Single read FHeaderSize write SetHeaderSize stored IsHeaderSizeStored nodefault;
    property HeaderMargins: TAdvMargins read FHeaderMargins write SetHeaderMargins;
    property HeaderAlignment: TAdvGraphicsTextAlign read FHeaderAlignment write SetHeaderAlignment default gtaCenter;
    property FooterSize: Single read FFooterSize write SetFooterSize stored IsFooterSizeStored nodefault;
    property FooterMargins: TAdvMargins read FFooterMargins write SetFooterMargins;
    property FooterAlignment: TAdvGraphicsTextAlign read FFooterAlignment write SetFooterAlignment default gtaCenter;
    property PageNumber: TAdvPDFViewerPageNumber read FPageNumber write SetPageNumber default pnHeader;
    property PageNumberMargins: TAdvMargins read FPageNumberMargins write SetPageNumberMargins;
    property PageNumberFormat: string read FPageNumberFormat write SetPageNumberFormat;
    property PageNumberAlignment: TAdvGraphicsTextAlign read FPageNumberAlignment write SetPageNumberAlignment default gtaTrailing;
    property PageNumberFont: TAdvGraphicsFont read FPageNumberFont write SetPageNumberFont;
    property PageNumberSize: Single read FPageNumberSize write SetPageNumberSize stored IsPageNumberSizeStored nodefault;
    property PageMargins: TAdvMargins read FPageMargins write SetPageMargins;
    property TouchScrolling: Boolean read FTouchScrolling write SetTouchScrolling default True;
    property DefaultThumbnail: TAdvBitmap read FDefaultThumbnail write SetDefaultThumbnail;
    property Thumbnails: Boolean read FThumbnails write SetThumbnails default False;
    property ThumbnailSize: Integer read FThumbnailSize write SetThumbnailSize default 150;
    property ThumbnailFont: TAdvGraphicsFont read FThumbnailFont write SetThumbnailFont;
    property ThumbnailSelectedFont: TAdvGraphicsFont read FThumbnailSelectedFont write SetThumbnailSelectedFont;
    property ThumbnailSelectedFill: TAdvGraphicsFill read FThumbnailSelectedFill write SetThumbnailSelectedFill;
    property PageScrollMode: TAdvPDFViewerPageScrollMode read FPageScrollMode write SetPageScrollMode default smDynamic;
  end;

  TAdvCustomPDFViewer = class(TAdvCustomScrollControl)
  private
    FThumbnailSizeDef: Integer;
    FVertScroll: TScrollBar;
    FOptions: TAdvPDFViewerOptions;
    FScrollTimer, FThumbnailTimer, FCursorTimer, FDocLoadTimer: TTimer;
    FLoadingPages: TList<IAsyncinfo>;
    FPages, FThumbnails: TAdvPDFViewerPages;
    FPdfDocument: Pdf_IPdfDocument;
    FFileName: TfileName;
    FPageIndex: Integer;
    FBlockPageUpdate, FBlockThumbnailUpdate, FScrollThumb: Boolean;
    FDocLoad, FFileLoad: IAsyncInfo;
    FAfterDrawPage: TAdvPDFViewerAfterDrawPageEvent;
    FBeforeDrawPage: TAdvPDFViewerBeforeDrawPageEvent;
    FOnBeforeDrawHeader: TAdvPDFViewerBeforeDrawHeaderEvent;
    FOnBeforeDrawPageNumber: TAdvPDFViewerBeforeDrawPageNumberEvent;
    FOnBeforeDrawFooter: TAdvPDFViewerBeforeDrawFooterEvent;
    FOnAfterDrawHeader: TAdvPDFViewerAfterDrawHeaderEvent;
    FOnAfterDrawPageNumber: TAdvPDFViewerAfterDrawPageNumberEvent;
    FOnAfterDrawFooter: TAdvPDFViewerAfterDrawFooterEvent;
    FOnLoaded: TNotifyEvent;
    FOnPageLoaded: TAdvPDFViewerPageLoadedEvent;
    FOnPageChanged: TAdvPDFViewerPageChangedEvent;
    FAfterDrawThumbnail: TAdvPDFViewerAfterDrawThumbnailEvent;
    FBeforeDrawThumbnail: TAdvPDFViewerBeforeDrawThumbnailEvent;
    FOnThumbnailClick: TAdvPDFViewerThumbnailClickEvent;
    procedure SetOptions(const Value: TAdvPDFViewerOptions);
    procedure SetFileName(const Value: TFileName);
    procedure SetPageIndex(const Value: Integer);
    procedure SetPageIndexInternal(const Value: Integer; ForceScroll: Boolean; ForceRepaint: Boolean);
    function Await(s: IInterface; APageIndex: Integer; c: TProc<IAsyncInfo, Integer>): IAsyncInfo;
    function GetPageCount: Integer;
    procedure WMDropFiles(var Message: TMessage); message WM_DROPFILES;
  protected
    property FileName: TFileName read FFileName write SetFileName;
    property PageIndex: Integer read FPageIndex write SetPageIndex default 0;
    property PageCount: Integer read GetPageCount;
    property OnBeforeDrawPage: TAdvPDFViewerBeforeDrawPageEvent read FBeforeDrawPage write FBeforeDrawPage;
    property OnAfterDrawPage: TAdvPDFViewerAfterDrawPageEvent read FAfterDrawPage write FAfterDrawPage;
    property OnBeforeDrawThumbnail: TAdvPDFViewerBeforeDrawThumbnailEvent read FBeforeDrawThumbnail write FBeforeDrawThumbnail;
    property OnAfterDrawThumbnail: TAdvPDFViewerAfterDrawThumbnailEvent read FAfterDrawThumbnail write FAfterDrawThumbnail;
    property OnBeforeDrawHeader: TAdvPDFViewerBeforeDrawHeaderEvent read FOnBeforeDrawHeader write FOnBeforeDrawHeader;
    property OnAfterDrawHeader: TAdvPDFViewerAfterDrawHeaderEvent read FOnAfterDrawHeader write FOnAfterDrawHeader;
    property OnBeforeDrawPageNumber: TAdvPDFViewerBeforeDrawPageNumberEvent read FOnBeforeDrawPageNumber write FOnBeforeDrawPageNumber;
    property OnAfterDrawPageNumber: TAdvPDFViewerAfterDrawPageNumberEvent read FOnAfterDrawPageNumber write FOnAfterDrawPageNumber;
    property OnBeforeDrawFooter: TAdvPDFViewerBeforeDrawFooterEvent read FOnBeforeDrawFooter write FOnBeforeDrawFooter;
    property OnAfterDrawFooter: TAdvPDFViewerAfterDrawFooterEvent read FOnAfterDrawFooter write FOnAfterDrawFooter;
    property OnLoaded: TNotifyEvent read FOnLoaded write FOnLoaded;
    property OnPageLoaded: TAdvPDFViewerPageLoadedEvent read FOnPageLoaded write FOnPageLoaded;
    property OnPageChanged: TAdvPDFViewerPageChangedEvent read FOnPageChanged write FOnPageChanged;
    property OnThumbnailClick: TAdvPDFViewerThumbnailClickEvent read FOnThumbnailClick write FOnThumbnailClick;
    property Options: TAdvPDFViewerOptions read FOptions write SetOptions;

    function GetMainScreenScale: Single;
    function ColumnStretchingActive: Boolean; override;
    function GetTotalContentHeight: Double; override;
    function GetTotalThumbnailHeight: Double; virtual;
    function GetTotalHeight: Double; virtual;
    function GetVerticalContentViewPortSize: Double; override;
    function GetThumbnailRect: TRectF;
    function GetAreaRect: TRectF;
    function GetHeaderRect: TRectF;
    function GetPageNumberRect: TRectF;
    function GetFooterRect: TRectF;
    function GetPageRect: TRectF;
    function GetScrollOffset: Single;

    function XYToThumbnail(X, Y: Single): Integer;

    function GetPage(APageIndex: Integer): TAdvPDFViewerPage;
    function GetThumbnail(APageIndex: Integer): TAdvPDFViewerPage;
    function GetVisibleThumbnailCount: Integer;
    function ProcessTouchScrolling(X, Y: Single): Boolean; override;

    procedure UpdateControlAfterResize; override;
    procedure OptionsChange(Sender: TObject);
    procedure ChangeDPIScale(M, D: Integer); override;
    procedure DoThumbnailClick(APageIndex: Integer); virtual;
    procedure DoBeforeDrawHeader(APageIndex: Integer; AHeader: string; ARect: TRectF; AGraphics: TAdvGraphics; var ADefaultDraw: Boolean); virtual;
    procedure DoBeforeDrawPageNumber(APageIndex: Integer; APageNumber: string; ARect: TRectF; AGraphics: TAdvGraphics; var ADefaultDraw: Boolean); virtual;
    procedure DoBeforeDrawFooter(APageIndex: Integer; AFooter: string; ARect: TRectF; AGraphics: TAdvGraphics; var ADefaultDraw: Boolean); virtual;
    procedure DoAfterDrawHeader(APageIndex: Integer; AHeader: string; ARect: TRectF; AGraphics: TAdvGraphics); virtual;
    procedure DoAfterDrawPageNumber(APageIndex: Integer; APageNumber: string; ARect: TRectF; AGraphics: TAdvGraphics); virtual;
    procedure DoAfterDrawFooter(APageIndex: Integer; AFooter: string; ARect: TRectF; AGraphics: TAdvGraphics); virtual;
    procedure DoPageLoaded(APageIndex: Integer); virtual;
    procedure DoPageChanged(APageIndex: Integer); virtual;
    procedure DoLoaded; virtual;
    procedure AwaitCursor;
    procedure RestoreCursor;
    procedure DoBeforeDrawPage(APageIndex: Integer; ABitmap: TAdvBitmap; ARect: TRectF; AGraphics: TAdvGraphics; var ADefaultDraw: Boolean); virtual;
    procedure DoAfterDrawPage(APageIndex: Integer; ABitmap: TAdvBitmap; ARect: TRectF; AGraphics: TAdvGraphics); virtual;
    procedure DoBeforeDrawThumbnail(APageIndex: Integer; ABitmap: TAdvBitmap; ARect: TRectF; AGraphics: TAdvGraphics; var ADefaultDraw: Boolean); virtual;
    procedure DoAfterDrawThumbnail(APageIndex: Integer; ABitmap: TAdvBitmap; ARect: TRectF; AGraphics: TAdvGraphics); virtual;
    procedure DoScrollTimer(Sender: TObject);
    procedure DoCursorTimer(Sender: TObject);
    procedure DoDocLoadTimer(Sender: TObject);
    procedure DoThumbnailTimer(Sender: TObject);
    procedure HandleKeyDown(var Key: Word; Shift: TShiftState); override;
    procedure HandleMouseDown(Button: TAdvMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure HandleMouseUp(Button: TAdvMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure HandleMouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); override;
    procedure CancelLoadingPages;
    procedure ReloadPages;
    procedure VerticalScrollPositionChanged; override;
    procedure UpdateControlScroll({%H-}AHorizontalPos, {%H-}AVerticalPos, {%H-}ANewHorizontalPos, {%H-}ANewVerticalPos: Double); override;
    procedure UnloadPage(AIndex: Integer);
    procedure LoadPage(AIndex: Integer; ALoadContent: Boolean);
    procedure LoadDocument;
    procedure LoadPages;
    procedure LoadThumbnails;
    procedure LoadActiveThumbnails(APageIndex: Integer);
    procedure LoadThumbnail(AIndex: Integer);
    procedure UnloadPages;
    procedure DrawHeader(AGraphics: TAdvGraphics);
    procedure DrawFooter(AGraphics: TAdvGraphics);
    procedure DrawPageNumber(AGraphics: TAdvGraphics);
    procedure ConfigureScrollBar; virtual;
    procedure VertScrollChange(Sender: TObject);
    procedure HandlePreviousPage;
    procedure HandleNextPage;
    procedure CreateWnd; override;

    procedure DrawThumbnail(AGraphics: TAdvGraphics; ARect: TRectF; APageIndex: Integer); virtual;
    procedure DrawPage(AGraphics: TAdvGraphics; ARect: TRectF; APageIndex: Integer; ADrawMode: TAdvPDFViewerDrawMode); virtual;
    procedure DrawPages(AGraphics: TAdvGraphics); virtual;
    procedure DrawThumbnails(AGraphics: TAdvGraphics); virtual;
    procedure Draw(AGraphics: TAdvGraphics; ARect: TRectF); override;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure NextPage;
    procedure PreviousPage;
    procedure LastPage;
    procedure FirstPage;
    procedure SavePageToImageFile(APageIndex: Integer; AFileName: string);
    procedure SavePageToImageStream(APageIndex: Integer; AStream: TStream);
  end;

  TAdvPDFViewer = class(TAdvCustomPDFViewer)
  public
    property PageCount;
  published
    property OnBeforeDrawThumbnail;
    property OnAfterDrawThumbnail;
    property OnAfterDrawPage;
    property OnBeforeDrawPage;
    property OnBeforeDrawHeader;
    property OnAfterDrawHeader;
    property OnBeforeDrawPageNumber;
    property OnAfterDrawPageNumber;
    property OnBeforeDrawFooter;
    property OnAfterDrawFooter;
    property OnLoaded;
    property OnPageLoaded;
    property OnPageChanged;

    property FileName;
    property PageIndex;
    property Fill;
    property Stroke;
    property Options;
  end;

function CreateRandomAccessStreamOverStream(stream: IStream; options: cardinal; riid: PGUID; out Obj): HRESULT; stdcall;

implementation

uses
  WinApi.ShellAPI, VCL.Forms, VCL.Graphics, Math, System.Threading, System.UITypes, AdvUtils;

const
  Shcore = 'Shcore.dll';
  DefaultThumbnailIcon = 'data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iaXNvLTg4NTktMSI/Pg0KPCE'+
  'tLSBVcGxvYWRlZCB0bzogU1ZHIFJlcG8sIHd3dy5zdmdyZXBvLmNvbSwgR2VuZXJhdG9yOiBTVkcgUmVwbyBNaXhlciBUb29scyAtLT4'+
  'NCjxzdmcgdmVyc2lvbj0iMS4xIiBpZD0iTGF5ZXJfMSIgeG1sbnM9Imh0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnIiB4bWxuczp4bGl'+
  'uaz0iaHR0cDovL3d3dy53My5vcmcvMTk5OS94bGluayIgDQoJIHZpZXdCb3g9IjAgMCA0ODUgNDg1IiB4bWw6c3BhY2U9InByZXNlcnZ'+
  'lIj4NCjxnPg0KCTxnPg0KCQk8cG9seWdvbiBzdHlsZT0iZmlsbDojQUZCNkJCOyIgcG9pbnRzPSIzMzcuNSw3LjUgNDIyLjUsOTcuNSA'+
  'zMzcuNSw5Ny41IAkJIi8+DQoJCTxwb2x5Z29uIHN0eWxlPSJmaWxsOiNGMkYyRjI7IiBwb2ludHM9IjQyMi41LDk3LjUgNDIyLjUsNDc'+
  '3LjUgNjIuNSw0NzcuNSA2Mi41LDcuNSAzMzcuNSw3LjUgMzM3LjUsOTcuNSAJCSIvPg0KCTwvZz4NCgk8Zz4NCgkJPGc+DQoJCQk8cGF'+
  '0aCBkPSJNMzQwLjczMiwwSDU1djQ4NWgzNzVWOTQuNTE4TDM0MC43MzIsMHogTTM0NSwyNi4zNjRMNDA1LjEsOTBIMzQ1VjI2LjM2NHo'+
  'gTTcwLDQ3MFYxNWgyNjB2OTBoODV2MzY1SDcweiIvPg0KCQkJPHJlY3QgeD0iMTAyLjUiIHk9IjE5MSIgd2lkdGg9IjI4MCIgaGVpZ2h'+
  '0PSIxNSIvPg0KCQkJPHJlY3QgeD0iMTAyLjUiIHk9IjE0MSIgd2lkdGg9IjEyMCIgaGVpZ2h0PSIxNSIvPg0KCQkJPHJlY3QgeD0iMTA'+
  'yLjUiIHk9IjI0MSIgd2lkdGg9IjI4MCIgaGVpZ2h0PSIxNSIvPg0KCQkJPHJlY3QgeD0iMTAyLjUiIHk9IjI5MSIgd2lkdGg9IjI4MCI'+
  'gaGVpZ2h0PSIxNSIvPg0KCQkJPHJlY3QgeD0iMTAyLjUiIHk9IjM0MSIgd2lkdGg9IjI4MCIgaGVpZ2h0PSIxNSIvPg0KCQkJPHJlY3Q'+
  'geD0iMTAyLjUiIHk9IjM5MSIgd2lkdGg9IjI4MCIgaGVpZ2h0PSIxNSIvPg0KCQk8L2c+DQoJPC9nPg0KPC9nPg0KPC9zdmc+';

function CreateRandomAccessStreamOverStream; external Shcore name 'CreateRandomAccessStreamOverStream' delayed;

procedure TAdvCustomPDFViewer.Assign(Source: TPersistent);
begin
  if Source is TAdvCustomPDFViewer then
  begin
    FFileName := (Source as TAdvCustomPDFViewer).FileName;
    FPageIndex := (Source as TAdvCustomPDFViewer).PageIndex;
    FOptions.Assign((Source as TAdvCustomPDFViewer).Options);
  end
  else
    inherited;
end;

function TAdvCustomPDFViewer.Await(s: IInterface; APageIndex: Integer; c: TProc<IAsyncInfo, Integer>): IAsyncInfo;
var
  LOut: IAsyncInfo;
  ch: TAsyncActionCompletedHandler;
  a: IAsyncAction;
  ap: IAsyncOperation_1__Pdf_IPdfDocument_Base;
  chp: TAsyncOperationCompletedHandler_1__Pdf_IPdfDocument;
  af: IAsyncOperation_1__IStorageFile_Base;
  chf: TAsyncOperationCompletedHandler_1__IStorageFile;
begin
  if Supports(s, IAsyncInfo, LOut) then
  begin
    Result := LOut;

    if Supports(s, IAsyncAction, a) then
    begin
      ch := TAsyncActionCompletedHandler.Create;
      ch.FPageIndex := APageIndex;
      ch.FAsyncInfo := lOut;
      ch.FCallBack := c;
      a.Completed := ch;
    end;

    if Supports(s, IAsyncOperation_1__Pdf_IPdfDocument_Base, ap) then
    begin
      chp := TAsyncOperationCompletedHandler_1__Pdf_IPdfDocument.Create;
      chp.FAsyncInfo := lOut;
      chp.FCallBack := c;
      ap.Completed := chp;
    end;

    if Supports(s, IAsyncOperation_1__IStorageFile_Base, af) then
    begin
      chf := TAsyncOperationCompletedHandler_1__IStorageFile.Create;
      chf.FAsyncInfo := lOut;
      chf.FCallBack := c;
      af.Completed := chf;
    end;
  end;

  AwaitCursor;
end;

procedure TAdvCustomPDFViewer.AwaitCursor;
begin
  if (FLoadingPages.Count = 0) and Options.WaitCursor then
  begin
    Screen.Cursor := crHourGlass;
    FCursorTimer.Enabled := True;
  end;
end;

{ TPdfDocument }

class function TPdfDocument.LoadFromFileAsync(
  &file: IStorageFile): IAsyncOperation_1__Pdf_IPdfDocument;
begin
  Result := Statics.LoadFromFileAsync(&file);
end;

{ TAdvCustomPDFViewer }

procedure TAdvCustomPDFViewer.CancelLoadingPages;
var
  I: Integer;
begin
  if Options.LoadMode = lmPreload then
    Exit;

  for I := FLoadingPages.Count - 1 downto 0 do
    FLoadingPages[I].Cancel;
end;

procedure TAdvCustomPDFViewer.ChangeDPIScale(M, D: Integer);
begin
  inherited;
  FOptions.ThumbnailSelectedFont.Height := TAdvUtils.MulDivInt(FOptions.ThumbnailSelectedFont.Height, M, D);
  FOptions.ThumbnailFont.Height := TAdvUtils.MulDivInt(FOptions.ThumbnailFont.Height, M, D);
  FOptions.HeaderFont.Height := TAdvUtils.MulDivInt(FOptions.HeaderFont.Height, M, D);
  FOptions.FooterFont.Height := TAdvUtils.MulDivInt(FOptions.FooterFont.Height, M, D);
  FOptions.PageNumberFont.Height := TAdvUtils.MulDivInt(FOptions.PageNumberFont.Height, M, D);

  FOptions.HeaderSize := TAdvUtils.MulDivSingle(FOptions.HeaderSize, M, D);
  FOptions.FooterSize := TAdvUtils.MulDivSingle(FOptions.FooterSize, M, D);
  FOptions.PageNumberSize := TAdvUtils.MulDivSingle(FOptions.PageNumberSize, M, D);

  FOptions.HeaderMargins.Left := TAdvUtils.MulDivSingle(FOptions.HeaderMargins.Left, M, D);
  FOptions.HeaderMargins.Top := TAdvUtils.MulDivSingle(FOptions.HeaderMargins.Top, M, D);
  FOptions.HeaderMargins.Right := TAdvUtils.MulDivSingle(FOptions.HeaderMargins.Right, M, D);
  FOptions.HeaderMargins.Bottom := TAdvUtils.MulDivSingle(FOptions.HeaderMargins.Bottom, M, D);

  FOptions.FooterMargins.Left := TAdvUtils.MulDivSingle(FOptions.FooterMargins.Left, M, D);
  FOptions.FooterMargins.Top := TAdvUtils.MulDivSingle(FOptions.FooterMargins.Top, M, D);
  FOptions.FooterMargins.Right := TAdvUtils.MulDivSingle(FOptions.FooterMargins.Right, M, D);
  FOptions.FooterMargins.Bottom := TAdvUtils.MulDivSingle(FOptions.FooterMargins.Bottom, M, D);

  FOptions.PageNumberMargins.Left := TAdvUtils.MulDivSingle(FOptions.PageNumberMargins.Left, M, D);
  FOptions.PageNumberMargins.Top := TAdvUtils.MulDivSingle(FOptions.PageNumberMargins.Top, M, D);
  FOptions.PageNumberMargins.Right := TAdvUtils.MulDivSingle(FOptions.PageNumberMargins.Right, M, D);
  FOptions.PageNumberMargins.Bottom := TAdvUtils.MulDivSingle(FOptions.PageNumberMargins.Bottom, M, D);
end;

procedure TAdvCustomPDFViewer.ReloadPages;
var
  I: Integer;
begin
  for I := 0 to FPages.Count - 1 do
    FPages[I].FReload := True;
end;

procedure TAdvCustomPDFViewer.SetOptions(const Value: TAdvPDFViewerOptions);
begin
  FOptions.Assign(Value);
end;

function TAdvCustomPDFViewer.ColumnStretchingActive: Boolean;
begin
  Result := True;
end;

procedure TAdvCustomPDFViewer.ConfigureScrollBar;
var
  r: TRectF;
  h, ch: Single;
  w: Integer;
begin
  FVertScroll.Kind := sbVertical;
  r := GetThumbnailRect;
  w := FVertScroll.Width;
  h := GetTotalThumbnailHeight;
  ch := Height;

  if Options.Thumbnails and (h > ch) then
  begin
    FVertScroll.Parent := Self;
    FVertScroll.Visible := True;
  end
  else
  begin
    FVertScroll.Parent := nil;
    FVertScroll.Visible := False;
  end;

  FVertScroll.PageSize := Round(Max(0, Min(h, ch)));
  FVertScroll.Max := Round(Max(FVertScroll.PageSize, h));
  FVertScroll.Position := Min(FVertScroll.Position, FVertScroll.Max);
  if Height > 0 then
    FVertScroll.LargeChange := Height;

  if Options.ThumbnailSize > 0 then
    FVertScroll.SmallChange := Options.ThumbnailSize;

  FVertScroll.SetBounds(Round(r.Right), Round(r.Top), w, Round(r.Bottom - r.Top));
end;

constructor TAdvCustomPDFViewer.Create(AOwner: TComponent);
begin
  inherited;

  FOptions := TAdvPDFViewerOptions.Create;
  FThumbnailSizeDef := Options.ThumbnailSize;

  if IsDesignTime then
  begin
    FOptions.ThumbnailSelectedFont.Color := gcWhite;
    FOptions.Header := 'TMS PDF Header';
    FOptions.Footer := 'TMS PDF Footer';
    FOptions.PageNumberFormat := '%d / %d';
    Fill.Color := gcWhitesmoke;
  end;

  FOptions.OnChange := OptionsChange;
  TouchScrolling := True;
  FPageIndex := 0;
  FLoadingPages := TList<IAsyncInfo>.Create;

  FCursorTimer := TTimer.Create(Self);
  FCursorTimer.OnTimer := DoCursorTimer;
  FCursorTimer.Enabled := False;
  FCursorTimer.Interval := 1;

  FScrollTimer := TTimer.Create(Self);
  FScrollTimer.OnTimer := DoScrollTimer;
  FScrollTimer.Enabled := False;
  FScrollTimer.Interval := 1;

  FThumbnailTimer := TTimer.Create(Self);
  FThumbnailTimer.OnTimer := DoThumbnailTimer;
  FThumbnailTimer.Enabled := False;
  FThumbnailTimer.Interval := 1;

  FDocLoadTimer := TTimer.Create(Self);
  FDocLoadTimer.OnTimer := DoDocLoadTimer;
  FDocLoadTimer.Enabled := False;
  FDocLoadTimer.Interval := 1;

  FPages := TAdvPDFViewerPages.Create;
  FThumbnails := TAdvPDFViewerPages.Create;
  FVertScroll := TScrollBar.Create(Self);
  FVertScroll.OnChange := VertScrollChange;
  ConfigureScrollBar;

  Width := 400;
  Height := 400;
end;

procedure TAdvCustomPDFViewer.CreateWnd;
begin
  inherited;
  if HandleAllocated then
    DragAcceptFiles(Handle, Options.AllowDropFiles);
end;

destructor TAdvCustomPDFViewer.Destroy;
begin
  FreeAndNil(FScrollTimer);
  FreeAndNil(FDocLoadTimer);
  FreeAndNil(FThumbnailTimer);
  FreeAndNil(FPages);
  FreeAndNil(FThumbnails);
  FreeAndNil(FLoadingPages);
  FreeAndNil(FOptions);
  inherited;
end;

procedure TAdvCustomPDFViewer.DoAfterDrawThumbnail(APageIndex: Integer; ABitmap: TAdvBitmap; ARect: TRectF; AGraphics: TAdvGraphics);
begin
  if Assigned(OnAfterDrawThumbnail) then
    OnAfterDrawThumbnail(Self, APageIndex, ABitmap, ARect, AGraphics);
end;

procedure TAdvCustomPDFViewer.DoBeforeDrawThumbnail(APageIndex: Integer; ABitmap: TAdvBitmap; ARect: TRectF; AGraphics: TAdvGraphics; var ADefaultDraw: Boolean);
begin
  if Assigned(OnBeforeDrawThumbnail) then
    OnBeforeDrawThumbnail(Self, APageIndex, ABitmap, ARect, AGraphics, ADefaultDraw);
end;

procedure TAdvCustomPDFViewer.DoCursorTimer(Sender: TObject);
begin
  if FLoadingPages.Count = 0 then
  begin
    FCursorTimer.Enabled := False;
    RestoreCursor;
  end;
end;

procedure TAdvCustomPDFViewer.DoAfterDrawPage(APageIndex: Integer; ABitmap: TAdvBitmap; ARect: TRectF; AGraphics: TAdvGraphics);
begin
  if Assigned(OnAfterDrawPage) then
    OnAfterDrawPage(Self, APageIndex, ABitmap, ARect, AGraphics);
end;

procedure TAdvCustomPDFViewer.DoBeforeDrawPage(APageIndex: Integer; ABitmap: TAdvBitmap; ARect: TRectF; AGraphics: TAdvGraphics; var ADefaultDraw: Boolean);
begin
  if Assigned(OnBeforeDrawPage) then
    OnBeforeDrawPage(Self, APageIndex, ABitmap, ARect, AGraphics, ADefaultDraw);
end;

procedure TAdvCustomPDFViewer.DoScrollTimer(Sender: TObject);
var
  pg, pgi: Integer;
begin
  if (FLoadingPages.Count = 0) then
  begin
    FScrollTimer.Enabled := False;
    if (Options.DisplayMode = dmContinuousTopToBottom) then
      pg := Round((GetVerticalScrollPosition / GetTotalHeight))
    else
      pg := PageIndex;

    pgi := PageIndex;
    SetPageIndexInternal(pg, False, False);
    if pgi <> pg then
      DoPageChanged(pg);

    Invalidate;
  end;
end;

procedure TAdvCustomPDFViewer.DoThumbnailClick(APageIndex: Integer);
begin
  if Assigned(OnThumbnailClick) then
    OnThumbnailClick(Self, APageIndex);
end;

procedure TAdvCustomPDFViewer.DoThumbnailTimer(Sender: TObject);
var
  pg: Integer;
begin
  if FLoadingPages.Count = 0 then
  begin
    FThumbnailTimer.Enabled := False;
    pg := 0;
    if Options.ThumbnailSize > 0 then
      pg := Round(FVertScroll.Position / Options.ThumbnailSize);

    LoadActiveThumbnails(pg + GetVisibleThumbnailCount div 2);

    Invalidate;
  end;
end;

procedure TAdvCustomPDFViewer.Draw(AGraphics: TAdvGraphics; ARect: TRectF);
begin
  inherited;
  DrawPages(AGraphics);
  DrawHeader(AGraphics);
  DrawFooter(AGraphics);
  DrawPageNumber(AGraphics);
  DrawThumbnails(AGraphics);
end;

procedure TAdvCustomPDFViewer.DoAfterDrawFooter(
  APageIndex: Integer; AFooter: UnicodeString; ARect: TRectF;
  AGraphics: TAdvGraphics);
begin
  if Assigned(OnAfterDrawFooter) then
    OnAfterDrawFooter(Self, APageIndex, AFooter, ARect, AGraphics);
end;

procedure TAdvCustomPDFViewer.DoAfterDrawHeader(
  APageIndex: Integer; AHeader: UnicodeString; ARect: TRectF;
  AGraphics: TAdvGraphics);
begin
  if Assigned(OnAfterDrawHeader) then
    OnAfterDrawHeader(Self, APageIndex, AHeader, ARect, AGraphics);
end;

procedure TAdvCustomPDFViewer.DoAfterDrawPageNumber(
  APageIndex: Integer; APageNumber: UnicodeString; ARect: TRectF;
  AGraphics: TAdvGraphics);
begin
  if Assigned(OnAfterDrawPageNumber) then
    OnAfterDrawPageNumber(Self, APageIndex, APageNumber, ARect, AGraphics);
end;

procedure TAdvCustomPDFViewer.DoBeforeDrawFooter(
  APageIndex: Integer; AFooter: UnicodeString; ARect: TRectF;
  AGraphics: TAdvGraphics; var ADefaultDraw: Boolean);
begin
  if Assigned(OnBeforeDrawFooter) then
    OnBeforeDrawFooter(Self, APageIndex, AFooter, ARect, AGraphics, ADefaultDraw);
end;

procedure TAdvCustomPDFViewer.DoBeforeDrawHeader(
  APageIndex: Integer; AHeader: UnicodeString; ARect: TRectF;
  AGraphics: TAdvGraphics; var ADefaultDraw: Boolean);
begin
  if Assigned(OnBeforeDrawHeader) then
    OnBeforeDrawHeader(Self, APageIndex, AHeader, ARect, AGraphics, ADefaultDraw);
end;

procedure TAdvCustomPDFViewer.DoBeforeDrawPageNumber(
  APageIndex: Integer; APageNumber: UnicodeString; ARect: TRectF;
  AGraphics: TAdvGraphics; var ADefaultDraw: Boolean);
begin
  if Assigned(OnBeforeDrawPageNumber) then
    OnBeforeDrawPageNumber(Self, APageIndex, APageNumber, ARect, AGraphics, ADefaultDraw);
end;

procedure TAdvCustomPDFViewer.DoDocLoadTimer(Sender: TObject);
var
  s: IAsyncOperation_1__IStorageFile;
  ws: TWindowsString;
begin
  if FLoadingPages.Count = 0 then
  begin
    FDocLoadTimer.Enabled := False;

    FPages.Clear;
    FThumbnails.Clear;
    FLoadingPages.Clear;

    ConfigureScrollBar;

    if Assigned(FFileLoad) then
      FFileLoad.Cancel;

    if Assigned(FDocLoad) then
      FDocLoad.Cancel;

    ws := TWindowsString.Create(FFileName);
    s := TStorageFile.GetFileFromPathAsync(ws);
    if Assigned(s) then
    begin
      FFileLoad := Await(s, -1,
      procedure(i1: IAsyncInfo; p1: Integer)
      var
        sf: IStorageFile;
        ai: IAsyncOperation_1__Pdf_IPdfDocument;
      begin
        FFileLoad := nil;
        if not (i1.Status = AsyncStatus.Completed) then
          Exit;

        sf := s.GetResults;
        if not Assigned(sf) then
          Exit;

        ai := TPdfDocument.LoadFromFileAsync(sf);

        FDocLoad := Await(ai, -1,
        procedure(i2: IAsyncInfo; p2: Integer)
        begin
          FDocLoad := nil;
          if not (i2.Status = AsyncStatus.Completed) then
            Exit;

          FPdfDocument := ai.GetResults;
          ConfigureScrollBar;
          UpdateControlScrollBars;
          ConfigureScrollBar;
          SetPageIndexInternal(0, True, True);
          if Options.LoadMode = lmPreload then
            LoadPages;
          DoLoaded;
        end);
      end
      );
    end;
  end;
end;

procedure TAdvCustomPDFViewer.DoLoaded;
begin
  if Assigned(OnLoaded) then
    OnLoaded(Self);
end;

procedure TAdvCustomPDFViewer.DoPageChanged(APageIndex: Integer);
begin
  if Assigned(OnPageChanged) then
    OnPageChanged(Self, APageIndex);
end;

procedure TAdvCustomPDFViewer.DoPageLoaded(APageIndex: Integer);
begin
  if Assigned(OnPageLoaded) then
    OnPageLoaded(Self, APageIndex);
end;

procedure TAdvCustomPDFViewer.DrawFooter(AGraphics: TAdvGraphics);
var
  r: TRectF;
  df: Boolean;
begin
  if Options.Footer = '' then
    Exit;

  AGraphics.Font.Assign(Options.FooterFont);
  r := GetFooterRect;
  df := True;
  DoBeforeDrawFooter(PageIndex, Options.Footer, r, AGraphics, df);
  if df then
  begin
    InflateRectEx(r, -2, -2);
    AGraphics.DrawText(r, Options.Footer, True, Options.FooterAlignment);
    DoAfterDrawFooter(PageIndex, Options.Footer, r, AGraphics);
  end;
end;

function TAdvCustomPDFViewer.GetHeaderRect: TRectF;
var
  r: TRectF;
begin
  Result := RectF(0, 0, 0, 0);
  if Options.Header <> '' then
  begin
    r := GetAreaRect;
    Result := RectF(r.Left + Options.HeaderMargins.Left, r.Top + Options.HeaderMargins.Top, r.Right - Options.HeaderMargins.Right, r.Top + Options.HeaderMargins.Top + Options.HeaderSize);
  end;
end;

function TAdvCustomPDFViewer.GetMainScreenScale: Single;
begin
  Result := Screen.PrimaryMonitor.PixelsPerInch / 96;
end;

function TAdvCustomPDFViewer.GetPageNumberRect: TRectF;
var
  r: TRectF;
begin
  Result := RectF(0, 0, 0, 0);
  r := GetAreaRect;
  case Options.PageNumber of
    pnHeader: Result := RectF(r.Left + Options.PageNumberMargins.Left, r.Top + Options.PageNumberMargins.Top, r.Right - Options.PageNumberMargins.Right, r.Top + Options.PageNumberMargins.Top + Options.PageNumberSize);
    pnFooter: Result := RectF(r.Left + Options.PageNumberMargins.Left, r.Bottom - Options.PageNumberSize - Options.PageNumberMargins.Bottom, r.Right - Options.PageNumberMargins.Right, r.Bottom - Options.PageNumberMargins.Bottom);
  end;
end;

function TAdvCustomPDFViewer.GetPageRect: TRectF;
var
  hr, fr, pr: TRectF;
begin
  hr := GetHeaderRect;
  fr := GetFooterRect;
  pr := GetPageNumberRect;

  Result := GetAreaRect;

  if (Options.Header <> '') and (Options.PageNumber = pnHeader) then
    Result.Top := Max(pr.Bottom, hr.Bottom)
  else if Options.PageNumber = pnHeader then
    Result.Top := pr.Bottom
  else if Options.Header <> '' then
    Result.Top := hr.Bottom;

  if (Options.Footer <> '') and (Options.PageNumber = pnFooter) then
    Result.Bottom := Min(pr.Top, fr.Top)
  else if Options.PageNumber = pnFooter then
    Result.Bottom := pr.Top
  else if Options.Footer <> '' then
    Result.Bottom := fr.Top;

  Result.Left := Result.Left + Options.PageMargins.Left;
  Result.Top := Result.Top + Options.PageMargins.Top;
  Result.Bottom := Result.Bottom - Options.PageMargins.Bottom;
  Result.Right := Result.Right - Options.PageMargins.Right;
end;

function TAdvCustomPDFViewer.GetScrollOffset: Single;
var
  pr, hr: TRectF;
begin
  pr := GetPageNumberRect;
  hr := GetHeaderRect;

  Result := 0;
  if (Options.Header <> '') and (Options.PageNumber = pnHeader) then
    Result := Max(pr.Bottom, hr.Bottom)
  else if Options.PageNumber = pnHeader then
    Result := pr.Bottom
  else if Options.Header <> '' then
    Result := hr.Bottom;
end;

function TAdvCustomPDFViewer.GetAreaRect: TRectF;
begin
  Result := GetContentRect;
  if Options.Thumbnails then
    Result.Left := Result.Left + Options.ThumbnailSize;
end;

function TAdvCustomPDFViewer.GetFooterRect: TRectF;
var
  r: TRectF;
begin
  Result := RectF(0, 0, 0, 0);
  if Options.Footer <> '' then
  begin
    r := GetAreaRect;
    Result := RectF(r.Left + Options.FooterMargins.Left, r.Bottom - Options.FooterSize - Options.FooterMargins.Bottom, r.Right - Options.FooterMargins.Right, r.Bottom - Options.FooterMargins.Bottom);
  end;
end;

procedure TAdvCustomPDFViewer.DrawHeader(AGraphics: TAdvGraphics);
var
  r: TRectF;
  df: Boolean;
begin
  if Options.Header = '' then
    Exit;

  AGraphics.Font.Assign(Options.HeaderFont);
  r := GetHeaderRect;
  df := True;
  DoBeforeDrawHeader(PageIndex, Options.Header, r, AGraphics, df);
  if df then
  begin
    InflateRectEx(r, -2, -2);
    AGraphics.DrawText(r, Options.Header, True, Options.HeaderAlignment);
    DoAfterDrawHeader(PageIndex, Options.Header, r, AGraphics);
  end;
end;

procedure TAdvCustomPDFViewer.DrawPageNumber(AGraphics: TAdvGraphics);
var
  r: TRectF;
  df: Boolean;
  s: string;
begin
  if (Options.PageNumber = pnNone) or not Assigned(FPdfDocument) then
    Exit;

  AGraphics.Font.Assign(Options.PageNumberFont);
  r := GetPageNumberRect;

  df := True;
  s := Format(Options.PageNumberFormat, [PageIndex + 1, PageCount]);
  DoBeforeDrawPageNumber(PageIndex, s, r, AGraphics, df);
  if df then
  begin
    InflateRectEx(r, -2, -2);
    AGraphics.DrawText(r, s, True, Options.PageNumberAlignment);
    DoAfterDrawPageNumber(PageIndex, s, r, AGraphics);
  end;
end;

procedure TAdvCustomPDFViewer.DrawPage(AGraphics: TAdvGraphics; ARect: TRectF;
  APageIndex: Integer; ADrawMode: TAdvPDFViewerDrawMode);
var
  b: TAdvBitmap;
  p: TAdvPDFViewerPage;
  w, h: Single;
  df: Boolean;
  r: TRectF;
begin
  p := GetPage(APageIndex);
  if Assigned(p) then
  begin
    if p.IsLoadedForPainting then
    begin
      b := p.Bitmap;

      case ADrawMode of
        drmLeft:
        begin
          TAdvGraphics.GetAspectSize(w, h, b.Width, b.Height, ARect.Width, ARect.Height, True, True);
          r := RectF(ARect.Left, ARect.Top, ARect.Left + w, ARect.Bottom);
        end;
        drmRight:
        begin
          TAdvGraphics.GetAspectSize(w, h, b.Width, b.Height, ARect.Width, ARect.Height, True, True);
          r := RectF(ARect.Right - w, ARect.Top, ARect.Right, ARect.Bottom);
        end;
        drmCenter: r := ARect;
      end;

      df := True;
      DoBeforeDrawPage(APageIndex, p.Bitmap, r, AGraphics, df);
      if df then
      begin
        AGraphics.DrawBitmap(r, p.Bitmap, True, True);
        DoAfterDrawPage(APageIndex, p.Bitmap, r, AGraphics);
      end;
    end;
  end;
end;

procedure TAdvCustomPDFViewer.DrawPages(AGraphics: TAdvGraphics);
var
  dm: TAdvPDFViewerDisplayMode;
  r, dr, rl, rr: TRectF;
  I: Integer;
  st: TAdvGraphicsSaveState;
begin
  if PageCount = 0 then
    Exit;

  r := GetPageRect;
  st := AGraphics.SaveState;
  try
    AGraphics.ClipRect(r);

    dm := Options.DisplayMode;
    if (dm <> dmSinglePage) and (PageCount = 1) then
      dm := dmSinglePage;

    case dm of
      dmSinglePage:
      begin
        DrawPage(AGraphics, r, PageIndex, drmCenter);
      end;
      dmDoublePage:
      begin
        rl := RectF(r.Left, r.Top, r.Left + r.Width / 2, r.Bottom);
        rr := RectF(r.Left + r.Width / 2, r.Top, r.Right, r.Bottom);

        DrawPage(AGraphics, rl, PageIndex, drmRight);
        DrawPage(AGraphics, rr, PageIndex + 1, drmLeft);
      end;
      dmContinuousTopToBottom:
      begin
        for I := PageIndex - 2 to PageIndex + 2 do
        begin
          dr := RectF(r.Left, r.Top - GetVerticalScrollPosition + (I * r.Height), r.Right, r.Bottom -
            GetVerticalScrollPosition + (I * r.Height));
          DrawPage(AGraphics, dr, I, drmCenter);
        end;
      end;
    end;
  finally
    AGraphics.RestoreState(st);
  end;
end;

procedure TAdvCustomPDFViewer.DrawThumbnail(AGraphics: TAdvGraphics;
  ARect: TRectF; APageIndex: Integer);
var
  th: TAdvPDFViewerPage;
  r, tr, br: TRectf;
  df: Boolean;
  b: TAdvBitmap;
begin
  r := ARect;
  InflateRectEx(r, -5, -5);

  th := GetThumbnail(APageIndex);
  if Assigned(th) and th.IsLoadedForPainting then
    b := th.Bitmap
  else
    b := Options.DefaultThumbnail;

  df := True;
  DoBeforeDrawThumbnail(APageIndex, b, r, AGraphics, df);
  if df then
  begin
    if APageIndex = PageIndex then
    begin
      AGraphics.Fill.Assign(Options.ThumbnailSelectedFill);
      AGraphics.Stroke.Assign(Options.ThumbnailSelectedFill);
      AGraphics.DrawRectangle(r);
    end;

    if Assigned(b) then
    begin
      br := RectF(r.Left, r.Top, r.Right, r.Bottom - 20);
      InflateRectEx(br, -2, -2);
      AGraphics.DrawBitmap(br, b, True, True);
    end;

    tr := RectF(r.Left, r.Bottom - 20, r.Right, r.Bottom);
    if APageIndex = PageIndex then
      AGraphics.Font.Assign(Options.ThumbnailSelectedFont)
    else
      AGraphics.Font.Assign(Options.ThumbnailFont);
    AGraphics.DrawText(tr, (APageIndex + 1).ToString, False, gtaCenter, gtaCenter);

    DoAfterDrawThumbnail(APageIndex, b, r, AGraphics);
  end;
end;

procedure TAdvCustomPDFViewer.DrawThumbnails(AGraphics: TAdvGraphics);
var
  I: Integer;
  r, rt: TRectF;
  w, v: Single;
  st: TAdvGraphicsSaveState;
begin
  if Options.Thumbnails and (Options.ThumbnailSize > 0) then
  begin
    r := GetThumbnailRect;
    st := AGraphics.SaveState;
    try
      AGraphics.ClipRect(r);
      v := Min(FVertScroll.Max - FVertScroll.PageSize, Max(0, Round(FVertScroll.Position)));
      for I := 0 to PageCount - 1 do
      begin
        w := Options.ThumbnailSize;
        rt := RectF(r.Left, r.Top + (w * I) - v, r.Right, r.Top + (w * (I + 1)) - v);
        if IntersectRectEx(rt, r) then
          DrawThumbnail(AGraphics, rt, I);
      end;
    finally
      AGraphics.RestoreState(st);
    end;
  end;
end;

procedure TAdvCustomPDFViewer.FirstPage;
begin
  PageIndex := 0;
end;

function TAdvCustomPDFViewer.GetPage(APageIndex: Integer): TAdvPDFViewerPage;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FPages.Count - 1 do
  begin
    if FPages[I].Index = APageIndex then
    begin
      Result := FPages[I];
      Break;
    end;
  end;
end;

function TAdvCustomPDFViewer.GetPageCount: Integer;
begin
  Result := 0;
  if Assigned(FPdfDocument) then
    Result := FPdfDocument.PageCount;
end;

function TAdvCustomPDFViewer.GetThumbnail(
  APageIndex: Integer): TAdvPDFViewerPage;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FThumbnails.Count - 1 do
  begin
    if FThumbnails[I].Index = APageIndex then
    begin
      Result := FThumbnails[I];
      Break;
    end;
  end;
end;

function TAdvCustomPDFViewer.GetThumbnailRect: TRectF;
var
  r: TRectF;
  w: Integer;
begin
  Result := RectF(0, 0, 0, 0);
  if Options.Thumbnails then
  begin
    r := GetContentRect;
    w := 0;
    if GetTotalThumbnailHeight > Height then
      w := FVertScroll.Width;
    Result := RectF(r.Left + 1, r.Top + 1, r.Left + Options.ThumbnailSize - w, r.Bottom - 1);
  end;
end;

function TAdvCustomPDFViewer.GetTotalContentHeight: Double;
begin
  Result := inherited GetTotalContentHeight;
  if Options.DisplayMode = dmContinuousTopToBottom then
    Result := (PageCount * GetTotalHeight) + (Height - GetTotalHeight);
end;

function TAdvCustomPDFViewer.GetTotalHeight: Double;
var
  r: TRectF;
begin
  r := GetPageRect;
  Result := r.Height;
end;

function TAdvCustomPDFViewer.GetTotalThumbnailHeight: Double;
var
  ts: Integer;
begin
  ts := Options.ThumbnailSize;
  Result := (PageCount * ts);
end;

function TAdvCustomPDFViewer.GetVerticalContentViewPortSize: Double;
begin
  Result := GetTotalHeight;
end;

function TAdvCustomPDFViewer.GetVisibleThumbnailCount: Integer;
begin
  Result := 0;
  if Options.Thumbnails and (Options.ThumbnailSize > 0) then
    Result := Round(Height / Options.ThumbnailSize) + 1;
end;

procedure TAdvCustomPDFViewer.HandleKeyDown(var Key: Word; Shift: TShiftState);
var
  pg: Integer;
begin
  inherited;
  pg := PageIndex;
  case Key of
    KEY_PRIOR, KEY_LEFT, KEY_UP:
    begin
      if Options.DisplayMode = dmContinuousTopToBottom then
      begin
        if Key = KEY_PRIOR then
          Scroll(0, GetVerticalScrollPosition - GetVerticalContentViewPortSize)
        else
          Scroll(0, GetVerticalScrollPosition - GetVerticalContentViewPortSize / 5)
      end
      else
        HandlePreviousPage;
    end;
    KEY_NEXT, KEY_RIGHT, KEY_DOWN:
    begin
      if Options.DisplayMode = dmContinuousTopToBottom then
      begin
        if Key = KEY_NEXT then
          Scroll(0, GetVerticalScrollPosition + GetVerticalContentViewPortSize)
        else
          Scroll(0, GetVerticalScrollPosition + GetVerticalContentViewPortSize / 5)
      end
      else
        HandleNextPage;
    end;
    KEY_HOME: FirstPage;
    KEY_END: LastPage;
  end;

  if (PageIndex <> pg) and (Key in [KEY_LEFT, KEY_UP, KEY_RIGHT, KEY_DOWN, KEY_PRIOR, KEY_NEXT, KEY_HOME, KEY_END]) then
    DoPageChanged(PageIndex);
end;

procedure TAdvCustomPDFViewer.HandleMouseDown(Button: TAdvMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  inherited;
  if CanFocus then
    SetFocus;
end;

procedure TAdvCustomPDFViewer.HandleMouseUp(Button: TAdvMouseButton;
  Shift: TShiftState; X, Y: Single);
var
  th, pgi: Integer;
begin
  inherited;
  th := XYToThumbnail(X, Y);
  if (th >= 0) and (th <= PageCount - 1) then
  begin
    FScrollThumb := True;
    pgi := PageIndex;
    SetPageIndexInternal(th, True, True);
    DoThumbnailClick(th);
    if pgi <> th then
      DoPageChanged(th);
    FScrollThumb := False;
  end;
end;

procedure TAdvCustomPDFViewer.HandleMouseWheel(Shift: TShiftState;
  WheelDelta: Integer; var Handled: Boolean);
begin
  inherited;

  if PtInRectEx(GetThumbnailRect, GetClientMousePos) then
  begin
    if WheelDelta < 0 then
      FVertScroll.Position := FVertScroll.Position + FVertScroll.PageSize
    else
      FVertScroll.Position := FVertScroll.Position - FVertScroll.PageSize
  end
  else
  begin
    if Options.DisplayMode = dmContinuousTopToBottom then
      Scroll(0, GetVerticalScrollPosition - WheelDelta)
    else
    begin
      if WheelDelta > 0 then
        HandlePreviousPage
      else
        HandleNextPage;
    end;
  end;

  Handled := True;
end;

procedure TAdvCustomPDFViewer.HandleNextPage;
begin
  case Options.PageScrollMode of
    smSinglePage: PageIndex := PageIndex + 1;
    smDoublePage: PageIndex := PageIndex + 2;
    smDynamic:
    begin
      if Options.DisplayMode = dmDoublePage then
      begin
        if PageIndex = 0 then
          PageIndex := PageIndex + 1
        else
          PageIndex := PageIndex + 2;
      end
      else
        PageIndex := PageIndex + 1;
    end;
  end;
end;

procedure TAdvCustomPDFViewer.HandlePreviousPage;
begin
  case Options.PageScrollMode of
    smSinglePage: PageIndex := PageIndex - 1;
    smDoublePage: PageIndex := PageIndex - 2;
    smDynamic:
    begin
      if Options.DisplayMode = dmDoublePage then
      begin
        if (PageIndex = PageCount - 1) and Odd(PageCount) then
          PageIndex := PageIndex - 1
        else
          PageIndex := PageIndex - 2;
      end
      else
        PageIndex := PageIndex - 1;
    end;
  end;
end;

procedure TAdvCustomPDFViewer.LastPage;
begin
  PageIndex := PageCount;
end;

procedure TAdvCustomPDFViewer.LoadActiveThumbnails(APageIndex: Integer);
var
  vst, I: Integer;
begin
  vst := GetVisibleThumbnailCount;

  if APageIndex + vst div 2 > PageCount then
    APageIndex := PageCount - vst div 2
  else if APageIndex - vst div 2 < 0 then
    APageIndex := vst div 2;

  for I := APageIndex - vst div 2 to APageIndex + vst div 2 do
    LoadThumbnail(I);
end;

procedure TAdvCustomPDFViewer.LoadDocument;
begin
  CancelLoadingPages;
  FDocLoadTimer.Enabled := True;
end;

procedure TAdvCustomPDFViewer.LoadPage(AIndex: Integer; ALoadContent: Boolean);
var
  page: Pdf_IPdfPage;
  sta: IStream;
  g: TGUID;
  w, h: Single;
  o: Pdf_IPdfPageRenderOptions;
  r: TRectF;
  ra: IRandomAccessStream;
  pg: TAdvPDFViewerPage;
  rao: IAsyncAction;
  sc: Single;
begin
  if not Assigned(FPdfDocument) or (AIndex < 0) or (AIndex > PageCount - 1) then
    Exit;

  page := FPdfDocument.GetPage(AIndex);
  if Assigned(page) then
  begin
    pg := GetPage(AIndex);
    if not Assigned(pg) then
    begin
      pg := TAdvPDFViewerPage.Create;
      FPages.Add(pg);
    end;

    pg.&Index := AIndex;

    if not ALoadContent or pg.IsLoaded or pg.FLoading then
      Exit;

    pg.FLoading := True;

    sta := TStreamAdapter.Create(pg.Stream);

    g := TGUID.Create('{905A0FE1-BC53-11DF-8C49-001E4FC686DA}');

    if not CreateRandomAccessStreamOverStream(sta, 0, @g, ra) = S_OK then
    begin
      FreeAndNil(pg);
      Exit;
    end;

    o := TPdfPageRenderOptions.Create;

    r := GetPageRect;
    TAdvGraphics.GetAspectSize(w, h, page.Size.Width, page.Size.Height, r.Width, r.Height, True, True);
    sc := GetMainScreenScale;

    if w > h then
      o.DestinationWidth := Round(w / sc)
    else
      o.DestinationHeight := Round(h / sc);

    rao := page.RenderToStreamAsync(ra, o);

    if Assigned(rao) then
    begin
      FLoadingPages.Add(Await(rao, AIndex,
      procedure(i: IAsyncInfo; p: Integer)
      begin
        if IsDestroying then
          Exit;

        pg := GetPage(p);
        if Assigned(pg) then
        begin
          pg.FLoading := False;
          if (i.Status = AsyncStatus.Completed) then
          begin
            pg.Stream.Position := 0;
            pg.Bitmap.LoadFromStream(pg.Stream);
            pg.DestroyStream;
            pg.FReload := False;
            DoPageLoaded(AIndex);
          end;
        end;

        FLoadingPages.Remove(i);
        LoadThumbnails;
        Invalidate;
      end));
    end
    else
      FreeAndNil(pg);
  end;
end;

procedure TAdvCustomPDFViewer.LoadPages;
var
  I: Integer;
  l: Boolean;
begin
  if not Assigned(FPDFDocument) then
    Exit;

  for I := 0 to PageCount - 1 do
  begin
    l := True;

    case Options.LoadMode of
      lmOnDemand:
      begin
        case Options.DisplayMode of
          dmSinglePage: l := (I = PageIndex);
          dmDoublePage: l := (I = PageIndex) or (I = PageIndex + 1);
          dmContinuousTopToBottom: l := (I >= PageIndex - 2) and (I <= PageIndex + 2);
        end;
      end;
    end;
    LoadPage(I, l);
  end;
end;

procedure TAdvCustomPDFViewer.LoadThumbnail(AIndex: Integer);
var
  page: Pdf_IPdfPage;
  sta: IStream;
  g: TGUID;
  o: Pdf_IPdfPageRenderOptions;
  ra: IRandomAccessStream;
  th: TAdvPDFViewerPage;
  rao: IAsyncAction;
  sc: Single;
begin
  if not Assigned(FPdfDocument) or (AIndex < 0) or (AIndex > PageCount - 1) then
    Exit;

  page := FPdfDocument.GetPage(AIndex);
  if Assigned(page) then
  begin
    th := GetThumbnail(AIndex);
    if not Assigned(th) then
    begin
      th := TAdvPDFViewerPage.Create;
      FThumbnails.Add(th);
    end;

    th.&Index := AIndex;

    if th.IsLoaded or th.FLoading then
      Exit;

    th.FLoading := True;

    sta := TStreamAdapter.Create(th.Stream);

    g := TGUID.Create('{905A0FE1-BC53-11DF-8C49-001E4FC686DA}');

    if not CreateRandomAccessStreamOverStream(sta, 0, @g, ra) = S_OK then
    begin
      FreeAndNil(th);
      Exit;
    end;

    o := TPdfPageRenderOptions.Create;
    sc := GetMainScreenScale;
    o.DestinationWidth := Round(Options.ThumbnailSize / sc);
    rao := page.RenderToStreamAsync(ra, o);

    if Assigned(rao) then
    begin
      FLoadingPages.Add(Await(rao, AIndex,
      procedure(i: IAsyncInfo; p: Integer)
      begin
        if IsDestroying then
          Exit;

        th := GetThumbnail(p);
        if Assigned(th) then
        begin
          th.FLoading := False;
          if (i.Status = AsyncStatus.Completed) then
          begin
            th.Stream.Position := 0;
            th.Bitmap.LoadFromStream(th.Stream);
            th.DestroyStream;
          end;
        end;

        FLoadingPages.Remove(i);

        Invalidate;
      end));
    end
    else
      FreeAndNil(th);
  end;
end;

procedure TAdvCustomPDFViewer.LoadThumbnails;
begin
  if not Options.Thumbnails then
    Exit;

  LoadActiveThumbnails(PageIndex);
end;

procedure TAdvCustomPDFViewer.NextPage;
begin
  PageIndex := PageIndex + 1;
end;

procedure TAdvCustomPDFViewer.OptionsChange(Sender: TObject);
begin
  if HandleAllocated then
    DragAcceptFiles(Handle, Options.AllowDropFiles);

  TouchScrolling := Options.TouchScrolling;
  UpdateControl;
  SetPageIndexInternal(PageIndex, True, True);
  if FThumbnailSizeDef <> Options.ThumbnailSize then
    FThumbnails.Clear;

  FThumbnailSizeDef := Options.ThumbnailSize;
  LoadThumbnails;
  ConfigureScrollBar;
end;

procedure TAdvCustomPDFViewer.PreviousPage;
begin
  PageIndex := PageIndex - 1;
end;

function TAdvCustomPDFViewer.ProcessTouchScrolling(X, Y: Single): Boolean;
begin
  Result := inherited ProcessTouchScrolling(X, Y) and not PtInRectEx(GetThumbnailRect, PointF(X, Y));
end;

procedure TAdvCustomPDFViewer.RestoreCursor;
begin
  if FLoadingPages.Count = 0 then
    Screen.Cursor := crDefault;
end;

procedure TAdvCustomPDFViewer.SavePageToImageFile(APageIndex: Integer;
  AFileName: string);
var
  p: TAdvPDFViewerPage;
begin
  p := GetPage(APageIndex);
  if Assigned(p) then
    p.Bitmap.SaveToFile(AFileName);
end;

procedure TAdvCustomPDFViewer.SavePageToImageStream(APageIndex: Integer;
  AStream: TStream);
var
  p: TAdvPDFViewerPage;
begin
  p := GetPage(APageIndex);
  if Assigned(p) then
    p.Bitmap.SaveToStream(AStream);
end;

procedure TAdvCustomPDFViewer.SetFileName(const Value: TFileName);
begin
  if FFileName <> Value then
  begin
    FFileName := Value;
    LoadDocument;
  end;
end;

procedure TAdvCustomPDFViewer.SetPageIndex(const Value: Integer);
begin
  SetPageIndexInternal(Value, True, True);
end;

procedure TAdvCustomPDFViewer.SetPageIndexInternal(const Value: Integer;
  ForceScroll: Boolean; ForceRepaint: Boolean);
var
  th: Integer;
begin
  CancelLoadingPages;

  FPageIndex := Max(0, Min(Value, PageCount - 1));
  if Options.LoadMode = lmOnDemand then
  begin
    LoadPage(FPageIndex, True);
    case Options.DisplayMode of
      dmDoublePage: LoadPage(FPageIndex + 1, True);
      dmContinuousTopToBottom:
      begin
        LoadPage(FPageIndex - 2, True);
        LoadPage(FPageIndex - 1, True);
        LoadPage(FPageIndex + 1, True);
        LoadPage(FPageIndex + 2, True);
      end;
    end;
  end;

  if ForceScroll then
  begin
    case Options.DisplayMode of
      dmContinuousTopToBottom:
      begin
        FBlockPageUpdate := True;
        Scroll(0, FPageIndex * GetTotalHeight);
        FBlockPageUpdate := False;
      end;
    end;
  end;

  if FVertScroll.Visible and not FScrollThumb then
  begin
    th := Options.ThumbnailSize;
    FBlockThumbnailUpdate := True;
    FVertScroll.Position := Min(th * FPageIndex - Height div 2 + Options.ThumbnailSize div 2, FVertScroll.Max);
    FBlockThumbnailUpdate := False;
  end;

  if ForceRepaint then
    Invalidate;
end;

procedure TAdvCustomPDFViewer.UnloadPage(AIndex: Integer);
var
  p: TAdvPDFViewerPage;
begin
  p := GetPage(AIndex);
  if Assigned(p) then
  begin
    p.DestroyStream;
    p.DestroyBitmap;
  end;
end;

procedure TAdvCustomPDFViewer.UnloadPages;
var
  I: Integer;
begin
  if not Assigned(FPDFDocument) or (Options.LoadMode = lmOnDemand) then
    Exit;

  for I := 0 to PageCount - 1 do
    UnloadPage(I);
end;

procedure TAdvCustomPDFViewer.UpdateControlAfterResize;
var
  pg: Integer;
begin
  pg := PageIndex;
  ReloadPages;
  inherited;
  ConfigureScrollBar;
  CancelLoadingPages;
  SetPageIndexInternal(pg, True, False);
  FScrollTimer.Enabled := True;
  FThumbnailTimer.Enabled := True;
  Invalidate;
end;

procedure TAdvCustomPDFViewer.UpdateControlScroll(AHorizontalPos, AVerticalPos,
  ANewHorizontalPos, ANewVerticalPos: Double);
begin
  inherited;
  VerticalScrollPositionChanged;
end;

procedure TAdvCustomPDFViewer.VerticalScrollPositionChanged;
var
  pg, pgi: Integer;
begin
  inherited;
  if FBlockPageUpdate then
    Exit;

  CancelLoadingPages;

  if Options.DisplayMode = dmContinuousTopToBottom then
  begin
    if (FLoadingPages.Count = 0) or (Options.LoadMode = lmPreload) then
    begin
      pg := Round((GetVerticalScrollPosition / GetTotalHeight));
      pgi := PageIndex;
      SetPageIndexInternal(pg, False, False);
      if pgi <> pg then
        DoPageChanged(pg);
    end
    else
      FScrollTimer.Enabled := True;

    Invalidate;
  end;
end;

procedure TAdvCustomPDFViewer.VertScrollChange(Sender: TObject);
var
  pg: Integer;
begin
  if Options.Thumbnails and not FBlockThumbnailUpdate then
  begin
    CancelLoadingPages;

    if FLoadingPages.Count = 0 then
    begin
      pg := 0;
      if Options.ThumbnailSize > 0 then
        pg := Round(FVertScroll.Position / Options.ThumbnailSize);

      LoadActiveThumbnails(pg + GetVisibleThumbnailCount div 2);
    end
    else
      FThumbnailTimer.Enabled := True;

    Invalidate;
  end;
end;

procedure TAdvCustomPDFViewer.WMDropFiles(var Message: TMessage);
var
  fc, Len: Integer;
  fn: array[0..255] of Char;
  str: string;
begin
  if not Options.AllowDropFiles then
    Exit;

  fc := DragQueryFile(Message.wParam, UINT(-1), nil, 0);
  if fc > 0 then
  begin
    Len := DragQueryFile(Message.wParam, 0, fn, 255);
    if Len > 0 then
    begin
      str := StrPas(fn);
      if UpperCase(ExtractFileExt(str)) = '.PDF' then
        FileName := fn;
    end;
  end;
end;

function TAdvCustomPDFViewer.XYToThumbnail(X, Y: Single): Integer;
var
  w: Integer;
  I: Integer;
  v: Integer;
  r, rt: TRectF;
begin
  Result := -1;
  if Options.Thumbnails and (Options.ThumbnailSize > 0) then
  begin
    r := GetThumbnailRect;
    v := Min(FVertScroll.Max - FVertScroll.PageSize, Max(0, Round(FVertScroll.Position)));
    for I := 0 to PageCount - 1 do
    begin
      w := Options.ThumbnailSize;
      rt := RectF(r.Left, r.Top + (w * I) - v, r.Right, r.Top + (w * (I + 1)) - v);
      if PtInRect(rt, PointF(X, Y)) then
      begin
        Result := I;
        Break;
      end;
    end;
  end;
end;

{ TAdvPDFViewerPage }

destructor TAdvPDFViewerPage.Destroy;
begin
  DestroyStream;
  DestroyBitmap;
  inherited;
end;

procedure TAdvPDFViewerPage.DestroyBitmap;
begin
  FreeAndNil(FBitmap);
end;

procedure TAdvPDFViewerPage.DestroyStream;
begin
  FreeAndNil(FStream);
end;

function TAdvPDFViewerPage.GetBitmap: TAdvBitmap;
begin
  if not Assigned(FBitmap) then
    FBitmap := TAdvBitmap.Create;

  Result := FBitmap;
end;

function TAdvPDFViewerPage.GetStream: TMemoryStream;
begin
  if not Assigned(FStream) then
    FStream := TMemoryStream.Create;

  Result := FStream;
end;

function TAdvPDFViewerPage.IsLoaded: Boolean;
begin
  Result := IsLoadedForPainting and not FReload;
end;

function TAdvPDFViewerPage.IsLoadedForPainting: Boolean;
begin
  Result := Assigned(FBitmap) and not IsBitmapEmpty(FBitmap);
end;

{ TAdvPDFViewerOptions }

procedure TAdvPDFViewerOptions.Assign(Source: TPersistent);
begin
  if Source is TAdvPDFViewerOptions then
  begin
    FDisplayMode := (Source as TAdvPDFViewerOptions).DisplayMode;
    FLoadMode := (Source as TAdvPDFViewerOptions).LoadMode;
    FFooter := (Source as TAdvPDFViewerOptions).Footer;
    FHeader := (Source as TAdvPDFViewerOptions).Header;
    FHeaderFont.Assign((Source as TAdvPDFViewerOptions).HeaderFont);
    FFooterFont.Assign((Source as TAdvPDFViewerOptions).FooterFont);
    FHeaderMargins.Assign((Source as TAdvPDFViewerOptions).HeaderMargins);
    FFooterMargins.Assign((Source as TAdvPDFViewerOptions).FooterMargins);
    FFooterSize := (Source as TAdvPDFViewerOptions).FooterSize;
    FHeaderSize := (Source as TAdvPDFViewerOptions).HeaderSize;
    FFooterAlignment := (Source as TAdvPDFViewerOptions).FooterAlignment;
    FHeaderAlignment := (Source as TAdvPDFViewerOptions).HeaderAlignment;
    FPageNumberFormat := (Source as TAdvPDFViewerOptions).PageNumberFormat;
    FPageNumber := (Source as TAdvPDFViewerOptions).PageNumber;
    FPageNumberFont.Assign((Source as TAdvPDFViewerOptions).PageNumberFont);
    FPageNumberAlignment := (Source as TAdvPDFViewerOptions).PageNumberAlignment;
    FPageNumberSize := (Source as TAdvPDFViewerOptions).PageNumberSize;
    FPageNumberMargins.Assign((Source as TAdvPDFViewerOptions).PageNumberMargins);
    FPageMargins.Assign((Source as TAdvPDFViewerOptions).PageMargins);
    FTouchScrolling := (Source as TAdvPDFViewerOptions).TouchScrolling;
    FThumbnailSize := (Source as TAdvPDFViewerOptions).ThumbnailSize;
    FThumbnails := (Source as TAdvPDFViewerOptions).Thumbnails;
    FThumbnailFont.Assign((Source as TAdvPDFViewerOptions).ThumbnailFont);
    FThumbnailSelectedFont.Assign((Source as TAdvPDFViewerOptions).ThumbnailSelectedFont);
    FThumbnailSelectedFill.Assign((Source as TAdvPDFViewerOptions).ThumbnailSelectedFill);
    FPageScrollMode := (Source as TAdvPDFViewerOptions).PageScrollMode;
    FDefaultThumbnail.Assign((Source as TAdvPDFViewerOptions).DefaultThumbnail);
    FAllowDropFiles := (Source as TAdvPDFViewerOptions).AllowDropFiles;
    FWaitCursor := (Source as TAdvPDFViewerOptions).WaitCursor;
  end;
end;

constructor TAdvPDFViewerOptions.Create;
var
  r: TRectF;
begin
  FTouchScrolling := True;
  FAllowDropFiles := True;
  FWaitCursor := True;
  FPageScrollMode := smDynamic;
  r := RectF(5, 5, 5, 5);
  FThumbnailSelectedFill := TAdvGraphicsFill.Create(gfkSolid, gcSteelblue);
  FThumbnailFont := TAdvGraphicsFont.Create;
  FThumbnailFont.OnChanged := DoChanged;
  FThumbnailSelectedFont := TAdvGraphicsFont.Create;
  FThumbnailSelectedFont.OnChanged := DoChanged;
  FHeaderFont := TAdvGraphicsFont.Create;
  FHeaderFont.OnChanged := DoChanged;
  FFooterFont := TAdvGraphicsFont.Create;
  FFooterFont.OnChanged := DoChanged;
  FHeaderSize := 30;
  FFooterSize := 30;
  FPageNumberSize := 30;
  FThumbnailSize := 150;
  FPageMargins := TAdvMargins.Create(r);
  FPageMargins.OnChange := DoChanged;
  FHeaderMargins := TAdvMargins.Create(r);
  FHeaderMargins.OnChange := DoChanged;
  FFooterMargins := TAdvMargins.Create(r);
  FFooterMargins.OnChange := DoChanged;
  FFooterAlignment := gtaCenter;
  FHeaderAlignment := gtaCenter;
  r := RectF(10, 5, 10, 5);
  FPageNumberMargins := TAdvMargins.Create(r);
  FPageNumberMargins.OnChange := DoChanged;
  FPageNumber := pnHeader;
  FPageNumberFont := TAdvGraphicsFont.Create;
  FPageNumberFont.OnChanged := DoChanged;
  FPageNumberAlignment := gtaTrailing;

  FDefaultThumbnail := TAdvBitmap.CreateFromBase64(DefaultThumbnailIcon);
  FDefaultThumbnail.OnChange := DoChanged;
end;

destructor TAdvPDFViewerOptions.Destroy;
begin
  FDefaultThumbnail.Free;
  FThumbnailSelectedFill.Free;
  FThumbnailSelectedFont.Free;
  FThumbnailFont.Free;
  FPageNumberFont.Free;
  FFooterFont.Free;
  FHeaderFont.Free;
  FFooterMargins.Free;
  FHeaderMargins.Free;
  FPageNumberMargins.Free;
  FPageMargins.Free;
  inherited;
end;

procedure TAdvPDFViewerOptions.DoChanged(Sender: TObject);
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

function TAdvPDFViewerOptions.IsFooterSizeStored: Boolean;
begin
  Result := FooterSize <> 30;
end;

function TAdvPDFViewerOptions.IsHeaderSizeStored: Boolean;
begin
  Result := HeaderSize <> 30;
end;

function TAdvPDFViewerOptions.IsPageNumberSizeStored: Boolean;
begin
  Result := PageNumberSize <> 30;
end;

procedure TAdvPDFViewerOptions.SetFooterAlignment(
  const Value: TAdvGraphicsTextAlign);
begin
  if FFooterAlignment <> Value then
  begin
    FFooterAlignment := Value;
    DoChanged(Self);
  end;
end;

procedure TAdvPDFViewerOptions.SetFooterFont(
  const Value: TAdvGraphicsFont);
begin
  FFooterFont.Assign(Value);
end;

procedure TAdvPDFViewerOptions.SetFooterMargins(const Value: TAdvMargins);
begin
  FFooterMargins.Assign(Value);
end;

procedure TAdvPDFViewerOptions.SetFooterSize(const Value: Single);
begin
  if FFooterSize <> Value then
  begin
    FFooterSize := Value;
    DoChanged(Self);
  end;
end;

procedure TAdvPDFViewerOptions.SetHeaderAlignment(
  const Value: TAdvGraphicsTextAlign);
begin
  if FHeaderAlignment <> Value then
  begin
    FHeaderAlignment := Value;
    DoChanged(Self);
  end;
end;

procedure TAdvPDFViewerOptions.SetHeaderFont(
  const Value: TAdvGraphicsFont);
begin
  FHeaderFont.Assign(Value);
end;

procedure TAdvPDFViewerOptions.SetHeaderMargins(const Value: TAdvMargins);
begin
  FHeaderMargins.Assign(Value);
end;

procedure TAdvPDFViewerOptions.SetHeaderSize(const Value: Single);
begin
  if FHeaderSize <> Value then
  begin
    FHeaderSize := Value;
    DoChanged(Self);
  end;
end;

procedure TAdvPDFViewerOptions.SetPageMargins(const Value: TAdvMargins);
begin
  FPageMargins.Assign(Value);
end;

procedure TAdvPDFViewerOptions.SetPageNumber(
  const Value: TAdvPDFViewerPageNumber);
begin
  if FPageNumber <> Value then
  begin
    FPageNumber := Value;
    DoChanged(Self);
  end;
end;

procedure TAdvPDFViewerOptions.SetPageNumberAlignment(
  const Value: TAdvGraphicsTextAlign);
begin
  if FPageNumberAlignment <> Value then
  begin
    FPageNumberAlignment := Value;
    DoChanged(Self);
  end;
end;

procedure TAdvPDFViewerOptions.SetPageNumberFont(
  const Value: TAdvGraphicsFont);
begin
  FPageNumberFont.Assign(Value);
end;

procedure TAdvPDFViewerOptions.SetPageNumberFormat(const Value: string);
begin
  if FPageNumberFormat <> Value then
  begin
    FPageNumberFormat := Value;
    DoChanged(Self);
  end;
end;

procedure TAdvPDFViewerOptions.SetPageNumberMargins(const Value: TAdvMargins);
begin
  FPageNumberMargins.Assign(Value);
end;

procedure TAdvPDFViewerOptions.SetPageNumberSize(const Value: Single);
begin
  if FPageNumberSize <> Value then
  begin
    FPageNumberSize := Value;
    DoChanged(Self);
  end;
end;

procedure TAdvPDFViewerOptions.SetPageScrollMode(
  const Value: TAdvPDFViewerPageScrollMode);
begin
  if FPageScrollMode <> Value then
  begin
    FPageScrollMode := Value;
    DoChanged(Self);
  end;
end;

procedure TAdvPDFViewerOptions.SetThumbnailFont(const Value: TAdvGraphicsFont);
begin
  FThumbnailFont.Assign(Value);
end;

procedure TAdvPDFViewerOptions.SetThumbnails(const Value: Boolean);
begin
  if FThumbnails <> Value then
  begin
    FThumbnails := Value;
    DoChanged(Self);
  end;
end;

procedure TAdvPDFViewerOptions.SetThumbnailSelectedFill(
  const Value: TAdvGraphicsFill);
begin
  FThumbnailSelectedFill.Assign(Value);
end;

procedure TAdvPDFViewerOptions.SetThumbnailSelectedFont(
  const Value: TAdvGraphicsFont);
begin
  FThumbnailSelectedFont.Assign(Value);
end;

procedure TAdvPDFViewerOptions.SetThumbnailSize(const Value: Integer);
begin
  if FThumbnailSize <> Value then
  begin
    FThumbnailSize := Value;
    DoChanged(Self);
  end;
end;

procedure TAdvPDFViewerOptions.SetTouchScrolling(const Value: Boolean);
begin
  if FTouchScrolling <> Value then
  begin
    FTouchScrolling := Value;
    DoChanged(Self);
  end;
end;

procedure TAdvPDFViewerOptions.SetWaitCursor(const Value: Boolean);
begin
  if FWaitCursor <> Value then
  begin
    FWaitCursor := Value;
    DoChanged(Self);
  end;
end;

procedure TAdvPDFViewerOptions.SetAllowDropFiles(const Value: Boolean);
begin
  if FAllowDropFiles <> Value then
  begin
    FAllowDropFiles := Value;
    DoChanged(Self);
  end;
end;

procedure TAdvPDFViewerOptions.SetDefaultThumbnail(const Value: TAdvBitmap);
begin
  FDefaultThumbnail.Assign(Value);
end;

procedure TAdvPDFViewerOptions.SetDisplayMode(
  const Value: TAdvPDFViewerDisplayMode);
begin
  if FDisplayMode <> Value then
  begin
    FDisplayMode := Value;
    DoChanged(Self);
  end;
end;

{ TAsyncOperationCompletedHandler_1__IStorageFile }

procedure TAsyncOperationCompletedHandler_1__IStorageFile.Invoke(asyncInfo: IAsyncOperation_1__IStorageFile; asyncStatus: AsyncStatus);
begin
  if Assigned(FCallBack) then
    FCallBack(FAsyncInfo, -1);
end;

{TAsyncOperationCompletedHandler_1__Pdf_IPdfDocument }

procedure TAsyncOperationCompletedHandler_1__Pdf_IPdfDocument.Invoke(asyncInfo: IAsyncOperation_1__Pdf_IPdfDocument; asyncStatus: AsyncStatus);
begin
  if Assigned(FCallBack) then
    FCallBack(FAsyncInfo, -1);
end;

{ TAsyncActionCompletedHandler }

procedure TAsyncActionCompletedHandler.Invoke(asyncInfo: IAsyncAction;
  asyncStatus: AsyncStatus);
begin
  if Assigned(FCallBack) then
    FCallBack(FAsyncInfo, FPageIndex);
end;

{ TStorageFile }

class function TStorageFile.GetFileFromPathAsync(path: HSTRING): IAsyncOperation_1__IStorageFile;
begin
  Result := Statics.GetFileFromPathAsync(path);
end;

end.

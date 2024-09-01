{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{            copyright (c) 2023                                      }
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

unit FMX.TMSFNCEdgeWebBrowser;

{$I FMX.TMSFNCDefines.inc}

interface

uses
  Classes, FMX.TMSFNCTypes, FMX.TMSFNCWebBrowser;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  //v1.0.0.0 : First release

type
  {$IFNDEF LCLLIB}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TTMSFNCEdgeWebBrowser = class(TTMSFNCCustomWebBrowser)
  public
    procedure StartDocumentReadyStateThread; override;
    procedure Navigate; overload; override;
    procedure Navigate(const AURL: string); overload; override;
    procedure ExecuteJavaScript(AScript: String; ACompleteEvent: TTMSFNCWebBrowserJavaScriptCompleteEvent = nil; AImmediate: Boolean = False); override;
    function ExecuteJavaScriptSync(AScript: string): string; override;
    procedure LoadHTML(AHTML: String); override;
    procedure LoadFile(AFile: String); override;
    procedure Initialize; override;
    procedure DeInitialize; override;
    procedure GoForward; override;
    procedure GoBack; override;
    procedure Reload; override;
    procedure StopLoading; override;
    procedure ShowDebugConsole; override;
    procedure AddBridge(ABridgeName: string; ABridgeObject: TObject); override;
    procedure RemoveBridge(ABridgeName: string); override;
    procedure CaptureScreenShot; override;
    function GetBridgeCommunicationLayer(ABridgeName: string): string; override;
    function NativeEnvironment: Pointer; override;
    function NativeBrowser: Pointer; override;
    function IsFMXBrowser: Boolean; override;
    function CanGoBack: Boolean; override;
    function CanGoForward: Boolean; override;
    {$IFDEF MSWINDOWS}
    function GetWebBrowserInstance: IInterface; override;
    {$ENDIF}
    procedure OpenTaskManager; override;
    procedure GetCookies(AURI: string = ''); override;
    procedure AddCookie(ACookie: TTMSFNCWebBrowserCookie); override;
    procedure DeleteAllCookies; override;
    procedure DeleteCookie(AName: string; ADomain: string; APath: string); override;
    procedure ShowPrintUI; override;
    procedure Print(APrintSettings: TTMSFNCWebBrowserPrintSettings); overload; override;
    procedure Print; overload; override;
    procedure PrintToPDFStream(APrintSettings: TTMSFNCWebBrowserPrintSettings); overload; override;
    procedure PrintToPDFStream; overload; override;
    procedure PrintToPDF(AFileName: string; APrintSettings: TTMSFNCWebBrowserPrintSettings); overload; override;
    procedure PrintToPDF(AFileName: string); overload; override;
    procedure NavigateWithData(AURI: string; AMethod: string; ABody: string; AHeaders: TStrings = nil); overload; override;
    procedure NavigateWithData(AURI: string; AMethod: string; ABodyStream: TStream; AHeaders: TStrings = nil); overload; override;
    function InitialPrintSettings: TTMSFNCWebBrowserPrintSettings; override;
    property OnCloseForm;
    property CacheFolder;
    property CacheFolderName;
    property AutoClearCache;
    procedure ClearCache; override;
    property UserAgent;
  published
    property OnInitialized;
    property URL;
    property OnBeforeNavigate;
    property OnNavigateComplete;
    property OnHardwareButtonClicked;
    property OnCaptureScreenShot;
    property OnDocumentComplete;
    property Version;
    property DesigntimeEnabled;
    property Settings;
    property OnGetContextMenu;
    property OnGetCookies;
    property OnGetPrintPDFStream;
    property OnPrintedToPDF;
    property OnPrinted;
    property OnCustomContextMenuItemSelected;
    property OnGetPopupMenuForContextMenu;
  end;

implementation

{ TTMSFNCEdgeWebBrowser }

procedure TTMSFNCEdgeWebBrowser.AddBridge(ABridgeName: string; ABridgeObject: TObject);
begin
  inherited AddBridge(ABridgeName, ABridgeObject);
end;

procedure TTMSFNCEdgeWebBrowser.AddCookie(ACookie: TTMSFNCWebBrowserCookie);
begin
  inherited AddCookie(ACookie);
end;

function TTMSFNCEdgeWebBrowser.CanGoBack: Boolean;
begin
  Result := inherited CanGoBack;
end;

function TTMSFNCEdgeWebBrowser.CanGoForward: Boolean;
begin
  Result := inherited CanGoForward;
end;

procedure TTMSFNCEdgeWebBrowser.CaptureScreenShot;
begin
  inherited CaptureScreenShot;
end;

procedure TTMSFNCEdgeWebBrowser.ClearCache;
begin
  inherited ClearCache;
end;

procedure TTMSFNCEdgeWebBrowser.DeInitialize;
begin
  inherited DeInitialize;
end;

procedure TTMSFNCEdgeWebBrowser.DeleteAllCookies;
begin
  inherited DeleteAllCookies;
end;

procedure TTMSFNCEdgeWebBrowser.DeleteCookie(AName, ADomain, APath: string);
begin
  inherited DeleteCookie(AName, ADomain, APath);
end;

procedure TTMSFNCEdgeWebBrowser.ExecuteJavaScript(AScript: String; ACompleteEvent: TTMSFNCWebBrowserJavaScriptCompleteEvent = nil; AImmediate: Boolean = False);
begin
  inherited ExecuteJavaScript(AScript, ACompleteEvent, AImmediate);
end;

function TTMSFNCEdgeWebBrowser.ExecuteJavaScriptSync(AScript: string): string;
begin
  Result := inherited ExecuteJavaScriptSync(AScript);
end;

function TTMSFNCEdgeWebBrowser.GetBridgeCommunicationLayer(ABridgeName: string): string;
begin
  Result := inherited GetBridgeCommunicationLayer(ABridgeName);
end;

procedure TTMSFNCEdgeWebBrowser.GetCookies(AURI: string);
begin
  inherited GetCookies(AURI);
end;

{$IFDEF MSWINDOWS}
function TTMSFNCEdgeWebBrowser.GetWebBrowserInstance: IInterface;
begin
  Result := inherited GetWebBrowserInstance;
end;
{$ENDIF}

procedure TTMSFNCEdgeWebBrowser.GoBack;
begin
  inherited GoBack;
end;

procedure TTMSFNCEdgeWebBrowser.GoForward;
begin
  inherited GoForward;
end;

procedure TTMSFNCEdgeWebBrowser.Initialize;
begin
  inherited Initialize;
end;

function TTMSFNCEdgeWebBrowser.InitialPrintSettings: TTMSFNCWebBrowserPrintSettings;
begin
  Result := inherited InitialPrintSettings;
end;

function TTMSFNCEdgeWebBrowser.IsFMXBrowser: Boolean;
begin
  Result := inherited IsFMXBrowser;
end;

procedure TTMSFNCEdgeWebBrowser.LoadFile(AFile: String);
begin
  inherited LoadFile(AFile);
end;

procedure TTMSFNCEdgeWebBrowser.LoadHTML(AHTML: String);
begin
  inherited LoadHTML(AHTML);
end;

function TTMSFNCEdgeWebBrowser.NativeBrowser: Pointer;
begin
  Result := inherited NativeBrowser;
end;

function TTMSFNCEdgeWebBrowser.NativeEnvironment: Pointer;
begin
  Result := inherited NativeEnvironment;
end;

procedure TTMSFNCEdgeWebBrowser.StartDocumentReadyStateThread;
begin
  inherited StartDocumentReadyStateThread;
end;

procedure TTMSFNCEdgeWebBrowser.Navigate;
begin
  inherited Navigate;
end;

procedure TTMSFNCEdgeWebBrowser.Navigate(const AURL: string);
begin
  inherited Navigate(AURL);
end;

procedure TTMSFNCEdgeWebBrowser.NavigateWithData(AURI: string; AMethod: string; ABodyStream: TStream; AHeaders: TStrings);
begin
  inherited NavigateWithData(AURI, AMethod, ABodyStream, AHeaders);
end;

procedure TTMSFNCEdgeWebBrowser.NavigateWithData(AURI, AMethod, ABody: string; AHeaders: TStrings);
begin
  inherited NavigateWithData(AURI, AMethod, ABody, AHeaders);
end;

procedure TTMSFNCEdgeWebBrowser.OpenTaskManager;
begin
  inherited OpenTaskManager;
end;

procedure TTMSFNCEdgeWebBrowser.Print(APrintSettings: TTMSFNCWebBrowserPrintSettings);
begin
  inherited Print(APrintSettings);
end;

procedure TTMSFNCEdgeWebBrowser.Print;
begin
  inherited Print;
end;

procedure TTMSFNCEdgeWebBrowser.PrintToPDF(AFileName: string);
begin
  inherited PrintToPDF(AFileName);
end;

procedure TTMSFNCEdgeWebBrowser.PrintToPDF(AFileName: string; APrintSettings: TTMSFNCWebBrowserPrintSettings);
begin
  inherited PrintToPDF(AFileName, APrintSettings);
end;

procedure TTMSFNCEdgeWebBrowser.PrintToPDFStream;
begin
  inherited PrintToPDFStream;
end;

procedure TTMSFNCEdgeWebBrowser.PrintToPDFStream(APrintSettings: TTMSFNCWebBrowserPrintSettings);
begin
  inherited PrintToPDFStream(APrintSettings);
end;

procedure TTMSFNCEdgeWebBrowser.Reload;
begin
  inherited Reload;
end;

procedure TTMSFNCEdgeWebBrowser.RemoveBridge(ABridgeName: string);
begin
  inherited RemoveBridge(ABridgeName);
end;

procedure TTMSFNCEdgeWebBrowser.ShowDebugConsole;
begin
  inherited ShowDebugConsole;
end;

procedure TTMSFNCEdgeWebBrowser.ShowPrintUI;
begin
  inherited ShowPrintUI;
end;

procedure TTMSFNCEdgeWebBrowser.StopLoading;
begin
  inherited StopLoading;
end;

end.

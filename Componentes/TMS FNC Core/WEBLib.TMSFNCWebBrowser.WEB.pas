{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{            copyright (c) 2020 - 2021                               }
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

unit WEBLib.TMSFNCWebBrowser.WEB;

{$I WEBLib.TMSFNCDefines.inc}

interface

procedure RegisterWebBrowserService;
procedure UnRegisterWebBrowserService;

implementation

uses
  Classes, SysUtils, WEBLib.Controls, WEBLib.TMSFNCWebBrowser, WEBLib.TMSFNCUtils, WEBLib.TMSFNCTypes
  {$IFDEF WEBLIB}
  ,WEB
  {$ENDIF}
  ;

type
  TTMSFNCWEBWebBrowserService = class;

  TTMSFNCWEBWebBrowserService = class(TTMSFNCWebBrowserFactoryService)
  protected
    function DoCreateWebBrowser(const AValue: TTMSFNCCustomWebBrowser): ITMSFNCCustomWebBrowser; override;
    procedure DeleteCookies; override;
  end;

  TTMSFNCCustomWebBrowserProtected = class(TTMSFNCCustomWebBrowser);

  TTMSFNCWEBWebBrowser = class(TInterfacedObject, ITMSFNCCustomWebBrowser)
  private
    {$IFDEF WEBLIB}
    FCustomObjectMessage: Pointer;
    FCustomBridge: string;
    FCustomBridgeObject: TObject;
    {$ENDIF}
    FExternalBrowser: Boolean;
    FURL: string;
    FWebControl: TTMSFNCCustomWebBrowser;
  protected
    {$IFDEF WEBLIB}
    function HandleDoCustomObjectMessage(Event: TJSEvent): Boolean; virtual;
    procedure ClearMethodPointers;
    procedure GetMethodPointers;
    {$ENDIF}
    function GetUserAgent: string;
    procedure SetUserAgent(const Value: string);
    procedure BeforeChangeParent;
    procedure SetFocus;
    procedure SetCacheFolderName(const Value: string);
    procedure SetCacheFolder(const Value: string);
    procedure SetAutoClearCache(const Value: Boolean);
    procedure SetURL(const AValue: string);
    procedure SetExternalBrowser(const AValue: Boolean);
    procedure SetEnableContextMenu(const AValue: Boolean);
    procedure SetEnableShowDebugConsole(const AValue: Boolean);
    procedure SetEnableAcceleratorKeys(const AValue: Boolean);
    procedure ShowDebugConsole;
    procedure Navigate(const AURL: string); overload;
    procedure Navigate; overload;
    procedure LoadHTML(AHTML: String);
    procedure LoadFile(AFile: String);
    procedure GoForward;
    procedure GoBack;
    procedure Close;
    procedure Reload;
    procedure StopLoading;
    procedure UpdateVisible;
    procedure UpdateEnabled;
    procedure UpdateBounds;
    procedure Initialize;
    procedure ClearCache;
    procedure AddBridge(ABridgeName: string; ABridgeObject: TObject);
    procedure RemoveBridge(ABridgeName: string);
    procedure CaptureScreenShot;
    procedure DeInitialize;
    function GetExternalBrowser: Boolean;
    function GetEnableContextMenu: Boolean;
    function GetEnableShowDebugConsole: Boolean;
    function GetEnableAcceleratorKeys: Boolean;
    function GetURL: string;
    procedure ExecuteJavascript(AScript: String; ACompleteEvent: TTMSFNCWebBrowserJavaScriptCompleteEvent; ACallback: TNotifyEvent);
    function BodyInnerHTML: string;
    function DocumentReadyState: string;
    function NativeEnvironment: Pointer;
    function NativeBrowser: Pointer;
    function GetBrowserInstance: IInterface;
    function NativeDialog: Pointer;
    function IsFMXBrowser: Boolean;
    function CanGoBack: Boolean;
    function CanGoForward: Boolean;
    function GetCacheFolderName: string;
    function GetAutoClearCache: Boolean;
    function GetCacheFolder: string;
    property CacheFolderName: string read GetCacheFolderName write SetCacheFolderName;
    property AutoClearCache: Boolean read GetAutoClearCache write SetAutoClearCache;
    property CacheFolder: string read GetCacheFolder write SetCacheFolder;
    property UserAgent: string read GetUserAgent write SetUserAgent;
  public
    constructor Create(const AWebControl: TTMSFNCCustomWebBrowser);
  end;

var
  WebBrowserService: ITMSFNCWebBrowserService;

procedure RegisterWebBrowserService;
begin
  if not TTMSFNCWebBrowserPlatformServices.Current.SupportsPlatformService(ITMSFNCWebBrowserService, IInterface(WebBrowserService)) then
  begin
    WebBrowserService := TTMSFNCWEBWebBrowserService.Create;
    TTMSFNCWebBrowserPlatformServices.Current.AddPlatformService(ITMSFNCWebBrowserService, WebBrowserService);
  end;
end;

procedure UnregisterWebBrowserService;
begin
  TTMSFNCWebBrowserPlatformServices.Current.RemovePlatformService(ITMSFNCWebBrowserService);
end;

function TTMSFNCWEBWebBrowser.GetBrowserInstance: IInterface;
begin
  Result := nil;
end;

function TTMSFNCWEBWebBrowser.GetCacheFolder: string;
begin
  Result := '';
end;

function TTMSFNCWEBWebBrowser.GetCacheFolderName: string;
begin
  Result := '';
end;

function TTMSFNCWEBWebBrowser.GetAutoClearCache: Boolean;
begin
  Result := False;
end;

function TTMSFNCWEBWebBrowser.GetExternalBrowser: Boolean;
begin
  Result := FExternalBrowser;
end;

function TTMSFNCWEBWebBrowser.GetURL: string;
begin
  Result := FURL;
end;

function TTMSFNCWEBWebBrowser.GetUserAgent: string;
begin
  Result := '';
end;

procedure TTMSFNCWEBWebBrowser.GoBack;
begin
end;

procedure TTMSFNCWEBWebBrowser.GoForward;
begin
end;

procedure TTMSFNCWEBWebBrowser.Initialize;
begin
end;

function TTMSFNCWEBWebBrowser.GetEnableAcceleratorKeys: Boolean;
begin
  Result := False;
end;

function TTMSFNCWEBWebBrowser.GetEnableContextMenu: Boolean;
begin
  Result := False;
end;

function TTMSFNCWEBWebBrowser.GetEnableShowDebugConsole: Boolean;
begin
  Result := False;
end;

procedure TTMSFNCWEBWebBrowser.SetCacheFolder(const Value: string);
begin

end;

procedure TTMSFNCWEBWebBrowser.SetCacheFolderName(const Value: string);
begin

end;

procedure TTMSFNCWEBWebBrowser.SetAutoClearCache(const Value: Boolean);
begin

end;

{$IFDEF WEBLIB}
procedure TTMSFNCWEBWebBrowser.ClearMethodPointers;
begin
  inherited;
  FCustomObjectMessage := nil;
end;

procedure TTMSFNCWEBWebBrowser.GetMethodPointers;
begin
  inherited;
  if FCustomObjectMessage = nil then
    FCustomObjectMessage := @HandleDoCustomObjectMessage;
end;

function TTMSFNCWEBWebBrowser.HandleDoCustomObjectMessage(Event: TJSEvent): Boolean;
var
  s, t: string;
  b: ITMSFNCCustomWebBrowserBridge;
begin
  asm
    s = Event.detail;
    t = Event.type;
  end;

  if Assigned(FCustomBridgeObject) and (t = FCustomBridge) and Supports(FCustomBridgeObject, ITMSFNCCustomWebBrowserBridge, b) then
    b.ObjectMessage := s;

  Result := True;
end;
{$ENDIF}

procedure TTMSFNCWEBWebBrowser.AddBridge(ABridgeName: string;
  ABridgeObject: TObject);
{$IFDEF WEBLIB}
var
  eh: TJSEventTarget;
{$ENDIF}
begin
  {$IFDEF WEBLIB}
  if Assigned(FWebControl) and Assigned(TTMSFNCCustomWebBrowserProtected(FWebControl).ElementBindHandle) and (ABridgeName <> '') and Assigned(ABridgeObject) then
  begin
    GetMethodPointers;
    eh := TTMSFNCCustomWebBrowserProtected(FWebControl).ElementBindHandle;
    eh.addEventListener(ABridgeName, FCustomObjectMessage);
    FCustomBridge := ABridgeName;
    FCustomBridgeObject := ABridgeObject;
  end;
  {$ENDIF}
end;

function TTMSFNCWEBWebBrowser.BodyInnerHTML: string;
begin
  Result := '';
end;

function TTMSFNCWEBWebBrowser.IsFMXBrowser: Boolean;
begin
  Result := True;
end;

procedure TTMSFNCWEBWebBrowser.LoadFile(AFile: String);
begin
end;

procedure TTMSFNCWEBWebBrowser.LoadHTML(AHTML: String);
begin
  if Assigned(FWebControl) then
  begin
    {$IFDEF WEBLIB}
    if Assigned(FWebControl.ElementHandle) then
      FWebControl.ElementHandle.innerHTML := AHTML;
    {$ENDIF}
  end;
end;

procedure TTMSFNCWEBWebBrowser.Navigate(const AURL: string);
begin
end;

function TTMSFNCWEBWebBrowser.NativeBrowser: Pointer;
begin
  Result := nil;
end;

function TTMSFNCWEBWebBrowser.NativeDialog: Pointer;
begin
  Result := nil;
end;

function TTMSFNCWEBWebBrowser.NativeEnvironment: Pointer;
begin
  Result := nil;
end;

procedure TTMSFNCWEBWebBrowser.SetFocus;
begin

end;

procedure TTMSFNCWEBWebBrowser.BeforeChangeParent;
begin

end;

procedure TTMSFNCWEBWebBrowser.Navigate;
begin
  Navigate(FURL);
end;

procedure TTMSFNCWEBWebBrowser.SetEnableAcceleratorKeys(const AValue: Boolean);
begin

end;

procedure TTMSFNCWEBWebBrowser.SetEnableContextMenu(const AValue: Boolean);
begin

end;

procedure TTMSFNCWEBWebBrowser.SetEnableShowDebugConsole(const AValue: Boolean);
begin

end;

procedure TTMSFNCWEBWebBrowser.Reload;
begin
end;

procedure TTMSFNCWEBWebBrowser.RemoveBridge(ABridgeName: string);
{$IFDEF WEBLIB}
var
  eh: TJSEventTarget;
{$ENDIF}
begin
  {$IFDEF WEBLIB}
  if Assigned(FWebControl) and Assigned(TTMSFNCCustomWebBrowserProtected(FWebControl).ElementBindHandle) and (ABridgeName <> '') then
  begin
    eh := TTMSFNCCustomWebBrowserProtected(FWebControl).ElementBindHandle;
    eh.removeEventListener(ABridgeName, FCustomObjectMessage);
    if ABridgeName = FCustomBridge then
    begin
      FCustomBridge := '';
      FCustomBridgeObject := nil;
    end;
  end;
  {$ENDIF}
end;

procedure TTMSFNCWEBWebBrowser.SetExternalBrowser(const AValue: Boolean);
begin
  FExternalBrowser := AValue;
end;

procedure TTMSFNCWEBWebBrowser.SetURL(const AValue: string);
begin
  FURL := AValue;
end;

procedure TTMSFNCWEBWebBrowser.SetUserAgent(const Value: string);
begin

end;

procedure TTMSFNCWEBWebBrowser.ShowDebugConsole;
begin

end;

procedure TTMSFNCWEBWebBrowser.StopLoading;
begin
end;

procedure TTMSFNCWEBWebBrowser.UpdateBounds;
begin
end;

procedure TTMSFNCWEBWebBrowser.UpdateEnabled;
begin
end;

procedure TTMSFNCWEBWebBrowser.UpdateVisible;
begin
end;

function TTMSFNCWEBWebBrowser.CanGoBack: Boolean;
begin
  Result := False;
end;

function TTMSFNCWEBWebBrowser.CanGoForward: Boolean;
begin
  Result := False;
end;

procedure TTMSFNCWEBWebBrowser.CaptureScreenShot;
begin

end;

procedure TTMSFNCWEBWebBrowser.ClearCache;
begin

end;

procedure TTMSFNCWEBWebBrowser.Close;
begin
  TTMSFNCCustomWebBrowserProtected(FWebControl).DoCloseForm;
end;

constructor TTMSFNCWEBWebBrowser.Create(const AWebControl: TTMSFNCCustomWebBrowser);
begin
  FExternalBrowser := False;
  FWebControl := AWebControl;
  {$IFDEF WEBLIB}
  ClearMethodPointers;
  {$ENDIF}
end;

procedure TTMSFNCWEBWebBrowser.DeInitialize;
begin
end;

function TTMSFNCWEBWebBrowser.DocumentReadyState: string;
begin

end;

procedure TTMSFNCWEBWebBrowser.ExecuteJavascript(AScript: String; ACompleteEvent: TTMSFNCWebBrowserJavaScriptCompleteEvent; ACallback: TNotifyEvent);
var
  v, s, id, ssub: string;
  res, sc, sn: string;
  ps: Integer;
  p, psub: Integer;
  {$IFDEF WEBLIB}
  e: TJSHTMLElement;
  {$ENDIF}
begin
  id := StringReplace(CreateNewGUID, '{', '', [rfReplaceAll]);
  id := StringReplace(id, '}', '', [rfReplaceAll]);
  id := StringReplace(id, '-', '', [rfReplaceAll]);
  v := 'executeScript' + id;
  ssub := 'function ' + v + '(){' + #13#10;
  if Pos('function', AScript) > 0 then
  begin
    sc := AScript;
    ps := Pos('function ', AScript) + 9;
    sn := Copy(AScript, ps, Pos('(', AScript) - ps);
    sc := StringReplace(sc, sn + '();', '', [rfReplaceAll]);
    ssub := ssub + ' return (' + sc + ')();' + #13#10
  end
  else
    ssub := ssub + ' return ' + AScript + #13#10;
  ssub := ssub + '}';

  {$IFDEF WEBLIB}
  e := FWebControl.ElementHandle;

  asm
    var script = document.createElement('script');
    script.id = id;
    script.text = ssub;
    var ctx;

    if (e && e.ownerDocument) {
      e.ownerDocument.body.append(script);
      ctx = e.ownerDocument.defaultView;
    }
    else{
      document.body.append(script);
      ctx = document.body.defaultView;
    }
  end;

  {$ENDIF}

  s := v;

  {$IFDEF WEBLIB}
  asm
    function functionFromString(context, funcDef, funcParam) {
       try {
          var fn = new Function("return function() {return this." + funcDef + "(" + funcParam + ")};");
          return fn();
       } catch (e) {
          return (function() {});
       }
    }

    function mainfunc (context, func, p){
      var f = functionFromString(context, func, p);
      return f.apply(context);
    }

    var obj;
    res = mainfunc(ctx, s, obj);
    if (res) {
      res = res.toString();
    }
    else
    {
      res = "";
    }
  end;
  {$ENDIF}
  if Assigned(ACompleteEvent) then
    ACompleteEvent(res);

  if Assigned(ACallback) then
    ACallback(Self);

  {$IFDEF WEBLIB}
  asm
    var exScript = document.getElementById(id);
    if (exScript){
      exScript.parentNode.removeChild(exScript);
    }
  end;
  {$ENDIF}
end;

{ TTMSFNCWEBWebBrowserService }

procedure TTMSFNCWEBWebBrowserService.DeleteCookies;
begin
end;

function TTMSFNCWEBWebBrowserService.DoCreateWebBrowser(const AValue: TTMSFNCCustomWebBrowser): ITMSFNCCustomWebBrowser;
begin
  Result := TTMSFNCWEBWebBrowser.Create(AValue);
end;

end.

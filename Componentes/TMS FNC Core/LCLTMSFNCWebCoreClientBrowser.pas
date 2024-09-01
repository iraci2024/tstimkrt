{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{            copyright (c) 2018 - 2021                               }
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

unit LCLTMSFNCWebCoreClientBrowser;

{$I LCLTMSFNCDefines.inc}

interface

uses
  Classes, LCLTMSFNCWebBrowser
  {$HINTS OFF}
  {$IF COMPILERVERSION > 26}
  ,JSON
  {$ELSE}
  ,DBXJSON
  {$IFEND}
  {$HINTS ON}
  ,LCLTMSFNCTypes;

type
  TTMSFNCWebCoreClientBrowserReceiveMessageEvent = procedure(Sender: TObject; AMessage: TJSONValue) of object;

  TTMSFNCCustomWebCoreClientBrowser = class(TTMSFNCCustomWebBrowser)
  private
    FBuffer: Boolean;
    FBufferString: string;
    FOnReceiveMessage: TTMSFNCWebCoreClientBrowserReceiveMessageEvent;
    FOnConnected: TNotifyEvent;
  protected
    procedure PerformHandshake; virtual;
    procedure DoDocumentComplete; override;
    procedure BeforeNavigate(var Params: TTMSFNCCustomWebBrowserBeforeNavigateParams); override;
    procedure DoReceiveMessage(AMessage: TJSONValue); virtual;
    procedure DoConnected; virtual;
    property OnReceiveMessage: TTMSFNCWebCoreClientBrowserReceiveMessageEvent read FOnReceiveMessage write FOnReceiveMessage;
    property OnConnected: TNotifyEvent read FOnConnected write FOnConnected;
    procedure Navigate; overload; override;
    procedure Navigate(const AURL: string); overload; override;
    procedure SendMessage(const AMessage: string); virtual;
    procedure Send(const AJSON: TJSONValue); virtual;
    procedure Connect; virtual;
  public
  end;

  {$IFNDEF LCLLIB}
  [ComponentPlatformsAttribute(TMSPlatforms)]
  {$ENDIF}
  TTMSFNCWebCoreClientBrowser = class(TTMSFNCCustomWebCoreClientBrowser)
  public
    procedure Navigate; overload; override;
    procedure Navigate(const AURL: string); overload; override;
    procedure ExecuteJavaScript(AScript: String; ACompleteEvent: TTMSFNCWebBrowserJavaScriptCompleteEvent = nil; AImmediate: Boolean = False); override;
    function CheckIdentifier: Boolean; override;
    function ExecuteJavaScriptSync(AScript: String): string; override;
    procedure LoadHTML(AHTML: String); override;
    procedure LoadFile(AFile: String); override;
    procedure Initialize; override;
    procedure DeInitialize; override;
    procedure GoForward; override;
    procedure GoBack; override;
    procedure Reload; override;
    procedure StopLoading; override;
    procedure AddBridge(ABridgeName: string; ABridgeObject: TObject); override;
    procedure RemoveBridge(ABridgeName: string); override;
    procedure CaptureScreenShot; override;
    function GetBridgeCommunicationLayer(ABridgeName: string): string; override;
    function NativeEnvironment: Pointer; override;
    function NativeBrowser: Pointer; override;
    function IsFMXBrowser: Boolean; override;
    function CanGoBack: Boolean; override;
    function CanGoForward: Boolean; override;
    {$IFDEF ANDROID}
    function NativeDialog: Pointer; override;
    {$ENDIF}
    {$IFDEF MSWINDOWS}
    function GetWebBrowserInstance: IInterface; override;
    {$ENDIF}
    property OnCloseForm;
    property EnableContextMenu;
    property EnableShowDebugConsole;
    property EnableAcceleratorKeys;
    property CacheFolder;
    property CacheFolderName;
    property AutoClearCache;
    procedure ClearCache; override;
    procedure SendMessage(const AMessage: string); override;
    procedure Send(const AJSON: TJSONValue); override;
    procedure Connect; override;
  published
    property URL;
    property OnBeforeNavigate;
    property OnNavigateComplete;
    property OnHardwareButtonClicked;
    property Version;
    property OnReceiveMessage;
    property OnConnected;
  end;

  TTMSFNCWebCoreClientBrowserPopup = class(TTMSFNCWebBrowserPopup)
  private
    FOnConnected: TNotifyEvent;
    FOnReceiveMessage: TTMSFNCWebCoreClientBrowserReceiveMessageEvent;
  protected
    function GetWebBrowserClass: TTMSFNCCustomWebBrowserClass; override;
    procedure InitializeWebBrowser(AWebBrowser: TTMSFNCCustomWebBrowser); override;
    procedure DoReceiveMessage(Sender: TObject; AMessage: TJSONValue); virtual;
    procedure DoConnected(Sender: TObject); virtual;
  public
    procedure SendMessage(const AMessage: string); virtual;
    procedure Send(const AJSON: TJSONValue); virtual;
    procedure Connect;
  published
    property OnReceiveMessage: TTMSFNCWebCoreClientBrowserReceiveMessageEvent read FOnReceiveMessage write FOnReceiveMessage;
    property OnConnected: TNotifyEvent read FOnConnected write FOnConnected;
  end;

implementation

uses
  LCLTMSFNCUtils, StrUtils;

{ TTMSFNCCustomWebCoreClientBrowser }

procedure TTMSFNCCustomWebCoreClientBrowser.BeforeNavigate(
  var Params: TTMSFNCCustomWebBrowserBeforeNavigateParams);
var
  s: string;
  o: TJSONValue;
  b: Boolean;
const
  prefix = 'webclientevent://';
begin
  s := TTMSFNCUtils.AddForwardslash(Params.URL);
  b := (Pos(prefix, s) = 1);
  Params.Cancel := b;
  if b then
  begin
    Delete(s, 1, Length(prefix) - 1);
    Delete(s, 1, 1);
    Delete(s, Length(s), 1);
    s := TTMSFNCUtils.URLDecode(s);
    if s = 'BUFFERSTART' then
    begin
      FBuffer := True;
      FBufferString := '';
    end
    else if s = 'BUFFEREND' then
    begin
      FBuffer := False;
      o := TJSONObject.ParseJSONValue(FBufferString);
      if Assigned(o) then
      begin
        DoReceiveMessage(o);
        o.Free;
      end;
    end
    else
    begin
      if FBuffer then
      begin
        FBufferString := FBufferString + s;
      end
      else
      begin
        o := TJSONObject.ParseJSONValue(s);
        if Assigned(o) then
        begin
          DoReceiveMessage(o);
          o.Free;
        end;
      end;
    end;
  end;

  inherited;
end;

procedure TTMSFNCCustomWebCoreClientBrowser.Navigate;
begin
  inherited;
  Connect;
end;

procedure TTMSFNCCustomWebCoreClientBrowser.Connect;
begin
  StartDocumentReadyStateThread;
end;

procedure TTMSFNCCustomWebCoreClientBrowser.DoConnected;
begin
  if Assigned(OnConnected) then
    OnConnected(Self);
end;

procedure TTMSFNCCustomWebCoreClientBrowser.DoDocumentComplete;
begin
  inherited;
  PerformHandshake;
  DoConnected;
end;

procedure TTMSFNCCustomWebCoreClientBrowser.DoReceiveMessage(
  AMessage: TJSONValue);
begin
  if Assigned(OnReceiveMessage) then
    OnReceiveMessage(Self, AMessage);
end;

procedure TTMSFNCCustomWebCoreClientBrowser.Navigate(const AURL: string);
begin
  inherited;
  Connect;
end;

procedure TTMSFNCCustomWebCoreClientBrowser.PerformHandshake;
var
  c: string;
begin
  {$IFDEF MSWINDOWS}
  c := 'windows';
  {$ENDIF}
  {$IFDEF MACOS}
  {$IFDEF IOS}
  c := 'ios';
  {$ENDIF}
  {$IFNDEF IOS}
  c := 'macos';
  {$ENDIF}
  {$ENDIF}
  {$IFDEF ANDROID}
  c := 'android';
  {$ENDIF}
  ExecuteJavascript('HandShake("'+c+'");');
end;

procedure TTMSFNCCustomWebCoreClientBrowser.Send(const AJSON: TJSONValue);
begin
  {$HINTS OFF}
  {$IF COMPILERVERSION > 26}
  SendMessage(AJSON.ToJSON);
  {$ELSE}
  SendMessage(AJSON.ToString);
  {$IFEND}
  {$HINTS ON}
end;

procedure TTMSFNCCustomWebCoreClientBrowser.SendMessage(const AMessage: string);
begin
  ExecuteJavascript('pas["WEBLib.Forms"].Application.ReceiveMessageFromClient('''+AMessage+''');');
end;

{ TTMSFNCWebCoreClientBrowserPopup }

procedure TTMSFNCWebCoreClientBrowserPopup.Connect;
begin
  if Assigned(WebBrowser) and (WebBrowser is TTMSFNCCustomWebCoreClientBrowser) then
    TTMSFNCCustomWebCoreClientBrowser(WebBrowser).Connect;
end;

procedure TTMSFNCWebCoreClientBrowserPopup.DoConnected;
begin
  if Assigned(OnConnected) then
    OnConnected(Self);
end;

procedure TTMSFNCWebCoreClientBrowserPopup.DoReceiveMessage(Sender: TObject;
  AMessage: TJSONValue);
begin
  if Assigned(OnReceiveMessage) then
    OnReceiveMessage(Self, AMessage);
end;

function TTMSFNCWebCoreClientBrowserPopup.GetWebBrowserClass: TTMSFNCCustomWebBrowserClass;
begin
  Result := TTMSFNCCustomWebCoreClientBrowser;
end;

procedure TTMSFNCWebCoreClientBrowserPopup.InitializeWebBrowser(
  AWebBrowser: TTMSFNCCustomWebBrowser);
begin
  inherited;
  if Assigned(AWebBrowser) and (AWebBrowser is TTMSFNCCustomWebCoreClientBrowser) then
  begin
    TTMSFNCCustomWebCoreClientBrowser(WebBrowser).OnConnected := DoConnected;
    TTMSFNCCustomWebCoreClientBrowser(WebBrowser).OnReceiveMessage := DoReceiveMessage;
  end;
end;

procedure TTMSFNCWebCoreClientBrowserPopup.Send(const AJSON: TJSONValue);
begin
  if Assigned(WebBrowser) and (WebBrowser is TTMSFNCCustomWebCoreClientBrowser) then
    TTMSFNCCustomWebCoreClientBrowser(WebBrowser).Send(AJSON);
end;

procedure TTMSFNCWebCoreClientBrowserPopup.SendMessage(const AMessage: string);
begin
  if Assigned(WebBrowser) and (WebBrowser is TTMSFNCCustomWebCoreClientBrowser) then
    TTMSFNCCustomWebCoreClientBrowser(WebBrowser).SendMessage(AMessage);
end;

{ TTMSFNCWebCoreClientBrowser }

procedure TTMSFNCWebCoreClientBrowser.AddBridge(ABridgeName: string; ABridgeObject: TObject);
begin
  inherited AddBridge(ABridgeName, ABridgeObject);
end;

function TTMSFNCWebCoreClientBrowser.CanGoBack: Boolean;
begin
  Result := inherited CanGoBack;
end;

function TTMSFNCWebCoreClientBrowser.CanGoForward: Boolean;
begin
  Result := inherited CanGoForward;
end;

procedure TTMSFNCWebCoreClientBrowser.CaptureScreenShot;
begin
  inherited CaptureScreenShot;
end;

function TTMSFNCWebCoreClientBrowser.CheckIdentifier: Boolean;
begin
  Result := True;
end;

procedure TTMSFNCWebCoreClientBrowser.SendMessage(const AMessage: string);
begin
  inherited SendMessage(AMessage);
end;

procedure TTMSFNCWebCoreClientBrowser.Send(const AJSON: TJSONValue);
begin
  inherited Send(AJSON);
end;

procedure TTMSFNCWebCoreClientBrowser.Connect;
begin
  inherited Connect;
end;

procedure TTMSFNCWebCoreClientBrowser.ClearCache;
begin
  inherited ClearCache;
end;

procedure TTMSFNCWebCoreClientBrowser.DeInitialize;
begin
  inherited DeInitialize;
end;

procedure TTMSFNCWebCoreClientBrowser.ExecuteJavaScript(AScript: String;
  ACompleteEvent: TTMSFNCWebBrowserJavaScriptCompleteEvent = nil; AImmediate: Boolean = False);
begin
  inherited ExecuteJavaScript(AScript, ACompleteEvent, AImmediate);
end;

function TTMSFNCWebCoreClientBrowser.ExecuteJavaScriptSync(AScript: String): string;
begin
  Result := inherited ExecuteJavaScriptSync(AScript);
end;

function TTMSFNCWebCoreClientBrowser.GetBridgeCommunicationLayer(ABridgeName: string): string;
begin
  Result := inherited GetBridgeCommunicationLayer(ABridgeName);
end;

{$IFDEF ANDROID}
function TTMSFNCWebCoreClientBrowser.NativeDialog: Pointer;
begin
  Result := inherited NativeDialog;
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
function TTMSFNCWebCoreClientBrowser.GetWebBrowserInstance: IInterface;
begin
  Result := inherited GetWebBrowserInstance;
end;
{$ENDIF}

procedure TTMSFNCWebCoreClientBrowser.GoBack;
begin
  inherited GoBack;
end;

procedure TTMSFNCWebCoreClientBrowser.GoForward;
begin
  inherited GoForward;
end;

procedure TTMSFNCWebCoreClientBrowser.Initialize;
begin
  inherited Initialize;
end;

function TTMSFNCWebCoreClientBrowser.IsFMXBrowser: Boolean;
begin
  Result := inherited IsFMXBrowser;
end;

procedure TTMSFNCWebCoreClientBrowser.LoadFile(AFile: String);
begin
  inherited LoadFile(AFile);
end;

procedure TTMSFNCWebCoreClientBrowser.LoadHTML(AHTML: String);
begin
  inherited LoadHTML(AHTML);
end;

function TTMSFNCWebCoreClientBrowser.NativeBrowser: Pointer;
begin
  Result := inherited NativeBrowser;
end;

function TTMSFNCWebCoreClientBrowser.NativeEnvironment: Pointer;
begin
  Result := inherited NativeEnvironment;
end;

procedure TTMSFNCWebCoreClientBrowser.Navigate;
begin
  inherited Navigate;
end;

procedure TTMSFNCWebCoreClientBrowser.Navigate(const AURL: string);
begin
  inherited Navigate(AURL);
end;

procedure TTMSFNCWebCoreClientBrowser.Reload;
begin
  inherited Reload;
end;

procedure TTMSFNCWebCoreClientBrowser.RemoveBridge(ABridgeName: string);
begin
  inherited RemoveBridge(ABridgeName);
end;

procedure TTMSFNCWebCoreClientBrowser.StopLoading;
begin
  inherited StopLoading;
end;

end.

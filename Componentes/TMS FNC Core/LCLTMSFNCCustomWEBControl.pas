{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{            copyright (c) 2021                                      }
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

unit LCLTMSFNCCustomWEBControl;

{$I LCLTMSFNCDefines.inc}

{$IFDEF WEBLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}
{$IFDEF LCLLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}

interface

uses
  Classes, Types, DateUtils, Forms
  {$IFNDEF LCLWEBLIB}
  ,Generics.Collections
  {$ENDIF}
  {$IFDEF LCLLIB}
  ,fgl
  {$ENDIF}
  {$IFDEF WEBLIB}
  ,web, Contnrs, WEBLib.Controls
  {$ENDIF}
  ,LCLTMSFNCTypes
  ,LCLTMSFNCGraphicsTypes
  ,LCLTMSFNCWebBrowser;

const
  LB = #13;
  STARTHTMLTAG = '<html>';
  STARTHEADTAG = '<head>';
  STARTBODYTAG = '<body>';
  STARTTITLETAG = '<title>';
  STARTSCRIPTTAG = '<script>';
  STARTSTYLETAG = '<style>';
  ENDHTMLTAG = '</html>';
  ENDHEADTAG = '</head>';
  ENDBODYTAG = '</body>';
  ENDTITLETAG = '</title>';
  ENDSCRIPTTAG = '</script>';
  ENDSTYLETAG = '</style>';

type
  TTMSFNCCustomWEBControl = class;

  TTMSFNCCustomWEBControlLinkKind = (mlkLink, mlkScript, mlkStyle);

  TTMSFNCCustomWEBControlLink = class
  private
    FCharSet: string;
    FType: string;
    FURL: string;
    FContent: string;
    FDefer: Boolean;
    FAsync: Boolean;
    FRel: string;
    FKind: TTMSFNCCustomWEBControlLinkKind;
  public
    constructor CreateScript(AURL: string; AType: string = ''; ACharSet: string = ''; AContent: string = ''; ADefer: Boolean = False; AAsync: Boolean = False); virtual;
    constructor CreateLink(AURL: string; AType: string = ''; ARel: string = ''); virtual;
    constructor Create(AKind: TTMSFNCCustomWEBControlLinkKind; AURL: string; AType: string; ACharSet: string; ARel: string; AContent: string; ADefer: Boolean; AAsync: Boolean); virtual;
    property URL: string read FURL write FURL;
    property &Type: string read FType write FType;
    property CharSet: string read FCharSet write FCharSet;
    property Content: string read FContent write FContent;
    property Rel: string read FRel write FRel;
    property Defer: Boolean read FDefer write FDefer;
    property Async: Boolean read FAsync write FAsync;
    property Kind: TTMSFNCCustomWEBControlLinkKind read FKind write FKind;
  end;

  {$IFDEF WEBLIB}
  TTMSFNCCustomWEBControlLinksList = class(TObjectList)
  private
    function GetItem(Index: Integer): TTMSFNCCustomWEBControlLink;
    procedure SetItem(Index: Integer; const Value: TTMSFNCCustomWEBControlLink);
  public
    property Items[Index: Integer]: TTMSFNCCustomWEBControlLink read GetItem write SetItem; default;
  end;
  {$ENDIF}
  {$IFNDEF WEBLIB}
  TTMSFNCCustomWEBControlLinksList = class(specialize TFPGObjectList<TTMSFNCCustomWEBControlLink>);
  {$ENDIF}

  TTMSFNCCustomWEBControlEventData = class(TPersistent)
  private
    FEvent: string;
    FEventName: string;
    FID: string;
    FCustomData: string;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property CustomData: string read FCustomData write FCustomData;
  published
    property ID: string read FID write FID;
    property EventName: string read FEventName write FEventName;
  end;

  TTMSFNCCustomWEBControlBridgeObjectMessageEvent = procedure(const AValue: string) of object;

  TTMSFNCCustomWEBControlBridge = class(TInterfacedPersistent, ITMSFNCCustomWebBrowserBridge)
  private
    FOnObjectMessage: TTMSFNCCustomWEBControlBridgeObjectMessageEvent;
    function GetObjectMessage: string;
    procedure SetObjectMessage(const Value: string);
  public
    property OnObjectMessage: TTMSFNCCustomWEBControlBridgeObjectMessageEvent read FOnObjectMessage write FOnObjectMessage;
  published
    property ObjectMessage: string read GetObjectMessage write SetObjectMessage;
  end;

  TTMSFNCCustomWEBControlCustomizeHeadLinksEvent = procedure(Sender: TObject; AList: TTMSFNCCustomWEBControlLinksList) of object;

  TTMSFNCCustomWEBControl = class(TTMSFNCCustomWebBrowser)
  private
    FControlInitialized: Boolean;
    {$IFDEF WEBLIB}
    FEventData: Pointer;
    {$ENDIF}
    FUpdateCount: Integer;
    FLocalFileAccess: Boolean;
    FRemoveMargins: Boolean;
    FOnCustomizeHeadLinks: TTMSFNCCustomWEBControlCustomizeHeadLinksEvent;
    procedure SetLocalFileAccess(const Value: Boolean);
    procedure SetRemoveMargins(const Value: Boolean);
    procedure SetControlInitialized(const Value: Boolean);
  protected
    function ParseEvent(AValue: string): Boolean; virtual;
    function ParseScript(AValue: string): string; virtual;
    function IsControlReady: Boolean; virtual;
    procedure Loaded; override;
    procedure DestroyControl; virtual;
    procedure DoControlInitialized; virtual;
    procedure BeforeNavigate(var Params: TTMSFNCCustomWebBrowserBeforeNavigateParams); override;
    procedure Initialized; override;
    procedure InitializeControl; virtual;
    procedure InitializeHTML; virtual;
    {$IFDEF WEBLIB}
    procedure LoadScripts(AHead: Boolean); virtual;
    procedure RemoveScripts; virtual;
    procedure LoadStyles; virtual;
    procedure RemoveStyles; virtual;
    {$ENDIF}
    procedure CreateClasses; override;
    procedure GetLinks(AList: TTMSFNCCustomWEBControlLinksList; AIncludeContent: Boolean = True; ACheckReady: Boolean = True); virtual;
    procedure GetBodyLinks(AList: TTMSFNCCustomWEBControlLinksList; AIncludeContent: Boolean = True; ACheckReady: Boolean = True); virtual;
    procedure GetHeadLinks(AList: TTMSFNCCustomWEBControlLinksList; ACheckReady: Boolean = True); virtual;
    procedure DoCustomizeHeadLinks(AList: TTMSFNCCustomWEBControlLinksList); virtual;
    procedure LoadLinks(AList: TTMSFNCCustomWEBControlLinksList); virtual;
    function GetCustomCSS: string; virtual;

    {$IFDEF WEBLIB}
    function HandleDoEventData(Event: TJSEvent): Boolean; virtual;
    procedure BindEvents; override;
    procedure UnbindEvents; override;
    procedure ClearMethodPointers; override;
    procedure GetMethodPointers; override;
    procedure SetName(const NewName: TComponentName); override;
    {$ENDIF}

    procedure CallCustomEvent(AEventData: TTMSFNCCustomWEBControlEventData); virtual;

    function GetGlobalVariables: string; virtual;
    function GetCustomFunctions: string; virtual;
    function GetCommunicationLayer: string; virtual;
    function GetDefaultHTML: string; virtual;
    function ParseLinks(AList: TTMSFNCCustomWEBControlLinksList): string; virtual;
    function GetHeadStyle: string; virtual;
    function GetBody: string; virtual;
    function GetControlTitle: string; virtual;
    function GetControlID: string; virtual;
    function GetControlVariable: string; virtual;
    function GetEventDataName: string; virtual;
    function GetDefaultHTMLMessage: string; virtual;
    function GetDefaultEventDataObject: string; virtual;
    function GetWaitForInitialization: string; virtual;
    function GetWaitInitVariables: string; virtual;
    function GetWaitInitCondition: string; virtual;
    function GetWaitInitElseStatement: string; virtual;

    function GetHTML: string; virtual;
    function CanLoadDefaultHTML: Boolean; override;

    property UpdateCount: Integer read FUpdateCount;
    property ControlInitialized: Boolean read FControlInitialized write SetControlInitialized;
    property LocalFileAccess: Boolean read FLocalFileAccess write SetLocalFileAccess default False;
    property RemoveMargins: Boolean read FRemoveMargins write SetRemoveMargins;
    property OnCustomizeHeadLinks: TTMSFNCCustomWEBControlCustomizeHeadLinksEvent read FOnCustomizeHeadLinks write FOnCustomizeHeadLinks;

  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate; override;
    procedure EndUpdate; override;
    procedure ReInitialize; virtual;

    property EnableContextMenu;
    property EnableShowDebugConsole;
    property EnableAcceleratorKeys;

  published
    property DesigntimeEnabled;
  end;

implementation

uses
  {%H-}Math,
  LCLTMSFNCUtils,
  SysUtils
  {$IFDEF FMXLIB}
  ,FMX.Types
  {$ENDIF}
  ;

const
  EVENTDATAPREFIX = 'jsevent://';
  CUSTOMDATAPREFIX = 'customdata://';

function TTMSFNCCustomWEBControl.GetDefaultHTMLMessage: string;
begin
  Result := Name + 'cannot be initialized';
end;

function TTMSFNCCustomWEBControl.GetEventDataName: string;
begin
  Result := Name + '_eventdata';
end;

function TTMSFNCCustomWEBControl.GetControlID: string;
begin
  Result := Name + '_id';
end;

function TTMSFNCCustomWEBControl.GetControlTitle: string;
begin
  Result := Name;
end;

function TTMSFNCCustomWEBControl.GetControlVariable: string;
begin
  Result := Name + '_var';
end;

function TTMSFNCCustomWEBControl.GetBody: string;
begin
  {$IFDEF WEBLIB}
  Result := '<div id="' + GetControlID + '" style="height:100%; width:100%"></div>';
  {$ELSE}
  Result := '<div id="' + GetControlID + '"></div>';
  {$ENDIF}
end;

function TTMSFNCCustomWEBControl.GetHeadStyle: string;
var
  m: string;
begin
  Result := '';

  m := GetCustomCSS;
  if m <> '' then
    Result := Result + LB + m;

  if RemoveMargins then
  begin
    Result := Result + LB +
      'body,html {' + LB +
      '  margin: 0;' + LB +
      '  padding: 0;' + LB +
      '}';
  end;
end;

function TTMSFNCCustomWEBControl.GetDefaultHTML: string;
var
  m: string;
begin
  Result :=
  {$IFNDEF WEBLIB}
  '<!DOCTYPE html>'+
  '<html lang="en">'+

  '<head>'+
  '  <meta charset="utf-8">' + LB +
  '  <meta http-equiv="X-UA-Compatible" content="IE=edge">' + LB +
  '  <meta name="viewport" content="width=device-width, initial-scale=1">' + LB +
  '  <title>404 HTML Template by Colorlib</title>' + LB +
  '  <link href="https://fonts.googleapis.com/css?family=Montserrat:200,400,700" rel="stylesheet">' + LB +
  '  <link type="text/css" rel="stylesheet" href="css/style.css" />' + LB +
  {$ENDIF}
  '  <style>' + LB +
//  {$IFNDEF WEBLIB}
//  '  * {'+ LB +
//  {$ELSE}
//  '  * {' + LB +
//  {$ENDIF}
//  '    -webkit-box-sizing: border-box;'+ LB +
//  '            box-sizing: border-box;'+ LB +
//  '  }'+ LB +

  {$IFDEF WEBLIB}
  '  #notfound {'+ LB +
  {$ELSE}
  '  html, body {'+ LB +
  {$ENDIF}
  '    background: #d7ebf6;' + LB +
  '    padding: 0;'+ LB +
  '    border: #000000;' + LB +
  '    width: 100%;' + LB +
  '    height: 100%;' + LB +
  '    margin: 0;' + LB +
  '    padding: 0;' + LB +
  '    border: solid #211b19;' + LB +
  '    border-width: thin;' + LB +
  '    overflow:hidden;' + LB +
  '    display:block;' + LB +
  '    -webkit-box-sizing: border-box;'+ LB +
  '            box-sizing: border-box;'+ LB +
  {$IFNDEF WEBLIB}
  '  }'+ LB +

  '  #notfound {'+ LB +
  {$ENDIF}
  '    position: relative;'+ LB +
  {$IFNDEF WEBLIB}
  '    height: 100vh;'+ LB +
  {$ENDIF}
  '  }'+ LB +

  '  #notfound .notfound {'+ LB +
  '    position: absolute;'+ LB +
  '    left: 50%;'+ LB +
  '    top: 50%;'+ LB +
  '    -webkit-transform: translate(-50%, -50%);'+ LB +
  '        -ms-transform: translate(-50%, -50%);'+ LB +
  '            transform: translate(-50%, -50%);'+ LB +
  '  }'+ LB +

  '  .notfound {'+ LB +
  '    max-width: 520px;'+ LB +
  '    width: 100%;'+ LB +
  '    line-height: 1.4;'+ LB +
  '    text-align: center;'+ LB +
  '  }'+ LB +

  '  .notfound .notfound-404 {'+ LB +
  '    position: relative;'+ LB +
  '    height: 200px;'+ LB +
  '    margin: 0px auto 20px;'+ LB +
  '    z-index: -1;'+ LB +
  '  }' + LB +

  '  .notfound .notfound-404 h2 {'+ LB +
  '    font-family: ''Montserrat'', sans-serif;'+ LB +
  '    font-size: 28px;'+ LB +
  '    font-weight: 400;'+ LB +
  '    text-transform: uppercase;'+ LB +
  '    color: #211b19;'+ LB +
  '    background: #d7ebf6;'+ LB +
  '    padding: 10px 5px;'+ LB +
  '    margin: auto;'+ LB +
  '    display: inline-block;'+ LB +
  '    position: absolute;'+ LB +
  '    bottom: 0px;'+ LB +
  '    left: 0;'+ LB +
  '    right: 0;'+ LB +
  '  }'+ LB +

  '  @media only screen and (max-width: 767px) {'+ LB +
  '    .notfound .notfound-404 h1 {'+ LB +
  '      font-size: 148px;'+ LB +
  '    }'+ LB +
  '  }'+ LB +

  '  @media only screen and (max-width: 480px) {'+ LB +
  '    .notfound .notfound-404 {'+ LB +
  '      height: 148px;'+ LB +
  '      margin: 0px auto 10px;'+ LB +
  '    }'+ LB +
  '    .notfound .notfound-404 h1 {'+ LB +
  '      font-size: 86px;'+ LB +
  '    }'+ LB +
  '    .notfound .notfound-404 h2 {'+ LB +
  '      font-size: 16px;'+ LB +
  '    }'+ LB +
  '    .notfound a {'+ LB +
  '      padding: 7px 15px;'+ LB +
  '      font-size: 14px;'+ LB +
  '    }'+ LB +
  '  }'+ LB +

  ' </style>' + LB +
  {$IFNDEF WEBLIB}
  '</head>' + LB +
  {$ENDIF}

  {$IFNDEF WEBLIB}
  '<body>' + LB +
  {$ENDIF}
  '  <div id="notfound">' + LB +
  '    <div class="notfound">' + LB +
  '      <div class="notfound-404">' + LB;


  m := GetDefaultHTMLMessage;

  Result := Result + '        <h2>' + m + '</h2>' + LB;

  Result := Result +
  '      </div>' + LB +
  '    </div>' + LB +
  '  </div>' + LB
  {$IFNDEF WEBLIB}
  +'</body>' + LB +
  '</html>'
  {$ENDIF}
  ;
end;

function TTMSFNCCustomWEBControl.GetHTML: string;
var
  hs, bs: string;
  l: TTMSFNCCustomWEBControlLinksList;
begin
  Result := '';
  if not IsControlReady then
  begin
    Result :=
    {$IFNDEF WEBLIB}
    StartHTMLTag + LB +
    StartBODYTag + LB +
    {$ENDIF}
    GetDefaultHTML
    {$IFNDEF WEBLIB}
     + LB + EndBODYTag + LB +
    EndHTMLTag
    {$ENDIF}
    ;
    Exit;
  end;

  {$IFDEF WEBLIB}
  Result := GetBody;
  {$ELSE}
  Result :=
    StartHTMLTag + LB +
    StartHEADTag + LB +
    '<meta name="viewport" content="width=device-width, initial-scale=1.0, user-scalable=no"/>'  + LB +
    '<meta http-equiv="content-type" content="text/html; charset=UTF-8"/>'  + LB +
    '<meta http-equiv="X-UA-Compatible" content="IE=11"/>'  + LB;

  l := TTMSFNCCustomWEBControlLinksList.Create;
  try
    GetHeadLinks(l);
    hs := ParseLinks(l);
    l.Clear;
    GetBodyLinks(l);
    bs := ParseLinks(l);
  finally
    l.Free;
  end;

  Result := Result +
    hs + LB +
    StartSTYLETag + LB +
    GetHeadStyle + LB +
    EndStyleTag + LB +
    StartTITLETag + LB +
    GetControlTitle + LB +
    EndTITLETag + LB +
    EndHEADTag + LB +
    StartBODYTag + LB +
    GetBody + LB +
    bs + LB +
    EndBODYTag + LB +
    EndHTMLTag + LB;
  {$ENDIF}
end;

procedure TTMSFNCCustomWEBControl.Assign(Source: TPersistent);
begin
  inherited;
  if (Source is TTMSFNCCustomWEBControl) then
  begin
  end;
end;

procedure TTMSFNCCustomWEBControl.BeginUpdate;
begin
  inherited;
  Inc(FUpdateCount);
end;

constructor TTMSFNCCustomWEBControl.Create(AOwner: TComponent);
begin
  inherited;
  {$IFDEF WEBLIB}
  if (ElementID = '') and IsDesigning then
    ElementID := GetNewName;
  {$ENDIF}
end;

procedure TTMSFNCCustomWEBControl.CreateClasses;
begin
  inherited;
end;

procedure TTMSFNCCustomWEBControl.Initialized;
begin
  inherited;
  FControlInitialized := False;
  InitializeControl;
  if IsDesigning then
  begin
    EnableContextMenu := False;
    EnableShowDebugConsole := False;
    EnableAcceleratorKeys := False;
  end;
end;

procedure TTMSFNCCustomWEBControl.InitializeHTML;
{$IFNDEF WEBLIB}
var
  s: TStringList;
  fn: string;
{$ENDIF}
{$IFDEF WEBLIB}
var
  v: Boolean;
{$ENDIF}
begin
  if (FUpdateCount > 0) or IsDestroying or ControlInitialized then
    Exit;

  {$IFDEF WEBLIB}
  LoadStyles;
  v := False;
  LoadScripts(True);
  LoadScripts(False);
  {$ENDIF}
  {$IFNDEF WEBLIB}
  if LocalFileAccess then
  begin
    s := TStringList.Create;
    try
      s.Text := GetHTML;
      fn := TTMSFNCUtils.AddBackslash(TTMSFNCUtils.GetTempPath) + CreateNewGUID + '.html';
      s.SaveToFile(fn{$IFNDEF LCLWEBLIB}, TEncoding.UTF8{$ENDIF});
      LoadFile(fn);
    finally
      s.Free;
    end;
  end
  else
  {$ENDIF}
  begin
    LoadHTML(GetHTML);
  end;
end;

procedure TTMSFNCCustomWEBControl.InitializeControl;
begin
  if (FUpdateCount > 0) or IsDestroying or ControlInitialized then
    Exit;

  {$IFDEF WEBLIB}
  if FEventData = nil then
  begin
    GetMethodPointers;
    BindEvents;
  end;
  {$ENDIF}

  InitializeHTML;
end;

procedure TTMSFNCCustomWEBControl.GetBodyLinks(AList: TTMSFNCCustomWEBControlLinksList; AIncludeContent: Boolean = True; ACheckReady: Boolean = True);
begin
  if ACheckReady and not IsControlReady then
    Exit;

  GetLinks(AList, AIncludeContent, ACheckReady);
end;

procedure TTMSFNCCustomWEBControl.GetHeadLinks(AList: TTMSFNCCustomWEBControlLinksList; ACheckReady: Boolean = True);
begin
  if ACheckReady and not IsControlReady then
    Exit;

  LoadLinks(AList);

  DoCustomizeHeadLinks(AList);
end;

{$IFDEF WEBLIB}
procedure TTMSFNCCustomWEBControl.ClearMethodPointers;
begin
  inherited;
  FEventData := nil;
end;

procedure TTMSFNCCustomWEBControl.SetName(const NewName: TComponentName);
begin
  DestroyControl;
  FControlInitialized := False;
  inherited;

  if FUpdateCount > 0 then
    Exit;

  InitializeControl;
end;

procedure TTMSFNCCustomWEBControl.GetMethodPointers;
begin
  inherited;
  if FEventData = nil then
    FEventData := @HandleDoEventData;
end;

function TTMSFNCCustomWEBControl.HandleDoEventData(Event: TJSEvent): Boolean;
var
  s: string;
begin
  asm
    s = Event.detail;
  end;
  ParseEvent(s);
  Result := True;
end;

procedure TTMSFNCCustomWEBControl.BindEvents;
var
  eh: TJSEventTarget;
begin
  if Assigned(ElementBindHandle) then
  begin
    GetMethodPointers;
    eh := ElementBindHandle;
    eh.addEventListener(GetEventDataName, FEventData);
  end;
end;

procedure TTMSFNCCustomWEBControl.UnbindEvents;
var
  eh: TJSEventTarget;
begin
  if Assigned(ElementBindHandle) then
  begin
    eh := ElementBindHandle;
    eh.removeEventListener(GetEventDataName, FEventData);
  end;
end;
{$ENDIF}

function TTMSFNCCustomWEBControl.GetCommunicationLayer: string;
  function GetWindowsComm: string;
  begin
    if IsDesignTime then
    begin
      Result := 'window.location = v;';
    end
    else
    begin
      Result :=

      '  if (!window.chrome || !window.chrome.webview || !window.chrome.webview.hostObjects || !window.chrome.webview.hostObjects.sync) {' + LB +
      '    return;' + LB +
      '  }' + LB +
      '  var obj = window.chrome.webview.hostObjects.sync.bridge;' + LB +
      '  if (obj) {' + LB +
      '    obj.ObjectMessage = v;' + LB +
      '  }else{' + #13 +
      '    window.location = v;' + LB +
      '  }';
    end;
  end;
begin
  Result :=
    'var ' + GetControlID + 'sendObjectMessage = function(parameters, customdata = undefined) {' + LB +
    '  var v = "' + EVENTDATAPREFIX + '" + parameters;' + LB +
    '  if (customdata) {' + LB +
    '    v = v + "' + CUSTOMDATAPREFIX + '" + customdata;' + LB +
    '  }' + LB +
    {$IFDEF ANDROID}
    '  if (!injectedObject) {' + LB +
    '    return;' + LB +
    '  }' + LB +
    '  injectedObject.Setjsvalue(v); ' + LB +
    '  injectedObject.performClick();' + LB +
    {$ENDIF}
    {$IFDEF LINUX}
    '  if (!window.webkit || !window.webkit.messageHandlers || !window.webkit.messageHandlers.bridge) {' + LB +
    '    return;' + LB +
    '  }' + LB +
    '  window.webkit.messageHandlers.bridge.postMessage(v);' + LB +
    {$ENDIF}
    {$IFDEF MACOS}
    '  if (!window.webkit || !window.webkit.messageHandlers || !window.webkit.messageHandlers.bridge) {' + LB +
    '    return;' + LB +
    '  }' + LB +
    '  window.webkit.messageHandlers.bridge.postMessage(v);' + LB +
    {$ENDIF}
    {$IFDEF MSWINDOWS}
    GetWindowsComm + LB +
    {$ENDIF}
    {$IFDEF WEBLIB}
    '  var event = new CustomEvent("' + GetEventDataName +'", {detail: v});' + LB +
    '  if (!' + GetControlVariable + '){' + LB +
    '  ' + GetControlVariable + ' = document.getElementById("' + ElementID + '");' + LB +
    '  }' + LB +
    '  if (' + GetControlVariable + '){' + LB +
    '    ' + GetControlVariable + '.dispatchEvent(event);' + LB +
    '  }' + LB +
    {$ENDIF}
    '};';
end;

procedure TTMSFNCCustomWEBControl.GetLinks(AList: TTMSFNCCustomWEBControlLinksList; AIncludeContent: Boolean = True; ACheckReady: Boolean = True);
var
  s, cf: string;
begin
  if ACheckReady and not IsControlReady then
    Exit;

  if AIncludeContent then
  begin
    s := s + GetGlobalVariables + LB + LB;

    {$IFDEF WEBLIB}
    s := s + 'var ' + GetControlVariable + ' = null;' + LB;
    {$ENDIF}

    s := s + GetCommunicationLayer + LB + LB;
    s := s + GetWaitForInitialization + LB + LB;

    cf := GetCustomFunctions;
    if cf <> '' then
      s := s + cf + LB + LB;
  end
  else
    s := 'Include';

  if s <> '' then
    AList.Add(TTMSFNCCustomWEBControlLink.CreateScript('', 'text/javascript', '', s));
end;

function TTMSFNCCustomWEBControl.GetWaitForInitialization: string;
begin
  Result :=
    'function ' + GetControlID + 'initialize () {' + LB +
    GetWaitInitVariables + LB +
    '  if (' + GetWaitInitCondition + ') {' + LB +
    '    setTimeout(' + GetControlID + 'initialize, 200);' + LB +
    '  } else {' + LB +
    GetWaitInitElseStatement + LB +
    '    var o = ' + GetDefaultEventDataObject + ';' + LB +
    '    o["EventName"] = "Initialized";' + LB +
    '    ' + GetControlID + 'sendObjectMessage(JSON.stringify(o));' + LB +
    '  };' + LB +
    '};' + LB +
    LB +
    GetControlID + 'initialize();';
end;

function TTMSFNCCustomWEBControl.GetWaitInitVariables: string;
begin
  Result := '';
end;

function TTMSFNCCustomWEBControl.GetWaitInitCondition: string;
begin
  Result := 'false';
end;

function TTMSFNCCustomWEBControl.GetWaitInitElseStatement: string;
begin
  Result := '';
end;

function TTMSFNCCustomWEBControl.GetGlobalVariables: string;
begin
  Result := '';
end;

function TTMSFNCCustomWEBControl.GetCustomFunctions: string;
begin
  Result := '';
end;

procedure TTMSFNCCustomWEBControl.DoControlInitialized;
begin
end;

procedure TTMSFNCCustomWEBControl.CallCustomEvent(AEventData: TTMSFNCCustomWEBControlEventData);
begin

end;

procedure TTMSFNCCustomWEBControl.BeforeNavigate(var Params: TTMSFNCCustomWebBrowserBeforeNavigateParams);
begin
  Params.Cancel := ParseEvent(Params.URL);
  inherited;
end;

function TTMSFNCCustomWEBControl.IsControlReady: Boolean;
begin
  Result := True;
end;

function TTMSFNCCustomWEBControl.ParseLinks(AList: TTMSFNCCustomWEBControlLinksList): string;
var
  I: Integer;
  lk: TTMSFNCCustomWEBControlLink;
begin
  Result := '';
  for I := 0 to AList.Count - 1 do
  begin
    lk := AList[I];
    case lk.Kind of
      mlkLink: Result := Result + '<link';
      mlkScript: Result := Result + '<script';
      mlkStyle: Result := Result + '<style';
    end;

    if lk.URL <> '' then
    begin
      case lk.Kind of
        mlkLink: Result := Result + ' href="';
        mlkScript: Result := Result + ' src="';
      end;

      Result := Result + lk.URL + '"';
    end;

    if lk.&Type <> '' then
      Result := Result + ' type="' + lk.&Type + '"';

    if lk.CharSet <> '' then
      Result := Result + ' charset="' + lk.CharSet + '"';

    if lk.Rel <> '' then
      Result := Result + ' rel="' + lk.Rel +'"';

    if lk.Async then
      Result := Result + ' async';

    if lk.Defer then
      Result := Result + ' defer';

    Result := Result + '>';

    if lk.Content <> '' then
      Result := Result + LB + lk.Content;

    case lk.Kind of
      mlkLink: Result := Result + LB + '</link>';
      mlkScript: Result := Result + LB + '</script>';
      mlkStyle: Result := Result + LB + '</style>';
    end;

    if (I < AList.Count - 1) then
      Result := Result + LB;
  end;
end;

destructor TTMSFNCCustomWEBControl.Destroy;
begin
  DestroyControl;
  ControlInitialized := False;
  inherited;
end;

procedure TTMSFNCCustomWEBControl.DestroyControl;
begin
  {$IFDEF WEBLIB}
  if Assigned(FEventData) then
  begin
    UnbindEvents;
    ClearMethodPointers;
  end;

  RemoveStyles;
  RemoveScripts;
  {$ENDIF}
end;

procedure TTMSFNCCustomWEBControl.EndUpdate;
begin
  inherited;
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
  begin
    if not ControlInitialized then
      InitializeControl;
  end;
end;

procedure TTMSFNCCustomWEBControl.SetLocalFileAccess(const Value: Boolean);
begin
  if FLocalFileAccess <> Value then
  begin
    FLocalFileAccess := Value;
    FControlInitialized := False;
    InitializeHTML;
  end;
end;

procedure TTMSFNCCustomWEBControl.SetControlInitialized(const Value: Boolean);
begin
  {$IFDEF WEBLIB}
//  if ControlInitialized and not Value then
//    ExecuteJavaScript('ResetControl();');
  {$ENDIF}

  FControlInitialized := Value;
end;

{$IFDEF WEBLIB}
function TTMSFNCCustomWEBControlLinksList.GetItem(Index: Integer): TTMSFNCCustomWEBControlLink;
begin
  Result := TTMSFNCCustomWEBControlLink(inherited Items[Index]);
end;

procedure TTMSFNCCustomWEBControlLinksList.SetItem(Index: Integer; const Value: TTMSFNCCustomWEBControlLink);
begin
  inherited Items[Index] := Value;
end;
{$ENDIF}

{ TTMSFNCCustomWEBControlLink }

constructor TTMSFNCCustomWEBControlLink.CreateScript(AURL: string; AType: string = ''; ACharSet: string = ''; AContent: string = ''; ADefer: Boolean = False; AAsync: Boolean = False);
begin
  Create(mlkScript, AURL, AType, ACharSet, '', AContent, ADefer, AASync);
end;

constructor TTMSFNCCustomWEBControlLink.CreateLink(AURL: string; AType: string = ''; ARel: string = '');
begin
  Create(mlkLink, AURL, AType, '', ARel, '', False, False);
end;

constructor TTMSFNCCustomWEBControlLink.Create(AKind: TTMSFNCCustomWEBControlLinkKind; AURL: string; AType: string; ACharSet: string; ARel: string; AContent: string; ADefer: Boolean; AAsync: Boolean);
begin
  FKind := AKind;
  FURL := AURL;
  FType := AType;
  FCharSet := ACharSet;
  FDefer := ADefer;
  FRel := ARel;
  FAsync := AAsync;
  FContent := AContent;
end;

procedure TTMSFNCCustomWEBControl.ReInitialize;
begin
  ControlInitialized := False;
  InitializeHTML;
end;

function TTMSFNCCustomWEBControl.ParseScript(AValue: string): string;
begin
  Result := AValue;
  if Result = 'null' then
  begin
    Result := '';
    Exit;
  end;

  Result := StringReplace(Result, '[', '', [rfReplaceAll]);
  Result := StringReplace(Result, ']', '', [rfReplaceAll]);
  Result := StringReplace(Result, '"', '', [rfReplaceAll]);
end;

function TTMSFNCCustomWEBControl.ParseEvent(AValue: string): Boolean;
var
  s, cd: string;
  b: Boolean;
  ed: TTMSFNCCustomWEBControlEventData;
  p, pcd: Integer;
begin
  s := AValue;
  b := (Pos(EVENTDATAPREFIX, s) = 1);
  Result := b;
  if b then
  begin
    p := Pos(EVENTDATAPREFIX, s);
    pcd := Pos(CUSTOMDATAPREFIX, s);
    if pcd > 0 then
    begin
      cd := Copy(s, pcd + Length(CUSTOMDATAPREFIX), Length(s) - pcd - 1);
      s := Copy(s, p + Length(EVENTDATAPREFIX), pcd - Length(EVENTDATAPREFIX) - 1);
    end
    else
    begin
      s := Copy(s, p + Length(EVENTDATAPREFIX), Length(s) - 1);
      cd := '';
    end;

    if cd <> '' then
      cd := TTMSFNCUtils.URLDecode(cd);

    s := TTMSFNCUtils.URLDecode(s);

    ed := TTMSFNCCustomWEBControlEventData.Create;
    try
      ed.JSON := s;
      ed.CustomData := cd;

      if ed.EventName = 'Initialized' then
      begin
        ControlInitialized := True;
        DoControlInitialized;
      end
      else
      begin
        CallCustomEvent(ed);
      end;
    finally
      ed.Free;
    end;
  end;
end;

procedure TTMSFNCCustomWEBControl.DoCustomizeHeadLinks(AList: TTMSFNCCustomWEBControlLinksList);
begin
  if Assigned(OnCustomizeHeadLinks) then
    OnCustomizeHeadLinks(Self, AList);
end;

procedure TTMSFNCCustomWEBControl.LoadLinks(AList: TTMSFNCCustomWEBControlLinksList);
begin

end;

function TTMSFNCCustomWEBControl.GetCustomCSS: string;
begin
  Result := '';
end;

function TTMSFNCCustomWEBControl.GetDefaultEventDataObject: string;
var
  obj: TTMSFNCCustomWEBControlEventData;
begin
  obj := TTMSFNCCustomWEBControlEventData.Create;
  try
    Result := obj.JSON;
  finally
    obj.Free;
  end;
end;

function TTMSFNCCustomWEBControl.CanLoadDefaultHTML: Boolean;
begin
  Result := False;
end;

procedure TTMSFNCCustomWEBControl.SetRemoveMargins(const Value: Boolean);
begin
  if FRemoveMargins <> Value then
  begin
    FRemoveMargins := Value;
    FControlInitialized := False;
    InitializeHTML;
  end;
end;

procedure TTMSFNCCustomWEBControl.Loaded;
begin
  inherited;
  BeginUpdate;
  EndUpdate;
end;

{$IFDEF WEBLIB}
procedure TTMSFNCCustomWEBControl.RemoveScripts;
var
  I: Integer;
  t, id: string;
  l: TTMSFNCCustomWEBControlLinksList;
  lk: TTMSFNCCustomWEBControlLink;
begin
  l := TTMSFNCCustomWEBControlLinksList.Create;

  GetHeadLinks(l, False);

  try
    for I := 0 to l.Count - 1 do
    begin
      lk := l[I];
      id := 'WEBLib.TMSFNCWEBControl.' + StringReplace(GetControlID, ' ', '', [rfReplaceAll]) + '.';

      case lk.Kind of
        mlkLink: t := 'LINK';
        mlkScript: t := 'SCRIPT';
      end;

      id := id + t + 'HEAD' + IntToStr(I);

      asm
        var exScript = document.getElementById(id);
        if (exScript){
          exScript.parentNode.removeChild(exScript);
        }
      end;
    end;

    l.Clear;

    GetBodyLinks(l, False, False);

    for I := 0 to l.Count - 1 do
    begin
      lk := l[I];
      id := 'WEBLib.TMSFNCWEBControl.' + StringReplace(GetControlID, ' ', '', [rfReplaceAll]) + '.';

      case lk.Kind of
        mlkLink: t := 'LINK';
        mlkScript: t := 'SCRIPT';
      end;

      id := id + t + 'BODY' + IntToStr(I);

      asm
        var exScript = document.getElementById(id);
        if (exScript){
          exScript.parentNode.removeChild(exScript);
        }
      end;
    end;

  finally
    l.Free;
  end;
end;

procedure TTMSFNCCustomWEBControl.RemoveStyles;
var
  s: string;
begin
  s := 'WEBLib.TMSFNCWEBControl.' + StringReplace(GetControlID, ' ', '', [rfReplaceAll]) + '.Styles';
  asm
    var exScript = document.getElementById(s);
    if (exScript){
       exScript.parentNode.removeChild(exScript);
    }
  end;
end;

procedure TTMSFNCCustomWEBControl.LoadScripts(AHead: Boolean);
var
  l: TTMSFNCCustomWEBControlLinksList;
  I: Integer;
  lk: TTMSFNCCustomWEBControlLink;
  id, src, tp, t, cs, rl, ct: string;
  asc, def: Boolean;
  h: Boolean;
  e: TJSHTMLElement;
begin
  if IsControlReady then
  begin
    h := AHead;

    e := ElementHandle;

    l := TTMSFNCCustomWEBControlLinksList.Create;
    try
      if h then
        GetHeadLinks(l)
      else
        GetBodyLinks(l);

      for I := 0 to l.Count - 1 do
      begin
        lk := l[I];
        id := 'WEBLib.TMSFNCWEBControl.' + StringReplace(GetControlID, ' ', '', [rfReplaceAll]) + '.';
        src := lk.URL;
        tp := lk.&Type;
        cs := lk.CharSet;
        def := lk.Defer;
        asc := lk.Async;
        rl := lk.Rel;
        ct := lk.Content;

        case lk.Kind of
          mlkLink: t := 'LINK';
          mlkScript: t := 'SCRIPT';
        end;

        if h then
          id := id + t + 'HEAD' + IntToStr(I)
        else
          id := id + t + 'BODY' + IntToStr(I);

        asm
          var exScript = document.getElementById(id) || document.querySelector('script[src="' + src + '"]') || document.querySelector('link[href="' + src + '"]');
        end;

        asm
          if (!exScript){
            var s = document.createElement(t);
            s.id = id;

            if (tp != ''){
              s.type = tp;
            }

            if (t == 'LINK'){
              if (rl != ''){
                s.rel = rl;
              }
              if (src != ''){
                s.href = src;
              }
            }
            else{
              s.defer = def;
              s.async = asc;
              if (src != ''){
                s.src = src;
              }
            }

            s.innerHTML = ct;

            if (e && e.ownerDocument) {
              if (h){
                e.ownerDocument.head.appendChild(s);
              }
              else{
                e.ownerDocument.body.appendChild(s);
              }
            }else{
              if (h){
                document.head.appendChild(s);
              }
              else{
                document.body.appendChild(s);
              }
            }
          }
        end;
      end;
    finally
      l.Free;
    end;
  end;
end;

procedure TTMSFNCCustomWEBControl.LoadStyles;
var
  s, sc: string;
  e: TJSHTMLElement;
begin
  if IsControlReady then
  begin
    e := ElementHandle;

    sc := GetHeadStyle;
    s := 'WEBLib.TMSFNCWEBControl.' + StringReplace(GetControlID, ' ', '', [rfReplaceAll]) + '.Styles';
    asm
      var exScript = document.getElementById(s);
      if (exScript){
        return;
      }

      var s = document.createElement("style");
      s.id = s;
      s.innerHTML = sc;

      if (e && e.ownerDocument){
        e.ownerDocument.head.appendChild(s);
      }else{
        document.head.appendChild(s);
      }
    end;
  end;
end;
{$ENDIF}

{ TTMSFNCCustomWEBControlEventData }

constructor TTMSFNCCustomWEBControlEventData.Create;
begin
  FEvent := '';
end;

destructor TTMSFNCCustomWEBControlEventData.Destroy;
begin
  inherited;
end;

{ TTMSFNCCustomWEBControlBridge }

function TTMSFNCCustomWEBControlBridge.GetObjectMessage: string;
begin
  Result := '';
end;

procedure TTMSFNCCustomWEBControlBridge.SetObjectMessage(const Value: string);
begin
  if Assigned(OnObjectMessage) then
    OnObjectMessage(Value);
end;

end.

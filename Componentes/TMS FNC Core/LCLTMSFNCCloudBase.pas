{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{            copyright (c) 2019 - 2022                               }
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

unit LCLTMSFNCCloudBase;

{$I LCLTMSFNCDefines.inc}

{$IFDEF WEBLIB}
{$DEFINE LCLWEBLIB}
{$DEFINE LCLWEBLIBTHREAD}
{$ENDIF}
{$IFDEF LCLLIB}
{$DEFINE LCLWEBLIB}
{$DEFINE LCLLIBTHREAD}
{$DEFINE LCLWEBLIBTHREAD}
{$ENDIF}

{$IFNDEF LCLWEBLIB}
{$HINTS OFF}
{$IF COMPILERVERSION <= 26}
{$DEFINE LCLWEBLIBTHREAD}
{$DEFINE LCLLIBTHREAD}
{$IFEND}
{$HINTS ON}
{$ENDIF}

{$IFNDEF LCLWEBLIB}
{$HINTS OFF}
{$IF COMPILERVERSION > 30}
{$DEFINE SUPPORTCURL}
{$IFEND}
{$HINTS ON}
{$ENDIF}

interface

uses
  Classes, Types, LCLTMSFNCCustomComponent, LCLTMSFNCUtils, LCLTMSFNCTypes, SysUtils
  {$IFNDEF LCLWEBLIB}
  ,Generics.Collections
  {$HINTS OFF}
  {$IF COMPILERVERSION > 26}
  ,Threading
  {$IFEND}
  {$HINTS ON}
  {$ENDIF}
  {$IFDEF LCLLIB}
  ,fgl
  {$ENDIF}
  {$IFDEF WEBLIB}
  ,contnrs, js
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 1; // Release nr.
  BLD_VER = 1; // Build nr.
  sRequestLineBreak = #13#10;
  sRequestBoundary = 'AaB03x';
  sRequestHeadBoundary = '--' + sRequestBoundary;
  sRequestTailBoundary = '--' + sRequestBoundary + '--';
  ValidHTTPCodes = [200, 201, 202, 204];

  //v1.0.0.0 : First release
  //v1.0.0.1 : Fixed : Issue with saving/loading additional information such as key, client id, secret and callback url
  //v1.0.1.0 : New : OnRequestCancelled event added
  //         : New : OnAccessDenied event added
  //v1.0.1.0 : Improved : cancelling requests while uploading files

type
  TTMSFNCCloudBaseLocale = (cblDefault, cblEnglish, cblDutch, cblGerman, cblFrench, cblSpanish, cblItalian,
     cblPortuguese, cblGreek, cblDanish, cblRussian, cblRomanian, cblSwedish, cblFinnish, cblTurkish, cblJapanese);

  TTMSFNCCustomCloudBase = class;

  TTMSFNCCloudBaseRequest = class;

  TTMSFNCCloudBaseRequestResult = class;

  TTMSFNCCloudBaseRequestEvent = procedure(const ARequest: TTMSFNCCloudBaseRequestResult) of object;

  TTMSFNCCloudBaseRequestCompleteEvent = procedure(Sender: TObject; const ARequest: TTMSFNCCloudBaseRequestResult) of object;

  TTMSFNCCloudBaseRequestRunEvent = procedure(const ARequest: TTMSFNCCloudBaseRequestResult) of object;

  TTMSFNCCloudBaseRequestResultStringEvent = {$IFNDEF LCLLIB}reference to {$ENDIF}procedure(const AResult: string){$IFDEF LCLLIB} of object{$ENDIF};

  TTMSFNCCloudBaseRequestResultStreamEvent = {$IFNDEF LCLLIB}reference to {$ENDIF}procedure(const AResult: TMemoryStream){$IFDEF LCLLIB} of object{$ENDIF};

  TTMSFNCCloudBaseRequestResultEvent = {$IFNDEF LCLLIB}reference to {$ENDIF}procedure(const ARequestResult: TTMSFNCCloudBaseRequestResult){$IFDEF LCLLIB} of object{$ENDIF};

  TTMSFNCCloudBaseRequestResultEventWrapper = class
  private
    FCallbackRequest: TTMSFNCCloudBaseRequestResultEvent;
    FCallbackString: TTMSFNCCloudBaseRequestResultStringEvent;
    FCallbackStream: TTMSFNCCloudBaseRequestResultStreamEvent;
  public
    constructor Create(ACallbackRequest: TTMSFNCCloudBaseRequestResultEvent; ACallbackString: TTMSFNCCloudBaseRequestResultStringEvent; ACallbackStream: TTMSFNCCloudBaseRequestResultStreamEvent);
  end;

  TTMSFNCCloudBaseRequestResultProgressEvent = {$IFNDEF LCLLIB}reference to {$ENDIF}procedure(const ARequestResult: TTMSFNCCloudBaseRequestResult; AProgress: Single){$IFDEF LCLLIB} of object{$ENDIF};

  TTMSFNCCloudBaseLogEvent = procedure(Sender: TObject; const ARequestResult: TTMSFNCCloudBaseRequestResult; AMessage: string) of object;

  TTMSFNCCloudBaseRequestLogEvent = procedure(const ARequestResult: TTMSFNCCloudBaseRequestResult; AMessage: string) of object;

  TTMSFNCCloudBaseProgressEvent = procedure(Sender: TObject; const ARequestResult: TTMSFNCCloudBaseRequestResult; AProgress: Single; AUpload: Boolean) of object;

  TTMSFNCCloudBaseRequestProgressEvent = procedure(const ARequestResult: TTMSFNCCloudBaseRequestResult; AProgress: Single; AUpload: Boolean) of object;

  TTMSFNCCloudBaseCompleteEvent = procedure(Sender: TObject; const ARequestResult: TTMSFNCCloudBaseRequestResult) of object;

  TTMSFNCCloudBaseCancelledEvent = procedure(Sender: TObject; const ARequestResult: TTMSFNCCloudBaseRequestResult) of object;

  TTMSFNCCloudBaseStartedEvent = procedure(Sender: TObject; const ARequestResult: TTMSFNCCloudBaseRequestResult) of object;

  TTMSFNCCloudBaseExternalBrowserCallBackEvent = procedure(const ACallbackURLResult: string) of object;

  ITMSFNCCustomCloudBase = interface(IInterface)
  ['{F5D2BC35-639A-4EBF-8201-06B3F9EFB82D}']
    function GetUploadFileSize(const ARequest: TTMSFNCCloudBaseRequest): Int64;
    procedure ExecuteRequest(const ARequest: TTMSFNCCloudBaseRequestResult);
    procedure RunExternalBrowser(const AURL: string; const ACallBackURL: string; const ACallBack: TTMSFNCCloudBaseExternalBrowserCallBackEvent = nil);
    procedure CloseBrowser;
  end;

  ITMSFNCCloudBaseService = interface(IInterface)
  ['{4876E6F4-B5D4-4322-A0B9-749040C239A5}']
    function CreateCloudBase(const AValue: TTMSFNCCustomCloudBase): ITMSFNCCustomCloudBase;
    procedure DestroyCloudBase(const AValue: ITMSFNCCustomCloudBase);
  end;

  TTMSFNCCloudBaseRequestHeader = class
  private
    FName: string;
    FValue: string;
  public
    constructor Create(const AName: string; const AValue: string);
    property Name: string read FName write FName;
    property Value: string read FValue write FValue;
  end;

  {$IFDEF LCLWEBLIBTHREAD}
  TTaskStatus = (Created, WaitingToRun, Running, Completed, WaitingForChildren, Canceled, Exception);

  ITask = interface
  ['{5A2BE42C-8E7B-468B-8D6A-2B27EAE87B55}']
    function GetStatus: TTaskStatus;
    procedure Start;
    procedure Cancel;
    property Status: TTaskStatus read GetStatus;
  end;

  {$IFDEF LCLLIBTHREAD}
  TTaskWorkerThread = class(TThread)
  private
    FRequest: TTMSFNCCloudBaseRequestResult;
    FRunEvent: TTMSFNCCloudBaseRequestRunEvent;
  protected
    procedure Execute; override;
  public
    constructor Create(const ARequest: TTMSFNCCloudBaseRequestResult; const ARunEvent: TTMSFNCCloudBaseRequestRunEvent);
  end;
  {$ENDIF}

  TTask = class(TInterfacedObject, ITask)
  private
    {$IFDEF LCLLIBTHREAD}
    FWorkerThread: TTaskWorkerThread;
    {$ENDIF}
    FTaskStatus: TTaskStatus;
    FRunEvent: TTMSFNCCloudBaseRequestRunEvent;
    FRequest: TTMSFNCCloudBaseRequestResult;
    function GetStatus: TTaskStatus;
  public
    constructor Create(const ARequest: TTMSFNCCloudBaseRequestResult; const ARunEvent: TTMSFNCCloudBaseRequestRunEvent);
    destructor Destroy; override;
    procedure Start;
    procedure Cancel;
    property Status: TTaskStatus read GetStatus;
  end;
  {$ENDIF}

  {$IFDEF WEBLIB}
  TTMSFNCCloudBaseRequestHeaders = class(TObjectList)
  private
    function GetItem(Index: Integer): TTMSFNCCloudBaseRequestHeader;
    procedure SetItem(Index: Integer; const Value: TTMSFNCCloudBaseRequestHeader);
  public
    property Items[Index: Integer]: TTMSFNCCloudBaseRequestHeader read GetItem write SetItem; default;
  end;

  TTMSFNCCloudBaseRequests = class(TObjectList)
  private
    function GetItem(Index: Integer): TTMSFNCCloudBaseRequest;
    procedure SetItem(Index: Integer; const Value: TTMSFNCCloudBaseRequest);
  public
    property Items[Index: Integer]: TTMSFNCCloudBaseRequest read GetItem write SetItem; default;
  end;
  {$ENDIF}
  {$IFNDEF WEBLIB}
  TTMSFNCCloudBaseRequestHeaders = class(specialize TFPGObjectList<TTMSFNCCloudBaseRequestHeader>);

  TTMSFNCCloudBaseRequests = class(specialize TFPGObjectList<TTMSFNCCloudBaseRequest>);
  {$ENDIF}

  TTMSFNCCloudBaseRequestMethod = (rmGET, rmPOST, rmPOSTMULTIPART, rmPOSTMULTIPARTRELATED, rmPUT, rmPUTMULTIPART, rmPUTMULTIPARTRELATED, rmDELETE, rmPATCH, rmUPDATE);

  TTMSFNCCloudBaseRequestScheme = (rsHTTP, rsHTTPS);

  TTMSFNCCloudBaseRequestResultType = (rrtString, rrtStream, rrtFile);

  {$IFDEF SUPPORTCURL}
  TTMSFNCCloudBaseRequestCurlParserMode = (cpmNone, cpmHeader, cpmFormData, cpmData, cpmDataBinary, cpmDataRaw, cpmDataUrlEncode,
    cpmUser, cpmBearer, cpmRequest, cpmResultFile);

  TTMSFNCCloudBaseRequestCurlParserFile = class
  private
    FFileName: string;
  end;

  TTMSFNCCloudBaseRequestCurlParser = class
  private
    FRequest: TTMSFNCCloudBaseRequest;
  public
    function Parse(ACurl: string): TTMSFNCCloudBaseRequest;
    constructor Create;
    destructor Destroy; override;
  end;
  {$ENDIF}

  TTMSFNCCloudBaseRequestPostDataBuilder = class
  private
    FPostData: string;
  protected
    procedure AddInternalData(ADataType: string; AName: string; AValue: string; ASkipBoundary: Boolean; AFileName: string; AContentType: string; AContentTransferEncoding: string; ASkipLineBreakOnEnd: Boolean);
  public
    procedure Clear;
    procedure AddText(AText: string; ALineBreak: Boolean = True);
    procedure AddContentType(AValue: string);
    procedure AddLineBreak;
    procedure AddAttachment(AName: string; AValue: string; ASkipBoundary: Boolean = False; AFileName: string = ''; AContentType: string = ''; AContentTransferEncoding: string = ''; ASkipLineBreakOnEnd: Boolean = False);
    procedure AddHeadBoundary;
    procedure AddTailBoundary;
    procedure AddFormData(AName: string; AValue: string; ASkipBoundary: Boolean = False; AFileName: string = ''; AContentType: string = ''; AContentTransferEncoding: string = ''; ASkipLineBreakOnEnd: Boolean = False);
    procedure Close;
    function Build: string;
  end;

  TTMSFNCCloudBaseRequest = class
  private
    FHeaders: TTMSFNCCloudBaseRequestHeaders;
    FAgent: string;
    FHost: string;
    FPort: Integer;
    FPath: string;
    FQuery: string;
    FMethod: TTMSFNCCloudBaseRequestMethod;
    FName: string;
    FPostData: string;
    FDataPointer: Pointer;
    FDataBoolean: Boolean;
    FDataString: String;
    FDataObject: TObject;
    FDataInteger: NativeInt;
    FResultType: TTMSFNCCloudBaseRequestResultType;
    FUploadStream: TMemoryStream;
    FUploadFile: TTMSFNCUtilsFile;
    FResultFile: string;
    FCustomHeaders: Boolean;
    FDataUpload: TTMSFNCUtilsFile;
    FResultFileSize: Int64;
    FDataInt64: Int64;
    function GetUploadStream: TMemoryStream;
    function GetHeaders: TTMSFNCCloudBaseRequestHeaders;
  protected
    function DuplicateHeader(AHeader: TTMSFNCCloudBaseRequestHeader): TTMSFNCCloudBaseRequestHeader;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Assign(ARequest: TTMSFNCCloudBaseRequest);
    procedure Clear; virtual;
    procedure ClearHeaders; virtual;
    function HasUploadStream: Boolean;
    function HasUploadFile: Boolean;
    function GenerateHeaders: string;
    function GetMethodString: string;
    function GetURL: string;
    function GetServer: string;
    function GetQueryPath: string;
    function GetPort: Integer;
    function AddHeader(const AName: string; const AValue: string): TTMSFNCCloudBaseRequestHeader;
    function AddBasicAuthHeader(const AUserName: string; const APassword: string): TTMSFNCCloudBaseRequestHeader;
    property CustomHeaders: Boolean read FCustomHeaders write FCustomHeaders;
    property DataUpload: TTMSFNCUtilsFile read FDataUpload write FDataUpload;
    property DataPointer: Pointer read FDataPointer write FDataPointer;
    property DataBoolean: Boolean read FDataBoolean write FDataBoolean;
    property DataObject: TObject read FDataObject write FDataObject;
    property DataString: String read FDataString write FDataString;
    property DataInteger: NativeInt read FDataInteger write FDataInteger;
    property DataInt64: Int64 read FDataInt64 write FDataInt64;
    property Headers: TTMSFNCCloudBaseRequestHeaders read GetHeaders;
    property Agent: string read FAgent write FAgent;
    property PostData: string read FPostData write FPostData;
    property Host: string read FHost write FHost;
    property Port: Integer read FPort write FPort;
    property Path: string read FPath write FPath;
    property Query: string read FQuery write FQuery;
    property Method: TTMSFNCCloudBaseRequestMethod read FMethod write FMethod default rmGET;
    property Name: string read FName write FName;
    property ResultType: TTMSFNCCloudBaseRequestResultType read FResultType write FResultType default rrtString;
    property ResultFile: string read FResultFile write FResultFile;
    property ResultFileSize: Int64 read FResultFileSize write FResultFileSize;
    property UploadStream: TMemoryStream read GetUploadStream;
    property UploadFile: TTMSFNCUtilsFile read FUploadFile write FUploadFile;
  end;

  TTMSFNCCloudBaseRequestResult = class(TTMSFNCCloudBaseRequest)
  private
    FResponseHeaders: TTMSFNCCloudBaseRequestHeaders;
    FMessage: string;
    FIsDestroying: Boolean;
    FResultString: string;
    FCallBackResult: TTMSFNCCloudBaseRequestResultEvent;
    FCallBackProgress: TTMSFNCCloudBaseRequestResultProgressEvent;
    FAsync: Boolean;
    FTask: ITask;
    {$IFDEF WEBLIB}
    FResolve: TJSPromiseResolver;
    {$ENDIF}
    FResponseCode: Integer;
    FProgress: Single;
    FRunning: Boolean;
    FOnRemove: TTMSFNCCloudBaseRequestEvent;
    FOnComplete: TTMSFNCCloudBaseRequestEvent;
    FOnLog: TTMSFNCCloudBaseRequestLogEvent;
    FOnProgress: TTMSFNCCloudBaseRequestProgressEvent;
    FResultStream: TMemoryStream;
    FSuccess: Boolean;
    FOnCancelled: TTMSFNCCloudBaseRequestEvent;
  protected
    FTotalBytes: Int64;
    FBytesReceived: Int64;
    FDone: Boolean;
    procedure ProcessResult;
    procedure ProcessProgress;
    procedure RequestResult;
    procedure ProcessLog;
    procedure RequestProgress(AProgress: Single; AUpload: Boolean);
    function IsAsyncAvailable: Boolean;
    property OnRemove: TTMSFNCCloudBaseRequestEvent read FOnRemove write FOnRemove;
    property Progress: Single read FProgress;
  public
    constructor Create; reintroduce; overload;
    constructor Create(const ARequest: TTMSFNCCloudBaseRequest; const ARequestResultEvent: TTMSFNCCloudBaseRequestResultEvent = nil; const ARequestResultProgressEvent: TTMSFNCCloudBaseRequestResultProgressEvent = nil; const AAsync: Boolean = True; const ATask: ITask = nil); reintroduce; overload;
    destructor Destroy; override;
    procedure Clear; override;
    procedure Cancel; virtual;
    procedure Log(AMessage: string); virtual;
    function CheckTaskStatus: Boolean;
    function GetResponseHeader(AName: string): string;
    property ResultBytesReceived: Int64 read FBytesReceived;
    property ResultTotalBytes: Int64 read FTotalBytes;
    property ResultString: string read FResultString write FResultString;
    property ResultStream: TMemoryStream read FResultStream write FResultStream;
    property ResponseCode: Integer read FResponseCode write FResponseCode;
    property ResponseHeaders: TTMSFNCCloudBaseRequestHeaders read FResponseHeaders;
    property CallBackResult: TTMSFNCCloudBaseRequestResultEvent read FCallBackResult write FCallBackResult;
    property CallBackProgress: TTMSFNCCloudBaseRequestResultProgressEvent read FCallBackProgress write FCallBackProgress;
    property Async: Boolean read FAsync write FAsync;
    property Success: Boolean read FSuccess write FSuccess;
    property Task: ITask read FTask write FTask;
    property Running: Boolean read FRunning write FRunning default False;
    property OnLog: TTMSFNCCloudBaseRequestLogEvent read FOnLog write FOnLog;
    property OnProgress: TTMSFNCCloudBaseRequestProgressEvent read FOnProgress write FOnProgress;
    property OnComplete: TTMSFNCCloudBaseRequestEvent read FOnComplete write FOnComplete;
    property OnCancelled: TTMSFNCCloudBaseRequestEvent read FOnCancelled write FOnCancelled;
  end;

  TTMSFNCCloudBaseService = class(TPersistent)
  private
    FDeveloperURL: string;
    FName: string;
    FBaseURL: string;
  public
    procedure Assign(Source: TPersistent); override;
    property Name: string read FName write FName;
    property DeveloperURL: string read FDeveloperURL write FDeveloperURL;
    property BaseURL: string read FBaseURL write FBaseURL;
  end;

  TTMSFNCCustomCloudBase = class(TTMSFNCCustomComponent)
  private
    FPostDataBuilder: TTMSFNCCloudBaseRequestPostDataBuilder;
    FCloudBase: ITMSFNCCustomCloudBase;
    FRequest: TTMSFNCCloudBaseRequest;
    FRequestResult: TTMSFNCCloudBaseRequestResult;
    FRequests: TTMSFNCCloudBaseRequests;
    FOnRequestLog: TTMSFNCCloudBaseLogEvent;
    FLogging: Boolean;
    FLogFileName: string;
    FOnRequestComplete: TTMSFNCCloudBaseCompleteEvent;
    FOnRequestProgress: TTMSFNCCloudBaseProgressEvent;
    FService: TTMSFNCCloudBaseService;
    FScopes: TStrings;
    FOnRequestsComplete: TNotifyEvent;
    FOnRequestStarted: TTMSFNCCloudBaseStartedEvent;
    FOnRequestCancelled: TTMSFNCCloudBaseCancelledEvent;
    function IsLogFileNameStored: Boolean;
    procedure SetService(const Value: TTMSFNCCloudBaseService);
    procedure SetScopes(const Value: TStrings);
    function GetPostDataBuilder: TTMSFNCCloudBaseRequestPostDataBuilder;
  protected
    function GetInstance: NativeUInt; override;
    function GetVersion: string; override;
    function GetScopes(ADelimiter: Char; AEncode: Boolean = False): string;
    class function GetCloudBaseGlobal: TTMSFNCCustomCloudBase;
    class function GetPostDataBuilderGlobal: TTMSFNCCloudBaseRequestPostDataBuilder;
    class function GetHeadersGlobal: TTMSFNCCloudBaseRequestHeaders;
    procedure Log(const ARequestResult: TTMSFNCCloudBaseRequestResult; AMessage: string); virtual;
    class procedure DoCurlRequestComplete(const ARequestResult: TTMSFNCCloudBaseRequestResult);
    procedure DoRequestRemove(const ARequestResult: TTMSFNCCloudBaseRequestResult);
    procedure DoRequestComplete(const ARequestResult: TTMSFNCCloudBaseRequestResult);
    procedure DoRequestCancelled(const ARequestResult: TTMSFNCCloudBaseRequestResult);
    procedure DoRequestStarted(const ARequestResult: TTMSFNCCloudBaseRequestResult);
    procedure DoRequestsComplete;
    procedure DoRequestProgress(const ARequestResult: TTMSFNCCloudBaseRequestResult; AProgress: single; AUpload: Boolean);
    procedure DoRequestLog(const ARequestResult: TTMSFNCCloudBaseRequestResult; ALogMessage: string);
    procedure InternalExecuteRequest(const ARequest: TTMSFNCCloudBaseRequestResult);
    {$IFDEF SUPPORTCURL}
    class procedure InternalCurl(const ACurl: string; ACallbackRequest: TTMSFNCCloudBaseRequestResultEvent; ACallbackString: TTMSFNCCloudBaseRequestResultStringEvent; ACallbackStream: TTMSFNCCloudBaseRequestResultStreamEvent); overload;
    {$ENDIF}
    class procedure InternalSimpleGETAsync(const AURL: string; const AResultType: TTMSFNCCloudBaseRequestResultType; const ARequestResultEvent: TTMSFNCCloudBaseRequestResultEvent);
    class function InternalSimpleGETSync(const AURL: string; const AResultType: TTMSFNCCloudBaseRequestResultType): TTMSFNCCloudBaseRequestResult; {$IFDEF WEBLIB} async;{$ENDIf}
    property CloudBase: ITMSFNCCustomCloudBase read FCloudBase;
    property Version: string read GetVersion;
    property Service: TTMSFNCCloudBaseService read FService write SetService;
    property OnRequestLog: TTMSFNCCloudBaseLogEvent read FOnRequestLog write FOnRequestLog;
    property OnRequestProgress: TTMSFNCCloudBaseProgressEvent read FOnRequestProgress write FOnRequestProgress;
    property OnRequestComplete: TTMSFNCCloudBaseCompleteEvent read FOnRequestComplete write FOnRequestComplete;
    property OnRequestCancelled: TTMSFNCCloudBaseCancelledEvent read FOnRequestCancelled write FOnRequestCancelled;
    property OnRequestStarted: TTMSFNCCloudBaseStartedEvent read FOnRequestStarted write FOnRequestStarted;
    property OnRequestsComplete: TNotifyEvent read FOnRequestsComplete write FOnRequestsComplete;
    property Logging: Boolean read FLogging write FLogging default False;
    property LogFileName: string read FLogFileName write FLogFileName stored IsLogFileNameStored nodefault;
  public
    constructor Create; reintroduce; overload; virtual;
    constructor Create(AOwner: TComponent); overload; override;
    procedure Assign(Source: TPersistent); override;
    destructor Destroy; override;
    procedure ExecuteAllRequests;
    procedure Test; virtual;
    procedure CancelRequests; virtual;
    procedure CancelRequest(const ARequest: TTMSFNCCloudBaseRequestResult); virtual;
    function GetUploadFileSize(const ARequest: TTMSFNCCloudBaseRequest): Int64; virtual;
    function GetRequestCount(const IsRunning: Boolean = False; const ARequestName: string = ''): Integer; virtual;
    function AddRequest(const ARequestResultEvent: TTMSFNCCloudBaseRequestResultEvent = nil; const ARequestResultProgressEvent: TTMSFNCCloudBaseRequestResultProgressEvent = nil; const AAsync: Boolean = True): TTMSFNCCloudBaseRequestResult; virtual;
    function ExecuteRequest(const ARequestResultEvent: TTMSFNCCloudBaseRequestResultEvent = nil; const ARequestResultProgressEvent: TTMSFNCCloudBaseRequestResultProgressEvent = nil; const AAsync: Boolean = True): {$IFDEF WEBLIB}TJSPromise{$ELSE}TTMSFNCCloudBaseRequestResult{$ENDIF}; virtual;
    property Request: TTMSFNCCloudBaseRequest read FRequest;
    property RequestResult: TTMSFNCCloudBaseRequestResult read FRequestResult;
    property PostDataBuilder: TTMSFNCCloudBaseRequestPostDataBuilder read GetPostDataBuilder;
    property Scopes: TStrings read FScopes write SetScopes;
    property RunningRequests: TTMSFNCCloudBaseRequests read FRequests;
    class procedure DownloadFileFromURL(const AURL: string; const ARequestResultEvent: TTMSFNCCloudBaseRequestResultEvent);
    class procedure SimpleGETAsyncAsString(const AURL: string; const ARequestResultEvent: TTMSFNCCloudBaseRequestResultEvent);
    class procedure SimpleGETAsyncAsStream(const AURL: string; const ARequestResultEvent: TTMSFNCCloudBaseRequestResultEvent);
    {$IFNDEF LCLWEBLIB}
    class function SimpleGETSyncAsString(const AURL: string): string;
    class function SimpleGETSyncAsStream(const AURL: string): TMemoryStream;
    {$ENDIF}
    {$IFDEF SUPPORTCURL}
    class procedure Curl(const ACurl: string; ACallback: TTMSFNCCloudBaseRequestResultEvent); overload;
    class procedure Curl(const ACurl: string; ACallback: TTMSFNCCloudBaseRequestResultStringEvent); overload;
    class procedure Curl(const ACurl: string; ACallback: TTMSFNCCloudBaseRequestResultStreamEvent); overload;
    {$ENDIF}
  end;

  {$IFNDEF LCLLIB}
  [ComponentPlatformsAttribute(TMSPlatformsWeb)]
  {$ENDIF}
  TTMSFNCCloudBase = class(TTMSFNCCustomCloudBase)
  published
    property Logging;
    property LogFileName;
    property Version;
    property OnRequestsComplete;
    property OnRequestLog;
    property OnRequestComplete;
    property OnRequestCancelled;
    property OnRequestStarted;
    property OnRequestProgress;
  end;

  {$IFDEF WEBLIB}
  TTMSFNCCloudBaseList = class(TList)
  private
    function GetItem(Index: Integer): ITMSFNCCustomCloudBase;
    procedure SetItem(Index: Integer; const Value: ITMSFNCCustomCloudBase);
  public
    property Items[Index: Integer]: ITMSFNCCustomCloudBase read GetItem write SetItem; default;
  end;
  {$ENDIF}
  {$IFNDEF WEBLIB}
  TTMSFNCCloudBaseList = class(specialize TFPGList<ITMSFNCCustomCloudBase>);
  {$ENDIF}

  TTMSFNCCloudBaseFactoryService = class(TInterfacedObject, ITMSFNCCloudBaseService)
  protected
    FCloudBases: TTMSFNCCloudBaseList;
    function DoCreateCloudBase(const AValue: TTMSFNCCustomCloudBase): ITMSFNCCustomCloudBase; virtual; abstract;
    procedure DoRemoveCloudBase(const AValue: ITMSFNCCustomCloudBase);
  public
    constructor Create;
    destructor Destroy; override;
    function CreateCloudBase(const AValue: TTMSFNCCustomCloudBase): ITMSFNCCustomCloudBase;
    procedure DestroyCloudBase(const AValue: ITMSFNCCustomCloudBase);
  end;

  TTMSFNCCloudPlatformServicesService = class
  private
    FInterface: IInterface;
    FGUID: string;
  public
    constructor Create(AGUID: string; AInterface: IInterface);
    property GUID: string read FGUID;
    property &Interface: IInterface read FInterface;
  end;

  {$IFDEF WEBLIB}
  TTMSFNCCloudPlatformServicesList = class(TObjectList)
  private
    function GetItem(Index: Integer): TTMSFNCCloudPlatformServicesService;
    procedure SetItem(Index: Integer; const Value: TTMSFNCCloudPlatformServicesService);
  public
    property Items[Index: Integer]: TTMSFNCCloudPlatformServicesService read GetItem write SetItem; default;
  {$ENDIF}
  {$IFNDEF WEBLIB}
  TTMSFNCCloudPlatformServicesList = class(specialize TFPGObjectList<TTMSFNCCloudPlatformServicesService>)
  {$ENDIF}
  private
    function GetValue(AGUID: string): IInterface;
  public
    function ContainsKey(AGUID: string): Boolean;
    procedure RemoveByGUID(AGUID: string);
    property Interfaces[AGUID: string]: IInterface read GetValue;
  end;

  TTMSFNCCloudBasePlatformServices = class
  private
    FServicesList: TTMSFNCCloudPlatformServicesList;
    class var FCurrent: TTMSFNCCloudBasePlatformServices;
    class var FCurrentReleased: Boolean;
{$IFNDEF AUTOREFCOUNT}
    class procedure ReleaseCurrent;
{$ENDIF}
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddPlatformService(const AServiceGUID: TGUID; const AService: IInterface);
    procedure RemovePlatformService(const AServiceGUID: TGUID);
    function GetPlatformService(const AServiceGUID: TGUID): IInterface;
    function SupportsPlatformService(const AServiceGUID: TGUID): Boolean; overload;
    function SupportsPlatformService(const AServiceGUID: TGUID; var AService: IInterface): Boolean; overload;
    class function Current: TTMSFNCCloudBasePlatformServices;
  end;

function HTTPPostDataBuilder: TTMSFNCCloudBaseRequestPostDataBuilder;
procedure HTTPClearHeaders;
procedure HTTPAddHeader(const AName: string; const AValue: string);
procedure HTTPCloudRequest(const AURL: string; AResultType: TTMSFNCCloudBaseRequestResultType = rrtString; AMethod: TTMSFNCCloudBaseRequestMethod = rmGET; const ARequestResultEvent: TTMSFNCCloudBaseRequestResultEvent = nil); overload;
procedure HTTPCloudRequest(const AHost, APath, AQuery, APostData: string; AResultType: TTMSFNCCloudBaseRequestResultType = rrtString; AMethod: TTMSFNCCloudBaseRequestMethod = rmPOST; const ARequestResultEvent: TTMSFNCCloudBaseRequestResultEvent = nil); overload;

implementation

uses
  StrUtils
  {$IFDEF MSWINDOWS}
  ,LCLTMSFNCCloudBaseWin
  {$ENDIF}
  {$IFDEF UNIX}
  ,LCLTMSFNCCloudBaseUnix
  {$ENDIF}
  {$IFDEF MACOS}
  ,LCLTMSFNCCloudBase.Mac
  {$ENDIF}
  {$IFDEF ANDROID}
  ,LCLTMSFNCCloudBase.Android
  {$ENDIF}
  {$IFDEF WEBLIB}
  ,LCLTMSFNCCloudBase.WEB
  {$ENDIF}
  ;

var
  FCloudBaseGlobal: TTMSFNCCustomCloudBase;
  FPostDataBuilderGlobal: TTMSFNCCloudBaseRequestPostDataBuilder;
  FHeadersGlobal: TTMSFNCCloudBaseRequestHeaders;

function HTTPPostDataBuilder: TTMSFNCCloudBaseRequestPostDataBuilder;
begin
  Result := TTMSFNCCustomCloudBase.GetPostDataBuilderGlobal;
end;

procedure HTTPClearHeaders;
var
  h: TTMSFNCCloudBaseRequestHeaders;
begin
  h := TTMSFNCCustomCloudBase.GetHeadersGlobal;
  h.Clear;
end;

procedure HTTPAddHeader(const AName: string; const AValue: string);
var
  h: TTMSFNCCloudBaseRequestHeaders;
begin
  h := TTMSFNCCustomCloudBase.GetHeadersGlobal;
  h.Add(TTMSFNCCloudBaseRequestHeader.Create(AName, AValue));
end;

procedure HTTPCloudRequest(const AURL: string; AResultType: TTMSFNCCloudBaseRequestResultType = rrtString; AMethod: TTMSFNCCloudBaseRequestMethod = rmGET; const ARequestResultEvent: TTMSFNCCloudBaseRequestResultEvent = nil); overload;
var
  c: TTMSFNCCustomCloudBase;
  I: Integer;
  h: TTMSFNCCloudBaseRequestHeaders;
begin
  c := TTMSFNCCustomCloudBase.GetCloudBaseGlobal;
  if Assigned(c) then
  begin
    c.Request.Clear;
    c.Request.Host := AURL;
    c.Request.Method := AMethod;
    h := TTMSFNCCustomCloudBase.GetHeadersGlobal;
    if Assigned(h) then
    begin
      for I := 0 to h.Count - 1 do
        c.Request.AddHeader(h[I].Name, h[I].Value);
    end;
    c.Request.ResultType := AResultType;
    c.ExecuteRequest(ARequestResultEvent);
  end;
end;

procedure HTTPCloudRequest(const AHost, APath, AQuery, APostData: string; AResultType: TTMSFNCCloudBaseRequestResultType = rrtString; AMethod: TTMSFNCCloudBaseRequestMethod = rmPOST; const ARequestResultEvent: TTMSFNCCloudBaseRequestResultEvent = nil); overload;
var
  c: TTMSFNCCustomCloudBase;
  I: Integer;
  h: TTMSFNCCloudBaseRequestHeaders;
begin
  c := TTMSFNCCustomCloudBase.GetCloudBaseGlobal;
  if Assigned(c) then
  begin
    c.Request.Clear;
    c.Request.Host := AHost;
    c.Request.Path := APath;
    c.Request.Query := AQuery;
    c.Request.Method := AMethod;
    c.Request.PostData := APostData;
    h := TTMSFNCCustomCloudBase.GetHeadersGlobal;
    if Assigned(h) then
    begin
      for I := 0 to h.Count - 1 do
        c.Request.AddHeader(h[I].Name, h[I].Value);
    end;
    c.Request.ResultType := AResultType;
    c.ExecuteRequest(ARequestResultEvent);
  end;
end;

{ TTMSFNCCustomCloudBase }

function TTMSFNCCustomCloudBase.AddRequest(const ARequestResultEvent: TTMSFNCCloudBaseRequestResultEvent = nil; const ARequestResultProgressEvent: TTMSFNCCloudBaseRequestResultProgressEvent = nil; const AAsync: Boolean = True): TTMSFNCCloudBaseRequestResult;
var
  req: TTMSFNCCloudBaseRequestResult;
  t: ITask;
begin
  t := nil;
  req := TTMSFNCCloudBaseRequestResult.Create(Request, ARequestResultEvent, ARequestResultProgressEvent, AAsync, t);
  req.OnRemove := @DoRequestRemove;
  req.OnComplete := @DoRequestComplete;
  req.OnCancelled := @DoRequestCancelled;
  req.OnProgress := @DoRequestProgress;
  req.OnLog := @DoRequestLog;

  if req.IsAsyncAvailable then
  begin
    {$IFDEF LCLWEBLIBTHREAD}
    t := TTask.Create(req, @InternalExecuteRequest);
    {$ENDIF}
    {$IFNDEF LCLWEBLIBTHREAD}
    t := TTask.Create(
    procedure
    begin
      if (req.Task.Status <> TTaskStatus.Canceled) and not IsDestroying then
      begin
        InternalExecuteRequest(req);
      end;
    end);
    {$ENDIF}
  end;

  req.Task := t;

  FRequests.Add(req);

  Result := req;
end;

procedure TTMSFNCCustomCloudBase.Assign(Source: TPersistent);
begin
  if Source is TTMSFNCCustomCloudBase then
  begin
    FLogging := (Source as TTMSFNCCustomCloudBase).Logging;
    FLogFileName := (Source as TTMSFNCCustomCloudBase).LogFileName;
  end
  else
    inherited;
end;

procedure TTMSFNCCustomCloudBase.CancelRequest(
  const ARequest: TTMSFNCCloudBaseRequestResult);
begin
  if Assigned(ARequest) then
    ARequest.Cancel;
end;

procedure TTMSFNCCustomCloudBase.CancelRequests;
var
  I: Integer;
  r: TTMSFNCCloudBaseRequest;
  rr: TTMSFNCCloudBaseRequestResult;
begin
  for I := 0 to RunningRequests.Count - 1 do
  begin
    r := RunningRequests[I];
    if r is TTMSFNCCloudBaseRequestResult then
    begin
      rr := (r as TTMSFNCCloudBaseRequestResult);
      rr.Cancel;
    end;
  end;
end;

constructor TTMSFNCCustomCloudBase.Create;
begin
  Create(nil);
end;

constructor TTMSFNCCustomCloudBase.Create(AOwner: TComponent);
var
  CloudBaseService: ITMSFNCCloudBaseService;
begin
  inherited;
  if TTMSFNCCloudBasePlatformServices.Current.SupportsPlatformService(ITMSFNCCloudBaseService, IInterface(CloudBaseService)) then
    FCloudBase := CloudBaseService.CreateCloudBase(Self);

  FLogging := False;
  FLogFileName := ClassName + '.LOG';

  FRequest := TTMSFNCCloudBaseRequest.Create;
  FRequests := TTMSFNCCloudBaseRequests.Create;

  FService := TTMSFNCCloudBaseService.Create;

  FScopes := TStringList.Create;
end;

class function TTMSFNCCustomCloudBase.GetCloudBaseGlobal: TTMSFNCCustomCloudBase;
begin
  if not Assigned(FCloudBaseGlobal) then
    FCloudBaseGlobal := TTMSFNCCustomCloudBase.Create;

  Result := FCloudBaseGlobal;
end;

class function TTMSFNCCustomCloudBase.GetHeadersGlobal: TTMSFNCCloudBaseRequestHeaders;
begin
  if not Assigned(FHeadersGlobal) then
    FHeadersGlobal := TTMSFNCCloudBaseRequestHeaders.Create;

  Result := FHeadersGlobal;
end;

class function TTMSFNCCustomCloudBase.GetPostDataBuilderGlobal: TTMSFNCCloudBaseRequestPostDataBuilder;
begin
  if not Assigned(FPostDataBuilderGlobal) then
    FPostDataBuilderGlobal := TTMSFNCCloudBaseRequestPostDataBuilder.Create;

  Result := FPostDataBuilderGlobal;
end;

function TTMSFNCCustomCloudBase.GetInstance: NativeUInt;
begin
  Result := HInstance;
end;

function TTMSFNCCustomCloudBase.GetUploadFileSize(
  const ARequest: TTMSFNCCloudBaseRequest): Int64;
begin
  Result := 0;
  if Assigned(FCloudBase) then
    Result := FCloudBase.GetUploadFileSize(ARequest);
end;

function TTMSFNCCustomCloudBase.GetPostDataBuilder: TTMSFNCCloudBaseRequestPostDataBuilder;
begin
  if not Assigned(FPostDataBuilder) then
    FPostDataBuilder := TTMSFNCCloudBaseRequestPostDataBuilder.Create;

  Result := FPostDataBuilder;
end;

function TTMSFNCCustomCloudBase.GetRequestCount(const IsRunning: Boolean = False; const ARequestName: string = ''): Integer;
var
  I: Integer;
  r: TTMSFNCCloudBaseRequest;
  rr: TTMSFNCCloudBaseRequestResult;
begin
  Result := 0;
  for I := 0 to FRequests.Count - 1 do
  begin
    r := FRequests[I];
    if r is TTMSFNCCloudBaseRequestResult then
    begin
      rr := r as TTMSFNCCloudBaseRequestResult;
      if ((IsRunning and not rr.FDone) or not IsRunning) and ((rr.Name = ARequestName) or (ARequestName = '')) then
        Inc(Result);
    end;
  end;
end;

function TTMSFNCCustomCloudBase.GetScopes(ADelimiter: Char; AEncode: Boolean = False): string;
var
  i: integer;
  scopestr: string;
begin
  Result := '';
  for i := 0 to Scopes.Count - 1 do
  begin
    if AEncode then
      scopestr := TTMSFNCUtils.URLEncode(Scopes[i])
    else
      scopestr := Scopes[i];

    if Result = '' then
      Result := scopestr
    else
      Result := Result + ADelimiter + scopestr;
  end;
end;

function TTMSFNCCustomCloudBase.GetVersion: string;
begin
  Result := GetVersionNumber(MAJ_VER, MIN_VER, REL_VER, BLD_VER);
end;

procedure TTMSFNCCustomCloudBase.InternalExecuteRequest(
  const ARequest: TTMSFNCCloudBaseRequestResult);
begin
  if Assigned(FCloudBase) and Assigned(ARequest) then
  begin
    FRequestResult := nil;
    DoRequestStarted(ARequest);
    ARequest.Running := True;
    ARequest.Log('Executing request [' + ARequest.Name + '] with url: [' + ARequest.GetURL + ']');
    FCloudBase.ExecuteRequest(ARequest);
  end;
end;

function TTMSFNCCustomCloudBase.IsLogFileNameStored: Boolean;
begin
  Result := FLogFileName <> ClassName + '.LOG';
end;

procedure TTMSFNCCustomCloudBase.Log(const ARequestResult: TTMSFNCCloudBaseRequestResult; AMessage: string);
var
  {$IFNDEF WEBLIB}
  tf: System.Text;
  fn: string;
  {$ENDIF}
  s: string;
begin
  if not Logging then
    Exit;

  if Assigned(OnRequestLog) then
    OnRequestLog(Self, ARequestResult, AMessage)
  else
  begin
    {$IFDEF FNCLIB}
    s := TTMSFNCUtils.DateTimeToISO(Now) + ': ' + AMessage;
    {$ELSE}
    s := DateTimeToStr(Now) + ': ' + AMessage;
    {$ENDIF}

    {$IFDEF WEBLIB}
    asm
      console.log(s);
    end;
    {$ELSE}
    if LogFileName = '' then
      Exit;

    if (Pos('\', LogFilename) = 0) and (Pos('/', LogFileName) = 0) and (Pos(':', LogFileName) = 0) then
      fn := TTMSFNCUtils.AddBackslash(TTMSFNCUtils.GetDocumentsPath) + LogFileName
    else
      fn := Logfilename;

    AssignFile(tf, fn);
    try
      {$i-}
      Append(tf);
      {$i+}
      if IOResult <> 0 then
      begin
        {$i-}
        Rewrite(tf);
        {$i+}
        if IOResult <> 0 then
          Exit;
      end;

      Writeln(tf, s);
    finally
      if TTextRec(tf).Mode <> fmClosed then
        CloseFile(tf);
    end;
    {$ENDIF}
  end;
end;

procedure TTMSFNCCustomCloudBase.SetScopes(const Value: TStrings);
begin
  FScopes.Assign(Value);
end;

procedure TTMSFNCCustomCloudBase.SetService(
  const Value: TTMSFNCCloudBaseService);
begin
  FService.Assign(Value);
end;

class procedure TTMSFNCCustomCloudBase.SimpleGETAsyncAsString(const AURL: string; const ARequestResultEvent: TTMSFNCCloudBaseRequestResultEvent);
begin
  InternalSimpleGETAsync(AURL, rrtString, ARequestResultEvent);
end;

class procedure TTMSFNCCustomCloudBase.SimpleGETAsyncAsStream(const AURL: string; const ARequestResultEvent: TTMSFNCCloudBaseRequestResultEvent);
begin
  InternalSimpleGETAsync(AURL, rrtStream, ARequestResultEvent);
end;

class procedure TTMSFNCCustomCloudBase.InternalSimpleGETAsync(const AURL: string; const AResultType: TTMSFNCCloudBaseRequestResultType;
  const ARequestResultEvent: TTMSFNCCloudBaseRequestResultEvent);
var
  c: TTMSFNCCustomCloudBase;
begin
  c := GetCloudBaseGlobal;
  if Assigned(c) then
  begin
    c.Request.Clear;
    c.Request.Host := AURL;
    c.Request.ResultType := AResultType;
    c.ExecuteRequest(ARequestResultEvent);
  end;
end;

class function TTMSFNCCustomCloudBase.InternalSimpleGETSync(
  const AURL: string; const AResultType: TTMSFNCCloudBaseRequestResultType): TTMSFNCCloudBaseRequestResult;
var
  c: TTMSFNCCustomCloudBase;
begin
  Result := nil;
  c := GetCloudBaseGlobal;
  if Assigned(c) then
  begin
    c.Request.Clear;
    c.Request.Host := AURL;
    c.Request.ResultType := AResultType;
    Result := {$IFDEF WEBLIB}await(TTMSFNCCloudBaseRequestResult,{$ENDIF} c.ExecuteRequest(nil, nil, False){$IFDEF WEBLIB}){$ENDIF};
  end;
end;

{$IFDEF SUPPORTCURL}
class procedure TTMSFNCCustomCloudBase.InternalCurl(const ACurl: string; ACallbackRequest: TTMSFNCCloudBaseRequestResultEvent; ACallbackString: TTMSFNCCloudBaseRequestResultStringEvent; ACallbackStream: TTMSFNCCloudBaseRequestResultStreamEvent);
var
  c: TTMSFNCCustomCloudBase;
  p: TTMSFNCCloudBaseRequestCurlParser;
begin
  c := GetCloudBaseGlobal;
  if Assigned(c) then
  begin
    p := TTMSFNCCloudBaseRequestCurlParser.Create;
    try
      c.Request.Assign(p.Parse(ACurl));
      if Assigned(ACallbackString) then
        c.Request.ResultType := rrtString;

      c.Request.DataObject := TTMSFNCCloudBaseRequestResultEventWrapper.Create(ACallbackRequest, ACallbackString, ACallbackStream);
    finally
      p.Free;
    end;

    c.ExecuteRequest({$IFDEF LCLWEBLIB}@{$ENDIF}DoCurlRequestComplete);
  end;
end;

class procedure TTMSFNCCustomCloudBase.Curl(const ACurl: string; ACallback: TTMSFNCCloudBaseRequestResultEvent);
begin
  InternalCurl(ACurl, ACallback, nil, nil);
end;

class procedure TTMSFNCCustomCloudBase.Curl(const ACurl: string; ACallback: TTMSFNCCloudBaseRequestResultStringEvent);
begin
  InternalCurl(ACurl, nil, ACallback, nil);
end;

class procedure TTMSFNCCustomCloudBase.Curl(const ACurl: string; ACallback: TTMSFNCCloudBaseRequestResultStreamEvent);
begin
  InternalCurl(ACurl, nil, nil, ACallback);
end;
{$ENDIF}

{$IFNDEF LCLWEBLIB}
class function TTMSFNCCustomCloudBase.SimpleGETSyncAsStream(
  const AURL: string): TMemoryStream;
begin
  Result := InternalSimpleGETSync(AURL, rrtStream).ResultStream;
end;

class function TTMSFNCCustomCloudBase.SimpleGETSyncAsString(
  const AURL: string): string;
begin
  Result := InternalSimpleGETSync(AURL, rrtString).ResultString;
end;
{$ENDIF}

procedure TTMSFNCCustomCloudBase.Test;
begin

end;

destructor TTMSFNCCustomCloudBase.Destroy;
var
  CloudBaseService: ITMSFNCCloudBaseService;
begin
  FreeAndNil(FRequests);
  FreeAndNil(FRequest);
  FreeAndNil(FPostDataBuilder);

  if TTMSFNCCloudBasePlatformServices.Current.SupportsPlatformService(ITMSFNCCloudBaseService, IInterface(CloudBaseService)) then
    CloudBaseService.DestroyCloudBase(FCloudBase);

  FCloudBase := nil;

  FService.Free;
  FScopes.Free;

  inherited;
end;

class procedure TTMSFNCCustomCloudBase.DoCurlRequestComplete(const ARequestResult: TTMSFNCCloudBaseRequestResult);
var
  obj: TTMSFNCCloudBaseRequestResultEventWrapper;
begin
  if Assigned(ARequestResult.DataObject) then
  begin
    obj := TTMSFNCCloudBaseRequestResultEventWrapper(ARequestResult.DataObject);
    if Assigned(obj.FCallbackRequest) then
      obj.FCallbackRequest(ARequestResult);

    if Assigned(obj.FCallbackString) then
      obj.FCallbackString(ARequestResult.ResultString);

    if Assigned(obj.FCallbackStream) then
      obj.FCallbackStream(ARequestResult.ResultStream);
    obj.Free;
  end;
end;

procedure TTMSFNCCustomCloudBase.DoRequestRemove(const ARequestResult: TTMSFNCCloudBaseRequestResult);
begin
  if Assigned(ARequestResult) and ARequestResult.Async then
    FRequests.Remove(ARequestResult);

  if RunningRequests.Count = 0 then
    DoRequestsComplete;
end;

procedure TTMSFNCCustomCloudBase.DoRequestsComplete;
begin
  if Assigned(OnRequestsComplete) then
    OnRequestsComplete(Self);
end;

procedure TTMSFNCCustomCloudBase.DoRequestStarted(
  const ARequestResult: TTMSFNCCloudBaseRequestResult);
begin
  if Assigned(OnRequestStarted) then
    OnRequestStarted(Self, ARequestResult);
end;

class procedure TTMSFNCCustomCloudBase.DownloadFileFromURL(const AURL: string;
  const ARequestResultEvent: TTMSFNCCloudBaseRequestResultEvent);
begin
  InternalSimpleGETAsync(AURL, rrtStream, ARequestResultEvent);
end;

procedure TTMSFNCCustomCloudBase.DoRequestCancelled(
  const ARequestResult: TTMSFNCCloudBaseRequestResult);
begin
  if Assigned(ARequestResult) then
  begin
    if Assigned(OnRequestCancelled) then
      OnRequestCancelled(Self, ARequestResult);
  end;
end;

procedure TTMSFNCCustomCloudBase.DoRequestComplete(
  const ARequestResult: TTMSFNCCloudBaseRequestResult);
begin
  if Assigned(ARequestResult) then
  begin
    ARequestResult.Log('Response code from request [' + ARequestResult.Name + '] is ' + IntTostr(ARequestResult.ResponseCode));
    if (ARequestResult.ResponseCode in ValidHTTPCodes) then
    begin
      case ARequestResult.ResultType of
        rrtString: ARequestResult.Log('Result from request [' + ARequestResult.Name + '] is ' + ARequestResult.ResultString);
        rrtStream: ARequestResult.Log('Result from request [' + ARequestResult.Name + '] is a stream with size ' + TTMSFNCUtils.FormatBytesAsString(ARequestResult.ResultStream.Size));
        rrtFile: ARequestResult.Log('Result from request [' + ARequestResult.Name + '] is a file with location [' + ARequestResult.ResultFile + ']');
      end;
    end;

    ARequestResult.Success := (ARequestResult.ResponseCode in ValidHTTPCodes);

    FRequestResult := ARequestResult;

    if Assigned(OnRequestComplete) then
      OnRequestComplete(Self, ARequestResult);

    ARequestResult.RequestResult;
  end;
end;

procedure TTMSFNCCustomCloudBase.DoRequestLog(
  const ARequestResult: TTMSFNCCloudBaseRequestResult; ALogMessage: string);
begin
  if Assigned(ARequestResult) then
    Log(ARequestResult, ALogMessage);
end;

procedure TTMSFNCCustomCloudBase.DoRequestProgress(
  const ARequestResult: TTMSFNCCloudBaseRequestResult; AProgress: single; AUpload: Boolean);
begin
  if Assigned(ARequestResult) then
  begin
    if Assigned(ARequestResult) then
      ARequestResult.RequestProgress(AProgress, AUpload);
    if Assigned(OnRequestProgress) then
      OnRequestProgress(Self, ARequestResult, AProgress, AUpload);
  end;
end;

procedure TTMSFNCCustomCloudBase.ExecuteAllRequests;
var
  I: Integer;
  req: TTMSFNCCloudBaseRequestResult;
begin
  for I := 0 to FRequests.Count - 1 do
  begin
    req := TTMSFNCCloudBaseRequestResult(FRequests[I]);
    if not req.Running then
    begin
      if req.IsAsyncAvailable then
      begin
        {$IFNDEF LCLWEBLIBTHREAD}
        if not (req.Task.Status = TTaskStatus.Canceled) and not IsDestroying then
        {$ENDIF}
          req.Task.Start
      end
      else
      begin
        InternalExecuteRequest(req);
      end;
    end;
  end;
end;

function TTMSFNCCustomCloudBase.ExecuteRequest(const ARequestResultEvent: TTMSFNCCloudBaseRequestResultEvent = nil; const ARequestResultProgressEvent: TTMSFNCCloudBaseRequestResultProgressEvent = nil; const AAsync: Boolean = True): {$IFDEF WEBLIB}TJSPromise{$ELSE}TTMSFNCCloudBaseRequestResult{$ENDIF};
{$IFDEF WEBLIB}
var
  req: TTMSFNCCloudBaseRequestResult;
{$ENDIF}
begin
  {$IFDEF WEBLIB}
  Result := TJSPromise.New(
    procedure(AResolve, AReject : TJSPromiseResolver)
    begin
      req := AddRequest(ARequestResultEvent, ARequestResultProgressEvent, AAsync);

      if not AAsync then
        req.FResolve := AResolve;

      if Assigned(req) then
        InternalExecuteRequest(req);

      if AAsync then
        AResolve(req);
    end);
  {$ELSE}
  begin
    Result := AddRequest(ARequestResultEvent, ARequestResultProgressEvent, AAsync);

    if Assigned(Result) then
    begin
      if Result.IsAsyncAvailable then
      begin
        {$IFNDEF LCLWEBLIBTHREAD}
        if not (Result.Task.Status = TTaskStatus.Canceled) and not IsDestroying then
        {$ENDIF}
          Result.Task.Start
      end
      else
        InternalExecuteRequest(Result);
    end;
  end;
  {$ENDIF}
end;

{ TTMSFNCCloudBaseFactoryService }

constructor TTMSFNCCloudBaseFactoryService.Create;
begin
  inherited Create;
  FCloudBases := TTMSFNCCloudBaseList.Create;
end;

function TTMSFNCCloudBaseFactoryService.CreateCloudBase(const AValue: TTMSFNCCustomCloudBase): ITMSFNCCustomCloudBase;
begin
  Result := DoCreateCloudBase(AValue);
  FCloudBases.Add(Result);
end;

destructor TTMSFNCCloudBaseFactoryService.Destroy;
begin
  FreeAndNil(FCloudBases);
  inherited Destroy;
end;

procedure TTMSFNCCloudBaseFactoryService.DestroyCloudBase(
  const AValue: ITMSFNCCustomCloudBase);
begin
  DoRemoveCloudBase(AValue);
end;

procedure TTMSFNCCloudBaseFactoryService.DoRemoveCloudBase(
  const AValue: ITMSFNCCustomCloudBase);
begin
  if (FCloudBases <> nil) and (AValue <> nil) then
    FCloudBases.Remove(AValue);
end;

{ TTMSFNCCloudBasePlatformServices }

procedure TTMSFNCCloudBasePlatformServices.AddPlatformService(const AServiceGUID: TGUID; const AService: IInterface);
var
  LService: IInterface;
begin
  if not FServicesList.ContainsKey(GUIDToString(AServiceGUID)) then
  begin
    if Supports(AService, AServiceGUID, LService) then
      FServicesList.Add(TTMSFNCCloudPlatformServicesService.Create(GUIDToString(AServiceGUID), AService));
  end;
end;

constructor TTMSFNCCloudBasePlatformServices.Create;
begin
  inherited;
  FServicesList := TTMSFNCCloudPlatformServicesList.Create;
end;

destructor TTMSFNCCloudBasePlatformServices.Destroy;
begin
  FreeAndNil(FServicesList);
  inherited;
end;

{$IFNDEF AUTOREFCOUNT}
class procedure TTMSFNCCloudBasePlatformServices.ReleaseCurrent;
begin
  FreeAndNil(FCurrent);
  FCurrentReleased := True;
end;
{$ENDIF}

class function TTMSFNCCloudBasePlatformServices.Current: TTMSFNCCloudBasePlatformServices;
begin
  if (FCurrent = nil) and not FCurrentReleased then
    FCurrent := TTMSFNCCloudBasePlatformServices.Create;
  Result := FCurrent;
end;

function TTMSFNCCloudBasePlatformServices.GetPlatformService(const AServiceGUID: TGUID): IInterface;
var
  k: IInterface;
begin
  k := FServicesList.Interfaces[GUIDToString(AServiceGUID)];
  Supports(k, AServiceGUID, Result);
end;

procedure TTMSFNCCloudBasePlatformServices.RemovePlatformService(const AServiceGUID: TGUID);
begin
  FServicesList.RemoveByGUID(GUIDToString(AServiceGUID));
end;

function TTMSFNCCloudBasePlatformServices.SupportsPlatformService(const AServiceGUID: TGUID;
  var AService: IInterface): Boolean;
begin
  if FServicesList.ContainsKey(GUIDToString(AServiceGUID)) then
    Result := Supports(FServicesList.Interfaces[GUIDToString(AServiceGUID)], AServiceGUID, AService)
  else
  begin
    AService := nil;
    Result := False;
  end;
end;

function TTMSFNCCloudBasePlatformServices.SupportsPlatformService(const AServiceGUID: TGUID): Boolean;
begin
  Result := FServicesList.ContainsKey(GUIDToString(AServiceGUID));
end;

{ TTMSFNCCloudBaseRequestHeader }

constructor TTMSFNCCloudBaseRequestHeader.Create(const AName, AValue: string);
begin
  FName := AName;
  FValue := AValue;
end;

{ TTMSFNCCloudBaseRequest }

function TTMSFNCCloudBaseRequest.AddBasicAuthHeader(const AUserName: string; const APassword: string): TTMSFNCCloudBaseRequestHeader;
var
  h: TTMSFNCCloudBaseRequestHeader;
begin
  h := TTMSFNCCloudBaseRequestHeader.Create('Authorization', 'Basic ' + TTMSFNCUtils.Encode64(AUserName + ':' + APassword));
  Headers.Add(h);
  Result := h;
end;

function TTMSFNCCloudBaseRequest.AddHeader(const AName,
  AValue: string): TTMSFNCCloudBaseRequestHeader;
var
  h: TTMSFNCCloudBaseRequestHeader;
begin
  h := TTMSFNCCloudBaseRequestHeader.Create(AName, AValue);
  Headers.Add(h);
  Result := h;
end;

function TTMSFNCCloudBaseRequest.HasUploadFile: Boolean;
begin
  {$IFDEF WEBLIB}
  Result := Assigned(FUploadFile.Data);
  {$ELSE}
  Result := FUploadFile <> '';
  {$ENDIF}
end;

function TTMSFNCCloudBaseRequest.HasUploadStream: Boolean;
begin
  Result := Assigned(FUploadStream) and (FUploadStream.Size > 0);
end;

procedure TTMSFNCCloudBaseRequest.Assign(ARequest: TTMSFNCCloudBaseRequest);
var
  I: Integer;
begin
  FAgent := ARequest.Agent;
  FHost := ARequest.Host;
  FPort := ARequest.Port;
  FPath := ARequest.Path;
  FMethod := ARequest.Method;
  FQuery := ARequest.Query;
  FName := ARequest.Name;
  FPostData := ARequest.PostData;
  FResultType := ARequest.ResultType;
  FResultFileSize := ARequest.ResultFileSize;

  if ARequest.HasUploadStream then
    UploadStream.LoadFromStream(ARequest.UploadStream);

  FUploadFile := ARequest.UploadFile;
  FResultFile := ARequest.ResultFile;
  FCustomHeaders := ARequest.CustomHeaders;

  FDataPointer := ARequest.DataPointer;
  FDataBoolean := ARequest.DataBoolean;
  FDataObject := ARequest.DataObject;
  FDataString := ARequest.DataString;
  FDataUpload := ARequest.DataUpload;
  FDataInteger := ARequest.DataInteger;
  FDataInt64 := ARequest.DataInt64;

  Headers.Clear;
  for I := 0 to ARequest.Headers.Count - 1 do
    Headers.Add(DuplicateHeader(ARequest.Headers[I]));
end;

procedure TTMSFNCCloudBaseRequest.Clear;
begin
  ClearHeaders;
  FHost := '';
  FPort := 0;
  FResultFileSize := -1;
  FPath := '';
  FQuery := '';
  FMethod := rmGET;
  FResultType := rrtString;
  FName := '';
  FPostData := '';
  FDataPointer := nil;
  FDataBoolean := False;
  FDataString := '';
  FDataObject := nil;
  FDataInteger := 0;
  FDataInt64 := 0;
  if Assigned(FUploadStream) then
  begin
    FUploadStream.Clear;
    FUploadStream.Position := 0;
  end;
  {$IFDEF WEBLIB}
  FDataUpload.Data := nil;
  FDataUpload.Name := '';
  FUploadFile.Data := nil;
  FUploadFile.Name := '';
  {$ELSE}
  FUploadFile := '';
  FDataUpload := '';
  {$ENDIF}
  FResultFile := '';
  FCustomHeaders := False;
end;

procedure TTMSFNCCloudBaseRequest.ClearHeaders;
begin
  Headers.Clear;
end;

constructor TTMSFNCCloudBaseRequest.Create;
begin
  FAgent := 'Mozilla/5.001 (windows; U; NT4.0; en-US; rv:1.0) Gecko/25250101';
  FMethod := rmGET;
  FResultType := rrtString;
  FResultFileSize := -1;
end;

destructor TTMSFNCCloudBaseRequest.Destroy;
begin
  FreeAndNil(FHeaders);
  FreeAndNil(FUploadStream);
  inherited;
end;

function TTMSFNCCloudBaseRequest.DuplicateHeader(
  AHeader: TTMSFNCCloudBaseRequestHeader): TTMSFNCCloudBaseRequestHeader;
begin
  Result := TTMSFNCCloudBaseRequestHeader.Create(AHeader.Name, AHeader.Value);
end;

function TTMSFNCCloudBaseRequest.GenerateHeaders: string;
var
  i: integer;
begin
  Result := '';
  for I := 0 to Headers.Count - 1 do
    Result := Result + Headers[I].Name + ': ' + headers[I].Value +#13#10;
end;

function TTMSFNCCloudBaseRequest.GetHeaders: TTMSFNCCloudBaseRequestHeaders;
begin
  if not Assigned(FHeaders) then
    FHeaders := TTMSFNCCloudBaseRequestHeaders.Create;
  Result := FHeaders;
end;

function TTMSFNCCloudBaseRequest.GetMethodString: string;
begin
  case Method of
    rmGET: Result := 'GET';
    rmPOST, rmPOSTMULTIPART, rmPOSTMULTIPARTRELATED: Result := 'POST';
    rmPUT, rmPUTMULTIPART, rmPUTMULTIPARTRELATED, rmUPDATE: Result := 'PUT';
    rmDELETE: Result := 'DELETE';
    rmPATCH: Result := 'PATCH';
  end;
end;

function TTMSFNCCloudBaseRequest.GetPort: Integer;
begin
  Result := Port;
  if Result = 0 then
  begin
    if Pos('HTTPS', UpperCase(Host)) > 0 then
      Result := 443
    else
      Result := 80;
  end;
end;

function TTMSFNCCloudBaseRequest.GetQueryPath: string;
var
  q: string;
begin
  q := '';
  if Query <> '' then
    q := '?' + Query;

  Result := Path + q;
end;

function TTMSFNCCloudBaseRequest.GetServer: string;
var
  firstslash: integer;
  firstat: integer;
  s: string;
begin
  s := Host;

  if Pos('://',UpperCase(s)) > 0 then
    Delete(s, 1, Pos('://', s) + 2);

  firstslash := Pos('/', s);
  firstat := Pos('@', s);

  if (firstat > 0) and (firstat < firstslash) then
    Delete(s, 1, firstat + 1);

  if firstslash > 0 then
    Delete(s, 1, firstslash - 1);

  Result := s;
end;

function TTMSFNCCloudBaseRequest.GetUploadStream: TMemoryStream;
begin
  if not Assigned(FUploadStream) then
    FUploadStream := TMemoryStream.Create;
  Result := FUploadStream;
end;

function TTMSFNCCloudBaseRequest.GetURL: string;
var
  p, q: string;
begin
  p := '';
  if Port > 0 then
    p := ':' + IntToStr(Port);

  q := '';
  if Query <> '' then
    q := '?' + Query;

  Result := Host + p + Path + q;
end;

{ TTMSFNCCloudBaseRequestResult }

procedure TTMSFNCCloudBaseRequestResult.Cancel;
begin
  if Assigned(Task) then
    Task.Cancel;
end;

function TTMSFNCCloudBaseRequestResult.CheckTaskStatus: Boolean;
begin
  if FIsDestroying then
    Result := False
  else
    Result := (IsAsyncAvailable and Assigned(Task) and (Task.Status <> TTaskStatus.Canceled)) or not IsAsyncAvailable;
end;

procedure TTMSFNCCloudBaseRequestResult.Clear;
begin
  inherited;
  FBytesReceived := 0;
  FTotalBytes := 0;
  FSuccess := False;
  FResultString := '';
  FResultStream.Clear;
  FCallBackResult := nil;
  FCallBackProgress := nil;
  FTask := nil;
  FResponseCode := 0;
  FRunning := False;
  FResponseHeaders.Clear;
end;

constructor TTMSFNCCloudBaseRequestResult.Create;
begin
  inherited Create;
end;

constructor TTMSFNCCloudBaseRequestResult.Create(const ARequest: TTMSFNCCloudBaseRequest; const ARequestResultEvent: TTMSFNCCloudBaseRequestResultEvent = nil; const ARequestResultProgressEvent: TTMSFNCCloudBaseRequestResultProgressEvent = nil; const AAsync: Boolean = True; const ATask: ITask = nil);
begin
  inherited Create;
  Assign(ARequest);
  FAsync := AAsync;
  FTask := ATask;
  FCallBackResult := ARequestResultEvent;
  FCallBackProgress := ARequestResultProgressEvent;
  FResultStream := TMemoryStream.Create;
  FResponseHeaders := TTMSFNCCloudBaseRequestHeaders.Create;
end;

destructor TTMSFNCCloudBaseRequestResult.Destroy;
begin
  FIsDestroying := True;

  if Assigned(FTask) then
    FTask.Cancel;

  FTask := nil;
  FCallBackResult := nil;
  FCallBackProgress := nil;
  if Assigned(FResultStream) then
    FResultStream.Free;

  if Assigned(FResponseHeaders) then
    FResponseHeaders.Free;
  inherited;
end;

function TTMSFNCCloudBaseRequestResult.GetResponseHeader(AName: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to ResponseHeaders.Count - 1 do
  begin
    if UpperCase(ResponseHeaders[I].Name) = UpperCase(AName) then
    begin
      Result := ResponseHeaders[I].Value;
      Break;
    end;
  end;
end;

function TTMSFNCCloudBaseRequestResult.IsAsyncAvailable: Boolean;
begin
  Result := Async;
end;

procedure TTMSFNCCloudBaseRequestResult.Log(AMessage: string);
begin
  FMessage := AMessage;
  {$IFNDEF WEBLIB}
  TThread.Queue(nil, @ProcessLog);
  {$ENDIF}
  {$IFDEF WEBLIB}
  ProcessLog;
  {$ENDIF}
end;

procedure TTMSFNCCloudBaseRequestResult.ProcessLog;
begin
  if Assigned(OnLog) then
    OnLog(Self, FMessage);
end;

procedure TTMSFNCCloudBaseRequestResult.ProcessProgress;
begin
  if Assigned(CallBackProgress) then
    CallBackProgress(Self, FProgress);
end;

procedure TTMSFNCCloudBaseRequestResult.ProcessResult;
begin
  if Assigned(CallBackResult) then
    CallBackResult(Self);

  {$IFDEF WEBLIB}
  if Assigned(FResolve) then
    FResolve(Self);
  {$ENDIF}

  if Assigned(OnRemove) then
    OnRemove(Self);
end;

procedure TTMSFNCCloudBaseRequestResult.RequestProgress(AProgress: Single; AUpload: Boolean);
begin
  FProgress := AProgress;
  if IsAsyncAvailable then
  begin
    {$IFNDEF LCLWEBLIBTHREAD}
    if TTask.CurrentTask.Status <> TTaskStatus.Canceled then
    {$ENDIF}
    begin
      {$IFNDEF WEBLIB}
      TThread.Queue(nil, @ProcessProgress);
      {$ENDIF}
      {$IFDEF WEBLIB}
      ProcessProgress;
      {$ENDIF}
    end;
  end
  else
    ProcessProgress;
end;

procedure TTMSFNCCloudBaseRequestResult.RequestResult;
begin
  if IsAsyncAvailable then
  begin
    {$IFNDEF LCLWEBLIBTHREAD}
    if TTask.CurrentTask.Status <> TTaskStatus.Canceled then
    {$ENDIF}
    begin
      {$IFNDEF WEBLIB}
      TThread.Queue(nil, @ProcessResult);
      {$ENDIF}
      {$IFDEF WEBLIB}
      ProcessResult;
      {$ENDIF}
    end;
  end
  else
    ProcessResult;
end;

{$IFDEF WEBLIB}
function TTMSFNCCloudBaseRequests.GetItem(Index: Integer): TTMSFNCCloudBaseRequest;
begin
  Result := TTMSFNCCloudBaseRequest(inherited Items[Index]);
end;

procedure TTMSFNCCloudBaseRequests.SetItem(Index: Integer; const Value: TTMSFNCCloudBaseRequest);
begin
  inherited Items[Index] := Value;
end;

function TTMSFNCCloudBaseRequestHeaders.GetItem(Index: Integer): TTMSFNCCloudBaseRequestHeader;
begin
  Result := TTMSFNCCloudBaseRequestHeader(inherited Items[Index]);
end;

procedure TTMSFNCCloudBaseRequestHeaders.SetItem(Index: Integer; const Value: TTMSFNCCloudBaseRequestHeader);
begin
  inherited Items[Index] := Value;
end;

function TTMSFNCCloudBaseList.GetItem(Index: Integer): ITMSFNCCustomCloudBase;
begin
  Result := ITMSFNCCustomCloudBase(inherited Items[Index]);
end;

procedure TTMSFNCCloudBaseList.SetItem(Index: Integer; const Value: ITMSFNCCustomCloudBase);
begin
  inherited Items[Index] := Value;
end;

function TTMSFNCCloudPlatformServicesList.GetItem(Index: Integer): TTMSFNCCloudPlatformServicesService;
begin
  Result := TTMSFNCCloudPlatformServicesService(inherited Items[Index]);
end;

procedure TTMSFNCCloudPlatformServicesList.SetItem(Index: Integer; const Value: TTMSFNCCloudPlatformServicesService);
begin
  inherited Items[Index] := Value;
end;
{$ENDIF}

{$IFDEF LCLWEBLIBTHREAD}
constructor TTask.Create(const ARequest: TTMSFNCCloudBaseRequestResult; const ARunEvent: TTMSFNCCloudBaseRequestRunEvent);
begin
  FTaskStatus := TTaskStatus.Created;
  FRunEvent := ARunEvent;
  FRequest := ARequest;
end;

destructor TTask.Destroy;
begin
  FRunEvent := nil;
  inherited;
end;

function TTask.GetStatus: TTaskStatus;
begin
  Result := FTaskStatus;
end;

procedure TTask.Cancel;
begin
  {$IFDEF LCLLIBTHREAD}
  FWorkerThread.Terminate;
  {$ENDIF}
  FTaskStatus := TTaskStatus.Canceled;
end;

procedure TTask.Start;
begin
  {$IFDEF LCLLIBTHREAD}
  FWorkerThread := TTaskWorkerThread.Create(FRequest, FRunEvent);
  {$ELSE}
  if Assigned(FRunEvent) then
    FRunEvent(FRequest);
  {$ENDIF}
  FTaskStatus := TTaskStatus.Running;
end;

{$IFDEF LCLLIBTHREAD}
constructor TTaskWorkerThread.Create(const ARequest: TTMSFNCCloudBaseRequestResult; const ARunEvent: TTMSFNCCloudBaseRequestRunEvent);
begin
  inherited Create(False);
  FRequest := ARequest;
  FRunEvent := ARunEvent;
  FreeOnTerminate := False;
end;

type
  TTMSFNCCloudBaseRequestResultOpen = class(TTMSFNCCloudBaseRequestResult);

procedure TTaskWorkerThread.Execute;
begin
  if Assigned(FRunEvent) then
  begin
    FRunEvent(FRequest);
    while not TTMSFNCCloudBaseRequestResultOpen(FRequest).FDone do
      Sleep(1);
  end;
end;
{$ENDIF}
{$ENDIF}

{ TTMSFNCCloudPlatformServicesService }

constructor TTMSFNCCloudPlatformServicesService.Create(AGUID: string;
  AInterface: IInterface);
begin
  FGUID := AGUID;
  FInterface := AInterface;
end;

{ TTMSFNCCloudPlatformServicesList }

function TTMSFNCCloudPlatformServicesList.ContainsKey(AGUID: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Count - 1 do
  begin
    if Items[I].GUID = AGUID then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function TTMSFNCCloudPlatformServicesList.GetValue(AGUID: string): IInterface;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
  begin
    if Items[I].GUID = AGUID then
    begin
      Result := Items[I].&Interface;
      Break;
    end;
  end;
end;

procedure TTMSFNCCloudPlatformServicesList.RemoveByGUID(AGUID: string);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
  begin
    if Items[I].GUID = AGUID then
    begin
      Remove(Items[I]);
      Break;
    end;
  end;
end;

{ TTMSFNCCloudBaseService }

procedure TTMSFNCCloudBaseService.Assign(Source: TPersistent);
begin
  if Source is TTMSFNCCloudBaseService then
  begin
    FBaseURL := (Source as TTMSFNCCloudBaseService).BaseURL;
    FDeveloperURL := (Source as TTMSFNCCloudBaseService).DeveloperURL;
    FName := (Source as TTMSFNCCloudBaseService).Name;
  end
  else
    inherited;
end;

{ TTMSFNCCloudBaseRequestPostDataBuilder }

procedure TTMSFNCCloudBaseRequestPostDataBuilder.AddAttachment(AName: string; AValue: string; ASkipBoundary: Boolean = False; AFileName: string = '';
  AContentType: string = ''; AContentTransferEncoding: string = ''; ASkipLineBreakOnEnd: Boolean = False);
begin
  AddInternalData('attachment', AName, AValue, ASkipBoundary, AFileName, AContentType, AContentTransferEncoding, ASkipLineBreakOnEnd);
end;

procedure TTMSFNCCloudBaseRequestPostDataBuilder.AddContentType(AValue: string);
begin
  FPostData := FPostData + Format('Content-Type: %s' + sRequestLineBreak, [AValue]);
end;

procedure TTMSFNCCloudBaseRequestPostDataBuilder.AddFormData(AName: string; AValue: string; ASkipBoundary: Boolean = False; AFileName: string = ''; AContentType: string = ''; AContentTransferEncoding: string = ''; ASkipLineBreakOnEnd: Boolean = False);
begin
  AddInternalData('form-data', AName, AValue, ASkipBoundary, AFileName, AContentType, AContentTransferEncoding, ASkipLineBreakOnEnd);
end;

procedure TTMSFNCCloudBaseRequestPostDataBuilder.AddHeadBoundary;
begin
  FPostData := FPostData + sRequestHeadBoundary + sRequestLineBreak;
end;

procedure TTMSFNCCloudBaseRequestPostDataBuilder.AddInternalData(ADataType: string; AName: string; AValue: string; ASkipBoundary: Boolean; AFileName: string; AContentType: string; AContentTransferEncoding: string; ASkipLineBreakOnEnd: Boolean);
begin
  if not ASkipBoundary then
    AddHeadBoundary;

  if AContentType <> '' then
    FPostData := FPostData + Format('Content-Type: %s' + sRequestLineBreak, [AContentType]);

  if AContentTransferEncoding <> '' then
    FPostData := FPostData + Format('Content-Transfer-Encoding: %s' + sRequestLineBreak, [AContentTransferEncoding]);

  if (AFileName <> '') and (AName <> '') then
    FPostData := FPostData + Format('Content-Disposition: ' + ADataType + '; name="%s"; filename="%s"' + sRequestLineBreak, [AName, AFileName])
  else if (AName <> '') then
    FPostData := FPostData + Format('Content-Disposition: ' + ADataType + '; name="%s"' + sRequestLineBreak, [AName])
  else if (AFileName <> '') then
    FPostData := FPostData + Format('Content-Disposition: ' + ADataType + '; filename="%s"' + sRequestLineBreak, [AFileName]);

  if AValue <> '' then
  begin
    if not ASkipLineBreakOnEnd then
      FPostData := FPostData + Format(sRequestLineBreak + '%s' + sRequestLineBreak, [AValue])
    else
      FPostData := FPostData + Format(sRequestLineBreak + '%s', [AValue])
  end
  else
    FPostData := FPostData + sRequestLineBreak;
end;

procedure TTMSFNCCloudBaseRequestPostDataBuilder.AddLineBreak;
begin
  FPostData := FPostData + sRequestLineBreak;
end;

procedure TTMSFNCCloudBaseRequestPostDataBuilder.AddTailBoundary;
begin
  FPostData := FPostData + sRequestTailBoundary + sRequestLineBreak;
end;

procedure TTMSFNCCloudBaseRequestPostDataBuilder.AddText(AText: string; ALineBreak: Boolean = True);
begin
  FPostData := FPostData + AText;
  if ALineBreak then
    FPostData := FPostData + sLineBreak;
end;

function TTMSFNCCloudBaseRequestPostDataBuilder.Build: string;
begin
  Result := FPostData;
end;

procedure TTMSFNCCloudBaseRequestPostDataBuilder.Clear;
begin
  FPostData := '';
end;

procedure TTMSFNCCloudBaseRequestPostDataBuilder.Close;
begin
  AddTailBoundary;
end;

{$IFDEF SUPPORTCURL}

{ TTMSFNCCloudBaseRequestCurlParser }

constructor TTMSFNCCloudBaseRequestCurlParser.Create;
begin
  FRequest := TTMSFNCCloudBaseRequest.Create;
end;

destructor TTMSFNCCloudBaseRequestCurlParser.Destroy;
begin
  FreeAndNil(FRequest);
  inherited;
end;

function TTMSFNCCloudBaseRequestCurlParser.Parse(
  ACurl: string): TTMSFNCCloudBaseRequest;
var
  t, l, e, ev, hd, pd: TStringList;
  fl: TStringStream;
  fn: string;
  cmd, vl, vln, vld: string;
  I, K, o, vldp: Integer;
  v, hs: Boolean;
  n, h, q, p: string;
  pt: Integer;
  s: string;
  f, fs, mp: Boolean;
  mt: string;
  fls: TFileStream;
  md: TTMSFNCCloudBaseRequestCurlParserMode;
  qp: Boolean;
  obj: TTMSFNCCloudBaseRequestCurlParserFile;
  ob: TObject;
  pdb: TStringStream;
begin
  Result := nil;
  if not Assigned(FRequest) then
    Exit;

  FRequest.Clear;
  FRequest.ResultType := rrtStream;

  v := False;
  cmd := ACurl;
  cmd := StringReplace(cmd, #13#10, '', [rfReplaceAll]);
  t := TStringList.Create('''', ' ');
  l := TStringList.Create('''', ' ');
  pd := TStringList.Create;
  pd.Delimiter := '&';
  pd.TrailingLineBreak := False;
  pd.QuoteChar := #0;
  pd.StrictDelimiter := True;

  try
    s := cmd;
    cmd := '';
    f := False;
    fs := False;
    {$IFDEF ZEROSTRINGINDEX}
    o := 1;
    {$ELSE}
    o := 0;
    {$ENDIF}
    I := 1;
    while I <= Length(s) do
    begin
      if (s[I - o] = '''') then
        fs := not fs;

      if (s[I - o] = '"') and not fs then
      begin
        if not f then
        begin
          f := True;
          s[I - o] := '''';
        end
        else if f then
        begin
          f := False;
          s[I - o] := '''';
        end;
      end;

      cmd := cmd + s[I - o];
      Inc(I);
    end;

    md := cpmNone;
    mp := False;
    qp := False;

    t.DelimitedText := cmd;

    for I := 0 to t.Count - 1 do
    begin
      vl := Trim(t[I]);

      if vl = '\' then
        Continue;

      if md = cpmNone then
      begin
        if (vl = '-H') or (vl = '--header') then
          md := cpmHeader
        else if (vl = '-g') or (vl = '--get') then
          qp := True
        else if (vl = '-F') or (vl = '--form') then
          md := cpmFormData
        else if (vl = '-d') or (vl = '--data') or (vl = '--data-ascii') or (vl = '--json') then
          md := cpmData
        else if (vl = '--data-binary') then
          md := cpmDataBinary
        else if (vl = '--data-urlencode') then
          md := cpmDataUrlEncode
        else if (vl = '--data-raw') then
          md := cpmDataRaw
        else if (vl = '-u') or (vl = '--user') then
          md := cpmUser
        else if (vl = '--oauth2-bearer') then
          md := cpmBearer
        else if (vl = '-X') or (vl = '--request') then
          md := cpmRequest
        else if (vl = '-o') or (vl = '--output') then
          md := cpmResultFile
        else
          l.Add(vl);
      end
      else
      begin
        case md of
          cpmHeader:
          begin
            hd := TStringList.Create;
            try
              TTMSFNCUtils.Split(':', vl, hd, True);
              if hd.Count = 1 then
                FRequest.AddHeader(Trim(hd[0]), '')
              else if hd.Count > 1 then
              begin
                s := '';
                for K := 1 to hd.Count - 1 do
                begin
                  s := s + hd[K];
                  if K < hd.Count - 1 then
                    s := s + ':';
                end;
                FRequest.AddHeader(Trim(hd[0]), Trim(s));
              end;
            finally
              hd.Free;
            end;
          end;
          cpmFormData, cpmData, cpmDataRaw, cpmDataUrlEncode:
          begin
            if md = cpmFormData then
              mp := True;

            if mp then
              ev := TStringList.Create('''', ';')
            else
              ev := TStringList.Create('''', '&');

            try
              ev.StrictDelimiter := True;
              ev.DelimitedText := vl;

              for K := 0 to ev.Count - 1 do
              begin
                e := TStringList.Create('''', '=');
                e.StrictDelimiter := True;
                try
                  e.DelimitedText := ev[K];

                  vln := '';
                  vld := '';

                  if e.Count = 2 then
                  begin
                    vln := e[0];
                    vld := e[1];
                  end
                  else if e.Count = 1 then
                  begin
                    vld := e[0];
                  end;

                  obj := nil;

                  if (md <> cpmDataRaw) and not TTMSFNCUtils.IsJSON(vld) then
                  begin
                    vldp := Pos('@', vld);
                    if (vldp > 0) then
                    begin
                      if vln = '' then
                        vln := Copy(vld, 1, vldp - 1);

                      fn := Copy(vld, vldp + 1, Length(vld) - vldp);
                      if FileExists(fn) then
                      begin
                        fl := TStringStream.Create;
                        try
                          if not mp then
                          begin
                            fl.LoadFromFile(fn);
                            vld := fl.DataString;
                          end;
                          obj := TTMSFNCCloudBaseRequestCurlParserFile.Create;
                          obj.FFileName := fn;
                        finally
                          fl.Free;
                        end;
                      end;
                    end;
                  end;

                  if md = cpmDataUrlEncode then
                    vld := TTMSFNCUtils.URLEncode(vld);

                  if (vln <> '') and (vld <> '') then
                    pd.AddObject(vln + '=' + vld, obj)
                  else if (vld <> '') then
                    pd.AddObject(vld, obj);
                finally
                  e.Free;
                end;
              end;
            finally
              ev.free;
            end;
          end;
          cpmBearer: FRequest.AddHeader('Authorization', 'Bearer ' + vl);
          cpmUser: FRequest.AddHeader('Authorization', 'Basic ' + TTMSFNCUtils.Encode64(vl));
          cpmResultFile:
          begin
            FRequest.ResultType := rrtFile;
            FRequest.ResultFile := vl;
          end;
          cpmRequest:
          begin
            if UpperCase(vl) = 'GET' then
              FRequest.Method := rmGET
            else if UpperCase(vl) = 'POST' then
              FRequest.Method := rmPOST
            else if UpperCase(vl) = 'PUT' then
              FRequest.Method := rmPUT
            else if UpperCase(vl) = 'DELETE' then
              FRequest.Method := rmDELETE;
          end;
        end;

        md := cpmNone;
      end;
    end;

    for I := 0 to l.Count - 1 do
    begin
      vl := l[I];

      if not v and (UpperCase(vl) = 'CURL') then
        v := True
      else if v and (Pos('http', LowerCase(vl)) > 0) then
      begin
        h := '';
        q := '';
        p := '';
        pt := 0;
        TTMSFNCUtils.SplitURL(vl, h, p, q, pt);
        if h <> '' then
          FRequest.Host := h;

        if q <> '' then
          FRequest.Query := q;

        if p <> '' then
          FRequest.Path := p;

        if pt <> 0 then
          FRequest.Port := pt;
      end;
    end;

    if not mp then
    begin
      if qp then
        FRequest.Query := FRequest.Query + '&' + pd.DelimitedText
      else
        FRequest.PostData := pd.DelimitedText
    end
    else
    begin
      pdb := TStringStream.Create;
      try
        for I := 0 to pd.Count - 1 do
        begin
          if I > 0 then
            pdb.WriteString(sRequestLineBreak + sRequestHeadBoundary + sRequestLineBreak)
          else
            pdb.WriteString(sRequestHeadBoundary + sRequestLineBreak);

          ob := pd.Objects[I];
          n := pd.Names[I];
          if Assigned(ob) and (ob is TTMSFNCCloudBaseRequestCurlParserFile) then
          begin
            obj := ob as TTMSFNCCloudBaseRequestCurlParserFile;
            try
              mt := TTMSFNCUtils.GetMimeType(obj.FFileName) ;
              if mt = '' then
                mt := 'application/octet-stream';

              pdb.WriteString('Content-Type: ' + mt + sRequestLineBreak);
              pdb.WriteString('Content-Disposition: form-data; name="' + n + '"; filename="' + obj.FFileName + '"' + sRequestLineBreak);
              pdb.WriteString(sRequestLineBreak);
              fls := TFileStream.Create(obj.FFileName, fmOpenRead or fmShareDenyWrite);
              try
                pdb.CopyFrom(fls, 0);
              finally
                fls.Free;
              end;
            finally
              obj.Free;
            end;
          end
          else
          begin
            pdb.WriteString('Content-Disposition: form-data; name="' + n + '"' + sRequestLineBreak);
            pdb.WriteString(sRequestLineBreak);
            pdb.WriteString(pd.Values[n]);
          end;
        end;

        pdb.WriteString(sRequestLineBreak + sRequestTailBoundary + sRequestLineBreak);
        FRequest.PostData := pdb.DataString;
      finally
        pdb.free;
      end;

    end;

    if FRequest.PostData <> '' then
    begin
      if (FRequest.Method = rmGET) then
        FRequest.Method := rmPOST;

      hs := False;
      for I := 0 to FRequest.Headers.Count - 1 do
      begin
        if UpperCase(FRequest.Headers[I].Name) = 'CONTENT-TYPE' then
        begin
          hs := True;
          Break;
        end;
      end;

      if not hs then
      begin
        if TTMSFNCUtils.IsJSON(FRequest.PostData) then
          FRequest.AddHeader('Content-Type', 'application/json')
        else
        begin
          if not mp then
            FRequest.AddHeader('Content-Type', 'application/x-www-form-urlencoded')
          else
            FRequest.AddHeader('Content-Type', 'multipart/form-data; boundary=' + sRequestBoundary);
        end;
      end;
    end;

    if not v then
      raise Exception.Create('Not a valid curl command');

  finally
    t.Free;
    l.Free;
    pd.Free;
  end;

  Result := FRequest;
end;
{$ENDIF}

{ TTMSFNCCloudBaseRequestResultEventWrapper }

constructor TTMSFNCCloudBaseRequestResultEventWrapper.Create(ACallbackRequest: TTMSFNCCloudBaseRequestResultEvent; ACallbackString: TTMSFNCCloudBaseRequestResultStringEvent; ACallbackStream: TTMSFNCCloudBaseRequestResultStreamEvent);
begin
  FCallbackRequest := ACallbackRequest;
  FCallbackString := ACallbackString;
  FCallbackStream := ACallbackStream;
end;

initialization
begin
  TTMSFNCCloudBasePlatformServices.FCurrentReleased := False;
  RegisterCloudBaseService;
end;

{$IFNDEF WEBLIB}
finalization
begin
  if Assigned(FCloudBaseGlobal) then
  begin
    FCloudBaseGlobal.Free;
    FCloudBaseGlobal := nil;
  end;

  if Assigned(FHeadersGlobal) then
  begin
    FHeadersGlobal.Free;
    FHeadersGlobal := nil;
  end;

  if Assigned(FPostDataBuilderGlobal) then
  begin
    FPostDataBuilderGlobal.Free;
    FPostDataBuilderGlobal := nil;
  end;

  UnRegisterCloudBaseService;
{$IFNDEF AUTOREFCOUNT}
  TTMSFNCCloudBasePlatformServices.ReleaseCurrent;
{$ENDIF}
end;
{$ENDIF}

end.

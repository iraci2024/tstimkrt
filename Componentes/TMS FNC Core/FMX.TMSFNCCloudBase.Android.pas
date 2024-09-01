{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{            copyright (c) 2019 - 2021                               }
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

unit FMX.TMSFNCCloudBase.Android;

{$I FMX.TMSFNCDefines.inc}

interface

{$DEFINE USEJSOBJECT}

procedure RegisterCloudBaseService;
procedure UnRegisterCloudBaseService;

implementation

uses
  Classes, SysUtils, Threading, FMX.TMSFNCCloudBase, FMX.TMSFNCUtils, FMX.TMSFNCWebBrowser
  {$IFDEF ANDROID}
  ,AndroidApi.JNI, AndroidApi.JNI.JavaTypes, AndroidApi.JNI.Java.Security, AndroidApi.JNIBridge, AndroidApi.Helpers
  ,AndroidApi.JNI.GraphicsContentViewText, AndroidApi.JNI.App
  {$HINTS OFF}
  {$IF COMPILERVERSION >= 30}
  ,AndroidApi.JNI.Java.Net
  {$IFEND}
  {$HINTS ON}
  {$ENDIF}
  ;

type
  {$IFDEF ANDROID}
  JURL = interface;
  {$HINTS OFF}
  {$IF COMPILERVERSION < 30}
  JSSLContext = interface;
  JTrustManager = interface;
  JSSLEngine = interface;
  JSSLSessionContext = interface;
  JSSLParameters = interface;
  JSSLServerSocketFactory = interface;
  Jssl_SSLSocketFactory = interface;
  JKeyManager = interface;
  JKeyStore = interface;
  JManagerFactoryParameters = interface;
  JSecureRandom = interface;
  JX509TrustManager = interface;

  [JavaSignature('javax/net/ssl/SSLEngine')]
  JSSLEngine = interface(JObject)
    ['{98CD6E2D-607F-4566-90B2-0A0F445EA0EB}']
  end;

  [JavaSignature('javax/net/ssl/SSLSessionContext')]
  JSSLSessionContext = interface(JObject)
    ['{59151545-8194-4967-9448-8BA1CA868932}']
  end;

  [JavaSignature('javax/net/ssl/SSLParameters')]
  JSSLParameters = interface(JObject)
    ['{37321D29-64DD-42EB-84A3-2FA5AA737AD4}']
  end;

  [JavaSignature('javax/net/ssl/SSLServerSocketFactory')]
  JSSLServerSocketFactory = interface(JObject)
    ['{EB80C487-856B-4B82-9680-1406F58DA3E3}']
  end;

  [JavaSignature('javax/net/ssl/KeyManager')]
  JKeyManager = interface(JObject)
    ['{669EEE8D-CF62-49C3-874F-6DD0C8CC9385}']
  end;

  [JavaSignature('javax/net/ssl/SSLSocketFactory')]
  Jssl_SSLSocketFactory = interface(JObject)
    ['{AE2A578D-D861-4F9D-8574-1E5089C3C093}']
  end;

  [JavaSignature('java/security/KeyStore')]
  JKeyStore = interface(JObject)
    ['{51332F92-DC3D-45E2-8EB6-65DEB2F497C9}']
  end;

  [JavaSignature('javax/net/ssl/ManagerFactoryParameters')]
  JManagerFactoryParameters = interface(JObject)
    ['{E5CF79CA-4B41-42D6-9DA6-29211FFF2A64}']
  end;

  [JavaSignature('java/security/Provider')]
  JProvider = interface(JObject)
    ['{8EEC1F7E-117E-4B8B-B996-2AF980F902B6}']
  end;

  JSecureRandomClass = interface(JObjectClass)
    ['{1648A030-95B7-45DF-980E-81A359E02971}']
    function init: JSecureRandom; cdecl;
  end;

  [JavaSignature('java/security/SecureRandom')]
  JSecureRandom = interface(JObject)
    ['{B61805B2-28C1-4158-9945-FCE2065F1414}']
  end;
  TJSecureRandom = class(TJavaGenericImport<JSecureRandomClass, JSecureRandom>) end;

  [JavaSignature('java/security/cert/Certificate')]
  JCertificate = interface(JObject)
    ['{09F6B021-E3AD-41D8-A661-BEA7863D61F3}']
  end;

  [JavaSignature('java/security/cert/X509Certificate')]
  JX509Certificate = interface(JCertificate)
    ['{38B67199-2FBF-4725-BBFB-ED05A7CE742F}']
  end;

  JSSLContextClass = interface(JObjectClass)
    ['{ECA2D26C-92C5-4B93-939E-8A26B85CA07D}']
    {class} function getDefault: JSSLContext; cdecl;
    {class} function getInstance(protocol: JString): JSSLContext; cdecl; overload;
    {class} function getInstance(protocol: JString; provider: JString): JSSLContext; cdecl; overload;
    {class} function getInstance(protocol: JString; provider: JProvider): JSSLContext; cdecl; overload;
    {class} procedure setDefault(sslContext: JSSLContext); cdecl;
  end;

  [JavaSignature('javax/net/ssl/SSLContext')]
  JSSLContext = interface(JObject)
    ['{265755CC-73B3-4A0E-9211-049A7B57809E}']
    function createSSLEngine: JSSLEngine; cdecl; overload;
    function createSSLEngine(peerHost: JString; peerPort: Integer): JSSLEngine; cdecl; overload;
    function getClientSessionContext: JSSLSessionContext; cdecl;
    function getDefaultSSLParameters: JSSLParameters; cdecl;
    function getProtocol: JString; cdecl;
    function getProvider: JProvider; cdecl;
    function getServerSessionContext: JSSLSessionContext; cdecl;
    function getServerSocketFactory: JSSLServerSocketFactory; cdecl;
    function getSocketFactory: Jssl_SSLSocketFactory; cdecl;
    function getSupportedSSLParameters: JSSLParameters; cdecl;
    procedure init(km: TJavaObjectArray<JKeyManager>; tm: TJavaObjectArray<JTrustManager>; sr: JSecureRandom); cdecl;
  end;
  TJSSLContext = class(TJavaGenericImport<JSSLContextClass, JSSLContext>) end;

  JTrustManagerFactory = interface;

  JTrustManagerClass = interface(IJavaClass)
    ['{59EC3810-55EE-498A-8B0A-72A03C4675D0}']
  end;

  [JavaSignature('javax/net/ssl/TrustManager')]
  JTrustManager = interface(IJavaInstance)
    ['{22BCC50A-E200-4B61-BEDD-10B84E9874FF}']
  end;
  TJTrustManager = class(TJavaGenericImport<JTrustManagerClass, JTrustManager>) end;

  JTrustManagerFactoryClass = interface(JObjectClass)
    ['{0BE1CDA3-C6D6-4A69-88EE-C349F77BEAB5}']
    {class} function getDefaultAlgorithm: JString; cdecl;
    {class} function getInstance(algorithm: JString): JTrustManagerFactory; cdecl; overload;
    {class} function getInstance(algorithm: JString; provider: JString): JTrustManagerFactory; cdecl; overload;
    {class} function getInstance(algorithm: JString; provider: JProvider): JTrustManagerFactory; cdecl; overload;
  end;

  [JavaSignature('javax/net/ssl/TrustManagerFactory')]
  JTrustManagerFactory = interface(JObject)
    ['{F907542C-2731-4410-93E0-76986A871C2D}']
    function getAlgorithm: JString; cdecl;
    function getProvider: JProvider; cdecl;
    function getTrustManagers: TJavaObjectArray<JTrustManager>; cdecl;
    procedure init(ks: JKeyStore); cdecl; overload;
    procedure init(spec: JManagerFactoryParameters); cdecl; overload;
  end;
  TJTrustManagerFactory = class(TJavaGenericImport<JTrustManagerFactoryClass, JTrustManagerFactory>) end;

  JX509TrustManagerClass = interface(JTrustManagerClass)
    ['{4AA4EEC0-BBDA-4FB0-B4D2-9C6137366AE3}']
  end;

  [JavaSignature('javax/net/ssl/X509TrustManager')]
  JX509TrustManager = interface(JTrustManager)
    ['{DD78B3A0-C3D0-4F84-95AE-7023A756C402}']
    procedure checkClientTrusted(chain: TJavaObjectArray<JX509Certificate>; authType: JString); cdecl;
    procedure checkServerTrusted(chain: TJavaObjectArray<JX509Certificate>; authType: JString); cdecl;
    function getAcceptedIssuers: TJavaObjectArray<JX509Certificate>; cdecl;
  end;
  TJX509TrustManager = class(TJavaGenericImport<JX509TrustManagerClass, JX509TrustManager>) end;
  {$IFEND}
  {$HINTS ON}

  JURLConnection = interface;

  JURLConnectionClass = interface(JObjectClass)
    ['{AEDB8433-1DD1-4B20-BECC-123544789307}']
  end;

  [JavaSignature('java/net/URLConnection')]
  JURLConnection = interface(JObject)
    ['{65F78131-FB82-416E-A50A-AAEFD1B2DD77}']
    procedure addRequestProperty(field: JString; newValue: JString); cdecl;
    procedure setRequestProperty(field: JString; newValue: JString); cdecl;
    procedure setDoInput(newValue: Boolean); cdecl;
    procedure setDoOutput(newValue: Boolean); cdecl;
    function getURL: JURL; cdecl;
    function getDoOutput: Boolean; cdecl;
    function getDoInput: Boolean; cdecl;
    function getOutputStream: JOutputStream; cdecl;
    function getInputStream: JInputStream; cdecl;
    function getErrorStream: JInputStream; cdecl;
    function getResponseCode: Integer; cdecl;
    function getHeaderField(name: JString): JString; cdecl; overload;
    function getHeaderField(n: Integer): JString; cdecl; overload;
    function getHeaderFieldKey(n: Integer): JString; cdecl;
    function getHeaderFields: JMap; cdecl;
    function getContentLength: Integer; cdecl;
  end;
  TJURLConnection = class(TJavaGenericImport<JURLConnectionClass, JURLConnection>) end;

  JHttpURLConnectionClass = interface(JURLConnectionClass)
    ['{5AB6376E-D394-4886-BE20-1810DBB6467E}']
  end;

  [JavaSignature('java/net/HttpURLConnection')]
  JHttpURLConnection = interface(JURLConnection)
  ['{72EC2B81-5962-48B0-8A78-4EF19F2256AC}']
    procedure setRequestMethod(method: JString); cdecl;
  end;
  TJHttpURLConnection = class(TJavaGenericImport<JHttpURLConnectionClass, JHttpURLConnection>) end;

  JHttpsURLConnectionClass = interface(JHttpURLConnectionClass)
    ['{719415C6-1391-4833-A637-AE58845D9367}']
    procedure setDefaultSSLSocketFactory(sf: Jssl_SSLSocketFactory); cdecl;
  end;

  [JavaSignature('javax/net/ssl/HttpsURLConnection')]
  JHttpsURLConnection = interface(JHttpURLConnection)
    ['{0FC7F513-21AA-4769-9818-D91CAA852447}']
  end;
  TJHttpsURLConnection = class(TJavaGenericImport<JHttpsURLConnectionClass, JHttpsURLConnection>) end;

  JDataOutputStream = interface;

  JDataOutputStreamClass = interface(JOutputStreamClass)
  ['{17073AD7-9BB5-4049-B118-001EC934C341}']
    function init(ot: JOutputStream): JDataOutputStream; cdecl;
  end;

  [JavaSignature('java/io/DataOutputStream')]
  JDataOutputStream = interface(JOutputStream)
  ['{72EC2B81-5962-48B0-8A78-4EF19F2256AC}']
  procedure flush; cdecl;
  procedure close; cdecl;
  procedure writeBytes(input: JString); cdecl;
  procedure write(buffer: TJavaArray<Byte>; offset: Integer; count: Integer); cdecl;
  end;
  TJDataOutputStream = class(TJavaGenericImport<JDataOutputStreamClass, JDataOutputStream>) end;

  TX509TrustManager = class(TJavaLocal, JX509TrustManager)
  public
    procedure checkClientTrusted(chain: TJavaObjectArray<JX509Certificate>; authType: JString); cdecl;
    procedure checkServerTrusted(chain: TJavaObjectArray<JX509Certificate>; authType: JString); cdecl;
    function getAcceptedIssuers: TJavaObjectArray<JX509Certificate>; cdecl;
  end;

  JURLClass = interface(JObjectClass)
  ['{36566040-887E-4669-A2C2-5E398BAB11A5}']
    function init(spec: JString): JURL; cdecl;
  end;

  [JavaSignature('java/net/URL')]
  JURL = interface(JObject)
  ['{90838206-3D5E-4E46-84DA-9B5E2D913242}']
  function openConnection: JURLConnection; cdecl;
  end;
  TJURL = class(TJavaGenericImport<JURLClass, JURL>) end;

  JStringWriter = interface;

  JStringWriterClass = interface(JObjectClass)
  ['{41443AA3-D415-4869-BA63-E523EDCA533F}']
    function init: JStringWriter; cdecl;
  end;

  [JavaSignature('java/io/StringWriter')]
  JStringWriter = interface(JObject)
  ['{1D7F9DB3-1146-4AE7-8698-65B2A1E56179}']
    procedure write(chars: TJavaArray<Char>; offset: Integer; count: Integer); cdecl;
  end;
  TJStringWriter = class(TJavaGenericImport<JStringWriterClass, JStringWriter>) end;

  JInputStreamReader = interface;

  JInputStreamReaderClass = interface(JObjectClass)
  ['{41443AA3-D415-4869-BA63-E523EDCA533F}']
    function init(input: JInputStream; charSetName: JString): JInputStreamReader; cdecl; overload;
    function init(input: JInputStream): JInputStreamReader; cdecl; overload;
  end;

  [JavaSignature('java/io/InputStreamReader')]
  JInputStreamReader = interface(JObject)
  ['{1D7F9DB3-1146-4AE7-8698-65B2A1E56179}']
    function read(buffer: TJavaArray<Byte>; byteOffset: Integer; byteCount: Integer): Integer; cdecl;
  end;
  TJInputStreamReader = class(TJavaGenericImport<JInputStreamReaderClass, JInputStreamReader>) end;

  JReader = interface;

  JReaderClass = interface(JObjectClass)
  ['{B600CC99-D89B-444A-8BCF-72AB33342B41}']
  end;

  [JavaSignature('java/io/Reader')]
  JReader = interface(JObject)
  ['{C449988A-B22E-4E3D-916E-BC7B65A25605}']
  end;
  TJReader = class(TJavaGenericImport<JReaderClass, JReader>) end;

  JBufferedReader = interface;

  JBufferedReaderClass = interface(JObjectClass)
  ['{6859FEC1-4D00-46D1-9252-94D4520D8374}']
    function init(input: JReader; size: Integer): JBufferedReader; cdecl; overload;
    function init(input: JReader): JBufferedReader; cdecl; overload;
  end;

  [JavaSignature('java/io/BufferedReader')]
  JBufferedReader = interface(JObject)
  ['{F2C9403F-2318-426B-A0B6-610B80D2F6D5}']
    function read(buf: TJavaArray<Char>): Integer; cdecl;
    function readLine: JString; cdecl;
  end;
  TJBufferedReader = class(TJavaGenericImport<JBufferedReaderClass, JBufferedReader>) end;

  JOutputStreamWriter = interface;

  JOutputStreamWriterClass = interface(JWriterClass)
  ['{587CF1E2-A3AE-4F79-B51A-136F41F8B316}']
    function init(output: JOutputStream): JOutputStreamWriter; cdecl; overload;
    function init(output: JOutputStream; charSetName: JString): JOutputStreamWriter; cdecl; overload;
  end;

  [JavaSignature('java/io/OutputStreamWriter')]
  JOutputStreamWriter = interface(JWriter)
  ['{280CDCFF-8181-49A7-9C0E-0B848C0D40E3}']
    procedure write(buffer: TJavaArray<Byte>; byteOffset: Integer; byteCount: Integer); cdecl; overload;
    procedure write(buffer: JString; byteOffset: Integer; byteCount: Integer); cdecl; overload;
  end;
  TJOutputStreamWriter = class(TJavaGenericImport<JOutputStreamWriterClass, JOutputStreamWriter>) end;

  JBufferedInputStream = interface;

  JBufferedInputStreamClass = interface(JInputStreamClass)
  ['{AA4B8D6A-D8D4-4FBC-9B8A-599A1B5B4E11}']
  function init(input: JInputStream): JBufferedInputStream; cdecl;
  end;

  [JavaSignature('java/io/BufferedInputStream')]
  JBufferedInputStream = interface(JInputStream)
  ['{FCD2D018-73C2-4B22-9C0C-6079ABFED67A}']
  end;
  TJBufferedInputStream = class(TJavaGenericImport<JBufferedInputStreamClass, JBufferedInputStream>) end;

  JBufferedOutputStream = interface;

  JBufferedOutputStreamClass = interface(JOutputStreamClass)
  ['{AA4B8D6A-D8D4-4FBC-9B8A-599A1B5B4E11}']
  function init(ouput: JOutputStream): JBufferedOutputStream; cdecl;
  end;

  [JavaSignature('java/io/BufferedOutputStream')]
  JBufferedOutputStream = interface(JOutputStream)
  ['{FCD2D018-73C2-4B22-9C0C-6079ABFED67A}']
  end;
  TJBufferedOutputStream = class(TJavaGenericImport<JBufferedOutputStreamClass, JBufferedOutputStream>) end;

  JStrictMode = interface;
  JStrictMode_ThreadPolicy = interface;
  JThreadPolicy_Builder = interface;

  JStrictModeClass = interface(JObjectClass)
    ['{72058E60-7E55-4508-AF2E-5BD7E7C403FC}']
    {class} function allowThreadDiskReads: JStrictMode_ThreadPolicy; cdecl;
    {class} function allowThreadDiskWrites: JStrictMode_ThreadPolicy; cdecl;
    {class} procedure enableDefaults; cdecl;
    {class} function getThreadPolicy: JStrictMode_ThreadPolicy; cdecl;
    {class} procedure noteSlowCall(name: JString); cdecl;
    {class} procedure setThreadPolicy(policy: JStrictMode_ThreadPolicy); cdecl;
  end;

  [JavaSignature('android/os/StrictMode')]
  JStrictMode = interface(JObject)
    ['{1F64CDA6-1C00-4A3B-9BE9-7D1C52A1BD6D}']
  end;

  TJStrictMode = class(TJavaGenericImport<JStrictModeClass, JStrictMode>) end;

  JStrictMode_ThreadPolicyClass = interface(JObjectClass)
    ['{F2812595-5847-403D-BAD2-38E2B01DE4A0}']
    {class} function _GetLAX: JStrictMode_ThreadPolicy; cdecl;
    {class} property LAX: JStrictMode_ThreadPolicy read _GetLAX;
  end;

  [JavaSignature('android/os/StrictMode$ThreadPolicy')]
  JStrictMode_ThreadPolicy = interface(JObject)
    ['{48313D4D-ADB1-4A08-BD39-9A5B8969E37D}']
    function toString: JString; cdecl;
  end;

  TJStrictMode_ThreadPolicy = class(TJavaGenericImport<JStrictMode_ThreadPolicyClass, JStrictMode_ThreadPolicy>) end;

  JThreadPolicy_BuilderClass = interface(JObjectClass)
    ['{4A13A7C1-770E-4484-9736-F87FF5903F8A}']
    {class} function init: JThreadPolicy_Builder; cdecl; overload;
    {class} function init(policy: JStrictMode_ThreadPolicy): JThreadPolicy_Builder; cdecl; overload;
    {class} function detectCustomSlowCalls: JThreadPolicy_Builder; cdecl;
    {class} function detectDiskReads: JThreadPolicy_Builder; cdecl;
    {class} function detectDiskWrites: JThreadPolicy_Builder; cdecl;
    {class} function penaltyDeathOnNetwork: JThreadPolicy_Builder; cdecl;
    {class} function penaltyDialog: JThreadPolicy_Builder; cdecl;
    {class} function penaltyDropBox: JThreadPolicy_Builder; cdecl;
    {class} function permitCustomSlowCalls: JThreadPolicy_Builder; cdecl;
    {class} function permitDiskReads: JThreadPolicy_Builder; cdecl;
    {class} function permitDiskWrites: JThreadPolicy_Builder; cdecl;
  end;

  [JavaSignature('android/os/StrictMode$ThreadPolicy$Builder')]
  JThreadPolicy_Builder = interface(JObject)
    ['{726B326D-EF0A-4E64-97E4-199B041B97B6}']
    function build: JStrictMode_ThreadPolicy; cdecl;
    function detectAll: JThreadPolicy_Builder; cdecl;
    function detectNetwork: JThreadPolicy_Builder; cdecl;
    function detectResourceMismatches: JThreadPolicy_Builder; cdecl;
    function penaltyDeath: JThreadPolicy_Builder; cdecl;
    function penaltyFlashScreen: JThreadPolicy_Builder; cdecl;
    function penaltyLog: JThreadPolicy_Builder; cdecl;
    function permitAll: JThreadPolicy_Builder; cdecl;
    function permitNetwork: JThreadPolicy_Builder; cdecl;
    function permitResourceMismatches: JThreadPolicy_Builder; cdecl;
  end;

  TJThreadPolicy_Builder = class(TJavaGenericImport<JThreadPolicy_BuilderClass, JThreadPolicy_Builder>) end;
  {$ENDIF}

  TTMSFNCCloudBaseRequestResultOpen = class(TTMSFNCCloudBaseRequestResult);

  TTMSFNCAndroidCloudBase = class;

  TTMSFNCAndroidCloudBaseService = class(TTMSFNCCloudBaseFactoryService)
  protected
    function DoCreateCloudBase(const AValue: TTMSFNCCustomCloudBase): ITMSFNCCustomCloudBase; override;
  end;

  TTMSFNCAndroidCloudBase = class(TInterfacedObject, ITMSFNCCustomCloudBase)
  private
    FCloudBaseInstance: TTMSFNCCustomCloudBase;
    {$IFDEF ANDROID}
    LJSSLContext: JSSLContext;
    FJTrustManager: JX509TrustManager;
    LArrayTrustManager: TJavaObjectArray<JTrustManager>;
    {$ENDIF}
    FWebBrowserPopup: TTMSFNCWebBrowserPopup;
  public
    constructor Create(const ACloudBaseInstance: TTMSFNCCustomCloudBase);
    destructor Destroy; override;
    function GetUploadFileSize(const ARequest: TTMSFNCCloudBaseRequest): Int64;
    procedure ExecuteRequest(const ARequest: TTMSFNCCloudBaseRequestResult);
    procedure RunExternalBrowser(const AURL: string; const ACallBackURL: string; const ACallBack: TTMSFNCCloudBaseExternalBrowserCallBackEvent = nil);
    procedure CloseBrowser;
  end;

var
  CloudBaseService: ITMSFNCCloudBaseService;

{$IFDEF ANDROID}
function SharedActivityEx: JActivity;
begin
  {$HINTS OFF}
  {$IF COMPILERVERSION > 29}
  Result := TAndroidHelper.Activity;
  {$ELSE}
  Result := SharedActivity;
  {$IFEND}
  {$HINTS ON}
end;
{$ENDIF}

procedure RegisterCloudBaseService;
begin
  if not TTMSFNCCloudBasePlatformServices.Current.SupportsPlatformService(ITMSFNCCloudBaseService, IInterface(CloudBaseService)) then
  begin
    CloudBaseService := TTMSFNCAndroidCloudBaseService.Create;
    TTMSFNCCloudBasePlatformServices.Current.AddPlatformService(ITMSFNCCloudBaseService, CloudBaseService);
  end;
end;

procedure UnregisterCloudBaseService;
begin
  TTMSFNCCloudBasePlatformServices.Current.RemovePlatformService(ITMSFNCCloudBaseService);
end;

procedure TTMSFNCAndroidCloudBase.CloseBrowser;
begin
  FWebBrowserPopup.Close;
end;

constructor TTMSFNCAndroidCloudBase.Create(const ACloudBaseInstance: TTMSFNCCustomCloudBase);
{$IFDEF ANDROID}
var
  LPolicyBuilder: JThreadPolicy_Builder;
  LPolicy: JStrictMode_ThreadPolicy;
{$ENDIF}
begin
  FCloudBaseInstance := ACloudBaseInstance;

  {$IFDEF ANDROID}
  LJSSLContext := TJSSLContext.JavaClass.getInstance(StringToJString('TLS'));

  FJTrustManager := TX509TrustManager.Create;
  LArrayTrustManager := TJavaObjectArray<JTrustManager>.Create(1);
  LArrayTrustManager.Items[0] := TJTrustManager.Wrap(FJTrustManager);


  LJSSLContext.init(nil, LArrayTrustManager, TJSecureRandom.JavaClass.init);

  TJHttpsURLConnection.JavaClass.setDefaultSSLSocketFactory(LJSSLContext.getSocketFactory);

  LPolicyBuilder := TJThreadPolicy_Builder.JavaClass.init.detectAll.permitNetwork;
  LPolicy := LPolicyBuilder.build;
  TJStrictMode.JavaClass.setThreadPolicy(LPolicy);
  {$ENDIF}

  FWebBrowserPopup := TTMSFNCWebBrowserPopup.Create(FCloudBaseInstance);
  FWebBrowserPopup.ExternalBrowser := True;
end;

{ TTMSFNCAndroidCloudBaseService }

function TTMSFNCAndroidCloudBaseService.DoCreateCloudBase(const AValue: TTMSFNCCustomCloudBase): ITMSFNCCustomCloudBase;
begin
  Result := TTMSFNCAndroidCloudBase.Create(AValue);
end;

destructor TTMSFNCAndroidCloudBase.Destroy;
begin
  {$IFDEF ANDROID}
  LJSSLContext := nil;
  {$ENDIF}
  FWebBrowserPopup.Free;
  inherited;
end;

procedure TTMSFNCAndroidCloudBase.ExecuteRequest(const ARequest: TTMSFNCCloudBaseRequestResult);
{$IFDEF ANDROID}
var
  urlCon: JHttpURLConnection;
  u: JURL;
  I: Integer;
  hh, hv: string;
  wr: JDataOutputStream;
  b: TArray<Byte>;
  bu: TJavaArray<Byte>;
  t: Integer;
  sz: Int64;
  f: JFile;
  head, tail: string;
  fs: JFileInputStream;
  m: TMemoryStream;

  function GetDataFromStream(AInputStream: JInputStream; AStream: TStream = nil; AEncoding: String = 'UTF-8'): String;
  var
    writer: JStringWriter;
    reader: JBufferedReader;
    inputreader: JInputStreamReader;
    n: Integer;
    buf: TJavaArray<Char>;
    b: TJavaArray<Byte>;
    res: JString;
    l: Integer;
  begin
    if Assigned(AInputStream) then
    begin
      l := urlCon.getContentLength;

      TTMSFNCCloudBaseRequestResultOpen(ARequest).FTotalBytes := l;

      if Assigned(AStream) then
      begin
        b := TJavaArray<Byte>.Create(1024);
        n := AInputStream.read(b);
        t := 0;
        while n <> -1 do
        begin
          t := t + n;
          AStream.Write(b.Data^, n);

          TTMSFNCCloudBaseRequestResultOpen(ARequest).FBytesReceived := t;

          if Assigned(ARequest.OnProgress) and (l > 0) then
            ARequest.OnProgress(ARequest, t / l * 100, False);
          n := AInputStream.read(b);
        end;
      end
      else
      begin
        writer := TJStringWriter.JavaClass.init;
        try
          inputreader := TJInputStreamReader.JavaClass.init(AInputStream, StringToJString(AEncoding));
          reader := TJBufferedReader.JavaClass.init(JReader(inputreader), 1024);

          buf := TJavaArray<Char>.Create(1024);

          n := reader.read(buf);
          t := 0;
          while (n <> -1) do
          begin
            t := t + n;
            writer.write(buf, 0, n);

            TTMSFNCCloudBaseRequestResultOpen(ARequest).FBytesReceived := t;

            if Assigned(ARequest.OnProgress) and (l > 0) then
              ARequest.OnProgress(ARequest, t / l * 100, False);
            n := reader.read(buf);
          end;

          if (l <= 0) and Assigned(ARequest.OnProgress) then
            ARequest.OnProgress(ARequest, 100, False);

        finally
          AInputStream.close;
          inputreader := nil;
          reader := nil;
        end;

        res := writer.toString;
        if Assigned(res) then
          Result := JStringToString(res)
        else
          Result := '';

        writer := nil;
      end;
    end;
  end;

  function ConvertBytesToTJavaArray(const ABytes: TBytes): TJavaArray<Byte>;
  var
    LLength: Integer;
  begin
    LLength := Length(ABytes);
    Result := TJavaArray<System.Byte>.Create(LLength);
    Move(ABytes[0], PByte(Result.Data)^, LLength);
  end;

{$ENDIF}
begin
  {$IFDEF ANDROID}
  try
    if ARequest.CheckTaskStatus and Assigned(LJSSLContext) then
    begin
      if ARequest.GetURL <> '' then
      begin
        u := TJURL.JavaClass.init(StringToJString(ARequest.GetURL));
        urlCon := TJHttpURLConnection.Wrap((u.openConnection as ILocalObject).GetObjectID);
        urlCon.setDoInput(True);
        case ARequest.Method of
          rmGET, rmDELETE: urlCon.setDoOutput(False);
          else urlCon.setDoOutput(True);
        end;

        urlCon.setRequestMethod(StringToJString(ARequest.GetMethodString));

        for I := 0 to ARequest.Headers.Count - 1 do
          urlCon.addRequestProperty(StringToJString(ARequest.Headers[I].Name), StringToJString(ARequest.Headers[I].value));

        sz := GetUploadFileSize(ARequest);
        head := '';
        tail := '';

        if not ARequest.CustomHeaders and (ARequest.Method in [rmPOST, rmPUT, rmPATCH]) and (ARequest.HasUploadStream or ARequest.HasUploadFile) then
        begin
          urlCon.addRequestProperty(StringToJString('Content-Length'), StringToJString(inttostr(sz)));
          urlCon.addRequestProperty(StringToJString('Content-Type'), StringToJString('application/octet-stream'));
          urlCon.addRequestProperty(StringToJString('Content-Transfer-Encoding'), StringToJString('binary'));
        end;

        if (ARequest.Method in [rmPOSTMULTIPART, rmPUTMULTIPART]) then
        begin
          head := sRequestHeadBoundary + #13#10;
          tail := #13#10 + sRequestTailBoundary + #13#10;
          urlCon.addRequestProperty(StringToJString('Content-Type'), StringToJString('multipart/form-data; boundary=' + sRequestBoundary));
        end;

        if (ARequest.Method in [rmPOSTMULTIPARTRELATED, rmPUTMULTIPARTRELATED]) then
        begin
          head := ''#13#10;
          tail := #13#10 + sRequestTailBoundary + #13#10;
          urlCon.addRequestProperty(StringToJString('Content-Type'), StringToJString('multipart/related; boundary=' + sRequestBoundary));
        end;

        if urlCon.getDoOutput then
        begin
          wr := TJDataOutputStream.JavaClass.init(urlCon.getOutputStream);
          try
            if head <> '' then
            begin
              b := TEncoding.UTF8.GetBytes(head);
              bu := ConvertBytesToTJavaArray(b);
              wr.write(bu, 0, bu.Length);
            end;

            if ARequest.PostData <> '' then
            begin
              b := TEncoding.UTF8.GetBytes(ARequest.PostData);
              bu := ConvertBytesToTJavaArray(b);
              wr.write(bu, 0, bu.Length);
            end;

            if ARequest.HasUploadFile then
            begin
              f := TJFile.JavaClass.init(StringToJString(ARequest.UploadFile));
              if Assigned(f) then
              begin
                fs := TJFileInputStream.JavaClass.init(f);
                bu := TJavaArray<Byte>.Create(f.length);
                fs.read(bu);
                wr.write(bu, 0, bu.Length);
                if Assigned(ARequest.OnProgress) then
                  ARequest.OnProgress(ARequest, 100, True);
              end;
            end
            else if ARequest.HasUploadStream then
            begin
              ARequest.UploadStream.Position := 0;
              bu := TJavaArray<Byte>.Create(ARequest.UploadStream.Size);
              ARequest.UploadStream.ReadBuffer(bu.Data^, ARequest.UploadStream.Size);
              if Assigned(ARequest.OnProgress) then
                ARequest.OnProgress(ARequest, 100, True);
            end;

            if tail <> '' then
            begin
              b := TEncoding.UTF8.GetBytes(tail);
              bu := ConvertBytesToTJavaArray(b);
              wr.write(bu, 0, bu.Length);
            end;
          finally
            wr.flush;
            wr.close;
          end;
        end;

        if ARequest.CheckTaskStatus then
        begin
          I := 0;
          while True do
          begin
            hh := JStringToString(urlCon.getHeaderFieldKey(I));
            if hh = '' then
              Break;
            hv := JStringToString(urlCon.getHeaderField(I));

            ARequest.ResponseHeaders.Add(TTMSFNCCloudBaseRequestHeader.Create(hh, hv));

            Inc(I);
          end;

          ARequest.ResponseCode := urlCon.getResponseCode;
          if not (ARequest.ResponseCode in ValidHTTPCodes) then
            ARequest.ResultString := GetDataFromStream(urlCon.getErrorStream)
          else
          begin
            case ARequest.ResultType of
              rrtString: ARequest.ResultString := GetDataFromStream(urlCon.getInputStream);
              rrtStream:
              begin
                GetDataFromStream(urlCon.getInputStream, ARequest.ResultStream);
                ARequest.ResultStream.Position := 0;
              end;
              rrtFile:
              begin
                m := TMemoryStream.Create;
                try
                  GetDataFromStream(urlCon.getInputStream, m);
                  m.Position := 0;
                  m.SaveToFile(ARequest.ResultFile);
                finally
                  m.Free;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  finally
    TTMSFNCCloudBaseRequestResultOpen(ARequest).FDone := True;
    if ARequest.CheckTaskStatus then
    begin
      if Assigned(ARequest.OnComplete) then
        ARequest.OnComplete(ARequest);
    end
    else
    begin
      if Assigned(ARequest.OnCancelled) then
        ARequest.OnCancelled(ARequest);
    end;
  end;
  {$ENDIF}
end;

function TTMSFNCAndroidCloudBase.GetUploadFileSize(
  const ARequest: TTMSFNCCloudBaseRequest): Int64;
{$IFDEF ANDROID}
var
  f: JFile;
{$ENDIF}
begin
  Result := 0;
  {$IFDEF ANDROID}
  if ARequest.HasUploadFile then
  begin
    f := TJFile.JavaClass.init(StringToJString(ARequest.UploadFile));
    if Assigned(f) then
      Result := f.length;
  end
  else if ARequest.HasUploadStream then
  begin
    ARequest.UploadStream.Position := 0;
    Result := ARequest.UploadStream.Size;
  end;
  {$ENDIF}
end;

procedure TTMSFNCAndroidCloudBase.RunExternalBrowser(const AURL: string; const ACallBackURL: string; const ACallBack: TTMSFNCCloudBaseExternalBrowserCallBackEvent = nil);
begin
  FWebBrowserPopup.Open(AURL);
end;

{$IFDEF ANDROID}

{ TX509TrustManager }

procedure TX509TrustManager.checkClientTrusted(chain: TJavaObjectArray<JX509Certificate>; authType: JString);
begin

end;

procedure TX509TrustManager.checkServerTrusted(chain: TJavaObjectArray<JX509Certificate>; authType: JString);
begin

end;

function TX509TrustManager.getAcceptedIssuers: TJavaObjectArray<JX509Certificate>;
begin
  Result := nil;
end;
{$ENDIF}

end.

{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{           copyright (c)  2016 - 2021                               }
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

unit LCLTMSFNCURLBitmapContainer;

{$I LCLTMSFNCDefines.inc}

interface

uses
  {$IFNDEF LCLLIB}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  {$eNDIF}
  Classes, LCLTMSFNCBitmapContainer, LCLTMSFNCTypes
  {$IFDEF LINUX}
  {$IFDEF FMXLIB}
  ,Linuxapi.Curl, Posix.SysTypes
  {$ENDIF}
  {$ENDIF}
  ;

type
  TTMSFNCURLBitmapContainer = class;

  {$IFNDEF WEBLIB}
  TTMSFNCURLDownloadThread = class(TThread)
  private
    FBitmapContainer: TTMSFNCURLBitmapContainer;
  protected
    procedure Execute; override;
  public
    constructor Create(ABitmapContainer: TTMSFNCURLBitmapContainer);
  end;
  {$ENDIF}

  TTMSFNCURLBitmapItem = class(TTMSFNCBitmapItem)
  private
    FURL: string;
    FLoaded: Boolean;
    procedure SetURL(const Value: string);
  public
    property Loaded: Boolean read FLoaded write FLoaded;
    procedure Assign(Source: TPersistent); override;
  published
    property URL: string read FURL write SetURL;
  end;

  TTMSFNCURLBitmapCollection = class(TTMSFNCBitmapCollection)
  private
    function GetItem(Index: Integer): TTMSFNCURLBitmapItem;
    procedure SetItem(Index: Integer; const Value: TTMSFNCURLBitmapItem);
  protected
    function GetBitmapItemClass: TCollectionItemClass; override;
  public
    function Add: TTMSFNCURLBitmapItem;
    function Insert(index: Integer): TTMSFNCURLBitmapItem;
    property Items[Index: Integer]: TTMSFNCURLBitmapItem read GetItem write SetItem; default;
  end;

  TTMSFNCURLBitmapContainerDownloadCompleteEvent = procedure(Sender: TObject; ItemIndex: integer) of object;
  TTMSFNCURLBitmapContainerDownloadProgressEvent = procedure(Sender: TObject; ItemIndex: integer; Position, TotalSize: Int64) of object;

  TTMSFNCURLBitmapContainer = class(TTMSFNCBitmapContainer)
  {$IFDEF LINUX}
  {$IFDEF FMXLIB}
  private class var FDownloadStream: TMemoryStream;
  {$ENDIF}
  {$ENDIF}
  private
    {$IFNDEF WEBLIB}
    FWorkerItem: Integer;
    FWorkerPosition: Int64;
    FWorkerTotalSize: Int64;
    FDownloader: TTMSFNCURLDownloadThread;
    {$ENDIF}
    {$IFNDEF LCLLIB}
    {$IFDEF MSWINDOWS}
    FNetHandle: Pointer;
    {$ENDIF}
    {$ENDIF}
    FDefaultBitmap: TTMSFNCBitmap;
    FOnDownloadComplete: TTMSFNCURLBitmapContainerDownloadCompleteEvent;
    FOnDownloadProgress: TTMSFNCURLBitmapContainerDownloadProgressEvent;
    FOnInternalDownloadComplete: TTMSFNCURLBitmapContainerDownloadCompleteEvent;
    procedure SetDefaultBitmap(const Value: TTMSFNCBitmap);
    function GetItems: TTMSFNCURLBitmapCollection;
    procedure SetItems(const Value: TTMSFNCURLBitmapCollection);
    procedure DoProgress;
    procedure DoComplete;
    procedure ThreadTerminated(Sender: TObject);
    {$IFDEF FMXLIB}
    {$IFDEF LINUX}
    class function CurlReadData(buffer: Pointer; size: size_t; nitems: size_t; instream: Pointer): size_t; cdecl; static;
    {$ENDIF}
    {$ENDIF}
  protected
    function CreateItems: TTMSFNCBitmapCollection; override;
    procedure DoDownloadComplete(ItemIndex: integer); virtual;
    procedure DoDownloadProgress(ItemIndex: integer; Position, TotalSize: int64); virtual;
    procedure RegisterRuntimeClasses; override;
    property OnInternalDownloadComplete: TTMSFNCURLBitmapContainerDownloadCompleteEvent read FOnInternalDownloadComplete write FOnInternalDownloadComplete;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function FindBitmap(s: string): TTMSFNCBitmap; override;
    function FindBitmapByURL(AURL: string): TTMSFNCBitmap;
    function Download(URL: string): TMemoryStream;
    procedure AddFromURL(URL, BitmapName: String); override;
  published
    property Items: TTMSFNCURLBitmapCollection read GetItems write SetItems;
    property DefaultBitmap: TTMSFNCBitmap read FDefaultBitmap write SetDefaultBitmap;
    property OnDownloadComplete: TTMSFNCURLBitmapContainerDownloadCompleteEvent read FOnDownloadComplete write FOnDownloadComplete;
    property OnDownloadProgress: TTMSFNCURLBitmapContainerDownloadProgressEvent read FOnDownloadProgress write FOnDownloadProgress;
  end;

implementation

uses
  {$IFNDEF LCLLIB}
  {$IFDEF MSWINDOWS}
  WinInet, Ansistrings,
  {$ENDIF}
  {$IFDEF MACOS}
  {$IFDEF IOS}
  iOSApi.Foundation,
  {$ELSE}
  MacApi.Foundation,
  {$ENDIF}
  {$ENDIF}
  {$IFDEF ANDROID}
  AndroidApi.Helpers, AndroidApi.JNIBridge,
  AndroidAPI.JNI.JavaTypes, FMX.Helpers.Android,
  {$ENDIF}
  {$ENDIF}
  {$IFDEF LCLLIB}
  FPHTTPClient,
  {$ENDIF}
  SysUtils, LCLTMSFNCUtils;

{$IFDEF ANDROID}
type
  JHttpURLConnection = interface;
  JURLConnection = interface;
  JURL = interface;

  JURLClass = interface(JObjectClass)
    ['{3E1A363B-FA2A-4399-A0C7-24DFC255E133}']
    {class} function init(spec: JString): JURL; cdecl; overload;
  end;

  [JavaSignature('java/net/URL')]
  JURL = interface(JObject)
    ['{BCC8860A-E48B-4894-95A9-26B3AFDF3EDC}']
    function openConnection: JURLConnection; cdecl; overload;
  end;
  TJURL = class(TJavaGenericImport<JURLClass, JURL>) end;

  JURLConnectionClass = interface(JObjectClass)
    ['{969FF7F8-7B73-491C-A621-855E197B0B6B}']
  end;

  [JavaSignature('java/net/URLConnection')]
  JURLConnection = interface(JObject)
    ['{F50D6528-AE5E-4862-89A1-CAE44274AED9}']
    function getHeaderField(key: JString): JString; cdecl; overload;
  end;
  TJURLConnection = class(TJavaGenericImport<JURLConnectionClass, JURLConnection>) end;

  JHttpURLConnectionClass = interface(JURLConnectionClass)
    ['{D80190E4-FBCC-4286-9E39-42F45C58E7D9}']
  end;
  [JavaSignature('java/net/HttpURLConnection')]
  JHttpURLConnection = interface(JURLConnection)
  ['{57A25834-F65A-4078-8B73-1C566DF70536}']
    procedure setRequestMethod(method: JString); cdecl;
    procedure setDoInput(newValue: Boolean); cdecl;
    procedure setDoOutput(newValue: Boolean); cdecl;
    function getOutputStream: JOutputStream; cdecl;
    function getInputStream: JInputStream; cdecl;
    function getResponseCode: Integer; cdecl;
  end;
  TJHttpURLConnection = class(TJavaGenericImport<JHttpURLConnectionClass, JHttpURLConnection>) end;
{$ENDIF}

{ TTMSFNCURLBitmapContainer }

procedure TTMSFNCURLBitmapContainer.AddFromURL(URL, BitmapName: String);
{$IFNDEF WEBLIB}
var
  bmpi: TTMSFNCURLBitmapItem;
{$ENDIF}
begin
  {$IFDEF WEBLIB}
  inherited;
  {$ENDIF}
  {$IFNDEF WEBLIB}
  bmpi := Items.Add;
  bmpi.Name := BitmapName;
  bmpi.URL := URL;
  {$ENDIF}
end;

constructor TTMSFNCURLBitmapContainer.Create(AOwner: TComponent);
begin
  inherited;
  FDefaultBitmap := TTMSFNCBitmap.Create;
  {$IFNDEF WEBLIB}
  FDownloader := nil;
  if not (csDesigning in ComponentState) then
  begin
    FDownloader := TTMSFNCURLDownloadThread.Create(Self);
    FDownloader.OnTerminate := @ThreadTerminated;
    {$IFDEF CMNLIB}
    FDownloader.Priority := tpLower;
    {$ENDIF}
    {$IFDEF FMXLIB}
    {$IFDEF MSWINDOWS}
    FDownloader.Priority := tpLower;
    {$ENDIF}
    {$IFDEF MACOS}
    FDownloader.Priority := 2;
    {$ENDIF}
    {$ENDIF}
    FDownloader.FreeOnTerminate := true;
  end;
  {$ENDIF}
end;

function TTMSFNCURLBitmapContainer.CreateItems: TTMSFNCBitmapCollection;
begin
  Result := TTMSFNCURLBitmapCollection.Create(Self);
end;

destructor TTMSFNCURLBitmapContainer.Destroy;
begin
  FDefaultBitmap.Free;
  {$IFNDEF WEBLIB}
  if Assigned(FDownloader) then
    FDownloader.Terminate;
  {$ENDIF}
  inherited;
end;

procedure TTMSFNCURLBitmapContainer.DoComplete;
begin
  {$IFNDEF WEBLIB}
  DoDownloadComplete(FWorkerItem);
  {$ENDIF}
end;

procedure TTMSFNCURLBitmapContainer.DoDownloadComplete(ItemIndex: integer);
begin
  if Assigned(OnInternalDownloadComplete) then
    OnInternalDownloadComplete(Self, ItemIndex);
  if Assigned(OnDownloadComplete) then
    OnDownloadComplete(Self, ItemIndex);
  Changed;
end;

procedure TTMSFNCURLBitmapContainer.DoDownloadProgress(ItemIndex: integer; Position,
  TotalSize: Int64);
begin
  if Assigned(OnDownloadProgress) then
    OnDownloadProgress(Self, ItemIndex, Position, TotalSize);
end;

{$IFDEF FMXLIB}
{$IFDEF LINUX}
class function TTMSFNCURLBitmapContainer.CurlReadData(buffer: Pointer; size, nitems: size_t;
  instream: Pointer): size_t; cdecl;
var
  l: Integer;
begin
  if instream <> nil then
  begin
    l := nitems * size;
    Result := FDownloadStream.Write(buffer^, l);
  end
  else
    Result := 0;
end;
{$ENDIF}
{$ENDIF}

procedure TTMSFNCURLBitmapContainer.DoProgress;
begin
  {$IFNDEF WEBLIB}
  DoDownloadProgress(FWorkerItem, FWorkerPosition, FWorkerTotalSize);
  {$ENDIF}
end;

function TTMSFNCURLBitmapContainer.FindBitmap(s: string): TTMSFNCBitmap;
var
  it: TTMSFNCURLBitmapItem;
  I: Integer;
begin
  Result := nil;
  it := nil;
  s := Uppercase(s);
  i := 1;
  while i <= Items.Count do
  begin
    if Uppercase(Items[i - 1].Name) = s then
    begin
      it := Items[i - 1];
      Break;
    end;
    Inc(i);
  end;

  if Assigned(it) then
    Result := it.Bitmap;

  if IsBitmapEmpty(Result) then
    Result := FDefaultBitmap;
end;

function TTMSFNCURLBitmapContainer.FindBitmapByURL(AURL: string): TTMSFNCBitmap;
var
  s: string;
  i: Integer;
begin
  Result := nil;
  s := Uppercase(AURL);
  i := 1;
  while i <= Items.Count do
  begin
    if Uppercase(Items[i - 1].URL) = s then
    begin
      Result := Items[i - 1].Bitmap;
      Break;
    end;
    Inc(i);
  end;
end;

function TTMSFNCURLBitmapContainer.GetItems: TTMSFNCURLBitmapCollection;
begin
  Result := TTMSFNCURLBitmapCollection(inherited Items);
end;

procedure TTMSFNCURLBitmapContainer.RegisterRuntimeClasses;
begin
  RegisterClass(TTMSFNCURLBitmapContainer);
end;

procedure TTMSFNCURLBitmapContainer.SetDefaultBitmap(const Value: TTMSFNCBitmap);
begin
  FDefaultBitmap.Assign(Value);
end;

procedure TTMSFNCURLBitmapContainer.SetItems(
  const Value: TTMSFNCURLBitmapCollection);
begin
  TTMSFNCURLBitmapCollection(inherited Items).Assign(Value);
end;

procedure TTMSFNCURLBitmapContainer.ThreadTerminated(Sender: TObject);
begin
  {$IFNDEF LCLLIB}
  {$IFDEF MSWINDOWS}
  if Assigned(FNetHandle) then
    InternetCloseHandle(FNetHandle);
  {$ENDIF}
  {$ENDIF}
end;

{ TTMSFNCURLBitmapItem }

procedure TTMSFNCURLBitmapItem.Assign(Source: TPersistent);
begin
  if (Source is TTMSFNCURLBitmapItem) then
  begin
    Name := (Source as TTMSFNCURLBitmapItem).Name;
    Tag := (Source as TTMSFNCURLBitmapItem).Tag;
    Bitmap.Assign((Source as TTMSFNCURLBitmapItem).Bitmap);
    URL := (Source as TTMSFNCURLBitmapItem).URL;
  end
  else if (Source is TTMSFNCBitmapItem) then
  begin
    Name := (Source as TTMSFNCBitmapItem).Name;
    Tag := (Source as TTMSFNCBitmapItem).Tag;
    Bitmap.Assign((Source as TTMSFNCBitmapItem).Bitmap)
  end
  else
    inherited;
end;

procedure TTMSFNCURLBitmapItem.SetURL(const Value: string);
begin
  if (FURL <> Value) then
  begin
    FURL := Value;
    FLoaded := False;
  end;
end;

{$IFNDEF WEBLIB}

{ TTMSFNCURLDownloadThread }

constructor TTMSFNCURLDownloadThread.Create(ABitmapContainer: TTMSFNCURLBitmapContainer);
begin
  inherited Create(False);
  FBitmapContainer := ABitmapContainer;
end;
{$ENDIF}

function TTMSFNCURLBitmapContainer.Download(URL: string): TMemoryStream;
{$IFDEF WEBLIB}
begin
  DoProgress;
{$ELSE}
{$IFDEF LCLLIB}
begin
  try
    Result := TMemoryStream.Create;
    TFPHTTPClient.SimpleGet(URL, Result);
    FWorkerTotalSize := Result.Size;
    FWorkerPosition := Result.Size;
    if Assigned(FDownloader) then
      FDownloader.Synchronize(@DoProgress)
    else
      DoProgress;
  finally
  end;
{$ENDIF}
{$IFNDEF LCLLIB}
{$IFDEF MACOS}
var
  req: NSMutableURLRequest;
  nurl: NSURL;
  res: Pointer;
  resdata: NSData;
begin
  Result := TMemoryStream.Create;
  try
    nurl := TNSURL.Wrap(TNSURL.OCClass.URLWithString(TTMSFNCUtils.NSStrEx(Url)));
    req := TNSMutableURLRequest.Wrap(TNSMutableURLRequest.OCClass.requestWithURL(nurl, NSURLRequestUseProtocolCachePolicy, 60.0));
    req.setHTTPMethod(TTMSFNCUtils.NSStrEx('GET'));

    res := nil;
    resdata := TNSURLConnection.OCClass.sendSynchronousRequest(req, @res, nil);
    if Assigned(resdata) and Assigned(res) then
    begin
      if not UTF8ToString(TNSString.Wrap(TNSHTTPURLResponse.Wrap(res).allHeaderFields.valueForKey(TTMSFNCUtils.NSStrEx('Content-Type'))).UTF8String).StartsWith('image/') then
        Exit;

      {$HINTS OFF}
      {$IF COMPILERVERSION > 31}
      Result.Write(resdata.bytes^, resdata.length);
      {$ELSE}
      Result.Write(resdata.bytes, resdata.length);
      {$IFEND}
      {$HINTS ON}

      FWorkerTotalSize := Result.Size;
      FWorkerPosition := Result.Size;
      if Assigned(FDownloader) then
        FDownloader.Synchronize(@DoProgress)
      else
        DoProgress;
    end;
  finally
  end;
{$ELSE}
{$IFDEF ANDROID}
  var
    urlCon: JHttpURLConnection;
    u: JURL;
    r: Integer;
    data: TJavaArray<Byte>;
    bs: JByteArrayOutputStream;
    ip: JInputStream;
  begin
    Result := TMemoryStream.Create;
    try
      u := TJURL.JavaClass.init(StringToJString(url));
      urlCon := TJHttpURLConnection.Wrap((u.openConnection as ILocalObject).GetObjectID);
      urlCon.setDoInput(True);
      urlCon.setDoOutput(False);
      urlCon.setRequestMethod(StringToJString('GET'));

      if not urlCon.getHeaderField(StringToJString('Content-Type')).startsWith(StringToJString('image/')) then
        Exit;

      if (urlCon.getResponseCode in [200, 201]) then
      begin
        bs := TJByteArrayOutputStream.JavaClass.init;
        ip := urlCon.getInputStream;
        r := ip.read();
        while r <> -1 do
        begin
          bs.write(r);
          r := ip.read();
        end;

        data := bs.toByteArray;
        Result :=  TMemoryStream.Create;
        Result.WriteBuffer(data.Data^, bs.size);
        bs.close;
        FWorkerTotalSize := Result.Size;
        FWorkerPosition := Result.Size;
        if Assigned(FDownloader) then
          FDownloader.Synchronize(@DoProgress)
        else
          DoProgress;
      end;
    finally
    end;
{$ELSE}
{$IFDEF LINUX}
{$IFDEF FMXLIB}
var
  r: PCURL;
{$ENDIF}
begin
  {$IFDEF FMXLIB}
  FDownloadStream := TMemoryStream.Create;
  Result := FDownloadStream;
  curl_global_init(CURL_GLOBAL_DEFAULT);
  r := curl_easy_init;

  curl_easy_setopt(r, CURLOPT_CERTINFO, 1);
  curl_easy_setopt(r, CURLOPT_TCP_KEEPALIVE, 1);
  curl_easy_setopt(r, CURLOPT_NOPROGRESS, 1);
  curl_easy_setopt(r, CURLOPT_CONNECTTIMEOUT_MS, LongInt(60000));
  curl_easy_setopt(r, CURLOPT_TIMEOUT_MS, LongInt(120000));
  curl_easy_setopt(r, CURLOPT_WRITEFUNCTION, @CurlReadData);
  curl_easy_setopt(r, CURLOPT_WRITEDATA, Self);

  curl_easy_setopt(r, CURLOPT_URL, UTF8String(URL));

  curl_easy_setopt(r, CURLOPT_CUSTOMREQUEST, PUTF8Char(UTF8String('GET')));

  curl_easy_perform(r);

  curl_easy_cleanup(r);

  curl_global_cleanup;
  {$ELSE}
  Result := TMemoryStream.Create;
  {$ENDIF}
  DoProgress;
{$ELSE}
var
  UrlHandle: HINTERNET;
  Buffer: array[0..1024] of AnsiChar;
  BytesRead: DWORD;
  position: Int64;
  lpdwlen,lpdwidx,lpdword: DWORD;
  Size: DWORD;
  cbuf: array[0..255] of char;
begin
  Result := TMemoryStream.Create;

  if not Assigned(FNetHandle) then
    FNetHandle := InternetOpen('TTMSFNCURLBitmapContainer', INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);

  if Assigned(FNetHandle) then
  begin
    UrlHandle := InternetOpenUrl(FNetHandle, PChar(Url), nil, 0, INTERNET_FLAG_PRAGMA_NOCACHE or INTERNET_FLAG_NO_CACHE_WRITE or INTERNET_FLAG_RELOAD, 0);

    position := 0;

    if Assigned(UrlHandle) then
    begin
      lpdwlen := SizeOf(cbuf);
      lpdwidx := 0;
      HttpQueryInfo(UrlHandle, HTTP_QUERY_CONTENT_TYPE,@cbuf,lpdwlen,lpdwidx);

      if Pos('IMAGE',UpperCase(StrPas(cbuf))) = 0 then
      begin
        InternetCloseHandle(UrlHandle);
        Exit;
      end;

      lpdword := 0;
      lpdwlen := 4;
      lpdwidx := 0;

      HttpQueryInfo(URLHandle,HTTP_QUERY_CONTENT_LENGTH,@lpdword,lpdwlen,lpdwidx);

      Size := lpdword;
      FWorkerTotalSize := Size;

      try
        FillChar(Buffer, SizeOf(Buffer), 0);
        repeat
          FillChar(Buffer, SizeOf(Buffer), 0);
          InternetReadFile(UrlHandle, @Buffer, SizeOf(Buffer), BytesRead);
          Result.Write(buffer, bytesread);
          position := position + bytesread;
          FWorkerPosition := position;
          if Assigned(FDownloader) then
            FDownloader.Synchronize(@DoProgress)
          else
            DoProgress;
        until BytesRead = 0;
      finally
      end;
      InternetCloseHandle(UrlHandle);
    end
    else
      raise Exception.CreateFmt('Cannot open URL %s', [Url]);
  end
  else
    raise Exception.Create('Unable to initialize Wininet');
    {$ENDIF}
  {$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}
end;

{$IFNDEF WEBLIB}
procedure TTMSFNCURLDownloadThread.Execute;
var
  i: Integer;
  ms: TMemoryStream;
begin
  while not Terminated do
  begin
    for I := 0 to FBitmapContainer.Items.Count - 1 do
    begin
      if not FBitmapContainer.Items[I].Loaded and (FBitmapContainer.Items[i].URL <> '') then
      begin
        FBitmapContainer.FWorkerItem := i;
        FBitmapContainer.FWorkerPosition := 0;
        FBitmapContainer.FWorkerTotalSize := 0;
        ms := nil;
        try
          ms := FBitmapContainer.Download(FBitmapContainer.Items[I].URL);
          if Assigned(ms) then
          begin
            ms.Position := 0;
            {$IFDEF ANDROID}
            TThread.Synchronize(@TThread.CurrentThread,
            procedure
            begin
            {$ENDIF}
              FBitmapContainer.Items[I].Bitmap.LoadFromStream(ms);
            {$IFDEF ANDROID}
            end
            );
            {$ENDIF}
            FBitmapContainer.Items[I].Loaded := True;
          end;
        finally
          if Assigned(ms) then
            ms.Free;
        end;

        Synchronize(@FBitmapContainer.DoComplete);
      end;
    end;
    Sleep(100);
  end;
end;
{$ENDIF}

{ TTMSFNCURLBitmapCollection }

function TTMSFNCURLBitmapCollection.Add: TTMSFNCURLBitmapItem;
begin
  Result := TTMSFNCURLBitmapItem(inherited Add);
end;

function TTMSFNCURLBitmapCollection.GetBitmapItemClass: TCollectionItemClass;
begin
  Result := TTMSFNCURLBitmapItem;
end;

function TTMSFNCURLBitmapCollection.GetItem(
  Index: Integer): TTMSFNCURLBitmapItem;
begin
  Result := TTMSFNCURLBitmapItem(inherited Items[Index]);
end;

function TTMSFNCURLBitmapCollection.Insert(
  index: Integer): TTMSFNCURLBitmapItem;
begin
  Result := TTMSFNCURLBitmapItem(inherited Insert(index));
end;

procedure TTMSFNCURLBitmapCollection.SetItem(Index: Integer;
  const Value: TTMSFNCURLBitmapItem);
begin
  inherited SetItem(Index, Value);
end;

end.

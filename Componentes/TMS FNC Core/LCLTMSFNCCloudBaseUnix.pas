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

unit LCLTMSFNCCloudBaseUnix;

{$I LCLTMSFNCDefines.inc}

interface

procedure RegisterCloudBaseService;
procedure UnRegisterCloudBaseService;

implementation

uses
  Classes, Types, SysUtils, LCLTMSFNCCloudBase, LCLTMSFNCUtils
  {$IFDEF LCLLIB}
  ,FPHTTPClient, openssl
  {$ENDIF}
  {$IFDEF FMXLIB}
  {$IFDEF LINUX}
  ,Posix.SysTypes, Linuxapi.Curl
  {$ENDIF}
  {$ENDIF}
  ;

type
  TTMSFNCUnixCloudBaseService = class;

  TTMSFNCUnixCloudBaseService = class(TTMSFNCCloudBaseFactoryService)
  protected
    function DoCreateCloudBase(const AValue: TTMSFNCCustomCloudBase): ITMSFNCCustomCloudBase; override;
  end;

  TTMSFNCUnixCloudBaseAllowedResponseCodes = array of Integer;

  {$IFDEF LCLLIB}
  TTMSFNCUnixCloudBaseHTTPClient = class(TFPHTTPClient)
  {$ENDIF}
  {$IFDEF FMXLIB}
  TTMSFNCUnixCloudBaseHTTPClientDataReceivedEvent = procedure(Sender: TObject; const ContentLength, CurrentPos: Int64) of object;
  TTMSFNCUnixCloudBaseHTTPClient = class
  {$ENDIF}
  private
    FRequest: TTMSFNCCloudBaseRequestResult;
    FContentLength: Int64;
    {$IFDEF FMXLIB}
    {$IFDEF LINUX}
    FStream: TStream;
    FResponseHeaders: TTMSFNCCloudBaseRequestHeaders;
    FNativeRequest: PCurl;
    FError: array [0 .. CURL_ERROR_SIZE] of Byte;
    FHeaders: pcurl_slist;
    FRequestBody: TStringStream;
    FBytesTotalSize, FBytesWritten, FBytesRead: Int64;
    FOnDataReceived: TTMSFNCUnixCloudBaseHTTPClientDataReceivedEvent;
    FResponseStatusCode: Integer;
    {$ENDIF}
    {$ENDIF}
  protected
    {$IFDEF FMXLIB}
    {$IFDEF LINUX}
    function ReceiveData(buffer: Pointer; size: size_t; nitems: size_t): size_t;
    function ReceiveHeader(buffer: Pointer; size: size_t; nitems: size_t): size_t;
    function SendData(buffer: Pointer; size: size_t; nitems: size_t): size_t;
    class function CurlReadData(buffer: Pointer; size: size_t; nitems: size_t; instream: Pointer): size_t; cdecl; static;
    class function CurlWriteData(buffer: Pointer; size: size_t; nitems: size_t; instream: Pointer): size_t; cdecl; static;
    class function CurlReadHeaders(buffer: Pointer; size: size_t; nitems: size_t; instream: Pointer): size_t; cdecl; static;
    {$ENDIF}
    {$ENDIF}
  public
    {$IFDEF FMXLIB}
    {$IFDEF LINUX}
    constructor Create(AOwner: TObject);
    destructor Destroy; override;
    procedure HTTPMethod(AMethod: string; AURL: string; AStream: TStream; AAllowedCodes: TTMSFNCUnixCloudBaseAllowedResponseCodes);
    procedure AddHeader(AName: string; AValue: string);
    function FindResponseHeaderValue(AName: string): string;
    property OnDataReceived: TTMSFNCUnixCloudBaseHTTPClientDataReceivedEvent read FOnDataReceived write FOnDataReceived;
    property RequestBody: TStringStream read FRequestBody write FRequestBody;
    property ResponseStatusCode: Integer read FResponseStatusCode;
    {$ENDIF}
    {$ENDIF}
    property Request: TTMSFNCCloudBaseRequestResult read FRequest write FRequest;
    property ContentLength: Int64 read FContentLength;
  end;

  TTMSFNCCloudBaseRequestResultOpen = class(TTMSFNCCloudBaseRequestResult);

  TTMSFNCUnixCloudBase = class(TInterfacedObject, ITMSFNCCustomCloudBase)
  private
    FCloudBaseInstance: TTMSFNCCustomCloudBase;
  protected
    procedure DoDataReceived(Sender: TObject; const ContentLength, CurrentPos: Int64);
  public
    constructor Create(const ACloudBaseInstance: TTMSFNCCustomCloudBase);
    function GetUploadFileSize(const ARequest: TTMSFNCCloudBaseRequest): Int64;
    procedure ExecuteRequest(const ARequest: TTMSFNCCloudBaseRequestResult);
    procedure RunExternalBrowser(const AURL: string; const ACallBackURL: string; const ACallBack: TTMSFNCCloudBaseExternalBrowserCallBackEvent = nil);
    procedure CloseBrowser;
  end;

var
  CloudBaseService: ITMSFNCCloudBaseService;

procedure RegisterCloudBaseService;
begin
  if not TTMSFNCCloudBasePlatformServices.Current.SupportsPlatformService(ITMSFNCCloudBaseService, IInterface(CloudBaseService)) then
  begin
    CloudBaseService := TTMSFNCUnixCloudBaseService.Create;
    TTMSFNCCloudBasePlatformServices.Current.AddPlatformService(ITMSFNCCloudBaseService, CloudBaseService);
  end;
end;

procedure UnregisterCloudBaseService;
begin
  TTMSFNCCloudBasePlatformServices.Current.RemovePlatformService(ITMSFNCCloudBaseService);
end;

procedure TTMSFNCUnixCloudBase.CloseBrowser;
begin

end;

constructor TTMSFNCUnixCloudBase.Create(const ACloudBaseInstance: TTMSFNCCustomCloudBase);
begin
  FCloudBaseInstance := ACloudBaseInstance;
end;

{ TTMSFNCUnixCloudBaseService }

function TTMSFNCUnixCloudBaseService.DoCreateCloudBase(const AValue: TTMSFNCCustomCloudBase): ITMSFNCCustomCloudBase;
begin
  Result := TTMSFNCUnixCloudBase.Create(AValue);
end;

procedure TTMSFNCUnixCloudBase.DoDataReceived(Sender: TObject; const ContentLength, CurrentPos: Int64);
var
  c: TTMSFNCUnixCloudBaseHTTPClient;
begin
  if Sender is TTMSFNCUnixCloudBaseHTTPClient then
  begin
    c := (Sender as TTMSFNCUnixCloudBaseHTTPClient);
    c.FContentLength := ContentLength;
    if Assigned(c.Request) then
    begin
      TTMSFNCCloudBaseRequestResultOpen(c.Request).FTotalBytes := ContentLength;
      TTMSFNCCloudBaseRequestResultOpen(c.Request).FBytesReceived := CurrentPos;

      if Assigned(c.Request.OnProgress) and (ContentLength > 0) then
        c.Request.OnProgress(c.Request, CurrentPos / ContentLength * 100, False);
    end;
  end;
end;

procedure TTMSFNCUnixCloudBase.ExecuteRequest(
  const ARequest: TTMSFNCCloudBaseRequestResult);
{$IFDEF LINUX}
var
  u: string;
  c: TTMSFNCUnixCloudBaseHTTPClient;
  h: TTMSFNCCloudBaseRequestHeader;
  I: Integer;
  ss, rs: TStringStream;
  m: TStream;
  d, head, tail: string;
  ms: TMemoryStream;
  sz: Int64;

  function GetAllowedResponseCodes: TTMSFNCUnixCloudBaseAllowedResponseCodes;
  var
    J: Integer;
  begin
    for J := 100 to 600 do
    begin
      SetLength(Result, Length(Result) + 1);
      Result[Length(Result) - 1] := J;
    end;
  end;
{$ENDIF}
begin
  {$IFDEF LINUX}
  try
    if ARequest.CheckTaskStatus then
    begin
      u := ARequest.GetURL;
      if u <> '' then
      begin
        c := TTMSFNCUnixCloudBaseHTTPClient.Create(nil);
        try
          c.Request := ARequest;
          c.OnDataReceived := {$IFDEF LCLLIB}@{$ENDIF}DoDataReceived;

          for I := 0 to ARequest.Headers.Count - 1 do
          begin
            h := ARequest.Headers[I];
            c.AddHeader(h.Name, h.Value);
          end;

          d := '';
          sz := GetUploadFileSize(ARequest);
          head := '';
          tail := '';

          if not ARequest.CustomHeaders and (ARequest.Method in [rmPOST, rmPUT, rmPATCH]) and (ARequest.HasUploadStream or ARequest.HasUploadFile) then
          begin
            c.AddHeader('Content-Length', inttostr(sz));
            c.AddHeader('Content-Type', 'application/octet-stream');
            c.AddHeader('Content-Transfer-Encoding', 'binary');
          end;

          if (ARequest.Method in [rmPOSTMULTIPART, rmPUTMULTIPART]) then
          begin
            head := sRequestHeadBoundary + #13#10;
            tail := #13#10 + sRequestTailBoundary + #13#10;
            c.AddHeader('Content-Type', 'multipart/form-data; boundary=' + sRequestBoundary);
          end;

          if (ARequest.Method in [rmPOSTMULTIPARTRELATED, rmPUTMULTIPARTRELATED]) then
          begin
            head := ''#13#10;
            tail := #13#10 + sRequestTailBoundary + #13#10;
            c.AddHeader('Content-Type', 'multipart/related; boundary=' + sRequestBoundary);
          end;

          ss := TStringStream.Create('');

          if head <> '' then
            ss.WriteString(head);

          if ARequest.PostData <> '' then
            ss.WriteString(ARequest.PostData);

          if ARequest.HasUploadFile then
          begin
            ms := TMemoryStream.Create;
            try
              ms.LoadFromFile(ARequest.UploadFile);
              ms.Position := 0;
              ss.CopyFrom(ms, ms.Size);
              if Assigned(ARequest.OnProgress) then
                ARequest.OnProgress(ARequest, 100, True);
            finally
              ms.Free;
            end;
          end
          else if ARequest.HasUploadStream then
          begin
            ARequest.UploadStream.Position := 0;
            ss.CopyFrom(ARequest.UploadStream, ARequest.UploadStream.Size);
            if Assigned(ARequest.OnProgress) then
              ARequest.OnProgress(ARequest, 100, True);
          end;

          if tail <> '' then
            ss.WriteString(tail);

          m := nil;
          case ARequest.ResultType of
            rrtString: m := TStringStream.Create('');
            rrtStream, rrtFile: m := TMemoryStream.Create;
          end;

          try
            ss.Position := 0;
            c.RequestBody := ss;
            c.HTTPMethod(ARequest.GetMethodString, u, m, GetAllowedResponseCodes);

            ARequest.ResponseCode := c.ResponseStatusCode;

            if Assigned(m) then
            begin
              if not (ARequest.ResponseCode in ValidHTTPCodes) then
              begin
                case ARequest.ResultType of
                  rrtString: ARequest.ResultString := TStringStream(m).DataString;
                  rrtStream, rrtFile:
                  begin
                    rs := TStringStream.Create('');
                    try
                      m.Position := 0;
                      rs.CopyFrom(m, m.Size);
                      ARequest.ResultString := rs.DataString;
                    finally
                      rs.Free;
                    end;
                  end;
                end;
              end
              else
              begin
                case ARequest.ResultType of
                  rrtString: ARequest.ResultString := TStringstream(m).DataString;
                  rrtStream:
                  begin
                    ARequest.ResultStream.LoadFromStream(m);
                    ARequest.ResultStream.Position := 0;
                  end;
                  rrtFile:
                  begin
                    if Assigned(m) then
                      TMemoryStream(m).SaveToFile(ARequest.ResultFile);
                  end;
                end;
              end;
            end;

            if Assigned(ARequest.OnProgress) and (c.ContentLength <= 0) then
              ARequest.OnProgress(ARequest, 100, False);
          finally
            m.Free;
            ss.Free;
          end;
        finally
          c.Free;
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

function TTMSFNCUnixCloudBase.GetUploadFileSize(
  const ARequest: TTMSFNCCloudBaseRequest): Int64;
{$IFDEF UNIX}
  function GetFileSize(const AFilename: String): Int64;
  var
    F: File Of byte;
  begin
    Assign(F, AFileName);
    Reset(F);
    Result := FileSize(F);
    Close(F);
  end;
{$ENDIF}
begin
  Result := 0;
  {$IFDEF UNIX}
  if ARequest.HasUploadFile then
    Result := GetFileSize(ARequest.UploadFile)
  else if ARequest.HasUploadStream then
  begin
    ARequest.UploadStream.Position := 0;
    Result := ARequest.UploadStream.Size;
  end;
  {$ENDIF}
end;

procedure TTMSFNCUnixCloudBase.RunExternalBrowser(const AURL,
  ACallBackURL: string;
  const ACallBack: TTMSFNCCloudBaseExternalBrowserCallBackEvent);
begin
  TTMSFNCUtils.OpenURL(AURL);
end;

{$IFDEF FMXLIB}
{$IFDEF LINUX}
constructor TTMSFNCUnixCloudBaseHTTPClient.Create(AOwner: TObject);
var
  c: CURLcode;
begin
  c := curl_global_init(CURL_GLOBAL_DEFAULT);
  FHeaders := nil;
  FNativeRequest := curl_easy_init;

  curl_easy_setopt(FNativeRequest, CURLOPT_CERTINFO, 1);
  curl_easy_setopt(FNativeRequest, CURLOPT_TCP_KEEPALIVE, 1);
  curl_easy_setopt(FNativeRequest, CURLOPT_NOPROGRESS, 1);
  curl_easy_setopt(FNativeRequest, CURLOPT_ERRORBUFFER, @FError[0]);
  curl_easy_setopt(FNativeRequest, CURLOPT_CONNECTTIMEOUT_MS, LongInt(60000));
  curl_easy_setopt(FNativeRequest, CURLOPT_TIMEOUT_MS, LongInt(120000));
  curl_easy_setopt(FNativeRequest, CURLOPT_WRITEFUNCTION, @CurlReadData);
  curl_easy_setopt(FNativeRequest, CURLOPT_FOLLOWLOCATION, LongInt(1));
  curl_easy_setopt(FNativeRequest, CURLOPT_WRITEDATA, Self);
  curl_easy_setopt(FNativeRequest, CURLOPT_HEADERFUNCTION, @CurlReadHeaders);
  curl_easy_setopt(FNativeRequest, CURLOPT_HEADERDATA, Self);
  FResponseHeaders := TTMSFNCCloudBaseRequestHeaders.Create;
end;

destructor TTMSFNCUnixCloudBaseHTTPClient.Destroy;
var
  r: PCurl;
begin
  if FNativeRequest <> nil then
  begin
    r := FNativeRequest;
    FNativeRequest := nil;
    curl_easy_cleanup(r);
  end;

  if FHeaders <> nil then
    curl_slist_free_all(FHeaders);

  curl_global_cleanup;
  FResponseHeaders.Free;
  inherited;
end;

function TTMSFNCUnixCloudBaseHTTPClient.ReceiveData(buffer: Pointer; size, nitems: size_t): size_t;
var
  l: Integer;
  cl: string;
  v: Int64;
begin
  l := nitems * size;
  Inc(FBytesRead, l);
  Result := FStream.Write(buffer^, l);
  cl := FindResponseHeaderValue('Content-Length');
  if TryStrToInt64(cl, v) then
  begin
    if Assigned(OnDataReceived) then
      OnDataReceived(Self, v, FBytesRead);
  end;
end;

function TTMSFNCUnixCloudBaseHTTPClient.ReceiveHeader(buffer: Pointer; size, nitems: size_t): size_t;
var
  h: string;
  p: Integer;
begin
  h := Trim(UTF8ToString(buffer));
  p := Pos(':', h);
  if p > 0 then
    FResponseHeaders.Add(TTMSFNCCloudBaseRequestHeader.Create(Trim(Copy(h, 0, p - 1)), Trim(Copy(h, p + 1, Length(h) - 1))));

  Result := size * nitems;
end;

function TTMSFNCUnixCloudBaseHTTPClient.SendData(buffer: Pointer; size, nitems: size_t): size_t;
begin
  if FBytesTotalSize > FBytesWritten then
  begin
    Result := FRequestBody.Read(buffer^, size * nitems);
    Inc(FBytesWritten, Result);
    if Assigned(FRequest.OnProgress) and (FBytesTotalSize > 0) then
      FRequest.OnProgress(FRequest, FBytesWritten / FBytesTotalSize * 100, False);
  end
  else
    Result := 0;
end;

class function TTMSFNCUnixCloudBaseHTTPClient.CurlReadData(buffer: Pointer; size, nitems: size_t;
  instream: Pointer): size_t;
begin
  if instream <> nil then
    Result := TTMSFNCUnixCloudBaseHTTPClient(instream).ReceiveData(buffer, size, nitems)
  else
    Result := 0;
end;

class function TTMSFNCUnixCloudBaseHTTPClient.CurlReadHeaders(buffer: Pointer; size: size_t; nitems: size_t; instream: Pointer): size_t;
begin
  if instream <> nil then
    Result := TTMSFNCUnixCloudBaseHTTPClient(instream).ReceiveHeader(buffer, size, nitems)
  else
    Result := 0;
end;

class function TTMSFNCUnixCloudBaseHTTPClient.CurlWriteData(buffer: Pointer; size, nitems: size_t; instream: Pointer): size_t;
begin
  if instream <> nil then
    Result := TTMSFNCUnixCloudBaseHTTPClient(instream).SendData(buffer, size, nitems)
  else
    Result := 0;
end;

procedure TTMSFNCUnixCloudBaseHTTPClient.HTTPMethod(AMethod: string; AURL: string; AStream: TStream; AAllowedCodes: TTMSFNCUnixCloudBaseAllowedResponseCodes);
var
  c: TCurlCode;
  lc: NativeInt;
  s: string;
  ls: curl_off_t;
  mm: TTMSFNCCloudBaseRequestMethod;

  function GetRequestMethod(AMethod: string): TTMSFNCCloudBaseRequestMethod;
  begin
    Result := rmGET;
    if AMethod = 'POST' then
      Result := rmPOST
    else if AMethod = 'PUT' then
      Result := rmPUT
    else if AMethod = 'DELETE' then
      Result := rmDELETE
    else if AMethod = 'PATCH' then
      Result := rmPATCH;
  end;
begin
  FBytesTotalSize := 0;
  FBytesWritten := 0;
  FBytesRead := 0;

  FStream := AStream;
  if Assigned(FStream) then
    FStream.Position := 0;

  curl_easy_setopt(FNativeRequest, CURLOPT_URL, UTF8String(AURL));

  mm := GetRequestMethod(AMethod);
  case mm of
    rmGET, rmDELETE, rmPATCH: curl_easy_setopt(FNativeRequest, CURLOPT_CUSTOMREQUEST, PUTF8Char(UTF8String(AMethod)));
    rmPUT:
    begin
      curl_easy_setopt(FNativeRequest, CURLOPT_UPLOAD, 1);
      curl_easy_setopt(FNativeRequest, CURLOPT_READFUNCTION, @CurlWriteData);
      curl_easy_setopt(FNativeRequest, CURLOPT_READDATA, Self);
    end;
    rmPOST:
    begin
      curl_easy_setopt(FNativeRequest, CURLOPT_POST, 1);
      curl_easy_setopt(FNativeRequest, CURLOPT_READFUNCTION, @CurlWriteData);
      curl_easy_setopt(FNativeRequest, CURLOPT_READDATA, Self);
    end;
  end;

  case mm of
    rmPOST, rmPUT:
    begin
      FHeaders := curl_slist_append(FHeaders, MarshaledAString(UTF8String('Expect:')));

      ls := 0;
      if Assigned(FRequestBody) then
        ls := FRequestBody.Size;

      FBytesTotalSize := ls;
      FBytesWritten := 0;

      if mm = rmPUT then
        curl_easy_setopt(FNativeRequest, CURLOPT_INFILESIZE_LARGE, ls)
      else
        curl_easy_setopt(FNativeRequest, CURLOPT_POSTFIELDSIZE, ls);
    end;
  end;

  if FHeaders <> nil then
    curl_easy_setopt(FNativeRequest, CURLOPT_HTTPHEADER, FHeaders);

  c := curl_easy_perform(FNativeRequest);
  case c of
    CURLE_OK:
    begin
      if curl_easy_getinfo(FNativeRequest, CURLINFO_RESPONSE_CODE, @lc) = CURLE_OK then
        FResponseStatusCode := lc;
    end;
    else
    begin
      s := UTF8ToString(@FError[0]);
      if FStream is TStringStream then
        (FStream as TStringStream).WriteString(s);
    end;
  end;
end;

procedure TTMSFNCUnixCloudBaseHTTPClient.AddHeader(AName: string; AValue: string);
begin
  FHeaders := curl_slist_append(FHeaders, MarshaledAString(UTF8String(AName + ': ' + AValue)));
end;

function TTMSFNCUnixCloudBaseHTTPClient.FindResponseHeaderValue(AName: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to FResponseHeaders.Count - 1 do
  begin
    if FResponseHeaders[I].Name = AName then
    begin
      Result := FResponseHeaders[I].Value;
      Break;
    end;
  end;
end;
{$ENDIF}
{$ENDIF}

initialization
begin
  {$IFDEF LCLLIB}
  InitSSLInterface;
  {$ENDIF}
end;

end.

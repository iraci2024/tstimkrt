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

unit LCLTMSFNCCloudBaseWin;

{$I LCLTMSFNCDefines.inc}

{$IFDEF WIN32}
{$HPPEMIT ''}
{$HPPEMIT '#pragma link "wininet.lib"'}
{$HPPEMIT ''}
{$ENDIF}

{$IFDEF WIN64}
{$HPPEMIT ''}
{$HPPEMIT '#pragma link "wininet.a"'}
{$HPPEMIT ''}
{$ENDIF}

interface

procedure RegisterCloudBaseService;
procedure UnRegisterCloudBaseService;

implementation

uses
  Classes, Types, LCLTMSFNCCloudBase, SysUtils, LCLTMSFNCUtils
  {$IFDEF MSWINDOWS}
  ,Windows, WinInet
  {$ENDIF}
  ;

type
  TTMSFNCWinCloudBaseService = class;

  TTMSFNCWinCloudBaseService = class(TTMSFNCCloudBaseFactoryService)
  protected
    function DoCreateCloudBase(const AValue: TTMSFNCCustomCloudBase): ITMSFNCCustomCloudBase; override;
  end;

  TTMSFNCWinCloudBase = class(TInterfacedObject, ITMSFNCCustomCloudBase)
  private
    FCloudBaseInstance: TTMSFNCCustomCloudBase;
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
    CloudBaseService := TTMSFNCWinCloudBaseService.Create;
    TTMSFNCCloudBasePlatformServices.Current.AddPlatformService(ITMSFNCCloudBaseService, CloudBaseService);
  end;
end;

procedure UnregisterCloudBaseService;
begin
  TTMSFNCCloudBasePlatformServices.Current.RemovePlatformService(ITMSFNCCloudBaseService);
end;

procedure TTMSFNCWinCloudBase.RunExternalBrowser(const AURL: string; const ACallBackURL: string; const ACallBack: TTMSFNCCloudBaseExternalBrowserCallBackEvent = nil);
begin
  TTMSFNCUtils.OpenURL(AURL);
end;

procedure TTMSFNCWinCloudBase.CloseBrowser;
begin
//
end;

constructor TTMSFNCWinCloudBase.Create(const ACloudBaseInstance: TTMSFNCCustomCloudBase);
begin
  FCloudBaseInstance := ACloudBaseInstance;
end;

{ TTMSFNCWinCloudBaseService }

function TTMSFNCWinCloudBaseService.DoCreateCloudBase(const AValue: TTMSFNCCustomCloudBase): ITMSFNCCustomCloudBase;
begin
  Result := TTMSFNCWinCloudBase.Create(AValue);
end;

type
  TTMSFNCCloudBaseRequestResultOpen = class(TTMSFNCCloudBaseRequestResult);

procedure TTMSFNCWinCloudBase.ExecuteRequest(const ARequest: TTMSFNCCloudBaseRequestResult);
{$IFDEF MSWINDOWS}
const
  MAXBUFFERSIZE = 4096;
  MAXFILEBUFFERSIZE = 64 * 1024;
var
  hdr: array of Char;
  hdrb: Boolean;
  hhp: Integer;
  hc, hh, hv: string;
  hcsl: TStringList;
  NetHandle: HINTERNET;
  buf: array of Char;
  Buffer: array[0..MAXBUFFERSIZE - 1] of Byte;
  BytesRead: DWORD;
  h, u, sh: string;
  filesz: Int64;
  lpdwlen, lpdwidx, lpdword: DWORD;
  m: TStream;
  ConnectHandle, RequestHandle: HINTERNET;
  Flags: DWORD;
  head, tail: string;
  bufsize, w: DWORD;
  bufferin: INTERNET_BUFFERS;
  sz: Int64;
  ss: TStream;
  b: LongBool;
  fs: TFileStream;
  I: Integer;

  procedure Progress(AProgress: Single; AUpload: Boolean);
  begin
    if Assigned(ARequest.OnProgress) then
      ARequest.OnProgress(ARequest, AProgress, AUpload);
  end;

  procedure WriteStreamInRequest(AStream: TStream; ADoUpload: Boolean);
  var
    bf: TMemoryStream;
    Len, BuffSize: Integer;
  begin
    AStream.Position := 0;
    BuffSize := AStream.Size;
    bf := TMemoryStream.Create;
    bf.SetSize(MAXBUFFERSIZE);
    try
      while ARequest.CheckTaskStatus do
      begin
        Len := BuffSize - AStream.Position;
        if Len > MAXBUFFERSIZE then
          Len := MAXBUFFERSIZE;
        if Len = 0 then
          Break;

        Len := AStream.Read(bf.Memory^, Len);
        InternetWriteFile(RequestHandle, @bf.Memory^, Len, lpdword);
        if ADoUpload then
          Progress(AStream.Position / BuffSize * 100, True);
      end;
    finally
      bf.Free;
    end;
  end;
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  try
    if ARequest.CheckTaskStatus then
    begin
      h := ARequest.GenerateHeaders;

      NetHandle := InternetOpen(PChar(ARequest.Agent), INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);

      if Assigned(NetHandle) then
      begin
        if ARequest.CheckTaskStatus then
        begin
          ConnectHandle := nil;
          RequestHandle := nil;
          try
            u := ARequest.GetURL;
            if u <> '' then
            begin
              case ARequest.Method of
                rmGET:
                begin
                  if h <> '' then
                    RequestHandle := InternetOpenUrl(NetHandle, PChar(u), @h[1], Length(h), INTERNET_FLAG_RELOAD, 0)
                  else
                    RequestHandle := InternetOpenUrl(NetHandle, PChar(u), nil, 0, INTERNET_FLAG_RELOAD, 0);
                end;
                rmPOST, rmPATCH, rmPUT, rmUPDATE, rmPOSTMULTIPART,
                rmPOSTMULTIPARTRELATED, rmPUTMULTIPART, rmPUTMULTIPARTRELATED, rmDELETE:
                begin
                  ConnectHandle := InternetConnect(NetHandle, PChar(ARequest.GetServer), ARequest.GetPort, nil, nil, INTERNET_SERVICE_HTTP, 0, 0);
                  Flags := INTERNET_FLAG_PASSIVE;
                  Flags := Flags or INTERNET_FLAG_KEEP_CONNECTION;
                  if Pos('HTTPS', UpperCase(ARequest.Host)) > 0 then                  
                    Flags := Flags or INTERNET_FLAG_SECURE;

                  RequestHandle := HttpOpenRequest(ConnectHandle, PChar(ARequest.GetMethodString), PChar(ARequest.GetQueryPath), HTTP_VERSION, '', nil, Flags, 0);

                  if h <> '' then
                    HttpAddRequestHeaders(RequestHandle, @h[1], length(h), HTTP_ADDREQ_FLAG_ADD);

                  sz := GetUploadFileSize(ARequest);

                  if not ARequest.CustomHeaders and (ARequest.Method in [rmPOST, rmPUT, rmPATCH]) and (ARequest.HasUploadStream or ARequest.HasUploadFile) then
                  begin
                    sh := 'Content-Length: ' + IntToStr(sz);
                    HttpAddRequestHeaders(RequestHandle, @sh[1], Length(sh), HTTP_ADDREQ_FLAG_ADD or HTTP_ADDREQ_FLAG_REPLACE);

                    sh := 'Content-Type: application/octet-stream';
                    HttpAddRequestHeaders(RequestHandle, @sh[1], Length(sh), HTTP_ADDREQ_FLAG_ADD or HTTP_ADDREQ_FLAG_REPLACE);

                    sh := 'Content-Transfer-Encoding: binary';
                    HttpAddRequestHeaders(RequestHandle, @sh[1], Length(sh), HTTP_ADDREQ_FLAG_ADD or HTTP_ADDREQ_FLAG_REPLACE);
                  end;

                  head := '';
                  tail := '';

                  if (ARequest.Method in [rmPOSTMULTIPART, rmPUTMULTIPART]) then
                  begin
                    head := sRequestHeadBoundary + #13#10;
                    tail := #13#10 + sRequestTailBoundary + #13#10;
                    sh := 'Content-Type: multipart/form-data; boundary=' + sRequestBoundary;
                    HttpAddRequestHeaders(RequestHandle, @sh[1], Length(sh), HTTP_ADDREQ_FLAG_ADD);
                  end;

                  if (ARequest.Method in [rmPOSTMULTIPARTRELATED, rmPUTMULTIPARTRELATED]) then
                  begin
                    head := ''#13#10;
                    tail := #13#10 + sRequestTailBoundary + #13#10;
                    sh := 'Content-Type: multipart/related; boundary=' + sRequestBoundary;
                    HttpAddRequestHeaders(RequestHandle, @sh[1], Length(sh), HTTP_ADDREQ_FLAG_ADD);
                  end;

                  FillChar(bufferin, SizeOf(bufferin),0);
                  bufferin.dwStructSize := SizeOf(INTERNET_BUFFERS);
                  ss := TBytesStream.Create(TEncoding.UTF8.GetBytes({$IFDEF LCLLIB}UTF8Decode{$ENDIF}(head + ARequest.PostData + tail)));
                  try
                    bufferin.dwBufferTotal := sz + ss.Size;
                  finally
                    ss.Free;
                  end;

                  b := HttpSendRequestEx(RequestHandle, @bufferin, nil, HSR_INITIATE, 0);
                  if not b and (GetLastError = ERROR_INTERNET_CLIENT_AUTH_CERT_NEEDED) then
                    b := HttpSendRequestEx(RequestHandle, @bufferin, nil, HSR_INITIATE, 0);

                  if b then
                  begin
                    if (ARequest.Method in [rmPUTMULTIPART, rmPOSTMULTIPART, rmPOSTMULTIPARTRELATED, rmPUTMULTIPARTRELATED]) and (head <> '') then
                    begin
                      ss := TBytesStream.Create(TEncoding.UTF8.GetBytes({$IFDEF LCLLIB}UTF8Decode{$ENDIF}(head)));
                      try
                        WriteStreamInRequest(ss, False);
                      finally
                        ss.Free;
                      end;
                    end;

                    if (ARequest.PostData <> '') then
                    begin
                      ss := TBytesStream.Create(TEncoding.UTF8.GetBytes({$IFDEF LCLLIB}UTF8Decode{$ENDIF}(ARequest.PostData)));
                      try
                        WriteStreamInRequest(ss, False);
                      finally
                        ss.Free;
                      end;
                    end;

                    if ARequest.HasUploadFile then
                    begin
                      fs := TFileStream.Create(ARequest.UploadFile, fmOpenRead or fmShareDenyWrite);
                      try
                        bufsize := MAXFILEBUFFERSIZE;
                        w := 0;
                        SetLength(buf, bufsize);
                        while w < sz do
                        begin
                          bufsize := MAXFILEBUFFERSIZE;
                          if sz - w < bufsize then
                            bufsize := sz - w;

                          fs.ReadBuffer(buf[0], bufsize);

                          if not InternetWriteFile(RequestHandle, @buf[0], bufsize, lpdword) or not ARequest.CheckTaskStatus then
                            Break;

                          if lpdword < bufsize then
                            fs.Position := fs.Position - (bufsize - lpdword);

                          w := w + lpdword;
                          if sz > 0 then
                            Progress(w / sz * 100, True);
                        end;
                      finally
                        fs.Free;
                      end;
                    end
                    else if ARequest.HasUploadStream then
                      WriteStreamInRequest(ARequest.UploadStream, True);

                    if (ARequest.Method in [rmPUTMULTIPART, rmPOSTMULTIPART, rmPOSTMULTIPARTRELATED, rmPUTMULTIPARTRELATED]) and (tail <> '') then
                    begin
                      ss := TStringStream.Create(tail{$IFNDEF LCLLIB}, TEncoding.UTF8{$ENDIF});
                      try
                        WriteStreamInRequest(ss, False);
                      finally
                        ss.Free;
                      end;
                    end;

                    HttpEndRequest(RequestHandle, nil, 0, 0);
                  end;
                end;
              end;

              if Assigned(RequestHandle) then
              begin
                if ARequest.CheckTaskStatus then
                begin
                  lpdwlen := 0;
                  lpdwidx := 0;

                  hdrb := HttpQueryInfo(RequestHandle, HTTP_QUERY_RAW_HEADERS_CRLF, nil, lpdwlen, lpdwidx);
                  if (not hdrb) and (GetLastError = ERROR_INSUFFICIENT_BUFFER) then
                  begin
                    SetLength(hdr, lpdwlen);
                    hdrb := HttpQueryInfo(RequestHandle, HTTP_QUERY_RAW_HEADERS_CRLF, Pointer(hdr), lpdwlen, lpdwidx);
                  end;

                  if hdrb then
                  begin
                    SetString(hc, PChar(@hdr[0]), Length(hdr));
                    hc := Trim(hc);
                    hcsl := TStringList.Create;
                    try
                      hcsl.Text := hc;
                      for I := 0 to hcsl.Count -1 do
                      begin
                        hhp := Pos(':', hcsl[I]);
                        hh := Trim(Copy(hcsl[I], 1, hhp - 1));
                        hv := Trim(Copy(hcsl[I], hhp + 1, Length(hcsl[I]) - hhp));

                        ARequest.ResponseHeaders.Add(TTMSFNCCloudBaseRequestHeader.Create(hh, hv));
                      end;
                    finally
                      hcsl.Free;
                    end;
                  end;

                  lpdword := 0;
                  lpdwlen := 4;
                  lpdwidx := 0;
                  HttpQueryInfo(RequestHandle, HTTP_QUERY_STATUS_CODE or HTTP_QUERY_FLAG_NUMBER, @lpdword, lpdwlen, lpdwidx);
                  ARequest.ResponseCode := lpdword;

                  m := nil;
                  if not (ARequest.ResponseCode in ValidHTTPCodes) then
                    m := TStringStream.Create(''{$IFNDEF LCLLIB}, TEncoding.UTF8{$ENDIF})
                  else
                  begin
                    case ARequest.ResultType of
                      rrtString: m := TStringStream.Create(''{$IFNDEF LCLLIB}, TEncoding.UTF8{$ENDIF});
                      rrtStream: m := TMemoryStream.Create;
                      rrtFile: m := TFileStream.Create(ARequest.ResultFile, fmCreate);
                    end;
                  end;

                  try
                    if ARequest.ResultFileSize = -1 then
                    begin
                      lpdword := 0;
                      lpdwlen := 4;
                      lpdwidx := 0;
                      HttpQueryInfo(RequestHandle, HTTP_QUERY_CONTENT_LENGTH or HTTP_QUERY_FLAG_NUMBER, @lpdword, lpdwlen, lpdwidx);
                      filesz := lpdword;
                    end
                    else
                      filesz := ARequest.ResultFileSize;

                    TTMSFNCCloudBaseRequestResultOpen(ARequest).FTotalBytes := filesz;

                    w := 0;
                    repeat
                      BytesRead := 0;
                      InternetReadFile(RequestHandle, @Buffer, Sizeof(Buffer), BytesRead);
                      if Assigned(m) then
                        m.Write(Buffer, BytesRead);

                      w := w + bytesread;

                      TTMSFNCCloudBaseRequestResultOpen(ARequest).FBytesReceived := BytesRead;

                      if filesz > 0 then
                        Progress(w / filesz * 100, False);
                    until (BytesRead = 0) or (not ARequest.CheckTaskStatus);


                    if not (ARequest.ResponseCode in ValidHTTPCodes) then
                      ARequest.ResultString := TStringstream(m).DataString
                    else
                    begin
                      case ARequest.ResultType of
                        rrtString: ARequest.ResultString := TStringstream(m).DataString;
                        rrtStream:
                        begin
                          ARequest.ResultStream.LoadFromStream(m);
                          ARequest.ResultStream.Position := 0;
                        end;
                      end;
                    end;

                    if filesz = 0 then
                      Progress(100, False);
                  finally
                    if Assigned(m) then
                      m.Free;

                    InternetCloseHandle(RequestHandle);
                  end;
                end;
              end
              else
                raise Exception.CreateFmt('Cannot open URL %s', [u]);
            end;
          finally
            case ARequest.Method of
              rmPOST:
              begin
                if Assigned(ConnectHandle) then
                  InternetCloseHandle(ConnectHandle);

                if Assigned(RequestHandle) then
                  InternetCloseHandle(RequestHandle);
              end;
              rmPUT: ;
              rmDELETE: ;
              rmPATCH: ;
            end;
            InternetCloseHandle(NetHandle);
          end;
        end;
      end
      else
        raise Exception.Create('Unable to initialize Wininet');
    end;
  finally
    TTMSFNCCloudbaseRequestResultOpen(ARequest).FDone := True;
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

function TTMSFNCWinCloudBase.GetUploadFileSize(const ARequest: TTMSFNCCloudBaseRequest): Int64;
  {$IFDEF MSWINDOWS}
  function FileSize(const AFilename: String): Int64;
  var
    AttributeData: TWin32FileAttributeData;
  begin
    if GetFileAttributesEx(PChar(AFileName), GetFileExInfoStandard, @AttributeData) then
    begin
      Int64Rec(Result).Lo := AttributeData.nFileSizeLow;
      Int64Rec(Result).Hi := AttributeData.nFileSizeHigh;
    end
    else
      Result := -1;
  end;
  {$ENDIF}
begin
  Result := 0;
  {$IFDEF MSWINDOWS}
  if ARequest.HasUploadFile then
    Result := FileSize(ARequest.UploadFile)
  else if ARequest.HasUploadStream then
  begin
    ARequest.UploadStream.Position := 0;
    Result := ARequest.UploadStream.Size;
  end;
  {$ENDIF}
end;

end.

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

unit FMX.TMSFNCCloudBase.Mac;

{$I FMX.TMSFNCDefines.inc}

interface

procedure RegisterCloudBaseService;
procedure UnRegisterCloudBaseService;

implementation

uses
  Classes, FMX.TMSFNCCloudBase, FMX.TMSFNCUtils, SysUtils, FMX.TMSFNCWebBrowser
  {$IFDEF MACOS}
  {$IFDEF IOS}
  ,iOSApi.Foundation, iOSApi.CocoaTypes
  {$ELSE}
  ,MacApi.Foundation, MacApi.CocoaTypes
  {$ENDIF}
  ,MacApi.Helpers, MacApi.ObjectiveC
  {$ENDIF}
  ;

type
  {$IFDEF MACOS}
  NSURLConnectionDelegate = interface(IObjectiveC)
    ['{467F147F-72A0-4E83-A58C-31011E08E8EA}']
    [MethodName('connection:didFailWithError:')]
    procedure connectionDidFailWithError(connection: NSURLConnection; didFailWithError: NSError); cdecl;
  end;

  NSURLConnectionDataDelegate = interface(IObjectiveC)
    ['{3F84DA8E-5A4F-4F87-8B64-7A98754308FD}']
    [MethodName('connection:didReceiveResponse:')]
    procedure connectionDidReceiveResponse(connection: NSURLConnection; didReceiveResponse: NSURLResponse); cdecl;
    [MethodName('connection:didReceiveData:')]
    procedure connectionDidReceiveData(connection: NSURLConnection; didReceiveData: NSData); cdecl;
    procedure connectionDidFinishLoading(connection: NSURLConnection); cdecl;
  end;

  TTMSFNCMacCloudBaseURLConnectionDelegate = class(TOCLocal, NSURLConnectionDelegate, NSURLConnectionDataDelegate)
  private
    FRequest: TTMSFNCCloudBaseRequestResult;
    FData: NSMutableData;
    FTotalSize: Int64;
  public
    constructor Create(const ARequest: TTMSFNCCloudBaseRequestResult);
    [MethodName('connection:didFailWithError:')]
    procedure connectionDidFailWithError(connection: NSURLConnection; didFailWithError: NSError); cdecl;
    [MethodName('connection:didReceiveResponse:')]
    procedure connectionDidReceiveResponse(connection: NSURLConnection; didReceiveResponse: NSURLResponse); cdecl;
    [MethodName('connection:didReceiveData:')]
    procedure connectionDidReceiveData(connection: NSURLConnection; didReceiveData: NSData); cdecl;
    procedure connectionDidFinishLoading(connection: NSURLConnection); cdecl;
    [MethodName('connection:willCacheResponse:')]
    function connectionWillCacheResponse(connection: NSURLConnection; willCacheResponse: NSCachedURLResponse): NSCachedURLResponse; cdecl;
  end;
  {$ENDIF}

  TTMSFNCCloudBaseRequestResultOpen = class(TTMSFNCCloudBaseRequestResult);

  TTMSFNCMacCloudBase = class;

  TTMSFNCMacCloudBaseService = class(TTMSFNCCloudBaseFactoryService)
  protected
    function DoCreateCloudBase(const AValue: TTMSFNCCustomCloudBase): ITMSFNCCustomCloudBase; override;
  end;

  TTMSFNCMacCloudBase = class(TInterfacedObject, ITMSFNCCustomCloudBase)
  private
    FCloudBaseInstance: TTMSFNCCustomCloudBase;
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

procedure RegisterCloudBaseService;
begin
  if not TTMSFNCCloudBasePlatformServices.Current.SupportsPlatformService(ITMSFNCCloudBaseService, IInterface(CloudBaseService)) then
  begin
    CloudBaseService := TTMSFNCMacCloudBaseService.Create;
    TTMSFNCCloudBasePlatformServices.Current.AddPlatformService(ITMSFNCCloudBaseService, CloudBaseService);
  end;
end;

procedure UnregisterCloudBaseService;
begin
  TTMSFNCCloudBasePlatformServices.Current.RemovePlatformService(ITMSFNCCloudBaseService);
end;

procedure TTMSFNCMacCloudBase.CloseBrowser;
begin
  FWebBrowserPopup.Close;
end;

constructor TTMSFNCMacCloudBase.Create(const ACloudBaseInstance: TTMSFNCCustomCloudBase);
begin
  FCloudBaseInstance := ACloudBaseInstance;
  FWebBrowserPopup := TTMSFNCWebBrowserPopup.Create(FCloudBaseInstance);
  FWebBrowserPopup.ExternalBrowser := True;
end;

destructor TTMSFNCMacCloudBase.Destroy;
begin
  FWebBrowserPopup.Free;
  inherited;
end;

function TTMSFNCMacCloudBaseService.DoCreateCloudBase(const AValue: TTMSFNCCustomCloudBase): ITMSFNCCustomCloudBase;
begin
  Result := TTMSFNCMacCloudBase.Create(AValue);
end;

procedure TTMSFNCMacCloudBase.ExecuteRequest(const ARequest: TTMSFNCCloudBaseRequestResult);
{$IFDEF MACOS}
var
  req: NSMutableURLRequest;
  nurl: NSURL;
  con: NSURLConnection;
  I: Integer;
  del: TTMSFNCMacCloudBaseURLConnectionDelegate;
  u: string;
  sz: Int64;
  head, tail: string;
  body: NSMutableData;
  d: NSData;
  m: TMemoryStream;

  procedure InternalWaitMessage(AInterval: Single);
  var
    TimeoutDate: NSDate;
  begin
    TimeoutDate := TNSDate.Wrap(TNSDate.OCClass.dateWithTimeIntervalSinceNow(AInterval));
    TNSRunLoop.Wrap(TNSRunLoop.OCClass.currentRunLoop).runMode(NSDefaultRunLoopMode, TimeoutDate);
    if TThread.CurrentThread.ThreadID <> MainThreadID then
      Sleep(Trunc(AInterval * 1000));
  end;
{$ENDIF}
begin
  {$IFDEF MACOS}
  try
    if ARequest.CheckTaskStatus then
    begin
      u := ARequest.GetURL;
      if u <> '' then
      begin
        nurl := TNSURL.Wrap(TNSURL.OCClass.URLWithString(StrToNSStr(u)));
        req := TNSMutableURLRequest.Wrap(TNSMutableURLRequest.OCClass.requestWithURL(nurl, NSURLRequestUseProtocolCachePolicy, 60.0));
        req.setHTTPMethod(StrToNSStr(ARequest.GetMethodString));

        sz := GetUploadFileSize(ARequest);
        head := '';
        tail := '';

        for I := 0 to ARequest.Headers.Count - 1 do
          req.setValue(StrToNSStr(ARequest.Headers[I].Value), StrToNSStr(ARequest.Headers[I].Name));

        if not ARequest.CustomHeaders and (ARequest.Method in [rmPOST, rmPUT, rmPATCH]) and (ARequest.HasUploadStream or ARequest.HasUploadFile) then
        begin
          req.setValue(NSStrEx(inttostr(sz)), NSStrEx('Content-Length'));
          req.setValue(NSStrEx('application/octet-stream'), NSStrEx('Content-Type'));
          req.setValue(NSStrEx('binary'), NSStrEx('Content-Transfer-Encoding'));
        end;

        if (ARequest.Method in [rmPOSTMULTIPART, rmPUTMULTIPART]) then
        begin
          head := sRequestHeadBoundary + #13#10;
          tail := #13#10 + sRequestTailBoundary + #13#10;
          req.setvalue(NSStrEx('multipart/form-data; boundary=' + sRequestBoundary), NSStrEx('Content-Type'));
        end;

        if (ARequest.Method in [rmPOSTMULTIPARTRELATED, rmPUTMULTIPARTRELATED]) then
        begin
          head := ''#13#10;
          tail := #13#10 + sRequestTailBoundary + #13#10;
          req.setvalue(NSStrEx('multipart/related; boundary=' + sRequestBoundary), NSStrEx('Content-Type'));
        end;

        body := TNSMutableData.Wrap(TNSMutableData.OCClass.data);
        if head <> '' then
          body.appendData(NSStrEx(head).dataUsingEncoding(NSUTF8StringEncoding));
        if ARequest.PostData <> '' then
          body.appendData(NSStrEx(ARequest.PostData).dataUsingEncoding(NSUTF8StringEncoding));        

        if ARequest.HasUploadFile then
        begin
          m := TMemoryStream.Create;
          try
            m.LoadFromFile(ARequest.UploadFile);
            m.Position := 0;
            d := TNSData.Wrap(TNSData.OCClass.dataWithBytes(m.Memory, m.size));
            body.appendData(d);
            if Assigned(ARequest.OnProgress) then
              ARequest.OnProgress(ARequest, 100, True);
          finally
            m.Free;
          end;
        end
        else if ARequest.HasUploadStream then
        begin
          ARequest.UploadStream.Position := 0;
          d := TNSData.Wrap(TNSData.OCClass.dataWithBytes(ARequest.UploadStream.Memory, ARequest.UploadStream.size));
          body.appendData(d);
          if Assigned(ARequest.OnProgress) then
            ARequest.OnProgress(ARequest, 100, True);
        end;

        if tail <> '' then
          body.appendData(NSStrEx(tail).dataUsingEncoding(NSUTF8StringEncoding));

        req.setHTTPBody(body);

        del := TTMSFNCMacCloudBaseURLConnectionDelegate.Create(ARequest);
        con := TNSURLConnection.Wrap(TNSURLConnection.Wrap(TNSURLConnection.OCClass.alloc).initWithRequest(req, del.GetObjectId, False));

        con.start;

        while not TTMSFNCCloudBaseRequestResultOpen(ARequest).FDone do
          InternalWaitMessage(0.01);

        del.Free;
        con.release;
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

function TTMSFNCMacCloudBase.GetUploadFileSize(
  const ARequest: TTMSFNCCloudBaseRequest): Int64;
{$IFDEF MACOS}
var
  dic: NSDictionary;
  fss: NSNumber;
{$ENDIF}
begin
  Result := 0;
  {$IFDEF MACOS}
  if ARequest.HasUploadFile then
  begin
    dic := TNSFileManager.Wrap(TNSFileManager.OCClass.defaultManager).attributesOfItemAtPath(NSStrEx(ARequest.UploadFile), nil);
    fss := TNSNumber.Wrap(dic.objectForKey((NSStrEx('NSFileSize') as ILocalObject).GetObjectID));
    Result := fss.intValue;
  end
  else if ARequest.HasUploadStream then
  begin
    ARequest.UploadStream.Position := 0;
    Result := ARequest.UploadStream.Size;
  end;
  {$ENDIF}
end;

procedure TTMSFNCMacCloudBase.RunExternalBrowser(const AURL: string; const ACallBackURL: string; const ACallBack: TTMSFNCCloudBaseExternalBrowserCallBackEvent = nil);
begin
  FWebBrowserPopup.Open(AURL);
end;

{$IFDEF MACOS}

{ TTMSFNCMacCloudBaseURLConnectionDelegate }

procedure TTMSFNCMacCloudBaseURLConnectionDelegate.connectionDidFailWithError(connection: NSURLConnection; didFailWithError: NSError);
begin
  FRequest.ResultString := UTF8ToString(didFailWithError.localizedDescription.UTF8String);
  TTMSFNCCloudBaseRequestResultOpen(FRequest).FDone := True;
end;

function TTMSFNCMacCloudBaseURLConnectionDelegate.connectionWillCacheResponse(connection: NSURLConnection; willCacheResponse: NSCachedURLResponse): NSCachedURLResponse;
begin
  Result := nil;
end;

procedure TTMSFNCMacCloudBaseURLConnectionDelegate.connectionDidFinishLoading(connection: NSURLConnection);
begin
  if Assigned(FRequest) then
  begin
    if (FRequest.ResponseCode in ValidHTTPCodes) then
    begin
      case FRequest.ResultType of
        rrtFile:
        begin
          try
            FData.writeToFile(NSStrEx(FRequest.ResultFile), False);
          finally
          end;
        end;
      end;
    end;

    if Assigned(FRequest.OnProgress) and (FTotalSize <= 0) then
      FRequest.OnProgress(FRequest, 100, False);

    TTMSFNCCloudBaseRequestResultOpen(FRequest).FDone := True;
  end;

  if Assigned(FData) then
  begin
    FData.release;
    FData := nil;
  end;
end;

procedure TTMSFNCMacCloudBaseURLConnectionDelegate.connectionDidReceiveData(connection: NSURLConnection; didReceiveData: NSData);
var
  dataStr: NSString;
begin
  if Assigned(FData) then
  begin
    FData.appendData(didReceiveData);
    TTMSFNCCloudBaseRequestResultOpen(FRequest).FTotalBytes := FTotalSize;
    TTMSFNCCloudBaseRequestResultOpen(FRequest).FBytesReceived := FData.length;

    if Assigned(FRequest.OnProgress) and (FTotalSize > 0) then
      FRequest.OnProgress(FRequest, FData.length / FTotalSize * 100, False);

    if Assigned(FRequest) then
    begin
      if not (FRequest.ResponseCode in ValidHTTPCodes) then
      begin
        dataStr := TNSString.Wrap((TNSString.Wrap(TNSString.OCClass.alloc)).initWithData(FData, NSUTF8StringEncoding));
        FRequest.ResultString := UTF8ToString(dataStr.UTF8String);
        dataStr.release;
      end
      else
      begin
        case FRequest.ResultType of
          rrtString:
          begin
            dataStr := TNSString.Wrap((TNSString.Wrap(TNSString.OCClass.alloc)).initWithData(FData, NSUTF8StringEncoding));
            FRequest.ResultString := UTF8ToString(dataStr.UTF8String);
            dataStr.release;
          end;
          rrtStream:
          begin
            FRequest.ResultStream.Write(FData.bytes^, FData.length);
            FRequest.ResultStream.Position := 0;
          end;
        end;
      end;
    end;
  end;
end;

procedure TTMSFNCMacCloudBaseURLConnectionDelegate.connectionDidReceiveResponse(connection: NSURLConnection;
  didReceiveResponse: NSURLResponse);
var
  res: NSHTTPURLResponse;
  k: Pointer;
  I: Integer;
  hh, hv: NSString;
begin
  FData := TNSMutableData.Wrap(TNSMutableData.Wrap(TNSMutableData.OCClass.alloc).init);
  FTotalSize := didReceiveResponse.expectedContentLength;

  if Assigned(FRequest) then
  begin
    res := TNSHTTPURLResponse.Wrap((didReceiveResponse as ILocalObject).GetObjectID);
    FRequest.ResponseCode := res.statusCode;
    for I := 0 to res.allHeaderFields.allKeys.count - 1 do
    begin
      k := res.allHeaderFields.allKeys.objectAtIndex(I);
      hh := TNSString.Wrap(k);
      hv := TNSString.Wrap(res.allHeaderFields.valueForKey(hh));
      FRequest.ResponseHeaders.Add(TTMSFNCCloudBaseRequestHeader.Create(UTF8ToString(hh.UTF8String), UTF8ToString(hv.UTF8String)));
    end;
  end;
end;

constructor TTMSFNCMacCloudBaseURLConnectionDelegate.Create(
  const ARequest: TTMSFNCCloudBaseRequestResult);
begin
  inherited Create;
  FRequest := ARequest;
end;

{$ENDIF}

end.

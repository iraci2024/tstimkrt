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

unit FMX.TMSFNCCloudBase.WEB;

{$I FMX.TMSFNCDefines.inc}

interface

procedure RegisterCloudBaseService;
procedure UnRegisterCloudBaseService;

implementation

uses
  Classes, Types, SysUtils, FMX.TMSFNCCloudBase, FMX.TMSFNCTypes
  {$IFDEF WEBLIB}
  ,web
  {$ENDIF}
  ;

type
  TTMSFNCWEBCloudBaseService = class;

  TTMSFNCWEBCloudBaseService = class(TTMSFNCCloudBaseFactoryService)
  protected
    function DoCreateCloudBase(const AValue: TTMSFNCCustomCloudBase): ITMSFNCCustomCloudBase; override;
  end;

  TTMSFNCCloudBaseRequestResultOpen = class(TTMSFNCCloudBaseRequestResult);

  TTMSFNCWEBCloudBase = class(TInterfacedObject, ITMSFNCCustomCloudBase)
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
    CloudBaseService := TTMSFNCWEBCloudBaseService.Create;
    TTMSFNCCloudBasePlatformServices.Current.AddPlatformService(ITMSFNCCloudBaseService, CloudBaseService);
  end;
end;

procedure UnregisterCloudBaseService;
begin
  TTMSFNCCloudBasePlatformServices.Current.RemovePlatformService(ITMSFNCCloudBaseService);
end;

procedure TTMSFNCWEBCloudBase.RunExternalBrowser(const AURL: string; const ACallBackURL: string; const ACallBack: TTMSFNCCloudBaseExternalBrowserCallBackEvent = nil);
{$IFNDEF WEBLIB}
begin
{$ELSE}
var
  s, sc: string;
  cb: TTMSFNCCloudBaseExternalBrowserCallBackEvent;
begin
  cb := ACallBack;
  s := AURL;
  sc := ACallBackURL;
  asm
    this.OAuthPopup = function(options)
    {

        var l, t, w, h;
        w = 800;
        h = 600;

        var dualScreenLeft = window.screenLeft != undefined ? window.screenLeft : window.screenX;
        var dualScreenTop = window.screenTop != undefined ? window.screenTop : window.screenY;

        var width = window.innerWidth ? window.innerWidth : document.documentElement.clientWidth ? document.documentElement.clientWidth : screen.width;
        var height = window.innerHeight ? window.innerHeight : document.documentElement.clientHeight ? document.documentElement.clientHeight : screen.height;

        var systemZoom = width / window.screen.availWidth;
        l = (width - w) / 2 / systemZoom + dualScreenLeft
        t = (height - h) / 2 / systemZoom + dualScreenTop

        options.windowName = options.windowName ||  'Authentication'; // should not include space for IE
        options.windowOptions = options.windowOptions || 'location=0,status=0,left='+l+',top='+t+',width='+w+',height='+h+'';
        options.callback = options.callback || function(){ window.location.reload(); };
        var that = this;
        that._oauthWindow = window.open(options.path, options.windowName, options.windowOptions);
        that._oauthInterval = window.setInterval(function(){
            var s, b;
            if (that._oauthWindow && that._oauthWindow.location){
              try{
                 s = that._oauthWindow.location.href;
                 if (!s){
                   s = that._oauthWindow.location;
                 }

                 b = s.indexOf(sc) != -1;
                 if (b) {
                   that._oauthWindow.close();
                   window.clearInterval(that._oauthInterval);
                   if (b){
                     options.callback(s);
                   }
                 }
              }catch(exception){
              }
            }

        }, 1000);
    };

    this.OAuthPopup({
        path: s,
        callback: function(callbackURL)
        {
          if (cb) {
            cb(callbackURL);
          }
        }
    });
  end;
{$ENDIF}
end;

procedure TTMSFNCWEBCloudBase.CloseBrowser;
begin
//
end;

constructor TTMSFNCWEBCloudBase.Create(const ACloudBaseInstance: TTMSFNCCustomCloudBase);
begin
  FCloudBaseInstance := ACloudBaseInstance;
end;

{ TTMSFNCWEBCloudBaseService }

function TTMSFNCWEBCloudBaseService.DoCreateCloudBase(const AValue: TTMSFNCCustomCloudBase): ITMSFNCCustomCloudBase;
begin
  Result := TTMSFNCWEBCloudBase.Create(AValue);
end;

procedure TTMSFNCWEBCloudBase.ExecuteRequest(const ARequest: TTMSFNCCloudBaseRequestResult);
{$IFNDEF WEBLIB}
begin
{$ELSE}
var
  u: string;
  up: Boolean;
  mt: string;
  sz: Int64;
  head, tail: string;
  h: TTMSFNCCloudBaseRequestHeaders;
  b: TBytes;

  procedure AddHeader(const AName, AValue: string);
  begin
    h.Add(TTMSFNCCloudBaseRequestHeader.Create(AName, AValue));
  end;

begin
  try
    u := ARequest.GetURL;
    mt := ARequest.GetMethodString;
    sz := GetUploadFileSize(ARequest);
    head := '';
    tail := '';

    h := TTMSFNCCloudBaseRequestHeaders.Create;
    up := (ARequest.HasUploadStream or ARequest.HasUploadFile);

    if not ARequest.CustomHeaders and (ARequest.Method in [rmPOST, rmPUT, rmPATCH]) and (ARequest.HasUploadStream or ARequest.HasUploadFile) then
    begin
      //AddHeader('Content-Length', inttostr(sz));
      AddHeader('Content-Type', 'application/octet-stream');
      AddHeader('Content-Transfer-Encoding', 'binary');
    end;

    if (ARequest.Method in [rmPOSTMULTIPART, rmPUTMULTIPART]) then
    begin
      head := sRequestHeadBoundary + #13#10;
      tail := #13#10 + sRequestTailBoundary + #13#10;
      AddHeader('Content-Type', 'multipart/form-data; boundary=' + sRequestBoundary);
    end;

    if (ARequest.Method in [rmPOSTMULTIPARTRELATED, rmPUTMULTIPARTRELATED]) then
    begin
      head := ''#13#10;
      tail := #13#10 + sRequestTailBoundary + #13#10;
      AddHeader('Content-Type', 'multipart/related; boundary=' + sRequestBoundary);
    end;

    asm
      var arr = [];
    end;

    if head <> '' then
    begin
      asm
        arr.push(head);
      end;
    end;

    if ARequest.PostData <> '' then
    begin
      asm
        arr.push(ARequest.FPostData);
      end;
    end;

    if ARequest.HasUploadFile then
    begin
      asm
        arr.push(ARequest.FUploadFile.Data);
      end;
      if Assigned(ARequest.OnProgress) then
        ARequest.OnProgress(ARequest, 100, True);
    end
    else if ARequest.HasUploadStream then
    begin
      ARequest.UploadStream.Position := 0;

      b := ARequest.UploadStream.Bytes;

      asm
        var byteArray = new Uint8Array(b);
        arr.push(byteArray);
      end;

      if Assigned(ARequest.OnProgress) then
        ARequest.OnProgress(ARequest, 100, True);
    end;

    if tail <> '' then
    begin
      asm
        arr.push(tail);
      end;
    end;

    if mt <> '' then
    begin
      asm
        function reqListener(e)
        {
          var $tmp1 = ARequest.FResultType;

          if (!(ARequest.FResponseCode in pas["WEBLib.TMSFNCCloudBase"].ValidHTTPCodes)){
            if (!this.responseType == "arrayBuffer"){
              ARequest.FResultString = this.responseText;
            }
          }
          else
          {
            if ($tmp1 === pas["WEBLib.TMSFNCCloudBase"].TTMSFNCCloudBaseRequestResultType.rrtString) {
              ARequest.FResultString = this.responseText;
            }
            else if ($tmp1 === pas["WEBLib.TMSFNCCloudBase"].TTMSFNCCloudBaseRequestResultType.rrtFile) {
              var byteArray = new Uint8Array(this.response);

              var userAgent = navigator.userAgent.toLowerCase();
              if (userAgent.indexOf('electron/') > -1)
              {
                require('fs').writeFile(ARequest.FResultFile, byteArray, (err) => {});
              }
              else
              {
                var m = pas["Classes"].TMemoryStream.$create("Create");
                try
                {
                  m.Write(byteArray, byteArray.length);
                  pas["WEBLib.TMSFNCTypes"].TTMSFNCMemoryStream.SaveToFile.call(m, ARequest.FResultFile);
                }
                finally
                {
                  m = rtl.freeLoc(m);
                }
              }
            }
            else
            {
              var byteArray = new Uint8Array(this.response);
              ARequest.FResultStream.Write(byteArray, byteArray.length);
            }
          }

          ARequest.FDone = true;
          if (ARequest.CheckTaskStatus()) {
            if (ARequest.FOnProgress != null && e.total == 0) ARequest.FOnProgress(ARequest, 100, false);
            if (ARequest.FOnComplete != null) ARequest.FOnComplete(ARequest);
          }
          else
          {
            if (ARequest.FOnCancelled != null) ARequest.FOnCancelled(ARequest);
          }
        }

        const req = new XMLHttpRequest();
        req.addEventListener('load', reqListener);
        req.onreadystatechange = () => {
          ARequest.FResponseCode = req.status;

          if(req.readyState == 2){
            // Get the raw header string
            var headers = req.getAllResponseHeaders();

            // Convert the header string into an array
            // of individual headers
            var arr = headers.trim().split(/[\r\n]+/);

            // Create a map of header names to values
            arr.forEach(function (line) {
              var parts = line.split(': ');
              var header = parts.shift();
              var value = parts.join(': ');
              if (value != ""){
                ARequest.FResponseHeaders.Add$1(pas["WEBLib.TMSFNCCloudBase"].TTMSFNCCloudBaseRequestHeader.$create("Create$1",[header,value]));
              }
            });
          }
        };
        req.upload.addEventListener("progress", function(event){
          if (ARequest.CheckTaskStatus() && up && event.total > 0) {
            if (ARequest.FOnProgress != null) ARequest.FOnProgress(ARequest, event.loaded / event.total * 100, true);
          };
        }, false);
        req.onprogress = function(event){
          if (ARequest.CheckTaskStatus() && !up && event.total > 0) {
            ARequest.FTotalBytes = event.total;
            ARequest.FBytesReceived = event.loaded;
            if (ARequest.FOnProgress != null) ARequest.FOnProgress(ARequest, event.loaded / event.total * 100, false);
          };
        };

        var $tmp1 = ARequest.FResultType;
        if ($tmp1 === pas["WEBLib.TMSFNCCloudBase"].TTMSFNCCloudBaseRequestResultType.rrtStream || $tmp1 === pas["WEBLib.TMSFNCCloudBase"].TTMSFNCCloudBaseRequestResultType.rrtFile) {
          req.responseType = 'arraybuffer';
        }

        req.open(mt, u);

        for (var $l1 = 0, $end2 = ARequest.FHeaders.GetCount() - 1; $l1 <= $end2; $l1++) {
            var I = $l1;
            var hd = ARequest.FHeaders.GetItem$1(I);
            req.setRequestHeader(hd.FName, hd.FValue);
        };

        for (var $l1 = 0, $end2 = h.GetCount() - 1; $l1 <= $end2; $l1++) {
            var I = $l1;
            var hd = h.GetItem$1(I);
            req.setRequestHeader(hd.FName, hd.FValue);
        };

        try
        {
          if (arr.length > 0){
            var b = new Blob(arr);
            req.send(b);
          }
          else{
            req.send();
          }
        }catch(exception){
        }
      end;
    end
    else
    begin
      if ARequest.CheckTaskStatus then
      begin
        TTMSFNCCloudbaseRequestResultOpen(ARequest).FDone := True;
        if Assigned(ARequest.OnComplete) then
          ARequest.OnComplete(ARequest);
      end;    
    end;
  finally
  end;
{$ENDIF}
end;

function TTMSFNCWEBCloudBase.GetUploadFileSize(
  const ARequest: TTMSFNCCloudBaseRequest): Int64;
begin
  Result := 0;
  {$IFDEF WEBLIB}
  if ARequest.HasUploadFile then
  begin
    Result := ARequest.UploadFile.Data.length;
  end
  else if ARequest.HasUploadStream then
  begin
    ARequest.UploadStream.Position := 0;
    Result := ARequest.UploadStream.Size;
  end;
  {$ENDIF}
end;

end.

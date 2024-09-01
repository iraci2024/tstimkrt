{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2022                                        }
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

unit FMX.TMSFNCWhatsAppServer;

{$I FMX.TMSFNCDefines.inc}

interface

uses
  Classes, FMX.TMSFNCWebSocketServer, IdContext, IdCustomHTTPServer,
  FMX.TMSFNCTypes;

type
  TTMSFNCWhatsAppServerMessageEvent = procedure(Sender: TObject; AMessage: string; var ABroadcast: Boolean) of object;

  TTMSFNCCustomWhatsAppServer = class(TTMSFNCCustomWebSocketServer)
  private
    FVerifyToken: string;
    FOnMessage: TTMSFNCWhatsAppServerMessageEvent;
  protected
    function GetInstance: NativeUInt; override;
    function GetDocURL: string; override;
    procedure DoHTTPRequest(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo); override;
    procedure DoRawMessage(AMessage: string; var ABroadcast: Boolean); virtual;
    property VerifyToken: string read FVerifyToken write FVerifyToken;
    property OnRawMessage: TTMSFNCWhatsAppServerMessageEvent read FOnMessage write FOnMessage;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  {$IFNDEF LCLLIB}
  [ComponentPlatformsAttribute(TMSPlatforms)]
  {$ENDIF}
  TTMSFNCWhatsAppServer = class(TTMSFNCCustomWhatsAppServer)
  published
    property AutoSyncEvents;
    property Port;
    property PathName;
    property Active;
    property VerifyToken;
    property CertificateFile;
    property CertificateKeyfile;
    property RootCertificateFile;
    property OnConnect;
    property OnAllow;
    property OnDisconnect;
    property OnPing;
    property OnPong;
    property OnClose;
    property OnRawMessage;
    property OnGetSSLPassword;
    property OnCommandGet;
    property OnMessageReceived;
    property OnBinaryDataReceived;
  end;

implementation

uses
  IdGlobal;

{$R TMSFNCWhatsAppServer.res}

{ TTMSFNCCustomWhatsAppServer }

constructor TTMSFNCCustomWhatsAppServer.Create(AOwner: TComponent);
begin
  inherited;
  UseSSL := True;
end;

procedure TTMSFNCCustomWhatsAppServer.DoHTTPRequest(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
   strm: TStream;
   S: string;
   b: Boolean;
begin
  inherited;
  if ARequestInfo.Command = 'POST' then
  begin
    strm := ARequestInfo.PostStream;
    if Assigned(strm) then
    begin
      strm.Position := 0;
      S := ReadStringFromStream(strm);

      b := True;
      DoRawMessage(s, b);
      if b then
        BroadcastMessage(s); //Broadcast to ALL connected clients
    end
  end
  else if ARequestInfo.Command = 'GET' then
  begin
    if VerifyToken = ARequestInfo.Params.values['hub.verify_token'] then
      AResponseInfo.ContentText := ARequestInfo.Params.Values['hub.challenge'];
  end
end;

procedure TTMSFNCCustomWhatsAppServer.DoRawMessage(AMessage: string;
  var ABroadcast: Boolean);
begin
  if Assigned(OnRawMessage) then
    OnRawMessage(Self, AMessage, ABroadcast);
end;

function TTMSFNCCustomWhatsAppServer.GetDocURL: string;
begin
  Result := 'https://download.tmssoftware.com/doc/tmsfncwebsocket/components/ttmsfncwhatsappserver';
end;

function TTMSFNCCustomWhatsAppServer.GetInstance: NativeUInt;
begin
  Result := HInstance;
end;

end.

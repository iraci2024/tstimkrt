{***************************************************************************
 sgcWebSocket component

 written by eSeGeCe
            copyright © 2022
            Email : info@esegece.com
            Web : http://www.esegece.com
***************************************************************************}

unit sgcWebSocket_Protocol_WebRTC_Message;

interface

{$I sgcVer.inc}

{$IFDEF SGC_PROTOCOLS}

uses
  Classes, SysUtils,
  // sgc
  sgcWebSocket_Protocol_sgc_message;

type
  TsgcWSMessageWebRTC = class(TsgcWSMessage)
  private
    FWebRTC: string;
  protected
    procedure DoJSONWebRTC;
  public
    procedure Read(const aMessage: String); override;
    function Write: string; override;
  published
    property WebRTC: string read FWebRTC write FWebRTC;
  end;

{$ENDIF}

implementation

{$IFDEF SGC_PROTOCOLS}

uses
  sgcWebSocket_Const, sgcJSON;


procedure TsgcWSMessageWebRTC.Read(const aMessage: String);
begin
  DoEnterRead(aMessage);

  WebRTC := '';

  JSON.Read(aMessage);
  if JSON.Node['webrtc'] <> nil then
    WebRTC := String(JSON.Node['webrtc'].JSONObject.Text);

  inherited;
end;

procedure TsgcWSMessageWebRTC.DoJSONWebRTC;
begin
  if (WebRTC <> '') and (WebRTC <> '{}') then
  begin
    Method := CS_SGC_WEBRTC;

    JSON.AddObject('webrtc', WebRTC);
  end;
end;

function TsgcWSMessageWebRTC.Write: string;
begin
  DoEnterWrite;

  DoJSONWebRTC;

  result := inherited Write;
end;

{$ENDIF}

end.

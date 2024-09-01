
//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2007-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF DAC}
{$I SB.inc}
unit ScSocketController;
{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$IFNDEF FPC}
  {$IFNDEF BCB}
  {$NOINCLUDE winsock}
  {$HPPEMIT '#include <winsock2.h>'}
  {$ENDIF}
  WinSock,
{$ENDIF}
{$ENDIF}
  SysUtils, SyncObjs, Classes,
  ScCLRClasses, ScTypes, ScVio, ScVioTcp,
  ScUtils, ScBridge, ScSSLTypes, ScReceiveBuffer,
  ScLayers, ScSSLMessages, ScSSLExtensions;

type
  TSecureSocketController = class(TSecureController)
  private
    FVio: TCRVio;

    procedure OnReceive(ar: IScAsyncResult);
    procedure CloseVio(Sender: TObject);

    procedure SendDataToTarget(const Data: TValueArr; Offset, Count: integer);
    function ReceiveDataFromSource(const Data: TValueArr; Offset, Count: integer): integer;

  public
    constructor Create(Vio: TCRVio; Options: TScSecurityOptions);

    procedure ClearVio;
    procedure ReStart; override;
    procedure StartTLS; override;
    function Read(const Buffer: TValueArr; Offset, Count: integer): integer;
    function ReadNoWait(const Buffer: TValueArr; Offset, Count: integer): integer;
  end;

implementation

uses
{$IFDEF VER17P}
  Types,
{$ENDIF}
  ScConsts, ScSecureSocket,
  ScClientHandshakeLayer, ScServerHandshakeLayer;

{ TSecureSocketController }

constructor TSecureSocketController.Create(Vio: TCRVio; Options: TScSecurityOptions);
var
  HandshakeLayerClass: THandshakeLayerClass;
begin
  if Options.Entity = ceClient then
    HandshakeLayerClass := TClientHandshakeLayer
  else
    HandshakeLayerClass := TServerHandshakeLayer;

  inherited Create(Options, HandshakeLayerClass);

  OnSendDataToTarget := SendDataToTarget;
  OnReceiveDataFromSource := ReceiveDataFromSource;
  OnClose := CloseVio;

  FVio := Vio;
end;

procedure TSecureSocketController.ClearVio;
begin
  FVio := nil;
end;

procedure TSecureSocketController.CloseVio(Sender: TObject);
begin
  if FVio <> nil then
    FVio.Close;
end;

procedure TSecureSocketController.SendDataToTarget(const Data: TValueArr; Offset, Count: integer);
var
  Sent: integer;
begin
  while Count > 0 do begin
    Sent := FVio.Write(Data, Offset, Count);
    if Sent = 0 then begin
      if FVio.LastError <> '' then
        raise SocketException.Create(FVio.LastError, FVio.LastErrorCode)
      else
        raise EScError.Create(seCannotWriteSocketData);
    end;

    Inc(Offset, Sent);
    Dec(Count, Sent);
  end;
end;

function TSecureSocketController.ReceiveDataFromSource(const Data: TValueArr; Offset, Count: integer): integer;
begin
  Result := FVio.ReadNoWait(Data, Offset, Count);
  if (Result <= 0) and (FVio.LastError <> '') then
    raise SocketException.Create(FVio.LastError, FVio.LastErrorCode);
end;

procedure TSecureSocketController.ReStart;
begin
  FVio.NonBlocking := False;
  inherited;
end;

procedure TSecureSocketController.StartTLS;
begin
  try
    inherited;

    FReceiveMessage.CheckAndRealloc(MAX_RECORD_FRAGMENT_LENGTH * 2);
    FVio.BeginReceive(FReceiveMessage.Fragment, FReceiveMessage.WriteOffset, MAX_RECORD_FRAGMENT_LENGTH * 2, OnReceive, nil);
  except
    on e: Exception do begin
      CloseConnection(e);
      raise;
    end;
  end;
end;

procedure TSecureSocketController.OnReceive(ar: IScAsyncResult);
var
  Status: TScSSLStatus;
  Size: integer;
begin
  try
    Size := FVio.EndReceive(ar);
    FReceiveMessage.WriteOffset := FReceiveMessage.WriteOffset + Size;

    if Size = 0 then begin
      CloseConnection(nil); // connection has been shut down
    end
    else begin
      Status := ProcessReceivedData;

      if (Status = ssShuttedDown) and not FOptions.DisableCloseSocketOnShutdownAlert then // Record Layer instructs us to shut down
        CloseConnection(nil);

      if not FIsDisposed then begin
        if Status = ssShuttedDown then begin
          if FReceiveMessage.WriteOffset <> FReceiveMessage.ReadOffset then
            raise EScError.Create(seDataSentAfterShutdown);

          FVio.StopAsync;
        end
        else begin
          FReceiveMessage.CheckAndRealloc(MAX_RECORD_FRAGMENT_LENGTH * 2);
          FVio.BeginReceive(FReceiveMessage.Fragment, FReceiveMessage.WriteOffset, MAX_RECORD_FRAGMENT_LENGTH * 2, OnReceive, nil);
        end;
      end;
    end;
  except
    on e: Exception do
      CloseConnection(e);
  end;
end;

function TSecureSocketController.Read(const Buffer: TValueArr; Offset, Count: integer): integer;
begin
  RaiseLastError;
  FDecryptedBuffer.WaitForData(Count, cardinal(FOptions.Timeout));
  Result := FDecryptedBuffer.Read(Buffer, Offset, Count);
  if (Result = 0) and not FVio.Connected then
    Result := -1;

  RaiseLastError;
end;

function TSecureSocketController.ReadNoWait(const Buffer: TValueArr; Offset, Count: integer): integer;
begin
  RaiseLastError;
  FDecryptedBuffer.WaitForData(1, cardinal(FOptions.Timeout));
  Result := FDecryptedBuffer.Read(Buffer, Offset, Count);
  if (Result = 0) and not FVio.Connected then
    Result := -1;

  RaiseLastError;
end;

end.

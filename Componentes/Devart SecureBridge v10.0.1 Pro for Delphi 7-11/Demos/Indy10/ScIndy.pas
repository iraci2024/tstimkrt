unit ScIndy;

interface

uses
{$IFDEF VER200}
  {$DEFINE VER12P}
{$ENDIF}
{$IFDEF VER210}
  {$DEFINE VER12P}
{$ENDIF}
{$IFDEF VER220}
  {$DEFINE VER12P}
{$ENDIF}
{$IFDEF VER230}
  {$DEFINE VER12P}
  {$DEFINE VER16P}
{$ENDIF}
{$IFDEF VER240}
  {$DEFINE VER12P}
  {$DEFINE VER16P}
{$ENDIF}
{$IFDEF VER250}
  {$DEFINE VER12P}
  {$DEFINE VER16P}
{$ENDIF}
{$IFDEF VER260}
  {$DEFINE VER12P}
  {$DEFINE VER16P}
{$ENDIF}
{$IFDEF VER270}
  {$DEFINE VER12P}
  {$DEFINE VER16P}
{$ENDIF}
{$IFDEF VER280}
  {$DEFINE VER12P}
  {$DEFINE VER16P}
{$ENDIF}
{$IFDEF VER290}
  {$DEFINE VER12P}
  {$DEFINE VER16P}
{$ENDIF}
{$IFDEF VER300}
  {$DEFINE VER12P}
  {$DEFINE VER16P}
{$ENDIF}
{$IFDEF VER310}
  {$DEFINE VER12P}
  {$DEFINE VER16P}
{$ENDIF}
{$IFDEF VER320}
  {$DEFINE VER12P}
  {$DEFINE VER16P}
{$ENDIF}
{$IFDEF VER330}
  {$DEFINE VER12P}
  {$DEFINE VER16P}
{$ENDIF}
{$IFDEF VER340}
  {$DEFINE VER12P}
  {$DEFINE VER16P}
{$ENDIF}
{$IFDEF VER350}
  {$DEFINE VER12P}
  {$DEFINE VER16P}
{$ENDIF}


{$IFDEF VER180}
  IdObjs,
{$ELSE}
  Classes,
{$ENDIF}
{$IFDEF VER160}
  IdCoreGlobal,
{$ELSE}
  IdGlobal,
{$ENDIF}
  ScTypes, IdComponent, IdIOHandler, IdIOHandlerSocket,
  ScSSHClient, ScSSHChannel;

type
{$IFDEF VER16P}
  [ComponentPlatformsAttribute({$IFDEF STD}pidDevartWinPlatforms{$ELSE}pidDevartAllPlatforms{$ENDIF})]
{$ENDIF}
  TScIdIOHandler = class(TIdIOHandlerSocket)
  protected
    FClient: TScSSHClient;
    FSSHChannel: TScSSHChannel;

    procedure SetClient(Value: TScSSHClient);
  {$IFDEF VER180}
    procedure Notification(AComponent: TIdNativeComponent; Operation: TIdOperation); override;
  {$ELSE}
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  {$ENDIF}

    procedure ConnectClient; override;
    {$IFNDEF VER12P}
    function ReadFromSource(ARaiseExceptionIfDisconnected: Boolean = True;
      ATimeout: Integer = IdTimeoutDefault;
      ARaiseExceptionOnTimeout: Boolean = True): Integer; override;
    {$ENDIF}
    function SourceIsAvailable: Boolean; {$IFDEF VER12P}override;{$ENDIF}
    function ReadDataFromSource(var VBuffer: TIdBytes): Integer; {$IFDEF VER12P}override;{$ENDIF}

  {$IFDEF VER160}
    procedure WriteToDestination(ABuffer: TIdBytes); override;
  {$ENDIF}
    function WriteDataToTarget(const ABuffer: TIdBytes; const AOffset, ALength: Integer): Integer; {$IFDEF VER12P}override;{$ENDIF}

  public
    destructor Destroy; override;
    procedure Close; override;
    function Connected: Boolean; override;

  {$IFNDEF VER12P}
    procedure CheckForDataOnSource(ATimeout: Integer = 0); override;
  {$ENDIF}
    procedure CheckForDisconnect(ARaiseExceptionIfDisconnected: Boolean = True;
      AIgnoreBuffer: Boolean = False); override;
    function Readable(AMSec: Integer = IdTimeoutDefault): Boolean; override;

  {$IFNDEF VER160}
  {$IFNDEF VER12P}
    procedure WriteDirect({$IFNDEF VER170}var{$ENDIF} aBuffer: TIdBytes); override;
  {$ENDIF}
  {$ENDIF}

  published
    property Client: TScSSHClient read FClient write SetClient;
    property ReadTimeout;
  end;

implementation

uses
{$IFDEF VER160}
  IdCoreResourceStrings,
{$ELSE}
  IdResourceStrings, IdResourceStringsCore, IdExceptionCore,
{$ENDIF}
  IdException, IdStack, IdAntiFreezeBase,
{$IFDEF VER12P}
  Math,
{$ENDIF}
  SysUtils, ScConsts, ScUtils;

{ TScIdIOHandler }

destructor TScIdIOHandler.Destroy;
begin
  FreeAndNil(FSSHChannel);

  inherited Destroy;
end;

procedure TScIdIOHandler.Close;
begin
  inherited Close;

  if Assigned(FSSHChannel) then
    FSSHChannel.Disconnect;
end;

procedure TScIdIOHandler.ConnectClient;
var
  IP: string;
begin
  if IPVersion = Id_IPv4 then
  begin
    if not GStack.IsIP(Host) then begin
      if Assigned(OnStatus) then begin
        DoStatus(hsResolving, [Host]);
      end;
      IP := GStack.ResolveHost(Host, FIPVersion);
    end
    else begin
      IP := Host;
    end;
  end
  else
  begin  //IPv6
    IP := MakeCanonicalIPv6Address(Host);
    if IP = '' then begin  //if MakeCanonicalIPv6Address failed, we have a hostname
      if Assigned(OnStatus) then begin
        DoStatus(hsResolving, [Host]);
      end;
      IP := GStack.ResolveHost(Host, IPVersion);
    end else begin
      IP := Host;
    end;
  end;
  Binding.SetBinding(IP, Port{$IFNDEF VER170}, IPVersion{$ENDIF});
  Binding.SetPeer(IP, Port{$IFNDEF VER170}, IPVersion{$ENDIF});

  if Client = nil then
    raise EScError.Create(SClientNotDefined);

  // Connect
  if Assigned(OnStatus) then begin
    DoStatus(hsConnecting, [Binding.PeerIP]);
  end;

{$IFNDEF VER160}
  if ConnectTimeout > 0 then
    Client.Timeout := ConnectTimeout div 1000;
{$ENDIF}

  if FSSHChannel = nil then
    FSSHChannel := TScSSHChannel.Create(nil);

  if ReadTimeout < 0 then
    FSSHChannel.Timeout := MaxInt
  else
    FSSHChannel.Timeout := ReadTimeout div 1000;

  FSSHChannel.Client := Client;
  FSSHChannel.Direct := True;
  FSSHChannel.DestHost := IP;
  FSSHChannel.DestPort := Port;
  FSSHChannel.Connect;
end;

function TScIdIOHandler.Connected: Boolean;
begin
  Result := SourceIsAvailable or not InputBufferIsEmpty;
end;

function TScIdIOHandler.ReadDataFromSource(var VBuffer: TIdBytes): Integer;
begin
  Assert(FSSHChannel <> nil);
  Result := FSSHChannel.ReadBuffer(TBytes(VBuffer), 0, Min(FSSHChannel.InCount, Length(VBuffer)));
end;

function TScIdIOHandler.SourceIsAvailable: Boolean;
begin
  Result := (FSSHChannel <> nil) and (FSSHChannel.Connected or (FSSHChannel.InCount > 0));
end;

{$IFNDEF VER12P}
// from Delphi2009
function TScIdIOHandler.ReadFromSource(ARaiseExceptionIfDisconnected: Boolean;
  ATimeout: Integer; ARaiseExceptionOnTimeout: Boolean): Integer;
var
  LByteCount: Integer;
  LBuffer: TIdBytes;
begin
  if ATimeout = IdTimeoutDefault then begin
    // MtW: check for 0 too, for compatibility
    if (ReadTimeout = IdTimeoutDefault) or (ReadTimeout = 0) then begin
      ATimeout := IdTimeoutInfinite;
    end else begin
      ATimeout := ReadTimeout;
    end;
  end;

  Result := 0;
  // Check here as this side may have closed the socket
  CheckForDisconnect(ARaiseExceptionIfDisconnected);
  if SourceIsAvailable then begin
    LByteCount := 0;
    repeat
      if Readable(ATimeout) then begin
        if Opened then begin
          // No need to call AntiFreeze, the Readable does that.
          if SourceIsAvailable then begin
            // TODO: Whey are we reallocating LBuffer every time? This should
            // be a one time operation per connection.
            SetLength(LBuffer, RecvBufferSize); try
              LByteCount := ReadDataFromSource(LBuffer);
              if LByteCount > 0 then begin
                SetLength(LBuffer, LByteCount);
                if Intercept <> nil then begin
                  Intercept.Receive(LBuffer);
                  LByteCount := Length(LBuffer);
                end;
    //AsciiFilter - needs to go in TIdIOHandler base class
    //            if ASCIIFilter then begin
    //              for i := 1 to IOHandler.RecvBuffer.Size do begin
    //                PChar(IOHandler.RecvBuffer.Memory)[i] := Chr(Ord(PChar(IOHandler.RecvBuffer.Memory)[i]) and $7F);
    //              end;
    //            end;
                // Pass through LBuffer first so it can go through Intercept
                //TODO: If not intercept, we can skip this step
                InputBuffer.Write(LBuffer);
              end;
            finally LBuffer := nil; end;
          end else begin
            EIdClosedSocket.Toss(RSStatusDisconnected);
          end;
        end else begin
          LByteCount := 0;
          EIdNotConnected.IfTrue(ARaiseExceptionIfDisconnected, RSNotConnected);
        end;
        if LByteCount = 0 then
          FClosedGracefully := True;
        // Check here as other side may have closed connection
        CheckForDisconnect(ARaiseExceptionIfDisconnected);
        Result := LByteCount;
      end else begin
        // Timeout
        EIdReadTimeout.IfTrue(ARaiseExceptionOnTimeout, RSReadTimeout);
        Result := -1;
        Break;
      end;
    until (LByteCount <> 0) or (not SourceIsAvailable);
  end
  else if ARaiseExceptionIfDisconnected then begin
    raise EIdException.Create(RSNotConnected);
  end;
end;
{$ENDIF}

{$IFNDEF VER12P}
procedure TScIdIOHandler.CheckForDataOnSource(ATimeout: Integer);
begin
  if Connected then begin
    ReadFromSource(False, ATimeout, False);
  end;
end;
{$ENDIF}

procedure TScIdIOHandler.CheckForDisconnect(
  ARaiseExceptionIfDisconnected: Boolean; AIgnoreBuffer: Boolean);
begin
  // ClosedGracefully // Server disconnected
  // IOHandler = nil // Client disconnected
  if ClosedGracefully then begin
    if SourceIsAvailable then begin
      TScSSHChannelUtils.CloseIfDisconnected(FSSHChannel);
      // Call event handlers to inform the user that we were disconnected
      DoStatus(hsDisconnected);
      //DoOnDisconnected;
    end;
  end;

  // Do not raise unless all data has been read by the user
  if ClosedGracefully or not SourceIsAvailable then begin
    if Assigned(FInputBuffer) then begin
      if ((FInputBuffer.Size = 0) or AIgnoreBuffer)
       and ARaiseExceptionIfDisconnected then begin
        RaiseConnClosedGracefully;
      end;
    end;
  end;
end;

function TScIdIOHandler.Readable(AMSec: integer): boolean;
begin
  Result := FSSHChannel <> nil;

  if Result then begin
    if AMSec = IdTimeoutInfinite then
      AMSec := MaxInt;

    Result := TScSSHChannelUtils.Readable(FSSHChannel, 1, AMSec);
    if not Result then
      Result := not FSSHChannel.Connected;
  end;
end;

function TScIdIOHandler.WriteDataToTarget(const ABuffer: TIdBytes; const AOffset, ALength: Integer): Integer;
begin
  Assert(FSSHChannel <> nil);
  Result := FSSHChannel.WriteBuffer(TBytes(ABuffer), AOffset, ALength);
end;

{$IFNDEF VER12P}
{$IFDEF VER160}
procedure TScIdIOHandler.WriteToDestination(ABuffer: TIdBytes);
{$ELSE}
procedure TScIdIOHandler.WriteDirect({$IFNDEF VER170}var{$ENDIF} aBuffer: TIdBytes);
{$ENDIF}
var
  LCount: Integer;
  LPos: Integer;
  LSize: Integer;
begin
{$IFDEF VER160}
  CheckForDisconnect(True, True);
  InterceptWrite(ABuffer);
{$ELSE}
  inherited;
{$ENDIF}

  LSize := Length(ABuffer);
  LPos := 0;
  repeat
    LCount := WriteDataToTarget(aBuffer, LPos, LSize - LPos);
    TIdAntiFreezeBase.DoProcess(False);
    if LCount = 0 then
      FClosedGracefully := True;

    // Check if other side disconnected
    CheckForDisconnect;
    DoWork(wmWrite, LCount);
    Inc(LPos, LCount);
  until LPos >= LSize;
end;
{$ENDIF}

{$IFDEF VER180}
procedure TScIdIOHandler.Notification(AComponent: TIdNativeComponent; Operation: TIdOperation);
{$ELSE}
procedure TScIdIOHandler.Notification(AComponent: TComponent; Operation: TOperation);
{$ENDIF}
begin
  if (Operation = opRemove) and (AComponent = FClient) then
    Client := nil;

  inherited Notification(AComponent, Operation);
end;

procedure TScIdIOHandler.SetClient(Value: TScSSHClient);
begin
  if Value <> FClient then begin
    if FClient <> nil then
      FClient.RemoveFreeNotification(Self);

    Close;
    FClient := Value;

    if Value <> nil then
      FClient.FreeNotification(Self);
  end;
end;

end.

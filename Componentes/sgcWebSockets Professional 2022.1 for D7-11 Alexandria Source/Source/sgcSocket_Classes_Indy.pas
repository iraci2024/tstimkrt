{ ***************************************************************************
  sgcSocket component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcSocket_Classes_Indy;

interface

{$I sgcVer.inc}

uses
  Classes, SysUtils, SyncObjs,
  // indy
  {$IFDEF SGC_INDY}sgcIdLogFile{$ELSE}IdLogFile{$ENDIF},
  {$IFDEF SGC_INDY}sgcIdServerInterceptLogFile{$ELSE}IdServerInterceptLogFile{$ENDIF},
  {$IFDEF SGC_INDY}sgcIdResourceStringsCore{$ELSE}IdResourceStringsCore{$ENDIF},
  {$IFDEF SGC_INDY}sgcIdGlobal{$ELSE}IdGlobal{$ENDIF};

type
  TsgcIdLogFileBase = class(TIdLogFile)
  private
    FLock: TIdCriticalSection;
  protected
    procedure InitComponent; override;
    procedure LogWriteString({$IFDEF INDY10_2}const {$ENDIF}AText: string); override;
  public
    destructor Destroy; override;
  public
    procedure EnterCS;
    procedure LeaveCS;
  public
    procedure LogSendString(const aText: string); virtual;
    procedure LogSendDecodedString(const aText: string); virtual;
    procedure LogRecvString(const aText: string); virtual;
    procedure LogRecvDecodedString(const aText: string); virtual;
  end;

  TsgcIdLogFileClient = class(TsgcIdLogFileBase)

  end;

  TsgcIdLogFileServer = class(TIdServerInterceptLogFile)

  end;

implementation

uses
  sgcBase_Helpers;

destructor TsgcIdLogFileBase.Destroy;
begin
  sgcFree(FLock);
  inherited;
end;

procedure TsgcIdLogFileBase.EnterCS;
begin
  FLock.Enter;
end;

procedure TsgcIdLogFileBase.InitComponent;
begin
  inherited;
  FLock := TIdCriticalSection.Create;
end;

procedure TsgcIdLogFileBase.LeaveCS;
begin
  FLock.Leave;
end;

procedure TsgcIdLogFileBase.LogRecvDecodedString(const aText: string);
begin
  LogWriteString(RSLogRecv + DateTimeToStr(Now) + ': ' + aText + EOL);
end;

procedure TsgcIdLogFileBase.LogRecvString(const aText: string);
begin
  LogReceivedData(DateTimeToStr(Now), aText);
end;

procedure TsgcIdLogFileBase.LogSendDecodedString(const aText: string);
begin
  LogWriteString(RSLogSent + DateTimeToStr(Now) + ': ' + aText + EOL);
end;

procedure TsgcIdLogFileBase.LogSendString(const aText: string);
begin
  LogSentData(DateTimeToStr(Now), aText);
end;

procedure TsgcIdLogFileBase.LogWriteString({$IFDEF INDY10_2}const {$ENDIF}AText: string);
begin
  EnterCS;
  Try
    inherited;
  Finally
    LeaveCS;
  End;
end;

end.

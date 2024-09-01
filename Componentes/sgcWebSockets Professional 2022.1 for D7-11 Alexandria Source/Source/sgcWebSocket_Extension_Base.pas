{***************************************************************************
 sgcWebSocket component

 written by eSeGeCe
            copyright © 2022
            Email : info@esegece.com
            Web : http://www.esegece.com
***************************************************************************}

unit sgcWebSocket_Extension_Base;

interface

uses
  Classes,
  sgcWebSocket_Types;

type
  TsgcWSExtension_Base = class(TPersistent)
  private
    FActive: Boolean;
    FEnabled: Boolean;
    FMode: TwsApplicationMode;
  protected
    FHeaderExtension: String;
  public
    function GetHeaderExtension: String; virtual;
  public
    function DecodeExtension(const aExtension: string): Boolean; virtual;
  public
    procedure Assign(aSource: TPersistent); override;
  public
    procedure DecodeHeader(aByte: Byte); virtual; abstract;
    procedure EncodeHeader(var aByte: Byte); virtual; abstract;
  public
    constructor Create; virtual;
  public
    function GetName: string; virtual; abstract;
  public
    property Mode: TwsApplicationMode read FMode write FMode;
  public
    property Active: Boolean read FActive write FActive;
  published
    property Enabled: Boolean read FEnabled write FEnabled;
  end;

implementation

uses
  sgcWebSocket_Const;

constructor TsgcWSExtension_Base.Create;
begin
  inherited;
  Mode := appNone;
  Enabled := False;
  Active := False;
end;

procedure TsgcWSExtension_Base.Assign(aSource: TPersistent);
begin
  if aSource.InheritsFrom(TsgcWSExtension_Base) then
    Enabled := TsgcWSExtension_Base(aSource).Enabled
  else
    inherited Assign(aSource);
end;

function TsgcWSExtension_Base.DecodeExtension(const aExtension: string):
    Boolean;
begin
  Result := False;
end;

function TsgcWSExtension_Base.GetHeaderExtension: String;
begin
  Result := CS_WS_EXTENSIONS + FHeaderExtension;
end;

end.



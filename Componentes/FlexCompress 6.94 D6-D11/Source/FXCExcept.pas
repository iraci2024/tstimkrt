unit FXCExcept;

{$I FXCVer.Inc}

interface

uses SysUtils, Classes;

type
  EFXCException = class(Exception)
  public
    ErrorCode:   integer;
    NativeError: integer;

    constructor Create(NativeErr: integer; Component: TComponent = nil); overload;
    constructor Create(NativeErr: integer; const Args: array of const;
      Component: TComponent = nil); overload;
  end;

implementation

uses FXCConst;

constructor EFXCException.Create(NativeErr: integer; Component: TComponent = nil);
var
  ErMessage, s: string;
  i: integer;
begin
  NativeError := NativeErr;

  ErrorCode := ErUnknownError;
  for i := 0 to FXCMaxNativeError do
    if (FXCNativeToErrorCode[i][0] = NativeErr) then
    begin
      ErrorCode := FXCNativeToErrorCode[i][1];
      break;
    end;
  ErMessage := FXCErrorMessages[ErrorCode];
  if Assigned(Component) and (Component.Name <> '') then
    ErMessage := Format('%s: %s', [Component.Name, ErMessage]);

  s := StringReplace(Format('%5d', [NativeError]), ' ', '0', [rfReplaceAll]);
  ErMessage := ErMessage + ' - Native error: ' + s;
  inherited Create(ErMessage);
end;


constructor EFXCException.Create(NativeErr: integer; const Args: array of const;
  Component: TComponent = nil);
var
  ErMessage, s: string;
  i: integer;
begin
  NativeError := NativeErr;

  ErrorCode := ErUnknownError;
  for i := 0 to FXCMaxNativeError do
    if (FXCNativeToErrorCode[i][0] = NativeErr) then
    begin
      ErrorCode := FXCNativeToErrorCode[i][1];
      break;
    end;
  ErMessage := FXCErrorMessages[ErrorCode];
  ErMessage := Format(ErMessage, Args);
  if Assigned(Component) and (Component.Name <> '') then
    ErMessage := Format('%s: %s', [Component.Name, ErMessage]);

  s := StringReplace(Format('%5d', [NativeError]), ' ', '0', [rfReplaceAll]);
  ErMessage := ErMessage + ' - Native error: ' + s;
  inherited Create(ErMessage);
end;

end.

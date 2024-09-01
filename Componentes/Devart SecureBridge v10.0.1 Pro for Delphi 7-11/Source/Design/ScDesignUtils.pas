
//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2007-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I SB.inc}

unit ScDesignUtils;

interface

uses
  Classes, SysUtils;

type
  TScDesignUtils = class
  public
    class function GetProjectName: string; virtual; // Returns ProjectName = ('DataEditor', 'ODAC', 'SDAC', 'MyDAC', ...)

  { Component }
    class function GetDesignCreate(Obj: TComponent): boolean; virtual;
    class procedure SetDesignCreate(Obj: TComponent; Value: boolean); virtual;

  {$IFDEF USE_SYNEDIT}
    class function SQLDialect: integer ; virtual; // SynHighlighterSQL TSQLDialect = (sqlStandard, sqlInterbase6, sqlMSSQL7, sqlMySQL, sqlOracle, sqlSybase, sqlIngres, sqlMSSQL2K);
  {$ENDIF}

    class function DBToolsAvailable: boolean; virtual;
  end;

  TScDesignUtilsClass = class of TScDesignUtils;

implementation

{ TScDesignUtils }

class function TScDesignUtils.GetProjectName: string;
begin
 Result := 'SecureBridge';
end;

class function TScDesignUtils.GetDesignCreate(Obj: TComponent): boolean;
begin
  Result := False;
  Assert(Obj <> nil);
  Assert(False, Obj.ClassName);
end;

class procedure TScDesignUtils.SetDesignCreate(Obj: TComponent; Value: boolean);
begin
  Assert(Obj <> nil);
  Assert(False, Obj.ClassName);
end;

{$IFDEF USE_SYNEDIT}
class function TScDesignUtils.SQLDialect: integer;
begin
  Result := 0; // sqlStandard
end;
{$ENDIF}

class function TScDesignUtils.DBToolsAvailable: boolean;
begin
  Result := False;
end;

end.

{***************************************************************************
 sgcWebSocket component

 written by eSeGeCe
            copyright © 2022
            Email : info@esegece.com
            Web : http://www.esegece.com
***************************************************************************}


unit sgcWebSocket_HTTPResponse;

interface

{$I sgcVer.inc}

uses
  Classes, SysUtils,
  {$IFDEF NEXTGEN}System.Generics.Collections{$ELSE}Contnrs{$ENDIF};

type
  TsgcWSHTTPResponse_Base = class;
  TsgcWSHTTPResponse_BaseClass = class of TsgcWSHTTPResponse_Base;

  TsgcWSHTTPResponse_Base = class(TObject)
  protected
    function GetResponse: string; virtual; abstract;
  public
    class function GetFileName: string; virtual;
  end;

{$IFDEF D2006}
  TsgcWSHTTPResponse = class(TObject)
  private
    class var FServiceClassList: {$IFDEF NEXTGEN} TList<TsgcWSHTTPResponse_BaseClass>{$ELSE}TClassList{$ENDIF};
  protected
    function GetFileNameClass(const aServiceName: string):
        TsgcWSHTTPResponse_BaseClass;
  public
    class procedure RegisterFileName(aClass: TsgcWSHTTPResponse_BaseClass);
  public
    function GetResponseMessage(const aFileName: String): string;
  end;

{$ELSE}
  TsgcWSHTTPResponseObject = class(TObject)
  private
    FServiceClassList: TClassList;
  protected
    function GetFileNameClass(const aServiceName: string):
        TsgcWSHTTPResponse_BaseClass;
  public
    procedure RegisterFileName(aClass: TsgcWSHTTPResponse_BaseClass);
  public
    function GetResponseMessage(const aFileName: String): string;
  end;

var
  TsgcWSHTTPResponse: TsgcWSHTTPResponseObject = nil;
{$ENDIF}

implementation

{ TsgcWSHTTPResponse }

uses sgcWebSocket_Helpers;

class function TsgcWSHTTPResponse_Base.GetFileName: string;
begin
  result := '';
end;

{$IFDEF D2006}
function TsgcWSHTTPResponse.GetResponseMessage(const aFileName: String):
    string;
var
  i: Integer;
  oClass: TsgcWSHTTPResponse_BaseClass;
  oInstance: TsgcWSHTTPResponse_Base;
  oList: TsgcDelimitedStringList;
begin
  Result := '';
  oList := TsgcDelimitedStringList.Create;
  Try
    oList.DelimitedText := aFileName;
    for i := 0 to oList.Count - 1 do
    begin
      oClass := GetFileNameClass(oList[i]);
      if Assigned(oClass) then
      begin
        oInstance := nil;
        try
          oInstance := oClass.Create;
          Result := Result + oInstance.GetResponse + #13#10;
        finally
          sgcFree(oInstance);
        end;
      end;
    end;
  Finally
    sgcFree(oList);
  End;
end;

function TsgcWSHTTPResponse.GetFileNameClass(const aServiceName: string):
    TsgcWSHTTPResponse_BaseClass;
var
  vCount: integer;
  oService: TsgcWSHTTPResponse_BaseClass;
begin
  Result := nil;
  for vCount := 0 to FServiceClassList.Count - 1 do
  begin
    oService := TsgcWSHTTPResponse_BaseClass(FServiceClassList[vCount]);
    if SameText(oService.GetFileName, aServiceName) then
    begin
      Result := oService;
      Exit;
    end;
  end;
end;

class procedure TsgcWSHTTPResponse.RegisterFileName(aClass:
    TsgcWSHTTPResponse_BaseClass);
begin
  FServiceClassList.Add(aClass);
end;

{$ELSE}

function TsgcWSHTTPResponseObject.GetResponseMessage(const aFileName: String):
    string;
var
  i: Integer;
  oClass: TsgcWSHTTPResponse_BaseClass;
  oInstance: TsgcWSHTTPResponse_Base;
  oList: TsgcDelimitedStringList;
begin
  Result := '';
  oList := TsgcDelimitedStringList.Create;
  Try
    oList.DelimitedText := aFileName;
    for i := 0 to oList.Count - 1 do
    begin
      oClass := GetFileNameClass(oList[i]);
      if Assigned(oClass) then
      begin
        oInstance := nil;
        try
          oInstance := oClass.Create;
          Result := Result + oInstance.GetResponse + #13#10;
        finally
          sgcFree(oInstance);
        end;
      end;
    end;
  Finally
    sgcFree(oList);
  End;
end;

function TsgcWSHTTPResponseObject.GetFileNameClass(const aServiceName: string):
    TsgcWSHTTPResponse_BaseClass;
var
  vCount: integer;
  oService: TsgcWSHTTPResponse_BaseClass;
begin
  Result := nil;
  for vCount := 0 to FServiceClassList.Count - 1 do
  begin
    oService := TsgcWSHTTPResponse_BaseClass(FServiceClassList[vCount]);
    if SameText(oService.GetFileName, aServiceName) then
    begin
      Result := oService;
      Exit;
    end;
  end;
end;

procedure TsgcWSHTTPResponseObject.RegisterFileName(aClass:
    TsgcWSHTTPResponse_BaseClass);
begin
  FServiceClassList.Add(aClass);
end;

{$ENDIF}

{$IFDEF D2006}
initialization
  TsgcWSHTTPResponse.FServiceClassList := {$IFDEF NEXTGEN}TList<TsgcWSHTTPResponse_BaseClass>.Create{$ELSE}TClassList.Create{$ENDIF};

finalization
  sgcFree(TsgcWSHTTPResponse.FServiceClassList);

{$ELSE}
initialization
  TsgcWSHTTPResponse := TsgcWSHTTPResponseObject.Create;
  TsgcWSHTTPResponse.FServiceClassList := TClassList.Create;

finalization
  sgcFree(TsgcWSHTTPResponse.FServiceClassList);
  sgcFree(TsgcWSHTTPResponse);
{$ENDIF}

end.

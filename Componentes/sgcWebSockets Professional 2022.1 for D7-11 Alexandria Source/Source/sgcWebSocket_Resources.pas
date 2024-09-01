{***************************************************************************
 sgcWebSocket component

 written by eSeGeCe
            copyright © 2022
            Email : info@esegece.com
            Web : http://www.esegece.com
***************************************************************************}

unit sgcWebSocket_Resources;

interface

uses
  Classes, SysUtils, StrUtils, {$IFDEF MSWINDOWS}Windows{$ELSE}Types{$ENDIF};

function GetResourceString(const aResource: String): string;
function GetResourceStream(const aResource: String; var aStream:
    TMemoryStream): Boolean;

implementation

uses
  sgcBase_Helpers;

function GetResourceString(const aResource: String): string;
var
  oStream: TResourceStream;
  oString: TsgcStringStream;
begin
  if aResource = '' then exit;

  oStream := TResourceStream.Create(HInstance, aResource, RT_RCDATA);
  try
    oString := TsgcStringStream.Create('');
    Try
      oString.CopyFrom(oStream, oStream.Size);
      result := oString.DataString;
    Finally
      sgcFree(oString);
    End;
  finally
    sgcFree(oStream);
  end;
end;

function GetResourceStream(const aResource: String; var aStream:
    TMemoryStream): Boolean;
var
  oResInfo: THandle;
  oHandle: THandle;
  oBuffer: {$IFDEF NEXTGEN}PChar{$ELSE}PAnsiChar{$ENDIF};
  size: Integer;
begin
  result := False;

  if aResource = '' then exit;

  oResInfo := FindResource(hInstance, PChar(aResource), RT_RCDATA);

  oHandle := LoadResource(hInstance, oResInfo);
  Try
    if oHandle <> 0 then
    begin
      oBuffer := LockResource(oHandle);
      Try
        size := SizeofResource(hInstance, oResInfo);
        {$IFDEF IOS64}
        aStream.SetSize(Int64(size));
        {$ELSE}
	   	    {$IFDEF LINUX64}
		      aStream.SetSize(Int64(size));
		      {$ELSE}
            {$IFDEF OSX64}
	  	      aStream.SetSize(Int64(size));
            {$ELSE}
              {$IFDEF ANDROID64}
  	  	      aStream.SetSize(Int64(size));
              {$ELSE}
	  	        aStream.SetSize(size);
              {$ENDIF}
            {$ENDIF}
		      {$ENDIF}
        {$ENDIF}
        aStream.Write(oBuffer^, size);
        aStream.Position := 0;

        result := True;
      Finally
        UnlockResource(oHandle);
      End;
    end;
  Finally
    FreeResource(oHandle);
  End;
end;

end.

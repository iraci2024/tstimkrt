{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2016                                        }
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

{$I TMSProductInfo.pas}

type
  TProductCategory = (pcVCL, pcFMX, pcFNC, pcBIZ, pcNET, pcIW, pcLCL, pcDEV, pcFREE, pcWEB);
  TProductKind = (pkTrial, pkRegistered, pkFree, pkX);
  TProductInfoArray = array of string;

const
  TMSProductCategoryColors: array[0..9] of string = ('#F44336', '#FF9800', '#BCC6CC', '#56B8FF', '#0068B4', '#4CAF50', '#9C00FF', '#DAA520', '#FF005A', '#00838F');
  TMSProductCategoryBackgroundColors: array[0..9] of string = ('#F6695E', '#FFAD33', '#CBD3D8', '#67BFFF', '#1977BB', '#5EB761', '#AB26FF', '#E1B74D', '#FF337B', '#00A2AB');

function GetProductKindName(AProductKind: TProductKind): string;
begin
  case AProductKind of
    pkTrial: Result := 'Trial';
    pkRegistered: Result := 'Registered';
    pkFree: Result := 'Free';
    pkX: Result := '';
  end;
end;

function GetProductCategoryName(AProductCategory: TProductCategory): string;
begin
  case AProductCategory of
    pcVCL: Result := 'VCL';
    pcFMX: Result := 'FMX';
    pcFNC: Result := 'FNC';
    pcBIZ: Result := 'BIZ';
    pcNET: Result := '.NET';
    pcIW: Result := 'IW';
    pcLCL: Result := 'LCL';
    pcDEV: Result := 'DEV';
    pcFREE: Result := 'FREE';
    pcWEB: Result := 'WEB';
  end;
end;

{$IFDEF FMXLIB}
function HTMLColorToTColor(AColor: String): TAlphaColor;
begin
  AColor := '$FF' + Copy(AColor,2,2) + Copy(AColor,4,2) + Copy(AColor,6,2);
  Result := StrToInt(AColor);
end;
{$ENDIF}
{$IFDEF VCLLIB}
function HTMLColorToTColor(AColor: String): TColor ;
begin
  AColor := '$' + Copy(AColor,6,2) + Copy(AColor,4,2) + Copy(AColor,2,2);
  Result := StrToInt(AColor);
end;
{$ENDIF}

function GetProductInfoURLs: TProductInfoArray;
var
  s: string;
  st: TStringList;
  I: Integer;
begin
  s := TMSProductInfo_ProductLinkTitleValues;
  st := TStringList.Create;
  st.StrictDelimiter := True;
  st.Delimiter := ',';
  st.DelimitedText := s;
  try
    SetLength(Result, st.Count);
    for I := 0 to st.Count - 1 do
      Result[I] := st[I];
  finally
    st.Free;
  end;
end;

function GetProductInfoNames: TProductInfoArray;
var
  s: string;
  st: TStringList;
  I: Integer;
begin
  s := TMSProductInfo_ProductLinkTitles;
  st := TStringList.Create;
  st.StrictDelimiter := True;
  st.Delimiter := ',';
  st.DelimitedText := s;
  try
    SetLength(Result, st.Count);
    for I := 0 to st.Count - 1 do
      Result[I] := st[I];
  finally
    st.Free;
  end;
end;

//

function GetProductName: string;
begin
  Result := TMSProductInfo_ProductName;
end;

function GetProductVersion: string;
begin
  Result := TMSProductInfo_ProductVersion;
end;

function GetProductKind: TProductKind;
var
  s: string;
begin
  s := TMSProductInfo_ProductKind;
  Result := pkRegistered;
  if UpperCase(s) = 'REGISTERED' then
    Result := pkRegistered
  else if UpperCase(s) = 'TRIAL' then
    Result := pkTrial
  else if UpperCase(s) = 'FREE' then
    Result := pkFree
   else if UpperCase(s) = 'X' then
     Result := pkX;
end;

function GetProductCategory: TProductCategory;
var
  s: string;
begin
  s := TMSProductInfo_ProductCategory;
  Result := pcVCL;
  if UpperCase(s) = 'VCL' then
   Result := pcVCL
  else if UpperCase(s) = 'FMX' then
   Result := pcFMX
  else if UpperCase(s) = 'FNC' then
   Result := pcFNC
  else if UpperCase(s) = 'BIZ' then
   Result := pcBIZ
  else if UpperCase(s) = 'NET' then
   Result := pcNET
  else if UpperCase(s) = 'IW' then
   Result := pcIW
  else if UpperCase(s) = 'LCL' then
   Result := pcLCL
  else if UpperCase(s) = 'DEV' then
   Result := pcDEV
  else if UpperCase(s) = 'FREE' then
   Result := pcFREE
  else if UpperCase(s) = 'WEB' then
   Result := pcWEB
end;


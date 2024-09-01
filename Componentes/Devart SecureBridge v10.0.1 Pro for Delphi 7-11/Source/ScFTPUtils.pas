
//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2007-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I SB.inc}
unit ScFTPUtils;

interface

uses
  Classes, SysUtils,
  ScCLRClasses, ScFunctions, ScUtils, ScConsts;

type
  TScDOSAttributes = class(TPersistent)
  protected
    FFileAttributes: cardinal;
    function GetReadOnly: boolean;
    procedure SetReadOnly(Value: boolean);
    function GetHidden: boolean;
    procedure SetHidden(Value: boolean);
    function GetSystem: boolean;
    procedure SetSystem(Value: boolean);
    function GetArchive: boolean;
    procedure SetArchive(Value: boolean);
    function GetDirectory: boolean;
    procedure SetDirectory(Value: boolean);
    function GetNormal: boolean;
    procedure SetNormal(Value: boolean);
  public
    procedure Assign(Source: TPersistent); override;
    function GetAsString: string; virtual;
    function AddAttribute(const Value: string): boolean;
  published
    property FileAttributes: cardinal read FFileAttributes write FFileAttributes;
    property AsString: string read GetAsString;
    property ReadOnly: boolean read GetReadOnly write SetReadOnly;
    property Archive: boolean read GetArchive write SetArchive;
    property System: boolean read GetSystem write SetSystem;
    property Directory: boolean read GetDirectory write SetDirectory;
    property Hidden: boolean read GetHidden write SetHidden;
    property Normal: boolean read GetNormal write SetNormal;
  end;

  TScWin32Attributes = class(TScDOSAttributes)
  protected
    function GetDevice: boolean;
    procedure SetDevice(Value: boolean);
    function GetTemporary: boolean;
    procedure SetTemporary(Value: boolean);
    function GetSparseFile: boolean;
    procedure SetSparseFile(Value: boolean);
    function GetReparsePoint: boolean;
    procedure SetReparsePoint(Value: boolean);
    function GetCompressed: boolean;
    procedure SetCompressed(Value: boolean);
    function GetOffline: boolean;
    procedure SetOffline(Value: boolean);
    function GetNotContextIndexed: boolean;
    procedure SetNotContextIndexed(Value: boolean);
    function GetEncrypted: boolean;
    procedure SetEncrypted(Value: boolean);
  public
    function GetAsString: string; override;
  published
    property Device: boolean read GetDevice write SetDevice;
    property Temporary: boolean read GetTemporary write SetTemporary;
    property SparseFile: boolean read GetSparseFile write SetSparseFile;
    property ReparsePoint: boolean read GetReparsePoint write SetReparsePoint;
    property Compressed: boolean read GetCompressed write SetCompressed;
    property Offline: boolean read GetOffline write SetOffline;
    property NotContextIndexed: boolean read GetNotContextIndexed write SetNotContextIndexed;
    property Encrypted: boolean read GetEncrypted write SetEncrypted;
  end;

function PatternsCount(const SearchPattern, Str: string): integer;
function IsSpaceLine(const Str: string): boolean;
function DetectFirstDelim(const Value: string): string;
function DetectFirstNumber(const Value: string; Default: integer): Int64;
function UnfoldLines(Listing: TStrings; Index: integer): string;

function IsDate(const Value: string; const Format: string): boolean;
function IsTime(const Value: string; const Format: string): boolean;
function StrToDate(const Value: string; const Format: string): TDateTime;
function StrToTime(const Value: string; const Format: string): TDateTime;
function CheckYear4(const Year: integer): integer;
function DetectYear(const Day, Month: cardinal): cardinal;

function IsValidEmulUnixPermissions(const Value: string): boolean;
function IsValidUnixPermissions(const Value: string): boolean;
procedure PermissionsToStr(const Chmod: integer; out Value: string); overload;
procedure PermissionsToStr(const Chmod: integer; out User, Group, Other: string); overload;

const
{$IFNDEF IS_UNICODE}
  // from www.FileFormat.info
  KoreanTotal    = #$EC#$B4#$9D;
  KoreanMonth    = #$EC#$9B#$94; // Hangul Syllable Ieung Weo Rieul
  KoreanDay      = #$EC#$9D#$BC; // Hangul Syllable Ieung I Rieul
  KoreanYear     = #$EB#$85#$84; // Hangul Syllable Nieun Yeo Nieun
  KoreanEUCMonth = #$EB#$BF#$B9;
  ChineseTotal   = #$E6#$80#$BB#$E6#$95#$B0; // CJK Unified Ideograph Collect/Overall + CJK Unified Ideograph Number/Several/Count
  ChineseMonth   = #$E6#$9C#$88; // CJK Unified Ideograph Month
  ChineseDay     = #$E6#$97#$A5; // CJK Unified Ideograph Day
  ChineseYear    = #$E5#$B9#$B4; // CJK Unified Ideograph Year

  JapaneseTotal  = #$E5#$90#$88#$E8#$A8#$88;
  JapaneseMonth  = #$E8#$B2#$8E; // Japanse Month symbol
  JapaneseDay    = #$E9#$8F#$BA; // Japanese Day Symbol - not valid Unicode
  JapaneseYear   = #$E9#$91#$8E; // Japanese Year symbol = not valid Unicode
{$ELSE}
  KoreanTotal    = #$CD1D;
  KoreanMonth    = #$C6D4;       // Hangul Syllable Ieung Weo Rieul
  KoreanDay      = #$C77C;       // Hangul Syllable Ieung I Rieul
  KoreanEUCMonth = #$BFF9;       // EUC-KR Same as #$C6#$D4
  KoreanYear     = #$B144;       // Hangul Syllable Nieun Yeo Nieun
  ChineseTotal   = #$603B#$6570; // CJK Unified Ideograph Collect/Overall + CJK Unified Ideograph Number/Several/Count
  ChineseMonth   = #$6708;       // CJK Unified Ideograph Month
  ChineseDay     = #$65E5;       // CJK Unified Ideograph Day
  ChineseYear    = #$5E74;       // CJK Unified Ideograph Year

  JapaneseTotal  = #$5408#$8A08;
  JapaneseMonth  = #$8C8E;       // Japanse Day symbol
  JapaneseDay    = #$93FA;       // Japanese Day Symbol - not valid Unicode
  JapaneseYear   = #$944E;       // Japanese Year symbol = not valid Unicode
{$ENDIF}

const
  YYMMDD = 'YYMMDD';
  MMDDYY = 'MMDDYY';
  YYMonthDD = 'YYMonthDD';
  DDMonthYY = 'DDMonthYY';
  Month_DD_YY = 'Month-DD-YY';
  MonthDDYY = 'Month DD YY';
  HHMMSS = 'HHMMSS';

implementation

const
  _ISUID = $800;
  _ISGID = $400;
  _ISVTX = $200;

  _IRUSR = $100;
  _IWUSR = $80;
  _IXUSR = $40;
  _IRWXU = $100 or $80 or $40;

  _IRGRP = _IRUSR shr 3;
  _IWGRP = _IWUSR shr 3;
  _IXGRP = _IXUSR shr 3;
  _IRWXG = _IRWXU shr 3;

  _IROTH = _IRGRP shr 3;
  _IWOTH = _IWGRP shr 3;
  _IXOTH = _IXGRP shr 3;
  _IRWXO = _IRWXG shr 3;

const
  FILE_ATTRIBUTE_READONLY             = $00000001;
  FILE_ATTRIBUTE_HIDDEN               = $00000002;
  FILE_ATTRIBUTE_SYSTEM               = $00000004;
  FILE_ATTRIBUTE_DIRECTORY            = $00000010;
  FILE_ATTRIBUTE_ARCHIVE              = $00000020;
  FILE_ATTRIBUTE_DEVICE               = $00000040;
  FILE_ATTRIBUTE_NORMAL               = $00000080;
  FILE_ATTRIBUTE_TEMPORARY            = $00000100;
  FILE_ATTRIBUTE_SPARSE_FILE          = $00000200;
  FILE_ATTRIBUTE_REPARSE_POINT        = $00000400;
  FILE_ATTRIBUTE_COMPRESSED           = $00000800;
  FILE_ATTRIBUTE_OFFLINE              = $00001000;
  FILE_ATTRIBUTE_NOT_CONTENT_INDEXED  = $00002000;
  FILE_ATTRIBUTE_ENCRYPTED            = $00004000;

function PatternsCount(const SearchPattern, Str: string): integer;
var
  Tmp: string;
begin
  Result := 0;
  Tmp := Str;

  while True do begin
    ExtractFirstWord(Tmp, SearchPattern);
    if Tmp = '' then
      Break
    else
      Inc(Result);
  end;
end;

function IsSpaceLine(const Str: string): boolean;
var
  i: integer;
Begin
  Result := True;

  for i := 1 to Length(Str) do begin
    if not CharInSet(Str[i], [' ', '-', '+', Char(#$96)]) then begin
      Result := False;
      Exit;
    end;
  end;
end;

function DetectFirstDelim(const Value: string): string;
var
  i: integer;
begin
  Result := '';
  for i := 1 to Length(Value) do begin
    if not CharInSet(Value[i], ['0'..'9']) then begin
      Result := Value[i];
      Exit;
    end;
  end;
end;

function DetectFirstNumber(const Value: string; Default: integer): Int64;
var
  Res: string;
  i: integer;
begin
  Res := '';
  for i := 1 to Length(Value) do begin
    if CharInSet(Value[i], ['0'..'9']) then
      Res := Res + Value[i]
    else
    if Value[i] <> ',' then
      Break;
  end;

  Result := StrToInt64Def(Res, Default);
end;

function UnfoldLines(Listing: TStrings; Index: integer): string;
var
  Line: string;
begin
  Result := Listing[Index];
  Inc(Index);

  while Index < Listing.Count do begin
    Line := Listing[Index];
    if (Line = '') or not CharInSet(Line[1], [#9, ' ']) then
      Exit;

    Result := Trim(Result) + ' ' + Trim(Line);
    Inc(Index);
  end;
end;

function IsDate(const Value: string; const Format: string): boolean;
var
  TmpValue: string;
  Delim: string;
  Day, Month: integer;
begin
  Result := False;

  if Format = MMDDYY then begin
    Delim := DetectFirstDelim(Value);
    if ((Delim = '/') or (Delim = '-')) and (PatternsCount(Delim, Value) = 2) then begin
      TmpValue := Value;
      Month := StrToIntDef(ExtractFirstWord(TmpValue, Delim), 0);
      if (Month > 0) and (Month <= 12) then begin
        Day := StrToIntDef(ExtractFirstWord(TmpValue, Delim), 0);
        if (Day > 0) and (Day <= 31) then
          Result := IsNumeric(TmpValue);
      end;
    end;
  end
  else
  if Format = YYMMDD then begin
    Result := (Length(Value) >= 8) and CharInSet(Value[5], ['/', '-']) and CharInSet(Value[8], ['/', '-']);
    if Result then
      Result := IsNumeric(Copy(Value, 1, 4)) and IsNumeric(Copy(Value, 6, 2)) and IsNumeric(Copy(Value, 9, 2))
    else begin
      Result := (Length(Value) >= 6) and CharInSet(Value[3], ['/', '-']) and CharInSet(Value[6], ['/', '-']);
      if Result then
        Result := IsNumeric(Copy(Value, 1, 2)) and IsNumeric(Copy(Value, 4, 2)) and IsNumeric(Copy(Value, 7, 2));
    end;
  end
  else
  if Format = DDMonthYY then begin
    if PatternsCount('-', Value) = 2 then begin
      TmpValue := Value;
      Day := StrToIntDef(ExtractFirstWord(TmpValue, '-'), 0);
      if (Day > 0) and (Day <= 31) then begin
        Month := StrToMonth(ExtractFirstWord(TmpValue, '-'));
        if Month > 0 then
          Result := IsNumeric(ExtractFirstWord(TmpValue, '-'));
      end;
    end;
  end
  else
    raise EScError.CreateFmt(SInvalidInputArg, ['Format'], seInvalidInputArg);
end;

function IsTime(const Value: string; const Format: string): boolean;
var
  TmpValue: string;
  Hour, Min: integer;
  Sec: string;
begin
  if Format = HHMMSS then begin
    Result := False;
    TmpValue := Value;

    if PatternsCount(':', TmpValue) > 0 then begin
      Hour := StrToIntDef(ExtractFirstWord(TmpValue, ':'), -1);
      if (Hour > -1) and (Hour < 24) then begin
        Min := StrToIntDef(ExtractFirstWord(TmpValue, ':'), -1);
        if (Min > -1) and (Min < 60) then begin
          Sec := ExtractFirstWord(TmpValue, ':');
          if Sec = '' then
            Result := True
          else
            Result := (StrToIntDef(Sec, -1) > -1) and (StrToIntDef(Sec, -1) < 60);
        end;
      end;
    end;
  end
  else
    raise EScError.CreateFmt(SInvalidInputArg, ['Format'], seInvalidInputArg);
end;

function CheckYear4(const Year: integer): integer;
var
  CurYear, CurMonth, CurDay: word;
begin
  Result := Year;
  //Y2K Complience for current code
  //Note that some OS/2 servers return years greater than 100 for
  //years such as 2000 and 2003
  if Result < 1000 then begin
    DecodeDate(Now, CurYear, CurMonth, CurDay);

  {$IFDEF USE_FORMATSETTINGS}
    if (FormatSettings.TwoDigitYearCenturyWindow > 0) and (Result > FormatSettings.TwoDigitYearCenturyWindow) then
  {$ELSE}
    if (TwoDigitYearCenturyWindow > 0) and (Result > TwoDigitYearCenturyWindow) then
  {$ENDIF}
      Inc(Result, ((CurYear div 100) - 1) * 100)
    else
      Inc(Result, (CurYear div 100) * 100);
  end;
end;

function StrToDate(const Value: string; const Format: string): TDateTime;
var
  TmpValue: string;
  Delim, StrMonth: string;
  Day, Month, Year: integer;
begin
  TmpValue := Value;

  if Format = YYMMDD then begin
    Delim := DetectFirstDelim(Value);
    Year := StrToIntDef(ExtractFirstWord(TmpValue, Delim), 0);
    Month := StrToIntDef(ExtractFirstWord(TmpValue, Delim), 0);
    Day := StrToIntDef(ExtractFirstWord(TmpValue, Delim), 0);
  end
  else
  if Format = YYMonthDD then begin
    Year := StrToIntDef(ExtractFirstWord(TmpValue, ' '), 0);
    Month := StrToMonth(Trim(ExtractFirstWord(TmpValue, ' ')));
    Day := StrToIntDef(ExtractFirstWord(TmpValue, ' '), 0);
  end
  else
  if Format = DDMonthYY then begin
    Day := StrToIntDef(ExtractFirstWord(TmpValue, '-'), 0);
    Month := StrToMonth(Trim(ExtractFirstWord(TmpValue, '-')));
    Year := StrToIntDef(ExtractFirstWord(TmpValue, '-'), 0);
  end
  else
  if Format = MMDDYY then begin
    Delim := DetectFirstDelim(Value);
    Month := StrToIntDef(ExtractFirstWord(TmpValue, Delim), 0);
    Day := StrToIntDef(ExtractFirstWord(TmpValue, Delim), 0);
    Year := StrToIntDef(ExtractFirstWord(TmpValue, Delim), 0);
  end
  else
  if Format = Month_DD_YY then begin
    StrMonth := Trim(ExtractFirstWord(TmpValue, '-'));
    Month := StrToIntDef(StrMonth, 0);
    if Month = 0 then
      Month := StrToMonth(StrMonth);
    Day := StrToIntDef(ExtractFirstWord(TmpValue, '-'), 0);
    Year := StrToIntDef(ExtractFirstWord(TmpValue, '-'), 0);
    if Year = 0 then
      Year := DetectYear(Day, Month);
  end
  else
  if Format = MonthDDYY then begin
    StrMonth := Trim(ExtractFirstWord(TmpValue, ' '));
    Month := StrToIntDef(StrMonth, 0);
    if Month = 0 then
      Month := StrToMonth(StrMonth);
    Day := StrToIntDef(ExtractFirstWord(TmpValue, ' '), 0);
    Year := StrToIntDef(ExtractFirstWord(TmpValue, ' '), 0);
    if Year = 0 then
      Year := DetectYear(Day, Month);
  end
  else
    raise EScError.CreateFmt(SInvalidInputArg, ['Format'], seInvalidInputArg);

  Year := CheckYear4(Year);
  Result := EncodeDate(Year, Month, Day);
end;

function StrToTime(const Value: string; const Format: string): TDateTime;
var
  UValue: string;
  Delim: string;
  IsPM, IsAM: boolean;
  Hour, Min, Sec, MSec: word;
begin
  if Format = HHMMSS then begin
    UValue := UpperCase(Value);

    IsPM := AnsiPos('PM', UValue) > 0;
    IsAM := AnsiPos('AM', UValue) > 0;
    if IsPM then
      UValue := ExtractFirstWord(UValue, 'PM')
    else
    if IsAM then
      UValue := ExtractFirstWord(UValue, 'AM')
    else
    if AnsiPos('P', UValue) > 0 then begin
      IsPM := True;
      UValue := ExtractFirstWord(UValue, 'P');
    end
    else
    if AnsiPos('A', UValue) > 0 then begin
      IsAM := True;
      UValue := ExtractFirstWord(UValue, 'A');
    end;

    UValue := Trim(UValue);
    Delim := DetectFirstDelim(Value);
    Hour := StrToIntDef(ExtractFirstWord(UValue, Delim), 0);
    Min := StrToIntDef(ExtractFirstWord(UValue, Delim), 0);
    Sec := StrToIntDef(ExtractFirstWord(UValue, Delim), 0);
    MSec := StrToIntDef(ExtractFirstWord(UValue, Delim), 0);

    if IsPM and (Hour < 12) then
      Inc(Hour, 12)
    else
    if IsAM and (Hour = 12) then
      Hour := 0;

    Result := EncodeTime(Hour, Min, Sec, MSec);
  end
  else
    raise EScError.CreateFmt(SInvalidInputArg, ['Format'], seInvalidInputArg);
end;

function DetectYear(const Day, Month: cardinal): cardinal;
var
  CurDay, CurMonth, CurYear: word;
  dt: TDateTime;
begin
  DecodeDate(Now, CurYear, CurMonth, CurDay);
  Result := CurYear;

  try
    dt := EncodeDate(CurYear, Month, Day);
    if dt > Trunc(Now + 1) then
      Result := Result - 1;
  except
  end;
end;

function IsValidEmulUnixPermissions(const Value: string): boolean;
var
  UPerms: string;
begin
  UPerms := UpperCase(Value);
  Result := (Length(UPerms) >= 10) and
    CharInSet(UPerms[1], ['L', 'D', '-', 'B', 'C', 'P', 'S', '+', 'I', 'M', 'W']) and
    CharInSet(UPerms[2], ['T', 'S', 'R', 'W', 'X', '-']) and
    CharInSet(UPerms[3], ['T', 'S', 'R', 'W', 'X', '-', 'A']) and
    CharInSet(UPerms[4], ['T', 'S', 'R', 'W', 'X', '-', 'L']) and
    CharInSet(UPerms[5], ['T', 'S', 'R', 'W', 'X', '-', 'H']) and
    CharInSet(UPerms[6], ['T', 'S', 'R', 'W', 'X', '-']) and
    CharInSet(UPerms[7], ['T', 'S', 'R', 'W', 'X', '-', 'Y', 'L']) and
    CharInSet(UPerms[8], ['T', 'S', 'R', 'W', 'X', '-', 'A']) and
    CharInSet(UPerms[9], ['T', 'S', 'R', 'W', 'X', '-']) and
    CharInSet(UPerms[10], ['T', 'S', 'R', 'W', 'X', '-']);
end;

function IsValidUnixPermissions(const Value: string): boolean;
begin
  Result := (Length(Value) >= 10) and
    CharInSet(Value[1], ['d', '-']) and
    CharInSet(Value[2], ['t', 's', 'r', 'w', 'x', '-']) and
    CharInSet(Value[3], ['t', 's', 'r', 'w', 'x', '-']) and
    CharInSet(Value[4], ['t', 's', 'r', 'w', 'x', '-']) and
    CharInSet(Value[5], ['t', 's', 'r', 'w', 'x', '-']) and
    CharInSet(Value[6], ['t', 's', 'r', 'w', 'x', '-']) and
    CharInSet(Value[7], ['t', 's', 'r', 'w', 'x', '-']) and
    CharInSet(Value[8], ['t', 's', 'r', 'w', 'x', '-']) and
    CharInSet(Value[9], ['t', 's', 'r', 'w', 'x', '-']) and
    CharInSet(Value[10], ['t', 's', 'r', 'w', 'x', '-']);
end;

function PermsBitsToStr(Perms: cardinal): string;
begin
  Result := '---------';

  if (Perms and _IRUSR) <> 0 then
    Result[1] := 'r';

  if (Perms and _IWUSR) <> 0 then
    Result[2] := 'w';

  if (Perms and _ISUID) <> 0 then
    Result[3] := 's'
  else
  if (Perms and _IXUSR) <> 0 then
    Result[3] := 'x';

  if (Perms and _IRGRP) <> 0 then
    Result[4] := 'r';

  if (Perms and _IWGRP) <> 0 then
    Result[5] := 'w';

  if (Perms and _ISGID) <> 0 then
    Result[6] := 's'
  else
  if (Perms and _IXGRP) <> 0 then
    Result[6] := 'x';

  if (Perms and _IROTH) <> 0 then
    Result[7] := 'r';

  if (Perms and _IWOTH) <> 0 then
    Result[8] := 'w';

  if (Perms and _ISVTX) <> 0 then
    Result[9] := 't'
  else
  if (Perms and _IXOTH) <> 0 then
    Result[9] := 'x';
end;

function ChmodToBits(Chmod: cardinal): cardinal;
var
  ProtectionBits, UserBits, GroupBits, OtherBits: cardinal;
begin
  Result := 0;
  ProtectionBits := (Chmod div 1000) and 7;
  if (ProtectionBits and 4) <> 0 then
    Result := Result + _ISUID;
  if (ProtectionBits and 2) <> 0 then
    Result := Result + _ISGID;
  if (ProtectionBits and 1) <> 0 then
    Result := Result + _ISVTX;

  Chmod := Chmod mod 1000;
  UserBits := (Chmod div 100) and 7;
  if (UserBits and 4) <> 0 then
    Result := Result + _IRUSR;
  if (UserBits and 2) <> 0 then
    Result := Result + _IWUSR;
  if (UserBits and 1) <> 0 then
    Result := Result + _IXUSR;

  Chmod := Chmod mod 100;
  GroupBits := (Chmod div 10) and 7;
  if (GroupBits and 4) <> 0 then
    Result := Result + _IRGRP;
  if (GroupBits and 2) <> 0 then
    Result := Result + _IWGRP;
  if (GroupBits and 1) <> 0 then
    Result := Result + _IXGRP;

  Chmod := Chmod mod 10;
  OtherBits := Chmod and 7;
  if (OtherBits and 4) <> 0 then
    Result := Result + _IROTH;
  if (OtherBits and 2) <> 0 then
    Result := Result + _IWOTH;
  if (OtherBits and 1) <> 0 then
    Result := Result + _IXOTH;
end;

procedure PermissionsToStr(const Chmod: integer; out Value: string);
begin
  Value := PermsBitsToStr(ChmodToBits(Chmod));
end;

procedure PermissionsToStr(const Chmod: integer; out User, Group, Other: string);
var
  Perms: string;
begin
  PermissionsToStr(Chmod, Perms);
  User := Copy(Perms, 1, 3);
  Group := Copy(Perms, 4, 3);
  Other := Copy(Perms, 7, 3);
end;

{ TScDOSAttributes }

function TScDOSAttributes.AddAttribute(const Value: string): boolean;
var
  UValue: string;
  i: integer;
begin
  UValue := UpperCase(Value);

  for i := 1 to Length(UValue) do begin
    case UValue[i] of
      'R':
        ReadOnly := True;
      'A':
        Archive := True;
      'S':
        System := True;
      'H':
        Hidden := True;
      'W':
        ReadOnly := False;
      '-', 'D': ;
    else
      Result := False;
      Exit;
    end;
  end;

  Result := True;
end;

procedure TScDOSAttributes.Assign(Source: TPersistent);
begin
  if Source is TScDOSAttributes then
    FFileAttributes := TScDOSAttributes(Source).FFileAttributes
  else
    inherited Assign(Source);
end;

function TScDOSAttributes.GetAsString: string;
begin
  Result := '      ';
  if Archive then
    Result[1] := 'A';
  if System then
    Result[4] := 'S';
  if Hidden then
    Result[5] := 'H';
  if ReadOnly then
    Result[6] := 'R';
end;

function TScDOSAttributes.GetReadOnly: boolean;
begin
  Result := (FFileAttributes and FILE_ATTRIBUTE_READONLY) > 0;
end;

function TScDOSAttributes.GetHidden: boolean;
begin
  Result := (FFileAttributes and FILE_ATTRIBUTE_HIDDEN) > 0;
end;

function TScDOSAttributes.GetSystem: boolean;
begin
  Result := (FFileAttributes and FILE_ATTRIBUTE_SYSTEM) > 0;
end;

function TScDOSAttributes.GetDirectory: boolean;
begin
  Result := (FFileAttributes and FILE_ATTRIBUTE_DIRECTORY) > 0;
end;

function TScDOSAttributes.GetArchive: boolean;
begin
  Result := (FFileAttributes and FILE_ATTRIBUTE_ARCHIVE) > 0;
end;

function TScDOSAttributes.GetNormal: boolean;
begin
  Result := (FFileAttributes and FILE_ATTRIBUTE_NORMAL) > 0;
end;

procedure TScDOSAttributes.SetReadOnly(Value: boolean);
begin
  if Value then
    FFileAttributes := FFileAttributes or FILE_ATTRIBUTE_READONLY
  else
    FFileAttributes := FFileAttributes and not FILE_ATTRIBUTE_READONLY;
end;

procedure TScDOSAttributes.SetHidden(Value: boolean);
begin
  if Value then
    FFileAttributes := FFileAttributes or FILE_ATTRIBUTE_HIDDEN
  else
    FFileAttributes := FFileAttributes and not FILE_ATTRIBUTE_HIDDEN;
end;

procedure TScDOSAttributes.SetSystem(Value: boolean);
begin
  if Value then
    FFileAttributes := FFileAttributes or FILE_ATTRIBUTE_SYSTEM
  else
    FFileAttributes := FFileAttributes and not FILE_ATTRIBUTE_SYSTEM;
end;

procedure TScDOSAttributes.SetDirectory(Value: boolean);
begin
  if Value then
    FFileAttributes := FFileAttributes or FILE_ATTRIBUTE_DIRECTORY
  else
    FFileAttributes := FFileAttributes and not FILE_ATTRIBUTE_DIRECTORY;
end;

procedure TScDOSAttributes.SetArchive(Value: boolean);
begin
  if Value then
    FFileAttributes := FFileAttributes or FILE_ATTRIBUTE_ARCHIVE
  else
    FFileAttributes := FFileAttributes and not FILE_ATTRIBUTE_ARCHIVE;
end;

procedure TScDOSAttributes.SetNormal(Value: boolean);
begin
  if Value then
    FFileAttributes := FFileAttributes or FILE_ATTRIBUTE_NORMAL
  else
    FFileAttributes := FFileAttributes and not FILE_ATTRIBUTE_NORMAL;
end;

{ TScWin32Attributes }

function TScWin32Attributes.GetDevice: boolean;
begin
  Result := (FFileAttributes and FILE_ATTRIBUTE_DEVICE) > 0;
end;

function TScWin32Attributes.GetTemporary: boolean;
begin
  Result := (FFileAttributes and FILE_ATTRIBUTE_TEMPORARY) > 0;
end;

function TScWin32Attributes.GetSparseFile: boolean;
begin
  Result := (FFileAttributes and FILE_ATTRIBUTE_SPARSE_FILE) > 0;
end;

function TScWin32Attributes.GetReparsePoint: boolean;
begin
  Result := (FFileAttributes and FILE_ATTRIBUTE_REPARSE_POINT) > 0;
end;

function TScWin32Attributes.GetCompressed: boolean;
begin
  Result := (FFileAttributes and FILE_ATTRIBUTE_COMPRESSED) > 0;
end;

function TScWin32Attributes.GetOffline: boolean;
begin
  Result := (FFileAttributes and FILE_ATTRIBUTE_OFFLINE) > 0;
end;

function TScWin32Attributes.GetNotContextIndexed: boolean;
begin
  Result := (FFileAttributes and FILE_ATTRIBUTE_NOT_CONTENT_INDEXED) > 0;
end;

function TScWin32Attributes.GetEncrypted: boolean;
begin
  Result := (FFileAttributes and FILE_ATTRIBUTE_ENCRYPTED) > 0;
end;

procedure TScWin32Attributes.SetDevice(Value: boolean);
begin
  if Value then
    FFileAttributes := FFileAttributes or FILE_ATTRIBUTE_DEVICE
  else
    FFileAttributes := FFileAttributes and not FILE_ATTRIBUTE_DEVICE;
end;

procedure TScWin32Attributes.SetTemporary(Value: boolean);
begin
  if Value then
    FFileAttributes := FFileAttributes or FILE_ATTRIBUTE_TEMPORARY
  else
    FFileAttributes := FFileAttributes and not FILE_ATTRIBUTE_TEMPORARY;
end;

procedure TScWin32Attributes.SetSparseFile(Value: boolean);
begin
  if Value then
    FFileAttributes := FFileAttributes or FILE_ATTRIBUTE_SPARSE_FILE
  else
    FFileAttributes := FFileAttributes and not FILE_ATTRIBUTE_SPARSE_FILE;
end;

procedure TScWin32Attributes.SetReparsePoint(Value: boolean);
begin
  if Value then
    FFileAttributes := FFileAttributes or FILE_ATTRIBUTE_NORMAL
  else
    FFileAttributes := FFileAttributes and not FILE_ATTRIBUTE_NORMAL;
end;

procedure TScWin32Attributes.SetCompressed(Value: boolean);
begin
  if Value then
    FFileAttributes := FFileAttributes or FILE_ATTRIBUTE_NORMAL
  else
    FFileAttributes := FFileAttributes and not FILE_ATTRIBUTE_NORMAL;
end;

procedure TScWin32Attributes.SetOffline(Value: boolean);
begin
  if Value then
    FFileAttributes := FFileAttributes or FILE_ATTRIBUTE_OFFLINE
  else
    FFileAttributes := FFileAttributes and not FILE_ATTRIBUTE_OFFLINE;
end;

procedure TScWin32Attributes.SetNotContextIndexed(Value: boolean);
begin
  if Value then
    FFileAttributes := FFileAttributes or FILE_ATTRIBUTE_NOT_CONTENT_INDEXED
  else
    FFileAttributes := FFileAttributes and not FILE_ATTRIBUTE_NOT_CONTENT_INDEXED;
end;

procedure TScWin32Attributes.SetEncrypted(Value: boolean);
begin
  if Value then
    FFileAttributes := FFileAttributes or FILE_ATTRIBUTE_ENCRYPTED
  else
    FFileAttributes := FFileAttributes and not FILE_ATTRIBUTE_ENCRYPTED;
end;

function TScWin32Attributes.GetAsString: string;
begin
  Result := '             ';
  if ReadOnly then
    Result[1] := 'R';
  if Hidden then
    Result[2] := 'H';
  if System then
    Result[3] := 'S';
  if Archive then
    Result[4] := 'A';
  if Directory then
    Result[5] := 'D';
  if Encrypted then
    Result[6] := 'E';
  if Normal then
    Result[7] := 'N';
  if Temporary then
    Result[8] := 'T';
  if ReparsePoint then
    Result[9] := 'J';
  if SparseFile then
    Result[10] := 'P';
  if Compressed then
    Result[11] := 'C';
  if Offline then
    Result[12] := 'O';
  if NotContextIndexed then
    Result[13] := 'I';
end;

end.

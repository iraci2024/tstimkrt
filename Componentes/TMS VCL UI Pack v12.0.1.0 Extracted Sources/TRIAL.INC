

function Scramble(s:string): string;
var
  r:string;
  i: integer;
  c: char;
  b: byte;
begin
  r := '';
  for i := 1 to length(s) do
  begin
    b := ord(s[i]);
    b := (b and $E0) + ((b and $1F) xor 5);
    c := chr(b);
    r := r + c;
  end;
  Result := r;
end;

function NoIDERunning: boolean;
begin
  Result := (FindWindow(PChar(Scramble('QDuuilfdqljk')), nil) = 0) OR
       (FindWindow(PChar(Scramble('QDuuGplia`w')), nil) = 0);
end;

procedure ShowTrialMessage;
var
 s1,s2: string;
begin
  s1 := Scramble('Duuilfdqljk%pv`v%qwldi%s`wvljk%jc%QHV%vjcqrdw`%fjhujk`kqv+') +#13#10 +
        Scramble('Fjkqdfq%QHV%vjcqrdw`%mqqu?**rrr+qhvvjcqrdw`+fjh%cjw%sdila%ilf`kvlkb+');
  s2 := Scramble('Rdwklkb');
  MessageBox(0,PChar(s1),PChar(s2),MB_OK);
end;


initialization

   if NoIDERunning then
     ShowTrialMessage;
(*
   if  (FindWindow('TApplication', nil) = 0) OR
       (FindWindow('TAppBuilder', nil) = 0) then
   begin
     MessageBox(0,'Application uses trial version of TMS software components.'#13#10'Contact TMS software http://www.tmssoftware.com for valid licensing','Info',MB_OK);
   end
*)







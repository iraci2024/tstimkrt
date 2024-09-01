unit PromptForm;

interface

uses
{$IFDEF CLR}
  System.ComponentModel,
{$ENDIF}
{$IFNDEF VER130}
  Types,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ScUtils, ScTypes;

type
  TfmPrompt = class(TForm)
    MainPanel: TPanel;
    lbName: TLabel;
    lbInstr: TLabel;
    Panel1: TPanel;
    btOK: TButton;
    procedure FormShow(Sender: TObject);
  private
    lbPrompt: array of TLabel;
    edPrompt: array of TEdit;
    procedure Clear;
  public
    procedure InitForm(const Name, Instruction: string; const Prompts: {$IFDEF CLR}array of string{$ELSE}TStringDynArray{$ENDIF});
    procedure GetResponse(var Responses: {$IFDEF CLR}array of string{$ELSE}TStringDynArray{$ENDIF});
  end;

var
  fmPrompt: TfmPrompt;

implementation

{$IFDEF CLR}
{$R *.nfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

procedure TfmPrompt.InitForm(const Name, Instruction: string; const Prompts: {$IFDEF CLR}array of string{$ELSE}TStringDynArray{$ENDIF});
var
  i: Integer;
begin
  Clear;

  lbName.Caption := Name;
  lbInstr.Caption := Instruction;
  lbName.Visible := Name <> '';
  lbInstr.Visible := Instruction <> '';
  if (Name = '') and (Instruction = '') then
    MainPanel.Height := 8
  else
    MainPanel.Height := 56;
  Height := MainPanel.Height + 70;

  for i := 0 to Length(Prompts) - 1 do begin
    SetLength(lbPrompt, i + 1);
    lbPrompt[i] := TLabel.Create(Self);
    lbPrompt[i].Parent := MainPanel;
    lbPrompt[i].Left := 16;
    lbPrompt[i].Top := MainPanel.Height;
    lbPrompt[i].Caption := Prompts[i];

    SetLength(edPrompt, i + 1);
    edPrompt[i] := TEdit.Create(Self);
    edPrompt[i].Parent := MainPanel;
    edPrompt[i].Left := 16;
    edPrompt[i].Top := MainPanel.Height + 18;
    edPrompt[i].Width := 200;
    edPrompt[i].Text := '';

    Height := Height + 48;
    MainPanel.Height := MainPanel.Height + 48;
  end;
end;

procedure TfmPrompt.GetResponse(var Responses: {$IFDEF CLR}array of string{$ELSE}TStringDynArray{$ENDIF});
var
  i: Integer;
  len: Integer;
begin
  if Length(Responses) < Length(edPrompt) then
    len := Length(Responses)
  else
    len := Length(edPrompt);

  for i := 0 to len - 1 do
    Responses[i] := edPrompt[i].Text;

  Clear;
end;

procedure TfmPrompt.FormShow(Sender: TObject);
begin
  if Length(edPrompt) > 0 then
    edPrompt[0].SetFocus;
end;

procedure TfmPrompt.Clear;
var
  i: Integer;
begin
  for i := 0 to Length(lbPrompt) - 1 do begin
    lbPrompt[i].Free;
    edPrompt[i].Free;
  end;
  SetLength(lbPrompt, 0);
  SetLength(edPrompt, 0);
end;

end.

unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, FlexCompress, Buttons;

type
  TfmMain = class(TForm)
    ArcName: TEdit;
    Label1: TLabel;
    bnChooseArchive: TButton;
    OpenDialog: TOpenDialog;
    Label2: TLabel;
    StubName: TEdit;
    bnChooseStub: TButton;
    OpenDialog1: TOpenDialog;
    Archiver: TFlexCompress;
    bnMakeSFX: TBitBtn;
    bnExit: TBitBtn;
    SaveDialog1: TSaveDialog;
    procedure bnCancelClick(Sender: TObject);
    procedure bnMakeSFXClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure bnChooseArchiveClick(Sender: TObject);
    procedure bnChooseStubClick(Sender: TObject);
    procedure bnExitClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmMain: TfmMain;

implementation

{$R *.DFM}

procedure TfmMain.bnCancelClick(Sender: TObject);
begin
 Close;
end;

procedure TfmMain.bnMakeSFXClick(Sender: TObject);
begin
 Archiver.FileName := ArcName.Text;
 Archiver.SFXStub := StubName.Text;
 if (SaveDialog1.Execute) then
  begin
   Archiver.MakeSFX(SaveDialog1.FileName);
   ShowMessage('SFX archive '+ExtractFileName(SaveDialog1.FileName)+' created successfully.');
  end;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
 ArcName.Text := 'test.fxc';
 OpenDialog1.FileName := '..\..\..\SFXStub\SFXStub.exe';
 StubName.Text := OpenDialog1.FileName;
 SaveDialog1.InitialDir := ExtractFilePath(Application.ExeName);
end;

procedure TfmMain.bnChooseArchiveClick(Sender: TObject);
begin
 OpenDialog.InitialDir := ExtractFilePath(ArcName.Text);
 if (OpenDialog.InitialDir = '') then
  OpenDialog.InitialDir := ExtractFilePath(Application.ExeName);
 if (OpenDialog.Execute) then
  ArcName.Text := OpenDialog.FileName;
end;

procedure TfmMain.bnChooseStubClick(Sender: TObject);
begin
 OpenDialog1.InitialDir := ExtractFilePath(StubName.Text);
 if (OpenDialog1.InitialDir = '') then
  OpenDialog1.InitialDir := ExtractFilePath(Application.ExeName);
 if (OpenDialog1.Execute) then
  StubName.Text := OpenDialog1.FileName;
end;

procedure TfmMain.bnExitClick(Sender: TObject);
begin
 Close;
 Application.Terminate;
end;

end.

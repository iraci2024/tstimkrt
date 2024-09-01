unit uMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, FlexCompress, uProgress;

type
  TfmMain = class(TForm)
    Archiver: TFlexCompress;
    bnStart: TBitBtn;
    BitBtn2: TBitBtn;
    Label1: TLabel;
    procedure bnStartClick(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);

    {$IFNDEF VER120}        // Delphi 4
      {$IFNDEF VER125}      // BCB 4
        {$IFNDEF VER130}    // Delphi 5
          {$IFNDEF VER135}  // BCB 5
            {$DEFINE UNICODE}
          {$ENDIF}
        {$ENDIF}
      {$ENDIF}
    {$ENDIF}


    {$IFDEF UNICODE}
    procedure ArchiverFileProgress(Sender: TObject; FileName: WideString;
      Progress: Double; Operation: TFXCProcessOperation;
      ProgressPhase: TFXCProgressPhase; var Cancel: Boolean);
    {$ELSE}
    procedure ArchiverFileProgress(Sender: TObject; FileName: string;
      Progress: Double; Operation: TFXCProcessOperation;
      ProgressPhase: TFXCProgressPhase; var Cancel: Boolean);
    {$ENDIF}

    procedure ArchiverOverallProgress(Sender: TObject; Progress: Double;
      Operation: TFXCProcessOperation; ProgressPhase: TFXCProgressPhase;
      var Cancel: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmMain: TfmMain;

implementation

{$R *.DFM}

procedure TfmMain.bnStartClick(Sender: TObject);
begin
 with Archiver do
    begin
        FileName := 'Archive\test.fxc';

        // Create a new archive file
        OpenArchive(fmCreate);

        // Set path to folder with some text files to BaseDir
        BaseDir := 'Source';

        // Add all files and directories from 'C:\SOURCE_FOLDER' to the archive
        AddFiles('*.*');

        // Set path to destination folder
        BaseDir := 'Dest';

        // Extract all files and directories from the archive to 'C:\DESTINATION_FOLDER'
        ExtractFiles('*.*');

        // Close the archive
        CloseArchive;
    end;
    ShowMessage('All files were added and extracted successfully.');
end;

procedure TfmMain.BitBtn2Click(Sender: TObject);
begin
 Close;
end;

procedure TfmMain.ArchiverFileProgress;
begin
 frmProgress.gFile.Progress := Round(Progress);
 frmProgress.lbFile.Caption := FileName;
 Cancel := frmProgress.bCancel;
 Application.ProcessMessages;
end;

procedure TfmMain.ArchiverOverallProgress(Sender: TObject;
  Progress: Double; Operation: TFXCProcessOperation;
  ProgressPhase: TFXCProgressPhase; var Cancel: Boolean);
begin
 if (ProgressPhase = ppStart) then
  frmProgress.Show
 else
 if (ProgressPhase = ppEnd) then
  frmProgress.Hide;

 frmProgress.gOverall.Progress := Round(Progress);
 Cancel := frmProgress.bCancel;
 Application.ProcessMessages;
end;

end.

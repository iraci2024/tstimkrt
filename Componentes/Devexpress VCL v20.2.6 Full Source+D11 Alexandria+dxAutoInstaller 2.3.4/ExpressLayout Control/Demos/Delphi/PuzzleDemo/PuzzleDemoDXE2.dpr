program PuzzleDemoDXE2;

uses
  Forms,
  Puzzle in 'Puzzle.pas' {frmPuzzle};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'PuzzleDemoDXE2';
  Application.CreateForm(TfrmPuzzle, frmPuzzle);
  Application.Run;
end.

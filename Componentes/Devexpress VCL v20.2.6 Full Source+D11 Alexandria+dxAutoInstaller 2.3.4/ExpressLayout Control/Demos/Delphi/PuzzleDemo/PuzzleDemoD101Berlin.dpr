program PuzzleDemoD101Berlin;

uses
  Forms,
  Puzzle in 'Puzzle.pas' {frmPuzzle};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'PuzzleDemoD101Berlin';
  Application.CreateForm(TfrmPuzzle, frmPuzzle);
  Application.Run;
end.

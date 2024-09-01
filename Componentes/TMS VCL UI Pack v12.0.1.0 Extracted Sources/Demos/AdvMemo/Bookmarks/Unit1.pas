unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, AdvMemo, AdvmPS;

type
  TForm1 = class(TForm)
    AdvMemo1: TAdvMemo;
    AdvPascalMemoStyler1: TAdvPascalMemoStyler;
    ListBox1: TListBox;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  i,b: integer;
begin
  AdvMemo1.Lines.LoadFromFile('.\advmps.pas');

  for i := 0 to 9 do
  begin
    b := Random(AdvMemo1.Lines.Count - 1);
    AdvMemo1.Bookmarks[i] := b;
    AdvMemo1.BookmarkIndex[b] := i;
    Listbox1.Items.Add(i.ToString+' ('+(b + 1).ToString+')');
  end;
end;

procedure TForm1.ListBox1Click(Sender: TObject);
var
  l: integer;
begin
  l := AdvMemo1.Bookmarks[ListBox1.ItemIndex];
  AdvMemo1.GotoBookmark(listbox1.ItemIndex);
end;

end.

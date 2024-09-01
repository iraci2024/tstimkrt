unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DB, Data.Win.ADODB,
  AdvScrollControl, AdvSearchList, DBAdvSearchList, AdvUtil, Vcl.Grids, AdvObj,
  BaseGrid, AdvGrid, Datasnap.DBClient, Vcl.StdCtrls, AdvSearchEdit,
  DBAdvSearchEdit;

type
  TForm1 = class(TForm)
    DBAdvSearchList1: TDBAdvSearchList;
    DataSource1: TDataSource;
    ClientDataSet1: TClientDataSet;
    Edit1: TEdit;
    DBAdvSearchEdit1: TDBAdvSearchEdit;
    Label1: TLabel;
    procedure Edit1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Edit1Change(Sender: TObject);
begin
  DBAdvSearchList1.FilterCondition.Text := Edit1.Text;
  DBAdvSearchList1.UpdateFilter;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  DBAdvSearchList1.Columns[0].WordWrap := false;
  DBAdvSearchList1.Columns[0].Trimming := true;
end;

end.

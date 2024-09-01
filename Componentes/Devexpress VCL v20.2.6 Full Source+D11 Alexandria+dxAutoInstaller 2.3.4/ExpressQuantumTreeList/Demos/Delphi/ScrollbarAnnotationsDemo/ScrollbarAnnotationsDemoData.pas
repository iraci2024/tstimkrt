unit ScrollbarAnnotationsDemoData;

interface

uses
  SysUtils, Classes, DB, dxmdaset;

type
  TScrollbarAnnotationsDemoDataDM = class(TDataModule)
    mdDepartments: TdxMemData;
    mdDepartmentsID: TAutoIncField;
    mdDepartmentsPARENTID: TIntegerField;
    mdDepartmentsMANAGERID: TIntegerField;
    mdDepartmentsNAME: TStringField;
    mdDepartmentsBUDGET: TFloatField;
    mdDepartmentsLOCATION: TStringField;
    mdDepartmentsPHONE: TStringField;
    mdDepartmentsFAX: TStringField;
    mdDepartmentsEMAIL: TStringField;
    mdDepartmentsVACANCY: TBooleanField;
    dsDepartments: TDataSource;
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ScrollbarAnnotationsDemoDataDM: TScrollbarAnnotationsDemoDataDM;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

procedure TScrollbarAnnotationsDemoDataDM.DataModuleCreate(Sender: TObject);
const
  DataPath = '..\..\Data\';
begin
  mdDepartments.LoadFromBinaryFile(DataPath + 'Departments.dat');
  mdDepartments.Open;
end;

end.

unit GridViewDemoData;

interface

{$I cxVer.inc}

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  SysUtils, Classes, Controls, Dialogs, DB, ADODB,
  dxEMF.Metadata, dxEMF.Core, dxEMF.DataDefinitions,
  dxEMF.DataProvider.ADO, dxEMF.DB.MSSQL,
  cxEMFData, cxGridEMFTableView,
  GridViewDemoConnection,
  EMFGridDemo.Entities, EMFGridDemo.Linq;

type
  TGridViewDemoDataDM = class(TDataModule)
    ADOConnection: TADOConnection;
    ADOQuery: TADOQuery;
    EMFDataSource: TdxEMFDataSource;
    EMFSession: TdxEMFSession;
    EMFADODataProvider: TdxEMFADODataProvider;
    EMFDataContext: TdxEMFDataContext;
  private
    procedure AddInsertValues(ASQL: TStringBuilder);
    procedure AddInsertInto(ASQL: TStringBuilder);
    procedure AddInsertSQL(ASQL: TStringBuilder; ARecordCount: Integer);
  public
    procedure AddRecords(ACount: Integer; AProgress: TdxProgressEvent);
    procedure Connect(const AHostName, ADatabaseName, AUserName, APassword: string;
      OSAuthentication: Boolean);
    procedure CreateDatabase;
    procedure ExecSQL(const ASQL: string);
    function GetRecordsCount: Integer;

    class function GetCaption: string; static;
    class function GetDatabaseName: string; static;
    class function GetDescription: string; static;
    class function GetTableName: string; static;
    class function GetEntityClass: TClass; static;
  end;

var
  GridViewDemoDataDM: TGridViewDemoDataDM;

implementation

uses
  DateUtils;

{$R *.dfm}

{ TGridViewDemoDataDM }

procedure TGridViewDemoDataDM.AddRecords(ACount: Integer; AProgress: TdxProgressEvent);
const
  SubCount = 100;
var
  I: Integer;
  ASQL: TStringBuilder;
begin
  ASQL := TStringBuilder.Create(16384);
  try
    ADOConnection.BeginTrans;
    try
      for I := 0 to (ACount div SubCount) - 1 do
      begin
        if I mod 10 = 0 then
          AProgress(Self, 100 / (ACount div SubCount) * I);
        ASQL.Length := 0;
        AddInsertSQL(ASQL, SubCount);
        ADOConnection.Execute(ASQL.ToString);
      end;
      ADOConnection.CommitTrans;
    except
      ADOConnection.RollbackTrans;
    end;
  finally
    ASQL.Free;
  end;
end;

procedure TGridViewDemoDataDM.Connect(const AHostName, ADatabaseName, AUserName, APassword: string;
  OSAuthentication: Boolean);
var
  AConnectionString: string;
begin
  ADOConnection.Connected := False;
  ADOConnection.Provider := 'SQLOLEDB.1';
  ADOConnection.LoginPrompt := False;
  AConnectionString := Format('Provider=SQLOLEDB.1;Data Source=%s;Initial Catalog=%s', [AHostName, ADatabaseName]);
  if OSAuthentication then
    ADOConnection.ConnectionString := AConnectionString + ';Integrated Security=SSPI;Persist Security Info=False'
  else
    ADOConnection.ConnectionString := AConnectionString +
      Format(';User ID=%s;Password=%s;Persist Security Info=True', [AUserName, APassword]);
end;

procedure TGridViewDemoDataDM.CreateDatabase;
begin
  EMFSession.CreateSchema(TEMFGridTableDemo);
end;

procedure TGridViewDemoDataDM.ExecSQL(const ASQL: string);
begin
  ADOQuery.Active := False;
  ADOQuery.SQL.Text := ASQL;
  ADOQuery.ExecSQL;
end;

class function TGridViewDemoDataDM.GetCaption: string;
begin
  Result := 'ExpressQuantumGrid ADO ExpressEntityMapping Framework Demo';
end;

class function TGridViewDemoDataDM.GetDatabaseName: string;
begin
  Result := 'EMFGridDemo';
end;

class function TGridViewDemoDataDM.GetDescription: string;
begin
  Result := 'This demo shows a grid control''s capabilities when bound to a large ' +
    'amount of data in ExpressEntityMapping Framework via an ADO connection.'
end;

class function TGridViewDemoDataDM.GetEntityClass: TClass;
begin
  Result := TEMFGridTableDemo;
end;

procedure TGridViewDemoDataDM.AddInsertSQL(ASQL: TStringBuilder; ARecordCount: Integer);
var
  J: Integer;
begin
  AddInsertInto(ASQL);
  for J := 1 to ARecordCount do
  begin
    AddInsertValues(ASQL);
    if J <> ARecordCount then
      ASQL.Append(',')
  end;
end;

procedure TGridViewDemoDataDM.AddInsertValues(ASQL: TStringBuilder);

  function GetDateTime: string;
  var
    ADate: TDateTime;
  begin
    ADate := IncSecond(Now, -Random(315360000));
    Result := FormatDateTime('yyyymmdd hh:mm:ss', ADate);
  end;

const
  Users: array[0..16] of string = (
    'Peter Dolan',
    'Ryan Fischer',
    'Richard Fisher',
    'Tom Hamlett',
    'Mark Hamilton',
    'Steve Lee',
    'Jimmy Lewis',
    'Jeffrey W McClain',
    'Andrew Miller',
    'Dave Murrel',
    'Bert Parkins',
    'Mike Roller',
    'Ray Shipman',
    'Paul Bailey',
    'Brad Barnes',
    'Carl Lucas',
    'Jerry Campbell'
  );
  Subjects: array[0..20] of string = (
    'Integrating Developer Express MasterView control into an Accounting System.',
    'Web Edition: Data Entry Page. There is an issue with date validation.',
    'Payables Due Calculator is ready for testing.',
    'Web Edition: Search Page is ready for testing.',
    'Main Menu: Duplicate Items. Somebody has to review all menu items in the system.',
    'Receivables Calculator. Where can I find the complete specs?',
    'Ledger: Inconsistency. Please fix it.',
    'Receivables Printing module is ready for testing.',
    'Screen Redraw. Somebody has to look at it.',
    'Email System. What library are we going to use?',
    'Cannot add new vendor. This module doesn''''t work!',
    'History. Will we track sales history in our system?',
    'Main Menu: Add a File menu. File menu item is missing.',
    'Currency Mask. The current currency mask in completely unusable.',
    'Drag & Drop operations are not available in the scheduler module.',
    'Data Import. What database types will we support?',
    'Reports. The list of incomplete reports.',
    'Data Archiving. We still don''''t have this features in our application.',
    'Email Attachments. Is it possible to add multiple attachments? I haven''''t found a way to do this.',
    'Check Register. We are using different paths for different modules.',
    'Data Export. Our customers asked us for export to Microsoft Excel');

begin
  ASQL.Append('(''')
    .Append(Subjects[Random(21)]).Append(''',''')
    .Append(Users[Random(17)]).Append(''',''')
    .Append(GetDateTime).Append(''',')
    .Append(Random(100000)).Append(',')
    .Append(Random(2)).Append(',')
    .Append(Random(3)).Append(')');
end;

procedure TGridViewDemoDataDM.AddInsertInto(ASQL: TStringBuilder);
begin
  ASQL.Append('INSERT INTO "')
    .Append(GetTableName)
    .Append('" ("Subject", "From", "Sent", "Size", "HasAttachment", "Priority")')
    .Append('VALUES ');
end;

function TGridViewDemoDataDM.GetRecordsCount: Integer;
var
  ASQL: string;
begin
  ASQL := Format('IF DB_ID(N''%s'') IS NOT NULL AND OBJECT_ID(N''%0:s.dbo.%1:s'') IS NOT NULL' + sLineBreak +
    '  SELECT COUNT(*) FROM %0:s.dbo.%1:s;' + sLineBreak +
    'ELSE' + sLineBreak +
    '  SELECT 0;', [GetDatabaseName, GetTableName]);
  ADOQuery.Active := False;
  ADOQuery.SQL.Text := ASQL;
  ADOQuery.Open;
  if not ADOQuery.Eof then
    Result := ADOQuery.Fields[0].AsInteger
  else
    Result := 0;
end;

class function TGridViewDemoDataDM.GetTableName: string;
begin
  Result := EntityManager.GetEntityInfo(GetEntityClass).DBTable.Name;
end;

end.

unit DevAVData;

{$I cxVer.inc}

interface

uses
  SysUtils, Classes, DB, dxmdaset, cxClasses, cxEdit, cxDBEditRepository,
  cxEditRepositoryItems, cxStyles, cxExtEditRepositoryItems;

type
  { TdmDevAV }

  TdmDevAV = class(TDataModule)
    mdPrefix: TdxMemData;
    mdPrefixPrefix_ID: TIntegerField;
    mdPrefixPrefix_Name: TStringField;
    dsPrefix: TDataSource;
    dsTasks: TDataSource;
    dsEmployees: TDataSource;
    mdTasks: TdxMemData;
    mdTasksId: TIntegerField;
    mdTasksSubject: TWideStringField;
    mdTasksDescription: TWideStringField;
    mdTasksRtfTextDescription: TWideStringField;
    mdTasksStartDate: TDateTimeField;
    mdTasksDueDate: TDateTimeField;
    mdTasksStatus: TIntegerField;
    mdTasksPriority: TIntegerField;
    mdTasksCompletion: TIntegerField;
    mdTasksReminder: TBooleanField;
    mdTasksReminderDateTime: TDateTimeField;
    mdTasksAssignedEmployeeId: TIntegerField;
    mdTasksOwnerId: TIntegerField;
    mdTasksCustomerEmployeeId: TIntegerField;
    mdTasksFollowUp: TIntegerField;
    mdTasksPrivate: TBooleanField;
    mdTasksCategory: TWideStringField;
    mdTasksAttachedCollectionsChanged: TBooleanField;
    mdEmployees: TdxMemData;
    mdEmployeesId: TIntegerField;
    mdEmployeesDepartment: TIntegerField;
    mdEmployeesTitle: TWideStringField;
    mdEmployeesStatus: TIntegerField;
    mdEmployeesHireDate: TDateTimeField;
    mdEmployeesPersonalProfile: TWideStringField;
    mdEmployeesFirstName: TWideStringField;
    mdEmployeesLastName: TWideStringField;
    mdEmployeesFullName: TWideStringField;
    mdEmployeesPrefix: TIntegerField;
    mdEmployeesHomePhone: TWideStringField;
    mdEmployeesMobilePhone: TWideStringField;
    mdEmployeesEmail: TWideStringField;
    mdEmployeesSkype: TWideStringField;
    mdEmployeesBirthDate: TDateTimeField;
    mdEmployeesPictureId: TIntegerField;
    mdEmployeesAddress_Line: TWideStringField;
    mdEmployeesAddress_City: TWideStringField;
    mdEmployeesAddress_State: TIntegerField;
    mdEmployeesAddress_ZipCode: TWideStringField;
    mdEmployeesAddress_Latitude: TFloatField;
    mdEmployeesAddress_Longitude: TFloatField;
    mdEmployeesProbationReason_Id: TIntegerField;
    mdEmployeesPicture: TBlobField;
    mdEmployeesPrefixByID: TStringField;
    mdEmployeesLevel: TIntegerField;
    mdDepartment: TdxMemData;
    mdDepartmentDepartment_ID: TAutoIncField;
    mdDepartmentDepartment_Name: TWideStringField;
    mdStates: TdxMemData;
    mdStatesID: TIntegerField;
    mdStatesShortName: TStringField;
    mdStatesLongName: TWideStringField;
    mdStatesFlag48px: TBlobField;
    mdStatesFlag24px: TBlobField;
    dsDepartment: TDataSource;
    dsStates: TDataSource;
    mdStatus: TdxMemData;
    mdStatusStatus_ID: TIntegerField;
    mdStatusStatus_Name: TStringField;
    dsStatus: TDataSource;
    erRepository: TcxEditRepository;
    erpState: TcxEditRepositoryLookupComboBoxItem;
    erpDepartment: TcxEditRepositoryLookupComboBoxItem;
    erpStatus: TcxEditRepositoryLookupComboBoxItem;
    erpPhoto: TcxEditRepositoryImageItem;
    erpHyperLink: TcxEditRepositoryHyperLinkItem;
    erpMemo: TcxEditRepositoryMemoItem;
    erpFullName: TcxEditRepositoryLookupComboBoxItem;
    erpProgress: TcxEditRepositoryProgressBar;
    srRepository: TcxStyleRepository;
    stPerson: TcxStyle;
    stWorkInfo: TcxStyle;
    procedure DataModuleCreate(Sender: TObject);
  protected
    procedure LoadEmployees(const APath: string);
  end;

var
  dmDevAV: TdmDevAV;

implementation

uses
  Forms;

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

procedure TdmDevAV.LoadEmployees(const APath: string);
begin
  mdEmployees.DisableControls;
  try
    mdEmployees.LoadFromBinaryFile(APath + 'Employees.dat');
    mdEmployees.First;
    while not mdEmployees.Eof do
    begin
      mdEmployees.Edit;
      mdEmployeesLevel.Value := Random(3) + 3;
      mdEmployees.Post;
      mdEmployees.Next;
    end;
    mdEmployees.First;
  finally
    mdEmployees.EnableControls;
  end;
end;

procedure TdmDevAV.DataModuleCreate(Sender: TObject);
var
  APath: string;
begin
  APath := ExtractFilePath(Application.ExeName) + '..\..\Data\';
  LoadEmployees(APath);
  mdStates.LoadFromBinaryFile(APath + 'States.dat');
  mdDepartment.LoadFromBinaryFile(APath + 'Department.dat');
  mdStatus.LoadFromBinaryFile(APath + 'Status.dat');
  mdPrefix.LoadFromBinaryFile(APath + 'Prefix.dat');
  mdTasks.LoadFromBinaryFile(APath + 'Tasks.dat');
end;

end.

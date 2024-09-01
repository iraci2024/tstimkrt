{ ******************************************************************** }
{ }
{ Developer Express Visual Component Library }
{ ExpressRichEditControl }
{ }
{ Copyright (c) 2000-2021 Developer Express Inc. }
{ ALL RIGHTS RESERVED }
{ }
{ The entire contents of this file is protected by U.S. and }
{ International Copyright Laws. Unauthorized reproduction, }
{ reverse-engineering, and distribution of all or any portion of }
{ the code contained in this file is strictly prohibited and may }
{ result in severe civil and criminal penalties and will be }
{ prosecuted to the maximum extent possible under the law. }
{ }
{ RESTRICTIONS }
{ }
{ THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES }
{ (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE }
{ SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS }
{ LICENSED TO DISTRIBUTE THE EXPRESSRICHEDITCONTROL AND ALL }
{ ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM }
{ ONLY. }
{ }
{ THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED }
{ FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE }
{ COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE }
{ AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT }
{ AND PERMISSION FROM DEVELOPER EXPRESS INC. }
{ }
{ CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON }
{ ADDITIONAL RESTRICTIONS. }
{ }
{ ******************************************************************** }

unit dxRichEdit.DocumentServer;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Classes, Controls,
  dxDocumentLayoutUnitConverter,
  dxRichEdit.InternalRichEditDocumentServer,
  dxRichEdit.InnerControl,
  dxRichEdit.Options,
  dxRichEdit.NativeApi,
  dxRichEdit.ServiceManager,
  dxRichEdit.Utils.BatchUpdateHelper,
  dxRichEdit.Types,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.RichEditDocumentServer,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.MailMerge,
  dxRichEdit.View.Core;

type

  { TdxRichEditDocumentServerOptions }

  TdxRichEditDocumentServerOptions = class(TdxRichEditControlOptionsBase)
  published
    property Authentication;
    property AutoCorrect;
    property DocumentCapabilities;
    property DocumentSaveOptions;
    property Export;
    property Fields;
    property Import;
    property MailMerge;
    property RangePermissions;
    property Search;
  end;

  TdxGetEncryptionPasswordEvent = procedure(Sender: TObject;
    var APassword: string; var AHandled: Boolean) of object;

  { TdxRichEditCustomDocumentServer }

  TdxRichEditCustomDocumentServer = class(TComponent, IdxRichEditDocumentServer,
    IdxBatchUpdatable, IdxServiceContainer, IdxServiceProvider,
    IdxInternalRichEditDocumentServerOwner)
  strict private
    FInnerServer: IdxRichEditDocumentServer;
    function GetInnerServer: IdxRichEditDocumentServer;
    function GetIsUpdateLocked: Boolean;
    function GetBatchUpdateHelper: TdxBatchUpdateHelper;
  private
    FOnMailMergeRecordStarted: TdxMailMergeRecordStartedEvent;
    FOnMailMergeFinished: TdxMailMergeFinishedEvent;
    FOnCalculateDocumentVariable: TdxCalculateDocumentVariableEvent;
    FOnDocumentLoaded: TNotifyEvent;
    FOnAfterExport: TNotifyEvent;
    FOnMailMergeStarted: TdxMailMergeStartedEvent;
    FOnCustomizeMergeFields: TdxCustomizeMergeFieldsEvent;
    FOnMailMergeGetTargetDocument: TdxMailMergeGetTargetDocumentEvent;
    FOnMailMergeRecordFinished: TdxMailMergeRecordFinishedEvent;
    FOnGetEncryptionPassword: TdxGetEncryptionPasswordEvent;
    FOnContentChanged: TNotifyEvent;
    FOnDocumentClosing: TdxCloseQueryEvent;
    FOnEmptyDocumentCreated: TNotifyEvent;
    function CreateDocumentServer: IdxRichEditDocumentServer; overload;
    function GetDocumentModel: TdxDocumentModel;
    function GetDocument: IdxRichEditDocument;
    function GetInternalServer: TdxInternalRichEditDocumentServer;
    function GetServiceContainer: IdxServiceContainer;
    procedure SubscribeInnerControlEvents;
    procedure UnsubscribeInnerServerEvents;
    function GetOptions: TdxRichEditDocumentServerOptions;
    procedure SetOptions(const Value: TdxRichEditDocumentServerOptions);
    function GetDocBytes: TArray<Byte>;
    function GetHtmlText: string;
    function GetOpenXmlBytes: TArray<Byte>;
    function GetRtfText: string;
    function GetText: string;
    procedure SetDocBytes(const AValue: TArray<Byte>);
    procedure SetHtmlText(const AValue: string);
    procedure SetOpenXmlBytes(const AValue: TArray<Byte>);
    procedure SetRtfText(const AValue: string);
    procedure SetText(const AValue: string);
  protected
    function DoGetEncryptionPassword(var APassword: string): Boolean;

    procedure DoAfterExport(Sender: TObject);
    procedure DoContentChanged(Sender: TObject);
    procedure DoDocumentClosing(Sender: TObject; var CanClose: Boolean);
    procedure DoDocumentLoaded(Sender: TObject);
    procedure DoEmptyDocumentCreated(Sender: TObject);

    procedure DoCalculateDocumentVariable(Sender: TObject;
      Args: TdxCalculateDocumentVariableEventArgs);
    procedure DoCustomizeMergeFields(Sender: TObject;
      const Args: TdxCustomizeMergeFieldsEventArgs);
    procedure DoMailMergeFinished(Sender: TObject;
      const Args: TdxMailMergeFinishedEventArgs);
    procedure DoMailMergeRecordFinished(Sender: TObject;
      const Args: TdxMailMergeRecordFinishedEventArgs);
    procedure DoMailMergeRecordStarted(Sender: TObject;
      const Args: TdxMailMergeRecordStartedEventArgs);
    procedure DoMailMergeStarted(Sender: TObject;
      const Args: TdxMailMergeStartedEventArgs);
    procedure DoMailMergeGetTargetDocument(Sender: TObject;
      const Args: TdxMailMergeGetTargetDocumentEventArgs);

    property BatchUpdateHelper: TdxBatchUpdateHelper read GetBatchUpdateHelper;
    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property InternalServer: TdxInternalRichEditDocumentServer
      read GetInternalServer;
    property InnerServer: IdxRichEditDocumentServer read GetInnerServer
      implements IdxRichEditDocumentServer;
    property ServiceContainer: IdxServiceContainer read GetServiceContainer;

    property OnAfterExport: TNotifyEvent read FOnAfterExport
      write FOnAfterExport;
    property OnEmptyDocumentCreated: TNotifyEvent read FOnEmptyDocumentCreated
      write FOnEmptyDocumentCreated;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure BeginUpdate;
    procedure EndUpdate;
    procedure CancelUpdate;

    // IdxServiceContainer
    procedure AddService(const AServiceType: TdxServiceType;
      const AServiceInstance: IInterface); overload;
    procedure AddService(const AServiceType: TdxServiceType;
      const AServiceInstance: IInterface; APromote: Boolean); overload;
    procedure AddService(const AServiceType: TdxServiceType;
      const ACallback: IdxServiceCreatorCallback); overload;
    procedure AddService(const AServiceType: TdxServiceType;
      const ACallback: IdxServiceCreatorCallback; APromote: Boolean); overload;
    procedure RemoveService(const AServiceType: TdxServiceType); overload;
    procedure RemoveService(const AServiceType: TdxServiceType;
      APromote: Boolean); overload;
    // IdxServiceProvider
    function GetService(const AServiceType: TdxServiceType)
      : IInterface; overload;
    function GetService<T: IInterface>: T; overload;

    function CreateNewDocument(ARaiseDocumentClosing: Boolean = False)
      : Boolean; virtual;
    procedure LoadDocumentTemplate(const AFileName: string); overload;
    procedure LoadDocumentTemplate(const AFileName: string;
      ADocumentFormat: TdxRichEditDocumentFormat); overload;
    procedure LoadDocumentTemplate(AStream: TStream;
      ADocumentFormat: TdxRichEditDocumentFormat); overload;
    procedure LoadDocument(const AFileName: string); overload;
    procedure LoadDocument(const AFileName: string;
      ADocumentFormat: TdxRichEditDocumentFormat); overload;
    function LoadDocument(AStream: TStream): Boolean; overload;
    function LoadDocument(AStream: TStream;
      ADocumentFormat: TdxRichEditDocumentFormat): Boolean; overload;
    procedure SaveDocument; overload;
    procedure SaveDocument(const AFileName: string); overload;
    procedure SaveDocument(const AFileName: string;
      ADocumentFormat: TdxRichEditDocumentFormat); overload;
    procedure SaveDocument(AStream: TStream;
      ADocumentFormat: TdxRichEditDocumentFormat); overload;

    function CreateMailMergeOptions: IdxRichEditMailMergeOptions;
    procedure MailMerge(const ADocument: IdxRichEditDocument); overload;
    procedure MailMerge(const AOptions: IdxRichEditMailMergeOptions;
      const ATargetDocument: IdxRichEditDocument); overload;
    procedure MailMerge(const AFileName: string;
      AFormat: TdxRichEditDocumentFormat); overload;
    procedure MailMerge(AStream: TStream;
      AFormat: TdxRichEditDocumentFormat); overload;
    procedure MailMerge(const AOptions: IdxRichEditMailMergeOptions;
      const AFileName: string; AFormat: TdxRichEditDocumentFormat); overload;
    procedure MailMerge(const AOptions: IdxRichEditMailMergeOptions;
      AStream: TStream; AFormat: TdxRichEditDocumentFormat); overload;

    property Document: IdxRichEditDocument read GetDocument;
    property IsUpdateLocked: Boolean read GetIsUpdateLocked;

    property DocBytes: TArray<Byte> read GetDocBytes write SetDocBytes;
    property OpenXmlBytes: TArray<Byte> read GetOpenXmlBytes
      write SetOpenXmlBytes;
    property RtfText: string read GetRtfText write SetRtfText;
    property HtmlText: string read GetHtmlText write SetHtmlText;
    property Text: string read GetText write SetText;

    property Options: TdxRichEditDocumentServerOptions read GetOptions
      write SetOptions;
    property OnContentChanged: TNotifyEvent read FOnContentChanged
      write FOnContentChanged;
    property OnDocumentClosing: TdxCloseQueryEvent read FOnDocumentClosing
      write FOnDocumentClosing;
    property OnDocumentLoaded: TNotifyEvent read FOnDocumentLoaded
      write FOnDocumentLoaded;

    property OnGetEncryptionPassword: TdxGetEncryptionPasswordEvent
      read FOnGetEncryptionPassword write FOnGetEncryptionPassword;
    property OnCalculateDocumentVariable: TdxCalculateDocumentVariableEvent
      read FOnCalculateDocumentVariable write FOnCalculateDocumentVariable;
    property OnCustomizeMergeFields: TdxCustomizeMergeFieldsEvent
      read FOnCustomizeMergeFields write FOnCustomizeMergeFields;
    property OnMailMergeFinished: TdxMailMergeFinishedEvent
      read FOnMailMergeFinished write FOnMailMergeFinished;
    property OnMailMergeRecordFinished: TdxMailMergeRecordFinishedEvent
      read FOnMailMergeRecordFinished write FOnMailMergeRecordFinished;
    property OnMailMergeRecordStarted: TdxMailMergeRecordStartedEvent
      read FOnMailMergeRecordStarted write FOnMailMergeRecordStarted;
    property OnMailMergeStarted: TdxMailMergeStartedEvent
      read FOnMailMergeStarted write FOnMailMergeStarted;
    property OnMailMergeGetTargetDocument: TdxMailMergeGetTargetDocumentEvent
      read FOnMailMergeGetTargetDocument write FOnMailMergeGetTargetDocument;
  end;

  { TdxRichEditDocumentServer }

  TdxRichEditDocumentServer = class(TdxRichEditCustomDocumentServer)
  published
    property Options;
    property OnContentChanged;
    property OnDocumentClosing;
    property OnDocumentLoaded;
    property OnCalculateDocumentVariable;
    property OnCustomizeMergeFields;
    property OnMailMergeFinished;
    property OnMailMergeRecordFinished;
    property OnMailMergeRecordStarted;
    property OnMailMergeStarted;
    property OnMailMergeGetTargetDocument;

    property OnGetEncryptionPassword;
  end;

implementation

uses
  IOUtils,
  dxRichEdit.Api.NativeDocument,
  dxRichEdit.Api.MailMerge,
  dxRichEdit.Export.Formats;

type

  TdxRichEditInternalDocumentServer = class(TdxInternalRichEditDocumentServer)
  private
    FOwner: TdxRichEditCustomDocumentServer;
  protected
    function CreateInnerServer(ADocumentModel: TdxDocumentModel)
      : TdxInnerRichEditDocumentServer; override;
    function CreateOptionsCore(ADocumentServer: TdxInnerRichEditDocumentServer)
      : TdxRichEditControlOptionsBase; override;
    property Owner: TdxRichEditCustomDocumentServer read FOwner;
  public
    constructor Create(AOwner: TdxRichEditCustomDocumentServer);
  end;

  TdxRichEditInnerDocumentServer = class(TdxInnerRichEditDocumentServer)
  protected
    function CreateNativeDocument: IdxRichEditDocument; override;
    function CreateNativeSubDocument(APieceTable: TdxPieceTable)
      : IdxRichEditSubDocument; override;
    function GetEncryptionPassword(Sender: TObject; var APassword: string)
      : Boolean; override;
    function GetMailMergeOptions(const AOptions: IdxRichEditMailMergeOptions)
      : TdxMailMergeOptions; override;
  public
    function CreateMailMergeOptions: IdxRichEditMailMergeOptions; override;
    property DocumentLayout;
  end;

  { TdxRichEditInternalDocumentServer }

constructor TdxRichEditInternalDocumentServer.Create
  (AOwner: TdxRichEditCustomDocumentServer);
begin
  inherited Create(nil);
  FOwner := AOwner;
end;

function TdxRichEditInternalDocumentServer.CreateInnerServer(ADocumentModel
  : TdxDocumentModel): TdxInnerRichEditDocumentServer;
begin
  Result := TdxRichEditInnerDocumentServer.Create(Self)
end;

function TdxRichEditInternalDocumentServer.CreateOptionsCore(ADocumentServer
  : TdxInnerRichEditDocumentServer): TdxRichEditControlOptionsBase;
begin
  Result := TdxRichEditDocumentServerOptions.Create(ADocumentServer);
end;

{ TdxRichEditInnerDocumentServer }

function TdxRichEditInnerDocumentServer.CreateMailMergeOptions
  : IdxRichEditMailMergeOptions;
begin
  Result := TdxNativeMailMergeOptions.Create;
end;

function TdxRichEditInnerDocumentServer.CreateNativeDocument
  : IdxRichEditDocument;
begin
  Result := TdxNativeDocument.Create(DocumentModel.MainPieceTable, Self);
end;

function TdxRichEditInnerDocumentServer.CreateNativeSubDocument
  (APieceTable: TdxPieceTable): IdxRichEditSubDocument;
begin
  Result := TdxNativeSubDocument.Create(APieceTable, Self);
end;

function TdxRichEditInnerDocumentServer.GetEncryptionPassword(Sender: TObject;
  var APassword: string): Boolean;
begin
  Result := (Owner as TdxRichEditInternalDocumentServer)
    .Owner.DoGetEncryptionPassword(APassword);
end;

function TdxRichEditInnerDocumentServer.GetMailMergeOptions(const AOptions
  : IdxRichEditMailMergeOptions): TdxMailMergeOptions;
begin
  Result := TdxNativeMailMergeOptions(AOptions).GetInternalMailMergeOptions;
end;

{ TdxRichEditCustomDocumentServer }

constructor TdxRichEditCustomDocumentServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInnerServer := CreateDocumentServer;
  SubscribeInnerControlEvents;
end;

destructor TdxRichEditCustomDocumentServer.Destroy;
begin
  UnsubscribeInnerServerEvents;
  inherited Destroy;
end;

procedure TdxRichEditCustomDocumentServer.AddService(const AServiceType
  : TdxServiceType; const ACallback: IdxServiceCreatorCallback);
begin
  ServiceContainer.AddService(AServiceType, ACallback);
end;

procedure TdxRichEditCustomDocumentServer.AddService(const AServiceType
  : TdxServiceType; const AServiceInstance: IInterface; APromote: Boolean);
begin
  ServiceContainer.AddService(AServiceType, AServiceInstance, APromote);
end;

procedure TdxRichEditCustomDocumentServer.AddService(const AServiceType
  : TdxServiceType; const AServiceInstance: IInterface);
begin
  ServiceContainer.AddService(AServiceType, AServiceInstance);
end;

procedure TdxRichEditCustomDocumentServer.AddService(const AServiceType
  : TdxServiceType; const ACallback: IdxServiceCreatorCallback;
  APromote: Boolean);
begin
  ServiceContainer.AddService(AServiceType, ACallback, APromote);
end;

procedure TdxRichEditCustomDocumentServer.BeginUpdate;
begin
  InnerServer.BeginUpdate;
end;

procedure TdxRichEditCustomDocumentServer.CancelUpdate;
begin
  InnerServer.CancelUpdate;
end;

function TdxRichEditCustomDocumentServer.CreateMailMergeOptions
  : IdxRichEditMailMergeOptions;
begin
  Result := InternalServer.CreateMailMergeOptions;
end;

procedure TdxRichEditCustomDocumentServer.EndUpdate;
begin
  InnerServer.EndUpdate;
end;

function TdxRichEditCustomDocumentServer.GetBatchUpdateHelper
  : TdxBatchUpdateHelper;
begin
  Result := InnerServer.BatchUpdateHelper;
end;

function TdxRichEditCustomDocumentServer.CreateDocumentServer
  : IdxRichEditDocumentServer;
begin
  Result := TdxRichEditInternalDocumentServer.Create(Self);
end;

function TdxRichEditCustomDocumentServer.GetDocument: IdxRichEditDocument;
begin
  Result := InnerServer.Document
end;

function TdxRichEditCustomDocumentServer.GetDocumentModel: TdxDocumentModel;
begin
  Result := InternalServer.DocumentModel;
end;

function TdxRichEditCustomDocumentServer.GetInnerServer
  : IdxRichEditDocumentServer;
begin
  Result := FInnerServer;
end;

function TdxRichEditCustomDocumentServer.GetInternalServer
  : TdxInternalRichEditDocumentServer;
begin
  Result := (InnerServer as TdxInternalRichEditDocumentServer);
end;

function TdxRichEditCustomDocumentServer.GetIsUpdateLocked: Boolean;
begin
  Result := (InnerServer <> nil) and InnerServer.IsUpdateLocked;
end;

function TdxRichEditCustomDocumentServer.GetOptions
  : TdxRichEditDocumentServerOptions;
begin
  Result := InternalServer.Options as TdxRichEditDocumentServerOptions;
end;

function TdxRichEditCustomDocumentServer.GetService(const AServiceType
  : TdxServiceType): IInterface;
begin
  (InnerServer as IdxServiceProvider).GetService(AServiceType);
end;

function TdxRichEditCustomDocumentServer.GetService<T>: T;
begin
  Result := DocumentModel.GetService<T>;
end;

function TdxRichEditCustomDocumentServer.GetServiceContainer
  : IdxServiceContainer;
begin
  Result := InnerServer as IdxServiceContainer;
end;

function TdxRichEditCustomDocumentServer.CreateNewDocument(ARaiseDocumentClosing
  : Boolean = False): Boolean;
begin
  Result := InternalServer.CreateNewDocument(ARaiseDocumentClosing);
end;

procedure TdxRichEditCustomDocumentServer.DoAfterExport(Sender: TObject);
begin
  if Assigned(FOnAfterExport) then
    FOnAfterExport(Self);
end;

procedure TdxRichEditCustomDocumentServer.DoCalculateDocumentVariable
  (Sender: TObject; Args: TdxCalculateDocumentVariableEventArgs);
begin
  if Assigned(FOnCalculateDocumentVariable) then
    FOnCalculateDocumentVariable(Self, Args);
end;

procedure TdxRichEditCustomDocumentServer.DoContentChanged(Sender: TObject);
begin
  if Assigned(FOnContentChanged) then
    FOnContentChanged(Self);
end;

procedure TdxRichEditCustomDocumentServer.DoCustomizeMergeFields
  (Sender: TObject; const Args: TdxCustomizeMergeFieldsEventArgs);
begin
  if Assigned(FOnCustomizeMergeFields) then
    FOnCustomizeMergeFields(Self, Args);
end;

procedure TdxRichEditCustomDocumentServer.DoDocumentClosing(Sender: TObject;
  var CanClose: Boolean);
begin
  if Assigned(FOnDocumentClosing) then
    FOnDocumentClosing(Self, CanClose);
end;

procedure TdxRichEditCustomDocumentServer.DoDocumentLoaded(Sender: TObject);
begin
  if Assigned(FOnDocumentLoaded) then
    FOnDocumentLoaded(Self);
end;

procedure TdxRichEditCustomDocumentServer.DoEmptyDocumentCreated
  (Sender: TObject);
begin
  if Assigned(FOnEmptyDocumentCreated) then
    FOnEmptyDocumentCreated(Self);
end;

function TdxRichEditCustomDocumentServer.DoGetEncryptionPassword
  (var APassword: string): Boolean;
begin
  if Assigned(FOnGetEncryptionPassword) then
    FOnGetEncryptionPassword(Self, APassword, Result)
  else
    Result := False;
end;

procedure TdxRichEditCustomDocumentServer.DoMailMergeFinished(Sender: TObject;
  const Args: TdxMailMergeFinishedEventArgs);
begin
  if Assigned(FOnMailMergeFinished) then
    FOnMailMergeFinished(Self, Args);
end;

procedure TdxRichEditCustomDocumentServer.DoMailMergeGetTargetDocument
  (Sender: TObject; const Args: TdxMailMergeGetTargetDocumentEventArgs);
begin
  if Assigned(FOnMailMergeGetTargetDocument) then
    FOnMailMergeGetTargetDocument(Self, Args);
end;

procedure TdxRichEditCustomDocumentServer.DoMailMergeRecordFinished
  (Sender: TObject; const Args: TdxMailMergeRecordFinishedEventArgs);
begin
  if Assigned(FOnMailMergeRecordFinished) then
    FOnMailMergeRecordFinished(Self, Args);
end;

procedure TdxRichEditCustomDocumentServer.DoMailMergeRecordStarted
  (Sender: TObject; const Args: TdxMailMergeRecordStartedEventArgs);
begin
  if Assigned(FOnMailMergeRecordStarted) then
    FOnMailMergeRecordStarted(Self, Args);
end;

procedure TdxRichEditCustomDocumentServer.DoMailMergeStarted(Sender: TObject;
  const Args: TdxMailMergeStartedEventArgs);
begin
  if Assigned(FOnMailMergeStarted) then
    FOnMailMergeStarted(Self, Args);
end;

procedure TdxRichEditCustomDocumentServer.LoadDocumentTemplate
  (const AFileName: string);
begin
  InternalServer.LoadDocumentTemplate(AFileName);
end;

procedure TdxRichEditCustomDocumentServer.LoadDocumentTemplate(const AFileName
  : string; ADocumentFormat: TdxRichEditDocumentFormat);
begin
  InternalServer.LoadDocumentTemplate(AFileName, ADocumentFormat);
end;

procedure TdxRichEditCustomDocumentServer.LoadDocumentTemplate(AStream: TStream;
  ADocumentFormat: TdxRichEditDocumentFormat);
begin
  InternalServer.LoadDocument(AStream, ADocumentFormat);
end;

procedure TdxRichEditCustomDocumentServer.MailMerge(const AFileName: string;
  AFormat: TdxRichEditDocumentFormat);
begin
  InternalServer.MailMerge(AFileName, AFormat);
end;

procedure TdxRichEditCustomDocumentServer.MailMerge(const AOptions
  : IdxRichEditMailMergeOptions; const ATargetDocument: IdxRichEditDocument);
begin
  InternalServer.MailMerge(AOptions, ATargetDocument);
end;

procedure TdxRichEditCustomDocumentServer.MailMerge(const ADocument
  : IdxRichEditDocument);
begin
  InternalServer.MailMerge(ADocument);
end;

procedure TdxRichEditCustomDocumentServer.MailMerge(const AOptions
  : IdxRichEditMailMergeOptions; AStream: TStream;
  AFormat: TdxRichEditDocumentFormat);
begin
  InternalServer.MailMerge(AOptions, AStream, AFormat);
end;

procedure TdxRichEditCustomDocumentServer.MailMerge(const AOptions
  : IdxRichEditMailMergeOptions; const AFileName: string;
  AFormat: TdxRichEditDocumentFormat);
begin
  InternalServer.MailMerge(AOptions, AFileName, AFormat);
end;

procedure TdxRichEditCustomDocumentServer.MailMerge(AStream: TStream;
  AFormat: TdxRichEditDocumentFormat);
begin
  InternalServer.MailMerge(AStream, AFormat);
end;

function TdxRichEditCustomDocumentServer.GetDocBytes: TArray<Byte>;
begin
  Result := Document.DocBytes;
end;

procedure TdxRichEditCustomDocumentServer.SetDocBytes
  (const AValue: TArray<Byte>);
begin
  Document.DocBytes := AValue;
end;

function TdxRichEditCustomDocumentServer.GetOpenXmlBytes: TArray<Byte>;
begin
  Result := Document.OpenXmlBytes;
end;

procedure TdxRichEditCustomDocumentServer.SetOpenXmlBytes
  (const AValue: TArray<Byte>);
begin
  Document.OpenXmlBytes := AValue;
end;

function TdxRichEditCustomDocumentServer.GetRtfText: string;
begin
  Result := Document.RtfText;
end;

procedure TdxRichEditCustomDocumentServer.SetRtfText(const AValue: string);
begin
  Document.RtfText := AValue;
end;

function TdxRichEditCustomDocumentServer.GetText: string;
begin
  Result := Document.Text;
end;

procedure TdxRichEditCustomDocumentServer.SetText(const AValue: string);
begin
  Document.Text := AValue;
end;

function TdxRichEditCustomDocumentServer.GetHtmlText: string;
begin
  Result := FInnerServer.HtmlText;
end;

procedure TdxRichEditCustomDocumentServer.SetHtmlText(const AValue: string);
begin
  FInnerServer.HtmlText := AValue;
end;

procedure TdxRichEditCustomDocumentServer.LoadDocument(const AFileName: string);
begin
  InternalServer.LoadDocument(AFileName);
end;

procedure TdxRichEditCustomDocumentServer.LoadDocument(const AFileName: string;
  ADocumentFormat: TdxRichEditDocumentFormat);
begin
  InternalServer.LoadDocument(AFileName, ADocumentFormat);
end;

function TdxRichEditCustomDocumentServer.LoadDocument(AStream: TStream;
  ADocumentFormat: TdxRichEditDocumentFormat): Boolean;
begin
  Result := TdxRichEditInnerDocumentServer(InternalServer.InnerServer)
    .LoadDocumentCore(AStream, ADocumentFormat, '');
end;

function TdxRichEditCustomDocumentServer.LoadDocument(AStream: TStream)
  : Boolean;
begin
  Result := TdxRichEditInnerDocumentServer(InternalServer.InnerServer)
    .LoadDocumentCore(AStream, TdxRichEditDocumentFormat.Undefined, '');
end;

procedure TdxRichEditCustomDocumentServer.SaveDocument;
var
  ADocumentSaveOptions: TdxDocumentSaveOptions;
begin
  ADocumentSaveOptions := Options.DocumentSaveOptions;
  if not(not ADocumentSaveOptions.CanSaveToCurrentFileName or
    (ADocumentSaveOptions.CurrentFileName = '') or
    (ADocumentSaveOptions.CurrentFormat = TdxRichEditDocumentFormat.Undefined))
  then
    SaveDocument(ADocumentSaveOptions.CurrentFileName,
      ADocumentSaveOptions.CurrentFormat);
end;

procedure TdxRichEditCustomDocumentServer.SaveDocument(const AFileName: string;
  ADocumentFormat: TdxRichEditDocumentFormat);
begin
  InternalServer.SaveDocument(AFileName, ADocumentFormat);
end;

procedure TdxRichEditCustomDocumentServer.SaveDocument(AStream: TStream;
  ADocumentFormat: TdxRichEditDocumentFormat);
begin
  InternalServer.SaveDocument(AStream, ADocumentFormat);
end;

procedure TdxRichEditCustomDocumentServer.SetOptions
  (const Value: TdxRichEditDocumentServerOptions);
begin
  InternalServer.Options.Assign(Value);
end;

procedure TdxRichEditCustomDocumentServer.SaveDocument(const AFileName: string);
var
  AFormat: TdxRichEditDocumentFormat;
begin
  if Length(TPath.GetExtension(AFileName)) > 1 then
  begin
    AFormat := DocumentModel.AutodetectDocumentFormat(AFileName, False);
    InternalServer.SaveDocument(AFileName, AFormat);
  end
  else
  begin
    AFormat := Options.DocumentSaveOptions.DefaultFormat;
    InternalServer.SaveDocument(TPath.ChangeExtension(AFileName,
      TdxExportFileFormats.GetFileExtension(AFormat)), AFormat);
  end;
end;

procedure TdxRichEditCustomDocumentServer.RemoveService(const AServiceType
  : TdxServiceType; APromote: Boolean);
begin
  ServiceContainer.RemoveService(AServiceType, APromote);
end;

procedure TdxRichEditCustomDocumentServer.RemoveService(const AServiceType
  : TdxServiceType);
begin
  ServiceContainer.RemoveService(AServiceType);
end;

procedure TdxRichEditCustomDocumentServer.SubscribeInnerControlEvents;
var
  AInnerServer: TdxInnerRichEditDocumentServer;
begin
  AInnerServer := (FInnerServer as TdxRichEditInternalDocumentServer)
    .InnerServer;

  AInnerServer.AfterExport.Add(DoAfterExport);
  AInnerServer.ContentChanged.Add(DoContentChanged);
  AInnerServer.DocumentClosing.Add(DoDocumentClosing);
  AInnerServer.DocumentLoaded.Add(DoDocumentLoaded);
  AInnerServer.EmptyDocumentCreated.Add(DoEmptyDocumentCreated);

  AInnerServer.CalculateDocumentVariable.Add(DoCalculateDocumentVariable);
  AInnerServer.CustomizeMergeFields.Add(DoCustomizeMergeFields);
  AInnerServer.MailMergeFinished.Add(DoMailMergeFinished);
  AInnerServer.MailMergeRecordFinished.Add(DoMailMergeRecordFinished);
  AInnerServer.MailMergeRecordStarted.Add(DoMailMergeRecordStarted);
  AInnerServer.MailMergeStarted.Add(DoMailMergeStarted);
  AInnerServer.MailMergeGetTargetDocument.Add(DoMailMergeGetTargetDocument);
end;

procedure TdxRichEditCustomDocumentServer.UnsubscribeInnerServerEvents;
var
  AInnerServer: TdxInnerRichEditDocumentServer;
begin
  AInnerServer := (FInnerServer as TdxRichEditInternalDocumentServer)
    .InnerServer;

  AInnerServer.AfterExport.Remove(DoAfterExport);
  AInnerServer.ContentChanged.Remove(DoContentChanged);
  AInnerServer.DocumentClosing.Remove(DoDocumentClosing);
  AInnerServer.DocumentLoaded.Remove(DoDocumentLoaded);
  AInnerServer.EmptyDocumentCreated.Remove(DoEmptyDocumentCreated);

  AInnerServer.CalculateDocumentVariable.Remove(DoCalculateDocumentVariable);
  AInnerServer.CustomizeMergeFields.Remove(DoCustomizeMergeFields);
  AInnerServer.MailMergeFinished.Remove(DoMailMergeFinished);
  AInnerServer.MailMergeRecordFinished.Remove(DoMailMergeRecordFinished);
  AInnerServer.MailMergeRecordStarted.Remove(DoMailMergeRecordStarted);
  AInnerServer.MailMergeStarted.Remove(DoMailMergeStarted);
  AInnerServer.MailMergeGetTargetDocument.Remove(DoMailMergeGetTargetDocument);
end;

end.

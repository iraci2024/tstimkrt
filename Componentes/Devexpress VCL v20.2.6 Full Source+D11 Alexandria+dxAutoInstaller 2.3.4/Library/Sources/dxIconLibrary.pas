{ ******************************************************************** }
{ }
{ Developer Express Visual Component Library }
{ Express Cross Platform Library classes }
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
{ LICENSED TO DISTRIBUTE THE EXPRESSCROSSPLATFORMLIBRARY AND ALL }
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

unit dxIconLibrary;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI22}
  System.Hash,
{$ENDIF}
  Classes, Windows, SysUtils, Controls, Generics.Defaults, Generics.Collections,
  StrUtils,
  cxClasses, dxCore, cxGraphics, dxSmartImage, cxGeometry, dxGDIPlusClasses,
  dxHash, dxHashUtils, dxThreading;

type
  TdxIconLibraryEnumFilesProc = reference to procedure(const AFileName: string);

type
  TdxIconLibrary = class;

  TFileObjectType = (fotFile, fotDirectory);

  { TdxIconLibraryCustomObject }

  TdxIconLibraryCustomObjectClass = class of TdxIconLibraryCustomObject;

  TdxIconLibraryCustomObject = class
  strict private
    FDisplayName: string;
    FName: string;
  private
    FParent: TdxIconLibraryCustomObject;
  protected
    procedure DoBeforeRemove(AObject: TdxIconLibraryCustomObject); virtual;
  public
    constructor Create(const AName: string;
      AParent: TdxIconLibraryCustomObject); virtual;
    procedure BeforeDestruction; override;
    procedure Refresh; virtual; abstract;

    property DisplayName: string read FDisplayName;
    property Name: string read FName;
    property Parent: TdxIconLibraryCustomObject read FParent;
  end;

  { TdxIconLibraryCollection }

  TdxIconLibraryCollection = class(TdxIconLibraryCustomObject)
  strict private
    function GetCount: Integer;
    function GetItem(AIndex: Integer): TdxIconLibraryCustomObject; inline;
  protected
    FItemClass: TdxIconLibraryCustomObjectClass;
    FItems: TcxObjectList;

    procedure EnumFiles(AProc: TdxIconLibraryEnumFilesProc); virtual; abstract;
  public
    constructor Create(const AName: string;
      AParent: TdxIconLibraryCustomObject); override;
    destructor Destroy; override;
    procedure BeforeDestruction; override;
    function Add(const AName: string): TdxIconLibraryCustomObject;
    function Find(const AName: string; var AIndex: Integer): Boolean;
    procedure Refresh; override;
    procedure Remove(AItem: TdxIconLibraryCustomObject);

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TdxIconLibraryCustomObject
      read GetItem; default;
  end;

  { TdxIconLibraryImage }

  TdxIconLibraryImage = class(TdxIconLibraryCustomObject)
  strict private
    FImage: TdxSmartGlyph;
    FImageSize: TSize;
    FImageSizeAssigned: Boolean;
    FImageSizeAsString: string;
    FTag: TcxTag;

    function FetchImageSizeFromFileName(out ASize: TSize): Boolean;
    procedure ImageSizeNeeded; inline;
    function GetFileName: string; inline;
    function GetImageSize: TSize; inline;
    function GetImageSizeAsString: string; inline;
  public
    constructor Create(const AName: string;
      AParent: TdxIconLibraryCustomObject); override;
    destructor Destroy; override;
    procedure LoadFromFile;
    procedure Refresh; override;

    property FileName: string read GetFileName;
    property Image: TdxSmartGlyph read FImage;
    property ImageSize: TSize read GetImageSize;
    property ImageSizeAsString: string read GetImageSizeAsString;
    property Tag: TcxTag read FTag write FTag;
  end;

  { TdxIconLibraryCategory }

  TdxIconLibraryCategory = class(TdxIconLibraryCollection)
  strict private
    function GetItem(AIndex: Integer): TdxIconLibraryImage; inline;
  protected
    procedure EnumFiles(AProc: TdxIconLibraryEnumFilesProc); override;
  public
    constructor Create(const AName: string;
      AParent: TdxIconLibraryCustomObject); override;

    property Items[Index: Integer]: TdxIconLibraryImage read GetItem; default;
  end;

  { TdxIconLibrarySet }

  TdxIconLibrarySet = class(TdxIconLibraryCollection)
  strict private
    function GetItem(AIndex: Integer): TdxIconLibraryCategory; inline;
  protected
    procedure EnumFiles(AProc: TdxIconLibraryEnumFilesProc); override;
  public
    constructor Create(const AName: string;
      AParent: TdxIconLibraryCustomObject); override;

    property Items[Index: Integer]: TdxIconLibraryCategory
      read GetItem; default;
  end;

  { IdxIconLibraryListener }

  IdxIconLibraryListener = interface
    ['{738906C8-47A7-4002-9FBE-34BA00419D4E}']
    procedure OnChanged(Sender: TdxIconLibraryCollection);
    procedure OnChanging(Sender: TdxIconLibraryCollection);
    procedure OnRemoving(Sender: TdxIconLibraryCustomObject);
  end;

  { TdxIconLibraryListeners }

  TdxIconLibraryListeners = class(TList<IdxIconLibraryListener>)
  protected
    procedure NotifyChanging(Sender: TdxIconLibraryCollection);
    procedure NotifyChanged(Sender: TdxIconLibraryCollection);
    procedure NotifyRemoving(Sender: TdxIconLibraryCustomObject);
  end;

  { TdxIconLibrary }

  TdxIconLibrary = class(TdxIconLibraryCollection)
  public const
    SizeVector = 'Vector';
  strict private
    FFileSystemMonitor: TObject;
    FListeners: TdxIconLibraryListeners;

    procedure FileSystemChangeHandler(Sender: TObject);
    function GetItem(AIndex: Integer): TdxIconLibrarySet; inline;
  protected
    procedure DoBeforeRemove(AObject: TdxIconLibraryCustomObject); override;
    procedure EnumFiles(AProc: TdxIconLibraryEnumFilesProc); override;
  public
    constructor Create(const APath: string); reintroduce;
    destructor Destroy; override;
    procedure Refresh; override;

    property Listeners: TdxIconLibraryListeners read FListeners;
    property Items[Index: Integer]: TdxIconLibrarySet read GetItem; default;
  end;

function dxGetIconLibraryPath: string;

implementation

uses
  Types, dxSVGImage;

const
  dxIconLibraryRelativePath = '\ExpressLibrary\Sources\Icon Library\';

type

  { TdxIconLibraryMonitor }

  TdxIconLibraryMonitor = class(TThread)
  strict private
    FChangeAggregator: TcxTimer;
    FChangeHandle: THandle;
    FPath: string;

    FOnChange: TNotifyEvent;

    procedure ReleaseHandle;
    procedure TimerHandler(Sender: TObject);
  protected
    procedure Execute; override;
  public
    constructor Create(const APath: string; AChangeNotify: TNotifyEvent);
    destructor Destroy; override;
  end;

procedure ListFiles(APath: string; AProc: TdxIconLibraryEnumFilesProc;
  AFileObjectType: TFileObjectType; ARecursive: Boolean);
var
  ASearchRec: TSearchRec;
  AType: TFileObjectType;
begin
  APath := IncludeTrailingPathDelimiter(APath);
  if FindFirst(APath + '*', faAnyFile, ASearchRec) = 0 then
    try
      repeat
        if (ASearchRec.Name = '.') or (ASearchRec.Name = '..') then
          Continue;

        if (ASearchRec.Attr and faDirectory) = faDirectory then
          AType := fotDirectory
        else
          AType := fotFile;

        if ARecursive and (AType = fotDirectory) then
          ListFiles(APath + ASearchRec.Name + PathDelim, AProc, AFileObjectType,
            ARecursive);

        if AType = AFileObjectType then
          AProc(APath + ASearchRec.Name);
      until FindNext(ASearchRec) <> 0;
    finally
      FindClose(ASearchRec);
    end;
end;

function dxGetIconLibraryPath: string;
begin
{$IF DEFINED(CXTEST) AND DEFINED(CXEDITORSTEST)}
  Result := ExtractFilePath(ParamStr(0)) + 'Icon Library\';
{$ELSE}
  Result := GetEnvironmentVariable('DXVCL') + dxIconLibraryRelativePath;
{$IFEND}
end;

{ TdxIconLibraryCustomObject }

constructor TdxIconLibraryCustomObject.Create(const AName: string;
  AParent: TdxIconLibraryCustomObject);
begin
  inherited Create;
  FName := AName;
  FParent := AParent;
  FDisplayName := ExtractFileName(FName);
end;

procedure TdxIconLibraryCustomObject.BeforeDestruction;
begin
  inherited;
  DoBeforeRemove(Self);
end;

procedure TdxIconLibraryCustomObject.DoBeforeRemove
  (AObject: TdxIconLibraryCustomObject);
begin
  if FParent <> nil then
    FParent.DoBeforeRemove(AObject);
end;

{ TdxIconLibraryCollection }

constructor TdxIconLibraryCollection.Create(const AName: string;
  AParent: TdxIconLibraryCustomObject);
begin
  inherited;
  FItems := TcxObjectList.Create;
  FItemClass := TdxIconLibraryCustomObject;
end;

destructor TdxIconLibraryCollection.Destroy;
begin
  FreeAndNil(FItems);
  inherited Destroy;
end;

procedure TdxIconLibraryCollection.BeforeDestruction;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].FParent := nil;
  inherited BeforeDestruction;
end;

function TdxIconLibraryCollection.Add(const AName: string)
  : TdxIconLibraryCustomObject;
var
  AIndex: Integer;
begin
  if Find(AName, AIndex) then
    Exit(Items[AIndex]);

  Result := FItemClass.Create(AName, Self);
  FItems.Insert(AIndex, Result);
end;

function TdxIconLibraryCollection.Find(const AName: string;
  var AIndex: Integer): Boolean;
var
  ACompareResult: Integer;
  AHigh: Integer;
  ALow: Integer;
  AMiddle: Integer;
begin
  ALow := 0;
  AHigh := Count - 1;
  Result := False;
  while ALow <= AHigh do
  begin
    AMiddle := (ALow + AHigh) shr 1;
    ACompareResult := CompareText(Items[AMiddle].Name, AName);
    if ACompareResult < 0 then
      ALow := AMiddle + 1
    else
    begin
      AHigh := AMiddle - 1;
      if ACompareResult = 0 then
        Result := True;
    end;
  end;
  AIndex := ALow;
end;

procedure TdxIconLibraryCollection.Remove(AItem: TdxIconLibraryCustomObject);
begin
  FItems.FreeAndRemove(AItem);
end;

procedure TdxIconLibraryCollection.Refresh;
var
  ARemovedItems: TList;
  I: Integer;
begin
  if Count > 0 then
  begin
    ARemovedItems := TList.Create;
    try
      ARemovedItems.Capacity := Count;
      ARemovedItems.Assign(FItems);
      EnumFiles(procedure(const S: string)
        var
          AItem: TdxIconLibraryCustomObject;
        begin
          AItem := Add(S);
          AItem.Refresh;
          ARemovedItems.Remove(AItem);
        end);
      for I := 0 to ARemovedItems.Count - 1 do
        Remove(ARemovedItems.List[I]);
    finally
      ARemovedItems.Free;
    end;
  end
  else
    EnumFiles(procedure(const S: string)
      begin
        Add(S).Refresh;
      end);
end;

function TdxIconLibraryCollection.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TdxIconLibraryCollection.GetItem(AIndex: Integer)
  : TdxIconLibraryCustomObject;
begin
  Result := TdxIconLibraryCustomObject(FItems.List[AIndex]);
end;

{ TdxIconLibraryImage }

constructor TdxIconLibraryImage.Create(const AName: string;
AParent: TdxIconLibraryCustomObject);
begin
  inherited;
  FImage := TdxSmartGlyph.Create;
end;

destructor TdxIconLibraryImage.Destroy;
begin
  FreeAndNil(FImage);
  inherited Destroy;
end;

procedure TdxIconLibraryImage.LoadFromFile;
begin
  FImage.LoadFromFile(FileName);
  FImage.HandleNeeded;
end;

procedure TdxIconLibraryImage.Refresh;
begin
  FImage.Clear;
  FImageSizeAssigned := False;
end;

function TdxIconLibraryImage.FetchImageSizeFromFileName
  (out ASize: TSize): Boolean;
var
  ADelimiter: Integer;
  ASizeString: string;
begin
  ASizeString := ChangeFileExt(DisplayName, '');
  ADelimiter := LastDelimiter('_', ASizeString);
  if ADelimiter <= 0 then
    Exit(False);

  ASizeString := Copy(ASizeString, ADelimiter + 1, MaxWord);
  ADelimiter := Pos('x', ASizeString);
  if ADelimiter <= 0 then
    Exit(False);

  ASize.cx := StrToIntDef(Copy(ASizeString, 1, ADelimiter - 1), -1);
  ASize.cy := StrToIntDef(Copy(ASizeString, ADelimiter + 1, MaxWord), -1);
  Result := (ASize.cx > 0) and (ASize.cy > 0);
end;

procedure TdxIconLibraryImage.ImageSizeNeeded;
var
  ACodec: TdxSmartImageCodecClass;
begin
  if not FImageSizeAssigned then
  begin
    FImageSizeAssigned := True;
    FImageSize := cxNullSize;

    if EndsText('.svg', Name) then
      FImageSizeAsString := TdxIconLibrary.SizeVector
    else if FetchImageSizeFromFileName(FImageSize) or
      TdxSmartImageCodecsRepository.GetImageInfo(FileName, FImageSize, ACodec)
    then
      FImageSizeAsString := cxSizeToString(FImageSize)
    else
      FImageSizeAsString := '?';
  end;
end;

function TdxIconLibraryImage.GetFileName: string;
begin
  Result := Name;
end;

function TdxIconLibraryImage.GetImageSize: TSize;
begin
  ImageSizeNeeded;
  Result := FImageSize;
end;

function TdxIconLibraryImage.GetImageSizeAsString: string;
begin
  ImageSizeNeeded;
  Result := FImageSizeAsString;
end;

{ TdxIconLibraryCategory }

constructor TdxIconLibraryCategory.Create(const AName: string;
AParent: TdxIconLibraryCustomObject);
begin
  inherited;
  FItemClass := TdxIconLibraryImage;
end;

procedure TdxIconLibraryCategory.EnumFiles(AProc: TdxIconLibraryEnumFilesProc);
begin
  ListFiles(Name, AProc, fotFile, True);
end;

function TdxIconLibraryCategory.GetItem(AIndex: Integer): TdxIconLibraryImage;
begin
  Result := TdxIconLibraryImage(inherited Items[AIndex]);
end;

{ TdxIconLibrarySet }

constructor TdxIconLibrarySet.Create(const AName: string;
AParent: TdxIconLibraryCustomObject);
begin
  inherited;
  FItemClass := TdxIconLibraryCategory;
end;

procedure TdxIconLibrarySet.EnumFiles(AProc: TdxIconLibraryEnumFilesProc);
begin
  ListFiles(Name, AProc, fotDirectory, False);
end;

function TdxIconLibrarySet.GetItem(AIndex: Integer): TdxIconLibraryCategory;
begin
  Result := TdxIconLibraryCategory(inherited Items[AIndex]);
end;

{ TdxIconLibraryListeners }

procedure TdxIconLibraryListeners.NotifyChanged
  (Sender: TdxIconLibraryCollection);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].OnChanged(Sender);
end;

procedure TdxIconLibraryListeners.NotifyChanging
  (Sender: TdxIconLibraryCollection);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].OnChanging(Sender);
end;

procedure TdxIconLibraryListeners.NotifyRemoving
  (Sender: TdxIconLibraryCustomObject);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].OnRemoving(Sender);
end;

{ TdxIconLibrary }

constructor TdxIconLibrary.Create(const APath: string);
begin
  inherited Create(APath, nil);
  FItemClass := TdxIconLibrarySet;
  FListeners := TdxIconLibraryListeners.Create;
  FFileSystemMonitor := TdxIconLibraryMonitor.Create(APath,
    FileSystemChangeHandler);
end;

destructor TdxIconLibrary.Destroy;
begin
  FreeAndNil(FFileSystemMonitor);
  FreeAndNil(FListeners);
  inherited;
end;

procedure TdxIconLibrary.DoBeforeRemove(AObject: TdxIconLibraryCustomObject);
begin
  Listeners.NotifyRemoving(AObject);
end;

procedure TdxIconLibrary.EnumFiles(AProc: TdxIconLibraryEnumFilesProc);
begin
  ListFiles(Name, AProc, fotDirectory, False);
end;

procedure TdxIconLibrary.FileSystemChangeHandler(Sender: TObject);
begin
  Refresh;
end;

function TdxIconLibrary.GetItem(AIndex: Integer): TdxIconLibrarySet;
begin
  Result := inherited Items[AIndex] as TdxIconLibrarySet;
end;

procedure TdxIconLibrary.Refresh;
begin
  Listeners.NotifyChanging(Self);
  try
    inherited;
  finally
    Listeners.NotifyChanged(Self);
  end;
end;

{ TdxIconLibraryMonitor }

constructor TdxIconLibraryMonitor.Create(const APath: string;
AChangeNotify: TNotifyEvent);
const
  ChangesFilter = FILE_NOTIFY_CHANGE_FILE_NAME or FILE_NOTIFY_CHANGE_DIR_NAME or
    FILE_NOTIFY_CHANGE_SIZE or FILE_NOTIFY_CHANGE_LAST_WRITE;
begin
  inherited Create(False);
  FOnChange := AChangeNotify;
  FPath := IncludeTrailingPathDelimiter(APath);
  FChangeHandle := FindFirstChangeNotification(PChar(FPath), True,
    ChangesFilter);
  if FChangeHandle = INVALID_HANDLE_VALUE then
    FChangeHandle := 0;
  FChangeAggregator := cxCreateTimer(TimerHandler, 1000, False);
end;

destructor TdxIconLibraryMonitor.Destroy;
begin
  ReleaseHandle;
  TdxUIThreadSyncService.Unsubscribe(Self);
  FreeAndNil(FChangeAggregator);
  inherited;
end;

procedure TdxIconLibraryMonitor.Execute;
begin
  while (FChangeHandle <> 0) and not Terminated do
  begin
    case WaitForSingleObject(FChangeHandle, INFINITE) of
      WAIT_OBJECT_0:
        if not Terminated then
        begin
          TdxUIThreadSyncService.EnqueueInvokeInUIThread(Self, procedure
            begin
              FChangeAggregator.Enabled := False;
              FChangeAggregator.Enabled := True;
            end);
          FindNextChangeNotification(FChangeHandle);
        end;
    end;
  end;
end;

procedure TdxIconLibraryMonitor.ReleaseHandle;
begin
  FindCloseChangeNotification(FChangeHandle);
  FChangeHandle := 0;
end;

procedure TdxIconLibraryMonitor.TimerHandler(Sender: TObject);
begin
  FChangeAggregator.Enabled := False;
  CallNotify(FOnChange, Self);
end;

end.

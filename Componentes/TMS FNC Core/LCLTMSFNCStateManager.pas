{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{            copyright (c) 2022                                      }
{            Email : info@tmssoftware.com                            }
{            Web : http://www.tmssoftware.com                        }
{                                                                    }
{ The source code is given as is. The author is not responsible      }
{ for any possible damage done due to the use of this code.          }
{ The complete source code remains property of the author and may    }
{ not be distributed, published, given or sold in any form as such.  }
{ No parts of the source code can be included in any other component }
{ or application without written authorization of the author.        }
{********************************************************************}

unit LCLTMSFNCStateManager;

{$I LCLTMSFNCDefines.inc}

interface

uses
  Classes, LCLTMSFNCCustomComponent, LCLTMSFNCTypes,
  Forms, Controls, LCLTMSFNCJSONWriter, LCLTMSFNCPersistence
  {$IFDEF FMXLIB}
  ,FMX.Types
  {$ENDIF}
  ,TypInfo
  ;

type
  TTMSFNCCustomStateManager = class;

  TTMSFNCStateManagerItem = class(TCollectionItem)
  private
    FOwner: TTMSFNCCustomStateManager;
    FDefault: Boolean;
    FName: string;
    FContent: string;
    FDataPointer: Pointer;
    FDataBoolean: Boolean;
    FDataString: String;
    FDataObject: TObject;
    FDataInteger: NativeInt;
    procedure SetDefault(const Value: Boolean);
    function GetName: string;
  protected
    function GetDisplayName: string; override;
    function Manager: TTMSFNCCustomStateManager;
  public
    constructor Create(ACollection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
    destructor Destroy; override;
    procedure Load;

    property DataPointer: Pointer read FDataPointer write FDataPointer;
    property DataBoolean: Boolean read FDataBoolean write FDataBoolean;
    property DataObject: TObject read FDataObject write FDataObject;
    property DataString: String read FDataString write FDataString;
    property DataInteger: NativeInt read FDataInteger write FDataInteger;

  published
    property Name: string read GetName write FName;
    property Content: string read FContent write FContent;
    property &Default: Boolean read FDefault write SetDefault default False;
  end;

  {$IFDEF WEBLIB}
  TTMSFNCStateManagerItems = class(TTMSFNCOwnedCollection, ITMSFNCBaseListIO, ITMSFNCBasePersistenceIO)
  {$ENDIF}
  {$IFNDEF WEBLIB}
  TTMSFNCStateManagerItems = class({$IFDEF LCLLIB}specialize {$ENDIF}TTMSFNCOwnedCollection<TTMSFNCStateManagerItem>, ITMSFNCBaseListIO, ITMSFNCBasePersistenceIO)
  {$ENDIF}
  private
    FOwnerInterface: IInterface;
    FOwner: TTMSFNCCustomStateManager;
    function GetItemEx(Index: Integer): TTMSFNCStateManagerItem;
    procedure SetItemEx(Index: Integer; const Value: TTMSFNCStateManagerItem);
  protected
    function GetStateManagerStateClass: TCollectionItemClass; virtual;
    function ITMSFNCBaseListIO.GetItemClass = GetInterfaceItemClass;
    function CreateObject(const AClassName: string; const ABaseClass: TClass): TObject; virtual;
    function GetInterfaceItemClass: TClass; virtual;
    {$IFDEF LCLLIB}
    function _AddRef : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _Release : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    {$ELSE}
    function _AddRef: Integer; {$IFNDEF WEBLIB}stdcall;{$ENDIF}
    function _Release: Integer; {$IFNDEF WEBLIB}stdcall;{$ENDIF}
    {$ENDIF}
  public
    {$IFDEF LCLLIB}
    function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid : tguid;out obj) : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    {$ELSE}
    function QueryInterface(const IID: TGUID; out Obj): HResult; virtual; {$IFNDEF WEBLIB}stdcall;{$ENDIF}
    {$ENDIF}
    constructor Create(AOwner: TTMSFNCCustomStateManager);
    function Add: TTMSFNCStateManagerItem;{$IFDEF WEBLIB}reintroduce;{$ENDIF}
    function Insert(index: Integer): TTMSFNCStateManagerItem;{$IFDEF WEBLIB}reintroduce;{$ENDIF}
    property Items[Index: Integer]: TTMSFNCStateManagerItem read GetItemEx write SetItemEx; default;
  end;

  {$IFDEF FMXLIB}
  TTMSFNCStateManagerControl = TFmxObject;
  {$ELSE}
  TTMSFNCStateManagerControl = TControl;
  {$ENDIF}

  TTMSFNCStateManagerLoadStateCustomCallback = {$IFNDEF LCLLIB}reference to {$ENDIF}procedure(AState: TTMSFNCStateManagerItem; var ALoad: Boolean){$IFDEF LCLLIB} of object{$ENDIF};
  TTMSFNCStateManagerLoadStateCustomEvent = procedure(Sender: TObject; AState: TTMSFNCStateManagerItem; var ALoad: Boolean) of object;

  TTMSFNCStateManagerBeforeLoadStateEvent = procedure(Sender: TObject; AState: TTMSFNCStateManagerItem; var ACanLoad: Boolean) of object;
  TTMSFNCStateManagerAfterLoadStateEvent = procedure(Sender: TObject; AState: TTMSFNCStateManagerItem) of object;

  TTMSFNCStateManagerBeforeLoadControlStateEvent = procedure(Sender: TObject; AState: TTMSFNCStateManagerItem; AControl: TTMSFNCStateManagerControl; var AValue: string; var ACanLoad: Boolean) of object;
  TTMSFNCStateManagerAfterLoadControlStateEvent = procedure(Sender: TObject; AState: TTMSFNCStateManagerItem; AControl: TTMSFNCStateManagerControl; AValue: string) of object;

  TTMSFNCCustomStateManager = class(TTMSFNCCustomComponent)
  private
    FUpdateCount: Integer;
    FActiveState: Integer;
    FStates: TTMSFNCStateManagerItems;
    FControl: TTMSFNCStateManagerControl;
    FOnLoadStateCustom: TTMSFNCStateManagerLoadStateCustomEvent;
    FAutoSave: Boolean;
    FOnAfterLoadState: TTMSFNCStateManagerAfterLoadStateEvent;
    FOnBeforeLoadState: TTMSFNCStateManagerBeforeLoadStateEvent;
    FOnAfterLoadControlState: TTMSFNCStateManagerAfterLoadControlStateEvent;
    FOnBeforeLoadControlState: TTMSFNCStateManagerBeforeLoadControlStateEvent;
    procedure SetStates(const Value: TTMSFNCStateManagerItems);
    procedure SetActiveState(const Value: Integer);
    procedure SetControl(const Value: TTMSFNCStateManagerControl);
  protected
    function GetInstance: NativeUInt; override;
    function GenerateContent: string;
    function CanPersist(AControl: TTMSFNCStateManagerControl): Boolean;
    function GetControlCount(AControl: TTMSFNCStateManagerControl): Integer;
    function GetControls(AControl: TTMSFNCStateManagerControl; AIndex: Integer): TTMSFNCStateManagerControl;
    function CreateStatesCollection: TTMSFNCStateManagerItems; virtual;
    function FindControlByName(AName: string; ARootControl: TTMSFNCStateManagerControl = nil): TTMSFNCStateManagerControl;

    procedure ResetState; virtual;
    procedure BeforeAssignControl; virtual;
    procedure AfterAssignControl; virtual;
    procedure DoBeforeLoadState(AState: TTMSFNCStateManagerItem; var ACanLoad: Boolean); virtual;
    procedure DoAfterLoadState(AState: TTMSFNCStateManagerItem); virtual;
    procedure DoBeforeLoadControlState(AState: TTMSFNCStateManagerItem; AControl: TTMSFNCStateManagerControl; var AValue: string; var ACanLoad: Boolean); virtual;
    procedure DoAfterLoadControlState(AState: TTMSFNCStateManagerItem; AControl: TTMSFNCStateManagerControl; AValue: string); virtual;
    procedure BeforeLoadState(AState: TTMSFNCStateManagerItem); virtual;
    procedure InternalLoadState(AState: TTMSFNCStateManagerItem); virtual;
    procedure InternalSaveToState(AState: TTMSFNCStateManagerItem; ANew: Boolean); virtual;
    procedure InternalSetActiveState(AState: TTMSFNCStateManagerItem); virtual;

    procedure DoCanWrite(AObject: TObject; APropertyName: string; APropertyKind: TTypeKind; AWriter: TTMSFNCJSONWriter; var ACanWrite: Boolean);
    property States: TTMSFNCStateManagerItems read FStates write SetStates;
    property Control: TTMSFNCStateManagerControl read FControl write SetControl;
    property OnLoadStateCustom: TTMSFNCStateManagerLoadStateCustomEvent read FOnLoadStateCustom write FOnLoadStateCustom;
    property OnBeforeLoadState: TTMSFNCStateManagerBeforeLoadStateEvent read FOnBeforeLoadState write FOnBeforeLoadState;
    property OnAfterLoadState: TTMSFNCStateManagerAfterLoadStateEvent read FOnAfterLoadState write FOnAfterLoadState;
    property OnBeforeLoadControlState: TTMSFNCStateManagerBeforeLoadControlStateEvent read FOnBeforeLoadControlState write FOnBeforeLoadControlState;
    property OnAfterLoadControlState: TTMSFNCStateManagerAfterLoadControlStateEvent read FOnAfterLoadControlState write FOnAfterLoadControlState;
    property ActiveState: Integer read FActiveState write SetActiveState default -1;
    property AutoSave: Boolean read FAutoSave write FAutoSave default True;
  public
    constructor Create; reintroduce; overload; virtual;
    constructor Create(AOwner: TComponent); overload; override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure BeginUpdate; override;
    procedure EndUpdate; override;
    procedure Loaded; override;

    procedure SaveToState(AState: TTMSFNCStateManagerItem); overload; virtual;
    procedure SaveToState(AIndex: Integer); overload; virtual;
    procedure SaveToState(AName: string); overload; virtual;
    procedure LoadStateByName(AName: string); overload; virtual;
    procedure LoadStateByIndex(AIndex: Integer); overload; virtual;
    procedure LoadStateCustom(ACallBack: TTMSFNCStateManagerLoadStateCustomCallback = nil);
    procedure Optimize; virtual;

    function FindConflicts(AConflictedControlNames: TStrings): Boolean; virtual;

    function GetDefaultState: TTMSFNCStateManagerItem; virtual;

    function FindStateByName(AName: string): TTMSFNCStateManagerItem; virtual;
  end;

implementation

uses
  LCLTMSFNCGraphics, SysUtils, Math, Types, LCLTMSFNCUtils,
  LCLTMSFNCCustomControl, LCLTMSFNCGraphicsTypes, Graphics
  {$IFDEF WEBLIB}
  ,WEBLib.JSON, Generics.Collections
  {$ENDIF}
  {$IFDEF FMXLIB}
  ,JSON, Generics.Collections
  {$ENDIF}
  {$IFDEF VCLLIB}
  ,JSON, Generics.Collections
  {$ENDIF}
  {$IFDEF LCLLIB}
  ,fpjson, fgl
  {$ENDIF}
  ;

//{$R 'TMSFNCState.res'}

type
  TTMSFNCStateManagerRootJSONValue = class
  private
    FState: TTMSFNCStateManagerItem;
    FValue: TJSONValue;
  public
    constructor Create(AState: TTMSFNCStateManagerItem; AValue: TJSONValue);
    destructor Destroy; override;
  end;

  TTMSFNCStateManagerJSONValue = class;

  TTMSFNCStateManagerJSONValueList = specialize TFPGObjectList<TTMSFNCStateManagerJSONValue>;

  TTMSFNCStateManagerJSONValueArray = array of TJSONValue;

  TTMSFNCStateManagerJSONValue = class
  private
    FValue: TJSONValue;
    FParentValue: TJSONValue;
    FValueName: string;
  public
    constructor Create(AParentValue, AValue: TJSONValue; AValueName: string);
    destructor Destroy; override;
  end;

  TTMSFNCStateManagerRootJSONValueList = specialize TFPGObjectList<TTMSFNCStateManagerRootJSONValue>;

  {$IFDEF FMXLIB}
  TCustomFormHelper = class helper for TCustomForm
  private
    function GetControlCount: Integer;
    function GetControls(AIndex: Integer): TTMSFNCStateManagerControl;
  public
    property ControlCount: Integer read GetControlCount;
    property Controls[Index: Integer]: TTMSFNCStateManagerControl read GetControls;
  end;
  {$ENDIF}

  {$IFDEF VCLLIB}
  TControlOpen = class(TWinControl);
  {$ENDIF}

{$IFDEF FMXLIB}

{ TCustomFormHelper }

function TCustomFormHelper.GetControls(AIndex: Integer): TTMSFNCStateManagerControl;
var
  c: TComponent;
begin
  Result := nil;
  c := Components[AIndex];
  if c is TTMSFNCStateManagerControl then
    Result := c as TTMSFNCStateManagerControl;
end;

function TCustomFormHelper.GetControlCount: Integer;
begin
  Result := ComponentCount;
end;

{$ENDIF}

{ TTMSFNCCustomStateManager }

procedure TTMSFNCCustomStateManager.AfterAssignControl;
begin

end;

procedure TTMSFNCCustomStateManager.Assign(Source: TPersistent);
begin
  if Source is TTMSFNCCustomStateManager then
  begin
    FStates.Assign((Source as TTMSFNCCustomStateManager).States);
    FControl := (Source as TTMSFNCCustomStateManager).Control;
    FAutoSave := (Source as TTMSFNCCustomStateManager).AutoSave;
    FActiveState := (Source as TTMSFNCCustomStateManager).ActiveState;
  end;
end;

procedure TTMSFNCCustomStateManager.BeforeAssignControl;
begin

end;

procedure TTMSFNCCustomStateManager.BeforeLoadState(
  AState: TTMSFNCStateManagerItem);
begin

end;

procedure TTMSFNCCustomStateManager.BeginUpdate;
begin
  inherited;
  Inc(FUpdateCount);
end;

constructor TTMSFNCCustomStateManager.Create;
begin
  Create(nil);
end;

procedure TTMSFNCCustomStateManager.DoAfterLoadControlState(
  AState: TTMSFNCStateManagerItem; AControl: TTMSFNCStateManagerControl;
  AValue: string);
begin
  if Assigned(OnAfterLoadControlState) then
    OnAfterLoadControlState(Self, AState, AControl, AValue);
end;

procedure TTMSFNCCustomStateManager.DoBeforeLoadControlState(
  AState: TTMSFNCStateManagerItem; AControl: TTMSFNCStateManagerControl;
  var AValue: string; var ACanLoad: Boolean);
begin
  if Assigned(OnBeforeLoadControlState) then
    OnBeforeLoadControlState(Self, AState, AControl, AValue, ACanLoad);
end;

procedure TTMSFNCCustomStateManager.DoAfterLoadState(
  AState: TTMSFNCStateManagerItem);
begin
  if Assigned(OnAfterLoadState) then
    OnAfterLoadState(Self, AState);
end;

procedure TTMSFNCCustomStateManager.DoBeforeLoadState(
  AState: TTMSFNCStateManagerItem; var ACanLoad: Boolean);
begin
  if Assigned(OnBeforeLoadState) then
    OnBeforeLoadState(Self, AState, ACanLoad);
end;

procedure TTMSFNCCustomStateManager.DoCanWrite(AObject: TObject;
  APropertyName: string; APropertyKind: TTypeKind; AWriter: TTMSFNCJSONWriter;
  var ACanWrite: Boolean);
begin
  ACanWrite := not (APropertyKind in [tkClassRef, tkPointer{$IFNDEF LCLLIB}, tkProcedure{$ENDIF}, tkMethod]);
end;

function TTMSFNCCustomStateManager.CanPersist(
  AControl: TTMSFNCStateManagerControl): Boolean;
begin
  Result := {$IFDEF FMXLIB}not (AControl is TStyleBook) and{$ENDIF}
            not (AControl is TTMSFNCCustomComponent);
end;

constructor TTMSFNCCustomStateManager.Create(AOwner: TComponent);
begin
  inherited;

  FStates := CreateStatesCollection;
  FActiveState := -1;
  FAutoSave := True;

  if Assigned(AOwner) and (AOwner is TTMSFNCStateManagerControl) and IsDesignTime then
  begin
    if AOwner is TTMSFNCStateManagerControl then
      Control := AOwner as TTMSFNCStateManagerControl;
  end;
end;

function TTMSFNCCustomStateManager.CreateStatesCollection: TTMSFNCStateManagerItems;
begin
  Result := TTMSFNCStateManagerItems.Create(Self);
end;

destructor TTMSFNCCustomStateManager.Destroy;
begin
  FStates.Free;
  inherited;
end;

procedure TTMSFNCCustomStateManager.EndUpdate;
begin
  inherited;
  Dec(FUpdateCount);
//  if FUpdateCount = 0 then
//    DoSomething;
end;

function TTMSFNCCustomStateManager.FindStateByName(
  AName: string): TTMSFNCStateManagerItem;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to States.Count - 1 do
  begin
    if UpperCase(States[I].Name) = UpperCase(AName) then
    begin
      Result := States[I];
      Break;
    end;
  end;
end;

function TTMSFNCCustomStateManager.FindConflicts(
  AConflictedControlNames: TStrings): Boolean;
var
  s: string;
  v, jv, jvv: TJSONValue;
  I, M: Integer;
  snm, nm: string;
  a: TJSONArray;
  c: TTMSFNCStateManagerControl;
  st: TTMSFNCStateManagerItem;
begin
  Result := False;
  for M := 0 to States.Count - 1 do
  begin
    st := States[M];
    s := st.Content;
    v := TTMSFNCUtils.ParseJSON(s);
    if Assigned(v) then
    begin
      try
        jv := TTMSFNCUtils.FindJSONValue(v, 'components');
        if Assigned(jv) and (jv is TJSONArray) then
        begin
          a := jv as TJSONArray;
          for I := 0 to TTMSFNCUtils.GetJSONArraySize(a) - 1 do
          begin
            jvv := TTMSFNCUtils.GetJSONArrayItem(a, I);
            if Assigned(jvv) and (jvv is TJSONObject) then
            begin
              nm := TTMSFNCUtils.GetJSONObjectName(jvv as TJSONObject, 0);
              c := FindControlByName(nm);
              if not Assigned(c) then
              begin
                Result := True;
                snm := st.Name;
                if AConflictedControlNames.Values[snm] = '' then
                  AConflictedControlNames.Values[snm] := nm
                else
                  AConflictedControlNames.Values[snm] := AConflictedControlNames.Values[snm] + ',' + nm;
              end;
            end;
          end;
        end;
      finally
        v.Free;
      end;
    end;
  end;
end;

function TTMSFNCCustomStateManager.FindControlByName(
  AName: string; ARootControl: TTMSFNCStateManagerControl = nil): TTMSFNCStateManagerControl;
var
  c: TTMSFNCStateManagerControl;

  function FindControl(AControl: TTMSFNCStateManagerControl): TTMSFNCStateManagerControl;
  var
    cn: string;
    ct: TComponent;
    I: Integer;
  begin
    Result := nil;
    if UpperCase(AControl.Name) = UpperCase(AName) then
    begin
      Result := AControl;
      Exit;
    end;


    for I := 0 to GetControlCount(AControl) - 1 do
    begin
      ct := GetControls(AControl, I);
      if Assigned(ct) and (ct is TTMSFNCStateManagerControl) and CanPersist(ct as TTMSFNCStateManagerControl) then
      begin
        cn := ct.Name;
        if UpperCase(cn) = UpperCase(AName) then
        begin
          Result := ct as TTMSFNCStateManagerControl;
          Break;
        end
        else if not Assigned(Result) then
        begin
          {$IFDEF FMXLIB}
          if not (AControl is TCustomForm) then
          {$ENDIF}
            Result := FindControl(ct as TTMSFNCStateManagerControl);
        end;
      end;
    end;
  end;
begin
  Result := nil;
  if Assigned(ARootControl) then
    c := ARootControl
  else
    c := Control;

  if not Assigned(c) then
    Exit;

  Result := FindControl(c);
end;

function TTMSFNCCustomStateManager.GenerateContent: string;
var
  cw: TTMSFNCWriterCustomWritePropertyEvent;
  bl: Boolean;
  sl: TStringList;
  K: Integer;

  procedure AddControls(AControl: TTMSFNCStateManagerControl);
  var
    s: string;
    cn, pn: string;
    ct: TComponent;
    I: Integer;
  begin
    for I := 0 to GetControlCount(AControl) - 1 do
    begin
      ct := GetControls(AControl, I);
      if Assigned(ct) and (ct is TTMSFNCStateManagerControl) then
      begin
        if CanPersist(ct as TTMSFNCStateManagerControl)  then
        begin
          s := ct.ToJSON;
          cn := ct.Name;
          if cn <> '' then
          begin
            pn := '';
            if Assigned((ct as TTMSFNCStateManagerControl).Parent) then
              pn := (ct as TTMSFNCStateManagerControl).Parent.Name;

            sl.Add('{"' + cn + '":{"content":' + s + ', "parent":"' + pn + '"}}');
          end;
        end;

        {$IFDEF FMXLIB}
        if not (AControl is TCustomForm) then
        {$ENDIF}
          AddControls(ct as TTMSFNCStateManagerControl);
      end;
    end;
  end;
begin
  Result := '';
  if not Assigned(Control) then
    Exit;

  cw := TTMSFNCPersistence.OnCustomWriteProperty;
  bl := TTMSFNCCustomControl.BlockPersistenceInterface;
  TTMSFNCPersistence.OnCustomWriteProperty := @DoCanWrite;
  TTMSFNCCustomControl.BlockPersistenceInterface := True;
  sl := TStringList.Create;
  try
    sl.Delimiter := ',';
    sl.StrictDelimiter := True;
    sl.LineBreak := '';
    Result := '{"components":[';

    AddControls(Control);

    for K := 0 to sl.Count - 1 do
    begin
      Result := Result + sl[K];
      if K < sl.Count - 1 then
        Result := Result + ','
    end;

    Result := Result + ']}';
  finally
    sl.Free;
    TTMSFNCPersistence.OnCustomWriteProperty := cw;
    TTMSFNCCustomControl.BlockPersistenceInterface := bl;
  end;
end;

function TTMSFNCCustomStateManager.GetDefaultState: TTMSFNCStateManagerItem;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to States.Count - 1 do
  begin
    if States[I].Default then
    begin
      Result := States[I];
      Break;
    end;
  end;
end;

function TTMSFNCCustomStateManager.GetInstance: NativeUInt;
begin
  Result := HInstance;
end;

function TTMSFNCCustomStateManager.GetControls(
  AControl: TTMSFNCStateManagerControl;
  AIndex: Integer): TTMSFNCStateManagerControl;
begin
  Result := nil;
  {$IFDEF FMXLIB}
  if AControl is TControl then
    Result := (AControl as TControl).Controls[AIndex]
  {$ELSE}
  if AControl is TWinControl then
    Result := (AControl as TWinControl).Controls[AIndex]
  {$ENDIF}
  else if AControl is TCustomForm then
    Result := (AControl as TCustomForm).Controls[AIndex];
end;

function TTMSFNCCustomStateManager.GetControlCount(
  AControl: TTMSFNCStateManagerControl): Integer;
begin
  Result := 0;
  {$IFDEF FMXLIB}
  if AControl is TControl then
    Result := (AControl as TControl).ControlCount
  {$ELSE}
  if AControl is TWinControl then
    Result := (AControl as TWinControl).ControlCount
  {$ENDIF}
  else if AControl is TCustomForm then
    Result := (AControl as TCustomForm).ControlCount;
end;

procedure TTMSFNCCustomStateManager.LoadStateByName(AName: string);
begin
  InternalLoadState(FindStateByName(AName));
end;

procedure TTMSFNCCustomStateManager.LoadStateCustom(
  ACallBack: TTMSFNCStateManagerLoadStateCustomCallback = nil);
var
  I: Integer;
  s: TTMSFNCStateManagerItem;
  b: Boolean;
begin
  for I := 0 to States.Count - 1 do
  begin
    s := States[I];
    b := False;
    if Assigned(ACallBack) then
      ACallBack(s, b)
    else if Assigned(OnLoadStateCustom) then
      OnLoadStateCustom(Self, s, b);

    if b then
    begin
      s.Load;
      Break;
    end;
  end;
end;

procedure TTMSFNCCustomStateManager.Loaded;
begin
  inherited;
  if not IsDesignTime then
    Optimize;
end;

procedure TTMSFNCCustomStateManager.LoadStateByIndex(AIndex: Integer);
begin
  if (AIndex >= 0) and (AIndex <= States.Count - 1) then
    InternalLoadState(States[AIndex]);
end;

procedure TTMSFNCCustomStateManager.InternalSaveToState(
  AState: TTMSFNCStateManagerItem; ANew: Boolean);
begin
  if Assigned(AState) then
  begin
    AState.Content := GenerateContent;
    if ANew then
      InternalSetActiveState(AState);
  end;
end;

procedure TTMSFNCCustomStateManager.InternalSetActiveState(
  AState: TTMSFNCStateManagerItem);
begin
  if Assigned(AState) then
    FActiveState := AState.Index;
end;

procedure TTMSFNCCustomStateManager.InternalLoadState(
  AState: TTMSFNCStateManagerItem);
var
  s: string;
  v, jv, jvv, jvvo, ct: TJSONValue;
  I: Integer;
  nm: string;
  a: TJSONArray;
  c, p: TTMSFNCStateManagerControl;
  bl, cl: Boolean;
  pn: string;
begin
  if Assigned(AState) and (AState.Index <> FActiveState) then
  begin
    if IsDesignTime then
    begin
      if AutoSave then
        SaveToState(ActiveState);
    end;

    cl := True;
    DoBeforeLoadState(AState, cl);
    if cl then
    begin
      InternalSetActiveState(AState);

      s := AState.Content;
      v := TTMSFNCUtils.ParseJSON(s);
      if Assigned(v) then
      begin
        bl := TTMSFNCCustomControl.BlockPersistenceInterface;
        TTMSFNCCustomControl.BlockPersistenceInterface := True;
        try
          jv := TTMSFNCUtils.FindJSONValue(v, 'components');
          if Assigned(jv) and (jv is TJSONArray) then
          begin
            {$IFDEF FMXLIB}
            if Assigned(Control) then
            begin
              if (Control is TControl) then
                (Control as TControl).BeginUpdate
              else if Control is TCustomForm then
                (Control as TCustomForm).BeginUpdate;
            end;
            {$ENDIF}

            {$IFDEF VCLLIB}
            if Assigned(Control) and (Control is TWinControl) then
              TControlOpen(Control).DisableAlign;
            {$ENDIF}

            if Assigned(Control) and IsDesignTime then
              BeforeLoadState(AState);

            try
              a := jv as TJSONArray;
              for I := 0 to TTMSFNCUtils.GetJSONArraySize(a) - 1 do
              begin
                jvv := TTMSFNCUtils.GetJSONArrayItem(a, I);
                if Assigned(jvv) and (jvv is TJSONObject) then
                begin
                  nm := TTMSFNCUtils.GetJSONObjectName(jvv as TJSONObject, 0);
                  c := FindControlByName(nm);
                  if Assigned(c) then
                  begin
                    jvvo := TTMSFNCUtils.GetJSONValue(jvv, nm);
                    if Assigned(jvvo) then
                    begin
                      ct := TTMSFNCUtils.GetJSONValue(jvvo, 'content');
                      pn := TTMSFNCUtils.GetJSONProp(jvvo, 'parent');
                      cl := True;
                      s := TTMSFNCUtils.GetJSONValueAsString(ct);
                      DoBeforeLoadControlState(AState, c, s, cl);
                      if cl then
                      begin
                        if Assigned(ct) and (s <> '') then
                        begin
                          {$IFDEF VCLLIB}
                          //prototype
                          if not IsDesigntime then
                            TTMSFNCUtils.ScaleForDPI(c, DesigntimeFormPixelsPerInch);
                          {$ENDIF}
                          c.FromJSON(s);
                          {$IFDEF VCLLIB}
                          //prototype
                          if not IsDesigntime then
                            TTMSFNCUtils.ScaleForDPI(c, Round(DesigntimeFormPixelsPerInch * TTMSFNCUtils.GetDPIScale(Self, DesigntimeFormPixelsPerInch)));
                          {$ENDIF}
                        end;

                        if pn <> '' then
                        begin
                          p := FindControlByName(pn);
                          if Assigned(p) then
                          begin
                            {$IFNDEF FMXLIB}
                            if p is TWinControl then
                            {$ENDIF}
                              c.Parent := p{$IFNDEF FMXLIB} as TWinControl{$ENDIF};
                          end;
                        end;

                        DoAfterLoadControlState(AState, c, s);
                      end;
                    end;
                  end;
                end;
              end;
            finally
              {$IFDEF FMXLIB}
              if Assigned(Control) then
              begin
                if (Control is TControl) then
                  (Control as TControl).EndUpdate
                else if Control is TCustomForm then
                  (Control as TCustomForm).EndUpdate;
              end;
              {$ENDIF}
              {$IFDEF VCLLIB}
              if Assigned(Control) and (Control is TWinControl) then
                TControlOpen(Control).EnableAlign;
              {$ENDIF}
            end;
          end;
        finally
          v.Free;
          TTMSFNCCustomControl.BlockPersistenceInterface := bl;
        end;
      end;

      DoAfterLoadState(AState);
    end;
  end;
end;

procedure TTMSFNCCustomStateManager.Optimize;
var
  L: Integer;
  ov, jvv, jov, jovsub: TJSONValue;
  ja: TJSONArray;
  o: TTMSFNCStateManagerRootJSONValue;
  oarr: TTMSFNCStateManagerRootJSONValueList;
  arr: TTMSFNCStateManagerJSONValueList;
  I: Integer;
  nm: string;
  M: Integer;

  function CompareValues(va: TTMSFNCStateManagerJSONValueArray): Boolean;
  var
    K: Integer;
    v: string;
    s: string;
  begin
    Result := False;
    if Length(va) > 0 then
    begin
      v := TTMSFNCUtils.GetJSONValueAsString(va[0]);
      s := v;
      for K := 1 to Length(va) - 1 do
      begin
        s := s + ', ' + TTMSFNCUtils.GetJSONValueAsString(va[K]);
        if TTMSFNCUtils.GetJSONValueAsString(va[K]) <> v then
        begin
          Result := True;
          Break;
        end;
      end;
    end;
  end;

  procedure CompareObjects(oa: TTMSFNCStateManagerJSONValueList; ARoot: Boolean);
  var
    J, {P, }K: Integer;
    jv{, a}: TJSONValue;
    v: TJSONValue;
    sz: Integer;
    oasub: TTMSFNCStateManagerJSONValueList;
    vasub: TTMSFNCStateManagerJSONValueArray;
    n: string;
    {$IFNDEF LCLLIB}
    pr: TJSONPair;
    {$ENDIF}
    sl: TStringList;
  begin
    if oa.Count > 0 then
    begin
      v := oa[0].FValue;
      if v is TJSONObject then
      begin
        sz := TTMSFNCUtils.GetJSONObjectSize(v as TJSONObject);
        sl := TStringList.Create;
        try
          for k := 0 to sz - 1 do
          begin
            n := TTMSFNCUtils.GetJSONObjectName(v as TJSONObject, k);
            sl.Add(n);
          end;

          for k := 0 to sl.Count - 1 do
          begin
            jv := TTMSFNCUtils.GetJSONValue(v, sl[k]);
            n := sl[k];

            oasub := TTMSFNCStateManagerJSONValueList.Create;
            try
              oasub.Add(TTMSFNCStateManagerJSONValue.Create(v, jv, n));
              for J := 1 to oa.Count - 1 do
                oasub.Add(TTMSFNCStateManagerJSONValue.Create(oa[J].FValue, TTMSFNCUtils.GetJSONValue(oa[J].FValue, n), n));

              CompareObjects(oasub, False);

            finally
              oasub.Free;
            end;
          end
        finally
          sl.Free;
        end;

        if not ARoot then
        begin
          for J := 0 to oa.Count - 1 do
          begin
            if TTMSFNCUtils.GetJSONValueAsString(oa[J].FValue) = '{}' then
            begin
              if oa[J].FParentValue is TJSONObject then
              begin
                {$IFDEF LCLLIB}
                (oa[J].FParentValue as TJSONObject).Delete(oa[J].FValueName);
                {$ELSE}
                pr := (oa[J].FParentValue as TJSONObject).RemovePair(oa[J].FValueName);
                if Assigned(pr) then
                  pr.Free;
                {$ENDIF}
              end;
            end;
          end;
        end;
      end
//      else if v is TJSONArray then
//      begin
//        for P := 0 to TTMSFNCUtils.GetJSONArraySize((v as TJSONArray)) - 1 do
//        begin
//          a := TTMSFNCUtils.GetJSONArrayItem((v as TJSONArray), P);
//
//          oasub := TTMSFNCStateManagerJSONValueList.Create;
//          try
//            oasub.Add(TTMSFNCStateManagerJSONValue.Create(v, a, ''));
//            for J := 1 to oa.Count - 1 do
//              oasub.Add(TTMSFNCStateManagerJSONValue.Create(oa[J].FValue, TTMSFNCUtils.GetJSONArrayItem((oa[J].FValue as TJSONArray), P), ''));
//
//            CompareObjects(oasub, False);
//
//          finally
//            oasub.Free;
//          end;
//        end;
//      end
      else if (v is TJSONValue) and not ARoot then
      begin
        SetLength(vasub, oa.Count);
        vasub[0] := v;
        for J := 1 to oa.Count - 1 do
          vasub[J] := oa[J].FValue;

        if not CompareValues(vasub) then
        begin
          for J := 0 to oa.Count - 1 do
          begin
            if oa[J].FParentValue is TJSONObject then
            begin
              {$IFDEF LCLLIB}
              (oa[J].FParentValue as TJSONObject).Delete(oa[J].FValueName);
              {$ELSE}
              pr := (oa[J].FParentValue as TJSONObject).RemovePair(oa[J].FValueName);
              if Assigned(pr) then
                pr.Free;
              {$ENDIF}
            end;
          end;
        end;
      end;
    end;
  end;

begin
  oarr := TTMSFNCStateManagerRootJSONValueList.Create;
  try
    for L := 0 to States.Count - 1 do
    begin
      ov := TTMSFNCUtils.ParseJSON(States[L].Content);
      if Assigned(ov) then
      begin
        o := TTMSFNCStateManagerRootJSONValue.Create(States[L], ov);
        oarr.Add(o);
      end;
    end;

    if oarr.Count > 0 then
    begin
      jov := TTMSFNCUtils.FindJSONValue(oarr[0].FValue, 'components');
      if Assigned(jov) and (jov is TJSONArray) then
      begin
        ja := jov as TJSONArray;
        for I := 0 to TTMSFNCUtils.GetJSONArraySize(ja) - 1 do
        begin
          jvv := TTMSFNCUtils.GetJSONArrayItem(ja, I);
          if Assigned(jvv) and (jvv is TJSONObject) then
          begin
            nm := TTMSFNCUtils.GetJSONObjectName(jvv as TJSONObject, 0);
            arr := TTMSFNCStateManagerJSONValueList.Create;
            try
              arr.Add(TTMSFNCStateManagerJSONValue.Create(jov, jvv, nm));
              for M := 1 to oarr.Count - 1 do
              begin
                jovsub := TTMSFNCUtils.FindJSONValue(oarr[M].FValue, 'components');
                if Assigned(jovsub) and (jovsub is TJSONArray) then
                  arr.Add(TTMSFNCStateManagerJSONValue.Create(jovsub, TTMSFNCUtils.GetJSONArrayItem(jovsub as TJSONArray, I), nm));
              end;

              CompareObjects(arr, True);

            finally
              arr.Free;
            end;
          end;
        end;
      end;

      for M := 0 to oarr.Count - 1 do
      begin
        if Assigned(oarr[M].FState) then
          oarr[M].FState.Content := TTMSFNCUtils.GetJSONValueAsString(oarr[M].FValue);
      end;
    end;
  finally
    oarr.Free;
  end;
end;

procedure TTMSFNCCustomStateManager.ResetState;
begin
  FActiveState := -1;
end;

procedure TTMSFNCCustomStateManager.SaveToState(
  AState: TTMSFNCStateManagerItem);
begin
  InternalSaveToState(AState, False)
end;

procedure TTMSFNCCustomStateManager.SaveToState(AIndex: Integer);
begin
  if (AIndex >= 0) and (AIndex <= States.Count - 1) then
    SaveToState(States[AIndex]);
end;

procedure TTMSFNCCustomStateManager.SaveToState(AName: string);
begin
  SaveToState(FindStateByName(AName));
end;

procedure TTMSFNCCustomStateManager.SetActiveState(const Value: Integer);
begin
  if FActiveState <> Value then
  begin
    LoadStateByIndex(Value);
    FActiveState := Value;
  end;
end;

procedure TTMSFNCCustomStateManager.SetControl(
  const Value: TTMSFNCStateManagerControl);
begin
  if FControl <> Value then
  begin
    BeforeAssignControl;
    FControl := Value;
    AfterAssignControl;
  end;
end;

procedure TTMSFNCCustomStateManager.SetStates(
  const Value: TTMSFNCStateManagerItems);
begin
  FStates.Assign(Value);
end;

{ TTMSFNCStateManagerItems }

{$IFDEF LCLLIB}
function TTMSFNCStateManagerItems.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid : tguid;out obj) : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
{$ELSE}
function TTMSFNCStateManagerItems.QueryInterface(const IID: TGUID; out Obj): HResult;
{$ENDIF}
const
  E_NOINTERFACE = HResult($80004002);
begin
  if GetInterface(IID, Obj) then Result := 0 else Result := E_NOINTERFACE;
end;

{$IFDEF LCLLIB}
function TTMSFNCStateManagerItems._AddRef: longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
{$ELSE}
function TTMSFNCStateManagerItems._AddRef: Integer;
{$ENDIF}
begin
  if FOwnerInterface <> nil then
    Result := FOwnerInterface._AddRef else
    Result := -1;
end;

{$IFDEF LCLLIB}
function TTMSFNCStateManagerItems._Release: longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
{$ELSE}
function TTMSFNCStateManagerItems._Release: Integer;
{$ENDIF}
begin
  if FOwnerInterface <> nil then
    Result := FOwnerInterface._Release else
    Result := -1;
end;

function TTMSFNCStateManagerItems.Add: TTMSFNCStateManagerItem;
begin
  Result := TTMSFNCStateManagerItem(inherited Add);
end;

constructor TTMSFNCStateManagerItems.Create(AOwner: TTMSFNCCustomStateManager);
begin
  inherited Create(AOwner, GetStateManagerStateClass);
  FOwner := AOwner;
end;

function TTMSFNCStateManagerItems.CreateObject(const AClassName: string;
  const ABaseClass: TClass): TObject;
begin
  Result := GetStateManagerStateClass.Create(nil);
end;

function TTMSFNCStateManagerItems.GetInterfaceItemClass: TClass;
begin
  Result := GetStateManagerStateClass;
end;

function TTMSFNCStateManagerItems.GetStateManagerStateClass: TCollectionItemClass;
begin
  Result := TTMSFNCStateManagerItem;
end;

function TTMSFNCStateManagerItems.GetItemEx(Index: Integer): TTMSFNCStateManagerItem;
begin
  Result := TTMSFNCStateManagerItem(inherited Items[Index]);
end;

function TTMSFNCStateManagerItems.Insert(index: Integer): TTMSFNCStateManagerItem;
begin
  Result := TTMSFNCStateManagerItem(inherited Insert(Index));
end;

procedure TTMSFNCStateManagerItems.SetItemEx(Index: Integer; const Value: TTMSFNCStateManagerItem);
begin
  inherited SetItem(Index, Value);
end;

{ TTMSFNCStateManagerItem }

procedure TTMSFNCStateManagerItem.Assign(Source: TPersistent);
begin
  if (Source is TTMSFNCStateManagerItem) then
  begin
    FName := (Source as TTMSFNCStateManagerItem).Name;
    FContent := (Source as TTMSFNCStateManagerItem).Content;
    FDefault := (Source as TTMSFNCStateManagerItem).Default;
  end;
end;

constructor TTMSFNCStateManagerItem.Create(ACollection: TCollection);
var
  m: TTMSFNCCustomStateManager;
begin
  inherited;

  if Assigned(ACollection) and Assigned((Collection as TTMSFNCStateManagerItems).FOwner) then
    FOwner := (Collection as TTMSFNCStateManagerItems).FOwner;

  m := Manager;
  FDefault := False;
  FContent := '';

  if Assigned(m) and m.IsDesignTime then
  begin
    if Collection.Count = 1 then
      FDefault := True;

    FName := 'State ' + IntToStr(Collection.Count);

    m.InternalSaveToState(Self, True);
  end;
end;

destructor TTMSFNCStateManagerItem.Destroy;
begin
  inherited;
end;

function TTMSFNCStateManagerItem.GetDisplayName: string;
begin
  if FName <> '' then
    Result := FName
  {$IFDEF WEBLIB}
  else
    Result := '';
  {$ENDIF}
  {$IFNDEF WEBLIB}
  else
    Result := inherited;
  {$ENDIF}
end;

function TTMSFNCStateManagerItem.GetName: string;
begin
  Result := FName;
  if Result = '' then
    Result := 'State ' + IntToStr(Index + 1);
end;

procedure TTMSFNCStateManagerItem.Load;
var
  m: TTMSFNCCustomStateManager;
begin
  m := Manager;
  if Assigned(m) then
    m.InternalLoadState(Self);
end;

function TTMSFNCStateManagerItem.Manager: TTMSFNCCustomStateManager;
begin
  Result := FOwner;
end;

procedure TTMSFNCStateManagerItem.SetDefault(const Value: Boolean);
var
  I: Integer;
  m: TTMSFNCCustomStateManager;
begin
  if FDefault <> Value then
  begin
    m := Manager;
    if Assigned(m) and not m.IsLoading then
    begin
      for I := 0 to m.States.Count - 1 do
        m.States[I].FDefault := False;
    end;

    FDefault := Value;
  end;
end;

{ TTMSFNCStateManagerRootJSONValue }

constructor TTMSFNCStateManagerRootJSONValue.Create(
  AState: TTMSFNCStateManagerItem; AValue: TJSONValue);
begin
  FState := AState;
  FValue := AValue;
end;

destructor TTMSFNCStateManagerRootJSONValue.Destroy;
begin
  FreeAndNil(FValue);
  FState := nil;
  inherited;
end;

constructor TTMSFNCStateManagerJSONValue.Create(AParentValue, AValue: TJSONValue; AValueName: string);
begin
  FValue := AValue;
  FParentValue := AParentValue;
  FValueName := AValueName;
end;

destructor TTMSFNCStateManagerJSONValue.Destroy;
begin
  FValue := nil;
  FParentValue := nil;
  FValueName := '';
  inherited;
end;

end.

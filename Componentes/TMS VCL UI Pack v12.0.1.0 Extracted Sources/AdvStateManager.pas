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

unit AdvStateManager;

{$I TMSDEFS.INC}

interface

uses
  Classes, AdvCustomComponent, AdvTypes,
  Forms, Controls, AdvJSONWriter, AdvPersistence
  {$IFDEF FMXLIB}
  ,FMX.Types
  {$ENDIF}
  ,TypInfo
  ;

type
  TAdvCustomStateManager = class;

  TAdvStateManagerItem = class(TCollectionItem)
  private
    FOwner: TAdvCustomStateManager;
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
    function Manager: TAdvCustomStateManager;
  public
    constructor Create(Collection: TCollection); override;
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
  TAdvStateManagerItems = class(TAdvOwnedCollection, IAdvBaseListIO, IAdvBasePersistenceIO)
  {$ENDIF}
  {$IFNDEF WEBLIB}
  TAdvStateManagerItems = class({$IFDEF LCLLIB}specialize {$ENDIF}TAdvOwnedCollection<TAdvStateManagerItem>, IAdvBaseListIO, IAdvBasePersistenceIO)
  {$ENDIF}
  private
    FOwnerInterface: IInterface;
    FOwner: TAdvCustomStateManager;
    function GetItemEx(Index: Integer): TAdvStateManagerItem;
    procedure SetItemEx(Index: Integer; const Value: TAdvStateManagerItem);
  protected
    function GetStateManagerStateClass: TCollectionItemClass; virtual;
    function IAdvBaseListIO.GetItemClass = GetInterfaceItemClass;
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
    constructor Create(AOwner: TAdvCustomStateManager);
    function Add: TAdvStateManagerItem;{$IFDEF WEBLIB}reintroduce;{$ENDIF}
    function Insert(index: Integer): TAdvStateManagerItem;{$IFDEF WEBLIB}reintroduce;{$ENDIF}
    property Items[Index: Integer]: TAdvStateManagerItem read GetItemEx write SetItemEx; default;
  end;

  {$IFDEF FMXLIB}
  TAdvStateManagerControl = TFmxObject;
  {$ELSE}
  TAdvStateManagerControl = TControl;
  {$ENDIF}

  TAdvStateManagerLoadStateCustomCallback = {$IFNDEF LCLLIB}reference to {$ENDIF}procedure(AState: TAdvStateManagerItem; var ALoad: Boolean){$IFDEF LCLLIB} of object{$ENDIF};
  TAdvStateManagerLoadStateCustomEvent = procedure(Sender: TObject; AState: TAdvStateManagerItem; var ALoad: Boolean) of object;

  TAdvStateManagerBeforeLoadStateEvent = procedure(Sender: TObject; AState: TAdvStateManagerItem; var ACanLoad: Boolean) of object;
  TAdvStateManagerAfterLoadStateEvent = procedure(Sender: TObject; AState: TAdvStateManagerItem) of object;

  TAdvStateManagerBeforeLoadControlStateEvent = procedure(Sender: TObject; AState: TAdvStateManagerItem; AControl: TAdvStateManagerControl; var AValue: string; var ACanLoad: Boolean) of object;
  TAdvStateManagerAfterLoadControlStateEvent = procedure(Sender: TObject; AState: TAdvStateManagerItem; AControl: TAdvStateManagerControl; AValue: string) of object;

  TAdvCustomStateManager = class(TAdvCustomComponent)
  private
    FUpdateCount: Integer;
    FActiveState: Integer;
    FStates: TAdvStateManagerItems;
    FControl: TAdvStateManagerControl;
    FOnLoadStateCustom: TAdvStateManagerLoadStateCustomEvent;
    FAutoSave: Boolean;
    FOnAfterLoadState: TAdvStateManagerAfterLoadStateEvent;
    FOnBeforeLoadState: TAdvStateManagerBeforeLoadStateEvent;
    FOnAfterLoadControlState: TAdvStateManagerAfterLoadControlStateEvent;
    FOnBeforeLoadControlState: TAdvStateManagerBeforeLoadControlStateEvent;
    procedure SetStates(const Value: TAdvStateManagerItems);
    procedure SetActiveState(const Value: Integer);
    procedure SetControl(const Value: TAdvStateManagerControl);
  protected
    function GetInstance: NativeUInt; override;
    function GenerateContent: string;
    function CanPersist(AControl: TAdvStateManagerControl): Boolean;
    function GetControlCount(AControl: TAdvStateManagerControl): Integer;
    function GetControls(AControl: TAdvStateManagerControl; AIndex: Integer): TAdvStateManagerControl;
    function CreateStatesCollection: TAdvStateManagerItems; virtual;
    function FindControlByName(AName: string; ARootControl: TAdvStateManagerControl = nil): TAdvStateManagerControl;

    procedure ResetState; virtual;
    procedure BeforeAssignControl; virtual;
    procedure AfterAssignControl; virtual;
    procedure DoBeforeLoadState(AState: TAdvStateManagerItem; var ACanLoad: Boolean); virtual;
    procedure DoAfterLoadState(AState: TAdvStateManagerItem); virtual;
    procedure DoBeforeLoadControlState(AState: TAdvStateManagerItem; AControl: TAdvStateManagerControl; var AValue: string; var ACanLoad: Boolean); virtual;
    procedure DoAfterLoadControlState(AState: TAdvStateManagerItem; AControl: TAdvStateManagerControl; AValue: string); virtual;
    procedure BeforeLoadState(AState: TAdvStateManagerItem); virtual;
    procedure InternalLoadState(AState: TAdvStateManagerItem); virtual;
    procedure InternalSaveToState(AState: TAdvStateManagerItem; ANew: Boolean); virtual;
    procedure InternalSetActiveState(AState: TAdvStateManagerItem); virtual;

    procedure DoCanWrite(AObject: TObject; APropertyName: string; APropertyKind: TTypeKind; AWriter: TAdvJSONWriter; var ACanWrite: Boolean);
    property States: TAdvStateManagerItems read FStates write SetStates;
    property Control: TAdvStateManagerControl read FControl write SetControl;
    property OnLoadStateCustom: TAdvStateManagerLoadStateCustomEvent read FOnLoadStateCustom write FOnLoadStateCustom;
    property OnBeforeLoadState: TAdvStateManagerBeforeLoadStateEvent read FOnBeforeLoadState write FOnBeforeLoadState;
    property OnAfterLoadState: TAdvStateManagerAfterLoadStateEvent read FOnAfterLoadState write FOnAfterLoadState;
    property OnBeforeLoadControlState: TAdvStateManagerBeforeLoadControlStateEvent read FOnBeforeLoadControlState write FOnBeforeLoadControlState;
    property OnAfterLoadControlState: TAdvStateManagerAfterLoadControlStateEvent read FOnAfterLoadControlState write FOnAfterLoadControlState;
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

    procedure SaveToState(AState: TAdvStateManagerItem); overload; virtual;
    procedure SaveToState(AIndex: Integer); overload; virtual;
    procedure SaveToState(AName: string); overload; virtual;
    procedure LoadStateByName(AName: string); overload; virtual;
    procedure LoadStateByIndex(AIndex: Integer); overload; virtual;
    procedure LoadStateCustom(ACallBack: TAdvStateManagerLoadStateCustomCallback = nil);
    procedure Optimize; virtual;

    function FindConflicts(AConflictedControlNames: TStrings): Boolean; virtual;

    function GetDefaultState: TAdvStateManagerItem; virtual;

    function FindStateByName(AName: string): TAdvStateManagerItem; virtual;
  end;

implementation

uses
  AdvGraphics, SysUtils, Math, Types, AdvUtils,
  AdvCustomControl, AdvGraphicsTypes, Graphics
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

//{$R 'AdvState.res'}

type
  TAdvStateManagerRootJSONValue = class
  private
    FState: TAdvStateManagerItem;
    FValue: TJSONValue;
  public
    constructor Create(AState: TAdvStateManagerItem; AValue: TJSONValue);
    destructor Destroy; override;
  end;

  TAdvStateManagerJSONValue = class;

  TAdvStateManagerJSONValueList = TObjectList<TAdvStateManagerJSONValue>;

  TAdvStateManagerJSONValueArray = array of TJSONValue;

  TAdvStateManagerJSONValue = class
  private
    FValue: TJSONValue;
    FParentValue: TJSONValue;
    FValueName: string;
  public
    constructor Create(AParentValue, AValue: TJSONValue; AValueName: string);
    destructor Destroy; override;
  end;

  TAdvStateManagerRootJSONValueList = TObjectList<TAdvStateManagerRootJSONValue>;

  {$IFDEF FMXLIB}
  TCustomFormHelper = class helper for TCustomForm
  private
    function GetControlCount: Integer;
    function GetControls(AIndex: Integer): TAdvStateManagerControl;
  public
    property ControlCount: Integer read GetControlCount;
    property Controls[Index: Integer]: TAdvStateManagerControl read GetControls;
  end;
  {$ENDIF}

  {$IFDEF VCLLIB}
  TControlOpen = class(TWinControl);
  {$ENDIF}

{$IFDEF FMXLIB}

{ TCustomFormHelper }

function TCustomFormHelper.GetControls(AIndex: Integer): TAdvStateManagerControl;
var
  c: TComponent;
begin
  Result := nil;
  c := Components[AIndex];
  if c is TAdvStateManagerControl then
    Result := c as TAdvStateManagerControl;
end;

function TCustomFormHelper.GetControlCount: Integer;
begin
  Result := ComponentCount;
end;

{$ENDIF}

{ TAdvCustomStateManager }

procedure TAdvCustomStateManager.AfterAssignControl;
begin

end;

procedure TAdvCustomStateManager.Assign(Source: TPersistent);
begin
  if Source is TAdvCustomStateManager then
  begin
    FStates.Assign((Source as TAdvCustomStateManager).States);
    FControl := (Source as TAdvCustomStateManager).Control;
    FAutoSave := (Source as TAdvCustomStateManager).AutoSave;
    FActiveState := (Source as TAdvCustomStateManager).ActiveState;
  end;
end;

procedure TAdvCustomStateManager.BeforeAssignControl;
begin

end;

procedure TAdvCustomStateManager.BeforeLoadState(
  AState: TAdvStateManagerItem);
begin

end;

procedure TAdvCustomStateManager.BeginUpdate;
begin
  inherited;
  Inc(FUpdateCount);
end;

constructor TAdvCustomStateManager.Create;
begin
  Create(nil);
end;

procedure TAdvCustomStateManager.DoAfterLoadControlState(
  AState: TAdvStateManagerItem; AControl: TAdvStateManagerControl;
  AValue: string);
begin
  if Assigned(OnAfterLoadControlState) then
    OnAfterLoadControlState(Self, AState, AControl, AValue);
end;

procedure TAdvCustomStateManager.DoBeforeLoadControlState(
  AState: TAdvStateManagerItem; AControl: TAdvStateManagerControl;
  var AValue: string; var ACanLoad: Boolean);
begin
  if Assigned(OnBeforeLoadControlState) then
    OnBeforeLoadControlState(Self, AState, AControl, AValue, ACanLoad);
end;

procedure TAdvCustomStateManager.DoAfterLoadState(
  AState: TAdvStateManagerItem);
begin
  if Assigned(OnAfterLoadState) then
    OnAfterLoadState(Self, AState);
end;

procedure TAdvCustomStateManager.DoBeforeLoadState(
  AState: TAdvStateManagerItem; var ACanLoad: Boolean);
begin
  if Assigned(OnBeforeLoadState) then
    OnBeforeLoadState(Self, AState, ACanLoad);
end;

procedure TAdvCustomStateManager.DoCanWrite(AObject: TObject;
  APropertyName: string; APropertyKind: TTypeKind; AWriter: TAdvJSONWriter;
  var ACanWrite: Boolean);
begin
  ACanWrite := not (APropertyKind in [tkClassRef, tkPointer{$IFNDEF LCLLIB}, tkProcedure{$ENDIF}, tkMethod]);
end;

function TAdvCustomStateManager.CanPersist(
  AControl: TAdvStateManagerControl): Boolean;
begin
  Result := {$IFDEF FMXLIB}not (AControl is TStyleBook) and{$ENDIF}
            not (AControl is TAdvCustomComponent);
end;

constructor TAdvCustomStateManager.Create(AOwner: TComponent);
begin
  inherited;

  FStates := CreateStatesCollection;
  FActiveState := -1;
  FAutoSave := True;

  if Assigned(AOwner) and (AOwner is TAdvStateManagerControl) and IsDesignTime then
  begin
    if AOwner is TAdvStateManagerControl then
      Control := AOwner as TAdvStateManagerControl;
  end;
end;

function TAdvCustomStateManager.CreateStatesCollection: TAdvStateManagerItems;
begin
  Result := TAdvStateManagerItems.Create(Self);
end;

destructor TAdvCustomStateManager.Destroy;
begin
  FStates.Free;
  inherited;
end;

procedure TAdvCustomStateManager.EndUpdate;
begin
  inherited;
  Dec(FUpdateCount);
//  if FUpdateCount = 0 then
//    DoSomething;
end;

function TAdvCustomStateManager.FindStateByName(
  AName: string): TAdvStateManagerItem;
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

function TAdvCustomStateManager.FindConflicts(
  AConflictedControlNames: TStrings): Boolean;
var
  s: string;
  v, jv, jvv: TJSONValue;
  I, M: Integer;
  snm, nm: string;
  a: TJSONArray;
  c: TAdvStateManagerControl;
  st: TAdvStateManagerItem;
begin
  Result := False;
  for M := 0 to States.Count - 1 do
  begin
    st := States[M];
    s := st.Content;
    v := TAdvUtils.ParseJSON(s);
    if Assigned(v) then
    begin
      try
        jv := TAdvUtils.FindJSONValue(v, 'components');
        if Assigned(jv) and (jv is TJSONArray) then
        begin
          a := jv as TJSONArray;
          for I := 0 to TAdvUtils.GetJSONArraySize(a) - 1 do
          begin
            jvv := TAdvUtils.GetJSONArrayItem(a, I);
            if Assigned(jvv) and (jvv is TJSONObject) then
            begin
              nm := TAdvUtils.GetJSONObjectName(jvv as TJSONObject, 0);
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

function TAdvCustomStateManager.FindControlByName(
  AName: string; ARootControl: TAdvStateManagerControl = nil): TAdvStateManagerControl;
var
  c: TAdvStateManagerControl;

  function FindControl(AControl: TAdvStateManagerControl): TAdvStateManagerControl;
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
      if Assigned(ct) and (ct is TAdvStateManagerControl) and CanPersist(ct as TAdvStateManagerControl) then
      begin
        cn := ct.Name;
        if UpperCase(cn) = UpperCase(AName) then
        begin
          Result := ct as TAdvStateManagerControl;
          Break;
        end
        else if not Assigned(Result) then
        begin
          {$IFDEF FMXLIB}
          if not (AControl is TCustomForm) then
          {$ENDIF}
            Result := FindControl(ct as TAdvStateManagerControl);
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

function TAdvCustomStateManager.GenerateContent: string;
var
  cw: TAdvWriterCustomWritePropertyEvent;
  bl: Boolean;
  sl: TStringList;
  K: Integer;

  procedure AddControls(AControl: TAdvStateManagerControl);
  var
    s: string;
    cn, pn: string;
    ct: TComponent;
    I: Integer;
  begin
    for I := 0 to GetControlCount(AControl) - 1 do
    begin
      ct := GetControls(AControl, I);
      if Assigned(ct) and (ct is TAdvStateManagerControl) then
      begin
        if CanPersist(ct as TAdvStateManagerControl)  then
        begin
          s := ct.ToJSON;
          cn := ct.Name;
          if cn <> '' then
          begin
            pn := '';
            if Assigned((ct as TAdvStateManagerControl).Parent) then
              pn := (ct as TAdvStateManagerControl).Parent.Name;

            sl.Add('{"' + cn + '":{"content":' + s + ', "parent":"' + pn + '"}}');
          end;
        end;

        {$IFDEF FMXLIB}
        if not (AControl is TCustomForm) then
        {$ENDIF}
          AddControls(ct as TAdvStateManagerControl);
      end;
    end;
  end;
begin
  Result := '';
  if not Assigned(Control) then
    Exit;

  cw := TAdvPersistence.OnCustomWriteProperty;
  bl := TAdvCustomControl.BlockPersistenceInterface;
  TAdvPersistence.OnCustomWriteProperty := DoCanWrite;
  TAdvCustomControl.BlockPersistenceInterface := True;
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
    TAdvPersistence.OnCustomWriteProperty := cw;
    TAdvCustomControl.BlockPersistenceInterface := bl;
  end;
end;

function TAdvCustomStateManager.GetDefaultState: TAdvStateManagerItem;
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

function TAdvCustomStateManager.GetInstance: NativeUInt;
begin
  Result := HInstance;
end;

function TAdvCustomStateManager.GetControls(
  AControl: TAdvStateManagerControl;
  AIndex: Integer): TAdvStateManagerControl;
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

function TAdvCustomStateManager.GetControlCount(
  AControl: TAdvStateManagerControl): Integer;
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

procedure TAdvCustomStateManager.LoadStateByName(AName: string);
begin
  InternalLoadState(FindStateByName(AName));
end;

procedure TAdvCustomStateManager.LoadStateCustom(
  ACallBack: TAdvStateManagerLoadStateCustomCallback = nil);
var
  I: Integer;
  s: TAdvStateManagerItem;
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

procedure TAdvCustomStateManager.Loaded;
begin
  inherited;
  if not IsDesignTime then
    Optimize;
end;

procedure TAdvCustomStateManager.LoadStateByIndex(AIndex: Integer);
begin
  if (AIndex >= 0) and (AIndex <= States.Count - 1) then
    InternalLoadState(States[AIndex]);
end;

procedure TAdvCustomStateManager.InternalSaveToState(
  AState: TAdvStateManagerItem; ANew: Boolean);
begin
  if Assigned(AState) then
  begin
    AState.Content := GenerateContent;
    if ANew then
      InternalSetActiveState(AState);
  end;
end;

procedure TAdvCustomStateManager.InternalSetActiveState(
  AState: TAdvStateManagerItem);
begin
  if Assigned(AState) then
    FActiveState := AState.Index;
end;

procedure TAdvCustomStateManager.InternalLoadState(
  AState: TAdvStateManagerItem);
var
  s: string;
  v, jv, jvv, jvvo, ct: TJSONValue;
  I: Integer;
  nm: string;
  a: TJSONArray;
  c, p: TAdvStateManagerControl;
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
      v := TAdvUtils.ParseJSON(s);
      if Assigned(v) then
      begin
        bl := TAdvCustomControl.BlockPersistenceInterface;
        TAdvCustomControl.BlockPersistenceInterface := True;
        try
          jv := TAdvUtils.FindJSONValue(v, 'components');
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
              for I := 0 to TAdvUtils.GetJSONArraySize(a) - 1 do
              begin
                jvv := TAdvUtils.GetJSONArrayItem(a, I);
                if Assigned(jvv) and (jvv is TJSONObject) then
                begin
                  nm := TAdvUtils.GetJSONObjectName(jvv as TJSONObject, 0);
                  c := FindControlByName(nm);
                  if Assigned(c) then
                  begin
                    jvvo := TAdvUtils.GetJSONValue(jvv, nm);
                    if Assigned(jvvo) then
                    begin
                      ct := TAdvUtils.GetJSONValue(jvvo, 'content');
                      pn := TAdvUtils.GetJSONProp(jvvo, 'parent');
                      cl := True;
                      s := TAdvUtils.GetJSONValueAsString(ct);
                      DoBeforeLoadControlState(AState, c, s, cl);
                      if cl then
                      begin
                        if Assigned(ct) and (s <> '') then
                        begin
                          {$IFDEF VCLLIB}
                          //prototype
                          if not IsDesigntime then
                            TAdvUtils.ScaleForDPI(c, DesigntimeFormPixelsPerInch);
                          {$ENDIF}
                          c.FromJSON(s);
                          {$IFDEF VCLLIB}
                          //prototype
                          if not IsDesigntime then
                            TAdvUtils.ScaleForDPI(c, Round(DesigntimeFormPixelsPerInch * TAdvUtils.GetDPIScale(Self, DesigntimeFormPixelsPerInch)));
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
          TAdvCustomControl.BlockPersistenceInterface := bl;
        end;
      end;

      DoAfterLoadState(AState);
    end;
  end;
end;

procedure TAdvCustomStateManager.Optimize;
var
  L: Integer;
  ov, jvv, jov, jovsub: TJSONValue;
  ja: TJSONArray;
  o: TAdvStateManagerRootJSONValue;
  oarr: TAdvStateManagerRootJSONValueList;
  arr: TAdvStateManagerJSONValueList;
  I: Integer;
  nm: string;
  M: Integer;

  function CompareValues(va: TAdvStateManagerJSONValueArray): Boolean;
  var
    K: Integer;
    v: string;
    s: string;
  begin
    Result := False;
    if Length(va) > 0 then
    begin
      v := TAdvUtils.GetJSONValueAsString(va[0]);
      s := v;
      for K := 1 to Length(va) - 1 do
      begin
        s := s + ', ' + TAdvUtils.GetJSONValueAsString(va[K]);
        if TAdvUtils.GetJSONValueAsString(va[K]) <> v then
        begin
          Result := True;
          Break;
        end;
      end;
    end;
  end;

  procedure CompareObjects(oa: TAdvStateManagerJSONValueList; ARoot: Boolean);
  var
    J, {P, }K: Integer;
    jv{, a}: TJSONValue;
    v: TJSONValue;
    sz: Integer;
    oasub: TAdvStateManagerJSONValueList;
    vasub: TAdvStateManagerJSONValueArray;
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
        sz := TAdvUtils.GetJSONObjectSize(v as TJSONObject);
        sl := TStringList.Create;
        try
          for k := 0 to sz - 1 do
          begin
            n := TAdvUtils.GetJSONObjectName(v as TJSONObject, k);
            sl.Add(n);
          end;

          for k := 0 to sl.Count - 1 do
          begin
            jv := TAdvUtils.GetJSONValue(v, sl[k]);
            n := sl[k];

            oasub := TAdvStateManagerJSONValueList.Create;
            try
              oasub.Add(TAdvStateManagerJSONValue.Create(v, jv, n));
              for J := 1 to oa.Count - 1 do
                oasub.Add(TAdvStateManagerJSONValue.Create(oa[J].FValue, TAdvUtils.GetJSONValue(oa[J].FValue, n), n));

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
            if TAdvUtils.GetJSONValueAsString(oa[J].FValue) = '{}' then
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
//        for P := 0 to TAdvUtils.GetJSONArraySize((v as TJSONArray)) - 1 do
//        begin
//          a := TAdvUtils.GetJSONArrayItem((v as TJSONArray), P);
//
//          oasub := TAdvStateManagerJSONValueList.Create;
//          try
//            oasub.Add(TAdvStateManagerJSONValue.Create(v, a, ''));
//            for J := 1 to oa.Count - 1 do
//              oasub.Add(TAdvStateManagerJSONValue.Create(oa[J].FValue, TAdvUtils.GetJSONArrayItem((oa[J].FValue as TJSONArray), P), ''));
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
  oarr := TAdvStateManagerRootJSONValueList.Create;
  try
    for L := 0 to States.Count - 1 do
    begin
      ov := TAdvUtils.ParseJSON(States[L].Content);
      if Assigned(ov) then
      begin
        o := TAdvStateManagerRootJSONValue.Create(States[L], ov);
        oarr.Add(o);
      end;
    end;

    if oarr.Count > 0 then
    begin
      jov := TAdvUtils.FindJSONValue(oarr[0].FValue, 'components');
      if Assigned(jov) and (jov is TJSONArray) then
      begin
        ja := jov as TJSONArray;
        for I := 0 to TAdvUtils.GetJSONArraySize(ja) - 1 do
        begin
          jvv := TAdvUtils.GetJSONArrayItem(ja, I);
          if Assigned(jvv) and (jvv is TJSONObject) then
          begin
            nm := TAdvUtils.GetJSONObjectName(jvv as TJSONObject, 0);
            arr := TAdvStateManagerJSONValueList.Create;
            try
              arr.Add(TAdvStateManagerJSONValue.Create(jov, jvv, nm));
              for M := 1 to oarr.Count - 1 do
              begin
                jovsub := TAdvUtils.FindJSONValue(oarr[M].FValue, 'components');
                if Assigned(jovsub) and (jovsub is TJSONArray) then
                  arr.Add(TAdvStateManagerJSONValue.Create(jovsub, TAdvUtils.GetJSONArrayItem(jovsub as TJSONArray, I), nm));
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
          oarr[M].FState.Content := TAdvUtils.GetJSONValueAsString(oarr[M].FValue);
      end;
    end;
  finally
    oarr.Free;
  end;
end;

procedure TAdvCustomStateManager.ResetState;
begin
  FActiveState := -1;
end;

procedure TAdvCustomStateManager.SaveToState(
  AState: TAdvStateManagerItem);
begin
  InternalSaveToState(AState, False)
end;

procedure TAdvCustomStateManager.SaveToState(AIndex: Integer);
begin
  if (AIndex >= 0) and (AIndex <= States.Count - 1) then
    SaveToState(States[AIndex]);
end;

procedure TAdvCustomStateManager.SaveToState(AName: string);
begin
  SaveToState(FindStateByName(AName));
end;

procedure TAdvCustomStateManager.SetActiveState(const Value: Integer);
begin
  if FActiveState <> Value then
  begin
    LoadStateByIndex(Value);
    FActiveState := Value;
  end;
end;

procedure TAdvCustomStateManager.SetControl(
  const Value: TAdvStateManagerControl);
begin
  if FControl <> Value then
  begin
    BeforeAssignControl;
    FControl := Value;
    AfterAssignControl;
  end;
end;

procedure TAdvCustomStateManager.SetStates(
  const Value: TAdvStateManagerItems);
begin
  FStates.Assign(Value);
end;

{ TAdvStateManagerItems }

{$IFDEF LCLLIB}
function TAdvStateManagerItems.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid : tguid;out obj) : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
{$ELSE}
function TAdvStateManagerItems.QueryInterface(const IID: TGUID; out Obj): HResult;
{$ENDIF}
const
  E_NOINTERFACE = HResult($80004002);
begin
  if GetInterface(IID, Obj) then Result := 0 else Result := E_NOINTERFACE;
end;

{$IFDEF LCLLIB}
function TAdvStateManagerItems._AddRef: longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
{$ELSE}
function TAdvStateManagerItems._AddRef: Integer;
{$ENDIF}
begin
  if FOwnerInterface <> nil then
    Result := FOwnerInterface._AddRef else
    Result := -1;
end;

{$IFDEF LCLLIB}
function TAdvStateManagerItems._Release: longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
{$ELSE}
function TAdvStateManagerItems._Release: Integer;
{$ENDIF}
begin
  if FOwnerInterface <> nil then
    Result := FOwnerInterface._Release else
    Result := -1;
end;

function TAdvStateManagerItems.Add: TAdvStateManagerItem;
begin
  Result := TAdvStateManagerItem(inherited Add);
end;

constructor TAdvStateManagerItems.Create(AOwner: TAdvCustomStateManager);
begin
  inherited Create(AOwner, GetStateManagerStateClass);
  FOwner := AOwner;
end;

function TAdvStateManagerItems.CreateObject(const AClassName: string;
  const ABaseClass: TClass): TObject;
begin
  Result := GetStateManagerStateClass.Create(nil);
end;

function TAdvStateManagerItems.GetInterfaceItemClass: TClass;
begin
  Result := GetStateManagerStateClass;
end;

function TAdvStateManagerItems.GetStateManagerStateClass: TCollectionItemClass;
begin
  Result := TAdvStateManagerItem;
end;

function TAdvStateManagerItems.GetItemEx(Index: Integer): TAdvStateManagerItem;
begin
  Result := TAdvStateManagerItem(inherited Items[Index]);
end;

function TAdvStateManagerItems.Insert(index: Integer): TAdvStateManagerItem;
begin
  Result := TAdvStateManagerItem(inherited Insert(Index));
end;

procedure TAdvStateManagerItems.SetItemEx(Index: Integer; const Value: TAdvStateManagerItem);
begin
  inherited SetItem(Index, Value);
end;

{ TAdvStateManagerItem }

procedure TAdvStateManagerItem.Assign(Source: TPersistent);
begin
  if (Source is TAdvStateManagerItem) then
  begin
    FName := (Source as TAdvStateManagerItem).Name;
    FContent := (Source as TAdvStateManagerItem).Content;
    FDefault := (Source as TAdvStateManagerItem).Default;
  end;
end;

constructor TAdvStateManagerItem.Create(Collection: TCollection);
var
  m: TAdvCustomStateManager;
begin
  inherited;

  if Assigned(Collection) and Assigned((Collection as TAdvStateManagerItems).FOwner) then
    FOwner := (Collection as TAdvStateManagerItems).FOwner;

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

destructor TAdvStateManagerItem.Destroy;
begin
  inherited;
end;

function TAdvStateManagerItem.GetDisplayName: string;
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

function TAdvStateManagerItem.GetName: string;
begin
  Result := FName;
  if Result = '' then
    Result := 'State ' + IntToStr(Index + 1);
end;

procedure TAdvStateManagerItem.Load;
var
  m: TAdvCustomStateManager;
begin
  m := Manager;
  if Assigned(m) then
    m.InternalLoadState(Self);
end;

function TAdvStateManagerItem.Manager: TAdvCustomStateManager;
begin
  Result := FOwner;
end;

procedure TAdvStateManagerItem.SetDefault(const Value: Boolean);
var
  I: Integer;
  m: TAdvCustomStateManager;
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

{ TAdvStateManagerRootJSONValue }

constructor TAdvStateManagerRootJSONValue.Create(
  AState: TAdvStateManagerItem; AValue: TJSONValue);
begin
  FState := AState;
  FValue := AValue;
end;

destructor TAdvStateManagerRootJSONValue.Destroy;
begin
  FreeAndNil(FValue);
  FState := nil;
  inherited;
end;

constructor TAdvStateManagerJSONValue.Create(AParentValue, AValue: TJSONValue; AValueName: string);
begin
  FValue := AValue;
  FParentValue := AParentValue;
  FValueName := AValueName;
end;

destructor TAdvStateManagerJSONValue.Destroy;
begin
  FValue := nil;
  FParentValue := nil;
  FValueName := '';
  inherited;
end;

end.

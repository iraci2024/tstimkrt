//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2018-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I SB.inc}
unit ScJsonSerializator;

interface

uses
{$IFDEF MSWINDOWS}
{$IFNDEF FPC}
  Windows,
{$ENDIF}
{$ENDIF}
  SysUtils, Classes, TypInfo, Variants,
  ScTypes, ScFunctions, ScCLRCLasses, ScJson;

resourcestring
  SNotObject = 'JSON is not an object declaration';
  SNotList = 'JSON is not an object array declaration';
  SPropertyNotFound = 'Property %s.%s was not found';
  SDataMismatch = 'Data mismatch for property %s.%s';
  SPropertyNotInitialized = 'Property %s.%s is not initialized';
  SCanNotSerialize = 'Can not serialize %s.%s';
  SCanNotDeserialize = 'Can not deserialize the %s class';
  SJSONInternalError = 'JSON deserializer internal error';
  SUnexpectedJsonTokenType = 'Unexpected JSON token type ''%s''. Expected a %s type';

type
  {$M+}

  TNullableClass = class of TNullable;

  TNullable = class
  private
    FIsNull: boolean;
  protected
    function GetIsNull: boolean; virtual;
    procedure SetIsNull(const Value: boolean);

    procedure InitDefaultValue; virtual; abstract;
  public
    constructor Create; virtual;
  published
    property IsNull: boolean read GetIsNull write SetIsNull;
  end;

  IntegerNullable = class(TNullable)
  private
    FValue: Int64;
    procedure SetValue(const Value: Int64);
  protected
    procedure InitDefaultValue; override;
  public
    procedure Assign(Source: IntegerNullable);
  published
    property Value: Int64 read FValue write SetValue;
  end;

  DoubleNullable = class(TNullable)
  private
    FValue: double;
    procedure SetValue(const Value: double);
  protected
    procedure InitDefaultValue; override;
  public
    procedure Assign(Source: DoubleNullable);
  published
    property Value: double read FValue write SetValue;
  end;

  BooleanNullable = class(TNullable)
  private
    FValue: boolean;
    procedure SetValue(const Value: boolean);
  protected
    procedure InitDefaultValue; override;
  public
    procedure Assign(Source: BooleanNullable);
  published
    property Value: boolean read FValue write SetValue;
  end;

  StringNullable = class(TNullable)
  private
    FValue: string;
    procedure SetValue(const Value: string);
  protected
    procedure InitDefaultValue; override;
  public
    procedure Assign(Source: StringNullable);
  published
    property Value: string read FValue write SetValue;
  end;

  DateTimeNullable = class(TNullable)
  private
    FValue: TDateTime;
    procedure SetValue(const Value: TDateTime);
  protected
    procedure InitDefaultValue; override;
  public
    procedure Assign(Source: DateTimeNullable);
  published
    property Value: TDateTime read FValue write SetValue;
  end;

  TIntArrNullable = class(TNullable)
  private
    FValue: TIntArr;
  protected
    function GetIsNull: boolean; override;

    procedure InitDefaultValue; override;
  public
    destructor Destroy; override;

    procedure Assign(Source: TIntArrNullable);
  published
    property Value: TIntArr read FValue write FValue;
  end;

  TSerializedObject = class
  public
    constructor Create; virtual;
  end;

  TSerializedClass = class of TSerializedObject;

  TSerializedList = class(TList)
  private
    FFreeOnDelete: Boolean;
    FItemClass: TSerializedClass;
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    constructor Create(AItemClass: TSerializedClass);

    property FreeOnDelete: boolean read FFreeOnDelete write FFreeOnDelete;
    property ItemClass: TSerializedClass read FItemClass;
  end;

  {$M-}

  TCustomObjectDeserializer = class
  private
    FmtSet: TFormatSettings;

    function CreateObject(ObjectClass: TClass): TObject;
    function GetArrayItemPtr(Instance: TObject; const Name: string; Index: integer; out ItemInfo: PTypeInfo): pointer;

    procedure SetStringProperty(Instance: TObject; const Name, Value: string);
    procedure SetStringArrayProperty(Instance: TObject; const Name: string; Index: integer; const Value: string);
    procedure SetNumberProperty(Instance: TObject; const Name, Value: string);
    procedure SetNumberArrayProperty(Instance: TObject; const Name: string; Index: integer; const Value: string);
    procedure SetBooleanProperty(Instance: TObject; const Name: string; const Value: boolean);
    procedure SetBooleanArrayProperty(Instance: TObject; const Name: string; Index: integer; const Value: boolean);
    function SetObjectProperty(Instance: TObject; const Name: string): TObject;
    function SetObjectArrayProperty(Instance: TObject; const Name: string): TObject;
    procedure SetNullProperty(Instance: TObject; const Name: string);
    procedure SetNullArrayProperty(Instance: TObject; const Name: string; Index: integer);
  public
    constructor Create; virtual;
  end;

  TTextToObjectDeserializer = class(TCustomObjectDeserializer)
  private
    FReader: TCustomJSONReader;

    procedure InitReader(ReaderClass: TJSONReaderClass);
    procedure FreeReader;

    procedure ProcessTag(const Tag: TJSONTag; Parent: TObject; const Name: string; Index: integer);
    procedure ProcessObject(Parent: TObject; const Name: string; Index: integer);
    procedure ProcessObjectProps(Parent: TObject);
    function ProcessPair(Parent: TObject): boolean;
    procedure ProcessArray(Parent: TObject; const Name: string);
    procedure ProcessNumber(Parent: TObject; const Name: string; Index: integer);
    procedure ProcessString(Parent: TObject; const Name: string; Index: integer);
    procedure ProcessBoolean(Parent: TObject; const Name: string; Index: integer);
    procedure ProcessNull(Parent: TObject; const Name: string; Index: integer);
  public
    constructor Create; override;
    destructor Destroy; override;

    function ToObject(const Text: string; ObjectClass: TClass): TObject;
    function ToList(const Text: string; ObjectClass: TSerializedClass): TSerializedList;
  end;

  TJSONToObjectDeserializer = class(TCustomObjectDeserializer)
  private
    procedure ProcessValue(Parent: TObject; const Name: string; JSONValue: TJSONValue; Index: integer);
    procedure ProcessObject(Parent: TObject; const Name: string; JSONObject: TJSONObject; Index: integer);
    procedure ProcessObjectProps(Parent: TObject; JSONObject: TJSONObject);
    procedure ProcessArray(Parent: TObject; const Name: string; JSONValue: TJSONArray);
    procedure ProcessNumber(Parent: TObject; const Name: string; JSONValue: TJSONNumber; Index: integer);
    procedure ProcessString(Parent: TObject; const Name: string; JSONValue: TJSONString; Index: integer); overload;
    procedure ProcessString(Parent: TObject; const Name, Value: string; Index: integer); overload;
    procedure ProcessBoolean(Parent: TObject; const Name: string; JSONValue: TJSONBoolean; Index: integer);
    procedure ProcessNull(Parent: TObject; const Name: string; JSONValue: TJSONNull; Index: integer);
  public
    function ToObject(JSONValue: TJSONValue; ObjectClass: TClass): TObject;
    function ToVariant(JSONValue: TJSONValue; VarType: TVarType; ObjectClass: TClass): Variant;
  end;

  TObjectSerializer = class
  private
    FWriter: TCustomJSONWriter;
    FWriterOwner: boolean;

    procedure InitWriter(const WriterClass: TJSONWriterClass);
    procedure FreeWriter;

    procedure ProcessVariant(const Value: Variant);
    procedure ProcessObject(Value: TObject);
    procedure ProcessArray(Parent: TObject; const PropInfo: PPropInfo);
    procedure ProcessProperty(Parent: TObject; const PropInfo: PPropInfo);
    procedure ProcessString(const Value: string; const Escape: boolean);
  public
    constructor Create;
    destructor Destroy; override;

    function ToText(Value: TObject): string; overload;
    function ToText(Value: TList): string; overload;
    procedure WriteVariant(Writer: TJSONTextWriter; const Value: Variant);
    procedure WriteObject(Writer: TJSONTextWriter; Value: TObject);
  end;

implementation

{$IFDEF VER12P}
  {$DEFINE USE_UINT64}
{$ENDIF}
{$IFDEF FPC}
  {$DEFINE USE_UINT64}
{$ENDIF}

const
{$IFDEF CPU64}
  PROPSLOT_MASK = $FF00000000000000;
{$ELSE}
  PROPSLOT_MASK = $FF000000;
{$ENDIF}

{$IFNDEF VER16P}
function DynArraySize(A: Pointer): NativeInt;
begin
  Result := NativeInt(A);
  if Result <> 0 then                                  // TDynArrayRec should be used here but a
    Result := PNativeInt(Result - SizeOf(NativeInt))^; // private symbol cannot be inlined
end;
{$ENDIF}

function DateTimeToISO8601(dt: TDateTime): string;
{$IFDEF VER7P}
var
  tmpFormatSettings: TFormatSettings;
{$ENDIF}
begin
{$IFDEF VER7P}
  tmpFormatSettings.DateSeparator := '-';
  tmpFormatSettings.TimeSeparator := ':';
  tmpFormatSettings.ShortDateFormat := 'yyyy-mm-dd';
  tmpFormatSettings.LongDateFormat := 'yyyy-mm-dd';
  tmpFormatSettings.ShortTimeFormat := 'hh:nn:ss';
  tmpFormatSettings.LongTimeFormat := 'hh:nn:ss';
{$ENDIF}
  DateTimeToString(Result, 'yyyy-mm-dd"T"hh:nn:ss.000', dt{$IFDEF VER7P}, tmpFormatSettings{$ENDIF});
end;

function ISO8601ToDateTime(const Value: string): TDateTime;
var
  y, m, d, h, n, s: word;
  Date, Time: TDateTime;
begin
  if Value = '' then begin
    Result := 0;
    Exit;
  end;

  y := StrToInt(Copy(Value, 1, 4));
  m := StrToInt(Copy(Value, 6, 2));
  d := StrToInt(Copy(Value, 9, 2));
  h := StrToInt(Copy(Value, 12, 2));
  n := StrToInt(Copy(Value, 15, 2));
  s := StrToInt(Copy(Value, 18, 2));

  Date := EncodeDate(y, m, d);
  Time := EncodeTime(h, n, s, 0);

  if Date < 0 then
    Result := Trunc(Date) - Abs(Frac(Time))
  else
    Result := Trunc(Date) + Abs(Frac(Time));
end;

{ TNullable }

constructor TNullable.Create;
begin
  inherited;

  FIsNull := True;
end;

function TNullable.GetIsNull: boolean;
begin
  Result := FIsNull;
end;

procedure TNullable.SetIsNull(const Value: boolean);
begin
  if FIsNull <> Value then begin
    FIsNull := Value;

    if not FIsNull then
      InitDefaultValue;
  end;
end;

{ IntegerNullable }

procedure IntegerNullable.SetValue(const Value: Int64);
begin
  if IsNull or (FValue <> Value) then begin
    IsNull := False;
    FValue := Value;
  end;
end;

procedure IntegerNullable.InitDefaultValue;
begin
  FValue := 0;
end;

procedure IntegerNullable.Assign(Source: IntegerNullable);
begin
  if Source <> nil then begin
    FIsNull := Source.FIsNull;
    FValue := Source.FValue;
  end
  else
    FIsNull := True;
end;

{ DoubleNullable }

procedure DoubleNullable.SetValue(const Value: double);
begin
  if IsNull or (FValue <> Value) then begin
    IsNull := False;
    FValue := Value;
  end;
end;

procedure DoubleNullable.InitDefaultValue;
begin
  FValue := 0;
end;

procedure DoubleNullable.Assign(Source: DoubleNullable);
begin
  if Source <> nil then begin
    FIsNull := Source.FIsNull;
    FValue := Source.FValue;
  end
  else
    FIsNull := True;
end;

{ BooleanNullable }

procedure BooleanNullable.SetValue(const Value: boolean);
begin
  if IsNull or (FValue <> Value) then begin
    IsNull := False;
    FValue := Value;
  end;
end;

procedure BooleanNullable.InitDefaultValue;
begin
  FValue := False;
end;

procedure BooleanNullable.Assign(Source: BooleanNullable);
begin
  if Source <> nil then begin
    FIsNull := Source.FIsNull;
    FValue := Source.FValue;
  end
  else
    FIsNull := True;
end;

{ StringNullable }

procedure StringNullable.SetValue(const Value: string);
begin
  if IsNull or (FValue <> Value) then begin
    IsNull := False;
    FValue := Value;
  end;
end;

procedure StringNullable.InitDefaultValue;
begin
  FValue := '';
end;

procedure StringNullable.Assign(Source: StringNullable);
begin
  if Source <> nil then begin
    FIsNull := Source.FIsNull;
    FValue := Source.FValue;
  end
  else
    FIsNull := True;
end;

{ DateTimeNullable }

procedure DateTimeNullable.SetValue(const Value: TDateTime);
begin
  if IsNull or (FValue <> Value) then begin
    IsNull := False;
    FValue := Value;
  end;
end;

procedure DateTimeNullable.InitDefaultValue;
begin
  FValue := 0;
end;

procedure DateTimeNullable.Assign(Source: DateTimeNullable);
begin
  if Source <> nil then begin
    FIsNull := Source.FIsNull;
    FValue := Source.FValue;
  end
  else
    FIsNull := True;
end;

{ TIntArrNullable }

destructor TIntArrNullable.Destroy;
begin
  SetLength(FValue, 0);
end;

function TIntArrNullable.GetIsNull: boolean;
begin
  Result := Length(FValue) = 0;
end;

procedure TIntArrNullable.InitDefaultValue;
begin
  SetLength(FValue, 0);
end;

procedure TIntArrNullable.Assign(Source: TIntArrNullable);
begin
  if Source <> nil then begin
    FIsNull := Source.FIsNull;
    FValue := Source.FValue;
  end
  else
    FIsNull := True;
end;

{ TSerializedObject }

constructor TSerializedObject.Create;
begin
  inherited;
end;

{ TSerializedList }

constructor TSerializedList.Create(AItemClass: TSerializedClass);
begin
  inherited Create;

  FFreeOnDelete := True;
  FItemClass := AItemClass;
end;

procedure TSerializedList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if FFreeOnDelete and (Action = lnDeleted) then
    TObject(Ptr).Free;
end;

{ TCustomObjectDeserializer }

constructor TCustomObjectDeserializer.Create;
begin
  inherited;

{$IFDEF FPC}
  FmtSet := FormatSettings;
{$ENDIF}
  FmtSet.DecimalSeparator := '.';
end;

function TCustomObjectDeserializer.CreateObject(ObjectClass: TClass): TObject;
begin
  if ObjectClass = nil then
    raise ArgumentException.CreateFmt(SCanNotDeserialize, ['nil']);

  if ObjectClass.InheritsFrom(TComponent) then
    Result := TComponentClass(ObjectClass).Create(nil)
  else
  if ObjectClass.InheritsFrom(TCollectionItem) then
    Result := TCollectionItemClass(ObjectClass).Create(nil)
  else
  if ObjectClass.InheritsFrom(TSerializedObject) then
    Result := TSerializedClass(ObjectClass).Create
  else
  if ObjectClass.InheritsFrom(TNullable) then
    Result := TNullableClass(ObjectClass).Create
  else
    raise JSONException.CreateFmt(SCanNotDeserialize, [ObjectClass.ClassName]);
end;

function TCustomObjectDeserializer.GetArrayItemPtr(Instance: TObject; const Name: string;
  Index: integer; out ItemInfo: PTypeInfo): pointer;
var
  PropInfo: PPropInfo;
  TypeInfo: PTypeInfo;
  TypeData: PTypeData;
  Size: NativeInt;
  ArrPtr: pointer;
  Obj: TObject;
begin
  ItemInfo := nil;

  PropInfo := GetPropInfo(Instance.ClassType, Name);
  if PropInfo <> nil then begin
    TypeInfo := PropInfo.PropType{$IFNDEF FPC}^{$ENDIF};

    if (TypeInfo.Kind = tkDynArray) and (Index >= 0) then begin
      ArrPtr := PtrOffset(PByte(Instance), NativeInt(PropInfo.GetProc) and not PROPSLOT_MASK);
      Size := Index + 1;
      DynArraySetLength(PPointer(ArrPtr)^, TypeInfo, 1, @Size);
      TypeData := GetTypeData(TypeInfo);
      Result := PtrOffset(PPointer(ArrPtr)^, Index * TypeData.elSize);
      ItemInfo := TypeData.elType2{$IFNDEF FPC}^{$ENDIF};
    end
    else if (TypeInfo.Kind = tkClass) and (TypeInfo.{$IFNDEF NEXTGEN}Name{$ELSE}NameFld.ToString{$ENDIF} = 'TIntArrNullable') and (Index >= 0) then begin
      Obj := GetObjectProp(Instance, PropInfo);
      if Obj = nil then
        Obj := TIntArrNullable.Create;
      SetObjectProp(Instance, PropInfo, Obj);
      Result := GetArrayItemPtr(Obj, 'Value', Index, ItemInfo);
    end
    else
      raise JSONException.CreateFmt(SDataMismatch, [Instance.ClassName, Name]);
  end
  else
    Result := nil;
end;

procedure TCustomObjectDeserializer.SetStringProperty(Instance: TObject; const Name, Value: string);
var
  Obj: TObject;
  PropInfo: PPropInfo;
  TypeInfo: PTypeInfo;
  TypeKind: TTypeKind;
begin
  if not Assigned(Instance) then
    exit;

  PropInfo := GetPropInfo(Instance.ClassType, Name);
  if PropInfo <> nil then begin
    Obj := Instance;
    TypeInfo := PropInfo.PropType{$IFNDEF FPC}^{$ENDIF};
    TypeKind := TypeInfo.Kind;

    if TypeKind = tkClass then begin
      if TypeInfo.{$IFNDEF NEXTGEN}Name{$ELSE}NameFld.ToString{$ENDIF} = 'StringNullable' then begin
        Obj := GetObjectProp(Instance, PropInfo);
        if Obj = nil then begin
          Obj := StringNullable.Create;
          SetObjectProp(Instance, PropInfo, Obj);
        end;
        PropInfo := GetPropInfo(StringNullable, 'Value');
      end
      else if TypeInfo.{$IFNDEF NEXTGEN}Name{$ELSE}NameFld.ToString{$ENDIF} = 'DateTimeNullable' then begin
        Obj := GetObjectProp(Instance, PropInfo);
        if Obj = nil then begin
          Obj := DateTimeNullable.Create;
          SetObjectProp(Instance, PropInfo, Obj);
        end;
        PropInfo := GetPropInfo(DateTimeNullable, 'Value');
      end
      else
        raise JSONException.CreateFmt(SDataMismatch, [Instance.ClassName, Name]);
      TypeKind := PropInfo.PropType{$IFNDEF FPC}^{$ENDIF}.Kind;
    end;

    case TypeKind of
    {$IFDEF FPC}
      tkAString,
    {$ENDIF}
      tkString,
      tkLString:
        SetStrProp(Obj, PropInfo, Value);
    {$IFDEF VER12P}
      tkUString,
    {$ENDIF}
      tkWString:
        {$IFNDEF NEXTGEN}SetWideStrProp{$ELSE}SetStrProp{$ENDIF}(Obj, PropInfo, WideString(Value));
      tkFloat:
        if PropInfo.PropType{$IFNDEF FPC}^{$ENDIF}.{$IFNDEF NEXTGEN}Name{$ELSE}NameFld.ToString{$ENDIF} = 'TDateTime' then
          SetFloatProp(Obj, PropInfo, ISO8601ToDateTime(Value))
        else
          raise JSONException.CreateFmt(SDataMismatch, [Instance.ClassName, Name]);
    else
      raise JSONException.CreateFmt(SDataMismatch, [Instance.ClassName, Name]);
    end;
  end;
end;

procedure TCustomObjectDeserializer.SetStringArrayProperty(Instance: TObject;
  const Name: string; Index: integer; const Value: string);
var
  PropInfo: PPropInfo;
  TypeInfo: PTypeInfo;
  Strings: TStrings;
  ItemPtr: pointer;
  ItemInfo: PTypeInfo;
begin
  PropInfo := GetPropInfo(Instance.ClassType, Name);
  if PropInfo <> nil then begin
    TypeInfo := PropInfo.PropType{$IFNDEF FPC}^{$ENDIF};

    if (TypeInfo.Kind = tkClass) and GetObjectPropClass(Instance, Name).InheritsFrom(TStrings) then begin
      Strings := GetObjectProp(Instance, Name) as TStrings;
      if Strings = nil then begin
        Strings := TStringList.Create;
        SetObjectProp(Instance, Name, Strings);
      end;
      Strings.Add(Value)
    end
    else begin
      ItemPtr := GetArrayItemPtr(Instance, Name, Index, ItemInfo);
      if (ItemPtr <> nil) and (ItemInfo.Kind = tkFloat) and (ItemInfo.{$IFNDEF NEXTGEN}Name{$ELSE}NameFld.ToString{$ENDIF} = 'TDateTime') then
        TDateTime(ItemPtr^) := ISO8601ToDateTime(Value)
      else
        raise JSONException.CreateFmt(SDataMismatch, [Instance.ClassName, Name]);
    end;
  end;
end;

procedure TCustomObjectDeserializer.SetNumberProperty(Instance: TObject; const Name, Value: string);
var
  Obj: TObject;
  PropInfo: PPropInfo;
  TypeInfo: PTypeInfo;
  TypeKind: TTypeKind;
begin
  if not Assigned(Instance) then
    exit;

  PropInfo := GetPropInfo(Instance.ClassType, Name);
  if PropInfo <> nil then begin
    Obj := Instance;
    TypeInfo := PropInfo.PropType{$IFNDEF FPC}^{$ENDIF};
    TypeKind := TypeInfo.Kind;

    if TypeKind = tkClass then begin
      if TypeInfo.{$IFNDEF NEXTGEN}Name{$ELSE}NameFld.ToString{$ENDIF} = 'IntegerNullable' then begin
        Obj := GetObjectProp(Instance, PropInfo);
        if Obj = nil then begin
          Obj := IntegerNullable.Create;
          SetObjectProp(Instance, PropInfo, Obj);
        end;
        PropInfo := GetPropInfo(IntegerNullable, 'Value');
      end
      else if TypeInfo.{$IFNDEF NEXTGEN}Name{$ELSE}NameFld.ToString{$ENDIF} = 'DoubleNullable' then begin
        Obj := GetObjectProp(Instance, PropInfo);
        if Obj = nil then begin
          Obj := DoubleNullable.Create;
          SetObjectProp(Instance, PropInfo, Obj);
        end;
        PropInfo := GetPropInfo(DoubleNullable, 'Value');
      end
      else
        raise JSONException.CreateFmt(SDataMismatch, [Instance.ClassName, Name]);
      TypeKind := PropInfo.PropType{$IFNDEF FPC}^{$ENDIF}.Kind;
    end;

    case TypeKind of
      tkInteger, tkEnumeration:
        SetOrdProp(Obj, PropInfo, StrToInt(Value));
      tkInt64:
        SetInt64Prop(Obj, PropInfo, StrToInt64(Value));
      tkFloat:
        SetFloatProp(Obj, PropInfo, StrToFloat(Value{$IFDEF VER7P}, FmtSet{$ENDIF}));
    else
      raise JSONException.CreateFmt(SDataMismatch, [Instance.ClassName, Name]);
    end;
  end;
end;

procedure TCustomObjectDeserializer.SetNumberArrayProperty(Instance: TObject;
  const Name: string; Index: integer; const Value: string);
var
  ItemPtr: pointer;
  ItemInfo: PTypeInfo;
begin
  ItemPtr := GetArrayItemPtr(Instance, Name, Index, ItemInfo);
  if ItemPtr <> nil then begin
    case ItemInfo.Kind of
      tkInteger:
        Integer(ItemPtr^) := StrToInt(Value);
      tkInt64:
        Int64(ItemPtr^) := StrToInt(Value);
      tkFloat:
        Double(ItemPtr^) := StrToFloat(Value{$IFDEF VER7P}, FmtSet{$ENDIF});
      tkClass:
        if ItemInfo.{$IFNDEF NEXTGEN}Name{$ELSE}NameFld.ToString{$ENDIF} = 'IntegerNullable' then begin
          if pointer(ItemPtr^) = nil then
            IntegerNullable(ItemPtr^) := IntegerNullable.Create;
          SetOrdProp(IntegerNullable(ItemPtr^), 'Value', StrToInt(Value));
        end
        else if ItemInfo.{$IFNDEF NEXTGEN}Name{$ELSE}NameFld.ToString{$ENDIF} = 'DoubleNullable' then begin
          if pointer(ItemPtr^) = nil then
            DoubleNullable(ItemPtr^) := DoubleNullable.Create;
          SetFloatProp(DoubleNullable(ItemPtr^), 'Value', StrToFloat(Value));
        end
        else
          raise JSONException.CreateFmt(SDataMismatch, [Instance.ClassName, Name]);
    else
      raise JSONException.CreateFmt(SDataMismatch, [Instance.ClassName, Name]);
    end;
  end;
end;

procedure TCustomObjectDeserializer.SetBooleanProperty(Instance: TObject; const Name: string; const Value: boolean);
var
  Obj: TObject;
  PropInfo: PPropInfo;
  TypeInfo: PTypeInfo;
  TypeKind: TTypeKind;
begin
  PropInfo := GetPropInfo(Instance.ClassType, Name);
  if PropInfo <> nil then begin
    Obj := Instance;
    TypeInfo := PropInfo.PropType{$IFNDEF FPC}^{$ENDIF};
    TypeKind := TypeInfo.Kind;

    if TypeKind = tkClass then begin
      if TypeInfo.{$IFNDEF NEXTGEN}Name{$ELSE}NameFld.ToString{$ENDIF} = 'BooleanNullable' then begin
        Obj := GetObjectProp(Instance, PropInfo);
        if Obj = nil then begin
          Obj := BooleanNullable.Create;
          SetObjectProp(Instance, PropInfo, Obj);
        end;
        PropInfo := GetPropInfo(BooleanNullable, 'Value');
      end
      else
        raise JSONException.CreateFmt(SDataMismatch, [Instance.ClassName, Name]);
      TypeKind := PropInfo.PropType{$IFNDEF FPC}^{$ENDIF}.Kind;
    end;

    if TypeKind in [tkEnumeration{$IFDEF FPC}, tkBool{$ENDIF}] then
      SetEnumProp(Obj, PropInfo, BoolToStr(Value, True))
    else
      raise JSONException.CreateFmt(SDataMismatch, [Instance.ClassName, Name]);
  end;
end;

procedure TCustomObjectDeserializer.SetBooleanArrayProperty(Instance: TObject;
  const Name: string; Index: integer; const Value: boolean);
var
  ItemPtr: pointer;
  ItemInfo: PTypeInfo;
begin
  ItemPtr := GetArrayItemPtr(Instance, Name, Index, ItemInfo);
  if ItemPtr <> nil then begin
    case ItemInfo.Kind of
    {$IFDEF FPC}
      tkBool,
    {$ENDIF}
      tkEnumeration:
        Boolean(ItemPtr^) := Value;
      tkClass:
        if ItemInfo.{$IFNDEF NEXTGEN}Name{$ELSE}NameFld.ToString{$ENDIF} = 'BooleanNullable' then begin
          if pointer(ItemPtr^) = nil then
            BooleanNullable(ItemPtr^) := BooleanNullable.Create;
          SetEnumProp(IntegerNullable(ItemPtr^), 'Value', BoolToStr(Value, True));
        end
        else
          raise JSONException.CreateFmt(SDataMismatch, [Instance.ClassName, Name])
    else
      raise JSONException.CreateFmt(SDataMismatch, [Instance.ClassName, Name]);
    end;
  end;
end;

function TCustomObjectDeserializer.SetObjectProperty(Instance: TObject; const Name: string): TObject;
var
  PropInfo: PPropInfo;
  TypeInfo: PTypeInfo;
  PropClass: TClass;
begin
  PropInfo := GetPropInfo(Instance.ClassType, Name);
  if PropInfo <> nil then begin
    TypeInfo := PropInfo.PropType{$IFNDEF FPC}^{$ENDIF};

    case TypeInfo.Kind of
      tkClass: begin
        PropClass := GetObjectPropClass(Instance, Name);
        if PropClass.InheritsFrom(TComponent) then
          Result := TComponentClass(PropClass).Create(nil)
        else
        if PropClass.InheritsFrom(TCollectionItem) then
          Result := TCollectionItemClass(PropClass).Create(nil)
        else
        if PropClass.InheritsFrom(TSerializedObject) then
          Result := TSerializedClass(PropClass).Create
        else
        if PropClass.InheritsFrom(TNullable) then
          Result := TNullableClass(PropClass).Create
        else
          raise JSONException.Create(SJSONInternalError);

        SetObjectProp(Instance, Name, Result);
      end;
    else
      raise JSONException.CreateFmt(SDataMismatch, [Instance.ClassName, Name]);
    end;
  end
  else
    Result := nil;
end;

function TCustomObjectDeserializer.SetObjectArrayProperty(Instance: TObject; const Name: string): TObject;
var
  PropInfo: PPropInfo;
  TypeInfo: PTypeInfo;
  PropClass: TClass;
  Collection: TCollection;
  List: TSerializedList;
begin
  PropInfo := GetPropInfo(Instance.ClassType, Name);
  if PropInfo <> nil then begin
    TypeInfo := PropInfo.PropType{$IFNDEF FPC}^{$ENDIF};

    case TypeInfo.Kind of
      tkClass: begin
        PropClass := GetObjectPropClass(Instance, Name);
        if PropClass.InheritsFrom(TCollection) then begin
          Collection := GetObjectProp(Instance, Name) as TCollection;
          if Collection <> nil then
            Result := Collection.Add
          else
            raise JSONException.CreateFmt(SPropertyNotInitialized, [Instance.ClassName, Name]);
        end
        else
        if PropClass.InheritsFrom(TSerializedList) then begin
          List := GetObjectProp(Instance, Name) as TSerializedList;
          if List <> nil then begin
            Result := List.ItemClass.Create;
            List.Add(Result);
          end
          else
            raise JSONException.CreateFmt(SPropertyNotInitialized, [Instance.ClassName, Name]);
        end
        else
          raise JSONException.Create(SJSONInternalError);

        SetObjectProp(Instance, Name, Result);
      end;
    else
      raise JSONException.CreateFmt(SDataMismatch, [Instance.ClassName, Name]);
    end;
  end
  else
    Result := nil;
end;

procedure TCustomObjectDeserializer.SetNullProperty(Instance: TObject; const Name: string);
var
  Obj: TObject;
  PropInfo: PPropInfo;
  TypeInfo: PTypeInfo;
  TypeKind: TTypeKind;
  PropClass: TClass;
  ArrPtr: pointer;
  Size: NativeInt;
begin
  PropInfo := GetPropInfo(Instance.ClassType, Name);
  if PropInfo <> nil then begin
    TypeInfo := PropInfo.PropType{$IFNDEF FPC}^{$ENDIF};
    TypeKind := TypeInfo.Kind;

    if TypeKind = tkClass then begin
      Obj := GetObjectProp(Instance, PropInfo);
      PropClass := GetObjectPropClass(Instance, Name);

      if Obj <> nil then begin
        if PropClass.InheritsFrom(TNullable) then
          TNullable(Obj).IsNull := True
        else
          Obj.Free;
      end
      else if PropClass.InheritsFrom(TNullable) then begin
        Obj := TNullableClass(PropClass).Create;
        TNullable(Obj).IsNull := True
      end;
      SetObjectProp(Instance, Name, Obj);
    end
    else if TypeKind = tkDynArray then begin
      ArrPtr := PtrOffset(PByte(Instance), NativeInt(PropInfo.GetProc) and not PROPSLOT_MASK);
      Size := 0;
      DynArraySetLength(PPointer(ArrPtr)^, TypeInfo, 1, @Size);
    end
    else
      raise JSONException.CreateFmt(SDataMismatch, [Instance.ClassName, Name]);
  end;
end;

procedure TCustomObjectDeserializer.SetNullArrayProperty(Instance: TObject; const Name: string; Index: integer);
begin
  raise JSONException.CreateFmt(SDataMismatch, [Instance.ClassName, Name]);
end;

{ TTextToObjectDeserializer }

constructor TTextToObjectDeserializer.Create;
begin
  inherited;

  FReader := nil;
end;

destructor TTextToObjectDeserializer.Destroy;
begin
  FreeReader;

  inherited;
end;

procedure TTextToObjectDeserializer.InitReader(ReaderClass: TJSONReaderClass);
begin
  if (FReader = nil) or (FReader.ClassType <> ReaderClass) then begin
    FreeReader;
    FReader := ReaderClass.Create;
  end;
end;

procedure TTextToObjectDeserializer.FreeReader;
begin
  FreeAndNil(FReader);
end;

procedure TTextToObjectDeserializer.ProcessTag(const Tag: TJSONTag; Parent: TObject;
  const Name: string; Index: integer);
begin
  case Tag of
    jtObject: ProcessObject(Parent, Name, Index);
    jtArray: ProcessArray(Parent, Name);
    jtString: ProcessString(Parent, Name, Index);
    jtNumber: ProcessNumber(Parent, Name, Index);
    jtBoolean: ProcessBoolean(Parent, Name, Index);
    jtNull: ProcessNull(Parent, Name, Index);
  else
    raise JSONException.Create(SJSONInternalError);
  end;
end;

procedure TTextToObjectDeserializer.ProcessObject(Parent: TObject; const Name: string; Index: integer);
var
  Obj: TObject;
begin
  if Parent <> nil then begin
    if Index < 0 then
      Obj := SetObjectProperty(Parent, Name)
    else
      Obj := SetObjectArrayProperty(Parent, Name);
  end
  else
    Obj := nil;

  ProcessObjectProps(Obj);
end;

procedure TTextToObjectDeserializer.ProcessObjectProps(Parent: TObject);
var
  Tag: TJSONTag;
begin
  while True do begin
    if ProcessPair(Parent) then begin
      Tag := FReader.ReadTag;
      if Tag = jtObjectEnd then
        Exit;
      if Tag = jtComma then
        Continue;
    end
    else
      Break;
  end;
end;

function TTextToObjectDeserializer.ProcessPair(Parent: TObject): boolean;
var
  Name: string;
  Tag: TJSONTag;
  Size: integer;
begin
  Result := False;

  Name := FReader.ReadString(False, False, Size);
  if Name = '' then
    Exit;

  if not FReader.ReadValueSeparator then
    Exit;

  Tag := FReader.ReadTag;
  if Tag = jtNone then
    Exit;

  ProcessTag(Tag, Parent, Name, -1);
  Result := True;
end;

procedure TTextToObjectDeserializer.ProcessArray(Parent: TObject; const Name: string);
var
  Tag: TJSONTag;
  Index: integer;
begin
  Index := 0;

  while True do begin
    Tag := FReader.ReadTag;
    if Tag in [jtNone, jtArrayEnd] then
      Exit;
    if Tag = jtComma then
      Continue;

    ProcessTag(Tag, Parent, Name, Index);
    Inc(Index);
  end;
end;

procedure TTextToObjectDeserializer.ProcessNumber(Parent: TObject; const Name: string; Index: integer);
var
  Value: string;
  Subtype: TJSONTag;
begin
  Value := FReader.ReadNumber(Subtype);
  if Parent <> nil then
    if Index < 0 then
      SetNumberProperty(Parent, Name, Value)
    else
      SetNumberArrayProperty(Parent, Name, Index, Value);
end;

procedure TTextToObjectDeserializer.ProcessString(Parent: TObject; const Name: string; Index: integer);
var
  Value: string;
  Size: integer;
begin
  Value := FReader.ReadString(False, True, Size);
  if Parent <> nil then
    if Index < 0 then
      SetStringProperty(Parent, Name, Value)
    else
      SetStringArrayProperty(Parent, Name, Index, Value);
end;

procedure TTextToObjectDeserializer.ProcessBoolean(Parent: TObject; const Name: string; Index: integer);
var
  Value: boolean;
begin
  Value := FReader.ReadBoolean;
  if Parent <> nil then
    if Index < 0 then
      SetBooleanProperty(Parent, Name, Value)
    else
      SetBooleanArrayProperty(Parent, Name, Index, Value);
end;

procedure TTextToObjectDeserializer.ProcessNull(Parent: TObject; const Name: string; Index: integer);
begin
  if Parent <> nil then
    if Index < 0 then
      SetNullProperty(Parent, Name)
    else
      SetNullArrayProperty(Parent, Name, Index);
end;

function TTextToObjectDeserializer.ToObject(const Text: string; ObjectClass: TClass): TObject;
var
  Tag: TJSONTag;
begin
{$IFNDEF VER25P}
  Result := nil;
 {$ENDIF} 
  InitReader(TJSONTextReader);
  TJSONTextReader(FReader).SetText(Text);

  FReader.Initialize;
  try
    Tag := FReader.ReadTag;
    if Tag <> jtObject then
      raise JSONException.Create(SNotObject);

    Result := CreateObject(ObjectClass);
    try
      ProcessObjectProps(Result);
    except
      Result.Free;
      raise;
    end;
  finally
    FReader.Finalize;
  end;
end;

function TTextToObjectDeserializer.ToList(const Text: string; ObjectClass: TSerializedClass): TSerializedList;
var
  Obj: TSerializedObject;
  Tag: TJSONTag;
begin
  Result := nil;

  InitReader(TJSONTextReader);
  TJSONTextReader(FReader).SetText(Text);

  FReader.Initialize;
  try
    Tag := FReader.ReadTag;
    if Tag = jtArray then begin
      Result := TSerializedList.Create(ObjectClass);
      try
        repeat
          Tag := FReader.ReadTag;

          case Tag of
            jtObject: begin
              Obj := ObjectClass.Create;
              try
                ProcessObjectProps(Obj);
                Result.Add(Obj);
              except
                Obj.Free;
                raise;
              end;
            end;
            jtComma:
              Continue;
            jtArrayEnd:
              Break;
          else
            raise JSONException.Create(SNotList);
          end;
        until False;
      except
        FreeAndNil(Result);
        raise;
      end;
    end
    else
      raise JSONException.Create(SNotList);
  finally
    FReader.Finalize;
  end;
end;

{ TJSONToObjectDeserializer }

function TJSONToObjectDeserializer.ToObject(JSONValue: TJSONValue; ObjectClass: TClass): TObject;
begin
  if not (JSONValue is TJSONObject) then
    raise ArgumentException.Create(SNotObject);

  Result := CreateObject(ObjectClass);
  try
    ProcessObjectProps(Result, TJSONObject(JSONValue));
  except
    Result.Free;
    raise;
  end;
end;

function TJSONToObjectDeserializer.ToVariant(JSONValue: TJSONValue; VarType: TVarType; ObjectClass: TClass): Variant;
begin
  if JSONValue is TJSONNull then begin
    Result := Null;
    Exit;
  end;

  case VarType of
    varNull: begin
      if JSONValue is TJSONNull then
        Result := Null
      else
        raise InvalidDataException.CreateFmt(SUnexpectedJsonTokenType, [JSONValue.ClassName, 'TJSONNull']);
    end;
    varBoolean: begin
      if JSONValue is TJSONBoolean then
        Result := TJSONBoolean(JSONValue).Value
      else
        raise InvalidDataException.CreateFmt(SUnexpectedJsonTokenType, [JSONValue.ClassName, 'TJSONBoolean']);
    end;
    varSmallInt, varInteger, varShortInt, varByte, varWord, varLongWord: begin
      if JSONValue is TJSONInt32 then
        Result := TJSONInt32(JSONValue).Value
      else
      if (JSONValue is TJSONNumber) or (JSONValue is TJSONDecimal128) then begin
        try
          Result := StrToInt(JSONValue.AsString);
        except
          raise InvalidDataException.CreateFmt(SUnexpectedJsonTokenType, [JSONValue.ClassName, 'TJSONInt32']);
        end;
      end
      else
        raise InvalidDataException.CreateFmt(SUnexpectedJsonTokenType, [JSONValue.ClassName, 'TJSONInt32']);
    end;
    varInt64{$IFDEF USE_UINT64}, varUInt64{$ENDIF}: begin
      if JSONValue is TJSONInt32 then
        Result := TJSONInt32(JSONValue).Value
      else
      if JSONValue is TJSONInt64 then
        Result := TJSONInt64(JSONValue).Value
      else
      if (JSONValue is TJSONNumber) or (JSONValue is TJSONDecimal128) then begin
        try
          Result := StrToInt64(JSONValue.AsString);
        except
          raise InvalidDataException.CreateFmt(SUnexpectedJsonTokenType, [JSONValue.ClassName, 'TJSONInt64']);
        end;
      end
      else
        raise InvalidDataException.CreateFmt(SUnexpectedJsonTokenType, [JSONValue.ClassName, 'TJSONInt64']);
    end;
    varSingle, varDouble, varCurrency: begin
      if JSONValue is TJSONDouble then
        Result := TJSONDouble(JSONValue).Value
      else
      if (JSONValue is TJSONNumber) or (JSONValue is TJSONDecimal128) then begin
        try
          Result := StrToFloat(JSONValue.AsString);
        except
          raise InvalidDataException.CreateFmt(SUnexpectedJsonTokenType, [JSONValue.ClassName, 'TJSONDouble']);
        end;
      end
      else
        raise InvalidDataException.CreateFmt(SUnexpectedJsonTokenType, [JSONValue.ClassName, 'TJSONDouble']);
    end;
    varDate: begin
      if JSONValue is TJSONDateTime then
        Result := TJSONDateTime(JSONValue).Value
      else
      if JSONValue is TJSONString then begin
        try
          Result := ISO8601ToDateTime(JSONValue.AsString);
        except
          try
            Result := StrToDateTime(JSONValue.AsString);
          except
            raise InvalidDataException.CreateFmt(SUnexpectedJsonTokenType, [JSONValue.ClassName, 'TJSONDateTime']);
          end;
        end;
      end
      else
        raise InvalidDataException.CreateFmt(SUnexpectedJsonTokenType, [JSONValue.ClassName, 'TJSONDateTime']);
    end;
    varByRef: begin
      if JSONValue is TJSONObject then begin
        TVarData(Result).VType := varByRef;
        TVarData(Result).VPointer := ToObject(JSONValue, ObjectClass);
      end
      else
        raise InvalidDataException.CreateFmt(SUnexpectedJsonTokenType, [JSONValue.ClassName, 'TJSONObject']);
    end;
    varString:
      Result := JSONValue.AsString;
  else
    if JSONValue is TJSONString then
      Result := JSONValue.AsString
    else
      raise InvalidDataException.CreateFmt(SUnexpectedJsonTokenType, [JSONValue.ClassName, 'TJSONString']);
  end;
end;

procedure TJSONToObjectDeserializer.ProcessValue(Parent: TObject; const Name: string;
  JSONValue: TJSONValue; Index: integer);
begin
  if JSONValue is TJSONObject then
    ProcessObject(Parent, Name, TJSONObject(JSONValue), Index)
  else
  if JSONValue is TJSONArray then
    ProcessArray(Parent, Name, TJSONArray(JSONValue))
  else
  if JSONValue is TJSONNumber then
    ProcessNumber(Parent, Name, TJSONNumber(JSONValue), Index)
  else
  if JSONValue is TJSONString then
    ProcessString(Parent, Name, TJSONString(JSONValue), Index)
  else
  if JSONValue is TJSONBoolean then
    ProcessBoolean(Parent, Name, TJSONBoolean(JSONValue), Index)
  else
  if JSONValue is TJSONNull then
    ProcessNull(Parent, Name, TJSONNull(JSONValue), Index)
  else
    ProcessString(Parent, Name, JSONValue.AsString, Index)
end;

procedure TJSONToObjectDeserializer.ProcessObject(Parent: TObject; const Name: string;
  JSONObject: TJSONObject; Index: integer);
var
  Obj: TObject;
begin
  if Parent <> nil then begin
    if Index < 0 then
      Obj := SetObjectProperty(Parent, Name)
    else
      Obj := SetObjectArrayProperty(Parent, Name);
  end
  else
    Obj := nil;

  ProcessObjectProps(Obj, JSONObject);
end;

procedure TJSONToObjectDeserializer.ProcessObjectProps(Parent: TObject; JSONObject: TJSONObject);
var
  JSONPair: TJSONPair;
  Index: integer;
begin
  for Index := 0 to JSONObject.Pairs.Count - 1 do begin
    JSONPair := JSONObject.Pairs[Index];
    ProcessValue(Parent, JSONPair.Name.Value, JSONPair.Value, -1);
  end;
end;

procedure TJSONToObjectDeserializer.ProcessArray(Parent: TObject; const Name: string; JSONValue: TJSONArray);
var
  Index: integer;
begin
  for Index := 0 to JSONValue.Elements.Count - 1 do
    ProcessValue(Parent, Name, JSONValue.Elements[Index], Index);
end;

procedure TJSONToObjectDeserializer.ProcessNumber(Parent: TObject; const Name: string;
  JSONValue: TJSONNumber; Index: integer);
begin
  if Parent <> nil then
    if Index < 0 then
      SetNumberProperty(Parent, Name, JSONValue.Value)
    else
      SetNumberArrayProperty(Parent, Name, Index, JSONValue.Value);
end;

procedure TJSONToObjectDeserializer.ProcessString(Parent: TObject; const Name: string;
  JSONValue: TJSONString; Index: integer);
begin
  if Parent <> nil then
    if Index < 0 then
      SetStringProperty(Parent, Name, JSONValue.Value)
    else
      SetStringArrayProperty(Parent, Name, Index, JSONValue.Value);
end;

procedure TJSONToObjectDeserializer.ProcessString(Parent: TObject; const Name, Value: string; Index: integer);
begin
  if Parent <> nil then
    if Index < 0 then
      SetStringProperty(Parent, Name, Value)
    else
      SetStringArrayProperty(Parent, Name, Index, Value);
end;

procedure TJSONToObjectDeserializer.ProcessBoolean(Parent: TObject; const Name: string;
  JSONValue: TJSONBoolean; Index: integer);
begin
  if Parent <> nil then
    if Index < 0 then
      SetBooleanProperty(Parent, Name, JSONValue.Value)
    else
      SetBooleanArrayProperty(Parent, Name, Index, JSONValue.Value);
end;

procedure TJSONToObjectDeserializer.ProcessNull(Parent: TObject; const Name: string;
  JSONValue: TJSONNull; Index: integer);
begin
  if Parent <> nil then
    if Index < 0 then
      SetNullProperty(Parent, Name)
    else
      SetNullArrayProperty(Parent, Name, Index);
end;

{ TObjectSerializer }

constructor TObjectSerializer.Create;
begin
  inherited Create;

  FWriter := nil;
end;

destructor TObjectSerializer.Destroy;
begin
  FreeWriter;

  inherited;
end;

procedure TObjectSerializer.InitWriter(const WriterClass: TJSONWriterClass);
begin
  if (FWriter = nil) or (FWriter.ClassType <> WriterClass) then begin
    FreeWriter;
    FWriter := WriterClass.Create;
    FWriterOwner := True;
  end
  else
    FWriter.Clear;
end;

procedure TObjectSerializer.FreeWriter;
begin
  if FWriterOwner then
    FreeAndNil(FWriter);
end;

procedure TObjectSerializer.ProcessVariant(const Value: Variant);
begin
  if VarIsNull(Value) or VarIsEmpty(Value) then
    FWriter.WriteNull
  else begin
    case TVarData(Value).VType of
      varBoolean:
        FWriter.WriteBoolean(Value);
      varSmallInt, varInteger, varShortInt, varByte, varWord, varLongWord:
        FWriter.WriteInt32(Value);
      varInt64{$IFDEF USE_UINT64}, varUInt64{$ENDIF}:
        FWriter.WriteInt64(Value);
      varSingle, varDouble, varCurrency:
        FWriter.WriteDouble(Value);
      varDate:
        FWriter.WriteString(DateTimeToISO8601(TDateTime(Value)));
      varByRef:
        ProcessObject(TObject(TVarData(Value).VPointer));
      else
        FWriter.WriteString(Value);
    end;
  end;
end;

procedure TObjectSerializer.ProcessObject(Value: TObject);
var
  i, n: integer;
  PropList: PPropList;
begin
  FWriter.WriteObjectBegin;

  if Value <> nil then begin
    n := GetPropList(Value, PropList);
    if n > 0 then
      try
        for i := 0 to n - 1 do begin
          if i > 0 then
            FWriter.WriteElementSeparator;
          ProcessProperty(Value, PropList^[i]);
        end;
      finally
        FreeMem(PropList);
      end;
  end;

  FWriter.WriteObjectEnd;
end;

procedure TObjectSerializer.ProcessArray(Parent: TObject; const PropInfo: PPropInfo);
var
  i, n: integer;
  ArrPtr: pointer;
  TypeInfo: PTypeInfo;
  TypeData: PTypeData;
  ItemInfo: PTypeInfo;
  ItemPtr: pointer;
  Strings: TStrings;
  List: TSerializedList;
begin
  FWriter.WriteArrayBegin;

  TypeInfo := PropInfo.PropType{$IFNDEF FPC}^{$ENDIF};
  if TypeInfo.Kind = tkDynArray then begin
    ArrPtr := PtrOffset(PByte(Parent), NativeInt(PropInfo.GetProc) and not PROPSLOT_MASK);
    n := DynArraySize(PPointer(ArrPtr)^);
    TypeData := GetTypeData(TypeInfo);
    ItemInfo := TypeData.elType2{$IFNDEF FPC}^{$ENDIF};

    for i := 0 to n - 1 do begin
      ItemPtr := PtrOffset(PPointer(ArrPtr)^, i * TypeData.elSize);

      case ItemInfo.Kind of
        tkInteger:
          FWriter.WriteInt32(integer(ItemPtr^));
        tkInt64:
          FWriter.WriteInt64(Int64(ItemPtr^), False);
        tkFloat:
          if TypeInfo.{$IFNDEF NEXTGEN}Name{$ELSE}NameFld.ToString{$ENDIF} <> 'TDateTime' then
            FWriter.WriteDouble(double(ItemPtr^))
          else
            raise JSONException.CreateFmt(SCanNotSerialize, [Parent.ClassName, TypeInfo.{$IFNDEF NEXTGEN}Name{$ELSE}NameFld.ToString{$ENDIF}]);
      {$IFDEF FPC}
        tkBool,
      {$ENDIF}
        tkEnumeration:
          FWriter.WriteBoolean(StrToBool(GetEnumProp(Parent, PropInfo)));
      else
        raise JSONException.CreateFmt(SCanNotSerialize, [Parent.ClassName, TypeInfo.{$IFNDEF NEXTGEN}Name{$ELSE}NameFld.ToString{$ENDIF}]);
      end;

      if i < (n - 1) then
        FWriter.WriteElementSeparator;
    end;
  end
  else if GetObjectPropClass(Parent, string(PropInfo.{$IFNDEF NEXTGEN}Name{$ELSE}NameFld.ToString{$ENDIF})).InheritsFrom(TStrings) then begin
    Strings := GetObjectProp(Parent, PropInfo) as TStrings;

    if Strings <> nil then begin
      n := Strings.Count;;

      for i := 0 to n - 1 do begin
        FWriter.WriteString(Strings[i], False, True);

        if i < (n - 1) then
          FWriter.WriteElementSeparator;
      end;
    end;
  end
  else if GetObjectPropClass(Parent, string(PropInfo.{$IFNDEF NEXTGEN}Name{$ELSE}NameFld.ToString{$ENDIF})).InheritsFrom(TSerializedList) then begin
    List := GetObjectProp(Parent, PropInfo) as TSerializedList;
    n := List.Count;

    for i := 0 to n - 1 do begin
      ProcessObject(List.Items[i]);

      if i < (n - 1) then
        FWriter.WriteElementSeparator;
    end;
  end;

  FWriter.WriteArrayEnd;
end;

procedure TObjectSerializer.ProcessProperty(Parent: TObject; const PropInfo: PPropInfo);
var
  TypeInfo: PTypeInfo;
  Obj: TObject;
begin
  ProcessString(string(PropInfo.{$IFNDEF NEXTGEN}Name{$ELSE}NameFld.ToString{$ENDIF}), False);
  FWriter.WriteValueSeparator;

  TypeInfo := PropInfo.PropType{$IFNDEF FPC}^{$ENDIF};
  case TypeInfo.Kind of
  {$IFDEF FPC}
    tkAString,
  {$ENDIF}
    tkString,
    tkLString:
      FWriter.WriteString(GetStrProp(Parent, PropInfo), False, True);
  {$IFDEF VER12P}
    tkUString,
  {$ENDIF}
    tkWString:
      FWriter.WriteWideString({$IFNDEF NEXTGEN}GetWideStrProp{$ELSE}GetStrProp{$ENDIF}(Parent, PropInfo), False, True);
    tkInteger:
      FWriter.WriteInt32(GetOrdProp(Parent, PropInfo));
    tkInt64:
      FWriter.WriteInt64(GetInt64Prop(Parent, PropInfo), False);
    tkFloat:
      if TypeInfo.{$IFNDEF NEXTGEN}Name{$ELSE}NameFld.ToString{$ENDIF} = 'TDateTime' then
        FWriter.WriteString(DateTimeToISO8601(GetFloatProp(Parent, PropInfo)), False, False)
      else
        FWriter.WriteDouble(GetFloatProp(Parent, PropInfo));
  {$IFDEF FPC}
    tkBool,
  {$ENDIF}
    tkEnumeration:
      if Trim(LowerCase(string(TypeInfo.{$IFNDEF NEXTGEN}Name{$ELSE}NameFld.ToString{$ENDIF}))) = 'boolean' then
        FWriter.WriteBoolean(StrToBool(GetEnumProp(Parent, PropInfo)))
      else
        FWriter.WriteInt32(GetOrdProp(Parent, PropInfo));
    tkClass: begin
      if GetObjectPropClass(Parent, string(PropInfo.{$IFNDEF NEXTGEN}Name{$ELSE}NameFld.ToString{$ENDIF})).InheritsFrom(TStrings)
      or GetObjectPropClass(Parent, string(PropInfo.{$IFNDEF NEXTGEN}Name{$ELSE}NameFld.ToString{$ENDIF})).InheritsFrom(TSerializedList) then
        ProcessArray(Parent, PropInfo)
      else begin
        Obj := GetObjectProp(Parent, PropInfo);
        if Obj <> nil then begin
          if Obj is TSerializedObject then
            ProcessObject(Obj as TSerializedObject)
          else if TypeInfo.{$IFNDEF NEXTGEN}Name{$ELSE}NameFld.ToString{$ENDIF} = 'IntegerNullable' then begin
            if not IntegerNullable(Obj).IsNull then
              FWriter.WriteInt64(GetInt64Prop(Obj, GetPropInfo(Obj, 'Value')), False)
            else
              FWriter.WriteNull;
          end
          else if TypeInfo.{$IFNDEF NEXTGEN}Name{$ELSE}NameFld.ToString{$ENDIF} = 'DoubleNullable' then begin
            if not DoubleNullable(Obj).IsNull then
              FWriter.WriteDouble(GetFloatProp(Obj, GetPropInfo(Obj, 'Value')))
            else
              FWriter.WriteNull;
          end
          else if TypeInfo.{$IFNDEF NEXTGEN}Name{$ELSE}NameFld.ToString{$ENDIF} = 'StringNullable' then begin
            if not StringNullable(Obj).IsNull then
              FWriter.WriteString(GetStrProp(Obj, GetPropInfo(Obj, 'Value')), False, True)
            else
              FWriter.WriteNull;
          end
          else if TypeInfo.{$IFNDEF NEXTGEN}Name{$ELSE}NameFld.ToString{$ENDIF} = 'DateTimeNullable' then begin
            if not DateTimeNullable(Obj).IsNull then
              FWriter.WriteString(DateTimeToISO8601(GetFloatProp(Obj, GetPropInfo(Obj, 'Value'))), False, False)
            else
              FWriter.WriteNull;
          end
          else if TypeInfo.{$IFNDEF NEXTGEN}Name{$ELSE}NameFld.ToString{$ENDIF} = 'BooleanNullable' then begin
            if not BooleanNullable(Obj).IsNull then
              FWriter.WriteBoolean(StrToBool(GetEnumProp(Obj, GetPropInfo(Obj, 'Value'))))
            else
              FWriter.WriteNull;
          end
          else if TypeInfo.{$IFNDEF NEXTGEN}Name{$ELSE}NameFld.ToString{$ENDIF} = 'TIntArrNullable' then begin
            if not TIntArrNullable(Obj).IsNull then
              ProcessArray(Obj, GetPropInfo(Obj, 'Value'))
            else
              FWriter.WriteNull;
          end
          else
            raise JSONException.CreateFmt(SCanNotSerialize, [Parent.ClassName, PropInfo.{$IFNDEF NEXTGEN}Name{$ELSE}NameFld.ToString{$ENDIF}]);
        end
        else
          FWriter.WriteNull;
      end;
    end;
    tkDynArray: ProcessArray(Parent, PropInfo);
  else
    raise JSONException.CreateFmt(SCanNotSerialize, [Parent.ClassName, PropInfo.{$IFNDEF NEXTGEN}Name{$ELSE}NameFld.ToString{$ENDIF}]);
  end;
end;

procedure TObjectSerializer.ProcessString(const Value: string; const Escape: boolean);
begin
  FWriter.WriteString(Value, False, Escape);
end;

function TObjectSerializer.ToText(Value: TObject): string;
begin
  InitWriter(TJSONTextWriter);

  ProcessObject(Value);

  Result := TJSONTextWriter(FWriter).AsText;
end;

function TObjectSerializer.ToText(Value: TList): string;
  var
  i, n: integer;
begin
  InitWriter(TJSONTextWriter);

  FWriter.WriteArrayBegin;

  n := Value.Count;
  if n > 0 then
    for i := 0 to n - 1 do begin
      ProcessObject(Value[i]);

      if i < (n - 1) then
        FWriter.WriteElementSeparator;
    end;

  FWriter.WriteArrayEnd;

  Result := TJSONTextWriter(FWriter).AsText;
end;

procedure TObjectSerializer.WriteVariant(Writer: TJSONTextWriter; const Value: Variant);
begin
  if FWriter <> Writer then
    FreeWriter;

  FWriter := Writer;
  FWriterOwner := False;

  ProcessVariant(Value);
end;

procedure TObjectSerializer.WriteObject(Writer: TJSONTextWriter; Value: TObject);
begin
  if FWriter <> Writer then
    FreeWriter;

  FWriter := Writer;
  FWriterOwner := False;

  ProcessObject(Value);
end;

end.

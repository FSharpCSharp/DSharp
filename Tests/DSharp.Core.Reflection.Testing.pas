unit DSharp.Core.Reflection.Testing;

interface

uses
  Classes,
  TestFramework;

implementation

uses
//  DSharp.Collections,
  DateUtils,
  DSharp.Core.Reflection,
  Generics.Collections,
  Messages,
  Rtti,
  SysUtils;

var
  Context: TRttiContext;

type
  TRttiTypeHelperTestCase = class(TTestCase)
  published
    procedure GetGenericArguments_TListOfTObject;
    procedure GetGenericArguments_TObjectListOfTObject;
    procedure GetGenericArguments_TDictionaryOfStringTObject;

    procedure GetGenericTypeDefinition_TListOfTObject;
    procedure GetGenericTypeDefinition_TObjectListOfTObject;
    procedure GetGenericTypeDefinition_TDictionaryOfStringTObject;

    procedure IsCovariantTo_TObject_TObject_True;
    procedure IsCovariantTo_TObject_TPersistent_False;

    procedure IsCovariantTo_TPersistent_TObject_True;

    procedure IsCovariantTo_TListOfTObject_TObject_True;
    procedure IsCovariantTo_TListOfTObject_TPersistent_False;
    procedure IsCovariantTo_TListOfTObject_TListOfTObject_True;

    procedure IsCovariantTo_TListOfTPersistent_TListOfTObject_True;

    procedure IsCovariantTo_TObjectListOfTObject_TObject_True;
    procedure IsCovariantTo_TObjectListOfTObject_TListOfTObject_True;
    procedure IsCovariantTo_TObjectListOfTObject_TObjectListOfTObject_True;

    procedure IsCovariantTo_TObjectListOfTObject_TPersistent_False;
    procedure IsCovariantTo_TObjectListOfTObject_TListOfTPersistent_False;

    procedure IsCovariantTo_TObjectListOfTPersistent_TListOfTObject_True;
    procedure IsCovariantTo_TObjectListOfTPersistent_TListOfTPersistent_True;
    procedure IsCovariantTo_TObjectListOfTPersistent_TObjectListOfTObject_True;

    procedure IsGenericTypeOf_TListOfTObject_TList_True;
    procedure IsGenericTypeOf_TListOfTObject_TObjectList_False;
  end;

  TValueHelperTestCase = class(TTestCase)
  private
    FEventCallCount: Integer;
    procedure TestEvent(Sender: TObject);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure IsFloat_ValueIsString_False;
    procedure IsFloat_ValueIsInteger_False;
    procedure IsFloat_ValueIsDouble_True;

    procedure IsNumeric_ValueIsString_False;
    procedure IsNumeric_ValueIsInteger_True;
    procedure IsNumeric_ValueIsDouble_True;
    procedure IsNumeric_ValueIsBoolean_True;
    procedure IsNumeric_ValueIsChar_True;

    procedure IsString_ValueIsString_True;
    procedure IsString_ValueIsInteger_False;
    procedure IsString_ValueIsDouble_False;
    procedure IsString_ValueIsChar_True;

    procedure TryCastEx_ValueIsString_TypeIsString_True;
    procedure TryCastEx_ValueIsString_TypeIsInteger_True;
    procedure TryCastEx_ValueIsString_TypeIsDouble_True;
    procedure TryCastEx_ValueIsString_TypeIsChar_True;
    procedure TryCastEx_ValueIsString_TypeIsBoolean_True;
    procedure TryCastEx_ValueIsString_TypeIsDateTime_True;

    procedure TryCastEx_ValueIsInteger_TypeIsString_True;
    procedure TryCastEx_ValueIsInteger_TypeIsInteger_True;
    procedure TryCastEx_ValueIsInteger_TypeIsDouble_True;
//    procedure TryCastEx_ValueIsInteger_TypeIsChar_True;
//    procedure TryCastEx_ValueIsInteger_TypeIsBoolean_True;
//    procedure TryCastEx_ValueIsInteger_TypeIsDateTime_True;

    procedure TryCastEx_ValueIsDouble_TypeIsString_True;
    procedure TryCastEx_ValueIsDouble_TypeIsInteger_True;
    procedure TryCastEx_ValueIsDouble_TypeIsDouble_True;

    procedure TryCastEx_ValueIsBoolean_TypeIsString_True;
    procedure TryCastEx_ValueIsBoolean_TypeIsInteger_True;
    procedure TryCastEx_ValueIsBoolean_TypeIsDouble_True;
    procedure TryCastEx_ValueIsBoolean_TypeIsBoolean_True;

    procedure TryCastEx_ValueIsDateTime_TypeIsString_True;
    procedure TryCastEx_ValueIsDateTime_TypeIsDouble_True;
    procedure TryCastEx_ValueIsDateTime_TypeIsDateTime_True;

    procedure TryCastEx_ValueIsEvent_TypeIsMethod_True;

    procedure TryCastEx_ValueIsObject_TypeIsBoolean_True;
  end;

{ TRttiTypeHelperTestCase }

procedure TRttiTypeHelperTestCase.GetGenericArguments_TDictionaryOfStringTObject;
var
  args: TArray<TRttiType>;
begin
  args := Context.GetType(TDictionary<string,TObject>).GetGenericArguments;
  CheckEquals(2, Length(args));
  Check(TypeInfo(string) = args[0].Handle);
  Check(TObject.ClassInfo = args[1].Handle);
end;

procedure TRttiTypeHelperTestCase.GetGenericArguments_TListOfTObject;
var
  args: TArray<TRttiType>;
begin
  args := Context.GetType(TList<TObject>).GetGenericArguments;
  CheckEquals(1, Length(args));
  Check(TObject.ClassInfo = args[0].Handle);
end;

procedure TRttiTypeHelperTestCase.GetGenericArguments_TObjectListOfTObject;
var
  args: TArray<TRttiType>;
begin
  args := Context.GetType(TObjectList<TObject>).GetGenericArguments;
  CheckEquals(1, Length(args));
  Check(TObject.ClassInfo = args[0].Handle);
end;

procedure TRttiTypeHelperTestCase.GetGenericTypeDefinition_TDictionaryOfStringTObject;
begin
  CheckEquals('TDictionary<T1,T2>', Context.GetType(TDictionary<string,TObject>).GetGenericTypeDefinition);
end;

procedure TRttiTypeHelperTestCase.GetGenericTypeDefinition_TListOfTObject;
begin
  CheckEquals('TList<T>', Context.GetType(TList<TObject>).GetGenericTypeDefinition);
end;

procedure TRttiTypeHelperTestCase.GetGenericTypeDefinition_TObjectListOfTObject;
begin
  CheckEquals('TObjectList<T>', Context.GetType(TObjectList<TObject>).GetGenericTypeDefinition);
end;

procedure TRttiTypeHelperTestCase.IsCovariantTo_TListOfTObject_TListOfTObject_True;
begin
  CheckTrue(Context.GetType(TList<TObject>).IsCovariantTo(TList<TObject>));
end;

procedure TRttiTypeHelperTestCase.IsCovariantTo_TListOfTObject_TObject_True;
begin
  CheckTrue(Context.GetType(TList<TObject>).IsCovariantTo(TObject));
end;

procedure TRttiTypeHelperTestCase.IsCovariantTo_TListOfTObject_TPersistent_False;
begin
  CheckFalse(Context.GetType(TList<TObject>).IsCovariantTo(TPersistent));
end;

procedure TRttiTypeHelperTestCase.IsCovariantTo_TListOfTPersistent_TListOfTObject_True;
begin
  CheckTrue(Context.GetType(TList<TPersistent>).IsCovariantTo(TList<TObject>));
end;

procedure TRttiTypeHelperTestCase.IsCovariantTo_TObjectListOfTObject_TListOfTObject_True;
begin
  CheckTrue(Context.GetType(TObjectList<TObject>).IsCovariantTo(TList<TObject>));
end;

procedure TRttiTypeHelperTestCase.IsCovariantTo_TObjectListOfTObject_TListOfTPersistent_False;
begin
  CheckFalse(Context.GetType(TObjectList<TObject>).IsCovariantTo(TList<TPersistent>));
end;

procedure TRttiTypeHelperTestCase.IsCovariantTo_TObjectListOfTObject_TObjectListOfTObject_True;
begin
  CheckTrue(Context.GetType(TObjectList<TObject>).IsCovariantTo(TObjectList<TObject>));
end;

procedure TRttiTypeHelperTestCase.IsCovariantTo_TObjectListOfTObject_TObject_True;
begin
  CheckTrue(Context.GetType(TObjectList<TObject>).IsCovariantTo(TObject));
end;

procedure TRttiTypeHelperTestCase.IsCovariantTo_TObjectListOfTObject_TPersistent_False;
begin
  CheckFalse(Context.GetType(TObjectList<TObject>).IsCovariantTo(TPersistent));
end;

procedure TRttiTypeHelperTestCase.IsCovariantTo_TObjectListOfTPersistent_TListOfTObject_True;
begin
  CheckTrue(Context.GetType(TObjectList<TPersistent>).IsCovariantTo(TList<TObject>));
end;

procedure TRttiTypeHelperTestCase.IsCovariantTo_TObjectListOfTPersistent_TListOfTPersistent_True;
begin
  CheckTrue(Context.GetType(TObjectList<TPersistent>).IsCovariantTo(TList<TPersistent>));
end;

procedure TRttiTypeHelperTestCase.IsCovariantTo_TObjectListOfTPersistent_TObjectListOfTObject_True;
begin
  CheckTrue(Context.GetType(TObjectList<TPersistent>).IsCovariantTo(TObjectList<TObject>));
end;

procedure TRttiTypeHelperTestCase.IsCovariantTo_TObject_TObject_True;
begin
  CheckTrue(Context.GetType(TObject).IsCovariantTo(TObject));
end;

procedure TRttiTypeHelperTestCase.IsCovariantTo_TObject_TPersistent_False;
begin
  CheckFalse(Context.GetType(TObject).IsCovariantTo(TPersistent));
end;

procedure TRttiTypeHelperTestCase.IsCovariantTo_TPersistent_TObject_True;
begin
  CheckTrue(Context.GetType(TPersistent).IsCovariantTo(TObject));
end;

procedure TRttiTypeHelperTestCase.IsGenericTypeOf_TListOfTObject_TList_True;
begin
  CheckTrue(Context.GetType(TList<TObject>).IsGenericTypeOf('TList'));
end;

procedure TRttiTypeHelperTestCase.IsGenericTypeOf_TListOfTObject_TObjectList_False;
begin
  CheckFalse(Context.GetType(TList<TObject>).IsGenericTypeOf('TObjectList'));
end;

{ TValueHelperTestCase }

procedure TValueHelperTestCase.IsFloat_ValueIsDouble_True;
begin
  CheckTrue(TValue.From<Double>(42.0).IsFloat);
end;

procedure TValueHelperTestCase.IsFloat_ValueIsInteger_False;
begin
  CheckFalse(TValue.From<Integer>(42).IsFloat);
end;

procedure TValueHelperTestCase.IsFloat_ValueIsString_False;
begin
  CheckFalse(TValue.From<string>('42').IsFloat);
end;

procedure TValueHelperTestCase.IsNumeric_ValueIsBoolean_True;
begin
  CheckTrue(TValue.From<Boolean>(True).IsNumeric);
end;

procedure TValueHelperTestCase.IsNumeric_ValueIsChar_True;
begin
  CheckTrue(TValue.From<Char>('4').IsNumeric);
end;

procedure TValueHelperTestCase.IsNumeric_ValueIsDouble_True;
begin
  CheckTrue(TValue.From<Double>(42.0).IsNumeric);
end;

procedure TValueHelperTestCase.IsNumeric_ValueIsInteger_True;
begin
  CheckTrue(TValue.From<Integer>(42).IsNumeric);
end;

procedure TValueHelperTestCase.IsNumeric_ValueIsString_False;
begin
  CheckFalse(TValue.From<string>('42').IsNumeric);
end;

procedure TValueHelperTestCase.IsString_ValueIsChar_True;
begin
  CheckTrue(TValue.From<Char>('4').IsString);
end;

procedure TValueHelperTestCase.IsString_ValueIsDouble_False;
begin
  CheckFalse(TValue.From<Double>(42.0).IsString);
end;

procedure TValueHelperTestCase.IsString_ValueIsInteger_False;
begin
  CheckFalse(TValue.From<Integer>(42).IsString);
end;

procedure TValueHelperTestCase.IsString_ValueIsString_True;
begin
  CheckTrue(TValue.From<string>('42').IsString);
end;

procedure TValueHelperTestCase.SetUp;
begin
  inherited;
  FEventCallCount := 0;
end;

procedure TValueHelperTestCase.TryCastEx_ValueIsString_TypeIsString_True;
var
  value: TValue;
begin
  CheckTrue(TValue.From<string>('42').TryCastEx(TypeInfo(string), value));
  Check(value.TypeInfo = TypeInfo(string));
  CheckEquals('42', value.AsString);
end;

procedure TValueHelperTestCase.TearDown;
begin
  inherited;
  FEventCallCount := 0;
end;

procedure TValueHelperTestCase.TestEvent(Sender: TObject);
begin
  Inc(FEventCallCount);
end;

procedure TValueHelperTestCase.TryCastEx_ValueIsBoolean_TypeIsBoolean_True;
var
  value: TValue;
begin
  CheckTrue(TValue.From<Boolean>(True).TryCastEx(TypeInfo(Boolean), value));
  Check(value.TypeInfo = TypeInfo(Boolean));
  CheckEquals(True, value.AsBoolean);
end;

procedure TValueHelperTestCase.TryCastEx_ValueIsBoolean_TypeIsDouble_True;
var
  value: TValue;
begin
  CheckTrue(TValue.From<Boolean>(True).TryCastEx(TypeInfo(Double), value));
  Check(value.TypeInfo = TypeInfo(Double));
  CheckEquals(1.0, value.AsExtended);
end;

procedure TValueHelperTestCase.TryCastEx_ValueIsBoolean_TypeIsInteger_True;
var
  value: TValue;
begin
  CheckTrue(TValue.From<Boolean>(True).TryCastEx(TypeInfo(Integer), value));
  Check(value.TypeInfo = TypeInfo(Integer));
  CheckEquals(1, value.AsInteger);
end;

procedure TValueHelperTestCase.TryCastEx_ValueIsBoolean_TypeIsString_True;
var
  value: TValue;
begin
  CheckTrue(TValue.From<Boolean>(True).TryCastEx(TypeInfo(string), value));
  Check(value.TypeInfo = TypeInfo(string));
  CheckEquals(BoolToStr(True), value.AsString);
end;

procedure TValueHelperTestCase.TryCastEx_ValueIsDateTime_TypeIsDateTime_True;
var
  value: TValue;
  datetime: TDateTime;
begin
  datetime := EncodeDateTime(2011, 7, 5, 15, 48, 0, 0);
  CheckTrue(TValue.From<TDateTime>(datetime).TryCastEx(TypeInfo(TDateTime), value));
  Check(value.TypeInfo = TypeInfo(TDateTime));
  CheckEquals(datetime, value.AsType<TDateTime>);
end;

procedure TValueHelperTestCase.TryCastEx_ValueIsDateTime_TypeIsDouble_True;
var
  value: TValue;
  datetime: TDateTime;
begin
  datetime := EncodeDateTime(2011, 7, 5, 15, 48, 0, 0);
  CheckTrue(TValue.From<TDateTime>(datetime).TryCastEx(TypeInfo(Double), value));
  Check(value.TypeInfo = TypeInfo(Double));
  CheckEquals(datetime, value.AsExtended);
end;

procedure TValueHelperTestCase.TryCastEx_ValueIsDateTime_TypeIsString_True;
var
  value: TValue;
  datetime: TDateTime;
begin
  datetime := EncodeDateTime(2011, 7, 5, 15, 48, 0, 0);
  CheckTrue(TValue.From<TDateTime>(datetime).TryCastEx(TypeInfo(string), value));
  Check(value.TypeInfo = TypeInfo(string));
  CheckEquals(DateTimeToStr(datetime), value.AsType<string>);
end;

procedure TValueHelperTestCase.TryCastEx_ValueIsDouble_TypeIsDouble_True;
var
  value: TValue;
begin
  CheckTrue(TValue.From<Double>(42.0).TryCastEx(TypeInfo(Double), value));
  Check(value.TypeInfo = TypeInfo(Double));
  CheckEquals(42.0, value.AsExtended);
end;

procedure TValueHelperTestCase.TryCastEx_ValueIsDouble_TypeIsInteger_True;
var
  value: TValue;
begin
  CheckTrue(TValue.From<Double>(42).TryCastEx(TypeInfo(Integer), value));
  Check(value.TypeInfo = TypeInfo(Integer));
  CheckEquals(42, value.AsInteger);
end;

procedure TValueHelperTestCase.TryCastEx_ValueIsDouble_TypeIsString_True;
var
  value: TValue;
begin
  CheckTrue(TValue.From<Double>(42).TryCastEx(TypeInfo(string), value));
  Check(value.TypeInfo = TypeInfo(string));
  CheckEquals('42', value.AsString);
end;

procedure TValueHelperTestCase.TryCastEx_ValueIsEvent_TypeIsMethod_True;
var
  value: TValue;
  method: TMethod;
  event: TNotifyEvent;
begin
  method.Code := @TValueHelperTestCase.TestEvent;
  method.Data := Self;
  CheckTrue(TValue.From<TMethod>(method).TryCastEx(TypeInfo(TNotifyEvent), value));
  Check(value.TypeInfo = TypeInfo(TNotifyEvent));
  event := value.AsType<TNotifyEvent>();
  Check(TMethod(event).Code = method.Code);
  event(Self);
  CheckEquals(1, FEventCallCount);
end;

procedure TValueHelperTestCase.TryCastEx_ValueIsInteger_TypeIsDouble_True;
var
  value: TValue;
begin
  CheckTrue(TValue.From<Integer>(42).TryCastEx(TypeInfo(Double), value));
  Check(value.TypeInfo = TypeInfo(Double));
  CheckEquals(42.0, value.AsExtended);
end;

procedure TValueHelperTestCase.TryCastEx_ValueIsInteger_TypeIsInteger_True;
var
  value: TValue;
begin
  CheckTrue(TValue.From<Integer>(42).TryCastEx(TypeInfo(Integer), value));
  Check(value.TypeInfo = TypeInfo(Integer));
  CheckEquals(42.0, value.AsInteger);
end;

procedure TValueHelperTestCase.TryCastEx_ValueIsInteger_TypeIsString_True;
var
  value: TValue;
begin
  CheckTrue(TValue.From<Integer>(42).TryCastEx(TypeInfo(string), value));
  Check(value.TypeInfo = TypeInfo(string));
  CheckEquals('42', value.AsString);
end;

procedure TValueHelperTestCase.TryCastEx_ValueIsObject_TypeIsBoolean_True;
var
  value: TValue;
begin
  CheckTrue(TValue.From<TObject>(Self).TryCastEx(TypeInfo(Boolean), value));
  Check(value.TypeInfo = TypeInfo(Boolean));
  CheckEquals(True, value.AsBoolean);
end;

procedure TValueHelperTestCase.TryCastEx_ValueIsString_TypeIsBoolean_True;
var
  value: TValue;
begin
  CheckTrue(TValue.From<string>(BoolToStr(True)).TryCastEx(TypeInfo(Boolean), value));
  Check(value.TypeInfo = TypeInfo(Boolean));
  CheckEquals(True, value.AsBoolean);
end;

procedure TValueHelperTestCase.TryCastEx_ValueIsString_TypeIsChar_True;
var
  value: TValue;
begin
  CheckTrue(TValue.From<string>('4').TryCastEx(TypeInfo(Char), value));
  Check(value.TypeInfo = TypeInfo(Char));
  CheckEquals('4', value.AsType<Char>);
end;

procedure TValueHelperTestCase.TryCastEx_ValueIsString_TypeIsDateTime_True;
var
  value: TValue;
  datetime: TDateTime;
begin
  datetime := EncodeDateTime(2011, 7, 5, 15, 48, 0, 0);
  CheckTrue(TValue.From<string>(DateTimeToStr(datetime)).TryCastEx(TypeInfo(TDateTime), value));
  Check(value.TypeInfo = TypeInfo(TDateTime));
  CheckEquals(datetime, value.AsType<TDateTime>);
end;

procedure TValueHelperTestCase.TryCastEx_ValueIsString_TypeIsDouble_True;
var
  value: TValue;
begin
  CheckTrue(TValue.From<string>(FloatToStr(42.0)).TryCastEx(TypeInfo(Double), value));
  Check(value.TypeInfo = TypeInfo(Double));
  CheckEquals(42.0, value.AsExtended);
end;

procedure TValueHelperTestCase.TryCastEx_ValueIsString_TypeIsInteger_True;
var
  value: TValue;
begin
  CheckTrue(TValue.From<string>('42').TryCastEx(TypeInfo(Integer), value));
  Check(value.TypeInfo = TypeInfo(Integer));
  CheckEquals(42, value.AsInteger);
end;

initialization
  RegisterTest('DSharp.Core.Reflection', TRttiTypeHelperTestCase.Suite);
  RegisterTest('DSharp.Core.Reflection', TValueHelperTestCase.Suite);

end.


unit DSharp.Core.Reflection.Tests;

interface

uses
  Generics.Collections,
  DSharp.Collections;

type
  TInvokableList = TList<IInvokable>;
  TPersistentList = class(TList<TObject>);

implementation

uses
  Classes,
  DateUtils,
  DSharp.Core.Nullable,
  DSharp.Core.Reflection,
  Rtti,
  SysUtils,
  TestClasses,
  TestFramework;

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

    procedure IsCovariantTo_TListOfIInterface_TObject_True;
    procedure IsCovariantTo_TListOfIInterface_TPersistent_False;
    procedure IsCovariantTo_TListOfIInterface_TListOfIInterface_True;
    procedure IsCovariantTo_TListOfIInvokable_TListOfIInterface_True;

    procedure IsCovariantTo_IListOfTObject_IInterface_True;
    procedure IsCovariantTo_IListOfTObject_IInvokable_False;
    procedure IsCovariantTo_IListOfTObject_IListOfTObject_True;
    procedure IsCovariantTo_IListOfTObject_IEnumerableOfTObject_True;
    procedure IsCovariantTo_IListOfTPersistent_IListOfTObject_True;

    procedure IsCovariantTo_TListOfTPersistent_TListOfTObject_True;

    procedure IsCovariantTo_TObjectListOfTObject_TObject_True;
    procedure IsCovariantTo_TObjectListOfTObject_TListOfTObject_True;
    procedure IsCovariantTo_TObjectListOfTObject_TObjectListOfTObject_True;

    procedure IsCovariantTo_TObjectListOfTObject_TPersistent_False;
    procedure IsCovariantTo_TObjectListOfTObject_TListOfTPersistent_False;

    procedure IsCovariantTo_TObjectListOfTPersistent_TListOfTObject_True;
    procedure IsCovariantTo_TObjectListOfTPersistent_TListOfTPersistent_True;
    procedure IsCovariantTo_TObjectListOfTPersistent_TObjectListOfTObject_True;

    procedure IsCovariantTo_TPersistentList_TListOfObject_True;

    procedure IsGenericTypeOf_TListOfTObject_TList_True;
    procedure IsGenericTypeOf_TListOfTObject_TObjectList_False;
  end;

  TValueHelperTestCase = class(TTestCase)
  private
    FEventCallCount: Integer;
    FValue: TValue;
    procedure TestEvent(Sender: TObject);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure AsUInt64_ValueIsHighUInt64;
    procedure AsUInt64_ValueIsHighInt64;

    procedure IsByte_ValueIsByte_True;
    procedure IsByte_ValueIsWord_False;
    procedure IsByte_ValueIsShortInt_False;

    procedure IsCardinal_ValueIsCardinal_True;
    procedure IsCardinal_ValueIsInteger_False;
    procedure IsCardinal_ValueIsInt64_False;

    procedure IsFloat_ValueIsString_False;
    procedure IsFloat_ValueIsInteger_False;
    procedure IsFloat_ValueIsDouble_True;

    procedure IsNumeric_ValueIsString_False;
    procedure IsNumeric_ValueIsInteger_True;
    procedure IsNumeric_ValueIsDouble_True;
    procedure IsNumeric_ValueIsBoolean_True;
    procedure IsNumeric_ValueIsChar_True;

    procedure IsShortInt_ValueIsShortInt_True;
    procedure IsShortInt_ValueIsByte_False;
    procedure IsShortInt_ValueIsSmallInt_False;

    procedure IsSmallInt_ValueIsSmallInt_True;
    procedure IsSmallInt_ValueIsWord_False;
    procedure IsSmallInt_ValueIsCardinal_False;

    procedure IsString_ValueIsString_True;
    procedure IsString_ValueIsInteger_False;
    procedure IsString_ValueIsDouble_False;
    procedure IsString_ValueIsChar_True;

    procedure IsUInt64_ValueIsUInt64_True;
    procedure IsUInt64_ValueIsInt64_False;

    procedure TryConvert_ValueIsString_TypeIsString_True;
    procedure TryConvert_ValueIsString_TypeIsInteger_True;
    procedure TryConvert_ValueIsString_TypeIsDouble_True;
    procedure TryConvert_ValueIsString_TypeIsChar_True;
    procedure TryConvert_ValueIsString_TypeIsBoolean_True;
    procedure TryConvert_ValueIsString_TypeIsDateTime_True;
    procedure TryConvert_ValueIsString_TypeIsEnum_True;

    procedure TryConvert_ValueIsInteger_TypeIsString_True;
    procedure TryConvert_ValueIsInteger_TypeIsInteger_True;
    procedure TryConvert_ValueIsInteger_TypeIsDouble_True;
//    procedure TryConvert_ValueIsInteger_TypeIsChar_True;
//    procedure TryConvert_ValueIsInteger_TypeIsBoolean_True;
//    procedure TryConvert_ValueIsInteger_TypeIsDateTime_True;

    procedure TryConvert_ValueIsDouble_TypeIsString_True;
    procedure TryConvert_ValueIsDouble_TypeIsInteger_True;
    procedure TryConvert_ValueIsDouble_TypeIsDouble_True;

    procedure TryConvert_ValueIsBoolean_TypeIsString_True;
    procedure TryConvert_ValueIsBoolean_TypeIsInteger_True;
    procedure TryConvert_ValueIsBoolean_TypeIsDouble_True;
    procedure TryConvert_ValueIsBoolean_TypeIsBoolean_True;

    procedure TryConvert_ValueIsDateTime_TypeIsString_True;
    procedure TryConvert_ValueIsDateTime_TypeIsDouble_True;
    procedure TryConvert_ValueIsDateTime_TypeIsDateTime_True;

    procedure TryConvert_ValueIsEvent_TypeIsMethod_True;

    procedure TryConvert_ValueIsObject_TypeIsBoolean_True;
    procedure TryConvert_ValueIsObject_TypeIsObject_True;

    procedure TryConvert_ValueIsEnum_TypeIsString_True;
    procedure TryConvert_ValueIsEnum_TypeIsInteger_True;

    procedure TryConvert_ValueIsChar_TypeIsInteger_True;
    procedure TryConvert_ValueIsChar_TypeIsDouble_True;

    procedure TryConvert_ValueIsInt64_TypeIsString_True;
    procedure TryConvert_ValueIsUInt64_TypeIsString_True;

    procedure TryConvert_ValueIsInterface_TypeIsInterface_True;
    procedure TryConvert_ValueIsInterface_TypeIsObject_True;

    procedure TryConvert_ValueIsSet_TypeIsTStrings_True;

    procedure TryConvert_ValueIsString_TypeIsNullable_True;
    procedure TryConvert_ValueIsNullable_TypeIsString_True;
  end;

  // only roughly tested with 32-bit - only for finding obvious errors
  TSameValueTestCase = class(TTestCase)
  published
    procedure LeftIsDouble_RightIsDouble_ValuesAreEqual_True;
    procedure LeftIsDouble_RightIsDouble_ValuesAreNotEqual_False;
    procedure LeftIsDouble_RightIsExtended_ValuesAreEqual_True;
    procedure LeftIsDouble_RightIsExtended_ValuesAreNotEqual_False;
    procedure LeftIsDouble_RightIsInteger_ValuesAreEqual_True;
    procedure LeftIsDouble_RightIsInteger_ValuesAreNotEqual_False;
    procedure LeftIsDouble_RightIsSingle_ValuesAreEqual_True;
    procedure LeftIsDouble_RightIsSingle_ValuesAreNotEqual_False;

    procedure LeftIsExtended_RightIsDouble_ValuesAreEqual_True;
    procedure LeftIsExtended_RightIsDouble_ValuesAreNotEqual_False;
    procedure LeftIsExtended_RightIsExtended_ValuesAreEqual_True;
    procedure LeftIsExtended_RightIsExtended_ValuesAreNotEqual_False;
    procedure LeftIsExtended_RightIsInteger_ValuesAreEqual_True;
    procedure LeftIsExtended_RightIsInteger_ValuesAreNotEqual_False;
    procedure LeftIsExtended_RightIsSingle_ValuesAreEqual_True;
    procedure LeftIsExtended_RightIsSingle_ValuesAreNotEqual_False;

    procedure LeftIsInteger_RightIsDouble_ValuesAreEqual_True;
    procedure LeftIsInteger_RightIsDouble_ValuesAreNotEqual_False;
    procedure LeftIsInteger_RightIsExtended_ValuesAreEqual_True;
    procedure LeftIsInteger_RightIsExtended_ValuesAreNotEqual_False;
    procedure LeftIsInteger_RightIsInteger_ValuesAreEqual_True;
    procedure LeftIsInteger_RightIsInteger_ValuesAreNotEqual_False;
    procedure LeftIsInteger_RightIsSingle_ValuesAreEqual_True;
    procedure LeftIsInteger_RightIsSingle_ValuesAreNotEqual_False;

    procedure LeftIsSingle_RightIsDouble_ValuesAreEqual_True;
    procedure LeftIsSingle_RightIsDouble_ValuesAreNotEqual_False;
    procedure LeftIsSingle_RightIsExtended_ValuesAreEqual_True;
    procedure LeftIsSingle_RightIsExtended_ValuesAreNotEqual_False;
    procedure LeftIsSingle_RightIsInteger_ValuesAreEqual_True;
    procedure LeftIsSingle_RightIsInteger_ValuesAreNotEqual_False;
    procedure LeftIsSingle_RightIsSingle_ValuesAreEqual_True;
    procedure LeftIsSingle_RightIsSingle_ValuesAreNotEqual_False;
  end;

  TRttiObjectHelperTestCase = class(TTestCase)
  published
    procedure Test;
  end;

var
  Context: TRttiContext;

type
  TTestEnum = (teOne, teTwo, teThree);
  TTestSet = set of TTestEnum;

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
  CheckEquals('TList<T>', Context.GetType(TList<TObject>).GetGenericTypeDefinition(False));
end;

procedure TRttiTypeHelperTestCase.GetGenericTypeDefinition_TObjectListOfTObject;
begin
  CheckEquals('TObjectList<T>', Context.GetType(TObjectList<TObject>).GetGenericTypeDefinition);
end;

procedure TRttiTypeHelperTestCase.IsCovariantTo_IListOfTObject_IEnumerableOfTObject_True;
begin
  CheckTrue(Context.GetType(TypeInfo(IList<TObject>)).IsCovariantTo(TypeInfo(IEnumerable<TObject>)));
end;

procedure TRttiTypeHelperTestCase.IsCovariantTo_IListOfTObject_IInterface_True;
begin
  CheckTrue(Context.GetType(TypeInfo(IList<TObject>)).IsCovariantTo(TypeInfo(IInterface)));
end;

procedure TRttiTypeHelperTestCase.IsCovariantTo_IListOfTObject_IInvokable_False;
begin
  CheckFalse(Context.GetType(TypeInfo(IList<TObject>)).IsCovariantTo(TypeInfo(IInvokable)));
end;

procedure TRttiTypeHelperTestCase.IsCovariantTo_IListOfTObject_IListOfTObject_True;
begin
  CheckTrue(Context.GetType(TypeInfo(IList<TObject>)).IsCovariantTo(TypeInfo(IList<TObject>)));
end;

procedure TRttiTypeHelperTestCase.IsCovariantTo_IListOfTPersistent_IListOfTObject_True;
begin
  CheckTrue(Context.GetType(TypeInfo(IList<TPersistent>)).IsCovariantTo(TypeInfo(IList<TObject>)));
end;

procedure TRttiTypeHelperTestCase.IsCovariantTo_TListOfIInterface_TListOfIInterface_True;
begin
  CheckTrue(Context.GetType(TList<IInterface>).IsCovariantTo(TList<IInterface>));
end;

procedure TRttiTypeHelperTestCase.IsCovariantTo_TListOfIInterface_TObject_True;
begin
  CheckTrue(Context.GetType(TList<IInterface>).IsCovariantTo(TObject));
end;

procedure TRttiTypeHelperTestCase.IsCovariantTo_TListOfIInterface_TPersistent_False;
begin
  CheckFalse(Context.GetType(TList<IInterface>).IsCovariantTo(TPersistent));
end;

procedure TRttiTypeHelperTestCase.IsCovariantTo_TListOfIInvokable_TListOfIInterface_True;
begin
  CheckTrue(Context.GetType(TInvokableList).IsCovariantTo(TList<IInterface>));
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

procedure TRttiTypeHelperTestCase.IsCovariantTo_TPersistentList_TListOfObject_True;
begin
  CheckTrue(Context.GetType(TPersistentList).IsCovariantTo(TList<TObject>));
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

procedure TValueHelperTestCase.AsUInt64_ValueIsHighInt64;
begin
  Check(High(Int64) = TValue.From<UInt64>(High(Int64)).AsUInt64);
end;

procedure TValueHelperTestCase.AsUInt64_ValueIsHighUInt64;
begin
  Check(High(UInt64) = TValue.From<UInt64>(High(UInt64)).AsUInt64);
end;

procedure TValueHelperTestCase.IsByte_ValueIsByte_True;
begin
  CheckTrue(TValue.From<Byte>(42).IsByte);
end;

procedure TValueHelperTestCase.IsByte_ValueIsShortInt_False;
begin
  CheckFalse(TValue.From<ShortInt>(42).IsByte);
end;

procedure TValueHelperTestCase.IsByte_ValueIsWord_False;
begin
  CheckFalse(TValue.From<Word>(42).IsByte);
end;

procedure TValueHelperTestCase.IsCardinal_ValueIsCardinal_True;
begin
  CheckTrue(TValue.From<Cardinal>(42).IsCardinal);
end;

procedure TValueHelperTestCase.IsCardinal_ValueIsInt64_False;
begin
  CheckFalse(TValue.From<Int64>(42).IsCardinal);
end;

procedure TValueHelperTestCase.IsCardinal_ValueIsInteger_False;
begin
  CheckFalse(TValue.From<Integer>(42).IsCardinal);
end;

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

procedure TValueHelperTestCase.IsShortInt_ValueIsByte_False;
begin
  CheckFalse(TValue.From<Byte>(42).IsShortInt);
end;

procedure TValueHelperTestCase.IsShortInt_ValueIsShortInt_True;
begin
  CheckTrue(TValue.From<ShortInt>(42).IsShortInt);
end;

procedure TValueHelperTestCase.IsShortInt_ValueIsSmallInt_False;
begin
  CheckFalse(TValue.From<SmallInt>(42).IsShortInt);
end;

procedure TValueHelperTestCase.IsSmallInt_ValueIsCardinal_False;
begin
  CheckFalse(TValue.From<Cardinal>(42).IsSmallInt);
end;

procedure TValueHelperTestCase.IsSmallInt_ValueIsSmallInt_True;
begin
  CheckTrue(TValue.From<SmallInt>(42).IsSmallInt);
end;

procedure TValueHelperTestCase.IsSmallInt_ValueIsWord_False;
begin
  CheckFalse(TValue.From<Word>(42).IsSmallInt);
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

procedure TValueHelperTestCase.IsUInt64_ValueIsInt64_False;
begin
  CheckFalse(TValue.From<Int64>(42).IsUInt64);
end;

procedure TValueHelperTestCase.IsUInt64_ValueIsUInt64_True;
begin
  CheckTrue(TValue.From<UInt64>(42).IsUInt64);
end;

procedure TValueHelperTestCase.SetUp;
begin
  inherited;
  FEventCallCount := 0;
end;

procedure TValueHelperTestCase.TryConvert_ValueIsString_TypeIsString_True;
begin
  CheckTrue(TValue.From<string>('42').TryConvert<string>(FValue));
  CheckTrue(FValue.IsType<string>);
  CheckEquals('42', FValue.AsString);
end;

procedure TValueHelperTestCase.TryConvert_ValueIsUInt64_TypeIsString_True;
begin
  CheckTrue(TValue.From<Int64>(High(Int64)).TryConvert<string>(FValue));
  CheckTrue(FValue.IsType<string>);
  CheckEquals(IntToStr(High(Int64)), FValue.AsString);
end;

procedure TValueHelperTestCase.TryConvert_ValueIsChar_TypeIsDouble_True;
begin
  CheckTrue(TValue.From<Char>('A').TryConvert<Double>(FValue));
  CheckTrue(FValue.IsType<Double>);
  CheckEquals(65.0, FValue.AsExtended);
end;

procedure TValueHelperTestCase.TryConvert_ValueIsChar_TypeIsInteger_True;
begin
  CheckTrue(TValue.From<Char>('A').TryConvert<Integer>(FValue));
  CheckTrue(FValue.IsType<Integer>);
  CheckEquals(65, FValue.AsInteger);
end;

procedure TValueHelperTestCase.TearDown;
begin
  inherited;
  FEventCallCount := 0;
  FValue := TValue.Empty;
end;

procedure TValueHelperTestCase.TestEvent(Sender: TObject);
begin
  Inc(FEventCallCount);
end;

procedure TValueHelperTestCase.TryConvert_ValueIsBoolean_TypeIsBoolean_True;
begin
  CheckTrue(TValue.From<Boolean>(True).TryConvert<Boolean>(FValue));
  CheckTrue(FValue.IsType<Boolean>);
  CheckEquals(True, FValue.AsBoolean);
end;

procedure TValueHelperTestCase.TryConvert_ValueIsBoolean_TypeIsDouble_True;
begin
  CheckTrue(TValue.From<Boolean>(True).TryConvert<Double>(FValue));
  CheckTrue(FValue.IsType<Double>);
  CheckEquals(1.0, FValue.AsExtended);
end;

procedure TValueHelperTestCase.TryConvert_ValueIsBoolean_TypeIsInteger_True;
begin
  CheckTrue(TValue.From<Boolean>(True).TryConvert<Integer>(FValue));
  CheckTrue(FValue.IsType<Integer>);
  CheckEquals(1, FValue.AsInteger);
end;

procedure TValueHelperTestCase.TryConvert_ValueIsBoolean_TypeIsString_True;
begin
  CheckTrue(TValue.From<Boolean>(True).TryConvert<string>(FValue));
  CheckTrue(FValue.IsType<string>);
  CheckEquals(BoolToStr(True), FValue.AsString);
end;

procedure TValueHelperTestCase.TryConvert_ValueIsDateTime_TypeIsDateTime_True;
var
  datetime: TDateTime;
begin
  datetime := EncodeDateTime(2011, 7, 5, 15, 48, 0, 0);
  CheckTrue(TValue.From<TDateTime>(datetime).TryConvert<TDateTime>(FValue));
  CheckTrue(FValue.IsType<TDateTime>);
  CheckEquals(datetime, FValue.AsType<TDateTime>);
end;

procedure TValueHelperTestCase.TryConvert_ValueIsDateTime_TypeIsDouble_True;
var
  datetime: TDateTime;
begin
  datetime := EncodeDateTime(2011, 7, 5, 15, 48, 0, 0);
  CheckTrue(TValue.From<TDateTime>(datetime).TryConvert<Double>(FValue));
  CheckTrue(FValue.IsType<Double>);
  CheckEquals(datetime, FValue.AsExtended);
end;

procedure TValueHelperTestCase.TryConvert_ValueIsDateTime_TypeIsString_True;
var
  datetime: TDateTime;
begin
  datetime := EncodeDateTime(2011, 7, 5, 15, 48, 0, 0);
  CheckTrue(TValue.From<TDateTime>(datetime).TryConvert<string>(FValue));
  CheckTrue(FValue.IsType<string>);
  CheckEquals(DateTimeToStr(datetime), FValue.AsType<string>);
end;

procedure TValueHelperTestCase.TryConvert_ValueIsDouble_TypeIsDouble_True;
begin
  CheckTrue(TValue.From<Double>(42.0).TryConvert<Double>(FValue));
  CheckTrue(FValue.IsType<Double>);
  CheckEquals(42.0, FValue.AsExtended);
end;

procedure TValueHelperTestCase.TryConvert_ValueIsDouble_TypeIsInteger_True;
begin
  CheckTrue(TValue.From<Double>(42).TryConvert<Integer>(FValue));
  CheckTrue(FValue.IsType<Integer>);
  CheckEquals(42, FValue.AsInteger);
end;

procedure TValueHelperTestCase.TryConvert_ValueIsDouble_TypeIsString_True;
begin
  CheckTrue(TValue.From<Double>(42).TryConvert<string>(FValue));
  CheckTrue(FValue.IsType<string>);
  CheckEquals('42', FValue.AsString);
end;

procedure TValueHelperTestCase.TryConvert_ValueIsEnum_TypeIsInteger_True;
begin
  CheckTrue(TValue.From<TTestEnum>(teOne).TryConvert<Integer>(FValue));
  CheckTrue(FValue.IsType<Integer>);
  CheckEquals(0, FValue.AsInteger);
end;

procedure TValueHelperTestCase.TryConvert_ValueIsEnum_TypeIsString_True;
begin
  CheckTrue(TValue.From<TTestEnum>(teOne).TryConvert<string>(FValue));
  CheckTrue(FValue.IsType<string>);
  CheckEquals('teOne', FValue.AsString);
end;

procedure TValueHelperTestCase.TryConvert_ValueIsEvent_TypeIsMethod_True;
var
  method: TMethod;
  event: TNotifyEvent;
begin
  method.Code := @TValueHelperTestCase.TestEvent;
  method.Data := Self;
  CheckTrue(TValue.From<TMethod>(method).TryConvert<TNotifyEvent>(FValue));
  CheckTrue(FValue.IsType<TNotifyEvent>);
  event := FValue.AsType<TNotifyEvent>();
  Check(TMethod(event).Code = method.Code);
  event(Self);
  CheckEquals(1, FEventCallCount);
end;

procedure TValueHelperTestCase.TryConvert_ValueIsInt64_TypeIsString_True;
begin
  CheckTrue(TValue.From<Int64>(High(Int64)).TryConvert<string>(FValue));
  CheckTrue(FValue.IsType<string>);
  CheckEquals(IntToStr(High(Int64)), FValue.AsString);
end;

procedure TValueHelperTestCase.TryConvert_ValueIsInteger_TypeIsDouble_True;
begin
  CheckTrue(TValue.From<Integer>(42).TryConvert<Double>(FValue));
  CheckTrue(FValue.IsType<Double>);
  CheckEquals(42.0, FValue.AsExtended);
end;

procedure TValueHelperTestCase.TryConvert_ValueIsInteger_TypeIsInteger_True;
begin
  CheckTrue(TValue.From<Integer>(42).TryConvert<Integer>(FValue));
  CheckTrue(FValue.IsType<Integer>);
  CheckEquals(42.0, FValue.AsInteger);
end;

procedure TValueHelperTestCase.TryConvert_ValueIsInteger_TypeIsString_True;
begin
  CheckTrue(TValue.From<Integer>(42).TryConvert<string>(FValue));
  CheckTrue(FValue.IsType<string>);
  CheckEquals('42', FValue.AsString);
end;

procedure TValueHelperTestCase.TryConvert_ValueIsInterface_TypeIsInterface_True;
var
  list: IList<TPersistent>;
begin
  list := TList<TPersistent>.Create;
  CheckTrue(TValue.From<IList<TPersistent>>(list).TryConvert<IList<TObject>>(FValue));
  CheckTrue(FValue.IsType<IList<TObject>>);
  CheckSame(list, FValue.AsInterface);

  CheckTrue(TValue.From<IList<TPersistent>>(list).TryConvert<IList>(FValue));
  CheckTrue(FValue.IsType<IList>);
  CheckSame(list.AsList, FValue.AsInterface);
end;

procedure TValueHelperTestCase.TryConvert_ValueIsInterface_TypeIsObject_True;
var
  list: IList<TPersistent>;
begin
  list := TList<TPersistent>.Create;
  CheckTrue(TValue.From<IList<TPersistent>>(list).TryConvert<TList<TObject>>(FValue));
  CheckTrue(FValue.IsType<TList<TObject>>);
  CheckSame(list.AsObject, FValue.AsObject);

  CheckTrue(TValue.From<IList<TPersistent>>(list).TryConvert<TEnumerable<TPersistent>>(FValue));
  CheckTrue(FValue.IsType<TEnumerable<TPersistent>>);
  CheckSame(list.AsObject, FValue.AsObject);
end;

procedure TValueHelperTestCase.TryConvert_ValueIsNullable_TypeIsString_True;
begin
  CheckTrue(TValue.From<Nullable<string>>('nullable').TryConvert<string>(FValue));
  CheckTrue(FValue.IsType<string>);
  CheckEquals('nullable', FValue.AsString);
end;

procedure TValueHelperTestCase.TryConvert_ValueIsObject_TypeIsBoolean_True;
begin
  CheckTrue(TValue.From<TObject>(Self).TryConvert<Boolean>(FValue));
  CheckTrue(FValue.IsType<Boolean>);
  CheckEquals(True, FValue.AsBoolean);
end;

procedure TValueHelperTestCase.TryConvert_ValueIsObject_TypeIsObject_True;
var
  list: TList<TPersistent>;
begin
  list := TList<TPersistent>.Create;
  try
    CheckTrue(TValue.From<TList<TPersistent>>(list).TryConvert<TList<TObject>>(FValue));
    CheckTrue(FValue.IsType<TList<TObject>>);
    CheckSame(list, FValue.AsObject);
  finally
    list.Free;
  end;
end;

procedure TValueHelperTestCase.TryConvert_ValueIsSet_TypeIsTStrings_True;
begin
  CheckTrue(TValue.From<TTestSet>([teOne, teTwo, teThree]).TryConvert<TStrings>(FValue));
  CheckTrue(FValue.IsType<TStrings>);
  CheckEquals(3, FValue.AsType<TStrings>.Count);
end;

procedure TValueHelperTestCase.TryConvert_ValueIsString_TypeIsBoolean_True;
begin
  CheckTrue(TValue.From<string>(BoolToStr(True)).TryConvert<Boolean>(FValue));
  CheckTrue(FValue.IsType<Boolean>);
  CheckEquals(True, FValue.AsBoolean);
end;

procedure TValueHelperTestCase.TryConvert_ValueIsString_TypeIsChar_True;
begin
  CheckTrue(TValue.From<string>('4').TryConvert<Char>(FValue));
  CheckTrue(FValue.IsType<Char>);
  CheckEquals('4', FValue.AsType<Char>);
end;

procedure TValueHelperTestCase.TryConvert_ValueIsString_TypeIsDateTime_True;
var
  datetime: TDateTime;
begin
  datetime := EncodeDateTime(2011, 7, 5, 15, 48, 0, 0);
  CheckTrue(TValue.From<string>(DateTimeToStr(datetime)).TryConvert<TDateTime>(FValue));
  CheckTrue(FValue.IsType<TDateTime>);
  CheckEquals(datetime, FValue.AsType<TDateTime>);
end;

procedure TValueHelperTestCase.TryConvert_ValueIsString_TypeIsDouble_True;
begin
  CheckTrue(TValue.From<string>(FloatToStr(42.0)).TryConvert<Double>(FValue));
  CheckTrue(FValue.IsType<Double>);
  CheckEquals(42.0, FValue.AsExtended);
end;

procedure TValueHelperTestCase.TryConvert_ValueIsString_TypeIsEnum_True;
begin
  CheckTrue(TValue.From<string>('teTwo').TryConvert<TTestEnum>(FValue));
  CheckTrue(FValue.IsType<TTestEnum>);
  CheckEquals(Ord(teTwo), FValue.AsOrdinal);
end;

procedure TValueHelperTestCase.TryConvert_ValueIsString_TypeIsInteger_True;
begin
  CheckTrue(TValue.From<string>('42').TryConvert<Integer>(FValue));
  CheckTrue(FValue.IsType<Integer>);
  CheckEquals(42, FValue.AsInteger);
end;

procedure TValueHelperTestCase.TryConvert_ValueIsString_TypeIsNullable_True;
begin
  CheckTrue(TValue.From<string>('nullable').TryConvert<Nullable<string>>(FValue));
  CheckTrue(FValue.IsType<Nullable<string>>);
  CheckEquals('nullable', FValue.AsType<Nullable<string>>.Value);
end;

// Taken from Math.pas
const
  FuzzFactor = 1000;
  DoubleResolution   = 1E-15 * FuzzFactor;
  SingleResolution   = 1E-7 * FuzzFactor;
{$IFDEF CPUX64}
  ExtendedResolution = DoubleResolution;
{$ELSE !CPUX64}
  ExtendedResolution = 1E-19 * FuzzFactor;
{$ENDIF}

{ TSameValueTestCase }

procedure TSameValueTestCase.LeftIsDouble_RightIsDouble_ValuesAreEqual_True;
begin
  CheckTrue(SameValue(TValue.From<Double>(42), TValue.From<Double>(42 + DoubleResolution)));
end;

procedure TSameValueTestCase.LeftIsDouble_RightIsDouble_ValuesAreNotEqual_False;
begin
  CheckFalse(SameValue(TValue.From<Double>(42), TValue.From<Double>(42 + SingleResolution)));
end;

procedure TSameValueTestCase.LeftIsDouble_RightIsExtended_ValuesAreEqual_True;
begin
  CheckTrue(SameValue(TValue.From<Double>(42), TValue.From<Extended>(42 + ExtendedResolution)));
end;

procedure TSameValueTestCase.LeftIsDouble_RightIsExtended_ValuesAreNotEqual_False;
begin
//  CheckFalse(SameValue(TValue.From<Double>(42), TValue.From<Extended>(42 + DoubleResolution)));
  CheckFalse(SameValue(TValue.From<Double>(42), TValue.From<Extended>(42 + SingleResolution)));
end;

procedure TSameValueTestCase.LeftIsDouble_RightIsInteger_ValuesAreEqual_True;
begin
  CheckTrue(SameValue(TValue.From<Double>(42), TValue.From<Integer>(42)));
end;

procedure TSameValueTestCase.LeftIsDouble_RightIsInteger_ValuesAreNotEqual_False;
begin
  CheckFalse(SameValue(TValue.From<Double>(42), TValue.From<Integer>(43)));
end;

procedure TSameValueTestCase.LeftIsDouble_RightIsSingle_ValuesAreEqual_True;
begin
  CheckTrue(SameValue(TValue.From<Double>(42 + DoubleResolution), TValue.From<Single>(42)));
end;

procedure TSameValueTestCase.LeftIsDouble_RightIsSingle_ValuesAreNotEqual_False;
begin
  CheckFalse(SameValue(TValue.From<Double>(42 + SingleResolution), TValue.From<Single>(42)));
end;

procedure TSameValueTestCase.LeftIsExtended_RightIsDouble_ValuesAreEqual_True;
begin
  CheckTrue(SameValue(TValue.From<Extended>(42 + ExtendedResolution), TValue.From<Double>(42)));
end;

procedure TSameValueTestCase.LeftIsExtended_RightIsDouble_ValuesAreNotEqual_False;
begin
//  CheckFalse(SameValue(TValue.From<Extended>(42 + DoubleResolution), TValue.From<Double>(42)));
  CheckFalse(SameValue(TValue.From<Extended>(42 + SingleResolution), TValue.From<Double>(42)));
end;

procedure TSameValueTestCase.LeftIsExtended_RightIsExtended_ValuesAreEqual_True;
begin
  CheckTrue(SameValue(TValue.From<Extended>(42 + ExtendedResolution), TValue.From<Extended>(42)));
end;

procedure TSameValueTestCase.LeftIsExtended_RightIsExtended_ValuesAreNotEqual_False;
begin
//  CheckFalse(SameValue(TValue.From<Extended>(42 + DoubleResolution), TValue.From<Extended>(42)));
  CheckFalse(SameValue(TValue.From<Extended>(42 + SingleResolution), TValue.From<Extended>(42)));
end;

procedure TSameValueTestCase.LeftIsExtended_RightIsInteger_ValuesAreEqual_True;
begin
  CheckTrue(SameValue(TValue.From<Extended>(42 + ExtendedResolution), TValue.From<Integer>(42)));
end;

procedure TSameValueTestCase.LeftIsExtended_RightIsInteger_ValuesAreNotEqual_False;
begin
//  CheckFalse(SameValue(TValue.From<Extended>(42 + DoubleResolution), TValue.From<Integer>(42)));
  CheckFalse(SameValue(TValue.From<Extended>(42 + SingleResolution), TValue.From<Integer>(42)));
end;

procedure TSameValueTestCase.LeftIsExtended_RightIsSingle_ValuesAreEqual_True;
begin
  CheckTrue(SameValue(TValue.From<Extended>(42 + ExtendedResolution), TValue.From<Single>(42)));
end;

procedure TSameValueTestCase.LeftIsExtended_RightIsSingle_ValuesAreNotEqual_False;
begin
//  CheckFalse(SameValue(TValue.From<Extended>(42 + DoubleResolution), TValue.From<Single>(42)));
  CheckFalse(SameValue(TValue.From<Extended>(42 + SingleResolution), TValue.From<Single>(42)));
end;

procedure TSameValueTestCase.LeftIsInteger_RightIsDouble_ValuesAreEqual_True;
begin
  CheckTrue(SameValue(TValue.From<Integer>(42), TValue.From<Double>(42 + DoubleResolution)));
end;

procedure TSameValueTestCase.LeftIsInteger_RightIsDouble_ValuesAreNotEqual_False;
begin
  CheckFalse(SameValue(TValue.From<Integer>(42), TValue.From<Double>(42 + SingleResolution)));
end;

procedure TSameValueTestCase.LeftIsInteger_RightIsExtended_ValuesAreEqual_True;
begin
  CheckTrue(SameValue(TValue.From<Integer>(42), TValue.From<Extended>(42 + ExtendedResolution)));
end;

procedure TSameValueTestCase.LeftIsInteger_RightIsExtended_ValuesAreNotEqual_False;
begin
//  CheckFalse(SameValue(TValue.From<Integer>(42), TValue.From<Extended>(42 + DoubleResolution)));
  CheckFalse(SameValue(TValue.From<Integer>(42), TValue.From<Extended>(42 + SingleResolution)));
end;

procedure TSameValueTestCase.LeftIsInteger_RightIsInteger_ValuesAreEqual_True;
begin
  CheckTrue(SameValue(TValue.From<Integer>(42), TValue.From<Integer>(42)));
end;

procedure TSameValueTestCase.LeftIsInteger_RightIsInteger_ValuesAreNotEqual_False;
begin
  CheckFalse(SameValue(TValue.From<Integer>(42), TValue.From<Integer>(43)));
end;

procedure TSameValueTestCase.LeftIsInteger_RightIsSingle_ValuesAreEqual_True;
begin
  CheckTrue(SameValue(TValue.From<Integer>(42), TValue.From<Single>(42 + SingleResolution)));
end;

procedure TSameValueTestCase.LeftIsInteger_RightIsSingle_ValuesAreNotEqual_False;
begin
  CheckFalse(SameValue(TValue.From<Integer>(42), TValue.From<Single>(42.005)));
end;

procedure TSameValueTestCase.LeftIsSingle_RightIsDouble_ValuesAreEqual_True;
begin
  CheckTrue(SameValue(TValue.From<Single>(42), TValue.From<Double>(42 + DoubleResolution)));
end;

procedure TSameValueTestCase.LeftIsSingle_RightIsDouble_ValuesAreNotEqual_False;
begin
  CheckFalse(SameValue(TValue.From<Single>(42), TValue.From<Double>(42 + SingleResolution)));
end;

procedure TSameValueTestCase.LeftIsSingle_RightIsExtended_ValuesAreEqual_True;
begin
  CheckTrue(SameValue(TValue.From<Single>(42), TValue.From<Extended>(42 + ExtendedResolution)));
end;

procedure TSameValueTestCase.LeftIsSingle_RightIsExtended_ValuesAreNotEqual_False;
begin
//  CheckFalse(SameValue(TValue.From<Single>(42), TValue.From<Extended>(42 + DoubleResolution)));
  CheckFalse(SameValue(TValue.From<Single>(42), TValue.From<Extended>(42 + SingleResolution)));
end;

procedure TSameValueTestCase.LeftIsSingle_RightIsInteger_ValuesAreEqual_True;
begin
  CheckTrue(SameValue(TValue.From<Single>(42), TValue.From<Integer>(42)));
end;

procedure TSameValueTestCase.LeftIsSingle_RightIsInteger_ValuesAreNotEqual_False;
begin
  CheckFalse(SameValue(TValue.From<Single>(42), TValue.From<Integer>(43)));
end;

procedure TSameValueTestCase.LeftIsSingle_RightIsSingle_ValuesAreEqual_True;
begin
  CheckTrue(SameValue(TValue.From<Single>(42), TValue.From<Single>(42 + SingleResolution)));
end;

procedure TSameValueTestCase.LeftIsSingle_RightIsSingle_ValuesAreNotEqual_False;
begin
  CheckFalse(SameValue(TValue.From<Single>(42), TValue.From<Single>(42.005)));
end;

{ TRttiObjectHelperTestCase }

procedure TRttiObjectHelperTestCase.Test;
var
  base: TBase;
begin
  base := TInherited.Create;
  base.MethodC;

  CheckFalse(base.GetMethod('MethodA').IsDefined<TestAttribute>);
  CheckFalse(base.GetMethod('MethodB').IsDefined<TestAttribute>);
  CheckTrue(base.GetMethod('MethodC').IsDefined<TestAttribute>);

  CheckTrue(base.GetMethod('MethodA').IsDefined<TestAttribute>(True));
  CheckTrue(base.GetMethod('MethodB').IsDefined<TestAttribute>(True));

  base.Free;
end;

initialization
  RegisterTest('DSharp.Core.Reflection', TRttiTypeHelperTestCase.Suite);
  RegisterTest('DSharp.Core.Reflection', TValueHelperTestCase.Suite);
  RegisterTest('DSharp.Core.Reflection', TSameValueTestCase.Suite);
  RegisterTest('DSharp.Core.Reflection', TRttiObjectHelperTestCase.Suite);

end.


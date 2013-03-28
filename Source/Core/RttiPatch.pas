unit RttiPatch;

interface

{$I DSharp.inc}

// Thanks to Andreas Hausladen

implementation

{$IF CompilerVersion > 20}
uses
  Generics.Collections,
  PatchUtils,
  RTLConsts,
  Rtti,
  SysConst,
  SysUtils,
  TypInfo,
  Windows;

var
  IsPatched: Boolean = False;

{--------------------------------------------------------------------------------------------------}

type
  PInterceptFrame = Pointer;

  PParamLoc = ^TParamLoc;
  TParamLoc = record
    FTypeInfo: PTypeInfo;
    FByRefParam: Boolean;
    FOffset: Integer;
    procedure SetArg(AFrame: PInterceptFrame; const Value: TValue);
  end;

const
  SetArgCallBytes: array[0..18] of SmallInt = (
    $8D, $04, $5B,      // lea eax,[ebx+ebx*2]                          //  0
    $8B, $55, $F0,      // mov edx,[ebp-$10]                            //  3
    $8D, $0C, $C2,      // lea ecx,[edx+eax*8]                          //  6
    $8B, $55, $FC,      // mov edx,[ebp-$04]                            //  9
    $8D, $04, $82,      // lea eax,[edx+eax*4]                          // 12
    $8B, $55, $F4,      // mov edx,[ebp-$0c]                            // 15
    $E8                 // call TMethodImplementation.TParamLoc.SetArg  // 18
  );

var
  TParamLoc_SetArg: procedure(var Self: TParamLoc; AFrame: PInterceptFrame; const Value: TValue);

procedure TParamLoc.SetArg(AFrame: PInterceptFrame; const Value: TValue);
begin
  // Fix from XE2
  if FByRefParam then
    TParamLoc_SetArg(Self, AFrame, Value);
end;

{--------------------------------------------------------------------------------------------------}

type
  {$M+}
  {$RTTI EXPLICIT METHODS([vcPublic, vcPublished])}
  IIntfMethodHelper = interface
    procedure IntfMethod;
  end;

  {$RTTI EXPLICIT METHODS([vcPublic, vcPublished])}
  TInstanceMethodHelper = class(TObject)
  public
    procedure InstanceMethod;
  end;
  {$M-}

  TRttiMethodFix = class(TRttiMethod)
  public
    function IntfDispatchInvoke(Instance: TValue; const Args: array of TValue): TValue;
    function InstanceDispatchInvoke(Instance: TValue; const Args: array of TValue): TValue;
  end;

  PPVtable = ^PVtable;
  PVtable = ^TVtable;
  TVtable = array[0..MaxInt div SizeOf(Pointer) - 1] of Pointer;

  TValueHelper = record helper for TValue
{$IF CompilerVersion = 21}
    function Cast(ATypeInfo: PTypeInfo): TValue;
    function TryCastFix(ATypeInfo: PTypeInfo; out AResult: TValue): Boolean;
{$IFEND}
    function TryAsOrdinalFix(out AResult: Int64): Boolean;
  end;

{$IF CompilerVersion = 21}
const
  GUID_NULL: TGUID = '{00000000-0000-0000-0000-000000000000}';

function ConvClass2Intf(const ASource: TValue; ATarget: PTypeInfo; out AResult: TValue): Boolean;
var
  iid: TGUID;
  obj: Pointer;
begin
  iid := GetTypeData(ATarget)^.Guid;
  if IsEqualGUID(iid, GUID_NULL) then
    Exit(False);
  Result := ASource.AsObject.GetInterface(iid, obj);
  if Result then
    TValue.MakeWithoutCopy(@obj, ATarget, AResult);
end;

function TValueHelper.Cast(ATypeInfo: PTypeInfo): TValue;
begin
  if not TryCastFix(ATypeInfo, Result) then
    raise EInvalidCast.CreateRes(@SInvalidCast);
end;

function TValueHelper.TryCastFix(ATypeInfo: PTypeInfo; out AResult: TValue): Boolean;
begin
  if Self.FData.FTypeInfo = ATypeInfo then
  begin
    AResult := Self;
    Exit(True);
  end;
  if (Self.FData.FTypeInfo^.Kind = tkClass) and (ATypeInfo^.Kind = tkInterface) then
  begin
    if IsEmpty then
    begin
      // nil converts to reference types
      AResult := TValue.Empty;
      Result := (ATypeInfo <> nil) and (ATypeInfo^.Kind in [tkInterface, tkClass, tkClassRef]);
      if Result then
        AResult.FData.FTypeInfo := ATypeInfo;
      Exit;
    end;
    if ATypeInfo = nil then
      Exit(False);
    Result := ConvClass2Intf(Self, ATypeInfo, AResult);
  end
  else
    Result := TryCast(ATypeInfo, AResult);
end;
{$IFEND}

function GetInlineSize(TypeInfo: PTypeInfo): Integer;
begin
  if TypeInfo = nil then
    Exit(0);

  case TypeInfo^.Kind of
    tkInteger, tkEnumeration, tkChar, tkWChar:
      case GetTypeData(TypeInfo)^.OrdType of
        otSByte, otUByte: Exit(1);
        otSWord, otUWord: Exit(2);
        otSLong, otULong: Exit(4);
      else
        Exit(0);
      end;
    tkInt64: Exit(8);
  else
    Exit(0);
  end;
end;

function TValueHelper.TryAsOrdinalFix(out AResult: Int64): Boolean;
begin
  Result := not Assigned(TypeInfo) or IsOrdinal;
  if Result then
  begin
    case GetInlineSize(TypeInfo) of
      1: AResult := Self.FData.FAsSByte;
      2: AResult := Self.FData.FAsSWord;
      4: AResult := Self.FData.FAsSLong;
      else
        AResult := Self.FData.FAsSInt64;
    end;
  end;
end;

procedure TInstanceMethodHelper.InstanceMethod;
begin
end;

procedure CheckCodeAddress(code: Pointer);
begin
  if (code = nil) or (PPointer(code)^ = nil) then
    raise EInsufficientRtti.CreateRes(@SInsufficientRtti);
end;

function PassByRef(TypeInfo: PTypeInfo; CC: TCallConv; IsConst: Boolean = False): Boolean;
begin
  if TypeInfo = nil then
    Exit(False);
  case TypeInfo^.Kind of
{$IF Defined(CPUX86)}
    tkVariant: 
      Result := IsConst or not (CC in [ccCdecl, ccStdCall, ccSafeCall]);
    tkRecord:
      if (CC in [ccCdecl, ccStdCall, ccSafeCall]) and not IsConst then
        Result := False
      else
        Result := GetTypeData(TypeInfo)^.RecSize > SizeOf(Pointer);
{$ELSEIF Defined(CPUX64)}
    tkVariant:
      Result := True;
    tkRecord:
      Result := not (GetTypeData(TypeInfo)^.RecSize in [1,2,4,8]);
    tkMethod:
      Result := True;
{$IFEND}
    tkArray:
      Result := GetTypeData(TypeInfo)^.ArrayData.Size > SizeOf(Pointer);
    tkString:
      Result := GetTypeData(TypeInfo)^.MaxLength > SizeOf(Pointer);
  else
    Result := False;
  end;
end;

procedure PassArg(Par: TRttiParameter; const ArgSrc: TValue; var ArgDest: TValue; CC: TCallConv);
begin
  if Par.ParamType = nil then
    ArgDest := TValue.From<Pointer>(ArgSrc.GetReferenceToRawData)
  else if Par.Flags * [TParamFlag.pfVar, TParamFlag.pfOut] <> [] then
  begin
    if Par.ParamType.Handle <> ArgSrc.TypeInfo then
      raise EInvalidCast.CreateRes(@SByRefArgMismatch);
    ArgDest := TValue.From<Pointer>(ArgSrc.GetReferenceToRawData);
  end
  else if (TParamFlag.pfConst in Par.Flags) and
    PassByRef(Par.ParamType.Handle, CC, True) then
  begin
    if Par.ParamType.Handle <> ArgSrc.TypeInfo then
      raise EInvalidCast.CreateRes(@SByRefArgMismatch);
    ArgDest := TValue.From(ArgSrc.GetReferenceToRawData);
  end
  else
    ArgDest := ArgSrc.Cast(Par.ParamType.Handle);
end;

procedure PushSelfFirst(CC: TCallConv; var argList: TArray<TValue>;
  var Index: Integer; const Value: TValue); inline;
begin
  if CC <> TCallConv.ccPascal then
  begin
    argList[Index] := Value;
    Inc(Index);
  end;
end;

procedure PushSelfLast(CC: TCallConv; var argList: TArray<TValue>;
  var Index: Integer; const Value: TValue); inline;
begin
  if CC = TCallConv.ccPascal then
    argList[Index] := Value;
end;

function TRttiMethodFix.IntfDispatchInvoke(Instance: TValue; const Args: array of TValue): TValue;
var
  Code: Pointer;
  ArgCount: Integer;
  ArgList: TArray<TValue>;
  ParList: TArray<TRttiParameter>;
  I, CurrArg: Integer;
  Inst: PPVtable;
begin
  ParList := GetParameters;
  if Length(Args) <> Length(ParList) then
    raise EInvocationError.CreateRes(@SParameterCountMismatch);

  ArgCount := Length(Args);
  SetLength(ArgList, ArgCount + 1);

  CurrArg := 0;
  Inst := PPVtable(Instance.AsInterface);
  PushSelfFirst(CallingConvention, ArgList, CurrArg, Instance);

  for I := 0 to Length(Args) - 1 do
  begin
    PassArg(ParList[I], Args[I], ArgList[CurrArg], CallingConvention);
    Inc(CurrArg);
  end;

  Assert(DispatchKind = dkInterface);
  Code := Inst^^[VirtualIndex];
  CheckCodeAddress(Code);

  PushSelfLast(CallingConvention, ArgList, CurrArg, Instance);

  if ReturnType <> nil then
    Result := Rtti.Invoke(Code, ArgList, CallingConvention, ReturnType.Handle)
  else
    Result := Rtti.Invoke(Code, ArgList, CallingConvention, nil);
end;

function TRttiMethodFix.InstanceDispatchInvoke(Instance: TValue; const Args: array of TValue): TValue;
var
  Code: Pointer;
  ArgCount: Integer;
  ArgList: TArray<TValue>;
  ParList: TArray<TRttiParameter>;
  I, CurrArg: Integer;
  Cls: TClass;
  Obj: TObject;
  Alloc: Boolean;
begin
  ParList := GetParameters;
  if Length(Args) <> Length(ParList) then
    raise EInvocationError.CreateRes(@SParameterCountMismatch);

  ArgCount := Length(Args);
  if IsConstructor or IsDestructor then
    Inc(ArgCount);
  if not IsStatic then
    Inc(ArgCount);

  SetLength(ArgList, ArgCount);
  CurrArg := 0;
  Cls := nil;

  Alloc := True;
  Obj := nil;

  if not IsStatic then
  begin
    if IsConstructor then
    begin
      Alloc := Instance.TryAsType<TClass>(Cls);
      if Alloc then
        Obj := nil
      else
      begin
        Obj := Instance.AsObject;
        if Obj <> nil then
          Cls := Obj.ClassType
        else
          Cls := nil;
      end;
      if Alloc then
        PushSelfFirst(CallingConvention, ArgList, CurrArg, Cls)
      else
        PushSelfFirst(CallingConvention, ArgList, CurrArg, Obj);
      ArgList[CurrArg] := Alloc;
      Inc(CurrArg);
    end
    else if IsDestructor then
    begin
      Cls := Instance.AsObject.ClassType;
      PushSelfFirst(CallingConvention, ArgList, CurrArg, Instance);
      ArgList[CurrArg] := True;
      Inc(CurrArg);
    end
    else if IsClassMethod then
    begin
      Cls := Instance.AsClass;
      PushSelfFirst(CallingConvention, ArgList, CurrArg, Instance);
    end
    else
    begin
      Cls := Instance.AsObject.ClassType;
      PushSelfFirst(CallingConvention, ArgList, CurrArg, Instance);
    end;

    if (Cls <> nil) and not Cls.InheritsFrom(TRttiInstanceType(Parent).MetaclassType) then
      raise EInvalidCast.CreateRes(@SInvalidCast);
  end;

  for I := 0 to Length(Args) - 1 do
  begin
    PassArg(ParList[I], Args[I], ArgList[CurrArg], CallingConvention);
    Inc(CurrArg);
  end;

  if IsStatic then
    Code := CodeAddress
  else
    case DispatchKind of
      dkVtable: Code := PVtable(Cls)^[VirtualIndex];
      dkDynamic: Code := GetDynaMethod(Cls, VirtualIndex);
    else
      Code := CodeAddress;
    end;

  CheckCodeAddress(Code);

  if not IsStatic then
  begin
    if IsConstructor then
    begin
      if Alloc then
        PushSelfLast(CallingConvention, ArgList, CurrArg, Cls)
      else
        PushSelfLast(CallingConvention, ArgList, CurrArg, Obj);
    end
    else
      PushSelfLast(CallingConvention, ArgList, CurrArg, Instance);
  end;

  if ReturnType <> nil then
    Result := Rtti.Invoke(Code, ArgList, CallingConvention, ReturnType.Handle{$IF CompilerVersion > 22}, IsStatic{$IFEND})
  else if IsConstructor then
    Result := Rtti.Invoke(Code, ArgList, CallingConvention, Cls.ClassInfo{$IF CompilerVersion > 22}, True{$IFEND})
  else
    Result := Rtti.Invoke(Code, ArgList, CallingConvention, nil);
end;

{--------------------------------------------------------------------------------------------------}

{$IF CompilerVersion = 22}
const
  TRttiMethod_CreateImplementationBytesCmp: array[0..11] of Byte = (
    $53,                // push ebx                        //  0
    $56,                // push esi                        //  1
    $57,                // push edi                        //  2
    $8B, $F9,           // mov edi,ecx                     //  3
    $8B, $F2,           // mov esi,edx                     //  5
    $8B, $D8,           // mov ebx,eax                     //  7
    $8B, $C3,           // mov eax,ebx                     //  9
    $E8                 // call TRttiMethod.GetInvokeInfo  // 11
  );

  TRttiMethod_GetInvokeInfoBytes: array[0..25] of SmallInt = (
    $B0, $01,             // mov al,$01                //  0
    $50,                  // push eax                  //  2
    $8B, $C6,             // mov eax,esi               //  3
    $8B, $10,             // mov edx,[eax]             //  5
    $FF, $52, $10,        // call dword ptr [edx+$10]  //  7
    $E8, -1, -1, -1, -1,  // call Generics + $4C9D7C   // 10
    $8B, $D0,             // mov edx,eax               // 15
    $8B, $45, $F8,        // mov eax,[ebp-$08]         // 17
    $59,                  // pop ecx                   // 20
    $E8, -1, -1, -1, -1   // call TMethodImplementation.TInvokeInfo.AddParameter // 21
  );

var
  OrgInvokeInfoAddParameter: procedure(AInvokeInfo: TObject; AType: PTypeInfo; ByRef: Boolean);

procedure FixInvokeInfoAddParameter(AInvokeInfo: TObject; AType: PTypeInfo; ByRef: Boolean;
  p: TRttiParameter; Method: TRttiMethod);
begin
  OrgInvokeInfoAddParameter(AInvokeInfo, p.ParamType.Handle,
    ([pfVar, pfOut] * p.Flags <> []) or
    PassByRef(p.ParamType.Handle, Method.CallingConvention, pfConst in p.Flags));
end;

procedure FixInvokeInfoAddParameterEnter(AInvokeInfo: TObject; AType: PTypeInfo; ByRef: Boolean);
asm
  push esi // p is kept in ESI
  push edi // TMethodRtti is kept in EDI
  call FixInvokeInfoAddParameter
end;
{$IFEND}

{--------------------------------------------------------------------------------------------------}

{$IF CompilerVersion = 21}
type
  TRttiPackageFix = class helper for TRttiPackage
    class function GetMakeTypeLookupTableAddress: Pointer;
    procedure MakeTypeLookupTableFix;
  end;

procedure PeekData(var P: PByte; var Data; Len: Integer);
begin
  Move(P^, Data, Len);
end;

procedure ReadData(var P: PByte; var Data; Len: Integer);
begin
  PeekData(P, Data, Len);
  Inc(P, Len);
end;

function ReadU8(var P: PByte): Byte;
begin
  ReadData(P, Result, SizeOf(Result));
end;

function ReadShortString(var P: PByte): string;
var
  len: Integer;
begin
  Result := UTF8ToString(PShortString(P)^);
  len := ReadU8(P);
  Inc(P, len);
end;

class function TRttiPackageFix.GetMakeTypeLookupTableAddress: Pointer;
asm
  lea eax, [TRttiPackage.MakeTypeLookupTable]
end;

procedure TRttiPackageFix.MakeTypeLookupTableFix;
  function GetUnits: TArray<string>;
  var
    p: PByte;
    i: Integer;
  begin
    SetLength(Result, Self.FTypeInfo^.UnitCount);
    p := Pointer(Self.FTypeInfo^.UnitNames);
    for i := 0 to Self.FTypeInfo^.UnitCount - 1 do
      Result[i] := ReadShortString(p);
  end;

  procedure DoMake;
  var
    units: TArray<string>;
    typeIter: PPTypeInfo;
    currUnit: Integer;
    typeName: string;
    i: Integer;
  begin
    Self.FLock.Acquire;
    try
      if Self.FNameToType <> nil then // presumes double-checked locking ok
        Exit;

      units := GetUnits;
      currUnit := 0;
      Self.FNameToType := TDictionary<string,PTypeInfo>.Create;
      Self.FTypeToName := TDictionary<PTypeInfo,string>.Create;
      for i := 0 to Self.FTypeInfo^.TypeCount - 1 do
      begin
        typeIter := Self.FTypeInfo^.TypeTable^[i];
        if typeIter = nil then
          Continue;
        if Integer(typeIter) = 1 then
        begin
          // inter-unit boundary
          Inc(currUnit);
          Continue;
        end;
        if typeIter^ = nil then // linker broke or fixup eliminated.
          Continue;
        typeName := units[currUnit] + '.' + UTF8ToString(typeIter^^.Name);
        if not Self.FNameToType.ContainsKey(typeName) then
          Self.FNameToType.Add(typeName, typeIter^);
        if not Self.FTypeToName.ContainsKey(typeIter^) then
          Self.FTypeToName.Add(typeIter^, typeName);
      end;
    finally
      Self.FLock.Release;
    end;
  end;

begin
  if Self.FNameToType <> nil then
    Exit;
  DoMake;
end;
{$IFEND}

{--------------------------------------------------------------------------------------------------}

function IsManaged(TypeInfo: PTypeInfo): Boolean;
var
  elTypePtr: PPTypeInfo;
begin
  if TypeInfo = nil then
    Exit(False);
  case TypeInfo^.Kind of
    tkDynArray, tkUString, tkWString, tkLString, tkInterface, tkVariant, tkMethod:
      Result := True;
    tkRecord:
      Result := GetTypeData(TypeInfo)^.ManagedFldCount > 0;
    tkArray:
    begin
      elTypePtr := GetTypeData(TypeInfo)^.ArrayData.ElType;
      Result := (elTypePtr <> nil) and IsManaged(elTypePtr^);
    end;
  else
    Result := False;
  end;
end;

{--------------------------------------------------------------------------------------------------}

const
  PassByRefBytes: array[0..47] of SmallInt = (
    // begin
    $48, $83, $EC, $28,
    // if TypeInfo = nil then
    $48, $85, $C9,
    $75, $05,
    // Exit(False);
    $48, $33, $C0,
    $EB, $79,
    // case TypeInfo^.Kind of
    $48, $0F, $B6, $01,
    $80, $E8, $05,
    $84, $C0,
    $74, $5A,
    $80, $E8, $07,
    $84, $C0,
    $74, $4F,
    $80, $E8, $01,
    $84, $C0,
    $74, $09,
    $80, $E8, $01,
    $84, $C0,
    $75, $56,
    $EB, $0D
  );

procedure PatchRtti;
{$HINTS OFF}
var
  Ctx: TRttiContext;
  Meth: TRttiMethod;
  P: PByte;
  Offset: Integer;
  n: UINT_PTR;
begin
{$IF CompilerVersion = 22}
  // Get the code pointer of the TMethodImplementation.TInvokeInfo.GetParamLocs method for which
  // extended RTTI is available to find the private type private method SaveArguments.
  P := Ctx.GetType(TMethodImplementation).GetField('FInvokeInfo').FieldType.GetMethod('GetParamLocs').CodeAddress;

  // Find for the "locs[i].SetArg(AFrame, Args[i]);" call and replace it with a call to our function.
  P := FindMethodBytes(P - 128 - SizeOf(SetArgCallBytes), SetArgCallBytes, 128 - SizeOf(SetArgCallBytes));
  if P <> nil then
  begin
    // Replace the call to SetArg with our method.
    @TParamLoc_SetArg := (P + 19 + 4) + PInteger(@P[19])^;
    Offset := PByte(@TParamLoc.SetArg) - (P + 19 + 4);
    if not WriteProcessMemory(GetCurrentProcess, P + 19, @Offset, SizeOf(Offset), n) then
      RaiseLastOSError;
  end
  else
    raise Exception.Create('Patching TMethodImplementation.TInvokeInfo.SaveArguments failed. Do you have set a breakpoint in the method?');

  RedirectFunction(@Rtti.IsManaged, @IsManaged);
{$IFEND}

{$IF CompilerVersion < 23}
  // Fix TRttiIntfMethod.DispatchInvoke
  Meth := ctx.GetType(TypeInfo(IIntfMethodHelper)).GetMethod('IntfMethod');
  RedirectFunction(GetVirtualMethod(Meth.ClassType, 13), @TRttiMethodFix.IntfDispatchInvoke);

  // Fix TRttiInstanceMethodEx.DispatchInvoke
  Meth := ctx.GetType(TInstanceMethodHelper).GetMethod('InstanceMethod');
  RedirectFunction(GetVirtualMethod(Meth.ClassType, 13), @TRttiMethodFix.InstanceDispatchInvoke);
{$IFEND}

{$IF Defined(CPUX64)}
  // Fix PassByRef
  P := FindMethodBytes(@Rtti.IsManaged, PassByRefBytes, 1000);
  if P <> nil then
    RedirectFunction(P, @PassByRef)
  else
    raise Exception.Create('Patching PassByRef failed. Do you have set a breakpoint in the method?');
{$IFEND}

{$IF CompilerVersion = 21}
  // Fix TRttiPackage.MakeTypeLookupTable
  RedirectFunction(TRttiPackage.GetMakeTypeLookupTableAddress, @TRttiPackage.MakeTypeLookupTableFix);
{$IFEND}

{$IF CompilerVersion < 23}
  // Fix TValue.TryAsOrdinal
  RedirectFunction(@TValue.TryAsOrdinal, @TValue.TryAsOrdinalFix);
{$IFEND}

{$IF CompilerVersion = 22}
  // Fix TRttiMethod.GetInvokeInfo
  // Find the private TRttiMethod.GetInvokeInfo method
  P := GetActualAddr(@TRttiMethod.CreateImplementation);
  if CompareMem(P, @TRttiMethod_CreateImplementationBytesCmp[0], SizeOf(TRttiMethod_CreateImplementationBytesCmp)) then
  begin
    P := PByte(P + 11 + 5) + PInteger(@P[11 + 1])^; // relative call => absolute address
    P := FindMethodBytes(P, TRttiMethod_GetInvokeInfoBytes, 400);
    if P <> nil then
    begin
      @OrgInvokeInfoAddParameter := PByte(P + 21 + 5) + PInteger(@P[21 + 1])^; // relative call => absolute address
      Offset := PByte(@FixInvokeInfoAddParameterEnter) - (P + 21 + 5);
      if not WriteProcessMemory(GetCurrentProcess, P + 21 + 1, @Offset, SizeOf(Offset), n) then
        RaiseLastOSError;
    end
    else
      raise Exception.Create('Patching TRttiMethod.GetInvokeInfoBytes failed. Do you have set a breakpoint in the method?');
  end
  else
    raise Exception.Create('TRttiMethod.CreateImplementation does not match the search pattern. Do you have set a breakpoint in the method?');
{$IFEND}
end;

initialization
  if not IsPatched and (ExtractFileName(ParamStr(0)) <> 'bds.exe') then
  try
    PatchRtti;
    IsPatched := True;
  except
    on e: Exception do
      if not (e is EAbort) and IsDebuggerPresent then
        MessageBox(0, PChar(e.ClassName + ': ' + e.Message), PChar(ExtractFileName(ParamStr(0))), MB_OK or MB_ICONERROR);
  end;
{$IFEND}

end.

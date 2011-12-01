unit RttiPatch;

interface

// Thanks to Andreas Hausladen

implementation

{$IF CompilerVersion = 22}
uses
  RTLConsts, PatchUtils, Rtti, SysConst, SysUtils, TypInfo, Windows;

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
  TVtable = array[0..MaxInt div 4 - 1] of Pointer;

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
    tkVariant:
      Result := IsConst or not (CC in [TCallConv.ccCdecl, TCallConv.ccStdCall, TCallConv.ccSafeCall]);
    tkRecord:
      if (CC in [TCallConv.ccCdecl, TCallConv.ccStdCall, TCallConv.ccSafeCall]) and not IsConst then
        Result := False
      else
        Result := GetTypeData(TypeInfo)^.RecSize > SizeOf(Pointer);
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
    Result := Rtti.Invoke(Code, ArgList, CallingConvention, ReturnType.Handle{, IsStatic})
  else if IsConstructor then
    Result := Rtti.Invoke(Code, ArgList, CallingConvention, Cls.ClassInfo{, IsStatic})
  else
    Result := Rtti.Invoke(Code, ArgList, CallingConvention, nil);
end;

{--------------------------------------------------------------------------------------------------}

procedure PatchRtti;
var
  Ctx: TRttiContext;
  Meth: TRttiMethod;
  P: PByte;
  Offset: Integer;
  n: Cardinal;
begin
  // Get the code pointer of the TMethodImplementation.TInvokeInfo.GetParamLocs method for which
  // extended RTTI is available to find the private type private method SaveArguments.
  Ctx := TRttiContext.Create;
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


  // Fix TRttiIntfMethod.DispatchInvoke
  Meth := ctx.GetType(TypeInfo(IIntfMethodHelper)).GetMethod('IntfMethod');
  RedirectFunction(GetVirtualMethod(Meth, $34), @TRttiMethodFix.IntfDispatchInvoke);

  // Fix TRttiInstanceMethodEx.DispatchInvoke
  Meth := ctx.GetType(TInstanceMethodHelper).GetMethod('InstanceMethod');
  RedirectFunction(GetVirtualMethod(Meth, $34), @TRttiMethodFix.InstanceDispatchInvoke);

  Ctx.Free;
end;

initialization
  if ExtractFileName(ParamStr(0)) <> 'bds.exe' then
  try
    PatchRtti;
  except
    on e: Exception do
      if not (e is EAbort) then
//        MessageBox(0, PChar(e.ClassName + ': ' + e.Message), PChar(ExtractFileName(ParamStr(0))), MB_OK or MB_ICONERROR);
  end;
{$IFEND}

end.

unit RttiSaveArgumentsPatch;

// Thanks to Andreas Hausladen for providing this fix

interface

implementation

uses
  Rtti,
  SysUtils,
  TypInfo,
  Windows;

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
  SetArgCallBytes: array[0..18] of Byte = (
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

procedure PatchRttiSaveArguments;
var
  Ctx: TRttiContext;
  P: PByte;
  Count, Offset: Integer;
  n: Cardinal;
begin
  // Get the code pointer of the TMethodImplementation.TInvokeInfo.GetParamLocs method for which
  // extended RTTI is available to find the private type private method SaveArguments.
  Ctx := TRttiContext.Create;
  P := Ctx.GetType(TMethodImplementation).GetField('FInvokeInfo').FieldType.GetMethod('GetParamLocs').CodeAddress;
  Ctx.Free;

  // Find the "locs[i].SetArg(AFrame, Args[i]);" call and replace it with a call to our function.
  P := P - SizeOf(SetArgCallBytes);
  Count := 128;
  while Count > 0 do
  begin
    while (Count > 0) and (P[0] <> SetArgCallBytes[0]) do
    begin
      Dec(P);
      Dec(Count);
    end;

    if (Count > 0) and CompareMem(P, @SetArgCallBytes[0], SizeOf(SetArgCallBytes)) then
    begin
      // Replace the call to SetArg with our method.
      @TParamLoc_SetArg := (P + 19 + 4) + PInteger(@P[19])^;
      Offset := PByte(@TParamLoc.SetArg) - (P + 19 + 4);
      WriteProcessMemory(GetCurrentProcess, P + 19, @Offset, SizeOf(Offset), n);
      Break;
    end;
    Dec(P);
    Dec(Count);
  end;

  Assert(Assigned(TParamLoc_SetArg), 'Patching TMethodImplementation.TInvokeInfo.SaveArguments failed. Do you have set a breakpoint in the method?');
end;

initialization
  PatchRttiSaveArguments;

end.

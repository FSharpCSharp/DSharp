unit ObjAutoPatch;

interface

implementation

// Thanks to Andreas Hausladen

{$IF CompilerVersion < 23}
uses
  ObjAuto, PatchUtils, SysUtils, TypInfo, Windows;

var
  IsPatched: Boolean = False;

{--------------------------------------------------------------------------------------------------}

const
{$IF CompilerVersion = 22}
  GetTypeSizeBytes: array[0..31] of SmallInt = (
    $0F, $B6, $10,                      // movzx edx,[eax]                //  0
    $83, $FA, $12,                      // cmp edx,$12                    //  3
    $0F, $87, $EF, $00, $00, $00,       // jnbe +$00EF                    //  6
    $0F, $B6, $92, -1, -1, -1, -1,      // movzx edx,[edx+$004e9266]      // 12
    $FF, $24, $95, -1, -1, -1, -1,      // jmp dword ptr [edx*4+$4e9279]  // 19
    $00, $03,                           // db $00, $03                    // 26
    $01, $03,                           // db $01, $03                    // 28
    $04, $05                            // db $04, $05                    // 30
  );
{$ELSE}
  GetTypeSizeBytes: array[0..31] of SmallInt = (
    $0F, $B6, $10,
    $83, $FA, $12,
    $0F, $87, $DB, $00, $00, $00,
    $0F, $B6, $92, -1, -1, -1, -1,
    $FF, $24, $95, -1, -1, -1, -1,
    $00, $01,
    $00, $01,
    $02, $03
  );
{$IFEND}

  SetLengthBytes: array[0..29] of SmallInt = (
    $8B, $45, $F8,
    $0F, $BF, $40, $08,
    $50,
    $8D, $47, $0C,
    $B9, -1, -1, -1, -1,
    $8B, $15, -1, -1, -1, -1,
    $E8, -1, -1, -1, -1,
    $83, $C4, $04
  );

  ParamCountBytes: array[0..6] of Byte = (
    $8B, $45, $F8,
    $0F, $B6, $40, $01
  );

function GetTypeSize(TypeInfo: PTypeInfo): Integer;
var
  TypeData: PTypeData;
begin
  case TypeInfo^.Kind of
    tkChar:
      Result := 1;
    tkWChar:
      Result := 2;
    tkInteger, tkEnumeration:
      begin
        TypeData := GetTypeData(TypeInfo);
        if TypeData^.MinValue >= 0 then
          if Cardinal(TypeData^.MaxValue) > $FFFF then
            Result := 4
          else if TypeData^.MaxValue > $FF then
            Result := 2
          else
            Result := 1
        else
          if (TypeData^.MaxValue > $7FFF) or (TypeData^.MinValue < -$7FFF - 1) then
            Result := 4
          else if (TypeData^.MaxValue > $7F) or (TypeData^.MinValue < -$7F - 1) then
            Result := 2
          else
            Result := 1;
      end;
    tkFloat:
      begin
        TypeData := GetTypeData(TypeInfo);
        case TypeData^.FloatType of
          ftSingle: Result := 4;
          ftComp, ftCurr, ftDouble: Result := 8;
        else
          Result := -1;
        end;
      end;
    tkString, tkLString, tkUString, tkWString, tkInterface, tkClass:
      Result := 4;
    tkMethod, tkInt64:
      Result := 8;
    tkVariant:
      Result := 16;
    tkSet:
      Result := 4;
  else
    Assert(False);
    Result := -1;
  end;
end;

procedure UseFunction(P: Pointer);
begin
end;

{--------------------------------------------------------------------------------------------------}

procedure PatchObjAuto;
var
  P: PByte;
  n: Cardinal;
begin
  // Replace ObjAuto.GetTypeSize
  UseFunction(@ObjAuto.ObjectInvoke); // The linker would remove GetTypeSize and the patch would fail
  P := FindMethodBytes(PByte(GetActualAddr(@ObjAuto.GetMethodInfo)) - 1100, GetTypeSizeBytes, 300);
  if P <> nil then
    RedirectFunction(P, @GetTypeSize)
  else
    raise Exception.Create('Patching ObjAuto.GetTypeSize failed. Do you have set a breakpoint in the method?');

  // Replace SetLength(ParamOffsets, TypeData^.PropCount) with SetLength(ParamOffsets, TypeData^.ParamCount);
  UseFunction(@ObjAuto.CreateMethodPointer);
  P := FindMethodBytes(PByte(GetActualAddr(@ObjAuto.GetInvokeInstance)), SetLengthBytes, 300);
  if P <> nil then
  begin
    if not WriteProcessMemory(GetCurrentProcess, P, @ParamCountBytes, SizeOf(ParamCountBytes), n) then
      RaiseLastOSError;
  end
  else
    raise Exception.Create('Patching TBaseMethodHandlerInstance.Create failed. Do you have set a breakpoint in the method?');
end;

initialization
  if not IsPatched and (ExtractFileName(ParamStr(0)) <> 'bds.exe') then
  try
    PatchObjAuto;
    IsPatched := True;
  except
    on e: Exception do
      if not (e is EAbort) then
        MessageBox(0, PChar(e.ClassName + ': ' + e.Message), PChar(ExtractFileName(ParamStr(0))), MB_OK or MB_ICONERROR);
  end;
{$IFEND}

end.

unit DSharp.Interception.InterfaceMapping;

interface

uses
  Rtti,
  TypInfo;

type
  TInterfaceMapping = record
  private
    FInterfaceMethods: TArray<TRttiMethod>;
    FInterfaceType: TRttiType;
    FTargetMethods: TArray<TRttiMethod>;
    FTargetType: TRttiType;
  public
    constructor Create(InterfaceType, TargetType: PTypeInfo);

    property InterfaceMethods: TArray<TRttiMethod> read FInterfaceMethods;
    property InterfaceType: TRttiType read FInterfaceType;
    property TargetMethods: TArray<TRttiMethod> read FTargetMethods;
    property TargetType: TRttiType read FTargetType;
  end;

implementation

uses
  DSharp.Core.Reflection;

//type
//  PCallStub = ^TCallStub;
//  TCallStub = record
//  private
//    FCodeAddress: Pointer;
//    FVirtualIndex: SmallInt;
//  public
//    constructor Create(Address: Pointer);
//    property CodeAddress: Pointer read FCodeAddress;
//    property VirtualIndex: SmallInt read FVirtualIndex;
//  end;
//
//constructor TCallStub.Create(Address: Pointer);
//var
//  i: Integer;
//begin
//  FVirtualIndex := -1;
//  i := 0;
//  while True do
//  begin
//    case PByte(Address)[i] of
//      $C3:
//      begin
//        FVirtualIndex := PByte(Address)[i - 4] div SizeOf(Pointer);
//        Break;
//      end;
//      $E9:
//      begin
//        FCodeAddress := Pointer(PNativeUInt(@PByte(Address)[i + 1])^ + NativeUInt(@PByte(Address)[i]) + 5);
//        Break;
//      end;
//    end;
//    Inc(i);
//  end;
//end;

{ TInterfaceMapping }

constructor TInterfaceMapping.Create(InterfaceType, TargetType: PTypeInfo);
//var
//  interfaceEntry: PInterfaceEntry;
//  i, k: Integer;
//  methods: TArray<TRttiMethod>;
//  stub: TCallStub;
type
  PVtable = ^TVtable;
  TVtable = array[0..MaxInt div SizeOf(Pointer) - 1] of Pointer;
begin
  FInterfaceType := GetRttiType(InterfaceType);
  FInterfaceMethods := FInterfaceType.GetMethods;
  FTargetType := GetRttiType(TargetType);
  SetLength(FTargetMethods, Length(FInterfaceMethods));

//  interfaceEntry := FTargetType.AsInstance.MetaclassType.GetInterfaceEntry(
//    FInterfaceType.AsInterface.GUID);
//  methods := FTargetType.GetMethods;
//  for i := 0 to High(FInterfaceMethods) do
//  begin
//    stub := TCallStub.Create(PVtable(interfaceEntry.VTable)[FInterfaceMethods[i].VirtualIndex]);
//    for k := 0 to High(methods) do
//    begin
//      if stub.VirtualIndex = -1 then
//      begin
//        if methods[k].CodeAddress = stub.CodeAddress then
//        begin
//          FTargetMethods[i] := methods[k];
//          Break;
//        end;
//      end
//      else
//      begin
//        if methods[k].VirtualIndex = stub.VirtualIndex then
//        begin
//          FTargetMethods[i] := methods[k];
//          Break;
//        end;
//      end;
//    end;

//    FTargetMethods[i] := FTargetType.GetMethod(
//      PVtable(interfaceEntry.VTable)[FInterfaceMethods[i].VirtualIndex]);
//  end;
end;

end.

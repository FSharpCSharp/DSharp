unit DSharp.Interception.MethodImplementationInfo;

interface

uses
  Rtti;

type
  TMethodImplementationInfo = record
  private
    FInterfaceMethodInfo: TRttiMethod;
    FImplementationMethodInfo: TRttiMethod;
  public
    constructor Create(InterfaceMethodInfo, ImplementationMethodInfo: TRttiMethod);
    class operator Equal(const Left, Right: TMethodImplementationInfo): Boolean;
    class operator NotEqual(const Left, Right: TMethodImplementationInfo): Boolean;
    function ToString: string;
    property InterfaceMethodInfo: TRttiMethod read FInterfaceMethodInfo;
    property ImplementationMethodInfo: TRttiMethod read FImplementationMethodInfo;
  end;

implementation

uses
  SysUtils;

{ TMethodImplementationInfo }

constructor TMethodImplementationInfo.Create(InterfaceMethodInfo,
  ImplementationMethodInfo: TRttiMethod);
begin
  FInterfaceMethodInfo := InterfaceMethodInfo;
  FImplementationMethodInfo := ImplementationMethodInfo;
end;

class operator TMethodImplementationInfo.Equal(const Left,
  Right: TMethodImplementationInfo): Boolean;
begin
  Result := (Left.InterfaceMethodInfo = Right.InterfaceMethodInfo)
    and (Left.ImplementationMethodInfo = Right.ImplementationMethodInfo);
end;

class operator TMethodImplementationInfo.NotEqual(const Left,
  Right: TMethodImplementationInfo): Boolean;
begin
  Result := not (Left = Right);
end;

function TMethodImplementationInfo.ToString: string;
begin
  if not Assigned(FInterfaceMethodInfo) then
  begin
    Result := Format('No interface, implementation %0:s.%1:s', [
      FImplementationMethodInfo.Parent.Name, FImplementationMethodInfo.Name]);
  end
  else
  begin
    Result := Format('Interface %0:s.%1:s, implementation %2:s.%3:s', [
      FInterfaceMethodInfo.Parent.Name, FInterfaceMethodInfo.Name,
      FImplementationMethodInfo.Parent.Name, FImplementationMethodInfo.Name])
  end;
end;

end.

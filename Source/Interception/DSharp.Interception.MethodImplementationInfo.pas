(*
  Copyright (c) 2012-2014, Stefan Glienke
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:

  - Redistributions of source code must retain the above copyright notice,
    this list of conditions and the following disclaimer.
  - Redistributions in binary form must reproduce the above copyright notice,
    this list of conditions and the following disclaimer in the documentation
    and/or other materials provided with the distribution.
  - Neither the name of this library nor the names of its contributors may be
    used to endorse or promote products derived from this software without
    specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
  POSSIBILITY OF SUCH DAMAGE.
*)

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

(*
  Copyright (c) 2011-2012, Stefan Glienke
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

unit DSharp.Core.DuckTyping;

interface

type
  Duck<T: IInterface> = record
  private
    FInstance: T;
    function GetInstance: TObject;
    procedure SetInstance(const Value: TObject);
  public
    class operator Implicit(const Value: TObject): Duck<T>;
    class operator Implicit(const Value: Duck<T>): T;

    property Instance: TObject read GetInstance write SetInstance;
  end;

implementation

uses
  DSharp.Core.Dynamics,
  DSharp.Core.Reflection,
  Rtti,
  SysUtils;

{ Duck<T> }

class operator Duck<T>.Implicit(const Value: TObject): Duck<T>;
begin
  Result.Instance := Value;
end;

function Duck<T>.GetInstance: TObject;
begin
  if Assigned(FInstance) then
  begin
    Result := (IInterface(FInstance) as TVirtualObjectInterface).Instance;
  end
  else
  begin
    Result := nil;
  end;
end;

class operator Duck<T>.Implicit(const Value: Duck<T>): T;
begin
  Result := Value.FInstance;
end;

procedure Duck<T>.SetInstance(const Value: TObject);
var
  LType: TRttiType;
  LVirtualInterface: TVirtualObjectInterface;
begin
  LVirtualInterface := (IInterface(FInstance) as TVirtualObjectInterface);
  if Assigned(LVirtualInterface)
    and (LVirtualInterface.Instance.ClassType = Value.ClassType) then
  begin
    LVirtualInterface.Instance := Value;
  end
  else
  begin
    LType := GetRttiType(TypeInfo(T));
    if LType is TRttiInterfaceType then
    begin
      LVirtualInterface := TVirtualObjectInterface.Create(LType.Handle, Value);
      Supports(LVirtualInterface, TRttiInterfaceType(LType).GUID, FInstance);
    end;
  end;
end;

end.
(*
  Copyright (c) 2011-2013, Stefan Glienke
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

unit DSharp.Testing.Mock;

{$IF COMPILERVERSION < 22}
  {$MESSAGE FATAL 'This unit requires Delphi XE or higher.'}
{$IFEND}

interface

uses
  DSharp.Testing.Mock.Interfaces,
  DSharp.Testing.Mock.Setup,
  Rtti,
  SysUtils;

type
  TValue = Rtti.TValue;

  Mock<T> = record
  private
    FMock: IMock<T>;
    function GetInstance: T;
    function GetMock: IMock<T>;
    function GetMode: TMockMode;
    procedure SetMode(const Value: TMockMode);
    property Mock: IMock<T> read GetMock;
  public
    class function Create: Mock<T>; static;
    procedure Free;

    class operator Implicit(var Value: Mock<T>): IMock<T>;
    class operator Implicit(var Value: Mock<T>): T;
    class operator Implicit(Value: T): Mock<T>;

    function Setup: Setup<T>;
    procedure Verify;

    property Instance: T read GetInstance;
    property Mode: TMockMode read GetMode write SetMode;
  end;

const
  Mock = TMockMode.Mock;
  Stub = TMockMode.Stub;

implementation

uses
  DSharp.Testing.Mock.Internals;

{ Mock<T> }

class function Mock<T>.Create: Mock<T>;
begin
  Result.Instance;
end;

procedure Mock<T>.Free;
begin
  FMock := nil;
end;

function Mock<T>.GetInstance: T;
begin
  Result := Mock.Instance;
end;

function Mock<T>.GetMock: IMock<T>;
begin
  if not Assigned(FMock) then
  begin
    FMock := TMock<T>.Create();
  end;
  Result := FMock;
end;

function Mock<T>.GetMode: TMockMode;
begin
  Result := Mock.Mode;
end;

class operator Mock<T>.Implicit(var Value: Mock<T>): IMock<T>;
begin
  Result := Value.Mock;
end;

class operator Mock<T>.Implicit(var Value: Mock<T>): T;
begin
  Result := Value.Mock.Instance;
end;

class operator Mock<T>.Implicit(Value: T): Mock<T>;
begin
  Result.FMock := nil;
end;

procedure Mock<T>.SetMode(const Value: TMockMode);
begin
  Mock.Mode := Value;
end;

function Mock<T>.Setup: Setup<T>;
begin
  Result := Mock.Setup;
end;

procedure Mock<T>.Verify;
begin
  Mock.Verify;
end;

end.

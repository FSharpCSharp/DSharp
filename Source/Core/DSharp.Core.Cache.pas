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

unit DSharp.Core.Cache;

interface

uses
  Classes;

type
  ICache<T: TPersistent, constructor> = interface
    function GetInstance: T;
    procedure SetInstance(const Value: T);
    procedure Restore;
    procedure Store;
    property Instance: T read GetInstance write SetInstance;
  end;

  TCache<T: TPersistent, constructor> = class(TInterfacedObject, ICache<T>)
  private
    FInstance: T;
    FStorage: T;
    function GetInstance: T;
    procedure SetInstance(const Value: T);
  public
    constructor Create(Instance: T);
    destructor Destroy; override;

    procedure Restore;
    procedure Store;

    property Instance: T read GetInstance write SetInstance;
  end;

  Cache<T: TPersistent, constructor> = record
  private
    FCache: ICache<T>;
    procedure EnsureInstance;
    function GetInstance: T;
    procedure SetInstance(const Value: T);
  public
    procedure Restore;
    procedure Store;

    class operator Implicit(const Value: Cache<T>): T;
    class operator Implicit(const Value: T): Cache<T>;

    property Instance: T read GetInstance write SetInstance;
  end;

implementation

{ TCache<T> }

constructor TCache<T>.Create(Instance: T);
begin
  inherited Create();

  FInstance := Instance;
  FStorage := T.Create();
end;

destructor TCache<T>.Destroy;
begin
  FStorage.Free();

  inherited Destroy();
end;

function TCache<T>.GetInstance: T;
begin
  Result := FInstance;
end;

procedure TCache<T>.Restore;
begin
  if Assigned(FInstance) then
  begin
    FInstance.Assign(FStorage);
  end;
end;

procedure TCache<T>.SetInstance(const Value: T);
begin
  FInstance := Value;
end;

procedure TCache<T>.Store;
begin
  if Assigned(FInstance) then
  begin
    FStorage.Assign(FInstance);
  end;
end;

{ Cache<T> }

procedure Cache<T>.EnsureInstance;
begin
  if not Assigned(FCache) then
  begin
    FCache := TCache<T>.Create(Default(T));
  end;
end;

function Cache<T>.GetInstance: T;
begin
  EnsureInstance();
  Result := FCache.Instance;
end;

class operator Cache<T>.Implicit(const Value: T): Cache<T>;
begin
  Result.Instance := Value;
end;

class operator Cache<T>.Implicit(const Value: Cache<T>): T;
begin
  Result := Value.Instance;
end;

procedure Cache<T>.Restore;
begin
  EnsureInstance();
  FCache.Restore();
end;

procedure Cache<T>.SetInstance(const Value: T);
begin
  EnsureInstance();
  FCache.Instance := Value;
end;

procedure Cache<T>.Store;
begin
  EnsureInstance();
  FCache.Store();
end;

end.

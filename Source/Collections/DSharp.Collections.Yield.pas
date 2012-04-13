(*
  Copyright (c) 2011, Stefan Glienke
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

unit DSharp.Collections.Yield;

interface

{$IFNDEF MSWINDOWS}
  {$DEFINE USE_THREADING}
{$ENDIF}

uses
{$IFDEF USE_THREADING}
  Classes,
  DSharp.Collections.Threading,
{$ELSE}
  DSharp.Collections.Fibers,
  DSharp.Core.Fibers,
{$ENDIF}
  DSharp.Collections,
  Rtti,
  SysUtils;

type
{$IFDEF USE_THREADING}
  TWorkerBase = TEnumeratorThread;
{$ELSE}
  TWorkerBase = TEnumeratorFiber;
{$ENDIF}

  TYieldEnumerable = class
  private
    class function GetCurrent: TValue; static;
    class function GetCurrentWorker: TWorkerBase; static;
  protected
    class procedure Yield(const Value: TValue); overload;
    class property Current: TValue read GetCurrent;
  end;

  TYieldEnumerable<T> = class(TEnumerable<T>)
  private
    FEnumeration: TProc;

    type
{$IFDEF USE_THREADING}
      TWorker = TEnumeratorThread<T>;
{$ELSE}
      TWorker = TEnumeratorFiber<T>;
{$ENDIF}

      TEnumerator = class(TEnumerator<T>)
      private
        FWorker: TWorker;
      protected
        function GetCurrent: T; override;
      public
        constructor Create(AOwner: TYieldEnumerable<T>; const Enumeration: TProc);
        destructor Destroy; override;
        function MoveNext: Boolean; override;
      end;

    class function GetCurrent: T; static;
    class function GetCurrentWorker: TWorker; static;
  protected
    class procedure Yield(const Value: T); overload;
    class procedure Yield(const Value: TValue); overload;
    class property Current: T read GetCurrent;
  public
    constructor Create(const AEnumeration: TProc);
    function GetEnumerator: IEnumerator<T>; override;
  end;

  Yield = record
{$IFDEF CPUX64}
{$HINTS OFF}
  private
    FValue: Pointer; // workaround for 64-bit bug
{$HINTS ON}
{$ENDIF}
  public
    class operator Implicit(const Value: IInterface): Yield;
    class operator Implicit(const Value: TObject): Yield;
    class operator Implicit(const Value: TValue): Yield;
    class operator Implicit(const Value: Variant): Yield;
    class operator Implicit(const Value: Yield): IInterface;
    class operator Implicit(const Value: Yield): TObject;
    class operator Implicit(const Value: Yield): TValue;
    class operator Implicit(const Value: Yield): Variant;
  end;

  Yield<T> = record
{$IFDEF CPUX64}
{$HINTS OFF}
  private
    FValue: Pointer; // workaround for 64-bit bug
{$HINTS ON}
{$ENDIF}
  public
    class operator Implicit(const Value: T): Yield<T>;
    class operator Implicit(const Value: TValue): Yield<T>;
    class operator Implicit(const Value: Yield<T>): T;
  end;

implementation

{ Yield }

class operator Yield.Implicit(const Value: IInterface): Yield;
begin
  TYieldEnumerable.Yield(TValue.From<IInterface>(Value));
end;

class operator Yield.Implicit(const Value: TObject): Yield;
begin
  TYieldEnumerable.Yield(TValue.From<TObject>(Value));
end;

class operator Yield.Implicit(const Value: TValue): Yield;
begin
  TYieldEnumerable.Yield(Value);
end;

class operator Yield.Implicit(const Value: Variant): Yield;
begin
  TYieldEnumerable.Yield(TValue.From(Value));
end;

class operator Yield.Implicit(const Value: Yield): IInterface;
begin
  Result := TYieldEnumerable.Current.AsInterface;
end;

class operator Yield.Implicit(const Value: Yield): TObject;
begin
  Result := TYieldEnumerable.Current.AsObject;
end;

class operator Yield.Implicit(const Value: Yield): TValue;
begin
  Result := TYieldEnumerable.Current;
end;

class operator Yield.Implicit(const Value: Yield): Variant;
begin
  Result := TYieldEnumerable.Current.AsVariant;
end;

{ Yield<T> }

class operator Yield<T>.Implicit(const Value: T): Yield<T>;
begin
  TYieldEnumerable<T>.Yield(Value);
end;

class operator Yield<T>.Implicit(const Value: TValue): Yield<T>;
begin
  TYieldEnumerable<T>.Yield(Value.AsType<T>);
end;

class operator Yield<T>.Implicit(const Value: Yield<T>): T;
begin
  Result := TYieldEnumerable<T>.Current;
end;

{ TYieldEnumerable }

class function TYieldEnumerable.GetCurrent: TValue;
begin
  Result := GetCurrentWorker.Result;
end;

class function TYieldEnumerable.GetCurrentWorker: TWorkerBase;
begin
{$IFDEF USE_THREADING}
  Result := TWorkerBase(TThread.CurrentThread);
{$ELSE}
  Result := TWorkerBase(TFiber.CurrentFiber);
{$ENDIF}
end;

class procedure TYieldEnumerable.Yield(const Value: TValue);
begin
  GetCurrentWorker.Yield(Value);
end;

{ TYieldEnumerable<T> }

constructor TYieldEnumerable<T>.Create(const AEnumeration: TProc);
begin
  inherited Create();
  FEnumeration := AEnumeration;
end;

class function TYieldEnumerable<T>.GetCurrent: T;
begin
  Result := GetCurrentWorker.Result;
end;

class function TYieldEnumerable<T>.GetCurrentWorker: TWorker;
begin
{$IFDEF USE_THREADING}
  Result := TWorker(TThread.CurrentThread);
{$ELSE}
  Result := TWorker(TFiber.CurrentFiber);
{$ENDIF}
end;

function TYieldEnumerable<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TEnumerator.Create(Self, FEnumeration);
end;

class procedure TYieldEnumerable<T>.Yield(const Value: T);
begin
  GetCurrentWorker.Yield(Value);
end;

class procedure TYieldEnumerable<T>.Yield(const Value: TValue);
begin
  GetCurrentWorker.Yield(Value);
end;

{ TYieldEnumerable<T>.TEnumerator }

constructor TYieldEnumerable<T>.TEnumerator.Create(
  AOwner: TYieldEnumerable<T>; const Enumeration: TProc);
begin
  FWorker := TWorker.Create(Enumeration);
end;

destructor TYieldEnumerable<T>.TEnumerator.Destroy;
begin
  FWorker.Free();
  inherited;
end;

function TYieldEnumerable<T>.TEnumerator.GetCurrent: T;
begin
  Result := FWorker.Result;
end;

function TYieldEnumerable<T>.TEnumerator.MoveNext: Boolean;
begin
  FWorker.Continue();
  Result := not FWorker.Finished;
end;

end.

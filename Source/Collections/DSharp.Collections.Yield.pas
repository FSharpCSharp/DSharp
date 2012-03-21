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

{.$DEFINE USE_COLLECTIONS}
{.$DEFINE USE_SPRING}
{$DEFINE USE_FIBERS}

uses
{$IFDEF USE_COLLECTIONS}
  Collections.Base,
{$ELSEIF DEFINED(USE_SPRING)}
  Spring.Collections,
  Spring.Collections.Base,
{$ELSE}
  DSharp.Collections,
{$IFEND}
{$IFDEF USE_FIBERS}
  DSharp.Collections.Fibers,
  DSharp.Core.Fibers,
{$ELSE}
  Classes,
  DSharp.Collections.Threading,
{$ENDIF}
  Rtti,
  SysUtils;

type
{$IFDEF USE_FIBERS}
  TWorkerBase = TEnumeratorFiber;
{$ELSE}
  TWorkerBase = TEnumeratorThread;
{$ENDIF}

  TDelegateEnumerable = class
  private
    class function GetCurrent: TValue; static;
    class function GetCurrentWorker: TWorkerBase; static;
  protected
    class procedure Yield(const Value: TValue); overload;
    class property Current: TValue read GetCurrent;
  end;

{$IFDEF USE_COLLECTIONS}
  TDelegateEnumerable<T> = class(TSequence<T>)
{$ELSEIF DEFINED(USE_SPRING)}
  TDelegateEnumerable<T> = class(TEnumerableBase<T>)
{$ELSE}
  TDelegateEnumerable<T> = class(TEnumerable<T>)
{$IFEND}
  private
    FEnumeration: TProc;

    type
{$IFDEF USE_FIBERS}
      TWorker = TEnumeratorFiber<T>;
{$ELSE}
      TWorker = TEnumeratorThread<T>;
{$ENDIF}

{$IFDEF USE_COLLECTIONS}
      TEnumerator = class(TAbstractEnumerator<T>)
{$ELSEIF DEFINED(USE_SPRING)}
      TEnumerator = class(TEnumeratorBase<T>)
{$ELSE}
      TEnumerator = class(TEnumerator<T>)
{$IFEND}
      private
        FWorker: TWorker;
      protected
{$IFDEF USE_COLLECTIONS}
        function TryMoveNext(out ACurrent: T): Boolean; override;
{$ELSE}
        function GetCurrent: T; override;
      public
        function MoveNext: Boolean; override;
{$ENDIF}
      public
        constructor Create(AOwner: TDelegateEnumerable<T>; const Enumeration: TProc);
        destructor Destroy; override;
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
  TDelegateEnumerable.Yield(TValue.From<IInterface>(Value));
end;

class operator Yield.Implicit(const Value: TObject): Yield;
begin
  TDelegateEnumerable.Yield(TValue.From<TObject>(Value));
end;

class operator Yield.Implicit(const Value: TValue): Yield;
begin
  TDelegateEnumerable.Yield(Value);
end;

class operator Yield.Implicit(const Value: Variant): Yield;
begin
  TDelegateEnumerable.Yield(TValue.From(Value));
end;

class operator Yield.Implicit(const Value: Yield): IInterface;
begin
  Result := TDelegateEnumerable.Current.AsInterface;
end;

class operator Yield.Implicit(const Value: Yield): TObject;
begin
  Result := TDelegateEnumerable.Current.AsObject;
end;

class operator Yield.Implicit(const Value: Yield): TValue;
begin
  Result := TDelegateEnumerable.Current;
end;

class operator Yield.Implicit(const Value: Yield): Variant;
begin
  Result := TDelegateEnumerable.Current.AsVariant;
end;

{ Yield<T> }

class operator Yield<T>.Implicit(const Value: T): Yield<T>;
begin
  TDelegateEnumerable<T>.Yield(Value);
end;

class operator Yield<T>.Implicit(const Value: TValue): Yield<T>;
begin
  TDelegateEnumerable<T>.Yield(Value.AsType<T>);
end;

class operator Yield<T>.Implicit(const Value: Yield<T>): T;
begin
  Result := TDelegateEnumerable<T>.Current;
end;

{ TDelegateEnumerable }

class function TDelegateEnumerable.GetCurrent: TValue;
begin
  Result := GetCurrentWorker.Result;
end;

class function TDelegateEnumerable.GetCurrentWorker: TWorkerBase;
begin
{$IFDEF USE_FIBERS}
  Result := TWorkerBase(TFiber.CurrentFiber);
{$ELSE}
  Result := TWorkerBase(TThread.CurrentThread);
{$ENDIF}
end;

class procedure TDelegateEnumerable.Yield(const Value: TValue);
begin
  GetCurrentWorker.Yield(Value);
end;

{ TDelegateEnumerable<T> }

constructor TDelegateEnumerable<T>.Create(const AEnumeration: TProc);
begin
  inherited Create();
  FEnumeration := AEnumeration;
end;

class function TDelegateEnumerable<T>.GetCurrent: T;
begin
  Result := GetCurrentWorker.Result;
end;

class function TDelegateEnumerable<T>.GetCurrentWorker: TWorker;
begin
{$IFDEF USE_FIBERS}
  Result := TWorker(TFiber.CurrentFiber);
{$ELSE}
  Result := TWorker(TThread.CurrentThread);
{$ENDIF}
end;

function TDelegateEnumerable<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TEnumerator.Create(Self, FEnumeration);
end;

class procedure TDelegateEnumerable<T>.Yield(const Value: T);
begin
  GetCurrentWorker.Yield(Value);
end;

class procedure TDelegateEnumerable<T>.Yield(const Value: TValue);
begin
  GetCurrentWorker.Yield(Value);
end;

{ TDelegateEnumerable<T>.TEnumerator }

constructor TDelegateEnumerable<T>.TEnumerator.Create(
  AOwner: TDelegateEnumerable<T>; const Enumeration: TProc);
begin
{$IFDEF USE_COLLECTIONS}
  inherited Create(AOwner);
{$ENDIF}
  FWorker := TWorker.Create(Enumeration);
end;

destructor TDelegateEnumerable<T>.TEnumerator.Destroy;
begin
  FWorker.Free();
  inherited;
end;

{$IFDEF USE_COLLECTIONS}
function TDelegateEnumerable<T>.TEnumerator.TryMoveNext(out ACurrent: T): Boolean;
begin
  FWorker.Continue();
  Result := not FWorker.Finished;
  if Result then
  begin
    ACurrent :=  FWorker.Result;
  end;
end;
{$ELSE}

function TDelegateEnumerable<T>.TEnumerator.GetCurrent: T;
begin
  Result := FWorker.Result;
end;

function TDelegateEnumerable<T>.TEnumerator.MoveNext: Boolean;
begin
  FWorker.Continue();
  Result := not FWorker.Finished;
end;
{$ENDIF}

end.

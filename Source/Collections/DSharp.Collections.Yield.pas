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
  SysUtils;

type
{$IFDEF USE_COLLECTIONS}
  TDelegateEnumerable<T> = class(TSequence<T>)
{$ELSEIF DEFINED(USE_SPRING)}
  TDelegateEnumerable<T> = class(TEnumerableBase<T>)
{$ELSE}
  TDelegateEnumerable<T> = class(TEnumerable<T>)
{$IFEND}
  strict private
    FEnumeration: TProc;

    type
{$IFDEF USE_COLLECTIONS}
      TEnumerator = class(TInterfacedObject, IEnumerator<T>)
{$ELSEIF DEFINED(USE_SPRING)}
      TEnumerator = class(TEnumeratorBase<T>)
{$ELSE}
      TEnumerator = class(TEnumerator<T>)
{$IFEND}
      private
{$IFDEF USE_FIBERS}
        FFiber: TEnumeratorFiber<T>;
{$ELSE}
        FThread: TEnumeratorThread<T>;
{$ENDIF}
{$IFNDEF USE_COLLECTIONS}
      protected
        function GetCurrent: T; override;
      public
        function MoveNext: Boolean; override;
{$ENDIF}
      public
        constructor Create(const Enumeration: TProc);
        destructor Destroy; override;
{$IFDEF USE_COLLECTIONS}
        function GetCurrent: T;
        function MoveNext: Boolean;
{$ENDIF}
      end;
    class function GetCurrent: T; static;
  protected
    class procedure Yield(const Value: T);
    class property Current: T read GetCurrent;
  public
    constructor Create(const AEnumeration: TProc);
    function GetEnumerator: IEnumerator<T>; override;
  end;

  Yield<T> = record
{$IFDEF CPUX64}
{$HINTS OFF}
  private
    FValue: Pointer; // workaround for 64-bit bug
{$HINTS ON}
{$ENDIF}
  public
    class operator Implicit(const Value: T): Yield<T>; inline;
    class operator Implicit(const Value: Yield<T>): T; inline;
  end;

implementation

{ Yield<T> }

class operator Yield<T>.Implicit(const Value: T): Yield<T>;
begin
  TDelegateEnumerable<T>.Yield(Value);
end;

class operator Yield<T>.Implicit(const Value: Yield<T>): T;
begin
  Result := TDelegateEnumerable<T>.Current;
end;

{ TDelegateEnumerable<T> }

constructor TDelegateEnumerable<T>.Create(const AEnumeration: TProc);
begin
  inherited Create();
  FEnumeration := AEnumeration;
end;

class function TDelegateEnumerable<T>.GetCurrent: T;
begin
{$IFDEF USE_FIBERS}
  Result := TEnumeratorFiber<T>(TFiber.CurrentFiber).Result;
{$ELSE}
  Result := TEnumeratorThread<T>(TThread.CurrentThread).Result;
{$ENDIF}
end;

function TDelegateEnumerable<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TEnumerator.Create(FEnumeration);
end;

class procedure TDelegateEnumerable<T>.Yield(const Value: T);
begin
{$IFDEF USE_FIBERS}
  TEnumeratorFiber<T>(TFiber.CurrentFiber).Yield(Value);
{$ELSE}
  TEnumeratorThread<T>(TThread.CurrentThread).Yield(Value);
{$ENDIF}
end;

{ TDelegateEnumerable<T>.TEnumerator }

constructor TDelegateEnumerable<T>.TEnumerator.Create(const Enumeration: TProc);
begin
{$IFDEF USE_FIBERS}
  FFiber := TEnumeratorFiber<T>.Create(Enumeration);
{$ELSE}
  FThread := TEnumeratorThread<T>.Create(Enumeration);
{$ENDIF}
end;

destructor TDelegateEnumerable<T>.TEnumerator.Destroy;
begin
{$IFDEF USE_FIBERS}
  FFiber.Free();
{$ELSE}
  FThread.Free();
{$ENDIF}
  inherited;
end;

function TDelegateEnumerable<T>.TEnumerator.GetCurrent: T;
begin
{$IFDEF USE_FIBERS}
  Result := FFiber.Result;
{$ELSE}
  Result := FThread.Result;
{$ENDIF}
end;

function TDelegateEnumerable<T>.TEnumerator.MoveNext: Boolean;
begin
{$IFDEF USE_FIBERS}
  FFiber.Resume();
  Result := not FFiber.Finished;
{$ELSE}
  FThread.Continue();
  Result := not FThread.Finished;
{$ENDIF}
end;

end.

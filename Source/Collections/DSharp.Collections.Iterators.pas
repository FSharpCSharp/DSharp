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

unit DSharp.Collections.Iterators;

{$IFDEF MSWINDOWS}
  {$DEFINE USE_FIBERS}
{$ENDIF}

interface

uses
  Spring.Collections,
  Spring.Collections.Base,
  Rtti,
  SysUtils;

type
  TIteratorProc<T> = reference to function(var current: T): Boolean;

  TCustomIterator<T> = class(TIterator<T>)
  private
    fProc: TIteratorProc<T>;
  protected
    function Clone: TIterator<T>; override;
    function TryMoveNext(var current: T): Boolean; override;
  public
    constructor Create(const proc: TIteratorProc<T>);
  end;

  TIteratorBlock<T> = class(TIterator<T>)
  private
    fProc: TProc;
    fEnumerator: IEnumerator<T>;
  protected
    function Clone: TIterator<T>; override;
    procedure Start; override;
    function TryMoveNext(var current: T): Boolean; override;
  public
    constructor Create(const proc: TProc);
  end;

  Iterator<T> = record
  private
    fValue: TProc;
  public
    class operator Implicit(const value: TProc): Iterator<T>;
    class operator Implicit(const value: Iterator<T>): IEnumerable<T>;
  end;

  Yield<T> = record
{$IFDEF CPUX64}
{$HINTS OFF}
  private
    fValue: Pointer; // workaround for 64-bit bug
{$HINTS ON}
{$ENDIF}
  public
    class operator Implicit(const value: T): Yield<T>;
    class operator Implicit(const value: TValue): Yield<T>;
    class operator Implicit(const value: Yield<T>): T;
  end;

procedure Yield(const value);

implementation

uses
{$IFDEF USE_FIBERS}
  DSharp.Collections.Fibers,
  DSharp.Core.Fibers;
{$ELSE}
  DSharp.Collections.Threading;
{$ENDIF}

type
{$IFDEF USE_FIBERS}
  TIterator = TIteratorFiber;
{$ELSE}
  TIterator = TIteratorThread;
{$ENDIF}

procedure Yield(const value); inline
begin
  TIterator.Yield(value);
end;

{ TCustomIterator<T> }

constructor TCustomIterator<T>.Create(const proc: TIteratorProc<T>);
begin
  inherited Create;
  fProc := proc;
end;

function TCustomIterator<T>.Clone: TIterator<T>;
begin
  Result := TCustomIterator<T>.Create(fProc);
end;

function TCustomIterator<T>.TryMoveNext(var current: T): Boolean;
begin
  Result := fProc(current);
end;

{ TIteratorBlock<T> }

constructor TIteratorBlock<T>.Create(const proc: TProc);
begin
  inherited Create;
  fProc := proc;
end;

function TIteratorBlock<T>.Clone: TIterator<T>;
begin
  Result := TIteratorBlock<T>.Create(fProc);
end;

procedure TIteratorBlock<T>.Start;
begin
{$IFDEF USE_FIBERS}
  fEnumerator := TIteratorFiber<T>.Create(fProc);
{$ELSE}
  fEnumerator := TIteratorThread<T>.Create(fProc);
{$ENDIF}
end;

function TIteratorBlock<T>.TryMoveNext(var current: T): Boolean;
begin
  Result := fEnumerator.MoveNext;
  if Result then
    current := fEnumerator.Current;
end;

{ Iterator<T> }

class operator Iterator<T>.Implicit(const value: TProc): Iterator<T>;
begin
  Result.fValue := value;
end;

class operator Iterator<T>.Implicit(const value: Iterator<T>): IEnumerable<T>;
begin
  Result := TIteratorBlock<T>.Create(value.fValue);
end;

{ Yield<T> }

class operator Yield<T>.Implicit(const value: T): Yield<T>;
begin
  TIterator.Yield(value);
end;

class operator Yield<T>.Implicit(const Value: TValue): Yield<T>;
begin
  TIterator.Yield(value);
end;

class operator Yield<T>.Implicit(const value: Yield<T>): T;
begin
{$IFDEF USE_FIBERS}
  Result := TIteratorFiber<T>(TFiber.CurrentFiber).Current;
{$ELSE}
  Result := TIteratorThread<T>(TIteratorThread.CurrentThread).Current;
{$ENDIF}
end;

end.

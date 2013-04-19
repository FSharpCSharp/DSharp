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
  TYieldEnumerator = TEnumeratorThread;
{$ELSE}
  TYieldEnumerator = TEnumeratorFiber;
{$ENDIF}

  TYieldEnumerable<T> = class(TEnumerable<T>)
  private
    type
{$IFDEF USE_THREADING}
      TEnumeratorBase = class(TEnumeratorThread<T>, IEnumerator)
{$ELSE}
      TEnumeratorBase = class(TEnumeratorFiber<T>, IEnumerator)
{$ENDIF}
      private
        function IEnumerator.GetCurrent = GetCurrentNonGeneric;
      end;

      TEnumerator = class(TEnumeratorBase, IEnumerator<T>);
  private
    FEnumeration: TProc;
    class function GetCurrentEnumerator: TEnumerator; static; inline;
    class function GetCurrentValue: T; static; inline;
  protected
    class procedure Yield(const Value: T); overload;
    class property Current: T read GetCurrentValue;
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
    class operator Implicit(const Value): Yield;
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

function GetCurrentEnumerator: TYieldEnumerator;
begin
{$IFDEF USE_THREADING}
  Result := TYieldEnumerator(TYieldEnumerator.CurrentThread);
{$ELSE}
  Result := TYieldEnumerator(TYieldEnumerator.CurrentFiber);
{$ENDIF}
end;

function GetCurrentValue: TValue;
begin
  Result := GetCurrentEnumerator.Current;
end;

{ Yield }

class operator Yield.Implicit(const Value): Yield;
begin
  GetCurrentEnumerator.Yield(Value);
end;

class operator Yield.Implicit(const Value: IInterface): Yield;
begin
  GetCurrentEnumerator.Yield(TValue.From<IInterface>(Value));
end;

class operator Yield.Implicit(const Value: TObject): Yield;
begin
  GetCurrentEnumerator.Yield(TValue.From<TObject>(Value));
end;

class operator Yield.Implicit(const Value: TValue): Yield;
begin
  GetCurrentEnumerator.Yield(Value);
end;

class operator Yield.Implicit(const Value: Variant): Yield;
begin
  GetCurrentEnumerator.Yield(TValue.From(Value));
end;

class operator Yield.Implicit(const Value: Yield): IInterface;
begin
  Result := GetCurrentValue.AsInterface;
end;

class operator Yield.Implicit(const Value: Yield): TObject;
begin
  Result := GetCurrentValue.AsObject;
end;

class operator Yield.Implicit(const Value: Yield): TValue;
begin
  Result := GetCurrentValue;
end;

class operator Yield.Implicit(const Value: Yield): Variant;
begin
  Result := GetCurrentValue.AsVariant;
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

{ TYieldEnumerable<T> }

constructor TYieldEnumerable<T>.Create(const AEnumeration: TProc);
begin
  inherited Create;
  FEnumeration := AEnumeration;
end;

class function TYieldEnumerable<T>.GetCurrentValue: T;
begin
  Result := GetCurrentEnumerator.Current;
end;

class function TYieldEnumerable<T>.GetCurrentEnumerator: TEnumerator;
begin
{$IFDEF USE_THREADING}
  Result := TEnumerator(TEnumerator.CurrentThread);
{$ELSE}
  Result := TEnumerator(TEnumerator.CurrentFiber);
{$ENDIF}
end;

function TYieldEnumerable<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TEnumerator.Create(FEnumeration);
end;

class procedure TYieldEnumerable<T>.Yield(const Value: T);
begin
  GetCurrentEnumerator.Yield(Value);
end;

end.

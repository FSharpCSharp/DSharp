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

unit Collections.Yield;

interface

{.$DEFINE USE_COLLECTIONS}
{$DEFINE USE_FIBERS}

uses
{$IFDEF USE_COLLECTIONS}
  Collections.Base,
{$ELSE}
  Collections.Generics,
  Generics.Collections,
{$ENDIF}
{$IFDEF USE_FIBERS}
  Collections.Fibers,
  System.Fibers,
{$ELSE}
  Classes,
  Collections.Threading,
{$ENDIF}
  SysUtils;

type
{$IFDEF USE_COLLECTIONS}
  TDelegateEnumerable<T> = class(TEnexCollection<T>)
{$ELSE}
  TDelegateEnumerable<T> = class(TEnumerableEx<T>)
{$ENDIF}
  strict private
    FEnumeration: TProc;

    type
{$IFDEF USE_COLLECTIONS}
      TEnumerator = class(TInterfacedObject, IEnumerator<T>)
{$ELSE}
      TEnumerator = class(TEnumeratorEx<T>)
{$ENDIF}
      private
{$IFDEF USE_FIBERS}
        FFiber: TEnumeratorFiber<T>;
{$ELSE}
        FThread: TEnumeratorThread<T>;
{$ENDIF}
{$IFNDEF USE_COLLECTIONS}
      protected
        function DoGetCurrent: T; override;
        function DoMoveNext: Boolean; override;
{$ENDIF}
      public
        constructor Create(const AEnumeration: TProc);
        destructor Destroy; override;
{$IFDEF USE_COLLECTIONS}
        function GetCurrent(): T;
        function MoveNext(): Boolean;
{$ENDIF}
      end;
  protected
    class procedure Yield(const AValue: T);
  public
    constructor Create(const AEnumeration: TProc);
{$IFDEF USE_COLLECTIONS}
    function GetEnumerator: IEnumerator<T>; override;
{$ELSE}
  protected
    function DoGetEnumerator: TEnumerator<T>; override;
{$ENDIF}
  end;

  Yield<T> = record
  public
    class operator Implicit(AValue: T): Yield<T>; inline;
  end;

implementation

{ Yield<T> }

class operator Yield<T>.Implicit(AValue: T): Yield<T>;
begin
  TDelegateEnumerable<T>.Yield(AValue);
end;

{ TDelegateEnumerable<T> }

constructor TDelegateEnumerable<T>.Create(const AEnumeration: TProc);
begin
{$IFDEF USE_COLLECTIONS}
  inherited Create();
{$ELSE}
  inherited Create(nil);
{$ENDIF}
  FEnumeration := AEnumeration;
end;

{$IFDEF USE_COLLECTIONS}
function TDelegateEnumerable<T>.GetEnumerator: IEnumerator<T>;
{$ELSE}
function TDelegateEnumerable<T>.DoGetEnumerator: TEnumerator<T>;
{$ENDIF}
begin
  Result := TEnumerator.Create(FEnumeration);
end;

class procedure TDelegateEnumerable<T>.Yield(const AValue: T);
begin
{$IFDEF USE_FIBERS}
  TEnumeratorFiber<T>(TFiber.CurrentFiber).Yield(AValue);
{$ELSE}
  TEnumeratorThread<T>(TThread.CurrentThread).Yield(AValue);
{$ENDIF}
end;

{ TDelegateEnumerable<T>.TEnumerator }

constructor TDelegateEnumerable<T>.TEnumerator.Create(const AEnumeration: TProc);
begin
{$IFDEF USE_FIBERS}
  FFiber := TEnumeratorFiber<T>.Create(AEnumeration);
{$ELSE}
  FThread := TEnumeratorThread<T>.Create(AEnumeration);
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

{$IFDEF USE_COLLECTIONS}
function TDelegateEnumerable<T>.TEnumerator.GetCurrent: T;
{$ELSE}
function TDelegateEnumerable<T>.TEnumerator.DoGetCurrent: T;
{$ENDIF}
begin
{$IFDEF USE_FIBERS}
  Result := FFiber.Result;
{$ELSE}
  Result := FThread.Result;
{$ENDIF}
end;

{$IFDEF USE_COLLECTIONS}
function TDelegateEnumerable<T>.TEnumerator.MoveNext: Boolean;
{$ELSE}
function TDelegateEnumerable<T>.TEnumerator.DoMoveNext: Boolean;
{$ENDIF}
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

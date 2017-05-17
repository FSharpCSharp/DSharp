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

unit DSharp.Collections.Fibers;

interface

uses
  DSharp.Core.Fibers,
  Spring.Collections,
  Rtti,
  SysUtils;

type
  TIteratorFiber = class(TFiber, IEnumerator)
  private
    fCanceled: Boolean;
    fProc: TProc;
  protected
    procedure Execute; override;
    function GetCurrentNonGeneric: TValue; virtual; abstract;
    function IEnumerator.GetCurrent = GetCurrentNonGeneric;
    procedure SetCurrent(const value); virtual; abstract;
    procedure Yield(const value: TValue); overload; virtual; abstract;

    property Canceled: Boolean read fCanceled;
  public
    constructor Create(const proc: TProc);
    destructor Destroy; override;

    function MoveNext: Boolean;
    procedure Reset;

    class procedure Yield(const value); overload;

    property Current: TValue read GetCurrentNonGeneric;
  end;

  TIteratorFiber<T> = class(TIteratorFiber, IEnumerator<T>)
  private
    fCurrent: T;
  protected
    function GetCurrentNonGeneric: TValue; override;
    function GetCurrent: T;
    procedure SetCurrent(const value); override;
    procedure Yield(const value: TValue); override;
  public
    property Current: T read GetCurrent;
  end;

implementation

{ TIteratorFiber }

constructor TIteratorFiber.Create(const proc: TProc);
begin
  inherited Create;
  fProc := proc;
end;

destructor TIteratorFiber.Destroy;
begin
  if not Finished then
  begin
    fCanceled := True;
    Invoke;
  end;
  inherited;
end;

procedure TIteratorFiber.Execute;
begin
  fProc;
end;

function TIteratorFiber.MoveNext: Boolean;
begin
  Invoke;
  Result := not Finished;
end;

procedure TIteratorFiber.Reset;
begin
  raise EInvalidOpException.Create('Reset');
end;

class procedure TIteratorFiber.Yield(const value);
begin
  with TIteratorFiber(CurrentFiber) do
  begin
    SetCurrent(value);
    Yield;
    if fCanceled then
      Abort;
  end;
end;

{ TIteratorFiber<T> }

function TIteratorFiber<T>.GetCurrentNonGeneric: TValue;
begin
  Result := TValue.From<T>(fCurrent);
end;

function TIteratorFiber<T>.GetCurrent: T;
begin
  Result := fCurrent;
end;

procedure TIteratorFiber<T>.SetCurrent(const value);
begin
  fCurrent := T(value);
end;

procedure TIteratorFiber<T>.Yield(const value: TValue);
var
  lValue: T;
begin
  lValue := value.AsType<T>;
  inherited Yield(lValue);
end;

end.

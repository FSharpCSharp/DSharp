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
  Rtti,
  SysUtils;

type
  TEnumeratorFiber = class(TFiber)
  private
    FCanceled: Boolean;
    FProc: TProc;
  protected
    procedure Execute; override;
    function GetCurrentNonGeneric: TValue; virtual; abstract;
    procedure SetCurrent(const AValue); virtual; abstract;

    property Canceled: Boolean read FCanceled;
  public
    constructor Create(const AProc: TProc);
    destructor Destroy; override;
    function MoveNext: Boolean;
    procedure Yield(const AValue); overload;
    procedure Yield(const AValue: TValue); overload; virtual; abstract;
    property Current: TValue read GetCurrentNonGeneric;
  end;

  TEnumeratorFiber<T> = class(TEnumeratorFiber)
  private
    FCurrent: T;
  protected
    function GetCurrentNonGeneric: TValue; override;
    function GetCurrent: T;
    procedure SetCurrent(const AValue); override;
  public
    procedure Yield(const AValue: TValue); override;
    property Current: T read GetCurrent;
  end;

procedure Yield(const AValue);

implementation

procedure Yield(const AValue);
begin
  TEnumeratorFiber(TFiber.CurrentFiber).Yield(AValue);
end;

{ TEnumeratorFiber }

constructor TEnumeratorFiber.Create(const AProc: TProc);
begin
  inherited Create;
  FProc := AProc;
end;

destructor TEnumeratorFiber.Destroy;
begin
  if not Finished then
  begin
    FCanceled := True;
    Invoke;
  end;
  inherited;
end;

procedure TEnumeratorFiber.Execute;
begin
  FProc;
end;

function TEnumeratorFiber.MoveNext: Boolean;
begin
  Invoke;
  Result := not Finished;
end;

procedure TEnumeratorFiber.Yield(const AValue);
begin
  if Assigned(Self) then
  begin
    SetCurrent(AValue);
    inherited Yield;
    if FCanceled then
      Abort;
  end;
end;

{ TEnumeratorFiber<T> }

function TEnumeratorFiber<T>.GetCurrentNonGeneric: TValue;
begin
  Result := TValue.From<T>(FCurrent);
end;

function TEnumeratorFiber<T>.GetCurrent: T;
begin
  Result := FCurrent;
end;

procedure TEnumeratorFiber<T>.SetCurrent(const AValue);
begin
  FCurrent := T(AValue);
end;

procedure TEnumeratorFiber<T>.Yield(const AValue: TValue);
var
  LValue: T;
begin
  LValue := AValue.AsType<T>;
  inherited Yield(LValue);
end;

end.

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

unit DSharp.Collections.Fibers;

interface

uses
  DSharp.Core.Fibers,
  Rtti,
  SysUtils;

type
  TEnumeratorFiber = class(TFiber)
  strict private
    FCanceled: Boolean;
    FProc: TProc;
  strict protected
    FResult: TValue;
    procedure Execute; override;
    property Canceled: Boolean read FCanceled;
  public
    constructor Create(const AProc: TProc);
    destructor Destroy; override;
    procedure Yield(const AValue: TValue); overload;
    property Result: TValue read FResult;
  end;

  TEnumeratorFiber<T> = class(TEnumeratorFiber)
  private
    function GetResult: T;
  public
    procedure Yield(const AValue: T);
    property Result: T read GetResult;
  end;

implementation

{ TEnumeratorFiber }

constructor TEnumeratorFiber.Create(const AProc: TProc);
begin
  inherited Create();
  FProc := AProc;
end;

destructor TEnumeratorFiber.Destroy;
begin
  if not Finished then
  begin
    FCanceled := True;
    Resume;
  end;
  inherited;
end;

procedure TEnumeratorFiber.Execute;
begin
  FProc();
end;

procedure TEnumeratorFiber.Yield(const AValue: TValue);
begin
  FResult := AValue;
  inherited Yield();
  if Canceled then
  begin
    Abort;
  end;
end;

{ TEnumeratorFiber<T> }

function TEnumeratorFiber<T>.GetResult: T;
begin
  Result := FResult.AsType<T>;
end;

procedure TEnumeratorFiber<T>.Yield(const AValue: T);
begin
  if Self <> nil then
  begin
    FResult := TValue.From<T>(AValue);
    inherited Yield();
    if Canceled then
    begin
      Abort;
    end;
  end;
end;

end.

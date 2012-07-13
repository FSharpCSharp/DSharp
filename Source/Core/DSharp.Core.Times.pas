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

unit DSharp.Core.Times;

interface

uses
  SysUtils;

type
  Times = record
  private
    FEvaluator: TPredicate<Cardinal>;
    FMin: Cardinal;
    FMax: Cardinal;
    constructor Create(Evaluator: TPredicate<Cardinal>; Min, Max: Cardinal);
  public
    class function Any: Times; static;
    class function AtLeast(Count: Cardinal): Times; static;
    class function AtLeastOnce: Times; static;
    class function AtMost(Count: Cardinal): Times; static;
    class function AtMostOnce: Times; static;
    class function Between(Min, Max: Cardinal): Times; static;
    class function Exactly(Count: Cardinal): Times; static;
    class function Never: Times; static;
    class function Once: Times; static;

    function Equals(const Value: Times): Boolean;
    function ToString: string;
    function Verify(Value: Cardinal): Boolean;

    property Min: Cardinal read FMin;
    property Max: Cardinal read FMax;

    class operator Equal(const Left, Right: Times): Boolean;
    class operator NotEqual(const Left, Right: Times): Boolean;
  end;

implementation

{ Times }

constructor Times.Create(Evaluator: TPredicate<Cardinal>; Min, Max: Cardinal);
begin
  FEvaluator := Evaluator;
  FMin := Min;
  FMax := Max;
end;

class function Times.Any: Times;
begin
  Result := Times.Create(
    function(Value: Cardinal): Boolean
    begin
      Result := True;
    end, 0, High(Cardinal));
end;

class function Times.AtLeast(Count: Cardinal): Times;
begin
  Result := Times.Create(
    function(Value: Cardinal): Boolean
    begin
      Result := Value >= Count;
    end, Count, High(Cardinal));
end;

class function Times.AtLeastOnce: Times;
begin
  Result := Times.Create(
    function(Value: Cardinal): Boolean
    begin
      Result := Value >= 1;
    end, 1, High(Cardinal));
end;

class function Times.AtMost(Count: Cardinal): Times;
begin
  Result := Times.Create(
    function(Value: Cardinal): Boolean
    begin
      Result := Value <= Count;
    end, 0, Count);
end;

class function Times.AtMostOnce: Times;
begin
  Result := Times.Create(
    function(Value: Cardinal): Boolean
    begin
      Result := Value <= 1;
    end, 0, 1);
end;

class function Times.Between(Min, Max: Cardinal): Times;
begin
  Result := Times.Create(
    function(Value: Cardinal): Boolean
    begin
      Result := (Value >= Min) and (Value <= Max);
    end, Min, Max);
end;

class function Times.Exactly(Count: Cardinal): Times;
begin
  Result := Times.Create(
    function(Value: Cardinal): Boolean
    begin
      Result := Value = Count;
    end, Count, Count);
end;

class function Times.Never: Times;
begin
  Result := Times.Create(
    function(Value: Cardinal): Boolean
    begin
      Result := Value = 0;
    end, 0, 0);
end;

class function Times.Once: Times;
begin
  Result := Times.Create(
    function(Value: Cardinal): Boolean
    begin
      Result := Value = 1;
    end, 1, 1);
end;

function Times.Equals(const Value: Times): Boolean;
begin
  Result := (FMin = Value.FMin) and (FMax = Value.FMax);
end;

function Times.ToString: string;
begin
  if FMin = 0 then
  begin
    if FMax = 0 then
      Result := 'never'
    else if FMax = High(Cardinal) then
      Result := 'any'
    else
      Result := Format('at most %u', [FMax]);
  end
  else
  begin
    if FMax = High(Cardinal) then
      Result := Format('at least %u', [FMin])
    else
      if FMin = FMax then
        Result := Format('exactly %u', [FMin])
      else
        Result := Format('between %u and %u', [FMin, FMax]);
  end;
end;

function Times.Verify(Value: Cardinal): Boolean;
begin
  if Assigned(FEvaluator) then
  begin
    Result := FEvaluator(Value);
  end
  else
  begin
    Result := Value = 0;
  end;
end;

class operator Times.Equal(const Left, Right: Times): Boolean;
begin
  Result := Left.Equals(Right);
end;

class operator Times.NotEqual(const Left, Right: Times): Boolean;
begin
  Result := not Left.Equals(Right);
end;

end.

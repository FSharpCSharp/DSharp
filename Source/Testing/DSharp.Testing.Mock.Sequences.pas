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

unit DSharp.Testing.Mock.Sequences;

interface

uses
  DSharp.Core.Times,
  DSharp.Testing.Mock.Expectation,
  DSharp.Testing.Mock.Interfaces,
  Generics.Collections,
  SysUtils;

type
  Sequence = record
  private
    FInstance: ISequence;
    procedure EnsureInstance;
  public
    procedure Verify;

    class operator Implicit(var Value: Sequence): ISequence;
  end;

  ESequenceException = class(EAbort);

implementation

resourcestring
  RAlreadyCompleted = '"%s" is not invokable because it has already completed';
  RExceededMaximumNumbers = '"%s" exceeded maximum number of invocations';
  RNotCompleted = 'invocations for "%s" were not completed';

type
  TSequence = class(TInterfacedObject, ISequence)
  private
    FExpectations: TList<TExpectation>;
    FIndex: Integer;
    procedure EnsureSatisfied(const Index: Integer);
  public
    constructor Create;
    destructor Destroy; override;

    procedure ExpectInvocation(Action: TObject; const Times: Times);
    procedure RecordInvocation(Action: TObject);

    procedure Verify;
  end;

{ Sequence }

procedure Sequence.EnsureInstance;
begin
  if not Assigned(FInstance) then
  begin
    FInstance := TSequence.Create;
  end;
end;

class operator Sequence.Implicit(var Value: Sequence): ISequence;
begin
  Value.EnsureInstance;
  Result := Value.FInstance;
end;

procedure Sequence.Verify;
begin
  EnsureInstance;
  FInstance.Verify;
end;

{ TSequence }

constructor TSequence.Create;
begin
  FExpectations := TList<TExpectation>.Create;
  FIndex := 0;
end;

destructor TSequence.Destroy;
begin
  FExpectations.Free;
  inherited;
end;

procedure TSequence.ExpectInvocation(Action: TObject; const Times: Times);
var
  LExpectation: TExpectation;
begin
  LExpectation := Action as TExpectation;
  LExpectation.ExpectedCount := Times;
  LExpectation.Callback :=
    procedure
    begin
      RecordInvocation(LExpectation);
    end;
  FExpectations.Add(LExpectation);
end;

procedure TSequence.RecordInvocation(Action: TObject);
var
  LExpectation: TExpectation;
  LIndex: Integer;
begin
  LExpectation := Action as TExpectation;

  if LExpectation.AllowsInvocation and FExpectations.Contains(LExpectation) then
  begin
    LIndex := FExpectations.IndexOf(LExpectation);

    if LIndex < FIndex then
    begin
      raise ESequenceException.CreateFmt(RAlreadyCompleted, [LExpectation.ToString]);
    end
    else
    begin
      EnsureSatisfied(LIndex);
    end;
  end;
end;

procedure TSequence.EnsureSatisfied(const Index: Integer);
begin
  while FIndex < Index do
  begin
    if FExpectations[FIndex].IsSatisfied then
    begin
      Inc(FIndex);
    end
    else
    begin
      raise ESequenceException.CreateFmt(RNotCompleted, [FExpectations[FIndex].ToString]);
    end
  end;
end;

procedure TSequence.Verify;
begin
  EnsureSatisfied(FExpectations.Count);
end;

end.

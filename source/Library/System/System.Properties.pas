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

unit System.Properties;

interface

uses
  Classes,
  System.CopyOperator,
  System.Events;

type
  TProperty<T> = record
  public
    OnChange: IEventHandler<TNotifyEvent>;
    Value: T;
  private
    CopyOperator: IInterface;
  public
    class operator Implicit(AValue: T): TProperty<T>;
    class operator Implicit(AValue: TProperty<T>): T;
  end;

  TField<T> = record
  private
    Event: TEvent<TNotifyEvent>;
  public
    Value: TProperty<T>;
    procedure Initialize;
    procedure Finalize;
  end;

  TPropertyCopyOperator<T> = class(TCopyOperator<TProperty<T>>)
  private
    FEventHandler: IEventHandler<TNotifyEvent>;
  protected
    procedure Copy; override;
    procedure Initialize; override;
  end;

implementation

{ TProperty }

class operator TProperty<T>.Implicit(AValue: T): TProperty<T>;
begin
  Result.Value := AValue;
end;

class operator TProperty<T>.Implicit(AValue: TProperty<T>): T;
begin
  Result := AValue.Value;
end;

{ TField }

procedure TField<T>.Finalize;
begin
  Value.CopyOperator := nil;
end;

procedure TField<T>.Initialize;
begin
  Value.OnChange := Event.EventHandler;
  Value.CopyOperator := TPropertyCopyOperator<T>.Create(@Value);
end;

{ TPropertyCopyOperator<T> }

procedure TPropertyCopyOperator<T>.Copy;
begin
  if FValue.OnChange = nil then
  begin
    FValue.CopyOperator := Self;
    FValue.OnChange := FEventHandler;
    FEventHandler.Invoke(nil);
  end
  else
  begin
    if Assigned(FValue.CopyOperator)
      and (TObject(FValue.CopyOperator) <> Self) then
    begin
      FValue.OnChange.Invoke(nil);
      FValue.CopyOperator := TPropertyCopyOperator<T>.Create(FValue);
    end;
  end;
end;

procedure TPropertyCopyOperator<T>.Initialize;
begin
  FEventHandler := FValue.OnChange;
end;

end.
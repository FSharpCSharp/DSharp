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

unit System.PropertyPath;

interface

uses
  Generics.Collections,
  Rtti;

type
  IPropertyPath = interface
    function GetIsReadable: Boolean;
    function GetIsWritable: Boolean;
    function GetName: string;
    function GetPropertyType: TRttiType;
    function GetValue(Instance: Pointer): TValue;
    procedure SetValue(Instance: Pointer; const AValue: TValue);
    property IsReadable: Boolean read GetIsReadable;
    property IsWritable: Boolean read GetIsWritable;
    property Name: string read GetName;
    property PropertyType: TRttiType read GetPropertyType;
  end;

  TPropertyPath = class(TInterfacedObject, IPropertyPath)
  private
    FName: string;
    FProperties: TList<TRttiProperty>;
  protected
    function DoGetValue(Instance: Pointer): TValue; virtual;
    procedure DoSetValue(Instance: Pointer; const AValue: TValue); virtual;
    function GetIsReadable: Boolean; virtual;
    function GetIsWritable: Boolean; virtual;
    function GetName: string; virtual;
    function GetPropertyType: TRttiType; virtual;
  public
    constructor Create(AClass: TClass; AName: string);
    destructor Destroy; override;

    function GetValue(Instance: Pointer): TValue;
    procedure SetValue(Instance: Pointer; const AValue: TValue);
    property IsReadable: Boolean read GetIsReadable;
    property IsWritable: Boolean read GetIsWritable;
    property Name: string read GetName;
    property PropertyType: TRttiType read GetPropertyType;
  end;

implementation

uses
  RTLConsts,
  StrUtils,
  SysUtils;

var
  Context: TRttiContext;

{ TPropertyPath }

constructor TPropertyPath.Create(AClass: TClass; AName: string);
var
  LType: TRttiType;
  LProperty: TRttiProperty;
  LObjectName: string;
  LPropertyName: string;
begin
  FName := AName;
  FProperties := TList<TRttiProperty>.Create();

  LType := Context.GetType(AClass);
  if not Assigned(LType) then
    raise EInsufficientRtti.CreateRes(@SInsufficientRtti);

  LPropertyName := AName;
  while ContainsText(LPropertyName, '.') do
  begin
    LObjectName := LeftStr(LPropertyName, Pred(Pos('.', LPropertyName)));
    LProperty := LType.GetProperty(LObjectName);
    if Assigned(LProperty) then
    begin
      FProperties.Add(LProperty);
      LType := LProperty.PropertyType;
      LPropertyName := RightStr(LPropertyName, Length(LPropertyName) - Pos('.', LPropertyName));
    end
    else
    begin
      raise EArgumentException.CreateResFmt(@SUnknownProperty, [LObjectName]);
    end;
  end;
  LProperty := LType.GetProperty(LPropertyName);
  if Assigned(LProperty) then
  begin
    FProperties.Add(LProperty);
  end;
end;

destructor TPropertyPath.Destroy;
begin
  FProperties.Free();
  inherited;
end;

function TPropertyPath.DoGetValue(Instance: Pointer): TValue;
var
  i: Integer;
  LObject: TObject;
  LValue: TValue;
begin
  Result := TValue.Empty;
  LObject := Instance;
  for i := 0 to Pred(FProperties.Count) do
  begin
    LValue := FProperties[i].GetValue(LObject);
    if i < Pred(FProperties.Count) then
    begin
      LObject := LValue.AsObject();
    end;
  end;
  Result := LValue;
end;

procedure TPropertyPath.DoSetValue(Instance: Pointer; const AValue: TValue);
var
  i: Integer;
  LObject: TObject;
  LValue: TValue;
begin
  LObject := Instance;
  for i := 0 to Pred(FProperties.Count) do
  begin
    if i < Pred(FProperties.Count) then
    begin
      LValue := FProperties[i].GetValue(LObject);
      LObject := LValue.AsObject();
    end
    else
    begin
      FProperties[i].SetValue(LObject, AValue);
    end;
  end;
end;

function TPropertyPath.GetIsReadable: Boolean;
var
  i: Integer;
begin
  Result := FProperties.Count > 0;
  for i := 0 to Pred(FProperties.Count) do
  begin
    Result := Result and FProperties[i].IsReadable;
  end;
end;

function TPropertyPath.GetIsWritable: Boolean;
var
  i: Integer;
begin
  Result := FProperties.Count > 0;
  for i := 0 to Pred(FProperties.Count) do
  begin
    Result := Result and FProperties[i].IsWritable;
  end;
end;

function TPropertyPath.GetName: string;
begin
  Result := FName;
end;

function TPropertyPath.GetPropertyType: TRttiType;
begin
  Result := nil;
  if FProperties.Count > 0 then
  begin
    Result := FProperties.Last.PropertyType;
  end;
end;

function TPropertyPath.GetValue(Instance: Pointer): TValue;
begin
  if not IsReadable then
    raise EPropWriteOnly.Create(Name);
  Result := DoGetValue(Instance);
end;

procedure TPropertyPath.SetValue(Instance: Pointer; const AValue: TValue);
begin
  if not IsWritable then
    raise EPropReadOnly.Create(Name);
  DoSetValue(Instance, AValue);
end;

end.

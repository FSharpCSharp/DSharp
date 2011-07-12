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

unit DSharp.Core.PropertyPath;

interface

uses
  DSharp.Collections,
  Rtti;

type
  IPropertyPath = interface
    function GetIsReadable: Boolean;
    function GetIsWritable: Boolean;
    function GetPath: string;
    function GetPropertyType: TRttiType;
    function GetRoot: string;
    function GetValue(Instance: Pointer): TValue;
    procedure SetValue(Instance: Pointer; const AValue: TValue);
    property IsReadable: Boolean read GetIsReadable;
    property IsWritable: Boolean read GetIsWritable;
    property Path: string read GetPath;
    property PropertyType: TRttiType read GetPropertyType;
    property Root: string read GetRoot;
  end;

  // cannot inherit from TRttiProperty because virtual methods are private
  TProperty = class
  private
    FIndex: Integer;
    FMethod: TRttiMethod;
    FProperty: TRttiProperty;
    function GetIsReadable: Boolean;
    function GetIsWritable: Boolean;
    function GetPropertyType: TRttiType;
  public
    constructor Create(AType: TRttiType; const AName: string);
    class function Lookup(AType: TRttiType; const AName: string): TProperty;

    function GetValue(Instance: Pointer): TValue;
    procedure SetValue(Instance: Pointer; const AValue: TValue);
    property IsReadable: Boolean read GetIsReadable;
    property IsWritable: Boolean read GetIsWritable;
    property PropertyType: TRttiType read GetPropertyType;
  end;

  TPropertyPath = class(TInterfacedObject, IPropertyPath)
  private
    FInstance: TObject;
    FPath: string;
    FProperties: TObjectList<TProperty>;
  protected
    function DoGetValue(Instance: Pointer): TValue; virtual;
    procedure DoSetValue(Instance: Pointer; const AValue: TValue); virtual;
    function GetIsReadable: Boolean; virtual;
    function GetIsWritable: Boolean; virtual;
    function GetPath: string; virtual;
    function GetPropertyType: TRttiType; virtual;
    function GetRoot: string; virtual;
    procedure InitProperties;
  public
    constructor Create(AInstance: TObject; APath: string);
    destructor Destroy; override;

    function GetValue(Instance: Pointer): TValue;
    procedure SetValue(Instance: Pointer; const AValue: TValue);
    property IsReadable: Boolean read GetIsReadable;
    property IsWritable: Boolean read GetIsWritable;
    property Path: string read GetPath;
    property PropertyType: TRttiType read GetPropertyType;
  end;

implementation

uses
  Classes,
  DSharp.Core.Reflection,
  RTLConsts,
  StrUtils,
  SysUtils,
  TypInfo;

var
  Context: TRttiContext;
  MethodType: TRttiType;

{ TProperty }

constructor TProperty.Create(AType: TRttiType; const AName: string);
var
  LName: string;
  LPos: Integer;
begin
  if AName <> '' then
  begin
    FIndex := -1;
    LName := AName;
    LPos := Pos('[', LName);
    if LPos > 0 then
    begin
      FIndex := StrToIntDef(Copy(LName, Succ(LPos),
        Pos(']', LName) - Succ(LPos)), -1);
      LName := LeftStr(LName, Pred(LPos));
    end;

    FProperty := AType.GetProperty(LName);
    if not Assigned(FProperty) then
    begin
      FMethod := AType.GetMethod(LName);
    end;
  end;
end;

function TProperty.GetIsReadable: Boolean;
begin
  Result := (Assigned(FProperty) and FProperty.IsReadable) or Assigned(FMethod);
end;

function TProperty.GetIsWritable: Boolean;
begin
  Result := Assigned(FProperty) and FProperty.IsWritable;
end;

function TProperty.GetPropertyType: TRttiType;
begin
  if Assigned(FProperty) then
    Result := FProperty.PropertyType
  else if Assigned(FMethod) then
    Result := MethodType
  else
    Result := nil;
end;

function TProperty.GetValue(Instance: Pointer): TValue;
var
  LList: TObject;
  LMethod: TMethod;
  LObject: TObject;
begin
  if Assigned(FProperty) then
  begin
    Result := FProperty.GetValue(Instance)
  end
  else
  begin
    LMethod.Code := FMethod.CodeAddress;
    LMethod.Data := Instance;
    Result := TValue.From(LMethod);
  end;

  if FIndex > -1 then
  begin
    if Result.IsObject then
    begin
      LList := Result.AsObject;
      Result := TValue.Empty;
      LObject := nil;

      if Assigned(LList) then
      begin
        if LList.InheritsFrom(TCollection) and (TCollection(LList).Count > FIndex) then
        begin
          LObject := TCollection(LList).Items[FIndex];
        end else
        if LList.InheritsFrom(TList) and (TList(LList).Count > FIndex) then
        begin
          LObject := TList(LList).Items[FIndex];
        end else
        if IsClassCovariantTo(LList.ClassType, TList<TObject>) and (TList<TObject>(LList).Count > 0) then
        begin
          LObject := TList<TObject>(LList).Items[FIndex];
        end else
        // hack to put the interface into an object reference to carry it around
        if IsClassCovariantTo(LList.ClassType, TList<IInterface>) and (TList<TObject>(LList).Count > 0) then
        begin
          LObject := TList<IInterface>(LList).Items[FIndex] as TObject;
        end;
        if Assigned(LObject) then
        begin
          Result := TValue.From(LObject);
        end;
      end
    end;
  end;
end;

class function TProperty.Lookup(AType: TRttiType;
  const AName: string): TProperty;
begin
  Result := TProperty.Create(AType, AName);
  if not Assigned(Result.FProperty) and not Assigned(Result.FMethod) then
  begin
    FreeAndNil(Result);
  end;
end;

procedure TProperty.SetValue(Instance: Pointer; const AValue: TValue);
begin
  if FIndex = -1 then
  begin
    FProperty.SetValue(Instance, AValue);
  end;
end;

{ TPropertyPath }

constructor TPropertyPath.Create(AInstance: TObject; APath: string);
begin
  FInstance := AInstance;
  FPath := APath;
  FProperties := TObjectList<TProperty>.Create();

  InitProperties();
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
  LValue := TValue.Empty;
  LObject := Instance;
  for i := 0 to Pred(FProperties.Count) do
  begin
    if Assigned(LObject) then
    begin
      LValue := FProperties[i].GetValue(LObject);
      if i < Pred(FProperties.Count) then
      begin
        LObject := LValue.AsObject();
      end;
    end
    else
    begin
      LValue := TValue.Empty;
      Break;
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
    if Assigned(LObject) then
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
    end
    else
    begin
      Break;
    end;
  end;
end;

function TPropertyPath.GetIsReadable: Boolean;
var
  i: Integer;
begin
  InitProperties();
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
  InitProperties();
  Result := FProperties.Count > 0;
  for i := 0 to Pred(FProperties.Count) do
  begin
    Result := Result and FProperties[i].IsWritable;
  end;
end;

function TPropertyPath.GetPath: string;
begin
  Result := FPath;
end;

function TPropertyPath.GetPropertyType: TRttiType;
begin
  InitProperties();
  Result := nil;
  if FProperties.Count > 0 then
  begin
    Result := FProperties.Last.PropertyType;
  end;
end;

function TPropertyPath.GetRoot: string;
begin
  Result := FPath;

  if ContainsText(Result, '.') then
  begin
    Result := LeftStr(Result, Pred(Pos('.', Result)));
  end;
  if ContainsText(Result, '[') then
  begin
    Result := LeftStr(Result, Pred(Pos('[', Result)));
  end;
end;

function TPropertyPath.GetValue(Instance: Pointer): TValue;
begin
  if not IsReadable then
    raise EPropWriteOnly.Create(Path);
  Result := DoGetValue(Instance);
end;

procedure TPropertyPath.InitProperties;
var
  LObject: TObject;
  LObjectName: string;
  LProperty: TProperty;
  LPropertyName: string;
  LType: TRttiType;
begin
  FProperties.Clear();

  LObject := FInstance;
  LType := Context.GetType(LObject.ClassType);
  if not Assigned(LType) then
  begin
    raise EInsufficientRtti.CreateRes(@SInsufficientRtti);
  end;

  LPropertyName := FPath;
  while ContainsText(LPropertyName, '.') do
  begin
    LObjectName := LeftStr(LPropertyName, Pred(Pos('.', LPropertyName)));

    LProperty := TProperty.Lookup(LType, LObjectName);
    if Assigned(LProperty) then
    begin
      FProperties.Add(LProperty);
      LType := LProperty.PropertyType;
      if LProperty.IsReadable and LProperty.PropertyType.IsInstance then
      begin
        LObject := LProperty.GetValue(LObject).AsObject;
        if Assigned(LObject) then
        begin
          LType := Context.GetType(LObject.ClassType);
        end;
      end;
      LPropertyName := RightStr(LPropertyName, Length(LPropertyName) - Pos('.', LPropertyName));
    end
    else
    begin
      Break;
//      raise EArgumentException.CreateResFmt(@SUnknownProperty, [LObjectName]);
    end;
  end;
  LProperty := TProperty.Lookup(LType, LPropertyName);
  if Assigned(LProperty) then
  begin
    FProperties.Add(LProperty);
  end;
end;

procedure TPropertyPath.SetValue(Instance: Pointer; const AValue: TValue);
begin
  if not IsWritable then
    raise EPropReadOnly.Create(Path);
  DoSetValue(Instance, AValue);
end;

initialization
  MethodType := Context.GetType(TypeInfo(TMethod));

end.

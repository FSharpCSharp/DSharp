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

unit DSharp.PresentationModel.ConventionManager;

interface

uses
  Classes,
  DSharp.Bindings,
  DSharp.PresentationModel.ElementConvention,
  Generics.Collections,
  Rtti;

type
  ConventionManager = record
  private
    class var FConventions: TDictionary<TClass, TElementConvention>;
  public
    class constructor Create;
    class destructor Destroy;

    class function AddElementConvention<T: class>(APropertyName: string;
      AEventName: string): TElementConvention; static;
    class procedure ApplyValidation(ABinding: TBinding;
      AViewModel: TObject; APropertyName: string); static;
    class procedure ConfigureSelectedItem(AViewModel: TObject;
      APropertyName: string; AViewElement: TComponent;
      ASelectedItemPropertyName: string); static;
    class function GetElementConvention(
      AElementType: TClass): TElementConvention; static;
    class procedure SetBinding(AViewModel: TObject; APropertyName: string;
      AViewElement: TComponent; ABindingType: TBindingType;
      AConvention: TElementConvention); overload; static;
    class procedure SetBinding(AViewModel: TObject; APropertyName: string;
      AViewElement: TComponent; ATargetPropertyName: string); overload; static;
  end;

implementation

uses
  DSharp.Core.Reflection,
  DSharp.PresentationModel.Validations,
  StrUtils;

function Singularize(const AName: string): string;
begin
  if EndsText('ies', AName) then
    Result := LeftStr(AName, Length(AName) - 3) + 'y'
  else if EndsText('s', AName) then
    Result := LeftStr(AName, Length(AName) - 1)
  else
    Result := AName;
end;

function DerivePotentialSelectionNames(const AName: string): TArray<string>;
var
  LSingular: string;
begin
  LSingular := Singularize(AName);
  Result := TArray<string>.Create(
    'Active' + LSingular,
    'Selected' + LSingular,
    'Current' + LSingular);
end;

{ ConventionManager }

class function ConventionManager.AddElementConvention<T>(APropertyName,
  AEventName: string): TElementConvention;
begin
  Result := TElementConvention.Create(APropertyName, AEventName);
  FConventions.AddOrSetValue(T, Result);
end;

class procedure ConventionManager.ApplyValidation(ABinding: TBinding;
  AViewModel: TObject; APropertyName: string);
var
  LProperty: TRttiProperty;
  LAttribute: ValidationAttribute;
begin

  LProperty := AViewModel.GetProperty(APropertyName);
  if Assigned(LProperty) then
  begin
    for LAttribute in LProperty.GetCustomAttributes<ValidationAttribute> do
    begin
      ABinding.ValidationRules.Add(LAttribute.ValidationRuleClass.Create);
    end;
  end;
end;

class procedure ConventionManager.ConfigureSelectedItem(AViewModel: TObject;
  APropertyName: string; AViewElement: TComponent; ASelectedItemPropertyName: string);
var
  LBindingGroup: TBindingGroup;
  LProperty: TRttiProperty;
  LPotentialName: string;
begin
  LBindingGroup := FindBindingGroup(AViewElement);

  for LPotentialName in DerivePotentialSelectionNames(AViewElement.Name) do
  begin
    LProperty := AViewModel.GetProperty(LPotentialName);
    if Assigned(LProperty) then
    begin
      LBindingGroup.AddBinding(
        AViewModel, LPotentialName, AViewElement, ASelectedItemPropertyName);
    end;
  end;
end;

class constructor ConventionManager.Create;
begin
  FConventions := TObjectDictionary<TClass, TElementConvention>.Create([doOwnsValues]);
end;

class destructor ConventionManager.Destroy;
begin
  FConventions.Free();
end;

class function ConventionManager.GetElementConvention(
  AElementType: TClass): TElementConvention;
begin
  if not FConventions.TryGetValue(AElementType, Result)
    and (AElementType.ClassParent <> nil) then
  begin
    Result := GetElementConvention(AElementType.ClassParent);
  end;
end;

class procedure ConventionManager.SetBinding(AViewModel: TObject;
  APropertyName: string; AViewElement: TComponent; ABindingType: TBindingType;
  AConvention: TElementConvention);
var
  LTargetPropertyName: string;
begin
  case ABindingType of
    btProperty: LTargetPropertyName := AConvention.PropertyName;
    btEvent: LTargetPropertyName := AConvention.EventName;
  end;

  SetBinding(AViewModel, APropertyName, AViewElement, LTargetPropertyName);
end;

class procedure ConventionManager.SetBinding(AViewModel: TObject;
  APropertyName: string; AViewElement: TComponent; ATargetPropertyName: string);
var
  LBindingGroup: TBindingGroup;
  LBinding: TBinding;
begin
  LBindingGroup := FindBindingGroup(AViewElement);
  if not Assigned(LBindingGroup) then
  begin
    LBindingGroup := TBindingGroup.Create(AViewElement.Owner);
  end;

  LBinding := LBindingGroup.AddBinding();

  ApplyValidation(LBinding, AViewModel, APropertyName);

  LBinding.Source := AViewModel;
  LBinding.SourcePropertyName := APropertyName;
  LBinding.Target := AViewElement;
  LBinding.TargetPropertyName := ATargetPropertyName;
end;

end.

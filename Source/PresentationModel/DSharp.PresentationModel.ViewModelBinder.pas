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

unit DSharp.PresentationModel.ViewModelBinder;

interface

uses
  Classes;

type
  ViewModelBinder = record
  public
    class procedure Bind(AViewModel: TObject; AView: TComponent); static;
    class procedure BindActions(AViewModel: TObject; AView: TComponent); static;
    class procedure BindProperties(AViewModel: TObject; AView: TComponent); static;
  end;

implementation

uses
  DSharp.Bindings,
  DSharp.Core.Reflection,
  DSharp.PresentationModel.ConventionManager,
  DSharp.PresentationModel.ElementConvention,
  DSharp.PresentationModel.Validations,
  Rtti,
  StrUtils,
  Types;

{ TModelViewBinder }

class procedure ViewModelBinder.Bind(AViewModel: TObject; AView: TComponent);
begin
  BindProperties(AViewModel, AView);
  BindActions(AViewModel, AView);
end;

class procedure ViewModelBinder.BindActions(
  AViewModel: TObject; AView: TComponent);
var
  LType: TRttiType;
  LComponent: TComponent;
  LMethod: TRttiMethod;
  LProperty: TRttiProperty;
  LConvention: TElementConvention;
  LPropertyName: string;
begin
  LType := GetRttiType(AViewModel.ClassInfo);

  for LComponent in AView do
  begin
    LMethod := LType.GetMethod(LComponent.Name);

    if not Assigned(LMethod) then
    begin
      Continue;
    end;

    LConvention := ConventionManager.GetElementConvention(LComponent.ClassType);
    if not Assigned(LConvention) then
    begin
      Continue;
    end;

    LPropertyName := LComponent.Name;
    LConvention.ApplyBinding(
      AViewModel, LPropertyName, LComponent, btEvent, LConvention);

    LPropertyName := 'Can' + LPropertyName;
    LProperty := LType.GetProperty(LPropertyName);
    if Assigned(LProperty) then
    begin
      LConvention := TElementConvention.Create('Enabled', '');
      LConvention.ApplyBinding(
        AViewModel, LPropertyName, LComponent, btProperty, LConvention);
      LConvention.Free;
    end;
  end;
end;

class procedure ViewModelBinder.BindProperties(
  AViewModel: TObject; AView: TComponent);
var
  i: Integer;
  LType: TRttiType;
  LComponent: TComponent;
  LParts: TStringDynArray;
  LPropertyName: string;
  LProperty: TRttiProperty;
  LPropertyType: TRttiType;
  LConvention: TElementConvention;

  LAttribute: ValidationAttribute;
  LBindingGroup: TBindingGroup;
begin
  LType := GetRttiType(AViewModel.ClassInfo);

  // TODO: refactor quick and dirty implementation
  LBindingGroup := FindBindingGroup(AView);
  if not Assigned(LBindingGroup) then
  begin
    LBindingGroup := TBindingGroup.Create(AView);
  end;
  for LAttribute in LType.GetCustomAttributes<ValidationAttribute>(True) do
  begin
    LBindingGroup.ValidationRules.Add(LAttribute.ValidationRuleClass.Create);
  end;

  for LComponent in AView do
  begin
    if LComponent.Name = '' then
    begin
      Continue;
    end;

    LParts := SplitString(LComponent.Name, '_');
    LPropertyName := LParts[0];

    LProperty := LType.GetProperty(LPropertyName);

    i := 1;
    while (i < Length(LParts)) and Assigned(LProperty) do
    begin
      LPropertyType := LProperty.PropertyType;
      LProperty := LPropertyType.GetProperty(LParts[i]);
      Inc(i);
    end;

    if not Assigned(LProperty) then
    begin
      Continue;
    end;

    LConvention := ConventionManager.GetElementConvention(LComponent.ClassType);
    if not Assigned(LConvention) then
    begin
      Continue;
    end;

    LPropertyName := ReplaceText(LComponent.Name, '_', '.');
    LConvention.ApplyBinding(
      AViewModel, LPropertyName, LComponent, btProperty, LConvention);
  end;
end;

end.
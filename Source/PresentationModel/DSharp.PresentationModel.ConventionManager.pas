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

unit DSharp.PresentationModel.ConventionManager;

interface

uses
  Classes,
  DSharp.Bindings,
  DSharp.Collections,
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
      AConvention: TElementConvention); static;
  end;

implementation

uses
  ActnList,
  Controls,
  DSharp.Bindings.Collections,
  DSharp.Bindings.VCLControls,
  DSharp.Core.Events,
  DSharp.Core.Reflection,
  DSharp.PresentationModel.Screen,
  DSharp.PresentationModel.TabSheetConductor,
  DSharp.PresentationModel.Validations,
  DSharp.PresentationModel.ViewLocator,
  DSharp.PresentationModel.ViewModelBinder,
  DSharp.Windows.CustomPresenter,
  StrUtils,
  SysUtils;

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

type
  TConventionManager = class
  private
    procedure PageControlCollectionChanged(Sender, Item: TObject;
      Action: TCollectionChangedAction);
  end;

var
  Manager: TConventionManager = nil;

{ TConventionManager }

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
    for LAttribute in LProperty.GetAttributesOfType<ValidationAttribute> do
    begin
      ABinding.ValidationRules.Add(LAttribute.ValidationRuleClass.Create);
    end;
  end;
end;

class procedure ConventionManager.ConfigureSelectedItem(AViewModel: TObject;
  APropertyName: string; AViewElement: TComponent; ASelectedItemPropertyName: string);
var
  LBindingGroup: TBindingGroup;
  LBinding: TBinding;
  LProperty: TRttiProperty;
  LPotentialName: string;
begin
  LBindingGroup := FindBindingGroup(AViewElement);

  for LPotentialName in DerivePotentialSelectionNames(AViewElement.Name) do
  begin
    LProperty := AViewModel.GetProperty(LPotentialName);
    if Assigned(LProperty) then
    begin
      LBinding := LBindingGroup.Bindings.Add();
      LBinding.Source := AViewModel;
      LBinding.SourcePropertyName := LPotentialName;
      LBinding.Target := AViewElement;
      LBinding.TargetPropertyName := ASelectedItemPropertyName;
    end;
  end;
end;

class constructor ConventionManager.Create;
begin
  FConventions := TObjectDictionary<TClass, TElementConvention>.Create([doOwnsValues]);

  AddElementConvention<TAction>('Caption', 'OnExecute');
  AddElementConvention<TButton>('Caption', 'OnClick');
  AddElementConvention<TCheckBox>('Checked', 'OnClick');
  AddElementConvention<TColorBox>('Selected', 'OnChange');
  AddElementConvention<TComboBox>('Text', 'OnChange');
  AddElementConvention<TCustomPresenter>('View.ItemsSource', 'OnCurrentChanged')
    .ApplyBinding :=
    procedure(AViewModel: TObject; APropertyName: string;
      AViewElement: TComponent; ABindingType: TBindingType;
      AConvention: TElementConvention)
    begin
      SetBinding(AViewModel, APropertyName, AViewElement, ABindingType, AConvention);
      ConfigureSelectedItem(AViewModel, APropertyName, AViewElement, 'View.CurrentItem');
    end;
  AddElementConvention<TEdit>('Text', 'OnChange');
  AddElementConvention<TLabel>('Caption', 'OnClick');
  AddElementConvention<TLabeledEdit>('Text', 'OnChange');
  AddElementConvention<TMemo>('Text', 'OnChange');
  AddElementConvention<TMonthCalendar>('Date', 'OnClick');
  AddElementConvention<TPageControl>('View.ItemsSource', 'OnCurrentChanged')
    .ApplyBinding :=
    procedure(AViewModel: TObject; APropertyName: string;
      AViewElement: TComponent; ABindingType: TBindingType;
      AConvention: TElementConvention)
    var
      LCollectionChanged: TEvent<TCollectionChangedEvent>;
      LItems: IList<TObject>;
      LItem: TObject;
    begin
      SetBinding(AViewModel, APropertyName, AViewElement, ABindingType, AConvention);
      ConfigureSelectedItem(AViewModel, APropertyName, AViewElement, 'View.CurrentItem');

      LItems := TPageControl(AViewElement).View.ItemsSource;
      if Assigned(LItems) then
      begin
        for LItem in LItems do
        begin
          Manager.PageControlCollectionChanged(AViewElement, LItem, caAdd);
        end;
      end;

      LCollectionChanged := TPageControl(AViewElement).View.OnCollectionChanged;
      LCollectionChanged.Add(Manager.PageControlCollectionChanged);
    end;
  AddElementConvention<TRadioButton>('Checked', 'OnClick');
  AddElementConvention<TRadioGroup>('ItemIndex', 'OnClick');
  AddElementConvention<TTrackBar>('Position', 'OnChange');
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
  LBindingGroup: TBindingGroup;
  LBinding: TBinding;
begin
  LBindingGroup := FindBindingGroup(AViewElement);
  if not Assigned(LBindingGroup) then
  begin
    LBindingGroup := TBindingGroup.Create(AViewElement.Owner);
  end;

  LBinding := LBindingGroup.Bindings.Add();

  ApplyValidation(LBinding, AViewModel, APropertyName);

  LBinding.Source := AViewModel;
  LBinding.SourcePropertyName := APropertyName;
  LBinding.Target := AViewElement;

  case ABindingType of
    btProperty:
    begin
      LBinding.TargetPropertyName := AConvention.PropertyName;
    end;
    btEvent:
    begin
      LBinding.TargetPropertyName := AConvention.EventName;
    end;
  end;
end;

{ TConventionManager }

procedure TConventionManager.PageControlCollectionChanged(Sender,
  Item: TObject; Action: TCollectionChangedAction);
var
  LView: TControl;
  LTabSheet: TTabSheet;
begin
  LView := ViewLocator.GetOrCreateViewType(Item.ClassType);
  LTabSheet := TPageControl(Sender).Pages[TPageControl(Sender).View.ItemsSource.IndexOf(Item)];
  LView.Parent := LTabSheet;
  TTabSheetConductor.Create(Item, LTabSheet);
  ViewModelBinder.Bind(Item, LView);

  if Supports(Item, IHaveDisplayName) then
  begin
    TBinding.Create(Item, 'DisplayName', LTabSheet, 'Caption', bmOneWay);
  end;
end;

end.

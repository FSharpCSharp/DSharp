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

unit DSharp.PresentationModel.VCLConventionManager;

interface

implementation

uses
  ActnList,
  Classes,
  Controls,
  DSharp.Bindings,
  DSharp.Bindings.Collections,
  DSharp.Bindings.VCLControls,
  DSharp.Core.Reflection,
  DSharp.PresentationModel.ConventionManager,
  DSharp.PresentationModel.ElementConvention,
  DSharp.PresentationModel.Screen,
  DSharp.PresentationModel.TabSheetConductor,
  DSharp.PresentationModel.ViewModelBinder,
  DSharp.PresentationModel.ViewLocator,
  DSharp.Windows.CustomPresenter,
  Spring,
  Spring.Collections,
  Rtti,
  SysUtils,
  TypInfo;

type
  TConventionManager = class
  private
    procedure FlowPanelCollectionChanged(Sender: TObject; const Item: TObject;
      Action: TCollectionChangedAction);
    procedure PageControlCollectionChanged(Sender: TObject; const Item: TObject;
      Action: TCollectionChangedAction);
    procedure ScrollBoxCollectionChanged(Sender: TObject; const Item: TObject;
      Action: TCollectionChangedAction);
  end;

  ConventionManagerVCLHelper = record helper for ConventionManager
    class procedure Initialize; static;
  end;

var
  Manager: TConventionManager = nil;

{ TConventionManager }

procedure TConventionManager.FlowPanelCollectionChanged(Sender: TObject;
  const Item: TObject; Action: TCollectionChangedAction);var
  i: Integer;
  LFlowPanel: TFlowPanel;
  LItemIndex: Integer;
  LView: TControl;
begin
  LFlowPanel := TFlowPanel(Sender);
  case Action of
    caAdded:
    begin
      LView := ViewLocator.GetOrCreateViewType(Item.ClassType) as TControl;

      LView.Parent := LFlowPanel;
      LView.Tag := NativeInt(Item);

      LItemIndex := LFlowPanel.View.ItemsSource.IndexOf(Item);
      LFlowPanel.SetControlIndex(LView, LItemIndex);

      ViewModelBinder.Bind(Item, LView);
    end;
    caRemoved:
    begin
      LView := nil;
      for i := 0 to Pred(LFlowPanel.ControlCount) do
      begin
        if LFlowPanel.Controls[i].Tag = NativeInt(Item) then
        begin
          LView := LFlowPanel.Controls[i];
        end;
      end;
      if Assigned(LView) then
      begin
        LView.Parent := nil;
        LView.Free;
      end;
    end;
  end;
end;

procedure TConventionManager.PageControlCollectionChanged(Sender: TObject;
  const Item: TObject; Action: TCollectionChangedAction);
var
  LItemIndex: Integer;
  LPageControl: TPageControl;
  LTabSheet: TTabSheet;
  LView: TControl;
begin
  LPageControl := TPageControl(Sender);
  case Action of
    caAdded:
    begin
      LView := ViewLocator.GetOrCreateViewType(Item.ClassType) as TControl;
      LItemIndex := LPageControl.View.ItemsSource.IndexOf(Item);
      LTabSheet := LPageControl.Pages[LItemIndex];
      LView.Parent := LTabSheet;
      TTabSheetConductor.Create(Item, LTabSheet);
      ViewModelBinder.Bind(Item, LView);

      if Supports(Item, IHaveDisplayName) then
      begin
        FindBindingGroup(LView).AddBinding(
          Item, 'DisplayName', LTabSheet, 'Caption', bmOneWay)
      end;
    end;
  end;
end;

procedure TConventionManager.ScrollBoxCollectionChanged(Sender: TObject;
  const Item: TObject; Action: TCollectionChangedAction);
var
  i: Integer;
  LItemIndex: Integer;
  LScrollBox: TScrollBox;
  LView: TControl;
begin
  LScrollBox := TScrollBox(Sender);
  case Action of
    caAdded:
    begin
      LView := ViewLocator.GetOrCreateViewType(Item.ClassType) as TControl;
      LView.Left := 0;
      LView.Top := 0;

      LItemIndex := LScrollBox.View.ItemsSource.IndexOf(Item);
      for i := 0 to Pred(LScrollBox.ControlCount) do
      begin
        if i < LItemIndex then
        begin
          LView.Top := LView.Top + LScrollBox.Controls[i].Height;
        end
        else
        begin
          LScrollBox.Controls[i].Top := LScrollBox.Controls[i].Top + LView.Height;
        end;
      end;

      LView.Parent := LScrollBox;
      LView.Tag := NativeInt(Item);

      ViewModelBinder.Bind(Item, LView);
    end;
    caRemoved:
    begin
      LView := nil;
      for i := 0 to Pred(LScrollBox.ControlCount) do
      begin
        if LScrollBox.Controls[i].Tag = NativeInt(Item) then
        begin
          LView := LScrollBox.Controls[i];
        end;
      end;
      if Assigned(LView) then
      begin
        for i := 0 to Pred(LScrollBox.ControlCount) do
        begin
          if LScrollBox.Controls[i].Top > LView.Top then
          begin
            LScrollBox.Controls[i].Top := LScrollBox.Controls[i].Top - LView.Height;
          end;
        end;
        LView.Parent := nil;
        LView.Free;
      end;
    end;
  end;
end;

{ ConventionManagerVCLHelper }

class procedure ConventionManagerVCLHelper.Initialize;
begin
  AddElementConvention<TAction>('Caption', 'OnExecute');
  AddElementConvention<TButton>('Caption', 'OnClick');
  AddElementConvention<TCheckBox>('Checked', 'OnClick');
  AddElementConvention<TColorBox>('Selected', 'OnChange');
  AddElementConvention<TComboBox>('Text', 'OnChange')
    .ApplyBinding :=
      procedure(AViewModel: TObject; APropertyName: string;
        AViewElement: TComponent; ABindingType: TBindingType;
        AConvention: TElementConvention)
      begin
        if (ABindingType = btProperty)
          and (AViewModel.GetProperty(APropertyName).PropertyType.TypeKind in [tkEnumeration, tkSet]) then
        begin
          SetBinding(AViewModel, APropertyName, AViewElement, 'Items');
          SetBinding(AViewModel, APropertyName, AViewElement, 'ItemIndex');
        end
        else
        begin
          SetBinding(AViewModel, APropertyName, AViewElement, ABindingType, AConvention);
        end;
      end;
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
  AddElementConvention<TFlowPanel>('View.ItemsSource', 'OnCurrentChanged')
    .ApplyBinding :=
      procedure(AViewModel: TObject; APropertyName: string;
        AViewElement: TComponent; ABindingType: TBindingType;
        AConvention: TElementConvention)
      var
        LCollectionChanged: IEvent<TCollectionChangedEvent>;
        LItems: IObjectList;
        LItem: TObject;
      begin
        SetBinding(AViewModel, APropertyName, AViewElement, ABindingType, AConvention);
        ConfigureSelectedItem(AViewModel, APropertyName, AViewElement, 'View.CurrentItem');
        LItems := TFlowPanel(AViewElement).View.ItemsSource;
        if Assigned(LItems) then
        begin
          for LItem in LItems do
          begin
            Manager.FlowPanelCollectionChanged(AViewElement, LItem, caAdded);
          end;
        end;
        LCollectionChanged := TFlowPanel(AViewElement).View.OnCollectionChanged;
        LCollectionChanged.Add(Manager.FlowPanelCollectionChanged);
      end;
  AddElementConvention<TLabel>('Caption', 'OnClick');
  AddElementConvention<TLabeledEdit>('Text', 'OnChange');
  AddElementConvention<TListBox>('View.ItemsSource', 'OnClick')
    .ApplyBinding :=
      procedure(AViewModel: TObject; APropertyName: string;
        AViewElement: TComponent; ABindingType: TBindingType;
        AConvention: TElementConvention)
      begin
        if (ABindingType = btProperty)
          and (AViewModel.GetProperty(APropertyName).PropertyType.TypeKind in [tkEnumeration, tkSet]) then
        begin
          SetBinding(AViewModel, APropertyName, AViewElement, 'Items');
          SetBinding(AViewModel, APropertyName, AViewElement, 'ItemIndex');
        end
        else
        begin
          SetBinding(AViewModel, APropertyName, AViewElement, ABindingType, AConvention);
          ConfigureSelectedItem(AViewModel, APropertyName, AViewElement, 'View.CurrentItem');
        end;
      end;
  AddElementConvention<TMemo>('Text', 'OnChange');
  AddElementConvention<TMonthCalendar>('Date', 'OnClick');
  AddElementConvention<TPageControl>('View.ItemsSource', 'OnCurrentChanged')
    .ApplyBinding :=
    procedure(AViewModel: TObject; APropertyName: string;
      AViewElement: TComponent; ABindingType: TBindingType;
      AConvention: TElementConvention)
    var
      LCollectionChanged: IEvent<TCollectionChangedEvent>;
      LItems: IObjectList;
      LItem: TObject;
    begin
      SetBinding(AViewModel, APropertyName, AViewElement, ABindingType, AConvention);
      ConfigureSelectedItem(AViewModel, APropertyName, AViewElement, 'View.CurrentItem');
      LItems := TPageControl(AViewElement).View.ItemsSource;
      if Assigned(LItems) then
      begin
        for LItem in LItems do
        begin
          Manager.PageControlCollectionChanged(AViewElement, LItem, caAdded);
        end;
      end;
      LCollectionChanged := TPageControl(AViewElement).View.OnCollectionChanged;
      LCollectionChanged.Add(Manager.PageControlCollectionChanged);
    end;
  AddElementConvention<TPanel>('BindingSource', '').ApplyBinding :=
    procedure(AViewModel: TObject; APropertyName: string;
      AViewElement: TComponent; ABindingType: TBindingType;
      AConvention: TElementConvention)
    var
      LProperty: TRttiProperty;
      LView: TControl;
      LViewModel: TObject;
    begin
      SetBinding(AViewModel, APropertyName, AViewElement, ABindingType, AConvention);

      if AViewModel.TryGetProperty(APropertyName, LProperty)
        and (LProperty.PropertyType.TypeKind in [tkClass, tkInterface]) then
      begin
        if LProperty.PropertyType.IsInstance then
        begin
          LViewModel := LProperty.GetValue(AViewModel).AsObject;
        end
        else
        begin
          LViewModel := LProperty.GetValue(AViewModel).AsInterface as TObject;
        end;

        if Assigned(LViewModel) then
        begin
          LView := ViewLocator.GetOrCreateViewType(LViewModel.ClassType) as TControl;
          LView.Parent := TPanel(AViewElement);
          LView.Align := alClient;
          ViewModelBinder.Bind(LViewModel, LView);
        end;
      end;
    end;
  AddElementConvention<TRadioButton>('Checked', 'OnClick');
  AddElementConvention<TRadioGroup>('ItemIndex', 'OnClick');
  AddElementConvention<TScrollBox>('View.ItemsSource', 'OnCurrentChanged').ApplyBinding :=
    procedure(AViewModel: TObject; APropertyName: string;
      AViewElement: TComponent; ABindingType: TBindingType;
      AConvention: TElementConvention)
    var
      LCollectionChanged: IEvent<TCollectionChangedEvent>;
      LItems: IObjectList;
      LItem: TObject;
    begin
      SetBinding(AViewModel, APropertyName, AViewElement, ABindingType, AConvention);
      ConfigureSelectedItem(AViewModel, APropertyName, AViewElement, 'View.CurrentItem');
      LItems := TScrollBox(AViewElement).View.ItemsSource;
      if Assigned(LItems) then
      begin
        for LItem in LItems do
        begin
          Manager.ScrollBoxCollectionChanged(AViewElement, LItem, caAdded);
        end;
      end;
      LCollectionChanged := TScrollBox(AViewElement).View.OnCollectionChanged;
      LCollectionChanged.Add(Manager.ScrollBoxCollectionChanged);
    end;
  AddElementConvention<TTrackBar>('Position', 'OnChange');
end;

initialization
  ConventionManager.Initialize;

end.

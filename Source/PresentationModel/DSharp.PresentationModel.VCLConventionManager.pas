unit DSharp.PresentationModel.VCLConventionManager;

interface

implementation

uses
  ActnList,
  Classes,
  Controls,
  ExtCtrls,
  ComCtrls,
  DSharp.Bindings,
  DSharp.Bindings.Collections,
  DSharp.Bindings.VCLControls,
  DSharp.Collections,
  DSharp.Core.Events,
  DSharp.Core.Reflection,
  DSharp.PresentationModel.ConventionManager,
  DSharp.PresentationModel.ElementConvention,
  DSharp.PresentationModel.ViewModelBinder,
  DSharp.PresentationModel.ViewLocator,
  DSharp.Windows.CustomPresenter,
  Rtti,
  SysUtils,
  TypInfo,
  DSharp.PresentationModel,
  DSharp.Bindings.Notifications,
  DSharp.PresentationModel.View,
  DSharp.PresentationModel.Extensions;

type
  TConventionManager = class
  private
    procedure FlowPanelCollectionChanged(Sender: TObject; const Item: TObject;
      Action: TCollectionChangedAction);
    procedure OnPanelBindSourceChanged(ASender: TObject; APropertyName: string;
      AUpdateTrigger: TUpdateTrigger = utPropertyChanged);
    procedure ScrollBoxCollectionChanged(Sender: TObject; const Item: TObject;
      Action: TCollectionChangedAction);
  end;

  ConventionManagerHelper = record helper for ConventionManager
    class procedure Initialize; static;
  end;

var
  Manager: TConventionManager = nil;

  { TConventionManager }

procedure TConventionManager.FlowPanelCollectionChanged(Sender: TObject;
  const Item: TObject; Action: TCollectionChangedAction);
var
  i: Integer;
  LFlowPanel: TFlowPanel;
  LItemIndex: Integer;
  LView: TControl;
begin
  LFlowPanel := TFlowPanel(Sender);
  case Action of
    caAdd:
      begin
        LView := ViewLocator.LocateForModel(Item, nil, nil) as TControl;

        LView.Parent := LFlowPanel;
        LView.Tag := NativeInt(Item);

        LItemIndex := LFlowPanel.View.ItemsSource.IndexOf(Item);
        LFlowPanel.SetControlIndex(LView, LItemIndex);

        ViewModelBinder.Bind(Item, LView, nil);
      end;
    caRemove:
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

procedure TConventionManager.OnPanelBindSourceChanged(ASender: TObject;
  APropertyName: string; AUpdateTrigger: TUpdateTrigger);
var
  LBindingSource: TObject;
begin
  LBindingSource := (ASender as TPanel).BindingSource;
  if Assigned(LBindingSource) then
    View.SetModel(ASender as TComponent, LBindingSource);
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
    caAdd:
      begin
        LView := ViewLocator.LocateForModel(Item, nil, nil) as TControl;
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
            LScrollBox.Controls[i].Top := LScrollBox.Controls[i].Top +
              LView.Height;
          end;
        end;

        LView.Parent := LScrollBox;
        LView.Tag := NativeInt(Item);

        ViewModelBinder.Bind(Item, LView, nil);
      end;
    caRemove:
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
              LScrollBox.Controls[i].Top := LScrollBox.Controls[i].Top -
                LView.Height;
            end;
          end;
          LView.Parent := nil;
          LView.Free;
        end;
      end;
  end;
end;

{ ConventionManagerHelper }

class procedure ConventionManagerHelper.Initialize;
begin
  AddElementConvention<TAction>('Caption', 'OnExecute');
  AddElementConvention<TButton>('Caption', 'OnClick');
  AddElementConvention<TCheckBox>('Checked', 'OnClick');
  AddElementConvention<TColorBox>('Selected', 'OnChange');
  AddElementConvention<TComboBox>('Text', 'OnChange').ApplyBinding :=
      function(AViewModel: TObject; APropertyName: string;
      AViewElement: TComponent; ABindingType: TBindingType;
      AConvention: TElementConvention): Boolean
    begin
      if (ABindingType = btProperty) and
        (AViewModel.GetProperty(APropertyName).PropertyType.TypeKind
        in [tkEnumeration, tkSet]) then
      begin
        SetBinding(AViewModel, APropertyName, AViewElement, 'Items');
        SetBinding(AViewModel, APropertyName, AViewElement, 'ItemIndex');
      end
      else
      begin
        SetBinding(AViewModel, APropertyName, AViewElement, ABindingType,
          AConvention);
      end;
      Result := True;
    end;
  AddElementConvention<TCustomPresenter>('View.ItemsSource', 'OnCurrentChanged')
    .ApplyBinding :=
      function(AViewModel: TObject; APropertyName: string;
      AViewElement: TComponent; ABindingType: TBindingType;
      AConvention: TElementConvention): Boolean
    begin
      SetBinding(AViewModel, APropertyName, AViewElement, ABindingType,
        AConvention);
      ConfigureSelectedItem(AViewModel, APropertyName, AViewElement,
        'View.CurrentItem');
      Result := True;
    end;
  AddElementConvention<TEdit>('Text', 'OnChange');
  AddElementConvention<TFlowPanel>('View.ItemsSource', 'OnCurrentChanged')
    .ApplyBinding :=
      function(AViewModel: TObject; APropertyName: string;
      AViewElement: TComponent; ABindingType: TBindingType;
      AConvention: TElementConvention): Boolean
    var
      LCollectionChanged: IEvent<TCollectionChangedEvent>;
      LItems: IList;
      LItem: TValue;
    begin
      SetBinding(AViewModel, APropertyName, AViewElement, ABindingType,
        AConvention);
      ConfigureSelectedItem(AViewModel, APropertyName, AViewElement,
        'View.CurrentItem');
      LItems := TFlowPanel(AViewElement).View.ItemsSource;
      if Assigned(LItems) then
      begin
        for LItem in LItems do
        begin
          Manager.FlowPanelCollectionChanged(AViewElement,
            LItem.ToObject, caAdd);
        end;
      end;
      LCollectionChanged := TFlowPanel(AViewElement).View.OnCollectionChanged;
      LCollectionChanged.Add(Manager.FlowPanelCollectionChanged);
      Result := True;
    end;
  AddElementConvention<TImage>('', 'OnClick');
  AddElementConvention<TLabel>('Caption', 'OnClick');
  AddElementConvention<TLabeledEdit>('Text', 'OnChange');
  AddElementConvention<TListBox>('View.ItemsSource', 'OnClick').ApplyBinding :=
      function(AViewModel: TObject; APropertyName: string;
      AViewElement: TComponent; ABindingType: TBindingType;
      AConvention: TElementConvention): Boolean
    begin
      SetBinding(AViewModel, APropertyName, AViewElement, ABindingType,
        AConvention);
      ConfigureSelectedItem(AViewModel, APropertyName, AViewElement,
        'View.CurrentItem');
      Result := True;
    end;
  AddElementConvention<TMemo>('Text', 'OnChange');
  AddElementConvention<TMonthCalendar>('Date', 'OnClick');
  AddElementConvention<TPanel>('BindingSource', '').ApplyBinding :=
      function(AViewModel: TObject; APropertyName: string;
      AViewElement: TComponent; ABindingType: TBindingType;
      AConvention: TElementConvention): Boolean
    var
      LBindingGroup: TBindingGroup;
    begin
      // Bind context screens by convention
      // Currently we handle enumerations and strings
      if AViewModel.GetProperty(APropertyName).PropertyType.TypeKind
        in [tkEnumeration, tkString, tkLString, tkWString, tkUString] then
      begin
        // Bind context screens
        LBindingGroup := FindBindingGroup(AViewElement);

        // [Binding('View.Context', '{Binding Mode}')]
        LBindingGroup.AddBinding(AViewElement.DataContext, APropertyName,
          AViewElement, 'View.Context');
        { TODO -o##jwp -cEnhance : Add logging of binding }

        // [Binding('View.Model', '{Binding}')]
        LBindingGroup.AddBinding(AViewElement, 'DataContext', AViewElement,
          'View.Model');
        { TODO -o##jwp -cEnhance : Add logging of binding }
      end
      else
      begin
        // Bind to TPanel
        (AViewElement as INotifyPropertyChanged).OnPropertyChanged.Add
          (Manager.OnPanelBindSourceChanged);
        SetBinding(AViewModel, APropertyName, AViewElement, ABindingType,
          AConvention);
      end;

      Result := True;
    end;
  AddElementConvention<TProgressBar>('Position', '');
  AddElementConvention<TRadioButton>('Checked', 'OnClick');
  AddElementConvention<TRadioGroup>('ItemIndex', 'OnClick');
  AddElementConvention<TScrollBox>('View.ItemsSource', 'OnCurrentChanged')
    .ApplyBinding :=
      function(AViewModel: TObject; APropertyName: string;
      AViewElement: TComponent; ABindingType: TBindingType;
      AConvention: TElementConvention): Boolean
    var
      LCollectionChanged: IEvent<TCollectionChangedEvent>;
      LItems: IList;
      LItem: TValue;
    begin
      SetBinding(AViewModel, APropertyName, AViewElement, ABindingType,
        AConvention);
      ConfigureSelectedItem(AViewModel, APropertyName, AViewElement,
        'View.CurrentItem');
      LItems := TScrollBox(AViewElement).View.ItemsSource;
      if Assigned(LItems) then
      begin
        for LItem in LItems do
        begin
          Manager.ScrollBoxCollectionChanged(AViewElement,
            LItem.ToObject, caAdd);
        end;
      end;
      LCollectionChanged := TScrollBox(AViewElement).View.OnCollectionChanged;
      LCollectionChanged.Add(Manager.ScrollBoxCollectionChanged);
      Result := True;
    end;
  AddElementConvention<TTrackBar>('Position', 'OnChange');
  AddElementConvention<TGroupBox>('Caption', 'OnClick');
end;

initialization

ConventionManager.Initialize;

end.

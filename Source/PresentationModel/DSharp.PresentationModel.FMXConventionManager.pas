unit DSharp.PresentationModel.FMXConventionManager;

interface

implementation

uses
  Classes,
  TypInfo,
  DSharp.Bindings,
  DSharp.Bindings.Notifications,
  DSharp.PresentationModel.ElementConvention,
  DSharp.PresentationModel.ConventionManager,
  FMX.Types,
  FMX.Calendar,
  FMX.CalendarEdit,
  FMX.Controls,
  FMX.ComboEdit,
  FMX.StdCtrls,
  FMX.DateTimeCtrls,
  FMX.Edit,
  FMX.Forms,
  FMX.ExtCtrls,
  FMX.Memo,
  FMX.Layouts,
  FMX.Objects,
  DSharp.PresentationModel.View,
  DSharp.Bindings.FMXControls,
  DSharp.Bindings.Collections,
  DSharp.Collections,
  DSharp.Core.Events,
  DSharp.Core.Reflection,
  DSharp.PresentationModel.Extensions,
  DSharp.PresentationModel.ViewLocator,
  DSharp.PresentationModel.ViewModelBinder;

type
  TManager = class
  private
    procedure ScrollBoxCollectionChanged(Sender: TObject; const Item: TObject;
      Action: TCollectionChangedAction);
  public
    procedure OnPanelBindSourceChanged(ASender: TObject; APropertyName: string;
      AUpdateTrigger: TUpdateTrigger = utPropertyChanged);
  end;

  ConventionManagerHelper = record helper for ConventionManager
    class procedure Initialize; static;
  end;

var
  Manager: TManager = nil;

  { ConventionManagerHelper }

class procedure ConventionManagerHelper.Initialize;
begin
  AddElementConvention<TBasicAction>('', 'OnExecute');
  AddElementConvention<TButton>('Caption', 'OnClick');
  AddElementConvention<TCalendar>('Date', 'OnChange');
  AddElementConvention<TCalendarEdit>('Date', 'OnChange');
  AddElementConvention<TCheckBox>('IsChecked', 'OnClick');
  AddElementConvention<TComboEdit>('Text', 'OnChange');
  AddElementConvention<TEdit>('Text', 'OnChangeTracking');
  AddElementConvention<TGroupBox>('Caption', 'OnClick');
  AddElementConvention<TImage>('', 'OnClick');
  AddElementConvention<TLabel>('Text', 'OnClick');
  AddElementConvention<TLayout>('BindingSource', '').ApplyBinding :=
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
        // Bind to TLayout/TPanel container
        (AViewElement as INotifyPropertyChanged).OnPropertyChanged.Add
          (Manager.OnPanelBindSourceChanged);
        SetBinding(AViewModel, APropertyName, AViewElement, ABindingType,
          AConvention);
      end;

      Result := True;
    end;
  AddElementConvention<TMemo>('Text', 'OnChangeTracking');
  AddElementConvention<TPanel>('BindingSource', '').ApplyBinding :=
    GetElementConvention(TLayout).ApplyBinding;
  // Use the same convention as TLayout
  AddElementConvention<TRadioButton>('IsChecked', 'OnClick');
  AddElementConvention<TTrackBar>('Value', 'OnChange');
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
end;

procedure TManager.OnPanelBindSourceChanged(ASender: TObject;
  APropertyName: string; AUpdateTrigger: TUpdateTrigger);
var
  LBindingSource: TObject;
begin
  if (ASender is TLayout) then
    LBindingSource := (ASender as TLayout).BindingSource
  else if (ASender is TPanel) then
    LBindingSource := (ASender as TPanel).BindingSource
  else
    LBindingSource := nil;
  if Assigned(LBindingSource) then
    View.SetModel(ASender as TComponent, LBindingSource);
end;

procedure TManager.ScrollBoxCollectionChanged(Sender: TObject;
  const Item: TObject; Action: TCollectionChangedAction);
var
  i: Integer;
  LItemIndex: Integer;
  LScrollBox: TScrollBox;
  LView: TControl;
  LForm: TForm;
begin
  LScrollBox := TScrollBox(Sender);
  case Action of
    caAdd:
      begin
        LForm := ViewLocator.LocateForModel(Item, nil, nil) as TForm;

        LView := LForm.Children[0] as TControl;
        LView.Position.X := 0;
        LView.Position.Y := 0;

        LItemIndex := LScrollBox.View.ItemsSource.IndexOf(Item);
        for i := 0 to Pred(LScrollBox.ChildrenCount) do
        begin
          if i < LItemIndex then
          begin
            LView.Position.Y := LView.Position.Y +
              TControl(LScrollBox.Children[i]).Height;
          end
          else
          begin
            TControl(LScrollBox.Children[i]).Position.Y :=
              TControl(LScrollBox.Children[i]).Position.Y + LView.Height;
          end;
        end;

        LView.Parent := LScrollBox;
        LView.Tag := NativeInt(Item);

        ViewModelBinder.Bind(Item, LForm, nil);
      end;
    caRemove:
      begin
        LView := nil;
        for i := 0 to Pred(LScrollBox.ChildrenCount) do
        begin
          if LScrollBox.Children[i].Tag = NativeInt(Item) then
          begin
            LView := TControl(LScrollBox.Children[i]);
          end;
        end;
        if Assigned(LView) then
        begin
          for i := 0 to Pred(LScrollBox.ChildrenCount) do
          begin
            if TControl(LScrollBox.Children[i]).Position.Y > LView.Position.Y
            then
            begin
              TControl(LScrollBox.Children[i]).Position.Y :=
                TControl(LScrollBox.Children[i]).Position.Y - LView.Height;
            end;
          end;
          LView.Parent := nil;
          LView.Free;
        end;
      end;
  end;
end;

initialization

ConventionManager.Initialize;

end.

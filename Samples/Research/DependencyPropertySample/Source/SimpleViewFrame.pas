unit SimpleViewFrame;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  ExtCtrls,
  DSharp.PresentationModel,
  StdCtrls;

type

  [Binding('ViewModelBinder.ConventionsApplied', 'True')]
  TSimpleView = class(TFrame)
    ButtonShowInfo: TButton;
    [Binding('ViewModelBinder.ConventionsApplied', 'True')]
    MemoLog: TMemo;
    PanelView: TPanel;
    LabelDataContext: TLabel;
    procedure ButtonShowInfoClick(Sender: TObject);
  private
    FMyModel: TObject;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetMyModel(const Value: TObject);
    property MyModel: TObject read FMyModel write SetMyModel;
  end;

implementation

uses
  DSharp.PresentationModel.Extensions,
  DSharp.PresentationModel.ViewModelBinder,
  StrUtils,
  DSharp.Bindings,
  ViewExtensions;

{$R *.dfm}

constructor TSimpleView.Create(AOwner: TComponent);
var
  LBindingGroup: TBindingGroup;
begin
  inherited;

  // Create handler for OnDataContextChanged
  OnDataContextChanged.Add(
    procedure(TargetLocation: TComponent; e: IDependencyPropertyChangedEventArgs)
    begin
      LabelDataContext.Caption := 'DataContext: ' + Self.DataContext.ToString();

      // Log all DataContext changes
      MemoLog.Lines.Add(Format('property changed -> target:%s  DP:%s  old:%s  new:%s', [TargetLocation.ClassName, e.Prop.Name, e.OldValue.ToString,
        e.NewValue.ToString]));
    end);

  // Find or create binding group
  LBindingGroup := FindBindingGroup(Self);
  if not Assigned(LBindingGroup) then
    LBindingGroup := TBindingGroup.Create(Self);

  // Create binding from Self.DataContext to Self.MyModel
  LBindingGroup.AddBinding(Self, 'DataContext', Self, 'MyModel');

  // This line should create a binding from DataContext to attached property "TViewExtensions.ModelProperty"
  LBindingGroup.AddBinding(Self, 'DataContext', Self, 'TViewExtensions.Model');
end;

procedure TSimpleView.ButtonShowInfoClick(Sender: TObject);
begin
  MemoLog.Lines.Add('TViewExtensions.Model: ' + TViewExtensions.ModelProperty.GetValue(Self).ToString);
end;

procedure TSimpleView.SetMyModel(const Value: TObject);
var
  ValueString: string;
begin
  FMyModel := Value;
  if Assigned(Value) then
    ValueString := Value.ToString()
  else
    ValueString := 'nil';
  MemoLog.Lines.Add('MyModel changed: ' + ValueString);
end;

end.

unit ConverterViewModel;

interface

uses
  Sysutils,
  StdCtrls,
  ConverterInterfaces,
  DSharp.Collections,
  DSharp.PresentationModel,
  DSharp.PresentationModel.ObservableCollectionIntf,
  TextConverter;

type
  TConverterViewModel = class(TScreen, IConverterViewModel)
  private
    FHistory: IObservableCollection<string>;
    FSomeText: string;
    FTextConverter: TTextConverter;
    procedure AddToHistory(Item: string);
    procedure SetSomeText(const Value: string);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure ConvertText;
    property History: IObservableCollection<string> read FHistory;
    property SomeText: string read FSomeText write SetSomeText;
  end;

implementation

uses
  DSharp.PresentationModel.BindableCollection;

constructor TConverterViewModel.Create;
begin
  inherited;

  // Initialize applications title
  DisplayName := 'Minimal MVVM';

  FHistory := TBindableCollection<string>.Create();

  FTextConverter := TTextConverter.Create(
    function(InputText: string): string
    begin
      Result := UpperCase(InputText);
    end);
end;

destructor TConverterViewModel.Destroy;
begin
  FTextConverter.Free;
  inherited;
end;

procedure TConverterViewModel.AddToHistory(Item: string);
begin
  if not FHistory.Contains(Item) then
  begin
    FHistory.Add(Item);
    NotifyOfPropertyChange('History');
  end;
end;

procedure TConverterViewModel.ConvertText;
begin
  AddToHistory(FTextConverter.ConvertText(SomeText));
  SomeText := '';
end;

procedure TConverterViewModel.SetSomeText(const Value: string);
begin
  FSomeText := Value;
  NotifyOfPropertyChange('SomeText');
end;

initialization

TConverterViewModel.ClassName;

end.

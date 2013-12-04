unit ShellViewModel;

interface

uses
  Classes,
  SysUtils,
  Graphics,
  DSharp.PresentationModel,
  DSharp.PresentationModel.EventAggregatorIntf,
  Interfaces,
  ColorEvent;

type
  ///	<summary>
  ///	  Implementation of <see cref="IShellViewModel" />
  ///	</summary>
  TShellViewModel = class(TScreen, IShellViewModel, IHandle<TColorEvent>)
  strict private
    FColor: TColor;
    FColorModel: IColorViewModel;
    procedure SetColor(const Value: TColor);
  public
    ///	<summary>
    ///	  This constructor is called by the Dependency Injection container with
    ///	  parameters already created for you.
    ///	</summary>
    constructor Create(const ColorModel: IColorViewModel;
      const Events: IEventAggregator);

    ///	<summary>
    ///	  This method is called after a ColorEvent message is published from
    ///	  somewhere else in the application.
    ///	</summary>
    procedure Handle(AMessage: TColorEvent);

    ///	<summary>
    ///	  This property is for changing the color of the rectangle.
    ///	</summary>
    property Color: TColor read FColor write SetColor;
    property ColorModel: IColorViewModel read FColorModel;
  end;

implementation

constructor TShellViewModel.Create(const ColorModel: IColorViewModel;
  const Events: IEventAggregator);
begin
  inherited Create;

  FColorModel := ColorModel;

  // Get the event aggregator through the constructor and
  // subscribe this ColorViewModel so it can listen for ColorEvent messages.
  Events.Subscribe(Self);
end;

procedure TShellViewModel.Handle(AMessage: TColorEvent);
begin
  Color := AMessage.Color;
end;

procedure TShellViewModel.SetColor(const Value: TColor);
begin
  FColor := Value;
  NotifyOfPropertyChange('Color');
end;

initialization

TShellViewModel.ClassName;

end.

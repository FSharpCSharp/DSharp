unit ColorViewModel;

interface

uses
  Graphics,
  Interfaces,
  DSharp.PresentationModel,
  DSharp.PresentationModel.EventAggregatorIntf;

type
  ///	<summary>
  ///	  Implementation of <see cref="IColorViewModel" />
  ///	</summary>
  TColorViewModel = class(TScreen, IColorViewModel)
  private
    ///	<summary>
    ///	  Get the event aggregator through the constructor and store it in a
    ///	  field so we can publish messages later.
    ///	</summary>
    FEvents: IEventAggregator;
  public
    constructor Create(const Events: IEventAggregator);
    procedure Red;
    procedure Green;
    procedure Blue;
  end;

implementation

uses
  ColorEvent;

constructor TColorViewModel.Create(const Events: IEventAggregator);
begin
  inherited Create();
  FEvents := Events;
end;

procedure TColorViewModel.Red;
begin
  FEvents.PublishOnUIThread(TColorEvent.Create(clRed));
end;

procedure TColorViewModel.Green;
begin
  FEvents.PublishOnUIThread(TColorEvent.Create(clGreen));
end;

procedure TColorViewModel.Blue;
begin
  FEvents.PublishOnUIThread(TColorEvent.Create(clBlue));
end;

initialization

TColorViewModel.ClassName;

end.

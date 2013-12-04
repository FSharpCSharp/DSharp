unit DSharp.PresentationModel.RoutedEvent;

interface

uses
  DSharp.Core.EventArgs;

type
  IRoutedEventArgs = interface(IEventArgs)
    ['{FD3969EE-4304-43F1-B352-E00EF244B430}']
  end;

  TRoutedEventArgs = class(TEventArgs, IRoutedEventArgs);

  {$M+}
  TRoutedEvent = reference to procedure(Sender: TObject;
    EventArgs: IRoutedEventArgs);

implementation

end.

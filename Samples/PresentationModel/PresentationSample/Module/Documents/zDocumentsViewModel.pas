unit zDocumentsViewModel;

interface

uses
  Classes,
  SysUtils,
  DSharp.PresentationModel,
  Interfaces;

type
  TDocumentsViewModel = class(TScreen, IDocumentsViewModel)
  public
    constructor Create; override;
  end;

implementation

{ TDocumentsViewModel }

constructor TDocumentsViewModel.Create;
begin
  inherited Create;
  DisplayName := 'Hello';
end;

end.

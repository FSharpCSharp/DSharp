unit PageTwoViewModel;

interface

uses
  Interfaces,
  DSharp.PresentationModel;

type
  ///	<summary>
  ///	  Implementation of <see cref="IPageTwoViewModel" />
  ///	</summary>
  TPageTwoViewModel = class(TScreen, IPageTwoViewModel)
  protected
    procedure OnActivate; override;
  end;

implementation

uses
  Dialogs;

{ TPageTwoViewModel }

procedure TPageTwoViewModel.OnActivate;
begin
  ShowMessage('Page Two Activated'); // Don't do this in a real VM.
  inherited;
end;

initialization

TPageTwoViewModel.ClassName;

end.

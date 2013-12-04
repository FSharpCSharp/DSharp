unit AppViewModel;

interface

uses
  Sysutils,
  StdCtrls,
  AppInterfaces,
  DSharp.PresentationModel;

type
  TAppViewModel = class(TScreen, IAppViewModel)
  public
    procedure ClickMe(Sender: TObject);
  end;

implementation

uses
  ButtonExtension;

procedure TAppViewModel.ClickMe(Sender: TObject);
var
  LButton: TButton;
begin
  LButton := Sender as TButton;
  if Assigned(LButton) then
  begin
    // Note: We have added Description property to the TButton without altering the TButton class!
    LButton.Description := 'I was clicked at ' + DateTimeToStr(Now);

    // // We can also set Description without using class helpers
    // TButtonExtension.DescriptionProperty.SetValue(LButton, 'I was clicked at ' + DateTimeToStr(Now));
  end;
end;

initialization

TAppViewModel.ClassName;

end.

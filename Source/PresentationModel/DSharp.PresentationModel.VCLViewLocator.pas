unit DSharp.PresentationModel.VCLViewLocator;

interface

uses
  Classes,
  DSharp.PresentationModel.ViewLocator;

type
  ViewLocatorHelper = class helper for ViewLocator
    class procedure Initialize; static;
  end;

implementation

uses
  Controls,
  StdCtrls;

class procedure ViewLocatorHelper.Initialize;
begin
  ViewLocator.CreateWarningView :=
      function(Owner: TComponent; Warning: string): TComponent
    var
      LLabel: TLabel;
    begin
      LLabel := TLabel.Create(Owner);
      LLabel.Align := alClient;
      LLabel.Caption := Warning;
      Result := LLabel;
    end;
end;

initialization

ViewLocator.Initialize;

end.

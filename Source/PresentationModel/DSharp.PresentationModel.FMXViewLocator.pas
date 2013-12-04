unit DSharp.PresentationModel.FMXViewLocator;

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
  FMX.StdCtrls,
  FMX.Types;

class procedure ViewLocatorHelper.Initialize;
begin
  CreateWarningView := function(Owner: TComponent; Warning: string): TComponent
    var
      LLabel: TLabel;
    begin
      LLabel := TLabel.Create(Owner);
      LLabel.Align := TAlignLayout.alClient;
      LLabel.Text := Warning;
      Result := LLabel;
    end;
end;

initialization

ViewLocator.Initialize;

end.

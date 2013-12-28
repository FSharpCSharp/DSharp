unit AppViewForm;

interface

uses
  DSharp.Bindings.VCLControls,
  Classes,
  Controls,
  StdCtrls;

type
  TAppView = class(TForm)
    Count: TEdit;
  end;

implementation

{$R *.dfm}

initialization
  TAppView.ClassName;
end.

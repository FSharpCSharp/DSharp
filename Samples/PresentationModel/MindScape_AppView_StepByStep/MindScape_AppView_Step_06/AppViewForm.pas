unit AppViewForm;

interface

uses
  Classes,
  Controls,
  StdCtrls,
  DSharp.Bindings.VCLControls;

type
  TAppView = class(TForm)
    Count: TEdit;
  end;

implementation

{$R *.dfm}

initialization
  TAppView.ClassName;
end.

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
    IncrementCount: TButton;
    DecrementCount: TButton;
  end;

implementation

{$R *.dfm}

initialization
  TAppView.ClassName;
end.

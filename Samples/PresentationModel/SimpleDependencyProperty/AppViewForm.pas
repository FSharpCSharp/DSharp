unit AppViewForm;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  DSharp.PresentationModel,
  DSharp.Bindings.VCLControls;

type
  TAppView = class(TForm)
    ClickMe: TButton;
    Label1: TLabel;
    // Here we bind label's Caption to ClickMe button's Description dependency property
    [Binding('Caption', '{Binding ElementName=ClickMe, Path=Description}')]
    LabelDescription: TLabel;
  end;

implementation

{$R *.dfm}

initialization

TAppView.ClassName;

end.

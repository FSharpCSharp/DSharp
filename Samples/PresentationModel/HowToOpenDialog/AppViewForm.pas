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
  Actions,
  ActnList,
  DSharp.PresentationModel,
  DSharp.Bindings.VCLControls;

type
  TAppView = class(TForm)
    Button1: TButton;
    Description: TLabel;
    ActionList1: TActionList;
    ActionClose: TAction;
    Button3: TButton;
    Button2: TButton;
    ActionOpen1: TAction;
    ActionOpen2: TAction;
  end;

implementation

{$R *.dfm}

initialization

TAppView.ClassName;

end.

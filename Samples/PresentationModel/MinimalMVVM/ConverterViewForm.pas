unit ConverterViewForm;

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
  TConverterView = class(TForm)
    Label1: TLabel;
    SomeText: TEdit;
    Label2: TLabel;
    Button1: TButton;
    ActionList1: TActionList;
    ConvertText: TAction;
    History: TListBox;
  end;

implementation

{$R *.dfm}

initialization

TConverterView.ClassName;

end.

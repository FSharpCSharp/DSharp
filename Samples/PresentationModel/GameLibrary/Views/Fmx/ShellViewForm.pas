unit ShellViewForm;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.Layouts,
  FMX.StdCtrls,
  FMX.Objects,
  DSharp.Bindings.FMXControls;

type
  TShellView = class(TForm)
    Layout1: TLayout;
    Header: TLayout;
    Back: TButton;
    Label1: TLabel;
    ActiveItem: TLayout;
    BusyOverlay: TRectangle;
    BusyContent: TLayout;
    BusyAnimation: TAniIndicator;
    BusyMessage: TText;
    BusyIndicator: TLayout;
  end;

implementation

{$R *.fmx}

initialization

TShellView.ClassName;

end.

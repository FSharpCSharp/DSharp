unit zIdentitiesView;

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
  DSharp.Bindings.FMXControls;

type
  TIdentitiesView = class(TForm)
    Panel1: TPanel;
    ActionShowSelectedAccount: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Container: TLayout;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

initialization

TIdentitiesView.ClassName;

end.

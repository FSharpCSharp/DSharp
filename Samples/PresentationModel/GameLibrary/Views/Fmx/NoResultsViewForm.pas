unit NoResultsViewForm;

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
  FMX.Edit,
  FMX.Layouts,
  FMX.StdCtrls,
  DSharp.PresentationModel,
  DSharp.Bindings.FMXControls;

type
  TNoResultsView = class(TForm)
    Layout1: TLayout;
    Label1: TLabel;
    Label2: TLabel;
    AddGame: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

initialization

TNoResultsView.ClassName;

end.

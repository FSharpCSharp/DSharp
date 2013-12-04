unit ResultsViewForm;

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
  DSharp.PresentationModel,
  DSharp.Bindings.FMXControls,
  FMX.StdCtrls;

type
  TResultsView = class(TForm)
    Layout1: TLayout;
    StatusMessage: TGroupBox;
    Results: TScrollBox;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

initialization

TResultsView.ClassName;

end.

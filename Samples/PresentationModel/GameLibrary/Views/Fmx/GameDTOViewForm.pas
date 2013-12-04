unit GameDTOViewForm;

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
  FMX.Memo,
  FMX.StdCtrls,
  DSharp.PresentationModel,
  DSharp.Bindings.FMXControls;

type
  TGameDTOView = class(TForm)
    Layout1: TLayout;
    Title: TLabel;
    Layout2: TLayout;
    Label1: TLabel;
    AddedOn: TLabel;
    Label2: TLabel;
    Rating: TLabel;
    Label3: TLabel;
    Notes: TMemo;
    Layout3: TLayout;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

initialization

TGameDTOView.ClassName;

end.

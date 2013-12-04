unit SearchViewForm;

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
  TSearchView = class(TForm)
    Layout1: TLayout;
    Layout2: TLayout;
    Layout3: TLayout;
    SearchResults: TLayout;
    [Binding('OnKeyDown', '{Binding SearchTextKeyDown}')]
    SearchText: TEdit;
    ExecuteSearch: TButton;
    AddGame: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

initialization

TSearchView.ClassName;

end.

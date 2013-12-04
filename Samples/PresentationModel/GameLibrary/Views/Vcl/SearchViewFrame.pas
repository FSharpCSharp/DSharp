unit SearchViewFrame;

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
  ExtCtrls,
  DSharp.PresentationModel,
  DSharp.Bindings.VCLControls;

type
  TSearchView = class(TFrame)
    SearchResults: TPanel;
    Panel1: TPanel;
    AddGame: TButton;
    Panel2: TPanel;
    [Binding('OnKeyPress', '{Binding SearchTextKeyPress}')]
    SearchText: TEdit;
    ExecuteSearch: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
    destructor Destroy; override;
  end;

implementation

{$R *.dfm}
{ TSearchView }

destructor TSearchView.Destroy;
begin

  inherited;
end;

initialization

TSearchView.ClassName;

end.

unit zFormFirst;

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
  FMX.StdCtrls;

type
  TFormFirst = class(TForm)
    FirstContainer: TLayout;
    FirstHeader: TPanel;
    FirstFooter: TPanel;
    FirstContent: TLayout;
    Label1: TLabel;
    Label2: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  zShared,
  zFormSecond;

constructor TFormFirst.Create(AOwner: TComponent);
begin
  inherited;
  SetContentPropertyCore(FirstContent, TFormSecond.Create(Manager));
end;

destructor TFormFirst.Destroy;
begin
  Logger.LogInfo('Destroying ' + Self.Name);
  inherited;
end;

{$R *.fmx}

end.

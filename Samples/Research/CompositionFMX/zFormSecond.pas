unit zFormSecond;

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
  TFormSecond = class(TForm)
    SecondContainer: TLayout;
    SecondHeader: TPanel;
    SecondFooter: TPanel;
    Label1: TLabel;
    Label3: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
    destructor Destroy; override;
  end;

implementation

uses
  zShared;

destructor TFormSecond.Destroy;
begin
  Logger.LogInfo('Destroying ' + Self.Name);
  inherited;
end;

{$R *.fmx}

end.

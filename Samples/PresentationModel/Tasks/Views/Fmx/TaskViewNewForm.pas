unit TaskViewNewForm;

interface

uses
  System.Classes,
  System.SysUtils,
  FMX.Controls,
  FMX.Edit,
  FMX.Forms,
  FMX.Layouts,
  FMX.Types,
  FMX.StdCtrls,
  DSharp.PresentationModel,
  DSharp.Bindings.FMXControls;

type
  /// <summary>
  /// View for TaskView
  /// </summary>
  TTaskViewNew = class(TForm)
    Add: TButton;
    Bevel1: TLayout;
    [Binding('OnKeyDown', '{Binding DescriptionKeyDown}')]
    Description: TEdit;
    Container: TLayout;
    procedure DescriptionChangeTracking(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

procedure TTaskViewNew.DescriptionChangeTracking(Sender: TObject);
begin

end;

{$R *.FMX}

initialization

TTaskViewNew.ClassName;

end.

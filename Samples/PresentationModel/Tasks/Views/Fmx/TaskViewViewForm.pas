unit TaskViewViewForm;

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
  TTaskViewView = class(TForm)
    IsCompleted: TCheckBox;
    Remove: TButton;
    Spacer: TLayout;
    [Binding('OnDblClick', '{Binding DescriptionDblClick}')]
    Description: TLabel;
    Container: TLayout;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.FMX}

initialization

TTaskViewView.ClassName;

end.

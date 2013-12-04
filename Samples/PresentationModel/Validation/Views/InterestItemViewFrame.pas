unit InterestItemViewFrame;

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
  Vcl.ExtCtrls,
  DSharp.PresentationModel,
  DSharp.Bindings.VCLControls;

type
  ///	<summary>
  ///	  View for InterestView
  ///	</summary>
  TInterestItemView = class(TFrame)
    IsSelected: TCheckBox;
    DisplayName: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }

  end;

implementation

{$R *.dfm}

initialization

TInterestItemView.ClassName;

end.

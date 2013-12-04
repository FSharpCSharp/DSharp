unit InterestSelectorViewFrame;

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
  ///	  View for InterestSelectorView
  ///	</summary>
  TInterestSelectorView = class(TFrame)
    Interests: TScrollBox;
    [Binding('Caption',
      '{Binding ElementName=Interests, Path=Validation.Errors[0].ErrorContent}')
      ]
    InterestSelectorErrorIndicator: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

initialization

TInterestSelectorView.ClassName;

end.

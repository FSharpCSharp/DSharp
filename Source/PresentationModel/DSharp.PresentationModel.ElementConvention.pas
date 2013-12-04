unit DSharp.PresentationModel.ElementConvention;

interface

uses
  Classes,
  DSharp.Bindings,
  SysUtils,
  TypInfo;

type
  TBindingType = (btProperty, btEvent);

  TElementConvention = class;

  TApplyBindingProc = reference to function(AViewModel: TObject;
    APropertyName: string; AViewElement: TComponent; ABindingType: TBindingType;
    AConvention: TElementConvention): Boolean;

  ///	<summary>
  ///	  Represents the conventions for a particular element type.
  ///	</summary>
  TElementConvention = class
  private
    FApplyBinding: TApplyBindingProc;
    FEventName: string;
    FPropertyName: string;
  public
    constructor Create(APropertyName: string; AEventName: string);

    property ApplyBinding: TApplyBindingProc read FApplyBinding
      write FApplyBinding;
    property EventName: string read FEventName;
    property PropertyName: string read FPropertyName;
  end;

implementation

uses
  DSharp.PresentationModel.ConventionManager;

{ TElementConvention }

constructor TElementConvention.Create(APropertyName: string;
  AEventName: string);
begin
  FApplyBinding := ConventionManager.SetBinding;
  FPropertyName := APropertyName;
  FEventName := AEventName;
end;

end.

unit ValidatorIntf;

interface

uses
  Error,
  DSharp.PresentationModel,
  DSharp.Collections;

type

  [InheritedExport]
  IValidator = interface
    ['{892DC37C-DCE7-46EE-A926-4AA3F80D41C6}']
    function Validate(Instance: TObject): IEnumerable<TError>; overload;
    function Validate(Instance: TObject; PropertyName: string)
      : IEnumerable<TError>; overload;
  end;

implementation

end.

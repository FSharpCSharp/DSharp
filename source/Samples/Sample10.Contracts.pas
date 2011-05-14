unit Sample10.Contracts;

interface

uses
  System.ComponentModel.Composition;

type
  [InheritedExport]
  IMessage = interface
    ['{7B32CB2C-F93F-4C59-8A19-89D6F86F36F1}']
    function ToString: string;
  end;

implementation

end.

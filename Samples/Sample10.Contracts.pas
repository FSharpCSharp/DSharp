unit Sample10.Contracts;

interface

uses
  DSharp.ComponentModel.Composition;

type
  [InheritedExport]
  IMessage = interface
    ['{7B32CB2C-F93F-4C59-8A19-89D6F86F36F1}']
    function ToString: string;
  end;

implementation

{$IFDEF VER210} // see http://qc.embarcadero.com/wc/qcmain.aspx?d=85277
initialization
  if TypeInfo(IMessage) = nil then;
{$ENDIF}

end.

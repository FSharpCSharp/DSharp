unit DSharp.Core.Utils;

interface

function Supports(const Instance: TObject; const IID: TGUID; out Intf): Boolean;

implementation

function Supports(const Instance: TObject; const IID: TGUID; out Intf): Boolean;
begin
  Result := Assigned(Instance) and Instance.GetInterface(IID, Intf);
end;

end.

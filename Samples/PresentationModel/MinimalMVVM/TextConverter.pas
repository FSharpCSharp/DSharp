unit TextConverter;

interface

uses
  SysUtils;

type
  TTextConverter = class
  private
    FConvertion: TFunc<string, string>;
  public
    constructor Create(Convertion: TFunc<string, string>);
    function ConvertText(InputText: string): string;
  end;

implementation

{ TTextConverter }

function TTextConverter.ConvertText(InputText: string): string;
begin
  Result := FConvertion(InputText);
end;

constructor TTextConverter.Create(Convertion: TFunc<string, string>);
begin
  FConvertion := Convertion;
end;

end.

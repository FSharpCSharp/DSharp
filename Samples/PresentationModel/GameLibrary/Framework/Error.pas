unit Error;

interface

type
  TError = class
  private
    FErrorMessage: string;
    FInstance: TObject;
    FKey: string;
  public
    constructor Create(Instance: TObject; PropertyName: string;
      ErrorMessage: string);
    function Clone: TError;
    property ErrorMessage: string read FErrorMessage;
    property Instance: TObject read FInstance;
    property Key: string read FKey;
  end;

implementation

{ TError }

function TError.Clone: TError;
begin
  Result := TError.Create(Instance, Key, ErrorMessage);
end;

constructor TError.Create(Instance: TObject;
  PropertyName, ErrorMessage: string);
begin
  FInstance := Instance;
  FKey := PropertyName;
  FErrorMessage := ErrorMessage;
end;

end.

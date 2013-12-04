unit UserRegistrationService;

interface

uses
  Classes,
  SysUtils,
  Generics.Collections,
  UserRegistrationServiceIntf;

type
  TUserRegistrationService = class(TInterfacedObject, IUserRegistrationService)
  private
    FResultCache: TDictionary<string, boolean>;
    FLastResult: boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function IsUserNameAvailable(UserName: string): boolean;
    property ResultCache: TDictionary<string, boolean> read FResultCache;
  end;

implementation

{ TUserRegistrationService }

constructor TUserRegistrationService.Create;
begin
  FResultCache := TDictionary<string, boolean>.Create;
  FLastResult := True;
end;

destructor TUserRegistrationService.Destroy;
begin
  FResultCache.Free;
  inherited;
end;

function TUserRegistrationService.IsUserNameAvailable(UserName: string)
  : boolean;
var
  LIsNameAvailable: boolean;
begin
  if not ResultCache.TryGetValue(UserName, LIsNameAvailable) then
  begin
    FLastResult := not FLastResult;

    LIsNameAvailable := FLastResult;

    ResultCache.Add(UserName, LIsNameAvailable);
  end;

  TThread.Sleep(500);

  Result := LIsNameAvailable;
end;

initialization

TUserRegistrationService.ClassName;

end.

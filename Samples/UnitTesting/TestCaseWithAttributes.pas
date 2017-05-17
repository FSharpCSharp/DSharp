unit TestCaseWithAttributes;

interface

uses
  DSharp.Testing.DUnit;

type
  UserPasswordTestCaseAttribute = class(TestCaseAttribute)
  public
    constructor Create(const AUserName, APassword: string; AResponse: Boolean);
  end;

  UserAgeTestCaseAttribute = class(TestCaseAttribute)
  public
    constructor Create(const AUserName: string; AAge: Integer; AResponse: Boolean);
  end;

  TLoginTestCase = class(TTestCase)
  published
    [UserPasswordTestCase('User1', 'Password1', True)]
    [UserPasswordTestCase('User2', 'Password2', True)]
    [UserPasswordTestCase('User3', 'Password3', True)]
    [UserPasswordTestCase('User3', '', False)]
    procedure TestUserLogin(const AUserName, APassword: string; AResponse: Boolean);
    [UserAgeTestCase('User1', 26, True)]
    [UserAgeTestCase('User2', 27, True)]
    [UserAgeTestCase('User3', 28, False)]
    procedure TestUserAge(const AUserName: string; AAge: Integer; AResponse: Boolean);
  end;

implementation

{ UserPasswordTestCaseAttribute }

constructor UserPasswordTestCaseAttribute.Create(const AUserName, APassword: string;
  AResponse: Boolean);
begin
  SetLength(FValues, 3);
  FValues[0] := AUserName;
  FValues[1] := APassword;
  FValues[2] := AResponse;
end;

{ UserAgeTestCaseAttribute }

constructor UserAgeTestCaseAttribute.Create(const AUserName: string; AAge: Integer;
  AResponse: Boolean);
begin
  SetLength(FValues, 3);
  FValues[0] := AUserName;
  FValues[1] := AAge;
  FValues[2] := AResponse;
end;

{ TLoginTestCase }

procedure TLoginTestCase.TestUserAge(const AUserName: string; AAge: Integer;
  AResponse: Boolean);
begin

end;

procedure TLoginTestCase.TestUserLogin(const AUserName, APassword: string;
  AResponse: Boolean);
begin

end;

initialization
  TLoginTestCase.RegisterTest;

end.

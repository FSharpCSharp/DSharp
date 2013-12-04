unit PasswordMeter;

interface

uses
  Classes,
  SysUtils,
  StrUtils,
  Generics.Collections,
  PasswordMeterIntf;

type
  ///	<summary>
  ///	  Password analysis result
  ///	</summary>
  TAnalyzePasswordResult = class
  private
    FAll: Integer;
    FChars: Integer;
    FLowercase: Integer;
    FNum: Integer;
    FOcc: TDictionary<Char, Integer>;
    FRepetitions: TList<Integer>;
    FSpecial: Integer;
    FUppercase: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    property All: Integer read FAll write FAll;
    property Chars: Integer read FChars write FChars;
    property Lowercase: Integer read FLowercase write FLowercase;
    property Num: Integer read FNum write FNum;
    property Occ: TDictionary<Char, Integer> read FOcc;
    property Repetitions: TList<Integer> read FRepetitions write FRepetitions;
    property Special: Integer read FSpecial write FSpecial;
    property Uppercase: Integer read FUppercase write FUppercase;
  end;

  ///	<summary>
  ///	  Password meter settings
  ///	</summary>
  TPasswordMeterSettings = class
  private
    FCommonPasswords: TArray<string>;
    FMinLength: Integer;
  public
    constructor Create;

    ///	<summary>
    ///	  Common passwords passwords black list
    ///	</summary>
    property CommonPasswords: TArray<string> read FCommonPasswords
      write FCommonPasswords;

    ///	<summary>
    ///	  Minimum password length
    ///	</summary>
    property MinLength: Integer read FMinLength write FMinLength;
  end;

  ///	<summary>
  ///	  Implementation of <see cref="IPasswordMeter" />
  ///	</summary>
  TPasswordMeter = class(TInterfacedObject, IPasswordMeter)
  class var
    FSettings: TPasswordMeterSettings;
    class function AnalyzePassword(Password: string): TAnalyzePasswordResult;
  public
    class constructor Create;
    class destructor Destroy;

    ///	<summary>
    ///	  Calculate password strength
    ///	</summary>
    ///	<returns>
    ///	  TPasswordStrengthResult
    ///	</returns>
    ///	<param name="Password">
    ///	  New password
    ///	</param>
    ///	<param name="Username">
    ///	  Username (optional)
    ///	</param>
    ///	<param name="OldPassword">
    ///	  Old password (optional)
    ///	</param>
    function PasswordStrength(Password: string; Username: string = '';
      OldPassword: string = ''): TPasswordStrengthResult;

    ///	<summary>
    ///	  Default Settings
    ///	</summary>
    class property Settings: TPasswordMeterSettings read FSettings;
  end;

resourcestring
  SAtLeastEightCharactersRequired = 'At least 8 characters are required.';
  SCharacterAppearsTooManyTimesInARow =
    'Character appears too many times in a row.';
  SCharacterAppearsTooOften = 'Character appears too often.';
  SForMoreSecurePasswordAddDigit = 'For a more secure password add a digit.';
  SForMoreSecurePasswordAddLetter = 'For a more secure password add a letter.';
  SForMoreSecurePasswordAddSpecialCharacter =
    'For a more secure password add the special character (npr.: "!", ";", ...).';
  SForMoreSecurePasswordUseSmallAndCapitalLetters =
    'For a more secure password use small and large letters.';
  SNewPasswordContainsOldPassword = 'New password contains the old password.';
  SNewPasswordContainsUsername = 'New password contains a user name.';
  SNewPasswordIsTheSameAsOldPassword =
    'The new password is the same as the old password.';
  SNewPasswordIsTheSameAsUsername =
    'The new password is the same as the user name.';
  SPasswordAppearsOnAListOfCommonPasswords =
    'The password is on the list of common passwords.';

implementation

{ TPasswordMeter }

class constructor TPasswordMeter.Create;
begin
  FSettings := TPasswordMeterSettings.Create;
end;

class destructor TPasswordMeter.Destroy;
begin
  FSettings.Free;
end;

class function TPasswordMeter.AnalyzePassword(Password: string)
  : TAnalyzePasswordResult;
var
  Chr: Char;
  Last: string;
  Repetition: Integer;
begin
  Result := TAnalyzePasswordResult.Create;
  Result.All := Password.Length;

  if Password.Length < Settings.MinLength then
    Exit;

  Last := 'aa';
  Repetition := 0;

  for Chr in Password do
  begin
    if Result.Occ.ContainsKey(Chr) then
      Result.Occ[Chr] := Result.Occ[Chr] + 1
    else
      Result.Occ.Add(Chr, 1);

    if Chr = Last then
      Inc(Repetition)
    else
    begin
      Last := Chr;
      if Repetition > 0 then
        Result.Repetitions.Add(Repetition + 1);
      Repetition := 0;
    end;

    if (Chr >= 'a') and (Chr <= 'z') then
    begin
      Result.Chars := Result.Chars + 1;
      Result.Lowercase := Result.Lowercase + 1;
    end
    else if (Chr >= 'A') and (Chr <= 'Z') then
    begin
      Result.Chars := Result.Chars + 1;
      Result.Uppercase := Result.Uppercase + 1;
    end
    else if (Chr >= '0') and (Chr <= '9') then
    begin
      Result.Num := Result.Num + 1;
    end
    else
    begin
      Result.Special := Result.Special + 1;
    end
  end;

  if Repetition > 0 then
  begin
    Result.Repetitions.Add(Repetition + 1);
  end;
end;

function TPasswordMeter.PasswordStrength(Password: string; Username: string;
  OldPassword: string): TPasswordStrengthResult;
var
  Analysis: TAnalyzePasswordResult;
  Chr: Char;
  Hint: Integer;
  Hints: TStringList;
  i: Integer;
  MaxOcc: Integer;
  Occ: Integer;
  Reps: Integer;
  Score: Double;
begin
  if (Password.Length < Settings.MinLength) then
  begin
    Result.Strength := TPasswordStrengthEnum.TooShort;
    Result.Hints := SAtLeastEightCharactersRequired;
    Exit;
  end;

  Hints := TStringList.Create;
  try
    Analysis := AnalyzePassword(Password);
    try
      Score := Analysis.All * 12.5;
      Score := Score + Analysis.Special * 15;

      if IndexText(Password, Settings.CommonPasswords) >= 0 then
      begin
        Hints.Add(SPasswordAppearsOnAListOfCommonPasswords);
        Score := Score - 10000;
      end;

      if (Username.Length > 0) and (AnsiCompareText(Password, Username) = 0)
      then
      begin
        Hints.Add(SNewPasswordIsTheSameAsUsername);
        Score := Score - 10000;
      end
      else if (Username.Length > 0) and AnsiContainsText(Password, Username)
      then
      begin
        Hints.Add(SNewPasswordContainsUsername);
        Score := Score - 100;
      end;

      if (OldPassword.Length > 0) and
        (AnsiCompareText(Password, OldPassword) = 0) then
      begin
        Hints.Add(SNewPasswordIsTheSameAsOldPassword);
        Score := Score - 10000;
      end
      else if (OldPassword.Length > 0) and AnsiContainsText(Password,
        OldPassword) then
      begin
        Hints.Add(SNewPasswordContainsOldPassword);
        Score := Score - 100;
      end;

      if Analysis.Chars = 0 then
      begin
        Hints.Add(SForMoreSecurePasswordAddLetter);
        Score := Score - 50;
      end
      else if (Analysis.Lowercase = 0) or (Analysis.Uppercase = 0) then
      begin
        Hints.Add(SForMoreSecurePasswordUseSmallAndCapitalLetters);
        Score := Score - 50;
      end;

      if Analysis.Num = 0 then
      begin
        Hints.Add(SForMoreSecurePasswordAddDigit);
        Score := Score - 50;
      end;

      if Analysis.Special = 0 then
      begin
        Hints.Add(SForMoreSecurePasswordAddSpecialCharacter);
        Score := Score - 50;
      end;

      if Analysis.Repetitions.Count > 0 then
      begin
        Hint := 0;
        for i := 0 to Analysis.Repetitions.Count - 1 do
        begin
          Reps := Analysis.Repetitions[i];
          if Reps > 2 then
          begin
            Score := Score - Reps * Reps * 5;
            Hint := 1;
          end
        end;
        if Hint > 0 then
        begin
          Hints.Add(SCharacterAppearsTooManyTimesInARow);
        end
      end;

      if Analysis.Occ.Count > 0 then
      begin
        Hint := 0;
        MaxOcc := 3;
        for Chr in Analysis.Occ.Keys do
        begin
          Occ := Analysis.Occ[Chr];
          if Occ > MaxOcc then
          begin
            Score := Score - Occ * Occ * Occ;
            Hint := 1;
          end
        end;
        if Hint > 0 then
        begin
          Hints.Add(SCharacterAppearsTooOften);
        end
      end;

      if (Score < 0) then
      begin
        Result.Strength := TPasswordStrengthEnum.Bad;
      end
      else if (Score >= 0) and (Score < 60) then
      begin
        Result.Strength := TPasswordStrengthEnum.Weak;
      end
      else if (Score >= 60) and (Score < 140) then
      begin
        Result.Strength := TPasswordStrengthEnum.Good;
      end
      else
      begin
        Result.Strength := TPasswordStrengthEnum.Strong;
      end;

      Result.Score := Score;
      Result.Hints := Hints.Text;
    finally
      Analysis.Free;
    end;
  finally
    Hints.Free;
  end;
end;

{ TPasswordMeter.TAnalyzePasswordResult }

constructor TAnalyzePasswordResult.Create;
begin
  FChars := 0;
  FLowercase := 0;
  FUppercase := 0;
  FNum := 0;
  FSpecial := 0;
  FAll := 0;
  FOcc := TDictionary<Char, Integer>.Create;
  FRepetitions := TList<Integer>.Create;
end;

destructor TAnalyzePasswordResult.Destroy;
begin
  FRepetitions.Free;
  FOcc.Free;
  inherited;
end;

{ TPasswordMeter.TPasswordMeterSettings }

constructor TPasswordMeterSettings.Create;
begin
  FMinLength := 6;
  FCommonPasswords := TArray<string>.Create('asdfasdf', '12341234', 'asdf1234',
    '1234asdf', '123456781', '12345678!', '12345678', '123456789', '1234567890',
    'password');
end;

initialization

TPasswordMeter.ClassName;

end.

unit PasswordMeterIntf;

interface

uses
  DSharp.PresentationModel;

type
  {$SCOPEDENUMS ON}
  TPasswordStrengthEnum = (TooShort, Bad, Weak, Good, Strong);
  {$SCOPEDENUMS OFF}

  ///	<summary>
  ///	  Password strength result
  ///	</summary>
  TPasswordStrengthResult = record
    Hints: string;
    Score: Double;
    Strength: TPasswordStrengthEnum;
  end;

  ///	<summary>
  ///	  Password meter implementation
  ///	</summary>
  [InheritedExport]
  IPasswordMeter = interface
    ['{FCA1E917-1A2A-4A02-8436-52EBAE29686C}']

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
  end;

implementation

end.

unit DSharp.Validation;

interface

uses
  Rtti,
  SysUtils,
  TimeSpan,
  Generics.Defaults,
  DSharp.Collections,
  DSharp.Core.EventArgs,
  DSharp.Core.Events,
  DSharp.Validation.ValidationError,
  DSharp.Validation.RuleResult;

type
  {$REGION 'Redefined types from DSharp.Validation.RuleResult'}
  TRuleResult = DSharp.Validation.RuleResult.TRuleResult;
  {$ENDREGION}

  ///	<summary>
  ///	  Represents an exception that occurs during validation
  ///	</summary>
  EValidationException = class(Exception);

  IValidationTarget = interface
    ['{7DD3C86F-868B-43CB-8EAE-ADD730B0F246}']
    function IsMatch(const Target: TValue): Boolean;
    function UnwrapTargets: TArray<TValue>;
  end;

  ///	<summary>
  ///	  Represents a validation rule.
  ///	</summary>

  IValidationRule = interface
    ['{C4B83435-291B-4636-88B0-65477C94DB81}']
    function Evaluate: TRuleResult;
    procedure EvaluateAsync(Completed: TProc<TRuleResult>);
    function GetSupportsSyncValidation: Boolean;
    function GetTarget: IValidationTarget;
    property SupportsSyncValidation: Boolean read GetSupportsSyncValidation;
    property Target: IValidationTarget read GetTarget;
  end;

  ///	<summary>
  ///	  Represents an asynchronious validation rule.
  ///	</summary>

  IAsyncValidationRule = interface(IValidationRule)
    ['{33A244B4-0B38-4108-BA92-36131123B9AF}']
  end;

  /// <summary>
  /// Represents a collection of <see cref="TValidationError"/> records.
  /// </summary>
  // IList<TValidationError> = IList<TValidationError>;

  IValidationResultFormatter = interface;

  ///	<summary>
  ///	  Encapsulates result of a validation. Contains a boolean
  ///	  <see cref="IsValid" /> and a collection of errors
  ///	  <see cref="ErrorList" />.
  ///	</summary>

  IValidationResult = interface
    ['{C8364EFB-DADA-48F8-893E-17FEFB80253C}']

    {$REGION 'Property Accessors'}
    function GetError(const Target: TValue): string;
    function GetErrorList: IList<TValidationError>;
    function GetIsValid: Boolean;
    {$ENDREGION}
    procedure AddError(const Target: TValue; Error: string); overload;

    ///	<summary>
    ///	  Returns a <see cref="string" /> that represents this instance.
    ///	</summary>
    ///	<returns>
    ///	  A <see cref="string" /> that represents this instance.
    ///	</returns>
    function ToString: string; overload;

    ///	<summary>
    ///	  Formats this instance to a string using given
    ///	  <see cref="IValidationResultFormatter" />.
    ///	</summary>
    ///	<param name="Formatter">
    ///	  The formatter that can format the validation result.
    ///	</param>
    ///	<returns>
    ///	  A string that represents this validation result.
    ///	</returns>
    function ToString(Formatter: IValidationResultFormatter): string; overload;

    ///	<summary>
    ///	  Gets an error by <paramref name="Target" />.
    ///	</summary>
    property Error[const Target: TValue]: string read GetError; default;

    ///	<summary>
    ///	  Gets the list of errors if any. If valid, returns an empty
    ///	  collection.
    ///	</summary>
    property ErrorList: IList<TValidationError> read GetErrorList;

    ///	<summary>
    ///	  Gets a value indicating whether the validation was sucessful. If not,
    ///	  see <see cref="ErrorList" /> for the list of errors.
    ///	</summary>
    property IsValid: Boolean read GetIsValid;
  end;

  ///	<summary>
  ///	  Represents a formatter that can be used to format an instance of
  ///	  <see cref="IValidationResult" /> to a string.
  ///	</summary>

  IValidationResultFormatter = interface
    ['{FE9B4553-7E80-4B60-9DFD-03CFCF1B2549}']

    ///	<summary>
    ///	  Converts the specified validation result object to a string.
    ///	</summary>
    ///	<param name="ValidationResult">
    ///	  The validation result to format.
    ///	</param>
    ///	<returns>
    ///	  A string representation of <paramref name="ValidationResult" />
    ///	</returns>
    function Format(ValidationResult: IValidationResult): string;
  end;

  ///	<summary>
  ///	  Contains arguments for the <see cref="ValidationHelper.ResultChanged" />
  ///	  event.
  ///	</summary>

  IValidationResultChangedEventArgs = interface(IEventArgs)
    ['{A2272EE4-0284-409D-A54C-7983AB5F6D5B}']

    {$REGION 'Property Accessors'}
    function GetNewResult: IValidationResult;
    function GetTarget: TValue;
    {$ENDREGION}

    ///	<summary>
    ///	  Gets the new validation result.
    ///	</summary>
    property NewResult: IValidationResult read GetNewResult;

    ///	<summary>
    ///	  Gets the target, for which the validation result has changed.
    ///	</summary>
    property Target: TValue read GetTarget;
  end;

  TValidationResultChangedEvent =
    TEventHandler<IValidationResultChangedEventArgs>;

  ///	<summary>
  ///	  Represents a method that takes a callback method for setting rule
  ///	  validation result as a parameter.
  ///	</summary>
  ///	<param name="ResultCallback">
  ///	  A continuation callback that should be called when the rule validation
  ///	  result is available.
  ///	</param>
  TAsyncRuleValidateAction = reference to procedure(ResultCallback
    : TProc<TRuleResult>);

implementation

end.

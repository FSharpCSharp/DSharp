unit DSharp.Validation.ValidationHelper;

interface

uses
  Rtti,
  Spring,
  SysUtils,
  DSharp.Core.Validations,
  DSharp.Validation.ValidationRule,
  Generics.Defaults,
  Generics.Collections,
  DSharp.Collections,
  DSharp.Validation,
  DSharp.Core.Events,
  DSharp.Core.Reflection,
  DSharp.Core.Threading;

type
  ///	<summary>
  ///	  Main helper class that contains the functionality of managing
  ///	  validation rules, executing validation using those rules and keeping
  ///	  validation results.
  ///	</summary>
  TValidationHelper = class
  private
    FAsyncRuleExecutionTimeout: TTimeSpan;
    FIsValidationSuspended: Boolean;
    FOwner: TObject;
    FResultChanged: Event<TValidationResultChangedEvent>;
    FRuleValidationResultMap
      : TDictionary<TValue, TDictionary<IValidationRule, TRuleResult>>;
    FSyncRoot: TObject;
    FValidationRules: IList<IValidationRule>;
    class procedure AddErrorsFromRuleResult(ResultToAddTo: IValidationResult;
      ValidationRule: IValidationRule; RuleResult: TRuleResult);
    function AddRuleCore(Target: IValidationTarget;
      ValidateDelegate: TFunc<TRuleResult>;
      AsyncValidateAction: TAsyncRuleValidateAction): IAsyncValidationRule;
    class function CreatePropertyValidationTarget(const Properties
      : array of string): IValidationTarget;
    function ExecuteRuleCore(Rule: IValidationRule): TRuleResult;
    function ExecuteValidationRules(const RulesToExecute
      : IEnumerable<IValidationRule>): IValidationResult;
    function GetAsyncRuleExecutionTimeout: TTimeSpan;
    class function GetCurrentValidationResultForRule(const RuleMap
      : TDictionary<IValidationRule, TRuleResult>; const Rule: IValidationRule)
      : TRuleResult;
    function GetResultChanged: IEvent<TValidationResultChangedEvent>;
    function GetResultInternal: IValidationResult; overload;
    function GetResultInternal(const Target: TValue)
      : IValidationResult; overload;
    function GetRuleMapForTarget(const Target: TValue)
      : TDictionary<IValidationRule, TRuleResult>;
    function GetRulesForTarget(const Target: TValue): IList<IValidationRule>;
    procedure ImportDataAnnotationRules;
    procedure RegisterValidationRule(Rule: IValidationRule);
    procedure SaveRuleValidationResultAndNotifyIfNeeded(Rule: IValidationRule;
      const RuleResult: TRuleResult);
    procedure SetAsyncRuleExecutionTimeout(const Value: TTimeSpan);
    procedure UnregisterAllValidationRules;
    procedure UnregisterValidationRule(Rule: IValidationRule);
    function ValidateInternal(const Target: TValue): IValidationResult;
    function ValidateInternalAsync(const Target: TValue)
      : ITask<IValidationResult>;
    function ExecuteValidationRulesAsync(Target: TValue)
      : ITask<IValidationResult>;
  public
    ///	<summary>
    ///	  Initializes a new instance of the <see cref="TValidationHelper" />clas
    ///	   s.
    ///	</summary>
    constructor Create(Owner: TObject);
    destructor Destroy; override;

    ///	<summary>
    ///	  Adds an asynchronious validation rule that validates a property of an
    ///	  object. The target property is specified in the
    ///	  <paramref name="PropertyName" />parameter.
    ///	</summary>
    ///	<param name="PropertyName">
    ///	  The target property name. Example: AddAsyncRule('MyProperty', ...).
    ///	</param>
    ///	<param name="ValidateAction">
    ///	  The validation delegate - a function that performs asyncrhonious
    ///	  validation and calls a continuation callback with an instance of
    ///	  <see cref="TRuleResult" /> that indicated whether the rule has passed
    ///	  and a collection of errors (in not passed).
    ///	</param>
    ///	<example>
    ///	  <code>
    ///	 AddAsyncRule('Foo',
    ///	 procedure (OnCompleted)
    ///	 begin
    ///	 ValidationServiceFacade.ValidateFoo(Foo, result =&gt; OnCompleted(RuleResult.Assert(Result.IsValid, 'Foo must be greater than 10')));
    ///	 end);
    ///	 </code>
    ///	</example>
    ///	<returns>
    ///	  An instance of <see cref="IAsyncValidationRule" /> that represents
    ///	  the newly created validation rule.
    ///	</returns>
    function AddAsyncRule(PropertyName: string;
      ValidateAction: TAsyncRuleValidateAction): IAsyncValidationRule; overload;

    ///	<summary>
    ///	  Adds an asynchronious validation rule that validates a collection of
    ///	  dependent properties.
    ///	</summary>
    ///	<param name="Properties">
    ///	  The collection of target property expressions. Example:
    ///	  AddAsyncRule(['MyProperty1', 'MyProperty2', 'MyProperty'], ...).
    ///	</param>
    ///	<param name="ValidateAction">
    ///	  The validation delegate - a function that performs asynchronous
    ///	  validation and calls a continuation callback with an instance of
    ///	  <see cref="TRuleResult" /> that indicated whether the rule has passed
    ///	  and a collection of errors (in not passed).
    ///	</param>
    ///	<returns>
    ///	  An instance of <see cref="IAsyncValidationRule" /> that represents
    ///	  the newly created validation rule.
    ///	</returns>
    function AddAsyncRule(const Properties: array of string;
      ValidateAction: TAsyncRuleValidateAction): IAsyncValidationRule; overload;

    ///	<summary>
    ///	  Adds an asynchronious validation rule.
    ///	</summary>
    ///	<param name="ValidateAction">
    ///	  The validation delegate - a function that performs asynchronous
    ///	  validation and calls a continuation callback with an instance of
    ///	  <see cref="TRuleResult" /> that indicated whether the rule has passed
    ///	  and a collection of errors (in not passed).
    ///	</param>
    ///	<returns>
    ///	  An instance of <see cref="IAsyncValidationRule" /> that represents
    ///	  the newly created validation rule.
    ///	</returns>
    function AddAsyncRule(ValidateAction: TAsyncRuleValidateAction)
      : IAsyncValidationRule; overload;

    ///	<summary>
    ///	  Adds an asynchronious validation rule that validates the
    ///	  <paramref name="Target" /> object.
    ///	</summary>
    ///	<param name="Target">
    ///	  The validation target (object that is being validated by
    ///	  <paramref name="ValidateAction" />).
    ///	</param>
    ///	<param name="ValidateAction">
    ///	  The validation delegate - a function that performs asynchronous
    ///	  validation and calls a continuation callback with an instance of
    ///	  <see cref="TRuleResult" /> that indicated whether the rule has passed
    ///	  and a collection of errors (in not passed).
    ///	</param>
    ///	<returns>
    ///	  An instance of <see cref="IAsyncValidationRule" /> that represents
    ///	  the newly created validation rule.
    ///	</returns>
    function AddAsyncRule(Target: TValue;
      ValidateAction: TAsyncRuleValidateAction): IAsyncValidationRule; overload;

    ///	<summary>
    ///	  Creates a validation rule that validates the specified child
    ///	  <see cref="IValidatable" /> object and adds errors to this object if
    ///	  invalid.
    ///	</summary>
    ///	<param name="validator">
    ///	  An instance of <see cref="ValidationHelper" /> that is used for
    ///	  validation.
    ///	</param>
    ///	<param name="childValidatableGetter">
    ///	  Expression for getting the <see cref="IValidatable" /> object to add
    ///	  as child.
    ///	</param>
    ///	<returns>
    ///	  An instance of <see cref="IValidationRule" /> that represents the
    ///	  newly created validation rule.
    ///	</returns>
    class function AddChildValidatable(PropertyName: string)
      : IAsyncValidationRule; static;

    ///	<summary>
    ///	  Adds a rule that checks that the property represented by
    ///	  <paramref name="PropertyName" /> is not empty string.
    ///	</summary>
    ///	<param name="PropertyName">
    ///	  Specifies the property to validate. Example: Validate('MyProperty').
    ///	</param>
    ///	<param name="ErrorMessage">
    ///	  Error message in case if the property is null or empty.
    ///	</param>
    ///	<returns>
    ///	  An instance of <see cref="IValidationRule" /> that represents the
    ///	  newly created validation rule.
    ///	</returns>
    function AddRequiredRule(PropertyName: string; ErrorMessage: string)
      : IValidationRule;

    ///	<summary>
    ///	  Adds a validation rule that validates a property of an object. The
    ///	  target property is specified in the <paramref name="PropertyName" />pa
    ///	   rameter.
    ///	</summary>
    ///	<param name="PropertyName">
    ///	  The target property name. Example: AddRule('MyProperty', ...).
    ///	</param>
    ///	<param name="ValidateDelegate">
    ///	  The validation delegate - a function that returns an instance of
    ///	  <see cref="TRuleResult" /> that indicated whether the rule has passed
    ///	  and a collection of errors (in not passed).
    ///	</param>
    ///	<returns>
    ///	  An instance of <see cref="IValidationRule" /> that represents the
    ///	  newly created validation rule.
    ///	</returns>
    ///	<example>
    ///	  <code>
    ///	 AddRule('Foo', TRuleResult.Assert(Foo &gt; 10, 'Foo must be greater than 10'))
    ///	 </code>
    ///	</example>
    function AddRule(PropertyName: string; ValidateDelegate: TFunc<TRuleResult>)
      : IValidationRule; overload;

    ///	<summary>
    ///	  Adds a validation rule that validates a collection of dependent
    ///	  properties.
    ///	</summary>
    ///	<param name="Properties">
    ///	  The collection of target property names. Example:
    ///	  AddRule(['MyProperty1', 'MyProperty2', 'MyProperty3'], ...).
    ///	</param>
    ///	<param name="ValidateDelegate">
    ///	  The validation delegate - a function that returns an instance of
    ///	  <see cref="TRuleResult" /> that indicated whether the rule has passed
    ///	  and a collection of errors (in not passed).
    ///	</param>
    ///	<returns>
    ///	  An instance of <see cref="IValidationRule" /> that represents the
    ///	  newly created validation rule.
    ///	</returns>
    function AddRule(const Properties: array of string;
      ValidateDelegate: TFunc<TRuleResult>): IValidationRule; overload;

    ///	<summary>
    ///	  Adds a simple validation rule.
    ///	</summary>
    ///	<param name="ValidateDelegate">
    ///	  The validation delegate - a function that returns an instance of
    ///	  <see cref="TRuleResult" /> that indicated whether the rule has passed
    ///	  and a collection of errors (in not passed).
    ///	</param>
    ///	<returns>
    ///	  An instance of <see cref="IValidationRule" /> that represents the
    ///	  newly created validation rule.
    ///	</returns>
    function AddRule(ValidateDelegate: TFunc<TRuleResult>)
      : IValidationRule; overload;

    ///	<summary>
    ///	  Adds a validation rule that validates the <paramref name="Target" />ob
    ///	   ject.
    ///	</summary>
    ///	<param name="Target">
    ///	  The validation target (object that is being validated by
    ///	  <paramref name="validateDelegate" />).
    ///	</param>
    ///	<param name="ValidateDelegate">
    ///	  The validation delegate - a function that returns an instance of
    ///	  <see cref="TRuleResult" /> that indicated whether the rule has passed
    ///	  and a collection of errors (in not passed).
    ///	</param>
    ///	<returns>
    ///	  An instance of <see cref="IValidationRule" /> that represents the
    ///	  newly created validation rule.
    ///	</returns>
    function AddRule(Target: TValue; ValidateDelegate: TFunc<TRuleResult>)
      : IValidationRule; overload;

    ///	<summary>
    ///	  Disables all the calls to the Validate* methods
    ///	</summary>
    ///	<remarks>
    ///	  This method is convenient to use when you want to suppress validation
    ///	  when setting initial value to a property. In this case you would wrap
    ///	  the code that sets the property into a <c>try finally</c> block. Like
    ///	  this:
    ///	  <code>
    ///	 Validation.DisableValidation;
    ///	 try
    ///	 MyProperty = "Initial Value";
    ///	 finally
    ///	 Validation.EnableValidation;
    ///	 end;
    ///	 </code>
    ///	</remarks>
    procedure DisableValidation;

    ///	<summary>
    ///	  Enables validation
    ///	</summary>
    procedure EnableValidation;

    ///	<summary>
    ///	  Returns the current validation state (all errors tracked by this
    ///	  instance of <see cref="TValidationHelper" />).
    ///	</summary>
    ///	<returns>
    ///	  An instance of <see cref="IValidationResult" /> that contains an
    ///	  indication whether the object is valid and a collection of errors if
    ///	  not.
    ///	</returns>
    function GetResult: IValidationResult; overload;

    ///	<summary>
    ///	  Returns the current validation state for a property represented by
    ///	  <paramref name="PropertyName" /> (all errors tracked by this instance
    ///	  of <see cref="ValidationHelper" />).
    ///	</summary>
    ///	<param name="PropertyName">
    ///	  The property for which to retrieve the validation state. Example:
    ///	  GetResult(() =&gt; MyProperty)
    ///	</param>
    ///	<returns>
    ///	  An instance of <see cref="IValidationResult" /> that contains an
    ///	  indication whether the object is valid and a collection of errors if
    ///	  not.
    ///	</returns>
    function GetResult(PropertyName: string): IValidationResult; overload;

    ///	<summary>
    ///	  Returns the current validation state for the given
    ///	  <paramref name="Target" /> (all errors tracked by this instance of
    ///	  <see cref="ValidationHelper" />) .
    ///	</summary>
    ///	<param name="Target">
    ///	  The validation target for which to retrieve the validation state.
    ///	</param>
    ///	<returns>
    ///	  An instance of <see cref="IValidationResult" /> that contains an
    ///	  indication whether the object is valid and a collection of errors if
    ///	  not.
    ///	</returns>
    function GetResult(Target: TValue): IValidationResult; overload;

    ///	<summary>
    ///	  Removes all validation rules.
    ///	</summary>
    procedure RemoveAllRules;

    ///	<summary>
    ///	  Removes the specified <paramref name="Rule" />.
    ///	</summary>
    ///	<param name="Rule">
    ///	  Validation rule instance.
    ///	</param>
    procedure RemoveRule(Rule: IValidationRule);

    ///	<summary>
    ///	  Validates (executes validation rules) the property specified by the
    ///	  <paramref name="PropertyName" /> parameter.
    ///	</summary>
    ///	<param name="PropertyName">
    ///	  Specifies the property to validate. Example: Validate('MyProperty').
    ///	</param>
    ///	<returns>
    ///	  Result that indicates whether the given property is valid and a
    ///	  collection of errors, if not valid.
    ///	</returns>
    function Validate(PropertyName: string): IValidationResult; overload;

    ///	<summary>
    ///	  Validates (executes validation rules) the specified target.
    ///	</summary>
    ///	<param name="Target">
    ///	  The target object to validate.
    ///	</param>
    ///	<returns>
    ///	  Result that indicates whether the given target object is valid and a
    ///	  collection of errors, if not valid.
    ///	</returns>
    function Validate(const Target: TValue): IValidationResult; overload;

    ///	<summary>
    ///	  Executes validation using all validation rules.
    ///	</summary>
    ///	<returns>
    ///	  Result that indicates whether the validation was succesfull and a
    ///	  collection of errors, if it wasn't.
    ///	</returns>
    function ValidateAll: IValidationResult;

    ///	<summary>
    ///	  Executes validation using all validation rules asynchronously.
    ///	</summary>
    ///	<returns>
    ///	  Task that represents the validation operation.
    ///	</returns>
    function ValidateAllAsync: ITask<IValidationResult>; overload;

    ///	<summary>
    ///	  Executes validation for the given property asynchronously. Executes
    ///	  all (normal and async) validation rules for the property specified in
    ///	  the <paramref name="propertyPathExpression" />.
    ///	</summary>
    ///	<param name="PropertyName">
    ///	  Property to validate. Example: ValidateAsync('MyProperty').
    ///	</param>
    ///	<returns>
    ///	  Task that represents the validation operation.
    ///	</returns>
    function ValidateAsync(PropertyName: string)
      : ITask<IValidationResult>; overload;

    ///	<summary>
    ///	  Executes validation for the given target asynchronously. Executes all
    ///	  (normal and async) validation rules for the target object specified
    ///	  in the <paramref name="Target" />.
    ///	</summary>
    ///	<param name="Target">
    ///	  The target object to validate.
    ///	</param>
    ///	<returns>
    ///	  Task that represents the validation operation.
    ///	</returns>
    function ValidateAsync(const Target: TValue)
      : ITask<IValidationResult>; overload;

    ///	<summary>
    ///	  Gets or sets a timeout that indicates how much time is allocated for
    ///	  an async rule to complete. If a rule did not complete in this
    ///	  timeout, then an exception will be thrown.
    ///	</summary>
    property AsyncRuleExecutionTimeout: TTimeSpan
      read GetAsyncRuleExecutionTimeout write SetAsyncRuleExecutionTimeout;

    ///	<summary>
    ///	  Occurs when the validation result have changed for a property or for
    ///	  the entire entity (the result that is returned by the
    ///	  <see cref="GetResult()" />method).
    ///	</summary>
    property ResultChanged: IEvent<TValidationResultChangedEvent>
      read GetResultChanged;
  end;

implementation

uses
  TypInfo,
  Windows,
  DSharp.ComponentModel.DataAnnotations,
  DSharp.Validation.UndefinedValidationTarget,
  DSharp.Validation.GenericValidationTarget,
  DSharp.Validation.PropertyValidationTarget,
  DSharp.Validation.PropertyCollectionValidationTarget,
  DSharp.Validation.ValidationResult,
  DSharp.Validation.ValidationError,
  DSharp.Validation.ValidationResultChangedEventArgs,
  DSharp.Validation.DataAnnotationValidationRule,
  DSharp.Validation.ValidationResultExtensions,
  DSharp.Validation.RuleResult,
  DSharp.PresentationModel.Execute,
  SyncObjs;

constructor TValidationHelper.Create(Owner: TObject);
begin
  FOwner := Owner;
  FValidationRules := TList<IValidationRule>.Create;
  FRuleValidationResultMap := TObjectDictionary < TValue,
    TDictionary < IValidationRule, TRuleResult >>.Create([doOwnsValues],
    TEqualityComparer<TValue>.Construct(
    // Compare function
    function(const Left, Right: TValue): Boolean
    begin
      Result := SameValue(Left, Right);
    end,
  // Hash function
    function(const Value: TValue): Integer
    begin
      if Value.IsEmpty then
        Result := 0
      else if Value.IsObject then
        Result := Value.AsObject.GetHashCode
      else if Value.IsString then
        // Result := Value.AsString.GetHashCode
        Result := BobJenkinsHash(PChar(Value.AsString)^,
          ByteLength(Value.AsString), 0)
      else
        raise ENotImplemented.Create('Implement hash function!');
    end));

  FSyncRoot := TObject.Create;
  FIsValidationSuspended := False;
  AsyncRuleExecutionTimeout := TTimeSpan.FromSeconds(30);

  ImportDataAnnotationRules;
end;

destructor TValidationHelper.Destroy;
begin
  FValidationRules := nil;
  FRuleValidationResultMap.Free;
  FSyncRoot.Free;
  inherited;
end;

function TValidationHelper.AddAsyncRule(PropertyName: string;
ValidateAction: TAsyncRuleValidateAction): IAsyncValidationRule;
begin
  Guard.CheckTrue(Length(PropertyName) > 0, 'PropertyName');
  Guard.CheckTrue(Assigned(ValidateAction), 'ValidateAction');

  Result := AddAsyncRule([PropertyName], ValidateAction);
end;

function TValidationHelper.AddAsyncRule(const Properties: array of string;
ValidateAction: TAsyncRuleValidateAction): IAsyncValidationRule;
var
  LTarget: IValidationTarget;
begin
  Guard.CheckTrue(Length(Properties) > 0, 'Properties');
  Guard.CheckTrue(Assigned(ValidateAction), 'ValidateAction');

  LTarget := CreatePropertyValidationTarget(Properties);

  Result := AddRuleCore(LTarget, nil, ValidateAction);
end;

function TValidationHelper.AddAsyncRule(ValidateAction
  : TAsyncRuleValidateAction): IAsyncValidationRule;
begin
  Guard.CheckTrue(Assigned(ValidateAction), 'ValidateAction');

  Result := AddRuleCore(TUndefinedValidationTarget.Create, nil, ValidateAction);
end;

function TValidationHelper.AddAsyncRule(Target: TValue;
ValidateAction: TAsyncRuleValidateAction): IAsyncValidationRule;
begin
  Guard.CheckTrue(not Target.IsEmpty, 'Target');
  Guard.CheckTrue(Assigned(ValidateAction), 'ValidateAction');

  Result := AddRuleCore(TGenericValidationTarget.Create(Target), nil,
    ValidateAction);
end;

class function TValidationHelper.AddChildValidatable(PropertyName: string)
  : IAsyncValidationRule;
begin
  Result := nil;
  // TODO -cMM: TValidationHelper.AddChildValidatable default body inserted
end;

class procedure TValidationHelper.AddErrorsFromRuleResult
  (ResultToAddTo: IValidationResult; ValidationRule: IValidationRule;
RuleResult: TRuleResult);
var
  LErrorTarget: TValue;
  LRuleError: string;
begin
  if not RuleResult.IsValid then
  begin
    for LErrorTarget in ValidationRule.Target.UnwrapTargets do
    begin
      for LRuleError in RuleResult.Errors do
      begin
        ResultToAddTo.AddError(LErrorTarget, LRuleError);
      end;
    end;
  end;
end;

function TValidationHelper.AddRequiredRule(PropertyName, ErrorMessage: string)
  : IValidationRule;
begin
  Guard.CheckTrue(Length(PropertyName) > 0, 'PropertyName');
  Guard.CheckTrue(Length(ErrorMessage) > 0, 'ErrorMessage');

  Result := AddRule(PropertyName,
    function: TRuleResult
    var
      LPropertyValue: string;
    begin
      LPropertyValue := FOwner.GetProperty(PropertyName)
        .GetValue(FOwner).AsString;

      if Length(Trim(LPropertyValue)) = 0 then
        Result := TRuleResult.Invalid(ErrorMessage)
      else
        Result := TRuleResult.Valid;
    end);
end;

function TValidationHelper.AddRule(PropertyName: string;
ValidateDelegate: TFunc<TRuleResult>): IValidationRule;
begin
  Guard.CheckTrue(Length(PropertyName) > 0, 'PropertyName');
  Guard.CheckNotNull(ValidateDelegate, 'ValidateDelegate');

  Result := AddRule([PropertyName], ValidateDelegate);
end;

function TValidationHelper.AddRule(const Properties: array of string;
ValidateDelegate: TFunc<TRuleResult>): IValidationRule;
var
  LTarget: IValidationTarget;
begin
  Guard.CheckTrue(Length(Properties) > 0, 'Properties');
  Guard.CheckNotNull(ValidateDelegate, 'ValidateDelegate');

  LTarget := CreatePropertyValidationTarget(Properties);

  Result := AddRuleCore(LTarget, ValidateDelegate, nil);
end;

function TValidationHelper.AddRule(ValidateDelegate: TFunc<TRuleResult>)
  : IValidationRule;
begin
  Guard.CheckNotNull(ValidateDelegate, 'ValidateDelegate');

  Result := AddRuleCore(TUndefinedValidationTarget.Create,
    ValidateDelegate, nil);
end;

function TValidationHelper.AddRule(Target: TValue;
ValidateDelegate: TFunc<TRuleResult>): IValidationRule;
begin
  Guard.CheckTrue(not Target.IsEmpty, 'Target');
  Guard.CheckNotNull(ValidateDelegate, 'ValidateDelegate');

  Result := AddRuleCore(TGenericValidationTarget.Create(Target),
    ValidateDelegate, nil);
end;

function TValidationHelper.AddRuleCore(Target: IValidationTarget;
ValidateDelegate: TFunc<TRuleResult>;
AsyncValidateAction: TAsyncRuleValidateAction): IAsyncValidationRule;
var
  LRule: IAsyncValidationRule;
begin
  LRule := TValidationRule.Create(Target, ValidateDelegate,
    AsyncValidateAction);
  RegisterValidationRule(LRule);
  Result := LRule;
end;

class function TValidationHelper.CreatePropertyValidationTarget(const Properties
  : array of string): IValidationTarget;
var
  LTarget: IValidationTarget;
begin
  if Length(Properties) = 1 then
  begin
    LTarget := TPropertyValidationTarget.Create(Properties[0]);
  end
  else
  begin
    LTarget := TPropertyCollectionValidationTarget.Create(Properties);
  end;
  Result := LTarget;
end;

procedure TValidationHelper.DisableValidation;
begin
  FIsValidationSuspended := True;
end;

procedure TValidationHelper.EnableValidation;
begin
  FIsValidationSuspended := False;
end;

function TValidationHelper.ExecuteRuleCore(Rule: IValidationRule): TRuleResult;
var
  LCompletedEvent: TSimpleEvent;
  LIsCompleted: TWaitResult;
  LResult: TRuleResult;
begin
  LResult := TRuleResult.Valid;

  if not Rule.SupportsSyncValidation then
  begin
    LCompletedEvent := TSimpleEvent.Create(nil, True, False, '');
    try
      Rule.EvaluateAsync(
        procedure(r: TRuleResult)
        begin
          LResult := r;
          LCompletedEvent.SetEvent;
        end);

      LIsCompleted := LCompletedEvent.WaitFor(AsyncRuleExecutionTimeout);
    finally
      LCompletedEvent.Free;
    end;

    if LIsCompleted <> wrSignaled then
    begin
      raise Exception.Create
        ('Rule Validation did not complete in specified timeout.');
    end;
  end
  else
  begin
    LResult := Rule.Evaluate;
  end;

  Result := LResult;
end;

function TValidationHelper.ExecuteValidationRules(const RulesToExecute
  : IEnumerable<IValidationRule>): IValidationResult;
var
  LFailedTargets: IHashSet<TObject>;
  LRuleResult: TRuleResult;
  LValidationRule: IValidationRule;
begin
  Result := TValidationResult.Create;

  LFailedTargets := THashSet<TObject>.Create;

  for LValidationRule in RulesToExecute do
  begin
    // Skip rule if the target is already invalid
    if LFailedTargets.Contains(LValidationRule.Target as TObject) then
    begin
      // Assume that the rule is valid at this point because we are not interested in this error until
      // previous rule is fixed.
      SaveRuleValidationResultAndNotifyIfNeeded(LValidationRule,
        TRuleResult.Valid);

      Continue;
    end;

    LRuleResult := ExecuteRuleCore(LValidationRule);

    SaveRuleValidationResultAndNotifyIfNeeded(LValidationRule, LRuleResult);

    AddErrorsFromRuleResult(Result, LValidationRule, LRuleResult);

    if not LRuleResult.IsValid then
    begin
      LFailedTargets.Add(LValidationRule.Target as TObject);
    end;
  end;
end;

function TValidationHelper.GetAsyncRuleExecutionTimeout: TTimeSpan;
begin
  Result := FAsyncRuleExecutionTimeout;
end;

class function TValidationHelper.GetCurrentValidationResultForRule
  (const RuleMap: TDictionary<IValidationRule, TRuleResult>;
const Rule: IValidationRule): TRuleResult;
begin
  TMonitor.Enter(RuleMap);
  try
    if not RuleMap.ContainsKey(Rule) then
    begin
      RuleMap.Add(Rule, TRuleResult.Valid);
    end;
    Result := RuleMap[Rule];
  finally
    TMonitor.Exit(RuleMap);
  end;
end;

function TValidationHelper.GetResult: IValidationResult;
begin
  Result := GetResultInternal;
end;

function TValidationHelper.GetResult(PropertyName: string): IValidationResult;
begin
  Guard.CheckTrue(Length(PropertyName) > 0, 'PropertyName');

  Result := GetResult(TValue.From(PropertyName));
end;

function TValidationHelper.GetResult(Target: TValue): IValidationResult;
var
  LReturnAllResults: Boolean;
begin
  Guard.CheckTrue(not Target.IsEmpty, 'Target');

  LReturnAllResults := Target.AsString = EmptyStr;

  if LReturnAllResults then
    Result := GetResultInternal
  else
    Result := GetResultInternal(Target);
end;

function TValidationHelper.GetResultChanged
  : IEvent<TValidationResultChangedEvent>;
begin
  Result := FResultChanged;
end;

function TValidationHelper.GetResultInternal: IValidationResult;
var
  LResult: IValidationResult;
  LRuleResultsMap: TDictionary<IValidationRule, TRuleResult>;
  LRuleResultsMapPair: TPair<TValue, TDictionary<IValidationRule, TRuleResult>>;
  LRuleTarget: TValue;
  LValidationResult: TRuleResult;
begin
  TMonitor.Enter(FSyncRoot);
  try
    LResult := TValidationResult.Valid;

    TMonitor.Enter(FRuleValidationResultMap);
    try
      for LRuleResultsMapPair in FRuleValidationResultMap do
      begin
        LRuleTarget := LRuleResultsMapPair.Key;
        LRuleResultsMap := LRuleResultsMapPair.Value;

        for LValidationResult in LRuleResultsMap.Values do
        begin
          LResult := TValidationResultExtensions.Combine(LResult,
            TValidationResult.Create(LRuleTarget, LValidationResult.Errors));
        end;
      end;
    finally
      TMonitor.Exit(FRuleValidationResultMap);
    end;

    Result := LResult;
  finally
    TMonitor.Exit(FSyncRoot);
  end;
end;

function TValidationHelper.GetResultInternal(const Target: TValue)
  : IValidationResult;
var
  LResult: IValidationResult;
  LRuleResultMap: TDictionary<IValidationRule, TRuleResult>;
  LRuleValidationResult: TRuleResult;
begin
  TMonitor.Enter(FSyncRoot);
  try
    LResult := TValidationResult.Valid;

    if (FRuleValidationResultMap.TryGetValue(Target, LRuleResultMap)) then
    begin
      for LRuleValidationResult in LRuleResultMap.Values do
      begin
        LResult := TValidationResultExtensions.Combine(LResult,
          TValidationResult.Create(Target, LRuleValidationResult.Errors));
      end;
    end;

    Result := LResult;
  finally
    TMonitor.Exit(FSyncRoot);
  end;
end;

function TValidationHelper.GetRuleMapForTarget(const Target: TValue)
  : TDictionary<IValidationRule, TRuleResult>;
begin
  TMonitor.Enter(FRuleValidationResultMap);
  try
    if not FRuleValidationResultMap.ContainsKey(Target) then
    begin
      FRuleValidationResultMap.Add(Target,
        TDictionary<IValidationRule, TRuleResult>.Create);
    end;

    Result := FRuleValidationResultMap[Target];
  finally
    TMonitor.Exit(FRuleValidationResultMap);
  end;
end;

function TValidationHelper.GetRulesForTarget(const Target: TValue)
  : IList<IValidationRule>;
var
  LValidationRule: IValidationRule;
begin
  TMonitor.Enter(FSyncRoot);
  try
    Result := TList<IValidationRule>.Create;

    if Target.IsEmpty then
    begin
      Result.AddRange(FValidationRules);
    end
    else
    begin
      for LValidationRule in FValidationRules do
      begin
        if LValidationRule.Target.IsMatch(Target) then
        begin
          Result.Add(LValidationRule);
        end;
      end;
    end;
  finally
    TMonitor.Exit(FSyncRoot);
  end;
end;

procedure TValidationHelper.ImportDataAnnotationRules;
var
  LAttribute: ValidationAttribute;
  LProperty: TRttiProperty;
  LRule: IValidationRule;
begin
  // Validate attributes
  for LProperty in FOwner.GetProperties do
  begin
    for LAttribute in LProperty.GetCustomAttributes<ValidationAttribute> do
    begin
      LRule := TDataAnnotationValidationRule.Create
        (TPropertyValidationTarget.Create(LProperty.Name), LAttribute,
        LProperty, FOwner);
      RegisterValidationRule(LRule);
    end;
  end;
end;

procedure TValidationHelper.RegisterValidationRule(Rule: IValidationRule);
begin
  TMonitor.Enter(FSyncRoot);
  try
    FValidationRules.Add(Rule);
  finally
    TMonitor.Exit(FSyncRoot);
  end;
end;

procedure TValidationHelper.RemoveAllRules;
begin
  UnregisterAllValidationRules;
end;

procedure TValidationHelper.RemoveRule(Rule: IValidationRule);
begin
  Guard.CheckNotNull(Rule, 'Rule');

  UnregisterValidationRule(Rule);
end;

procedure TValidationHelper.SaveRuleValidationResultAndNotifyIfNeeded
  (Rule: IValidationRule; const RuleResult: TRuleResult);
var
  LCurrentRuleResult: TRuleResult;
  LEvent: IValidationResultChangedEventArgs;
  LRuleTarget: TValue;
  LTargetRuleMap: TDictionary<IValidationRule, TRuleResult>;
begin
  TMonitor.Enter(FSyncRoot);
  try
    for LRuleTarget in Rule.Target.UnwrapTargets do
    begin
      LTargetRuleMap := GetRuleMapForTarget(LRuleTarget);

      LCurrentRuleResult := GetCurrentValidationResultForRule
        (LTargetRuleMap, Rule);

      if LCurrentRuleResult <> RuleResult then
      begin
        LTargetRuleMap[Rule] := RuleResult;

        LEvent := TValidationResultChangedEventArgs.Create(LRuleTarget,
          GetResult(LRuleTarget));

        // Notify that validation result for the target has changed (on UI thread)
        Execute.OnUIThread(
          procedure
          begin
            ResultChanged.Invoke(Self, LEvent);
          end);
      end;
    end;
  finally
    TMonitor.Exit(FSyncRoot);
  end;
end;

procedure TValidationHelper.SetAsyncRuleExecutionTimeout
  (const Value: TTimeSpan);
begin
  FAsyncRuleExecutionTimeout := Value;
end;

procedure TValidationHelper.UnregisterAllValidationRules;
begin
  TMonitor.Enter(FSyncRoot);
  try
    FValidationRules.Clear;
  finally
    TMonitor.Exit(FSyncRoot);
  end;
end;

procedure TValidationHelper.UnregisterValidationRule(Rule: IValidationRule);
begin
  TMonitor.Enter(FSyncRoot);
  try
    FValidationRules.Remove(Rule);
  finally
    TMonitor.Exit(FSyncRoot);
  end;
end;

function TValidationHelper.Validate(PropertyName: string): IValidationResult;
begin
  Guard.CheckTrue(Length(PropertyName) > 0, 'PropertyName');

  Result := ValidateInternal(PropertyName);
end;

function TValidationHelper.Validate(const Target: TValue): IValidationResult;
begin
  Guard.CheckTrue(not Target.IsEmpty, 'Target');

  Result := ValidateInternal(Target);
end;

function TValidationHelper.ValidateAll: IValidationResult;
begin
  Result := ValidateInternal(nil);
end;

function TValidationHelper.ValidateAllAsync: ITask<IValidationResult>;
begin
  Result := ValidateInternalAsync(nil);
end;

function TValidationHelper.ValidateAsync(PropertyName: string)
  : ITask<IValidationResult>;
begin
  Guard.CheckTrue(Length(PropertyName) > 0, 'PropertyName');

  Result := ValidateInternalAsync(PropertyName);
end;

function TValidationHelper.ValidateAsync(const Target: TValue)
  : ITask<IValidationResult>;
begin
  Guard.CheckTrue(not Target.IsEmpty, 'Target');

  Result := ValidateInternalAsync(Target);
end;

function TValidationHelper.ValidateInternal(const Target: TValue)
  : IValidationResult;
var
  LRule: IValidationRule;
  LRulesToExecute: IList<IValidationRule>;
begin
  TMonitor.Enter(FSyncRoot);
  try
    if FIsValidationSuspended then
    begin
      Exit(TValidationResult.Valid);
    end;

    // Get target rules
    LRulesToExecute := GetRulesForTarget(Target);

    // Validate rules
    for LRule in LRulesToExecute do
    begin
      if not LRule.SupportsSyncValidation then
      begin
        raise EInvalidOperationException.Create
          ('There are asynchronous rules that cannot be executed synchronously. Please use ValidateAsync method to execute validation instead.');
      end;
    end;

    try
      Result := ExecuteValidationRules(LRulesToExecute);
    except
      Exception.RaiseOuterException
        (EValidationException.Create
        ('An exception occurred during validation. See inner exception for details.')
        );
    end;
  finally
    TMonitor.Exit(FSyncRoot);
  end;
end;

function TValidationHelper.ValidateInternalAsync(const Target: TValue)
  : ITask<IValidationResult>;
begin
  if FIsValidationSuspended then
  begin
    Result := TTask<IValidationResult>.Create(
      function: IValidationResult
      begin
        Result := TValidationResult.Valid;
      end);
    // TODO
    Result.Start;
    Exit;
  end;
  Result := ExecuteValidationRulesAsync(Target);
end;

function TValidationHelper.ExecuteValidationRulesAsync(Target: TValue)
  : ITask<IValidationResult>;
begin
  Result := TTask<IValidationResult>.Create(
    function: IValidationResult
    var
      LRulesToExecute: IList<IValidationRule>;
    begin
      TMonitor.Enter(FSyncRoot);
      try
        try
          LRulesToExecute := GetRulesForTarget(Target);
          Result := ExecuteValidationRules(LRulesToExecute);
        except
          Exception.RaiseOuterException(EValidationException.Create
            ('An exception occurred during validation. See inner exception for details.')
            );
        end;
      finally
        TMonitor.Exit(FSyncRoot);
      end;
    end);
  // TODO
  Result.Start;
  Sleep(100);
end;

end.

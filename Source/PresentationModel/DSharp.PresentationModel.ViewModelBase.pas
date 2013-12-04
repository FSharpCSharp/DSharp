unit DSharp.PresentationModel.ViewModelBase;

interface

uses
  Classes,
  SysUtils,
  Rtti,
  DSharp.Bindings.Validations,
  DSharp.Collections,
  DSharp.ComponentModel.DataAnnotations,
  DSharp.Core.Cache,
  DSharp.Core.Editable,
  DSharp.Core.Validations,
  DSharp.PresentationModel.IHaveSubjectIntf,
  DSharp.PresentationModel.Screen,
  DSharp.PresentationModel.ScreenIntf,
  DSharp.Validation,
  DSharp.Validation.ValidationHelper;

type
  TEditAction = (eaCancel, eaSave);

  ///	<summary>
  ///	  Base class for view models with support for validation
  ///	</summary>
  ///	<remarks>
  ///	  Use when you need validation (no need for external Subject)
  ///	</remarks>
  TValidatingScreen = class(TScreen, IDataErrorInfo)
  private
    FThrowOnInvalidPropertyName: Boolean;
    FValidator: TValidationHelper;
  protected
    function GetError: string; virtual;
    function GetItem(const AName: string): string; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
    // ICanClose
    procedure CanClose(Callback: TProc<Boolean>); override;
    procedure VerifyPropertyName(const APropertyName: string);

    ///	<summary>
    ///	  Gets an error message indicating what is wrong with this object.
    ///	</summary>
    ///	<value>
    ///	  An error message indicating what is wrong with this object. The
    ///	  default is an empty string ("").
    ///	</value>
    property Error: string read GetError;

    ///	<summary>
    ///	  Gets the error message for the property with the given name.
    ///	</summary>
    ///	<param name="Name">
    ///	  The name of the property whose error message to get.
    ///	</param>
    ///	<value>
    ///	  The error message for the property. The default is an empty string
    ///	  ('').
    ///	</value>
    property Item[const Name: string]: string read GetItem; default;

    ///	<summary>
    ///	  Enable runtime checking of valid property names
    ///	</summary>
    property ThrowOnInvalidPropertyName: Boolean
      read FThrowOnInvalidPropertyName write FThrowOnInvalidPropertyName;
    property Validator: TValidationHelper read FValidator;
  end;

  ///	<summary>
  ///	  Base class for view models with support for validation, subject
  ///	</summary>
  ///	<remarks>
  ///	  Use when you work with external subject
  ///	</remarks>
  TViewModel = class(TValidatingScreen, IHaveSubject)
  private
    FSubject: TValue;
    function GetSubject: TValue;
  public
    ///	<summary>
    ///	  Configures the screen with the subject.
    ///	</summary>
    ///	<param name="Subject">
    ///	  The subject.
    ///	</param>
    ///	<returns>
    ///	  Self
    ///	</returns>
    function WithSubject(Subject: TValue): IHaveSubject; overload; virtual;

    ///	<summary>
    ///	  Gets the subject.
    ///	</summary>
    ///	<value>
    ///	  The subject.
    ///	</value>
    property Subject: TValue read GetSubject;
  end;

  ///	<summary>
  ///	  Base class for view models with support for validation, typed subject
  ///	</summary>
  ///	<typeparam name="T">
  ///	  The screen's type.
  ///	</typeparam>
  ///	<remarks>
  ///	  Use when you work with typed external subject
  ///	</remarks>

  TViewModel<T> = class(TViewModel, IHaveSubject<T>)
  private
    function GetSubject: T;
  public
    ///	<summary>
    ///	  Configures the screen with the subject.
    ///	</summary>
    ///	<param name="Subject">
    ///	  The subject.
    ///	</param>
    ///	<returns>
    ///	  Self
    ///	</returns>
    function WithSubject(Subject: T): IHaveSubject<T>; overload; virtual;

    ///	<summary>
    ///	  Gets the subject.
    ///	</summary>
    ///	<value>
    ///	  The subject.
    ///	</value>
    property Subject: T read GetSubject;
  end;

  ///	<summary>
  ///	  Base class for view models with support for validation, typed subject,
  ///	  editing/caching
  ///	</summary>
  ///	<typeparam name="T">
  ///	  The screen's type.
  ///	</typeparam>
  ///	<remarks>
  ///	  Use for the most complex scenarios
  ///	</remarks>

  TEditableViewModel<T: TPersistent, constructor> = class(TViewModel<T>,
    IEditable)
  private
    FCache: Cache<T>;
  protected
    // IEditable
    procedure BeginEdit;
    procedure CancelEdit;
    procedure EndEdit;
  public
    property Item[const AName: string]: string read GetItem; default;
  end;

implementation

uses
  DSharp.Core.Reflection;

{ TValidatingScreen }

constructor TValidatingScreen.Create;
begin
  inherited Create;
  FValidator := TValidationHelper.Create(Self);
end;

destructor TValidatingScreen.Destroy;
begin
  FValidator.Free;
  inherited;
end;

procedure TValidatingScreen.CanClose(Callback: TProc<Boolean>);
begin
  // TODO: Necessary?
  Callback(Validator.ValidateAll.IsValid);
end;

function TValidatingScreen.GetError: string;
begin
  Result := Validator.GetResult.ToString;
end;

function TValidatingScreen.GetItem(const AName: string): string;
begin
  Result := Validator.GetResult(AName).ToString;
end;

procedure TValidatingScreen.VerifyPropertyName(const APropertyName: string);
{$IFDEF DEBUG}
var
  LMessage: string;
  {$ENDIF}
begin
  {$IFDEF DEBUG}
  if GetProperty(APropertyName) = nil then
  begin
    LMessage := 'Invalid property name: ' + APropertyName;
    if FThrowOnInvalidPropertyName then
      raise Exception.Create(LMessage)
    else
      Log.LogWarning(LMessage, []);
  end;
  {$ENDIF}
end;

{ TViewModel }

function TViewModel.GetSubject: TValue;
begin
  Result := FSubject;
end;

function TViewModel.WithSubject(Subject: TValue): IHaveSubject;
begin
  FSubject := Subject;
  Result := Self;
end;

{ TViewModel<T> }

function TViewModel<T>.GetSubject: T;
begin
  Result := (inherited GetSubject).AsType<T>;
end;

function TViewModel<T>.WithSubject(Subject: T): IHaveSubject<T>;
begin
  inherited WithSubject(TValue.From<T>(Subject));
  Result := Self;
end;

{ TEditableViewModel<T> }

procedure TEditableViewModel<T>.BeginEdit;
begin
  FCache.Instance := Subject;
  FCache.Store();
end;

procedure TEditableViewModel<T>.CancelEdit;
begin
  FCache.Restore();
  FCache.Instance := nil;
end;

procedure TEditableViewModel<T>.EndEdit;
begin
  FCache.Instance := nil;
end;

end.

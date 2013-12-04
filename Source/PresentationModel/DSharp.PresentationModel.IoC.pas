unit DSharp.PresentationModel.IoC;

interface

uses
  Rtti,
  SysUtils,
  TypInfo;

type
  ///	<summary>
  ///	  Used by the framework to pull instances from an IoC container and to
  ///	  inject dependencies into certain existing classes.
  ///	</summary>
  IoC = class
  private
  class var
    FGetInstance: TFunc<PTypeInfo, string, TValue>;
    FGetAllInstances: TFunc<PTypeInfo, TArray<TValue>>;
    FBuildUp: TProc<TObject>;
  public
    class constructor Create;

    ///	<summary>
    ///	  Gets all instances of a particular type.
    ///	</summary>
    ///	<typeparam name="T">
    ///	  The type to resolve from the container.
    ///	</typeparam>
    ///	<returns>
    ///	  The resolved instances.
    ///	</returns>
    class function GetAll<T>(): TArray<T>; overload; static;

    ///	<summary>
    ///	  Gets an instance from the container.
    ///	</summary>
    ///	<typeparam name="T">
    ///	  The type to resolve.
    ///	</typeparam>
    ///	<param name="Key">
    ///	  The key to look up.
    ///	</param>
    ///	<returns>
    ///	  The resolved instance.
    ///	</returns>
    class function Get<T>(const Key: string = ''): T; overload; static;

    ///	<summary>
    ///	  Gets an instance by type and key.
    ///	</summary>
    class property GetInstance: TFunc<PTypeInfo, string, TValue>
      read FGetInstance write FGetInstance;

    ///	<summary>
    ///	  Gets all instances of a particular type.
    ///	</summary>
    class property GetAllInstances: TFunc < PTypeInfo, TArray < TValue >>
      read FGetAllInstances write FGetAllInstances;

    ///	<summary>
    ///	  Passes an existing instance to the IoC container to enable
    ///	  dependencies to be injected.
    ///	</summary>
    class property BuildUp: TProc<TObject> read FBuildUp write FBuildUp;
  end;

implementation

uses
  Spring;

{ IoC }

class constructor IoC.Create;
begin
  FGetInstance := function(Service: PTypeInfo; Key: string): TValue
    begin
      raise EInvalidOperationException.Create('IoC is not initialized.');
    end;
  FGetAllInstances := function(Service: PTypeInfo): TArray<TValue>
    begin
      raise EInvalidOperationException.Create('IoC is not initialized.');
    end;
  FBuildUp := procedure(Value: TObject)
    begin
      raise EInvalidOperationException.Create('IoC is not initialized.');
    end;
end;

class function IoC.GetAll<T>: TArray<T>;
var
  i: Integer;
  LInstances: TArray<TValue>;
begin
  LInstances := GetAllInstances(TypeInfo(T));
  SetLength(Result, Length(LInstances));
  for i := 0 to Length(LInstances) - 1 do
  begin
    Result[i] := LInstances[i].AsType<T>;
  end;
end;

class function IoC.Get<T>(const Key: string = ''): T;
var
  LValue: TValue;
begin
  if Assigned(GetInstance) then
  begin
    LValue := GetInstance(TypeInfo(T), Key);
    Result := LValue.AsType<T>;
  end
  else
  begin
    Result := Default (T);
  end;
end;

end.

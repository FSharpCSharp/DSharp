unit DSharp.PresentationModel.BindingScope;

interface

uses
  Classes,
  DSharp.Collections,
  DSharp.Collections.Extensions,
  SysUtils,
  Rtti;

type
  ///	<summary>
  ///	  Provides methods for searching a given scope for named elements.
  ///	</summary>
  BindingScope = class
  private
  class var
    FGetNamedElements: TFunc<TComponent, IEnumerable<TComponent>>;
    FGetActionableMethods: TFunc<TRttiType, IEnumerable<TRttiMethod>>;
    FMethodFilter: IHashSet<string>;
  public
    class constructor Create;

    ///	<summary>
    ///	  Searches through the list of named elements looking for a
    ///	  case-insensitive match.
    ///	</summary>
    ///	<param name="elementsToSearch">
    ///	  The named elements to search through.
    ///	</param>
    ///	<param name="name">
    ///	  The name to search for.
    ///	</param>
    ///	<returns>
    ///	  The named element or null if not found.
    ///	</returns>
    class function FindName(ElementsToSearch: IEnumerable<TComponent>;
      Name: string): TComponent;

    ///	<summary>
    ///	  Gets all the <see cref="TComponent" /> instances with names in the
    ///	  scope.
    ///	</summary>
    ///	<returns>
    ///	  Named <see cref="TComponent" /> instances in the provided scope.
    ///	</returns>
    ///	<remarks>
    ///	  Pass in a <see cref="TComponent" /> and receive a list of named
    ///	  <see cref="TComponent" /> instances in the same scope.
    ///	</remarks>
    class property GetNamedElements: TFunc < TComponent,
      IEnumerable < TComponent >> read FGetNamedElements;

    ///	<summary>
    ///	  Gets all the <see cref="TRttiMethod" /> actionable methods in the
    ///	  scope.
    ///	</summary>
    ///	<returns>
    ///	  <see cref="TRttiMethod" /> methods in the provided scope.
    ///	</returns>
    ///	<remarks>
    ///	  Pass in a <see cref="TRttiType" /> and receive a list of actionable
    ///	  <see cref="TRttiMethod" /> methods in the same scope.
    ///	</remarks>
    class property GetActionableMethods: TFunc < TRttiType,
      IEnumerable < TRttiMethod >> read FGetActionableMethods;
  end;

implementation

uses
  TypInfo;

const
  CDefaultMethodFilter: array [0 .. 34] of string = ('AfterConstruction',
    'Assign', 'BeforeDestruction', 'ClassType', 'CleanupInstance',
    'DefaultHandler', 'DestroyComponents', 'Destroying', 'Dispatch',
    'DisposeOf', 'Equals', 'ExecuteAction', 'FieldAddress', 'FindComponent',
    'Free', 'FreeInstance', 'FreeNotification', 'FreeOnRelease',
    'GetEnumerator', 'GetHashCode', 'GetInterface', 'GetNamePath',
    'GetParentComponent', 'HasParent', 'InsertComponent', 'IsImplementorOf',
    'NotifyOfPropertyChange ', 'RaisePropertyChangedEventImmediately',
    'ReferenceInterface', 'RemoveComponent', 'RemoveFreeNotification',
    'SafeCallException', 'SetSubComponent', 'ToString', 'UpdateAction');

  { BindingScope }

class constructor BindingScope.Create;
var
  LMethodName: string;
begin
  // Initialize name filter
  FMethodFilter := THashSet<string>.Create(nil);
  for LMethodName in CDefaultMethodFilter do
    FMethodFilter.Add(LMethodName);

  // Initialize get named elements
  FGetNamedElements :=
      function(ElementInScope: TComponent): IEnumerable<TComponent>
    var
      LComponent: TComponent;
      LList: IList<TComponent>;
    begin
      LList := TList<TComponent>.Create;
      for LComponent in ElementInScope do
      begin
        if LComponent.Name <> EmptyStr then
        begin
          LList.Add(LComponent);
        end;
      end;
      Result := LList;
    end;

  // Initialize get actionable methods
  FGetActionableMethods :=
      function(ElementInScope: TRttiType): IEnumerable<TRttiMethod>
    var
      LList: IList<TRttiMethod>;
      LMethod: TRttiMethod;
    begin
      LList := TList<TRttiMethod>.Create;

      for LMethod in ElementInScope.GetMethods do
      begin
        // Skip constructors, destructors, class methods...
        if not(LMethod.MethodKind in [mkProcedure, mkFunction]) then
          Continue;

        // Skip private methods
        if LMethod.Visibility = mvPrivate then
          Continue;

        // Skip filtered method names
        if FMethodFilter.Contains(LMethod.Name) then
          Continue;

        LList.Add(LMethod);
      end;
      Result := LList;
    end;
end;

class function BindingScope.FindName(ElementsToSearch: IEnumerable<TComponent>;
  Name: string): TComponent;
var
  LComponent: TComponent;
begin
  Result := nil;
  for LComponent in ElementsToSearch do
  begin
    if SameText(LComponent.Name, Name) then
      Exit(LComponent);
  end;
end;

end.

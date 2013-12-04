unit DSharp.PresentationModel.ViewLocator;

interface

uses
  Classes,
  SysUtils,
  DSharp.Collections,
  DSharp.Logging,
  DSharp.PresentationModel,
  DSharp.PresentationModel.NameTransformer;

type
  TLocateForModelFunc = reference to function(Model: TObject;
    DisplayLocation: TComponent; Context: TValue): TComponent;
  TLocateForModelTypeFunc = reference to function(Model: TObject;
    DisplayLocation: TComponent; Context: TValue): TComponent;
  TLocateTypeForModelTypeFunc = reference to function(ModelType: TClass;
    DisplayLocation: TComponent; Context: TValue): TClass;
  TGetOrCreateViewTypeFunc = reference to function(Model: TObject;
    ViewType: TClass): TComponent;

  ///	<summary>
  ///	  A strategy for determining which view to use for a given model.
  ///	</summary>
  ViewLocator = class
  private
  class var
    FGetOrCreateViewType: TGetOrCreateViewTypeFunc;
    FLocateForModel: TLocateForModelFunc;
    FLocateForModelType: TLocateForModelTypeFunc;
    FLocateTypeForModelType: TLocateTypeForModelTypeFunc;
    FLog: ILog;
    FNameTransformer: INameTransformer;
    FCreateWarningView: TFunc<TComponent, string, TComponent>;

    class function GetLog: ILog; static;
    class property Log: ILog read GetLog;
  public
    class constructor Create;

    ///	<summary>
    ///	  Retrieves the view from the IoC container or tries to create it if
    ///	  not found.
    ///	</summary>
    ///	<remarks>
    ///	  Pass the type of view as a parameter and recieve an instance of the
    ///	  view.
    ///	</remarks>
    class property GetOrCreateViewType: TGetOrCreateViewTypeFunc
      read FGetOrCreateViewType write FGetOrCreateViewType;

    ///	<summary>
    ///	  Locates the view for the specified model instance.
    ///	</summary>
    ///	<returns>
    ///	  The view.
    ///	</returns>
    ///	<remarks>
    ///	  Pass the model instance, display location (or null) and the context
    ///	  (or null) as parameters and receive a view instance.
    ///	</remarks>
    class property LocateForModel: TLocateForModelFunc read FLocateForModel
      write FLocateForModel;

    ///	<summary>
    ///	  Locates the view for the specified model type.
    ///	</summary>
    ///	<returns>
    ///	  The view.
    ///	</returns>
    ///	<remarks>
    ///	  Pass the model type, display location (or null) and the context
    ///	  instance (or null) as parameters and receive a view instance.
    ///	</remarks>
    class property LocateForModelType: TLocateForModelTypeFunc
      read FLocateForModelType write FLocateForModelType;

    ///	<summary>
    ///	  Locates the view type based on the specified model type.
    ///	</summary>
    ///	<returns>
    ///	  The view.
    ///	</returns>
    ///	<remarks>
    ///	  Pass the model type, display location (or null) and the context
    ///	  instance (or null) as parameters and receive a view type.
    ///	</remarks>
    class property LocateTypeForModelType: TLocateTypeForModelTypeFunc
      read FLocateTypeForModelType write FLocateTypeForModelType;

    ///	<summary>
    ///	  Creates warning view if default not found
    ///	</summary>
    class property CreateWarningView: TFunc<TComponent, string, TComponent>
      read FCreateWarningView write FCreateWarningView;
  end;

implementation

uses
  DSharp.Core.Reflection,
  Rtti;

{ ViewLocator }

class constructor ViewLocator.Create;
begin
  FNameTransformer := TNameTransformer.Create();

  FNameTransformer.AddRule('Model$', '');
  FNameTransformer.AddRule('ViewModel$', 'View');
  FNameTransformer.AddRule('ViewModel$', 'ViewForm');
  FNameTransformer.AddRule('ViewModel$', 'ViewFrame');
  FNameTransformer.AddRule('ViewModel$', 'Form');
  FNameTransformer.AddRule('ViewModel$', 'Frame');

  CreateWarningView := function(Owner: TComponent; Warning: string): TComponent
    begin
      raise Exception.Create('CreateWarningView() not assigned.');
    end;

  LocateForModel :=
      function(Model: TObject; DisplayLocation: TComponent; Context: TValue)
      : TComponent
    var
      LViewAware: IViewAware;
      LView: TComponent;
    begin
      if Supports(Model, IViewAware, LViewAware) then
      begin
        LView := LViewAware.GetView(Context) as TComponent;
        if Assigned(LView) then
        begin
          Log.LogMessage('Using cached view for %s.', [Model]);
          Exit(LView);
        end;
      end;
      Result := LocateForModelType(Model, DisplayLocation, Context);
    end;

  LocateForModelType :=
      function(Model: TObject; DisplayLocation: TComponent; Context: TValue)
      : TComponent
    var
      LViewType: TClass;
      LWarning: string;
    begin
      LViewType := LocateTypeForModelType(Model.ClassType,
        DisplayLocation, Context);

      if LViewType = nil then
      begin
        LWarning := 'View not found for ' + Model.ClassType.ClassName;
        if not Context.IsEmpty then
          LWarning := LWarning + ' with context ' + Context.ToString;

        Result := CreateWarningView(DisplayLocation, LWarning);
      end
      else
        Result := GetOrCreateViewType(Model, LViewType);
    end;

  LocateTypeForModelType :=
      function(ModelType: TClass; DisplayLocation: TComponent;
      Context: TValue): TClass
    var
      LViewType: TClass;
      LViewTypeList: IEnumerable<string>;
      LViewTypeName: string;
      LType: TRttiType;
      LSuffix: string;
      s: string;
    begin
      LSuffix := '';
      if not Context.IsEmpty then
        LSuffix := Context.ToString;

      LViewType := nil;
      LViewTypeList := FNameTransformer.Transform(ModelType.ClassName);

      for LViewTypeName in LViewTypeList do
      begin
        if FindType(LViewTypeName + LSuffix, LType) then
        begin
          LViewType := LType.AsInstance.MetaclassType;
          Break;
        end;
      end;

      if LViewType = nil then
      begin
        if FindType(ModelType.ClassName + 'View', LType) then
        begin
          LViewType := LType.AsInstance.MetaclassType;
        end;
      end;

      if LViewType = nil then
      begin
        for LViewTypeName in LViewTypeList do
          s := s + LViewTypeName + ', ';
        Log.LogWarning('View not found. Searched: %s.', [s]);
      end;

      Result := LViewType;
    end;

  GetOrCreateViewType := function(Model: TObject; ViewType: TClass): TComponent
    begin
      if ViewType.InheritsFrom(TComponent) and Model.InheritsFrom(TComponent)
      then
      begin
        Result := TComponentClass(ViewType).Create(TComponent(Model));
      end
      else
        // TODO: Experimental support to enable binding of views to ordinary objects (needed by GameLibrary - TGameDTO)
        if ViewType.InheritsFrom(TComponent) and Model.InheritsFrom(TObject)
        then
        begin
          Result := TComponentClass(ViewType).Create(nil);
        end
        else
        begin
          raise Exception.CreateFmt('Cannot create view %s.',
            [ViewType.ClassName]);
        end;
    end;
end;

class function ViewLocator.GetLog: ILog;
begin
  if not Assigned(FLog) then
  begin
    FLog := LogManager.GetLog(TypeInfo(ViewLocator));
  end;
  Result := FLog;
end;

end.

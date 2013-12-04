unit DSharp.PresentationModel.VCLApplication;

interface

uses
  Classes,
  Forms,
  DSharp.Logging,
  DSharp.PresentationModel.INPC,
  DSharp.PresentationModel.Bootstrapper,
  DSharp.PresentationModel.VCLFramework,
  DSharp.PresentationModel.VCLView,
  DSharp.PresentationModel.VCLViewLocator,
  DSharp.PresentationModel.VCLViewModelBinder,
  DSharp.PresentationModel.VCLWindowManager;

type
  TApplicationHelper = class helper for TApplication

    ///	<summary>
    ///	  Starts the application with custom bootstrapper
    ///	</summary>
    ///	<param name="BootstrapperClass">
    ///	  The class of the custom bootstrapper
    ///	</param>
    procedure Start(BootstrapperClass: TBootstrapperClass); overload;

    ///	<summary>
    ///	  Starts the application with default Spring bootstrapper
    ///	</summary>
    ///	<typeparam name="T">
    ///	  The type of root view model
    ///	</typeparam>
    procedure Start<T: IInterface>; overload;

    ///	<summary>
    ///	  Configures the application to use OutputDebugString debug logger
    ///	</summary>
    function WithDebugLogger: TApplication;

    ///	<summary>
    ///	  Configures the application to use a specific logger
    ///	</summary>
    function WithLogger<T: TLogBase, constructor>(): TApplication;
  end;

implementation

uses
  TypInfo,
  DSharp.Logging.Debug;

procedure TApplicationHelper.Start(BootstrapperClass: TBootstrapperClass);
var
  LBootstrapper: TBootstrapper;
begin
  LBootstrapper := BootstrapperClass.Create();
  try
    LBootstrapper.StartRuntime();
  finally
    LBootstrapper.Free();
  end;
end;

procedure TApplicationHelper.Start<T>;
begin
  Start(TSpringBootstrapper<T>);
end;

function TApplicationHelper.WithDebugLogger: TApplication;
begin
  Result := WithLogger<TDebugLog>();
end;

function TApplicationHelper.WithLogger<T>(): TApplication;
begin
  LogManager.GetLog := function(ATypeInfo: PTypeInfo): ILog
    var
      LLogBaseClass: TLogBaseClass;
      LLogBase: TLogBase;
    begin
      // LLogBase := T.Create(ATypeInfo); // [dcc32 Error] E2029 ')' expected but identifier 'ATypeInfo' found
      LLogBaseClass := T;
      LLogBase := LLogBaseClass.Create(ATypeInfo);
      Result := LLogBase;
    end;
  Result := Self;
end;

end.

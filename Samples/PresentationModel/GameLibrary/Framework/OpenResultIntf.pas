unit OpenResultIntf;

interface

uses
  SysUtils,
  DSharp.PresentationModel;

type
  IOpenResult<TTarget> = interface(IResult)
    ['{F055DE22-190E-468B-A1CE-1AE635665896}']
    function GetOnClose: TProc<TTarget>;
    function GetOnConfigure: TProc<TTarget>;
    procedure SetOnClose(const Value: TProc<TTarget>);
    procedure SetOnConfigure(const Value: TProc<TTarget>);
    property OnClose: TProc<TTarget> read GetOnClose write SetOnClose;
    property OnConfigure: TProc<TTarget> read GetOnConfigure
      write SetOnConfigure;
    function Configured(Configure: TProc<TTarget>): IOpenResult<TTarget>;
    function WhenClosing(OnShutdown: TProc<TTarget>): IOpenResult<TTarget>;
  end;

implementation

end.

unit OpenResultBase;

interface

uses
  SysUtils,
  DSharp.PresentationModel,
  OpenResultIntf;

type
  TOpenResultBase<TTarget> = class(TResultBase, IOpenResult<TTarget>)
  private
    FOnClose: TProc<TTarget>;
    FOnConfigure: TProc<TTarget>;
  public
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

{ TOpenResultBase<TTarget> }

function TOpenResultBase<TTarget>.Configured(Configure: TProc<TTarget>)
  : IOpenResult<TTarget>;
begin
  OnConfigure := Configure;
  Result := Self;
end;

function TOpenResultBase<TTarget>.GetOnClose: TProc<TTarget>;
begin
  Result := FOnClose;
end;

function TOpenResultBase<TTarget>.GetOnConfigure: TProc<TTarget>;
begin
  Result := FOnConfigure;
end;

procedure TOpenResultBase<TTarget>.SetOnClose(const Value: TProc<TTarget>);
begin
  FOnClose := Value;
end;

procedure TOpenResultBase<TTarget>.SetOnConfigure(const Value: TProc<TTarget>);
begin
  FOnConfigure := Value;
end;

function TOpenResultBase<TTarget>.WhenClosing(OnShutdown: TProc<TTarget>)
  : IOpenResult<TTarget>;
begin
  OnClose := OnShutdown;
  Result := Self;
end;

end.

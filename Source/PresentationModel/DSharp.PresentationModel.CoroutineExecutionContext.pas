unit DSharp.PresentationModel.CoroutineExecutionContext;

interface

uses
  Classes,
  DSharp.PresentationModel.CoroutineExecutionContextIntf;

type
  ///	<summary>
  ///	  An implementation of <see cref="ICoroutineExecutionContext" />
  ///	</summary>
  TCoroutineExecutionContext = class(TInterfacedObject, ICoroutineExecutionContext)
  private
    FSender: TObject;
    FTarget: TObject;
    FView: TComponent;
    function GetSender: TObject;
    function GetTarget: TObject;
    function GetView: TComponent;
    procedure SetSender(const Value: TObject);
    procedure SetView(const Value: TComponent);
    procedure SetTarget(const Value: TObject);
  end;

implementation

function TCoroutineExecutionContext.GetSender: TObject;
begin
  Result := FSender;
end;

function TCoroutineExecutionContext.GetTarget: TObject;
begin
  Result := FTarget;
end;

function TCoroutineExecutionContext.GetView: TComponent;
begin
  Result := FView;
end;

procedure TCoroutineExecutionContext.SetSender(const Value: TObject);
begin
  FSender := Value;
end;

procedure TCoroutineExecutionContext.SetTarget(const Value: TObject);
begin
  FTarget := Value;
end;

procedure TCoroutineExecutionContext.SetView(const Value: TComponent);
begin
  FView := Value;
end;

end.

unit Show;

interface

uses
  DSharp.PresentationModel,
  OpenChildResult;

type
  TShow = class
  public
    class function Busy: IResult;
    class function Child<TChild>: TOpenChildResult<TChild>;
    class function NotBusy: IResult;
  end;

implementation

uses
  BusyResult;

{ TShow }

class function TShow.Busy: IResult;
begin
  Result := TBusyResult.Create(False);
end;

class function TShow.Child<TChild>: TOpenChildResult<TChild>;
begin
  Result := TOpenChildResult<TChild>.Create;
end;

class function TShow.NotBusy: IResult;
begin
  Result := TBusyResult.Create(True);
end;

end.

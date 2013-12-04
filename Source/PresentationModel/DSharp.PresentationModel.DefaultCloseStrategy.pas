unit DSharp.PresentationModel.DefaultCloseStrategy;

interface

uses
  Classes,
  SysUtils,
  DSharp.Collections,
  DSharp.PresentationModel.CloseStrategyIntf;

type
  ///	<summary>
  ///	  Used to gather the results from multiple child elements which may or
  ///	  may not prevent closing.
  ///	</summary>
  ///	<typeparam name="T">
  ///	  The type of child element.
  ///	</typeparam>
  TDefaultCloseStrategy<T> = class(TInterfacedObject, ICloseStrategy<T>)
  private
    FClosable: IList<T>;
    FFinalResult: Boolean;
    FGuardMustCallEvaluate: Boolean;
    FCloseConductedItemsWhenConductorCannotClose: Boolean;
    procedure Evaluate(Result: Boolean; Enumerator: IEnumerator<T>;
      Callback: TProc < Boolean, IEnumerable < T >> );
  public
    ///	<summary>
    ///	  Creates an instance of the class.
    ///	</summary>
    ///	<param name="closeConductedItemsWhenConductorCannotClose">
    ///	  Indicates that even if all conducted items are not closable, those
    ///	  that are should be closed. The default is FALSE.
    ///	</param>
    constructor Create(CloseConductedItemsWhenConductorCannotClose
      : Boolean = False);

    ///	<summary>
    ///	  Executes the strategy.
    ///	</summary>
    ///	<param name="ToClose">
    ///	  Items that are requesting close.
    ///	</param>
    ///	<param name="Callback">
    ///	  The action to call when all enumeration is complete and the close
    ///	  results are aggregated. The bool indicates whether close can occur.
    ///	  The enumerable indicates which children should close if the parent
    ///	  cannot.
    ///	</param>
    procedure Execute(ToClose: IEnumerable<T>;
      Callback: TProc < Boolean, IEnumerable < T >> );
  end;

implementation

uses
  DSharp.Core.Reflection,
  DSharp.PresentationModel;

{ TDefaultCloseStrategy<T> }

constructor TDefaultCloseStrategy<T>.Create
  (CloseConductedItemsWhenConductorCannotClose: Boolean);
begin
  FCloseConductedItemsWhenConductorCannotClose :=
    CloseConductedItemsWhenConductorCannotClose;
end;

procedure TDefaultCloseStrategy<T>.Evaluate(Result: Boolean;
  Enumerator: IEnumerator<T>; Callback: TProc < Boolean, IEnumerable < T >> );
var
  LCurrent: T;
  LGuard: IGuardClose;
  LGuardPending: Boolean;
  LList: IList<T>;
begin
  FFinalResult := FFinalResult and Result;

  LGuardPending := False;
  repeat
    if not Enumerator.MoveNext() then
    begin
      if FCloseConductedItemsWhenConductorCannotClose then
      begin
        Callback(FFinalResult, FClosable);
      end
      else
      begin
        LList := TList<T>.Create;
        Callback(FFinalResult, LList);
      end;
      FClosable := nil;
      Break;
    end;

    LCurrent := Enumerator.Current;
    if Supports(TValue.From<T>(LCurrent), IGuardClose, LGuard) then
    begin
      LGuardPending := True;
      LGuard.CanClose(
        procedure(CanClose: Boolean)
        begin
          LGuardPending := False;
          if CanClose then
          begin
            FClosable.Add(LCurrent);
          end;
          if FGuardMustCallEvaluate then
          begin
            FGuardMustCallEvaluate := False;
            Evaluate(CanClose, Enumerator, Callback);
          end
          else
          begin
            FFinalResult := FFinalResult and CanClose;
          end;
        end);
      FGuardMustCallEvaluate := FGuardMustCallEvaluate or LGuardPending;
    end
    else
    begin
      FClosable.Add(LCurrent);
    end;
  until LGuardPending;
end;

procedure TDefaultCloseStrategy<T>.Execute(ToClose: IEnumerable<T>;
Callback: TProc < Boolean, IEnumerable < T >> );
begin
  FFinalResult := True;
  FClosable := TList<T>.Create;
  FGuardMustCallEvaluate := False;

  Evaluate(True, ToClose.GetEnumerator(), Callback);
end;

end.

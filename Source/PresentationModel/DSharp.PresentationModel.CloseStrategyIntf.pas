unit DSharp.PresentationModel.CloseStrategyIntf;

interface

uses
  SysUtils,
  DSharp.Collections;

type
  ///	<summary>
  ///	  Used to gather the results from multiple child elements which may or
  ///	  may not prevent closing.
  ///	</summary>
  ///	<typeparam name="T">
  ///	  The type of child element.
  ///	</typeparam>
  ICloseStrategy<T> = interface

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

end.

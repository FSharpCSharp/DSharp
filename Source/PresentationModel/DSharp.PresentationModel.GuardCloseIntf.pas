unit DSharp.PresentationModel.GuardCloseIntf;

interface

uses
  SysUtils,
  DSharp.PresentationModel.CloseIntf;

type
  ///	<summary>
  ///	  Denotes an instance which may prevent closing.
  ///	</summary>
  IGuardClose = interface(IClose)
    ['{459F9F70-14FB-4C2E-9DC1-B076F05A03E8}']

    ///	<summary>
    ///	  Called to check whether or not this instance can close.
    ///	</summary>
    ///	<param name="callback">
    ///	  The implementer calls this action with the result of the close check.
    ///	</param>
    procedure CanClose(Callback: TProc<Boolean>);
  end;

implementation

end.

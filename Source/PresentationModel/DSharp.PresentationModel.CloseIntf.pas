unit DSharp.PresentationModel.CloseIntf;

interface

uses
  {$IF CompilerVersion > 22}
  UITypes,
  {$ELSE}
  Controls,
  {$IFEND}
  SysUtils;

const
  {$IF CompilerVersion > 22}
  mrNone = UITypes.mrNone;
  {$ELSE}
  mrNone = Controls.mrNone;
  {$IFEND}

type
  {$IF CompilerVersion > 22}
  TModalResult = UITypes.TModalResult;
  {$ELSE}
  TModalResult = Controls.TModalResult;
  {$IFEND}

  ///	<summary>
  ///	  Denotes an object that can be closed.
  ///	</summary>

  IClose = interface
    ['{B3A2C8D5-77D4-4088-BA43-9CE29CD7872E}']

    {$REGION 'Property Accessors'}
    function GetModalResult: TModalResult;
    procedure SetModalResult(const Value: TModalResult);
    {$ENDREGION}

    ///	<summary>
    ///	  Tries to close this instance. Also provides an opportunity to pass a
    ///	  dialog result to it's corresponding view.
    ///	</summary>
    ///	<param name="dialogResult">
    ///	  The dialog result.
    ///	</param>
    procedure TryClose(ModalResult: TModalResult = mrNone);

    ///	<summary>
    ///	  TModalResult represents the return value from a modal dialog.
    ///	</summary>
    property ModalResult: TModalResult read GetModalResult write SetModalResult;
  end;

implementation

end.

unit DSharp.PresentationModel.CoroutineExecutionContextIntf;

interface

uses
  Classes;

type
  ///	<summary>
  ///	  The context used during the execution of a Coroutine.
  ///	</summary>
  ICoroutineExecutionContext = interface
    ['{D9E2ED27-2DB7-4F79-8CAD-941C689D9097}']
    {$REGION 'Property Accessors'}
    function GetSender: TObject;
    function GetView: TComponent;
    function GetTarget: TObject;
    procedure SetSender(const Value: TObject);
    procedure SetView(const Value: TComponent);
    procedure SetTarget(const Value: TObject);
    {$ENDREGION}

    ///	<summary>
    ///	  The source from which the message originates.
    ///	</summary>
    property Sender: TObject read GetSender write SetSender;

    ///	<summary>
    ///	  The view associated with the target.
    ///	</summary>
    property View: TComponent read GetView write SetView;

    ///	<summary>
    ///	  The instance on which the action is invoked.
    ///	</summary>
    property Target: TObject read GetTarget write SetTarget;
  end;

implementation

end.

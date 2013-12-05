unit DSharp.PresentationModel.ActionExecutionContextIntf;

interface

uses
  Classes,
  Rtti;

type
  ///	<summary>
  ///	  The context used during the execution of an Action or its guard.
  ///	</summary>
  IActionExecutionContext = interface
    ['{44D9CDDD-9A8D-48F9-B697-2C280DF0E7A5}']
    {$REGION 'Property Accessors'}
    function GetMethod: TRttiMethod;
    function GetSender: TObject;
    function GetView: TComponent;
    function GetTarget: TObject;
    procedure SetMethod(const Value: TRttiMethod);
    procedure SetSender(const Value: TObject);
    procedure SetView(const Value: TComponent);
    procedure SetTarget(const Value: TObject);
    {$ENDREGION}

    ///	<summary>
    ///	  The actual method info to be invoked.
    ///	</summary>
    property Method: TRttiMethod read GetMethod write SetMethod;

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

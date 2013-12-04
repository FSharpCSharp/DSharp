unit DSharp.PresentationModel.ViewAwareIntf;

interface

uses
  Rtti,
  DSharp.Core.Events,
  DSharp.PresentationModel.ViewAttachedEventArgsIntf;

type
  ///	<summary>
  ///	  Denotes a class which is aware of its view(s).
  ///	</summary>
  IViewAware = interface
    ['{99512E07-7645-4D20-8ECF-51CD43E15BAE}']

    {$REGION 'Property Accessors'}
    function GetViewAttached: IEvent<TViewAttachedEvent>;
    {$ENDREGION}

    ///	<summary>
    ///	  Attaches a view to this instance.
    ///	</summary>
    ///	<param name="view">
    ///	  The view.
    ///	</param>
    ///	<param name="context">
    ///	  The context in which the view appears.
    ///	</param>
    procedure AttachView(View: TObject); overload;
    procedure AttachView(View: TObject; Context: TValue); overload;

    ///	<summary>
    ///	  Gets a view previously attached to this instance.
    ///	</summary>
    ///	<param name="context">
    ///	  The context denoting which view to retrieve.
    ///	</param>
    ///	<returns>
    ///	  The view.
    ///	</returns>
    function GetView: TObject; overload;
    function GetView(Context: TValue): TObject; overload;

    ///	<summary>
    ///	  Raised when a view is attached.
    ///	</summary>
    property ViewAttached: IEvent<TViewAttachedEvent> read GetViewAttached;
    procedure TakeOwnership(const View: TObject);
  end;

implementation

end.

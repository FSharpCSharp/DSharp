unit DSharp.PresentationModel.ViewAttachedEventArgsIntf;

interface

uses
  Rtti,
  DSharp.Core.EventArgs;

type
  ///	<summary>
  ///	  The event args for the <see cref="IViewAware.ViewAttached" /> event.
  ///	</summary>
  IViewAttachedEventArgs = interface(IEventArgs)
    ['{442668DB-3DDA-43B8-935B-EF928786F010}']
    {$REGION 'Property Accessors'}
    function GetView: TObject;
    function GetContext: TValue;
    {$ENDREGION}

    ///	<summary>
    ///	  The view.
    ///	</summary>
    property View: TObject read GetView;

    ///	<summary>
    ///	  The context.
    ///	</summary>
    property Context: TValue read GetContext;
  end;

  ///	<summary>
  ///	  The event for the <see cref="IViewAware.ViewAttached" /> event.
  ///	</summary>
  TViewAttachedEvent = TEventHandler<IViewAttachedEventArgs>;

implementation

end.

unit DSharp.PresentationModel.HaveActiveItemIntf;

interface

uses
  Rtti;

type
  ///	<summary>
  ///	  Denotes an instance which maintains an active item.
  ///	</summary>
  IHaveActiveItem = interface
    ['{F849A40D-7075-4573-B804-F750C37AEF60}']

    {$REGION 'Property Accessors'}
    function GetActiveItem: TValue;
    procedure SetActiveItem(const Value: TValue);
    {$ENDREGION}

    ///	<summary>
    ///	  The currently active item.
    ///	</summary>
    property ActiveItem: TValue read GetActiveItem write SetActiveItem;
  end;

implementation

end.

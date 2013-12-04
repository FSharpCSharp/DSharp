unit DSharp.PresentationModel.ChildIntf;

interface

uses
  Rtti;

type
  ///	<summary>
  ///	  Denotes a node within a parent/child hierarchy.
  ///	</summary>
  IChild = interface
    ['{484A9CB4-2EF6-41B1-BD0D-9836E03909DF}']

    {$REGION 'Property Accessors'}
    function GetParent: TValue;
    procedure SetParent(const Value: TValue);
    {$ENDREGION}

    ///	<summary>
    ///	  Gets or Sets the Parent
    ///	</summary>
    property Parent: TValue read GetParent write SetParent;
  end;

  ///	<summary>
  ///	  Denotes a node within a parent/child hierarchy.
  ///	</summary>
  ///	<typeparam name="TParent">
  ///	  The type of parent.
  ///	</typeparam>
  IChild<TParent> = interface(IChild)

    {$REGION 'Property Accessors'}
    function GetParent: TParent;
    procedure SetParent(const Value: TParent);
    {$ENDREGION}

    ///	<summary>
    ///	  Gets or Sets the Parent
    ///	</summary>
    property Parent: TParent read GetParent write SetParent;
  end;

implementation

end.

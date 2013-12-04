unit DSharp.PresentationModel.ParentIntf;

interface

uses
  DSharp.Collections;

type
  ///	<summary>
  ///	  Interface used to define an object associated to a collection of
  ///	  children.
  ///	</summary>
  IParent = interface
    ['{6FA2F654-73C2-4C76-A640-271791F2FAC4}']

    ///	<summary>
    ///	  Gets the children.
    ///	</summary>
    ///	<returns>
    ///	  The collection of children.
    ///	</returns>
    function GetChildren: IEnumerable;
  end;

  ///	<summary>
  ///	  Interface used to define a specialized parent.
  ///	</summary>
  ///	<typeparam name="T">
  ///	  The type of children.
  ///	</typeparam>
  IParent<T> = interface(IParent)

    ///	<summary>
    ///	  Gets the children.
    ///	</summary>
    ///	<returns>
    ///	  The collection of children.
    ///	</returns>
    function GetChildren: IEnumerable<T>;
  end;

implementation

end.

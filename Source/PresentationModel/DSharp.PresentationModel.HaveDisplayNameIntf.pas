unit DSharp.PresentationModel.HaveDisplayNameIntf;

interface

type
  ///	<summary>
  ///	  Denotes an instance which has a display name.
  ///	</summary>
  IHaveDisplayName = interface
    ['{6C03B51A-7848-4545-B471-DCDE2DAC964C}']

    {$REGION 'Property Accessors'}
    function GetDisplayName: string;
    procedure SetDisplayName(const Value: string);
    {$ENDREGION}

    ///	<summary>
    ///	  Gets or Sets the Display Name
    ///	</summary>
    property DisplayName: string read GetDisplayName write SetDisplayName;
  end;

implementation

end.

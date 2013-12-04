unit ColorEvent;

interface

uses
  Graphics;

type
  ///	<summary>
  ///	  Model ColorEvent
  ///	</summary>
  TColorEvent = class
  strict private
    FColor: TColor;
  public
    constructor Create(Color: TColor);
    property Color: TColor read FColor;
  end;

implementation

constructor TColorEvent.Create(Color: TColor);
begin
  FColor := Color;
end;

end.

unit Sample1.Settings;

interface

uses
  DSharp.Core.PropertyChangedBase,
  Graphics;

type
  TSettings = class(TPropertyChangedBase)
  private
    FCaption: string;
    FColor: TColor;
    procedure SetCaption(const Value: string);
    procedure SetColor(const Value: TColor);
  public
    property Caption: string read FCaption write SetCaption;
    property Color: TColor read FColor write SetColor;
  end;

implementation

{ TSettings }

procedure TSettings.SetCaption(const Value: string);
begin
  FCaption := Value;
  DoPropertyChanged('Caption');
end;

procedure TSettings.SetColor(const Value: TColor);
begin
  FColor := Value;
  DoPropertyChanged('Color');
end;

end.

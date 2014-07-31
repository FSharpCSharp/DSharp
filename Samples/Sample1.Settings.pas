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
    FDate: TDate;
    procedure SetCaption(const Value: string);
    procedure SetColor(const Value: TColor);
  public
    property Caption: string read FCaption write SetCaption;
    property Color: TColor read FColor write SetColor;
    property Date: TDate read FDate write FDate;
  end;

implementation

{ TSettings }

procedure TSettings.SetCaption(const Value: string);
begin
  FCaption := Value;
  NotifyOfPropertyChange('Caption');
end;

procedure TSettings.SetColor(const Value: TColor);
begin
  FColor := Value;
  NotifyOfPropertyChange('Color');
end;

end.

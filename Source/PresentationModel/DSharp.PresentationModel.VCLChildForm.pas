unit DSharp.PresentationModel.VCLChildForm;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs;

type
  TChildForm = class(TForm)
  private
    FContent: TControl;
    procedure SetContent(const Value: TControl);
  public
    property Content: TControl read FContent write SetContent;
  end;

implementation

{$R *.dfm}

procedure TChildForm.SetContent(const Value: TControl);
begin
  FContent := Value;
  ClientHeight := FContent.Height;
  ClientWidth := FContent.Width;
  InsertControl(FContent);
end;

end.

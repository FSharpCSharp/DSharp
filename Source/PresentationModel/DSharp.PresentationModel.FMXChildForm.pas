unit DSharp.PresentationModel.FMXChildForm;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs;

type
  TChildForm = class(TForm)
  private
    FContent: TControl;
    procedure SetContent(const Value: TControl);
  public
    property Content: TControl read FContent write SetContent;
  end;

implementation

{$R *.fmx}

procedure TChildForm.SetContent(const Value: TControl);
begin
  FContent := Value;
  InsertComponent(FContent);
  FContent.Parent := Self;
end;

end.

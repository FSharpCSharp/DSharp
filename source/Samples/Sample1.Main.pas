unit Sample1.Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,
  System.Bindings.Controls;

type
  TMainForm = class(TForm)
    Edit1: TEdit;
    Edit2: TEdit;
    ColorBox1: TColorBox;
    procedure FormCreate(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  System.Bindings;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  TBinding.Create(Edit1, 'Text', Edit2, 'Text');
  TBinding.Create(Self, 'Color', ColorBox1, 'Selected', bmOneWayToSource);
end;

end.

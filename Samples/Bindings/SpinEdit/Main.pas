unit Main;

interface

(*
  On http://docwiki.embarcadero.com/RADStudio/XE2/en/Creating_the_Application_and_Placing_the_Visual_Objects
  you can see an example on how to use LiveBindings in a VCL application in Delphi XE2.

  However it actually requires lots of things to do and in fact you end up having
  more code to write than doing it the "traditional way". You actually have to
  implement the OnChange event for the SpinEdits - so where is the point actually
  using bindings?!

  In this sample you can see how this exact same example is done with
  DSharp Bindings and how you can benefit from using them.

  If you are using Delphi XE2 feel free to compare both possibilities.

  DSharp bindings work in Delphi 2010 and later.
*)

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls, Grids, DSharp.Bindings.VCLControls,
  DSharp.Bindings, Spin,
  // the following unit is added manually (needs to be after Spin.pas)
  // it's not included in the DSharp Bindings package
  // because if would cause requiring the samples package
  DSharp.Bindings.VCLControls.SpinEdit;

type
  TForm1 = class(TForm)
    seTop: TSpinEdit;
    seLeft: TSpinEdit;
    seWidth: TSpinEdit;
    seHeight: TSpinEdit;
    lblTop: TLabel;
    lblLeft: TLabel;
    lblWidth: TLabel;
    lblHeight: TLabel;
    pnlPicture: TPanel;
    Image1: TImage;
    BindingGroup1: TBindingGroup;
    procedure FormCreate(Sender: TObject);
  private
    procedure ResizeImage(Sender: TObject);
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

type
  // Yeah I know, a hack to access protected member,
  // but hey why manually triggering the repaint when we can use the resize event?!
  TImageHack = class(ExtCtrls.TImage);

procedure TForm1.FormCreate(Sender: TObject);
begin
  TImageHack(Image1).OnResize := ResizeImage;
  // trigger it manually to paint the image when the application starts
  ResizeImage(nil);
end;

procedure TForm1.ResizeImage(Sender: TObject);
var
  I, J: Integer;
  Bitmap: TBitmap;
begin
  Bitmap := TBitmap.Create;
  Bitmap.Width := Image1.Width;
  Bitmap.Height := Image1.Height;

  for I := 0 to Bitmap.Width do
    for J := 0 to Bitmap.Height do
      Bitmap.Canvas.Pixels[I, J] := RGB(I, J, 128);

  Image1.Picture.Bitmap := Bitmap;
  Bitmap.Free;
end;

end.

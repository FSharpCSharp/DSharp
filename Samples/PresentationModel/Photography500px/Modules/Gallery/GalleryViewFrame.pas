unit GalleryViewFrame;

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
  Dialogs,
  StdCtrls,
  Vcl.ExtCtrls,
  DSharp.PresentationModel,
  DSharp.Bindings.VCLControls;

type
  ///	<summary>
  ///	  View for GalleryView
  ///	</summary>
  TGalleryView = class(TFrame)
    Panel1: TPanel;
    Label1: TLabel;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    ScrollBox1: TScrollBox;
    PhotoCollection: TFlowPanel;
  private
    { Private declarations }
  protected
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint)
      : Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint)
      : Boolean; override;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

function TGalleryView.DoMouseWheelDown(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  Result := inherited;
  if not Result then
  begin
    ScrollBox1.Perform(WM_HSCROLL, 1, 0);
    Result := True;
  end;
end;

function TGalleryView.DoMouseWheelUp(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  Result := inherited;
  if not Result then
  begin
    if ScrollBox1.HorzScrollBar.Position > 0 then
      ScrollBox1.Perform(WM_HSCROLL, 0, 0);
    Result := True;
  end;
end;

initialization

TGalleryView.ClassName;

end.

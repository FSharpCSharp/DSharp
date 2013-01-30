unit MainViewForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ExtCtrls, DSharp.Bindings.VCLControls;

type
  TMainView = class(TForm)
    Content: TPageControl;
    Panel1: TPanel;
    AddDetail: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainView: TMainView;

implementation

{$R *.dfm}

initialization
  TMainView.ClassName;

end.

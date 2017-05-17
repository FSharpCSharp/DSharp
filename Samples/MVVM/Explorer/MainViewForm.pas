unit MainViewForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, DSharp.Bindings.VCLControls;

type
  TMainView = class(TForm)
    Navigation: TPanel;
    Splitter: TSplitter;
    WorkingArea: TPanel;
  end;

implementation

{$R *.dfm}

initialization
  TMainView.ClassName;

end.

unit DetailViewFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, DSharp.Bindings.VclControls;

type
  TDetailView = class(TFrame)
    Close: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

initialization
  TDetailView.ClassName;

end.

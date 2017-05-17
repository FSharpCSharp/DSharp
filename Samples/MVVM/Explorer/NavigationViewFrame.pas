unit NavigationViewFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, DSharp.Bindings.VCLControls;

type
  TNavigationView = class(TFrame)
    Elements: TListBox;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

initialization
  TNavigationView.ClassName;

end.

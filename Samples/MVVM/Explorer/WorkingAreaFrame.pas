unit WorkingAreaFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls;

type
  TWorkingAreaView = class(TFrame)
    Content: TMemo;
  end;

implementation

{$R *.dfm}

initialization
  TWorkingAreaView.ClassName;

end.

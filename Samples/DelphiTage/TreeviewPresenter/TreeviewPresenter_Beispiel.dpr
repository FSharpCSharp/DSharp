program TreeviewPresenter_Beispiel;

uses
  Forms,
  Main in 'Main.pas' {Form7},
  DSharp.Core.XmlSerialization in '..\..\..\Source\Core\DSharp.Core.XmlSerialization.pas',
  DSharp.Core.XmlSerialization.XmlReader in '..\..\..\Source\Core\DSharp.Core.XmlSerialization.XmlReader.pas',
  DSharp.Core.XmlSerialization.XmlSerializer in '..\..\..\Source\Core\DSharp.Core.XmlSerialization.XmlSerializer.pas',
  DSharp.Core.XmlSerialization.XmlWriter in '..\..\..\Source\Core\DSharp.Core.XmlSerialization.XmlWriter.pas',
  Customer in 'Customer.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm7, Form7);
  Application.Run;
end.

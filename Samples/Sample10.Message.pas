unit Sample10.Message;

interface

uses
  System.ComponentModel.Composition,
  Sample10.Contracts;

type
//  [Export('Message')]
//  TSimpleHello = class

  TSimpleHello = class(TInterfacedObject, IMessage)
  private
    FText: string;
  public
    function ToString: string; override;
    [Import('Text')]
    property Text: string read FText write FText;
  end;

  TSimpleHola = class(TInterfacedObject, IMessage)
  public
    function ToString: string; override;
  end;

  TTextProvider = class
  private
    function GetText: string;
  public
    [Export('Text')]
    property Text: string read GetText;
  end;

implementation

{ TSimpleHello }

function TSimpleHello.ToString: string;
begin
//  Result := 'Hello World!';
  Result := FText;
end;

{ TSimpleHola }

function TSimpleHola.ToString: string;
begin
  Result := 'Hola mundo';
end;

{ TTextProvider }

function TTextProvider.GetText: string;
begin
  Result := 'Bonjour tout le monde';
end;

initialization
  TSimpleHello.ClassName;
  TSimpleHola.ClassName;
  TTextProvider.ClassName;

end.

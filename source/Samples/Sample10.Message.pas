unit Sample10.Message;

interface

uses
  System.ComponentModel.Composition,
  Sample10.Contracts;

type
  // step 1
//  [Export('Message')]
//  TSimpleHello = class

  // step 2
  TSimpleHello = class(TInterfacedObject, IMessage)

  public
    function ToString: string; override;
  end;

  // step 3
  TSimpleHola = class(TInterfacedObject, IMessage)
  public
    function ToString: string; override;
  end;

  TSimpleHello2 = class(TInterfacedObject, IMessage)
  private
    FText: string;
  public
    function ToString: string; override;
    [Import('Text')]
    property Text: string read FText write FText;
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
  Result := 'Hello World!';
end;

{ TSimpleHola }

function TSimpleHola.ToString: string;
begin
  Result := 'Hola';
end;

{ TSimpleHello2 }

function TSimpleHello2.ToString: string;
begin
  Result := FText;
end;

{ TTextProvider }

function TTextProvider.GetText: string;
begin
  Result := 'Bonjour!';
end;

initialization
  TSimpleHello.ClassName;
  TSimpleHola.ClassName;
  TSimpleHello2.ClassName;
  TTextProvider.ClassName;

end.

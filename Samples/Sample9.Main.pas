unit Sample9.Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TMainForm = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    procedure PropertyChanged(Sender: TObject);
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  DSharp.Core.Properties;

{$IF COMPILERVERSION < 22}{$Message Warn 'Feature in this sample is not supported in Delphi 2010'}{$IFEND}
type
  TPerson = class
  private
    FName: TField<string>;
  public
    constructor Create(AName: string);
    destructor Destroy; override;
    property Name: TProperty<string> read FName.Value write FName.Value;
  end;

function SayHello(AProp: TProperty<string>): string;
begin
  Result := Format('"Hello, my name is %s!"', [AProp.Value]);
end;

procedure TMainForm.Button1Click(Sender: TObject);
var
  person1, person2: TPerson;
  prop: TProperty<string>;

  func: TFunc<string>;
begin
  Memo1.Lines.Add('creating person1 with name "joe"');
  person1 := TPerson.Create('Joe');
  person1.Name.OnChange.Add(PropertyChanged);
  Memo1.Lines.Add('Person1: ' + SayHello(person1.Name));

  Memo1.Lines.Add('getting property reference of person1');
  prop := person1.Name;

  Memo1.Lines.Add('renaming person1 from "joe" to "bob"');
  person1.Name := 'bob';
  Memo1.Lines.Add('value of property reference: ' + prop.Value);

  Memo1.Lines.Add('creating person2 with name "jane"');
  person2 := TPerson.Create('jane');
  person2.Name.OnChange.Add(PropertyChanged);
  Memo1.Lines.Add('Person2: ' + SayHello(person2.Name));

  Memo1.Lines.Add('assigning person2 the property of person1');
  person2.Name := person1.Name;
  Memo1.Lines.Add('Person2: ' + SayHello(person2.Name));

  Memo1.Lines.Add('getting property reference of person2');
  prop := person2.Name;
  Memo1.Lines.Add('renaming person1 from "bob" to "joe"');
  person1.Name := 'joe';
  Memo1.Lines.Add('Person1: ' + SayHello(person1.Name));
  Memo1.Lines.Add('Person2: ' + SayHello(person2.Name));

  func := function: string begin Result := prop.Value end;

  Memo1.Lines.Add('getting value from property reference: ' + func);
  Memo1.Lines.Add('setting value on property reference to "charlie"');
  // important!
  // don't use implicit operator here, it will break the prop reference
  // because of creating a new record that has no chance to save the values
  // use Value property for assignment instead
  prop.Value := 'charlie';
  Memo1.Lines.Add('getting value from property reference: ' + func);
  Memo1.Lines.Add('Person2: ' + SayHello(person2.Name));

  person1.Free();
  person2.Free();
end;

{ TPerson }

constructor TPerson.Create(AName: string);
begin
  FName.Initialize(Self);
  FName := AName;
end;

procedure TMainForm.PropertyChanged(Sender: TObject);
begin
  Memo1.Lines.Add('property changed to: ' + (Sender as TPerson).Name);
end;

destructor TPerson.Destroy;
begin
  FName.Finalize;
  inherited;
end;

initialization
  ReportMemoryLeaksOnShutdown := True;

end.

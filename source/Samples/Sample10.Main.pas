unit Sample10.Main;

interface

uses
  System.ComponentModel.Composition,
  Sample10.Contracts;

type
  TMain = class
  private
//    FMsg: TObject;
//    FMsg: IMessage;
    FMsgs: TArray<IMessage>;
  public
    procedure Run;

//    [Import('Message)]
//    property Msg: TObject read FMsg write FMsg;

//    [Import]
//    property Msg: IMessage read FMsg write FMsg;

    [ImportMany]
    property Msgs: TArray<IMessage> read FMsgs write FMsgs;
  end;

implementation

{ TMain }

procedure TMain.Run;
var
  m: IMessage;

begin
//  Writeln(Msg.ToString);

//  Writeln(Msg.ToString);

  // step 3
  for m in FMsgs do
    Writeln(m.ToString);
end;

end.

unit Sample10.Main;

interface

uses
  System.ComponentModel.Composition,
  Sample10.Contracts;

type
  TMain = class
  private
    // step 1
//    FMsg: TObject;

    // step 2
//    FMsg: IMessage;

    // step 3
    FMsgs: TArray<IMessage>;
  public
    procedure Run;

    // step 1
//    [Import('Message)]
//    property Msg: TObject read FMsg write FMsg;

    // step 2
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
  // step 1
//  Writeln(Msg.ToString);

  // step 2
//  Writeln(Msg.ToString);

  // step 3
  for m in FMsgs do
    Writeln(m.ToString);

end;

end.

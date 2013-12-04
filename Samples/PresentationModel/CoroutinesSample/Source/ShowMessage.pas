unit ShowMessage;

interface

uses
  DSharp.PresentationModel;

type
  TShowMessage = class(TResultBase)
  private
    FMessage: string;
  public
    constructor Create(AMessage: string);
    procedure Execute(Context: IActionExecutionContext); override;
  end;

implementation

uses
  Dialogs,
  SysUtils;

constructor TShowMessage.Create(AMessage: string);
begin
  FMessage := AMessage;
end;

procedure TShowMessage.Execute(Context: IActionExecutionContext);
begin
  Dialogs.ShowMessage(Format( //
    '%s' + sLineBreak + sLineBreak + //
    'Method=%s' + sLineBreak + //
    'Sender=%s' + sLineBreak + //
    'Target=%s' + sLineBreak + //
    'View=%s', [FMessage, Context.Method.ToString, Context.Sender.ToString,
    Context.Target.ToString, Context.View.ToString]));

  Completed.Invoke(Self, TResultCompletionEventArgs.Create);
end;

end.

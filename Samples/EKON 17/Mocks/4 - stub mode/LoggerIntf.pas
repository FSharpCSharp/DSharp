unit LoggerIntf;

interface

type
  ILogger = interface(IInvokable)
    procedure LogMessage(const AMessage: string);
  end;

implementation

end.

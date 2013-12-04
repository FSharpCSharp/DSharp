unit Commands;

interface

uses
  DSharp.PresentationModel.EventAggregatorIntf;

type
  ICustomerMessage = interface
    ['{9AD24B62-88FA-4103-BEF7-FC665A592AB6}']
    function GetMessage: string;
    property Message: string read GetMessage;
  end;

  ICustomerCreated = interface(ICustomerMessage)
    ['{62CA459F-4B08-4EA3-968B-E37D4C6E650B}']
    function GetCreated: string;
    property Created: string read GetCreated;
  end;

  ICustomerDeleted = interface(ICustomerMessage)
    ['{7D6DFFD2-1832-43C7-8EBA-F7638428DEA1}']
    function GetDeleted: string;
    property Deleted: string read GetDeleted;
  end;

  TCustomerMessage = class(TInterfacedObject, ICustomerCreated,
    ICustomerDeleted)
  strict private
    FMessage: string;
    FCreated: string;
    FDeleted: string;
    function GetMessage: string;
    function GetCreated: string;
    function GetDeleted: string;
  public
    constructor Create(AMessage: string; ACreated: string; ADeleted: string);
  end;

  TLogMessage = class
  strict private
    FMessage: string;
    function GetMessage: string;
  public
    constructor Create(AMessage: string);
    function ToString: string; override;
    property Message: string read GetMessage;
  end;

  TCommandA = class(TInterfacedObject)
  strict private
    FMessage: string;
  public
    constructor Create(AMessage: string);
    function ToString: string; override;
  end;

  TCommandB = class(TInterfacedObject)
  strict private
    FMessage: string;
  public
    constructor Create(AMessage: string);
    function ToString: string; override;
  end;

var
  EventAggregator: IEventAggregator;

implementation

uses
  DSharp.PresentationModel.EventAggregator;

{ TCustomerMessage }

constructor TCustomerMessage.Create(AMessage: string; ACreated: string;
  ADeleted: string);
begin
  FMessage := AMessage;
  FCreated := ACreated;
  FDeleted := ADeleted;
end;

function TCustomerMessage.GetCreated: string;
begin
  Result := FCreated;
end;

function TCustomerMessage.GetDeleted: string;
begin
  Result := FDeleted;
end;

function TCustomerMessage.GetMessage: string;
begin
  Result := FMessage;;
end;

{ TLogMessage }

constructor TLogMessage.Create(AMessage: string);
begin
  FMessage := AMessage;
end;

function TLogMessage.GetMessage: string;
begin
  Result := FMessage;;
end;

function TLogMessage.ToString: string;
begin
  Result := FMessage;
end;

{ TCommandA }

constructor TCommandA.Create(AMessage: string);
begin
  FMessage := AMessage;
end;

function TCommandA.ToString: string;
begin
  Result := FMessage;
end;

{ TCommandB }

constructor TCommandB.Create(AMessage: string);
begin
  FMessage := AMessage;
end;

function TCommandB.ToString: string;
begin
  Result := FMessage;
end;

initialization

EventAggregator := TEventAggregator.Create;

end.

unit DSharp.PresentationModel.VCLWindowConductor;

interface

uses
  Forms,
  DSharp.Core.EventArgs,
  DSharp.PresentationModel;

type
  TWindowConductor = class(TComponent)
  private
    FActuallyClosing: Boolean;
    FDeactivated: TDeactivationEvent;
    FDeactivateFromViewModel: Boolean;
    FDeactivatingFromView: Boolean;
    FModel: TObject;
    FView: TForm;
    procedure Closed(Sender: TObject; var Action: TCloseAction);
    procedure Closing(ASender: TObject; var ACanClose: Boolean);
    procedure Deactivated(Sender: TObject; Args: IDeactivationEventArgs);
  public
    constructor Create(AModel: TObject; AWindow: TForm); reintroduce;
  end;

implementation

uses
  DSharp.Core.Editable,
  DSharp.Core.Validations,
  SysUtils;

{ TWindowConductor }

constructor TWindowConductor.Create(AModel: TObject; AWindow: TForm);
var
  LActivatable: IActivate;
  LDeactivatable: IDeactivate;
  LEditable: IEditable;
  LGuard: IGuardClose;
begin
  inherited Create(AWindow);

  FModel := AModel;
  FView := AWindow;

  if Supports(AModel, IActivate, LActivatable) then
  begin
    LActivatable.Activate();
  end;

  if Supports(AModel, IDeactivate, LDeactivatable) then
  begin
    FView.OnClose := Closed;
    FDeactivated := Deactivated;
    LDeactivatable.Deactivated.Add(FDeactivated);
  end;

  if Supports(AModel, IGuardClose, LGuard) then
  begin
    FView.OnCloseQuery := Closing;
  end;

  if Supports(FModel, IEditable, LEditable) then
  begin
    LEditable.BeginEdit();
  end;
end;

procedure TWindowConductor.Closed(Sender: TObject; var Action: TCloseAction);
var
  LDeactivatable: IDeactivate;
  LEditable: IEditable;
begin
  Action := TCloseAction.caFree;

  FView.OnClose := nil;
  FView.OnCloseQuery := nil;

  if Supports(FModel, IEditable, LEditable) then
  begin
    case FView.ModalResult of
      mrOk:
        LEditable.EndEdit();
    else
      LEditable.CancelEdit();
    end;
  end;

  if FDeactivateFromViewModel then
    Exit;

  if Supports(FModel, IDeactivate, LDeactivatable) then
  begin
    FDeactivatingFromView := True;
    LDeactivatable.Deactivate(True);
    FDeactivatingFromView := False;
  end;
end;

procedure TWindowConductor.Closing(ASender: TObject; var ACanClose: Boolean);
var
  LGuard: IGuardClose;
  LRunningAsync: Boolean;
  LShouldEnd: Boolean;
  LCanClose: Boolean;
  LValidatable: IValidatable;
begin
  if not ACanClose then
    Exit;

  Supports(FModel, IGuardClose, LGuard);

  if Assigned(LGuard) then
  begin
    LGuard.ModalResult := FView.ModalResult;
  end;

  if FView.ModalResult = mrOk then
  begin
    if Supports(FModel, IValidatable, LValidatable) then
    begin
      LValidatable.Validate();
    end;
  end;

  if FActuallyClosing then
  begin
    FActuallyClosing := False;
    Exit;
  end;

  LRunningAsync := False;
  LShouldEnd := False;

  if Assigned(LGuard) then
  begin
    LGuard.CanClose(
      procedure(CanClose: Boolean)
      begin
        Execute.OnUIThread(
          procedure
          begin
            if LRunningAsync and CanClose then
            begin
              FActuallyClosing := True;
              FView.Close();
            end
            else
            begin
              LCanClose := CanClose;
            end;

            LShouldEnd := True;
          end);
      end);
  end;

  ACanClose := LCanClose;

  if LShouldEnd then
    Exit;

  LRunningAsync := True;
  ACanClose := False;
end;

procedure TWindowConductor.Deactivated(Sender: TObject;
Args: IDeactivationEventArgs);
var
  LDeactivatable: IDeactivate;
begin
  if not Args.WasClosed then
    Exit;

  if Supports(FModel, IDeactivate, LDeactivatable) then
  begin
    LDeactivatable.Deactivated.Remove(FDeactivated);
  end;

  if FDeactivatingFromView then
    Exit;

  FDeactivateFromViewModel := True;
  FActuallyClosing := True;
  FView.Close();
  FActuallyClosing := False;
  FDeactivateFromViewModel := False;
end;

end.

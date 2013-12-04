unit DSharp.PresentationModel.ConductorWithCollectionOneActive.Tests;

interface

implementation

uses
  SysUtils,
  TestFramework,
  DSharp.Collections,
  DSharp.PresentationModel,
  DSharp.PresentationModel.ConductorWithCollectionOneActive;

type
  TConductorWithCollectionOneActiveTestCase = class(TTestCase)
  published
    procedure AddedItemAppearsInChildren;
    procedure ParentItemIsSetOnAddedConductedItem;
    procedure ParentItemIsSetOnReplacedConductedItem;
    procedure ChildrenAreActivatedIfConductorIsActive;
    procedure CanCloseIsTrueWhenItemsAreClosable;
  end;

  TStateScreen = class(TScreen)
  private
    FIsClosable: Boolean;
    FIsClosed: Boolean;
  protected
    procedure OnDeactivate(Close: Boolean); override;
  public
    procedure CanClose(Callback: TProc<Boolean>); override;
    property IsClosable: Boolean read FIsClosable write FIsClosable;
    property IsClosed: Boolean read FIsClosed;
  end;

  { TConductorWithCollectionOneActiveTestCase }

procedure TConductorWithCollectionOneActiveTestCase.AddedItemAppearsInChildren;
var
  LConductor: TConductorCollectionOneActive<IScreen>;
  LConducted: TScreen;
begin
  LConductor := TConductorCollectionOneActive<IScreen>.Create;
  LConducted := TScreen.Create;
  LConductor.Items.Add(LConducted);
  CheckTrue(LConductor.GetChildren.Contains(LConducted));
  LConductor.Free;
end;

procedure TConductorWithCollectionOneActiveTestCase.ParentItemIsSetOnAddedConductedItem;
var
  LConductor: TConductorCollectionOneActive<IScreen>;
  LConducted: TScreen;
begin
  LConductor := TConductorCollectionOneActive<IScreen>.Create;
  LConducted := TScreen.Create;
  LConductor.Items.Add(LConducted);
  CheckTrue(LConductor = LConducted.Parent.AsType < TConductorCollectionOneActive < IScreen >> );
  LConductor.Free;
end;

procedure TConductorWithCollectionOneActiveTestCase.ParentItemIsSetOnReplacedConductedItem;
var
  LConductor: TConductorCollectionOneActive<IScreen>;
  LOriginalConducted: TScreen;
  LNewConducted: TScreen;
begin
  LConductor := TConductorCollectionOneActive<IScreen>.Create;
  LOriginalConducted := TScreen.Create;
  LConductor.Items.Add(LOriginalConducted);
  LNewConducted := TScreen.Create;
  LConductor.Items[0] := LNewConducted;
  CheckTrue(LConductor = LNewConducted.Parent.AsType < TConductorCollectionOneActive < IScreen >> );
  LConductor.Free;
end;

procedure TConductorWithCollectionOneActiveTestCase.ChildrenAreActivatedIfConductorIsActive;
var
  LConductor: TConductorCollectionOneActive<IScreen>;
  LConducted: TScreen;
begin
  LConductor := TConductorCollectionOneActive<IScreen>.Create;
  LConducted := TScreen.Create;
  LConductor.Items.Add(LConducted);
  (LConductor as IActivate).Activate;
  LConductor.ActivateItem(LConducted);
  CheckTrue(LConducted.IsActive);
  CheckTrue((LConducted as IScreen) = LConductor.ActiveItem);
  LConductor.Free;
end;

procedure TConductorWithCollectionOneActiveTestCase.CanCloseIsTrueWhenItemsAreClosable;
var
  LConductor: TConductorCollectionOneActive<IScreen>;
  LConducted: TStateScreen;
begin
  LConductor := TConductorCollectionOneActive<IScreen>.Create;
  LConducted := TStateScreen.Create;
  LConducted.IsClosable := True;
  LConductor.Items.Add(LConducted);
  (LConductor as IActivate).Activate;
  LConductor.CanClose(
    procedure(CanClose: Boolean)
    begin
      CheckTrue(CanClose);
    end);
  CheckFalse(LConducted.IsClosed);
  LConductor.Free;
  LConducted.Free;
end;

{ TStateScreen }

procedure TStateScreen.CanClose(Callback: TProc<Boolean>);
begin
  Callback(IsClosable);
end;

procedure TStateScreen.OnDeactivate(Close: Boolean);
begin
  inherited OnDeactivate(Close);
  FIsClosed := Close;
end;

initialization

RegisterTest('DSharp.PresentationModel.ConductorWithCollectionOneActive', TConductorWithCollectionOneActiveTestCase.Suite);

end.

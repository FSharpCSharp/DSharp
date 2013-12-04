unit DSharp.Bindings.FMXControls;

interface

uses
  Classes,
  FMX.StdCtrls,
  FMX.Edit,
  FMX.Memo,
  FMX.Layouts,
  FMX.Types,
  DSharp.Bindings.Notifications,
  DSharp.Bindings.Collections,
  DSharp.Bindings.CollectionView;

type
  TCheckBox = class(FMX.StdCtrls.TCheckBox, INotifyPropertyChanged)
  private
    FNotifyPropertyChanged: INotifyPropertyChanged;
    property NotifyPropertyChanged: INotifyPropertyChanged
      read FNotifyPropertyChanged implements INotifyPropertyChanged;
  protected
    procedure Click; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TEdit = class(FMX.Edit.TEdit, INotifyPropertyChanged)
  private
    FNotifyPropertyChanged: INotifyPropertyChanged;
    property NotifyPropertyChanged: INotifyPropertyChanged
      read FNotifyPropertyChanged implements INotifyPropertyChanged;
  protected
    procedure DoChangeTracking; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TGroupBox = class(FMX.StdCtrls.TGroupBox, INotifyPropertyChanged)
  private
    FBindingSource: TObject;
    FNotifyPropertyChanged: INotifyPropertyChanged;
    procedure SetBindingSource(const Value: TObject);
    property NotifyPropertyChanged: INotifyPropertyChanged
      read FNotifyPropertyChanged implements INotifyPropertyChanged;
  public
    constructor Create(AOwner: TComponent); override;
    property BindingSource: TObject read FBindingSource write SetBindingSource;
  end;

  TLayout = class(FMX.Layouts.TLayout, INotifyPropertyChanged)
  private
    FBindingSource: TObject;
    FNotifyPropertyChanged: INotifyPropertyChanged;
    procedure SetBindingSource(const Value: TObject);
    property NotifyPropertyChanged: INotifyPropertyChanged
      read FNotifyPropertyChanged implements INotifyPropertyChanged;
  public
    constructor Create(AOwner: TComponent); override;
    property BindingSource: TObject read FBindingSource write SetBindingSource;
  end;

  TMemo = class(FMX.Memo.TMemo, INotifyPropertyChanged)
  private
    FNotifyPropertyChanged: INotifyPropertyChanged;
    property NotifyPropertyChanged: INotifyPropertyChanged
      read FNotifyPropertyChanged implements INotifyPropertyChanged;
  protected
    procedure SetText(const Value: string); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TPanel = class(FMX.StdCtrls.TPanel, INotifyPropertyChanged)
  private
    FBindingSource: TObject;
    FNotifyPropertyChanged: INotifyPropertyChanged;
    procedure SetBindingSource(const Value: TObject);
    property NotifyPropertyChanged: INotifyPropertyChanged
      read FNotifyPropertyChanged implements INotifyPropertyChanged;
  public
    constructor Create(AOwner: TComponent); override;
    property BindingSource: TObject read FBindingSource write SetBindingSource;
  end;

  TScrollBox = class(FMX.Layouts.TScrollBox, INotifyPropertyChanged,
    ICollectionView)
  private
    FNotifyPropertyChanged: INotifyPropertyChanged;
    FView: TCollectionView;
    property NotifyPropertyChanged: INotifyPropertyChanged
      read FNotifyPropertyChanged implements INotifyPropertyChanged;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property View: TCollectionView read FView implements ICollectionView;
  end;

implementation

uses
  DSharp.Bindings.CollectionView.Adapters;

{ TCheckBox }

constructor TCheckBox.Create(AOwner: TComponent);
begin
  inherited;
  FNotifyPropertyChanged := TNotifyPropertyChanged.Create(Self);
end;

procedure TCheckBox.Click;
begin
  inherited;
  NotifyPropertyChanged.DoPropertyChanged('Checked');
  NotifyPropertyChanged.DoPropertyChanged('State');
end;

{ TEdit }

constructor TEdit.Create(AOwner: TComponent);
begin
  inherited;
  FNotifyPropertyChanged := TNotifyPropertyChanged.Create(Self);
end;

procedure TEdit.DoChangeTracking;
begin
  inherited;
  NotifyPropertyChanged.DoPropertyChanged('Text');
end;

{ TGroupBox }

constructor TGroupBox.Create(AOwner: TComponent);
begin
  inherited;
  FNotifyPropertyChanged := TNotifyPropertyChanged.Create(Self);
end;

procedure TGroupBox.SetBindingSource(const Value: TObject);
begin
  FBindingSource := Value;
  NotifyPropertyChanged.DoPropertyChanged('BindingSource');
end;

{ TLayout }

constructor TLayout.Create(AOwner: TComponent);
begin
  inherited;
  FNotifyPropertyChanged := TNotifyPropertyChanged.Create(Self);
end;

procedure TLayout.SetBindingSource(const Value: TObject);
begin
  FBindingSource := Value;
  NotifyPropertyChanged.DoPropertyChanged('BindingSource');
end;

{ TMemo }

constructor TMemo.Create(AOwner: TComponent);
begin
  inherited;
  FNotifyPropertyChanged := TNotifyPropertyChanged.Create(Self);
end;

procedure TMemo.SetText(const Value: string);
begin
  inherited;
  NotifyPropertyChanged.DoPropertyChanged('Text');
end;

{ TPanel }

constructor TPanel.Create(AOwner: TComponent);
begin
  inherited;
  FNotifyPropertyChanged := TNotifyPropertyChanged.Create(Self);
end;

procedure TPanel.SetBindingSource(const Value: TObject);
begin
  FBindingSource := Value;
  NotifyPropertyChanged.DoPropertyChanged('BindingSource');
end;

{ TScrollBox }

constructor TScrollBox.Create(AOwner: TComponent);
begin
  inherited;
  FNotifyPropertyChanged := TNotifyPropertyChanged.Create(Self);
  FView := TCollectionViewAdapter.Create(Self);
end;

destructor TScrollBox.Destroy;
begin
  FView.Free();
  inherited;
end;

end.

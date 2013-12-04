unit Main;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  SimpleViewFrame,
  ExtCtrls,
  DSharp.PresentationModel;

type
  TMainForm = class(TForm)
    ButtonFreeFrame: TButton;
    ButtonCreateFrame: TButton;
    ButtonChangeDataContext: TButton;
    PanelLeft: TPanel;
    PanelContent: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure ButtonFreeFrameClick(Sender: TObject);
    procedure ButtonCreateFrameClick(Sender: TObject);
    procedure ButtonChangeDataContextClick(Sender: TObject);
  private
    procedure CreateFrame;
  end;

var
  MainForm: TMainForm;

implementation

uses
  ViewExtensions,
  DSharp.PresentationModel.Extensions,
  DSharp.PresentationModel.InitializeComponent;

const
  CSimpleViewName = 'SimpleView';

  {$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  CreateFrame();
end;

procedure TMainForm.ButtonFreeFrameClick(Sender: TObject);
var
  LView: TControl;
begin
  LView := PanelContent.FindChildControl(CSimpleViewName);
  if Assigned(LView) then
    LView.Free;
end;

procedure TMainForm.ButtonCreateFrameClick(Sender: TObject);
begin
  CreateFrame();
end;

procedure TMainForm.ButtonChangeDataContextClick(Sender: TObject);
var
  LView: TControl;
begin
  LView := PanelContent.FindChildControl(CSimpleViewName);
  if Assigned(LView) then
  begin
    if LView.DataContext = Self then
      LView.DataContext := Sender
    else
      LView.DataContext := Self;
  end;
end;

procedure TMainForm.CreateFrame;
var
  LView: TControl;
begin
  if PanelContent.ControlCount > 0 then
    Exit;

  // Create View (IoC)
  LView := TSimpleView.Create(Self);
  LView.Name := CSimpleViewName;
  LView.Align := alClient;
  LView.DataContext := Self;

  // Explicitly call InitializeComponent to test initialization of dependency properties
  TInitializeComponent.Initialize(LView);

  // Load view into visual tree
  LView.Parent := PanelContent;
end;

end.

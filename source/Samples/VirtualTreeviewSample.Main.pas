unit VirtualTreeviewSample.Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls, System.Bindings.Controls.VCL,
  System.Bindings, System.Windows.TreeViewPresenter, VirtualTrees;

type
  TMain = class(TForm)
    VirtualStringTree1: TVirtualStringTree;
    TreeViewPresenter1: TTreeViewPresenter;
    BindingGroup1: TBindingGroup;
    Lastname: TLabeledEdit;
    Firstname: TLabeledEdit;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Main: TMain;

implementation

{$R *.dfm}

uses
  Collections.ObservableCollection,
  Sample5.Contact;

procedure TMain.FormCreate(Sender: TObject);
begin
  TreeViewPresenter1.ItemsSource := TObservableCollection<TObject>.Create();
  TreeViewPresenter1.ItemsSource.Add(TContact.Create('John', 'Doe'));
  TreeViewPresenter1.ItemsSource.Add(TContact.Create('Jane', 'Doe'));
end;

end.

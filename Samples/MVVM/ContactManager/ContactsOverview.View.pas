unit ContactsOverview.View;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, DSharp.Windows.ColumnDefinitions, ExtCtrls, StdCtrls, ComCtrls,
  Grids, DSharp.Bindings.VCLControls, DSharp.Bindings,
  DSharp.Windows.CustomPresenter, DSharp.Windows.TreeViewPresenter, Menus,
  ActnList, ImgList, VirtualTrees, ToolWin;

type
  TContactsOverviewView = class(TFrame)
    ToolBar1: TToolBar;
    ImageList: TImageList;
    ActionList: TActionList;
    btnNew: TToolButton;
    btnEdit: TToolButton;
    btnDelete: TToolButton;
    AddNewContact: TAction;
    EditContact: TAction;
    DeleteContact: TAction;
    PopupMenu: TPopupMenu;
    mniNew: TMenuItem;
    mniEdit: TMenuItem;
    mniDelete: TMenuItem;
    TreeImageList: TImageList;
    Contacts: TTreeViewPresenter;
    BindingGroup: TBindingGroup;
    ContactsTreeView: TVirtualStringTree;
  end;

implementation

{$R *.dfm}

initialization
  TContactsOverviewView.ClassName();

end.

unit ContactManager.Interfaces;

interface

uses
  Contact,
  DSharp.Aspects.Logging,
  DSharp.Collections,
  DSharp.Core.Lazy,
  DSharp.PresentationModel;

type

  [InheritedExport]
  [Logging]
  IContactDetailsViewModel = interface(IInvokable)
    ['{04FC1B4C-5B2A-4C41-BB84-39E69816F7D6}']
    function GetContact: TContact;
    procedure SetContact(const Value: TContact);

    property Contact: TContact read GetContact write SetContact;
  end;

  [InheritedExport]
  [Logging]
  IContactsOverviewViewModel = interface(IInvokable)
    ['{15AC3244-E2BB-42BE-AB15-61C2DF87FEB8}']
    function GetCanDeleteContact: Boolean;
    function GetCanEditContact: Boolean;
    function GetSelectedContact: TContact;
    procedure SetSelectedContact(const Value: TContact);

    procedure AddNewContact;
    procedure EditContact;
    procedure DeleteContact;

    property CanDeleteContact: Boolean read GetCanDeleteContact;
    property CanEditContact: Boolean read GetCanEditContact;
    property SelectedContact: TContact read GetSelectedContact
      write SetSelectedContact;
  end;

implementation

end.

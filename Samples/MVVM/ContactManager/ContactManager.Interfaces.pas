unit ContactManager.Interfaces;

interface

uses
  Contact,
  DSharp.Collections,
  DSharp.ComponentModel.Composition,
  DSharp.Core.Lazy;

type
  [InheritedExport]
  IContactDetailsViewModel = interface
    ['{04FC1B4C-5B2A-4C41-BB84-39E69816F7D6}']
    function GetContact: TContact;
    procedure SetContact(const Value: TContact);

    property Contact: TContact read GetContact write SetContact;
  end;

  [InheritedExport]
  IContactsOverviewViewModel = interface
    ['{15AC3244-E2BB-42BE-AB15-61C2DF87FEB8}']
    function GetCanDeleteContact: Boolean;
    function GetCanEditContact: Boolean;
    function GetContactDetails: Lazy<IContactDetailsViewModel>;
    function GetContacts: IList<TObject>;
    function GetSelectedContact: TContact;
    procedure SetContactDetails(const Value: Lazy<IContactDetailsViewModel>);
    procedure SetContacts(const Value: IList<TObject>);
    procedure SetSelectedContact(const Value: TContact);

    procedure AddNewContact;
    procedure EditContact;
    procedure DeleteContact;

    property Contacts: IList<TObject> read GetContacts write SetContacts;
    property SelectedContact: TContact read GetSelectedContact write SetSelectedContact;
    property ContactDetails: Lazy<IContactDetailsViewModel> read GetContactDetails
      write SetContactDetails;

    property CanDeleteContact: Boolean read GetCanDeleteContact;
    property CanEditContact: Boolean read GetCanEditContact;
  end;

implementation

end.

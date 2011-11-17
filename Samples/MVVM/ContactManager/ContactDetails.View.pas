unit ContactDetails.View;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls, Grids, DSharp.Bindings.VCLControls,
  DSharp.Bindings;

type
  TContactDetailsView = class(TFrame)
    Contact_Title: TEdit;
    Contact_Lastname: TEdit;
    Save: TButton;
    lblTitle: TLabel;
    Contact_Firstname: TEdit;
    lblLastname: TLabel;
    lblFirstname: TLabel;
    Contact_Phone: TEdit;
    Contact_Fax: TEdit;
    lblPhone: TLabel;
    Contact_Email: TEdit;
    lblFax: TLabel;
    lblEmail: TLabel;
    Contact_Address: TEdit;
    lblAddress: TLabel;
    lblZipcode: TLabel;
    lblCity: TLabel;
    lblCountry: TLabel;
    Contact_Zipcode: TEdit;
    Contact_City: TEdit;
    Contact_Country: TEdit;
    Cancel: TButton;
    BindingGroup: TBindingGroup;
    Contact_PreferedContact: TRadioGroup;
  end;

implementation

{$R *.dfm}

initialization
  TContactDetailsView.ClassName;

end.

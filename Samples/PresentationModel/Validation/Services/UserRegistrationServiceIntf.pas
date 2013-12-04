unit UserRegistrationServiceIntf;

interface

uses
  DSharp.PresentationModel;

type

  [InheritedExport]
  IUserRegistrationService = interface
    ['{CC1A978A-2D98-4B7F-9976-5D9EE2B7CEA4}']
    function IsUserNameAvailable(UserName: string): Boolean;
  end;

implementation

end.

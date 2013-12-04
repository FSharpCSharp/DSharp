unit Interfaces;

interface

uses
  DSharp.Core.Collections,
  DSharp.ComponentModel.Composition;

type

  [InheritedExport]
  IShell = interface
    ['{7C45BB4C-FD7F-47CC-90F6-31E0632AE21E}']
  end;

  [InheritedExport]
  IIdentitiesViewModel = interface
    ['{7A2F1A7D-89DE-480D-B126-82F05FE31858}']
  end;

  [InheritedExport]
  IAccountViewModel = interface
    ['{645082ED-5D35-4B3B-BC1E-6672DC7E3215}']
  end;

  [InheritedExport]
  IDocumentsViewModel = interface
    ['{8F0CA739-99E1-4F24-991B-0EDD5F9BEBB4}']
  end;

implementation

end.

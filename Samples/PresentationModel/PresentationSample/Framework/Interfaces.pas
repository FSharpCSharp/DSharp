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
  IHeaderViewModel = interface
    ['{9C9F8DE0-62B4-4C0C-838F-560A7978F5AF}']
  end;

  [InheritedExport]
  IMainViewModel = interface
    ['{A0911E9D-C5D4-42DC-B86E-EDABC819ADEC}']
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

  [InheritedExport]
  ISomeViewModel = interface
    ['{ACF31EFF-580F-4A55-8AE8-5DD2EBE4AC74}']
  end;

implementation

end.

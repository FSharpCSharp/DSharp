unit Interfaces;

interface

type
  /// <summary>
  /// Denotes an object that can create something
  /// </summary>
  ICreator = interface
    ['{8DDCAC96-D48D-4CD6-8BE3-9F7A75D4BC78}']
    procedure Make;
  end;

  /// <summary>
  /// Denotes how an application is structured
  /// </summary>
  {$SCOPEDENUMS ON} TApplicationStructure = (Custom, ViewModels, Features); {$SCOPEDENUMS OFF}

  /// <summary>
  /// Application settings
  /// </summary>
  ISettings = interface
    ['{F0500B2F-4178-4BBD-A979-70B600347DA0}']
    function GetModelFolder: string;
    function GetApplicationFolder: string;
    function GetApplicationInterfacesFileName: string;
    function GetApplicationName: string;
    function GetApplicationStructure: TApplicationStructure;
    function GetViewFolder(Name: string): string;
    function GetViewModelFolder(Name: string): string;
    property ModelFolder: string read GetModelFolder;
    property ApplicationFolder: string read GetApplicationFolder;
    property ApplicationInterfacesFileName: string read GetApplicationInterfacesFileName;
    property ApplicationName: string read GetApplicationName;
    property ApplicationStructure: TApplicationStructure read GetApplicationStructure;
    property ViewFolder[Name: string]: string read GetViewFolder;
    property ViewModelFolder[Name: string]: string read GetViewModelFolder;
  end;

implementation

end.

unit ShellViewForm;

interface

uses
  System.Classes,
  System.SysUtils,
  FMX.Controls,
  FMX.Edit,
  FMX.Forms,
  FMX.Layouts,
  FMX.Types,
  FMX.StdCtrls,
  DSharp.PresentationModel,
  DSharp.Bindings.FMXControls;

type
  /// <summary>
  /// View for ShellView
  /// </summary>
  TShellView = class(TForm)
    Container: TLayout;
    Tasks: TLayout;
    Todo: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{
dcc command line for "TasksFmx.dpr"
  c:\program files (x86)\embarcadero\rad studio\12.0\bin\dcc32.exe -$O- -$W+ --no-config -M -Q -TX.exe
  -AGenerics.Collections=System.Generics.Collections;Generics.Defaults=System.Generics.Defaults;WinTypes=Winapi.Windows;WinProcs=Winapi.Windows;
  DbiTypes=BDE;DbiProcs=BDE;DbiErrs=BDE -DDEBUG -E..\..\Build\Tasks\Win32\Debug -I"c:\program files (x86)\embarcadero\rad studio\12.0\lib\Win32\debug";
  ..\..\Source\PresentationModel;..\..\Source\Validation;..\..\External\DSharp\Aspects;..\..\External\DSharp\Bindings;..\..\External\DSharp\Collections;
  ..\..\External\DSharp\ComponentModel;..\..\External\DSharp\Core;..\..\External\DSharp\DelphiWebScript;..\..\External\DSharp\DevExpress;
  ..\..\External\DSharp\Interception;..\..\External\DSharp\Logging;..\..\External\DSharp\Testing;..\..\External\DSharp\Windows;
  C:\Users\developer\Versioned\Spring\Source\Base;C:\Users\developer\Versioned\Spring\Source\Base\Collections;
  C:\Users\developer\Versioned\Spring\Source\Base\Reflection;C:\Users\developer\Versioned\Spring\Source\Core\Container;
  C:\Users\developer\Versioned\Spring\Source\Core\Services;"c:\program files (x86)\embarcadero\rad studio\12.0\lib\Win32\release";
  "C:\Users\developer\Documents\RAD Studio\12.0\Imports";"c:\program files (x86)\embarcadero\rad studio\12.0\Imports";"C:\Users\Public\Documents\RAD
  Studio\12.0\Dcp";"c:\program files (x86)\embarcadero\rad studio\12.0\include";"C:\Program Files (x86)\FastReports\LibD19";"C:\Program Files
  (x86)\Raize\CS5\Lib\RS-XE5\Win32" -LE"C:\Users\Public\Documents\RAD Studio\12.0\Bpl" -LN"C:\Users\Public\Documents\RAD Studio\12.0\Dcp"
  -NU..\..\Build\Tasks\Win32\Debug -NSWinapi;System.Win;Data.Win;Datasnap.Win;Web.Win;Soap.Win;Xml.Win;Bde;System;Xml;Data;Datasnap;Web;Soap;Fmx;
  -O..\..\Source\PresentationModel;..\..\Source\Validation;..\..\External\DSharp\Aspects;..\..\External\DSharp\Bindings;
  ..\..\External\DSharp\Collections;..\..\External\DSharp\ComponentModel;..\..\External\DSharp\Core;..\..\External\DSharp\DelphiWebScript;
  ..\..\External\DSharp\DevExpress;..\..\External\DSharp\Interception;..\..\External\DSharp\Logging;..\..\External\DSharp\Testing;
  ..\..\External\DSharp\Windows;C:\Users\developer\Versioned\Spring\Source\Base;C:\Users\developer\Versioned\Spring\Source\Base\Collections;
  C:\Users\developer\Versioned\Spring\Source\Base\Reflection;C:\Users\developer\Versioned\Spring\Source\Core\Container;
  C:\Users\developer\Versioned\Spring\Source\Core\Services;"c:\program files (x86)\embarcadero\rad studio\12.0\lib\Win32\release";
  "C:\Users\developer\Documents\RAD Studio\12.0\Imports";"c:\program files (x86)\embarcadero\rad studio\12.0\Imports";"C:\Users\Public\Documents\RAD
  Studio\12.0\Dcp";"c:\program files (x86)\embarcadero\rad studio\12.0\include";"C:\Program Files (x86)\FastReports\LibD19";"C:\Program Files
  (x86)\Raize\CS5\Lib\RS-XE5\Win32" -R..\..\Source\PresentationModel;..\..\Source\Validation;..\..\External\DSharp\Aspects;
  ..\..\External\DSharp\Bindings;..\..\External\DSharp\Collections;..\..\External\DSharp\ComponentModel;..\..\External\DSharp\Core;
  ..\..\External\DSharp\DelphiWebScript;..\..\External\DSharp\DevExpress;..\..\External\DSharp\Interception;..\..\External\DSharp\Logging;
  ..\..\External\DSharp\Testing;..\..\External\DSharp\Windows;C:\Users\developer\Versioned\Spring\Source\Base;
  C:\Users\developer\Versioned\Spring\Source\Base\Collections;C:\Users\developer\Versioned\Spring\Source\Base\Reflection;
  C:\Users\developer\Versioned\Spring\Source\Core\Container;C:\Users\developer\Versioned\Spring\Source\Core\Services;"c:\program files
  (x86)\embarcadero\rad studio\12.0\lib\Win32\release";"C:\Users\developer\Documents\RAD Studio\12.0\Imports";"c:\program files (x86)\embarcadero\rad
  studio\12.0\Imports";"C:\Users\Public\Documents\RAD Studio\12.0\Dcp";"c:\program files (x86)\embarcadero\rad studio\12.0\include";"C:\Program Files
  (x86)\FastReports\LibD19";"C:\Program Files (x86)\Raize\CS5\Lib\RS-XE5\Win32" -U"c:\program files (x86)\embarcadero\rad studio\12.0\lib\Win32\debug";
  ..\..\Source\PresentationModel;..\..\Source\Validation;..\..\External\DSharp\Aspects;..\..\External\DSharp\Bindings;..\..\External\DSharp\Collections;
  ..\..\External\DSharp\ComponentModel;..\..\External\DSharp\Core;..\..\External\DSharp\DelphiWebScript;..\..\External\DSharp\DevExpress;
  ..\..\External\DSharp\Interception;..\..\External\DSharp\Logging;..\..\External\DSharp\Testing;..\..\External\DSharp\Windows;
  C:\Users\developer\Versioned\Spring\Source\Base;C:\Users\developer\Versioned\Spring\Source\Base\Collections;
  C:\Users\developer\Versioned\Spring\Source\Base\Reflection;C:\Users\developer\Versioned\Spring\Source\Core\Container;
  C:\Users\developer\Versioned\Spring\Source\Core\Services;"c:\program files (x86)\embarcadero\rad studio\12.0\lib\Win32\release";
  "C:\Users\developer\Documents\RAD Studio\12.0\Imports";"c:\program files (x86)\embarcadero\rad studio\12.0\Imports";"C:\Users\Public\Documents\RAD
  Studio\12.0\Dcp";"c:\program files (x86)\embarcadero\rad studio\12.0\include";"C:\Program Files (x86)\FastReports\LibD19";"C:\Program Files
  (x86)\Raize\CS5\Lib\RS-XE5\Win32" -V -VN -NB"C:\Users\Public\Documents\RAD Studio\12.0\Dcp" -NH"C:\Users\Public\Documents\RAD Studio\12.0\hpp"
  -NO..\..\Build\Tasks\Win32\Debug   TasksFmx.dpr
[dcc32 Error] E1026 File not found: 'ShellViewForm.dfm'
}
{$R *.FMX}

initialization

TShellView.ClassName;

end.

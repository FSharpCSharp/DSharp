unit Utils;

interface

function IsEmptyStr(const s: string): Boolean;
function IsValidEmail(const s: string): Boolean;

implementation

uses
  DSharp.Core.RegularExpressions,
  SysUtils;

function IsEmptyStr(const s: string): Boolean;
begin
  Result := Trim(s) = EmptyStr;
end;

function IsValidEmail(const s: string): Boolean;
const
  CRegex: string = '^([a-zA-Z0-9_\-\.]+)@((\[[0-9]{1,3}' +
    '\.[0-9]{1,3}\.[0-9]{1,3}\.)|(([a-zA-Z0-9\-]+\' +
    '.)+))([a-zA-Z]{2,5}|[0-9]{1,3})(\]?)$';
begin
  Result := IsEmptyStr(s) or TRegex.Create(CRegex).IsMatch(s);
end;

end.

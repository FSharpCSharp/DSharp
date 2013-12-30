unit DSharp.PresentationModel.RegExHelper;

interface

uses
  SysUtils;

type
  /// <summary>
  ///   Helper class for encoding strings to regular expression patterns
  /// </summary>
  TRegExHelper = class
  public const

    /// <summary>
    ///   Regular expression pattern for valid name
    /// </summary>
    NameRegEx = '[A-Za-z_]\w*';

    /// <summary>
    ///   Regular expression pattern for subnamespace (including dot)
    /// </summary>
    SubNamespaceRegEx = NameRegEx + '\.';

    /// <summary>
    ///   Regular expression pattern for namespace or namespace fragment
    /// </summary>
    NamespaceRegEx = '(' + SubNamespaceRegEx + ')*';

    /// <summary>
    ///   Creates a named capture group with the specified regular expression
    /// </summary>
    /// <param name="GroupName">
    ///   Name of capture group to create
    /// </param>
    /// <param name="RegEx">
    ///   Regular expression pattern to capture
    /// </param>
    /// <returns>
    ///   Regular expression capture group with the specified group name
    /// </returns>
    class function GetCaptureGroup(GroupName: string; RegEx: string): string;

    /// <summary>
    ///   Creates a capture group for a valid name regular expression pattern
    /// </summary>
    /// <param name="GroupName">
    ///   Name of capture group to create
    /// </param>
    /// <returns>
    ///   Regular expression capture group with the specified group name
    /// </returns>
    class function GetNameCaptureGroup(GroupName: string): string;

    /// <summary>
    ///   Creates a capture group for a namespace regular expression pattern
    /// </summary>
    /// <param name="GroupName">
    ///   Name of capture group to create
    /// </param>
    /// <returns>
    ///   Regular expression capture group with the specified group name
    /// </returns>
    class function GetNamespaceCaptureGroup(GroupName: string): string;

    /// <summary>
    ///   Converts a namespace (including wildcards) to a regular expression
    ///   string
    /// </summary>
    /// <param name="SrcNamespace">
    ///   Source namespace to convert to regular expression
    /// </param>
    /// <returns>
    ///   Namespace converted to a regular expression
    /// </returns>
    class function NamespaceToRegEx(SrcNamespace: string): string;
  end;

implementation

{ TRegExHelper }

class function TRegExHelper.GetCaptureGroup(GroupName, RegEx: string): string;
begin
  Result := Concat('(?<', GroupName, '>', RegEx, ')');
end;

class function TRegExHelper.GetNameCaptureGroup(GroupName: string): string;
begin
  Result := GetCaptureGroup(GroupName, NameRegEx);
end;

class function TRegExHelper.GetNamespaceCaptureGroup(GroupName: string): string;
begin
  Result := GetCaptureGroup(GroupName, NamespaceRegEx);
end;

class function TRegExHelper.NamespaceToRegEx(SrcNamespace: string): string;
var
  LNsEncoded: string;
begin
  // Need to escape the '.' as it's a special character in regular expression syntax
  LNsEncoded := StringReplace(SrcNamespace, '.', '\.', [rfReplaceAll]);

  // Replace '*' wildcard with regular expression syntax
  LNsEncoded := StringReplace(LNsEncoded, '*\.', NamespaceRegEx, [rfReplaceAll]);
  Result := LNsEncoded;
end;

end.

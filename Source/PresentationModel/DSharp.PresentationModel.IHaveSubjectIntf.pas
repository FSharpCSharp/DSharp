unit DSharp.PresentationModel.IHaveSubjectIntf;

interface

uses
  Rtti;

type
  ///	<summary>
  ///	  Indicates an item which has subject matter.
  ///	</summary>
  IHaveSubject = interface
    ['{4B4954BE-2F58-49F9-A705-4C937C9D6919}']
    {$REGION 'Property Accessors'}
    function GetSubject: TValue;
    {$ENDREGION}

    ///	<summary>
    ///	  Gets the subject.
    ///	</summary>
    ///	<value>
    ///	  The subject.
    ///	</value>
    property Subject: TValue read GetSubject;

    ///	<summary>
    ///	  Configures the screen with the subject.
    ///	</summary>
    ///	<param name="Subject">
    ///	  The subject.
    ///	</param>
    ///	<returns>
    ///	  Self
    ///	</returns>
    function WithSubject(Subject: TValue): IHaveSubject;
  end;

  ///	<summary>
  ///	  Indicates an item which has subject matter.
  ///	</summary>

  IHaveSubject<T> = interface(IHaveSubject)
    {$REGION 'Property Accessors'}
    function GetSubject: T;
    {$ENDREGION}

    ///	<summary>
    ///	  Configures the screen with the subject.
    ///	</summary>
    ///	<param name="Subject">
    ///	  The subject.
    ///	</param>
    ///	<returns>
    ///	  Self
    ///	</returns>
    function WithSubject(Subject: T): IHaveSubject<T>;

    ///	<summary>
    ///	  Gets the subject.
    ///	</summary>
    ///	<value>
    ///	  The subject.
    ///	</value>
    property Subject: T read GetSubject;
  end;

implementation

end.

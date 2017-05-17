unit TestClasses;

interface

type
  TestAttribute = class(TCustomAttribute);

  TBase = class
  public
    [Test]
    procedure MethodA;
    [Test]
    procedure MethodB; virtual; abstract;
    [Test]
    procedure MethodC; virtual;
  end;

  TInherited = class(TBase)
  public
    procedure MethodA;
    procedure MethodB; override;
    [Test]
    procedure MethodC; override;
  end;

implementation

{ TBase }

procedure TBase.MethodA;
begin

end;

procedure TBase.MethodC;
begin

end;

{ TInherited }

procedure TInherited.MethodA;
begin

end;

procedure TInherited.MethodB;
begin

end;

procedure TInherited.MethodC;
begin
  inherited;
end;

end.

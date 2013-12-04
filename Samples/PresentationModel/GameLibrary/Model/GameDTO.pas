unit GameDTO;

interface

uses
  SysUtils;

type
  TGameDTO = class
  private
    FAddedOn: TDateTime;
    FBorrower: string;
    FId: TGuid;
    FNotes: string;
    FRating: Double;
    FTitle: string;
  public
    constructor Create; overload;
    constructor Create(Title: string; Rating: Double; Notes: string); overload;
    property AddedOn: TDateTime read FAddedOn write FAddedOn;
    property Borrower: string read FBorrower write FBorrower;
    property Id: TGuid read FId write FId;
    property Notes: string read FNotes write FNotes;
    property Rating: Double read FRating write FRating;
    property Title: string read FTitle write FTitle;
  end;

implementation

{ TGameDTO }

constructor TGameDTO.Create;
begin

end;

constructor TGameDTO.Create(Title: string; Rating: Double; Notes: string);
begin
  CreateGuid(FId);
  FTitle := Title;
  FRating := Rating;
  FNotes := Notes;
  FAddedOn := Date;
end;

end.

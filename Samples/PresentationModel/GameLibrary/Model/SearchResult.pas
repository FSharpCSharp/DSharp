unit SearchResult;

interface

type
  TSearchResult = class
  private
    FId: TGUID;
    FTitle: string;
  public
    property Id: TGUID read FId write FId;
    property Title: string read FTitle write FTitle;
  end;

implementation

end.

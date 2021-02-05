unit DPB.Common.Threads;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
{ TShowStatusEvent }
  TShowStatusEvent = procedure(const ALen, APos: Int64) of object;

{ TDownloadThread }
  TDownloadThread = class(TThread)
  private
    FSize: Int64;
    FPos: Int64;
    FURL: String;
    FFilename: String;
    FOnShowStatus: TShowStatusEvent;

    procedure DataReceived(Sender : TObject; Const ContentLength, CurrentPos : Int64);
    procedure ShowStatus;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended : boolean);
    destructor Destroy; override;

    property URL: String
      read FURL
      write FURL;

    property Filename: String
      read FFilename
      write FFilename;

    property OnShowStatus: TShowStatusEvent
      read FOnShowStatus
      write FOnShowStatus;
  published
  end;

implementation

uses
  fphttpclient
, opensslsockets
;

{ TDownloadThread }

constructor TDownloadThread.Create(CreateSuspended: boolean);
begin
  inherited Create(CreateSuspended);
  FreeOnTerminate := True;
end;

destructor TDownloadThread.Destroy;
begin
  inherited Destroy;
end;

procedure TDownloadThread.DataReceived(Sender: TObject; const ContentLength,
  CurrentPos: Int64);
begin
  FPos:= CurrentPos;
  Synchronize(@ShowStatus);
end;

procedure TDownloadThread.ShowStatus;
begin
  if Assigned(FOnShowStatus) then
  begin
    FOnShowStatus(FSize, FPos);
  end;
end;

procedure TDownloadThread.Execute;
var
  http: TFPHTTPClient;
  headers: TStringList;
  index: Integer;
begin
  http:= TFPHTTPClient.Create(nil);
  http.AllowRedirect:= True;
  try
    try
      headers:= TStringList.Create;
      headers.Delimiter:=':';
      TFPHTTPClient.Head(FURL, headers);
      FSize := 0;
      for index := 0 to Pred(headers.Count) do
      begin
        if LowerCase(headers.Names[index]) = 'content-length' then
        begin
          FSize:= StrToInt64(headers.ValueFromIndex[index]);
        end;
      end;
      http.OnDataReceived:= @DataReceived;
      http.Get(FURL);
    except
      on E: Exception do
      begin
        if http.ResponseStatusCode > 399 then
        begin
          //Log(Format('Status: %d', [http.ResponseStatusCode]));
        end;
        //Log('Error: ' + E.Message);
      end;
    end;
  finally
    headers.Free;
    http.Free;
  end;
end;

end.


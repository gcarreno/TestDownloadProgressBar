{ Implements Forms.Sequential

  MIT License

  Copyright (c) 2021 Gustavo Carreno <guscarreno@gmail.com>

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to
  deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
  IN THE SOFTWARE.
}
unit DPB.Forms.Sequencial;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls
{$IF FPC_FULLVERSION < 30200}
, ssockets
, sslsockets
{$ENDIF}
;

type
{ TDownload }
  TDownload = record
    URL: String;
    Filename: String;
  end;

{ TfrmSequencial }
  TfrmSequencial = class(TForm)
    lblTop: TLabel;
    lblDownloads: TLabel;
    pbDownloads: TProgressBar;
    lblBytes: TLabel;
    pbBytes: TProgressBar;

    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormActivate(Sender: TObject);
  private
    FDownloads: Array of TDownload;
    FSize: Int64;
    FAllDone: Boolean;

{$IF FPC_FULLVERSION < 30200}
    procedure GetSocketHandler(Sender: TObject; const UseSSL: Boolean;
      out AHandler: TSocketHandler);
{$ENDIF}
    procedure DataReceived(Sender : TObject; Const ContentLength, CurrentPos : Int64);
    procedure DoDownload(const AIndex: Integer);
  public
    procedure AddDownload(const AURL, AFilename: String);
  end;

var
  frmSequencial: TfrmSequencial;

implementation

uses
  fphttpclient
{$IF FPC_FULLVERSION >= 30200}
, opensslsockets
{$ELSE}
, fpopenssl
, openssl
{$ENDIF}
, FileCtrl
, DPB.Forms.Main
, DPB.Common.Utils
;

{$R *.lfm}

{ TfrmSequencial }

procedure TfrmSequencial.FormCreate(Sender: TObject);
begin
  FSize:= 0;
  FAllDone:= False;
end;

procedure TfrmSequencial.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction:= caFree;
end;

procedure TfrmSequencial.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose:= FallDone;
end;

procedure TfrmSequencial.FormActivate(Sender: TObject);
var
  index: Integer;
begin
  OnActivate:= nil;
  Application.ProcessMessages;
  pbDownloads.Max:= Length(FDownloads);
  for index:= 0 to Pred(Length(FDownloads)) do
  begin
    lblTop.Caption:= MiniMizeName(
      FDownloads[index].Filename,
      lblTop.Canvas,
      lblTop.ClientWidth
    );
    lblDownloads.Caption:= Format('%d of %d', [index + 1, Length(FDownloads)]);
    Application.ProcessMessages;
    try
      DoDownload(index);
    except
      on E: Exception do
      begin
        { #todo 1 -ogcarreno : Inform about error }
        break;
      end;
    end;
    pbDownloads.Position:= index + 1;
    Application.ProcessMessages;
  end;
  FAllDone:= True;
  Close;
end;

{$IF FPC_FULLVERSION < 30200}
procedure TfrmSequencial.GetSocketHandler(Sender: TObject;
  const UseSSL: Boolean; out AHandler: TSocketHandler);
begin
  AHandler := TSSLSocketHandler.Create;
  TSSLSocketHandler(AHandler).SSLType := stTLSv1_2;
end;
{$ENDIF}

procedure TfrmSequencial.DoDownload(const AIndex: Integer);
var
  http: TFPHTTPClient;
  index: Integer;
begin
{$IF FPC_FULLVERSION < 30200}
  InitSSLInterface;
{$ENDIF}
  http:= TFPHTTPClient.Create(nil);
{$IF FPC_FULLVERSION < 30200}
  http.OnGetSocketHandler:=@GetSocketHandler;
{$ENDIF}
  http.AllowRedirect:= True;
  pbBytes.Position:= 0;
  try
    try
      lblBytes.Caption:= 'Determining size...';
      Application.ProcessMessages;
      http.HTTPMethod('HEAD', FDownloads[AIndex].URL, nil, []);
      FSize := 0;
      for index := 0 to Pred(http.ResponseHeaders.Count) do
      begin
        if LowerCase(http.ResponseHeaders.Names[index]) = 'content-length' then
        begin
          FSize:= StrToInt64(http.ResponseHeaders.ValueFromIndex[index]);
        end;
      end;
      http.OnDataReceived:= @DataReceived;
      // Discarding the actual content
      http.Get(FDownloads[AIndex].URL);
    except
      on E: Exception do
      begin
        if http.ResponseStatusCode > 399 then
        begin
          frmMain.Log(Format('Status: %d', [http.ResponseStatusCode]));
        end;
        frmMain.Log('Error: ' + E.Message);
      end;
    end;
  finally
    http.Free;
  end;
end;

procedure TfrmSequencial.AddDownload(const AURL, AFilename: String);
var
  len: Integer;
begin
  { #todo 1 -ogcarreno : Maybe test for duplicates? }
  len:= Length(FDownloads);
  SetLength(FDownloads, len + 1);
  FDownloads[len].URL:= AURL;
  FDownloads[len].Filename:= AFilename;
end;

procedure TfrmSequencial.DataReceived(Sender: TObject; const ContentLength,
  CurrentPos: Int64);
var
  currentPercent: Double;
begin
  currentPercent:= (CurrentPos*100)/FSize;
  pbBytes.Position:= round(currentPercent);
  lblBytes.Caption:= Format('%s of %s', [FormatBytes(CurrentPos), FormatBytes(FSize)]);
  Application.ProcessMessages;
end;

end.


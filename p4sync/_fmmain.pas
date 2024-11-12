unit _fmMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, ComCtrls, Dialogs, ExtCtrls, Forms, IniFiles, StdCtrls, SysUtils;

type

  { TfmMain }

  TfmMain = class(TForm)
    BtnLoad: TButton;
    BtnSync: TButton;
    BtnLogout: TButton;
    EdtUsername: TLabeledEdit;
    LbWorkspace: TListBox;
    Status: TStatusBar;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure BtnLoadClick(Sender: TObject);
    procedure BtnSyncClick(Sender: TObject);
    procedure BtnLogoutClick(Sender: TObject);
    procedure LbWorkspaceDblClick(Sender: TObject);
  private
    FLoaded: Boolean;
    FWorkspaceIndex: Integer;
    procedure LoadIni;
    procedure SaveIni;
    procedure SaveUsername;
    function GetWorkspaceIndex: Integer;

    function GetUsername: string;
    procedure SetUsername(AValue: string);
    function GetWorkspaceCount: Integer;
    function GetWorkspaces(Index: Integer = -1): string;

    function ReadUsername: string;
    function CheckLogin: Boolean;
    function Login: Boolean;
    function ReadWorkspace: string;
    procedure Load(DoLogin: Boolean = False);

    function Synchronize(Workspace: string): string;
    function SynchronizeAll: string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property WorkspaceIndex: Integer read GetWorkspaceIndex;

    property Username: string read GetUsername write SetUsername;
    property WorkspaceCount: Integer read GetWorkspaceCount;
    property Workspaces[Index: Integer]: string read GetWorkspaces;
    property Workspace: string index -1 read GetWorkspaces;
  end;

const
  AllWorkspaces = '[All workspaces]';
  FmtCmdLogin = 'p4 -u %s login';
  FmtCmdCheckLogin = FmtCmdLogin + ' -s';
  FmtCmdWorkspaces = 'p4 -u %s workspaces -u %s';
  FmtCmdSync = 'p4 -u %s -c %s sync';
  FmtCmdLogout = 'p4 -u %s logout';
  UsernameEnvVar = 'P4USER';
  CheckLoginStr : array[0..8] of string = ('User', '', 'ticket', 'expires', 'in', '', 'hours', '', 'minutes.');

var
  fmMain: TfmMain;

implementation

uses
  LConvEncoding,
  Windows;

{$R *.frm}

function OutputExecute(CommandLine: string; WorkDir: string = ''): string;
var
  SecAtrrs: TSecurityAttributes;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  hOutputReadPipe, hOutputWritePipe: THandle;
  WasOK: Boolean;
  Buffer: array[0..255] of AnsiChar;
  BytesRead: DWORD;
  Handle: Boolean;
begin
  Result := '';

  if WorkDir = '' then begin
    WorkDir := GetCurrentDir;
  end;

  SecAtrrs.nLength := SizeOf(SecAtrrs);
  SecAtrrs.lpSecurityDescriptor := nil;
  SecAtrrs.bInheritHandle := True;

  FillChar(StartupInfo, SizeOf(StartupInfo), 0);
  StartupInfo.cb := SizeOf(StartupInfo);
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
  StartupInfo.wShowWindow := SW_HIDE;

  CreatePipe(hOutputReadPipe, hOutputWritePipe, @SecAtrrs, 0);
  try
    StartupInfo.hStdInput := GetStdHandle(STD_INPUT_HANDLE);
    StartupInfo.hStdOutput := hOutputWritePipe;
    StartupInfo.hStdError := hOutputWritePipe;

    Handle := CreateProcess(nil, PChar('CMD.exe /C ' + CommandLine), nil, nil, True, 0, nil, PChar(WorkDir), StartupInfo, ProcessInfo);
    CloseHandle(hOutputWritePipe);
    if Handle then begin
      try
        repeat
          WasOK := Windows.ReadFile(hOutputReadPipe, Buffer, SizeOf(Buffer) - 1, BytesRead, nil);
          if BytesRead > 0 then begin
            Buffer[BytesRead] := #0;
            Result := Result + LConvEncoding.CP949ToUTF8(Buffer);
          end;
        until not WasOK and (BytesRead = 0);
        WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
      finally
        CloseHandle(ProcessInfo.hThread);
        CloseHandle(ProcessInfo.hProcess);
      end;
    end;
  finally
    CloseHandle(hOutputReadPipe);
  end;
end;

function WaitExecute(CommandLine: string; WorkDir: string = ''): string;
var
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  Handle: Boolean;
begin
  Result := '';

  if WorkDir = '' then begin
    WorkDir := GetCurrentDir;
  end;

  FillChar(StartupInfo, SizeOf(StartupInfo), 0);
  StartupInfo.cb := SizeOf(StartupInfo);
  StartupInfo.wShowWindow := SW_SHOW;

  try
    Handle := CreateProcess(nil, PChar('CMD.exe /C ' + CommandLine), nil, nil, True, 0, nil, PChar(WorkDir), StartupInfo, ProcessInfo);
    if Handle then
      WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
  finally
    CloseHandle(ProcessInfo.hThread);
    CloseHandle(ProcessInfo.hProcess);
  end;
end;

function ExtractWorkspace(Value: string): string;
var
  Parser: TStrings;
begin
  Result := '';
  Parser := TStringList.Create;
  try
    Parser.Delimiter := ' ';
    Parser.DelimitedText := Value;
    if Parser.Count >= 2 then
      Result := Parser.Strings[1];
  finally
    FreeAndNil(Parser);
  end;
end;

{ TfmMain }

constructor TfmMain.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FLoaded := False;
  LoadIni;

  if Length(Username) = 0 then
    Username := ReadUsername;
  Load;
end;

destructor TfmMain.Destroy;
begin
  SaveIni;

  inherited Destroy;
end;

procedure TfmMain.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
  );
begin
  case Key of
    VK_UP: begin
      if LbWorkspace.ItemIndex > 0 then
        LbWorkspace.ItemIndex := LbWorkspace.ItemIndex - 1;
      Key := 0;
    end;
    VK_DOWN: begin
      if LbWorkspace.ItemIndex < Pred(LbWorkspace.Items.Count) then
        LbWorkspace.ItemIndex := LbWorkspace.ItemIndex + 1;
      Key := 0;
    end;
  end;
end;

procedure TfmMain.BtnLoadClick(Sender: TObject);
begin
  Load(True);
end;

procedure TfmMain.BtnSyncClick(Sender: TObject);
begin
  if LbWorkspace.Items.Count = 0 then
    BtnLoad.Click;
  DefaultMessageBox(PChar(Synchronize(Workspace)), 'Sync Result', MB_ICONINFORMATION or MB_OK);
end;

procedure TfmMain.BtnLogoutClick(Sender: TObject);
begin
  Status.SimpleText := OutputExecute(Format(FmtCmdLogout, [Username]));
  LbWorkspace.Clear;
end;

procedure TfmMain.LbWorkspaceDblClick(Sender: TObject);
begin
  BtnSync.Click();
end;

procedure TfmMain.LoadIni;
var
  Ini: TIniFile;
  Parser: TStrings;
begin
  FLoaded := True;

  Ini := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  try
    Username := Ini.ReadString('Config', 'Username', Username);
    FWorkspaceIndex := Ini.ReadInteger('Config', 'WorkspaceIndex', 0);

    Parser := TStringList.Create;
    try
      Parser.CommaText := Ini.ReadString('Config', 'Location', '');
      if Parser.Count = 4 then begin
        Position := poDesigned;
        SetBounds(
          StrToIntDef(Parser.Strings[0], Left),
          StrToIntDef(Parser.Strings[1], Top),
          StrToIntDef(Parser.Strings[2], Width),
          StrToIntDef(Parser.Strings[3], Height));
      end;
    finally
      FreeAndNil(Parser);
    end;
  finally
    FreeAndNil(Ini);
  end;
end;

procedure TfmMain.SaveIni;
var
  Ini: TIniFile;
begin
  if not FLoaded then
    Exit;

  Ini := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  try
    Ini.WriteString('Config', 'Location', Format('%d,%d,%d,%d', [Left, Top, Width, Height]));
    Ini.WriteInteger('Config', 'WorkspaceIndex', LbWorkspace.ItemIndex);
  finally
    FreeAndNil(Ini);
  end;
end;

procedure TfmMain.SaveUsername;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  try
    Ini.WriteString('Config', 'Username', Username);
  finally
    FreeAndNil(Ini);
  end;
end;

function TfmMain.GetWorkspaceIndex: Integer;
begin
  if (0 > FWorkspaceIndex) or (FWorkspaceIndex >= LbWorkspace.Items.Count) then
    FWorkspaceIndex := 0;
  Result := FWorkspaceIndex;
end;

function TfmMain.GetUsername: string;
begin
  Result := EdtUsername.Text;
end;

procedure TfmMain.SetUsername(AValue: string);
begin
  EdtUsername.Text := AValue;
end;

function TfmMain.GetWorkspaceCount: Integer;
begin
  Result := LbWorkspace.Items.Count - 1;
end;

function TfmMain.GetWorkspaces(Index: Integer): string;
begin
  if Index = -1 then
    Index := LbWorkspace.ItemIndex - 1;

  Result := '';
  if Index >= 0 then
    Result := ExtractWorkspace(LbWorkspace.Items.Strings[Index + 1]);
end;

function TfmMain.ReadUsername: string;
begin
  Result := SysUtils.GetEnvironmentVariable(UsernameEnvVar);
end;

function TfmMain.CheckLogin: Boolean;
var
  Parser: TStrings;
begin
  Result := False;

  Status.SimpleText := OutputExecute(Format(FmtCmdCheckLogin, [Username]));

  Parser := TStringList.Create;
  try
    Parser.Delimiter := ' ';
    Parser.DelimitedText := Status.SimpleText;
    if Parser.Count = 9 then begin
      if (Parser.Strings[0] = CheckLoginStr[0]) and
         (Parser.Strings[1] = Username) and
         (Parser.Strings[2] = CheckLoginStr[2]) and
         (Parser.Strings[3] = CheckLoginStr[3]) and
         (Parser.Strings[4] = CheckLoginStr[4]) and
         (Parser.Strings[6] = CheckLoginStr[6]) and
         (Parser.Strings[8] = CheckLoginStr[8]) then
        Result := True;
    end;
  finally
    FreeAndNil(Parser);
  end;

  if Result then SaveUsername;
end;

function TfmMain.Login: Boolean;
begin
  WaitExecute(Format(FmtCmdLogin, [Username]));
  Result := CheckLogin;
end;

function TfmMain.ReadWorkspace: string;
var
  Data: TStrings;
begin
  Result := '';

  Data := TStringList.Create;
  try
    Data.Text := OutputExecute(Format(FmtCmdWorkspaces, [Username, Username]));
    Result := Data.CommaText;
  finally
    FreeAndNil(Data);
  end;
end;

procedure TfmMain.Load(DoLogin: Boolean);
var
  sReader: TStrings;
  sHostname: string;
  sLine: string;
  sCurrentHostname: string;
  I: Int32;
begin
  if not CheckLogin and (not DoLogin or not Login) then
    Exit;

  sHostname := Trim(OutputExecute('hostname'));

  LbWorkspace.Items.Clear;
  with TStringList.Create do begin
    try
      CommaText := ReadWorkspace;
      sReader := TStringList.Create;
      try
        sReader.NameValueSeparator := ':';
        for I := 0 to Pred(Count) do begin
          sLine := Strings[I];
          sReader.Text := OutputExecute('p4 client -o ' + ExtractWorkspace(sLine));
          sCurrentHostname := Trim(sReader.Values['Host']);
          if (Length(sCurrentHostname) = 0) or (sCurrentHostname = sHostname) then begin
            LbWorkspace.Items.Add(sLine);
          end;
        end;
      finally
        sReader.Free;
      end;
    finally
      Free;
    end;
  end;
  LbWorkspace.Items.Insert(0, AllWorkspaces);

  LbWorkspace.ItemIndex := WorkspaceIndex;
end;

function TfmMain.Synchronize(Workspace: string): string;
begin
  if Length(Workspace) = 0 then begin
    Exit(SynchronizeAll);
  end;
  Result := Format('[%s]'#13#10'%s', [Workspace, Trim(OutputExecute(Format(FmtCmdSync, [Username, Workspace])))]);
end;

function TfmMain.SynchronizeAll: string;
var
  I: Int32;
begin
  Result := '';
  for I := 0 to Pred(WorkspaceCount) do begin
    if Length(Result) > 0 then
      Result := Result + #13#10#13#10;
    Result := Result + Synchronize(Workspaces[I]);
  end;
  Result := Trim(Result);
end;

end.


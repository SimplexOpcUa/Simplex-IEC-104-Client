unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, FMX.TreeView,
  FMX.ScrollBox, FMX.Memo, System.Rtti, FMX.Grid.Style, FMX.Grid,
  System.Generics.Collections, System.Generics.Defaults, FMX.Platform, System.Math,
  ConnectSettings, Simplex.ClientIEC104, Simplex.ClientIEC101, Simplex.Common,
  Simplex.TcpTransfer, Simplex.SerialTransfer, Client.Common;

type
  TSpxTreeItem = class;

  TfrmMain = class(TForm)
    gbxActions: TGroupBox;
    btnConnect: TButton;
    btnDisconnect: TButton;
    lblLinkStatus: TLabel;
    gbxAsduList: TGroupBox;
    tvAsduList: TTreeView;
    gbxLog: TGroupBox;
    mmLog: TMemo;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    gbxValueList: TGroupBox;
    sgValueList: TStringGrid;
    sclAddressInfoObject: TStringColumn;
    sclValueType: TStringColumn;
    sclValue: TStringColumn;
    sclQuality: TStringColumn;
    sclTime: TStringColumn;
    sclComments: TStringColumn;
    tmProcess: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure btnDisconnectClick(Sender: TObject);
    procedure tmProcessTimer(Sender: TObject);
    procedure tvAsduListClick(Sender: TObject);
  private
    FTransfer: TTransfer;
    FClientIEC104: TClientIEC104;
    FClientIEC101: TClientIEC101;
    FDataCallback: TDataCallback;
    FLogCallback: TLogCallback;
    procedure Init();
    procedure SetState(AConnected: Boolean);
    procedure SetCursor(ACursor: TCursor);
    function AddAsdu(AAddressAsdu: UInt16): TSpxTreeItem;
    procedure UpdateValues(AAsdu: TSpxTreeItem);
  private
    procedure OnValue(AValueParams: TValueParams);
    procedure OnLog(AMessage: string);
    procedure OnStateLink(ALine: string; AConnected: Boolean);
  public
    { Public declarations }
  end;

  TSpxTreeItem = class(TTreeViewItem)
  private
    FAddressAsdu: UInt16;
    FValueList: TList<TValueParams>;
    function ValueSort(const AValue1, AValue2: TValueParams): Integer;
  public
    constructor Create(AOwner: TComponent; AAddressAsdu: UInt16); reintroduce;
    destructor Destroy(); override;
    procedure OnValue(AValueParams: TValueParams);
    property AddressAsdu: UInt16 read FAddressAsdu;
    property ValueList: TList<TValueParams> read FValueList;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

{$region 'TfrmMain'}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FTransfer := nil;
  FClientIEC104 := nil;
  FClientIEC101 := nil;
  FDataCallback := TDataCallback.Create(OnValue);
  FLogCallback := TLogCallback.Create(TrlDebug, OnLog);
  Init();

  SetState(False);
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  if Assigned(FClientIEC104) then FreeAndNil(FClientIEC104);
  if Assigned(FClientIEC101) then FreeAndNil(FClientIEC101);
  if Assigned(FTransfer) then FreeAndNil(FTransfer);
  FreeAndNil(FDataCallback);
  FreeAndNil(FLogCallback);
end;

procedure TfrmMain.btnConnectClick(Sender: TObject);
var ConnectSettings: TConnectSettings;
  OldCursor: TCursor;
  ErrorCode: TIECErrorCode;
begin
  if not GetConnectSettings(Self, ConnectSettings) then Exit;

  OldCursor := Self.Cursor;
  SetCursor(crHourGlass);
  try
    if Assigned(FClientIEC104) then FreeAndNil(FClientIEC104);
    if Assigned(FClientIEC101) then FreeAndNil(FClientIEC101);
    if Assigned(FTransfer) then FreeAndNil(FTransfer);
    ConnectSettings.Iec101Settings.Log := FLogCallback;
    ConnectSettings.Iec101Settings.StateLink := OnStateLink;
    ConnectSettings.Iec104Settings.Log := FLogCallback;
    ConnectSettings.Iec104Settings.StateLink := OnStateLink;

    if (ConnectSettings.ChannelType = ChtSerial) then
      FTransfer := TSerialTransfer.Create(ConnectSettings.SerialSettings.PortName,
        ConnectSettings.SerialSettings.Speed, ConnectSettings.SerialSettings.Parity,
        ConnectSettings.SerialSettings.DtrControl, ConnectSettings.SerialSettings.RtsControl,
        ConnectSettings.SerialSettings.ReadIntervalTimeout, FLogCallback)
    else FTransfer := TTcpTransfer.Create(ConnectSettings.TcpSettings.IpAddress,
      ConnectSettings.TcpSettings.TcpPort, FLogCallback);

    if (ConnectSettings.ProtocolType = PrtIec101) then
    begin
      FClientIEC101 := TClientIEC101.Create(ConnectSettings.Iec101Settings, FTransfer, FDataCallback);
      FClientIEC101.Connect(ErrorCode);
    end
    else begin
      FClientIEC104 := TClientIEC104.Create(ConnectSettings.Iec104Settings, FTransfer, FDataCallback);
      FClientIEC104.Connect(ErrorCode);
    end;

    SetState(True);
  finally
    SetCursor(OldCursor);
  end;
end;

procedure TfrmMain.btnDisconnectClick(Sender: TObject);
var OldCursor: TCursor;
  ErrorCode: TIECErrorCode;
begin
  OldCursor := Self.Cursor;
  SetCursor(crHourGlass);
  try
    if Assigned(FClientIEC101) then
    begin
      FClientIEC101.Disconnect(ErrorCode);
      FreeAndNil(FClientIEC101);
    end;
    if Assigned(FClientIEC104) then
    begin
      FClientIEC104.Disconnect(ErrorCode);
      FreeAndNil(FClientIEC104);
    end;
    if Assigned(FTransfer) then FreeAndNil(FTransfer);

    SetState(False);
  finally
    SetCursor(OldCursor);
  end;
end;

procedure TfrmMain.tmProcessTimer(Sender: TObject);
var ErrorCode: TIECErrorCode;
begin
  if Assigned(FClientIEC104) then
    FClientIEC104.ReceiveCommand(ErrorCode);
  if Assigned(FClientIEC101) then
    FClientIEC101.ReceiveCommand(ErrorCode);
end;

procedure TfrmMain.tvAsduListClick(Sender: TObject);
var Asdu: TSpxTreeItem;
  OldCursor: TCursor;
begin
  if not (tvAsduList.Selected is TSpxTreeItem) then Exit;
  Asdu := tvAsduList.Selected as TSpxTreeItem;

  OldCursor := Self.Cursor;
  SetCursor(crHourGlass);
  try
    UpdateValues(Asdu);
  finally
    SetCursor(OldCursor);
  end;
end;

procedure TfrmMain.OnValue(AValueParams: TValueParams);
var i: Integer;
  Asdu: TSpxTreeItem;
begin
  for i := 0 to tvAsduList.Count - 1 do
    if (tvAsduList.Items[i] is TSpxTreeItem) then
    begin
      Asdu := tvAsduList.Items[i] as TSpxTreeItem;
      if (Asdu.AddressAsdu = AValueParams.AddressAsdu) then
      begin
        Asdu.OnValue(AValueParams);
        if (tvAsduList.Selected = Asdu) then
          UpdateValues(Asdu);
        Exit;
      end;
    end;

  Asdu := AddAsdu(AValueParams.AddressAsdu);
  Asdu.OnValue(AValueParams);
  if not (tvAsduList.Selected is TSpxTreeItem) then
  begin
    tvAsduList.Selected := Asdu;
    UpdateValues(Asdu);
  end;
end;

procedure TfrmMain.OnLog(AMessage: string);
begin
  mmLog.BeginUpdate();
  try
    mmLog.Lines.Add(AMessage);
    mmLog.GoToTextEnd();
    mmLog.GoToLineBegin();
  finally
    mmLog.EndUpdate();
  end;
end;

procedure TfrmMain.OnStateLink(ALine: string; AConnected: Boolean);
begin
  if AConnected then
    lblLinkStatus.Text := 'Link Status: Connected'
  else lblLinkStatus.Text := 'Link Status: Not connected'
end;

{$region 'Helper functions'}

procedure TfrmMain.SetState(AConnected: Boolean);
begin
  btnConnect.Enabled := not AConnected;
  btnDisconnect.Enabled := AConnected;

  if not AConnected then
  begin
    tvAsduList.Clear();
    sgValueList.RowCount := 0;
    lblLinkStatus.Text := 'Link Status: None';
  end;
end;

procedure TfrmMain.SetCursor(ACursor: TCursor);
var CursorService: IFMXCursorService;
begin
  if not TPlatformServices.Current.SupportsPlatformService(IFMXCursorService) then Exit;
  CursorService := TPlatformServices.Current.GetPlatformService(IFMXCursorService) as IFMXCursorService;
  CursorService.SetCursor(ACursor);
end;

procedure TfrmMain.Init();
begin
  sgValueList.Cells[0, 0] := 'AddressInfoObject';
  sgValueList.Cells[1, 0] := 'ValueType';
  sgValueList.Cells[2, 0] := 'Value';
  sgValueList.Cells[3, 0] := 'Quality';
  sgValueList.Cells[4, 0] := 'Time';
  sgValueList.Cells[5, 0] := 'Comments';
end;

function SortAsdu(ANode1, ANode2: TFmxObject):  Integer;
var Asdu1, Asdu2: TSpxTreeItem;
begin
  Result := 0;
  if not (ANode1 is TSpxTreeItem) then Exit;
  if not (ANode2 is TSpxTreeItem) then Exit;
  Asdu1 := ANode1 as TSpxTreeItem;
  Asdu2 := ANode2 as TSpxTreeItem;
  Result := CompareValue(Asdu1.AddressAsdu, Asdu2.AddressAsdu);
end;

function TfrmMain.AddAsdu(AAddressAsdu: UInt16): TSpxTreeItem;
begin
  tvAsduList.BeginUpdate();
  try
    Result := TSpxTreeItem.Create(Self, AAddressAsdu);
    Result.Text := IntToStr(AAddressAsdu);
    Result.Parent := tvAsduList;
    tvAsduList.Sort(SortAsdu);
  finally
    tvAsduList.EndUpdate();
  end;
end;

procedure TfrmMain.UpdateValues(AAsdu: TSpxTreeItem);
var i: Integer;
  StrValue: string;
begin
  if (AAsdu.ValueList.Count = 0) then
  begin
    sgValueList.RowCount := 0;
    Exit;
  end;

  if (sgValueList.RowCount <> AAsdu.ValueList.Count) then
    sgValueList.RowCount := AAsdu.ValueList.Count;

  for i := 0 to AAsdu.ValueList.Count - 1 do
  begin
    // AddressInfoObject
    StrValue := IntToStr(AAsdu.ValueList[i].AddressInfoObject);
    if (sgValueList.Cells[0, i] <> StrValue) then
      sgValueList.Cells[0, i] := StrValue;

    // ValueType
    StrValue := AAsdu.ValueList[i].ValueType;
    if (sgValueList.Cells[1, i] <> StrValue) then
      sgValueList.Cells[1, i] := StrValue;

    // Value
    StrValue := AAsdu.ValueList[i].Value;
    if (sgValueList.Cells[2, i] <> StrValue) then
      sgValueList.Cells[2, i] := StrValue;

    // Quality
    StrValue := AAsdu.ValueList[i].Quality;
    if (sgValueList.Cells[3, i] <> StrValue) then
      sgValueList.Cells[3, i] := StrValue;

    // Time
    StrValue := AAsdu.ValueList[i].Time;
    if (sgValueList.Cells[4, i] <> StrValue) then
      sgValueList.Cells[4, i] := StrValue;

    // Comments
    StrValue := AAsdu.ValueList[i].Comments;
    if (sgValueList.Cells[5, i] <> StrValue) then
      sgValueList.Cells[5, i] := StrValue;
  end;
end;

{$endregion}

{$endregion}

{$region 'TSpxTreeItem'}

constructor TSpxTreeItem.Create(AOwner: TComponent; AAddressAsdu: UInt16);
begin
  inherited Create(AOwner);
  FAddressAsdu := AAddressAsdu;
  FValueList := TList<TValueParams>.Create();
end;

destructor TSpxTreeItem.Destroy();
begin
  FreeAndNil(FValueList);
  inherited;
end;

procedure TSpxTreeItem.OnValue(AValueParams: TValueParams);
var i: Integer;
begin
  for i := 0 to FValueList.Count - 1 do
    if (FValueList[i].AddressInfoObject = AValueParams.AddressInfoObject) and
      (FValueList[i].ValueType = AValueParams.ValueType) then
    begin
      FValueList[i] := AValueParams;
      Exit;
    end;

  FValueList.Add(AValueParams);
  FValueList.Sort(TComparer<TValueParams>.Construct(ValueSort));
end;

function TSpxTreeItem.ValueSort(const AValue1, AValue2: TValueParams): Integer;
begin
  Result := CompareValue(AValue1.AddressInfoObject, AValue2.AddressInfoObject);
end;

{$endregion}

end.

unit ConnectSettings;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,
  System.Win.Registry, Simplex.Common, Simplex.SerialTransfer;

type
  TChannelType = (
    ChtTcp = 0,
    ChtSerial = 1
  );

  TProtocolType = (
    PrtIec101 = 0,
    PrtIec104 = 1
  );

  TTcpSettings = record
    IpAddress: string;
    TcpPort: UInt16;
  end;

  TSerialSettings = record
    PortName: string;
    Speed: UInt32;
    Parity: TParity;
    DtrControl: TDtrControl;
    RtsControl: TRtsControl;
    ReadIntervalTimeout: UInt16;
  end;

  TConnectSettings = record
    ChannelType: TChannelType;
    ProtocolType: TProtocolType;
    TcpSettings: TTcpSettings;
    SerialSettings: TSerialSettings;
    Iec101Settings: TIEC101Params;
    Iec104Settings: TIEC104Params;
  end;

  TfrmConnectSettings = class(TForm)
    gbxConnectSettings: TGroupBox;
    pcSettings: TPageControl;
    tabChannelSettings: TTabSheet;
    tabIec104Settings: TTabSheet;
    tabIec101Settings: TTabSheet;
    btnCancel: TButton;
    btnOK: TButton;
    cbxProtocol: TComboBox;
    lblProtocol: TLabel;
    cbxChannel: TComboBox;
    lblChannel: TLabel;
    lblIec101TimeoutAnswer: TLabel;
    edtIec101TimeoutAnswer: TEdit;
    lblIec101CountTrySend: TLabel;
    edtIec101CountTrySend: TEdit;
    edtIec101OfflinePollPeriod: TEdit;
    lblIec101OfflinePollPeriod: TLabel;
    edtIec101PeriodPollData: TEdit;
    edtIec101AddressDevice: TEdit;
    lblIec101PeriodPollData: TLabel;
    lblIec101SizeAddressChannel: TLabel;
    lblIec101SizeAddressAsdu: TLabel;
    lblIec101SizeAddressInfoObject: TLabel;
    lblIec101SizeCouseOfTransmit: TLabel;
    lblIec101BalancedMode: TLabel;
    lblIec101AddressDevice: TLabel;
    lblIec101FormatTime: TLabel;
    cbxIec101SizeAddressChannel: TComboBox;
    cbxIec101SizeAddressAsdu: TComboBox;
    cbxIec101SizeAddressInfoObject: TComboBox;
    cbxIec101SizeCouseOfTransmit: TComboBox;
    chbxIec101BalancedMode: TCheckBox;
    cbxIec101FormatTime: TComboBox;
    gbxChannelTcpSettings: TGroupBox;
    lblChannelTcpIpAddress: TLabel;
    lblChannelTcpPort: TLabel;
    edtChannelTcpIpAddress: TEdit;
    edtChannelTcpPort: TEdit;
    gbxChannelSerialSettings: TGroupBox;
    lblChannelSerialPortName: TLabel;
    lblChannelSerialSpeed: TLabel;
    cbxChannelSerialPortName: TComboBox;
    cbxChannelSerialSpeed: TComboBox;
    cbxChannelSerialParity: TComboBox;
    lblChannelSerialParity: TLabel;
    cbxChannelSerialRtsDtr: TComboBox;
    lblChannelSerialRtsDtr: TLabel;
    edtChannelSerialReadIntervalTimeout: TEdit;
    lblChannelSerialReadIntervalTimeout: TLabel;
    lblIec104TimeoutAnswer: TLabel;
    edtIec104TimeoutAnswer: TEdit;
    edtIec104TimeoutConfirm: TEdit;
    lblIec104TimeoutConfirm: TLabel;
    edtIec104TimeoutTest: TEdit;
    edtIec104OfflinePollPeriod: TEdit;
    edtIec104MaxNotConfirmCommand: TEdit;
    edtIec104CountConfirmCommand: TEdit;
    lblIec104TimeoutTest: TLabel;
    lblIec104OfflinePollPeriod: TLabel;
    lblIec104MaxNotConfirmCommand: TLabel;
    lblIec104CountConfirmCommand: TLabel;
    lblIec104FormatTime: TLabel;
    cbxIec104FormatTime: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private
    FConnectSettings: TConnectSettings;
    procedure FillComPorts(AComboBox: TComboBox);
    function GetString(ATabSheet: TTabSheet; ALabel: TLabel; AEdit: TEdit;
      out AValue: string): boolean; overload;
    function GetString(ATabSheet: TTabSheet; ALabel: TLabel; AComboBox: TComboBox;
      out AValue: string): boolean; overload;
    function GetByte(ATabSheet: TTabSheet; ALabel: TLabel; AEdit: TEdit;
      out AValue: Byte): boolean; overload;
    function GetByte(ATabSheet: TTabSheet; ALabel: TLabel; AComboBox: TComboBox;
      out AValue: Byte): boolean; overload;
    function GetUInt16(ATabSheet: TTabSheet; ALabel: TLabel; AEdit: TEdit;
      out AValue: UInt16): boolean;
    function GetUInt32(ATabSheet: TTabSheet; ALabel: TLabel; AComboBox: TComboBox;
      out AValue: UInt32): boolean;
  public
    { Public declarations }
  end;

function GetConnectSettings(AOwner: TComponent; out AConnectSettings: TConnectSettings): Boolean;

implementation

{$R *.dfm}

function GetConnectSettings(AOwner: TComponent; out AConnectSettings: TConnectSettings): Boolean;
var frmConnectSettings: TfrmConnectSettings;
begin
  Result := False;
  frmConnectSettings := TfrmConnectSettings.Create(AOwner);
  try
    if (frmConnectSettings.ShowModal() <> mrOK) then Exit;

    AConnectSettings := frmConnectSettings.FConnectSettings;
    Result := True;
  finally
    FreeAndNil(frmConnectSettings);
  end;
end;

procedure TfrmConnectSettings.FormCreate(Sender: TObject);
begin
  FillComPorts(cbxChannelSerialPortName);
end;

procedure TfrmConnectSettings.FillComPorts(AComboBox: TComboBox);
var
  Reg :TRegistry;
  Strs, ComPorts :TStringList;
  i : integer;
  Str: string;
  function Sort(AList: TStringList; AIndex1, AIndex2: Integer): Integer;
  begin
    if (Length(AList[AIndex1]) > Length(AList[AIndex2])) then
      Result := 1
    else if (Length(AList[AIndex1]) < Length(AList[AIndex2])) then
      Result := -1
    else Result := AnsiCompareStr(AList[AIndex1], AList[AIndex2]);
  end;
begin
  AComboBox.Items.Clear;
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    Reg.OpenKeyReadOnly('hardware\devicemap\serialcomm');
    Strs := TStringList.Create;
    ComPorts := TStringList.Create;
    try
      Reg.GetValueNames(Strs);
      for i := 0 to Strs.Count-1 do
      begin
        Str := Reg.ReadString(Strs.Strings[i]);
        Str := UpperCase(Str);
        if (Str >= 'COM1') and (Str <= 'COM999') then
          ComPorts.Add(Str);
      end;
      ComPorts.CustomSort(@Sort);
      AComboBox.Items.AddStrings(ComPorts);
      if (AComboBox.Items.Count > 0) then
        AComboBox.ItemIndex := 0;
    finally
      Strs.Free();
      ComPorts.Free();
    end;
    Reg.CloseKey;
  finally
    Reg.Free;
  end;
end;

function TfrmConnectSettings.GetString(ATabSheet: TTabSheet; ALabel: TLabel; AEdit: TEdit;
  out AValue: string): boolean;
begin
  Result := False;

  if (Length(AEdit.Text) = 0) then
  begin
    ShowMessage(Format('Invalid value - "%s"', [ALabel.Caption]));
    pcSettings.ActivePage := ATabSheet;
    AEdit.SetFocus();
    Exit;
  end;

  AValue := AEdit.Text;
  Result := True;
end;

function TfrmConnectSettings.GetString(ATabSheet: TTabSheet; ALabel: TLabel; AComboBox: TComboBox;
  out AValue: string): boolean;
begin
  Result := False;

  if (Length(AComboBox.Text) = 0) then
  begin
    ShowMessage(Format('Invalid value - "%s"', [ALabel.Caption]));
    pcSettings.ActivePage := ATabSheet;
    AComboBox.SetFocus();
    Exit;
  end;

  AValue := AComboBox.Text;
  Result := True;
end;

function TfrmConnectSettings.GetByte(ATabSheet: TTabSheet; ALabel: TLabel; AEdit: TEdit;
  out AValue: Byte): boolean;
var ValInt: Integer;
begin
  Result := False;

  if not TryStrToInt(AEdit.Text, ValInt) then
  begin
    ShowMessage(Format('Invalid value - "%s"', [ALabel.Caption]));
    pcSettings.ActivePage := ATabSheet;
    AEdit.SetFocus();
    Exit;
  end;

  AValue := Byte(ValInt);
  Result := True;
end;

function TfrmConnectSettings.GetByte(ATabSheet: TTabSheet; ALabel: TLabel; AComboBox: TComboBox;
  out AValue: Byte): boolean;
var ValInt: Integer;
begin
  Result := False;

  if not TryStrToInt(AComboBox.Text, ValInt) then
  begin
    ShowMessage(Format('Invalid value - "%s"', [ALabel.Caption]));
    pcSettings.ActivePage := ATabSheet;
    AComboBox.SetFocus();
    Exit;
  end;

  AValue := Byte(ValInt);
  Result := True;
end;

function TfrmConnectSettings.GetUInt16(ATabSheet: TTabSheet; ALabel: TLabel; AEdit: TEdit;
  out AValue: UInt16): boolean;
var ValInt: Integer;
begin
  Result := False;

  if not TryStrToInt(AEdit.Text, ValInt) then
  begin
    ShowMessage(Format('Invalid value - "%s"', [ALabel.Caption]));
    pcSettings.ActivePage := ATabSheet;
    AEdit.SetFocus();
    Exit;
  end;

  AValue := UInt16(ValInt);
  Result := True;
end;

function TfrmConnectSettings.GetUInt32(ATabSheet: TTabSheet; ALabel: TLabel; AComboBox: TComboBox;
  out AValue: UInt32): boolean;
var ValInt: Integer;
begin
  Result := False;

  if not TryStrToInt(AComboBox.Text, ValInt) then
  begin
    ShowMessage(Format('Invalid value - "%s"', [ALabel.Caption]));
    pcSettings.ActivePage := ATabSheet;
    AComboBox.SetFocus();
    Exit;
  end;

  AValue := UInt32(ValInt);
  Result := True;
end;

procedure TfrmConnectSettings.btnOKClick(Sender: TObject);
begin
  FConnectSettings := Default(TConnectSettings);

  FConnectSettings.ChannelType := TChannelType(cbxChannel.ItemIndex);
  FConnectSettings.ProtocolType := TProtocolType(cbxProtocol.ItemIndex);

  // TCP Settings
  if (FConnectSettings.ChannelType = ChtTcp) then
  begin
    if not GetString(tabChannelSettings, lblChannelTcpIpAddress, edtChannelTcpIpAddress,
      FConnectSettings.TcpSettings.IpAddress) then Exit;
    if not GetUInt16(tabChannelSettings, lblChannelTcpPort, edtChannelTcpPort,
      FConnectSettings.TcpSettings.TcpPort) then Exit;
  end

  // Serial Settings
  else if (FConnectSettings.ChannelType = ChtSerial) then
  begin
    if not GetString(tabChannelSettings, lblChannelSerialPortName, cbxChannelSerialPortName,
      FConnectSettings.SerialSettings.PortName) then Exit;
    if not GetUInt32(tabChannelSettings, lblChannelSerialSpeed, cbxChannelSerialSpeed,
      FConnectSettings.SerialSettings.Speed) then Exit;
    FConnectSettings.SerialSettings.Parity := TParity(cbxChannelSerialParity.ItemIndex);
    FConnectSettings.SerialSettings.DtrControl := dtrNONE;
    FConnectSettings.SerialSettings.RtsControl := rtsNONE;
    if (cbxChannelSerialRtsDtr.ItemIndex > 0) then
    begin
      FConnectSettings.SerialSettings.DtrControl := dtrENABLE;
      FConnectSettings.SerialSettings.RtsControl := rtsENABLE;
    end;
    if not GetUInt16(tabChannelSettings, lblChannelSerialReadIntervalTimeout,
      edtChannelSerialReadIntervalTimeout,
      FConnectSettings.SerialSettings.ReadIntervalTimeout) then Exit;
  end;

  // IEC 104 Settings
  if (FConnectSettings.ProtocolType = PrtIec104) then
  begin
    if not GetByte(tabIec104Settings, lblIec104TimeoutAnswer, edtIec104TimeoutAnswer,
      FConnectSettings.Iec104Settings.TimeoutAnswer) then Exit;
    if not GetByte(tabIec104Settings, lblIec104TimeoutConfirm, edtIec104TimeoutConfirm,
      FConnectSettings.Iec104Settings.TimeoutConfirm) then Exit;
    if not GetByte(tabIec104Settings, lblIec104TimeoutTest, edtIec104TimeoutTest,
      FConnectSettings.Iec104Settings.TimeoutTest) then Exit;
    if not GetByte(tabIec104Settings, lblIec104OfflinePollPeriod, edtIec104OfflinePollPeriod,
      FConnectSettings.Iec104Settings.OfflinePollPeriod) then Exit;
    if not GetUInt16(tabIec104Settings, lblIec104MaxNotConfirmCommand, edtIec104MaxNotConfirmCommand,
      FConnectSettings.Iec104Settings.MaxNotConfirmCommand) then Exit;
    if not GetUInt16(tabIec104Settings, lblIec104CountConfirmCommand, edtIec104CountConfirmCommand,
      FConnectSettings.Iec104Settings.CountConfirmCommand) then Exit;
    FConnectSettings.Iec104Settings.UseUtcTime := (cbxIec104FormatTime.ItemIndex > 0);
  end

  // IEC 101 Settings
  else if (FConnectSettings.ProtocolType = PrtIec101) then
  begin
    if not GetByte(tabIec101Settings, lblIec101TimeoutAnswer, edtIec101TimeoutAnswer,
      FConnectSettings.Iec101Settings.TimeoutAnswer) then Exit;
    if not GetByte(tabIec101Settings, lblIec101CountTrySend, edtIec101CountTrySend,
      FConnectSettings.Iec101Settings.CountTrySend) then Exit;
    if not GetByte(tabIec101Settings, lblIec101OfflinePollPeriod, edtIec101OfflinePollPeriod,
      FConnectSettings.Iec101Settings.OfflinePollPeriod) then Exit;
    if not GetUInt16(tabIec101Settings, lblIec101PeriodPollData, edtIec101PeriodPollData,
      FConnectSettings.Iec101Settings.PeriodPollData) then Exit;
    if not GetByte(tabIec101Settings, lblIec101SizeAddressChannel, cbxIec101SizeAddressChannel,
      FConnectSettings.Iec101Settings.SizeAddressChannel) then Exit;
    if not GetByte(tabIec101Settings, lblIec101SizeAddressAsdu, cbxIec101SizeAddressAsdu,
      FConnectSettings.Iec101Settings.SizeAddressAsdu) then Exit;
    if not GetByte(tabIec101Settings, lblIec101SizeAddressInfoObject, cbxIec101SizeAddressInfoObject,
      FConnectSettings.Iec101Settings.SizeAddressInfoObject) then Exit;
    if not GetByte(tabIec101Settings, lblIec101SizeCouseOfTransmit, cbxIec101SizeCouseOfTransmit,
      FConnectSettings.Iec101Settings.SizeCouseOfTransmit) then Exit;
    FConnectSettings.Iec101Settings.BalancedMode := chbxIec101BalancedMode.Checked;
    if not GetUInt16(tabIec101Settings, lblIec101AddressDevice, edtIec101AddressDevice,
      FConnectSettings.Iec101Settings.AddressDevice) then Exit;
    FConnectSettings.Iec101Settings.UseUtcTime := (cbxIec101FormatTime.ItemIndex > 0);
  end;

  ModalResult := mrOK;
end;

end.

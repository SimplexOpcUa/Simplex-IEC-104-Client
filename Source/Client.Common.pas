unit Client.Common;

interface

uses System.SysUtils,
  Simplex.Common, Simplex.ProtocolCommon;

type
  TValueParams = record
    AddressAsdu: UInt16;
    AddressInfoObject: UInt32;
    ValueType: string;
    Value: string;
    Quality: string;
    Time: string;
    Comments: string;
  end;

  TOnValue = procedure(AValueParams: TValueParams) of object;

  TDataCallback = class(TClientIECCallback)
  private
    FValue: TOnValue;
    function AppendString(AString, AAppendString: string): string;
    function QualityToStr(AQuality: TQuality): string;
    function Quality2ToStr(AQuality: TQuality2): string;
    function Quality3ToStr(AQuality: TQuality3): string;
    function Quality4ToStr(AQuality: TQuality4): string;
    function TSStateToStr(ATSState: TTSState): string;
    function ProtectRelayToStr(AProtectRelay: TProtectRelay): string;
    function CommandRelayToStr(ACommandRelay: TCommandRelay): string;
  public
    constructor Create(AValue: TOnValue);
  public
    procedure OnCompleteCommand(AAsduType: TAsduType; AAddressAsdu: UInt16;
      AAddressInfoObject: UInt32; AErrorCode: TIECErrorCode); override;
    procedure OnMspna(AAddressAsdu: UInt16; AAddressInfoObject: UInt32;
      ACot: TCauseOfTransmit; AValue: Boolean; AQuality: TQuality); override;
    procedure OnMspta(AAddressAsdu: UInt16; AAddressInfoObject: UInt32;
      ACot: TCauseOfTransmit; AValue: Boolean; AQuality: TQuality;
      ADateTime: TDateTime); override;
    procedure OnMdpna(AAddressAsdu: UInt16; AAddressInfoObject: UInt32;
      ACot: TCauseOfTransmit; AValue: TTSState; AQuality: TQuality); override;
    procedure OnMdpta(AAddressAsdu: UInt16; AAddressInfoObject: UInt32;
      ACot: TCauseOfTransmit; AValue: TTSState; AQuality: TQuality;
      ADateTime: TDateTime); override;
    procedure OnMstna(AAddressAsdu: UInt16; AAddressInfoObject: UInt32;
      ACot: TCauseOfTransmit; AValue: Int8; ATransition: Boolean;
      AQuality: TQuality2); override;
    procedure OnMstta(AAddressAsdu: UInt16; AAddressInfoObject: UInt32;
      ACot: TCauseOfTransmit; AValue: Int8; ATransition: Boolean;
      AQuality: TQuality2; ADateTime: TDateTime); override;
    procedure OnMbona(AAddressAsdu: UInt16; AAddressInfoObject: UInt32;
      ACot: TCauseOfTransmit; AMaskValues: UInt32; AQuality: TQuality2); override;
    procedure OnMbota(AAddressAsdu: UInt16; AAddressInfoObject: UInt32;
      ACot: TCauseOfTransmit; AMaskValues: UInt32; AQuality: TQuality2;
      ADateTime: TDateTime); override;
    procedure OnMmena(AAddressAsdu: UInt16; AAddressInfoObject: UInt32;
      ACot: TCauseOfTransmit; AValue: Int16; AQuality: TQuality2); override;
    procedure OnMmeta(AAddressAsdu: UInt16; AAddressInfoObject: UInt32;
      ACot: TCauseOfTransmit; AValue: Int16; AQuality: TQuality2;
      ADateTime: TDateTime); override;
    procedure OnMmenb(AAddressAsdu: UInt16; AAddressInfoObject: UInt32;
      ACot: TCauseOfTransmit; AValue: Int16; AQuality: TQuality2); override;
    procedure OnMmetb(AAddressAsdu: UInt16; AAddressInfoObject: UInt32;
      ACot: TCauseOfTransmit; AValue: Int16; AQuality: TQuality2;
      ADateTime: TDateTime); override;
    procedure OnMmenc(AAddressAsdu: UInt16; AAddressInfoObject: UInt32;
      ACot: TCauseOfTransmit; AValue: Single; AQuality: TQuality2); override;
    procedure OnMmetc(AAddressAsdu: UInt16; AAddressInfoObject: UInt32;
      ACot: TCauseOfTransmit; AValue: Single; AQuality: TQuality2;
      ADateTime: TDateTime); override;
    procedure OnMitna(AAddressAsdu: UInt16; AAddressInfoObject: UInt32;
      ACot: TCauseOfTransmit; AValue: Integer; ANumber: Byte; AQuality: TQuality3); override;
    procedure OnMitta(AAddressAsdu: UInt16; AAddressInfoObject: UInt32;
      ACot: TCauseOfTransmit; AValue: Integer; ANumber: Byte; AQuality: TQuality3;
      ADateTime: TDateTime); override;
    procedure OnMepta(AAddressAsdu: UInt16; AAddressInfoObject: UInt32;
      ACot: TCauseOfTransmit; AValue: TTSState; AQuality: TQuality4; APeriod: UInt16;
      ADateTime: TDateTime); override;
    procedure OnMeptb(AAddressAsdu: UInt16; AAddressInfoObject: UInt32;
      ACot: TCauseOfTransmit; AValue: TProtectRelay; AQuality: TQuality4; APeriod: UInt16;
      ADateTime: TDateTime); override;
    procedure OnMeptc(AAddressAsdu: UInt16; AAddressInfoObject: UInt32;
      ACot: TCauseOfTransmit; AValue: TCommandRelay; AQuality: TQuality4; APeriod: UInt16;
      ADateTime: TDateTime); override;
    procedure OnMpsna(AAddressAsdu: UInt16; AAddressInfoObject: UInt32;
      ACot: TCauseOfTransmit; AMaskValues: UInt16; AMaskChanges: UInt16; AQuality: TQuality2); override;
    procedure OnMmend(AAddressAsdu: UInt16; AAddressInfoObject: UInt32;
      ACot: TCauseOfTransmit; AValue: Int16); override;
    procedure OnMsptb(AAddressAsdu: UInt16; AAddressInfoObject: UInt32;
      ACot: TCauseOfTransmit; AValue: Boolean; AQuality: TQuality;
      ADateTime: TDateTime); override;
    procedure OnMdptb(AAddressAsdu: UInt16; AAddressInfoObject: UInt32;
      ACot: TCauseOfTransmit; AValue: TTSState; AQuality: TQuality;
      ADateTime: TDateTime); override;
    procedure OnMsttb(AAddressAsdu: UInt16; AAddressInfoObject: UInt32;
      ACot: TCauseOfTransmit; AValue: Int8; ATransition: Boolean;
      AQuality: TQuality2; ADateTime: TDateTime); override;
    procedure OnMbotb(AAddressAsdu: UInt16; AAddressInfoObject: UInt32;
      ACot: TCauseOfTransmit; AMaskValues: UInt32; AQuality: TQuality2;
      ADateTime: TDateTime); override;
    procedure OnMmetd(AAddressAsdu: UInt16; AAddressInfoObject: UInt32;
      ACot: TCauseOfTransmit; AValue: Int16; AQuality: TQuality2;
      ADateTime: TDateTime); override;
    procedure OnMmete(AAddressAsdu: UInt16; AAddressInfoObject: UInt32;
      ACot: TCauseOfTransmit; AValue: Int16; AQuality: TQuality2;
      ADateTime: TDateTime); override;
    procedure OnMmetf(AAddressAsdu: UInt16; AAddressInfoObject: UInt32;
      ACot: TCauseOfTransmit; AValue: Single; AQuality: TQuality2; ADateTime: TDateTime); override;
    procedure OnMittb(AAddressAsdu: UInt16; AAddressInfoObject: UInt32;
      ACot: TCauseOfTransmit; AValue: Integer; ANumber: Byte; AQuality: TQuality3;
      ADateTime: TDateTime); override;
    procedure OnMeptd(AAddressAsdu: UInt16; AAddressInfoObject: UInt32;
      ACot: TCauseOfTransmit; AValue: TTSState; AQuality: TQuality4; APeriod: UInt16;
      ADateTime: TDateTime); override;
    procedure OnMepte(AAddressAsdu: UInt16; AAddressInfoObject: UInt32;
      ACot: TCauseOfTransmit; AValue: TProtectRelay; AQuality: TQuality4; APeriod: UInt16;
      ADateTime: TDateTime); override;
    procedure OnMeptf(AAddressAsdu: UInt16; AAddressInfoObject: UInt32;
      ACot: TCauseOfTransmit; AValue: TCommandRelay; AQuality: TQuality4; APeriod: UInt16;
      ADateTime: TDateTime); override;
    procedure OnMeina(AAddressAsdu: UInt16; AAddressInfoObject: UInt32;
      ACot: TCauseOfTransmit; ACoi: TCauseOfInit; AAfterChange: Boolean); override;
    procedure OnCcsna(AAddressAsdu: UInt16; AAddressInfoObject: UInt32;
      ACot: TCauseOfTransmit; ADateTime: TDateTime; ASummer: Boolean); override;
  end;

  TOnLog = procedure(AMessage: string) of object;

  TLogCallback = class(TLog)
  private
    FTraceLevel: TTraceLevel;
    FLog: TOnLog;
  public
    constructor Create(ATraceLevel: TTraceLevel; ALog: TOnLog);
  public
    function TraceLevel(): TTraceLevel; override;
    procedure Log(AMessage: string); override;
    procedure LogWarning(AMessage: string); override;
    procedure LogError(AMessage: string); override;
    procedure LogException(AException: Exception; AMessage: string); override;
  end;

  function DateTimeToStr(ADateTime: TDateTime): string;

implementation

function DateTimeToStr(ADateTime: TDateTime): string;
begin
  Result := FormatDateTime('yyyy.mm.dd hh:nn:ss', ADateTime);
end;

{$region 'TDataCallback'}

constructor TDataCallback.Create(AValue: TOnValue);
begin
  inherited Create();
  FValue := AValue;
end;

/// <summary>
/// Command completion event
/// </summary>
/// <param name="AAsduType">ASDU type</param>
/// <param name="AAddressAsdu">ASDU address</param>
/// <param name="AAddressInfoObject">information object address</param>
/// <param name="AErrorCode">error code</param>
/// <remarks>The command with the activation, confirmation, termination</remarks>
procedure TDataCallback.OnCompleteCommand(AAsduType: TAsduType; AAddressAsdu: UInt16;
  AAddressInfoObject: UInt32; AErrorCode: TIECErrorCode);
begin
  //
end;

/// <summary>
/// M_SP_NA receive event. Single-point information
/// </summary>
/// <param name="AAddressAsdu">ASDU address</param>
/// <param name="AAddressInfoObject">information object address</param>
/// <param name="ACot">cause of transmission</param>
/// <param name="AValue">value</param>
/// <param name="AQuality">quality</param>
procedure TDataCallback.OnMspna(AAddressAsdu: UInt16; AAddressInfoObject: UInt32;
  ACot: TCauseOfTransmit; AValue: Boolean; AQuality: TQuality);
var ValueParams: TValueParams;
begin
  ValueParams := Default(TValueParams);
  ValueParams.AddressAsdu := AAddressAsdu;
  ValueParams.AddressInfoObject := AAddressInfoObject;
  ValueParams.ValueType := 'M_SP_NA';
  ValueParams.Value := BoolToStr(AValue, True);
  ValueParams.Quality := QualityToStr(AQuality);
  FValue(ValueParams);
end;

/// <summary>
/// M_SP_TA receive event. Single-point information with time tag (3 bytes)
/// </summary>
/// <param name="AAddressAsdu">ASDU address</param>
/// <param name="AAddressInfoObject">information object address</param>
/// <param name="ACot">cause of transmission</param>
/// <param name="AValue">value</param>
/// <param name="AQuality">quality</param>
/// <param name="ADateTime">time (0 - invalid time)</param>
procedure TDataCallback.OnMspta(AAddressAsdu: UInt16; AAddressInfoObject: UInt32;
  ACot: TCauseOfTransmit; AValue: Boolean; AQuality: TQuality;
  ADateTime: TDateTime);
var ValueParams: TValueParams;
begin
  ValueParams := Default(TValueParams);
  ValueParams.AddressAsdu := AAddressAsdu;
  ValueParams.AddressInfoObject := AAddressInfoObject;
  ValueParams.ValueType := 'M_SP_TA';
  ValueParams.Value := BoolToStr(AValue, True);
  ValueParams.Quality := QualityToStr(AQuality);
  ValueParams.Time := DateTimeToStr(ADateTime);
  FValue(ValueParams);
end;

/// <summary>
/// M_DP_NA receive event. Double-point information
/// </summary>
/// <param name="AAddressAsdu">ASDU address</param>
/// <param name="AAddressInfoObject">information object address</param>
/// <param name="ACot">cause of transmission</param>
/// <param name="AValue">value</param>
/// <param name="AQuality">quality</param>
procedure TDataCallback.OnMdpna(AAddressAsdu: UInt16; AAddressInfoObject: UInt32;
  ACot: TCauseOfTransmit; AValue: TTSState; AQuality: TQuality);
var ValueParams: TValueParams;
begin
  ValueParams := Default(TValueParams);
  ValueParams.AddressAsdu := AAddressAsdu;
  ValueParams.AddressInfoObject := AAddressInfoObject;
  ValueParams.ValueType := 'M_DP_NA';
  ValueParams.Value := TSStateToStr(AValue);
  ValueParams.Quality := QualityToStr(AQuality);
  FValue(ValueParams);
end;

/// <summary>
/// M_DP_TA receive event. Double-point information with time tag (3 bytes)
/// </summary>
/// <param name="AAddressAsdu">ASDU address</param>
/// <param name="AAddressInfoObject">information object address</param>
/// <param name="ACot">cause of transmission</param>
/// <param name="AValue">value</param>
/// <param name="AQuality">quality</param>
/// <param name="ADateTime">time (0 - invalid time)</param>
procedure TDataCallback.OnMdpta(AAddressAsdu: UInt16; AAddressInfoObject: UInt32;
  ACot: TCauseOfTransmit; AValue: TTSState; AQuality: TQuality;
  ADateTime: TDateTime);
var ValueParams: TValueParams;
begin
  ValueParams := Default(TValueParams);
  ValueParams.AddressAsdu := AAddressAsdu;
  ValueParams.AddressInfoObject := AAddressInfoObject;
  ValueParams.ValueType := 'M_DP_TA';
  ValueParams.Value := TSStateToStr(AValue);
  ValueParams.Quality := QualityToStr(AQuality);
  ValueParams.Time := DateTimeToStr(ADateTime);
  FValue(ValueParams);
end;

/// <summary>
/// M_ST_NA receive event. Step position information
/// </summary>
/// <param name="AAddressAsdu">ASDU address</param>
/// <param name="AAddressInfoObject">information object address</param>
/// <param name="ACot">cause of transmission</param>
/// <param name="AValue">value</param>
/// <param name="ATransition">equipment is in transient state</param>
/// <param name="AQuality">quality</param>
procedure TDataCallback.OnMstna(AAddressAsdu: UInt16; AAddressInfoObject: UInt32;
  ACot: TCauseOfTransmit; AValue: Int8; ATransition: Boolean;
  AQuality: TQuality2);
var ValueParams: TValueParams;
begin
  ValueParams := Default(TValueParams);
  ValueParams.AddressAsdu := AAddressAsdu;
  ValueParams.AddressInfoObject := AAddressInfoObject;
  ValueParams.ValueType := 'M_ST_NA';
  ValueParams.Value := IntToStr(AValue);
  ValueParams.Quality := Quality2ToStr(AQuality);
  ValueParams.Comments := Format('Transition=%s',
    [BoolToStr(ATransition, True)]);
  FValue(ValueParams);
end;

/// <summary>
/// M_ST_TA receive event. Step position information with time tag (3 bytes)
/// </summary>
/// <param name="AAddressAsdu">ASDU address</param>
/// <param name="AAddressInfoObject">information object address</param>
/// <param name="ACot">cause of transmission</param>
/// <param name="AValue">value</param>
/// <param name="ATransition">equipment is in transient state</param>
/// <param name="AQuality">quality</param>
/// <param name="ADateTime">time (0 - invalid time)</param>
procedure TDataCallback.OnMstta(AAddressAsdu: UInt16; AAddressInfoObject: UInt32;
  ACot: TCauseOfTransmit; AValue: Int8; ATransition: Boolean;
  AQuality: TQuality2; ADateTime: TDateTime);
var ValueParams: TValueParams;
begin
  ValueParams := Default(TValueParams);
  ValueParams.AddressAsdu := AAddressAsdu;
  ValueParams.AddressInfoObject := AAddressInfoObject;
  ValueParams.ValueType := 'M_ST_TA';
  ValueParams.Value := IntToStr(AValue);
  ValueParams.Quality := Quality2ToStr(AQuality);
  ValueParams.Time := DateTimeToStr(ADateTime);
  ValueParams.Comments := Format('Transition=%s',
    [BoolToStr(ATransition, True)]);
  FValue(ValueParams);
end;

/// <summary>
/// M_BO_NA receive event. Bitstring of 32 bit
/// </summary>
/// <param name="AAddressAsdu">ASDU address</param>
/// <param name="AAddressInfoObject">information object address</param>
/// <param name="ACot">cause of transmission</param>
/// <param name="AMaskValues">mask 32 bit</param>
/// <param name="AQuality">quality</param>
procedure TDataCallback.OnMbona(AAddressAsdu: UInt16; AAddressInfoObject: UInt32;
  ACot: TCauseOfTransmit; AMaskValues: UInt32; AQuality: TQuality2);
var ValueParams: TValueParams;
begin
  ValueParams := Default(TValueParams);
  ValueParams.AddressAsdu := AAddressAsdu;
  ValueParams.AddressInfoObject := AAddressInfoObject;
  ValueParams.ValueType := 'M_BO_NA';
  ValueParams.Value := IntToStr(AMaskValues);
  ValueParams.Quality := Quality2ToStr(AQuality);
  FValue(ValueParams);
end;

/// <summary>
/// M_BO_TA receive event. Bitstring of 32 bit with time tag (3 bytes)
/// </summary>
/// <param name="AAddressAsdu">ASDU address</param>
/// <param name="AAddressInfoObject">information object address</param>
/// <param name="ACot">cause of transmission</param>
/// <param name="AMaskValues">mask 32 bit</param>
/// <param name="AQuality">quality</param>
/// <param name="ADateTime">time (0 - invalid time)</param>
procedure TDataCallback.OnMbota(AAddressAsdu: UInt16; AAddressInfoObject: UInt32;
  ACot: TCauseOfTransmit; AMaskValues: UInt32; AQuality: TQuality2;
  ADateTime: TDateTime);
var ValueParams: TValueParams;
begin
  ValueParams := Default(TValueParams);
  ValueParams.AddressAsdu := AAddressAsdu;
  ValueParams.AddressInfoObject := AAddressInfoObject;
  ValueParams.ValueType := 'M_BO_TA';
  ValueParams.Value := IntToStr(AMaskValues);
  ValueParams.Quality := Quality2ToStr(AQuality);
  ValueParams.Time := DateTimeToStr(ADateTime);
  FValue(ValueParams);
end;

/// <summary>
/// M_ME_NA receive event. Measured value, normalized value
/// </summary>
/// <param name="AAddressAsdu">ASDU address</param>
/// <param name="AAddressInfoObject">information object address</param>
/// <param name="ACot">cause of transmission</param>
/// <param name="AValue">value (normalized=value/32768)</param>
/// <param name="AQuality">quality</param>
procedure TDataCallback.OnMmena(AAddressAsdu: UInt16; AAddressInfoObject: UInt32;
  ACot: TCauseOfTransmit; AValue: Int16; AQuality: TQuality2);
var ValueParams: TValueParams;
begin
  ValueParams := Default(TValueParams);
  ValueParams.AddressAsdu := AAddressAsdu;
  ValueParams.AddressInfoObject := AAddressInfoObject;
  ValueParams.ValueType := 'M_ME_NA';
  ValueParams.Value := IntToStr(AValue);
  ValueParams.Quality := Quality2ToStr(AQuality);
  FValue(ValueParams);
end;

/// <summary>
/// M_ME_TA receive event. Measured value, normalized value with time tag (3 bytes)
/// </summary>
/// <param name="AAddressAsdu">ASDU address</param>
/// <param name="AAddressInfoObject">information object address</param>
/// <param name="ACot">cause of transmission</param>
/// <param name="AValue">value (normalized=value/32768)</param>
/// <param name="AQuality">quality</param>
/// <param name="ADateTime">time (0 - invalid time)</param>
procedure TDataCallback.OnMmeta(AAddressAsdu: UInt16; AAddressInfoObject: UInt32;
  ACot: TCauseOfTransmit; AValue: Int16; AQuality: TQuality2;
  ADateTime: TDateTime);
var ValueParams: TValueParams;
begin
  ValueParams := Default(TValueParams);
  ValueParams.AddressAsdu := AAddressAsdu;
  ValueParams.AddressInfoObject := AAddressInfoObject;
  ValueParams.ValueType := 'M_ME_TA';
  ValueParams.Value := IntToStr(AValue);
  ValueParams.Quality := Quality2ToStr(AQuality);
  ValueParams.Time := DateTimeToStr(ADateTime);
  FValue(ValueParams);
end;

/// <summary>
/// M_ME_NB receive event. Measured value, scaled value
/// </summary>
/// <param name="AAddressAsdu">ASDU address</param>
/// <param name="AAddressInfoObject">information object address</param>
/// <param name="ACot">cause of transmission</param>
/// <param name="AValue">scaled value</param>
/// <param name="AQuality">quality</param>
procedure TDataCallback.OnMmenb(AAddressAsdu: UInt16; AAddressInfoObject: UInt32;
  ACot: TCauseOfTransmit; AValue: Int16; AQuality: TQuality2);
var ValueParams: TValueParams;
begin
  ValueParams := Default(TValueParams);
  ValueParams.AddressAsdu := AAddressAsdu;
  ValueParams.AddressInfoObject := AAddressInfoObject;
  ValueParams.ValueType := 'M_ME_NB';
  ValueParams.Value := IntToStr(AValue);
  ValueParams.Quality := Quality2ToStr(AQuality);
  FValue(ValueParams);
end;

/// <summary>
/// M_ME_TB receive event. Measured value, scaled value with time tag (3 bytes)
/// </summary>
/// <param name="AAddressAsdu">ASDU address</param>
/// <param name="AAddressInfoObject">information object address</param>
/// <param name="ACot">cause of transmission</param>
/// <param name="AValue">scaled value</param>
/// <param name="AQuality">quality</param>
/// <param name="ADateTime">time (0 - invalid time)</param>
procedure TDataCallback.OnMmetb(AAddressAsdu: UInt16; AAddressInfoObject: UInt32;
  ACot: TCauseOfTransmit; AValue: Int16; AQuality: TQuality2;
  ADateTime: TDateTime);
var ValueParams: TValueParams;
begin
  ValueParams := Default(TValueParams);
  ValueParams.AddressAsdu := AAddressAsdu;
  ValueParams.AddressInfoObject := AAddressInfoObject;
  ValueParams.ValueType := 'M_ME_TB';
  ValueParams.Value := IntToStr(AValue);
  ValueParams.Quality := Quality2ToStr(AQuality);
  ValueParams.Time := DateTimeToStr(ADateTime);
  FValue(ValueParams);
end;

/// <summary>
/// M_ME_NC receive event. Measured value, short floating point number
/// </summary>
/// <param name="AAddressAsdu">ASDU address</param>
/// <param name="AAddressInfoObject">information object address</param>
/// <param name="ACot">cause of transmission</param>
/// <param name="AValue">value</param>
/// <param name="AQuality">quality</param>
procedure TDataCallback.OnMmenc(AAddressAsdu: UInt16; AAddressInfoObject: UInt32;
  ACot: TCauseOfTransmit; AValue: Single; AQuality: TQuality2);
var ValueParams: TValueParams;
begin
  ValueParams := Default(TValueParams);
  ValueParams.AddressAsdu := AAddressAsdu;
  ValueParams.AddressInfoObject := AAddressInfoObject;
  ValueParams.ValueType := 'M_ME_NC';
  ValueParams.Value := FloatToStr(AValue);
  ValueParams.Quality := Quality2ToStr(AQuality);
  FValue(ValueParams);
end;

/// <summary>
/// M_ME_TC receive event. Measured value, short floating point number with time tag (3 bytes)
/// </summary>
/// <param name="AAddressAsdu">ASDU address</param>
/// <param name="AAddressInfoObject">information object address</param>
/// <param name="ACot">cause of transmission</param>
/// <param name="AValue">value</param>
/// <param name="AQuality">quality</param>
/// <param name="ADateTime">time (0 - invalid time)</param>
procedure TDataCallback.OnMmetc(AAddressAsdu: UInt16; AAddressInfoObject: UInt32;
  ACot: TCauseOfTransmit; AValue: Single; AQuality: TQuality2;
  ADateTime: TDateTime);
var ValueParams: TValueParams;
begin
  ValueParams := Default(TValueParams);
  ValueParams.AddressAsdu := AAddressAsdu;
  ValueParams.AddressInfoObject := AAddressInfoObject;
  ValueParams.ValueType := 'M_ME_TC';
  ValueParams.Value := FloatToStr(AValue);
  ValueParams.Quality := Quality2ToStr(AQuality);
  ValueParams.Time := DateTimeToStr(ADateTime);
  FValue(ValueParams);
end;

/// <summary>
/// M_IT_NA receive event. Integrated totals
/// </summary>
/// <param name="AAddressAsdu">ASDU address</param>
/// <param name="AAddressInfoObject">information object address</param>
/// <param name="ACot">cause of transmission</param>
/// <param name="AValue">value</param>
/// <param name="ANumber">number value</param>
/// <param name="AQuality">quality</param>
procedure TDataCallback.OnMitna(AAddressAsdu: UInt16; AAddressInfoObject: UInt32;
  ACot: TCauseOfTransmit; AValue: Integer; ANumber: Byte; AQuality: TQuality3);
var ValueParams: TValueParams;
begin
  ValueParams := Default(TValueParams);
  ValueParams.AddressAsdu := AAddressAsdu;
  ValueParams.AddressInfoObject := AAddressInfoObject;
  ValueParams.ValueType := 'M_IT_NA';
  ValueParams.Value := IntToStr(AValue);
  ValueParams.Quality := Quality3ToStr(AQuality);
  ValueParams.Comments := Format('Number=%d',
    [ANumber]);
  FValue(ValueParams);
end;

/// <summary>
/// M_IT_TA receive event. Integrated totals with time tag (3 bytes)
/// </summary>
/// <param name="AAddressAsdu">ASDU address</param>
/// <param name="AAddressInfoObject">information object address</param>
/// <param name="ACot">cause of transmission</param>
/// <param name="AValue">value</param>
/// <param name="ANumber">number value</param>
/// <param name="AQuality">quality</param>
/// <param name="ADateTime">time (0 - invalid time)</param>
procedure TDataCallback.OnMitta(AAddressAsdu: UInt16; AAddressInfoObject: UInt32;
  ACot: TCauseOfTransmit; AValue: Integer; ANumber: Byte; AQuality: TQuality3;
  ADateTime: TDateTime);
var ValueParams: TValueParams;
begin
  ValueParams := Default(TValueParams);
  ValueParams.AddressAsdu := AAddressAsdu;
  ValueParams.AddressInfoObject := AAddressInfoObject;
  ValueParams.ValueType := 'M_IT_TA';
  ValueParams.Value := IntToStr(AValue);
  ValueParams.Quality := Quality3ToStr(AQuality);
  ValueParams.Time := DateTimeToStr(ADateTime);
  ValueParams.Comments := Format('Number=%d',
    [ANumber]);
  FValue(ValueParams);
end;

/// <summary>
/// M_EP_TA receive event. Event of protection equipment with time tag (3 bytes)
/// </summary>
/// <param name="AAddressAsdu">ASDU address</param>
/// <param name="AAddressInfoObject">information object address</param>
/// <param name="ACot">cause of transmission</param>
/// <param name="AValue">event state</param>
/// <param name="AQuality">quality</param>
/// <param name="APeriod">duration time, ms</param>
/// <param name="ADateTime">time (0 - invalid time)</param>
procedure TDataCallback.OnMepta(AAddressAsdu: UInt16; AAddressInfoObject: UInt32;
  ACot: TCauseOfTransmit; AValue: TTSState; AQuality: TQuality4; APeriod: UInt16;
  ADateTime: TDateTime);
var ValueParams: TValueParams;
begin
  ValueParams := Default(TValueParams);
  ValueParams.AddressAsdu := AAddressAsdu;
  ValueParams.AddressInfoObject := AAddressInfoObject;
  ValueParams.ValueType := 'M_EP_TA';
  ValueParams.Value := TSStateToStr(AValue);
  ValueParams.Quality := Quality4ToStr(AQuality);
  ValueParams.Time := DateTimeToStr(ADateTime);
  ValueParams.Comments := Format('Period=%d',
    [APeriod]);
  FValue(ValueParams);
end;

/// <summary>
/// M_EP_TB receive event. Packed start events of protection equipment with time tag (3 bytes)
/// </summary>
/// <param name="AAddressAsdu">ASDU address</param>
/// <param name="AAddressInfoObject">information object address</param>
/// <param name="ACot">cause of transmission</param>
/// <param name="AValue">start events of protection equipment</param>
/// <param name="AQuality">quality</param>
/// <param name="APeriod">duration time, ms</param>
/// <param name="ADateTime">time (0 - invalid time)</param>
procedure TDataCallback.OnMeptb(AAddressAsdu: UInt16; AAddressInfoObject: UInt32;
  ACot: TCauseOfTransmit; AValue: TProtectRelay; AQuality: TQuality4; APeriod: UInt16;
  ADateTime: TDateTime);
var ValueParams: TValueParams;
begin
  ValueParams := Default(TValueParams);
  ValueParams.AddressAsdu := AAddressAsdu;
  ValueParams.AddressInfoObject := AAddressInfoObject;
  ValueParams.ValueType := 'M_EP_TB';
  ValueParams.Value := ProtectRelayToStr(AValue);
  ValueParams.Quality := Quality4ToStr(AQuality);
  ValueParams.Time := DateTimeToStr(ADateTime);
  ValueParams.Comments := Format('Period=%d',
    [APeriod]);
  FValue(ValueParams);
end;

/// <summary>
/// M_EP_TC receive event. Packed output circuit information of protection equipment with time tag
/// (3 bytes)
/// </summary>
/// <param name="AAddressAsdu">ASDU address</param>
/// <param name="AAddressInfoObject">information object address</param>
/// <param name="ACot">cause of transmission</param>
/// <param name="AValue">output circuit information of protection equipment</param>
/// <param name="AQuality">quality</param>
/// <param name="APeriod">duration time, ms</param>
/// <param name="ADateTime">time (0 - invalid time)</param>
procedure TDataCallback.OnMeptc(AAddressAsdu: UInt16; AAddressInfoObject: UInt32;
  ACot: TCauseOfTransmit; AValue: TCommandRelay; AQuality: TQuality4; APeriod: UInt16;
  ADateTime: TDateTime);
var ValueParams: TValueParams;
begin
  ValueParams := Default(TValueParams);
  ValueParams.AddressAsdu := AAddressAsdu;
  ValueParams.AddressInfoObject := AAddressInfoObject;
  ValueParams.ValueType := 'M_EP_TC';
  ValueParams.Value := CommandRelayToStr(AValue);
  ValueParams.Quality := Quality4ToStr(AQuality);
  ValueParams.Time := DateTimeToStr(ADateTime);
  ValueParams.Comments := Format('Period=%d',
    [APeriod]);
  FValue(ValueParams);
end;

/// <summary>
/// M_PS_NA receive event. Packed single-point information with status change detection
/// </summary>
/// <param name="AAddressAsdu">ASDU address</param>
/// <param name="AAddressInfoObject">information object address</param>
/// <param name="ACot">cause of transmission</param>
/// <param name="AMaskValues">values mask</param>
/// <param name="AMaskChanges">changes in the values mask</param>
/// <param name="AQuality">quality</param>
procedure TDataCallback.OnMpsna(AAddressAsdu: UInt16; AAddressInfoObject: UInt32;
  ACot: TCauseOfTransmit; AMaskValues: UInt16; AMaskChanges: UInt16; AQuality: TQuality2);
var ValueParams: TValueParams;
begin
  ValueParams := Default(TValueParams);
  ValueParams.AddressAsdu := AAddressAsdu;
  ValueParams.AddressInfoObject := AAddressInfoObject;
  ValueParams.ValueType := 'M_PS_NA';
  ValueParams.Value := IntToStr(AMaskValues);
  ValueParams.Quality := Quality2ToStr(AQuality);
  ValueParams.Comments := Format('MaskChanges=%d',
    [AMaskChanges]);
  FValue(ValueParams);
end;

/// <summary>
/// M_ME_ND receive event. Measured value, normalized value without quality descriptor
/// </summary>
/// <param name="AAddressAsdu">ASDU address</param>
/// <param name="AAddressInfoObject">information object address</param>
/// <param name="ACot">cause of transmission</param>
/// <param name="AValue">value (normalized=value/32768)</param>
procedure TDataCallback.OnMmend(AAddressAsdu: UInt16; AAddressInfoObject: UInt32;
  ACot: TCauseOfTransmit; AValue: Int16);
var ValueParams: TValueParams;
begin
  ValueParams := Default(TValueParams);
  ValueParams.AddressAsdu := AAddressAsdu;
  ValueParams.AddressInfoObject := AAddressInfoObject;
  ValueParams.ValueType := 'M_ME_ND';
  ValueParams.Value := IntToStr(AValue);
  FValue(ValueParams);
end;

/// <summary>
/// M_SP_TB receive event. Single-point information with time tag (7 bytes)
/// </summary>
/// <param name="AAddressAsdu">ASDU address</param>
/// <param name="AAddressInfoObject">information object address</param>
/// <param name="ACot">cause of transmission</param>
/// <param name="AValue">value</param>
/// <param name="AQuality">quality</param>
/// <param name="ADateTime">time (0 - invalid time)</param>
procedure TDataCallback.OnMsptb(AAddressAsdu: UInt16; AAddressInfoObject: UInt32;
  ACot: TCauseOfTransmit; AValue: Boolean; AQuality: TQuality;
  ADateTime: TDateTime);
var ValueParams: TValueParams;
begin
  ValueParams := Default(TValueParams);
  ValueParams.AddressAsdu := AAddressAsdu;
  ValueParams.AddressInfoObject := AAddressInfoObject;
  ValueParams.ValueType := 'M_SP_TB';
  ValueParams.Value := BoolToStr(AValue, True);
  ValueParams.Quality := QualityToStr(AQuality);
  ValueParams.Time := DateTimeToStr(ADateTime);
  FValue(ValueParams);
end;

/// <summary>
/// M_DP_TB receive event. Double-point information with time tag (7 bytes)
/// </summary>
/// <param name="AAddressAsdu">ASDU address</param>
/// <param name="AAddressInfoObject">information object address</param>
/// <param name="ACot">cause of transmission</param>
/// <param name="AValue">value</param>
/// <param name="AQuality">quality</param>
/// <param name="ADateTime">time (0 - invalid time)</param>
procedure TDataCallback.OnMdptb(AAddressAsdu: UInt16; AAddressInfoObject: UInt32;
  ACot: TCauseOfTransmit; AValue: TTSState; AQuality: TQuality;
  ADateTime: TDateTime);
var ValueParams: TValueParams;
begin
  ValueParams := Default(TValueParams);
  ValueParams.AddressAsdu := AAddressAsdu;
  ValueParams.AddressInfoObject := AAddressInfoObject;
  ValueParams.ValueType := 'M_DP_TB';
  ValueParams.Value := TSStateToStr(AValue);
  ValueParams.Quality := QualityToStr(AQuality);
  ValueParams.Time := DateTimeToStr(ADateTime);
  FValue(ValueParams);
end;

/// <summary>
/// M_ST_TB receive event. Step position information with time tag (7 bytes)
/// </summary>
/// <param name="AAddressAsdu">ASDU address</param>
/// <param name="AAddressInfoObject">information object address</param>
/// <param name="ACot">cause of transmission</param>
/// <param name="AValue">value</param>
/// <param name="ATransition">equipment is in transient state</param>
/// <param name="AQuality">quality</param>
/// <param name="ADateTime">time (0 - invalid time)</param>
procedure TDataCallback.OnMsttb(AAddressAsdu: UInt16; AAddressInfoObject: UInt32;
  ACot: TCauseOfTransmit; AValue: Int8; ATransition: Boolean;
  AQuality: TQuality2; ADateTime: TDateTime);
var ValueParams: TValueParams;
begin
  ValueParams := Default(TValueParams);
  ValueParams.AddressAsdu := AAddressAsdu;
  ValueParams.AddressInfoObject := AAddressInfoObject;
  ValueParams.ValueType := 'M_ST_TB';
  ValueParams.Value := IntToStr(AValue);
  ValueParams.Quality := Quality2ToStr(AQuality);
  ValueParams.Time := DateTimeToStr(ADateTime);
  ValueParams.Comments := Format('Transition=%s',
    [BoolToStr(ATransition, True)]);
  FValue(ValueParams);
end;

/// <summary>
/// M_BO_TB receive event. Bitstring of 32 bits with time tag (7 bytes)
/// </summary>
/// <param name="AAddressAsdu">ASDU address</param>
/// <param name="AAddressInfoObject">information object address</param>
/// <param name="ACot">cause of transmission</param>
/// <param name="AMaskValues">mask 32 bit</param>
/// <param name="AQuality">quality</param>
/// <param name="ADateTime">time (0 - invalid time)</param>
procedure TDataCallback.OnMbotb(AAddressAsdu: UInt16; AAddressInfoObject: UInt32;
  ACot: TCauseOfTransmit; AMaskValues: UInt32; AQuality: TQuality2;
  ADateTime: TDateTime);
var ValueParams: TValueParams;
begin
  ValueParams := Default(TValueParams);
  ValueParams.AddressAsdu := AAddressAsdu;
  ValueParams.AddressInfoObject := AAddressInfoObject;
  ValueParams.ValueType := 'M_BO_TB';
  ValueParams.Value := IntToStr(AMaskValues);
  ValueParams.Quality := Quality2ToStr(AQuality);
  ValueParams.Time := DateTimeToStr(ADateTime);
  FValue(ValueParams);
end;

/// <summary>
/// M_ME_TD receive event. Measured value, normalized value with time tag (7 bytes)
/// </summary>
/// <param name="AAddressAsdu">ASDU address</param>
/// <param name="AAddressInfoObject">information object address</param>
/// <param name="ACot">cause of transmission</param>
/// <param name="AValue">value (normalized=value/32768)</param>
/// <param name="AQuality">quality</param>
/// <param name="ADateTime">time (0 - invalid time)</param>
procedure TDataCallback.OnMmetd(AAddressAsdu: UInt16; AAddressInfoObject: UInt32;
  ACot: TCauseOfTransmit; AValue: Int16; AQuality: TQuality2;
  ADateTime: TDateTime);
var ValueParams: TValueParams;
begin
  ValueParams := Default(TValueParams);
  ValueParams.AddressAsdu := AAddressAsdu;
  ValueParams.AddressInfoObject := AAddressInfoObject;
  ValueParams.ValueType := 'M_ME_TD';
  ValueParams.Value := IntToStr(AValue);
  ValueParams.Quality := Quality2ToStr(AQuality);
  ValueParams.Time := DateTimeToStr(ADateTime);
  FValue(ValueParams);
end;

/// <summary>
/// M_ME_TE receive event. Measured value, scaled value with time tag (7 bytes)
/// </summary>
/// <param name="AAddressAsdu">ASDU address</param>
/// <param name="AAddressInfoObject">information object address</param>
/// <param name="ACot">cause of transmission</param>
/// <param name="AValue">scaled value</param>
/// <param name="AQuality">quality</param>
/// <param name="ADateTime">time (0 - invalid time)</param>
procedure TDataCallback.OnMmete(AAddressAsdu: UInt16; AAddressInfoObject: UInt32;
  ACot: TCauseOfTransmit; AValue: Int16; AQuality: TQuality2;
  ADateTime: TDateTime);
var ValueParams: TValueParams;
begin
  ValueParams := Default(TValueParams);
  ValueParams.AddressAsdu := AAddressAsdu;
  ValueParams.AddressInfoObject := AAddressInfoObject;
  ValueParams.ValueType := 'M_ME_TE';
  ValueParams.Value := IntToStr(AValue);
  ValueParams.Quality := Quality2ToStr(AQuality);
  ValueParams.Time := DateTimeToStr(ADateTime);
  FValue(ValueParams);
end;

/// <summary>
/// M_ME_TF receive event. Measured value, short floating point number with time tag (7 bytes)
/// </summary>
/// <param name="AAddressAsdu">ASDU address</param>
/// <param name="AAddressInfoObject">information object address</param>
/// <param name="ACot">cause of transmission</param>
/// <param name="AValue">value</param>
/// <param name="AQuality">quality</param>
/// <param name="ADateTime">time (0 - invalid time)</param>
procedure TDataCallback.OnMmetf(AAddressAsdu: UInt16; AAddressInfoObject: UInt32;
  ACot: TCauseOfTransmit; AValue: Single; AQuality: TQuality2; ADateTime: TDateTime);
var ValueParams: TValueParams;
begin
  ValueParams := Default(TValueParams);
  ValueParams.AddressAsdu := AAddressAsdu;
  ValueParams.AddressInfoObject := AAddressInfoObject;
  ValueParams.ValueType := 'M_ME_TF';
  ValueParams.Value := FloatToStr(AValue);
  ValueParams.Quality := Quality2ToStr(AQuality);
  ValueParams.Time := DateTimeToStr(ADateTime);
  FValue(ValueParams);
end;

/// <summary>
/// M_IT_TB receive event. Integrated totals with time tag (3 bytes)
/// </summary>
/// <param name="AAddressAsdu">ASDU address</param>
/// <param name="AAddressInfoObject">information object address</param>
/// <param name="ACot">cause of transmission</param>
/// <param name="AValue">value</param>
/// <param name="ANumber">number value</param>
/// <param name="AQuality">quality</param>
/// <param name="ADateTime">time (0 - invalid time)</param>
procedure TDataCallback.OnMittb(AAddressAsdu: UInt16; AAddressInfoObject: UInt32;
  ACot: TCauseOfTransmit; AValue: Integer; ANumber: Byte; AQuality: TQuality3;
  ADateTime: TDateTime);
var ValueParams: TValueParams;
begin
  ValueParams := Default(TValueParams);
  ValueParams.AddressAsdu := AAddressAsdu;
  ValueParams.AddressInfoObject := AAddressInfoObject;
  ValueParams.ValueType := 'M_IT_TB';
  ValueParams.Value := IntToStr(AValue);
  ValueParams.Quality := Quality3ToStr(AQuality);
  ValueParams.Time := DateTimeToStr(ADateTime);
  ValueParams.Comments := Format('Number=%d',
    [ANumber]);
  FValue(ValueParams);
end;

/// <summary>
/// M_EP_TD receive event. Event of protection equipment with time tag (7 bytes)
/// </summary>
/// <param name="AAddressAsdu">ASDU address</param>
/// <param name="AAddressInfoObject">information object address</param>
/// <param name="ACot">cause of transmission</param>
/// <param name="AValue">event state</param>
/// <param name="AQuality">quality</param>
/// <param name="APeriod">duration time, ms</param>
/// <param name="ADateTime">time (0 - invalid time)</param>
procedure TDataCallback.OnMeptd(AAddressAsdu: UInt16; AAddressInfoObject: UInt32;
  ACot: TCauseOfTransmit; AValue: TTSState; AQuality: TQuality4; APeriod: UInt16;
  ADateTime: TDateTime);
var ValueParams: TValueParams;
begin
  ValueParams := Default(TValueParams);
  ValueParams.AddressAsdu := AAddressAsdu;
  ValueParams.AddressInfoObject := AAddressInfoObject;
  ValueParams.ValueType := 'M_EP_TD';
  ValueParams.Value := TSStateToStr(AValue);
  ValueParams.Quality := Quality4ToStr(AQuality);
  ValueParams.Time := DateTimeToStr(ADateTime);
  ValueParams.Comments := Format('Period=%d',
    [APeriod]);
  FValue(ValueParams);
end;

/// <summary>
/// M_EP_TE receive event. Packed start events of protection equipment with time tag (7 bytes)
/// </summary>
/// <param name="AAddressAsdu">ASDU address</param>
/// <param name="AAddressInfoObject">information object address</param>
/// <param name="ACot">cause of transmission</param>
/// <param name="AValue">start events of protection equipment</param>
/// <param name="AQuality">quality</param>
/// <param name="APeriod">duration time, ms</param>
/// <param name="ADateTime">time (0 - invalid time)</param>
procedure TDataCallback.OnMepte(AAddressAsdu: UInt16; AAddressInfoObject: UInt32;
  ACot: TCauseOfTransmit; AValue: TProtectRelay; AQuality: TQuality4; APeriod: UInt16;
  ADateTime: TDateTime);
var ValueParams: TValueParams;
begin
  ValueParams := Default(TValueParams);
  ValueParams.AddressAsdu := AAddressAsdu;
  ValueParams.AddressInfoObject := AAddressInfoObject;
  ValueParams.ValueType := 'M_EP_TE';
  ValueParams.Value := ProtectRelayToStr(AValue);
  ValueParams.Quality := Quality4ToStr(AQuality);
  ValueParams.Time := DateTimeToStr(ADateTime);
  ValueParams.Comments := Format('Period=%d',
    [APeriod]);
  FValue(ValueParams);
end;

/// <summary>
/// M_EP_TF receive event. Packed output circuit information of protection equipment with time tag
/// (7 bytes)
/// </summary>
/// <param name="AAddressAsdu">ASDU address</param>
/// <param name="AAddressInfoObject">information object address</param>
/// <param name="ACot">cause of transmission</param>
/// <param name="AValue">output circuit information of protection equipment</param>
/// <param name="AQuality">quality</param>
/// <param name="APeriod">duration time, ms</param>
/// <param name="ADateTime">time (0 - invalid time)</param>
procedure TDataCallback.OnMeptf(AAddressAsdu: UInt16; AAddressInfoObject: UInt32;
  ACot: TCauseOfTransmit; AValue: TCommandRelay; AQuality: TQuality4; APeriod: UInt16;
  ADateTime: TDateTime);
var ValueParams: TValueParams;
begin
  ValueParams := Default(TValueParams);
  ValueParams.AddressAsdu := AAddressAsdu;
  ValueParams.AddressInfoObject := AAddressInfoObject;
  ValueParams.ValueType := 'M_EP_TF';
  ValueParams.Value := CommandRelayToStr(AValue);
  ValueParams.Quality := Quality4ToStr(AQuality);
  ValueParams.Time := DateTimeToStr(ADateTime);
  ValueParams.Comments := Format('Period=%d',
    [APeriod]);
  FValue(ValueParams);
end;

/// <summary>
/// M_EI_NA receive event. End of initialization
/// </summary>
/// <param name="AAddressAsdu">ASDU address</param>
/// <param name="AAddressInfoObject">information object address</param>
/// <param name="ACot">cause of transmission</param>
/// <param name="ACoi">сause of initialization</param>
/// <param name="AAfterChange">initialization after change of local parameters</param>
procedure TDataCallback.OnMeina(AAddressAsdu: UInt16; AAddressInfoObject: UInt32;
  ACot: TCauseOfTransmit; ACoi: TCauseOfInit; AAfterChange: Boolean);
begin
  //
end;

/// <summary>
/// C_CS_NA receive event. Current time (Clock synchronization command)
/// </summary>
/// <param name="AAddressAsdu">ASDU address</param>
/// <param name="AAddressInfoObject">information object address</param>
/// <param name="ACot">cause of transmission</param>
/// <param name="ADateTime">дата/time (0 - invalid time)</param>
/// <param name="ASummer">summer time</param>
procedure TDataCallback.OnCcsna(AAddressAsdu: UInt16; AAddressInfoObject: UInt32;
  ACot: TCauseOfTransmit; ADateTime: TDateTime; ASummer: Boolean);
begin
  //
end;

{$region 'Helper functions'}

function TDataCallback.AppendString(AString, AAppendString: string): string;
begin
  if (Length(AString) > 0) then
    Result := Format('%s,%s', [AString, AAppendString])
  else Result := AAppendString;
end;

function TDataCallback.QualityToStr(AQuality: TQuality): string;
begin
  Result := '';

  if QltBlocked in AQuality then
    Result := AppendString(Result, 'Blocked');
  if QltSubstituted in AQuality then
    Result := AppendString(Result, 'Substituted');
  if QltNotTopical in AQuality then
    Result := AppendString(Result, 'NotTopical');
  if QltInvalid in AQuality then
    Result := AppendString(Result, 'Invalid');

  if (Length(Result) = 0) then
    Result := 'Good';
end;

function TDataCallback.Quality2ToStr(AQuality: TQuality2): string;
begin
  Result := '';

  if Qlt2Overflow in AQuality then
    Result := AppendString(Result, 'Overflow');
  if Qlt2Blocked in AQuality then
    Result := AppendString(Result, 'Blocked');
  if Qlt2Substituted in AQuality then
    Result := AppendString(Result, 'Substituted');
  if Qlt2NotTopical in AQuality then
    Result := AppendString(Result, 'NotTopical');
  if Qlt2Invalid in AQuality then
    Result := AppendString(Result, 'Invalid');

  if (Length(Result) = 0) then
    Result := 'Good';
end;

function TDataCallback.Quality3ToStr(AQuality: TQuality3): string;
begin
  Result := '';

  if Qlt3Overflow in AQuality then
    Result := AppendString(Result, 'Overflow');
  if Qlt3Set in AQuality then
    Result := AppendString(Result, 'Set');
  if Qlt3Invalid in AQuality then
    Result := AppendString(Result, 'Invalid');

  if (Length(Result) = 0) then
    Result := 'Good';
end;

function TDataCallback.Quality4ToStr(AQuality: TQuality4): string;
begin
  Result := '';

  if Qlt4PeriodInvalid in AQuality then
    Result := AppendString(Result, 'PeriodInvalid');
  if Qlt4Blocked in AQuality then
    Result := AppendString(Result, 'Blocked');
  if Qlt4Substituted in AQuality then
    Result := AppendString(Result, 'Substituted');
  if Qlt4NotTopical in AQuality then
    Result := AppendString(Result, 'NotTopical');
  if Qlt4Invalid in AQuality then
    Result := AppendString(Result, 'Invalid');

  if (Length(Result) = 0) then
    Result := 'Good';
end;

function TDataCallback.TSStateToStr(ATSState: TTSState): string;
begin
  if (ATSState = TssOn) then
    Result := 'On'
  else if (ATSState = TssOff) then
    Result := 'Off'
  else Result := 'None';
end;

function TDataCallback.ProtectRelayToStr(AProtectRelay: TProtectRelay): string;
begin
  Result := '';

  if PrrProtect in AProtectRelay then
    Result := AppendString(Result, 'Protect');
  if PrrProtectL1 in AProtectRelay then
    Result := AppendString(Result, 'ProtectL1');
  if PrrProtectL2 in AProtectRelay then
    Result := AppendString(Result, 'ProtectL2');
  if PrrProtectL3 in AProtectRelay then
    Result := AppendString(Result, 'ProtectL3');
  if PrrProtectGnd in AProtectRelay then
    Result := AppendString(Result, 'ProtectGnd');
  if PrrProtectRd in AProtectRelay then
    Result := AppendString(Result, 'ProtectRd');

  if (Length(Result) = 0) then
    Result := 'None';
end;

function TDataCallback.CommandRelayToStr(ACommandRelay: TCommandRelay): string;
begin
  Result := '';

  if CmrCommand in ACommandRelay then
    Result := AppendString(Result, 'Command');
  if CmrCommandL1 in ACommandRelay then
    Result := AppendString(Result, 'CommandL1');
  if CmrCommandL2 in ACommandRelay then
    Result := AppendString(Result, 'CommandL2');
  if CmrCommandL3 in ACommandRelay then
    Result := AppendString(Result, 'CommandL3');

  if (Length(Result) = 0) then
    Result := 'None';
end;

{$endregion}

{$endregion}

{$region 'TLogCallback'}

constructor TLogCallback.Create(ATraceLevel: TTraceLevel; ALog: TOnLog);
begin
  inherited Create();
  FTraceLevel := ATraceLevel;
  FLog := ALog;
end;

function TLogCallback.TraceLevel(): TTraceLevel;
begin
  Result := FTraceLevel;
end;

procedure TLogCallback.Log(AMessage: string);
begin
  FLog(Format('%s %s',
    [DateTimeToStr(Now), AMessage]));
end;

procedure TLogCallback.LogWarning(AMessage: string);
begin
  Log(Format('[Warning] %s',
    [AMessage]));
end;

procedure TLogCallback.LogError(AMessage: string);
begin
  Log(Format('[Error] %s',
    [AMessage]));
end;

procedure TLogCallback.LogException(AException: Exception; AMessage: string);
begin
  Log(Format('[Exception] %s, E.Message=%s',
    [AMessage, AException.Message]));
end;

{$endregion}

end.

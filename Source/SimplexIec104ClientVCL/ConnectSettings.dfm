object frmConnectSettings: TfrmConnectSettings
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = #1057'onnection Settings'
  ClientHeight = 462
  ClientWidth = 350
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object gbxConnectSettings: TGroupBox
    Left = 0
    Top = 0
    Width = 350
    Height = 73
    Align = alTop
    TabOrder = 0
    object lblProtocol: TLabel
      Left = 185
      Top = 5
      Width = 39
      Height = 13
      Caption = 'Protocol'
    end
    object lblChannel: TLabel
      Left = 16
      Top = 5
      Width = 39
      Height = 13
      Caption = 'Channel'
    end
    object cbxProtocol: TComboBox
      Left = 185
      Top = 24
      Width = 146
      Height = 21
      Style = csDropDownList
      ItemIndex = 1
      TabOrder = 0
      Text = 'IEC 60870-5-104'
      Items.Strings = (
        'IEC 60870-5-101'
        'IEC 60870-5-104')
    end
    object cbxChannel: TComboBox
      Left = 16
      Top = 24
      Width = 147
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 1
      Text = 'TCP'
      Items.Strings = (
        'TCP'
        'Serial')
    end
  end
  object pcSettings: TPageControl
    Left = 0
    Top = 73
    Width = 350
    Height = 347
    ActivePage = tabChannelSettings
    Align = alTop
    TabOrder = 1
    object tabChannelSettings: TTabSheet
      Caption = 'Channel Settings'
      object gbxChannelTcpSettings: TGroupBox
        Left = 0
        Top = 0
        Width = 342
        Height = 89
        Align = alTop
        Caption = 'TCP Settings'
        TabOrder = 0
        object lblChannelTcpIpAddress: TLabel
          Left = 12
          Top = 27
          Width = 51
          Height = 13
          Caption = 'IP address'
        end
        object lblChannelTcpPort: TLabel
          Left = 12
          Top = 54
          Width = 42
          Height = 13
          Caption = 'TCP port'
        end
        object edtChannelTcpIpAddress: TEdit
          Left = 181
          Top = 23
          Width = 146
          Height = 21
          TabOrder = 0
          Text = '127.0.0.1'
        end
        object edtChannelTcpPort: TEdit
          Left = 181
          Top = 50
          Width = 146
          Height = 21
          NumbersOnly = True
          TabOrder = 1
          Text = '2404'
        end
      end
      object gbxChannelSerialSettings: TGroupBox
        Left = 0
        Top = 89
        Width = 342
        Height = 168
        Align = alTop
        Caption = 'Serial Settings'
        TabOrder = 1
        object lblChannelSerialPortName: TLabel
          Left = 12
          Top = 27
          Width = 49
          Height = 13
          Caption = 'Port name'
        end
        object lblChannelSerialSpeed: TLabel
          Left = 12
          Top = 54
          Width = 30
          Height = 13
          Caption = 'Speed'
        end
        object lblChannelSerialParity: TLabel
          Left = 12
          Top = 81
          Width = 28
          Height = 13
          Caption = 'Parity'
        end
        object lblChannelSerialRtsDtr: TLabel
          Left = 12
          Top = 108
          Width = 43
          Height = 13
          Caption = 'DTR/RTS'
        end
        object lblChannelSerialReadIntervalTimeout: TLabel
          Left = 12
          Top = 135
          Width = 134
          Height = 13
          Caption = 'Read interval timeout, msec'
        end
        object cbxChannelSerialPortName: TComboBox
          Left = 181
          Top = 23
          Width = 146
          Height = 21
          Style = csDropDownList
          TabOrder = 0
        end
        object cbxChannelSerialSpeed: TComboBox
          Left = 181
          Top = 50
          Width = 147
          Height = 21
          Style = csDropDownList
          ItemIndex = 8
          TabOrder = 1
          Text = '19200'
          Items.Strings = (
            '110'
            '300'
            '600'
            '1200'
            '2400'
            '4800'
            '9600'
            '14400'
            '19200'
            '38400'
            '56000'
            '57600'
            '115200'
            '128000'
            '230400'
            '256000'
            '460800'
            '921600')
        end
        object cbxChannelSerialParity: TComboBox
          Left = 181
          Top = 77
          Width = 146
          Height = 21
          Style = csDropDownList
          ItemIndex = 0
          TabOrder = 2
          Text = 'no'
          Items.Strings = (
            'no'
            'odd'
            'even'
            'mark'
            'space')
        end
        object cbxChannelSerialRtsDtr: TComboBox
          Left = 182
          Top = 104
          Width = 146
          Height = 21
          Style = csDropDownList
          ItemIndex = 1
          TabOrder = 3
          Text = 'yes'
          Items.Strings = (
            'no'
            'yes')
        end
        object edtChannelSerialReadIntervalTimeout: TEdit
          Left = 181
          Top = 131
          Width = 146
          Height = 21
          NumbersOnly = True
          TabOrder = 4
          Text = '10'
        end
      end
    end
    object tabIec104Settings: TTabSheet
      Caption = 'IEC 104 Settings'
      ImageIndex = 1
      object lblIec104TimeoutAnswer: TLabel
        Left = 12
        Top = 15
        Width = 122
        Height = 13
        Caption = 'Timeout answer, sec (T1)'
      end
      object lblIec104TimeoutConfirm: TLabel
        Left = 12
        Top = 42
        Width = 148
        Height = 13
        Caption = 'Acknowledge timeout, sec (T2)'
      end
      object lblIec104TimeoutTest: TLabel
        Left = 12
        Top = 69
        Width = 103
        Height = 13
        Caption = 'Idle timeout, sec (T3)'
      end
      object lblIec104OfflinePollPeriod: TLabel
        Left = 12
        Top = 88
        Width = 141
        Height = 26
        Caption = 'Poll period of the unavailable channel, sec'
        WordWrap = True
      end
      object lblIec104MaxNotConfirmCommand: TLabel
        Left = 12
        Top = 120
        Width = 161
        Height = 26
        Caption = 'Maximum number of unconfirmed commands (K)'
        WordWrap = True
      end
      object lblIec104CountConfirmCommand: TLabel
        Left = 12
        Top = 147
        Width = 163
        Height = 26
        Caption = 'Number of received commands to send confirmation (W)'
        WordWrap = True
      end
      object lblIec104FormatTime: TLabel
        Left = 12
        Top = 174
        Width = 98
        Height = 26
        Caption = 'Format current time for 3-byte time'
        WordWrap = True
      end
      object edtIec104TimeoutAnswer: TEdit
        Left = 181
        Top = 12
        Width = 146
        Height = 21
        NumbersOnly = True
        TabOrder = 0
        Text = '45'
      end
      object edtIec104TimeoutConfirm: TEdit
        Left = 181
        Top = 39
        Width = 146
        Height = 21
        NumbersOnly = True
        TabOrder = 1
        Text = '30'
      end
      object edtIec104TimeoutTest: TEdit
        Left = 181
        Top = 66
        Width = 146
        Height = 21
        NumbersOnly = True
        TabOrder = 2
        Text = '60'
      end
      object edtIec104OfflinePollPeriod: TEdit
        Left = 181
        Top = 93
        Width = 146
        Height = 21
        NumbersOnly = True
        TabOrder = 3
        Text = '30'
      end
      object edtIec104MaxNotConfirmCommand: TEdit
        Left = 181
        Top = 120
        Width = 146
        Height = 21
        NumbersOnly = True
        TabOrder = 4
        Text = '12'
      end
      object edtIec104CountConfirmCommand: TEdit
        Left = 181
        Top = 147
        Width = 146
        Height = 21
        NumbersOnly = True
        TabOrder = 5
        Text = '8'
      end
      object cbxIec104FormatTime: TComboBox
        Left = 181
        Top = 174
        Width = 146
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 6
        Text = 'Local'
        Items.Strings = (
          'Local'
          'UTC')
      end
    end
    object tabIec101Settings: TTabSheet
      Caption = 'IEC 101 Settings'
      ImageIndex = 2
      object lblIec101TimeoutAnswer: TLabel
        Left = 12
        Top = 15
        Width = 99
        Height = 13
        Caption = 'Timeout answer, sec'
      end
      object lblIec101CountTrySend: TLabel
        Left = 12
        Top = 42
        Width = 135
        Height = 13
        Caption = 'Number of attempts to send'
      end
      object lblIec101OfflinePollPeriod: TLabel
        Left = 12
        Top = 61
        Width = 141
        Height = 26
        Caption = 'Poll period of the unavailable channel, sec'
        WordWrap = True
      end
      object lblIec101PeriodPollData: TLabel
        Left = 12
        Top = 96
        Width = 137
        Height = 13
        Caption = 'Poll period of the data, msec'
      end
      object lblIec101SizeAddressChannel: TLabel
        Left = 12
        Top = 123
        Width = 135
        Height = 13
        Caption = 'Channel address size, bytes'
      end
      object lblIec101SizeAddressAsdu: TLabel
        Left = 12
        Top = 150
        Width = 123
        Height = 13
        Caption = 'ASDU address size, bytes'
      end
      object lblIec101SizeAddressInfoObject: TLabel
        Left = 12
        Top = 169
        Width = 157
        Height = 26
        Caption = 'Address information object size, bytes'
        WordWrap = True
      end
      object lblIec101SizeCouseOfTransmit: TLabel
        Left = 12
        Top = 204
        Width = 160
        Height = 13
        Caption = 'Cause of transmission size, bytes'
      end
      object lblIec101BalancedMode: TLabel
        Left = 12
        Top = 232
        Width = 72
        Height = 13
        Caption = 'Balanced mode'
      end
      object lblIec101AddressDevice: TLabel
        Left = 12
        Top = 259
        Width = 73
        Height = 13
        Caption = 'Device address'
      end
      object lblIec101FormatTime: TLabel
        Left = 12
        Top = 278
        Width = 98
        Height = 26
        Caption = 'Format current time for 3-byte time'
        WordWrap = True
      end
      object edtIec101TimeoutAnswer: TEdit
        Left = 181
        Top = 12
        Width = 146
        Height = 21
        NumbersOnly = True
        TabOrder = 0
        Text = '3'
      end
      object edtIec101CountTrySend: TEdit
        Left = 181
        Top = 39
        Width = 146
        Height = 21
        NumbersOnly = True
        TabOrder = 1
        Text = '3'
      end
      object edtIec101OfflinePollPeriod: TEdit
        Left = 181
        Top = 66
        Width = 146
        Height = 21
        NumbersOnly = True
        TabOrder = 2
        Text = '60'
      end
      object edtIec101PeriodPollData: TEdit
        Left = 181
        Top = 93
        Width = 146
        Height = 21
        NumbersOnly = True
        TabOrder = 3
        Text = '200'
      end
      object edtIec101AddressDevice: TEdit
        Left = 181
        Top = 255
        Width = 146
        Height = 21
        NumbersOnly = True
        TabOrder = 4
        Text = '1'
      end
      object cbxIec101SizeAddressAsdu: TComboBox
        Left = 181
        Top = 146
        Width = 146
        Height = 21
        Style = csDropDownList
        ItemIndex = 1
        TabOrder = 5
        Text = '2'
        Items.Strings = (
          '1'
          '2')
      end
      object cbxIec101SizeAddressChannel: TComboBox
        Left = 181
        Top = 120
        Width = 146
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 6
        Text = '1'
        Items.Strings = (
          '1'
          '2')
      end
      object cbxIec101SizeAddressInfoObject: TComboBox
        Left = 181
        Top = 173
        Width = 146
        Height = 21
        Style = csDropDownList
        ItemIndex = 2
        TabOrder = 7
        Text = '3'
        Items.Strings = (
          '1'
          '2'
          '3')
      end
      object cbxIec101SizeCouseOfTransmit: TComboBox
        Left = 181
        Top = 200
        Width = 146
        Height = 21
        Style = csDropDownList
        ItemIndex = 1
        TabOrder = 8
        Text = '2'
        Items.Strings = (
          '1'
          '2')
      end
      object chbxIec101BalancedMode: TCheckBox
        Left = 181
        Top = 232
        Width = 146
        Height = 17
        TabOrder = 9
      end
      object cbxIec101FormatTime: TComboBox
        Left = 181
        Top = 282
        Width = 146
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 10
        Text = 'Local'
        Items.Strings = (
          'Local'
          'UTC')
      end
    end
  end
  object btnCancel: TButton
    Left = 249
    Top = 426
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object btnOK: TButton
    Left = 168
    Top = 426
    Width = 75
    Height = 25
    Caption = 'OK'
    TabOrder = 3
    OnClick = btnOKClick
  end
end

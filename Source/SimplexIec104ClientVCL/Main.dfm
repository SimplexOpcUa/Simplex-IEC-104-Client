object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'Simplex IEC 101/104 Client (VCL)'
  ClientHeight = 440
  ClientWidth = 825
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 349
    Width = 825
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    ExplicitTop = 49
    ExplicitWidth = 391
  end
  object Splitter2: TSplitter
    Left = 107
    Top = 55
    Height = 294
    ExplicitLeft = 280
    ExplicitTop = 120
    ExplicitHeight = 100
  end
  object gbxActions: TGroupBox
    Left = 0
    Top = 0
    Width = 825
    Height = 55
    Align = alTop
    Caption = 'Actions'
    TabOrder = 0
    object lblLinkStatus: TLabel
      Left = 216
      Top = 23
      Width = 84
      Height = 13
      Caption = 'Link Status: None'
    end
    object btnConnect: TButton
      Left = 32
      Top = 18
      Width = 75
      Height = 25
      Caption = 'Connect'
      TabOrder = 0
      OnClick = btnConnectClick
    end
    object btnDisconnect: TButton
      Left = 121
      Top = 18
      Width = 75
      Height = 25
      Caption = 'Disconnect'
      TabOrder = 1
      OnClick = btnDisconnectClick
    end
  end
  object gbxAsduList: TGroupBox
    Left = 0
    Top = 55
    Width = 107
    Height = 294
    Align = alLeft
    Caption = 'ASDUs'
    TabOrder = 1
    object tvAsduList: TTreeView
      Left = 2
      Top = 15
      Width = 103
      Height = 277
      Align = alClient
      DoubleBuffered = True
      HideSelection = False
      Indent = 19
      ParentDoubleBuffered = False
      ReadOnly = True
      TabOrder = 0
      OnClick = tvAsduListClick
    end
  end
  object gbxValueList: TGroupBox
    Left = 110
    Top = 55
    Width = 715
    Height = 294
    Align = alClient
    Caption = 'Values'
    TabOrder = 2
    object sgValueList: TStringGrid
      Left = 2
      Top = 15
      Width = 711
      Height = 277
      Align = alClient
      ColCount = 6
      DefaultColWidth = 116
      DoubleBuffered = True
      FixedCols = 0
      RowCount = 2
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowSelect]
      ParentDoubleBuffered = False
      TabOrder = 0
      ExplicitLeft = 1
      ExplicitTop = 14
    end
  end
  object gbxLog: TGroupBox
    Left = 0
    Top = 352
    Width = 825
    Height = 88
    Align = alBottom
    Caption = 'Log'
    TabOrder = 3
    object mmLog: TMemo
      Left = 2
      Top = 15
      Width = 821
      Height = 71
      Align = alClient
      TabOrder = 0
    end
  end
  object tmProcess: TTimer
    Interval = 100
    OnTimer = tmProcessTimer
    Left = 24
    Top = 88
  end
  object MainMenu1: TMainMenu
    Left = 508
    Top = 418
  end
end

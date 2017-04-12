object MainForm: TMainForm
  Left = 319
  Top = 197
  Caption = 'PC/SC Sample Application'
  ClientHeight = 624
  ClientWidth = 642
  Color = clBtnFace
  Constraints.MaxWidth = 658
  Constraints.MinHeight = 200
  Constraints.MinWidth = 650
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnActivate = FormActivate
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 180
    Width = 642
    Height = 8
    Cursor = crVSplit
    Align = alTop
    Beveled = True
    MinSize = 100
    ResizeStyle = rsUpdate
  end
  object TopPanel: TPanel
    Left = 0
    Top = 0
    Width = 642
    Height = 180
    Align = alTop
    BevelOuter = bvNone
    Constraints.MinHeight = 70
    ParentBackground = False
    TabOrder = 0
    object Panel3: TPanel
      Left = 0
      Top = 0
      Width = 642
      Height = 21
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      object Label1: TLabel
        Left = 8
        Top = 3
        Width = 76
        Height = 13
        Caption = 'Card Readers'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
    end
    object ReaderListBox: TListBox
      Left = 0
      Top = 21
      Width = 642
      Height = 159
      Style = lbOwnerDrawFixed
      Align = alClient
      BevelKind = bkFlat
      BorderStyle = bsNone
      ItemHeight = 30
      TabOrder = 1
      OnClick = ReaderListBoxClick
      OnDrawItem = ReaderListBoxDrawItem
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 188
    Width = 642
    Height = 436
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object Panel2: TPanel
      Left = 0
      Top = 0
      Width = 642
      Height = 101
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      object Label2: TLabel
        Left = 8
        Top = 83
        Width = 79
        Height = 13
        Caption = 'Log Messages'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label3: TLabel
        Left = 16
        Top = 48
        Width = 51
        Height = 13
        Caption = 'Command:'
      end
      object Bevel1: TBevel
        Left = 8
        Top = 80
        Width = 625
        Height = 2
      end
      object ConnectSharedButton: TButton
        Left = 86
        Top = 8
        Width = 97
        Height = 25
        Caption = 'Connect shared'
        Enabled = False
        TabOrder = 0
        OnClick = ConnectSharedButtonClick
      end
      object ConnectExclusiveButton: TButton
        Left = 187
        Top = 8
        Width = 97
        Height = 25
        Caption = 'Connect exclusive'
        Enabled = False
        TabOrder = 1
        OnClick = ConnectExclusiveButtonClick
      end
      object DisconnectButton: TButton
        Left = 390
        Top = 8
        Width = 97
        Height = 25
        Caption = 'Disconnect card'
        Enabled = False
        TabOrder = 2
        OnClick = DisconnectButtonClick
      end
      object CommandComboBox: TComboBox
        Left = 88
        Top = 45
        Width = 401
        Height = 21
        Enabled = False
        ItemHeight = 13
        MaxLength = 260
        TabOrder = 3
        OnChange = CommandComboBoxChange
        OnEnter = CommandComboBoxEnter
        OnExit = CommandComboBoxExit
        OnKeyDown = CommandComboBoxKeyDown
        OnKeyPress = CommandComboBoxKeyPress
        Items.Strings = (
          'FF CA 00 00 00')
      end
      object TransmitButton: TButton
        Left = 496
        Top = 43
        Width = 129
        Height = 25
        Caption = 'Execute command'
        Enabled = False
        TabOrder = 4
        OnClick = TransmitButtonClick
      end
      object ConnectDirectButton: TButton
        Left = 288
        Top = 8
        Width = 97
        Height = 25
        Caption = 'Connect direct'
        Enabled = False
        TabOrder = 5
        OnClick = ConnectDirectButtonClick
      end
    end
    object StatusBar1: TStatusBar
      Left = 0
      Top = 417
      Width = 642
      Height = 19
      Panels = <>
      SimplePanel = True
      SimpleText = '(C) 2013 Infintuary'
    end
    object LogMemo: TMemo
      Left = 0
      Top = 101
      Width = 642
      Height = 316
      TabStop = False
      Align = alClient
      BevelKind = bkFlat
      BorderStyle = bsNone
      Lines.Strings = (
        'LogMemo')
      ReadOnly = True
      ScrollBars = ssBoth
      TabOrder = 2
      WordWrap = False
    end
  end
  object XPManifest1: TXPManifest
    Left = 8
    Top = 192
  end
end

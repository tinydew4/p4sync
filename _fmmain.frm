object fmMain: TfmMain
  Left = 434
  Height = 240
  Top = 250
  Width = 640
  Caption = 'P4 Sync'
  ClientHeight = 240
  ClientWidth = 640
  KeyPreview = True
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  Position = poWorkAreaCenter
  LCLVersion = '6.1'
  object EdtUsername: TLabeledEdit
    Left = 72
    Height = 23
    Top = 4
    Width = 80
    EditLabel.AnchorSideTop.Control = EdtUsername
    EditLabel.AnchorSideTop.Side = asrCenter
    EditLabel.AnchorSideRight.Control = EdtUsername
    EditLabel.AnchorSideBottom.Control = EdtUsername
    EditLabel.AnchorSideBottom.Side = asrBottom
    EditLabel.Left = 7
    EditLabel.Height = 12
    EditLabel.Top = 9
    EditLabel.Width = 62
    EditLabel.Caption = '&Username '
    EditLabel.ParentColor = False
    LabelPosition = lpLeft
    TabOrder = 0
  end
  object BtnLoad: TButton
    Left = 160
    Height = 23
    Top = 4
    Width = 67
    Caption = '&Load'
    OnClick = BtnLoadClick
    TabOrder = 1
  end
  object BtnSync: TButton
    Left = 570
    Height = 23
    Top = 4
    Width = 67
    Anchors = [akTop, akRight]
    Caption = '&Sync'
    OnClick = BtnSyncClick
    TabOrder = 2
  end
  object LbWorkspace: TListBox
    Left = 0
    Height = 186
    Top = 32
    Width = 640
    Anchors = [akTop, akLeft, akRight, akBottom]
    ItemHeight = 0
    OnDblClick = LbWorkspaceDblClick
    Options = [lboDrawFocusRect]
    TabOrder = 4
  end
  object Status: TStatusBar
    Left = 0
    Height = 22
    Top = 218
    Width = 640
    Panels = <>
  end
  object BtnLogout: TButton
    Left = 228
    Height = 23
    Top = 4
    Width = 67
    Caption = 'Log&out'
    OnClick = BtnLogoutClick
    TabOrder = 3
  end
end

object fmMain: TfmMain
  Left = 429
  Height = 240
  Top = 257
  Width = 640
  Caption = 'Perforce Sync'
  ClientHeight = 240
  ClientWidth = 640
  KeyPreview = True
  Position = poWorkAreaCenter
  LCLVersion = '8.5'
  OnKeyDown = FormKeyDown
  object EdtUsername: TLabeledEdit
    Left = 72
    Height = 23
    Top = 4
    Width = 80
    EditLabel.Height = 23
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
    TabOrder = 1
    OnClick = BtnLoadClick
  end
  object BtnSync: TButton
    Left = 570
    Height = 23
    Top = 4
    Width = 67
    Anchors = [akTop, akRight]
    Caption = '&Sync'
    TabOrder = 2
    OnClick = BtnSyncClick
  end
  object LbWorkspace: TListBox
    Left = 0
    Height = 186
    Top = 32
    Width = 640
    Anchors = [akTop, akLeft, akRight, akBottom]
    ItemHeight = 0
    TabOrder = 4
    OnDblClick = LbWorkspaceDblClick
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
    TabOrder = 3
    OnClick = BtnLogoutClick
  end
end

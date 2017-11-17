Option Strict On
Option Explicit On
Option Infer Off
Imports System.ComponentModel

Public Interface IHeaderControlContainer
    Property IsDragging As Boolean
    Sub AddEventHandlers()
    Sub RemoveEventHandlers()
End Interface
Public Class Dashboard
    Implements IHeaderControlContainer
    Dim Drag As Boolean
    Dim MouseX, MouseY As Integer
    Dim WithEvents TestTask As New PSTask(True, "Testtittel", "C:\Users\Magnus\Desktop\G4Test\Test1.ps1")
    Private Sub Dashboard_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        ' VIKTIG!!!!!!!!
        ' Først må man sette ExecutionPolicy til RemoteSigned (eller bruke signaturer) PÅ x86-VERSJONEN av PowerShell.
        WindowControl = New MultiTabWindow(Me)
        OverviewTab = New Overview(WindowControl)
        TemplatesTab = New TemplateBrowser(WindowControl)
        SchedulesTab = New ScheduleBrowser(WindowControl)
        RoutinesTab = New RoutineBrowser(WindowControl)
        TasksTab = New TaskBrowser(WindowControl)
        With WindowControl
            .BackColor = Color.White
            .AddTab(OverviewTab)
            .AddTab(TemplatesTab)
            .AddTab(SchedulesTab)
            .AddTab(RoutinesTab)
            .AddTab(TasksTab)
            .ShowTab(0)
        End With
        With Header
            .Parent = Me
            .BringToFront()
        End With
        AddHandler Header.ViewClicked, AddressOf Header_ViewClicked
        AddHeaderEventHandlers()
        Loader.LoadAll()

        'TestTask.Execute(New String() {"Første!!", "Andre!!"})
        'Dim TempList As TemplateList = ComponentFileReader.CreateTemplateList()
        'Dim Test As Template = ComponentFileReader.CreateTemplate("SomeTemplate")
        'MsgBox(Test.varScheduleList.Items.Count)
    End Sub
    Protected Overrides Sub OnSizeChanged(e As EventArgs)
        MyBase.OnSizeChanged(e)
        If WindowState = FormWindowState.Maximized Then
            WindowState = FormWindowState.Normal
        End If
    End Sub
    Private Property IsDragging As Boolean Implements IHeaderControlContainer.IsDragging
        Get
            Return Drag
        End Get
        Set(value As Boolean)
            Drag = value
        End Set
    End Property
    Private Sub Header_ViewClicked(Sender As Object, e As HeaderViewEventArgs)
        Dim CurrentView As HeaderControl.Views = e.CurrentDirectory.Last
        If Not e.ClickTarget = CurrentView Then
            Dim SwitchView As Boolean = True
            Select Case CurrentView
                Case HeaderControl.Views.Overview
                    ' Nowhere to go
                Case HeaderControl.Views.TemplateBrowser
                    SwitchView = TemplatesTab.VerifyViewerDiscard
                Case HeaderControl.Views.ScheduleBrowser
                    SwitchView = SchedulesTab.VerifyViewerDiscard
                Case HeaderControl.Views.RoutineBrowser
                    SwitchView = RoutinesTab.VerifyViewerDiscard
                Case HeaderControl.Views.TaskBrowser
                    SwitchView = TasksTab.VerifyViewerDiscard
                Case Else ' Invalid
                    Throw New Exception("Invalid CurrentDirectory")
            End Select
            If SwitchView Then
                ShowView(e.ClickTarget)
            End If
        End If
    End Sub
    Private Sub ShowView(ByVal View As HeaderControl.Views)
        'Select Case View
        'Case HeaderControl.Views.Overview
        WindowControl.ShowTab(View)
        'End Select
    End Sub
    Public Sub AddHeaderEventHandlers() Implements IHeaderControlContainer.AddEventHandlers
        AddHandler Header.MouseDown, AddressOf Header_MouseDown
        AddHandler Header.MouseUp, AddressOf Header_MouseUp
        AddHandler Header.MouseMove, AddressOf Header_MouseMove
        AddHandler Header.ExitClicked, AddressOf ExitClicked
        AddHandler Header.MinimizeClicked, AddressOf MinimizeClicked
    End Sub
    Public Sub RemoveHeaderEventHandlers() Implements IHeaderControlContainer.RemoveEventHandlers
        RemoveHandler Header.MouseDown, AddressOf Header_MouseDown
        RemoveHandler Header.MouseUp, AddressOf Header_MouseUp
        RemoveHandler Header.MouseMove, AddressOf Header_MouseMove
        RemoveHandler Header.ExitClicked, AddressOf ExitClicked
        RemoveHandler Header.MinimizeClicked, AddressOf MinimizeClicked
    End Sub
    Protected Overrides Sub OnClick(e As EventArgs)
        MyBase.OnClick(e)
        Focus()
    End Sub
    Private Sub ExitClicked(Sender As Object, e As MouseEventArgs)
        End
    End Sub
    Private Sub MinimizeClicked(Sender As Object, e As MouseEventArgs)
        WindowState = FormWindowState.Minimized
    End Sub
    Private Sub Header_MouseDown(Sender As Object, e As MouseEventArgs)
        IsDragging = True
        MouseX = Cursor.Position.X - Left
        MouseY = Cursor.Position.Y - Top
    End Sub
    Private Sub Header_MouseUp(Sender As Object, e As MouseEventArgs)
        IsDragging = False
    End Sub
    Private Sub Header_MouseMove(Sender As Object, e As MouseEventArgs)
        If IsDragging Then
            Top = Cursor.Position.Y - MouseY
            Left = Cursor.Position.X - MouseX
        End If
    End Sub
    Private Sub TaskCompleted(Sender As Object, e As PSTaskCompletedEventArgs) Handles TestTask.TaskCompleted
        'MsgBox(e.Result.Result.Item(0).ToString)
    End Sub
End Class
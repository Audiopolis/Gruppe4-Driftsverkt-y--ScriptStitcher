Module Globals
    Public RalewayFonts As New RalewayFontCollection
    Public WithEvents Header As New HeaderControl
    Public GradientFirstColor As Color = Color.FromArgb(0, 130, 190)
    Public GradientSecondColor As Color = Color.FromArgb(10, 135, 150)
    Public ApplicationBackColor As Color = Color.FromArgb(0, 112, 158)
    Public ApplicationGradientBrush As Drawing2D.LinearGradientBrush

    Public WindowControl As MultiTabWindow
    ' Tabs
    Public OverviewTab As Overview
    Public TemplatesTab As TemplateBrowser
    Public SchedulesTab As ScheduleBrowser
    Public RoutinesTab As RoutineBrowser
    Public TasksTab As TaskBrowser

    Public ActiveTemplate As TemplateEntry
    Public ConfigLoaded, IsRunning As Boolean
    Public PSEngine As New PSExecutionEngine

    Public GarbageBinName As String = "Garbage Bin"
    ' TODO: Remove header resource and use Header.Height instead of My.Resources.Header.Height

    Public NothingSelectedTitleFont As Font = RalewayFonts.GetFont(RalewayFontCollection.FontVariant.Bold, 24, FontStyle.Bold)
    Public NothingSelectedSubTitleFont As Font = RalewayFonts.GetFont(RalewayFontCollection.FontVariant.Light, 12, FontStyle.Regular)
    Public TitleText As String = "nothing selected", SubTitleText As String = "pick an item from the list on the left"
    Public Function GetNothingSelectedRects(NothingSelectedScreenSize As Size) As Rectangle()
        Dim TitleSize As Size = TextRenderer.MeasureText(TitleText, NothingSelectedTitleFont)
        Dim SubTitleSize As Size = TextRenderer.MeasureText(SubTitleText, NothingSelectedSubTitleFont)
        Dim TotalHeight As Integer = TitleSize.Height + SubTitleSize.Height
        Dim TitleRect As New Rectangle(New Point((NothingSelectedScreenSize.Width - SubTitleSize.Width) \ 2, (NothingSelectedScreenSize.Height - TotalHeight) \ 2), TitleSize)
        Dim SubTitleRect As New Rectangle(New Point(TitleRect.Left + 3, TitleRect.Bottom), SubTitleSize)
        Return {TitleRect, SubTitleRect}
    End Function
    Public Sub LoadActiveTemplate()
        ReportActiveTemplateLoading(True)
        AsyncFileReader.OperationsQueue.AddOperation(AsyncFileReader.OperationsQueue.FileOperation.ReadLines(ApplicationPath & "\Data\Config.txt"), AddressOf ActiveTemplateLoaded, Nothing)
    End Sub
    Private Sub ActiveTemplateLoaded(e As FileOperationEventArgs)
        Dim Lines As String() = DirectCast(e.Result, String())
        For Each Line As String In Lines
            If Line.StartsWith("$template: ") Then
                Dim TemplateName As String = Line.Split("==".ToCharArray, StringSplitOptions.None).Last
                SetActiveTemplate(TemplateName)
                TemplatesTab.ReloadList()
                Exit For
            End If
        Next
    End Sub
    Public Sub SetActiveTemplate(TemplateName As String)
        Dim Template As TemplateEntry = Loader.LookupTemplate(TemplateName)
        SetActiveTemplate(Template)
    End Sub
    Public Sub SetActiveTemplate(Template As TemplateEntry)
        ActiveTemplate = Template
        If Template IsNot Nothing Then
            OverviewTab.Labels(1).SetStatusText(ActiveTemplate.Name)
            AsyncFileReader.OperationsQueue.AddOperation(AsyncFileReader.OperationsQueue.FileOperation.OverwriteFile(ApplicationPath & "\Data\Config.txt", New String() {"$template: %name%==" & ActiveTemplate.Name}), AddressOf ActiveTemplateChanged, Nothing)
        Else
            OverviewTab.Labels(1).SetStatusText("None")
            AsyncFileReader.OperationsQueue.AddOperation(AsyncFileReader.OperationsQueue.FileOperation.OverwriteFile(ApplicationPath & "\Data\Config.txt", New String() {"$template: %name%==" & "None"}), AddressOf ActiveTemplateChanged, Nothing)
        End If
    End Sub
    Private Sub ReportActiveTemplateLoading(Loading As Boolean)
        ConfigLoaded = (Not Loading)
        If ConfigLoaded Then OverviewTab.Labels(1).ForeColor = Color.White Else OverviewTab.Labels(1).ForeColor = Color.FromArgb(180, 210, 220)
        OverviewTab.Invalidate()
    End Sub
    Private Sub ActiveTemplateChanged(e As FileOperationEventArgs)
        ReportActiveTemplateLoading(False)
        If ActiveTemplate Is Nothing Then OverviewTab.Labels(0).ForeColor = Color.FromArgb(180, 210, 220) Else OverviewTab.Labels(0).ForeColor = Color.White
    End Sub
    Public Sub StartStopService(Start As Boolean)
        If ConfigLoaded AndAlso ActiveTemplate IsNot Nothing Then
            Dim EditButton As EditButton = OverviewTab.Labels(0).UnpositionedEditButtons.First
            With EditButton
                If Start AndAlso Not IsRunning AndAlso ActiveTemplate IsNot Nothing Then
                    IsRunning = True
                    OverviewTab.Labels(1).ForeColor = Color.FromArgb(180, 210, 220)
                    OverviewTab.Labels(0).ForeColor = Color.White
                    .Arguments = False
                    .Text = "Pause"
                    PSEngine.Start()
                    .Parent.SetStatusText("Running")
                    OverviewTab.Invalidate()
                ElseIf IsRunning Then
                    OverviewTab.Labels(0).ForeColor = Color.FromArgb(180, 210, 220)
                    OverviewTab.Labels(1).ForeColor = Color.FromArgb(180, 210, 220)
                    .Arguments = True
                    .Parent.SetStatusText("Pausing...")
                    .Text = "Start"
                    PSEngine.RequestPause()
                    OverviewTab.Invalidate()
                End If
            End With
        End If
    End Sub
    Public Sub PSEngine_Paused(Sender As Object, e As ExecutionPausedEventArgs)
        IsRunning = False
        OverviewTab.Labels(0).ForeColor = Color.White
        OverviewTab.Labels(1).ForeColor = Color.White
        OverviewTab.Labels(0).SetStatusText("Paused")
        OverviewTab.Invalidate()
    End Sub
End Module
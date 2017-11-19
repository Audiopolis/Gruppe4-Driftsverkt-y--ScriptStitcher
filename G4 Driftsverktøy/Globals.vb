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
    Public Sub SetActiveTemplate(Template As TemplateEntry)
        ActiveTemplate = Template
        OverviewTab.Labels(1).SetStatusText(ActiveTemplate.Name)
        AsyncFileReader.Queue.AddOperation(AsyncFileReader.Queue.FileOperation.OverwriteFile(ApplicationPath & "\Data\Config.txt", New String() {"$template: %name%==" & ActiveTemplate.Name}), Nothing, Nothing) ' TODO: Load on startup
    End Sub
End Module
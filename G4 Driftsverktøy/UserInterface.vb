Option Strict On
Option Explicit On
Option Infer Off
Imports System.Drawing.Imaging
Imports System.Threading
Imports AudiopoLib
Imports G4_Driftsverktøy

Public Module UserInterface


    Public Class TaskBrowser
        Inherits Tab
        Private TaskCategoriesContainer As New AutoSizedContainer
        Private WithEvents TasksInCategoryContainer As New AutoSizedContainer
        Private WithEvents CategoriesList As New CategoryList
        Private WithEvents TasksInSelectedCategory As New TasksInCategoryList
        'Private NewCategory As New NewCategoryControl
        Private WithEvents Viewer As TaskViewer
        'Private CurrentX As Integer = 0, LastX As Integer = 20
        'Private Stage As Integer = 0
        'Private SC As SynchronizationContext = SynchronizationContext.Current
        Private varCurrentCategoryName As String
        Private TaskNameToSelectAfterReload As String = Nothing
        Private RenameOccurred As Boolean
        'Private WithEvents SlideTimer As New Timers.Timer(1000 / 60)
        Public Sub New(Parent As MultiTabWindow)
            MyBase.New(Parent)
            'SlideTimer.AutoReset = False
            CategoriesList.BackColor = ApplicationBackColor
            With TaskCategoriesContainer
                .HeaderText = "task categories"
                .Location = New Point(0, My.Resources.Header.Height)
                .InnerControl = CategoriesList
                .Parent = Me
                .Padding = New Padding(0)
                .BorderWidth = 0
                .SetTotalSize(New Size(CategoriesList.Width, Height - Header.Height))
                .AddHeaderControl(New Image() {My.Resources.ReloadIconDefault, My.Resources.ReloadIconHover, My.Resources.ReloadIconPress}, AddressOf ReloadCategoriesIcon_Clicked)
                .AddHeaderControl(New Image() {My.Resources.AddIconDefault, My.Resources.AddIconHover, My.Resources.AddIconPress}, AddressOf AddCategoryIcon_Clicked)
            End With
            With TasksInCategoryContainer
                .HeaderText = "tasks > category"
                .Location = New Point(0, My.Resources.Header.Height)
                .InnerControl = TasksInSelectedCategory
                .Parent = Me
                .SetTotalSize(New Size(CategoriesList.Width, Height - Header.Height))
                .AddHeaderControl(New Image() {My.Resources.ReloadIconDefault, My.Resources.ReloadIconHover, My.Resources.ReloadIconPress}, AddressOf ReloadRoutinesIcon_Clicked)
                .AddHeaderControl(New Image() {My.Resources.AddIconDefault, My.Resources.AddIconHover, My.Resources.AddIconPress}, AddressOf AddTaskIcon_Clicked)
                .AddHeaderControl(New Image() {My.Resources.BackButtonDefault, My.Resources.BackButtonHover, My.Resources.BackButtonPress}, AddressOf BackIcon_Clicked)
            End With
            Viewer = New TaskViewer(New Rectangle(New Point(TaskCategoriesContainer.Right, My.Resources.Header.Height), New Size(Width - TaskCategoriesContainer.Right, Height - My.Resources.Header.Height)), Me)
            Viewer.BringToFront()
            AddHandler Loader.AllTasksLoaded, AddressOf Loader_AllTasksLoaded
        End Sub
        Public Function VerifyViewerDiscard() As Boolean
            Return Viewer.VerifyDiscard
        End Function
        Private Sub Loader_AllTasksLoaded(e As EventArgs)
            ReloadCategories()
        End Sub
        Private Sub Viewer_TaskChanged(Sender As Object, e As CurrentComponentChangedEventArgs) Handles Viewer.TaskChanged
            If Viewer.CurrentTask IsNot Nothing AndAlso Loader.TaskCategoryExists(Viewer.CurrentTask.CategoryName) Then
                Dim Match As CategoryListItem = CategoriesList.FindName(Viewer.CurrentTask.CategoryName)
                Match.IsActive = True
                CategoriesList.SetAllActive(False, Match.Index)
            Else
                CategoriesList.SetAllActive(False)
            End If
        End Sub
        Public Property CurrentCategoryName As String
            Get
                Return varCurrentCategoryName
            End Get
            Set(value As String)
                varCurrentCategoryName = value
                TasksInCategoryContainer.HeaderText = "tasks   " & varCurrentCategoryName
            End Set
        End Property
        Private Sub SchedulesInCategoryContainer_HeaderPaint(Sender As Object, e As PaintEventArgs) Handles TasksInCategoryContainer.HeaderPaint
            With e.Graphics
                Dim DrawImage As Image = My.Resources.SmallArrowBlack
                Dim ImageSize As Size = DrawImage.Size
                Dim TextLength As Integer = TextRenderer.MeasureText(e.Graphics, "tasks", TasksInCategoryContainer.Font).Width + TasksInCategoryContainer.TextPadding - 3
                Dim ImageLocation As New Point(TextLength, CInt(.ClipBounds.Height - ImageSize.Height) \ 2 + 1)
                e.Graphics.DrawImage(DrawImage, ImageLocation)
            End With
        End Sub
        Private Sub CategoriesList_CategoryEdited(Sender As Object, e As CategoryEditedEventArgs) Handles CategoriesList.CategoryEdited
            Dim OldName As String = e.Trigger.OldCategoryName
            Dim NewName As String = e.Trigger.NewCategoryName
            If Viewer.VerifyDiscard Then
                If Loader.TaskCategoryExists(NewName) Then
                    MsgBox("There is already a category with that name.")
                Else
                    RenameCategory(OldName, NewName, True)
                End If
            Else
                e.CategoryItem.Value = New CategoryListItemValue(OldName)
            End If
        End Sub
        Private Sub CategoriesList_CategoryDropped(Sender As Object, e As CategoryDroppedEventArgs) Handles CategoriesList.CategoryDropped
            If Viewer.VerifyDiscard Then
                RenameOccurred = True
                Dim OldCategoryName As String = DirectCast(e.Trigger.Data.GetData(DataFormats.StringFormat), String)
                Viewer.CurrentTask.ChangeCategoryAndMoveFile(e.TargetItem.Value.CategoryName, AddressOf CategoryChanged, Viewer.CurrentTask)
            End If
        End Sub
        Private Sub CategoryChanged(e As FileOperationEventArgs)
            If e.ErrorOccurred Then
                MsgBox(e.Exception.Message)
            Else
                Dim TargetComponent As TaskEntry = DirectCast(e.State, TaskEntry)
                RequestView(TargetComponent.Name)
                'SelectCategory(TargetComponent.CategoryName)
                'SelectTask(TargetCompon)
            End If
        End Sub
        Private Sub CategoriesList_CategoryDeleted(Sender As Object, e As CategoryDeletedEventArgs) Handles CategoriesList.CategoryDeleted
            Dim Value As CategoryListItemValue = e.CategoryItem.Value
            If Value.CategoryName = GarbageBinName Then
                MsgBox("You cannot delete the garbage bin.")
            Else
                Dim TargetItems As IEnumerable(Of ComponentEntry) = Loader.TasksInCategory(Value.CategoryName)
                If TargetItems.Count > 0 Then
                    If MsgBox("Are you sure you want to delete this category? All items contained in it will be moved to the garbage bin.", MsgBoxStyle.OkCancel) = MsgBoxResult.Ok Then
                        For Each Item As ComponentEntry In TargetItems
                            Dim OldDirectory As String = Item.GetFullFilePath
                            Dim NewDirectory As String = ApplicationPath & "\Data\" & GarbageBinName & "\Tasks\" & Item.GetFileNameFromName
                            AsyncFileReader.Queue.AddOperation(AsyncFileReader.Queue.FileOperation.MoveFile(OldDirectory, NewDirectory, True), AddressOf MoveToBinFinished, Nothing)
                        Next
                    End If
                Else
                    Loader.RemoveTaskCategory(Value.CategoryName)
                    AsyncFileReader.Queue.AddOperation(AsyncFileReader.Queue.FileOperation.DeleteFolder(TaskEntry.BaseDirectory & "\" & Value.CategoryName), AddressOf DeleteFolderFinished, Nothing)
                End If
            End If
        End Sub
        Private Sub MoveToBinFinished(e As FileOperationEventArgs)
            Dim OriginalPath As String = DirectCast(e.Arguments("OriginalPath"), String)
            Dim NewPath As String = DirectCast(e.Arguments("NewPath"), String)
            Dim OriginalName As String = IO.Path.GetFileNameWithoutExtension(OriginalPath)
            Dim NewName As String = IO.Path.GetFileNameWithoutExtension(NewPath)
            Dim CategoryName As String = FormatConverter.ExtractHighestLevelDirectory(OriginalPath, True)
            If e.ErrorOccurred Then
                MsgBox("An unexpected error occurred (code 10).")
            Else
                Loader.RemoveTask(OriginalName)
                If Loader.TasksInCategory(CategoryName).Count = 0 Then
                    Loader.RemoveTaskCategory(CategoryName)
                    Dim OldFolder As String = IO.Directory.GetParent(OriginalPath).FullName
                    If CurrentCategoryName = CategoryName Then Viewer.CurrentTask = Nothing
                    AsyncFileReader.Queue.AddOperation(AsyncFileReader.Queue.FileOperation.DeleteFolder(OldFolder), AddressOf DeleteFolderFinished, Nothing)
                End If
            End If
        End Sub
        Private Sub DeleteFolderFinished(e As FileOperationEventArgs)
            If e.ErrorOccurred Then
                MsgBox("Could not delete " & DirectCast(e.Arguments("Path"), String) & ": " & e.Exception.Message)
            End If
            CurrentCategoryName = Nothing
            TaskNameToSelectAfterReload = Nothing
            ReloadCategories()
        End Sub
        Private Sub RenameCategory(OldName As String, NewName As String, Optional ClearList As Boolean = False)
            If Not RenameOccurred Then
                RenameOccurred = True
                If ClearList Then CategoriesList.Clear()
                AsyncFileReader.Queue.AddOperation(AsyncFileReader.Queue.FileOperation.RenameFolder(TaskEntry.BaseDirectory & "\" & OldName, NewName, False), AddressOf RenameFinished, Nothing)
            End If
        End Sub
        Private Sub RenameFinished(e As FileOperationEventArgs)
            If e.ErrorOccurred Then
                MsgBox("Could not rename category: " & e.Exception.Message)
            Else
                Dim OldName As String = FormatConverter.ExtractHighestLevelDirectory(DirectCast(e.Arguments("Path"), String), False)
                Dim NewName As String = DirectCast(e.Result, String)
                Loader.RenameTaskCategory(OldName, NewName)
                CurrentCategoryName = NewName
                If Viewer.CurrentTask IsNot Nothing Then
                    TaskNameToSelectAfterReload = Viewer.CurrentTask.Name
                Else
                    TaskNameToSelectAfterReload = Nothing
                End If
                ReloadCategories()
                ReloadTasks()
            End If
        End Sub
        Private Sub BackIcon_Clicked(Sender As Object, e As MouseEventArgs)
            NavigateBack()
        End Sub
        Private Sub ReloadCategoriesIcon_Clicked(Sender As Object, e As MouseEventArgs)
            ReloadCategories()
        End Sub
        Private Sub ReloadRoutinesIcon_Clicked(Sender As Object, e As MouseEventArgs)
            If Viewer.VerifyDiscard Then
                If Viewer.CurrentTask IsNot Nothing Then TaskNameToSelectAfterReload = Viewer.CurrentTask.Name
                ReloadTasks()
            End If
        End Sub
        Public Sub RequestView(ByVal TaskName As String)
            Dim Match As TaskEntry = Loader.LookupTask(TaskName)
            If Match IsNot Nothing Then
                SelectCategory(Match.CategoryName)
                Dim ItemMatch As TasksInCategoryListItem = TasksInSelectedCategory.Items.Find(Function(x As TasksInCategoryListItem) As Boolean
                                                                                                  Return (x.Value.Name = TaskName)
                                                                                              End Function)
                If ItemMatch IsNot Nothing Then
                    ItemMatch.IsSelected = True
                End If
            Else
                MsgBox("No entry match: " & TaskName)
            End If
        End Sub
        Public Sub NavigateBack()
            TasksInCategoryContainer.Hide()
            TaskCategoriesContainer.Show()
        End Sub
        Public Sub ShowItemsInCategoryList()
            TaskCategoriesContainer.Hide()
            TasksInCategoryContainer.Show()
        End Sub
        Private Sub ReloadCategories()
            'Viewer.ChangesMade = False
            AsyncFileReader.Queue.AddOperation(AsyncFileReader.Queue.FileOperation.ListDirectories(TaskEntry.BaseDirectory), AddressOf ReloadCategories_Finished, Nothing)
        End Sub
        Private Sub ReloadTasks()
            AsyncFileReader.Queue.AddOperation(AsyncFileReader.Queue.FileOperation.ListFiles(TaskEntry.BaseDirectory & "\" & CurrentCategoryName), AddressOf ReloadTasks_Finished, Nothing)
        End Sub
        Private Sub ReloadCategories_Finished(e As FileOperationEventArgs)
            Dim Directories() As String = DirectCast(e.Result, String())
            CategoriesList.Clear()
            Dim iLast As Integer = Directories.Count - 1
            If iLast >= 0 Then
                For i As Integer = 0 To Directories.Count - 1
                    Directories(i) = FormatConverter.ExtractHighestLevelDirectory(Directories(i), False)
                    Dim NewItem As New CategoryListItem(Directories(i)) With {.CanEdit = True}
                    If Viewer.CurrentTask IsNot Nothing Then NewItem.IsActive = (Directories(i) = Viewer.CurrentTask.CategoryName)
                    If Not Loader.TaskCategories.Contains(Directories(i)) Then
                        Loader.AddTaskCategory(Directories(i))
                    End If
                    CategoriesList.Add(NewItem)
                Next
            End If
            'Viewer.RefreshLists()
            'ScheduleNameToSelectAfterReload = Nothing
        End Sub
        Private Sub ReloadTasks_Finished(e As FileOperationEventArgs)
            Dim Files() As String = DirectCast(e.Result, String())
            TasksInSelectedCategory.Clear()
            Dim iLast As Integer = Files.Count - 1
            If iLast >= 0 Then
                For i As Integer = 0 To iLast
                    Dim TaskName As String = FormatConverter.ExtractFileName(Files(i), False)
                    TasksInSelectedCategory.Add(New TasksInCategoryListItem(New ItemsInCategoryListItemValue(TaskName)))
                Next
            End If
            If TaskNameToSelectAfterReload = Nothing AndAlso Viewer.CurrentTask IsNot Nothing Then TaskNameToSelectAfterReload = Viewer.CurrentTask.Name
            For Each Item As TasksInCategoryListItem In TasksInSelectedCategory.Items
                If Item.Value.Name = TaskNameToSelectAfterReload Then
                    Item.IsSelected = True
                    Exit For
                End If
            Next
            'Viewer.RefreshLists()
            TaskNameToSelectAfterReload = Nothing
            RenameOccurred = False
        End Sub
        Private Sub Viewer_ChangesSaved(Sender As Object, e As EventArgs) Handles Viewer.ChangesSaved, Viewer.TaskDeleted
            ReloadTasks()
            ReloadCategories()
        End Sub
        Protected Overrides Sub OnTabShown(e As EventArgs)
            MyBase.OnTabShown(e)
            Header.CurrentDirectiry = {HeaderControl.Views.Overview, HeaderControl.Views.TemplateBrowser, HeaderControl.Views.ScheduleBrowser, HeaderControl.Views.RoutineBrowser, HeaderControl.Views.TaskBrowser}
            Viewer.CurrentTask = Viewer.CurrentTask
        End Sub
        Private Sub AddCategoryIcon_Clicked(Sender As Object, e As EventArgs)
            CreateCategory()
        End Sub
        Private Sub AddTaskIcon_Clicked(Sender As Object, e As EventArgs)
            If Viewer.VerifyDiscard Then AddNewTask()
        End Sub
        Private Sub CreateCategory()
            AsyncFileReader.Queue.AddOperation(AsyncFileReader.Queue.FileOperation.CreateDirectory(TaskEntry.BaseDirectory & "\New category", True), AddressOf CategoryCreated, Nothing)
        End Sub
        Private Sub CategoryCreated(e As FileOperationEventArgs)
            Dim NewName As String = FormatConverter.ExtractHighestLevelDirectory(DirectCast(e.Result, String), False)
            Loader.AddTaskCategory(NewName)
            ReloadCategories()
        End Sub
        Private Sub AddNewTask()
            AsyncFileReader.Queue.AddOperation(AsyncFileReader.Queue.FileOperation.CreateFile(TaskEntry.BaseDirectory & "\" & CurrentCategoryName, "New Task.txt", True, FormatConverter.GetBytesFromString("// meta" & vbNewLine & "$meta: %desc%==This task does not have a description yet" & vbNewLine & "// parameters" & vbNewLine & "$parameter: %name%==FirstParameter && %type%==STR && %comment%==First word to print" & "$parameter: %name%==SecondParameter && %type%==STR && %comment%==Second word to print"), TaskEntry.BaseDirectory), AddressOf AddNewTask_Finished, Nothing)
        End Sub
        Private Sub AddNewTask_Finished(e As FileOperationEventArgs)
            If Not e.ErrorOccurred Then
                TaskNameToSelectAfterReload = IO.Path.GetFileNameWithoutExtension(DirectCast(e.Result, String))
            Else
                MsgBox("An unexpected error occurred (code 6): " & e.Exception.Message)
            End If
            ReloadTasks()
        End Sub
        Protected Overrides Sub OnSizeChanged(e As EventArgs)
            MyBase.OnSizeChanged(e)
            If TaskCategoriesContainer IsNot Nothing Then
                With TaskCategoriesContainer
                    .SetTotalSize(New Size(.Width, Height - My.Resources.Header.Height))
                End With
            End If
        End Sub
        Private Sub CategorySelected(Sender As Object, e As ListItemSelectionChangedEventArgs) Handles CategoriesList.ItemSelectionChanged
            If e.Data.Selected Then
                Dim SenderItem As CategoryListItem = DirectCast(e.SenderItem, CategoryListItem)
                SelectCategory(SenderItem.Value.CategoryName)
            End If
        End Sub
        Private Sub SelectCategory(ByVal NewCategoryName As String)
            CategoriesList.DeselectAll()
            If NewCategoryName <> CurrentCategoryName Then
                TasksInSelectedCategory.Clear()
                'Viewer.CurrentSchedule = Nothing
                Dim Tasks As List(Of TaskEntry) = Loader.TasksInCategory(NewCategoryName)
                For Each Task As TaskEntry In Tasks
                    TasksInSelectedCategory.Add(New TasksInCategoryListItem(New ItemsInCategoryListItemValue(Task.Name)))
                Next
            End If
            CurrentCategoryName = NewCategoryName
            If Viewer.CurrentTask IsNot Nothing Then
                Dim iLast As Integer = TasksInSelectedCategory.Items.Count - 1
                If iLast >= 0 Then
                    For i As Integer = 0 To iLast
                        If TasksInSelectedCategory.Item(i).Value.Name = Viewer.CurrentTask.Name Then
                            TasksInSelectedCategory.Item(i).IsSelected = True
                            Exit For
                        End If
                    Next
                End If
            End If
            TaskCategoriesContainer.Hide()
            TasksInCategoryContainer.Show()
        End Sub
        Private Sub TaskSelected(Sender As Object, e As ListItemSelectionChangedEventArgs) Handles TasksInSelectedCategory.ItemSelectionChanged
            If e.Data.Selected Then
                Dim SenderItem As TasksInCategoryListItem = DirectCast(e.SenderItem, TasksInCategoryListItem)
                SelectTask(SenderItem)
            End If
        End Sub
        Private Overloads Sub SelectTask(SelectItem As TasksInCategoryListItem)
            'RoutinesInSelectedCategory.DeselectAll(SelectItem.Index)
            If Viewer.CurrentTask IsNot Nothing AndAlso SelectItem.Value.Name <> Viewer.CurrentTask.Name Then
                If Viewer.VerifyDiscard Then
                    TasksInSelectedCategory.DeselectAll(SelectItem.Index)
                    Viewer.ViewTask(TaskEntry.BaseDirectory & "\" & CurrentCategoryName & "\" & SelectItem.Value.Name & ".txt")
                Else
                    SelectItem.IsSelected = False
                End If
            ElseIf Viewer.CurrentTask IsNot Nothing Then ' Same task selected
                If RenameOccurred Then ' Need to update stuff (easy: reload)
                    Viewer.ViewTask(TaskEntry.BaseDirectory & "\" & CurrentCategoryName & "\" & SelectItem.Value.Name & ".txt", True, True) ' True: Don't load from memory
                End If ' Otherwise, no need to reload
            Else ' No task is being viewed
                Viewer.ViewTask(TaskEntry.BaseDirectory & "\" & CurrentCategoryName & "\" & SelectItem.Value.Name & ".txt")
            End If
            RenameOccurred = False
        End Sub
    End Class
    Public Class TaskViewer
        Inherits Control
        Private VarCurrentTask As TaskEntry = Nothing
        Private varPadding As New Padding(10)
        Private varUseCompatibleTextRendering As Boolean = True
        'Private varLoadingSurface As New PictureBox
        'Private varLoadingGraphics As LoadingGraphics(Of PictureBox)
        Private InnerRect As Rectangle = New Rectangle(New Point(varPadding.Left, varPadding.Top), New Size(Width - varPadding.Left - varPadding.Right, 100))
        Private CancelButton, SaveButton As New PictureBox
        Private ElementBrush As SolidBrush
        Private varElementColor As Color = Color.FromArgb(200, 200, 200)
        'Private PropertiesListContainer, RoutineListContainer As AutoSizedContainer
        Private PropertiesListContainer As AutoSizedContainer

        Private WithEvents PropertiesList As New PropertyList
        'Private WithEvents TasksInRoutine As New TasksInRoutineList
        Private NameItem, DescriptionItem, ScriptItem As ComponentViewItem
        Private CategoryItem As CategoryComponentViewItem
        Private PathItem As ComponentViewItem
        'Private WithEvents TaskPicker As New TaskPicker
        Public Event TaskChanged(Sender As Object, e As CurrentComponentChangedEventArgs)
        Public Event ChangesSaved(Sender As Object, e As CurrentComponentChangedEventArgs)
        Public Event TaskDeleted(Sender As Object, e As EventArgs)
        Private varChangesMade As Boolean
        Private WithEvents NothingSelectedScreen As New PictureBox
        Private Sub PropertiesList_ScrollHandleVisibleChanged(Sender As Object, e As VisibleEventArgs) Handles PropertiesList.ScrollHandleVisibleChanged
            With PropertiesListContainer
                If e.Visible Then
                    .Padding = New Padding(0, 0, 0, 4)
                Else
                    .Padding = New Padding(0, 0, 0, 0)
                End If
                .SetTotalSize(New Size(Width, 5 * 80 + .HeaderHeight))
            End With
        End Sub
        Public Property ChangesMade As Boolean
            Get
                Return varChangesMade
            End Get
            Set(value As Boolean)
                varChangesMade = value
            End Set
        End Property
        Private Property NothingSelected As Boolean
            Get
                Return (CurrentTask Is Nothing)
            End Get
            Set(value As Boolean)
                If value Then
                    NothingSelectedScreen.Show()
                Else
                    NothingSelectedScreen.Hide()
                End If
            End Set
        End Property
        'Private Sub TaskPicker_ItemsAdded(Sender As Object, e As ItemsAddedEventArgs) Handles TaskPicker.AddClicked
        '    For Each Item As ChildComponentInformation In e.SelectedItems
        '        If CurrentTask.Tasks.Find(Function(x As ChildComponentInformation) As Boolean
        '                                      Return (x.Name = Item.Name)
        '                                  End Function) IsNot Nothing Then
        '            ' TODO: Remove this check. Multiple routines allowed.
        '        End If
        '        Dim NewInfo As TaskInformation = DirectCast(Item, TaskInformation).Copy
        '        TasksInRoutine.Add(New TasksInRoutineListItem(New TasksInRoutineListItemValue(NewInfo)) With {.Index = TasksInRoutine.Items.Count})
        '        AddHandler TasksInRoutine.Items.Last.RemoveClicked, AddressOf RemoveClicked
        '        AddHandler TasksInRoutine.Items.Last.EditClicked, AddressOf EditClicked
        '        ' TODO: Add handlers for routine editing controls
        '        ChangesMade = True
        '    Next
        '    ExitPicker()
        'End Sub
        'Private Sub ScheduleBrowser_Cancelled(Sender As Object, e As EventArgs) Handles TaskPicker.CancelClicked
        '    ExitPicker()
        'End Sub
        'Private Sub ExitPicker()
        '    TaskPicker.SetDirectory()
        '    TaskPicker.Hide()
        '    TasksInRoutine.Show()
        'End Sub
        Public Property CurrentTask As TaskEntry
            Get
                Return VarCurrentTask
            End Get
            Set(value As TaskEntry)
                VarCurrentTask = value
                If VarCurrentTask Is Nothing Then
                    NothingSelected = True
                    OnTaskChanged(New CurrentComponentChangedEventArgs(VarCurrentTask))
                Else
                    NameItem.Content = VarCurrentTask.Name
                    NameItem.Info = VarCurrentTask.GetShortFilePath
                    DescriptionItem.Content = VarCurrentTask.Description
                    CategoryItem.Content = VarCurrentTask.CategoryName
                    PathItem.Content = VarCurrentTask.ScriptPath
                    'For Each Item As TasksInRoutineListItem In TasksInRoutine.Items
                    '    RemoveHandler Item.RemoveClicked, AddressOf RemoveClicked
                    '    RemoveHandler Item.EditClicked, AddressOf EditClicked
                    'Next
                    'TasksInRoutine.Clear()
                    'Dim iLast As Integer = value.Tasks.Count - 1
                    'If iLast >= 0 Then
                    '    For i As Integer = 0 To iLast
                    '        Dim NewItem As New TasksInRoutineListItem(New TasksInRoutineListItemValue(value.Tasks(i).Copy)) With {.Index = i}
                    '        With VarCurrentTask.Tasks(i)
                    '            NewItem.Name = .Name
                    '            NewItem.IsActive = .Active
                    '            'NewItem.WaitForPrevious = .
                    '            'NewItem.RunAsync = .RunAsync
                    '        End With
                    '        AddHandler NewItem.RemoveClicked, AddressOf RemoveClicked
                    '        AddHandler NewItem.EditClicked, AddressOf EditClicked
                    '        TasksInRoutine.Add(NewItem)
                    '    Next
                    'End If
                    OnTaskChanged(New CurrentComponentChangedEventArgs(VarCurrentTask))
                    ChangesMade = False
                    NothingSelected = False
                End If
            End Set
        End Property
        Public Shadows Property Parent As TaskBrowser
            Get
                Return DirectCast(MyBase.Parent, TaskBrowser)
            End Get
            Set(value As TaskBrowser)
                MyBase.Parent = value
            End Set
        End Property
        Public Sub New(ByVal Rectangle As Rectangle, ByVal Parent As TaskBrowser)
            DoubleBuffered = True
            Location = Rectangle.Location
            Size = Rectangle.Size
            BackColor = ColorHelper.Multiply(ApplicationBackColor, 0.7)
            With NothingSelectedScreen
                .Hide()
                .Size = Size
                .BackColor = BackColor
                .Parent = Me
            End With
            NameItem = New ComponentViewItem(PropertiesList.ListContainer)
            With NameItem
                .EditErrorMessage = "A task with this name already exists. Please choose another name."
                .ValidateEditFunction = AddressOf ValidateNameChange
                .Location = Point.Empty
                .Size = New Size(Width - 40, 80)
                .Title = "Name"
                .Content = "No task selected"
                .Info = "C:/test"
                .AddAction(My.Resources.EditIconDefault, My.Resources.EditIconHover, AddressOf ChangeName)
                .AddAction(My.Resources.OpenFileLocationIconDefault, My.Resources.OpenFileLocationIconHover, AddressOf OpenFileLocation)
                AddHandler .ContentChanged, AddressOf ContentChanged
            End With
            DescriptionItem = New ComponentViewItem(PropertiesList.ListContainer)
            With DescriptionItem
                .Location = New Point(0, NameItem.Bottom)
                .Size = New Size(Width - 40, 80)
                .Title = "Description"
                .Content = "My description goes here"
                .AddAction(My.Resources.EditIconDefault, My.Resources.EditIconHover, AddressOf ChangeDescription)
                AddHandler .ContentChanged, AddressOf ContentChanged
            End With
            CategoryItem = New CategoryComponentViewItem(PropertiesList.ListContainer)
            With CategoryItem
                .Location = New Point(0, DescriptionItem.Bottom)
                .Size = New Size(Width - 40, 80)
                .Title = "Category"
                .Content = "No task selected"
                '.AddAction(My.Resources.EditIconDefault, My.Resources.EditIconHover, AddressOf ChangeDescription)
                'AddHandler .ContentChanged, AddressOf ContentChanged
                AddHandler .DragStarted, AddressOf CategoryItem_DragStarted
                AddHandler .DragEventHappened, AddressOf CategoryItem_DragEventHappened
            End With
            PathItem = New ComponentViewItem(PropertiesList.ListContainer)
            With PathItem
                .Location = New Point(0, CategoryItem.Bottom)
                .Size = New Size(Width - 40, 80)
                .Title = "Script"
                .Content = "No task selected"
                .AddAction(My.Resources.OpenFileLocationIconDefault, My.Resources.OpenFileLocationIconHover, AddressOf BrowseScripts)
                AddHandler .ContentChanged, AddressOf ContentChanged
            End With
            Dim CancelAcceptButtonsSize As New Size(2 * 64, 64)
            Dim CancelAcceptButtonsRect As New Rectangle(New Point((Width - CancelAcceptButtonsSize.Width) \ 2, Height - CancelAcceptButtonsSize.Height), CancelAcceptButtonsSize)
            With CancelButton
                .Size = New Size(64, 64)
                .BackgroundImage = My.Resources.CancelChangesIconDefault
                .Location = CancelAcceptButtonsRect.Location
                .Parent = Me
                .Cursor = Cursors.Hand
                AddHandler .MouseEnter, AddressOf Button_MouseEnter
                AddHandler .MouseLeave, AddressOf Button_MouseLeave
                AddHandler .Click, AddressOf Button_Click
            End With
            With SaveButton
                .Size = New Size(64, 64)
                .BackgroundImage = My.Resources.AcceptChangesIconDefault
                .Location = New Point(CancelAcceptButtonsRect.Right - 64, CancelAcceptButtonsRect.Top)
                .Cursor = Cursors.Hand
                .Parent = Me
                AddHandler .MouseEnter, AddressOf Button_MouseEnter
                AddHandler .MouseLeave, AddressOf Button_MouseLeave
                AddHandler .Click, AddressOf Button_Click
            End With
            PropertiesListContainer = New AutoSizedContainer
            With PropertiesList
                With .Items
                    .Add(NameItem)
                    .Add(DescriptionItem)
                    .Add(CategoryItem)
                End With
                .Padding = New Padding(0)
                .Location = New Point(0, PropertiesListContainer.HeaderHeight)
            End With
            With PropertiesListContainer
                .HeaderText = "properties"
                .InnerControl = PropertiesList
                .Padding = New Padding(0, 0, 4, 0)
                .BorderWidth = 0
                .BackColor = ColorHelper.Multiply(ApplicationBackColor, 0.7)
                .Parent = Me
                .SetTotalSize(New Size(Width, 252 + .HeaderHeight))
                .AddHeaderControl({My.Resources.DeleteIconDefault, My.Resources.DeleteIconHover, My.Resources.DeleteIconPress}, AddressOf DeleteTask_Clicked)
            End With
            'RoutineListContainer = New AutoSizedContainer
            'With RoutineListContainer
            '    .Top = PropertiesListContainer.Bottom
            '    .BackColor = BackColor
            '    .HeaderText = "tasks"
            '    .InnerControl = TasksInRoutine
            '    .Padding = New Padding(0)
            '    .BorderWidth = 0
            '    .SetTotalSize(New Size(Width, SaveButton.Top - .Top))
            '    .Parent = Me
            '    .AddHeaderControl({My.Resources.AddIconDefault, My.Resources.AddIconHover, My.Resources.AddIconPress}, AddressOf AddTask)
            'End With
            'With TaskPicker
            '    .Parent = Me
            '    .Location = RoutineListContainer.Location
            '    .Size = New Size(Width, Height - .Top)
            '    .BackColor = BackColor
            '    .BringToFront()
            '    .Hide()
            'End With
            CurrentTask = Nothing
            Me.Parent = Parent
        End Sub
        Private Sub BrowseScripts(Info As Object)
            Using FileDialog As New OpenFileDialog()
                FileDialog.InitialDirectory = ApplicationPath & "\Data\Scripts"
                FileDialog.Filter = "PowerShell scripts|*.ps1"
                FileDialog.Title = "Select a script"
                If FileDialog.ShowDialog() = DialogResult.OK AndAlso FileDialog.FileName <> Nothing AndAlso IO.File.Exists(FileDialog.FileName) Then
                    PathItem.Content = FileDialog.FileName
                End If
            End Using
        End Sub
        Private Sub DeleteTask_Clicked(Sender As Object, e As EventArgs)
            If MsgBox("Are you sure you want to delete this task?", MsgBoxStyle.OkCancel, "Confirm deletion of task") = MsgBoxResult.Ok Then
                Dim OldDirectory As String = CurrentTask.GetFullFilePath
                Dim NewDirectory As String = ApplicationPath & "\Data\" & GarbageBinName & "\Tasks\" & CurrentTask.GetFileNameFromName
                AsyncFileReader.Queue.AddOperation(AsyncFileReader.Queue.FileOperation.MoveFile(OldDirectory, NewDirectory, True), AddressOf MoveToBinFinished, Nothing)
            End If
        End Sub
        Private Sub MoveToBinFinished(e As FileOperationEventArgs)
            Dim OriginalPath As String = DirectCast(e.Arguments("OriginalPath"), String)
            Dim OriginalName As String = IO.Path.GetFileNameWithoutExtension(OriginalPath)
            If e.ErrorOccurred Then
                MsgBox("An unexpected error occurred (code: no time, deadline tomorrow).")
            Else
                Loader.RemoveTask(OriginalName)
                OnTaskDeleted(EventArgs.Empty)
            End If
        End Sub
        Protected Sub OnTaskDeleted(e As EventArgs)
            RaiseEvent TaskDeleted(Me, e)
            CurrentTask = Nothing
        End Sub
        Private Sub CategoryItem_DragStarted(Sender As Object, e As MouseEventArgs)
            TasksTab.NavigateBack()
        End Sub
        Private Sub CategoryItem_DragEventHappened(Sender As Object, e As QueryContinueDragEventArgs)
            Select Case e.Action
                Case DragAction.Cancel, DragAction.Drop
                    TasksTab.ShowItemsInCategoryList()
                Case Else
                    MsgBox("ELSE!")
            End Select
        End Sub
        Private Sub ContentChanged(Sender As Object, e As EventArgs)
            ChangesMade = True
        End Sub
        Private Function ValidateNameChange(Sender As Object, e As ValidateEditEventArgs) As ValidateEditEventArgs
            If DirectCast(e.OldContent, String) = DirectCast(e.NewContent, String) Then
                e.Cancel = True
            ElseIf Loader.LookupTask(DirectCast(e.NewContent, String)) IsNot Nothing Then
                e.Valid = False
            End If
            Return e
        End Function
        'Private Sub RemoveClicked(Sender As Object, e As EventArgs)
        '    If MsgBox("Are you sure you want to remove this routine from this schedule?" & vbNewLine & "This will not delete the routine object.", MsgBoxStyle.YesNo, "Confirm removal") = MsgBoxResult.Yes Then
        '        Dim SenderItem As TasksInRoutineListItem = DirectCast(Sender, TasksInRoutineListItem)
        '        'If Not CurrentSchedule.Routines.Contains(SenderItem.Value.Information) Then
        '        '    MsgBox("An unexpected error occurred (code 5)" & vbNewLine & "It is recommended that you do not save any changes.")
        '        'End If
        '        'TasksInRoutine.Remove(SenderItem)
        '        RemoveHandler SenderItem.RemoveClicked, AddressOf RemoveClicked
        '        RemoveHandler SenderItem.EditClicked, AddressOf EditClicked
        '        ChangesMade = True
        '    End If
        'End Sub
        Private Sub NothingSelectedScreen_Paint(Sender As Object, e As PaintEventArgs) Handles NothingSelectedScreen.Paint
            Dim TitleRects() As Rectangle = Globals.GetNothingSelectedRects(NothingSelectedScreen.Size)
            TextRenderer.DrawText(e.Graphics, TitleText, NothingSelectedTitleFont, TitleRects(0), Color.White)
            TextRenderer.DrawText(e.Graphics, SubTitleText, NothingSelectedSubTitleFont, TitleRects(1), Color.White)
        End Sub
        'Private Sub EditClicked(Sender As Object, e As EventArgs)
        '    Dim SenderItem As TasksInRoutineListItem = DirectCast(Sender, TasksInRoutineListItem)
        '    TasksTab.RequestView(SenderItem.Value.Information.Name)
        '    WindowControl.ShowTab(3)
        'End Sub
        'Private Sub AddTask(Sender As Object, e As MouseEventArgs)
        '    TasksInRoutine.Hide()
        '    TaskPicker.SetDirectory()
        '    TaskPicker.Show()
        'End Sub
        'Public Sub RefreshLists()
        '    TaskPicker.RefreshLists()
        'End Sub
        'Private Sub TaskList_ItemActiveChanged(Sender As Object, e As ListItemActiveChangedEventArgs) Handles TasksInRoutine.ItemActiveChanged
        '    'Dim SenderItem As RoutinesInScheduleListItem = DirectCast(e.SenderItem, RoutinesInScheduleListItem)
        '    'Dim RoutineInformationToChange As RoutineInformation = varCurrentSchedule.Routines(SenderItem.Index)
        '    'RoutineInformationToChange.Active = e.Data.Active
        '    ChangesMade = True
        'End Sub
        Private Sub Button_MouseEnter(Sender As Object, e As EventArgs)
            Dim SenderButton As PictureBox = DirectCast(Sender, PictureBox)
            If ReferenceEquals(SenderButton, CancelButton) Then
                SenderButton.BackgroundImage = My.Resources.CancelChangesIconHover
            Else
                SenderButton.BackgroundImage = My.Resources.AcceptChangesIconHover
            End If
        End Sub
        Private Sub Button_MouseLeave(Sender As Object, e As EventArgs)
            Dim SenderButton As PictureBox = DirectCast(Sender, PictureBox)
            If ReferenceEquals(SenderButton, CancelButton) Then
                SenderButton.BackgroundImage = My.Resources.CancelChangesIconDefault
            Else
                SenderButton.BackgroundImage = My.Resources.AcceptChangesIconDefault
            End If
        End Sub
        Public Function VerifyDiscard() As Boolean
            If ChangesMade Then
                If MessageBox.Show("Any changes made to this task will be lost. If you want to save these changes, click 'Cancel'", "Unsaved changes have been made", MessageBoxButtons.OKCancel, MessageBoxIcon.Exclamation, MessageBoxDefaultButton.Button2) = DialogResult.Cancel Then Return False
            End If
            Return True
        End Function
        Private Sub Button_Click(Sender As Object, e As EventArgs)
            Dim SenderButton As PictureBox = DirectCast(Sender, PictureBox)
            Dim OldName As String = CurrentTask.Name
            If ReferenceEquals(SenderButton, CancelButton) Then
                If VerifyDiscard() Then CurrentTask = Loader.LookupTask(OldName)
            Else
                Dim OldDirectory As String = CurrentTask.GetFullFilePath
                Dim NewName As String = NameItem.Content
                With VarCurrentTask
                    .Name = NameItem.Content
                    .Description = DescriptionItem.Content
                    .CategoryName = CategoryItem.Content
                    .ScriptPath = PathItem.Content
                    ' TODO: Add other properties
                End With
                Dim NewFullPath As String = VarCurrentTask.GetFullFilePath
                If NewFullPath <> OldDirectory Then
                    AsyncFileReader.Queue.AddOperation(AsyncFileReader.Queue.FileOperation.MoveFile(OldDirectory, CurrentTask.GetFullFilePath, False), AddressOf MoveFinished, Nothing)
                    AsyncFileReader.Queue.AddOperation(AsyncFileReader.Queue.FileOperation.UpdateTaskInformationInRoutines(OldName, NewName), AddressOf TaskInformationInRoutinesUpdated, Nothing)
                Else
                    AsyncFileReader.Queue.AddOperation(AsyncFileReader.Queue.FileOperation.OverwriteFile(NewFullPath, CurrentTask.OutputFileContents), AddressOf OverwriteFinished, Nothing)
                End If
            End If

        End Sub
        Private Sub TaskInformationInRoutinesUpdated(e As FileOperationEventArgs)
            If e.ErrorOccurred Then
                MsgBox("(Finished) Errors occurred: " & vbNewLine & e.Exception.Message)
            Else
                Dim ModifiedRoutines() As String = DirectCast(e.Result, String()) ' TODO: get rid of these
            End If
        End Sub
        Private Sub MoveFinished(Result As FileOperationEventArgs)
            If Result.ErrorOccurred Then
                MsgBox("Error occurred: " & Result.Exception.Message & vbNewLine & DirectCast(Result.Result, String))
                ' TODO: Log error
            Else
                AsyncFileReader.Queue.AddOperation(AsyncFileReader.Queue.FileOperation.OverwriteFile(VarCurrentTask.GetFullFilePath, VarCurrentTask.OutputFileContents), AddressOf OverwriteFinished, Nothing)
            End If
        End Sub
        Private Sub OverwriteFinished(Result As FileOperationEventArgs)
            If Result.ErrorOccurred Then
                MsgBox("Overwrite error: " & Result.Exception.Message)
                ' TODO: Log error
            Else
                RaiseEvent ChangesSaved(Me, New CurrentComponentChangedEventArgs(VarCurrentTask))
                CurrentTask = VarCurrentTask ' TODO: Replace with "refresh" method
            End If
        End Sub
        Private Sub OpenFileLocation(Args As Object)
            Process.Start("explorer.exe", "/select," & VarCurrentTask.GetFullFilePath)
        End Sub
        Private Sub ChangeName(Args As Object)
            NameItem.EditContent()
        End Sub
        Private Sub ChangeDescription(Args As Object)
            DescriptionItem.EditContent()
        End Sub
        Protected Overridable Sub OnTaskChanged(e As CurrentComponentChangedEventArgs)
            Invalidate()
            RaiseEvent TaskChanged(Me, e)
        End Sub
        Public Overloads Sub ViewTask(TaskFile As String, Optional ByVal SurelyLoaded As Boolean = False, Optional ByVal ForceReload As Boolean = False)
            Dim Match As TaskEntry = Loader.LookupTask(FormatConverter.ExtractFileName(TaskFile, False))
            If Not ForceReload Then
                If Match IsNot Nothing Then
                    CurrentTask = Match
                ElseIf SurelyLoaded Then
                    Throw New Exception("Could not find the task " & FormatConverter.ExtractFileName(TaskFile, False) & ". It may not be loaded yet.")
                Else
                    AsyncFileReader.Queue.AddOperation(AsyncFileReader.Queue.FileOperation.ReadLines(TaskFile), AddressOf TaskLinesLoaded, Nothing)
                    'varLoadingGraphics.Spin(50, 10)
                End If
            Else
                CurrentTask = Nothing
                If Match IsNot Nothing Then
                    Loader.RemoveTask(Match.Name)
                    AsyncFileReader.Queue.AddOperation(AsyncFileReader.Queue.FileOperation.ReadLines(TaskFile), AddressOf TaskLinesLoaded, Nothing)
                    'varLoadingGraphics.Spin(50, 10)
                ElseIf SurelyLoaded Then
                    Throw New Exception("Could not find the task " & FormatConverter.ExtractFileName(TaskFile, False) & ". It may not be loaded yet.")
                Else
                    AsyncFileReader.Queue.AddOperation(AsyncFileReader.Queue.FileOperation.ReadLines(TaskFile), AddressOf TaskLinesLoaded, Nothing)
                    'varLoadingGraphics.Spin(50, 10)
                End If
            End If
        End Sub
        Private Sub TaskLinesLoaded(e As FileOperationEventArgs)
            'varLoadingGraphics.StopSpin()
            Dim Path As String = DirectCast(e.Arguments("Path"), String)
            Loader.AddTask(TaskEntry.CreateFromLines(DirectCast(e.Result, String()), Path))
            ViewTask(Path, True)
        End Sub
    End Class





    Public Class RoutineBrowser
        Inherits Tab
        Private RoutineCategoriesContainer As New AutoSizedContainer
        Private WithEvents RoutinesInCategoryContainer As New AutoSizedContainer
        Private WithEvents CategoriesList As New CategoryList
        Private WithEvents RoutinesInSelectedCategory As New RoutinesInCategoryList
        'Private NewCategory As New NewCategoryControl
        Private WithEvents Viewer As RoutineViewer
        'Private CurrentX As Integer = 0, LastX As Integer = 20
        'Private Stage As Integer = 0
        'Private SC As SynchronizationContext = SynchronizationContext.Current
        Private varCurrentCategoryName As String
        Private RoutineNameToSelectAfterReload As String = Nothing
        Private RenameOccurred As Boolean
        'Private WithEvents SlideTimer As New Timers.Timer(1000 / 60)
        Public Sub New(Parent As MultiTabWindow)
            MyBase.New(Parent)
            'SlideTimer.AutoReset = False
            CategoriesList.BackColor = ApplicationBackColor
            With RoutineCategoriesContainer
                .HeaderText = "routine categories"
                .Location = New Point(0, My.Resources.Header.Height)
                .InnerControl = CategoriesList
                .Parent = Me
                .Padding = New Padding(0)
                .BorderWidth = 0
                .SetTotalSize(New Size(CategoriesList.Width, Height - Header.Height))
                .AddHeaderControl(New Image() {My.Resources.ReloadIconDefault, My.Resources.ReloadIconHover, My.Resources.ReloadIconPress}, AddressOf ReloadCategoriesIcon_Clicked)
                .AddHeaderControl(New Image() {My.Resources.AddIconDefault, My.Resources.AddIconHover, My.Resources.AddIconPress}, AddressOf AddCategoryIcon_Clicked)
            End With
            With RoutinesInCategoryContainer
                .HeaderText = "routines > category"
                .Location = New Point(0, My.Resources.Header.Height)
                .InnerControl = RoutinesInSelectedCategory
                .Parent = Me
                .SetTotalSize(New Size(CategoriesList.Width, Height - Header.Height))
                .AddHeaderControl(New Image() {My.Resources.ReloadIconDefault, My.Resources.ReloadIconHover, My.Resources.ReloadIconPress}, AddressOf ReloadRoutinesIcon_Clicked)
                .AddHeaderControl(New Image() {My.Resources.AddIconDefault, My.Resources.AddIconHover, My.Resources.AddIconPress}, AddressOf AddRoutineIcon_Clicked)
                .AddHeaderControl(New Image() {My.Resources.BackButtonDefault, My.Resources.BackButtonHover, My.Resources.BackButtonPress}, AddressOf BackIcon_Clicked)
            End With
            Viewer = New RoutineViewer(New Rectangle(New Point(RoutineCategoriesContainer.Right, My.Resources.Header.Height), New Size(Width - RoutineCategoriesContainer.Right, Height - My.Resources.Header.Height)), Me)
            Viewer.BringToFront()
            AddHandler Loader.AllSchedulesLoaded, AddressOf Loader_AllRoutinesLoaded
        End Sub
        Private Sub Loader_AllRoutinesLoaded(e As EventArgs)
            ReloadCategories()
        End Sub
        Private Sub Viewer_RoutineChanged(Sender As Object, e As CurrentComponentChangedEventArgs) Handles Viewer.RoutineChanged
            If Viewer.CurrentRoutine IsNot Nothing AndAlso Loader.RoutineCategoryExists(Viewer.CurrentRoutine.CategoryName) Then
                Dim Match As CategoryListItem = CategoriesList.FindName(Viewer.CurrentRoutine.CategoryName)
                Match.IsActive = True
                CategoriesList.SetAllActive(False, Match.Index)
            Else
                CategoriesList.SetAllActive(False)
            End If
        End Sub
        Public Property CurrentCategoryName As String
            Get
                Return varCurrentCategoryName
            End Get
            Set(value As String)
                varCurrentCategoryName = value
                RoutinesInCategoryContainer.HeaderText = "routines   " & varCurrentCategoryName
            End Set
        End Property
        Private Sub SchedulesInCategoryContainer_HeaderPaint(Sender As Object, e As PaintEventArgs) Handles RoutinesInCategoryContainer.HeaderPaint
            With e.Graphics
                Dim DrawImage As Image = My.Resources.SmallArrowBlack
                Dim ImageSize As Size = DrawImage.Size
                Dim TextLength As Integer = TextRenderer.MeasureText(e.Graphics, "routines", RoutinesInCategoryContainer.Font).Width + RoutinesInCategoryContainer.TextPadding - 3
                Dim ImageLocation As New Point(TextLength, CInt(.ClipBounds.Height - ImageSize.Height) \ 2 + 1)
                e.Graphics.DrawImage(DrawImage, ImageLocation)
            End With
        End Sub
        Private Sub CategoriesList_CategoryEdited(Sender As Object, e As CategoryEditedEventArgs) Handles CategoriesList.CategoryEdited
            Dim OldName As String = e.Trigger.OldCategoryName
            Dim NewName As String = e.Trigger.NewCategoryName
            If Viewer.VerifyDiscard Then
                If Loader.RoutineCategoryExists(NewName) Then
                    MsgBox("There is already a category with that name.")
                Else
                    RenameCategory(OldName, NewName, True)
                End If
            Else
                e.CategoryItem.Value = New CategoryListItemValue(OldName)
            End If
        End Sub
        Public Function VerifyViewerDiscard() As Boolean
            Return Viewer.VerifyDiscard
        End Function
        Private Sub CategoriesList_CategoryDropped(Sender As Object, e As CategoryDroppedEventArgs) Handles CategoriesList.CategoryDropped
            If Viewer.VerifyDiscard Then
                RenameOccurred = True
                Dim OldCategoryName As String = DirectCast(e.Trigger.Data.GetData(DataFormats.StringFormat), String)
                Viewer.CurrentRoutine.ChangeCategoryAndMoveFile(e.TargetItem.Value.CategoryName, AddressOf CategoryChanged, Viewer.CurrentRoutine)
            End If
        End Sub
        Private Sub CategoryChanged(e As FileOperationEventArgs)
            If e.ErrorOccurred Then
                MsgBox(e.Exception.Message)
            Else
                Dim TargetComponent As RoutineEntry = DirectCast(e.State, RoutineEntry)
                RequestView(TargetComponent.Name)
                'SelectCategory(TargetComponent.CategoryName)
                'SelectTask(TargetCompon)
            End If
        End Sub
        Private Sub CategoriesList_CategoryDeleted(Sender As Object, e As CategoryDeletedEventArgs) Handles CategoriesList.CategoryDeleted
            Dim Value As CategoryListItemValue = e.CategoryItem.Value
            If Value.CategoryName = GarbageBinName Then
                MsgBox("You cannot delete the garbage bin.")
            Else
                Dim TargetItems As IEnumerable(Of ComponentEntry) = Loader.RoutinesInCategory(Value.CategoryName)
                If TargetItems.Count > 0 Then
                    If MsgBox("Are you sure you want to delete this category? All items contained in it will be moved to the garbage bin.", MsgBoxStyle.OkCancel) = MsgBoxResult.Ok Then
                        For Each Item As ComponentEntry In TargetItems
                            Dim OldDirectory As String = Item.GetFullFilePath
                            Dim NewDirectory As String = ApplicationPath & "\Data\" & GarbageBinName & "\Routines\" & Item.GetFileNameFromName
                            AsyncFileReader.Queue.AddOperation(AsyncFileReader.Queue.FileOperation.MoveFile(OldDirectory, NewDirectory, True), AddressOf MoveToBinFinished, Nothing)
                        Next
                    End If
                Else
                    Loader.RemoveRoutineCategory(Value.CategoryName)
                    AsyncFileReader.Queue.AddOperation(AsyncFileReader.Queue.FileOperation.DeleteFolder(RoutineEntry.BaseDirectory & "\" & Value.CategoryName), AddressOf DeleteFolderFinished, Nothing)
                End If
            End If
        End Sub
        Private Sub MoveToBinFinished(e As FileOperationEventArgs)
            Dim OriginalPath As String = DirectCast(e.Arguments("OriginalPath"), String)
            Dim NewPath As String = DirectCast(e.Arguments("NewPath"), String)
            Dim OriginalName As String = IO.Path.GetFileNameWithoutExtension(OriginalPath)
            Dim NewName As String = IO.Path.GetFileNameWithoutExtension(NewPath)
            Dim CategoryName As String = FormatConverter.ExtractHighestLevelDirectory(OriginalPath, True)
            If e.ErrorOccurred Then
                MsgBox("An unexpected error occurred (code 10).")
            Else
                Loader.RemoveRoutine(OriginalName)
                If Loader.RoutinesInCategory(CategoryName).Count = 0 Then
                    Loader.RemoveRoutineCategory(CategoryName)
                    Dim OldFolder As String = IO.Directory.GetParent(OriginalPath).FullName
                    If CurrentCategoryName = CategoryName Then Viewer.CurrentRoutine = Nothing
                    AsyncFileReader.Queue.AddOperation(AsyncFileReader.Queue.FileOperation.DeleteFolder(OldFolder), AddressOf DeleteFolderFinished, Nothing)
                End If
            End If
        End Sub
        Private Sub DeleteFolderFinished(e As FileOperationEventArgs)
            If e.ErrorOccurred Then
                MsgBox("Could not delete " & DirectCast(e.Arguments("Path"), String) & ": " & e.Exception.Message)
            End If
            CurrentCategoryName = Nothing
            RoutineNameToSelectAfterReload = Nothing
            ReloadCategories()
        End Sub
        Private Sub RenameCategory(OldName As String, NewName As String, Optional ClearList As Boolean = False)
            If Not RenameOccurred Then
                RenameOccurred = True
                If ClearList Then CategoriesList.Clear()
                AsyncFileReader.Queue.AddOperation(AsyncFileReader.Queue.FileOperation.RenameFolder(RoutineEntry.BaseDirectory & "\" & OldName, NewName, False), AddressOf RenameFinished, Nothing)
            End If
        End Sub
        Private Sub RenameFinished(e As FileOperationEventArgs)
            If e.ErrorOccurred Then
                MsgBox("Could not rename category: " & e.Exception.Message)
            Else
                Dim OldName As String = FormatConverter.ExtractHighestLevelDirectory(DirectCast(e.Arguments("Path"), String), False)
                Dim NewName As String = DirectCast(e.Result, String)
                Loader.RenameRoutineCategory(OldName, NewName)
                CurrentCategoryName = NewName
                If Viewer.CurrentRoutine IsNot Nothing Then
                    RoutineNameToSelectAfterReload = Viewer.CurrentRoutine.Name
                Else
                    RoutineNameToSelectAfterReload = Nothing
                End If
                ReloadCategories()
                ReloadRoutines()
            End If
        End Sub
        Private Sub BackIcon_Clicked(Sender As Object, e As MouseEventArgs)
            NavigateBack()
        End Sub
        Private Sub ReloadCategoriesIcon_Clicked(Sender As Object, e As MouseEventArgs)
            ReloadCategories()
        End Sub
        Private Sub ReloadRoutinesIcon_Clicked(Sender As Object, e As MouseEventArgs)
            If Viewer.VerifyDiscard Then
                If Viewer.CurrentRoutine IsNot Nothing Then RoutineNameToSelectAfterReload = Viewer.CurrentRoutine.Name
                ReloadRoutines()
            End If
        End Sub
        Public Sub RequestView(ByVal RoutineName As String)
            Dim Match As RoutineEntry = Loader.LookupRoutine(RoutineName)
            If Match IsNot Nothing Then
                SelectCategory(Match.CategoryName)
                Dim ItemMatch As RoutinesInCategoryListItem = RoutinesInSelectedCategory.Items.Find(Function(x As RoutinesInCategoryListItem) As Boolean
                                                                                                        Return (x.Value.Name = RoutineName)
                                                                                                    End Function)
                If ItemMatch IsNot Nothing Then
                    ItemMatch.IsSelected = True
                End If
            Else
                MsgBox("No entry match")
            End If
        End Sub
        Public Sub NavigateBack()
            RoutinesInCategoryContainer.Hide()
            RoutineCategoriesContainer.Show()
        End Sub
        Public Sub ShowItemsInCategoryList()
            RoutineCategoriesContainer.Hide()
            RoutinesInCategoryContainer.Show()
        End Sub

        Private Sub ReloadCategories()
            Viewer.ChangesMade = False
            AsyncFileReader.Queue.AddOperation(AsyncFileReader.Queue.FileOperation.ListDirectories(RoutineEntry.BaseDirectory), AddressOf ReloadCategories_Finished, Nothing)
        End Sub
        Private Sub ReloadRoutines()
            AsyncFileReader.Queue.AddOperation(AsyncFileReader.Queue.FileOperation.ListFiles(RoutineEntry.BaseDirectory & "\" & CurrentCategoryName), AddressOf ReloadRoutines_Finished, Nothing)
        End Sub
        Private Sub ReloadCategories_Finished(e As FileOperationEventArgs)
            Dim Directories() As String = DirectCast(e.Result, String())
            CategoriesList.Clear()
            Dim iLast As Integer = Directories.Count - 1
            If iLast >= 0 Then
                For i As Integer = 0 To Directories.Count - 1
                    Directories(i) = FormatConverter.ExtractHighestLevelDirectory(Directories(i), False)
                    Dim NewItem As New CategoryListItem(Directories(i)) With {.CanEdit = True}
                    If Viewer.CurrentRoutine IsNot Nothing Then NewItem.IsActive = (Directories(i) = Viewer.CurrentRoutine.CategoryName)
                    If Not Loader.RoutineCategories.Contains(Directories(i)) Then
                        Loader.AddRoutineCategory(Directories(i))
                    End If
                    CategoriesList.Add(NewItem)
                Next
            End If
            'Viewer.RefreshLists()
            'ScheduleNameToSelectAfterReload = Nothing
        End Sub
        Private Sub ReloadRoutines_Finished(e As FileOperationEventArgs)
            Dim Files() As String = DirectCast(e.Result, String())
            RoutinesInSelectedCategory.Clear()
            Dim iLast As Integer = Files.Count - 1
            If iLast >= 0 Then
                For i As Integer = 0 To iLast
                    Dim RoutineName As String = FormatConverter.ExtractFileName(Files(i), False)
                    RoutinesInSelectedCategory.Add(New RoutinesInCategoryListItem(New ItemsInCategoryListItemValue(RoutineName)))
                Next
            End If
            If RoutineNameToSelectAfterReload = Nothing AndAlso Viewer.CurrentRoutine IsNot Nothing Then RoutineNameToSelectAfterReload = Viewer.CurrentRoutine.Name
            For Each Item As RoutinesInCategoryListItem In RoutinesInSelectedCategory.Items
                If Item.Value.Name = RoutineNameToSelectAfterReload Then
                    Item.IsSelected = True
                    Exit For
                End If
            Next
            Viewer.RefreshLists()
            RoutineNameToSelectAfterReload = Nothing
            RenameOccurred = False
        End Sub
        Private Sub Viewer_ChangesSaved(Sender As Object, e As EventArgs) Handles Viewer.ChangesSaved, Viewer.RoutineDeleted
            ReloadRoutines()
            ReloadCategories()
        End Sub
        Protected Overrides Sub OnTabShown(e As EventArgs)
            MyBase.OnTabShown(e)
            Header.CurrentDirectiry = {HeaderControl.Views.Overview, HeaderControl.Views.TemplateBrowser, HeaderControl.Views.ScheduleBrowser, HeaderControl.Views.RoutineBrowser}
            Viewer.CurrentRoutine = Viewer.CurrentRoutine
        End Sub
        Private Sub AddCategoryIcon_Clicked(Sender As Object, e As EventArgs)
            CreateCategory()
        End Sub
        Private Sub AddRoutineIcon_Clicked(Sender As Object, e As EventArgs)
            If Viewer.VerifyDiscard Then AddNewRoutine()
        End Sub
        Private Sub CreateCategory()
            AsyncFileReader.Queue.AddOperation(AsyncFileReader.Queue.FileOperation.CreateDirectory(RoutineEntry.BaseDirectory & "\New category", True), AddressOf CategoryCreated, Nothing)
        End Sub
        Private Sub CategoryCreated(e As FileOperationEventArgs)
            Dim NewName As String = FormatConverter.ExtractHighestLevelDirectory(DirectCast(e.Result, String), False)
            Loader.AddRoutineCategory(NewName)
            ReloadCategories()
        End Sub
        Private Sub AddNewRoutine()
            AsyncFileReader.Queue.AddOperation(AsyncFileReader.Queue.FileOperation.CreateFile(RoutineEntry.BaseDirectory & "\" & CurrentCategoryName, "New Routine.txt", True, FormatConverter.GetBytesFromString("//meta" & vbNewLine & "$meta: %desc%==This routine does not have a description yet" & vbNewLine & "// tasks" & vbNewLine), RoutineEntry.BaseDirectory), AddressOf AddNewRoutine_Finished, Nothing)
        End Sub
        Private Sub AddNewRoutine_Finished(e As FileOperationEventArgs)
            If Not e.ErrorOccurred Then
                RoutineNameToSelectAfterReload = IO.Path.GetFileNameWithoutExtension(DirectCast(e.Result, String))
            Else
                MsgBox("An unexpected error occurred (code 6): " & e.Exception.Message)
            End If
            ReloadRoutines()
        End Sub
        Protected Overrides Sub OnSizeChanged(e As EventArgs)
            MyBase.OnSizeChanged(e)
            If RoutineCategoriesContainer IsNot Nothing Then
                With RoutineCategoriesContainer
                    .SetTotalSize(New Size(.Width, Height - My.Resources.Header.Height))
                End With
            End If
        End Sub
        Private Sub CategorySelected(Sender As Object, e As ListItemSelectionChangedEventArgs) Handles CategoriesList.ItemSelectionChanged
            If e.Data.Selected Then
                Dim SenderItem As CategoryListItem = DirectCast(e.SenderItem, CategoryListItem)
                SelectCategory(SenderItem.Value.CategoryName)
            End If
        End Sub
        Private Sub SelectCategory(ByVal NewCategoryName As String)
            CategoriesList.DeselectAll()
            If NewCategoryName <> CurrentCategoryName Then
                RoutinesInSelectedCategory.Clear()
                'Viewer.CurrentSchedule = Nothing
                Dim Routines As List(Of RoutineEntry) = Loader.RoutinesInCategory(NewCategoryName)
                For Each Routine As RoutineEntry In Routines
                    RoutinesInSelectedCategory.Add(New RoutinesInCategoryListItem(New ItemsInCategoryListItemValue(Routine.Name)))
                Next
            End If
            CurrentCategoryName = NewCategoryName
            If Viewer.CurrentRoutine IsNot Nothing Then
                Dim iLast As Integer = RoutinesInSelectedCategory.Items.Count - 1
                If iLast >= 0 Then
                    For i As Integer = 0 To iLast
                        If RoutinesInSelectedCategory.Item(i).Value.Name = Viewer.CurrentRoutine.Name Then
                            RoutinesInSelectedCategory.Item(i).IsSelected = True
                            Exit For
                        End If
                    Next
                End If
            End If
            RoutineCategoriesContainer.Hide()
            RoutinesInCategoryContainer.Show()
        End Sub
        Private Sub RoutineSelected(Sender As Object, e As ListItemSelectionChangedEventArgs) Handles RoutinesInSelectedCategory.ItemSelectionChanged
            If e.Data.Selected Then
                Dim SenderItem As RoutinesInCategoryListItem = DirectCast(e.SenderItem, RoutinesInCategoryListItem)
                SelectRoutine(SenderItem)
            End If
        End Sub
        Private Sub SelectRoutine(SelectItem As RoutinesInCategoryListItem)
            'RoutinesInSelectedCategory.DeselectAll(SelectItem.Index)
            If Viewer.CurrentRoutine IsNot Nothing AndAlso SelectItem.Value.Name <> Viewer.CurrentRoutine.Name Then
                If Viewer.VerifyDiscard Then
                    RoutinesInSelectedCategory.DeselectAll(SelectItem.Index)
                    Viewer.ViewRoutine(RoutineEntry.BaseDirectory & "\" & CurrentCategoryName & "\" & SelectItem.Value.Name & ".txt")
                Else
                    SelectItem.IsSelected = False
                End If
            ElseIf Viewer.CurrentRoutine IsNot Nothing Then ' Same routine selected
                If RenameOccurred Then ' Need to update stuff (easy: reload)
                    Viewer.ViewRoutine(RoutineEntry.BaseDirectory & "\" & CurrentCategoryName & "\" & SelectItem.Value.Name & ".txt", True, True) ' True: Don't load from memory
                End If ' Otherwise, no need to reload
            Else ' No routine is being viewed
                Viewer.ViewRoutine(RoutineEntry.BaseDirectory & "\" & CurrentCategoryName & "\" & SelectItem.Value.Name & ".txt")
            End If
            RenameOccurred = False
        End Sub
    End Class
    Public Class RoutineViewer
        Inherits Control
        Private varCurrentRoutine As RoutineEntry = Nothing
        'Private varPadding As New Padding(10)
        'Private varUseCompatibleTextRendering As Boolean = True
        'Private varLoadingSurface As New PictureBox
        'Private varLoadingGraphics As LoadingGraphics(Of PictureBox)
        'Private InnerRect As Rectangle = New Rectangle(New Point(varPadding.Left, varPadding.Top), New Size(Width - varPadding.Left - varPadding.Right, 100))
        Private CancelButton, SaveButton As New PictureBox
        Private ElementBrush As SolidBrush
        Private varElementColor As Color = Color.FromArgb(200, 200, 200)
        Private PropertiesListContainer, TaskListContainer As AutoSizedContainer
        Private WithEvents PropertiesList As New PropertyList
        Private WithEvents TasksInRoutine As New TasksInRoutineList
        Private NameItem, DescriptionItem As ComponentViewItem
        Private CategoryItem As CategoryComponentViewItem
        Private WithEvents TaskPicker As New TaskPicker
        Public Event RoutineChanged(Sender As Object, e As CurrentComponentChangedEventArgs)
        Public Event ChangesSaved(Sender As Object, e As CurrentComponentChangedEventArgs)
        Public Event RoutineDeleted(Sender As Object, e As EventArgs)
        Private varChangesMade As Boolean
        Private WithEvents NothingSelectedScreen As New PictureBox
        Private Sub PropertiesList_ScrollHandleVisibleChanged(Sender As Object, e As VisibleEventArgs) Handles PropertiesList.ScrollHandleVisibleChanged
            With PropertiesListContainer
                If e.Visible Then
                    .Padding = New Padding(0, 0, 0, 4)
                Else
                    .Padding = New Padding(0, 0, 0, 0)
                End If
                .SetTotalSize(New Size(Width, 3 * 80 + .HeaderHeight))
            End With
        End Sub
        Public Property ChangesMade As Boolean
            Get
                Return varChangesMade
            End Get
            Set(value As Boolean)
                varChangesMade = value
            End Set
        End Property
        Private Property NothingSelected As Boolean
            Get
                Return (CurrentRoutine Is Nothing)
            End Get
            Set(value As Boolean)
                If value Then
                    NothingSelectedScreen.Show()
                Else
                    NothingSelectedScreen.Hide()
                End If
            End Set
        End Property
        Private Sub TasksInRoutine_ItemValueChanged(Sender As Object, e As TaskItemValueChangedEventArgs) Handles TasksInRoutine.ItemValueChanged
            ChangesMade = True
        End Sub
        Private Sub TaskPicker_ItemsAdded(Sender As Object, e As ItemsAddedEventArgs) Handles TaskPicker.AddClicked
            For Each Item As ChildComponentInformation In e.SelectedItems
                If CurrentRoutine.Tasks.Find(Function(x As ChildComponentInformation) As Boolean
                                                 Return (x.Name = Item.Name)
                                             End Function) IsNot Nothing Then
                    ' TODO: Remove this check. Multiple routines allowed.
                End If
                Dim NewInfo As TaskInformation = DirectCast(Item, TaskInformation).Copy
                TasksInRoutine.Add(New TasksInRoutineListItem(New TasksInRoutineListItemValue(NewInfo)) With {.Index = TasksInRoutine.Items.Count})
                With TasksInRoutine.Items.Last
                    AddHandler .RemoveClicked, AddressOf RemoveClicked
                    AddHandler .EditClicked, AddressOf EditClicked
                    'AddHandler .ArgumentRemoved, AddressOf Task_ArgumentRemoved
                End With
                ' TODO: Add handlers for routine editing controls
                ChangesMade = True
            Next
            ExitPicker()
        End Sub
        'Private Sub Task_ArgumentRemoved(Sender As Object, e As ArgumentRemovedEventArgs)
        '    ChangesMade = True
        'End Sub
        Private Sub ScheduleBrowser_Cancelled(Sender As Object, e As EventArgs) Handles TaskPicker.CancelClicked
            ExitPicker()
        End Sub
        Private Sub ExitPicker()
            TaskPicker.SetDirectory()
            TaskPicker.Hide()
            TasksInRoutine.Show()
        End Sub
        Public Property CurrentRoutine As RoutineEntry
            Get
                Return varCurrentRoutine
            End Get
            Set(value As RoutineEntry)
                varCurrentRoutine = value
                If varCurrentRoutine Is Nothing Then
                    NothingSelected = True
                    OnRoutineChanged(New CurrentComponentChangedEventArgs(varCurrentRoutine))
                Else
                    NameItem.Content = varCurrentRoutine.Name
                    NameItem.Info = varCurrentRoutine.GetShortFilePath
                    DescriptionItem.Content = varCurrentRoutine.Description
                    CategoryItem.Content = varCurrentRoutine.CategoryName
                    For Each Item As TasksInRoutineListItem In TasksInRoutine.Items
                        RemoveHandler Item.RemoveClicked, AddressOf RemoveClicked
                        RemoveHandler Item.EditClicked, AddressOf EditClicked
                    Next
                    TasksInRoutine.Clear()
                    Dim iLast As Integer = value.Tasks.Count - 1
                    If iLast >= 0 Then
                        For i As Integer = 0 To iLast
                            Dim CurrentInfo As TaskInformation = varCurrentRoutine.Tasks(i)
                            CurrentInfo.MatchingEntryExists = (Loader.LookupTask(CurrentInfo.Name) IsNot Nothing)
                            Dim NewItem As New TasksInRoutineListItem(New TasksInRoutineListItemValue(CurrentInfo)) With {.Index = i}
                            With NewItem
                                .Name = CurrentInfo.Name
                                .IsActive = CurrentInfo.Active
                                '.RunAsync = CurrentInfo.RunAsync
                                '.WaitForPrevious = CurrentInfo.WaitForPrevious
                                '.Arguments
                                ' TODO: Extend functionality of tasksinroutinelistitem
                                'NewItem.WaitForPrevious = .
                                'NewItem.RunAsync = .RunAsync
                            End With
                            AddHandler NewItem.RemoveClicked, AddressOf RemoveClicked
                            AddHandler NewItem.EditClicked, AddressOf EditClicked
                            TasksInRoutine.Add(NewItem)
                        Next
                    End If


                    'For i As Integer = 0 To iLast
                    '    Dim CurrentInfo As RoutineInformation = varCurrentSchedule.Routines(i)
                    '    CurrentInfo.MatchingEntryExists = (Loader.LookupRoutine(CurrentInfo.Name) IsNot Nothing)
                    '    Dim NewItem As New RoutinesInScheduleListItem(New RoutinesInScheduleListItemValue(CurrentInfo)) With {.Index = i}
                    '    With NewItem
                    '        .Name = .Name
                    '        .IsActive = CurrentInfo.Active
                    '        .RunEvery = CurrentInfo.RunEvery
                    '        .Interval = CurrentInfo.Interval
                    '        AddHandler .RemoveClicked, AddressOf RemoveClicked
                    '        AddHandler .EditClicked, AddressOf EditClicked
                    '    End With
                    '    RoutinesInSchedule.Add(NewItem)
                    'Next



                    ' TODO: CHANGE TO:
                    'If iLast >= 0 Then
                    '    For i As Integer = 0 To iLast
                    '        Dim CurrentInfo As ScheduleInformation = varCurrentTemplate.Schedules(i)
                    '        CurrentInfo.MatchingEntryExists = (Loader.LookupSchedule(CurrentInfo.Name) IsNot Nothing)
                    '        Dim NewItem As New SchedulesInTemplateListItem(New SchedulesInTemplateListItemValue(CurrentInfo)) With {.Index = i}
                    '        NewItem.Name = CurrentInfo.Name
                    '        NewItem.IsActive = CurrentInfo.Active
                    '        AddHandler NewItem.RemoveClicked, AddressOf SchedulesInTemplateList_RemoveClicked
                    '        AddHandler NewItem.EditClicked, AddressOf SchedulesInTemplateList_EditClicked
                    '        SchedulesInTemplateList.Add(NewItem)
                    '    Next
                    'End If


                    OnRoutineChanged(New CurrentComponentChangedEventArgs(varCurrentRoutine))
                    ChangesMade = False
                    NothingSelected = False
                End If
            End Set
        End Property
        Public Shadows Property Parent As RoutineBrowser
            Get
                Return DirectCast(MyBase.Parent, RoutineBrowser)
            End Get
            Set(value As RoutineBrowser)
                MyBase.Parent = value
            End Set
        End Property
        Public Sub New(ByVal Rectangle As Rectangle, ByVal Parent As RoutineBrowser)
            DoubleBuffered = True
            Location = Rectangle.Location
            Size = Rectangle.Size
            BackColor = ColorHelper.Multiply(ApplicationBackColor, 0.7)
            With NothingSelectedScreen
                .Hide()
                .Size = Size
                .BackColor = BackColor
                .Parent = Me
            End With
            'With varLoadingSurface
            '    .Size = New Size(40, 40)
            '    .Location = New Point((Width - .Width) \ 2, (Height - .Height) \ 2)
            '    .Parent = Me
            '    .Hide()
            '    .SendToBack()
            'End With
            'varLoadingGraphics = New LoadingGraphics(Of PictureBox)(varLoadingSurface)
            'With varLoadingGraphics
            '    .Stroke = 3
            '    .Pen.Color = Color.White
            'End With
            NameItem = New ComponentViewItem(PropertiesList.ListContainer)
            With NameItem
                .EditErrorMessage = "A routine with this name already exists. Please choose another name."
                .ValidateEditFunction = AddressOf ValidateNameChange
                .Location = Point.Empty
                .Size = New Size(Width - 40, 80)
                .Title = "Name"
                .Content = "No routine selected"
                .Info = "C:/test"
                .AddAction(My.Resources.EditIconDefault, My.Resources.EditIconHover, AddressOf ChangeName)
                .AddAction(My.Resources.OpenFileLocationIconDefault, My.Resources.OpenFileLocationIconHover, AddressOf OpenFileLocation)
                AddHandler .ContentChanged, AddressOf ContentChanged
            End With
            DescriptionItem = New ComponentViewItem(PropertiesList.ListContainer)
            With DescriptionItem
                .Location = New Point(0, NameItem.Bottom)
                .Size = New Size(Width - 40, 80)
                .Title = "Description"
                .Content = "My description goes here"
                .AddAction(My.Resources.EditIconDefault, My.Resources.EditIconHover, AddressOf ChangeDescription)
                AddHandler .ContentChanged, AddressOf ContentChanged
            End With
            CategoryItem = New CategoryComponentViewItem(PropertiesList.ListContainer)
            With CategoryItem
                .Location = New Point(0, DescriptionItem.Bottom)
                .Size = New Size(Width - 40, 80)
                .Title = "Category"
                .Content = "No schedule selected"
                '.AddAction(My.Resources.EditIconDefault, My.Resources.EditIconHover, AddressOf ChangeDescription)
                '.AddAction(My.Resources.EditIconDefault, My.Resources.EditIconHover, AddressOf ChangeDescription)
                'AddHandler .ContentChanged, AddressOf ContentChanged ' TODO: Remove (not needed)
                AddHandler .DragStarted, AddressOf CategoryItem_DragStarted
                AddHandler .DragEventHappened, AddressOf CategoryItem_DragEventHappened
            End With
            Dim CancelAcceptButtonsSize As New Size(2 * 64, 64)
            Dim CancelAcceptButtonsRect As New Rectangle(New Point((Width - CancelAcceptButtonsSize.Width) \ 2, Height - CancelAcceptButtonsSize.Height), CancelAcceptButtonsSize)
            With CancelButton
                .Size = New Size(64, 64)
                .BackgroundImage = My.Resources.CancelChangesIconDefault
                .Location = CancelAcceptButtonsRect.Location
                .Parent = Me
                .Cursor = Cursors.Hand
                AddHandler .MouseEnter, AddressOf Button_MouseEnter
                AddHandler .MouseLeave, AddressOf Button_MouseLeave
                AddHandler .Click, AddressOf Button_Click
            End With
            With SaveButton
                .Size = New Size(64, 64)
                .BackgroundImage = My.Resources.AcceptChangesIconDefault
                .Location = New Point(CancelAcceptButtonsRect.Right - 64, CancelAcceptButtonsRect.Top)
                .Cursor = Cursors.Hand
                .Parent = Me
                AddHandler .MouseEnter, AddressOf Button_MouseEnter
                AddHandler .MouseLeave, AddressOf Button_MouseLeave
                AddHandler .Click, AddressOf Button_Click
            End With
            PropertiesListContainer = New AutoSizedContainer
            With PropertiesList
                With .Items
                    .Add(NameItem)
                    .Add(DescriptionItem)
                    .Add(CategoryItem)
                End With
                .Padding = New Padding(0)
                .Location = New Point(0, PropertiesListContainer.HeaderHeight)
            End With
            With PropertiesListContainer
                .HeaderText = "properties"
                .InnerControl = PropertiesList
                .Padding = New Padding(0, 0, 4, 0)
                .BorderWidth = 0
                .BackColor = ColorHelper.Multiply(ApplicationBackColor, 0.7)
                .Parent = Me
                .SetTotalSize(New Size(Width, 252 + .HeaderHeight))
                .AddHeaderControl({My.Resources.DeleteIconDefault, My.Resources.DeleteIconHover, My.Resources.DeleteIconPress}, AddressOf DeleteRoutine_Clicked)
            End With
            TaskListContainer = New AutoSizedContainer
            With TaskListContainer
                .Top = PropertiesListContainer.Bottom
                .BackColor = BackColor
                .HeaderText = "tasks"
                .InnerControl = TasksInRoutine
                .Padding = New Padding(0)
                .BorderWidth = 0
                .SetTotalSize(New Size(Width, SaveButton.Top - .Top))
                .Parent = Me
                .AddHeaderControl({My.Resources.AddIconDefault, My.Resources.AddIconHover, My.Resources.AddIconPress}, AddressOf AddTask)
            End With
            With TaskPicker
                .Parent = Me
                .Location = TaskListContainer.Location
                .Size = New Size(Width, Height - .Top)
                .BackColor = BackColor
                .BringToFront()
                .Hide()
            End With
            CurrentRoutine = Nothing
            Me.Parent = Parent
        End Sub
        Private Sub DeleteRoutine_Clicked(Sender As Object, e As EventArgs)
            If CurrentRoutine.Tasks.Count = 0 OrElse MsgBox("Are you sure you want to delete this routine?", MsgBoxStyle.OkCancel, "Confirm deletion of routine") = MsgBoxResult.Ok Then
                Dim OldDirectory As String = CurrentRoutine.GetFullFilePath
                Dim NewDirectory As String = ApplicationPath & "\Data\" & GarbageBinName & "\Routines\" & CurrentRoutine.GetFileNameFromName
                AsyncFileReader.Queue.AddOperation(AsyncFileReader.Queue.FileOperation.MoveFile(OldDirectory, NewDirectory, True), AddressOf MoveToBinFinished, Nothing)
            End If
        End Sub
        Private Sub MoveToBinFinished(e As FileOperationEventArgs)
            Dim OriginalPath As String = DirectCast(e.Arguments("OriginalPath"), String)
            Dim OriginalName As String = IO.Path.GetFileNameWithoutExtension(OriginalPath)
            If e.ErrorOccurred Then
                MsgBox("An unexpected error occurred (code: no time, deadline tomorrow).")
            Else
                Loader.RemoveRoutine(OriginalName)
                OnRoutineDeleted(EventArgs.Empty)
            End If
        End Sub
        Protected Sub OnRoutineDeleted(e As EventArgs)
            RaiseEvent RoutineDeleted(Me, e)
            CurrentRoutine = Nothing
        End Sub
        Private Sub ContentChanged(Sender As Object, e As EventArgs)
            ChangesMade = True
        End Sub
        Private Function ValidateNameChange(Sender As Object, e As ValidateEditEventArgs) As ValidateEditEventArgs
            If DirectCast(e.OldContent, String) = DirectCast(e.NewContent, String) Then
                e.Cancel = True
            ElseIf Loader.LookupRoutine(DirectCast(e.NewContent, String)) IsNot Nothing Then
                e.Valid = False
            End If
            Return e
        End Function
        Private Sub RemoveClicked(Sender As Object, e As EventArgs)
            If MsgBox("Are you sure you want to remove this routine from this schedule?" & vbNewLine & "This will not delete the routine object.", MsgBoxStyle.YesNo, "Confirm removal") = MsgBoxResult.Yes Then
                Dim SenderItem As TasksInRoutineListItem = DirectCast(Sender, TasksInRoutineListItem)
                'If Not CurrentSchedule.Routines.Contains(SenderItem.Value.Information) Then
                '    MsgBox("An unexpected error occurred (code 5)" & vbNewLine & "It is recommended that you do not save any changes.")
                'End If
                TasksInRoutine.Remove(SenderItem)
                RemoveHandler SenderItem.RemoveClicked, AddressOf RemoveClicked
                RemoveHandler SenderItem.EditClicked, AddressOf EditClicked
                ChangesMade = True
            End If
        End Sub
        Private Sub CategoryItem_DragStarted(Sender As Object, e As MouseEventArgs)
            RoutinesTab.NavigateBack()
        End Sub
        Private Sub CategoryItem_DragEventHappened(Sender As Object, e As QueryContinueDragEventArgs)
            Select Case e.Action
                Case DragAction.Cancel, DragAction.Drop
                    RoutinesTab.ShowItemsInCategoryList()
                Case Else
                    MsgBox("ELSE!")
            End Select
        End Sub
        Private Sub EditClicked(Sender As Object, e As EventArgs)
            If VerifyDiscard() Then
                Dim SenderItem As TasksInRoutineListItem = DirectCast(Sender, TasksInRoutineListItem)
                TasksTab.RequestView(SenderItem.Value.Information.Name)
                WindowControl.ShowTab(4)
            End If
        End Sub
        Private Sub AddTask(Sender As Object, e As MouseEventArgs)
            TasksInRoutine.Hide()
            TaskPicker.SetDirectory()
            TaskPicker.Show()
        End Sub
        Public Sub RefreshLists()
            TaskPicker.RefreshLists()
        End Sub
        Private Sub TaskList_ItemActiveChanged(Sender As Object, e As ListItemActiveChangedEventArgs) Handles TasksInRoutine.ItemActiveChanged
            'Dim SenderItem As RoutinesInScheduleListItem = DirectCast(e.SenderItem, RoutinesInScheduleListItem)
            'Dim RoutineInformationToChange As RoutineInformation = varCurrentSchedule.Routines(SenderItem.Index)
            'RoutineInformationToChange.Active = e.Data.Active
            ChangesMade = True
        End Sub
        Private Sub Button_MouseEnter(Sender As Object, e As EventArgs)
            Dim SenderButton As PictureBox = DirectCast(Sender, PictureBox)
            If ReferenceEquals(SenderButton, CancelButton) Then
                SenderButton.BackgroundImage = My.Resources.CancelChangesIconHover
            Else
                SenderButton.BackgroundImage = My.Resources.AcceptChangesIconHover
            End If
        End Sub
        Private Sub Button_MouseLeave(Sender As Object, e As EventArgs)
            Dim SenderButton As PictureBox = DirectCast(Sender, PictureBox)
            If ReferenceEquals(SenderButton, CancelButton) Then
                SenderButton.BackgroundImage = My.Resources.CancelChangesIconDefault
            Else
                SenderButton.BackgroundImage = My.Resources.AcceptChangesIconDefault
            End If
        End Sub
        Public Function VerifyDiscard() As Boolean
            If ChangesMade Then
                If MessageBox.Show("Any changes made to this routine will be lost. If you want to save these changes, click 'Cancel'", "Unsaved changes have been made", MessageBoxButtons.OKCancel, MessageBoxIcon.Exclamation, MessageBoxDefaultButton.Button2) = DialogResult.Cancel Then Return False
            End If
            Return True
        End Function
        Private Sub Button_Click(Sender As Object, e As EventArgs)
            Dim SenderButton As PictureBox = DirectCast(Sender, PictureBox)
            Dim OldName As String = CurrentRoutine.Name
            If ReferenceEquals(SenderButton, CancelButton) Then
                If VerifyDiscard() Then CurrentRoutine = Loader.LookupRoutine(OldName)
            Else
                Dim OldDirectory As String = CurrentRoutine.GetFullFilePath
                Dim NewName As String = NameItem.Content
                With varCurrentRoutine
                    .Name = NameItem.Content
                    .Description = DescriptionItem.Content
                    .CategoryName = CategoryItem.Content
                    .Tasks.Clear()
                    Dim iLast As Integer = TasksInRoutine.Items.Count - 1
                    If iLast >= 0 Then
                        For i As Integer = 0 To iLast
                            With TasksInRoutine.Items(i)
                                Dim NewInformation As TaskInformation = .Value.Information.Copy
                                NewInformation.MatchingEntryExists = (Loader.LookupTask(.Name) IsNot Nothing)
                                varCurrentRoutine.Tasks.Add(NewInformation)
                            End With
                        Next
                    End If
                End With
                Dim NewFullPath As String = varCurrentRoutine.GetFullFilePath
                If NewFullPath <> OldDirectory Then
                    AsyncFileReader.Queue.AddOperation(AsyncFileReader.Queue.FileOperation.MoveFile(OldDirectory, CurrentRoutine.GetFullFilePath, False), AddressOf MoveFinished, Nothing)
                    AsyncFileReader.Queue.AddOperation(AsyncFileReader.Queue.FileOperation.UpdateRoutineInformationInSchedules(OldName, NewName), AddressOf RoutineInformationInSchedulesUpdated, Nothing)
                Else
                    AsyncFileReader.Queue.AddOperation(AsyncFileReader.Queue.FileOperation.OverwriteFile(NewFullPath, CurrentRoutine.OutputFileContents), AddressOf OverwriteFinished, Nothing)
                End If
            End If
            'Dim SenderButton As PictureBox = DirectCast(Sender, PictureBox)
            'If ReferenceEquals(SenderButton, CancelButton) Then
            '    If VerifyDiscard() Then CurrentTemplate = Loader.LookupTemplate(varCurrentTemplate.Name)
            'Else
            '    Dim OldDirectory As String = varCurrentTemplate.GetFullFilePath
            '    With varCurrentTemplate
            '        .Name = NameItem.Content
            '        .Description = DescriptionItem.Content
            '        .CategoryName = CategoryItem.Content
            '        .Schedules.Clear()
            '        Dim iLast As Integer = SchedulesInTemplateList.Items.Count - 1
            '        If iLast >= 0 Then
            '            For i As Integer = 0 To iLast
            '                With SchedulesInTemplateList.Items(i)
            '                    'varCurrentTemplate.Schedules.Add(.Value.Information)
            '                    varCurrentTemplate.Schedules.Add(New ScheduleInformation(.Name, .IsActive) With {.MatchingEntryExists = (Loader.LookupSchedule(.Name) IsNot Nothing)})
            '                End With
            '            Next
            '        End If
            '        If varCurrentTemplate.GetFullFilePath <> OldDirectory Then ' TODO: Use Rename file operation instead
            '            AsyncFileReader.Queue.AddOperation(AsyncFileReader.Queue.FileOperation.MoveFile(OldDirectory, .GetFullFilePath, False), AddressOf MoveFinished, Nothing)
            '        Else
            '            AsyncFileReader.Queue.AddOperation(AsyncFileReader.Queue.FileOperation.OverwriteFile(.GetFullFilePath, .OutputFileContents), AddressOf OverwriteFinished, Nothing)
            '        End If
            '    End With


        End Sub
        Private Sub NothingSelectedScreen_Paint(Sender As Object, e As PaintEventArgs) Handles NothingSelectedScreen.Paint
            Dim TitleRects() As Rectangle = Globals.GetNothingSelectedRects(NothingSelectedScreen.Size)
            TextRenderer.DrawText(e.Graphics, TitleText, NothingSelectedTitleFont, TitleRects(0), Color.White)
            TextRenderer.DrawText(e.Graphics, SubTitleText, NothingSelectedSubTitleFont, TitleRects(1), Color.White)
        End Sub
        Private Sub RoutineInformationInSchedulesUpdated(e As FileOperationEventArgs)
            If e.ErrorOccurred Then
                MsgBox("(Finished) Errors occurred: " & vbNewLine & e.Exception.Message)
            Else
                Dim ModifiedSchedules() As String = DirectCast(e.Result, String()) ' TODO: get rid of these
            End If
        End Sub
        Private Sub MoveFinished(Result As FileOperationEventArgs)
            If Result.ErrorOccurred Then
                MsgBox("Error occurred: " & Result.Exception.Message & vbNewLine & DirectCast(Result.Result, String))
                ' TODO: Log error
            Else
                AsyncFileReader.Queue.AddOperation(AsyncFileReader.Queue.FileOperation.OverwriteFile(varCurrentRoutine.GetFullFilePath, varCurrentRoutine.OutputFileContents), AddressOf OverwriteFinished, Nothing)
            End If
        End Sub
        Private Sub OverwriteFinished(Result As FileOperationEventArgs)
            If Result.ErrorOccurred Then
                MsgBox("Overwrite error: " & Result.Exception.Message)
                ' TODO: Log error
            Else
                RaiseEvent ChangesSaved(Me, New CurrentComponentChangedEventArgs(varCurrentRoutine))
                CurrentRoutine = varCurrentRoutine ' TODO: Replace with "refresh" method
            End If
        End Sub
        Private Sub OpenFileLocation(Args As Object)
            Process.Start("explorer.exe", "/select," & varCurrentRoutine.GetFullFilePath)
        End Sub
        Private Sub ChangeName(Args As Object)
            NameItem.EditContent()
        End Sub
        Private Sub ChangeDescription(Args As Object)
            DescriptionItem.EditContent()
        End Sub
        Protected Overridable Sub OnRoutineChanged(e As CurrentComponentChangedEventArgs)
            Invalidate()
            RaiseEvent RoutineChanged(Me, e)
        End Sub
        Public Overloads Sub ViewRoutine(RoutineFile As String, Optional ByVal SurelyLoaded As Boolean = False, Optional ByVal ForceReload As Boolean = False)
            Dim Match As RoutineEntry = Loader.LookupRoutine(FormatConverter.ExtractFileName(RoutineFile, False))
            If Not ForceReload Then
                If Match IsNot Nothing Then
                    CurrentRoutine = Match
                ElseIf SurelyLoaded Then
                    Throw New Exception("Could not find the routine " & FormatConverter.ExtractFileName(RoutineFile, False) & ". It may not be loaded yet.")
                Else
                    AsyncFileReader.Queue.AddOperation(AsyncFileReader.Queue.FileOperation.ReadLines(RoutineFile), AddressOf RoutineLinesLoaded, Nothing)
                    'varLoadingGraphics.Spin(50, 10)
                End If
            Else
                CurrentRoutine = Nothing
                If Match IsNot Nothing Then
                    Loader.RemoveRoutine(Match.Name)
                    AsyncFileReader.Queue.AddOperation(AsyncFileReader.Queue.FileOperation.ReadLines(RoutineFile), AddressOf RoutineLinesLoaded, Nothing)
                    'varLoadingGraphics.Spin(50, 10)
                ElseIf SurelyLoaded Then
                    Throw New Exception("Could not find the routine " & FormatConverter.ExtractFileName(RoutineFile, False) & ". It may not be loaded yet.")
                Else
                    AsyncFileReader.Queue.AddOperation(AsyncFileReader.Queue.FileOperation.ReadLines(RoutineFile), AddressOf RoutineLinesLoaded, Nothing)
                    'varLoadingGraphics.Spin(50, 10)
                End If
            End If
        End Sub
        Private Sub RoutineLinesLoaded(e As FileOperationEventArgs)
            'varLoadingGraphics.StopSpin()
            Dim Path As String = DirectCast(e.Arguments("Path"), String)
            Loader.AddRoutine(RoutineEntry.CreateFromLines(DirectCast(e.Result, String()), Path))
            ViewRoutine(Path, True)
        End Sub
    End Class





    Public Class ScheduleBrowser
        Inherits Tab
        Private ScheduleCategoriesContainer As New AutoSizedContainer
        Private WithEvents SchedulesInCategoryContainer As New AutoSizedContainer
        Private WithEvents CategoriesList As New CategoryList
        Private WithEvents SchedulesInSelectedCategory As New SchedulesInCategoryList
        'Private NewCategory As New NewCategoryControl
        Private WithEvents Viewer As ScheduleViewer
        'Private CurrentX As Integer = 0, LastX As Integer = 20
        'Private Stage As Integer = 0
        'Private SC As SynchronizationContext = SynchronizationContext.Current
        Private varCurrentCategoryName As String
        Private ScheduleNameToSelectAfterReload As String = Nothing
        Private RenameOccurred As Boolean
        'Private WithEvents SlideTimer As New Timers.Timer(1000 / 60)
        Public Sub New(Parent As MultiTabWindow)
            MyBase.New(Parent)
            'SlideTimer.AutoReset = False
            CategoriesList.BackColor = ApplicationBackColor
            With ScheduleCategoriesContainer
                .HeaderText = "schedule categories"
                .Location = New Point(0, My.Resources.Header.Height)
                .InnerControl = CategoriesList
                .Parent = Me
                .Padding = New Padding(0)
                .BorderWidth = 0
                .SetTotalSize(New Size(CategoriesList.Width, Height - Header.Height))
                .AddHeaderControl(New Image() {My.Resources.ReloadIconDefault, My.Resources.ReloadIconHover, My.Resources.ReloadIconPress}, AddressOf ReloadCategoriesIcon_Clicked)
                .AddHeaderControl(New Image() {My.Resources.AddIconDefault, My.Resources.AddIconHover, My.Resources.AddIconPress}, AddressOf AddCategoryIcon_Clicked)
            End With
            With SchedulesInCategoryContainer
                .HeaderText = "schedules > category"
                .Location = New Point(0, My.Resources.Header.Height)
                .InnerControl = SchedulesInSelectedCategory
                .Parent = Me
                .SetTotalSize(New Size(CategoriesList.Width, Height - Header.Height))
                .AddHeaderControl(New Image() {My.Resources.ReloadIconDefault, My.Resources.ReloadIconHover, My.Resources.ReloadIconPress}, AddressOf ReloadSchedulesIcon_Clicked)
                .AddHeaderControl(New Image() {My.Resources.AddIconDefault, My.Resources.AddIconHover, My.Resources.AddIconPress}, AddressOf AddScheduleIcon_Clicked)
                .AddHeaderControl(New Image() {My.Resources.BackButtonDefault, My.Resources.BackButtonHover, My.Resources.BackButtonPress}, AddressOf BackIcon_Clicked)
            End With
            Viewer = New ScheduleViewer(New Rectangle(New Point(ScheduleCategoriesContainer.Right, My.Resources.Header.Height), New Size(Width - ScheduleCategoriesContainer.Right, Height - My.Resources.Header.Height)), Me)
            Viewer.BringToFront()
            AddHandler Loader.AllSchedulesLoaded, AddressOf Loader_AllSchedulesLoaded
        End Sub
        Private Sub Loader_AllSchedulesLoaded(e As EventArgs)
            ReloadCategories()
        End Sub
        Private Sub Viewer_ScheduleChanged(Sender As Object, e As CurrentComponentChangedEventArgs) Handles Viewer.ScheduleChanged
            If Viewer.CurrentSchedule IsNot Nothing AndAlso Loader.ScheduleCategoryExists(Viewer.CurrentSchedule.CategoryName) Then
                Dim Match As CategoryListItem = CategoriesList.FindName(Viewer.CurrentSchedule.CategoryName)
                Match.IsActive = True
                CategoriesList.SetAllActive(False, Match.Index)
            Else
                CategoriesList.SetAllActive(False)
            End If
        End Sub
        Public Function VerifyViewerDiscard() As Boolean
            Return Viewer.VerifyDiscard
        End Function
        Public Property CurrentCategoryName As String
            Get
                Return varCurrentCategoryName
            End Get
            Set(value As String)
                varCurrentCategoryName = value
                SchedulesInCategoryContainer.HeaderText = "schedules   " & varCurrentCategoryName
            End Set
        End Property
        Private Sub SchedulesInCategoryContainer_HeaderPaint(Sender As Object, e As PaintEventArgs) Handles SchedulesInCategoryContainer.HeaderPaint
            With e.Graphics
                Dim DrawImage As Image = My.Resources.SmallArrowBlack
                Dim ImageSize As Size = DrawImage.Size
                Dim TextLength As Integer = TextRenderer.MeasureText(e.Graphics, "schedules", SchedulesInCategoryContainer.Font).Width + SchedulesInCategoryContainer.TextPadding - 3
                Dim ImageLocation As New Point(TextLength, CInt(.ClipBounds.Height - ImageSize.Height) \ 2 + 1)
                e.Graphics.DrawImage(DrawImage, ImageLocation)
            End With
        End Sub
        Private Sub CategoriesList_CategoryDropped(Sender As Object, e As CategoryDroppedEventArgs) Handles CategoriesList.CategoryDropped
            If Viewer.VerifyDiscard Then
                RenameOccurred = True
                Dim OldCategoryName As String = DirectCast(e.Trigger.Data.GetData(DataFormats.StringFormat), String)
                Viewer.CurrentSchedule.ChangeCategoryAndMoveFile(e.TargetItem.Value.CategoryName, AddressOf CategoryChanged, Viewer.CurrentSchedule)
            End If
        End Sub
        Private Sub CategoryChanged(e As FileOperationEventArgs)
            If e.ErrorOccurred Then
                MsgBox(e.Exception.Message)
            Else
                Dim TargetComponent As ScheduleEntry = DirectCast(e.State, ScheduleEntry)
                ReloadCategories()
                RequestView(TargetComponent.Name)
                'SelectCategory(TargetComponent.CategoryName)
                'SelectTask(TargetCompon)
            End If
        End Sub
        Private Sub CategoriesList_CategoryEdited(Sender As Object, e As CategoryEditedEventArgs) Handles CategoriesList.CategoryEdited
            Dim OldName As String = e.Trigger.OldCategoryName
            Dim NewName As String = e.Trigger.NewCategoryName
            If Viewer.VerifyDiscard Then
                If Loader.ScheduleCategoryExists(NewName) Then
                    MsgBox("There is already a category with that name.")
                Else
                    RenameCategory(OldName, NewName, True)
                End If
            Else
                e.CategoryItem.Value = New CategoryListItemValue(OldName)
            End If
        End Sub
        Private Sub CategoriesList_CategoryDeleted(Sender As Object, e As CategoryDeletedEventArgs) Handles CategoriesList.CategoryDeleted
            Dim Value As CategoryListItemValue = e.CategoryItem.Value
            If Value.CategoryName = GarbageBinName Then
                MsgBox("You cannot delete the garbage bin.")
            Else
                Dim TargetItems As IEnumerable(Of ComponentEntry) = Loader.SchedulesInCategory(Value.CategoryName)
                If TargetItems.Count > 0 Then
                    If MsgBox("Are you sure you want to delete this category? All items contained in it will be moved to the garbage bin.", MsgBoxStyle.OkCancel) = MsgBoxResult.Ok Then
                        For Each Item As ComponentEntry In TargetItems
                            Dim OldDirectory As String = Item.GetFullFilePath
                            Dim NewDirectory As String = ApplicationPath & "\Data\" & GarbageBinName & "\Schedules\" & Item.GetFileNameFromName
                            AsyncFileReader.Queue.AddOperation(AsyncFileReader.Queue.FileOperation.MoveFile(OldDirectory, NewDirectory, True), AddressOf MoveToBinFinished, Nothing)
                        Next
                    End If
                Else
                    Loader.RemoveScheduleCategory(Value.CategoryName)
                    AsyncFileReader.Queue.AddOperation(AsyncFileReader.Queue.FileOperation.DeleteFolder(ScheduleEntry.BaseDirectory & "\" & Value.CategoryName), AddressOf DeleteFolderFinished, Nothing)
                End If ' TODO: Move entire folder instead.
            End If
        End Sub
        Private Sub MoveToBinFinished(e As FileOperationEventArgs)
            Dim OriginalPath As String = DirectCast(e.Arguments("OriginalPath"), String)
            Dim NewPath As String = DirectCast(e.Arguments("NewPath"), String)
            Dim OriginalName As String = IO.Path.GetFileNameWithoutExtension(OriginalPath)
            Dim NewName As String = IO.Path.GetFileNameWithoutExtension(NewPath)
            Dim CategoryName As String = FormatConverter.ExtractHighestLevelDirectory(OriginalPath, True)
            If e.ErrorOccurred Then
                MsgBox("An unexpected error occurred (code 10).")
            Else
                Loader.RemoveSchedule(OriginalName)
                If Loader.SchedulesInCategory(CategoryName).Count = 0 Then
                    Loader.RemoveScheduleCategory(CategoryName)
                    Dim OldFolder As String = IO.Directory.GetParent(OriginalPath).FullName
                    If CurrentCategoryName = CategoryName Then Viewer.CurrentSchedule = Nothing
                    AsyncFileReader.Queue.AddOperation(AsyncFileReader.Queue.FileOperation.DeleteFolder(OldFolder), AddressOf DeleteFolderFinished, Nothing)
                End If
            End If
        End Sub
        Private Sub DeleteFolderFinished(e As FileOperationEventArgs)
            If e.ErrorOccurred Then
                MsgBox("Could not delete " & DirectCast(e.Arguments("Path"), String) & ": " & e.Exception.Message)
            End If
            CurrentCategoryName = Nothing
            ScheduleNameToSelectAfterReload = Nothing
            ReloadCategories()
        End Sub
        Private Sub RenameCategory(OldName As String, NewName As String, Optional ClearList As Boolean = False)
            If Not RenameOccurred Then
                RenameOccurred = True
                If ClearList Then CategoriesList.Clear()
                AsyncFileReader.Queue.AddOperation(AsyncFileReader.Queue.FileOperation.RenameFolder(ScheduleEntry.BaseDirectory & "\" & OldName, NewName, False), AddressOf RenameCategory_Finished, Nothing)
            End If
        End Sub
        Private Sub RenameCategory_Finished(e As FileOperationEventArgs)
            If e.ErrorOccurred Then
                MsgBox("Could not rename category: " & e.Exception.Message)
            Else
                Dim OldName As String = FormatConverter.ExtractHighestLevelDirectory(DirectCast(e.Arguments("Path"), String), False)
                Dim NewName As String = DirectCast(e.Result, String)
                Loader.RenameScheduleCategory(OldName, NewName)
                CurrentCategoryName = NewName
                If Viewer.CurrentSchedule IsNot Nothing Then
                    ScheduleNameToSelectAfterReload = Viewer.CurrentSchedule.Name
                Else
                    ScheduleNameToSelectAfterReload = Nothing
                End If
                ReloadCategories()
                ReloadSchedules()
            End If
        End Sub
        Private Sub BackIcon_Clicked(Sender As Object, e As MouseEventArgs)
            NavigateBack()
        End Sub
        Private Sub ReloadCategoriesIcon_Clicked(Sender As Object, e As MouseEventArgs)
            ReloadCategories()
        End Sub
        Private Sub ReloadSchedulesIcon_Clicked(Sender As Object, e As MouseEventArgs)
            If Viewer.VerifyDiscard Then
                If Viewer.CurrentSchedule IsNot Nothing Then ScheduleNameToSelectAfterReload = Viewer.CurrentSchedule.Name
                ReloadSchedules()
            End If
        End Sub
        Public Sub RequestView(ByVal ScheduleName As String)
            Dim Match As ScheduleEntry = Loader.LookupSchedule(ScheduleName)
            If Match IsNot Nothing Then
                SelectCategory(Match.CategoryName)
                Dim ItemMatch As SchedulesInCategoryListItem = SchedulesInSelectedCategory.Items.Find(Function(x As SchedulesInCategoryListItem) As Boolean
                                                                                                          If x.Value.Name = ScheduleName Then Return True
                                                                                                          Return False
                                                                                                      End Function)
                If ItemMatch IsNot Nothing Then
                    ItemMatch.IsSelected = True
                End If
            Else
                MsgBox("No entry match")
            End If
        End Sub
        Public Sub NavigateBack()
            'CurrentCategoryName = Nothing
            SchedulesInCategoryContainer.Hide()
            ScheduleCategoriesContainer.Show()
        End Sub
        Public Sub ShowItemsInCategoryList()
            ScheduleCategoriesContainer.Hide()
            SchedulesInCategoryContainer.Show()
        End Sub
        Private Sub ReloadCategories()
            AsyncFileReader.Queue.AddOperation(AsyncFileReader.Queue.FileOperation.ListDirectories(ApplicationPath & "\Data\Schedules"), AddressOf ReloadCategories_Finished, Nothing)
        End Sub
        Private Sub ReloadSchedules()
            AsyncFileReader.Queue.AddOperation(AsyncFileReader.Queue.FileOperation.ListFiles(ApplicationPath & "\Data\Schedules\" & CurrentCategoryName), AddressOf ReloadSchedules_Finished, Nothing)
        End Sub
        Private Sub ReloadCategories_Finished(e As FileOperationEventArgs)
            Dim Directories() As String = DirectCast(e.Result, String())
            CategoriesList.Clear()
            Dim iLast As Integer = Directories.Count - 1
            If iLast >= 0 Then
                For i As Integer = 0 To Directories.Count - 1
                    Directories(i) = FormatConverter.ExtractHighestLevelDirectory(Directories(i), False)
                    Dim NewItem As New CategoryListItem(Directories(i)) With {.CanEdit = True}
                    If Viewer.CurrentSchedule IsNot Nothing Then NewItem.IsActive = (Directories(i) = Viewer.CurrentSchedule.CategoryName)
                    If Not Loader.ScheduleCategories.Contains(Directories(i)) Then
                        Loader.AddScheduleCategory(Directories(i))
                    End If
                    CategoriesList.Add(NewItem)
                Next
            End If
            'Viewer.RefreshLists()
            'ScheduleNameToSelectAfterReload = Nothing
        End Sub
        Private Sub ReloadSchedules_Finished(e As FileOperationEventArgs)
            Dim Files() As String = DirectCast(e.Result, String())
            SchedulesInSelectedCategory.Clear()
            Dim iLast As Integer = Files.Count - 1
            If iLast >= 0 Then
                For i As Integer = 0 To iLast
                    Dim ScheduleName As String = FormatConverter.ExtractFileName(Files(i), False)
                    'If Loader.LookupSchedule(ScheduleName) Is Nothing Then Loader.AddSchedule(Files(i))
                    SchedulesInSelectedCategory.Add(New SchedulesInCategoryListItem(New ItemsInCategoryListItemValue(ScheduleName)))
                Next
            End If
            If ScheduleNameToSelectAfterReload = Nothing AndAlso Viewer.CurrentSchedule IsNot Nothing Then ScheduleNameToSelectAfterReload = Viewer.CurrentSchedule.Name
            For Each Item As SchedulesInCategoryListItem In SchedulesInSelectedCategory.Items
                If Item.Value.Name = ScheduleNameToSelectAfterReload Then
                    Item.IsSelected = True
                    Exit For
                End If
            Next
            Viewer.RefreshLists()
            ScheduleNameToSelectAfterReload = Nothing
            RenameOccurred = False
        End Sub
        Private Sub Viewer_ChangesSaved(Sender As Object, e As EventArgs) Handles Viewer.ChangesSaved, Viewer.ScheduleDeleted
            ReloadSchedules()
            ReloadCategories()
        End Sub
        Protected Overrides Sub OnTabShown(e As EventArgs)
            MyBase.OnTabShown(e)
            Header.CurrentDirectiry = {HeaderControl.Views.Overview, HeaderControl.Views.TemplateBrowser, HeaderControl.Views.ScheduleBrowser}
            Viewer.CurrentSchedule = Viewer.CurrentSchedule
        End Sub
        Private Sub CreateScheduleFromFileTest(Path As String)
            AsyncFileReader.Queue.AddOperation(AsyncFileReader.Queue.FileOperation.ReadLines(Path), AddressOf FileReadFinished, Nothing)
        End Sub
        Private Sub FileReadFinished(e As FileOperationEventArgs)
            If e.ErrorOccurred Then
                Throw e.Exception
            End If
            Dim Lines() As String = DirectCast(e.Result, String())
            Dim NewSchedule As ScheduleEntry = ScheduleEntry.CreateFromLines(Lines, DirectCast(e.Arguments("Path"), String))
        End Sub
        Private Sub AddCategoryIcon_Clicked(Sender As Object, e As EventArgs)
            CreateCategory()
        End Sub
        Private Sub AddScheduleIcon_Clicked(Sender As Object, e As EventArgs)
            If Viewer.VerifyDiscard Then AddNewSchedule()
        End Sub
        Private Sub CreateCategory()
            ' TODO: Create new folder
            AsyncFileReader.Queue.AddOperation(AsyncFileReader.Queue.FileOperation.CreateDirectory(ScheduleEntry.BaseDirectory & "\New category", True), AddressOf CategoryCreated, Nothing)
        End Sub
        Private Sub CategoryCreated(e As FileOperationEventArgs)
            Dim NewName As String = FormatConverter.ExtractHighestLevelDirectory(DirectCast(e.Result, String), False)
            Loader.AddScheduleCategory(NewName)
            ReloadCategories()
        End Sub
        Private Sub AddNewSchedule()
            AsyncFileReader.Queue.AddOperation(AsyncFileReader.Queue.FileOperation.CreateFile(ScheduleEntry.BaseDirectory & "\" & CurrentCategoryName, "New Schedule.txt", True, FormatConverter.GetBytesFromString("//meta" & vbNewLine & "$meta: %desc%==This schedule does not have a description yet" & vbNewLine & "//routines" & vbNewLine), ScheduleEntry.BaseDirectory), AddressOf AddNewSchedule_Finished, Nothing)
        End Sub
        Private Sub AddNewSchedule_Finished(e As FileOperationEventArgs)
            If Not e.ErrorOccurred Then
                ScheduleNameToSelectAfterReload = IO.Path.GetFileNameWithoutExtension(DirectCast(e.Result, String))
            Else
                MsgBox("An unexpected error occurred (code 6): " & e.Exception.Message)
            End If
            ReloadSchedules()
        End Sub
        Protected Overrides Sub OnSizeChanged(e As EventArgs)
            MyBase.OnSizeChanged(e)
            If ScheduleCategoriesContainer IsNot Nothing Then
                With ScheduleCategoriesContainer
                    .SetTotalSize(New Size(.Width, Height - My.Resources.Header.Height))
                End With
            End If
        End Sub
        Private Sub CategorySelected(Sender As Object, e As ListItemSelectionChangedEventArgs) Handles CategoriesList.ItemSelectionChanged
            If e.Data.Selected Then
                Dim SenderItem As CategoryListItem = DirectCast(e.SenderItem, CategoryListItem)
                SelectCategory(SenderItem.Value.CategoryName)
            End If
        End Sub
        Private Sub SelectCategory(ByVal NewCategoryName As String)
            CategoriesList.DeselectAll()
            If NewCategoryName <> CurrentCategoryName Then
                SchedulesInSelectedCategory.Clear()
                'Viewer.CurrentSchedule = Nothing
                Dim Schedules As List(Of ScheduleEntry) = Loader.SchedulesInCategory(NewCategoryName)
                For Each Schedule As ScheduleEntry In Schedules
                    SchedulesInSelectedCategory.Add(New SchedulesInCategoryListItem(New ItemsInCategoryListItemValue(Schedule.Name)))
                Next
            End If
            CurrentCategoryName = NewCategoryName
            If Viewer.CurrentSchedule IsNot Nothing Then
                Dim iLast As Integer = SchedulesInSelectedCategory.Items.Count - 1
                If iLast >= 0 Then
                    For i As Integer = 0 To iLast
                        If SchedulesInSelectedCategory.Item(i).Value.Name = Viewer.CurrentSchedule.Name Then
                            SchedulesInSelectedCategory.Item(i).IsSelected = True
                            Exit For
                        End If
                    Next
                End If
            End If
            ScheduleCategoriesContainer.Hide()
            SchedulesInCategoryContainer.Show()
        End Sub
        Private Sub ScheduleSelected(Sender As Object, e As ListItemSelectionChangedEventArgs) Handles SchedulesInSelectedCategory.ItemSelectionChanged
            If e.Data.Selected Then
                Dim SenderItem As SchedulesInCategoryListItem = DirectCast(e.SenderItem, SchedulesInCategoryListItem)
                SelectSchedule(SenderItem)
            End If
        End Sub
        Private Sub SelectSchedule(SelectItem As SchedulesInCategoryListItem)
            'SchedulesInSelectedCategory.DeselectAll(SelectItem.Index)
            If Viewer.CurrentSchedule IsNot Nothing AndAlso SelectItem.Value.Name <> Viewer.CurrentSchedule.Name Then
                If Viewer.VerifyDiscard Then
                    SchedulesInSelectedCategory.DeselectAll(SelectItem.Index)
                    Viewer.ViewSchedule(ScheduleEntry.BaseDirectory & "\" & CurrentCategoryName & "\" & SelectItem.Value.Name & ".txt")
                Else
                    SelectItem.IsSelected = False
                End If
            ElseIf Viewer.CurrentSchedule IsNot Nothing Then ' Same schedule selected
                'SchedulesInSelectedCategory.DeselectAll(SelectItem.Index)
                If RenameOccurred Then
                    Viewer.ViewSchedule(ScheduleEntry.BaseDirectory & "\" & CurrentCategoryName & "\" & SelectItem.Value.Name & ".txt", True, True)
                End If
            Else ' No schedule is being viewed
                Viewer.ViewSchedule(ScheduleEntry.BaseDirectory & "\" & CurrentCategoryName & "\" & SelectItem.Value.Name & ".txt")
            End If
            RenameOccurred = False
        End Sub
    End Class
    Public Class ScheduleViewer
        Inherits Control
        Private varCurrentSchedule As ScheduleEntry = Nothing
        Private varPadding As New Padding(10)
        Private varUseCompatibleTextRendering As Boolean = True
        'Private varLoadingSurface As New PictureBox
        'Private varLoadingGraphics As LoadingGraphics(Of PictureBox)
        Private InnerRect As Rectangle = New Rectangle(New Point(varPadding.Left, varPadding.Top), New Size(Width - varPadding.Left - varPadding.Right, 100))
        Private CancelButton, SaveButton As New PictureBox
        Private ElementBrush As SolidBrush
        Private varElementColor As Color = Color.FromArgb(200, 200, 200)
        Private PropertiesListContainer, RoutineListContainer As AutoSizedContainer
        Private WithEvents PropertiesList As New PropertyList
        Private WithEvents RoutinesInSchedule As New RoutinesInScheduleList
        Private NameItem, DescriptionItem As ComponentViewItem
        Private CategoryItem As CategoryComponentViewItem
        Private WithEvents RoutinePicker As New RoutinePicker
        Public Event ScheduleChanged(Sender As Object, e As CurrentComponentChangedEventArgs)
        Public Event ScheduleDeleted(Sender As Object, e As EventArgs)
        Public Event ChangesSaved(Sender As Object, e As CurrentComponentChangedEventArgs)
        Private varChangesMade As Boolean
        Private WithEvents NothingSelectedScreen As New PictureBox ' TODO: HANDLE PAINT EVENT AND DRAW MESSAGE
        Private Sub PropertiesList_ScrollHandleVisibleChanged(Sender As Object, e As VisibleEventArgs) Handles PropertiesList.ScrollHandleVisibleChanged
            With PropertiesListContainer
                If e.Visible Then
                    .Padding = New Padding(0, 0, 0, 4)
                Else
                    .Padding = New Padding(0, 0, 0, 0)
                End If
                .SetTotalSize(New Size(Width, 3 * 80 + .HeaderHeight))
            End With
        End Sub
        Public Property ChangesMade As Boolean
            Get
                Return varChangesMade
            End Get
            Set(value As Boolean)
                varChangesMade = value
            End Set
        End Property
        Private Property NothingSelected As Boolean
            Get
                Return (CurrentSchedule Is Nothing)
            End Get
            Set(value As Boolean)
                If value Then
                    NothingSelectedScreen.Show()
                Else
                    NothingSelectedScreen.Hide()
                End If
            End Set
        End Property
        Private Sub RoutinePicker_ItemsAdded(Sender As Object, e As ItemsAddedEventArgs) Handles RoutinePicker.AddClicked
            For Each Item As ChildComponentInformation In e.SelectedItems
                If CurrentSchedule.Routines.Find(Function(x As ChildComponentInformation) As Boolean
                                                     If x.Name = Item.Name Then Return True
                                                     Return False
                                                 End Function) IsNot Nothing Then
                    'MsgBox("Already in")
                End If
                Dim NewInfo As RoutineInformation = DirectCast(Item, RoutineInformation).Copy
                RoutinesInSchedule.Add(New RoutinesInScheduleListItem(New RoutinesInScheduleListItemValue(NewInfo)) With {.Index = RoutinesInSchedule.Items.Count})
                AddHandler RoutinesInSchedule.Items.Last.RemoveClicked, AddressOf RemoveClicked
                AddHandler RoutinesInSchedule.Items.Last.EditClicked, AddressOf EditClicked
                ' TODO: Add handlers for routine editing controls
                ChangesMade = True
            Next
            ExitPicker()
        End Sub
        Private Sub RoutinesInScheduleList_ValueChanged(Sender As Object, e As RoutineItemValueChangedEventArgs) Handles RoutinesInSchedule.ItemValueChanged
            ChangesMade = True
        End Sub
        Private Sub RoutinePicker_Cancelled(Sender As Object, e As EventArgs) Handles RoutinePicker.CancelClicked
            ExitPicker()
        End Sub
        Private Sub ExitPicker()
            RoutinePicker.SetDirectory()
            RoutinePicker.Hide()
            RoutinesInSchedule.Show()
        End Sub
        Public Property CurrentSchedule As ScheduleEntry
            Get
                Return varCurrentSchedule
            End Get
            Set(value As ScheduleEntry)
                varCurrentSchedule = value
                If varCurrentSchedule Is Nothing Then
                    NothingSelected = True
                    OnScheduleChanged(New CurrentComponentChangedEventArgs(varCurrentSchedule))
                Else
                    NameItem.Content = varCurrentSchedule.Name
                    NameItem.Info = varCurrentSchedule.GetShortFilePath
                    DescriptionItem.Content = varCurrentSchedule.Description
                    CategoryItem.Content = varCurrentSchedule.CategoryName
                    For Each Item As RoutinesInScheduleListItem In RoutinesInSchedule.Items
                        RemoveHandler Item.RemoveClicked, AddressOf RemoveClicked
                        RemoveHandler Item.EditClicked, AddressOf EditClicked
                    Next
                    RoutinesInSchedule.Clear()
                    Dim iLast As Integer = value.Routines.Count - 1
                    If iLast >= 0 Then
                        For i As Integer = 0 To iLast
                            Dim CurrentInfo As RoutineInformation = varCurrentSchedule.Routines(i)
                            CurrentInfo.MatchingEntryExists = (Loader.LookupRoutine(CurrentInfo.Name) IsNot Nothing)
                            Dim NewItem As New RoutinesInScheduleListItem(New RoutinesInScheduleListItemValue(CurrentInfo)) With {.Index = i}
                            With NewItem
                                .Name = .Name
                                .IsActive = CurrentInfo.Active
                                '.RunEvery = CurrentInfo.RunEvery
                                '.Interval = CurrentInfo.Interval
                                AddHandler .RemoveClicked, AddressOf RemoveClicked
                                AddHandler .EditClicked, AddressOf EditClicked
                            End With
                            RoutinesInSchedule.Add(NewItem)
                        Next
                    End If
                    ' CORRECT BACKUP (small syntactic 
                    'If iLast >= 0 Then
                    '    For i As Integer = 0 To iLast
                    '        Dim CurrentInfo As ScheduleInformation = varCurrentTemplate.Schedules(i)
                    '        CurrentInfo.MatchingEntryExists = (Loader.LookupSchedule(CurrentInfo.Name) IsNot Nothing)
                    '        Dim NewItem As New SchedulesInTemplateListItem(New SchedulesInTemplateListItemValue(CurrentInfo)) With {.Index = i}
                    '        NewItem.Name = CurrentInfo.Name
                    '        NewItem.IsActive = CurrentInfo.Active
                    '        AddHandler NewItem.RemoveClicked, AddressOf SchedulesInTemplateList_RemoveClicked
                    '        AddHandler NewItem.EditClicked, AddressOf SchedulesInTemplateList_EditClicked
                    '        SchedulesInTemplateList.Add(NewItem)
                    '    Next
                    'End If
                    OnScheduleChanged(New CurrentComponentChangedEventArgs(varCurrentSchedule))
                    ChangesMade = False
                    NothingSelected = False
                End If
            End Set
        End Property
        Public Shadows Property Parent As ScheduleBrowser
            Get
                Return DirectCast(MyBase.Parent, ScheduleBrowser)
            End Get
            Set(value As ScheduleBrowser)
                MyBase.Parent = value
            End Set
        End Property
        Public Sub New(ByVal Rectangle As Rectangle, ByVal Parent As ScheduleBrowser)
            DoubleBuffered = True
            Location = Rectangle.Location
            Size = Rectangle.Size
            BackColor = ColorHelper.Multiply(ApplicationBackColor, 0.7)
            With NothingSelectedScreen
                .Hide()
                .Size = Size
                .BackColor = BackColor
                .Parent = Me
            End With
            'With varLoadingSurface
            '    .Size = New Size(40, 40)
            '    .Location = New Point((Width - .Width) \ 2, (Height - .Height) \ 2)
            '    .Parent = Me
            '    .Hide()
            '    .SendToBack()
            'End With
            'varLoadingGraphics = New LoadingGraphics(Of PictureBox)(varLoadingSurface)
            'With varLoadingGraphics
            '    .Stroke = 3
            '    .Pen.Color = Color.White
            'End With
            NameItem = New ComponentViewItem(PropertiesList.ListContainer)
            With NameItem
                .EditErrorMessage = "A schedule with this name already exists. Please choose another name."
                .ValidateEditFunction = AddressOf ValidateNameChange
                .Location = Point.Empty
                .Size = New Size(Width - 40, 80)
                .Title = "Name"
                .Content = "No schedule selected"
                .Info = "C:/test"
                .AddAction(My.Resources.EditIconDefault, My.Resources.EditIconHover, AddressOf ChangeName)
                .AddAction(My.Resources.OpenFileLocationIconDefault, My.Resources.OpenFileLocationIconHover, AddressOf OpenFileLocation)
                AddHandler .ContentChanged, AddressOf ContentChanged
            End With
            DescriptionItem = New ComponentViewItem(PropertiesList.ListContainer)
            With DescriptionItem
                .Location = New Point(0, NameItem.Bottom)
                .Size = New Size(Width - 40, 80)
                .Title = "Description"
                .Content = "My description goes here"
                .AddAction(My.Resources.EditIconDefault, My.Resources.EditIconHover, AddressOf ChangeDescription)
                AddHandler .ContentChanged, AddressOf ContentChanged
            End With
            CategoryItem = New CategoryComponentViewItem(PropertiesList.ListContainer)
            With CategoryItem
                .Location = New Point(0, DescriptionItem.Bottom)
                .Size = New Size(Width - 40, 80)
                .Title = "Category"
                .Content = "No schedule selected"
                '.AddAction(My.Resources.EditIconDefault, My.Resources.EditIconHover, AddressOf ChangeDescription)
                '.AddAction(My.Resources.EditIconDefault, My.Resources.EditIconHover, AddressOf ChangeDescription)
                'AddHandler .ContentChanged, AddressOf ContentChanged ' TODO: Remove
                AddHandler .DragStarted, AddressOf CategoryItem_DragStarted
                AddHandler .DragEventHappened, AddressOf CategoryItem_DragEventHappened
            End With
            Dim CancelAcceptButtonsSize As New Size(2 * 64, 64)
            Dim CancelAcceptButtonsRect As New Rectangle(New Point((Width - CancelAcceptButtonsSize.Width) \ 2, Height - CancelAcceptButtonsSize.Height), CancelAcceptButtonsSize)
            With CancelButton
                .Size = New Size(64, 64)
                .BackgroundImage = My.Resources.CancelChangesIconDefault
                .Location = CancelAcceptButtonsRect.Location
                .Parent = Me
                .Cursor = Cursors.Hand
                AddHandler .MouseEnter, AddressOf Button_MouseEnter
                AddHandler .MouseLeave, AddressOf Button_MouseLeave
                AddHandler .Click, AddressOf Button_Click
            End With
            With SaveButton
                .Size = New Size(64, 64)
                .BackgroundImage = My.Resources.AcceptChangesIconDefault
                .Location = New Point(CancelAcceptButtonsRect.Right - 64, CancelAcceptButtonsRect.Top)
                .Cursor = Cursors.Hand
                .Parent = Me
                AddHandler .MouseEnter, AddressOf Button_MouseEnter
                AddHandler .MouseLeave, AddressOf Button_MouseLeave
                AddHandler .Click, AddressOf Button_Click
            End With
            PropertiesListContainer = New AutoSizedContainer
            With PropertiesList
                With .Items
                    .Add(NameItem)
                    .Add(DescriptionItem)
                    .Add(CategoryItem)
                End With
                .Padding = New Padding(0)
                .Location = New Point(0, PropertiesListContainer.HeaderHeight)
            End With
            With PropertiesListContainer
                .HeaderText = "properties"
                .InnerControl = PropertiesList
                .Padding = New Padding(0, 0, 4, 0)
                .BorderWidth = 0
                .BackColor = ColorHelper.Multiply(ApplicationBackColor, 0.7)
                .Parent = Me
                .SetTotalSize(New Size(Width, 252 + .HeaderHeight))
                .AddHeaderControl({My.Resources.DeleteIconDefault, My.Resources.DeleteIconHover, My.Resources.DeleteIconPress}, AddressOf DeleteSchedule_Clicked)
            End With
            RoutineListContainer = New AutoSizedContainer
            With RoutineListContainer
                .Top = PropertiesListContainer.Bottom
                .BackColor = BackColor
                .HeaderText = "routines in this schedule"
                .InnerControl = RoutinesInSchedule
                .Padding = New Padding(0)
                .BorderWidth = 0
                .SetTotalSize(New Size(Width, SaveButton.Top - .Top))
                .Parent = Me
                .AddHeaderControl({My.Resources.AddIconDefault, My.Resources.AddIconHover, My.Resources.AddIconPress}, AddressOf AddRoutine)
            End With
            With RoutinePicker
                .Parent = Me
                .Location = RoutineListContainer.Location
                .Size = New Size(Width, Height - .Top)
                .BackColor = BackColor
                .BringToFront()
                .Hide()
            End With
            CurrentSchedule = Nothing
            Me.Parent = Parent
        End Sub
        Private Sub DeleteSchedule_Clicked(Sender As Object, e As EventArgs)
            If CurrentSchedule.Routines.Count = 0 OrElse MsgBox("Are you sure you want to delete this schedule?", MsgBoxStyle.OkCancel, "Confirm deletion of schedule") = MsgBoxResult.Ok Then
                Dim OldDirectory As String = CurrentSchedule.GetFullFilePath
                Dim NewDirectory As String = ApplicationPath & "\Data\" & GarbageBinName & "\Schedules\" & CurrentSchedule.GetFileNameFromName
                AsyncFileReader.Queue.AddOperation(AsyncFileReader.Queue.FileOperation.MoveFile(OldDirectory, NewDirectory, True), AddressOf MoveToBinFinished, Nothing)
            End If
        End Sub
        Private Sub MoveToBinFinished(e As FileOperationEventArgs)
            Dim OriginalPath As String = DirectCast(e.Arguments("OriginalPath"), String)
            Dim OriginalName As String = IO.Path.GetFileNameWithoutExtension(OriginalPath)
            If e.ErrorOccurred Then
                MsgBox("An unexpected error occurred (code: no time, deadline tomorrow).")
            Else
                Loader.RemoveSchedule(OriginalName)
                OnScheduleDeleted(EventArgs.Empty)
            End If
        End Sub
        Protected Sub OnScheduleDeleted(e As EventArgs)
            RaiseEvent ScheduleDeleted(Me, e)
            CurrentSchedule = Nothing
        End Sub
        Private Sub CategoryItem_DragStarted(Sender As Object, e As MouseEventArgs)
            SchedulesTab.NavigateBack()
        End Sub
        Private Sub CategoryItem_DragEventHappened(Sender As Object, e As QueryContinueDragEventArgs)
            Select Case e.Action
                Case DragAction.Cancel, DragAction.Drop
                    SchedulesTab.ShowItemsInCategoryList()
                Case Else
                    MsgBox("ELSE!")
            End Select
        End Sub
        Private Sub ContentChanged(Sender As Object, e As EventArgs)
            ChangesMade = True
        End Sub
        Private Function ValidateNameChange(Sender As Object, e As ValidateEditEventArgs) As ValidateEditEventArgs
            If DirectCast(e.OldContent, String) = DirectCast(e.NewContent, String) Then
                e.Cancel = True
            ElseIf Loader.LookupSchedule(DirectCast(e.NewContent, String)) IsNot Nothing Then
                e.Valid = False
            End If
            Return e
        End Function
        Private Sub RemoveClicked(Sender As Object, e As EventArgs)
            'If MsgBox("Are you sure you want to remove this routine from this schedule?" & vbNewLine & "This will not delete the routine object.", MsgBoxStyle.YesNo, "Confirm removal") = MsgBoxResult.Yes Then
            Dim SenderItem As RoutinesInScheduleListItem = DirectCast(Sender, RoutinesInScheduleListItem)
            'If Not CurrentSchedule.Routines.Contains(SenderItem.Value.Information) Then
            '    MsgBox("An unexpected error occurred (code 5)" & vbNewLine & "It is recommended that you do not save any changes.")
            'End If
            RoutinesInSchedule.Remove(SenderItem)
            RemoveHandler SenderItem.RemoveClicked, AddressOf RemoveClicked
            RemoveHandler SenderItem.EditClicked, AddressOf EditClicked
            ChangesMade = True
            'End If
        End Sub
        Private Sub NothingSelectedScreen_Paint(Sender As Object, e As PaintEventArgs) Handles NothingSelectedScreen.Paint
            Dim TitleRects() As Rectangle = Globals.GetNothingSelectedRects(NothingSelectedScreen.Size)
            TextRenderer.DrawText(e.Graphics, TitleText, NothingSelectedTitleFont, TitleRects(0), Color.White)
            TextRenderer.DrawText(e.Graphics, SubTitleText, NothingSelectedSubTitleFont, TitleRects(1), Color.White)
        End Sub
        Private Sub EditClicked(Sender As Object, e As EventArgs)
            If VerifyDiscard() Then
                Dim SenderItem As RoutinesInScheduleListItem = DirectCast(Sender, RoutinesInScheduleListItem)
                RoutinesTab.RequestView(SenderItem.Value.Information.Name)
                WindowControl.ShowTab(3)
            End If
        End Sub
        Private Sub AddRoutine(Sender As Object, e As MouseEventArgs)
            RoutinesInSchedule.Hide()
            RoutinePicker.SetDirectory()
            RoutinePicker.Show()
        End Sub
        Public Sub RefreshLists()
            RoutinePicker.RefreshLists()
        End Sub
        Private Sub RoutineList_ItemActiveChanged(Sender As Object, e As ListItemActiveChangedEventArgs) Handles RoutinesInSchedule.ItemActiveChanged
            'Dim SenderItem As RoutinesInScheduleListItem = DirectCast(e.SenderItem, RoutinesInScheduleListItem)
            'Dim RoutineInformationToChange As RoutineInformation = varCurrentSchedule.Routines(SenderItem.Index)
            'RoutineInformationToChange.Active = e.Data.Active
            ChangesMade = True
        End Sub
        Private Sub Button_MouseEnter(Sender As Object, e As EventArgs)
            Dim SenderButton As PictureBox = DirectCast(Sender, PictureBox)
            If ReferenceEquals(SenderButton, CancelButton) Then
                SenderButton.BackgroundImage = My.Resources.CancelChangesIconHover
            Else
                SenderButton.BackgroundImage = My.Resources.AcceptChangesIconHover
            End If
        End Sub
        Private Sub Button_MouseLeave(Sender As Object, e As EventArgs)
            Dim SenderButton As PictureBox = DirectCast(Sender, PictureBox)
            If ReferenceEquals(SenderButton, CancelButton) Then
                SenderButton.BackgroundImage = My.Resources.CancelChangesIconDefault
            Else
                SenderButton.BackgroundImage = My.Resources.AcceptChangesIconDefault
            End If
        End Sub
        Public Function VerifyDiscard() As Boolean
            If ChangesMade Then
                If MessageBox.Show("Any changes made to this schedule will be lost. If you want to save these changes, click 'Cancel'", "Unsaved changes have been made", MessageBoxButtons.OKCancel, MessageBoxIcon.Exclamation, MessageBoxDefaultButton.Button2) = DialogResult.Cancel Then Return False
            End If
            Return True
        End Function
        Private Sub Button_Click(Sender As Object, e As EventArgs)
            Dim SenderButton As PictureBox = DirectCast(Sender, PictureBox)
            Dim OldName As String = CurrentSchedule.Name
            If ReferenceEquals(SenderButton, CancelButton) Then
                If VerifyDiscard() Then CurrentSchedule = Loader.LookupSchedule(OldName)
            Else
                Dim OldDirectory As String = CurrentSchedule.GetFullFilePath
                Dim NewName As String = NameItem.Content
                With varCurrentSchedule
                    .Name = NameItem.Content
                    .Description = DescriptionItem.Content
                    .CategoryName = CategoryItem.Content
                    .Routines.Clear()
                    Dim iLast As Integer = RoutinesInSchedule.Items.Count - 1
                    If iLast >= 0 Then
                        For i As Integer = 0 To iLast
                            With RoutinesInSchedule.Items(i)
                                'varCurrentSchedule.Routines.Add(New RoutineInformation(.Name, 1, RoutineInformation.RoutineDateInterval.Day, .IsActive))
                                varCurrentSchedule.Routines.Add(.Value.Information.Copy)
                            End With
                        Next
                    End If
                End With
                Dim NewFullPath As String = varCurrentSchedule.GetFullFilePath
                If NewFullPath <> OldDirectory Then
                    AsyncFileReader.Queue.AddOperation(AsyncFileReader.Queue.FileOperation.MoveFile(OldDirectory, CurrentSchedule.GetFullFilePath, False), AddressOf MoveFinished, Nothing)
                    AsyncFileReader.Queue.AddOperation(AsyncFileReader.Queue.FileOperation.UpdateScheduleInformationInTemplates(OldName, NewName), AddressOf ScheduleInformationInTemplatesUpdated, Nothing)
                Else
                    AsyncFileReader.Queue.AddOperation(AsyncFileReader.Queue.FileOperation.OverwriteFile(NewFullPath, CurrentSchedule.OutputFileContents), AddressOf OverwriteFinished, Nothing)
                End If
            End If
        End Sub
        Private Sub ScheduleInformationInTemplatesUpdated(e As FileOperationEventArgs)
            If e.ErrorOccurred Then
                MsgBox("(Finished) Errors occurred: " & vbNewLine & e.Exception.Message)
            Else
                Dim ModifiedTemplates() As String = DirectCast(e.Result, String())
            End If
        End Sub
        Private Sub MoveFinished(Result As FileOperationEventArgs)
            If Result.ErrorOccurred Then
                MsgBox("Error occurred: " & Result.Exception.Message & vbNewLine & DirectCast(Result.Result, String))
                ' TODO: Log error
            Else
                AsyncFileReader.Queue.AddOperation(AsyncFileReader.Queue.FileOperation.OverwriteFile(varCurrentSchedule.GetFullFilePath, varCurrentSchedule.OutputFileContents), AddressOf OverwriteFinished, Nothing)
            End If
        End Sub
        Private Sub OverwriteFinished(Result As FileOperationEventArgs)
            If Result.ErrorOccurred Then
                MsgBox("Overwrite error: " & Result.Exception.Message)
                ' TODO: Log error
            Else
                RaiseEvent ChangesSaved(Me, New CurrentComponentChangedEventArgs(varCurrentSchedule))
                CurrentSchedule = varCurrentSchedule ' TODO: Replace with "refresh" method
            End If
        End Sub
        Private Sub ChangeName(Args As Object)
            NameItem.EditContent()
        End Sub
        Private Sub OpenFileLocation(Args As Object)
            Process.Start("explorer.exe", "/select," & varCurrentSchedule.GetFullFilePath)
        End Sub
        Private Sub ChangeDescription(Args As Object)
            DescriptionItem.EditContent()
        End Sub
        Protected Overridable Sub OnScheduleChanged(e As CurrentComponentChangedEventArgs)
            Invalidate()
            RaiseEvent ScheduleChanged(Me, e)
        End Sub
        Public Overloads Sub ViewSchedule(ScheduleFile As String, Optional ByVal SurelyLoaded As Boolean = False, Optional ByVal ForceReload As Boolean = False)
            Dim Match As ScheduleEntry = Loader.LookupSchedule(FormatConverter.ExtractFileName(ScheduleFile, False))
            If Not ForceReload Then
                If Match IsNot Nothing Then
                    CurrentSchedule = Match
                ElseIf SurelyLoaded Then
                    Throw New Exception("Could not find the schedule " & FormatConverter.ExtractFileName(ScheduleFile, False) & ". It may not be loaded yet.")
                Else
                    AsyncFileReader.Queue.AddOperation(AsyncFileReader.Queue.FileOperation.ReadLines(ScheduleFile), AddressOf ScheduleLinesLoaded, Nothing)
                    'varLoadingGraphics.Spin(50, 10)
                End If
            Else
                CurrentSchedule = Nothing
                If Match IsNot Nothing Then
                    Loader.RemoveSchedule(Match.Name)
                    AsyncFileReader.Queue.AddOperation(AsyncFileReader.Queue.FileOperation.ReadLines(ScheduleFile), AddressOf ScheduleLinesLoaded, Nothing)
                    'varLoadingGraphics.Spin(50, 10)
                ElseIf SurelyLoaded Then
                    Throw New Exception("Could not find the schedule " & FormatConverter.ExtractFileName(ScheduleFile, False) & ". It may not be loaded yet.")
                Else
                    AsyncFileReader.Queue.AddOperation(AsyncFileReader.Queue.FileOperation.ReadLines(ScheduleFile), AddressOf ScheduleLinesLoaded, Nothing)
                    'varLoadingGraphics.Spin(50, 10)
                End If
            End If
        End Sub
        Private Sub ScheduleLinesLoaded(e As FileOperationEventArgs)
            'varLoadingGraphics.StopSpin()
            Dim Path As String = DirectCast(e.Arguments("Path"), String)
            Loader.AddSchedule(ScheduleEntry.CreateFromLines(DirectCast(e.Result, String()), Path))
            ViewSchedule(Path, True)
        End Sub
    End Class

    Public Class TemplateBrowser
        Inherits Tab
        Private WithEvents TemplateList As New ScrollableList(Of ScrollableListLabel)
        Private TemplateListContainer As New AutoSizedContainer
        Private WithEvents Viewer As TemplateViewer
        'Private CurrentX As Integer = 0, LastX As Integer = 20
        'Private Stage As Integer = 0
        'Private SC As SynchronizationContext = SynchronizationContext.Current
        Private TemplateNameToSelectAfterReload As String = Nothing
        'Private WithEvents SlideTimer As New Timers.Timer(1000 / 60)
        Public Sub New(Parent As MultiTabWindow)
            MyBase.New(Parent)
            'SlideTimer.AutoReset = False
            With TemplateListContainer
                .HeaderText = "templates"
                .Location = New Point(0, My.Resources.Header.Height)
                .InnerControl = TemplateList
                .Parent = Me
                .SetTotalSize(New Size(TemplateList.Width, Height - My.Resources.Header.Height))
                .AddHeaderControl(New Image() {My.Resources.ReloadIconDefault, My.Resources.ReloadIconHover, My.Resources.ReloadIconPress}, AddressOf ReloadIcon_Clicked)
                .AddHeaderControl(New Image() {My.Resources.AddIconDefault, My.Resources.AddIconHover, My.Resources.AddIconPress}, AddressOf AddIcon_Clicked)
            End With
            Viewer = New TemplateViewer(New Rectangle(New Point(TemplateListContainer.Right, My.Resources.Header.Height), New Size(Width - TemplateListContainer.Right, Height - My.Resources.Header.Height)), Me)
            Viewer.BringToFront()
            ReloadList()
        End Sub
        Public Function VerifyViewerDiscard() As Boolean
            Return Viewer.VerifyDiscard
        End Function
        Private Sub ReloadIcon_Clicked(Sender As Object, e As MouseEventArgs)
            If Viewer.VerifyDiscard Then
                If Viewer.CurrentTemplate IsNot Nothing Then TemplateNameToSelectAfterReload = Viewer.CurrentTemplate.Name
                ReloadList()
            End If
        End Sub
        Private Sub AddIcon_Clicked(Sender As Object, e As MouseEventArgs)
            If Viewer.VerifyDiscard Then AddNew()
        End Sub
        Private Sub AddNew()
            AsyncFileReader.Queue.AddOperation(AsyncFileReader.Queue.FileOperation.CreateFile(TemplateEntry.BaseDirectory, "New Template.txt", True, FormatConverter.GetBytesFromString("//meta" & vbNewLine & "$meta: %desc%==This template does not have a description yet && %category%==User" & vbNewLine & "//schedules" & vbNewLine), TemplateEntry.BaseDirectory), AddressOf AddNew_Finished, Nothing)
        End Sub
        Private Sub ReloadList()
            Viewer.ChangesMade = False
            AsyncFileReader.Queue.AddOperation(AsyncFileReader.Queue.FileOperation.ListFiles(TemplateEntry.BaseDirectory), AddressOf ReloadFinished, Nothing)
        End Sub
        Private Sub Viewer_ChangesSaved(Sender As Object, e As EventArgs) Handles Viewer.ChangesSaved, Viewer.TemplateDeleted
            ReloadList()
        End Sub
        Private Sub AddNew_Finished(Result As FileOperationEventArgs)
            If Not Result.ErrorOccurred Then
                TemplateNameToSelectAfterReload = IO.Path.GetFileNameWithoutExtension(DirectCast(Result.Result, String))
            Else
                MsgBox("An unexpected error occurred (code 3)")
            End If
            ReloadList()
        End Sub
        Public Sub ReloadFinished(e As FileOperationEventArgs)
            Dim Files() As String = DirectCast(e.Result, String())
            'Dim ObjArray(Files.Count - 1) As Object
            TemplateList.Clear()
            Dim iLast As Integer = Files.Count - 1
            If iLast >= 0 Then
                For i As Integer = 0 To iLast
                    'ObjArray(i) = FormatConverter.ExtractFileName(Files(i), False)
                    Dim TemplateName As String = IO.Path.GetFileNameWithoutExtension(Files(i))
                    'If Loader.LookupTemplate(TemplateName) Is Nothing Then Loader.AddTemplate(Files(i))
                    Dim NewItem As New ScrollableListLabel With {.Value = New ScrollableListLabelValue(TemplateName)}
                    If ActiveTemplate IsNot Nothing AndAlso TemplateName = ActiveTemplate.Name Then NewItem.IsActive = True
                    TemplateList.Add(NewItem)
                Next
            End If
            'TemplateList.CreateItems(ObjArray)
            If TemplateNameToSelectAfterReload = Nothing AndAlso Viewer.CurrentTemplate IsNot Nothing Then TemplateNameToSelectAfterReload = Viewer.CurrentTemplate.Name
            For Each Item As ScrollableListLabel In TemplateList.Items
                If Item.Value.Text = TemplateNameToSelectAfterReload Then
                    Item.IsSelected = True
                    Exit For
                End If
            Next
            Viewer.RefreshLists()
            TemplateNameToSelectAfterReload = Nothing
        End Sub
        Protected Overrides Sub OnTabShown(e As EventArgs)
            MyBase.OnTabShown(e)
            Header.CurrentDirectiry = {HeaderControl.Views.Overview, HeaderControl.Views.TemplateBrowser}
            Viewer.CurrentTemplate = Viewer.CurrentTemplate
        End Sub
        Protected Overrides Sub OnSizeChanged(e As EventArgs)
            MyBase.OnSizeChanged(e)
            If TemplateListContainer IsNot Nothing Then
                With TemplateListContainer
                    .SetTotalSize(New Size(.Width, Height - My.Resources.Header.Height))
                End With
            End If
        End Sub
        Private Sub TemplateSelected(Sender As Object, e As ListItemSelectionChangedEventArgs) Handles TemplateList.ItemSelectionChanged
            If e.Data.Selected Then
                Dim SenderItem As ScrollableListLabel = DirectCast(e.SenderItem, ScrollableListLabel)
                If Viewer.VerifyDiscard Then
                    TemplateList.DeselectAll(SenderItem.Index)
                    Viewer.ViewTemplate(TemplateEntry.BaseDirectory & "\" & SenderItem.Value.Text & ".txt")
                Else
                    SenderItem.IsSelected = False
                End If
            End If
        End Sub
        Private Sub TemplateActive(Sender As Object, e As ListItemActiveChangedEventArgs) Handles TemplateList.ItemActiveChanged
            If e.Data.Active Then
                Dim SenderItem As ScrollableListLabel = DirectCast(e.SenderItem, ScrollableListLabel)
                TemplateList.SetAllActive(False, SenderItem.Index)
                SetActiveTemplate(Loader.LookupTemplate(SenderItem.Value.Text))
            End If
        End Sub
    End Class
    Public Class TemplateViewer
        Inherits Control
        Private varCurrentTemplate As TemplateEntry = Nothing
        Private varPadding As New Padding(10)
        Private varUseCompatibleTextRendering As Boolean = True, varChangesMade As Boolean = False
        Private varLoadingSurface As New PictureBox, CancelButton, SaveButton As New PictureBox
        Private varLoadingGraphics As LoadingGraphics(Of PictureBox)
        Private InnerRect As Rectangle = New Rectangle(New Point(varPadding.Left, varPadding.Top), New Size(Width - varPadding.Left - varPadding.Right, 100))
        Private ElementBrush As SolidBrush
        Private varElementColor As Color = Color.FromArgb(200, 200, 200)
        Private PropertiesListContainer, ScheduleListContainer As AutoSizedContainer
        Private WithEvents PropertiesList As New PropertyList
        Private WithEvents SchedulesInTemplateList As New SchedulesInTemplateList
        Private NameItem, DescriptionItem, CategoryItem As ComponentViewItem
        Private WithEvents ScheduleBrowser As New SchedulePicker
        Public Event TemplateChanged(Sender As Object, e As CurrentComponentChangedEventArgs)
        Public Event TemplateDeleted(Sender As Object, e As EventArgs)
        Public Event ChangesSaved(Sender As Object, e As CurrentComponentChangedEventArgs)
        Private WithEvents NothingSelectedScreen As New PictureBox
        Private Property NothingSelected As Boolean
            Get
                Return (CurrentTemplate Is Nothing)
            End Get
            Set(value As Boolean)
                If value Then
                    NothingSelectedScreen.Show()
                Else
                    NothingSelectedScreen.Hide()
                End If
            End Set
        End Property
        Private Sub PropertiesList_ScrollHandleVisibleChanged(Sender As Object, e As VisibleEventArgs) Handles PropertiesList.ScrollHandleVisibleChanged
            With PropertiesListContainer
                If e.Visible Then
                    .Padding = New Padding(0, 0, 0, 4)
                Else
                    .Padding = New Padding(0, 0, 0, 0)
                End If
                .SetTotalSize(New Size(Width, 3 * 80 + .HeaderHeight))
            End With
        End Sub
        Public Property ChangesMade As Boolean
            Get
                Return varChangesMade
            End Get
            Set(value As Boolean)
                varChangesMade = value
            End Set
        End Property
        Private Sub ScheduleBrowser_ItemsAdded(Sender As Object, e As ItemsAddedEventArgs) Handles ScheduleBrowser.AddClicked
            For Each Item As ChildComponentInformation In e.SelectedItems
                If SchedulesInTemplateList.Items.Find(Function(x As SchedulesInTemplateListItem) As Boolean
                                                          Return (x.Value.Name = Item.Name)
                                                      End Function) IsNot Nothing Then
                    ' Ignore if already in
                Else
                    Dim NewInfo As ScheduleInformation = DirectCast(Item, ScheduleInformation).Copy
                    SchedulesInTemplateList.Add(New SchedulesInTemplateListItem(New SchedulesInTemplateListItemValue(NewInfo)) With {.Index = SchedulesInTemplateList.Items.Count})
                    AddHandler SchedulesInTemplateList.Items.Last.RemoveClicked, AddressOf SchedulesInTemplateList_RemoveClicked
                    AddHandler SchedulesInTemplateList.Items.Last.EditClicked, AddressOf SchedulesInTemplateList_EditClicked
                    ' TODO: Remove handlers before clearing list
                    ChangesMade = True
                End If
            Next
            'CurrentTemplate = CurrentTemplate
            ExitPicker()
        End Sub
        Private Sub ScheduleBrowser_Cancelled(Sender As Object, e As EventArgs) Handles ScheduleBrowser.CancelClicked
            ExitPicker()
        End Sub
        Private Sub ExitPicker()
            ScheduleBrowser.SetDirectory()
            ScheduleBrowser.Hide()
            SchedulesInTemplateList.Show()
        End Sub
        Public Property CurrentTemplate As TemplateEntry
            Get
                Return varCurrentTemplate
            End Get
            Set(value As TemplateEntry)
                varCurrentTemplate = value
                If varCurrentTemplate Is Nothing Then
                    NothingSelected = True
                    OnTemplateChanged(New CurrentComponentChangedEventArgs(varCurrentTemplate))
                Else
                    'For Each Schedule As ScheduleInformation In DirectCast(CurrentTemplate, TemplateEntry).Schedules
                    '    Schedule.MatchingEntryExists = (Loader.LookupSchedule(Schedule.Name) IsNot Nothing)
                    'Next
                    NameItem.Content = varCurrentTemplate.Name
                    NameItem.Info = varCurrentTemplate.GetShortFilePath
                    DescriptionItem.Content = varCurrentTemplate.Description
                    CategoryItem.Content = varCurrentTemplate.CategoryName
                    For Each Item As SchedulesInTemplateListItem In SchedulesInTemplateList.Items
                        RemoveHandler Item.RemoveClicked, AddressOf SchedulesInTemplateList_RemoveClicked
                        RemoveHandler Item.EditClicked, AddressOf SchedulesInTemplateList_EditClicked
                    Next
                    SchedulesInTemplateList.Clear()
                    Dim iLast As Integer = varCurrentTemplate.Schedules.Count - 1
                    If iLast >= 0 Then
                        For i As Integer = 0 To iLast
                            Dim CurrentInfo As ScheduleInformation = varCurrentTemplate.Schedules(i)
                            CurrentInfo.MatchingEntryExists = (Loader.LookupSchedule(CurrentInfo.Name) IsNot Nothing)
                            Dim NewItem As New SchedulesInTemplateListItem(New SchedulesInTemplateListItemValue(CurrentInfo)) With {.Index = i}
                            NewItem.Name = CurrentInfo.Name
                            NewItem.IsActive = CurrentInfo.Active
                            AddHandler NewItem.RemoveClicked, AddressOf SchedulesInTemplateList_RemoveClicked
                            AddHandler NewItem.EditClicked, AddressOf SchedulesInTemplateList_EditClicked
                            SchedulesInTemplateList.Add(NewItem)
                        Next
                    End If
                    OnTemplateChanged(New CurrentComponentChangedEventArgs(varCurrentTemplate))
                    ChangesMade = False
                    NothingSelected = False
                End If
            End Set
        End Property
        Public Function VerifyDiscard() As Boolean
            If ChangesMade Then
                If MessageBox.Show("Any changes made to this template will be lost. If you want to save these changes, click 'Cancel'", "Unsaved changes have been made", MessageBoxButtons.OKCancel, MessageBoxIcon.Exclamation, MessageBoxDefaultButton.Button2) = DialogResult.Cancel Then Return False
            End If
            Return True
        End Function
        Public Shadows Property Parent As TemplateBrowser
            Get
                Return DirectCast(MyBase.Parent, TemplateBrowser)
            End Get
            Set(value As TemplateBrowser)
                MyBase.Parent = value
            End Set
        End Property
        Public Sub New(ByVal Rectangle As Rectangle, ByVal Parent As TemplateBrowser)
            DoubleBuffered = True
            Location = Rectangle.Location
            Size = Rectangle.Size
            BackColor = ColorHelper.Multiply(ApplicationBackColor, 0.7)
            With NothingSelectedScreen
                .Hide()
                .Size = Size
                .BringToFront()
                .BackColor = BackColor
                .Parent = Me
            End With
            With varLoadingSurface
                .Size = New Size(40, 40)
                .Location = New Point((Width - .Width) \ 2, (Height - .Height) \ 2)
                .Parent = Me
                .Hide()
                .SendToBack()
            End With
            varLoadingGraphics = New LoadingGraphics(Of PictureBox)(varLoadingSurface)
            With varLoadingGraphics
                .Stroke = 3
                .Pen.Color = Color.White
            End With
            NameItem = New ComponentViewItem(PropertiesList.ListContainer)
            With NameItem
                .EditErrorMessage = "A template with this name already exists. Please choose a different name."
                .ValidateEditFunction = AddressOf ValidateNameChange
                .Location = Point.Empty
                .Size = New Size(Width - 40, 80)
                .Title = "Name"
                .Content = "No template selected"
                .Info = "C:/test"
                .AddAction(My.Resources.EditIconDefault, My.Resources.EditIconHover, AddressOf ChangeName)
                .AddAction(My.Resources.OpenFileLocationIconDefault, My.Resources.OpenFileLocationIconHover, AddressOf OpenFileLocation)
                AddHandler .ContentChanged, AddressOf ContentChanged
            End With
            DescriptionItem = New ComponentViewItem(PropertiesList.ListContainer)
            With DescriptionItem
                .Location = New Point(0, NameItem.Bottom)
                .Size = New Size(Width - 40, 80)
                .Title = "Description"
                .Content = "My description goes here"
                .AddAction(My.Resources.EditIconDefault, My.Resources.EditIconHover, AddressOf ChangeDescription)
                AddHandler .ContentChanged, AddressOf ContentChanged
            End With
            CategoryItem = New ComponentViewItem(PropertiesList.ListContainer)
            With CategoryItem
                .Location = New Point(0, DescriptionItem.Bottom)
                .Size = New Size(Width - 40, 80)
                .Title = "Category"
                .Content = "No template selected"
                .AddAction(My.Resources.EditIconDefault, My.Resources.EditIconHover, AddressOf ChangeCategory)
                AddHandler .ContentChanged, AddressOf ContentChanged
            End With
            Dim CancelAcceptButtonsSize As New Size(2 * 64, 64)
            Dim CancelAcceptButtonsRect As New Rectangle(New Point((Width - CancelAcceptButtonsSize.Width) \ 2, Height - CancelAcceptButtonsSize.Height), CancelAcceptButtonsSize)
            With CancelButton
                .Size = New Size(64, 64)
                .BackgroundImage = My.Resources.CancelChangesIconDefault
                .Location = CancelAcceptButtonsRect.Location
                .Parent = Me
                .Cursor = Cursors.Hand
                AddHandler .MouseEnter, AddressOf Button_MouseEnter
                AddHandler .MouseLeave, AddressOf Button_MouseLeave
                AddHandler .Click, AddressOf Button_Click
            End With
            With SaveButton
                .Size = New Size(64, 64)
                .BackgroundImage = My.Resources.AcceptChangesIconDefault
                .Location = New Point(CancelAcceptButtonsRect.Right - 64, CancelAcceptButtonsRect.Top)
                .Cursor = Cursors.Hand
                .Parent = Me
                AddHandler .MouseEnter, AddressOf Button_MouseEnter
                AddHandler .MouseLeave, AddressOf Button_MouseLeave
                AddHandler .Click, AddressOf Button_Click
            End With
            PropertiesListContainer = New AutoSizedContainer
            With PropertiesList
                With .Items
                    .Add(NameItem)
                    .Add(DescriptionItem)
                    .Add(CategoryItem)
                End With
                .Padding = New Padding(0)
                .Location = New Point(0, PropertiesListContainer.HeaderHeight)
            End With
            With PropertiesListContainer
                .HeaderText = "properties"
                .InnerControl = PropertiesList
                .Padding = New Padding(0, 0, 4, 0)
                .BorderWidth = 0
                .BackColor = ColorHelper.Multiply(ApplicationBackColor, 0.7)
                .Parent = Me
                .SetTotalSize(New Size(Width, 252 + .HeaderHeight))
                .AddHeaderControl({My.Resources.DeleteIconDefault, My.Resources.DeleteIconHover, My.Resources.DeleteIconPress}, AddressOf DeleteTemplate_Clicked)
            End With
            ScheduleListContainer = New AutoSizedContainer
            With ScheduleListContainer
                .Top = PropertiesListContainer.Bottom
                .BackColor = BackColor
                .HeaderText = "schedules in this template"
                .InnerControl = SchedulesInTemplateList
                .Padding = New Padding(0)
                .BorderWidth = 0
                .SetTotalSize(New Size(Width, SaveButton.Top - .Top))
                .Parent = Me
                .AddHeaderControl({My.Resources.AddIconDefault, My.Resources.AddIconHover, My.Resources.AddIconPress}, AddressOf AddSchedule)
            End With
            With ScheduleBrowser
                .Parent = Me
                .BaseDirectory = "Schedules"
                .Location = ScheduleListContainer.Location
                .Size = New Size(Width, Height - .Top)
                .BackColor = BackColor
                .BringToFront()
                .Hide()
            End With
            CurrentTemplate = Nothing
            Me.Parent = Parent
        End Sub
        Private Sub DeleteTemplate_Clicked(Sender As Object, e As EventArgs)
            If CurrentTemplate.Schedules.Count = 0 OrElse MsgBox("Are you sure you want to delete this template?" & vbNewLine & "This will not delete the schedules it contains.", MsgBoxStyle.OkCancel, "Confirm deletion of template") = MsgBoxResult.Ok Then DeleteTemplate(CurrentTemplate.GetFullFilePath)
        End Sub
        Private Sub DeleteTemplate(Path As String)
            AsyncFileReader.Queue.AddOperation(AsyncFileReader.Queue.FileOperation.DeleteFile(Path), AddressOf OnTemplateDeleted, Nothing)
        End Sub
        Private Sub OnTemplateDeleted(e As FileOperationEventArgs)
            If e.ErrorOccurred Then
                Throw e.Exception
            Else
                Loader.AllTemplates.Remove(CurrentTemplate)
                CurrentTemplate = Nothing
                RaiseEvent TemplateDeleted(Me, EventArgs.Empty)
            End If
        End Sub
        Private Sub ContentChanged(Sender As Object, e As EventArgs)
            ChangesMade = True
        End Sub
        Private Function ValidateNameChange(Sender As Object, e As ValidateEditEventArgs) As ValidateEditEventArgs
            If DirectCast(e.OldContent, String) = DirectCast(e.NewContent, String) Then
                e.Cancel = True
            ElseIf Loader.LookupTemplate(DirectCast(e.NewContent, String)) IsNot Nothing Then
                e.Valid = False
            End If
            Return e
        End Function
        Private Sub SchedulesInTemplateList_RemoveClicked(Sender As Object, e As EventArgs)
            'If MsgBox("Are you sure you want to remove this schedule from this template?" & vbNewLine & "This will not delete the schedule object.", MsgBoxStyle.YesNo, "Confirm removal") = MsgBoxResult.Yes Then
            Dim SenderItem As SchedulesInTemplateListItem = DirectCast(Sender, SchedulesInTemplateListItem)
            'If Not CurrentTemplate.Schedules.Contains(SenderItem.Value.Information) Then
            '    MsgBox("An unexpected error occurred (code 0)" & vbNewLine & "It is recommended that you do not save any changes.")
            'End If
            SchedulesInTemplateList.Remove(SenderItem)
            RemoveHandler SenderItem.RemoveClicked, AddressOf SchedulesInTemplateList_RemoveClicked
            RemoveHandler SenderItem.EditClicked, AddressOf SchedulesInTemplateList_EditClicked
            ChangesMade = True
            'End If
        End Sub
        Private Sub SchedulesInTemplateList_EditClicked(Sender As Object, e As EventArgs)
            If VerifyDiscard() Then
                Dim SenderItem As SchedulesInTemplateListItem = DirectCast(Sender, SchedulesInTemplateListItem)
                SchedulesTab.RequestView(SenderItem.Value.Information.Name)
                WindowControl.ShowTab(2)
                CurrentTemplate = Loader.LookupTemplate(varCurrentTemplate.Name)
                'CurrentTemplate = Nothing
            End If
        End Sub
        Private Sub AddSchedule(Sender As Object, e As MouseEventArgs)
            SchedulesInTemplateList.Hide()
            ScheduleBrowser.SetDirectory()
            ScheduleBrowser.Show()
        End Sub
        Public Sub RefreshLists()
            ScheduleBrowser.RefreshLists()
        End Sub
        Private Sub NothingSelectedScreen_Paint(Sender As Object, e As PaintEventArgs) Handles NothingSelectedScreen.Paint
            Dim TitleRects() As Rectangle = Globals.GetNothingSelectedRects(NothingSelectedScreen.Size)
            TextRenderer.DrawText(e.Graphics, TitleText, NothingSelectedTitleFont, TitleRects(0), Color.White)
            TextRenderer.DrawText(e.Graphics, SubTitleText, NothingSelectedSubTitleFont, TitleRects(1), Color.White)
        End Sub
        Private Sub SchedulesList_ItemActiveChanged(Sender As Object, e As ListItemActiveChangedEventArgs) Handles SchedulesInTemplateList.ItemActiveChanged
            'Dim SenderItem As SchedulesInTemplateListItem = DirectCast(e.SenderItem, SchedulesInTemplateListItem)
            'Dim ScheduleInformationToChange As ScheduleInformation = varCurrentTemplate.Schedules(SenderItem.Index)
            'ScheduleInformationToChange.Active = e.Data.Active
            ChangesMade = True
        End Sub
        'Private Sub SchedulesList_ItemSelectedChanged(Sender As Object, e As ListItemSelectionChangedEventArgs) Handles SchedulesInTemplateList.ItemSelectionChanged
        '    'Dim SenderItem As SchedulesInTemplateListItem = DirectCast(e.SenderItem, SchedulesInTemplateListItem)
        '    'Dim ScheduleInformationToChange As ScheduleInformation = varCurrentTemplate.Schedules(SenderItem.Index)
        '    'ScheduleInformationToChange.Active = e.Data.Active
        '    ChangesMade = True
        'End Sub

        Private Sub Button_MouseEnter(Sender As Object, e As EventArgs)
            Dim SenderButton As PictureBox = DirectCast(Sender, PictureBox)
            If ReferenceEquals(SenderButton, CancelButton) Then
                SenderButton.BackgroundImage = My.Resources.CancelChangesIconHover
            Else
                SenderButton.BackgroundImage = My.Resources.AcceptChangesIconHover
            End If
        End Sub
        Private Sub Button_MouseLeave(Sender As Object, e As EventArgs)
            Dim SenderButton As PictureBox = DirectCast(Sender, PictureBox)
            If ReferenceEquals(SenderButton, CancelButton) Then
                SenderButton.BackgroundImage = My.Resources.CancelChangesIconDefault
            Else
                SenderButton.BackgroundImage = My.Resources.AcceptChangesIconDefault
            End If
        End Sub
        Private Sub Button_Click(Sender As Object, e As EventArgs)
            Dim SenderButton As PictureBox = DirectCast(Sender, PictureBox)
            If ReferenceEquals(SenderButton, CancelButton) Then
                If VerifyDiscard() Then CurrentTemplate = Loader.LookupTemplate(varCurrentTemplate.Name)
            Else
                Dim OldDirectory As String = varCurrentTemplate.GetFullFilePath
                With varCurrentTemplate
                    .Name = NameItem.Content
                    .Description = DescriptionItem.Content
                    .CategoryName = CategoryItem.Content
                    .Schedules.Clear()
                    Dim iLast As Integer = SchedulesInTemplateList.Items.Count - 1
                    If iLast >= 0 Then
                        For i As Integer = 0 To iLast
                            With SchedulesInTemplateList.Items(i)
                                'varCurrentTemplate.Schedules.Add(.Value.Information)
                                varCurrentTemplate.Schedules.Add(New ScheduleInformation(.Name, .IsActive) With {.MatchingEntryExists = (Loader.LookupSchedule(.Name) IsNot Nothing)})
                            End With
                        Next
                    End If
                    If varCurrentTemplate.GetFullFilePath <> OldDirectory Then ' TODO: Use Rename file operation instead
                        AsyncFileReader.Queue.AddOperation(AsyncFileReader.Queue.FileOperation.MoveFile(OldDirectory, .GetFullFilePath, False), AddressOf MoveFinished, Nothing)
                    Else
                        AsyncFileReader.Queue.AddOperation(AsyncFileReader.Queue.FileOperation.OverwriteFile(.GetFullFilePath, .OutputFileContents), AddressOf OverwriteFinished, Nothing)
                    End If
                End With
            End If
        End Sub
        Private Sub MoveFinished(Result As FileOperationEventArgs)
            If Result.ErrorOccurred Then
                MsgBox("An unexpected error occurred (code 4)")
            Else
                AsyncFileReader.Queue.AddOperation(AsyncFileReader.Queue.FileOperation.OverwriteFile(varCurrentTemplate.GetFullFilePath, varCurrentTemplate.OutputFileContents), AddressOf OverwriteFinished, Nothing)
            End If
        End Sub
        Private Sub OverwriteFinished(Result As FileOperationEventArgs)
            ChangesMade = False
            If Result.ErrorOccurred Then
                MsgBox("Overwrite error: " & Result.Exception.Message)
                ' TODO: Log error
            Else
                RaiseEvent ChangesSaved(Me, New CurrentComponentChangedEventArgs(varCurrentTemplate))
            End If
        End Sub
        Private Sub ChangeName(Args As Object)
            NameItem.EditContent()
        End Sub
        Private Sub OpenFileLocation(Args As Object)
            Process.Start("explorer.exe", "/select," & varCurrentTemplate.GetFullFilePath)
        End Sub
        Private Sub ChangeDescription(Args As Object)
            DescriptionItem.EditContent()
        End Sub
        Private Sub ChangeCategory(Args As Object)
            CategoryItem.EditContent()
        End Sub
        Protected Overridable Sub OnTemplateChanged(e As CurrentComponentChangedEventArgs)
            RaiseEvent TemplateChanged(Me, e)
        End Sub
        Public Overloads Sub ViewTemplate(TemplateFile As String, Optional ByVal SurelyLoaded As Boolean = False)
            Dim Match As TemplateEntry = Loader.LookupTemplate(TemplateEntry.ExtractFromPath(TemplateFile))
            If Match IsNot Nothing Then
                CurrentTemplate = Match
            ElseIf SurelyLoaded Then
                Throw New Exception("Could not load the specified template.")
            Else
                AsyncFileReader.Queue.AddOperation(AsyncFileReader.Queue.FileOperation.ReadLines(TemplateFile), AddressOf TemplateLinesLoaded, Nothing)
                varLoadingGraphics.Spin(50, 10)
            End If
        End Sub
        Private Sub TemplateLinesLoaded(e As FileOperationEventArgs)
            varLoadingGraphics.StopSpin()
            Dim Path As String = DirectCast(e.Arguments("Path"), String)
            Loader.AddTemplate(TemplateEntry.CreateFromLines(DirectCast(e.Result, String()), Path))
            ViewTemplate(Path, True)
        End Sub
    End Class
    Public Class ValidateEditEventArgs
        Public Valid, Cancel As Boolean
        Public InvalidMessage As String
        Public OldContent, NewContent As Object
        Public Sub New(OldContent As Object, NewContent As Object, Valid As Boolean, Cancel As Boolean, Optional InvalidMessage As String = Nothing)
            Me.Valid = Valid
            Me.Cancel = Cancel
            Me.InvalidMessage = InvalidMessage
            Me.OldContent = OldContent
            Me.NewContent = NewContent
        End Sub
    End Class
    Public Class ComponentViewItem
        ' TODO: Inherit ScrollableListItem
        Inherits ScrollableListItem
        Private varTitleRect, varContentRect, varInfoRect As Rectangle
        Private varTitle, varContent, varInfo As String
        Private varForeColor As Color = Color.White
        Private varIconsAreaColor As Color = Color.FromArgb(200, 200, 200)
        Private varIconSize As New Size(20, 16)
        Private IconsAreaWidth As Integer = 80, varLineSpacing As Integer = 5, X As Integer = 30, varGoalX As Integer = 30, CoverWidth As Integer = IconsAreaWidth, CoverInitialWidth As Integer = IconsAreaWidth, CoverGoalWidth As Integer = 0, SeparatorThickness As Integer = 4
        Private IconsInitialLeft(), IconsGoalLeft() As Integer
        Private IconsRect As Rectangle
        Private Actions As New List(Of ControlButton)
        Private varIsHovering As Boolean
        Private SC As SynchronizationContext = SynchronizationContext.Current
        Protected GradientBrush As Drawing2D.LinearGradientBrush
        Protected OpacityBrush As New SolidBrush(BackColor)
        'Private PatternBrush As New Drawing2D.HatchBrush(Drawing2D.HatchStyle.WideUpwardDiagonal, Color.FromArgb(35, 117, 171), Color.FromArgb(28, 92, 136))
        Private LinePen As New Pen(Color.FromArgb(34, 97, 149))
        Private HoveredAction As ControlButton = Nothing
        Private WithEvents ActionsFadeTimer As New System.Timers.Timer(1000 / 50)
        Public Event TitleChanged(Sender As Object, e As EventArgs)
        Public Event ContentChanged(Sender As Object, e As EventArgs)
        Private IsEditing As Boolean = False
        Private varEditErrorMessage As String = "No error message specified"
        Private varValidateEditFunction As Func(Of Object, ValidateEditEventArgs, ValidateEditEventArgs) = Nothing
        Public Property EditErrorMessage As String
            Get
                Return varEditErrorMessage
            End Get
            Set(value As String)
                varEditErrorMessage = value
            End Set
        End Property
        Public Property ValidateEditFunction As Func(Of Object, ValidateEditEventArgs, ValidateEditEventArgs)
            Get
                Return varValidateEditFunction
            End Get
            Set(value As Func(Of Object, ValidateEditEventArgs, ValidateEditEventArgs))
                varValidateEditFunction = value
            End Set
        End Property
        Public Property IsHovering As Boolean
            Get
                Return varIsHovering
            End Get
            Set(value As Boolean)
                varIsHovering = value
                X = 0
                ' TODO: Set initial and goal
                If Not varIsHovering Then
                    If Actions.Count > 0 Then
                        Dim Option1 As Integer
                        If Actions.Count > 1 Then
                            Option1 = Actions(0).Left - Actions(1).Left
                        Else
                            Option1 = Actions(0).Width
                        End If
                        Dim Option2 As Integer = IconsRect.Width - CoverWidth
                        If Option2 > Option1 Then
                            varGoalX = Option1
                        Else
                            varGoalX = Option2
                        End If
                        If varGoalX = 0 Then varGoalX = 1
                        CoverInitialWidth = CoverWidth
                        CoverGoalWidth = IconsRect.Width
                        Dim iLast As Integer = Actions.Count - 1
                        For i As Integer = 0 To iLast
                            IconsInitialLeft(i) = Actions(i).Left
                            IconsGoalLeft(i) = Actions(0).Left
                        Next
                        ActionsFadeTimer.Start()
                    End If
                Else
                    If Actions.Count > 1 Then
                        Dim DefaultIconsLeft As Integer = GetDefaultLeft()
                        varGoalX = varIconSize.Width
                        CoverInitialWidth = CoverWidth
                        CoverGoalWidth = 0
                        Dim iLast As Integer = Actions.Count - 1
                        For i As Integer = 0 To iLast
                            IconsInitialLeft(i) = Actions(i).Left
                            IconsGoalLeft(i) = DefaultIconsLeft - i * varIconSize.Width
                        Next
                        ActionsFadeTimer.Start()
                    ElseIf Actions.Count > 0 Then
                        Dim DefaultIconsLeft As Integer = IconsRect.Left + (IconsRect.Width - Actions(0).Width) \ 2
                        varGoalX = varIconSize.Width
                        CoverInitialWidth = CoverWidth
                        CoverGoalWidth = 0
                        IconsInitialLeft(0) = Actions(0).Left
                        IconsGoalLeft(0) = DefaultIconsLeft
                        ActionsFadeTimer.Start()
                    End If
                End If
            End Set
        End Property
        Public Property Title As String
            Get
                Return varTitle
            End Get
            Set(value As String)
                varTitle = value
                OnTitleChanged(EventArgs.Empty)
            End Set
        End Property
        Public Property Content As String
            Get
                Return varContent
            End Get
            Set(value As String)
                varContent = value
                OnContentChanged(EventArgs.Empty)
            End Set
        End Property
        Public Property Info As String
            Get
                Return varInfo
            End Get
            Set(value As String)
                varInfo = value
            End Set
        End Property
        Protected Shadows Property Parent As IListElementFontProvider
            Get
                Return DirectCast(MyBase.Parent, IListElementFontProvider)
            End Get
            Set(value As IListElementFontProvider)
                MyBase.Parent = DirectCast(value, ListContainerControl)
            End Set
        End Property
        Public Overrides ReadOnly Property DefaultValue As ScrollableListItemValue
            Get
                Return New PropertyValueInfo("No template selected", "Test", "")
            End Get
        End Property
        Public Sub New(Parent As IListElementFontProvider)
            MyBase.Parent = DirectCast(Parent, ListContainerControl)
            Initialize()
        End Sub
        Public Sub New()
            Initialize()
        End Sub
        Private Sub Initialize()
            ResizeRedraw = True
            DoubleBuffered = True
            Padding = New Padding(5)
            BackColor = ColorHelper.Multiply(ApplicationBackColor, 0.9)
            ActionsFadeTimer.AutoReset = False
            LinePen.DashPattern = {1, 2}
        End Sub
        Private Function GetActionsWidth() As Integer
            Select Case Actions.Count
                Case > 0
                    Return (Actions.Count - 1) * varIconSize.Width + Actions(0).Width
                Case Else
                    Return 0
            End Select
        End Function
        Public Sub EditContent()
            If Not IsEditing Then
                IsEditing = True
                OnEditContent(EventArgs.Empty)
            End If
        End Sub
        Protected Overridable Sub OnEditContent(e As EventArgs) ' Todo: make EditContentEventArgs
            Dim EditBox As New TextBox
            With EditBox
                AddHandler .KeyDown, AddressOf EditBox_KeyDown
                AddHandler .TextChanged, AddressOf EditBox_TextChanged
                AddHandler .LostFocus, AddressOf EditBox_LostFocus
                AddHandler .Leave, AddressOf EditBox_LostFocus
                .Multiline = True
                .Cursor = Cursors.IBeam
                .Location = varContentRect.Location
                .Size = New Size(IconsRect.Left - Padding.Left - 10, varContentRect.Height)
                .Padding = New Padding(2)
                .BorderStyle = BorderStyle.None
                .BackColor = ColorHelper.Multiply(ApplicationBackColor, 0.4)
                .Parent = Me
                .Text = varContent
                .Font = Parent.StatusLabelFont(ListElementFontSize.Regular)
                .ForeColor = varForeColor
                .Focus()
            End With
            ' TODO: Don't assume that a textbox must be created
        End Sub
        Private Sub EditBox_KeyDown(Sender As Object, e As KeyEventArgs)
            Select Case e.KeyCode
                Case Keys.Enter
                    FinishEdit(DirectCast(Sender, TextBox))
                    e.Handled = True
                Case Keys.Escape
                    Dim SenderTextBox As TextBox = DirectCast(Sender, TextBox)
                    SenderTextBox.Text = varContent
                    FinishEdit(SenderTextBox)
            End Select
        End Sub
        Private Sub EditBox_TextChanged(Sender As Object, e As EventArgs)
            Dim SenderTextBox As TextBox = DirectCast(Sender, TextBox)
            If TextRenderer.MeasureText(SenderTextBox.Text, SenderTextBox.Font).Width > SenderTextBox.Width Then
                SenderTextBox.Text = SenderTextBox.Text.Remove(SenderTextBox.Text.Length - 1, 1)
                SenderTextBox.SelectionStart = SenderTextBox.Text.Length
            End If
        End Sub
        Private Sub EditBox_LostFocus(Sender As Object, e As EventArgs)
            FinishEdit(DirectCast(Sender, TextBox))
        End Sub
        Protected Overridable Sub FinishEdit(Info As Object)
            Dim Sender As TextBox = DirectCast(Info, TextBox)
            RemoveHandler Sender.KeyDown, AddressOf EditBox_KeyDown
            RemoveHandler Sender.TextChanged, AddressOf EditBox_TextChanged
            RemoveHandler Sender.LostFocus, AddressOf EditBox_LostFocus
            RemoveHandler Sender.Leave, AddressOf EditBox_LostFocus
            Dim Success As Boolean = True
            Dim Cancel As Boolean = False
            Dim InvalidMessage As String = EditErrorMessage
            Dim Validation As ValidateEditEventArgs = Nothing
            If ValidateEditFunction IsNot Nothing Then
                Validation = ValidateEditFunction.Invoke(Me, New ValidateEditEventArgs(Content, Sender.Text, True, False))
                Success = Validation.Valid
                Cancel = Validation.Cancel
                If Validation.InvalidMessage <> Nothing Then
                    InvalidMessage = Validation.InvalidMessage
                End If
            End If
            If Not Cancel Then
                If Not Success Then
                    MsgBox(InvalidMessage)
                ElseIf Validation IsNot Nothing Then
                    Content = DirectCast(Validation.NewContent, String)
                Else
                    Content = Sender.Text
                End If
            End If
            Sender.Dispose()
            OnEditFinished(EventArgs.Empty)
        End Sub
        Protected Overridable Sub OnEditFinished(e As EventArgs)
            IsEditing = False
            CheckHover()
        End Sub
        Public Sub AddAction(DefaultIcon As Image, HoverIcon As Image, ClickAction As Action(Of Object))
            Dim NewAction As New ControlButton(DefaultIcon, HoverIcon, ClickAction)
            Actions.Add(NewAction)
            Dim iLast As Integer = Actions.Count - 1
            ReDim IconsInitialLeft(iLast)
            ReDim IconsGoalLeft(iLast)
            Dim DefaultIconsLeft As Integer = GetDefaultLeft()
            NewAction.Left = DefaultIconsLeft
            For i As Integer = 0 To iLast
                IconsInitialLeft(i) = DefaultIconsLeft
            Next
            AdjustRectangles(True)
        End Sub
        Private Sub ActionsFadeTimer_Tick(Sender As Object, e As Timers.ElapsedEventArgs) Handles ActionsFadeTimer.Elapsed
            SC.Post(AddressOf NextFadeStep, Nothing)
        End Sub
        Private Sub NextFadeStep(State As Object)
            X += 1
            Dim iLast As Integer = Actions.Count - 1
            For i As Integer = 0 To iLast
                Actions(i).Left = CInt(EaseInOut.GetY(IconsInitialLeft(i), IconsGoalLeft(i), X, varGoalX))
            Next
            CoverWidth = CInt(EaseInOut.GetY(CoverInitialWidth, CoverGoalWidth, X, varGoalX))
            If X < varGoalX Then
                ActionsFadeTimer.Start()
            End If
            CheckHover()
            Invalidate()
        End Sub
        Protected Overridable Sub OnTitleChanged(e As EventArgs)
            RaiseEvent TitleChanged(Me, e)
            AdjustRectangles(True)
        End Sub
        Protected Overridable Sub OnContentChanged(e As EventArgs)
            RaiseEvent ContentChanged(Me, e)
            AdjustRectangles(True)
        End Sub
        Protected Overrides Sub OnMouseMove(e As MouseEventArgs)
            MyBase.OnMouseMove(e)
            CheckHover()
        End Sub
        Protected Overrides Sub OnClick(e As EventArgs)
            MyBase.OnClick(e)
            If HoveredAction IsNot Nothing AndAlso FormatConverter.PointIsInRectangle(PointToClient(MousePosition), New Rectangle(HoveredAction.Location, varIconSize)) Then
                HoveredAction.ClickAction.Invoke(Nothing)
            End If
        End Sub
        Private Sub CheckHover()
            Dim MatchFound As Boolean
            For Each Action As ControlButton In Actions
                If FormatConverter.PointIsInRectangle(PointToClient(MousePosition), New Rectangle(Action.Location, varIconSize)) Then
                    HoveredAction = Action
                    MatchFound = True
                    Exit For
                End If
            Next
            Dim DoInvalidate As Boolean
            If Not MatchFound Then
                If HoveredAction IsNot Nothing Then DoInvalidate = True
                HoveredAction = Nothing
                DoInvalidate = True
                If Not FormatConverter.PointIsInRectangle(PointToClient(MousePosition), New Rectangle(varContentRect.Location, New Size(IconsRect.Left - Padding.Left - 10, varContentRect.Height))) Then
                    Cursor = Cursors.Default
                ElseIf IsEditing Then
                    Cursor = Cursors.IBeam
                End If
            Else
                Cursor = Cursors.Hand
                DoInvalidate = True
            End If
            If DoInvalidate Then Invalidate()
        End Sub
        Protected Overrides Sub OnPaint(e As PaintEventArgs)
            MyBase.OnPaint(e)
            With e.Graphics
                .FillRectangle(GradientBrush, New Rectangle(New Point(0, 0), New Size(Width, Height)))
                TextRenderer.DrawText(e.Graphics, Title, Parent.StaticLabelFont(ListElementFontSize.Large), varTitleRect.Location, varForeColor)
                TextRenderer.DrawText(e.Graphics, Content, Parent.StatusLabelFont(ListElementFontSize.Regular), varContentRect.Location, varForeColor)
                TextRenderer.DrawText(e.Graphics, Info, Parent.EditButtonFont(ListElementFontSize.Small), varInfoRect.Location, varForeColor)
                If Actions.Count > 0 Then
                    OpacityBrush.Color = varIconsAreaColor
                    .FillRectangle(OpacityBrush, IconsRect)
                    Dim OpacityColor As Color = Color.FromArgb(255 - CInt((1 - (CoverWidth / IconsRect.Width)) * 255), varIconsAreaColor)
                    OpacityBrush.Color = OpacityColor
                    For Each Action As ControlButton In Actions
                        Dim IconRectangle As New Rectangle(Action.Location, New Size(16, 16))
                        If HoveredAction IsNot Nothing AndAlso ReferenceEquals(Action, HoveredAction) Then
                            .DrawImageUnscaled(Action.HoverIcon, Action.Location)
                        Else
                            .DrawImageUnscaled(Action.DefaultIcon, Action.Location)
                        End If
                        .FillRectangle(OpacityBrush, IconsRect)
                    Next
                    OpacityBrush.Color = BackColor
                    .FillRectangle(OpacityBrush, New Rectangle(IconsRect.Location, New Size(CoverWidth, Height \ 2)))
                    .FillRectangle(GradientBrush, New Rectangle(New Point(IconsRect.Left, 0), New Size(CoverWidth, Height)))
                End If
                '.FillRectangle(PatternBrush, New Rectangle(New Point(0, Height - SeparatorThickness), New Size(Width - IconsRect.Width + CoverWidth, SeparatorThickness)))
                .DrawLine(LinePen, New Point(0, Height - 1), New Point(Width - IconsRect.Width + CoverWidth, Height - 1))
            End With
        End Sub
        Protected Overrides Sub OnMouseEnter(e As EventArgs)
            MyBase.OnMouseEnter(e)
            IsHovering = True
        End Sub
        Protected Overrides Sub OnMouseLeave(e As EventArgs)
            MyBase.OnMouseLeave(e)
            If Not FormatConverter.PointIsInRectangle(PointToClient(MousePosition), New Rectangle(varContentRect.Location, New Size(IconsRect.Left - Padding.Left - 10, varContentRect.Height))) Then
                IsHovering = False
            End If
        End Sub
        Protected Overrides Sub OnSizeChanged(e As EventArgs)
            MyBase.OnSizeChanged(e)
            If GradientBrush IsNot Nothing Then
                GradientBrush.Dispose()
            End If
            Dim SecondColor As Color = Color.FromArgb(28, 127, 171)
            GradientBrush = New Drawing2D.LinearGradientBrush(Point.Empty, New Point(Width, Height), BackColor, Color.FromArgb(28, 127, 171))
            AdjustRectangles(True)
        End Sub
        Protected Overrides Sub OnParentChanged(e As EventArgs)
            MyBase.OnParentChanged(e)
            AdjustRectangles(True)
        End Sub
        Private Function GetDefaultLeft() As Integer
            Dim ActionsWidth As Integer = GetActionsWidth()
            Dim ActionsRect As New Rectangle(New Point(IconsRect.Left + (IconsRect.Width - ActionsWidth) \ 2, 0), New Size(ActionsWidth, 0))
            Dim DefaultIconsLeft As Integer = ActionsRect.Right - Actions(0).Width
            Return DefaultIconsLeft
        End Function
        Private Sub AdjustRectangles(ByVal Invalidate As Boolean)
            IconsRect = New Rectangle(New Point(Width - IconsAreaWidth, 0), New Size(IconsAreaWidth, Height))
            'CoverInitialWidth = IconsAreaWidth
            'CoverGoalWidth = 0
            If Actions.Count > 0 Then
                If Not varIsHovering Then
                    Dim DefaultIconsLeft As Integer = GetDefaultLeft()
                    For Each Action As ControlButton In Actions
                        Action.Location = New Point(DefaultIconsLeft, (Height - varIconSize.Height) \ 2)
                    Next
                End If
            End If
            If Parent IsNot Nothing Then
                Dim TitleSize As Size = TextRenderer.MeasureText(Title, Parent.StaticLabelFont(ListElementFontSize.Large))
                varTitleRect = New Rectangle(New Point(Padding.Left, Padding.Top), TitleSize)
                Dim ContentSize As Size = TextRenderer.MeasureText(Content, Parent.StatusLabelFont(ListElementFontSize.Regular))
                varContentRect = New Rectangle(New Point(Padding.Left + 2, varTitleRect.Bottom + varLineSpacing), ContentSize)
                Dim InfoSize As Size = TextRenderer.MeasureText(Info, Parent.EditButtonFont(ListElementFontSize.Small))
                varInfoRect = New Rectangle(New Point(Padding.Left + 2, varContentRect.Bottom + 1), InfoSize)
            End If
            If Invalidate Then Me.Invalidate()
        End Sub
        Public Overrides Sub ApplyArguments(Arguments As Object)
            Throw New NotImplementedException()
        End Sub
        Private Class ControlButton
            Private varIconDefault, varIconHover As Image
            Private varLocation As Point
            Private varClickAction As Action(Of Object)
            Public ReadOnly Property Width As Integer
                Get
                    If varIconDefault IsNot Nothing Then Return varIconDefault.Width
                    Throw New Exception("The width of this control is not known before a default icon is specified.")
                    Return 0
                End Get
            End Property
            Public Property ClickAction As Action(Of Object)
                Get
                    Return varClickAction
                End Get
                Set(value As Action(Of Object))
                    varClickAction = value
                End Set
            End Property
            Public Property Location As Point
                Get
                    Return varLocation
                End Get
                Set(value As Point)
                    varLocation = value
                End Set
            End Property
            Public Property Left As Integer
                Get
                    Return varLocation.X
                End Get
                Set(value As Integer)
                    varLocation.X = value
                End Set
            End Property
            Public Property Top As Integer
                Get
                    Return varLocation.Y
                End Get
                Set(value As Integer)
                    varLocation.Y = value
                End Set
            End Property
            Public Property DefaultIcon As Image
                Get
                    Return varIconDefault
                End Get
                Set(value As Image)
                    varIconDefault = value
                End Set
            End Property
            Public Property HoverIcon As Image
                Get
                    Return varIconHover
                End Get
                Set(value As Image)
                    varIconHover = value
                End Set
            End Property
            Public Sub New(DefaultIcon As Image, HoverIcon As Image, ClickAction As Action(Of Object))
                Me.ClickAction = ClickAction
                Me.DefaultIcon = DefaultIcon
                Me.HoverIcon = HoverIcon
            End Sub
        End Class
    End Class
    Public Class CategoryComponentViewItem
        Inherits ComponentViewItem
        Public Event DragStarted(Sender As Object, e As MouseEventArgs)
        Public Event DragEventHappened(Sender As Object, e As QueryContinueDragEventArgs)
        Private DragRect As New Rectangle(New Point(200, 20), New Size(32, 32))
        Private Images() As Image = {My.Resources.DragIconDefault, My.Resources.DragIconHover, My.Resources.DragIconPress}
        Private IsHoveringDrag As Boolean
        Private IsPressing As Boolean
        Public Sub New(Parent As IListElementFontProvider)
            MyBase.New(Parent)
        End Sub
        Protected Overrides Sub OnMouseMove(e As MouseEventArgs)
            MyBase.OnMouseMove(e)
            Dim IsHovering As Boolean = FormatConverter.PointIsInRectangle(e.Location, DragRect)
            Dim DoInvalidate As Boolean = (IsHovering <> IsHoveringDrag)
            IsHoveringDrag = IsHovering
            If DoInvalidate Then Invalidate()
        End Sub
        Protected Overrides Sub OnSizeChanged(e As EventArgs)
            MyBase.OnSizeChanged(e)
            Dim Margin As Integer = (Height - DragRect.Height) \ 2
            DragRect.Location = New Point(Width - DragRect.Width - Margin, Margin)
        End Sub
        Protected Overrides Sub OnPaint(e As PaintEventArgs)
            MyBase.OnPaint(e)
            With e.Graphics
                If IsPressing Then
                    .DrawImageUnscaled(Images(2), DragRect)
                ElseIf IsHoveringDrag Then
                    .DrawImageUnscaled(Images(1), DragRect)
                Else
                    .DrawImageUnscaled(Images(0), DragRect)
                End If
            End With
        End Sub
        Protected Overrides Sub OnMouseDown(e As MouseEventArgs)
            MyBase.OnMouseDown(e)
            If e.Button = MouseButtons.Left AndAlso IsHoveringDrag Then
                IsPressing = True
                Refresh()
                RaiseEvent DragStarted(Me, e)
                DoDragDrop(Content.Clone, DragDropEffects.Move)
            End If
        End Sub
        Private Sub Me_QueryContinueDrag(ByVal Sender As Object, ByVal e As QueryContinueDragEventArgs) Handles Me.QueryContinueDrag
            If e.Action <> DragAction.Continue Then
                IsPressing = False
                IsHoveringDrag = FormatConverter.PointIsInRectangle(PointToClient(MousePosition), DragRect)
                Invalidate()
                RaiseEvent DragEventHappened(Me, e)
            End If
        End Sub
    End Class
    Public Class SchedulesInTemplateList
        Inherits ItemsInComponentList(Of ScheduleInformation, SchedulesInTemplateListItemValue, SchedulesInTemplateListItem)
    End Class
    Public Class SchedulesInTemplateListItem
        Inherits ItemsInComponentListItem(Of ScheduleInformation, SchedulesInTemplateListItemValue)
        Public Sub New()
            Throw New Exception("Cannot use the parameterless constructor for this class.")
        End Sub
        Public Sub New(Value As SchedulesInTemplateListItemValue)
            Me.Value = Value
        End Sub
        Public Shadows Property Value As SchedulesInTemplateListItemValue
            Get
                Return DirectCast(MyBase.Value, SchedulesInTemplateListItemValue)
            End Get
            Set(value As SchedulesInTemplateListItemValue)
                'Me.IsActive = value.Active
                MyBase.Value = value
            End Set
        End Property
        Public Overrides ReadOnly Property DefaultValue As ScrollableListItemValue
            Get
                Return New SchedulesInTemplateListItemValue(New ScheduleInformation("Default name", False))
            End Get
        End Property
    End Class
    Public Class SchedulesInTemplateListItemValue
        Inherits ItemsInComponentListItemValue(Of ScheduleInformation)
        Public Sub New(Information As ScheduleInformation)
            MyBase.New(Information.Copy)
        End Sub
        Public Sub New()
            MyBase.New(New ScheduleInformation("Default name", False))
        End Sub
    End Class
    Public MustInherit Class ItemsInComponentList(Of InformationType As ChildComponentInformation, ValueType As {New, ItemsInComponentListItemValue(Of InformationType)}, ItemType As {New, ItemsInComponentListItem(Of InformationType, ValueType)})
        Inherits ScrollableList(Of ItemType)
        Public Sub New()
            BackColor = ColorHelper.Multiply(ApplicationBackColor, 0.7)
            With ScrollHandle
                ScrollHandle.RoundedEnds = False
                .Width = 10
            End With
        End Sub
    End Class

    Public MustInherit Class ItemsInComponentListItem(Of InformationType As ChildComponentInformation, ValueType As {New, ItemsInComponentListItemValue(Of InformationType)})
        Inherits ScrollableListItem
        Private varTitleSize As Size
        Protected FillBrush As New SolidBrush(Color.White)
        Protected CheckPath As New Drawing2D.GraphicsPath
        Private varIsHovering, varIsHoveringCheck As Boolean
        Private varTitle As String
        Private varButtonList As New List(Of ItemButton)
        'Private varActive As Boolean
        Private varCheckBoxSize As New Size(15, 15)
        Private varDisableEditOnNoMatch As Boolean = True
        Protected GradientBrush As Drawing2D.LinearGradientBrush
        Public Event EditClicked(Sender As Object, e As EventArgs)
        Public Event RemoveClicked(Sender As Object, e As EventArgs)
        Public Sub New()
            Height = 31
            'Dim OffsetPathX As Integer = 11
            'Dim OffsetPathY As Integer = 12
            'CheckPath.AddPolygon(New Point() {New Point(0 + OffsetPathX, 2 + OffsetPathY), New Point(1 + OffsetPathX, 1 + OffsetPathY), New Point(4 + OffsetPathX, 4 + OffsetPathY), New Point(8 + OffsetPathX, 0 + OffsetPathY), New Point(9 + OffsetPathX, 1 + OffsetPathY), New Point(4 + OffsetPathX, 6 + OffsetPathY)})
            CheckPath.AddPolygon(GetCheckPolygon(True))
            AddButton(New ItemButton(My.Resources.RemoveIconDefault, My.Resources.RemoveIconHover, Me, AddressOf OnRemoveClicked, "Remove"))
            AddButton(New ItemButton(My.Resources.EditIconDefault, My.Resources.EditIconHover, Me, AddressOf OnEditClicked, "Edit"))
            BackColor = ColorHelper.Multiply(ApplicationBackColor, 0.9)
            Value = DirectCast(DefaultValue, ItemsInComponentListItemValue(Of InformationType))
        End Sub
        Protected ReadOnly Property CheckBoxSize As Size
            Get
                Return varCheckBoxSize
            End Get
        End Property
        Protected Overridable Function GetCheckPolygon(ByVal AutoOffset As Boolean, Optional ManualOffset As Point = Nothing) As Point()
            Dim OffsetPathX, OffsetPathY As Integer
            If AutoOffset Then
                Dim CheckRect As Rectangle = GetCheckBoxRectangle()
                OffsetPathX = CheckRect.X + 3
                OffsetPathY = CheckRect.Y + 4
            Else
                OffsetPathX = ManualOffset.X
                OffsetPathY = ManualOffset.Y
            End If
            Return New Point() {New Point(0 + OffsetPathX, 2 + OffsetPathY), New Point(1 + OffsetPathX, 1 + OffsetPathY), New Point(4 + OffsetPathX, 4 + OffsetPathY), New Point(8 + OffsetPathX, 0 + OffsetPathY), New Point(9 + OffsetPathX, 1 + OffsetPathY), New Point(4 + OffsetPathX, 6 + OffsetPathY)}
        End Function
        Private Sub OnEditClicked(Sender As ItemButton) ' TODO: Raise "ButtonClicked" event instead and select case name
            RaiseEvent EditClicked(Me, EventArgs.Empty)
        End Sub
        Private Sub OnRemoveClicked(Sender As ItemButton)
            RaiseEvent RemoveClicked(Me, EventArgs.Empty)
        End Sub
        Public Property DisableEditOnNoMatch As Boolean
            Get
                Return varDisableEditOnNoMatch
            End Get
            Set(value As Boolean)
                varDisableEditOnNoMatch = value
                Invalidate()
            End Set
        End Property
        Protected Friend ReadOnly Property ButtonList As List(Of ItemButton)
            Get
                Return varButtonList
            End Get
        End Property
        Public Property Data As Object
            Get
                Return Value.Data
            End Get
            Set(value As Object)
                Me.Value.Data = value
            End Set
        End Property
        Private Property IsHovering As Boolean
            Get
                Return varIsHovering
            End Get
            Set(value As Boolean)
                varIsHovering = value
                If Not value Then
                    For Each Button As ItemButton In ButtonList
                        Button.IsHovering = False
                    Next
                End If
                Invalidate()
            End Set
        End Property
        Private Property IsHoveringCheck As Boolean
            Get
                Return varIsHoveringCheck
            End Get
            Set(value As Boolean)
                varIsHoveringCheck = value
                Invalidate()
            End Set
        End Property
        Public Shadows Property Value As ItemsInComponentListItemValue(Of InformationType)
            Get
                Return DirectCast(MyBase.Value, ItemsInComponentListItemValue(Of InformationType))
            End Get
            Set(value As ItemsInComponentListItemValue(Of InformationType))
                MyBase.Value = value
                IsActive = value.Active
            End Set
        End Property
        Public Shadows Property Name As String
            Get
                Return Value.Information.Name
            End Get
            Set(value As String)
                Me.Value.Information.Name = value
                OnValueChanged(New ValueChangedEventArgs(Me.Value))
            End Set
        End Property
        Public Overrides Property IsActive As Boolean
            Get
                Return MyBase.IsActive
            End Get
            Set(value As Boolean)
                MyBase.IsActive = value
                Me.Value.Information.Active = value
                'OnValueChanged(New ValueChangedEventArgs(Me.Value))
                'OnActiveChanged(New ItemActiveEventArgs(Me.Value.Information.Active))
            End Set
        End Property
        Public Overrides ReadOnly Property DefaultValue As ScrollableListItemValue
            Get
                Throw New NotImplementedException
            End Get
        End Property
        Public Overrides Sub ApplyArguments(Arguments As Object)
            Throw New NotImplementedException()
        End Sub
        Private Sub AddButton(Button As ItemButton)
            With Button
                .Location = New Point(Width - .Size.Width * (1 + ButtonList.Count), 0)
                .Index = ButtonList.Count
            End With
            ButtonList.Add(Button)
        End Sub
        Protected Overrides Sub OnSizeChanged(e As EventArgs)
            MyBase.OnSizeChanged(e)
            Dim iLast As Integer = ButtonList.Count - 1
            For i As Integer = 0 To iLast
                With ButtonList(i)
                    .Location = New Point(Width - .Size.Width * (i + 1), 0)
                End With
                If GradientBrush IsNot Nothing Then GradientBrush.Dispose()
                GradientBrush = New Drawing2D.LinearGradientBrush(Point.Empty, New Point(Width, Height), BackColor, Color.FromArgb(28, 127, 171))
            Next
            If CheckPath IsNot Nothing Then
                CheckPath.Reset()
                Dim CheckRect As Rectangle = GetCheckBoxRectangle()
                Dim OffsetPathX As Integer = CheckRect.X + 3
                Dim OffsetPathY As Integer = CheckRect.Y + 4
                CheckPath.AddPolygon(New Point() {New Point(0 + OffsetPathX, 2 + OffsetPathY), New Point(1 + OffsetPathX, 1 + OffsetPathY), New Point(4 + OffsetPathX, 4 + OffsetPathY), New Point(8 + OffsetPathX, 0 + OffsetPathY), New Point(9 + OffsetPathX, 1 + OffsetPathY), New Point(4 + OffsetPathX, 6 + OffsetPathY)})
            End If
        End Sub
        Protected Overrides Sub OnValueChanged(e As ValueChangedEventArgs)
            MyBase.OnValueChanged(e)
            If Parent IsNot Nothing Then ' "lj" to get full possible height, don't know if this matters
                varTitleSize = TextRenderer.MeasureText(Name & "lj", Parent.StaticLabelFont(ListElementFontSize.Regular))
            End If
            Invalidate()
        End Sub
        Protected Overrides Sub OnMouseEnter(e As EventArgs)
            MyBase.OnMouseEnter(e)
            IsHovering = True
        End Sub
        Protected Overrides Sub OnMouseLeave(e As EventArgs)
            MyBase.OnMouseLeave(e)
            IsHovering = False
        End Sub
        Protected Overrides Sub OnMouseMove(e As MouseEventArgs)
            MyBase.OnMouseMove(e)
            If FormatConverter.PointIsInRectangle(PointToClient(MousePosition), GetCheckBoxRectangle) Then
                IsHoveringCheck = True
            ElseIf IsHoveringCheck = True Then
                IsHoveringCheck = False
            End If
            For Each Button As ItemButton In ButtonList
                If FormatConverter.PointIsInRectangle(PointToClient(MousePosition), Button.Rectangle) Then
                    Button.IsHovering = True
                Else
                    Button.IsHovering = False
                End If
            Next
        End Sub
        Protected Overrides Sub OnMouseUp(e As MouseEventArgs)
            MyBase.OnMouseUp(e)
            If IsHoveringCheck Then
                If Not IsActive Then
                    IsActive = True
                Else
                    IsActive = False
                End If
            Else
                For Each Button As ItemButton In ButtonList
                    If Value.Information.MatchingEntryExists OrElse Button.Name <> "Edit" OrElse Not DisableEditOnNoMatch Then
                        If FormatConverter.PointIsInRectangle(PointToClient(MousePosition), Button.Rectangle) Then
                            Button.ClickAction.Invoke(Button)
                            Exit For
                        End If
                    End If
                Next
            End If
        End Sub
        Protected Overridable Function GetCheckBoxRectangle() As Rectangle
            Dim Margin As Integer = (Height - varCheckBoxSize.Height) \ 2
            Return New Rectangle(New Point(Margin, Margin), varCheckBoxSize)
        End Function
        Protected Overrides Sub OnPaint(pe As PaintEventArgs)
            MyBase.OnPaint(pe)
            If varTitleSize = Nothing Then
                varTitleSize = TextRenderer.MeasureText(pe.Graphics, Name & "lj", Parent.StaticLabelFont(ListElementFontSize.Regular))
            End If
            With pe.Graphics
                .SetClip(ClientRectangle)
                .SmoothingMode = Drawing2D.SmoothingMode.AntiAlias
                .FillRectangle(GradientBrush, ClientRectangle)
                .SmoothingMode = Drawing2D.SmoothingMode.Default
                FillBrush.Color = Color.White
                .FillRectangle(FillBrush, GetCheckBoxRectangle)
                If varIsHoveringCheck AndAlso Not IsActive Then
                    FillBrush.Color = Color.Gray
                    .FillPath(FillBrush, CheckPath)
                ElseIf IsActive Then
                    FillBrush.Color = Color.Black
                    .FillPath(FillBrush, CheckPath)
                End If
                FillBrush.Color = Color.FromArgb(200, 200, 200)
                For Each Icon As ItemButton In ButtonList
                    If Value.Information.MatchingEntryExists OrElse Icon.Name <> "Edit" OrElse Not DisableEditOnNoMatch Then
                        .FillRectangle(FillBrush, Icon.Rectangle)
                        .DrawImageUnscaled(Icon.GetImage, GetButtonDrawRectangle(Icon))
                    End If
                Next
            End With
            If Value.Information.MatchingEntryExists Then
                TextRenderer.DrawText(pe.Graphics, Name, Parent.StaticLabelFont(ListElementFontSize.Regular), GetTextLocation, Color.White)
            Else
                FillBrush.Color = Color.FromArgb(30, Color.OrangeRed)
                pe.Graphics.FillRectangle(FillBrush, ClientRectangle)
                TextRenderer.DrawText(pe.Graphics, Name & " (not found)", Parent.StaticLabelFont(ListElementFontSize.Regular), GetTextLocation, Color.DarkGray)
            End If
        End Sub
        Protected Overridable Function GetTextLocation() As Point
            Return New Point(30, (Height - varTitleSize.Height) \ 2)
        End Function
        Protected Overrides Sub Dispose(disposing As Boolean)
            If CheckPath IsNot Nothing Then CheckPath.Dispose()
            If FillBrush IsNot Nothing Then FillBrush.Dispose()
            If GradientBrush IsNot Nothing Then GradientBrush.Dispose()
            MyBase.Dispose(disposing)
        End Sub
        Private Function GetButtonDrawRectangle(ByVal Button As ItemButton) As Rectangle
            Dim DiffXY() As Integer = {Button.Size.Width - Button.IconDefault.Width, Button.Size.Width - Button.IconDefault.Width}
            Dim Ret As New Rectangle(New Point(Button.Rectangle.X + DiffXY(0) \ 2, Button.Rectangle.Y + DiffXY(1) \ 2), Button.IconDefault.Size)
            Return Ret
        End Function
        Protected Friend Class ItemButton
            Private varIconDefault, varIconHover As Image
            Private varRectangle As Rectangle
            Private varParent As Control
            Private varClickAction As Action(Of ItemButton)
            Private varIsHovering As Boolean
            Private varIndex As Integer = -1
            Private varName As String = "Default"
            Public Sub New(IconDefault As Image, IconHover As Image, Parent As Control, ClickAction As Action(Of ItemButton), Name As String)
                With Me
                    .Name = Name
                    .IconDefault = IconDefault
                    .IconHover = IconHover
                    .ClickAction = ClickAction
                    .Parent = Parent
                    .Size = New Size(Parent.Height, Parent.Height)
                End With
            End Sub
            Public Property Name As String
                Get
                    Return varName
                End Get
                Set(value As String)
                    varName = value
                End Set
            End Property
            Public Property Index As Integer
                Get
                    Return varIndex
                End Get
                Set(value As Integer)
                    varIndex = value
                End Set
            End Property
            Public Property ClickAction As Action(Of ItemButton)
                Get
                    Return varClickAction
                End Get
                Set(value As Action(Of ItemButton))
                    varClickAction = value
                End Set
            End Property
            Public Property IsHovering As Boolean
                Get
                    Return varIsHovering
                End Get
                Set(value As Boolean)
                    Dim DoInvalidate As Boolean
                    If varIsHovering <> value Then
                        DoInvalidate = True
                    End If
                    varIsHovering = value
                    If DoInvalidate Then
                        Parent.Invalidate()
                    End If
                End Set
            End Property ' TODO: Remove parent property; invalidate when the property is set from parent instead
            Public Function GetImage() As Image
                If IsHovering Then
                    Return varIconHover
                End If
                Return varIconDefault
            End Function
            Public Property IconDefault As Image
                Get
                    Return varIconDefault
                End Get
                Set(value As Image)
                    varIconDefault = value
                End Set
            End Property
            Public Property IconHover As Image
                Get
                    Return varIconHover
                End Get
                Set(value As Image)
                    varIconHover = value
                End Set
            End Property
            Public Property Location As Point
                Get
                    Return varRectangle.Location
                End Get
                Set(value As Point)
                    varRectangle.Location = value
                End Set
            End Property
            Public Property Size As Size
                Get
                    Return varRectangle.Size
                End Get
                Set(value As Size)
                    varRectangle.Size = value
                End Set
            End Property
            Public ReadOnly Property Rectangle As Rectangle
                Get
                    Return varRectangle
                End Get
            End Property
            Public Property Parent As Control
                Get
                    Return varParent
                End Get
                Set(value As Control)
                    varParent = value
                End Set
            End Property
        End Class
    End Class
    Public MustInherit Class ItemsInComponentListItemValue(Of InformationType As ChildComponentInformation)
        Inherits ScrollableListItemValue
        Private varInformation As InformationType
        Public Property Information As InformationType
            Get
                Return varInformation
            End Get
            Set(value As InformationType)
                varInformation = value
            End Set
        End Property
        Public Property Name As String
            Get
                Return varInformation.Name
            End Get
            Set(value As String)
                varInformation.Name = value
            End Set
        End Property
        Public Property Active As Boolean
            Get
                Return varInformation.Active
            End Get
            Set(value As Boolean)
                varInformation.Active = value
            End Set
        End Property
        Public Sub New(ByVal InformationCopy As InformationType, Optional Data As Object = Nothing)
            MyBase.New(Data)
            varInformation = InformationCopy
        End Sub
    End Class
    Public Class RoutinesInScheduleList
        Inherits ItemsInComponentList(Of RoutineInformation, RoutinesInScheduleListItemValue, RoutinesInScheduleListItem)
        Public Event ItemValueChanged(Sender As Object, e As RoutineItemValueChangedEventArgs)
        Public Overrides Sub Add(Item As RoutinesInScheduleListItem)
            MyBase.Add(Item)
            AddHandler Item.ValueChanged, AddressOf OnItemValueChanged
        End Sub
        Protected Overrides Sub RemoveItemHandlers(ByRef Item As RoutinesInScheduleListItem)
            MyBase.RemoveItemHandlers(Item)
            RemoveHandler Item.ValueChanged, AddressOf OnItemValueChanged
        End Sub
        Protected Sub OnItemValueChanged(Sender As Object, e As ValueChangedEventArgs)
            RaiseEvent ItemValueChanged(Me, New RoutineItemValueChangedEventArgs(DirectCast(Sender, RoutinesInScheduleListItem), e))
        End Sub
    End Class
    Public Class RoutineItemValueChangedEventArgs
        Public SenderItem As RoutinesInScheduleListItem
        Public Trigger As ValueChangedEventArgs
        Public Sub New(SenderItem As RoutinesInScheduleListItem, Trigger As ValueChangedEventArgs)
            Me.SenderItem = SenderItem
            Me.Trigger = Trigger
        End Sub
    End Class
    Public Class RoutinesInScheduleListItem
        Inherits ItemsInComponentListItem(Of RoutineInformation, RoutinesInScheduleListItemValue)
        Private Shared ControlFont As Font = RalewayFonts.GetFont(RalewayFontCollection.FontVariant.Light, 9.5, FontStyle.Regular)
        Private ControlRects(1) As Rectangle
        Private ArrowRects(3) As Rectangle
        Private HoveredArrowIndex As Integer = -1
        Private ControlRectSize() As Size = {New Size(100, 21), New Size(70, 21)}
        Private Shadows FillBrush As New SolidBrush(Color.Red)
        Private PathPen As New Pen(Color.White)
        Private HoverColor As Color = Color.FromArgb(228, 228, 228)
        Private ArrowLeftPath(1)(), ArrowRightPath(1)() As Point
        Private GradientTop, GradientBottom As Integer
        Private DefaultColor As Color = ColorHelper.Multiply(ApplicationBackColor, 0.9)
        Private BGColor As Color = ColorHelper.Multiply(ApplicationBackColor, 0.7)
        Private Shadows GradientBrush As Drawing2D.LinearGradientBrush
        Private MarginRight As Integer = 65
        Public Sub New(Value As RoutinesInScheduleListItemValue)
            Me.Value = Value
            UpdateRects()
        End Sub
        Protected Overrides Sub OnSizeChanged(e As EventArgs)
            MyBase.OnSizeChanged(e)
            If ControlRects IsNot Nothing Then
                UpdateRects()
            End If
        End Sub
        Private Sub UpdateRects()
            GradientTop = (Height - ControlRectSize(0).Height) \ 2
            GradientBottom = (Height - ControlRectSize(0).Height) \ 2 + ControlRectSize(0).Height
            For i As Integer = 0 To ControlRects.Count - 1
                If i = 0 Then
                    ControlRects(i) = New Rectangle(New Point(Width - (ControlRectSize(i).Width + 5) - MarginRight, GradientTop), ControlRectSize(0))
                Else
                    ControlRects(i) = New Rectangle(New Point(ControlRects(0).Left - (ControlRectSize(i).Width + 5), GradientTop), ControlRectSize(i))
                End If
                ArrowRects(2 * i) = New Rectangle(ControlRects(i).Location, New Size(ControlRectSize(i).Height, ControlRectSize(i).Height))
                ArrowRects(2 * i + 1) = New Rectangle(New Point(ControlRects(i).Right - ControlRectSize(i).Height, ControlRects(i).Top), New Size(ControlRectSize(i).Height, ControlRectSize(i).Height))
                Dim dxyL As Point = ArrowRects(2 * i).Location
                Dim dxyR As Point = ArrowRects(2 * i + 1).Location
                ArrowLeftPath(i) = New Point() {New Point(7, 10), New Point(11, 6), New Point(12, 7), New Point(9, 10), New Point(12, 13), New Point(11, 14)}
                ArrowRightPath(i) = New Point() {New Point(13, 10), New Point(9, 14), New Point(8, 13), New Point(11, 10), New Point(8, 7), New Point(9, 6)}
                Dim nLast As Integer = ArrowLeftPath(i).Count - 1
                For n As Integer = 0 To nLast
                    ArrowLeftPath(i)(n).Offset(dxyL)
                    ArrowRightPath(i)(n).Offset(dxyR)
                Next
            Next
            If GradientBrush IsNot Nothing Then GradientBrush.Dispose()
            GradientBrush = New Drawing2D.LinearGradientBrush(New Point(0, GradientTop - 1), New Point(0, GradientBottom + 1), DefaultColor, ColorHelper.Multiply(DefaultColor, 0.9))
            Invalidate()
        End Sub
        Protected Overrides Sub OnMouseMove(e As MouseEventArgs)
            MyBase.OnMouseMove(e)
            If Value.Information.MatchingEntryExists Then
                Dim MatchIndex As Integer = -1
                For i As Integer = 0 To ArrowRects.Count - 1
                    If FormatConverter.PointIsInRectangle(e.Location, ArrowRects(i)) Then
                        MatchIndex = i
                        Exit For
                    End If
                Next
                If MatchIndex <> HoveredArrowIndex Then
                    HoveredArrowIndex = MatchIndex
                    Invalidate()
                End If
            End If
        End Sub
        Protected Overrides Sub OnMouseUp(e As MouseEventArgs)
            MyBase.OnMouseUp(e)
            With Value.Information
                If .MatchingEntryExists Then
                    Select Case HoveredArrowIndex
                        Case -1 ' Do Nothing
                        Case 2
                            ' Decrement RunEvery
                            If .RunEvery > 1 Then RunEvery -= 1
                        Case 3
                            RunEvery += 1
                        Case 0
                            Dim CurrentInterval As Integer = .Interval
                            If CurrentInterval > 0 Then CurrentInterval -= 1
                            If CurrentInterval <> Interval Then Interval = CType(CurrentInterval, RoutineInformation.RoutineDateInterval)
                        Case 1
                            Dim CurrentInterval As Integer = .Interval
                            Dim Last As RoutineInformation.RoutineDateInterval = [Enum].GetValues(GetType(RoutineInformation.RoutineDateInterval)).Cast(Of RoutineInformation.RoutineDateInterval).Max
                            If CurrentInterval < Last Then CurrentInterval += 1
                            If CurrentInterval <> Interval Then Interval = CType(CurrentInterval, RoutineInformation.RoutineDateInterval)
                        Case Else
                            Throw New Exception("Index was out of range")
                    End Select
                    Invalidate()
                End If
            End With
        End Sub
        Protected Overrides Sub OnHoveringChanged(e As ItemHoveringEventArgs)
            If Not Hovering Then
                HoveredArrowIndex = -1
            End If
            MyBase.OnHoveringChanged(e)
        End Sub
        Public Sub New()
            Throw New Exception("Cannot use this constructor")
        End Sub
        Public Shadows Property Value As RoutinesInScheduleListItemValue
            Get
                Return DirectCast(MyBase.Value, RoutinesInScheduleListItemValue)
            End Get
            Set(value As RoutinesInScheduleListItemValue)
                MyBase.Value = value
            End Set
        End Property
        Public Overrides ReadOnly Property DefaultValue As ScrollableListItemValue
            Get
                Return New RoutinesInScheduleListItemValue(New RoutineInformation("Empty", 10, RoutineInformation.RoutineDateInterval.Days, False, "Never"))
            End Get
        End Property
        Public Property RunEvery As Integer
            Get
                Return Value.Information.RunEvery
            End Get
            Set(value As Integer)
                Me.Value.Information.RunEvery = value
                OnValueChanged(New ValueChangedEventArgs(Me.Value))
            End Set
        End Property
        Public Property Interval As RoutineInformation.RoutineDateInterval
            Get
                Return Value.Information.Interval
            End Get
            Set(value As RoutineInformation.RoutineDateInterval)
                Me.Value.Information.Interval = value
                OnValueChanged(New ValueChangedEventArgs(Me.Value))
            End Set
        End Property
        Private Function GetCenterRectTextLocation(ByVal Text As String, ByVal CenterIn As Rectangle) As Point
            Dim TextSize As Size = TextRenderer.MeasureText(Text, ControlFont)
            Dim TextLeft As Integer = CenterIn.Left + (CenterIn.Width - TextSize.Width) \ 2
            Dim TextTop As Integer = CenterIn.Top + (CenterIn.Height - TextSize.Height) \ 2
            Return New Point(TextLeft, TextTop)
        End Function
        Protected Overrides Sub OnPaint(pe As PaintEventArgs)
            MyBase.OnPaint(pe)
            If Value.Information.MatchingEntryExists Then
                With pe.Graphics
                    For i As Integer = 0 To ControlRects.Count - 1
                        FillBrush.Color = BGColor
                        .FillRectangle(FillBrush, ControlRects(i))
                        If HoveredArrowIndex = 2 * i Then
                            FillBrush.Color = HoverColor
                            .FillRectangle(FillBrush, ArrowRects(2 * i))
                            FillBrush.Color = ColorHelper.Multiply(HoverColor, 0.7)
                        Else
                            .FillRectangle(GradientBrush, ArrowRects(2 * i))
                            FillBrush.Color = ColorHelper.Multiply(DefaultColor, 0.7)
                        End If
                        .FillPolygon(FillBrush, ArrowLeftPath(i))
                        PathPen.Color = FillBrush.Color
                        .DrawPolygon(PathPen, ArrowLeftPath(i))
                        If HoveredArrowIndex = 2 * i + 1 Then
                            FillBrush.Color = HoverColor
                            .FillRectangle(FillBrush, ArrowRects(2 * i + 1))
                            FillBrush.Color = ColorHelper.Multiply(HoverColor, 0.7)
                        Else
                            .FillRectangle(GradientBrush, ArrowRects(2 * i + 1))
                            FillBrush.Color = ColorHelper.Multiply(DefaultColor, 0.7)
                        End If
                        .FillPolygon(FillBrush, ArrowRightPath(i))
                        PathPen.Color = FillBrush.Color
                        .DrawPolygon(PathPen, ArrowRightPath(i))
                    Next
                End With
                TextRenderer.DrawText(pe.Graphics, CStr(Value.Information.RunEvery), ControlFont, GetCenterRectTextLocation(CStr(Value.Information.RunEvery), ControlRects(1)), Color.White)
                TextRenderer.DrawText(pe.Graphics, Value.Information.Interval.ToString, ControlFont, GetCenterRectTextLocation(Value.Information.Interval.ToString, ControlRects(0)), Color.White)
            End If
        End Sub
        Protected Overrides Sub Dispose(disposing As Boolean)
            If PathPen IsNot Nothing Then PathPen.Dispose()
            If FillBrush IsNot Nothing Then FillBrush.Dispose()
            If GradientBrush IsNot Nothing Then GradientBrush.Dispose()

            MyBase.Dispose(disposing)
        End Sub
    End Class
    Public Class RoutinesInScheduleListItemValue
        Inherits ItemsInComponentListItemValue(Of RoutineInformation)
        Public Sub New(Information As RoutineInformation)
            MyBase.New(Information.Copy)
        End Sub
        Public Sub New()
            MyBase.New(Nothing)
            Throw New NotImplementedException
        End Sub
    End Class
    Public Class TaskItemValueChangedEventArgs
        Public SenderItem As TasksInRoutineListItem
        Public Trigger As ValueChangedEventArgs
        Public Sub New(SenderItem As TasksInRoutineListItem, Trigger As ValueChangedEventArgs)
            Me.SenderItem = SenderItem
            Me.Trigger = Trigger
        End Sub
    End Class
    Public Class TasksInRoutineList
        Inherits ItemsInComponentList(Of TaskInformation, TasksInRoutineListItemValue, TasksInRoutineListItem)
        Public Event ItemValueChanged(Sender As Object, e As TaskItemValueChangedEventArgs)
        Public Overrides Sub Add(Item As TasksInRoutineListItem)
            MyBase.Add(Item)
            AddHandler Item.ValueChanged, AddressOf OnItemValueChanged
        End Sub
        Protected Overrides Sub RemoveItemHandlers(ByRef Item As TasksInRoutineListItem)
            MyBase.RemoveItemHandlers(Item)
            RemoveHandler Item.ValueChanged, AddressOf OnItemValueChanged
        End Sub
        Protected Sub OnItemValueChanged(Sender As Object, e As ValueChangedEventArgs)
            RaiseEvent ItemValueChanged(Me, New TaskItemValueChangedEventArgs(DirectCast(Sender, TasksInRoutineListItem), e))
        End Sub
    End Class
    Public Class TasksInRoutineListItem
        Inherits ItemsInComponentListItem(Of TaskInformation, TasksInRoutineListItemValue)
        Private OpacityBrush As New SolidBrush(Color.FromArgb(40, ColorHelper.Multiply(ApplicationBackColor, 0.5)))
        Private HoveringAsync, HoveringWait, HoveringAdd As Boolean
        Private DenseFont As Font = RalewayFonts.GetFont(RalewayFontCollection.FontVariant.Bold, 8.5, FontStyle.Regular)
        Private AddArgumentRect As Rectangle
        Private LinePen As New Pen(Color.White) With {.DashPattern = {1, 2}}
        Public WithEvents ArgumentsList As New ArgumentsList
        Public Event ArgumentRemoved(Sender As Object, e As ArgumentRemovedEventArgs)
        Public Sub New(Value As TasksInRoutineListItemValue)
            Me.Value = Value
            Height = 130
            ArgumentsList.Parent = Me
            Dim InformationArguments As New List(Of Object)
            For Each Argument As Object In Value.Information.Arguments
                InformationArguments.Add(Argument)
            Next
        End Sub
        Private Sub ArgumentsList_ArgumentRemoved(Sender As Object, e As ArgumentRemovedEventArgs) Handles ArgumentsList.ItemRemoveClicked
            OnArgumentRemoved(e)
        End Sub
        Private Sub OnArgumentRemoved(e As ArgumentRemovedEventArgs)
            Value.Information.Arguments.Remove(e.SenderItem.Value.Argument)
            ArgumentsList.Remove(e.SenderItem)
            OnValueChanged(New ValueChangedEventArgs(Value))
            RaiseEvent ArgumentRemoved(Me, e)
        End Sub
        Public Sub New()
            Throw New Exception("Cannot use this constructor")
        End Sub
        Protected Overrides Function GetCheckBoxRectangle() As Rectangle
            ' Return MyBase.GetCheckBoxRectangle()
            Dim Margin As Integer = (31 - CheckBoxSize.Height) \ 2
            Return New Rectangle(New Point(Margin, Margin), CheckBoxSize)
        End Function
        Private Function GetLeftInnerRect() As Rectangle
            Return New Rectangle(New Point(20, ArgumentsList.Top), New Size(ArgumentsList.Left - (ArgumentsList.Bottom - Height) - 1, ArgumentsList.Height))
        End Function
        Private Function GetRunAsyncCheckRect() As Rectangle
            Dim LeftInnerRect As Rectangle = GetLeftInnerRect()
            Dim Margin As Integer = (31 - CheckBoxSize.Height) \ 2
            Dim TotalHeight As Integer = 2 * CheckBoxSize.Height + Margin
            Dim Top As Integer = LeftInnerRect.Top + (LeftInnerRect.Height - TotalHeight) \ 2
            Dim Left As Integer = LeftInnerRect.Left
            Return New Rectangle(New Point(Left, Top), CheckBoxSize)
        End Function
        Private Function GetWaitCheckRect() As Rectangle
            Dim LeftInnerRect As Rectangle = GetLeftInnerRect()
            Dim Margin As Integer = (31 - CheckBoxSize.Height) \ 2
            Dim TotalHeight As Integer = 2 * CheckBoxSize.Height + Margin
            Dim Top As Integer = LeftInnerRect.Top + (LeftInnerRect.Height - TotalHeight) \ 2 + CheckBoxSize.Height + Margin
            Dim Left As Integer = LeftInnerRect.Left
            Return New Rectangle(New Point(Left, Top), CheckBoxSize)
        End Function
        Protected Overrides Sub OnHoveringChanged(e As ItemHoveringEventArgs)
            If Not Hovering Then
                HoveringAsync = False
                HoveringWait = False
                HoveringAdd = False
            End If
            MyBase.OnHoveringChanged(e)
        End Sub
        Protected Overrides Sub OnSizeChanged(e As EventArgs)
            MyBase.OnSizeChanged(e)
            If ArgumentsList IsNot Nothing Then
                With ArgumentsList
                    .Size = New Size(200, 110 - 31 - 10)
                    .Location = New Point(Width - .Width, 51 + 5)
                End With
                Dim AddArgumentSize As Size = TextRenderer.MeasureText("Add an argument...", DenseFont)
                Dim AddArgumentLocation As New Point(ArgumentsList.Left, 31 + ((ArgumentsList.Top - 31 - AddArgumentSize.Height) \ 2))
                AddArgumentRect = New Rectangle(AddArgumentLocation, AddArgumentSize)
            End If
        End Sub
        Protected Overrides Sub OnValueChanged(e As ValueChangedEventArgs)
            MyBase.OnValueChanged(e)
            If ArgumentsList IsNot Nothing AndAlso Value.Information.Arguments IsNot Nothing Then
                ArgumentsList.Clear()
                For Each Arg As Object In Value.Information.Arguments
                    ArgumentsList.Add(New ArgumentsListItem(ArgumentsList, New ArgumentsListItemValue(Arg)))
                Next
                If Arguments.Contains("$PREVIOUS") AndAlso Not Value.Information.WaitForPrevious Then
                    Value.Information.WaitForPrevious = True
                    OnValueChanged(New ValueChangedEventArgs(Value))
                End If
            End If
        End Sub
        Protected Overrides Function GetTextLocation() As Point
            Dim CheckRect As Rectangle = GetCheckBoxRectangle()
            Dim TextSize As Size = TextRenderer.MeasureText(Value.Name, Parent.StaticLabelFont(ListElementFontSize.Regular))
            Return New Point(CheckRect.Right + (31 - CheckBoxSize.Height) \ 2, (31 - TextSize.Height) \ 2 + 1)
        End Function
        Protected Overrides Sub OnMouseMove(e As MouseEventArgs)
            MyBase.OnMouseMove(e)
            Dim NewAsyncHover As Boolean = False
            Dim NewWaitHover As Boolean = False
            Dim NewAddHover As Boolean = False
            If FormatConverter.PointIsInRectangle(e.Location, GetRunAsyncCheckRect) Then
                NewAsyncHover = True
            ElseIf FormatConverter.PointIsInRectangle(e.Location, GetWaitCheckRect) Then
                NewWaitHover = True
            ElseIf FormatConverter.PointIsInRectangle(e.Location, AddArgumentRect) Then
                NewAddHover = True
                Cursor = Cursors.Hand
            Else
                Cursor = Cursors.Default
            End If
            If HoveringAsync <> NewAsyncHover OrElse NewWaitHover <> HoveringWait OrElse HoveringAdd <> NewAddHover Then
                HoveringAsync = NewAsyncHover
                HoveringWait = NewWaitHover
                HoveringAdd = NewAddHover
                Invalidate()
            End If
        End Sub
        Protected Overrides Sub OnMouseUp(e As MouseEventArgs)
            MyBase.OnMouseUp(e)
            If HoveringAsync Then
                RunAsync = (RunAsync = False)
            ElseIf HoveringWait Then
                WaitForPrevious = (WaitForPrevious = False)
            ElseIf HoveringAdd Then
                Dim NewArgument As String = InputBox("Please enter an argument to be passed to the script when it runs." & vbNewLine & "The default value represents the object returned by the previous script to run.", "Add an argument", "$PREVIOUS")
                If NewArgument <> "" Then
                    AddArgument(NewArgument)
                End If
            End If
        End Sub
        Public Property RunAsync As Boolean
            Get
                Return Value.Information.RunAsync
            End Get
            Set(value As Boolean)
                Me.Value.Information.RunAsync = value
                OnValueChanged(New ValueChangedEventArgs(Me.Value))
            End Set
        End Property
        Public Property WaitForPrevious As Boolean
            Get
                Return Value.Information.WaitForPrevious
            End Get
            Set(value As Boolean)
                Me.Value.Information.WaitForPrevious = value
                OnValueChanged(New ValueChangedEventArgs(Me.Value))
            End Set
        End Property
        Public ReadOnly Property Arguments As Object()
            Get
                Return Value.Information.Arguments.ToArray
            End Get
        End Property
        Public Sub AddArgument(ByVal Argument As Object)
            Value.Information.Arguments.Add(Argument)
            OnValueChanged(New ValueChangedEventArgs(Value))
        End Sub
        Public Sub AddArguments(ByVal Arguments() As Object)
            Value.Information.Arguments.AddRange(Arguments)
            OnValueChanged(New ValueChangedEventArgs(Value))
        End Sub
        Public Shadows Property Value As TasksInRoutineListItemValue
            Get
                Return DirectCast(MyBase.Value, TasksInRoutineListItemValue)
            End Get
            Set(value As TasksInRoutineListItemValue)
                MyBase.Value = value
            End Set
        End Property
        Public Overrides ReadOnly Property DefaultValue As ScrollableListItemValue
            Get
                Return New TasksInRoutineListItemValue(New TaskInformation("Empty", False, True, False, New List(Of Object)))
            End Get
        End Property
        Protected Overrides Sub OnPaint(pe As PaintEventArgs)
            MyBase.OnPaint(pe)
            Dim UpperPart As New Rectangle(Point.Empty, New Size(Width, 31))
            With pe.Graphics
                .ExcludeClip(GetCheckBoxRectangle)
                .ExcludeClip(New Rectangle(New Point(Width - 2 * 31, 0), New Size(2 * 31, UpperPart.Height)))
                .FillRectangle(OpacityBrush, UpperPart)
                .SetClip(ClientRectangle)
                Dim OriginalGradientColors() As Color = GradientBrush.LinearColors
                GradientBrush.LinearColors = New Color() {ColorHelper.Multiply(OriginalGradientColors(0), 0.8), ColorHelper.Multiply(OriginalGradientColors(1), 0.8)}
                Dim ArgumentsLocation As New Point(ArgumentsList.Left - 5, 31)
                Dim ArgumentsRectangle As New Rectangle(ArgumentsLocation, New Size(Width - ArgumentsLocation.X, Height - ArgumentsLocation.Y))
                .FillRectangle(GradientBrush, ArgumentsRectangle)
                Dim LeftRect As New Rectangle(New Point(0, 31), New Size(Width - ArgumentsRectangle.Width - 1, Height - 31))
                .FillRectangle(GradientBrush, LeftRect)
                LinePen.DashStyle = Drawing2D.DashStyle.Solid
                LinePen.Color = ColorHelper.Multiply(GradientBrush.LinearColors(0), 0.95)
                .DrawLine(LinePen, New Point(LeftRect.Width, LeftRect.Top), New Point(LeftRect.Width, LeftRect.Bottom))
                .DrawLine(LinePen, New Point(0, 31), New Point(Width, 31))
                .DrawLine(LinePen, New Point(0, 31), New Point(0, Height))
                .DrawLine(LinePen, New Point(0, Height - 1), New Point(Width, Height - 1))

                LinePen.DashPattern = {1, 2}
                LinePen.Color = Color.White
                FillBrush.Color = ApplicationBackColor
                .FillRectangle(FillBrush, New Rectangle(New Point(5, 56), New Size(LeftRect.Width - 10, LeftRect.Height - 30)))
                GradientBrush.LinearColors = OriginalGradientColors
                TextRenderer.DrawText(pe.Graphics, "Add an argument...", DenseFont, AddArgumentRect, Color.White)
                If HoveringAdd Then .DrawLine(LinePen, New Point(AddArgumentRect.Left + 3, AddArgumentRect.Bottom), New Point(AddArgumentRect.Right - 3, AddArgumentRect.Bottom))
                Dim AsyncCheck As Rectangle = GetRunAsyncCheckRect()
                Dim WaitCheck As Rectangle = GetWaitCheckRect()
                FillBrush.Color = Color.White
                .FillRectangle(FillBrush, AsyncCheck)
                .FillRectangle(FillBrush, WaitCheck)
                If RunAsync Then
                    FillBrush.Color = Color.Black
                    .FillPolygon(FillBrush, GetCheckPolygon(False, New Point(AsyncCheck.Left + 3, AsyncCheck.Top + 4)))
                ElseIf HoveringAsync Then
                    FillBrush.Color = Color.Gray
                    .FillPolygon(FillBrush, GetCheckPolygon(False, New Point(AsyncCheck.Left + 3, AsyncCheck.Top + 4)))
                End If
                If WaitForPrevious AndAlso Not Arguments.Contains("$PREVIOUS") Then
                    FillBrush.Color = Color.Black
                    .FillPolygon(FillBrush, GetCheckPolygon(False, New Point(WaitCheck.Left + 3, WaitCheck.Top + 4)))
                ElseIf HoveringWait OrElse Arguments.Contains("$PREVIOUS") Then
                    FillBrush.Color = Color.Gray
                    .FillPolygon(FillBrush, GetCheckPolygon(False, New Point(WaitCheck.Left + 3, WaitCheck.Top + 4)))
                End If

                Dim Margin As Integer = (31 - CheckBoxSize.Height) \ 2
                Dim TextSize As Size = TextRenderer.MeasureText(pe.Graphics, "RUN ASYNCRHONOUSLY", DenseFont)
                FillBrush.Color = Color.White
                TextRenderer.DrawText(pe.Graphics, "RUN ASYNCHRONOUSLY", DenseFont, New Point(AsyncCheck.Right + 5, AsyncCheck.Top + (AsyncCheck.Height - TextSize.Height) \ 2 + 1), Color.White)
                Dim WaitColor As Color = Color.White
                If Arguments.Contains("$PREVIOUS") Then WaitColor = Color.FromArgb(180, 210, 220)
                'If RunAsync Then WaitColor = Color.White Else WaitColor = Color.FromArgb(180, 210, 220)
                TextRenderer.DrawText(pe.Graphics, "WAIT FOR PREVIOUS", DenseFont, New Point(WaitCheck.Right + 5, WaitCheck.Top + (WaitCheck.Height - TextSize.Height) \ 2 + 1), WaitColor)
                '.DrawString("WAIT FOR PREVIOUS", DenseFont, FillBrush, New Point(WaitCheck.Right + 5, WaitCheck.Top + (WaitCheck.Height - TextSize.Height) \ 2 + 1))

            End With
        End Sub
    End Class
    Public Class TasksInRoutineListItemValue
        Inherits ItemsInComponentListItemValue(Of TaskInformation)
        Public Sub New(Information As TaskInformation)
            MyBase.New(Information.Copy)
        End Sub
        Public Sub New()
            MyBase.New(Nothing)
            Throw New NotImplementedException
        End Sub
    End Class
    Public Class ArgumentRemovedEventArgs
        Public SenderItem As ArgumentsListItem
        Public Sub New(SenderItem As ArgumentsListItem)
            Me.SenderItem = SenderItem
        End Sub
    End Class
    Public Class ArgumentsList
        Inherits ScrollableList(Of ArgumentsListItem)
        Protected Friend Event Scrolled(Sender As ArgumentsList, e As EventArgs)
        Public Event ItemRemoveClicked(Sender As Object, e As ArgumentRemovedEventArgs)
        'Private GradientColors() As Color = {ColorHelper.Multiply(ApplicationBackColor, 0.8), ColorHelper.Multiply(ApplicationBackColor, 0.5)}
        Public Sub New()
            AllowWheel = False
            ScrollHandle.RoundedEnds = False
            ScrollHandle.Width = 4
            BackColor = ColorHelper.Multiply(Color.FromArgb(28, 127, 171), 0.6)
            ListContainer.BackColor = ApplicationBackColor
        End Sub
        'Protected Friend Function GetItemGradientBrush(TopInListContainer As Integer) As Drawing2D.LinearGradientBrush
        '    Dim GradientTop As Integer = (-ListContainer.Top) - TopInListContainer
        '    Dim GradientBottom As Integer = GradientTop + Height
        '    Return New Drawing2D.LinearGradientBrush(New Point(0, GradientTop), New Point(0, GradientBottom), GradientColors(0), GradientColors(1))
        'End Function
        Private Sub WheelScrolled(Sender As Object, e As EventArgs) Handles ScrollHandle.ScrollValueChanged
            RaiseEvent Scrolled(Me, EventArgs.Empty)
        End Sub
        Public Overrides Sub Add(Item As ArgumentsListItem)
            MyBase.Add(Item)
            AddHandler Item.RemoveClicked, AddressOf Item_RemoveClicked
        End Sub
        Private Sub Item_RemoveClicked(Sender As Object, e As EventArgs)
            RaiseEvent ItemRemoveClicked(Me, New ArgumentRemovedEventArgs(DirectCast(Sender, ArgumentsListItem)))
        End Sub
        Protected Overrides Sub RemoveItemHandlers(ByRef Item As ArgumentsListItem)
            RemoveHandler Item.RemoveClicked, AddressOf Item_RemoveClicked
            MyBase.RemoveItemHandlers(Item)
        End Sub
        'Private Sub ListContainer_Paint(Sender As Object, e As PaintEventArgs) Handles ListContainer.Paint
        '    With e.Graphics
        '        Dim AdjustedRectangle As New Rectangle(New Point(0, -ListContainer.Top), New Size(ListContainer.Width, ClientSize.Height))
        '        .SetClip(AdjustedRectangle)
        '        Using GradientBrush As Drawing2D.LinearGradientBrush = GetItemGradientBrush(-ListContainer.Top)
        '            .FillRectangle(GradientBrush, AdjustedRectangle)
        '        End Using
        '    End With
        'End Sub
    End Class
    Public Class ArgumentsListItem
        Inherits ScrollableListItem
        Private WithEvents ParentList As ArgumentsList
        Private TextHeight As Integer
        Private RemovePath() As Point = {New Point(15, 10), New Point(13, 8), New Point(14, 7), New Point(16, 9), New Point(18, 7), New Point(19, 8), New Point(17, 10), New Point(19, 12), New Point(18, 13), New Point(16, 11), New Point(14, 13), New Point(13, 12)}
        Private IsHoveringRemove As Boolean
        Private DefaultRemoveColor As Color = ColorHelper.Multiply(ApplicationBackColor, 0.7)
        Private FillBrush As New SolidBrush(DefaultRemoveColor)
        Private LinePen As New Pen(DefaultRemoveColor)
        Public Event RemoveClicked(Sender As Object, e As EventArgs)
        Public Sub New()
            Throw New NotImplementedException
        End Sub
        Public Sub New(ParentList As ArgumentsList, Value As ArgumentsListItemValue)
            Me.ParentList = ParentList
            AddHandler ParentList.Scrolled, AddressOf Parent_Scrolled
            Height = 20
            UpdateStyles()
            'BackColor = ColorHelper.Multiply(ApplicationBackColor, 0.6)
            Me.Value = Value
            For i As Integer = 0 To RemovePath.Count - 1
                RemovePath(i).Offset(New Point(170, 0))
            Next
        End Sub
        Private Function CheckRemoveHover(ClientMouseLocation As Point) As Boolean
            Dim ActualArea As New Size(6, 6)
            Dim MinXY As New Point(13, 7)
            MinXY.Offset(170, 0)
            Dim AcceptableRect As New Rectangle(MinXY, ActualArea)
            AcceptableRect.Inflate(3, 3)
            Return FormatConverter.PointIsInRectangle(ClientMouseLocation, AcceptableRect)
        End Function
        Protected Overrides Sub OnMouseUp(e As MouseEventArgs)
            MyBase.OnMouseUp(e)
            If CheckRemoveHover(e.Location) Then
                RaiseEvent RemoveClicked(Me, EventArgs.Empty)
            End If
        End Sub
        Protected Overrides Sub OnMouseMove(e As MouseEventArgs)
            MyBase.OnMouseMove(e)
            Dim HoverState As Boolean = CheckRemoveHover(e.Location)
            If HoverState <> IsHoveringRemove Then
                If HoverState Then
                    Cursor = Cursors.Hand
                    FillBrush.Color = Color.White
                    LinePen.Color = Color.White
                Else
                    Cursor = Cursors.Default
                    FillBrush.Color = DefaultRemoveColor
                    LinePen.Color = DefaultRemoveColor
                End If
                IsHoveringRemove = HoverState
                Invalidate()
            End If
        End Sub
        Protected Overrides Sub OnHoveringChanged(e As ItemHoveringEventArgs)
            MyBase.OnHoveringChanged(e)
            If Not Hovering Then
                IsHoveringRemove = False
                FillBrush.Color = DefaultRemoveColor
                LinePen.Color = DefaultRemoveColor
            End If
            Invalidate()
        End Sub
        Public Shadows Property Parent As ListContainerControl
            Get
                Return MyBase.Parent
            End Get
            Set(value As ListContainerControl)
                MyBase.Parent = value
                AddHandler Parent.WheelScrolled, AddressOf Parent_Scrolled
                AddHandler Parent.LocationChanged, AddressOf Parent_Scrolled
                TextHeight = TextRenderer.MeasureText("abdefghij", Parent.StaticLabelFont(ListElementFontSize.Small)).Height
            End Set
        End Property
        Private Sub Parent_Scrolled(Sender As Object, e As EventArgs)
            Invalidate()
        End Sub
        Protected Overrides Sub OnPaint(pe As PaintEventArgs)
            MyBase.OnPaint(pe)
            'Using GradientBrush As Drawing2D.LinearGradientBrush = ParentList.GetItemGradientBrush(Top)
            With pe.Graphics
                .SetClip(ClientRectangle)
                '.FillRectangle(GradientBrush, ClientRectangle)
                TextRenderer.DrawText(pe.Graphics, "> " & Value.Argument.ToString, Parent.StaticLabelFont(ListElementFontSize.Small), New Point(2, (Height - TextHeight) \ 2 - 5), Color.White)
                .DrawPolygon(LinePen, RemovePath)
                .FillPolygon(FillBrush, RemovePath)
            End With
            'End Using
        End Sub
        Public Shadows Property Value As ArgumentsListItemValue
            Get
                Return DirectCast(MyBase.Value, ArgumentsListItemValue)
            End Get
            Set(value As ArgumentsListItemValue)
                MyBase.Value = value
                OnValueChanged(New ValueChangedEventArgs(value))
            End Set
        End Property
        Public Overrides ReadOnly Property DefaultValue As ScrollableListItemValue
            Get
                Return New ArgumentsListItemValue("TEEEEST")
            End Get
        End Property
        Public Overrides Sub ApplyArguments(Arguments As Object)
            Throw New NotImplementedException()
        End Sub
    End Class
    Public Class ArgumentsListItemValue
        Inherits ScrollableListItemValue
        Private varArgument As Object
        Public Sub New(ByVal Argument As Object)
            MyBase.New(Nothing)
            varArgument = Argument
        End Sub
        Public Property Argument As Object
            Get
                Return varArgument
            End Get
            Set(value As Object)
                varArgument = value
            End Set
        End Property
    End Class
    Public Class CurrentComponentChangedEventArgs
        Inherits EventArgs
        Public SelectedEntry As ComponentEntry
        Public Sub New(SelectedEntry As ComponentEntry)
            Me.SelectedEntry = SelectedEntry
        End Sub
    End Class
    Public Class Overview
        Inherits Tab
        Implements IListElementFontProvider

        Private LayoutHelper As New FormLayoutTools(Me)
        Private varForeColor As Color = Color.White
        Private varStaticLabelFont As Font = RalewayFonts.GetFont(RalewayFontCollection.FontVariant.Bold, 14, FontStyle.Bold)
        Private varStatusLabelFont As Font = RalewayFonts.GetFont(RalewayFontCollection.FontVariant.Regular, 14, FontStyle.Regular)
        Private varEditButtonFont As Font = RalewayFonts.GetFont(RalewayFontCollection.FontVariant.Regular, 9.5, FontStyle.Regular)
        Private varLineHeight As Integer = 50
        Private varLineSpacing As Integer = 15
        Private LinePen As New Pen(Color.White)
        Private InnerRect As New Rectangle(Point.Empty, New Size(400, 400))
        Private varLabels As New List(Of ListElement)
        Private EditButtons As New List(Of EditButton)
        Private varHoveredEditButton As EditButton = Nothing
        Private ShortcutRects(4) As Rectangle
        Private HoveredShortcutIndex As Integer = -1
        Public ReadOnly Property Labels As List(Of ListElement) Implements IListElementFontProvider.Labels
            Get
                Return varLabels
            End Get
        End Property
        Public Property EditButtonFont(ByVal FontSize As ListElementFontSize) As Font Implements IListElementFontProvider.EditButtonFont
            Get
                Return varEditButtonFont
            End Get
            Set(value As Font)
                varEditButtonFont = value
                Invalidate()
            End Set
        End Property
        Public Property StaticLabelFont(ByVal FontSize As ListElementFontSize) As Font Implements IListElementFontProvider.StaticLabelFont
            Get
                Return varStaticLabelFont
            End Get
            Set(value As Font)
                varStaticLabelFont = value
                Invalidate()
            End Set
        End Property
        Public Property StatusLabelFont(ByVal FontSize As ListElementFontSize) As Font Implements IListElementFontProvider.StatusLabelFont
            Get
                Return varStatusLabelFont
            End Get
            Set(value As Font)
                varStatusLabelFont = value
                Invalidate()
            End Set
        End Property
        Public Property HoveredEditButton As EditButton Implements IListElementFontProvider.HoveredEditButton
            Get
                Return varHoveredEditButton
            End Get
            Set(value As EditButton)
                varHoveredEditButton = value
            End Set
        End Property
        Public Sub New(Parent As MultiTabWindow)
            MyBase.New(Parent)
            'LinePen.DashStyle = Drawing2D.DashStyle.Dot
            LinePen.DashPattern = New Single() {1, 2}
            ResizeRedraw = True
            BackColor = DefaultBackColor
            With InnerRect
                Dim ResourceHeight As Integer = My.Resources.Header.Height
                Dim X As Integer = (Width - .Width) \ 2
                Dim Y As Integer = (Height - .Height - ResourceHeight) \ 2 + ResourceHeight
                .Location = New Point(X, Y)
            End With
            Labels.Add(New ListElement(Me, "Status", "Paused"))
            Labels.Last.AddEditButton("Start", AddressOf StartStopService, True)
            Labels.Add(New ListElement(Me, "Template", "SomeTemplate"))
            Labels.Last.AddEditButton("Change", AddressOf ChangeTemplate, Nothing)

            Dim PicSpacing As Integer = 20
            Dim PicsArea As New Size(ShortcutRects.Count * 32 + (ShortcutRects.Count - 1) * PicSpacing, 32)
            Dim PicsAreaLocation As New Point((Width - PicsArea.Width) \ 2, (Height \ 2))
            Dim PicsRect As New Rectangle(PicsAreaLocation, PicsArea)
            For i As Integer = 0 To ShortcutRects.Count - 1
                ShortcutRects(i) = New Rectangle(New Point(PicsRect.X + i * (32 + PicSpacing), PicsRect.Top), New Size(32, 32))
            Next
            AddHandler PSEngine.Paused, AddressOf PSEngine_Paused
            Invalidate()
        End Sub
        Private Sub PSEngine_Paused(Sender As Object, e As ExecutionPausedEventArgs)
            Labels(0).SetStatusText("Paused")
        End Sub
        Protected Overrides Sub OnSizeChanged(e As EventArgs)
            MyBase.OnSizeChanged(e)
            With InnerRect
                Dim ResourceHeight As Integer = My.Resources.Header.Height
                Dim X As Integer = (Width - .Width) \ 2
                Dim Y As Integer = (Height - .Height - ResourceHeight) \ 2 + ResourceHeight
                .Location = New Point(X, Y)
            End With
            If ShortcutRects IsNot Nothing Then
                Dim PicSpacing As Integer = 20
                Dim PicsArea As New Size(ShortcutRects.Count * 32 + (ShortcutRects.Count - 1) * PicSpacing, 32)
                Dim PicsAreaLocation As New Point((Width - PicsArea.Width) \ 2, (Height \ 2))
                Dim PicsRect As New Rectangle(PicsAreaLocation, PicsArea)
                For i As Integer = 0 To ShortcutRects.Count - 1
                    ShortcutRects(i) = New Rectangle(New Point(PicsRect.X + i * (32 + PicSpacing), PicsRect.Top), New Size(32, 32))
                Next
            End If
            If FindForm.WindowState = FormWindowState.Normal Then
                If ApplicationGradientBrush IsNot Nothing Then ApplicationGradientBrush.Dispose()
                ApplicationGradientBrush = New Drawing2D.LinearGradientBrush(Point.Empty, New Point(Width, Height), GradientFirstColor, GradientSecondColor)
            End If
        End Sub
        Protected Overrides Sub OnPaint(e As PaintEventArgs)
            MyBase.OnPaint(e)
            Dim iLast As Integer = Labels.Count - 1
            e.Graphics.SetClip(ClientRectangle)
            e.Graphics.FillRectangle(ApplicationGradientBrush, ClientRectangle)
            e.Graphics.SetClip(InnerRect)
            For i As Integer = 0 To iLast
                With Labels(i)
                    Dim LabelRect As New Rectangle(New Point(InnerRect.Left, i * (varLineHeight + varLineSpacing) + InnerRect.Top), New Size(InnerRect.Width, varLineHeight))
                    TextRenderer.DrawText(e.Graphics, .LabelText, StaticLabelFont(ListElementFontSize.Regular), .GetLabelRectangle(LabelRect.Location), varForeColor)
                    TextRenderer.DrawText(e.Graphics, .StatusText, StatusLabelFont(ListElementFontSize.Regular), .GetStatusRectangle(LabelRect.Right, LabelRect.Top), varForeColor)
                    e.Graphics.DrawLine(LinePen, New Point(LabelRect.Left + 5, LabelRect.Bottom), New Point(LabelRect.Right - 5, LabelRect.Bottom))
                    If .HasEditButtons Then
                        For Each EditButton As EditButton In .GetEditButtons(LabelRect.Bottom - 23, LabelRect.Right - 5)
                            Dim EditButtonRect As Rectangle = EditButton.Rectangle
                            TextRenderer.DrawText(e.Graphics, EditButton.Text, EditButtonFont(ListElementFontSize.Regular), EditButton.Rectangle, varForeColor)
                            If HoveredEditButton IsNot Nothing AndAlso ReferenceEquals(EditButton, HoveredEditButton) Then
                                With EditButton.Rectangle
                                    e.Graphics.DrawLine(LinePen, New Point(.X + 2, .Bottom), New Point(.Right - 1, .Bottom))
                                End With
                            End If
                        Next
                    End If
                End With
            Next
            For n As Integer = 0 To ShortcutRects.Count - 1
                e.Graphics.SetClip(ShortcutRects(n))
                e.Graphics.DrawImage(Header.ViewImages(n), ShortcutRects(n))
                If n = HoveredShortcutIndex Then
                    Dim LineRectangle As New Rectangle(New Point(ShortcutRects(n).X, ShortcutRects(n).Bottom + 4), New Size(ShortcutRects(n).Width, 5))
                    e.Graphics.SetClip(LineRectangle)
                    e.Graphics.Clear(Color.White)
                End If
            Next
            ' End Using
        End Sub
        Protected Overrides Sub OnTabShown(e As EventArgs)
            MyBase.OnTabShown(e)
            Header.CurrentDirectiry = {HeaderControl.Views.Overview}
        End Sub
        Protected Overrides Sub OnMouseMove(e As MouseEventArgs) Implements IListElementFontProvider.CheckMouseLocation
            MyBase.OnMouseMove(e)
            Dim Match As EditButton = Nothing
            For Each Label As ListElement In Labels
                Dim PointInsideButton As EditButton = Label.PointInside(e.Location)
                If PointInsideButton IsNot Nothing Then
                    Match = PointInsideButton
                    Exit For
                End If
            Next
            If Not (Match Is Nothing AndAlso HoveredEditButton Is Nothing) Then
                HoveredEditButton = Match
                Invalidate()
                If HoveredEditButton IsNot Nothing Then
                    Cursor = Cursors.Hand
                Else
                    Cursor = Cursors.Default
                End If
            End If
            Dim MatchIndex As Integer
            For i As Integer = 0 To ShortcutRects.Count - 1
                If FormatConverter.PointIsInRectangle(e.Location, ShortcutRects(i)) Then
                    MatchIndex = i
                    Exit For
                End If
                MatchIndex = -1
            Next
            If MatchIndex <> HoveredShortcutIndex Then
                HoveredShortcutIndex = MatchIndex
                Invalidate()
            End If
        End Sub
        Protected Overrides Sub OnMouseUp(e As MouseEventArgs)
            MyBase.OnMouseUp(e)
            If HoveredEditButton IsNot Nothing Then
                HoveredEditButton.ClickAction.Invoke(HoveredEditButton, HoveredEditButton.Arguments)
            ElseIf HoveredShortcutIndex <> -1 Then
                WindowControl.ShowTab(HoveredShortcutIndex)
                HoveredShortcutIndex = -1
            End If
        End Sub
#Region "Click actions"
        Private Sub StartStopService(Sender As EditButton, ByVal Start As Object)
            Dim StartBool As Boolean = DirectCast(Start, Boolean)
            If StartBool AndAlso ActiveTemplate IsNot Nothing Then
                Sender.Arguments = False
                Sender.Text = "Pause"
                PSEngine.Start()
                Sender.Parent.SetStatusText("Running")
            Else
                Sender.Arguments = True
                Sender.Parent.SetStatusText("Pausing...")
                Sender.Text = "Start"
                PSEngine.RequestPause()
            End If
        End Sub
        Private Sub ChangeTemplate(Sender As EditButton, Optional ByVal State As Object = Nothing)
            Parent.ShowTab(1)
        End Sub
#End Region
    End Class
    Public Class ListElement
        Private varLabelText, varStatusText As String
        Private varLabelSize, varStatusSize As Size
        Private varEditButtons As New List(Of EditButton)
        Private varEditButtonSpacing As Integer = 10
        Protected Friend varStaticFontSize, varStatusFontSize, varEditButtonFontSize As ListElementFontSize
        Public ReadOnly Property Parent As IListElementFontProvider
        Public ReadOnly Property HasEditButtons As Boolean
            Get
                Return (varEditButtons.Count > 0)
            End Get
        End Property
        Public Function PointInside(ByVal Point As Point) As EditButton
            For Each EditButton As EditButton In varEditButtons
                If EditButton.PointIsInside(Point) Then
                    Return EditButton
                End If
            Next
            Return Nothing
        End Function
        Public Function GetEditButtons(ByVal Top As Integer, ByVal LabelRight As Integer) As EditButton()
            Dim Ret(varEditButtons.Count - 1) As EditButton
            Dim LeftMost As Integer = LabelRight
            For i As Integer = 0 To Ret.Count - 1
                With varEditButtons(i)
                    LeftMost -= .Size.Width
                    .Rectangle = New Rectangle(New Point(LeftMost, Top), .Size)
                    LeftMost -= varEditButtonSpacing
                End With
                Ret(i) = varEditButtons(i)
            Next
            Return Ret
        End Function
        Public ReadOnly Property EditButtons As List(Of EditButton)
            Get
                Return varEditButtons
            End Get
        End Property
        Public ReadOnly Property StatusText As String
            Get
                Return varStatusText
            End Get
        End Property
        Public ReadOnly Property LabelText As String
            Get
                Return varLabelText
            End Get
        End Property
        Public Sub AddEditButton(ByVal Text As String, ClickAction As Action(Of EditButton, Object), Arguments As Object)
            varEditButtons.Add(New EditButton(Text, Me, ClickAction, Arguments)) ' TODO: Check if ReferanceEquals to determine which action to invoke
        End Sub
        Public Sub SetStatusText(ByVal Text As String)
            varStatusText = Text
            varStatusSize = TextRenderer.MeasureText(varStatusText, Parent.StatusLabelFont(varStatusFontSize))
        End Sub
        Public Sub SetLabelText(ByVal Text As String)
            varLabelText = Text
            varLabelSize = TextRenderer.MeasureText(varLabelText, Parent.StaticLabelFont(varStaticFontSize))
        End Sub
        Public Function GetLabelRectangle(ByVal LabelLocation As Point, Optional Offset As Point = Nothing) As Rectangle
            Dim Ret As New Rectangle(LabelLocation, varLabelSize)
            Ret.Offset(Offset)
            Return Ret
        End Function
        Public Function GetStatusRectangle(ByVal StatusRight As Integer, ByVal StatusTop As Integer, Optional Offset As Point = Nothing) As Rectangle
            Dim Ret As New Rectangle(New Point(StatusRight - varStatusSize.Width, StatusTop), varStatusSize)
            Ret.Offset(Offset)
            Return Ret
        End Function
        Public Sub New(Parent As IListElementFontProvider, LabelText As String, StatusText As String, Optional ByVal StaticFontSize As ListElementFontSize = ListElementFontSize.Regular, Optional ByVal StatusFontSize As ListElementFontSize = ListElementFontSize.Regular, Optional ByVal EditButtonFontSize As ListElementFontSize = ListElementFontSize.Regular)
            Me.Parent = Parent
            varStaticFontSize = StaticFontSize
            varStatusFontSize = StatusFontSize
            varEditButtonFontSize = EditButtonFontSize
            ' TODO: Handle parent font changed (label + status)
            SetLabelText(LabelText)
            SetStatusText(StatusText)
        End Sub
    End Class
    Public Class EditButton
        Private varText As String
        Private varTextSize As Size
        Private varRect As Rectangle
        Private varParent As ListElement
        Private varArguments As Object = Nothing
        Private varClickAction As Action(Of EditButton, Object)
        Public Function PointIsInside(ByVal Point As Point) As Boolean
            If Point.X >= Rectangle.X AndAlso Point.X <= Rectangle.Right AndAlso Point.Y >= Rectangle.Y AndAlso Point.Y <= Rectangle.Bottom Then
                Return True
            End If
            Return False
        End Function
        Public Property Parent As ListElement
            Get
                Return varParent
            End Get
            Set(value As ListElement)
                varParent = value
            End Set
        End Property
        Public Property Arguments As Object
            Get
                Return varArguments
            End Get
            Set(value As Object)
                varArguments = value
            End Set
        End Property
        Public ReadOnly Property ClickAction As Action(Of EditButton, Object)
            Get
                Return varClickAction
            End Get
        End Property
        Public Sub SetClickAction(ClickAction As Action(Of EditButton, Object), Arguments As Object)
            varClickAction = ClickAction
            varArguments = Arguments
        End Sub
        Public Property Rectangle As Rectangle
            Get
                Return varRect
            End Get
            Set(value As Rectangle)
                varRect = value
            End Set
        End Property
        Public Property Text As String
            Get
                Return varText
            End Get
            Set(value As String)
                varText = value
                With varParent
                    varTextSize = TextRenderer.MeasureText(varText, .Parent.EditButtonFont(.varEditButtonFontSize))
                End With
            End Set
        End Property
        Public ReadOnly Property Size As Size
            Get
                Return varTextSize
            End Get
        End Property
        Public Sub SetText(Text As String)
            varText = Text
            With varParent
                varTextSize = TextRenderer.MeasureText(varText, .Parent.EditButtonFont(.varEditButtonFontSize))
            End With
        End Sub
        Public Sub New(Text As String, ByRef Parent As ListElement, ClickAction As Action(Of EditButton, Object), Arguments As Object)
            varArguments = Arguments
            varClickAction = ClickAction
            varParent = Parent
            varText = Text
            With varParent
                varTextSize = TextRenderer.MeasureText(varText, .Parent.EditButtonFont(.varEditButtonFontSize))
            End With
        End Sub
    End Class
    Public Class ListItemActiveChangedEventArgs
        Inherits EventArgs
        Public SenderItem As ScrollableListItem
        Public Data As ScrollableListItem.ItemActiveEventArgs
        Public Sub New(SenderItem As ScrollableListItem, Data As ScrollableListItem.ItemActiveEventArgs)
            Me.SenderItem = SenderItem
            Me.Data = Data
        End Sub
    End Class
    Public Class ListItemSelectionChangedEventArgs
        Inherits EventArgs
        Public SenderItem As ScrollableListItem
        Public Data As ScrollableListItem.ItemSelectedEventArgs
        Public Sub New(SenderItem As ScrollableListItem, Data As ScrollableListItem.ItemSelectedEventArgs)
            Me.SenderItem = SenderItem
            Me.Data = Data
        End Sub
    End Class
    Public Interface IDragEventReporter
        Sub ReportDragEvent(Sender As Control, e As DragEventArgs)
        Event ChildDragEventReported(Target As Control, e As DragEventArgs)
    End Interface
    Public Class ListContainerControl
        Inherits PictureBox
        Implements IDragEventReporter
        Implements IListElementFontProvider
        'Private WithEvents ScrollTimer As New System.Timers.Timer(1000 / 30)
        'Private varDelta As Integer = 1
        'Private varPositive As Boolean
        Private varFontSizes() As Font = {RalewayFonts.GetFont(RalewayFontCollection.FontVariant.Regular, 10, FontStyle.Regular), RalewayFonts.GetFont(RalewayFontCollection.FontVariant.Regular, 9, FontStyle.Regular), RalewayFonts.GetFont(RalewayFontCollection.FontVariant.Regular, 14, FontStyle.Regular)}
        Friend Event DisposeRequested(Sender As Object, e As EventArgs)
        Public Event WheelScrolled(Sender As Object, e As MouseEventArgs)
        Public Event ChildDragEventReported(Target As Control, e As DragEventArgs) Implements IDragEventReporter.ChildDragEventReported
        Private LastScrollEvent As Date
        Public Property StaticLabelFont(ByVal FontSize As ListElementFontSize) As Font Implements IListElementFontProvider.StaticLabelFont
            Get
                Return varFontSizes(FontSize)
            End Get
            Set(value As Font)
                varFontSizes(FontSize) = value
            End Set
        End Property
        Public Property StatusLabelFont(ByVal FontSize As ListElementFontSize) As Font Implements IListElementFontProvider.StatusLabelFont
            Get
                Return varFontSizes(FontSize)
            End Get
            Set(value As Font)
                varFontSizes(FontSize) = value
            End Set
        End Property
        Public Property EditButtonFont(ByVal FontSize As ListElementFontSize) As Font Implements IListElementFontProvider.EditButtonFont
            Get
                Return varFontSizes(FontSize)
            End Get
            Set(value As Font)
                varFontSizes(FontSize) = value
            End Set
        End Property
        Public Property HoveredEditButton As EditButton Implements IListElementFontProvider.HoveredEditButton
            Get
                Throw New NotImplementedException()
            End Get
            Set(value As EditButton)
                Throw New NotImplementedException()
            End Set
        End Property
        Public ReadOnly Property Labels As List(Of ListElement) Implements IListElementFontProvider.Labels
            Get
                Throw New NotImplementedException()
            End Get
        End Property
        Public Sub ReportDragEvent(Sender As Control, e As DragEventArgs) Implements IDragEventReporter.ReportDragEvent
            RaiseEvent ChildDragEventReported(Sender, e)
        End Sub
        Public Sub DisposeItems()
            RaiseEvent DisposeRequested(Me, EventArgs.Empty)
        End Sub
        Public Sub Scroll(ByVal e As MouseEventArgs)
            RaiseEvent WheelScrolled(Me, e)
        End Sub
        Public Sub New()
            DoubleBuffered = True
            'ScrollTimer.AutoReset = False
        End Sub
        Protected Overrides Sub OnPaint(pe As PaintEventArgs)
            With pe.Graphics
                Dim ParentRectangle As New Rectangle(New Point(0, -Top), New Size(Width, Parent.Height))
                .SetClip(ParentRectangle)
            End With
            MyBase.OnPaint(pe)
        End Sub
        Public Sub CheckMouseLocation(e As MouseEventArgs) Implements IListElementFontProvider.CheckMouseLocation
            Throw New NotImplementedException()
        End Sub
    End Class
    Public Class VisibleEventArgs
        Inherits EventArgs
        Public Visible As Boolean
        Public Sub New(ByVal Visible As Boolean)
            Me.Visible = Visible
        End Sub
    End Class
    Public Class ScrollableList(Of ItemType As {ScrollableListItem, New})
        Inherits PictureBox
        Protected WithEvents ListContainer As New ListContainerControl
        Protected WithEvents ScrollHandle As New ScrollHandleControl(Of ScrollableList(Of ItemType))
        Private varItems As New List(Of ItemType)
        Protected varItemSpacing As Integer
        Protected AllowWheel As Boolean = True
        Public Event ItemActiveChanged(Sender As Object, e As ListItemActiveChangedEventArgs)
        Public Event ItemSelectionChanged(Sender As Object, e As ListItemSelectionChangedEventArgs)
        Public Event ScrollHandleVisibleChanged(Sender As Object, e As VisibleEventArgs)
        Private Sub ListContainer_WheelScrolled(Sender As Object, e As MouseEventArgs) Handles ListContainer.WheelScrolled
            If AllowWheel Then
                If varItems.Count > 0 Then
                    Dim CalculatedItemHeight As Integer = ListContainer.Height \ varItems.Count
                    Dim ScrollDelta As Double = CalculatedItemHeight / ListContainer.Height
                    If e.Delta > 0 Then
                        ScrollHandle.ScrollValue -= ScrollDelta
                    Else
                        ScrollHandle.ScrollValue += ScrollDelta
                    End If
                End If
            End If
        End Sub
        Public Overridable Sub Add(Item As ItemType)
            With Item
                If Items.Count > 0 Then
                    .Top = Items.Last.Bottom + varItemSpacing
                Else
                    .Top = 0
                End If
                .Index = Items.Count
                AddHandler .ActiveChanged, AddressOf Item_ActiveChanged
                AddHandler .SelectedChanged, AddressOf Item_SelectedChanged
                Items.Add(Item)
                .Parent = ListContainer
                AutoAdjust()
            End With
        End Sub
        Public Overridable Sub Remove(ByRef Item As ItemType)
            RemoveHandler Item.ActiveChanged, AddressOf Item_ActiveChanged
            RemoveHandler Item.SelectedChanged, AddressOf Item_SelectedChanged
            Items.Remove(Item)
            RepositionItems()
            Item.Dispose()
        End Sub
        Private Sub RepositionItems()
            Dim iLast As Integer = varItems.Count - 1
            If iLast >= 0 Then
                For i As Integer = 0 To iLast
                    With Items(i)
                        If i = 0 Then
                            .Top = 0
                        Else
                            .Top = Items(i - 1).Bottom + varItemSpacing
                        End If
                        .Index = i
                    End With
                Next
            End If
            AutoAdjust()
        End Sub
        Public Property UseRoundedEnds As Boolean
            Get
                Return ScrollHandle.RoundedEnds
            End Get
            Set(value As Boolean)
                ScrollHandle.RoundedEnds = value
                ScrollHandle.Invalidate()
                Invalidate()
            End Set
        End Property
        Protected Overrides Sub OnMouseWheel(e As MouseEventArgs)
            ListContainer.Scroll(e)
        End Sub
        Public Property ItemSpacing As Integer
            Get
                Return varItemSpacing
            End Get
            Set(value As Integer)
                varItemSpacing = value
                RepositionItems()
                AutoAdjust()
            End Set
        End Property
        Public Property SeparatorColor As Color
            Get
                Return ListContainer.BackColor
            End Get
            Set(value As Color)
                ListContainer.BackColor = value
            End Set
        End Property
        Public ReadOnly Property ContentHeight As Integer
            Get
                Return ListContainer.Height
            End Get
        End Property
        Public Overloads Sub DeselectAll()
            For Each Item As ItemType In varItems
                Item.IsSelected = False
            Next
        End Sub
        Public Overloads Sub DeselectAll(ByVal ExceptIndex As Integer)
            Dim iLast As Integer = varItems.Count - 1
            For i As Integer = 0 To iLast
                If Not i = ExceptIndex Then varItems(i).IsSelected = False
            Next
        End Sub
        Public Overloads Sub SetAllActive(ByVal Active As Boolean)
            For Each Item As ItemType In varItems
                Item.IsActive = Active
            Next
        End Sub
        Public Overloads Sub SetAllActive(ByVal Active As Boolean, ByVal ExceptIndex As Integer)
            Dim iLast As Integer = varItems.Count - 1
            For i As Integer = 0 To iLast
                If Not i = ExceptIndex Then varItems(i).IsActive = Active
            Next
        End Sub
        Protected Overridable Sub RemoveItemHandlers(ByRef Item As ItemType)
            RemoveHandler Item.ActiveChanged, AddressOf Item_ActiveChanged
            RemoveHandler Item.SelectedChanged, AddressOf Item_SelectedChanged
        End Sub
        Public Sub Clear()
            For Each Item As ItemType In Items
                RemoveItemHandlers(Item)
            Next
            ListContainer.DisposeItems()
            varItems.Clear()
            ScrollHandle.ScrollValue = 0
            AutoAdjust()
        End Sub
        Public ReadOnly Property SelectedItems As List(Of ItemType)
            Get
                Dim Ret As New List(Of ItemType)
                For Each Item As ItemType In varItems
                    If Item.IsSelected Then
                        Ret.Add(Item)
                    End If
                Next
                Return Ret
            End Get
        End Property
        Public Sub CreateItems(ArgumentsArray() As Object)
            For i As Integer = 0 To ArgumentsArray.Count - 1
                Dim NewItem As New ItemType
                With NewItem
                    .Width = ListContainer.Width
                    .Parent = ListContainer
                    .Top = (.Height + varItemSpacing) * i
                    .ApplyArguments(ArgumentsArray(i))
                    .Index = i
                    AddHandler .ActiveChanged, AddressOf Item_ActiveChanged
                    AddHandler .SelectedChanged, AddressOf Item_SelectedChanged
                End With
                varItems.Add(NewItem)
            Next
            ListContainer.Height = varItems.Last.Bottom
        End Sub
        Protected Sub Item_ActiveChanged(Sender As Object, e As ScrollableListItem.ItemActiveEventArgs)
            Dim SenderItem As ItemType = DirectCast(Sender, ItemType)
            RaiseEvent ItemActiveChanged(Me, New ListItemActiveChangedEventArgs(SenderItem, e))
        End Sub
        Protected Sub Item_SelectedChanged(Sender As Object, e As ScrollableListItem.ItemSelectedEventArgs)
            Dim SenderItem As ItemType = DirectCast(Sender, ItemType)
            RaiseEvent ItemSelectionChanged(Me, New ListItemSelectionChangedEventArgs(SenderItem, e))
        End Sub
        Public ReadOnly Property Items As List(Of ItemType)
            Get
                Return varItems
            End Get
        End Property
        Public ReadOnly Property Item(ByVal Index As Integer) As ItemType
            Get
                Return varItems(Index)
            End Get
        End Property
        Public Sub New()
            DoubleBuffered = True
            Dim DefaultSize As New Size(300, 400)
            BackColor = ApplicationBackColor
            With ListContainer
                .Parent = Me
                .MinimumSize = New Size(DefaultSize.Width - ScrollHandle.Width, DefaultSize.Height)
            End With
            ScrollHandle.Parent = Me
            AddHandler ScrollHandle.ScrollValueChanged, AddressOf ScrollHandle_ScrollValueChanged
            AddHandler ListContainer.SizeChanged, AddressOf ListContainer_SizeChanged
            AddHandler ListContainer.Click, AddressOf ListContainer_Click
            Size = DefaultSize
        End Sub
        Private Sub ScrollHandle_ScrollValueChanged(Sender As Object, e As HandleScrollEventArgs)
            ListContainer.Top = GetContainerLocationFromScrollValue(ScrollHandle.ScrollValue)
        End Sub
        Private Sub ScrollHandle_VisibleChanged(Sender As Object, e As EventArgs) Handles ScrollHandle.VisibleChanged
            ListContainer.MinimumSize = New Size(Width - ScrollHandle.Width, Height)
            If ScrollHandle.Visible Then
                ListContainer.Width = Width - ScrollHandle.Width
            Else
                ListContainer.Width = Width
            End If
            If Items.Count > 0 AndAlso ListContainer.Height > Items.Last.Bottom Then
                ListContainer.Height = Items.Last.Bottom
            End If
            RaiseEvent ScrollHandleVisibleChanged(Me, New VisibleEventArgs(ScrollHandle.Visible))
            ListContainer.Invalidate(True)
        End Sub
        Protected Friend Function GetContainerLocationFromScrollValue(ByVal ScrollValue As Double) As Integer
            Return CInt(-ScrollValue * (ListContainer.Height - Height))
        End Function
        Protected Overrides Sub OnSizeChanged(e As EventArgs)
            MyBase.OnSizeChanged(e)
            AutoAdjust()
        End Sub
        Protected Overridable Sub AutoAdjust()
            ListContainer.MinimumSize = New Size(Width - ScrollHandle.Width, Height)
            If ScrollHandle.Visible Then
                ListContainer.Width = Width - ScrollHandle.Width
            Else
                ListContainer.Width = Width
            End If
            If Items.Count > 0 Then
                ListContainer.Height = Items.Last.Bottom
            Else
                ListContainer.Height = Height
            End If
            If ListContainer.Bottom < Height Then
                ScrollHandle.ScrollValue = 1
            End If
            ScrollHandle.AutoAdjust()
            For Each Item As ScrollableListItem In Items
                Item.Width = ListContainer.Width
            Next
        End Sub
        Private Sub ListContainer_SizeChanged(Sender As Object, e As EventArgs)
            ScrollHandle.AutoAdjust()
            If ScrollHandle.Height = Height AndAlso ScrollHandle.Visible Then
                ScrollHandle.Hide()
            ElseIf ScrollHandle.Height < Height AndAlso Not ScrollHandle.Visible Then
                ScrollHandle.Show()
            End If
        End Sub
        Private Sub ListContainer_Click(Sender As Object, e As EventArgs)
            ListContainer.Focus()
        End Sub
        Protected Overrides Sub OnMouseDown(e As MouseEventArgs)
            MyBase.OnMouseDown(e)
            ScrollHandle.StartDrag(True)
        End Sub
        Protected Overrides Sub OnMouseMove(e As MouseEventArgs)
            MyBase.OnMouseMove(e)
            ScrollHandle.DoDrag()
        End Sub
        Protected Overrides Sub OnMouseUp(e As MouseEventArgs)
            MyBase.OnMouseUp(e)
            ScrollHandle.StopDrag()
        End Sub
        Protected Class ScrollHandleControl(Of ParentType As ScrollableList(Of ItemType))
            Inherits PictureBox
            Private varScrollValue As Double = 0
            Private Drag As Boolean
            Private varRoundedEnds As Boolean = True
            Private MouseY As Integer
            Private varMinHandleHeight As Integer = 20
            Private TopBottomImage() As Image = {My.Resources.ScrollHandleTop, My.Resources.ScrollHandleBottom}
            Private FillBrush As New SolidBrush(Color.White)
            Public Event ScrollValueChanged(Sender As Object, e As HandleScrollEventArgs)
            Public Event AutoAdjusted(Sender As Object, e As EventArgs)
            Public Property RoundedEnds As Boolean
                Get
                    Return varRoundedEnds
                End Get
                Set(value As Boolean)
                    varRoundedEnds = value
                    Invalidate()
                End Set
            End Property
            Protected Friend Property ScrollValue As Double
                Get
                    Return varScrollValue
                End Get
                Set(value As Double)
                    If value < 0 Then
                        value = 0
                    ElseIf value > 1 Then
                        value = 1
                    End If
                    varScrollValue = value
                    AutoAdjust()
                    RaiseEvent ScrollValueChanged(Me, New HandleScrollEventArgs(varScrollValue))
                End Set
            End Property
            Protected Friend Sub New()
                DoubleBuffered = True
                ResizeRedraw = True
                Size = New Size(17, 50)
            End Sub
            Public Shadows Property Parent As ParentType
                Get
                    Return DirectCast(MyBase.Parent, ParentType)
                End Get
                Set(value As ParentType)
                    MyBase.Parent = value
                    AutoAdjust()
                End Set
            End Property
            Protected Friend Sub AutoAdjust()
                If Parent IsNot Nothing Then
                    With Parent
                        Height = GetHandleHeightFromContentHeight(.ContentHeight)
                        Location = New Point(.Width - Width, GetLocationFromScrollValue(varScrollValue))
                    End With
                    RaiseEvent AutoAdjusted(Me, EventArgs.Empty)
                End If
            End Sub
            Protected Overrides Sub OnMouseDown(e As MouseEventArgs)
                MyBase.OnMouseDown(e)
                MouseY = Cursor.Position.Y - Top
                StartDrag()
            End Sub
            Protected Friend Sub StartDrag(Optional ByVal ResetMouseY As Boolean = False)
                Drag = True
                If ResetMouseY Then
                    MouseY = Parent.PointToScreen(New Point(0, Height \ 2)).Y
                End If
                DoDrag()
            End Sub
            Protected Friend Sub StopDrag()
                Drag = False
            End Sub
            Protected Friend Sub DoDrag()
                If Drag Then
                    Dim NewTop As Integer = Cursor.Position.Y - MouseY
                    If NewTop < 0 Then
                        NewTop = 0
                    ElseIf NewTop > (Parent.Height - Height) Then
                        NewTop = Parent.Height - Height
                    End If
                    ScrollValue = GetScrollValueFromLocation(NewTop)
                End If
            End Sub
            Protected Overrides Sub OnMouseUp(e As MouseEventArgs)
                StopDrag()
            End Sub
            Protected Overrides Sub OnMouseMove(e As MouseEventArgs)
                DoDrag()
            End Sub
            Protected Overrides Sub OnPaint(pe As PaintEventArgs)
                MyBase.OnPaint(pe)
                With pe.Graphics
                    .SetClip(ClientRectangle)
                    Dim SrcRect As New Rectangle(New Point(0, 0), New Size(10, 7))
                    Dim DrawRectTop As New Rectangle(New Point(Width - 10, 0), New Size(10, 7))
                    Dim DrawRectBottom As New Rectangle(New Point(Width - 10, Height - 7), New Size(10, 7))
                    If Height > 14 Then
                        Dim FillRect As Rectangle = New Rectangle(New Point(Width - 10, 7), New Size(10, Height - 14))
                        .FillRectangle(FillBrush, FillRect)
                    End If
                    If varRoundedEnds Then
                        .DrawImage(TopBottomImage(0), DrawRectTop, SrcRect, GraphicsUnit.Pixel)
                        .DrawImage(TopBottomImage(1), DrawRectBottom, SrcRect, GraphicsUnit.Pixel)
                    Else
                        .FillRectangles(FillBrush, {DrawRectTop, DrawRectBottom})
                    End If
                End With
            End Sub
            Private Function GetScrollValueFromLocation(ByVal HandleTop As Integer) As Double
                Dim Difference As Integer = Parent.Height - Height
                If Difference = 0 Then Return 0
                Return HandleTop / Difference
            End Function
            Protected Friend Function GetLocationFromScrollValue(ByVal ScrollValue As Double) As Integer
                Return CInt(ScrollValue * (Parent.Height - Height))
            End Function
            Protected Friend Function GetHandleHeightFromContentHeight(ByVal ContentHeight As Integer) As Integer
                Dim Ret As Integer = CInt((Parent.Height / ContentHeight) * Parent.Height)
                If Ret < varMinHandleHeight Then Return varMinHandleHeight
                Return Ret
            End Function
        End Class
        Protected Class HandleScrollEventArgs
            Inherits EventArgs
            Public ScrollValue As Double
            Public Sub New(ByVal ScrollValue As Double)
                Me.ScrollValue = ScrollValue
            End Sub
        End Class
    End Class
    Public Class PropertyList
        Inherits ScrollableList(Of ComponentViewItem)
        Public Shadows ReadOnly Property ListContainer As ListContainerControl
            Get
                Return MyBase.ListContainer
            End Get
        End Property
        Public Sub New()
            ItemSpacing = 0
            BackColor = ColorHelper.Multiply(ApplicationBackColor, 0.7)
        End Sub
    End Class
    Public Class AutoSizedContainer
        Inherits PictureBox
        Private varInnerControl As Control
        Private varHeaderHeight As Integer = 32
        Private varPaddingLeft As Integer = 5
        Private varPaddingRight As Integer = 5
        Private varPaddingTop As Integer = 5
        Private varPaddingBottom As Integer = 5
        Private varTextPadding As Integer = 5
        Private varBorderWidth As Integer = 2
        Private varHeaderColor As Color = Color.FromArgb(230, 230, 230)
        Private varBorderColor As Color = ApplicationBackColor
        Private varHeaderText As String = "HEADER TEXT"
        ' Private varHeaderFontSize As Single = 8
        Private varHeaderForeColor As Color = Color.FromArgb(50, 50, 50)
        Private varHeaderControls As New List(Of ContainerHeaderControl)
        Private varFont As Font = RalewayFonts.GetFont(RalewayFontCollection.FontVariant.Regular, 10, FontStyle.Regular)
        Private varUseCompatibleTextRendering As Boolean = True
        Private FillBrush As New SolidBrush(Color.White)
        Private varTextSize As Size = TextRenderer.MeasureText(varHeaderText, varFont)
        Public Event HeaderPaint(Sender As Object, e As PaintEventArgs)
        Public Sub New()
            DoubleBuffered = True
            BackColor = ApplicationBackColor
        End Sub
        Public Property HeaderHeight As Integer
            Get
                Return varHeaderHeight
            End Get
            Set(value As Integer)
                varHeaderHeight = value
                Invalidate()
            End Set
        End Property
        Public Property TextPadding As Integer
            Get
                Return varTextPadding
            End Get
            Set(value As Integer)
                varTextPadding = value
                Invalidate()
            End Set
        End Property
        Public Property UserCompatibleTextRendering As Boolean
            Get
                Return varUseCompatibleTextRendering
            End Get
            Set(value As Boolean)
                varUseCompatibleTextRendering = value
            End Set
        End Property
        Public Property BorderWidth As Integer
            Get
                Return varBorderWidth
            End Get
            Set(value As Integer)
                varBorderWidth = value
                Invalidate()
            End Set
        End Property
        Public Shadows Property Padding As Padding
            Get
                Return New Padding(varPaddingLeft, varPaddingTop, varPaddingRight, varPaddingBottom)
            End Get
            Set(value As Padding)
                varPaddingLeft = value.Left
                varPaddingTop = value.Top
                varPaddingRight = value.Right
                varPaddingBottom = value.Bottom
                AutoAdjust()
            End Set
        End Property
        Public Shadows Property BackColor As Color
            Get
                Return MyBase.BackColor
            End Get
            Set(value As Color)
                varBorderColor = value
                MyBase.BackColor = value
            End Set
        End Property
        Public Property HeaderText As String
            Get
                Return varHeaderText
            End Get
            Set(value As String)
                varHeaderText = value
                varTextSize = TextRenderer.MeasureText(varHeaderText, varFont)
                Invalidate()
            End Set
        End Property
        Public Overrides Property Font As Font
            Get
                Return varFont
            End Get
            Set(value As Font)
                If varFont IsNot Nothing Then
                    varFont.Dispose()
                End If
                varFont = value
                varTextSize = TextRenderer.MeasureText(varHeaderText, varFont)
            End Set
        End Property
        Public Shadows Property ForeColor As Color
            Get
                Return varHeaderForeColor
            End Get
            Set(value As Color)
                varHeaderForeColor = value
            End Set
        End Property
        Public Property InnerControl As Control
            Get
                Return varInnerControl
            End Get
            Set(value As Control)
                varInnerControl = value
                With varInnerControl
                    .Parent = Me
                End With
                AutoAdjust()
            End Set
        End Property
        Public ReadOnly Property HeaderControls As List(Of ContainerHeaderControl)
            Get
                Return varHeaderControls
            End Get
        End Property
        Public Sub AddHeaderControl(NormalHoverPressImages() As Image, ClickAction As Action(Of Object, MouseEventArgs))
            Dim HeaderRect As Rectangle = GetHeaderRectangle()
            Dim NewControl As New ContainerHeaderControl(Me, NormalHoverPressImages, ClickAction)
            With NewControl
                .BackColor = varHeaderColor
                .Size = New Size(HeaderRect.Height, HeaderRect.Height)
                If varHeaderControls.Count = 0 Then
                    .Location = New Point(HeaderRect.Width - .Width, 0)
                Else
                    .Location = New Point(varHeaderControls.Last.Left - .Width, 0)
                End If
                .Show()
            End With
            varHeaderControls.Add(NewControl)
        End Sub
        Private Sub RepositionHeaderControls()
            If varHeaderControls.Count > 0 Then
                Dim iLast As Integer = varHeaderControls.Count - 1
                Dim HeaderRect As Rectangle = GetHeaderRectangle()
                For i As Integer = 0 To iLast
                    With varHeaderControls(i)
                        .Size = New Size(HeaderRect.Height, HeaderRect.Height)
                        .Location = New Point(HeaderRect.Width - (i + 1) * .Width, 0)
                    End With
                Next
            End If
        End Sub
        Public Sub SetTotalSize(ByVal TotalSize As Size)
            varInnerControl.Size = GetInnerControlSizeFromTotalSize(TotalSize)
            AutoAdjust()
        End Sub
        Private Sub AutoAdjust()
            If varInnerControl IsNot Nothing Then
                varInnerControl.Location = New Point(varPaddingLeft + varBorderWidth, varHeaderHeight + varPaddingTop)
                Size = New Size(varPaddingLeft + varPaddingRight + 2 * varBorderWidth + varInnerControl.Width, varHeaderHeight + varPaddingTop + varPaddingBottom + varBorderWidth + varInnerControl.Height)
            End If
        End Sub
        Private Function GetInnerRectangle(InnerControlSize As Size) As Rectangle
            Return New Rectangle(New Point(varBorderWidth, varHeaderHeight), New Size(InnerControlSize.Width + varPaddingLeft + varPaddingRight, InnerControlSize.Height + varPaddingTop + varPaddingBottom))
        End Function
        Private Function GetInnerControlSizeFromTotalSize(TotalSize As Size) As Size
            Return New Size(TotalSize.Width - (2 * varBorderWidth + varPaddingLeft + varPaddingRight), TotalSize.Height - (varHeaderHeight + varPaddingTop + varPaddingBottom + varBorderWidth))
        End Function
        Private Function GetHeaderRectangle() As Rectangle
            Return New Rectangle(Point.Empty, New Size(Width, varHeaderHeight))
        End Function
        Private Function GetHeaderTextRectangle() As Rectangle
            Dim RectTop As Integer = (varHeaderHeight - varTextSize.Height) \ 2 + 1
            Return New Rectangle(New Point(varTextPadding, RectTop), varTextSize)
        End Function
        Protected Overrides Sub OnSizeChanged(e As EventArgs)
            MyBase.OnSizeChanged(e)
            varTextSize = TextRenderer.MeasureText(varHeaderText, varFont)
            RepositionHeaderControls()
        End Sub
        Protected Overrides Sub OnLocationChanged(e As EventArgs)
            Dim RefreshRect As New Rectangle(New Point(-Left, 0), New Size(10, Height))
            Invalidate(RefreshRect, True)
            Update()
            MyBase.OnLocationChanged(e)
        End Sub
        Protected Overrides Sub OnClick(e As EventArgs)
            MyBase.OnClick(e)
            Focus()
        End Sub
        Protected Overrides Sub OnPaint(pe As PaintEventArgs)
            MyBase.OnPaint(pe)
            With pe.Graphics
                .Clip = New Region(ClientRectangle)
                If varInnerControl IsNot Nothing Then
                    .ExcludeClip(GetInnerRectangle(varInnerControl.Size))
                End If
                FillBrush.Color = varBorderColor
                .FillRectangle(FillBrush, ClientRectangle)
                FillBrush.Color = varHeaderColor
                Dim HeaderRectangle As Rectangle = GetHeaderRectangle()
                .FillRectangle(FillBrush, HeaderRectangle)
                FillBrush.Color = varHeaderForeColor
                .TextRenderingHint = Drawing.Text.TextRenderingHint.SystemDefault
                Dim TextRect As Rectangle = GetHeaderTextRectangle()
                .Clip = New Region(TextRect)
                If varUseCompatibleTextRendering Then
                    TextRenderer.DrawText(pe.Graphics, varHeaderText, varFont, GetHeaderTextRectangle(), varHeaderForeColor)
                Else
                    .DrawString(varHeaderText, varFont, FillBrush, GetHeaderTextRectangle())
                End If
                .SetClip(HeaderRectangle)
                RaiseEvent HeaderPaint(Me, pe)
            End With
        End Sub
        Public Class ContainerHeaderControl
            Inherits PictureBox
            Private varClickMethod As Action(Of Object, MouseEventArgs)
            Private varImageStates(2) As Image
            Private varState As HeaderControlState = HeaderControlState.Normal
            Protected Friend Sub New(Parent As AutoSizedContainer, NormalHoverPressImages() As Image, ClickAction As Action(Of Object, MouseEventArgs))
                DoubleBuffered = True
                Hide()
                varClickMethod = ClickAction
                BackgroundImageLayout = ImageLayout.None
                varImageStates = NormalHoverPressImages
                SetState(varState)
                Me.Parent = Parent
            End Sub
            Public Enum HeaderControlState
                Normal
                Hover
                Press
            End Enum
            Public ReadOnly Property State As HeaderControlState
                Get
                    Return varState
                End Get
            End Property
            Protected Friend Sub SetState(ByVal State As HeaderControlState)
                varState = State
                BackgroundImage = Image(State)
            End Sub
            Public Shadows Property Image(ByVal State As HeaderControlState) As Image
                Get
                    Select Case State
                        Case HeaderControlState.Normal
                            Return varImageStates(0)
                        Case HeaderControlState.Hover
                            Return varImageStates(1)
                        Case Else
                            Return varImageStates(2)
                    End Select
                End Get
                Set(value As Image)
                    Select Case State
                        Case HeaderControlState.Normal
                            varImageStates(0) = value
                        Case HeaderControlState.Hover
                            varImageStates(1) = value
                        Case HeaderControlState.Press
                            varImageStates(2) = value
                    End Select
                    SetState(varState)
                End Set
            End Property
            Public Shadows Property Parent As AutoSizedContainer
                Get
                    Return DirectCast(MyBase.Parent, AutoSizedContainer)
                End Get
                Set(value As AutoSizedContainer)
                    MyBase.Parent = value
                End Set
            End Property
            Protected Overrides Sub OnMouseEnter(e As EventArgs)
                MyBase.OnMouseEnter(e)
                SetState(HeaderControlState.Hover)
            End Sub
            Protected Overrides Sub OnMouseLeave(e As EventArgs)
                MyBase.OnMouseLeave(e)
                If Not varState = HeaderControlState.Press Then
                    SetState(HeaderControlState.Normal)
                End If
            End Sub
            Protected Overrides Sub OnMouseDown(e As MouseEventArgs)
                MyBase.OnMouseDown(e)
                Focus()
                SetState(HeaderControlState.Press)
            End Sub
            Protected Overrides Sub OnMouseUp(e As MouseEventArgs)
                MyBase.OnMouseUp(e)
                With e.Location
                    If .X >= 0 AndAlso .X <= Width AndAlso .Y >= 0 AndAlso .Y <= Height Then
                        SetState(HeaderControlState.Hover)
                        varClickMethod.Invoke(Me, e)
                    Else
                        SetState(HeaderControlState.Normal)
                    End If
                End With
            End Sub
        End Class
    End Class
    Public Class ValueChangedEventArgs
        Inherits EventArgs
        Public Value As ScrollableListItemValue
        Public Sub New(Value As ScrollableListItemValue)
            Me.Value = Value
        End Sub
    End Class
    Public MustInherit Class ScrollableListItem
        Inherits PictureBox
        Private varValue As ScrollableListItemValue = ScrollableListItemValue.Empty
        Protected DefaultHeight As Integer = 30
        Protected varIsActive, varHovering, varIsSelected As Boolean
        Protected varItemIndex As Integer
        Private varUseCompatibleTextRendering As Boolean = True
        Public Event ActiveChanged(Sender As Object, e As ItemActiveEventArgs)
        Public Event SelectedChanged(Sender As Object, e As ItemSelectedEventArgs)
        Public Event HoveringChanged(Sender As Object, e As ItemHoveringEventArgs)
        Public Event ValueChanged(Sender As Object, e As ValueChangedEventArgs)
        Public MustOverride ReadOnly Property DefaultValue As ScrollableListItemValue
        Public MustOverride Sub ApplyArguments(Arguments As Object)
        Public Sub New()
            varValue = DefaultValue
        End Sub
        Public Property UseCompatibleTextRendering As Boolean
            Get
                Return varUseCompatibleTextRendering
            End Get
            Set(value As Boolean)
                varUseCompatibleTextRendering = value
                Invalidate()
            End Set
        End Property
        Public Property Value As ScrollableListItemValue
            Get
                Return varValue
            End Get
            Set(value As ScrollableListItemValue)
                varValue = value
                OnValueChanged(New ValueChangedEventArgs(varValue))
            End Set
        End Property
        Public Property Index As Integer
            Get
                Return varItemIndex
            End Get
            Set(value As Integer)
                varItemIndex = value
                ' TODO: Change order
            End Set
        End Property
        Public Overridable Property IsActive As Boolean
            Get
                Return varIsActive
            End Get
            Set(value As Boolean)
                If value <> varIsActive Then
                    varIsActive = value
                    OnActiveChanged(New ItemActiveEventArgs(varIsActive))
                End If
            End Set
        End Property
        Public Property Hovering As Boolean
            Get
                Return varHovering
            End Get
            Set(value As Boolean)
                varHovering = value
                OnHoveringChanged(New ItemHoveringEventArgs(value))
            End Set
        End Property
        Public Property IsSelected As Boolean
            Get
                Return varIsSelected
            End Get
            Set(value As Boolean)
                If value <> varIsSelected Then
                    varIsSelected = value
                    OnSelectedChanged(New ItemSelectedEventArgs(varIsSelected))
                End If
            End Set
        End Property
        'Protected Friend Sub SetIndex(ByVal Index As Integer)
        '    varItemIndex = Index
        'End Sub
        Protected Overridable Sub OnActiveChanged(e As ItemActiveEventArgs)
            RaiseEvent ActiveChanged(Me, e)
            Invalidate()
        End Sub
        Protected Overridable Sub OnValueChanged(e As ValueChangedEventArgs)
            RaiseEvent ValueChanged(Me, e)
            Invalidate()
        End Sub
        Protected Overridable Sub OnSelectedChanged(e As ItemSelectedEventArgs)
            RaiseEvent SelectedChanged(Me, e)
            Invalidate()
        End Sub
        Protected Overridable Sub OnHoveringChanged(e As ItemHoveringEventArgs)
            RaiseEvent HoveringChanged(Me, e)
            Invalidate()
        End Sub
        Protected Overrides Sub OnMouseEnter(e As EventArgs)
            Hovering = True
            MyBase.OnMouseEnter(e)
        End Sub
        Protected Overrides Sub OnMouseLeave(e As EventArgs)
            Hovering = False
            MyBase.OnMouseEnter(e)
        End Sub
        Protected Overrides Sub OnMouseMove(e As MouseEventArgs)
            If Not Hovering Then Hovering = True
            MyBase.OnMouseMove(e)
        End Sub
        Public Shadows Property Parent As ListContainerControl
            Get
                Return DirectCast(MyBase.Parent, ListContainerControl)
            End Get
            Set(value As ListContainerControl)
                If MyBase.Parent IsNot Nothing Then
                    RemoveHandler Parent.SizeChanged, AddressOf Parent_SizeChangedEventHandler
                    RemoveHandler Parent.DisposeRequested, AddressOf Parent_DisposeRequested
                End If
                MyBase.Parent = value
                AddHandler MyBase.Parent.SizeChanged, AddressOf Parent_SizeChangedEventHandler
                AddHandler Parent.DisposeRequested, AddressOf Parent_DisposeRequested
            End Set
        End Property
        Private Sub Parent_DisposeRequested(Sender As Object, e As EventArgs)
            Dispose(True)
        End Sub
        Protected Overridable Sub Parent_SizeChangedEventHandler(Sender As Object, e As EventArgs)
            If Parent IsNot Nothing Then
                Width = Parent.Width
            End If
        End Sub
        Protected Overrides Sub OnMouseWheel(e As MouseEventArgs)
            MyBase.OnMouseWheel(e)
            Parent.Scroll(e)
        End Sub
        Protected Overrides Sub OnClick(e As EventArgs)
            MyBase.OnClick(e)
            Focus()
        End Sub
        Protected Overrides Sub Dispose(disposing As Boolean)
            If MyBase.Parent IsNot Nothing Then
                RemoveHandler Parent.SizeChanged, AddressOf Parent_SizeChangedEventHandler
                RemoveHandler Parent.DisposeRequested, AddressOf Parent_DisposeRequested
            End If
            MyBase.Dispose(disposing)
        End Sub
        Public Class ItemActiveEventArgs
            Inherits EventArgs
            Public Active As Boolean
            Public Sub New(Active As Boolean)
                Me.Active = Active
            End Sub
        End Class
        Public Class ItemHoveringEventArgs
            Inherits EventArgs
            Public Hovering As Boolean
            Public Sub New(Hovering As Boolean)
                Me.Hovering = Hovering
            End Sub
        End Class
        Public Class ItemSelectedEventArgs
            Inherits EventArgs
            Public Selected As Boolean
            Public Sub New(Selected As Boolean)
                Me.Selected = Selected
            End Sub
        End Class
    End Class
    Public Class ScrollableListItemValue
        Private varData As Object = Nothing
        Protected Sub New(Data As Object)
            SetData(Data)
        End Sub
        Public Overridable Property Data As Object
            Get
                Return varData
            End Get
            Set(value As Object)
                varData = value
            End Set
        End Property
        Protected Sub SetData(Data As Object)
            varData = Data
        End Sub
        ''' <summary>
        ''' Creates a new ScrollableListItemValue instance with no data.
        ''' </summary>
        Public Shared ReadOnly Property Empty As ScrollableListItemValue
            Get
                Return New ScrollableListItemValue(Nothing)
            End Get
        End Property
    End Class
    Public Class PropertyValueInfo
        Inherits ScrollableListItemValue
        Private varTitle, varContent, varInfo As String
        Public Sub New(ByVal Title As String, ByVal Content As String, ByVal Info As String, Optional Data As Object = Nothing)
            MyBase.New(Data)
            varTitle = Title
            varContent = Content
            varInfo = Info
        End Sub
    End Class
    Public Class ScrollableListLabelValue
        Inherits ScrollableListItemValue
        Private varText As String
        Public Sub New(ByVal Text As String, Optional Data As Object = Nothing)
            MyBase.New(Data)
            varText = Text
        End Sub
        Public ReadOnly Property Text As String
            Get
                Return varText
            End Get
        End Property
        Public Shared Shadows ReadOnly Property Empty As ScrollableListItemValue
            Get
                Return New ScrollableListLabelValue("")
            End Get
        End Property
    End Class
    Public Class ScrollableListLabel
        Inherits ScrollableListItem
        'Private WithEvents varLabel As New Label
        Private varText As String
        Private varFont As Font = RalewayFonts.GetFont(RalewayFontCollection.FontVariant.Light, 10, FontStyle.Regular)
        Private LinePen As New Pen(ColorHelper.FillRemainingRGB(ApplicationBackColor, 0.2))
        Private ActiveBrush As New SolidBrush(Color.White)
        Private varDarkColor As Color = ColorHelper.Multiply(ApplicationBackColor, 0.8)
        Private CircleSize As New Size(8, 8)
        Private CircleRect As Rectangle
        Private TextSize As Size
        Public Sub New()
            LinePen.DashStyle = Drawing2D.DashStyle.Dot
            Size = New Size(10, DefaultHeight)
            BackColor = ApplicationBackColor
            ForeColor = Color.White
            UseCompatibleTextRendering = False
        End Sub
        Public Shadows Property Font As Font
            Get
                Return varFont
            End Get
            Set(value As Font)
                If varFont IsNot Nothing Then
                    varFont.Dispose()
                End If
                varFont = value
                TextSize = TextRenderer.MeasureText(varText, varFont)
                Invalidate(False)
            End Set
        End Property
        Public Shadows Property Value As ScrollableListLabelValue
            Get
                Return DirectCast(MyBase.Value, ScrollableListLabelValue)
            End Get
            Set(value As ScrollableListLabelValue)
                varText = value.Text
                TextSize = TextRenderer.MeasureText(varText, varFont)
                MyBase.Value = value
            End Set
        End Property
        Public Overrides ReadOnly Property DefaultValue As ScrollableListItemValue
            Get
                Return New ScrollableListLabelValue("Default label text")
            End Get
        End Property
        Protected Overrides Sub OnMouseUp(e As MouseEventArgs)
            MyBase.OnMouseUp(e)
            If FormatConverter.PointIsInRectangle(PointToClient(MousePosition), CircleRect) Then
                If Not IsActive Then
                    IsActive = True
                Else
                    IsActive = False
                End If
            Else
                IsSelected = True
            End If
        End Sub
        Protected Overrides Sub OnSizeChanged(e As EventArgs)
            MyBase.OnSizeChanged(e)
            Dim RectTop As Integer = (Height - CircleSize.Height) \ 2
            CircleRect = New Rectangle(New Point(4, RectTop), CircleSize)
        End Sub
        Private Function GetArrowPath(ByVal UpperLeft As Point) As Drawing2D.GraphicsPath
            With UpperLeft
                Dim Ret As New Drawing2D.GraphicsPath
                Dim ArrowThickness As Integer = 6
                Dim ArrowHalfHeight As Integer = 6
                Dim Points() As Point = {New Point(.X, .Y), New Point(.X + ArrowThickness, .Y), New Point(.X + ArrowThickness * 2, .Y + ArrowHalfHeight), New Point(.X + ArrowThickness - 1, .Y + ArrowHalfHeight * 2 + 1), New Point(.X - 1, .Y + ArrowHalfHeight * 2 + 1), New Point(.X + ArrowThickness, .Y + ArrowHalfHeight)}
                Ret.AddPolygon(Points)
                Return Ret
            End With
        End Function
        Protected Overrides Sub OnPaint(e As PaintEventArgs)
            With e.Graphics
                ActiveBrush.Color = Color.White
                If IsSelected Then
                    .FillPath(ActiveBrush, GetArrowPath(New Point(Width - 15, 8)))
                ElseIf Hovering Then
                    ActiveBrush.Color = varDarkColor
                    .FillPath(ActiveBrush, GetArrowPath(New Point(Width - 15, 8)))
                End If
                If Not IsActive Then
                    ActiveBrush.Color = varDarkColor
                Else
                    ActiveBrush.Color = Color.White
                End If
                .TextRenderingHint = Drawing.Text.TextRenderingHint.SystemDefault
                .SmoothingMode = Drawing2D.SmoothingMode.AntiAlias
                .FillEllipse(ActiveBrush, CircleRect)
                .SmoothingMode = Drawing2D.SmoothingMode.None
                .DrawLine(LinePen, New Point(0, Height - 1), New Point(Width, Height - 1))
                Dim TextRect As New Rectangle(New Point(20, (Height - TextSize.Height) \ 2), TextSize)
                If UseCompatibleTextRendering Then
                    TextRenderer.DrawText(e.Graphics, varText, varFont, TextRect, ForeColor)
                Else
                    ActiveBrush.Color = Color.White
                    .DrawString(varText, varFont, ActiveBrush, TextRect)
                End If
            End With
        End Sub
        ''' <summary>
        ''' This method is used to specify arguments that could not be passed to the constructor because the calling code (such as that in the ScrollableList class) uses type parameters to determine which type of items to create.
        ''' </summary>
        ''' <param name="Arguments">String; the item's text</param>
        Public Overrides Sub ApplyArguments(Arguments As Object)
            Value = New ScrollableListLabelValue(DirectCast(Arguments, String))
        End Sub
    End Class
    Public Class HeaderControl
        Inherits PictureBox
        Private Shared AllViewImages() As Image = {My.Resources.OverviewIconDefault, My.Resources.Templates, My.Resources.calendar, My.Resources.Routines, My.Resources.Tasks}
        Private Shared AllViewImagesHover() As Image = {My.Resources.OverviewIconDefault, My.Resources.Templates, My.Resources.calendar, My.Resources.Routines, My.Resources.Tasks}
        Private Shared SeparatorImage As Image = My.Resources.HeaderViewSeparator
        Private Shared ImageSize As New Size(32, 32)
        Private varCurrentDirectory() As Views = {Views.Overview}
        Private varHoveredIndex As Integer = -1
        Private varViewSpacing As Integer = 5
        Private ExitRect, MinimizeRect As Rectangle
        Public Event ViewClicked(Sender As Object, e As HeaderViewEventArgs)
        Public Event MinimizeClicked(Sender As Object, e As MouseEventArgs)
        Public Event ExitClicked(Sender As Object, e As MouseEventArgs)
        Public Sub New()
            DoubleBuffered = True
            Size = New Size(794, 65)
            Dim ControlButtonSize As New Size(24, 24)
            Dim Margin As Integer = (Height - ControlButtonSize.Height) \ 2
            ExitRect = New Rectangle(New Point(Width - Margin - ControlButtonSize.Width, Margin), ControlButtonSize)
            MinimizeRect = New Rectangle(New Point(ExitRect.X - ControlButtonSize.Width - 10, Margin), ControlButtonSize)
            BackgroundImageLayout = ImageLayout.None
        End Sub
        Public ReadOnly Property ViewImages As Image()
            Get
                Return AllViewImages
            End Get
        End Property
        Public Property CurrentDirectiry As Views()
            Get
                Return varCurrentDirectory
            End Get
            Set(value As Views())
                varCurrentDirectory = value
                Invalidate()
            End Set
        End Property
        Private Property HoveredIndex As Integer
            Get
                Return varHoveredIndex
            End Get
            Set(value As Integer)
                varHoveredIndex = value
                Invalidate()
            End Set
        End Property
        Protected Overrides Sub OnMouseMove(e As MouseEventArgs)
            MyBase.OnMouseMove(e)
            Dim iLast As Integer = varCurrentDirectory.Count - 1
            If iLast >= 0 Then
                Dim Match As Boolean = False
                Dim SeparatorSize As Size = SeparatorImage.Size
                Dim ImageTop As Integer = (Height - ImageSize.Height) \ 2
                For i As Integer = 0 To iLast
                    Dim ImageRect As New Rectangle(New Point(ImageTop + i * (ImageSize.Width + (SeparatorSize.Width + 2 * varViewSpacing)), ImageTop), ImageSize)
                    If FormatConverter.PointIsInRectangle(e.Location, ImageRect) Then
                        Match = True
                        Cursor = Cursors.Hand
                        HoveredIndex = i
                        Exit For
                    End If
                Next
                If Not Match Then
                    HoveredIndex = -1
                    If FormatConverter.PointIsInRectangle(e.Location, MinimizeRect) Then
                        Cursor = Cursors.Hand
                    ElseIf FormatConverter.PointIsInRectangle(e.Location, ExitRect) Then
                        Cursor = Cursors.Hand
                    Else
                        Cursor = Cursors.Default
                    End If
                End If
            End If
        End Sub
        Protected Overrides Sub OnMouseUp(e As MouseEventArgs)
            MyBase.OnMouseUp(e)
            Focus()
            If HoveredIndex <> -1 Then
                RaiseEvent ViewClicked(Me, New HeaderViewEventArgs(CurrentDirectiry, CurrentDirectiry(HoveredIndex)))
            ElseIf FormatConverter.PointIsInRectangle(e.Location, MinimizeRect) Then
                RaiseEvent MinimizeClicked(Me, e)
            ElseIf FormatConverter.PointIsInRectangle(e.Location, ExitRect) Then
                RaiseEvent ExitClicked(Me, e)
            End If
        End Sub
        Protected Overrides Sub OnPaint(pe As PaintEventArgs)
            MyBase.OnPaint(pe)
            With pe.Graphics
                .SmoothingMode = Drawing2D.SmoothingMode.None
                .FillRectangle(ApplicationGradientBrush, ClientRectangle)
                Dim iLast As Integer = CurrentDirectiry.Count - 1
                If iLast >= 0 Then
                    Dim SeparatorSize As Size = SeparatorImage.Size
                    Dim ImageTop As Integer = (Height - ImageSize.Height) \ 2
                    For i As Integer = 0 To iLast
                        Dim DrawRect As New Rectangle(New Point(ImageTop + i * (ImageSize.Width + (SeparatorSize.Width + 2 * varViewSpacing)), ImageTop), ImageSize)
                        If i = HoveredIndex Then ' Draw hover image
                            .DrawImage(AllViewImages(varCurrentDirectory(i)), DrawRect)
                        Else ' Draw default image
                            .DrawImage(AllViewImagesHover(varCurrentDirectory(i)), DrawRect)
                        End If
                        If Not i = iLast Then
                            Dim SeparatorLocation As New Point(ImageTop + (i + 1) * (ImageSize.Width + SeparatorSize.Width + 2 * varViewSpacing) - (SeparatorSize.Width + varViewSpacing), (Height - SeparatorSize.Height) \ 2)
                            .DrawImage(SeparatorImage, New Rectangle(SeparatorLocation, SeparatorSize))
                        End If
                    Next
                End If
                .DrawImage(My.Resources._Exit, ExitRect)
                .DrawImage(My.Resources.Minimize, MinimizeRect.Location)
            End With
        End Sub
        Public Enum Views As Integer
            Invalid = -1
            Overview = 0
            TemplateBrowser = 1
            ScheduleBrowser = 2
            RoutineBrowser = 3
            TaskBrowser = 4
        End Enum
    End Class
    Public Class HeaderViewEventArgs
        Inherits EventArgs
        Private varClickTarget, varCurrentDirectory() As HeaderControl.Views
        Public Sub New(ByVal CurrentDirectory As HeaderControl.Views(), ByVal ClickTarget As HeaderControl.Views)
            varCurrentDirectory = CurrentDirectory
            Me.ClickTarget = ClickTarget
        End Sub
        Public ReadOnly Property CurrentDirectory As HeaderControl.Views()
            Get
                Return varCurrentDirectory
            End Get
        End Property
        Public Property ClickTarget As HeaderControl.Views
            Get
                Return varClickTarget
            End Get
            Set(value As HeaderControl.Views)
                varClickTarget = value
            End Set
        End Property
    End Class
#Region "MultiTabWindow"
    Public NotInheritable Class MultiTabWindow
        Inherits Panel
#Region "Fields"
        Private TabList As List(Of Tab)
        Private Shared ZeroPoint As New Point(0, 0)
        Private SelectedIndex As Integer = 0
        Private ScaleXY() As Boolean = {True, False}
#End Region
#Region "Events"
        Public Event TabChanged(Sender As MultiTabWindow)
        Public Event TabCountChanged(Sender As MultiTabWindow)
        Public Event TabResized(Sender As MultiTabWindow, Tab As Tab)
#End Region
#Region "Public properties"
        Public Overloads Property ScaleTabsToSize As Boolean()
            Get
                Return ScaleXY
            End Get
            Set(value As Boolean())
                ScaleXY(0) = value(0)
                ScaleXY(1) = value(1)
            End Set
        End Property
        Public Enum TabSize
            Horizontal
            Vertical
        End Enum
        Public Overloads Property ScaleTabsToSize(ByVal Component As TabSize) As Boolean
            Get
                Select Case Component
                    Case TabSize.Horizontal
                        Return ScaleXY(0)
                    Case Else
                        Return ScaleXY(1)
                End Select
            End Get
            Set(value As Boolean)
                Select Case Component
                    Case TabSize.Horizontal
                        ScaleXY(0) = value
                    Case Else
                        ScaleXY(1) = value
                End Select
            End Set
        End Property
        Public ReadOnly Property Tab(ByVal Index As Integer) As Tab
            Get
                Return TabList(Index)
            End Get
        End Property
        Public ReadOnly Property Tabs As List(Of Tab)
            Get
                Return TabList
            End Get
        End Property
        Public Property Index As Integer
            Get
                Return SelectedIndex
            End Get
            Set(value As Integer)
                If SelectedIndex >= 0 AndAlso SelectedIndex < TabList.Count Then
                    TabList(SelectedIndex).Hide()
                End If
                If value >= 0 AndAlso value < TabList.Count Then
                    SelectedIndex = value
                    With TabList(SelectedIndex)
                        .Show()
                        .BringToFront()
                    End With
                Else
                    SelectedIndex = -1
                End If
            End Set
        End Property
        Private Enum WindowProperty
            Tab
            ScaleTabs
            TabCount
        End Enum
        Public ReadOnly Property CurrentTab As Tab
            Get
                If SelectedIndex >= 0 AndAlso SelectedIndex < TabList.Count Then
                    Return TabList(SelectedIndex)
                Else
                    Return Nothing
                End If
            End Get
        End Property
#End Region
#Region "Public methods"
        Public Sub New(Parent As Control)
            TabList = New List(Of Tab)
            With Me
                .Hide()
                .DoubleBuffered = True
                .Parent = Parent
                .Location = ZeroPoint
                .ClientSize = Parent.ClientSize
                .Show()
            End With
            AddHandler Parent.Resize, AddressOf OnParentResize
        End Sub
        Public Sub ResetAll(Optional ArgumentAll As Object = Nothing)
            For Each T As Tab In TabList
                T.ResetTab(ArgumentAll)
            Next
        End Sub
        Public Overloads Sub AddTab(Tab As Tab)
            AddTab(Tab, TabList.Count)
        End Sub
        Public Overloads Sub AddTab(Controls As List(Of Control))
            Dim iLast As Integer = Controls.Count - 1
            Dim NT As New Tab(Me)
            With NT
                For i As Integer = 0 To iLast
                    .Controls.Add(Controls(i))
                Next
                .ListIndex = TabList.Count
            End With
            AddTab(NT)
        End Sub
        Public Overloads Sub AddTab(Controls() As Control)
            Dim iLast As Integer = Controls.Count - 1
            Dim NT As New Tab(Me)
            With NT
                For i As Integer = 0 To iLast
                    .Controls.Add(Controls(i))
                Next
                .ListIndex = TabList.Count
            End With
            AddTab(NT)
        End Sub
        Public Overloads Sub AddTab(Tab As Tab, AtIndex As Integer)
            With Tab
                .Hide()
                .Location = ZeroPoint
                .Size = ClientSize
                .ListIndex = AtIndex
            End With
            Dim iLast As Integer
            With TabList
                .Insert(AtIndex, Tab)
                iLast = .Count - 1
            End With
            If AtIndex < iLast Then
                For i As Integer = AtIndex + 1 To iLast
                    TabList(i).ListIndex = i
                Next
            End If
        End Sub
        Public Overloads Sub RemoveTab(Index As Integer)
            If Index = SelectedIndex Then
                Me.Index -= 1
            End If
            TabList.RemoveAt(Index)
            Dim iLast As Integer = TabList.Count - 1
            If iLast >= Index Then
                For i As Integer = Index To TabList.Count - 1
                    TabList(i).ListIndex = i
                Next
            End If
        End Sub
        Public Sub ShowTab(ByVal Index As Integer)
            Me.Index = Index
        End Sub
#End Region
#Region "Private methods"
        Private Sub OnParentResize(Sender As Object, e As EventArgs)
            With Me
                .SuspendLayout()
                .ClientSize = Parent.ClientSize
                If .CurrentTab IsNot Nothing Then
                    .CurrentTab.RefreshLayout()
                End If
                .ResumeLayout()
            End With
        End Sub
        Protected Overrides Sub OnParentChanged(e As EventArgs)
            MyBase.OnParentChanged(e)
            With Me
                .Location = ZeroPoint
                .ClientSize = .Parent.ClientSize
            End With
            If CurrentTab IsNot Nothing Then
                CurrentTab.RefreshLayout()
            End If
        End Sub
#End Region
    End Class
    Public Class TabCancelEventArgs
        Inherits EventArgs
        Public Cancel As Boolean
        Public Sub New(Optional ByVal Cancel As Boolean = False)
            Me.Cancel = Cancel
        End Sub
    End Class
    ''' <summary>
    ''' A view or screen to be displayed in a MultiTabWindow, used for keeping several views within a single window.
    ''' </summary>
    Public Class Tab
        Inherits Control
        Private varScaleToParent As Boolean = True
        Private varRefreshLayoutOnShow As Boolean = True
        Protected Friend ListIndex As Integer = -1
        Public Event LayoutRefreshed(Sender As Object, e As EventArgs)
        Public Event TabClosing(Sender As Object, e As TabClosingEventArgs)
        Public Event TabClosed(Sender As Object, e As EventArgs)
        Public Event TabShown(Sender As Object, e As EventArgs)
        ''' <summary>
        ''' Gets or sets a value indicating whether or not this Tab should handle its parent's Resize event and resize itself accordingly.
        ''' Default = True.
        ''' </summary>
        Public Property ScaleToWindow As Boolean
            Get
                Return varScaleToParent
            End Get
            Set(value As Boolean)
                varScaleToParent = value
            End Set
        End Property
        ''' <summary>
        ''' Gets or sets a value specifying whether or not to automatically call the RefreshLayout method when the tab is shown (default = true).
        ''' </summary>
        Public Property RefreshLayoutOnShow As Boolean
            Get
                Return varRefreshLayoutOnShow
            End Get
            Set(value As Boolean)
                varRefreshLayoutOnShow = value
            End Set
        End Property
        ''' <summary>
        ''' If the ScaleToWindow property is set to true, but no new Resize event is raised by the parent, sets this tab's size to the ClientSize of its parent MultiTabWindow.
        ''' </summary>
        Public Overridable Sub RefreshLayout()
            OnLayoutRefreshed(New TabCancelEventArgs)
        End Sub
        ''' <summary>
        ''' Raises the LayoutRefreshed event.
        ''' </summary>
        ''' <param name="e">Contains information about this event, if any.</param>
        Protected Overridable Sub OnLayoutRefreshed(e As TabCancelEventArgs)
            If Not e.Cancel Then
                If varScaleToParent AndAlso Parent IsNot Nothing Then
                    ClientSize = Parent.ClientSize
                End If
                RaiseEvent LayoutRefreshed(Me, EventArgs.Empty)
            End If
        End Sub
        ''' <summary>
        ''' Shows this tab. This action should only be performed internally 
        ''' </summary>
        Protected Friend Overridable Shadows Sub Show()
            If varRefreshLayoutOnShow Then
                RefreshLayout()
            End If
            MyBase.Show()
            OnTabShown(EventArgs.Empty)
        End Sub
        Protected Overridable Sub OnTabShown(e As EventArgs)
            RaiseEvent TabShown(Me, e)
        End Sub
        ''' <summary>
        ''' If properly overridden in a class that inherits Tab, sets all variables and states to their initial values, erasing changes made to the Tab.
        ''' </summary>
        ''' <param name="Arguments">Optional parameter that may or may not be used by classes derived from Tab.</param>
        Public Overridable Sub ResetTab(Optional Arguments As Object = Nothing)

        End Sub
        ''' <summary>
        ''' Gets or sets the MultiTabWindow instance that contains this Tab.
        ''' </summary>
        Public Shadows Property Parent As MultiTabWindow
            Get
                Return DirectCast(MyBase.Parent, MultiTabWindow)
            End Get
            Set(value As MultiTabWindow)
                If Parent IsNot Nothing AndAlso ListIndex >= 0 Then
                    Parent.RemoveTab(ListIndex)
                End If
                MyBase.Parent = value
            End Set
        End Property
        ''' <summary>
        ''' Creates a new Tab instance and inserts it into the specified MultiTabWindow (at the last index). For complex (form-like) tabs, inherit this class instead.
        ''' </summary>
        ''' <param name="Parent">The MultiTabWindow instance to which this tab should be added.</param>
        Public Sub New(Parent As MultiTabWindow)
            Hide()
            DoubleBuffered = True
            SetStyle(ControlStyles.UserPaint, True)
            SetStyle(ControlStyles.OptimizedDoubleBuffer, True)
            SetStyle(ControlStyles.AllPaintingInWmPaint, True)
            UpdateStyles()
            Me.Parent = Parent
        End Sub
        ''' <summary>
        ''' Creates a new Tab instance. See the other overload. For complex (form-like) tabs, inherit this class instead.
        ''' </summary>
        Public Sub New()
            Hide()
            DoubleBuffered = True
            SetStyle(ControlStyles.UserPaint, True)
            SetStyle(ControlStyles.OptimizedDoubleBuffer, True)
            SetStyle(ControlStyles.AllPaintingInWmPaint, True)
            UpdateStyles()
        End Sub
        ''' <summary>
        ''' Closes the Tab, which may or may not result in disposal, depending on the state of the TabClosingEventArgs after it has been passed to the overridable method OnClosing(e as TabClosingEventArgs), in which the Tab's closing may be canceled.
        ''' </summary>
        ''' <param name="Dispose">Specifies whether or not to automatically release the resources used by this Tab after closing.</param>
        Public Sub Close(Optional Dispose As Boolean = True)
            Dim Args As New TabClosingEventArgs(False, Dispose)
            OnClosing(New TabClosingEventArgs)
        End Sub
        ''' <summary>
        ''' Raises the TabClosing event and calls the OnClosed method unless canceled.
        ''' </summary>
        ''' <param name="e">Contains information about how to proceed.</param>
        Protected Overridable Sub OnClosing(e As TabClosingEventArgs)
            RaiseEvent TabClosing(Me, e)
            If Not e.Cancel Then
                OnClosed(e)
            End If
        End Sub
        ''' <summary>
        ''' Raises the TabClosed event and calls Dispose(True), depending on the Dispose property of the TabClosingEventArgs passed to it.
        ''' </summary>
        ''' <param name="e">Contains information about whether or not to proceed. e.Cancel is not checked at this point.</param>
        Protected Overridable Sub OnClosed(e As TabClosingEventArgs)
            RaiseEvent TabClosed(Me, EventArgs.Empty)
            If e.Dispose Then
                Dispose(True)
            End If
        End Sub
        ''' <summary>
        ''' Raises the ParentChanged event and sets this Tab's size to the ClientSize of its parent.
        ''' </summary>
        Protected Overrides Sub OnParentChanged(e As EventArgs)
            MyBase.OnParentChanged(e)
            ClientSize = Parent.ClientSize
        End Sub
        Protected Overrides Sub Dispose(disposing As Boolean)
            MyBase.Dispose(disposing)
        End Sub
    End Class
    Public Class TabClosingEventArgs
        Inherits EventArgs
        Private DoCancel As Boolean
        Private DisposeOnClose As Boolean
        Public Property Cancel As Boolean
            Get
                Return DoCancel
            End Get
            Set(value As Boolean)
                DoCancel = value
            End Set
        End Property
        Public Property Dispose As Boolean
            Get
                Return DisposeOnClose
            End Get
            Set(value As Boolean)
                DisposeOnClose = value
            End Set
        End Property
        Public Sub New(Optional ByVal Cancel As Boolean = False, Optional ByVal Dispose As Boolean = True)
            DoCancel = Cancel
            DisposeOnClose = Dispose
        End Sub
    End Class
#End Region
    Public Class ItemsAddedEventArgs
        Inherits EventArgs
        Private varSelectedItems As IEnumerable(Of ChildComponentInformation)
        Public Sub New(SelectedItems As IEnumerable(Of ChildComponentInformation))
            varSelectedItems = SelectedItems
        End Sub
        Public ReadOnly Property SelectedItems As IEnumerable(Of ChildComponentInformation)
            Get
                Return varSelectedItems
            End Get
        End Property
    End Class
    Public Class ComponentPickerItemsList(Of InformationType As {New, ChildComponentInformation})
        Inherits ScrollableList(Of ComponentPickerItemsListItem(Of InformationType))
        Public Sub New()
            With ScrollHandle
                ScrollHandle.RoundedEnds = False
                .Width = 10
            End With
        End Sub
    End Class
    Public Class ComponentPickerItemsListItem(Of InformationType As {New, ChildComponentInformation})
        Inherits ScrollableListItem
        Private varForeColor As Color = Color.White
        Private varPaddingLeft As Integer = 30
        Private FillBrush As New SolidBrush(Color.White)
        Private varCheckBoxSize As New Size(15, 15)
        Private CheckPath As New Drawing2D.GraphicsPath
        Private GradientBrush As Drawing2D.LinearGradientBrush
        Public Sub New(Information As InformationType)
            Initialize(DirectCast(Information.CopyUnknown, InformationType))
        End Sub
        Public Sub New()
            Throw New Exception("The parameterless constructor cannot be used with this class.")
            Initialize(Nothing)
        End Sub
        Private Function GetCheckBoxRectangle() As Rectangle
            Dim Margin As Integer = (Height - varCheckBoxSize.Height) \ 2
            Return New Rectangle(New Point(Margin, Margin), varCheckBoxSize)
        End Function
        Private Sub Initialize(Information As InformationType)
            Dim OffsetPathX As Integer = 11
            Dim OffsetPathY As Integer = 12
            CheckPath.AddPolygon(New Point() {New Point(0 + OffsetPathX, 2 + OffsetPathY), New Point(1 + OffsetPathX, 1 + OffsetPathY), New Point(4 + OffsetPathX, 4 + OffsetPathY), New Point(8 + OffsetPathX, 0 + OffsetPathY), New Point(9 + OffsetPathX, 1 + OffsetPathY), New Point(4 + OffsetPathX, 6 + OffsetPathY)})
            BackColor = ColorHelper.Multiply(ApplicationBackColor, 0.9)
            Height = 31
            IsSelected = Information.Active
            Value = New ComponentPickerItemsListItemValue(Of InformationType)(Information, Nothing)
        End Sub
        Protected Overrides Sub OnSizeChanged(e As EventArgs)
            MyBase.OnSizeChanged(e)
            If GradientBrush IsNot Nothing Then GradientBrush.Dispose()
            GradientBrush = New Drawing2D.LinearGradientBrush(Point.Empty, New Point(Width, Height), BackColor, Color.FromArgb(28, 127, 171))
        End Sub
        Public Property Information As InformationType
            Get
                Return Value.Information
            End Get
            Set(value As InformationType)
                Me.Value.Information = value
                OnValueChanged(New ValueChangedEventArgs(Me.Value))
            End Set
        End Property
        Public Shadows Property Value As ComponentPickerItemsListItemValue(Of InformationType)
            Get
                Return DirectCast(MyBase.Value, ComponentPickerItemsListItemValue(Of InformationType))
            End Get
            Set(value As ComponentPickerItemsListItemValue(Of InformationType))
                MyBase.Value = value
            End Set
        End Property
        Public Overrides ReadOnly Property DefaultValue As ScrollableListItemValue
            Get
                Dim NewInformation As New InformationType With {.Name = "Empty", .Active = False}
                Return New ComponentPickerItemsListItemValue(Of InformationType)(NewInformation, Nothing)
            End Get
        End Property
        Protected Overrides Sub OnMouseUp(e As MouseEventArgs)
            MyBase.OnMouseUp(e)
            If IsSelected Then IsSelected = False Else IsSelected = True
        End Sub
        Protected Overrides Sub OnSelectedChanged(e As ItemSelectedEventArgs)
            Value.Information.Active = IsSelected
            MyBase.OnSelectedChanged(e)
        End Sub
        Protected Overrides Sub OnPaint(pe As PaintEventArgs)
            MyBase.OnPaint(pe)
            FillBrush.Color = Color.White
            With pe.Graphics
                .FillRectangle(GradientBrush, ClientRectangle)
                TextRenderer.DrawText(pe.Graphics, Value.Information.Name, Parent.StaticLabelFont(ListElementFontSize.Regular), GetTextLocation(varPaddingLeft), varForeColor)
                .FillRectangle(FillBrush, GetCheckBoxRectangle)
                If IsSelected Then
                    FillBrush.Color = Color.Black
                    .FillPath(FillBrush, CheckPath)
                End If
            End With
        End Sub
        Protected Overrides Sub OnValueChanged(e As ValueChangedEventArgs)
            MyBase.OnValueChanged(e)
            Invalidate()
        End Sub
        Public Overrides Sub ApplyArguments(Arguments As Object)
            Throw New NotImplementedException()
        End Sub
        Private Function GetTextLocation(ByVal PaddingLeft As Integer) As Point
            Dim TextSize As Size = TextRenderer.MeasureText(Value.Information.Name, Parent.StaticLabelFont(ListElementFontSize.Regular))
            Return New Point(PaddingLeft, (Height - TextSize.Height) \ 2)
        End Function

    End Class
    Public Class ComponentPickerItemsListItemValue(Of InformationType As ChildComponentInformation)
        Inherits ScrollableListItemValue
        Private varInformation As InformationType
        Public Sub New(Information As InformationType, Optional Data As Object = Nothing)
            MyBase.New(Data)
            varInformation = Information
        End Sub
        Public Property Information As InformationType
            Get
                Return varInformation
            End Get
            Set(value As InformationType)
                varInformation = value
            End Set
        End Property
    End Class
    Public MustInherit Class ComponentPicker(Of InformationType As {New, ChildComponentInformation})
        Inherits AutoSizedContainer
        Protected WithEvents DirectoryBox As New PictureBox
        Protected varBaseDirectory As String = "Base directory"
        Protected varDirectory() As String = {varBaseDirectory}
        Protected varHeaderHeight As Integer = 32
        Protected varHeaderColor As Color = Color.FromArgb(230, 230, 230)
        Public Event AddClicked(Sender As Object, e As ItemsAddedEventArgs)
        Public Event CancelClicked(Sender As Object, e As EventArgs)
        Private varAddText As String = "Add 0 components"
        Private varCancelText As String = "Cancel"
        Protected varSelectedItems As New Dictionary(Of String, InformationType)
        Protected AddRect, CancelRect As Rectangle
        Private ButtonFont As Font = RalewayFonts.GetFont(RalewayFontCollection.FontVariant.Regular, 9.5, FontStyle.Regular)
        Private varHoveredItem As ActiveControl = ActiveControl.Nothing
        Private varAddCancelSpacing As Integer = 10

        'Protected WithEvents varItems As New BrowseForSchedulesList ' TODO: Make 
        Protected WithEvents varItems As New ComponentPickerItemsList(Of InformationType)
        Protected WithEvents varCategories As New CategoryList
        Protected MustOverride ReadOnly Property ItemName As String
        Protected MustOverride Function GetCategories() As String()
        Protected MustOverride Function GetItemsInCategory(ByVal Category As String) As IEnumerable(Of ComponentEntry)
        Public Sub New()
            Initialize()
        End Sub
        Public Property BaseDirectory As String
            Get
                Return varBaseDirectory
            End Get
            Set(value As String)
                varBaseDirectory = value
                varDirectory(0) = varBaseDirectory
                Invalidate()
            End Set
        End Property
        Private Sub Initialize()
            DoubleBuffered = True
            ForeColor = Color.White
            DirectoryBox.Parent = Me
            DirectoryBox.BackColor = varHeaderColor
            With varItems
                .UseRoundedEnds = False
                .Parent = Me
            End With
            With varCategories
                .CanEdit = False
                .UseRoundedEnds = False
                .Parent = Me
                .BringToFront()
            End With
            HeaderControls.Add(New ContainerHeaderControl(Me, {My.Resources.BackButtonDefault, My.Resources.BackButtonHover, My.Resources.BackButtonPress}, AddressOf HeaderControl_Click))
            varAddText = "Add 0 " & ItemName & "s"
            With HeaderControls.Last
                .BackColor = varHeaderColor
                .BringToFront()
                .Show()
            End With
            AutoAdjust()
        End Sub
        Public Sub SetDirectory(Optional Category As String = Nothing)
            If Category = Nothing Then
                varCategories.Clear()
                Dim ComponentCategories() As String = GetCategories()
                For Each Key As String In ComponentCategories
                    varCategories.Add(New CategoryListItem(Key))
                Next
                varItems.Hide()
                varCategories.Show()
                varDirectory = New String() {varBaseDirectory}
                HeaderControls.Last.Hide()
            Else
                varItems.Clear()
                Dim ItemsInCategory As List(Of ComponentEntry) = GetItemsInCategory(Category).ToList
                For Each Item As ComponentEntry In ItemsInCategory
                    Dim ItemIsSelected As Boolean = False
                    Dim ContainsKey As Boolean = varSelectedItems.ContainsKey(Item.Name)
                    If ContainsKey Then ItemIsSelected = varSelectedItems(Item.Name).Active
                    Dim NewInformation As New InformationType With {.Name = Item.Name, .Active = ItemIsSelected}
                    varItems.Add(New ComponentPickerItemsListItem(Of InformationType)(NewInformation))
                Next
                varCategories.Hide()
                varItems.Show()
                varDirectory = New String() {varBaseDirectory, Category}
                HeaderControls.Last.Show()
            End If
            DirectoryBox.Invalidate()
        End Sub
        Private Sub HeaderControl_Click(Sender As Object, e As MouseEventArgs)
            NavigateBack()
        End Sub
        Private Sub NavigateBack()
            SetDirectory()
        End Sub
        Public Sub RefreshLists()
            SetDirectory()
        End Sub
        Public ReadOnly Property Directory As String()
            Get
                Return varDirectory
            End Get
        End Property
        Private Property HoveredItem As ActiveControl
            Get
                Return varHoveredItem
            End Get
            Set(value As ActiveControl)
                Dim Changed As Boolean
                If value <> varHoveredItem Then
                    Changed = True
                End If
                varHoveredItem = value
                If Changed Then
                    Select Case varHoveredItem
                        Case ActiveControl.Nothing
                            Cursor = Cursors.Default
                        Case Else
                            Cursor = Cursors.Hand
                    End Select
                    Invalidate()
                End If
            End Set
        End Property
        Private Sub UpdateAddCancelRects()
            varAddText = "Add " & varSelectedItems.Values.Count & " " & ItemName & "s"
            Dim AddSize As Size = TextRenderer.MeasureText(varAddText, ButtonFont)
            Dim CancelSize As Size = TextRenderer.MeasureText(varCancelText, ButtonFont)
            Dim TotalSize As New Size(AddSize.Width + CancelSize.Width + varAddCancelSpacing, AddSize.Height)
            Dim ButtonsLocation As New Point((Width - TotalSize.Width) \ 2, (Height - TotalSize.Height - 10))
            AddRect = New Rectangle(ButtonsLocation, AddSize)
            CancelRect = New Rectangle(New Point(AddRect.Right + varAddCancelSpacing, ButtonsLocation.Y), CancelSize)
            Invalidate()
        End Sub
        Private Sub Categories_SelectedChanged(Sender As Object, e As ListItemSelectionChangedEventArgs) Handles varCategories.ItemSelectionChanged
            SetDirectory(DirectCast(e.SenderItem, CategoryListItem).Value.CategoryName)
        End Sub
        Private Sub Items_SelectedChanged(Sender As Object, e As ListItemSelectionChangedEventArgs) Handles varItems.ItemSelectionChanged
            Dim SenderItem As ComponentPickerItemsListItem(Of InformationType) = DirectCast(e.SenderItem, ComponentPickerItemsListItem(Of InformationType))
            If SenderItem.IsSelected Then
                varSelectedItems.Add(SenderItem.Information.Name, SenderItem.Information)
            Else
                varSelectedItems.Remove(SenderItem.Information.Name)
            End If
            DirectoryBox.Invalidate()
            UpdateAddCancelRects()
        End Sub
        Protected Overrides Sub OnMouseMove(e As MouseEventArgs)
            MyBase.OnMouseMove(e)
            If FormatConverter.PointIsInRectangle(e.Location, AddRect) Then
                HoveredItem = ActiveControl.Add
            ElseIf FormatConverter.PointIsInRectangle(e.Location, CancelRect) Then
                HoveredItem = ActiveControl.Cancel
            Else
                HoveredItem = ActiveControl.Nothing
            End If
        End Sub
        Protected Overrides Sub OnClick(e As EventArgs)
            MyBase.OnClick(e)
            If FormatConverter.PointIsInRectangle(PointToClient(MousePosition), AddRect) Then
                Dim SelectedItems As New List(Of InformationType)
                With varSelectedItems
                    Dim iLast As Integer = .Values.Count - 1
                    If iLast >= 0 Then
                        For i As Integer = 0 To iLast
                            If .Values(i).Active Then
                                SelectedItems.Add(.Values(i))
                            End If
                        Next
                    End If
                End With
                RaiseEvent AddClicked(Me, New ItemsAddedEventArgs(SelectedItems))
                varSelectedItems.Clear()
            ElseIf FormatConverter.PointIsInRectangle(PointToClient(MousePosition), CancelRect) Then
                RaiseEvent CancelClicked(Me, EventArgs.Empty)
                varSelectedItems.Clear()
            End If
            HoveredItem = ActiveControl.Nothing
            UpdateAddCancelRects()
        End Sub
        Protected Overrides Sub OnBackColorChanged(e As EventArgs)
            MyBase.OnBackColorChanged(e)
            If varItems IsNot Nothing Then varItems.BackColor = BackColor
        End Sub
        Protected Overrides Sub OnSizeChanged(e As EventArgs)
            MyBase.OnSizeChanged(e)
            AutoAdjust()
        End Sub
        Protected Overrides Sub OnPaint(pe As PaintEventArgs)
            'MyBase.OnPaint(pe)
            Select Case HoveredItem
                Case ActiveControl.Add
                    Using LinePen As New Pen(Color.White) With {.DashPattern = {1, 2}}
                        pe.Graphics.DrawLine(LinePen, New Point(AddRect.Left + 1, AddRect.Bottom), New Point(AddRect.Right - 1, AddRect.Bottom))
                    End Using
                Case ActiveControl.Cancel
                    Using LinePen As New Pen(Color.White) With {.DashPattern = {1, 2}}
                        pe.Graphics.DrawLine(LinePen, New Point(CancelRect.Left + 1, CancelRect.Bottom), New Point(CancelRect.Right - 1, CancelRect.Bottom))
                    End Using
            End Select
            TextRenderer.DrawText(pe.Graphics, varAddText, ButtonFont, AddRect, Color.White)
            TextRenderer.DrawText(pe.Graphics, varCancelText, ButtonFont, CancelRect, Color.White)
        End Sub
        Private Sub DirectoryBox_Paint(Sender As Object, e As PaintEventArgs) Handles DirectoryBox.Paint
            Dim TextSize As Size = TextRenderer.MeasureText(e.Graphics, varDirectory(0), ButtonFont)
            Dim TextLocation As New Point(10, (DirectoryBox.Height - TextSize.Height) \ 2)
            TextRenderer.DrawText(e.Graphics, varDirectory(0), ButtonFont, TextLocation, Color.Black)
            If varDirectory.Count = 2 Then
                Dim DrawImage As Image = My.Resources.SmallArrowBlack
                Dim ImageSize As Size = DrawImage.Size
                Dim ImageLocation As New Point(TextLocation.X + TextSize.Width, (DirectoryBox.Height - ImageSize.Height) \ 2)
                e.Graphics.DrawImage(DrawImage, ImageLocation)
                Dim SecondTextSize As Size = TextRenderer.MeasureText(e.Graphics, varDirectory(1), ButtonFont)
                Dim SecondTextLocation As New Point(ImageLocation.X + ImageSize.Width, (DirectoryBox.Height - SecondTextSize.Height) \ 2)
                TextRenderer.DrawText(e.Graphics, varDirectory(1), ButtonFont, SecondTextLocation, Color.Black)
            End If
            Dim CountText As String = varSelectedItems.Values.Count & " items selected"
            Dim CountTextSize As Size = TextRenderer.MeasureText(e.Graphics, CountText, ButtonFont)
            Dim CountTextLocation As New Point(Width - CountTextSize.Width - DirectoryBox.Height - 5, (DirectoryBox.Height - CountTextSize.Height) \ 2)
            TextRenderer.DrawText(e.Graphics, CountText, ButtonFont, CountTextLocation, Color.Black)
        End Sub
        Private Sub AutoAdjust()
            DirectoryBox.Size = New Size(Width, varHeaderHeight)
            UpdateAddCancelRects()
            With varItems
                .Location = New Point(0, DirectoryBox.Bottom)
                .Size = New Size(Width, AddRect.Top - .Top - 5)
            End With
            With varCategories
                .Location = New Point(0, DirectoryBox.Bottom)
                .Size = New Size(Width, AddRect.Top - .Top - 5)
            End With
            With HeaderControls.Last
                .Left = Width - .Width
            End With
        End Sub
        Protected Enum ActiveControl As Integer
            [Nothing] = 0
            Add = 1
            Cancel = 2
        End Enum
    End Class

    Public Class TaskPicker
        Inherits ComponentPicker(Of TaskInformation)
        Public Sub New()

        End Sub
        Protected Overrides ReadOnly Property ItemName As String
            Get
                Return "task"
            End Get
        End Property
        Protected Overrides Function GetCategories() As String()
            Return Loader.TaskCategories.ToArray
        End Function
        Protected Overrides Function GetItemsInCategory(Category As String) As IEnumerable(Of ComponentEntry)
            Return Loader.TasksInCategory(Category)
        End Function
    End Class
    Public Class RoutinePicker
        Inherits ComponentPicker(Of RoutineInformation)
        Public Sub New()

        End Sub
        Protected Overrides ReadOnly Property ItemName As String
            Get
                Return "routine"
            End Get
        End Property
        Protected Overrides Function GetCategories() As String()
            Return Loader.RoutineCategories.ToArray
        End Function
        Protected Overrides Function GetItemsInCategory(Category As String) As IEnumerable(Of ComponentEntry)
            Return Loader.RoutinesInCategory(Category)
        End Function
    End Class
    Public Class SchedulePicker
        Inherits ComponentPicker(Of ScheduleInformation)
        Public Sub New()
        End Sub
        Protected Overrides ReadOnly Property ItemName As String
            Get
                Return "schedule"
            End Get
        End Property
        Protected Overrides Function GetCategories() As String()
            Return Loader.ScheduleCategories
        End Function
        Protected Overrides Function GetItemsInCategory(Category As String) As IEnumerable(Of ComponentEntry)
            Return Loader.SchedulesInCategory(Category)
        End Function
    End Class
    Public Class CategoryEditedEventArgs
        Private varCategoryItem As CategoryListItem
        Private varTrigger As CategoryEditEventArgs
        Public Sub New(CategoryItem As CategoryListItem, Trigger As CategoryEditEventArgs)
            varCategoryItem = CategoryItem
            varTrigger = Trigger
        End Sub
        Public ReadOnly Property CategoryItem As CategoryListItem
            Get
                Return varCategoryItem
            End Get
        End Property
        Public ReadOnly Property Trigger As CategoryEditEventArgs
            Get
                Return varTrigger
            End Get
        End Property
    End Class
    Public Class CategoryDeletedEventArgs
        Private varCategoryItem As CategoryListItem
        Public Sub New(CategoryItem As CategoryListItem)
            varCategoryItem = CategoryItem
        End Sub
        Public ReadOnly Property CategoryItem As CategoryListItem
            Get
                Return varCategoryItem
            End Get
        End Property
    End Class
    Public Class CategoryDroppedEventArgs
        Public Trigger As DragEventArgs
        Public TargetItem As CategoryListItem
        Public Sub New(Trigger As DragEventArgs, TargetItem As CategoryListItem)
            Me.Trigger = Trigger
            Me.TargetItem = TargetItem
        End Sub
    End Class
    Public Class CategoryList
        Inherits ScrollableList(Of CategoryListItem)
        Public Event CategoryEdited(Sender As Object, e As CategoryEditedEventArgs)
        Public Event CategoryDeleted(Sender As Object, e As CategoryDeletedEventArgs)
        Public Event CategoryDropped(Sender As Object, e As CategoryDroppedEventArgs)
        Private varCanEdit As Boolean = True
        Public Sub New()
            BackColor = ColorHelper.Multiply(ApplicationBackColor, 0.7)
        End Sub
        Public Function FindName(ByVal CategoryName As String) As CategoryListItem
            Return Items.Find(Function(x As CategoryListItem) As Boolean
                                  Return (x.Value.CategoryName = CategoryName)
                              End Function)
        End Function
        Public Property CanEdit As Boolean
            Get
                Return varCanEdit
            End Get
            Set(value As Boolean)
                varCanEdit = value
                For Each Item As CategoryListItem In Items
                    Item.CanEdit = value
                Next
            End Set
        End Property
        Public Overrides Sub Add(Item As CategoryListItem)
            MyBase.Add(Item)
            AddHandler Item.EditFinished, AddressOf Item_EditFinished
            AddHandler Item.DeleteClicked, AddressOf Item_DeleteClicked
            AddHandler Item.CategoryDropped, AddressOf Item_CategoryDropped
            Item.CanEdit = varCanEdit
        End Sub
        Private Sub Item_EditFinished(Sender As Object, e As CategoryEditEventArgs)
            RaiseEvent CategoryEdited(Me, New CategoryEditedEventArgs(DirectCast(Sender, CategoryListItem), e))
        End Sub
        Private Sub Item_DeleteClicked(Sender As Object, e As EventArgs)
            RaiseEvent CategoryDeleted(Me, New CategoryDeletedEventArgs(DirectCast(Sender, CategoryListItem)))
        End Sub
        Private Sub Item_CategoryDropped(Sender As Object, e As DragEventArgs)
            RaiseEvent CategoryDropped(Me, New CategoryDroppedEventArgs(e, DirectCast(Sender, CategoryListItem)))
        End Sub
        Protected Overrides Sub RemoveItemHandlers(ByRef Item As CategoryListItem)
            RemoveHandler Item.EditFinished, AddressOf Item_EditFinished
            RemoveHandler Item.DeleteClicked, AddressOf Item_DeleteClicked
            RemoveHandler Item.CategoryDropped, AddressOf Item_CategoryDropped
            MyBase.RemoveItemHandlers(Item)
        End Sub
    End Class
    Public Class CategoryListItem
        Inherits ScrollableListItem
        Private varForeColor As Color = Color.White
        Private varPaddingLeft As Integer = 5
        Private FillBrush As New SolidBrush(Color.White)
        Private SeparatorPen As New Pen(Color.FromArgb(20, Color.White))
        Private GradientBrush As Drawing2D.LinearGradientBrush
        Private HoverGradientBrush As Drawing2D.LinearGradientBrush
        Private varIsEditing As Boolean
        Private EditBackColor As Color = ColorHelper.Multiply(ApplicationBackColor, 0.9)
        Private varBaseDirectory As String
        Private DeleteRect As Rectangle
        Private DeleteImages() As Image = {My.Resources.DeleteIconDefault, My.Resources.DeleteIconHover}
        Private CurrentDeleteImage As Image = DeleteImages(0)
        Private varCanEdit As Boolean
        Public Event EditFinished(Sender As Object, e As CategoryEditEventArgs)
        Public Event DeleteClicked(Sender As Object, e As EventArgs)
        Public Event CategoryDropped(Sender As Object, e As DragEventArgs)
        ''' <summary>
        ''' If this constructor is used, the category name cannot be edited unless the BaseDirectory property is set after construction.
        ''' </summary>
        Public Sub New()
            varCanEdit = False
            Initialize(DirectCast(DefaultValue, CategoryListItemValue).CategoryName)
        End Sub
        Public Sub New(CategoryName As String)
            varCanEdit = True
            Initialize(CategoryName)
        End Sub
        Private Sub Initialize(CategoryName As String)
            Hide()
            Height = 31
            Value = New CategoryListItemValue(CategoryName)
            UseCompatibleTextRendering = False
            Show()
            AllowDrop = True
        End Sub
        Public Property CanEdit As Boolean
            Get
                Return varCanEdit
            End Get
            Set(value As Boolean)
                varCanEdit = value
                Invalidate()
            End Set
        End Property
        Protected Overrides Sub OnDragEnter(e As DragEventArgs)
            MyBase.OnDragEnter(e)
            If (e.Data.GetDataPresent(DataFormats.Text)) Then
                e.Effect = DragDropEffects.Move
            End If
            Hovering = True
            'Parent.ReportDragEvent(Me, e)
        End Sub
        Protected Overrides Sub OnDragDrop(e As DragEventArgs)
            MyBase.OnDragDrop(e)
            If (e.Data.GetDataPresent(DataFormats.Text)) Then
                Hovering = False
                RaiseEvent CategoryDropped(Me, e)
            End If
            'Parent.ReportDragEvent(Me, e)
        End Sub
        'Private Sub Parent_ChildDragEventReported(Target As Control, e As DragEventArgs)
        '    If Target Is Me Then
        '        Hovering = True
        '    Else
        '        Hovering = False
        '    End If
        'End Sub
        Private Sub Me_DragLeave(Sender As Object, e As EventArgs) Handles Me.DragLeave
            Hovering = False
        End Sub
        Public Sub Edit()
            If varCanEdit AndAlso Not varIsEditing AndAlso Parent IsNot Nothing Then
                varIsEditing = True
                Dim MarginTop As Integer = 3
                Dim MarginLeft As Integer = 2
                Dim EditRect As Rectangle = GetEditRectangle()
                Dim EditBox As New TextBox
                With EditBox
                    .Location = New Point(EditRect.Left + MarginLeft, EditRect.Top + MarginTop)
                    .Width = EditRect.Width - MarginLeft
                    .BorderStyle = BorderStyle.None
                    .BackColor = EditBackColor
                    .ForeColor = Color.White
                    .Font = Parent.StaticLabelFont(ListElementFontSize.Regular)
                    .Padding = New Padding(0, 10, 0, 0)
                    .Text = Value.CategoryName
                    .Parent = Me
                    .Focus()
                    .SelectAll()
                    AddHandler .KeyDown, AddressOf EditBox_KeyDown
                    AddHandler .LostFocus, AddressOf EditBox_LostFocus
                    AddHandler .Leave, AddressOf EditBox_LostFocus
                    AddHandler .TextChanged, AddressOf EditBox_TextChanged
                End With
                Invalidate()
            End If
        End Sub
        Private Function GetEditRectangle() As Rectangle
            Dim EditLocation As New Point(varPaddingLeft, 5)
            Dim EditSize As New Size(Width - EditLocation.X * 2, Height - EditLocation.Y * 2)
            Return New Rectangle(EditLocation, EditSize)
        End Function
        Private Sub EditBox_KeyDown(Sender As Object, e As KeyEventArgs)
            Dim SenderTextBox As TextBox = DirectCast(Sender, TextBox)
            ' TODO: Validate input, only allow legal characters in directory
            Select Case e.KeyCode
                Case Keys.Escape
                    SenderTextBox.Text = Value.CategoryName
                    FinishEdit(SenderTextBox)
                Case Keys.Enter
                    FinishEdit(SenderTextBox)
            End Select
        End Sub
        Private Sub EditBox_TextChanged(Sender As Object, e As EventArgs)
            Dim SenderTextBox As TextBox = DirectCast(Sender, TextBox)
            If TextRenderer.MeasureText(SenderTextBox.Text, SenderTextBox.Font).Width > SenderTextBox.Width Then
                SenderTextBox.Text = SenderTextBox.Text.Remove(SenderTextBox.Text.Length - 1, 1)
                SenderTextBox.SelectionStart = SenderTextBox.Text.Length
            End If
        End Sub
        Private Sub EditBox_LostFocus(Sender As Object, e As EventArgs)
            FinishEdit(DirectCast(Sender, TextBox))
        End Sub
        Private Sub FinishEdit(ByRef EditBox As TextBox)
            Dim OldName As String = Value.CategoryName
            Dim NewName As String = EditBox.Text
            With EditBox
                RemoveHandler .KeyDown, AddressOf EditBox_KeyDown
                RemoveHandler .LostFocus, AddressOf EditBox_LostFocus
                RemoveHandler .Leave, AddressOf EditBox_LostFocus
                RemoveHandler .TextChanged, AddressOf EditBox_TextChanged
                .Hide()
                .Dispose()
            End With
            varIsEditing = False
            If OldName <> NewName Then
                Value = New CategoryListItemValue(NewName)
                OnEditFinished(New CategoryEditEventArgs(OldName, NewName))
            Else
                Invalidate()
            End If
        End Sub
        Protected Overridable Sub OnEditFinished(e As CategoryEditEventArgs)
            RaiseEvent EditFinished(Me, e)
        End Sub
        Public Shadows Property Value As CategoryListItemValue
            Get
                Return DirectCast(MyBase.Value, CategoryListItemValue)
            End Get
            Set(value As CategoryListItemValue)
                MyBase.Value = value
            End Set
        End Property
        Public Overrides ReadOnly Property DefaultValue As ScrollableListItemValue
            Get
                Return New CategoryListItemValue("SomeCategory")
            End Get
        End Property
        Protected Overrides Sub OnMouseUp(e As MouseEventArgs)
            MyBase.OnMouseUp(e)
            Focus()
            If CheckDeleteHover(e.Location) Then
                RaiseEvent DeleteClicked(Me, EventArgs.Empty)
            Else
                If e.Button = MouseButtons.Left Then
                    If IsSelected Then IsSelected = False Else IsSelected = True
                ElseIf e.Button = MouseButtons.Right Then
                    Edit()
                End If
            End If
        End Sub
        Protected Overrides Sub OnHoveringChanged(e As ItemHoveringEventArgs)
            If Not e.Hovering Then
                CurrentDeleteImage = DeleteImages(0)
            End If
            MyBase.OnHoveringChanged(e)
        End Sub
        Protected Overrides Sub OnMouseMove(e As MouseEventArgs)
            MyBase.OnMouseMove(e)
            If CanEdit AndAlso Not varIsEditing Then
                Dim HoverCheck As Boolean = CheckDeleteHover(e.Location)
                If HoverCheck AndAlso CurrentDeleteImage IsNot DeleteImages(1) Then
                    CurrentDeleteImage = DeleteImages(1)
                    Invalidate()
                ElseIf Not HoverCheck AndAlso CurrentDeleteImage IsNot DeleteImages(0) Then
                    CurrentDeleteImage = DeleteImages(0)
                    Invalidate()
                End If
            End If
        End Sub
        'Protected Overrides Sub OnParentChanged(e As EventArgs)
        '    MyBase.OnParentChanged(e)
        '    AddHandler Parent.ChildDragEventReported, AddressOf Parent_ChildDragEventReported
        'End Sub
        Private Function CheckDeleteHover(ClientMouseLocation As Point) As Boolean
            Return FormatConverter.PointIsInRectangle(ClientMouseLocation, DeleteRect)
        End Function
        Protected Overrides Sub OnPaint(pe As PaintEventArgs)
            MyBase.OnPaint(pe)
            Dim CheckSize As New Size(16, 16)
            With pe.Graphics
                .SetClip(ClientRectangle)
                If IsActive OrElse Hovering Then
                    .FillRectangle(HoverGradientBrush, ClientRectangle)
                Else
                    .FillRectangle(GradientBrush, ClientRectangle)
                End If
                If varIsEditing Then
                    .SetClip(GetEditRectangle)
                    .Clear(EditBackColor)
                    .SetClip(ClientRectangle)
                Else
                    If UseCompatibleTextRendering Then
                        TextRenderer.DrawText(pe.Graphics, Value.CategoryName, Parent.StaticLabelFont(ListElementFontSize.Regular), GetTextLocation(varPaddingLeft), varForeColor)
                    Else
                        .DrawString(Value.CategoryName, Parent.StaticLabelFont(ListElementFontSize.Regular), FillBrush, GetTextLocation(varPaddingLeft))
                    End If
                End If
                .DrawLine(SeparatorPen, New Point(0, Height - 1), New Point(Width, Height - 1))
                If CanEdit AndAlso Not varIsEditing Then .DrawImageUnscaled(CurrentDeleteImage, DeleteRect)
            End With
        End Sub
        Protected Overrides Sub OnSizeChanged(e As EventArgs)
            MyBase.OnSizeChanged(e)
            Dim DeleteSize As New Size(32, 32)
            Dim Margin As Integer = (Height - DeleteSize.Height) \ 2
            DeleteRect = New Rectangle(New Point(Width - DeleteSize.Width, Margin), DeleteSize)
            If GradientBrush IsNot Nothing Then GradientBrush.Dispose()
            GradientBrush = New Drawing2D.LinearGradientBrush(Point.Empty, New Point(Width, Height), GradientFirstColor, GradientSecondColor)
            If HoverGradientBrush IsNot Nothing Then HoverGradientBrush.Dispose()
            HoverGradientBrush = New Drawing2D.LinearGradientBrush(Point.Empty, New Point(Width, Height), ColorHelper.Multiply(GradientFirstColor, 1.1), ColorHelper.Multiply(GradientSecondColor, 1.1))
        End Sub
        Public Overrides Sub ApplyArguments(Arguments As Object)
            Throw New NotImplementedException()
        End Sub
        Private Function GetTextLocation(ByVal PaddingLeft As Integer) As Point
            Dim TextSize As Size = TextRenderer.MeasureText(Value.CategoryName, Parent.StaticLabelFont(ListElementFontSize.Regular))
            Return New Point(PaddingLeft, (Height - TextSize.Height) \ 2)
        End Function
        Protected Overrides Sub Dispose(disposing As Boolean)
            If FillBrush IsNot Nothing Then FillBrush.Dispose()
            If SeparatorPen IsNot Nothing Then SeparatorPen.Dispose()
            If GradientBrush IsNot Nothing Then GradientBrush.Dispose()
            If HoverGradientBrush IsNot Nothing Then HoverGradientBrush.Dispose()
            MyBase.Dispose(disposing)
            'RemoveHandler Parent.ChildDragEventReported, AddressOf Parent_ChildDragEventReported
        End Sub
    End Class
    Public Class CategoryEditEventArgs
        Inherits EventArgs
        Private varOldCategoryName As String
        Private varNewCategoryName As String
        Public ReadOnly Property OldCategoryName As String
            Get
                Return varOldCategoryName
            End Get
        End Property
        Public ReadOnly Property NewCategoryName As String
            Get
                Return varNewCategoryName
            End Get
        End Property
        Public Sub New(OldCategoryName As String, NewCategoryName As String)
            varOldCategoryName = OldCategoryName
            varNewCategoryName = NewCategoryName
        End Sub
    End Class
    Public Class CategoryListItemValue
        Inherits ScrollableListItemValue
        Private varName As String
        Public Property CategoryName As String
            Get
                Return varName
            End Get
            Set(value As String)
                varName = value
            End Set
        End Property
        Public Sub New(CategoryName As String)
            MyBase.New(Nothing)
            varName = CategoryName
        End Sub
    End Class
    Public MustInherit Class ItemsInCategoryList(Of ItemType As {New, ItemsInCategoryListItem})
        Inherits ScrollableList(Of ItemType)
        Public Sub New()
        End Sub
    End Class
    Public MustInherit Class ItemsInCategoryListItem
        Inherits ScrollableListItem
        Private TextLocation As Point
        Private LinePen As New Pen(ColorHelper.FillRemainingRGB(ApplicationBackColor, 0.2)) With {.DashStyle = Drawing2D.DashStyle.Dot}
        Private ActiveBrush As New SolidBrush(Color.White)
        Private varDarkColor As Color = ColorHelper.Multiply(ApplicationBackColor, 0.8)
        Private Shared varFont As Font = RalewayFonts.GetFont(RalewayFontCollection.FontVariant.Light, 10, FontStyle.Regular)
        'Private GradientBrush As Drawing2D.LinearGradientBrush
        Public Sub New()
            Initialize(DirectCast(DefaultValue, ItemsInCategoryListItemValue))
        End Sub
        Public Sub New(Value As ItemsInCategoryListItemValue)
            Initialize(Value)
        End Sub
        Private Sub Initialize(Value As ItemsInCategoryListItemValue)
            Me.Value = Value
            Size = New Size(10, DefaultHeight)
            BackColor = ApplicationBackColor
            ForeColor = Color.White
        End Sub
        Public Shadows Property Name As String
            Get
                Return Value.Name
            End Get
            Set(value As String)
                Me.Value.Name = value
                OnValueChanged(New ValueChangedEventArgs(Me.Value))
            End Set
        End Property
        Private Function GetArrowPath(ByVal UpperLeft As Point) As Drawing2D.GraphicsPath
            With UpperLeft
                Dim Ret As New Drawing2D.GraphicsPath
                Dim ArrowThickness As Integer = 6
                Dim ArrowHalfHeight As Integer = 6
                Dim Points() As Point = {New Point(.X, .Y), New Point(.X + ArrowThickness, .Y), New Point(.X + ArrowThickness * 2, .Y + ArrowHalfHeight), New Point(.X + ArrowThickness - 1, .Y + ArrowHalfHeight * 2 + 1), New Point(.X - 1, .Y + ArrowHalfHeight * 2 + 1), New Point(.X + ArrowThickness, .Y + ArrowHalfHeight)}
                Ret.AddPolygon(Points)
                Return Ret
            End With
        End Function
        Protected Overrides Sub OnMouseUp(e As MouseEventArgs)
            MyBase.OnMouseUp(e)
            IsSelected = True
        End Sub
        Protected Overrides Sub OnSizeChanged(e As EventArgs)
            MyBase.OnSizeChanged(e)
            'If GradientBrush IsNot Nothing Then GradientBrush.Dispose()
            'GradientBrush = New Drawing2D.LinearGradientBrush(Point.Empty, New Point(Width, Height), BackColor, Color.FromArgb(28, 127, 171))
            UpdateTextLocation()
        End Sub
        Protected Overrides Sub OnValueChanged(e As ValueChangedEventArgs)
            MyBase.OnValueChanged(e)
            UpdateTextLocation()
        End Sub
        Protected Overrides Sub OnParentChanged(e As EventArgs)
            MyBase.OnParentChanged(e)
            UpdateTextLocation()
        End Sub
        Private Sub UpdateTextLocation()
            If Parent IsNot Nothing Then
                Dim TextSize As Size = TextRenderer.MeasureText(Value.Name, Parent.StaticLabelFont(ListElementFontSize.Regular))
                TextLocation = New Point(0, (Height - TextSize.Height) \ 2)
                Invalidate()
            End If
        End Sub
        Protected Overrides Sub OnHoveringChanged(e As ItemHoveringEventArgs)
            Invalidate()
            MyBase.OnHoveringChanged(e)
        End Sub
        Protected Overrides Sub OnSelectedChanged(e As ItemSelectedEventArgs)
            Invalidate()
            MyBase.OnSelectedChanged(e)
        End Sub
        Protected Overrides Sub OnPaint(pe As PaintEventArgs)
            MyBase.OnPaint(pe)
            With pe.Graphics
                .SetClip(ClientRectangle)
                '.FillRectangle(GradientBrush, ClientRectangle)
                ActiveBrush.Color = Color.White
                If IsSelected Then
                    .FillPath(ActiveBrush, GetArrowPath(New Point(Width - 15, 8)))
                ElseIf Hovering Then
                    ActiveBrush.Color = varDarkColor
                    .FillPath(ActiveBrush, GetArrowPath(New Point(Width - 15, 8)))
                End If
                .TextRenderingHint = Drawing.Text.TextRenderingHint.SystemDefault
                .DrawLine(LinePen, New Point(0, Height - 1), New Point(Width, Height - 1))
                Dim UseCompatibleTextRendering As Boolean = False
                If UseCompatibleTextRendering Then
                    TextRenderer.DrawText(pe.Graphics, Value.Name, varFont, TextLocation, ForeColor)
                Else
                    ActiveBrush.Color = ForeColor
                    .DrawString(Value.Name, varFont, ActiveBrush, TextLocation)
                End If
            End With
        End Sub
        Public Shadows Property Value As ItemsInCategoryListItemValue
            Get
                Return DirectCast(MyBase.Value, ItemsInCategoryListItemValue)
            End Get
            Set(value As ItemsInCategoryListItemValue)
                MyBase.Value = value
            End Set
        End Property
        Public Overrides ReadOnly Property DefaultValue As ScrollableListItemValue
            Get
                Return New ItemsInCategoryListItemValue("Default name")
            End Get
        End Property
        Public Overrides Sub ApplyArguments(Arguments As Object)
            Throw New NotImplementedException()
        End Sub
    End Class
    Public Class ItemsInCategoryListItemValue
        Inherits ScrollableListItemValue
        Private varName As String
        Public Sub New(Name As String)
            MyBase.New(Nothing)
            varName = Name
        End Sub
        Public Property Name As String
            Get
                Return varName
            End Get
            Set(value As String)
                varName = value
            End Set
        End Property
    End Class
    Public Class SchedulesInCategoryList
        Inherits ItemsInCategoryList(Of SchedulesInCategoryListItem)
        Public Sub New()

        End Sub
    End Class
    Public Class SchedulesInCategoryListItem
        Inherits ItemsInCategoryListItem
        Public Sub New(Value As ItemsInCategoryListItemValue)
            MyBase.New(Value)
        End Sub
        Public Sub New()
            MyBase.New()
        End Sub
    End Class
    Public Class RoutinesInCategoryList
        Inherits ItemsInCategoryList(Of RoutinesInCategoryListItem)
        Public Sub New()

        End Sub
    End Class
    Public Class TasksInCategoryList
        Inherits ItemsInCategoryList(Of TasksInCategoryListItem)
        Public Sub New()

        End Sub
    End Class
    Public Class TasksInCategoryListItem
        Inherits ItemsInCategoryListItem
        Public Sub New(Value As ItemsInCategoryListItemValue)
            MyBase.New(Value)
        End Sub
        Public Sub New()
            MyBase.New()
        End Sub
    End Class

    Public Class RoutinesInCategoryListItem
        Inherits ItemsInCategoryListItem
        Public Sub New(Value As ItemsInCategoryListItemValue)
            MyBase.New(Value)
        End Sub
        Public Sub New()
            MyBase.New()
        End Sub
    End Class

End Module


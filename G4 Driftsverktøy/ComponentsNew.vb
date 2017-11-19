Option Strict On
Option Explicit On
Option Infer Off
Imports System.IO
Imports G4_Driftsverktøy
'Imports AudiopoLib
'Imports G4_Driftsverktøy
'Imports System.Reflection
Public Module ComponentsNew

    Public Interface IListElementFontProvider
        Property StaticLabelFont(ByVal FontSize As ListElementFontSize) As Font
        Property StatusLabelFont(ByVal FontSize As ListElementFontSize) As Font
        Property EditButtonFont(ByVal FontSize As ListElementFontSize) As Font
        Property HoveredEditButton As EditButton
        ReadOnly Property Labels As List(Of ListElement)
        Sub CheckMouseLocation(e As MouseEventArgs)
    End Interface
    Public Enum ListElementFontSize As Integer
        Regular = 0
        Small = 1
        Large = 2
    End Enum
#Region "FormatConverter"
    Public Class FormatConverter
        Private Sub New()

        End Sub
        Public Shared Function GetBytesFromString(ByVal StringToConvert As String) As Byte()
            Return New Text.UTF8Encoding(True).GetBytes(StringToConvert)
        End Function
        Public Shared Function PointIsInRectangle(ByVal Point As Point, ByVal Rectangle As Rectangle) As Boolean
            If Point.X >= Rectangle.X AndAlso Point.X <= Rectangle.Right AndAlso Point.Y >= Rectangle.Y AndAlso Point.Y <= Rectangle.Bottom Then
                Return True
            End If
            Return False
        End Function
        Public Shared Function ExtractHighestLevelDirectory(ByVal FullPath As String, ByVal IncludesFileName As Boolean) As String
            If IncludesFileName Then
                Dim Parts() As String = FullPath.Split(New String() {"\"}, StringSplitOptions.RemoveEmptyEntries)
                Return Parts(Parts.Count - 2)
            Else
                Return FullPath.Split(New String() {"\"}, StringSplitOptions.RemoveEmptyEntries).Last
            End If
        End Function
        Public Shared Function ExtractFileName(ByVal FilePath As String, ByVal KeepExtension As Boolean) As String
            Dim Ret As String = FilePath.Split(New String() {"\"}, StringSplitOptions.RemoveEmptyEntries).Last
            If Not KeepExtension Then Ret = Ret.Split(New String() {"."}, StringSplitOptions.RemoveEmptyEntries).First
            Return Ret
        End Function
        Public Shared Function GetNumericBoolean(ByVal BooleanValue As Boolean) As Integer
            If BooleanValue Then Return 1
            Return 0
        End Function
        Public Overloads Shared Function GetBooleanFromNumeric(ByVal Numeric As Integer) As Boolean
            If Numeric = 1 Then Return True
            Return False
        End Function
        Public Overloads Shared Function GetBooleanFromNumeric(ByVal Numeric As String) As Boolean
            If Numeric = "1" Then Return True
            Return False
        End Function
        Public Shared Function GetRoutineIntervalFromString(ByVal TextRepresentation As String) As RoutineInformation.RoutineDateInterval
            Try
                Return DirectCast([Enum].Parse(GetType(RoutineInformation.RoutineDateInterval), TextRepresentation), RoutineInformation.RoutineDateInterval)
            Catch
                Throw New Exception("'" & TextRepresentation & "' is not a valid interval.")
            End Try
        End Function
    End Class
#End Region
    Public Class Loader
        Private Sub New()

        End Sub
        Private Shared varAllTemplates As New List(Of TemplateEntry)
        Private Shared varAllSchedules As New List(Of ScheduleEntry)
        Private Shared varAllRoutines As New List(Of RoutineEntry)
        Private Shared varAllTasks As New List(Of TaskEntry)
        Private Shared varSchedulesInCategory As New Dictionary(Of String, List(Of ScheduleEntry))
        Private Shared varRoutinesInCategory As New Dictionary(Of String, List(Of RoutineEntry))
        Private Shared varTasksInCategory As New Dictionary(Of String, List(Of TaskEntry))
        Private Shared varLoadSchedulesInEachCategory, varLoadRoutinesInEachCategory, varLoadTasksInEachCategory As Boolean
        Private Shared DoLoadAll As Boolean
        Public Shared Event AllTemplatesLoaded(e As EventArgs)
        Public Shared Event AllSchedulesLoaded(e As EventArgs)
        Public Shared Event AllRoutinesLoaded(e As EventArgs)
        Public Shared Event AllTasksLoaded(e As EventArgs)

        Public Shared PendingOperations As New Dictionary(Of String, Integer) From {{"LoadTaskCategories", 0}, {"LoadTaskListInCategory", 0}, {"LoadTask", 0}, {"LoadRoutineCategories", 0}, {"LoadRoutineListInCategory", 0}, {"LoadRoutine", 0}, {"LoadScheduleCategories", 0}, {"LoadScheduleListInCategory", 0}, {"LoadSchedule", 0}, {"LoadTemplateList", 0}, {"LoadTemplate", 0}}

        Public Class StringReference
            Inherits Object
            Private varString As String = ""
            Public Property Value As String
                Get
                    Return varString
                End Get
                Set(value As String)
                    varString = value
                End Set
            End Property
            Public Sub New(ByVal Value As String)
                varString = Value
            End Sub
        End Class
        Public Shared ReadOnly Property AllTemplates As List(Of TemplateEntry)
            Get
                Return varAllTemplates
            End Get
        End Property
        Public Shared ReadOnly Property AllSchedules As List(Of ScheduleEntry)
            Get
                Return varAllSchedules
            End Get
        End Property
        Public Shared ReadOnly Property AllRoutines As List(Of RoutineEntry)
            Get
                Return varAllRoutines
            End Get
        End Property
        Public Shared ReadOnly Property AllTasks As List(Of TaskEntry)
            Get
                Return varAllTasks
            End Get
        End Property
        Public Shared ReadOnly Property ScheduleCategories As String()
            Get
                Return varSchedulesInCategory.Keys.ToArray
            End Get
        End Property
        Public Shared ReadOnly Property RoutineCategories As List(Of String)
            Get
                Return varRoutinesInCategory.Keys.ToList
            End Get
        End Property
        Public Shared ReadOnly Property TaskCategories As List(Of String)
            Get
                Return varTasksInCategory.Keys.ToList
            End Get
        End Property
        Public Shared ReadOnly Property SchedulesInCategory(ByVal CategoryName As String) As List(Of ScheduleEntry)
            Get
                Return varSchedulesInCategory(CategoryName)
            End Get
        End Property
        Public Shared ReadOnly Property RoutinesInCategory(ByVal CategoryName As String) As List(Of RoutineEntry)
            Get
                Return varRoutinesInCategory(CategoryName)
            End Get
        End Property
        Public Shared ReadOnly Property TasksInCategory(ByVal CategoryName As String) As List(Of TaskEntry)
            Get
                Return varTasksInCategory(CategoryName)
            End Get
        End Property
        Public Shared Sub AddScheduleCategory(ByVal CategoryName As String)
            If Not ScheduleCategories.Contains(CategoryName) Then
                varSchedulesInCategory.Add(CategoryName, New List(Of ScheduleEntry))
            Else
                Throw New Exception("This category is already in the collection.")
            End If
        End Sub
        Public Shared Sub AddRoutineCategory(ByVal CategoryName As String)
            If Not RoutineCategories.Contains(CategoryName) Then
                varRoutinesInCategory.Add(CategoryName, New List(Of RoutineEntry))
            Else
                Throw New Exception("This category is already in the collection.")
            End If
        End Sub
        Public Shared Sub RemoveTaskCategory(ByVal CategoryName As String) ' TODO: See how I loaded routine categories and checked for garbage instead of clearing the dictionary (which would not be desirable if we ever want to just load the categories without reloading everything else)
            If TaskCategories.Contains(CategoryName) Then
                varTasksInCategory.Remove(CategoryName)
            Else
                Throw New Exception("This category is does not exist in memory.")
            End If
        End Sub
        Public Shared Sub RemoveRoutineCategory(ByVal CategoryName As String) ' TODO: See how I loaded routine categories and checked for garbage instead of clearing the dictionary (which would not be desirable if we ever want to just load the categories without reloading everything else)
            If RoutineCategories.Contains(CategoryName) Then
                varRoutinesInCategory.Remove(CategoryName)
            Else
                Throw New Exception("This category is does not exist in memory.")
            End If
        End Sub
        Public Shared Sub RemoveScheduleCategory(ByVal CategoryName As String) ' TODO: See how I loaded routine categories and checked for garbage instead of clearing the dictionary (which would not be desirable if we ever want to just load the categories without reloading everything else)
            If ScheduleCategories.Contains(CategoryName) Then
                varSchedulesInCategory.Remove(CategoryName)
            Else
                Throw New Exception("This category is does not exist in memory.")
            End If
        End Sub
        Public Shared Sub AddTaskCategory(ByVal CategoryName As String)
            If Not TaskCategories.Contains(CategoryName) Then
                varTasksInCategory.Add(CategoryName, New List(Of TaskEntry))
            Else
                Throw New Exception("This category is already in the collection.")
            End If
        End Sub
        Public Shared Function ScheduleCategoryExists(ByVal CategoryName As String) As Boolean
            Return ScheduleCategories.Contains(CategoryName)
        End Function
        Public Shared Function RoutineCategoryExists(ByVal CategoryName As String) As Boolean
            Return RoutineCategories.Contains(CategoryName)
        End Function
        Public Shared Function TaskCategoryExists(ByVal CategoryName As String) As Boolean
            Return TaskCategories.Contains(CategoryName)
        End Function
        ''' <summary>
        ''' Important! If the necessary disk operations (renaming the category folder) have not been been executed successfully prior to a call to this method, unexpected behavior will occur.
        ''' All schedules entries contained in the category will be updated to reflect the change. This method does not perform any disk operations
        ''' </summary>
        ''' <param name="OldName">The old name (not path) of the category.</param>
        ''' <param name="NewName">The new name (not path) of the category</param>
        Public Shared Sub RenameScheduleCategory(ByVal OldName As String, ByVal NewName As String)
            Dim ContainedSchedules As List(Of ScheduleEntry) = SchedulesInCategory(OldName)
            varSchedulesInCategory.Remove(OldName)
            varSchedulesInCategory.Add(NewName, ContainedSchedules)
            For Each Schedule As ScheduleEntry In ContainedSchedules
                Schedule.CategoryName = NewName
            Next
        End Sub
        Public Shared Sub RenameTaskCategory(ByVal OldName As String, ByVal NewName As String)
            Dim ContainedTasks As List(Of TaskEntry) = TasksInCategory(OldName)
            varTasksInCategory.Remove(OldName)
            varTasksInCategory.Add(NewName, ContainedTasks)
            For Each Task As TaskEntry In ContainedTasks
                Task.CategoryName = NewName
            Next
        End Sub
        Public Shared Sub RenameRoutineCategory(ByVal OldName As String, ByVal NewName As String)
            Dim ContainedRoutines As List(Of RoutineEntry) = RoutinesInCategory(OldName)
            varRoutinesInCategory.Remove(OldName)
            varRoutinesInCategory.Add(NewName, ContainedRoutines)
            For Each Routine As RoutineEntry In ContainedRoutines
                Routine.CategoryName = NewName
            Next
        End Sub
        ''' <summary>
        ''' Important! This method does not perform any disk operations, and will not affect schedule files. The schedule entry will be removed from memory only.
        ''' Should only be used when the schedule is about to be reloaded from disk.
        ''' </summary>
        ''' <param name="Name">The name (not path) of the schedule.</param>
        Public Shared Sub RemoveSchedule(ByVal Name As String)
            Dim TargetSchedule As ScheduleEntry = AllSchedules.Find(Function(x As ScheduleEntry) As Boolean
                                                                        Return (x.Name = Name)
                                                                    End Function)
            If TargetSchedule Is Nothing Then
                Throw New Exception("No schedule named " & Name & " has been loaded.")
            Else
                SchedulesInCategory(TargetSchedule.CategoryName).Remove(TargetSchedule)
                AllSchedules.Remove(TargetSchedule)
                Dim AffectedTemplates As List(Of TemplateEntry) = AllTemplates.FindAll(Function(x As TemplateEntry) As Boolean
                                                                                           Dim Match As List(Of ScheduleInformation) = x.Schedules.FindAll(Function(s As ScheduleInformation) As Boolean
                                                                                                                                                               s.MatchingEntryExists = LookupSchedule(s.Name) IsNot Nothing
                                                                                                                                                               Return (s.Name = Name)
                                                                                                                                                           End Function)
                                                                                           Return (Match.Count > 0)
                                                                                       End Function)
            End If
            ' Maybe do something with affected templates.
        End Sub
        Public Shared Sub RemoveRoutine(ByVal Name As String)
            Dim TargetRoutines As List(Of RoutineEntry) = varAllRoutines.FindAll(Function(x As RoutineEntry) As Boolean
                                                                                     Return (x.Name = Name)
                                                                                 End Function)
            If TargetRoutines.Count = 0 Then
                Throw New Exception("No routine named " & Name & " has been loaded.")
            ElseIf TargetRoutines.Count > 1 Then
                Throw New Exception("Duplicate routine entries were found.")
            Else
                For Each Routine As RoutineEntry In TargetRoutines
                    RoutinesInCategory(Routine.CategoryName).Remove(Routine)
                    varAllRoutines.Remove(Routine)
                Next
                Dim AffectedSchedules As List(Of ScheduleEntry) = AllSchedules.FindAll(Function(x As ScheduleEntry) As Boolean
                                                                                           Dim Match As List(Of RoutineInformation) = x.Routines.FindAll(Function(s As RoutineInformation) As Boolean
                                                                                                                                                             s.MatchingEntryExists = LookupRoutine(s.Name) IsNot Nothing
                                                                                                                                                             Return (s.Name = Name)
                                                                                                                                                         End Function)
                                                                                           Return (Match.Count > 0)
                                                                                       End Function)
            End If
        End Sub
        Public Shared Sub RemoveTask(ByVal Name As String)
            Dim TargetTasks As List(Of TaskEntry) = varAllTasks.FindAll(Function(x As TaskEntry) As Boolean
                                                                            Return (x.Name = Name)
                                                                        End Function)
            If TargetTasks.Count = 0 Then
                Throw New Exception("No task named " & Name & " has been loaded.")
            ElseIf TargetTasks.Count > 1 Then
                Throw New Exception("Duplicate routine entries were found.")
            Else
                For Each Task As TaskEntry In TargetTasks
                    TasksInCategory(Task.CategoryName).Remove(Task)
                    varAllTasks.Remove(Task)
                Next
                Dim AffectedRoutines As List(Of RoutineEntry) = AllRoutines.FindAll(Function(x As RoutineEntry) As Boolean
                                                                                        Dim Match As List(Of TaskInformation) = x.Tasks.FindAll(Function(s As TaskInformation) As Boolean
                                                                                                                                                    s.MatchingEntryExists = LookupTask(s.Name) IsNot Nothing
                                                                                                                                                    Return (s.Name = Name)
                                                                                                                                                End Function)
                                                                                        Return (Match.Count > 0)
                                                                                    End Function)
            End If
        End Sub
        Public Shared Function LookupTemplate(ByVal Name As String) As TemplateEntry
            Dim Ret As TemplateEntry = varAllTemplates.Find(Function(Item As TemplateEntry) As Boolean
                                                                Return (Item.Name = Name)
                                                            End Function)
            Return Ret
        End Function
        Public Shared Function LookupSchedule(ByVal Name As String) As ScheduleEntry
            Dim Ret As ScheduleEntry = varAllSchedules.Find(Function(Item As ScheduleEntry) As Boolean
                                                                Return (Item.Name = Name)
                                                            End Function)
            Return Ret
        End Function
        Public Shared Function LookupRoutine(ByVal Name As String) As RoutineEntry
            Dim Ret As RoutineEntry = varAllRoutines.Find(Function(Item As RoutineEntry) As Boolean
                                                              Return (Item.Name = Name)
                                                          End Function)
            Return Ret
        End Function
        Public Shared Function LookupTask(ByVal Name As String) As TaskEntry
            Dim Ret As TaskEntry = varAllTasks.Find(Function(Item As TaskEntry) As Boolean
                                                        Return (Item.Name = Name)
                                                    End Function)
            Return Ret
        End Function
        Public Overloads Shared Sub AddTemplate(Template As TemplateEntry)
            If Not varAllTemplates.Contains(Template) Then
                varAllTemplates.Add(Template)
            Else
                Throw New Exception("This template is already in the list.")
            End If
        End Sub
        Public Overloads Shared Sub AddTemplate(Path As String)
            If LookupTemplate(IO.Path.GetFileNameWithoutExtension(Path)) IsNot Nothing Then
                Throw New Exception("This schedule is already in the collection")
            Else
                AsyncFileReader.Queue.AddOperation(AsyncFileReader.Queue.FileOperation.ReadLines(Path), AddressOf TemplateLoadedFromPath, Path.Clone)
            End If
        End Sub
        Private Shared Sub TemplateLoadedFromPath(e As FileOperationEventArgs)
            Dim Lines() As String = DirectCast(e.Result, String())
            Dim FullPath As String = DirectCast(e.State, String)
            Dim NewTemplate As TemplateEntry = TemplateEntry.CreateFromLines(Lines, FullPath)
            varAllTemplates.Add(NewTemplate)
        End Sub
        Public Overloads Shared Sub AddSchedule(Schedule As ScheduleEntry)
            If LookupSchedule(Schedule.Name) IsNot Nothing Then Throw New Exception("This schedule is already in the list.")
            varAllSchedules.Add(Schedule)
            varSchedulesInCategory(Schedule.CategoryName).Add(Schedule)
        End Sub
        Public Overloads Shared Sub AddSchedule(Path As String)
            If LookupSchedule(IO.Path.GetFileNameWithoutExtension(Path)) IsNot Nothing Then Throw New Exception("This schedule is already in the collection")
            AsyncFileReader.Queue.AddOperation(AsyncFileReader.Queue.FileOperation.ReadLines(Path), AddressOf ScheduleLoadedFromPath, Path.Clone)
        End Sub
        Private Shared Sub ScheduleLoadedFromPath(e As FileOperationEventArgs)
            Dim Lines() As String = DirectCast(e.Result, String())
            Dim FullPath As String = DirectCast(e.State, String)
            Dim NewSchedule As ScheduleEntry = ScheduleEntry.CreateFromLines(Lines, FullPath)
            varAllSchedules.Add(NewSchedule)
            varSchedulesInCategory(NewSchedule.CategoryName).Add(NewSchedule)
        End Sub
        Public Overloads Shared Sub AddRoutine(Routine As RoutineEntry)
            If LookupRoutine(Routine.Name) IsNot Nothing Then Throw New Exception("This routine is already in the list.")
            varAllRoutines.Add(Routine)
            varRoutinesInCategory(Routine.CategoryName).Add(Routine)
        End Sub
        Public Overloads Shared Sub AddRoutine(Path As String)
            If LookupRoutine(IO.Path.GetFileNameWithoutExtension(Path)) IsNot Nothing Then Throw New Exception("This routine is already in the collection")
            AsyncFileReader.Queue.AddOperation(AsyncFileReader.Queue.FileOperation.ReadLines(Path), AddressOf RoutineLoadedFromPath, Path.Clone)
        End Sub
        Private Shared Sub RoutineLoadedFromPath(e As FileOperationEventArgs)
            Dim Lines() As String = DirectCast(e.Result, String())
            Dim FullPath As String = DirectCast(e.State, String)
            Dim NewRoutine As RoutineEntry = RoutineEntry.CreateFromLines(Lines, FullPath)
            varAllRoutines.Add(NewRoutine)
            varRoutinesInCategory(NewRoutine.CategoryName).Add(NewRoutine)
        End Sub
        Public Overloads Shared Sub AddTask(Task As TaskEntry)
            If LookupTask(Task.Name) IsNot Nothing Then Throw New Exception("This task is already in the list.")
            varAllTasks.Add(Task)
            varTasksInCategory(Task.CategoryName).Add(Task)
        End Sub
        Public Overloads Shared Sub AddTask(Path As String)
            If LookupTask(IO.Path.GetFileNameWithoutExtension(Path)) IsNot Nothing Then Throw New Exception("This task is already in the collection")
            AsyncFileReader.Queue.AddOperation(AsyncFileReader.Queue.FileOperation.ReadLines(Path), AddressOf TaskLoadedFromPath, Path.Clone)
        End Sub
        Private Shared Sub TaskLoadedFromPath(e As FileOperationEventArgs)
            Dim Lines() As String = DirectCast(e.Result, String())
            Dim FullPath As String = DirectCast(e.State, String)
            Dim NewTask As TaskEntry = TaskEntry.CreateFromLines(Lines, FullPath)
            varAllTasks.Add(NewTask)
            varTasksInCategory(NewTask.CategoryName).Add(NewTask)
        End Sub
        Public Shared Sub LoadAll()
            DoLoadAll = True
            LoadTaskCategories(True)
        End Sub
        Public Shared Sub LoadTaskCategories(LoadTasksInEach As Boolean)
            If PendingOperations("LoadTaskCategories") = 0 Then
                varTasksInCategory.Clear() ' TODO: Don't. See how routine categories are loaded
                varLoadTasksInEachCategory = LoadTasksInEach
                If LoadTasksInEach Then varAllTasks.Clear()
                PendingOperations("LoadTaskCategories") += 1
                AsyncFileReader.Queue.AddOperation(AsyncFileReader.Queue.FileOperation.ListDirectories(TaskEntry.BaseDirectory), AddressOf TaskCategoriesLoaded, Nothing)
            End If
        End Sub
        Private Shared Sub TaskCategoriesLoaded(e As FileOperationEventArgs)
            PendingOperations("LoadTaskCategories") -= 1
            Dim Result As String() = DirectCast(e.Result, String())
            For Each Line As String In Result
                Dim CategoryName As String = FormatConverter.ExtractHighestLevelDirectory(Line, False)
                If Not varTasksInCategory.ContainsKey(CategoryName) Then varTasksInCategory.Add(CategoryName, New List(Of TaskEntry))
                If varLoadTasksInEachCategory Then
                    varTasksInCategory(CategoryName).Clear()
                    PendingOperations("LoadTaskListInCategory") += 1
                    AsyncFileReader.Queue.AddOperation(AsyncFileReader.Queue.FileOperation.ListFiles(Line), AddressOf TaskListInCategoryLoaded, Nothing)
                End If
            Next
        End Sub
        Private Shared Sub TaskListInCategoryLoaded(e As FileOperationEventArgs)
            PendingOperations("LoadTaskListInCategory") -= 1
            Dim TaskFiles() As String = DirectCast(e.Result, String())
            Dim CurrentCategory As String = FormatConverter.ExtractHighestLevelDirectory(DirectCast(e.Arguments("Path"), String), False)
            varTasksInCategory(CurrentCategory).Capacity = TaskFiles.Count + 10
            For Each Filename As String In TaskFiles
                PendingOperations("LoadTask") += 1
                AsyncFileReader.Queue.AddOperation(AsyncFileReader.Queue.FileOperation.ReadLines(Filename), AddressOf TaskLoaded, Filename.Clone)
            Next
        End Sub
        Private Shared Sub TaskLoaded(e As FileOperationEventArgs)
            PendingOperations("LoadTask") -= 1
            Dim Lines() As String = DirectCast(e.Result, String())
            Dim FullPath As String = DirectCast(e.State, String)
            Dim NewTask As TaskEntry = TaskEntry.CreateFromLines(Lines, FullPath)
            varAllTasks.Add(NewTask)
            varTasksInCategory(NewTask.CategoryName).Add(NewTask)
            'If AsyncFileReader.Queue.NextInQueue Is Nothing Then ' TODO: Check: If queue is not empty, see if the remaining operations are the same type of operation
            If PendingOperations("LoadTask") = 0 Then
                varLoadTasksInEachCategory = False
                RaiseEvent AllTasksLoaded(EventArgs.Empty)
                If DoLoadAll Then
                    LoadRoutineCategories(True)
                End If
            End If
        End Sub
        Public Shared Sub LoadRoutineCategories(LoadRoutinesInEach As Boolean)
            'varRoutinesInCategory.Clear()
            PendingOperations("LoadRoutineCategories") += 1
            varLoadRoutinesInEachCategory = LoadRoutinesInEach
            If varLoadRoutinesInEachCategory Then varAllRoutines.Clear()
            AsyncFileReader.Queue.AddOperation(AsyncFileReader.Queue.FileOperation.ListDirectories(RoutineEntry.BaseDirectory), AddressOf RoutineCategoriesLoaded, Nothing)
        End Sub
        Private Shared Sub RoutineCategoriesLoaded(e As FileOperationEventArgs)
            PendingOperations("LoadRoutineCategories") -= 1
            Dim Result As String() = DirectCast(e.Result, String())
            Dim LoadedCategories As List(Of String) = varRoutinesInCategory.Keys.ToList
            For Each Line As String In Result
                Dim CategoryName As String = FormatConverter.ExtractHighestLevelDirectory(Line, False)
                If Not varRoutinesInCategory.ContainsKey(CategoryName) Then
                    varRoutinesInCategory.Add(CategoryName, New List(Of RoutineEntry))
                Else
                    LoadedCategories.Remove(CategoryName)
                End If
                If varLoadRoutinesInEachCategory Then
                    varRoutinesInCategory(CategoryName).Clear()
                    PendingOperations("LoadRoutineListInCategory") += 1
                    AsyncFileReader.Queue.AddOperation(AsyncFileReader.Queue.FileOperation.ListFiles(Line), AddressOf RoutineListInCategoryLoaded, Nothing)
                End If
            Next
            If LoadedCategories.Count > 0 Then ' There are leftover categories in memory that, according to the load operation we just did, should not be there
                MsgBox("Found " & LoadedCategories.Count & " leftover routine categories.")
                For Each Category As String In LoadedCategories
                    RemoveRoutineCategory(Category)
                Next
            End If
        End Sub
        Private Shared Sub RoutineListInCategoryLoaded(e As FileOperationEventArgs)
            PendingOperations("LoadRoutineListInCategory") -= 1
            Dim RoutineFiles() As String = DirectCast(e.Result, String())
            Dim CurrentCategory As String = FormatConverter.ExtractHighestLevelDirectory(DirectCast(e.Arguments("Path"), String), False)
            varRoutinesInCategory(CurrentCategory).Capacity = RoutineFiles.Count + 10
            For Each Filename As String In RoutineFiles
                PendingOperations("LoadRoutine") += 1
                AsyncFileReader.Queue.AddOperation(AsyncFileReader.Queue.FileOperation.ReadLines(Filename), AddressOf RoutineLoaded, Filename.Clone)
            Next
        End Sub
        Private Shared Sub RoutineLoaded(e As FileOperationEventArgs)
            PendingOperations("LoadRoutine") -= 1
            Dim Lines() As String = DirectCast(e.Result, String())
            Dim FullPath As String = DirectCast(e.State, String)
            Dim NewRoutine As RoutineEntry = RoutineEntry.CreateFromLines(Lines, FullPath)
            varAllRoutines.Add(NewRoutine)
            varRoutinesInCategory(NewRoutine.CategoryName).Add(NewRoutine)
            If PendingOperations("LoadRoutine") = 0 Then
                RaiseEvent AllRoutinesLoaded(EventArgs.Empty)
                varLoadRoutinesInEachCategory = False
                If DoLoadAll Then LoadScheduleCategories(True)
            End If
        End Sub
        Private Shared Sub LoadScheduleCategories(LoadSchedulesInEach As Boolean)
            varSchedulesInCategory.Clear()
            varLoadSchedulesInEachCategory = LoadSchedulesInEach
            If LoadSchedulesInEach Then varAllSchedules.Clear()
            PendingOperations("LoadScheduleCategories") += 1
            AsyncFileReader.Queue.AddOperation(AsyncFileReader.Queue.FileOperation.ListDirectories(ScheduleEntry.BaseDirectory), AddressOf ScheduleCategoriesLoaded, Nothing)
        End Sub
        Private Shared Sub ScheduleCategoriesLoaded(e As FileOperationEventArgs)
            PendingOperations("LoadScheduleCategories") -= 1
            Dim Result As String() = DirectCast(e.Result, String())
            For Each Line As String In Result
                Dim CategoryName As String = FormatConverter.ExtractHighestLevelDirectory(Line, False)
                If Not varSchedulesInCategory.ContainsKey(CategoryName) Then varSchedulesInCategory.Add(CategoryName, New List(Of ScheduleEntry))
                If varLoadSchedulesInEachCategory Then
                    varSchedulesInCategory(CategoryName).Clear()
                    PendingOperations("LoadScheduleListInCategory") += 1
                    AsyncFileReader.Queue.AddOperation(AsyncFileReader.Queue.FileOperation.ListFiles(Line), AddressOf ScheduleListInCategoryLoaded, Nothing)
                End If
            Next
        End Sub
        Private Shared Sub ScheduleListInCategoryLoaded(e As FileOperationEventArgs)
            PendingOperations("LoadScheduleListInCategory") -= 1
            Dim ScheduleFiles() As String = DirectCast(e.Result, String())
            Dim CurrentCategory As String = FormatConverter.ExtractHighestLevelDirectory(DirectCast(e.Arguments("Path"), String), False)
            varSchedulesInCategory(CurrentCategory).Capacity = ScheduleFiles.Count + 10
            For Each Filename As String In ScheduleFiles
                PendingOperations("LoadSchedule") += 1
                AsyncFileReader.Queue.AddOperation(AsyncFileReader.Queue.FileOperation.ReadLines(Filename), AddressOf ScheduleLoaded, Filename.Clone)
            Next
        End Sub
        Private Shared Sub ScheduleLoaded(e As FileOperationEventArgs)
            PendingOperations("LoadSchedule") -= 1
            Dim Lines() As String = DirectCast(e.Result, String())
            Dim FullPath As String = DirectCast(e.State, String)
            Dim NewSchedule As ScheduleEntry = ScheduleEntry.CreateFromLines(Lines, FullPath)
            varAllSchedules.Add(NewSchedule)
            varSchedulesInCategory(NewSchedule.CategoryName).Add(NewSchedule)
            If PendingOperations("LoadSchedule") = 0 Then
                varLoadSchedulesInEachCategory = False
                RaiseEvent AllSchedulesLoaded(EventArgs.Empty)
                LoadTemplates()
            End If
        End Sub
        Public Shared Sub LoadTemplates()
            If PendingOperations("LoadTemplateList") = 0 Then
                PendingOperations("LoadTemplateList") += 1
                AsyncFileReader.Queue.AddOperation(AsyncFileReader.Queue.FileOperation.ListFiles(TemplateEntry.BaseDirectory), AddressOf TemplateListLoaded, Nothing)
            Else
                MsgBox("Please wait")
            End If
        End Sub
        Private Shared Sub TemplateListLoaded(e As FileOperationEventArgs)
            PendingOperations("LoadTemplateList") -= 1
            Dim TemplateFiles() As String = DirectCast(e.Result, String())
            varAllTemplates.Clear()
            For Each File As String In TemplateFiles
                PendingOperations("LoadTemplate") += 1
                AsyncFileReader.Queue.AddOperation(AsyncFileReader.Queue.FileOperation.ReadLines(File), AddressOf TemplateLoaded, New StringReference(File))
            Next
        End Sub
        Private Shared Sub TemplateLoaded(e As FileOperationEventArgs)
            PendingOperations("LoadTemplate") -= 1
            Dim NewTemplate As TemplateEntry = TemplateEntry.CreateFromLines(DirectCast(e.Result, String()), DirectCast(e.State, StringReference).Value)
            varAllTemplates.Add(NewTemplate)
            If PendingOperations("LoadTemplate") = 0 Then
                RaiseEvent AllTemplatesLoaded(EventArgs.Empty)
                If DoLoadAll Then OnAllLoaded()
            End If
        End Sub
        Private Shared Sub OnAllLoaded()
            DoLoadAll = False

        End Sub
    End Class
#Region "Categories"
    Public MustInherit Class ComponentEntry
        ' Meta-data:
        Private varName, varCategoryName, varDescription As String
        ' Configuration:
        'Private ChangeCategoryTo As String
        Protected Shared DataFolderPath As String = "C:\Users\Magnus\Desktop\Prosjekt\Mappestruktur\Data" ' TODO: Get from startup path
        Public Property Name As String
            Get
                Return varName
            End Get
            Set(value As String)
                varName = value
            End Set
        End Property
        Public Property Description As String
            Get
                Return varDescription
            End Get
            Set(value As String)
                varDescription = value
            End Set
        End Property
        Public Property CategoryName As String
            Get
                Return varCategoryName
            End Get
            Set(value As String)
                varCategoryName = value
            End Set
        End Property
        ''' <summary>
        ''' This will return the data folder path.
        ''' </summary>
        ''' <returns></returns>
        Public Shared ReadOnly Property BaseDirectory As String
            Get
                Return DataFolderPath
            End Get
        End Property
        ''' <summary>
        ''' This method will return the correct base directory, even if the narrow type (for example ScheduleEntry) is not known, but can sometimes not be used.
        ''' </summary>
        ''' <returns></returns>
        Public MustOverride Function GetBaseDirectory() As String
        Protected Sub New()

        End Sub
        Public Overridable Function GetFileNameFromName() As String
            Return varName & ".txt"
        End Function
        Public Shared Function ExtractFromPath(ByVal FilePath As String, ByVal ExtractName As Boolean, ByVal ExtractCategory As Boolean) As String()
            Dim ExtractedName As String = Nothing
            Dim ExtractedCategory As String = Nothing
            Dim Parts() As String = FilePath.Split(New String() {"\"}, StringSplitOptions.RemoveEmptyEntries)
            If ExtractName Then
                ExtractedName = Parts.Last.Split(New String() {"."}, StringSplitOptions.RemoveEmptyEntries).First
            End If
            If ExtractCategory Then
                ExtractedCategory = Parts(Parts.Count - 2)
            End If
            Return New String() {ExtractedName, ExtractedCategory}
        End Function
        Public Overridable Function GetFullFilePath() As String
            Return BaseDirectory & "\" & CategoryName & "\" & GetFileNameFromName()
        End Function
        Public Overridable Function GetShortFilePath() As String
            Return FormatConverter.ExtractHighestLevelDirectory(BaseDirectory, False) & "\" & CategoryName & "\" & GetFileNameFromName()
        End Function
        Public Overridable Sub ChangeCategoryAndMoveFile(ByVal NewCategoryName As String, WhenFinished As Action(Of FileOperationEventArgs), State As Object)
            If NewCategoryName <> CategoryName Then
                Select Case Me.GetType
                    Case GetType(ScheduleEntry)
                        Loader.SchedulesInCategory(CategoryName).Remove(DirectCast(Me, ScheduleEntry))
                        Loader.SchedulesInCategory(NewCategoryName).Add(DirectCast(Me, ScheduleEntry))
                        'For Each Routine As RoutineInformation In DirectCast(Me, ScheduleEntry).Routines
                        '    Routine.MatchingEntryExists = Loader.LookupRoutine(Routine.Name) IsNot Nothing
                        'Next
                    Case GetType(RoutineEntry)
                        Loader.RoutinesInCategory(CategoryName).Remove(DirectCast(Me, RoutineEntry))
                        Loader.RoutinesInCategory(NewCategoryName).Add(DirectCast(Me, RoutineEntry))
                        'For Each Task As TaskInformation In DirectCast(Me, RoutineEntry).Tasks
                        '    Task.MatchingEntryExists = Loader.LookupTask(Task.Name) IsNot Nothing
                        'Next
                    Case GetType(TaskEntry)
                        Loader.TasksInCategory(CategoryName).Remove(DirectCast(Me, TaskEntry))
                        Loader.TasksInCategory(NewCategoryName).Add(DirectCast(Me, TaskEntry))
                    Case Else
                        Throw New NotImplementedException("This method cannot be used on templates.")
                End Select
                Dim OldPath As String = GetFullFilePath()
                Dim NewPath As String = GetBaseDirectory() & "\" & NewCategoryName & "\" & GetFileNameFromName()
                CategoryName = NewCategoryName
                AsyncFileReader.Queue.AddOperation(AsyncFileReader.Queue.FileOperation.MoveFile(OldPath, NewPath, False), WhenFinished, State)
            End If
        End Sub
        'Protected Overridable Sub OnCategoryChanged(e As FileOperationEventArgs)
        '    If e.ErrorOccurred Then
        '        ChangeCategoryTo = Nothing
        '        If Not e.ExceptionHandled Then
        '            Throw e.Exception
        '        End If
        '    Else
        '        CategoryName = ChangeCategoryTo
        '        ChangeCategoryTo = Nothing
        '    End If
        'End Sub
        Public Overridable Function GetDirectoryFromCategory() As String
            Return BaseDirectory & "\" & CategoryName
        End Function
        Public MustOverride Function OutputFileContents() As String()
        Public MustOverride Sub ApplyFileContents(Lines As String())
        Public Shared Function CreateFromLines(Lines As String(), FullPath As String) As ComponentEntry
            Throw New NotImplementedException
        End Function
    End Class
    Public Class ScheduleEntry
        Inherits ComponentEntry
        Private varRoutineList As New List(Of RoutineInformation)
        Public ReadOnly Property Routines As List(Of RoutineInformation)
            Get
                Return varRoutineList
            End Get
        End Property
        Public Shared Shadows ReadOnly Property BaseDirectory As String
            Get
                Return DataFolderPath & "\Schedules"
            End Get
        End Property
        Public Overrides Function GetFullFilePath() As String
            Return BaseDirectory & "\" & CategoryName & "\" & GetFileNameFromName()
        End Function
        Public Overrides Function GetBaseDirectory() As String
            Return DataFolderPath & "\Schedules"
        End Function
        Public Overrides Sub ApplyFileContents(Lines() As String)
            Throw New NotImplementedException()
        End Sub
        Public Overrides Function OutputFileContents() As String()
            Dim Lines As New List(Of String) From {"// meta", "$meta: %desc%==" & Description}
            For Each Routine As RoutineInformation In Routines
                Lines.Add(Routine.OutputLine)
            Next
            Return Lines.ToArray
        End Function
        Public Shared Shadows Function CreateFromLines(Lines() As String, FullPath As String) As ScheduleEntry
            Dim NameAndCategory As String() = ExtractFromPath(FullPath, True, True)
            Dim Ret As New ScheduleEntry
            With Ret
                .Name = NameAndCategory(0)
                .CategoryName = NameAndCategory(1)
            End With
            For Each Line As String In Lines
                If Line.StartsWith("//") Then Continue For ' TODO: Lagre kommentarer
                If Line.StartsWith("$meta: ") Then
                    ' Fjern "$meta: " fra linjen (7 tegn)
                    Dim CutLine As String = Line.Remove(0, 7)
                    ' Separer de ulike egenskap-verdi-parene
                    Dim Parts() As String = CutLine.Split(New String() {" && "}, StringSplitOptions.RemoveEmptyEntries)
                    For Each Part As String In Parts
                        ' Separerer egenskap og verdi
                        Dim PropertyValuePair() As String = Part.Split(New String() {"=="}, StringSplitOptions.RemoveEmptyEntries)
                        ' Finn ut hvilken egenskap som skal settes
                        Select Case PropertyValuePair(0)
                            Case "%desc%" ' Description
                                If PropertyValuePair.Count = 2 Then
                                    Ret.Description = PropertyValuePair(1)
                                Else
                                    Ret.Description = ""
                                End If
                            Case Else
                                Throw New Exception("Unknown property: " & PropertyValuePair(0))
                        End Select
                    Next
                ElseIf Line.StartsWith("$routine: ") Then
                    Dim NewRoutineInformation As RoutineInformation = RoutineInformation.CreateFromLine(Line)
                    NewRoutineInformation.MatchingEntryExists = (Loader.LookupRoutine(NewRoutineInformation.Name) IsNot Nothing)
                    Ret.Routines.Add(NewRoutineInformation)
                End If
            Next
            Return Ret
        End Function
    End Class

    Public Class RoutineEntry
        Inherits ComponentEntry
        Private varTaskList As New List(Of TaskInformation)
        Public ReadOnly Property Tasks As List(Of TaskInformation)
            Get
                Return varTaskList
            End Get
        End Property
        Public Shared Shadows ReadOnly Property BaseDirectory As String
            Get
                Return DataFolderPath & "\Routines"
            End Get
        End Property
        Public Overrides Function GetBaseDirectory() As String
            Return DataFolderPath & "\Routines"
        End Function
        Public Overrides Sub ApplyFileContents(Lines() As String)
            Throw New NotImplementedException()
        End Sub
        Public Overrides Function OutputFileContents() As String()
            Dim Ret As New List(Of String) From {"// meta", "$meta: %desc%==" & Description, "// tasks"}
            For Each Task As TaskInformation In Tasks
                Ret.Add(Task.OutputLine)
            Next
            Return Ret.ToArray
        End Function
        Public Overrides Function GetFullFilePath() As String
            Return BaseDirectory & "\" & CategoryName & "\" & GetFileNameFromName()
        End Function
        Public Shared Shadows Function CreateFromLines(Lines() As String, FullPath As String) As RoutineEntry
            Dim NameAndCategory As String() = ExtractFromPath(FullPath, True, True)
            Dim Ret As New RoutineEntry
            With Ret
                .Name = NameAndCategory(0)
                .CategoryName = NameAndCategory(1)
            End With
            For Each Line As String In Lines
                If Line.StartsWith("//") Then Continue For ' TODO: Lagre kommentarer
                If Line.StartsWith("$meta: ") Then
                    ' Fjern "$meta: " fra linjen (7 tegn)
                    Dim CutLine As String = Line.Remove(0, 7)
                    ' Separer de ulike egenskap-verdi-parene
                    Dim Parts() As String = CutLine.Split(New String() {" && "}, StringSplitOptions.RemoveEmptyEntries)
                    For Each Part As String In Parts
                        ' Separerer egenskap og verdi
                        Dim PropertyValuePair() As String = Part.Split(New String() {"=="}, StringSplitOptions.RemoveEmptyEntries)
                        ' Finn ut hvilken egenskap som skal settes
                        Select Case PropertyValuePair(0)
                            Case "%desc%" ' Description
                                Ret.Description = PropertyValuePair(1)
                            Case Else
                                Throw New Exception("Unknown property: " & PropertyValuePair(0))
                        End Select
                    Next
                ElseIf Line.StartsWith("$task: ") Then
                    Dim NewTaskInformation As TaskInformation = TaskInformation.CreateFromLine(Line)
                    NewTaskInformation.MatchingEntryExists = (Loader.LookupTask(NewTaskInformation.Name) IsNot Nothing)
                    Ret.Tasks.Add(NewTaskInformation)
                End If
            Next
            Return Ret
        End Function
    End Class

    Public Class PSParameterInfo
        Private varParameterName As String
        Private varComment As String
        Private varType As ArgumentValueType
        Public Sub New(ParameterName As String, Comment As String)
            varParameterName = ParameterName
            varComment = Comment
        End Sub
        Public Sub New()
        End Sub
        Public Property ValueType As ArgumentValueType
            Get
                Return varType
            End Get
            Set(value As ArgumentValueType)
                varType = value
            End Set
        End Property
        Public Property ParameterName As String
            Get
                Return varParameterName
            End Get
            Set(value As String)
                varParameterName = value
            End Set
        End Property
        Public Property Comment As String
            Get
                Return varComment
            End Get
            Set(value As String)
                varComment = value
            End Set
        End Property
        Public Enum ArgumentValueType As Integer
            INT = 0
            STR = 1
            OBJ = 2
        End Enum
    End Class
    Public Class TaskEntry
        Inherits ComponentEntry
        Private varParameters As New List(Of PSParameterInfo)
        Private varScriptPath As String
        'Private varScript As PSTask
        Public Property ScriptPath As String
            Get
                Return varScriptPath
            End Get
            Set(value As String)
                varScriptPath = value
            End Set
        End Property
        Public ReadOnly Property Parameters As List(Of PSParameterInfo)
            Get
                Return varParameters
            End Get
        End Property
        Public Shared Shadows ReadOnly Property BaseDirectory As String
            Get
                Return DataFolderPath & "\Tasks"
            End Get
        End Property
        Public Overrides Function GetBaseDirectory() As String
            Return DataFolderPath & "\Tasks"
        End Function
        Public Overrides Function GetFullFilePath() As String
            Return BaseDirectory & "\" & CategoryName & "\" & GetFileNameFromName()
        End Function
        Public Overrides Sub ApplyFileContents(Lines() As String)
            Throw New NotImplementedException()
        End Sub
        Public Overrides Function OutputFileContents() As String()
            Dim Ret As New List(Of String) From {"// meta", "$meta: %desc%==" & Description & " && %path%==" & ScriptPath, "// parameters"}
            For Each Parameter As PSParameterInfo In Parameters
                Ret.Add("%parameter: %name%==" & Parameter.ParameterName & " && %type%==" & Parameter.ValueType.ToString & " && %comment%==" & Parameter.Comment)
            Next
            Return Ret.ToArray
        End Function
        Public Shared Shadows Function CreateFromLines(Lines() As String, FullPath As String) As TaskEntry
            Dim NameAndCategory As String() = ExtractFromPath(FullPath, True, True)
            Dim Ret As New TaskEntry
            With Ret
                .Name = NameAndCategory(0)
                .CategoryName = NameAndCategory(1)
            End With
            For Each Line As String In Lines
                If Line.StartsWith("//") Then Continue For ' TODO: Save comments
                If Line.StartsWith("$meta: ") Then
                    Dim CutLine As String = Line.Remove(0, 7)
                    Dim Parts() As String = CutLine.Split(New String() {" && "}, StringSplitOptions.RemoveEmptyEntries)
                    For Each Part As String In Parts
                        Dim PropertyValuePair() As String = Part.Split(New String() {"=="}, StringSplitOptions.RemoveEmptyEntries)
                        Select Case PropertyValuePair(0)
                            Case "%desc%"
                                Ret.Description = PropertyValuePair(1)
                            Case "%path%"
                                Ret.ScriptPath = PropertyValuePair(1)
                            Case Else
                                Throw New Exception("Unknown property: " & PropertyValuePair(0))
                        End Select
                    Next
                ElseIf Line.StartsWith("$parameter: ") Then
                    Dim Parameter As New PSParameterInfo
                    Dim CutLine As String = Line.Remove(0, 12)
                    Dim Parts() As String = CutLine.Split(New String() {" && "}, StringSplitOptions.RemoveEmptyEntries)
                    For Each Part As String In Parts
                        Dim PropertyValuePair() As String = Part.Split(New String() {"=="}, StringSplitOptions.RemoveEmptyEntries)
                        Select Case PropertyValuePair(0)
                            Case "%name%"
                                Parameter.ParameterName = PropertyValuePair(1)
                            Case "%type%"
                                Parameter.ValueType = DirectCast([Enum].Parse(GetType(PSParameterInfo.ArgumentValueType), PropertyValuePair(1)), PSParameterInfo.ArgumentValueType)
                            Case "%comment%"
                                Parameter.Comment = PropertyValuePair(1)
                            Case Else
                                Throw New Exception("Unknown property: " & PropertyValuePair(0))
                        End Select
                    Next
                    Ret.Parameters.Add(Parameter)
                End If
            Next
            Return Ret
        End Function
    End Class

    Public Class TemplateEntry
        Inherits ComponentEntry
        Private varScheduleList As New List(Of ScheduleInformation)
        Public ReadOnly Property Schedules As List(Of ScheduleInformation)
            Get
                Return varScheduleList
            End Get
        End Property
        ''' <summary>
        ''' This will return the BaseDirectory for TemplateEntry.
        ''' </summary>
        ''' <returns></returns>
        Public Shared Shadows ReadOnly Property BaseDirectory As String ' Only works when the type is known to be TemplateEntry
            Get
                Return DataFolderPath & "\Templates"
            End Get
        End Property
        Public Overrides Function GetFullFilePath() As String
            Return DataFolderPath & "\Templates\" & GetFileNameFromName()
        End Function
        Public Overrides Function GetBaseDirectory() As String ' Accessed through non-shared methods
            Return DataFolderPath & "\Templates"
        End Function
        Public Shared Shadows Function ExtractFromPath(ByVal FullPath As String) As String
            Dim Parts() As String = FullPath.Split(New String() {"\"}, StringSplitOptions.RemoveEmptyEntries)
            Dim ExtractedName As String = Parts.Last.Split(New String() {"."}, StringSplitOptions.RemoveEmptyEntries).First
            Return ExtractedName
        End Function
        Public Overrides Function GetShortFilePath() As String
            Return FormatConverter.ExtractHighestLevelDirectory(BaseDirectory, False) & "\" & GetFileNameFromName()
        End Function
        Public Overrides Sub ApplyFileContents(Lines() As String)
            Throw New NotImplementedException()
        End Sub
        Public Overrides Function OutputFileContents() As String()
            Dim Lines As New List(Of String) From {"// meta", "$meta: %desc%==" & Description & " && %category%==" & CategoryName, "// schedules"}
            For Each Schedule As ScheduleInformation In Schedules
                Lines.Add(Schedule.OutputLine)
            Next
            Return Lines.ToArray
        End Function
        Public Shared Shadows Function CreateFromLines(Lines() As String, FullPath As String) As TemplateEntry
            Dim Ret As New TemplateEntry With {.Name = ExtractFromPath(FullPath)}
            For Each Line As String In Lines
                If Line.StartsWith("//") Then Continue For ' TODO: Lagre kommentarer
                If Line.StartsWith("$meta: ") Then
                    ' Fjern "$meta: " fra linjen (7 tegn)
                    Dim CutLine As String = Line.Remove(0, 7)
                    ' Separer de ulike egenskap-verdi-parene
                    Dim Parts() As String = CutLine.Split(New String() {" && "}, StringSplitOptions.RemoveEmptyEntries)
                    For Each Part As String In Parts
                        ' Separerer egenskap og verdi
                        Dim PropertyValuePair() As String = Part.Split(New String() {"=="}, StringSplitOptions.RemoveEmptyEntries)
                        ' Finn ut hvilken egenskap som skal settes
                        Select Case PropertyValuePair(0)
                            Case "%desc%" ' Description
                                Ret.Description = PropertyValuePair(1)
                            Case "%category%"
                                Ret.CategoryName = PropertyValuePair(1)
                            Case Else
                                Throw New Exception("Unknown property: " & PropertyValuePair(0))
                        End Select
                    Next
                ElseIf Line.StartsWith("$schedule: ") Then
                    Dim NewScheduleInformation As ScheduleInformation = ScheduleInformation.CreateFromLine(Line)
                    NewScheduleInformation.MatchingEntryExists = (Loader.LookupSchedule(NewScheduleInformation.Name) IsNot Nothing)
                    Ret.Schedules.Add(NewScheduleInformation)
                End If
            Next
            Return Ret
        End Function
    End Class

    Public Class CategoryEntry
        Private varCategoryName As String
        Public ReadOnly Property Name As String
            Get
                Return varCategoryName
            End Get
        End Property
        Public Sub New(ByVal Name As String)
            varCategoryName = Name
        End Sub
        Public Shared Function CreateFromName(ByVal CategoryName As String) As CategoryEntry
            Return New CategoryEntry(CategoryName)
        End Function
        Public Overrides Function ToString() As String
            Return varCategoryName
        End Function
    End Class

    Public MustInherit Class ChildComponentInformation
        Private varName As String
        Private varActive As Boolean
        Private varMatchingEntryExists As Boolean = True
        Public Property MatchingEntryExists As Boolean
            Get
                Return varMatchingEntryExists
            End Get
            Set(value As Boolean)
                varMatchingEntryExists = value
            End Set
        End Property
        Public Property Name As String
            Get
                Return varName
            End Get
            Set(value As String)
                varName = value
            End Set
        End Property
        Public Property Active As Boolean
            Get
                Return varActive
            End Get
            Set(value As Boolean)
                varActive = value
            End Set
        End Property
        Public Sub New(Name As String, Active As Boolean)
            varName = Name
            varActive = Active
        End Sub
        Public Shared Function CreateFromLine(ByVal Line As String) As ChildComponentInformation
            Throw New NotImplementedException
        End Function
        Public Overridable Function OutputLine() As String
            Throw New NotImplementedException
        End Function
        Public MustOverride Function CopyUnknown() As ChildComponentInformation
        Public Overridable Function Copy() As ChildComponentInformation
            Throw New NotImplementedException
        End Function
    End Class
    Public Class ScheduleInformation
        Inherits ChildComponentInformation
        Public Shared Shadows Function CreateFromLine(Line As String) As ScheduleInformation
            Dim RetName As String = Nothing
            Dim RetActive As Boolean
            Dim CutLine As String = Line.Remove(0, 11)
            Dim Parts() As String = CutLine.Split(New String() {" && "}, StringSplitOptions.RemoveEmptyEntries)
            For Each Part As String In Parts
                Dim PropertyValuePair() As String = Part.Split(New String() {"=="}, StringSplitOptions.RemoveEmptyEntries)
                ' Finn ut hvilken egenskap som skal settes
                Select Case PropertyValuePair(0)
                    Case "%name%" ' Description
                        RetName = PropertyValuePair(1)
                    Case "%active%"
                        RetActive = FormatConverter.GetBooleanFromNumeric(PropertyValuePair(1))
                    Case Else
                        Throw New Exception("Unknown property: " & PropertyValuePair(0))
                End Select
            Next
            Return New ScheduleInformation(RetName, RetActive)
        End Function
        Public Sub New(ByVal Name As String, Active As Boolean)
            MyBase.New(Name, Active)
        End Sub
        Public Overrides Function CopyUnknown() As ChildComponentInformation
            Return Copy()
        End Function
        Public Sub New()
            MyBase.New("Empty", False)
        End Sub
        Public Overrides Function OutputLine() As String
            Return "$schedule: %name%==" & Name & " && %active%==" & FormatConverter.GetNumericBoolean(Active).ToString
        End Function
        Public Shadows Function Copy() As ScheduleInformation
            Dim Ret As New ScheduleInformation(Name, Active)
            Ret.MatchingEntryExists = MatchingEntryExists
            Return Ret
        End Function
    End Class
    Public Class RoutineInformation
        Inherits ChildComponentInformation
        Private varRunEvery As Integer
        Private varInterval As RoutineDateInterval
        Private varLastRun As String
        Public Property RunEvery As Integer
            Get
                Return varRunEvery
            End Get
            Set(value As Integer)
                varRunEvery = value
            End Set
        End Property
        Public Property LastRun As String
            Get
                Return varLastRun
            End Get
            Set(value As String)
                varLastRun = value
            End Set
        End Property
        Public Overrides Function CopyUnknown() As ChildComponentInformation
            Return Copy()
        End Function
        Public Property Interval As RoutineDateInterval
            Get
                Return varInterval
            End Get
            Set(value As RoutineDateInterval)
                varInterval = value
            End Set
        End Property
        Public Sub New(ByVal RoutineName As String, RunEvery As Integer, Interval As RoutineDateInterval, Active As Boolean, LastRun As String)
            MyBase.New(RoutineName, Active)
            varRunEvery = RunEvery
            varInterval = Interval
            varLastRun = LastRun
        End Sub
        Public Sub New()
            MyBase.New("Empty", False)
            varRunEvery = 1
            varInterval = RoutineDateInterval.Weeks
            varLastRun = "Never"
        End Sub
        Public Shadows Function Copy() As RoutineInformation
            Dim Ret As New RoutineInformation(Name, RunEvery, Interval, Active, LastRun)
            Ret.MatchingEntryExists = MatchingEntryExists
            Return Ret
        End Function
        Public Shared Shadows Function CreateFromLine(Line As String) As RoutineInformation
            Dim RetName As String = Nothing
            Dim RetRunEvery As Integer = 0
            Dim RetInterval As RoutineDateInterval = Nothing
            Dim RetActive As Boolean
            Dim RetLastRun As String = "Not loaded"
            Dim CutLine As String = Line.Remove(0, 10)
            Dim Parts() As String = CutLine.Split(New String() {" && "}, StringSplitOptions.RemoveEmptyEntries)
            For Each Part As String In Parts
                Dim PropertyValuePair() As String = Part.Split(New String() {"=="}, StringSplitOptions.RemoveEmptyEntries)
                ' Finn ut hvilken egenskap som skal settes
                Select Case PropertyValuePair(0)
                    Case "%name%" ' Description
                        RetName = PropertyValuePair(1)
                    Case "%runevery%"
                        RetRunEvery = Integer.Parse(PropertyValuePair(1))
                    Case "%timespan%"
                        RetInterval = FormatConverter.GetRoutineIntervalFromString(PropertyValuePair(1))
                    Case "%active%"
                        RetActive = FormatConverter.GetBooleanFromNumeric(PropertyValuePair(1))
                    Case "%lastrun%"
                        RetLastRun = PropertyValuePair(1)
                    Case Else
                        Throw New Exception("Unknown property: " & PropertyValuePair(0))
                End Select
            Next
            Return New RoutineInformation(RetName, RetRunEvery, RetInterval, RetActive, RetLastRun)
        End Function
        Public Overrides Function OutputLine() As String
            Return "$routine: %name%==" & Name & " && %runevery%==" & varRunEvery.ToString & " && %timespan%==" & varInterval.ToString() & " && %active%==" & FormatConverter.GetNumericBoolean(Active).ToString & " && %lastrun%==" & varLastRun
        End Function
        Public Enum RoutineDateInterval As Byte
            Minutes
            Hours
            Days
            Weeks
            Months
        End Enum
    End Class
    Public Class TaskInformation
        Inherits ChildComponentInformation
        Private varWaitForPrevious, varRunAsync As Boolean
        Private varArguments As New List(Of Object)
        Public Sub New(ByVal TaskName As String, Active As Boolean, WaitForPrevious As Boolean, RunAsync As Boolean, Optional Arguments As IEnumerable(Of Object) = Nothing)
            MyBase.New(TaskName, Active)
            varWaitForPrevious = WaitForPrevious
            varRunAsync = RunAsync
            If Arguments IsNot Nothing Then
                varArguments.AddRange(Arguments.ToArray)
            End If
        End Sub
        Public Overrides Function CopyUnknown() As ChildComponentInformation
            Return Copy()
        End Function
        Public Sub New()
            MyBase.New("Empty", False)
        End Sub
        Public Property WaitForPrevious As Boolean
            Get
                Return varWaitForPrevious
            End Get
            Set(value As Boolean)
                varWaitForPrevious = value
            End Set
        End Property
        Public Property RunAsync As Boolean
            Get
                Return varRunAsync
            End Get
            Set(value As Boolean)
                varRunAsync = value
            End Set
        End Property
        Public ReadOnly Property Arguments As List(Of Object)
            Get
                Return varArguments
            End Get
        End Property
        Public Shadows Function Copy() As TaskInformation
            Dim Ret As New TaskInformation(Name, Active, WaitForPrevious, RunAsync, Arguments)
            Ret.MatchingEntryExists = MatchingEntryExists
            Return Ret
        End Function
        Public Shared Shadows Function CreateFromLine(Line As String) As TaskInformation
            Dim RetName As String = Nothing
            Dim RetActive As Boolean, RetWaitForPrevious, RetRunAsync As Boolean
            Dim RetArgs As New List(Of String)
            Dim CutLine As String = Line.Remove(0, 7)
            Dim Parts() As String = CutLine.Split(New String() {" && "}, StringSplitOptions.RemoveEmptyEntries)
            For Each Part As String In Parts
                Dim PropertyValuePair() As String = Part.Split(New String() {"=="}, StringSplitOptions.RemoveEmptyEntries)
                ' Finn ut hvilken egenskap som skal settes
                Select Case PropertyValuePair(0)
                    Case "%name%" ' Description
                        RetName = PropertyValuePair(1)
                    Case "%active%"
                        RetActive = FormatConverter.GetBooleanFromNumeric(PropertyValuePair(1))
                    Case "%waitforprevious%"
                        RetWaitForPrevious = FormatConverter.GetBooleanFromNumeric(PropertyValuePair(1))
                    Case "%runasync%"
                        RetRunAsync = FormatConverter.GetBooleanFromNumeric(PropertyValuePair(1))
                    Case "%arg%"
                        RetArgs.Add(PropertyValuePair(1))
                    Case Else
                        Throw New Exception("Unknown property: " & PropertyValuePair(0))
                End Select
            Next
            Return New TaskInformation(RetName, RetActive, RetWaitForPrevious, RetRunAsync, RetArgs)
        End Function
        Public Overrides Function OutputLine() As String
            Dim Ret As String = "$task: %name%==" & Name & " && %waitforprevious%==" & FormatConverter.GetNumericBoolean(varWaitForPrevious) & " && %active%==" & FormatConverter.GetNumericBoolean(Active).ToString & " && %runasync%==" & FormatConverter.GetNumericBoolean(RunAsync).ToString
            For Each Arg As Object In Arguments
                Ret &= " && %arg%==" & Arg.ToString
            Next
            Return Ret
        End Function
    End Class
    Public Class AsyncFileReader
        Private Shared WithEvents TS As Threader
        Private Shared varIsRunning As Boolean = False
        Public Shared ReadOnly Property IsRunning As Boolean
            Get
                Return varIsRunning
            End Get
        End Property
        Public Shared Sub Start()
            If Not varIsRunning Then
                If Queue.NextInQueue IsNot Nothing Then
                    varIsRunning = True
                    If TS IsNot Nothing AndAlso Not TS.Disposed Then
                        Throw New Exception("The ThreadStarterLight in AsyncFileReader was not disposed. Investigate the call stack.")
                    End If
                    TS = New Threader(AddressOf ExecuteOperation)
                    Dim Arguments() As Object = {Queue.NextInQueue.Operation, Queue.NextInQueue.State}
                    TS.Start(Arguments)
                End If
            End If
        End Sub
        Private Shared Function ExecuteOperation(Arguments As Object) As Object
            Dim ArgArr() As Object = DirectCast(Arguments, Object())
            Dim varOperation As Queue.FileOperation = DirectCast(ArgArr(0), Queue.FileOperation)
            Dim State As Object = ArgArr(1)
            Dim Pairs As Dictionary(Of String, Object) = varOperation.ParameterValuePairs
            Select Case DirectCast(Pairs("OperationName"), String)
                Case "ListDirectories"
                    Return ListDirectories(Pairs, State)
                Case "MoveFile"
                    Return MoveFile(Pairs, State)
                Case "ReadLines"
                    Return ReadLines(Pairs, State)
                Case "ListFiles"
                    Return ListFiles(Pairs, State)
                Case "OverwriteFile"
                    Return OverwriteFile(Pairs, State)
                Case "CreateFile"
                    Return CreateFile(Pairs, State)
                Case "UpdateScheduleInformationInTemplates"
                    Return UpdateScheduleInformationInTemplates(Pairs, State)
                Case "UpdateRoutineInformationInSchedules"
                    Return UpdateRoutineInformationInSchedules(Pairs, State)
                Case "UpdateTaskInformationInRoutines"
                    Return UpdateTaskInformationInRoutines(Pairs, State)
                Case "DeleteFile"
                    Return DeleteFile(Pairs, State)
                Case "DeleteFolder"
                    Return DeleteFolder(Pairs, State)
                Case "CreateDirectory"
                    Return CreateDirectory(Pairs, State)
                Case "RenameFolder"
                    Return RenameFolder(Pairs, State)
                Case "CustomOperation"
                    Dim FunctionToRun As Func(Of Dictionary(Of String, Object), Object, FileOperationEventArgs) = DirectCast(Pairs("FunctionToRun"), Func(Of Dictionary(Of String, Object), Object, FileOperationEventArgs))
                    Return FunctionToRun.Invoke(Pairs, State)
                Case Else
                    Throw New Exception("The operation '" & DirectCast(Pairs("OperationName"), String) & "' does not exist.")
            End Select
        End Function
        Private Shared Sub OperationFinished(Sender As Object, e As ThreadStarterEventArgs) Handles TS.WorkCompleted
            Dim fe As FileOperationEventArgs = DirectCast(e.Result, FileOperationEventArgs)
            TS.Dispose()
            With Queue.NextInQueue.WhenFinished
                Queue.RemoveFirst()
                .Invoke(fe)
            End With
            varIsRunning = False
            Start()
        End Sub
        Private Shared Function CreateDirectory(ByVal Pairs As Dictionary(Of String, Object), ByRef State As Object) As FileOperationEventArgs
            Dim PreferredFullPath As String = DirectCast(Pairs("Path"), String)
            Dim AutoRenameIfExists As Boolean = DirectCast(Pairs("AutoRenameIfExists"), Boolean)
            Dim NewFullPath As String = PreferredFullPath
            Try
                If AutoRenameIfExists Then
                    NewFullPath = GetFirstAvailableFolderName(PreferredFullPath)
                ElseIf Directory.Exists(PreferredFullPath) Then
                    Throw New Exception("The directory already exists.")
                End If
                Directory.CreateDirectory(NewFullPath)
                Return New FileOperationEventArgs(NewFullPath, False, Pairs, State)
            Catch ex As Exception
                Return New FileOperationEventArgs(Nothing, True, Pairs, State, ex)
            End Try
        End Function
        Private Shared Function DeleteFolder(ByVal Pairs As Dictionary(Of String, Object), ByRef State As Object) As FileOperationEventArgs
            Dim Path As String = DirectCast(Pairs("Path"), String)
            Try
                Directory.Delete(Path)
                Return New FileOperationEventArgs(Nothing, False, Pairs, State, Nothing)
            Catch ex As Exception
                Return New FileOperationEventArgs(Nothing, True, Pairs, State, ex)
            End Try
        End Function
        Private Shared Function DeleteFile(ByVal Pairs As Dictionary(Of String, Object), ByRef State As Object) As FileOperationEventArgs
            Dim Path As String = DirectCast(Pairs("Path"), String)
            Try
                My.Computer.FileSystem.DeleteFile(Path, FileIO.UIOption.OnlyErrorDialogs, FileIO.RecycleOption.SendToRecycleBin, FileIO.UICancelOption.ThrowException)
                Return New FileOperationEventArgs(Nothing, False, Pairs, State)
            Catch ex As Exception
                Return New FileOperationEventArgs(Nothing, True, Pairs, State, ex)
            End Try
        End Function
        Private Shared Function RenameFolder(ByVal Pairs As Dictionary(Of String, Object), ByRef State As Object) As FileOperationEventArgs
            Dim FolderToModify As String = DirectCast(Pairs("Path"), String)
            Dim PreferredName As String = DirectCast(Pairs("PreferredName"), String)
            Dim AutoRenameIfExists As Boolean = DirectCast(Pairs("AutoRenameIfExists"), Boolean)
            Dim ParentDirectory As String = Directory.GetParent(FolderToModify).FullName
            Dim IdealFullPath As String = ParentDirectory & "\" & PreferredName
            Dim NewName As String = PreferredName
            Try
                If AutoRenameIfExists Then
                    NewName = GetFirstAvailableFolderName(IdealFullPath, False, False)
                ElseIf Directory.Exists(IdealFullPath) Then
                    Throw New Exception("The directory already exists.")
                End If
                'If Directory.Exists(IdealFullPath) Then
                'If Not AutoRenameIfExists Then
                '    Throw New Exception("The directory already exists.")
                'Else
                '    Dim Result() As String = Directory.GetDirectories(ParentDirectory, PreferredName & " (*)")
                '    Dim HighestNumber As Integer = -1
                '    Dim iLast As Integer = Result.Count - 1
                '    If iLast >= 0 Then
                '        For i As Integer = 0 To iLast
                '            Dim CurrentDirectory As String = Result(i)
                '            Dim CurrentNumber As Integer = CInt(CurrentDirectory.Split("(".ToCharArray, StringSplitOptions.RemoveEmptyEntries).Last.Split(")".ToCharArray, StringSplitOptions.RemoveEmptyEntries).First)
                '            If CurrentNumber > HighestNumber Then
                '                HighestNumber = CurrentNumber
                '            End If
                '        Next
                '    Else
                '        MsgBox("An unexpected error occurred (code 1)")
                '    End If
                '    NewName = PreferredName & " (" & HighestNumber + 1 & ")"
                'End If
                'Else
                '    NewName = PreferredName
                'End If
                My.Computer.FileSystem.RenameDirectory(FolderToModify, NewName)
                Return New FileOperationEventArgs(NewName, False, Pairs, State)
            Catch ex As Exception
                Return New FileOperationEventArgs(Nothing, True, Pairs, State, ex)
            End Try
        End Function

        Private Shared Function UpdateScheduleInformationInTemplates(ByVal Pairs As Dictionary(Of String, Object), ByRef State As Object) As FileOperationEventArgs
            Dim OldName As String = DirectCast(Pairs("OldScheduleName"), String)
            Dim NewName As String = DirectCast(Pairs("NewScheduleName"), String)
            Dim Match As List(Of TemplateEntry) = Loader.AllTemplates.FindAll(Function(x As TemplateEntry) As Boolean
                                                                                  Dim ScheduleMatch As List(Of ScheduleInformation) = x.Schedules.FindAll(Function(info As ScheduleInformation) As Boolean
                                                                                                                                                              If info.Name = NewName Then info.MatchingEntryExists = True
                                                                                                                                                              If info.Name = OldName Then
                                                                                                                                                                  info.Name = NewName
                                                                                                                                                                  info.MatchingEntryExists = True
                                                                                                                                                                  Return True
                                                                                                                                                              End If
                                                                                                                                                              Return False
                                                                                                                                                          End Function)
                                                                                  If ScheduleMatch.Count > 0 Then Return True
                                                                                  Return False
                                                                              End Function)
            Dim ModifiedTemplates As New List(Of String)
            Try
                For Each OutdatedTemplate As TemplateEntry In Match
                    Dim OverwriteArguments As New Dictionary(Of String, Object) From {{"Path", OutdatedTemplate.GetFullFilePath}, {"NewContents", OutdatedTemplate.OutputFileContents}}
                    Dim ThisTemplateResult As FileOperationEventArgs = OverwriteFile(OverwriteArguments, Nothing)
                    If ThisTemplateResult.ErrorOccurred Then
                        Throw ThisTemplateResult.Exception
                    Else
                        ModifiedTemplates.Add(OutdatedTemplate.Name)
                    End If
                Next
            Catch ex As Exception
                Return New FileOperationEventArgs(ModifiedTemplates.ToArray, True, Pairs, State, ex)
            End Try
            Return New FileOperationEventArgs(ModifiedTemplates.ToArray, False, Pairs, State, Nothing)
        End Function
        Private Shared Function UpdateRoutineInformationInSchedules(ByVal Pairs As Dictionary(Of String, Object), ByRef State As Object) As FileOperationEventArgs
            Dim OldName As String = DirectCast(Pairs("OldRoutineName"), String)
            Dim NewName As String = DirectCast(Pairs("NewRoutineName"), String)
            Dim Match As List(Of ScheduleEntry) = Loader.AllSchedules.FindAll(Function(x As ScheduleEntry) As Boolean
                                                                                  Dim RoutineMatch As List(Of RoutineInformation) = x.Routines.FindAll(Function(info As RoutineInformation) As Boolean
                                                                                                                                                           If info.Name = NewName Then info.MatchingEntryExists = True
                                                                                                                                                           If info.Name = OldName Then
                                                                                                                                                               info.Name = NewName
                                                                                                                                                               Return True
                                                                                                                                                           End If
                                                                                                                                                           Return False
                                                                                                                                                       End Function)
                                                                                  If RoutineMatch.Count > 0 Then Return True
                                                                                  Return False
                                                                              End Function)
            Dim ModifiedSchedules As New List(Of String)
            Try
                For Each OutdatedSchedule As ScheduleEntry In Match
                    Dim OverwriteArguments As New Dictionary(Of String, Object) From {{"Path", OutdatedSchedule.GetFullFilePath}, {"NewContents", OutdatedSchedule.OutputFileContents}}
                    Dim ThisScheduleResult As FileOperationEventArgs = OverwriteFile(OverwriteArguments, Nothing)
                    If ThisScheduleResult.ErrorOccurred Then
                        Throw ThisScheduleResult.Exception
                    Else
                        ModifiedSchedules.Add(OutdatedSchedule.Name)
                    End If
                Next
            Catch ex As Exception
                Return New FileOperationEventArgs(ModifiedSchedules.ToArray, True, Pairs, State, ex)
            End Try
            Return New FileOperationEventArgs(ModifiedSchedules.ToArray, False, Pairs, State, Nothing)
        End Function
        Private Shared Function UpdateTaskInformationInRoutines(ByVal Pairs As Dictionary(Of String, Object), ByRef State As Object) As FileOperationEventArgs
            Dim OldName As String = DirectCast(Pairs("OldTaskName"), String)
            Dim NewName As String = DirectCast(Pairs("NewTaskName"), String)
            Dim Match As List(Of RoutineEntry) = Loader.AllRoutines.FindAll(Function(x As RoutineEntry) As Boolean
                                                                                Dim TaskMatch As List(Of TaskInformation) = x.Tasks.FindAll(Function(info As TaskInformation) As Boolean
                                                                                                                                                If info.Name = OldName Then
                                                                                                                                                    info.Name = NewName
                                                                                                                                                    info.MatchingEntryExists = True
                                                                                                                                                    Return True
                                                                                                                                                End If
                                                                                                                                                Return False
                                                                                                                                            End Function)
                                                                                If TaskMatch.Count > 0 Then Return True
                                                                                Return False
                                                                            End Function)
            Dim ModifiedRoutines As New List(Of String)
            Try
                For Each OutdatedRoutine As RoutineEntry In Match
                    Dim OverwriteArguments As New Dictionary(Of String, Object) From {{"Path", OutdatedRoutine.GetFullFilePath}, {"NewContents", OutdatedRoutine.OutputFileContents}}
                    Dim ThisRoutineResult As FileOperationEventArgs = OverwriteFile(OverwriteArguments, Nothing)
                    If ThisRoutineResult.ErrorOccurred Then
                        Throw ThisRoutineResult.Exception
                    Else
                        ModifiedRoutines.Add(OutdatedRoutine.Name)
                    End If
                Next
            Catch ex As Exception
                Return New FileOperationEventArgs(ModifiedRoutines.ToArray, True, Pairs, State, ex)
            End Try
            Return New FileOperationEventArgs(ModifiedRoutines.ToArray, False, Pairs, State, Nothing)
        End Function
        Private Shared Function CreateFile(ByVal Pairs As Dictionary(Of String, Object), ByRef State As Object) As FileOperationEventArgs
            Dim DirectoryWithoutBackslash As String = DirectCast(Pairs("DirectoryWithoutBackslash"), String)
            Dim PreferredNameWithExtension As String = DirectCast(Pairs("PreferredNameWithExtension"), String)
            Dim AutoRenameIfExists As Boolean = DirectCast(Pairs("AutoRenameIfExists"), Boolean)
            Dim SearchBase As String = DirectCast(Pairs("SearchBase"), String)
            If SearchBase = "ParentDirectory" Then SearchBase = DirectoryWithoutBackslash
            Dim WriteContent() As Byte = DirectCast(Pairs("WriteContent"), Byte())
            Dim IdealFullPath As String = DirectoryWithoutBackslash & "\" & PreferredNameWithExtension
            Dim OutputFilePath As String = Nothing
            Try
                'If Not AutoRenameIfExists Then
                '    Dim Matches() As String = My.Computer.FileSystem
                '    Throw New Exception("The file already exists. Use the OverwriteFile operation if this is intentional.")
                'Else
                OutputFilePath = GetFirstAvailableFileName(IdealFullPath, SearchBase)
                If Not AutoRenameIfExists AndAlso OutputFilePath <> IdealFullPath Then
                    Throw New Exception("A file with this name already exists in the specified search base (" & SearchBase & ")")
                Else
                    Using fs As FileStream = File.Create(OutputFilePath)
                        If WriteContent IsNot Nothing Then fs.Write(WriteContent, 0, WriteContent.Length)
                    End Using
                    Return New FileOperationEventArgs(OutputFilePath, False, Pairs, State)
                End If
                'OutputFilePath = IdealFullPath
                'End If
            Catch ex As Exception
                Return New FileOperationEventArgs(Nothing, True, Pairs, State, ex)
            End Try
        End Function
        Private Shared Function MoveFile(ByVal Pairs As Dictionary(Of String, Object), ByRef State As Object) As FileOperationEventArgs
            Dim OriginalPath As String = DirectCast(Pairs("OriginalPath"), String)
            Dim NewPath As String = DirectCast(Pairs("NewPath"), String)
            Dim OverwriteConflict As Boolean = DirectCast(Pairs("OverwriteConflict"), Boolean)
            If OverwriteConflict AndAlso File.Exists(NewPath) Then File.Delete(NewPath)
            Try
                Directory.Move(OriginalPath, NewPath)
            Catch ex As Exception
                Return New FileOperationEventArgs("Could not move files from " & OriginalPath & " to " & NewPath & " (see exception).", True, Pairs, State, ex)
            End Try
            Return New FileOperationEventArgs("Successfully moved file from " & OriginalPath & " to " & NewPath, False, Pairs, State)
        End Function
        Private Shared Function ListDirectories(ByVal Pairs As Dictionary(Of String, Object), ByRef State As Object) As FileOperationEventArgs
            Dim Path As String = DirectCast(Pairs("Path"), String)
            Dim Result() As String = Nothing
            Try
                Result = Directory.GetDirectories(Path)
            Catch Ex As Exception
                Return New FileOperationEventArgs(Result, True, Pairs, State, Ex)
            End Try
            Return New FileOperationEventArgs(Result, False, Pairs, State)
        End Function
        Private Shared Function ListFiles(ByVal Pairs As Dictionary(Of String, Object), ByRef State As Object) As FileOperationEventArgs
            Dim Path As String = DirectCast(Pairs("Path"), String)
            Dim Result() As String = Nothing
            Try
                Result = Directory.GetFiles(Path)
            Catch Ex As Exception
                Return New FileOperationEventArgs(Result, True, Pairs, State, Ex)
            End Try
            Return New FileOperationEventArgs(Result, False, Pairs, State)
        End Function
        Private Shared Function OverwriteFile(ByVal Pairs As Dictionary(Of String, Object), ByRef State As Object) As FileOperationEventArgs
            Dim Path As String = DirectCast(Pairs("Path"), String)
            Dim NewContents() As String = DirectCast(Pairs("NewContents"), String())
            Try
                Using Writer As New IO.StreamWriter(Path, False, Text.Encoding.Default, 4096)
                    For Each Line As String In NewContents
                        Writer.WriteLine(Line)
                    Next
                End Using
            Catch Ex As Exception
                Return New FileOperationEventArgs(Nothing, True, Pairs, State, Ex)
            End Try
            Return New FileOperationEventArgs(Nothing, False, Pairs, State)
        End Function
        Private Shared Function ReadLines(ByVal Pairs As Dictionary(Of String, Object), ByRef State As Object) As FileOperationEventArgs
            Dim Path As String = DirectCast(Pairs("Path"), String)
            Dim Encoding As Text.Encoding = DirectCast(Pairs("Encoding"), Text.Encoding)
            Dim SR As StreamReader = Nothing
            Dim Lines As New List(Of String)
            Try
                SR = New StreamReader(Path, Encoding)
                Do While SR.Peek >= 0
                    Lines.Add(SR.ReadLine)
                Loop
            Catch Ex As Exception
                Return New FileOperationEventArgs(Lines.ToArray, True, Pairs, State, Ex)
            Finally
                If SR IsNot Nothing Then
                    SR.Close()
                End If
            End Try
            Return New FileOperationEventArgs(Lines.ToArray, False, Pairs, State)
        End Function
        ''' <summary>
        ''' Searches the specified path (or search base and all subdirectories if search base is specified) for file name conflicts, and returns a modified full path.
        ''' </summary>
        ''' <param name="PreferredFullPath"></param>
        ''' <param name="SearchBase">A directory that, if not nothing, will be searched (along with all subdirectories) for conflicting names. If nothing, the directory of the preferred full path will be searched.
        ''' This is useful if no two files can have the same name, even if they are in separate directories.</param>
        ''' <returns>The best alternative to the preferred full path, for example: C:/example/myfile.txt -> C:/example/myfile (1).txt</returns>
        Private Shared Function GetFirstAvailableFileName(ByVal PreferredFullPath As String, SearchBase As String) As String
            Dim Options As SearchOption = SearchOption.TopDirectoryOnly
            Dim FileExists As Boolean = True
            Dim FileNameOnly As String = Path.GetFileNameWithoutExtension(PreferredFullPath)
            Dim Extension As String = Path.GetExtension(PreferredFullPath)
            Dim ParentDirectory As String = Directory.GetParent(PreferredFullPath).FullName
            If SearchBase = "ParentDirectory" OrElse SearchBase = ParentDirectory Then
                FileExists = File.Exists(PreferredFullPath)
                SearchBase = ParentDirectory
            Else
                Options = SearchOption.AllDirectories
            End If
            If FileExists Then
                Dim Result() As String = Directory.GetFiles(SearchBase, FileNameOnly & " (*)" & Extension, Options)
                Dim HighestNumber As Integer = 0
                Dim iLast As Integer = Result.Count - 1
                If iLast >= 0 Then
                    For i As Integer = 0 To iLast
                        Dim CurrentFile As String = Path.GetFileNameWithoutExtension(Result(i))
                        Dim CurrentNumber As Integer = CInt(CurrentFile.Split("(".ToCharArray).Last.Split(")".ToCharArray).First)
                        If CurrentNumber > HighestNumber Then HighestNumber = CurrentNumber
                    Next
                Else
                    HighestNumber = 0
                End If
                Return ParentDirectory & "\" & FileNameOnly & " (" & HighestNumber + 1 & ")" & Extension
            Else
                Return PreferredFullPath
            End If
        End Function
        Private Shared Function GetFirstAvailableFolderName(ByVal PreferredFullPath As String, Optional ByVal SkipExistsCheck As Boolean = False, Optional ByVal ReturnFullPath As Boolean = True, Optional ByVal Options As SearchOption = SearchOption.TopDirectoryOnly) As String
            PreferredFullPath.TrimEnd("\".ToCharArray)
            Dim DirectoryExists As Boolean = True
            If Not SkipExistsCheck Then DirectoryExists = Directory.Exists(PreferredFullPath)
            Dim Info As New DirectoryInfo(PreferredFullPath)
            Dim PreferredName As String = Info.Name
            Dim ParentDirectory As String = Directory.GetParent(PreferredFullPath).FullName
            If DirectoryExists Then
                Dim Result() As String = Directory.GetDirectories(ParentDirectory, PreferredName & " (*)", Options)
                Dim HighestNumber As Integer = 0
                Dim iLast As Integer = Result.Count - 1
                If iLast >= 0 Then
                    For i As Integer = 0 To iLast
                        Dim CurrentDirectory As String = Result(i)
                        Dim CurrentNumber As Integer = CInt(CurrentDirectory.Split("(".ToCharArray).Last.Split(")".ToCharArray, StringSplitOptions.RemoveEmptyEntries).First)
                        If CurrentNumber > HighestNumber Then
                            HighestNumber = CurrentNumber
                        End If
                    Next
                End If
                Dim NewName As String = PreferredName & " (" & HighestNumber + 1 & ")"
                If ReturnFullPath Then Return ParentDirectory & "\" & NewName
                Return NewName
            Else
                If ReturnFullPath Then Return PreferredFullPath
                Return PreferredName
            End If
        End Function

        Public Class Queue
            Private Shared varOperations As New List(Of OperationActionPair)
            Public Shared Sub RemoveFirst()
                varOperations(0) = Nothing
                varOperations.RemoveAt(0)
            End Sub
            Protected Friend Shared ReadOnly Property NextInQueue As OperationActionPair
                Get
                    If varOperations.Count > 0 Then
                        Return varOperations.First
                    Else
                        Return Nothing
                    End If
                End Get
            End Property
            Public Shared Sub AddOperation(Operation As FileOperation, WhenFinished As Action(Of FileOperationEventArgs), ByRef State As Object)
                varOperations.Add(New OperationActionPair(Operation, WhenFinished, State))
                Start()
            End Sub
            Protected Friend Class OperationActionPair
                Private varOperation As FileOperation
                Private varWhenFinished As Action(Of FileOperationEventArgs)
                Private varState As Object
                Public ReadOnly Property State As Object
                    Get
                        Return varState
                    End Get
                End Property
                Public ReadOnly Property Operation As FileOperation
                    Get
                        Return varOperation
                    End Get
                End Property
                Public ReadOnly Property WhenFinished As Action(Of FileOperationEventArgs)
                    Get
                        Return varWhenFinished
                    End Get
                End Property
                Public Sub New(Operation As FileOperation, WhenFinished As Action(Of FileOperationEventArgs), ByRef State As Object)
                    varState = State
                    varOperation = Operation
                    varWhenFinished = WhenFinished
                End Sub
            End Class
            Public Class FileOperation
                Private varParameterValuePairs As New Dictionary(Of String, Object)
                Protected Friend ReadOnly Property ParameterValuePairs As Dictionary(Of String, Object)
                    Get
                        Return varParameterValuePairs
                    End Get
                End Property
                Protected Friend Sub New(ByVal OperationName As String)
                    varParameterValuePairs.Add("OperationName", OperationName)
                End Sub
                Public Shared Function CustomOperation(ByVal Arguments As Object, ByVal FunctionToRun As Func(Of Dictionary(Of String, Object), Object, FileOperationEventArgs)) As FileOperation
                    Dim Ret As New FileOperation("CustomOperation")
                    With Ret.ParameterValuePairs
                        .Add("ArgumentsObject", Arguments)
                        .Add("FunctionToRun", FunctionToRun)
                    End With
                    Return Ret
                End Function
                Public Shared Function DeleteFile(ByVal Path As String) As FileOperation
                    Dim Ret As New FileOperation("DeleteFile")
                    With Ret.ParameterValuePairs
                        .Add("Path", Path)
                    End With
                    Return Ret
                End Function
                Public Shared Function UpdateScheduleInformationInTemplates(ByVal OldScheduleName As String, ByVal NewScheduleName As String) As FileOperation
                    Dim Ret As New FileOperation("UpdateScheduleInformationInTemplates")
                    With Ret.ParameterValuePairs
                        .Add("OldScheduleName", OldScheduleName)
                        .Add("NewScheduleName", NewScheduleName)
                    End With
                    Return Ret
                End Function
                Public Shared Function UpdateRoutineInformationInSchedules(ByVal OldRoutineName As String, ByVal NewRoutineName As String) As FileOperation
                    Dim Ret As New FileOperation("UpdateRoutineInformationInSchedules")
                    With Ret.ParameterValuePairs
                        .Add("OldRoutineName", OldRoutineName)
                        .Add("NewRoutineName", NewRoutineName)
                    End With
                    Return Ret
                End Function
                Public Shared Function UpdateTaskInformationInRoutines(ByVal OldTaskName As String, ByVal NewTaskName As String) As FileOperation
                    Dim Ret As New FileOperation("UpdateTaskInformationInRoutines")
                    With Ret.ParameterValuePairs
                        .Add("OldTaskName", OldTaskName)
                        .Add("NewTaskName", NewTaskName)
                    End With
                    Return Ret
                End Function
                ' TODO: Make SearchBase optional
                Public Shared Function CreateFile(ByVal DirectoryWithoutBackslash As String, ByVal PreferredNameWithExtension As String, ByVal AutoRenameIfExists As Boolean, ByVal WriteContent As Byte(), Optional ByVal SearchBase As String = "ParentDirectory") As FileOperation
                    Dim Ret As New FileOperation("CreateFile")
                    With Ret.ParameterValuePairs
                        .Add("DirectoryWithoutBackslash", DirectoryWithoutBackslash)
                        .Add("PreferredNameWithExtension", PreferredNameWithExtension)
                        .Add("AutoRenameIfExists", AutoRenameIfExists)
                        .Add("WriteContent", WriteContent)
                        .Add("SearchBase", SearchBase)
                    End With
                    Return Ret
                End Function
                Public Shared Function CreateDirectory(ByVal Path As String, ByVal AutoRenameIfExists As Boolean) As FileOperation
                    Dim Ret As New FileOperation("CreateDirectory")
                    With Ret.ParameterValuePairs
                        .Add("Path", Path)
                        .Add("AutoRenameIfExists", AutoRenameIfExists)
                    End With
                    Return Ret
                End Function
                Public Shared Function DeleteFolder(ByVal Path As String) As FileOperation
                    Dim Ret As New FileOperation("DeleteFolder")
                    With Ret.ParameterValuePairs
                        .Add("Path", Path)
                    End With
                    Return Ret
                End Function
                Public Shared Function RenameFolder(ByVal Path As String, ByVal PreferredName As String, ByVal AutoRenameIfExists As Boolean) As FileOperation
                    Dim Ret As New FileOperation("RenameFolder")
                    With Ret.ParameterValuePairs
                        .Add("Path", Path)
                        .Add("PreferredName", PreferredName)
                        .Add("AutoRenameIfExists", AutoRenameIfExists)
                    End With
                    Return Ret
                End Function
                Public Shared Function OverwriteFile(ByVal Path As String, ByVal NewContents() As String) As FileOperation
                    Dim Ret As New FileOperation("OverwriteFile")
                    With Ret.ParameterValuePairs
                        .Add("Path", Path)
                        .Add("NewContents", NewContents)
                    End With
                    Return Ret
                End Function
                Public Shared Function ListDirectories(ByVal Path As String) As FileOperation
                    Dim Ret As New FileOperation("ListDirectories")
                    With Ret.ParameterValuePairs
                        .Add("Path", Path)
                    End With
                    Return Ret
                End Function
                Public Shared Function ListFiles(ByVal Path As String) As FileOperation
                    Dim Ret As New FileOperation("ListFiles")
                    With Ret.ParameterValuePairs
                        .Add("Path", Path)
                    End With
                    Return Ret
                End Function
                Public Shared Function MoveFile(ByVal OriginalPath As String, ByVal NewPath As String, ByVal OverwriteConflict As Boolean) As FileOperation
                    Dim Ret As New FileOperation("MoveFile")
                    With Ret.ParameterValuePairs
                        .Add("OriginalPath", OriginalPath)
                        .Add("NewPath", NewPath)
                        .Add("OverwriteConflict", OverwriteConflict)
                    End With
                    Return Ret
                End Function
                ''' <summary>
                ''' Represents an operation that reads all lines of the specified file, using the specified encoding.
                ''' </summary>
                ''' <param name="Path">The full path (including file name) of the file to be read.</param>
                ''' <param name="Encoding">Defaults to system default.</param>
                ''' <returns>A FileOperation that, when executed, produces a FileOperationEventArgs instance whose Result object is of the following type: String()</returns>
                Public Shared Function ReadLines(ByVal Path As String, Optional ByVal Encoding As Text.Encoding = Nothing) As FileOperation
                    If Encoding Is Nothing Then
                        Encoding = Text.Encoding.Default
                    End If
                    Dim Ret As New FileOperation("ReadLines")
                    With Ret.ParameterValuePairs
                        .Add("Path", Path)
                        .Add("Encoding", Encoding)
                    End With
                    Return Ret
                End Function
            End Class
        End Class
    End Class
    Public Class FileOperationEventArgs
        Inherits EventArgs
        Private varArguments As Dictionary(Of String, Object)
        Private varResult As Object
        Private varErrorOccurred As Boolean
        Private varException As Exception
        Private varState As Object
        Public ExceptionHandled As Boolean = False
        Public ReadOnly Property Arguments As Dictionary(Of String, Object)
            Get
                Return varArguments
            End Get
        End Property
        Public ReadOnly Property State As Object
            Get
                Return varState
            End Get
        End Property
        Public ReadOnly Property Result As Object
            Get
                Return varResult
            End Get
        End Property
        Public ReadOnly Property ErrorOccurred As Boolean
            Get
                Return varErrorOccurred
            End Get
        End Property
        Public ReadOnly Property Exception As Exception
            Get
                Return varException
            End Get
        End Property
        Public Sub New(Result As Object, ErrorOccurred As Boolean, Arguments As Dictionary(Of String, Object), ByRef State As Object, Optional Exception As Exception = Nothing)
            varArguments = Arguments
            varResult = Result
            varState = State
            varErrorOccurred = ErrorOccurred
            varException = Exception
        End Sub
    End Class
#End Region
End Module

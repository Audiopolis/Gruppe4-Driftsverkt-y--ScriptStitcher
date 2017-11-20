Imports System.Management.Automation
Imports System.Threading
Imports System.Linq
''' <summary>
''' Executes the tasks within routines in an orderly fashion.
''' </summary>
Public Class PSExecutionEngine
    Implements IDisposable
    Private RoutineQueue As New List(Of RoutineQueueInformation)
    Private WithEvents QueueTimer As New Timers.Timer(5000)
    Private SC As SynchronizationContext = SynchronizationContext.Current
    Private PreviousValue As Object = Nothing
    Private TaskExecutionQueue As New List(Of TaskExecutionQueueInformation)
    Private RunningTasks As New List(Of TaskExecutionInfo)
    Private varReady As Boolean = True
    Private varPauseRequested As Boolean
    Public Event Paused(Sender As Object, e As ExecutionPausedEventArgs)
    Public Sub New()
        QueueTimer.AutoReset = False
    End Sub
    Private Sub BuildQueue()
        varReady = False
        RoutineQueue.Clear()
        With ActiveTemplate.Schedules
            Dim iLast As Integer = .Count - 1
            If iLast >= 0 Then
                For i As Integer = 0 To iLast
                    Dim Schedule As ScheduleEntry = Loader.LookupSchedule(.Item(i).Name)
                    If Schedule IsNot Nothing Then
                        For Each Routine As RoutineInformation In Schedule.Routines
                            If Routine.MatchingEntryExists Then RoutineQueue.Add(New RoutineQueueInformation(Routine, Schedule))
                        Next
                    End If
                Next
            End If
        End With
        If RoutineQueue.Count > 0 Then
            RoutineQueue = RoutineQueue.OrderBy(Function(x) As Date
                                                    Return x.RunOn
                                                End Function).ToList
            Dim nLast As Integer = RoutineQueue.Count - 1
            For n As Integer = nLast To 0 Step -1
                If Date.Compare(RoutineQueue(n).RunOn, Date.Now) > 0 Then RoutineQueue.RemoveAt(n) Else Exit For
            Next
            If RoutineQueue.Count > 0 Then
                ExecuteFirstRoutine()
            Else
                varReady = True
                If Not varPauseRequested Then Start()
            End If
        Else
            varReady = True
            Start()
        End If
    End Sub
    Private Sub ExecuteFirstRoutine()
        If Not varPauseRequested Then
            If RoutineQueue.Count > 0 Then
                With RoutineQueue.First
                    .Information.LastRun = Date.Now.ToString
                    AsyncFileReader.OperationsQueue.AddOperation(AsyncFileReader.OperationsQueue.FileOperation.OverwriteFile(.InSchedule.GetFullFilePath, .InSchedule.OutputFileContents), AddressOf OverwriteFinished, Nothing)
                    With .Entry.Tasks
                        Dim iLast As Integer = .Count - 1
                        If iLast >= 0 Then
                            For i As Integer = 0 To iLast
                                Dim NewInfo As New TaskExecutionQueueInformation(.Item(i), RoutineQueue.First.Entry)
                                TaskExecutionQueue.Add(NewInfo)
                            Next
                        End If
                    End With
                End With
                RoutineQueue.RemoveAt(0)
                StartTaskQueueExecution()
            Else
                varReady = True
                Start()
            End If
        Else
            varReady = True
            RaiseEvent Paused(Me, New ExecutionPausedEventArgs(TaskExecutionQueue))
        End If
    End Sub
    Public Sub Start()
        varPauseRequested = False
        If varReady Then
            If RoutineQueue.Count = 0 Then
                QueueTimer.Start()
            Else
                ExecuteFirstRoutine()
            End If
        Else
            MsgBox("Already running")
        End If
    End Sub
    Public Sub RequestPause()
        varPauseRequested = True
        If varReady Then RaiseEvent Paused(Me, New ExecutionPausedEventArgs(TaskExecutionQueue))
    End Sub
    Private Sub StartTaskQueueExecution()
        Dim iLast As Integer = TaskExecutionQueue.Count - 1
        If iLast >= 0 Then
            Do
                Dim TaskToExecute As TaskExecutionQueueInformation = TaskExecutionQueue(0)
                If TaskToExecute.Information.WaitForPrevious AndAlso RunningTasks.Count > 0 Then
                    Exit Do
                Else
                    Dim NewTask As New TaskExecutionInfo(TaskToExecute.Information, AddressOf TaskExecution_Finished)
                    RunningTasks.Add(NewTask)
                    TaskExecutionQueue.RemoveAt(0)
                    NewTask.Execute(PreviousValue)
                End If
            Loop Until TaskExecutionQueue.Count = 0
        Else
            PreviousValue = Nothing
            ExecuteFirstRoutine()
        End If
    End Sub
    Private Sub TaskExecution_Finished(Sender As TaskExecutionInfo, ExecutionInfo As PSTaskCompletedEventArgs)
        RunningTasks.Remove(Sender)
        If ExecutionInfo.ErrorsOccurred Then
            For Each ErrorOccurrance As ErrorRecord In ExecutionInfo.Result.Errors
                MsgBox("Error: " & ErrorOccurrance.ErrorDetails.Message)
            Next
        Else
            If ExecutionInfo.Result.Result.Count = 1 Then
                PreviousValue = ExecutionInfo.Result.Result(0).BaseObject ' TODO: Test out .BaseObject if this doesn't work
            ElseIf ExecutionInfo.Result.Result.Count > 1 Then
                PreviousValue = ExecutionInfo.Result.Result.ToArray
            End If
        End If
        Sender.Dispose()
        If RunningTasks.Count = 0 Then
            StartTaskQueueExecution()
        End If
    End Sub
    Private Sub QueueTimer_Tick(Sender As Object, e As Timers.ElapsedEventArgs) Handles QueueTimer.Elapsed
        SC.Post(AddressOf QueueTimer_Posted, Nothing)
    End Sub
    Private Sub QueueTimer_Posted(State As Object)
        If Not varPauseRequested Then BuildQueue()
    End Sub
    Private Sub OverwriteFinished(e As FileOperationEventArgs)
        If e.ErrorOccurred Then
            MsgBox("Error: " & e.Exception.ToString)
        End If
    End Sub

#Region "IDisposable Support"
    Private disposedValue As Boolean
    Protected Overridable Sub Dispose(disposing As Boolean)
        If Not disposedValue Then
            If disposing Then
                QueueTimer.Dispose()
                For Each Task As TaskExecutionInfo In RunningTasks
                    Task.Dispose()
                Next
            End If
        End If
        disposedValue = True
    End Sub
    Public Sub Dispose() Implements IDisposable.Dispose
        Dispose(True)
    End Sub
#End Region
End Class
Public Class RoutineQueueInformation
    Private varInformation As RoutineInformation
    Private varEntry As RoutineEntry
    Private varInSchedule As ScheduleEntry
    Public Sub New(Information As RoutineInformation, InSchedule As ScheduleEntry)
        varInformation = Information
        varEntry = Loader.LookupRoutine(varInformation.Name)
        varInSchedule = InSchedule
    End Sub
    Public ReadOnly Property InSchedule As ScheduleEntry
        Get
            Return varInSchedule
        End Get
    End Property
    Public ReadOnly Property RunOn As Date
        Get
            Dim AddTime As TimeSpan
            Select Case varInformation.Interval
                Case RoutineInformation.RoutineDateInterval.Days
                    AddTime = New TimeSpan(varInformation.RunEvery, 0, 0, 0)
                Case RoutineInformation.RoutineDateInterval.Weeks
                    AddTime = New TimeSpan(varInformation.RunEvery * 7, 0, 0)
                Case RoutineInformation.RoutineDateInterval.Months
                    AddTime = New TimeSpan(varInformation.RunEvery * 31, 0, 0, 0)
                Case RoutineInformation.RoutineDateInterval.Hours
                    AddTime = New TimeSpan(0, varInformation.RunEvery, 0, 0)
                Case RoutineInformation.RoutineDateInterval.Minutes
                    AddTime = New TimeSpan(0, 0, varInformation.RunEvery, 0, 0)
            End Select
            Dim LastRun As Date
            If varInformation.LastRun = "Never" Then
                LastRun = Date.MinValue
            Else
                LastRun = Date.Parse(varInformation.LastRun)
            End If
            Return LastRun.Add(AddTime)
        End Get
    End Property
    Public ReadOnly Property Information As RoutineInformation
        Get
            Return varInformation
        End Get
    End Property
    Public ReadOnly Property Entry As RoutineEntry
        Get
            Return varEntry
        End Get
    End Property
End Class

Public Class TaskExecutionQueueInformation
    Private varInformation As TaskInformation
    Private varEntry As TaskEntry
    Private varInRoutine As RoutineEntry
    Public Sub New(Information As TaskInformation, InRoutine As RoutineEntry)
        varInformation = Information
        varEntry = Loader.LookupTask(varInformation.Name)
        varInRoutine = InRoutine
    End Sub
    Public ReadOnly Property InRoutine As RoutineEntry
        Get
            Return varInRoutine
        End Get
    End Property
    Public ReadOnly Property Information As TaskInformation
        Get
            Return varInformation
        End Get
    End Property
    Public ReadOnly Property Entry As TaskEntry
        Get
            Return varEntry
        End Get
    End Property
End Class
Public Class ExecutionPausedEventArgs
    Inherits EventArgs
    Public WaitingTasks As List(Of TaskExecutionQueueInformation)
    Public Sub New(WaitingTasks As List(Of TaskExecutionQueueInformation))
        Me.WaitingTasks = WaitingTasks
    End Sub
End Class
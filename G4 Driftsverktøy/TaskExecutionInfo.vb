
Imports System.Management.Automation
    Imports System.Collections.ObjectModel

Public Class TaskExecutionInfo
    Implements IDisposable
    Private WithEvents TaskRunner As Threader
    Private varTaskInformation As TaskInformation
    Private varTaskEntry As TaskEntry
    Private varWhenFinished As Action(Of TaskExecutionInfo, PSTaskCompletedEventArgs)
    Public Sub New(ByVal TaskInformation As TaskInformation, WhenFinished As Action(Of TaskExecutionInfo, PSTaskCompletedEventArgs))
        varTaskInformation = TaskInformation
        varTaskEntry = Loader.LookupTask(TaskInformation.Name)
        varWhenFinished = WhenFinished
        If varTaskInformation.RunAsync Then TaskRunner = New Threader(AddressOf RunTaskAsync)
    End Sub
    Protected Friend Property TaskInformation As TaskInformation
        Get
            Return varTaskInformation
        End Get
        Set(value As TaskInformation)
            varTaskInformation = value
        End Set
    End Property
    Public Property WhenFinished As Action(Of TaskExecutionInfo, PSTaskCompletedEventArgs)
        Get
            Return varWhenFinished
        End Get
        Set(value As Action(Of TaskExecutionInfo, PSTaskCompletedEventArgs))
            varWhenFinished = value
        End Set
    End Property
    Public Sub Execute(ByVal PreviousResult As Object)
        Dim NewArguments() As Object = varTaskInformation.Arguments.ToArray
        Dim iLast As Integer = NewArguments.Count - 1
        If iLast >= 0 Then
            For i As Integer = 0 To iLast
                If NewArguments(i).ToString = "$PREVIOUS" Then
                    NewArguments(i) = PreviousResult
                End If
            Next
        End If
        If varTaskInformation.RunAsync Then
            Dim BoxedArgs() As Object = {varTaskEntry.ScriptPath, NewArguments}
            TaskRunner.Start(BoxedArgs)
        Else
            Dim Result As ScriptResult = RunTask(NewArguments)
            WhenFinished.Invoke(Me, New PSTaskCompletedEventArgs(Result, varTaskInformation))
        End If
    End Sub
    Private Function RunTaskAsync(Arguments As Object) As Object
        Dim UnboxedArguments() As Object = DirectCast(Arguments, Object())
        Dim ScriptLocation As String = DirectCast(UnboxedArguments(0), String)
        Dim ScriptArguments() As Object = DirectCast(UnboxedArguments(1), Object())
        Using PSHost As PowerShell = PowerShell.Create
            PSHost.AddCommand(ScriptLocation)
            Dim ParamCount As Integer = ScriptArguments.Count
            If ParamCount > 0 Then
                For i As Integer = 0 To ParamCount - 1
                    PSHost.AddArgument(ScriptArguments(i))
                Next
            End If
            Dim Async As IAsyncResult = PSHost.BeginInvoke()
            Dim Result As Collection(Of PSObject)
            Dim ErrorResult As Collection(Of ErrorRecord) = Nothing
            Using PSResult As PSDataCollection(Of PSObject) = PSHost.EndInvoke(Async)
                Result = PSResult.ReadAll
            End Using
            If PSHost.HadErrors Then ErrorResult = PSHost.Streams.Error.ReadAll
            Dim Ret As New ScriptResult(Result, ErrorResult)
            Return Ret
        End Using
    End Function
    Private Sub AsyncTaskCompleted(Sender As Object, e As ThreadStarterEventArgs) Handles TaskRunner.WorkCompleted
        Dim UnboxedResult As ScriptResult = DirectCast(e.Result, ScriptResult)
        WhenFinished.Invoke(Me, New PSTaskCompletedEventArgs(UnboxedResult, varTaskInformation))
        If TaskRunner IsNot Nothing Then TaskRunner.Dispose()
    End Sub
    Private Function RunTask(Arguments() As Object) As ScriptResult
        Dim Result As Collection(Of PSObject)
        Dim Errors As Collection(Of ErrorRecord) = Nothing
        Using PSHost As PowerShell = PowerShell.Create
            PSHost.AddCommand(varTaskEntry.ScriptPath)
            Dim ParamCount As Integer = Arguments.Count
            If ParamCount > 0 Then
                For i As Integer = 0 To ParamCount - 1
                    PSHost.AddArgument(Arguments(i))
                Next
            End If
            Try
                Result = PSHost.Invoke()
            Catch Ex As Exception
                Throw
                Result = Nothing
            End Try
            If PSHost.HadErrors Then
                Errors = PSHost.Streams.Error.ReadAll
            End If
        End Using
        Return New ScriptResult(Result, Errors)
    End Function
#Region "IDisposable Support"
    Private disposedValue As Boolean ' To detect redundant calls
    Protected Overridable Sub Dispose(disposing As Boolean)
        If Not disposedValue Then
            If disposing Then
                Try
                    If TaskRunner IsNot Nothing AndAlso Not TaskRunner.Disposed Then TaskRunner.Dispose()
                Catch ex As Exception
                    Throw
                End Try
            End If
        End If
        disposedValue = True
    End Sub
    Public Sub Dispose() Implements IDisposable.Dispose
        Dispose(True)
    End Sub
#End Region
End Class

Public Class ScriptResult
    Private varResult As Collection(Of PSObject)
    Private varErrors As Collection(Of ErrorRecord)
    Public ReadOnly Property Result As Collection(Of PSObject)
        Get
            Return varResult
        End Get
    End Property
    Public ReadOnly Property Errors As Collection(Of ErrorRecord)
        Get
            Return varErrors
        End Get
    End Property
    Public Sub New(Result As Collection(Of PSObject), Errors As Collection(Of ErrorRecord))
        varResult = Result
        varErrors = Errors
    End Sub
End Class

Public Class PSTaskCompletedEventArgs
    Inherits EventArgs
    Private varErrorsOccurred As Boolean
    Private varTaskInfo As TaskInformation
    Private varResult As ScriptResult
    Public Sub New(Result As ScriptResult, TaskInfo As TaskInformation)
        varTaskInfo = TaskInfo
        varResult = Result
        If Result.Errors IsNot Nothing Then varErrorsOccurred = True
    End Sub
    Public ReadOnly Property TaskInformation As TaskInformation
        Get
            Return varTaskInfo
        End Get
    End Property
    Public ReadOnly Property ErrorsOccurred As Boolean
        Get
            Return varErrorsOccurred
        End Get
    End Property
    Public ReadOnly Property Result As ScriptResult
        Get
            Return varResult
        End Get
    End Property
End Class
Option Strict On
Option Explicit On
Option Infer Off
Imports System.ComponentModel
Imports System.Threading

Public NotInheritable Class Threader
    Implements IDisposable

#Region "Fields"
    Private SC As SynchronizationContext = SynchronizationContext.Current
    Private MethodToRun As Action(Of Object)
    Private FuncToRun As Func(Of Object, Object)
    Private MethodType As Integer
    Private Running As Boolean
    Private DisposeWhenFinished As Boolean = False
    Private IsBG As Boolean = True
    Private RaiseEventIfTrue As RaiseCompletedIfTrueDelegate
    Private SubMethod As Task, FuncMethod As Task(Of Object)
#End Region
#Region "Events"
    Public Event WorkCompleted(sender As Object, e As ThreadStarterEventArgs)
    Public Delegate Function RaiseCompletedIfTrueDelegate(Result As Object) As Boolean
#End Region
    Public ReadOnly Property Disposed As Boolean
        Get
            Return disposedValue
        End Get
    End Property
    ''' <summary>
    ''' Only define if special circumstances call for it. If defined, this the return value of this function will determine whether or not to raise the WorkCompleted event. If the asynchronous method was a function, the data returned from it will be passed.
    ''' Can be used as a substitute for event handlers (always return 0 to prevent the event from firing) if the sender object is not required.
    ''' </summary>
    Public WriteOnly Property RaiseWorkCompletedIfTrue As RaiseCompletedIfTrueDelegate
        Set(BooleanFunction As RaiseCompletedIfTrueDelegate)
            RaiseEventIfTrue = BooleanFunction
        End Set
    End Property
    Public Property IsBackground As Boolean
        Get
            Return IsBG
        End Get
        Set(value As Boolean)
            IsBG = value
        End Set
    End Property
    Public ReadOnly Property IsRunning As Boolean
        Get
            Return Running
        End Get
    End Property
    Public Sub New(Method As Action(Of Object))
        MethodToRun = Method
        MethodType = 1
    End Sub
    Public Sub New(Func As Func(Of Object, Object))
        FuncToRun = Func
        MethodType = 2
    End Sub
    Private Sub RaiseWorkCompleted(Result As Object)
        Dim e As New ThreadStarterEventArgs(Result)
        Running = False
        If RaiseEventIfTrue IsNot Nothing Then
            If RaiseEventIfTrue.Invoke(Result) Then RaiseEvent WorkCompleted(Me, e)
        Else
            RaiseEvent WorkCompleted(Me, e)
        End If
        If DisposeWhenFinished Then
            Dispose()
        End If
    End Sub
    Public Overloads Sub Start(Optional Parameters As Object = Nothing)
        If Running = False Then
            Running = True
            If IsBG Then
                Select Case MethodType
                    Case 0
                        Running = False
                        Throw New Exception("Cannot start this thread before it has been assigned a method to run (use the New constructor).")
                    Case 1
                        Dim MethodAndParameters() As Object = {MethodToRun, Parameters}
                        SubMethod = Task.Factory.StartNew(AddressOf Execute, MethodAndParameters)
                    Case 2
                        Dim MethodAndParameters() As Object = {FuncToRun, Parameters}
                        FuncMethod = Task.Factory.StartNew(AddressOf ExecuteFunction, MethodAndParameters)
                End Select
            Else
                Select Case MethodType
                    Case 0
                        Running = False
                        Throw New Exception("Cannot start this thread before it has been assigned a method to run (use the New constructor).")
                    Case 1
                        Dim MethodAndParameters() As Object = {MethodToRun, Parameters}
                        Dim SubMethodThread As New Thread(AddressOf Execute) With {.IsBackground = False}
                        SubMethodThread.Start(MethodAndParameters)
                    Case 2
                        Dim MethodAndParameters() As Object = {FuncToRun, Parameters}
                        Dim FuncMethodThread As New Thread(AddressOf ExecuteFunction) With {.IsBackground = False}
                        FuncMethodThread.Start(MethodAndParameters)
                End Select
            End If
        Else
            Throw New Exception("A thread is already in progress.")
        End If
    End Sub
    Private Sub Execute(MethodAndParameters As Object)
        Try
            Dim ArgArr() As Object = DirectCast(MethodAndParameters, Object())
            DirectCast(ArgArr(0), Action(Of Object)).Invoke(ArgArr(1))
        Catch Ex As Exception
            Throw
        Finally
            SC.Post(AddressOf RaiseWorkCompleted, Nothing)
        End Try
    End Sub
    Private Overloads Function ExecuteFunction(MethodAndParameters As Object) As Object
        Dim Ret As Object = Nothing
        Try
            Dim ArgArr() As Object = DirectCast(MethodAndParameters, Object())
            Ret = DirectCast(ArgArr(0), Func(Of Object, Object)).Invoke(ArgArr(1))
            Return Ret
        Catch Ex As Exception
            Throw
            Return Ret
        Finally
            SC.Post(AddressOf RaiseWorkCompleted, Ret)
        End Try
    End Function

#Region "IDisposable Support"
    Private disposedValue As Boolean ' To detect redundant calls

    ' IDisposable
    Protected Sub Dispose(disposing As Boolean)
        If Not disposedValue Then
            If disposing Then
                If SubMethod IsNot Nothing Then
                    If SubMethod.IsCompleted Then
                        SubMethod.Dispose()
                        Running = False
                    Else
                        'Throw New Exception("The Threader instance was disposed before the thread completed. The thread is still running and may be stuck.")
                    End If
                End If
                If FuncMethod IsNot Nothing Then
                    If FuncMethod.IsCompleted Then
                        FuncMethod.Dispose()
                        Running = False
                    Else
                        'Throw New Exception("The Threader instance was disposed before the thread completed. The thread is still running and may be stuck.")
                    End If
                End If
                MethodToRun = Nothing
                FuncToRun = Nothing
                MethodType = 0
            End If
        End If
        disposedValue = True
    End Sub
    Public Sub Dispose() Implements IDisposable.Dispose
        Dispose(True)
    End Sub
#End Region
End Class

Public Class ThreadStarterEventArgs
    Inherits EventArgs
    Private varResult As Object
    Public Property Result As Object
        Get
            Return varResult
        End Get
        Set(value As Object)
            varResult = value
        End Set
    End Property
    Public Sub New(Result As Object)
        varResult = Result
    End Sub
End Class
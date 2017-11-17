Option Strict On
Option Explicit On
Option Infer Off

Imports System.Drawing.Text
Imports System.Runtime.InteropServices

Public MustInherit Class FontCollection
    Implements IDisposable
    Private ResourceList As Byte()()
    Dim fontMemPointers() As IntPtr
    Private FontCollection() As PrivateFontCollection
    Private FontCache As New Dictionary(Of String, Font)
    Private varInitialized As Boolean = False
    Protected MustOverride Function DefineFontResourceList() As Byte()()
    Public Overridable ReadOnly Property GetFont(ByVal FontIndex As Integer, ByVal Size As Single, ByVal FontStyle As FontStyle) As Font
        Get
            If Not varInitialized Then
                ResourceList = DefineFontResourceList()
            End If
            If FontCollection Is Nothing Then LoadFonts()
            Dim Identity As New FontIdentity(FontIndex, Size, FontStyle)
            If Not FontCache.Keys.Contains(Identity.Identity) Then
                Dim NewFont As New Font(FontCollection(FontIndex).Families(0), Size, FontStyle)
                FontCache.Add(Identity.Identity, NewFont)
            End If
            Return FontCache(Identity.Identity)
        End Get
    End Property
    Public Sub New()
    End Sub
    Protected MustOverride ReadOnly Property DefaultFontOnException As Font
    Private Sub LoadFonts()
        ReDim FontCollection(ResourceList.Count - 1)
        ReDim fontMemPointers(ResourceList.Count - 1)
        For i As Integer = 0 To ResourceList.Count - 1
            Dim ResourceLength As Integer = ResourceList(i).Length
            fontMemPointers(i) = Marshal.AllocCoTaskMem(ResourceLength)
            Marshal.Copy(ResourceList(i), 0, fontMemPointers(i), ResourceLength)
            FontCollection(i) = New PrivateFontCollection
            FontCollection(i).AddMemoryFont(fontMemPointers(i), ResourceLength)
            Marshal.FreeCoTaskMem(fontMemPointers(i))
        Next
    End Sub
#Region "IDisposable Support"
    Private disposedValue As Boolean
    Protected Overridable Sub Dispose(disposing As Boolean)
        If Not disposedValue Then
            If disposing Then
                For Each Font As PrivateFontCollection In FontCollection
                    Font.Dispose()
                Next
                For Each Font As Font In FontCache.Values
                    Font.Dispose()
                Next
            End If
            ResourceList = Nothing
        End If
        disposedValue = True
    End Sub
    Public Sub Dispose() Implements IDisposable.Dispose
        Dispose(True)
    End Sub
#End Region
    Private Structure FontIdentity
        Private varIdentity As String
        Public ReadOnly Property Identity As String
            Get
                Return varIdentity
            End Get
        End Property
        Public Sub New(FontIndex As Integer, FontSize As Single, FontStyle As FontStyle)
            varIdentity = FontIndex & "/" & FontSize & "/" & FontStyle
        End Sub
    End Structure
End Class
Public Class RalewayFontCollection
    Inherits FontCollection
    Public Enum FontVariant As Integer
        Regular
        Light
        Medium
        Bold
    End Enum
    Public Shadows ReadOnly Property GetFont(ByVal FontVariant As FontVariant, ByVal Size As Single, ByVal FontStyle As FontStyle) As Font
        Get
            Return MyBase.GetFont(FontVariant, Size, FontStyle)
        End Get
    End Property
    Protected Overrides ReadOnly Property DefaultFontOnException As Font
        Get
            Return Nothing
        End Get
    End Property
    Protected Overrides Function DefineFontResourceList() As Byte()() ' TODO: add try-catch block in base GetFont function
        Return New Byte()() {My.Resources.Raleway_Light, My.Resources.Raleway_Medium, My.Resources.Raleway_Regular, My.Resources.Raleway_Bold}
    End Function
    Public Sub New()

    End Sub
End Class

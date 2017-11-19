'Imports System.Management.Automation
'Imports System.Collections.ObjectModel

'Module PSClasses
'    Public Class PSTask
'#Region "Erklæringer"
'        Implements IDisposable
'        Private varScriptLocation As String
'        Private varTaskDescriptiveName As String = "New task"
'        Private WithEvents TaskRunner As Threader
'        Private varRunAsync As Boolean
'        Public Event TaskCompleted(Sender As Object, e As PSTaskCompletedEventArgs)
'        Protected Friend Property RunAsync As Boolean
'            Get
'                Return varRunAsync
'            End Get
'            Set(value As Boolean)
'                varRunAsync = value
'            End Set
'        End Property
'#End Region
'        ''' <summary>
'        ''' Oppretter en ny PowerShellTask-instans for kjøring av PowerShell-script (synkront eller asynkront)
'        ''' </summary>
'        ''' <param name="RunAsync">Spesifiserer om scriptet skal kjøres asynkront (True) eller ikke (False)</param>
'        ''' <param name="DescriptiveName">Tittelen til oppgaven i brukergrensesnittet</param>
'        ''' <param name="ScriptFilePath">Full filbane til PowerShell-scriptet som skal kjøres</param>
'        Public Sub New(ByVal RunAsync As Boolean, ByVal DescriptiveName As String, ByVal ScriptFilePath As String)
'            ' Legger argumentene i de private variablene erklært i deklarasjonene
'            varScriptLocation = ScriptFilePath
'            varTaskDescriptiveName = DescriptiveName
'            varRunAsync = RunAsync
'            ' Hvis scriptet skal kjøres asynkront, trenger vi en instans av trådstarterklassen; ellers trenger vi ikke å bruke ressurser på dette
'            If varRunAsync Then
'                ' Scriptet vil kjøres i funksjonen RunTaskAsync()
'                TaskRunner = New Threader(AddressOf RunTaskAsync)
'            End If
'        End Sub
'        ''' <summary>
'        ''' Kjører scriptet
'        ''' </summary>
'        ''' <param name="Arguments">For script som har parametre. Det første elementet i matrisen gis til første parameter, og så videre.</param>
'        Public Sub Execute(Optional Arguments() As Object = Nothing)
'            ' Dersom scriptet skal kjøres asynkront...
'            If varRunAsync Then
'                ' Asynkrone metoder bør verken lese eller skrive til variabler som muligens kan endres under kjøringen av asynkron kode. Dette kan føre til
'                ' flere typer problemer. Derfor bør vi sende all nødvendig data som argumenter, slik at den asynkrone metoden får rådighet over en lokal kopi.
'                ' Fordi klassen ThreadStarterLight ikke godtar flere argumenter, må vi pakke alle variablene inn i ett objekt.
'                Dim BoxedArgs() As Object = {varScriptLocation, Arguments}
'                ' Vi kjører den asynkrone metoden, og gir argumentene våre i form av ett objekt (en matrise av objekter er fortsatt ett objekt)
'                TaskRunner.Start(BoxedArgs)
'            Else ' Hvis scriptet skal kjøres synkront...
'                ' Vi kjører den synkrone metoden RunTask og gir argumentene våre på vanlig måte. Fordi synkrone metoder blokkerer utføringen av ventende metoder
'                ' til koden har kjørt ferdig, trenger vi ikke å gi metoden en lokal kopi av nødvendig data.
'                Dim Result As ScriptResult = RunTask(Arguments)
'                RaiseEvent TaskCompleted(Me, New PSTaskCompletedEventArgs(Result, varScriptLocation))
'            End If
'        End Sub
'        ''' <summary>
'        ''' Metoden som kjøres (asynkront) dersom flagget varRunAsync er satt til True.
'        ''' </summary>
'        ''' <param name="Arguments">En matrise av objekter, der det første objektet er filbanen til scriptet (string), og det andre objektet er en matrise av strings (argumenter scriptets parametre)</param>
'        ''' <returns>Eventuelt svar fra scriptet (husk å caste fra Object til PSDataCollection når denne dataen skal behandles)</returns>
'        Private Function RunTaskAsync(Arguments As Object) As Object
'            ' Før tråden ble startet, pakket vi to nødvendige data inn i en matrise av objekter.
'            ' Denne funksjonen vet ikke hva den opprinnelige datatypen av Arguments var (nemlig en matrise av objekter, eller Object()), men vi vet det.
'            ' Fordi vi er sikre på at den opprinnelige datatypen var Object(), kan vi trygt "konvertere" objektet tilbake til en matrise.
'            Dim UnboxedArguments() As Object = DirectCast(Arguments, Object())
'            ' Variabelen UnboxedArguments() tilsvarer nå variablen BoxedArgs() i metoden Execute().
'            ' Videre vet vi denne objektmatrisen inneholder en string (filbanen) som objekt #1, og en matrise av strings (script-argumentene) i objekt #2:
'            Dim ScriptLocation As String = DirectCast(UnboxedArguments(0), String)
'            Dim ScriptArguments() As Object = DirectCast(UnboxedArguments(1), Object())
'            ' Vi har nå den opprinnelige dataen med riktig datatype (string og string()).
'            ' PowerShell-klassen implementerer IDisposable. Ved å bruke stikkordet Using, forsikrer vi oss om at unhåndterte ressursers frigjøres automatisk
'            ' ved End Using.
'            Using PSHost As PowerShell = PowerShell.Create ' Lager en ny instans av PowerShell
'                ' Vi spesifiserer filbanen til scriptet som skal kjøres
'                PSHost.AddCommand(ScriptLocation)
'                ' Vi finner ut hvor mange parametre vi skal legge til i scriptet ved å telle antall elementer i string-matrisen (script-argumentene)
'                Dim ParamCount As Integer = ScriptArguments.Count
'                ' Vi legger til ett nytt parameter for hvert argument. Det første elementet får navnet param1, det neste får navnet param2, osv.
'                ' Vi kan også direkte legge til en verdi argumentet i stedet for å bruke metoden AddArgument(). Vi kan ikke bruke en 'For Each'-løkke
'                ' her, da vi trenger en teller både for å generere parameternavnet og for å hente tilsvarende verdi for argumentet.
'                ' Dersom ingen argumenter er gitt, hopper vi over dette.
'                If ParamCount > 0 Then
'                    For i As Integer = 0 To ParamCount - 1
'                        ' PSHost.AddParameter("param" & (i + 1).ToString, ScriptArguments(i))
'                        PSHost.AddArgument(ScriptArguments(i))
'                    Next
'                End If
'                ' Her har jeg valgt å kjøre scriptet asynkront, selv om PowerShell-hosten som skal kjøre scriptet allerede kjører asynkront i en ny tråd,
'                ' fordi PowerShell-hosten skal få vite at scriptet kjøres asynkront og muligens hindre ulovlig aksess av ressurser.
'                ' Merk deg at vi fortsatt trenger en asynkron metode for å kjøre asynkrone script, fordi hentingen av resultatet vil blokkere kjøring til
'                ' resultatet er klart. Vi ønsker ikke at grensesnittet skal fryse.

'                ' Vi erklærer en variabel av typen IAsyncResult som inneholder informasjon om tilstanden til en asynkron prosess.
'                ' Når variabelen tildeles en instans av klassen gjennom metoden BeginInvoke, får den en "kjører"-status. Når scriptet er ferdig vil
'                ' statusen endres, og vi kan hente ut resultatet. Metoden EndInvoke returnerer altså ikke noen data før tilstanden endres til "fullført,"
'                ' og videre utføring av koden vil blokkeres inntil dette har skjedd.
'                Dim Async As IAsyncResult = PSHost.BeginInvoke()
'                ' Resultatet av et PowerShell-script har datatypen PSDataCollection(Of T), der T er datatypen til elementene i samlingen, nemlig PSObjekt.
'                ' Vi erklærer en variabel av denne typen og lar PSHost lagre resultatet i variabelen når scriptet er ferdig.
'                Dim Result As Collection(Of PSObject)
'                Dim ErrorResult As Collection(Of ErrorRecord) = Nothing
'                Using PSResult As PSDataCollection(Of PSObject) = PSHost.EndInvoke(Async)
'                    Result = PSResult.ReadAll
'                End Using
'                If PSHost.HadErrors Then
'                    ErrorResult = PSHost.Streams.Error.ReadAll
'                End If
'                Dim Ret As New ScriptResult(Result, ErrorResult)
'                Return Ret ' Scriptet er ferdig, og vi kan returnere resultatet.
'                ' Merk at vi ikke vil skrive resultatet direkte til en ikke-lokal variabel, selv om dette ville være lettvint. Ikke-lokale variabler må ikke
'                ' endres eller leses av asynkron kode. Vi lar ThreadStarter-klassen ta seg av dette.
'            End Using ' Unhåndterte ressurser brukt av PowerShell-hosten frigjøres automatisk. Vi trenger ikke et kall til PSHost.Dispose()
'        End Function
'        Private Sub AsyncTaskCompleted(Sender As Object, e As ThreadStarterEventArgs) Handles TaskRunner.WorkCompleted
'            ' Denne metoden kalles synkront når den asynkrone metoden er fullført. Resultatet vi ikke ville lagre direkte gjøres nå tilgjengelig gjennom et parameter.
'            ' Merk at resultatet er pakket inn i et objekt, og vi må pakke det ut igjen.
'            Dim UnboxedResult As ScriptResult = DirectCast(e.Result, ScriptResult)
'            ' Vi kan nå behandle dataen.
'            RaiseEvent TaskCompleted(Me, New PSTaskCompletedEventArgs(UnboxedResult, varTaskDescriptiveName))
'        End Sub
'        Private Function RunTask(Arguments() As Object) As ScriptResult
'            Dim Result As Collection(Of PSObject)
'            Dim Errors As Collection(Of ErrorRecord) = Nothing
'            Using PSHost As PowerShell = PowerShell.Create
'                PSHost.AddCommand(varScriptLocation)
'                Dim ParamCount As Integer = Arguments.Count
'                If ParamCount > 0 Then
'                    For i As Integer = 0 To ParamCount - 1
'                        PSHost.AddArgument(Arguments(i))
'                    Next
'                End If
'                Try
'                    Result = PSHost.Invoke()
'                Catch Ex As Exception
'                    Throw
'                    Result = Nothing
'                End Try
'                If PSHost.HadErrors Then
'                    Errors = PSHost.Streams.Error.ReadAll
'                End If
'            End Using
'            Return New ScriptResult(Result, Errors)
'        End Function
'#Region "IDisposable Support"
'        Private disposedValue As Boolean ' To detect redundant calls
'        Protected Overridable Sub Dispose(disposing As Boolean)
'            If Not disposedValue Then
'                If disposing Then
'                    Try
'                        If TaskRunner IsNot Nothing OrElse Not TaskRunner.Disposed Then TaskRunner.Dispose()
'                    Catch ex As Exception
'                        Throw
'                    End Try
'                End If
'            End If
'            disposedValue = True
'        End Sub
'        Public Sub Dispose() Implements IDisposable.Dispose
'            Dispose(True)
'        End Sub
'#End Region
'    End Class

'    Public Class ScriptResult
'        Private varResult As Collection(Of PSObject)
'        Private varErrors As Collection(Of ErrorRecord)
'        Public ReadOnly Property Result As Collection(Of PSObject)
'            Get
'                Return varResult
'            End Get
'        End Property
'        Public ReadOnly Property Errors As Collection(Of ErrorRecord)
'            Get
'                Return varErrors
'            End Get
'        End Property
'        Public Sub New(Result As Collection(Of PSObject), Errors As Collection(Of ErrorRecord))
'            varResult = Result
'            varErrors = Errors
'        End Sub
'    End Class

'    Public Class PSTaskCompletedEventArgs
'        Inherits EventArgs
'        Private varErrorsOccurred As Boolean
'        Private varResult As ScriptResult
'        Private varDescriptiveName As String
'        Public ReadOnly Property DescriptiveName As String
'            Get
'                Return varDescriptiveName
'            End Get
'        End Property
'        Public ReadOnly Property ErrorsOccurred As Boolean
'            Get
'                Return varErrorsOccurred
'            End Get
'        End Property
'        Public ReadOnly Property Result As ScriptResult
'            Get
'                Return varResult
'            End Get
'        End Property
'        Public Sub New(Result As ScriptResult, DescriptiveName As String)
'            varDescriptiveName = DescriptiveName
'            varResult = Result
'            If Result.Errors IsNot Nothing Then varErrorsOccurred = True
'        End Sub
'    End Class
'End Module

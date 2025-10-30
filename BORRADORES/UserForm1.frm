VERSION 5.00
Begin {C62A69F0-16DC-11CE-9E98-00AA00574A4F} UserForm1 
   Caption         =   "COOPERATIVA FAMILIA UNIDA"
   ClientHeight    =   8160
   ClientLeft      =   120
   ClientTop       =   468
   ClientWidth     =   6216
   OleObjectBlob   =   "UserForm1.frx":0000
   StartUpPosition =   1  'CenterOwner
End
Attribute VB_Name = "UserForm1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Sub CargarPrestamosPendientes()

 Dim wsPrestamos As Worksheet
    Dim ultimaFila As Long
    Dim i As Long
    Dim celdaREG As Range, celdaESTATUS As Range, celdaNOMBRE As Range
    Dim colREG As Long, colESTATUS As Long, colNOMBRE As Long
    Dim regValor As String, estatusValor As Variant

    ' Intentar establecer la hoja
    On Error Resume Next
    Set wsPrestamos = ThisWorkbook.Sheets("PRESTAMOS")
    On Error GoTo 0

    If wsPrestamos Is Nothing Then
        MsgBox "No se encontró la hoja 'PRESTAMOS'.", vbExclamation
        Exit Sub
    End If

    ' Buscar columnas por nombre
    Set celdaREG = wsPrestamos.Rows(2).Find("REG", LookIn:=xlValues, LookAt:=xlWhole)
    Set celdaESTATUS = wsPrestamos.Rows(2).Find("ESTATUS", LookIn:=xlValues, LookAt:=xlWhole)
    Set celdaNOMBRE = wsPrestamos.Rows(2).Find("NOMBRE CLIENTE", LookIn:=xlValues, LookAt:=xlWhole)

    If celdaREG Is Nothing Or celdaESTATUS Is Nothing Or celdaNOMBRE Is Nothing Then
        MsgBox "No se encontraron las columnas 'REG' o 'ESTATUS' o 'NOMBRE'.", vbExclamation
        Exit Sub
    End If
    
    colREG = celdaREG.Column
    colESTATUS = celdaESTATUS.Column
    colNOMBRE = celdaNOMBRE.Column
    ultimaFila = wsPrestamos.Cells(wsPrestamos.Rows.Count, colREG).End(xlUp).Row

    cmbprestpendt.Clear ' Limpiar ComboBox antes de llenar

    ' Recorrer filas y agregar REG si el ESTATUS es "PENDIENTE"
'    For i = 2 To ultimaFila
'        estatusValor = wsPrestamos.Cells(i, colESTATUS).Value
'        regValor = wsPrestamos.Cells(i, colREG).Value
'
'        If Not IsError(estatusValor) Then
'            If Trim(UCase(estatusValor)) = "PENDIENTE" Then
'                cmbprestpendt.AddItem "PREST" & regValor
'            End If
'        End If
'    Next i
    
        ' Recorrer filas y agregar REG + NOMBRE si el ESTATUS es "PENDIENTE"
    For i = 3 To ultimaFila
        estatusValor = wsPrestamos.Cells(i, colESTATUS).Value
        regValor = wsPrestamos.Cells(i, colREG).Value
        nombreValor = wsPrestamos.Cells(i, colNOMBRE).Value

        If Not IsError(estatusValor) Then
            If Trim(UCase(estatusValor)) = "PENDIENTE" Then
                With cmbprestpendt
                    .AddItem "PREST" & regValor
                    .List(.ListCount - 1, 1) = nombreValor
                End With
            End If
        End If
    Next i
'
    'Recuperar el registro sin prefijo
'    Dim numeroSinPrefijo As String
'    numeroSinPrefijo = Replace(cmbprestpendt.Value, "PRÉSTAMO-", "")

    ' Verificar si se agregaron elementos
    If cmbprestpendt.ListCount = 0 Then
        MsgBox "No hay préstamos con estatus 'PENDIENTE'.", vbInformation
    End If

'    Dim wsPrestamos As Worksheet
'    Dim ultimaFila As Long
'    Dim i As Long
'    Dim celdaREG As Range, celdaESTATUS As Range
'    Dim colREG As Long, colESTATUS As Long
'    Dim regValor As String, estatusValor As Variant
'
'    ' Intentar establecer la hoja
'    On Error Resume Next
'    Set wsPrestamos = ThisWorkbook.Sheets("PRESTAMOS")
'    On Error GoTo 0
'
'    If wsPrestamos Is Nothing Then
'        MsgBox "No se encontró la hoja 'PRESTAMOS'.", vbExclamation
'        Exit Sub
'    End If
'
'    ' Buscar columnas por nombre
'    Set celdaREG = wsPrestamos.Rows(2).Find("REG", LookIn:=xlValues, LookAt:=xlWhole)
'    Set celdaESTATUS = wsPrestamos.Rows(2).Find("ESTATUS", LookIn:=xlValues, LookAt:=xlWhole)
'
'    If celdaREG Is Nothing Or celdaESTATUS Is Nothing Then
'        MsgBox "No se encontraron las columnas 'REG' o 'ESTATUS'.", vbExclamation
'        Exit Sub
'    End If
'
'    colREG = celdaREG.Column
'    colESTATUS = celdaESTATUS.Column
'    ultimaFila = wsPrestamos.Cells(wsPrestamos.Rows.Count, colREG).End(xlUp).Row
'
'    cmbprestpendt.Clear ' Limpiar ComboBox antes de llenar
'
'    ' Recorrer filas y agregar REG si el ESTATUS es "PENDIENTE"
'    For i = 2 To ultimaFila
'        estatusValor = wsPrestamos.Cells(i, colESTATUS).Value
'        regValor = wsPrestamos.Cells(i, colREG).Value
'
'        If Not IsError(estatusValor) Then
'            If Trim(UCase(estatusValor)) = "PENDIENTE" Then
'                cmbprestpendt.AddItem "PREST" & regValor
'            End If
'        End If
'    Next i
'
'    'Recuperar el registro sin prefijo
''    Dim numeroSinPrefijo As String
''    numeroSinPrefijo = Replace(cmbprestpendt.Value, "PRÉSTAMO-", "")
'
'    ' Verificar si se agregaron elementos
'    If cmbprestpendt.ListCount = 0 Then
'        MsgBox "No hay préstamos con estatus 'PENDIENTE'.", vbInformation
'    End If
End Sub

Private Sub cbclearcaj_Click()
    Call LimpiarCamposCaja
End Sub

Private Sub cbdatospestmo_Click()
    Dim codPrest As Long

    ' Habilita el combo
    Me.cmbprestpendt.Enabled = True

    ' Verifica si se ha seleccionado un valor válido
    If Trim(Me.cmbprestpendt.Text) = "" Then
        MsgBox "Elige una secuencia de préstamo para traer los datos", vbExclamation
        Exit Sub
    End If

    ' Extrae el número del texto (asumiendo formato tipo "PREST123")
    On Error GoTo ErrorHandler
    codPrest = CLng(Replace(Me.cmbprestpendt.Text, "PREST", ""))
    Call BuscarRegistroPrestamoPorID(codPrest)
    Exit Sub

ErrorHandler:
    MsgBox "El formato del préstamo no es válido.", vbCritical

End Sub

Private Sub cbreportpr_Click()
    Call ReportePrestamoFormulario
End Sub

Private Sub cbsavepr_Click()
    Call GuardarRegistroPrestamos
End Sub

Private Sub cbupdatepr_Click()
valorBuscado = ObtenerID2(Me.tbsecpret.Value)
'    If IsNumeric(Me.tbsecpret.Text) Then

If tbsecpret.Value = "" Or tbfechprest.Value = "" Or tbcodsocprest.Value = "" _
Or tbcomprest.Value = "" Or cbnomsocprest.Value = "" Or tbmontprest.Value = "" _
Or tbplzprest.Value = "" Or tbintrprest.Value = "" Then
    MsgBox "Busque el id_prestamo para editar"
    Exit Sub
'    End
Else

'    If IsNumeric(valorBuscado) Then
'    If IsNumeric(valorBuscado) Then
    
        Dim respuesta As VbMsgBoxResult
        respuesta = MsgBox("¿Estás seguro de que deseas editar este registro?", vbYesNo + vbQuestion, "Confirmar eliminación")

        If respuesta = vbYes Then
                Dim clave As String
                clave = InputBox("Ingresa la contraseña para eliminar el registro:", "Autenticación")
        
                If clave <> "1998" Then
                    MsgBox "Contraseña incorrecta. No se puede eliminar el registro.", vbCritical
                    Exit Sub
                End If
            If IsNumeric(valorBuscado) Then
'            Call EliminarRegistroPrestamoPorID(CLng(Me.tbsecpret.Text))
'            Call EliminarRegistroPrestamoPorID(valorBuscado)
            Call EditarRegistroPrestamosPorID(CLng(valorBuscado))
'            MsgBox "Registro eliminado (si existía).", vbInformation
            Call LimpiarCamposFormularioPrestamo
'            Me.tbsecpret.Text = GenerarSiguienteIDPrest("")
            Else
            'Call EliminarRegistroPrestamoPorID(CLng(Me.tbsecpret.Text))
            Call EditarRegistroPrestamosPorID(valorBuscado)
'            Call EliminarRegistroPrestamoPorID(CLng(valorBuscado))
'            MsgBox "Registro eliminado (si existía).", vbInformation
            Call LimpiarCamposFormularioPrestamo
'            Me.tbsecpret.Text = GenerarSiguienteIDPrest("")
            End If
        Else
            MsgBox "Actualizacion cancelada.", vbInformation
        End If
'    Else
'        MsgBox "Por favor ingresa un ID numérico válido.", vbExclamation
'    End If
End If
End Sub

Private Sub cmbprestpendt_Change()
'    Dim codPrest As Long
'    codPrest = CLng(Replace(Me.cmbprestpendt.Text, "PREST", ""))
'    Call BuscarRegistroPrestamoPorID(codPrest)
    Me.tbcomntpag.Text = "PAGO DE LA LETRA #"
    Me.cmbnomsocpg.Enabled = False
    Me.tbcodsocio.Enabled = False
End Sub

Private Sub cbtnsavepago_Click()
    Call GuardarRegistroPagos
End Sub
Private Sub CommandButton3_Click()
    Me.cmbnomsocpg.Clear ' ?? Limpia el ComboBox para evitar duplicados
    Call LimpiarCamposFormularioPagos
End Sub

Private Sub CommandButton5_Click()

valorBuscado = ObtenerID(Me.tbsecpag.Value)

If tbsecpag.Value = "" Or cmbprestpendt.Value = "" Or tbcodsocio.Value = "" _
Or cmbnomsocpg.Value = "" Or tbmontprstpg.Value = "" Or tbfechpag1.Value = "" _
Or tbcuotpg.Value = "" Or tbintprestpg.Value = "" Or tbfechpag2.Value = "" _
Or tbintpg.Value = "" Or tbcomntpag.Value = "" Then
    MsgBox "Busque el id_prestamo para editar"
    Exit Sub
Else
        Dim respuesta As VbMsgBoxResult
        respuesta = MsgBox("¿Estás seguro de que deseas editar este registro?", vbYesNo + vbQuestion, "Confirmar eliminación")

        If respuesta = vbYes Then
                Dim clave As String
                clave = InputBox("Ingresa la contraseña para editar el registro:", "Autenticación")
        
                If clave <> "1998" Then
                    MsgBox "Contraseña incorrecta. No se puede eliminar el registro.", vbCritical
                    Exit Sub
                End If
            If IsNumeric(valorBuscado) Then
            Call EditarRegistroPagosPorID(CLng(valorBuscado))
'            MsgBox "Registro eliminado (si existía).", vbInformation
            Call LimpiarCamposFormularioPagos
'            Me.tbsecpag.Text = GenerarSiguienteIDPrest("")
            Else
            Call EditarRegistroPagosPorID(valorBuscado)
'            MsgBox "Registro editado.", vbInformation
            Call LimpiarCamposFormularioPagos
'            Me.tbsecpag.Text = GenerarSiguienteIDPrest("")
            End If
        Else
            MsgBox "Eliminación cancelada.", vbInformation
        End If
End If

'    valorBuscado = ObtenerID(Me.tbsecpag.Value)
'    EditarRegistroPagosPorID (Me.tbsecpag.Value)
End Sub

Private Sub CommandButton6_Click()
    Dim ws As Worksheet
    Dim i As Long
    Dim celda As Range

    Set ws = Sheets("LINEAMIENTOS")
    Me.cmbnomsocpg.Enabled = True
    ' Configura el ComboBox para tener 2 columnas
    Me.cmbnomsocpg.ColumnCount = 2
    Me.cmbnomsocpg.ColumnWidths = "150 pt;0 pt" ' Muestra solo el nombre, oculta el código

    For i = 5 To 20 ' Desde fila 3 hasta la 20
        If ws.Cells(i, 2).Value <> "" Then
            Me.cmbnomsocpg.AddItem ws.Cells(i, 2).Value  ' Nombre en columna B
            Me.cmbnomsocpg.List(Me.cmbnomsocpg.ListCount - 1, 1) = ws.Cells(i, 1).Value  ' Código en columna A
        End If
    Next i
    Call ReportePagosFormulario
End Sub

Private Sub obtnayud_Click()
    nuevoIDPrestamo = GenerarSiguienteIDPrest("AYUDA")
    Me.tbsecpret.Text = nuevoIDPrestamo
    Me.tbcodsocprest.Text = 2
    Me.tbplzprest.Text = 0
    Me.tbintrprest.Text = 0
    Me.tbplzprest.Enabled = False
    Me.tbintrprest.Enabled = False
    
'    Set ws = Sheets("LINEAMIENTOS")
'    For i = 5 To 20 ' Desde fila 3 hasta la 20
'    If ws.Cells(i, 2).Value <> "" Then
'        Me.cbnomsocprest.AddItem ws.Cells(i, 2).Value  ' Nombre en columna B
''        Me.cbmsocap.List(Me.cbmsocap.ListCount - 1, 1) = ws.Cells(i, 1).Value  ' Código en columna A
'    End If
'    Next i
    Call LimpiarCamposAyuda
End Sub

Private Sub obtnegre_Click()
    Call LimpiarCamposEgresos
    
    Me.cbdatospestmo.Enabled = True
'    Me.obtnpag.Enabled = True
'    Me.obtning.Enabled = True
'    Me.obtnegre.Enabled = True
'    Me.obtnpag.Value = False
'    Me.obtning.Value = False
'    Me.obtnegre.Value = False
    
    Me.tbcomntpag.MultiLine = True
    Me.tbsecpag.Enabled = False
    Me.cmbprestpendt.Enabled = True
    Me.tbcodsocio.Enabled = False
    Me.cmbnomsocpg.Enabled = True
    Me.tbmontprstpg.Enabled = True
    Me.tbfechpag1.Enabled = True
    Me.tbcuotpg.Enabled = True
    Me.tbintprestpg.Enabled = True
    Me.tbfechpag2.Enabled = True
    Me.tbintpg.Enabled = True
    Me.tbcomntpag.Enabled = True
    
    nuevoIDPago = GenerarSiguienteIDPagos("EGRESO")
    Me.tbsecpag.Text = nuevoIDPago
'    Dim codPrest As Long
    Me.cmbprestpendt.Enabled = True
    Me.cmbnomsocpg.Enabled = True
'    codPrest = CLng(Replace(Me.cmbprestpendt.Text, "PREST", ""))
'    Call BuscarRegistroPrestamoPorID(codPrest)
    Me.tbcomntpag.Text = ""
    
    Set ws = Sheets("LINEAMIENTOS")

    ' Configura el ComboBox para tener 2 columnas
    Me.cmbnomsocpg.ColumnCount = 2
    Me.cmbnomsocpg.ColumnWidths = "150 pt;0 pt" ' Muestra solo el nombre, oculta el código
    Me.cmbnomsocpg.Clear

    For i = 4 To 4
'    If i <> 4 Then  ' Excluye la fila 4
        If ws.Cells(i, 2).Value <> "" Then
            Me.cmbnomsocpg.AddItem ws.Cells(i, 2).Value  ' Nombre en columna B
            Me.cmbnomsocpg.List(Me.cmbnomsocpg.ListCount - 1, 1) = ws.Cells(i, 1).Value  ' Código en columna A
        End If
'    End If
    Next i
End Sub

Private Sub obtning_Click()
    Call LimpiarCamposIngresos
    nuevoIDPago = GenerarSiguienteIDPagos("INGRESO")
    Me.tbsecpag.Text = nuevoIDPago
'    Me.cmbprestpendt.Enabled = False
    Me.cmbprestpendt.Text = 0
    Me.tbcodsocio.Text = 1
    Me.tbmontprstpg.Text = ""
    Me.tbcuotpg.Text = 0
    Me.tbintprestpg.Text = ""
    Me.tbintpg.Text = 0
    Me.tbcomntpag.Text = ""
    Me.tb1pg.Text = ""
    Me.tb2pg.Text = ""
    Me.tb3pg.Text = ""
    Me.tb4pg.Text = ""
    Me.tb5pg.Text = ""
    Me.tb6pg.Text = ""
    
    Set ws = Sheets("LINEAMIENTOS")
    Me.cmbnomsocpg.Enabled = True
    ' Configura el ComboBox para tener 2 columnas
    Me.cmbnomsocpg.ColumnCount = 2
    Me.cmbnomsocpg.ColumnWidths = "150 pt;0 pt" ' Muestra solo el nombre, oculta el código
    Me.cmbnomsocpg.Clear ' ?? Limpia el ComboBox para evitar duplicados

    For i = 3 To 20
    If i <> 4 Then  ' Excluye la fila 4
        If ws.Cells(i, 2).Value <> "" Then
            Me.cmbnomsocpg.AddItem ws.Cells(i, 2).Value  ' Nombre en columna B
    '            Me.cbmsocap.List(Me.cbmsocap.ListCount - 1, 1) = ws.Cells(i, 1).Value  ' Código en columna A
        End If
    End If
    Next i
    
End Sub

Private Sub obtnpag_Click()
    Call LimpiarCamposPagos
    
    Me.cbdatospestmo.Enabled = True
'    Me.obtnpag.Enabled = True
'    Me.obtning.Enabled = True
'    Me.obtnegre.Enabled = True
'    Me.obtnpag.Value = False
'    Me.obtning.Value = False
'    Me.obtnegre.Value = False
    
    Me.tbcomntpag.MultiLine = True
    Me.tbsecpag.Enabled = False
    Me.cmbprestpendt.Enabled = True
    Me.tbcodsocio.Enabled = False
    Me.cmbnomsocpg.Enabled = True
    Me.tbmontprstpg.Enabled = True
    Me.tbfechpag1.Enabled = True
    Me.tbcuotpg.Enabled = True
    Me.tbintprestpg.Enabled = True
    Me.tbfechpag2.Enabled = True
    Me.tbintpg.Enabled = True
    Me.tbcomntpag.Enabled = True
    
    nuevoIDPago = "PAGOS" & Format(GenerarSiguienteIDPagos(""), "") ' Asegúrate de que esta función esté definida correctamente
    Me.tbsecpag.Text = nuevoIDPago
    Me.cmbprestpendt.Enabled = True
'    Dim codPrest As Long
'    codPrest = CLng(Replace(Me.cmbprestpendt.Text, "PREST", ""))
'    Call BuscarRegistroPrestamoPorID(codPrest)
    Me.tbcomntpag.Text = ""
'    Call UserForm_Initialize
    
'    Set ws = Sheets("LINEAMIENTOS")

    ' Configura el ComboBox para tener 2 columnas
'    Me.cmbnomsocpg.ColumnCount = 2
'    Me.cmbnomsocpg.ColumnWidths = "150 pt;0 pt" ' Muestra solo el nombre, oculta el código
'    Me.cmbnomsocpg.Clear ' ?? Limpia el ComboBox para evitar duplicados

'    For i = 5 To 20
''    If i <> 4 Then  ' Excluye la fila 4
'        If ws.Cells(i, 2).Value <> "" Then
'            Me.cmbnomsocpg.AddItem ws.Cells(i, 2).Value  ' Nombre en columna B
'            Me.cmbnomsocpg.List(Me.cmbnomsocpg.ListCount - 1, 1) = ws.Cells(i, 1).Value  ' Código en columna A
'        End If
''    End If
'    Next i
End Sub

Private Sub obtnprest_Click()
    Call LimpiarCamposPrestamos
    nuevoIDPrestamo = "PREST" & Format(GenerarSiguienteIDPrest(""), "") ' Asegúrate de que esta función esté definida correctamente
    Me.tbsecpret.Text = nuevoIDPrestamo
    Me.tbplzprest.Enabled = True
    Me.tbintrprest.Enabled = True
    Me.tbcodsocprest.Enabled = False
    Me.tbcodsocprest.Value = ""
End Sub
Private Sub tbmontprest_Change()
    CalcularCuotaPrestamo
    CalcularToatalPrestamo
    CalcularMontMens
    CalcularIntMens
End Sub

Private Sub tbintrprest_Change()
'    Dim valor As Double
'
'    ' Obtener el valor numérico sin el símbolo %
'    valor = Val(Replace(Me.tbintrprest.Text, "%", ""))
'
'    ' Mostrar el valor con el símbolo % visualmente
'    Me.tbintrprest.Text = Format(valor, "00.00") & "%"
'
'    ' Colocar el cursor al final para evitar sobrescribir
'    Me.tbintrprest.SelStart = Len(Me.tbintrprest.Text)
    CalcularCuotaPrestamo
    CalcularToatalPrestamo
    CalcularMontMens
    CalcularIntMens
End Sub

Private Sub tbplzprest_Change()
    CalcularCuotaPrestamo
    CalcularToatalPrestamo
    CalcularMontMens
    CalcularIntMens
End Sub
Private Sub CalcularCuotaPrestamo()
    Dim monto As Double, interes As Double, plazo As Double
    Dim esAyuda As Boolean
    esAyuda = Me.obtnayud.Value
    
    ' Validar que los campos no estén vacíos
    If Trim(Me.tbmontprest.Text) <> "" And _
       Trim(Me.tbintrprest.Text) <> "" And _
       Trim(Me.tbplzprest.Text) <> "" Then

        ' Convertir a valores numéricos
        monto = Val(Me.tbmontprest.Text)
        interes = Val(Me.tbintrprest.Text)
        plazo = Val(Me.tbplzprest.Text)
        If esAyuda = True Then
                Me.tbintrprest.Text = 0
                Me.tbplzprest.Text = 0
        Else
        ' Validar que no sean cero
            If monto > 0 And interes > 0 And plazo > 0 Then
                Dim cuota As Double
                cuota = Round((monto / plazo) + (monto * interes / 100), 2)
                Me.tbcoutprst.Text = Round(Format(cuota, "0.00"), 2)
            Else
                MsgBox "Los valores no pueden ser cero.", vbExclamation, "Validación"
                Me.tbmontprest.Text = ""
                Me.tbcoutprst.Text = ""
                Me.tbintrprest.Text = ""
                Me.tbplzprest.Text = ""
            End If
        End If
    Else
        Me.tbcoutprst.Text = ""
    End If
End Sub
Private Sub CalcularToatalPrestamo()
    Dim monto As Double, plazo As Integer, interes As Double
    Dim esAyuda As Boolean
    esAyuda = Me.obtnayud.Value
    
    If Trim(Me.tbmontprest.Text) <> "" And _
       Trim(Me.tbintrprest.Text) <> "" And _
       Trim(Me.tbplzprest.Text) <> "" Then

    monto = Val(Me.tbmontprest.Text)
    plazo = Val(Me.tbplzprest.Text)
    interes = Val(Me.tbintrprest.Text)
    
        If esAyuda = True Then
            Me.tbintrprest.Text = 0
            Me.tbplzprest.Text = 0
        Else
            If monto > 0 And interes > 0 And plazo > 0 Then
            Dim total As Double
        '        total = ((Val(Me.tbmontprest.Text) / Val(Me.tbplzprest)) + (Val(Me.tbmontprest.Text) * Val(Me.tbintrprest.Text) / 100)) * Val(Me.tbplzprest)
        '        Me.lbtotlprest.Text = Format(total, "0.00")
            total = ((monto / plazo) + (monto * (interes / 100))) * plazo
            Me.lbtotlprest.Text = Format(total, "0.00")
            Else
            MsgBox "Los valores no pueden ser cero.", vbExclamation, "Validación"
            Me.tbmontprest.Text = ""
            Me.lbtotlprest.Text = ""
            Me.tbintrprest.Text = ""
            Me.tbplzprest.Text = ""
            End If
        End If
    Else
        Me.lbtotlprest.Text = ""
    End If
End Sub
Private Sub CalcularMontMens()
    Dim monto As Double, plazo As Integer, interes As Double
    Dim esAyuda As Boolean
    esAyuda = Me.obtnayud.Value
    
    If Trim(Me.tbmontprest.Text) <> "" And _
       Trim(Me.tbintrprest.Text) <> "" And _
       Trim(Me.tbplzprest.Text) <> "" Then
       
    monto = Val(Me.tbmontprest.Text)
    plazo = Val(Me.tbplzprest.Text)
    interes = Val(Me.tbintrprest.Text)
    
        If esAyuda = True Then
            Me.tbintrprest.Text = 0
            Me.tbplzprest.Text = 0
        Else
            If monto > 0 And interes > 0 And plazo > 0 Then
                Dim total As Double
        '        total = Val(Me.tbmontprest.Text) / Val(Me.tbplzprest)
                total = Round(monto / plazo, 2)
                Me.tbmontmprest.Text = Format(total, "0.00")
            Else
                MsgBox "Los valores no pueden ser cero.", vbExclamation, "Validación"
                Me.tbmontprest.Text = ""
                Me.tbmontmprest.Text = ""
                Me.tbintrprest.Text = ""
                Me.tbplzprest.Text = ""
            End If
        End If
    Else
        Me.tbmontmprest.Text = ""
    End If
End Sub
Private Sub CalcularIntMens()
    Dim monto As Double, plazo As Integer, interes As Double
    Dim esAyuda As Boolean
    esAyuda = Me.obtnayud.Value
    
    If Trim(Me.tbmontprest.Text) <> "" And _
       Trim(Me.tbintrprest.Text) <> "" And _
       Trim(Me.tbplzprest.Text) <> "" Then
    
    monto = Val(Me.tbmontprest.Text)
    plazo = Val(Me.tbplzprest.Text)
    interes = Val(Me.tbintrprest.Text)
    
        If esAyuda = True Then
            Me.tbintrprest.Text = 0
            Me.tbplzprest.Text = 0
        Else
            If monto > 0 And interes > 0 And plazo > 0 Then
            Dim total As Double
    '        total = Val(Me.tbmontprest.Text) * (Val(Me.tbintrprest.Text) / 100)
            total = monto * (interes / 100)
            Me.tbintmenprest.Text = Format(total, "0.00")
            Else
                MsgBox "Los valores no pueden ser cero.", vbExclamation, "Validación"
                Me.tbmontprest.Text = ""
                Me.tbintmenprest.Text = ""
                Me.tbintrprest.Text = ""
                Me.tbplzprest.Text = ""
            End If
        End If
    Else
        Me.tbintmenprest.Text = ""
    End If
End Sub
Private Sub UserForm_Initialize()
    Dim nuevoIDAporte As String
    Dim nuevoIDPrestamo As String
    Dim nuevoIDPago As String
    Dim nuevoIDCaja As String
    
'    nuevoIDAporte = GenerarSiguienteIDAportes("")   ' Asegúrate de que esta función esté definida correctamente
    nuevoIDAporte = "AP" & Format(GenerarSiguienteIDAportes(""), "000")
    Me.tbsecap.Text = nuevoIDAporte
'    nuevoIDPrestamo = "PREST" & Format(GenerarSiguienteIDPrest(""), "000") ' Asegúrate de que esta función esté definida correctamente
'    Me.tbsecpret.Text = nuevoIDPrestamo
'    nuevoIDPago = "PAGOS" & Format(GenerarSiguienteIDPagos(""), "000") ' Asegúrate de que esta función esté definida correctamente
'    Me.tbsecpag.Text = nuevoIDPago
    nuevoIDCaja = "CAJA" & Format(GenerarSiguienteIDCaja(""), "000") ' Asegúrate de que esta función esté definida correctamente
    Me.tbsecaj.Text = nuevoIDCaja
    
    Call CargarPrestamosPendientes
    Call LimpiarCamposFormularioPagos
    Call LimpiarCamposFormularioPrestamo

    Dim ws As Worksheet
    Dim i As Long
    Dim celda As Range

    Set ws = Sheets("LINEAMIENTOS")

    ' Configura el ComboBox para tener 2 columnas
    Me.cbmsocap.ColumnCount = 2
    Me.cbmsocap.ColumnWidths = "150 pt;0 pt" ' Muestra solo el nombre, oculta el código

    For i = 5 To 20 ' Desde fila 3 hasta la 20
        If ws.Cells(i, 2).Value <> "" Then
            Me.cbmsocap.AddItem ws.Cells(i, 2).Value  ' Nombre en columna B
            Me.cbmsocap.List(Me.cbmsocap.ListCount - 1, 1) = ws.Cells(i, 1).Value  ' Código en columna A
        End If
    Next i
    
        ' Configura el ComboBox para tener 2 columnas
    Me.cbnomsocprest.ColumnCount = 2
    Me.cbnomsocprest.ColumnWidths = "150 pt;0 pt" ' Muestra solo el nombre, oculta el código

    For i = 5 To 20 ' Desde fila 3 hasta la 20
        If ws.Cells(i, 2).Value <> "" Then
            Me.cbnomsocprest.AddItem ws.Cells(i, 2).Value  ' Nombre en columna B
            Me.cbnomsocprest.List(Me.cbnomsocprest.ListCount - 1, 1) = ws.Cells(i, 1).Value  ' Código en columna A
        End If
    Next i
    
'    If obtnpag.Value = True Then
'    Set ws = Sheets("LINEAMIENTOS")
'
'    ' Configura el ComboBox para tener 2 columnas
'    Me.cmbnomsocpg.ColumnCount = 2
'    Me.cmbnomsocpg.ColumnWidths = "150 pt;0 pt" ' Muestra solo el nombre, oculta el código
'    Me.cmbnomsocpg.Clear ' ?? Limpia el ComboBox para evitar duplicados
'
'    For i = 5 To 20
''    If i <> 4 Then  ' Excluye la fila 4
'        If ws.Cells(i, 2).Value <> "" Then
'            Me.cmbnomsocpg.AddItem ws.Cells(i, 2).Value  ' Nombre en columna B
'    '            Me.cbmsocap.List(Me.cbmsocap.ListCount - 1, 1) = ws.Cells(i, 1).Value  ' Código en columna A
'        End If
''    End If
'    Next i
'    ElseIf obtning.Value = True Then
'        Set ws = Sheets("LINEAMIENTOS")
'    ' Configura el ComboBox para tener 2 columnas
''    Me.cmbnomsocpg.ColumnCount = 2
'    Me.cmbnomsocpg.ColumnWidths = "150 pt;0 pt" ' Muestra solo el nombre, oculta el código
'    Me.cmbnomsocpg.Clear ' ?? Limpia el ComboBox para evitar duplicados
'
'    For i = 3 To 20
'    If i <> 4 Then  ' Excluye la fila 4
'        If ws.Cells(i, 2).Value <> "" Then
'            Me.cmbnomsocpg.AddItem ws.Cells(i, 2).Value  ' Nombre en columna B
'    '            Me.cbmsocap.List(Me.cbmsocap.ListCount - 1, 1) = ws.Cells(i, 1).Value  ' Código en columna A
'        End If
'    End If
'    Next i
'    ElseIf obtnegre.Value = True Then
'        Set ws = Sheets("LINEAMIENTOS")
'
'    ' Configura el ComboBox para tener 2 columnas
'    Me.cmbnomsocpg.ColumnCount = 2
'    Me.cmbnomsocpg.ColumnWidths = "150 pt;0 pt" ' Muestra solo el nombre, oculta el código
'    Me.cmbnomsocpg.Clear
'
'    For i = 4 To 20
''    If i <> 4 Then  ' Excluye la fila 4
'        If ws.Cells(i, 2).Value <> "" Then
'            Me.cmbnomsocpg.AddItem ws.Cells(i, 2).Value  ' Nombre en columna B
'            Me.cbmsocap.List(Me.cbmsocap.ListCount - 1, 1) = ws.Cells(i, 1).Value  ' Código en columna A
'        End If
''    End If
'    Next i
'    Else
'
'    End If
    
'    Dim fechaFormateada As String
'    fechaFormateada = Format(CDate(Me.tbfechap.Text), "dd/mm/yyyy")
    Me.tbfechap.Text = Format(Date, "dd/mm/yyyy")
    Me.tbfechprest.Text = Format(Date, "dd/mm/yyyy")
    Me.tbfechpag1.Text = Format(Date, "dd/mm/yyyy")
    Me.tbfechpag2.Text = Format(Date, "dd/mm/yyyy")
    Me.tbfechcaj.Text = Format(Date, "dd/mm/yyyy")
End Sub
Private Sub cbmsocap_Change()
    Dim codigo As String
    If Me.cbmsocap.ListIndex >= 0 Then
        codigo = Me.cbmsocap.List(Me.cbmsocap.ListIndex, 1) ' Código en segunda columna
        Me.tbcodsocap.Text = codigo
    End If
End Sub
Private Sub cbnomsocprest_Change()
'        Dim codigo As String
'        If Me.cbnomsocprest.ListIndex >= 0 Then
'            codigo = Me.cbnomsocprest.List(Me.cbnomsocprest.ListIndex, 1) ' Código en segunda columna
'            Me.tbcodsocprest.Text = codigo
'        End If
    Dim codigo As String

    If Me.cbnomsocprest.ListIndex >= 0 Then
        codigo = Me.cbnomsocprest.List(Me.cbnomsocprest.ListIndex, 1) ' Código en segunda columna

        ' Si el botón "Ayuda" está activo, usar valor por defecto "2"
        If Me.obtnayud.Value = True Then
            Me.tbcodsocprest.Text = "2"
        Else
            Me.tbcodsocprest.Text = codigo
        End If
    End If
End Sub
Private Sub cmbnomsocpg_Change()
    Dim codigo As String

    If Me.obtning.Value = True Then
        Me.tbcodsocio.Text = 1
    Else
        If Me.cmbnomsocpg.ListIndex >= 0 Then
            If Not IsNull(Me.cmbnomsocpg.List(Me.cmbnomsocpg.ListIndex, 1)) Then
                Me.tbcodsocio.Text = Me.cmbnomsocpg.List(Me.cmbnomsocpg.ListIndex, 1)
            Else
                Me.tbcodsocio.Text = ""
            End If
        End If
    End If


'    Dim codigo As String
'    If Me.cmbnomsocpg.ListIndex >= 0 Then
'    If Not IsNull(Me.cmbnomsocpg.List(Me.cmbnomsocpg.ListIndex, 1)) Then
'            codigo = Me.cmbnomsocpg.List(Me.cmbnomsocpg.ListIndex, 1)
'            ' Validación según el OptionButton seleccionado
'            If Me.obtnpag.Value = True Then
'                If Me.cmbnomsocpg.ListIndex >= 0 Then
'                    If Not IsNull(Me.cmbnomsocpg.List(Me.cmbnomsocpg.ListIndex, 1)) Then
'                        Me.tbcodsocio.Text = Me.cmbnomsocpg.List(Me.cmbnomsocpg.ListIndex, 1)
'                    Else
'                        Me.tbcodsocio.Text = ""
'                    End If
'                End If
'            ElseIf Me.obtning.Value = True Then
'                    Me.tbcodsocio.Text = 1
'            ElseIf Me.obtnegre.Value = True Then
'                If Me.cmbnomsocpg.ListIndex >= 0 Then
'                    If Not IsNull(Me.cmbnomsocpg.List(Me.cmbnomsocpg.ListIndex, 1)) Then
'                        Me.tbcodsocio.Text = Me.cmbnomsocpg.List(Me.cmbnomsocpg.ListIndex, 1)
'                    Else
'                        Me.tbcodsocio.Text = ""
'                    End If
'                End If
'            Else
'                Me.obtnegre.Text = "Seleccione una opción"
'            End If
'
'        Else
'            Me.tbcodsocio.Text = "*"
'        End If
'    End If

End Sub
Private Sub cbsaveap_Click()
    Dim ws As Worksheet
    Dim tbl As ListObject
    Dim nuevaFila As ListRow

    ' Establecer la hoja y la tabla
    Set ws = ThisWorkbook.Sheets("SOCIOS")
    Set tbl = ws.ListObjects("APORTACIONESOCIOS")
    ws.Unprotect Password:="1998"

    ' Agregar nueva fila
    Set nuevaFila = tbl.ListRows.Add

    ' Asignar valores a cada columna
    With nuevaFila.Range
        .Cells(1, 1).Value = tbl.ListRows.Count ' SECUENCIA (número de fila como secuencia)
        Dim fechaFormateada As String
        fechaFormateada = Format(CDate(Me.tbfechap.Text), "mm/dd/yyyy")
        .Cells(1, 2).Value = fechaFormateada ' FECHA
        .Cells(1, 3).Value = tbmontap.Text ' APORTE
        .Cells(1, 4).Value = tbcodsocap.Text ' CODIGO CLIENTE
        .Cells(1, 5).Value = cbmsocap.Text ' SOCIO
        .Cells(1, 6).Value = tbcomap.Text ' COMENTARIO
    End With
    
    If Trim(tbfechap.Text) = "" Or _
    Trim(tbmontap.Text) = "" Or _
    Trim(cbmsocap.Text) = "" Or _
    Trim(tbcodsocap.Text) = "" Then

    MsgBox "Por favor, completa todos los campos antes de guardar.", vbExclamation
    Exit Sub
    End If
    
    ' Eliminar el ítem seleccionado del ComboBox
    Dim idx As Integer
    idx = Me.cbmsocap.ListIndex
    If idx >= 0 Then
        Me.cbmsocap.RemoveItem idx
    End If
     
    Call LimpiarCamposAportes
    
    MsgBox "¡Datos guardados correctamente!", vbInformation
    
    ' 3. Generar nuevo ID y mostrarlo
    Dim nuevoID As String
    nuevoID = GenerarSiguienteIDAportes("") ' Esta función debería calcular el nuevo consecutivo
    Me.tbsecap.Text = nuevoID
    ws.Protect Password:="1998"
    
End Sub
Private Sub cbsavecj_Click()
    Dim ws As Worksheet
    Dim tbl As ListObject
    Dim nuevaFila As ListRow

    ' Establecer la hoja y la tabla
    Set ws = ThisWorkbook.Sheets("CAJA")
    Set tbl = ws.ListObjects("DENERO_CAJA")
    ws.Unprotect Password:="1998"

    ' Agregar nueva fila
    Set nuevaFila = tbl.ListRows.Add

    ' Asignar valores a cada columna
    With nuevaFila.Range
        .Cells(1, 1).Value = tbl.ListRows.Count ' SECUENCIA (número de fila como secuencia)
        Dim fechaFormateada As String
        fechaFormateada = Format(CDate(Me.tbfechcaj.Text), "mm/dd/yyyy")
        .Cells(1, 2).Value = fechaFormateada ' FECHA
        .Cells(1, 3).Value = tbmontcj.Text ' APORTE
        .Cells(1, 4).Value = tbcmtcj.Text ' COMENTARIO
    End With
    
    If Trim(tbfechcaj.Text) = "" Or _
    Trim(tbmontcj.Text) = "" Or _
    Trim(tbcmtcj.Text) = "" Then

    MsgBox "Por favor, completa todos los campos antes de guardar.", vbExclamation
    Exit Sub
    End If
     
    Call LimpiarCamposCaja
    
    MsgBox "¡Datos guardados correctamente!", vbInformation
    
        ' 3. Generar nuevo ID y mostrarlo
    Dim nuevoID As String
    nuevoID = GenerarSiguienteIDCaja("") ' Esta función debería calcular el nuevo consecutivo
    Me.tbsecap.Text = nuevoID
    ws.Protect Password:="1998"
    
End Sub
Private Sub LimpiarCamposCaja()
        ' Desactivar el TextBox
    Me.tbsecaj.Enabled = False
    tbfechcaj.Text = Format(Date, "dd/mm/yyyy")
    tbmontcj.Text = ""
    tbcmtcj.Value = ""
    Dim nuevoID As String
    nuevoID = GenerarSiguienteIDCaja("") ' Esta función debería calcular el nuevo consecutivo
    Me.tbsecap.Text = nuevoID
End Sub
Private Sub LimpiarCamposAportes()
        ' Desactivar el TextBox
    Me.tbsecap.Enabled = False
    tbfechap.Text = Format(Date, "dd/mm/yyyy")
    tbmontap.Text = ""
    tbcodsocap.Text = ""
    cbmsocap.Text = ""
    tbcomap.Value = ""
    Dim nuevoID As String
    nuevoID = GenerarSiguienteIDAportes("") ' Esta función debería calcular el nuevo consecutivo
    Me.tbsecap.Text = nuevoID
End Sub
Private Sub LimpiarCamposPrestamos()
        ' Desactivar el TextBox
    Me.tbsecpret.Enabled = True
    Me.tbsecpret.Text = ""
    tbfechprest.Text = Format(Date, "dd/mm/yyyy")
    tbcodsocprest.Text = ""
    Me.tbcodsocprest.Enabled = False
    cbnomsocprest.Text = ""
    tbmontprest.Text = ""
    tbplzprest.Value = ""
    tbintrprest.Value = ""
    tbcomprest.Value = ""
    tbsecpret.Text = ""
    
    Me.obtnayud.Enabled = True
    Me.obtnprest.Enabled = True
    Me.obtnayud.Value = False
    Me.obtnprest.Value = True
    Me.tbsecpret.Enabled = False
    Me.tbfechprest.Enabled = True
    Me.tbcodsocprest.Enabled = False
    Me.cbnomsocprest.Enabled = True
    Me.tbmontprest.Enabled = True
    Me.tbplzprest.Enabled = True
    Me.tbintrprest.Enabled = True
    Me.tbcomprest.Enabled = True
    
'    Dim nuevoID As String
'    nuevoIDPrestamo = "PREST" & Format(GenerarSiguienteIDPrest(""), "000") ' Asegúrate de que esta función esté definida correctamente
'    Me.tbsecpret.Text = nuevoIDPrestamo
End Sub
Private Sub LimpiarCamposAyuda()
        ' Desactivar el TextBox
    Me.tbsecpret.Enabled = False
    tbfechprest.Text = Format(Date, "dd/mm/yyyy")
    tbfechprest.Enabled = True
    tbcodsocprest.Text = 2
    cbnomsocprest.Text = ""
    Me.tbcodsocprest.Enabled = False
    tbmontprest.Text = ""
    tbmontprest.Enabled = True
    tbplzprest.Value = 0
    tbintrprest.Value = 0
    tbcomprest.Value = ""
    tbcomprest.Enabled = True
    cbnomsocprest.Enabled = True
    Dim nuevoID As String
    nuevoIDPrestamo = GenerarSiguienteIDPrest("AYUDA")
    Me.tbsecpret.Text = nuevoIDPrestamo
End Sub
Private Sub LimpiarCamposPagos()
'    nuevoIDPago = "PAGOS" & Format(GenerarSiguienteIDPagos(""), "000") ' Asegúrate de que esta función esté definida correctamente
'    Me.tbsecpag.Text = nuevoIDPago
    cmbprestpendt.Text = ""
    tbcodsocio.Text = ""
    cmbnomsocpg.Text = ""
    tbmontprstpg.Text = ""
    tbfechpag1.Text = Format(Date, "dd/mm/yyyy")
    tbcuotpg.Text = ""
    tbintprestpg.Text = ""
    tbfechpag2.Text = Format(Date, "dd/mm/yyyy")
    tbintpg.Text = ""
    tbcomntpag.Text = ""
End Sub
Private Sub LimpiarCamposIngresos()
    nuevoIDPago = GenerarSiguienteIDPagos("INGRESO")
    Me.tbsecpag.Text = nuevoIDPago
    Me.cmbnomsocpg.Enabled = True
    cmbprestpendt.Text = ""
    tbcodsocio.Text = ""
    cmbnomsocpg.Text = ""
    tbmontprstpg.Text = ""
    tbfechpag1.Text = Format(Date, "dd/mm/yyyy")
    tbfechpag1.Enabled = True
    tbcuotpg.Text = ""
    tbintprestpg.Text = ""
    tbfechpag2.Text = Format(Date, "dd/mm/yyyy")
    tbfechpag2.Enabled = True
    tbintpg.Text = ""
    tbcomntpag.Text = ""
End Sub
Private Sub LimpiarCamposEgresos()
    nuevoIDPago = GenerarSiguienteIDPagos("EGRESO")
    Me.tbsecpag.Text = nuevoIDPago
    Me.cmbnomsocpg.Enabled = True
    cmbprestpendt.Text = 0
    tbcodsocio.Text = ""
    cmbnomsocpg.Text = ""
    tbmontprstpg.Text = ""
    tbfechpag1.Text = Format(Date, "dd/mm/yyyy")
    tbcuotpg.Text = 0
    tbintprestpg.Text = ""
    tbfechpag2.Text = Format(Date, "dd/mm/yyyy")
    tbintpg.Text = 0
    tbcomntpag.Text = ""
End Sub
Private Sub LimpiarCamposFormularioPagos()
'    nuevoIDPago = "PAGOS" & Format(GenerarSiguienteIDPagos(""), "000") ' Asegúrate de que esta función esté definida correctamente
'    Me.tbsecpag.Text = nuevoIDPago
    Me.cmbnomsocpg.Enabled = False
    tbsecpag.Text = ""
    cmbprestpendt.Text = ""
    tbcodsocio.Text = ""
    cmbnomsocpg.Text = ""
    tbmontprstpg.Text = ""
    tbfechpag1.Text = Format(Date, "dd/mm/yyyy")
    tbcuotpg.Text = ""
    tbintprestpg.Text = ""
    tbfechpag2.Text = Format(Date, "dd/mm/yyyy")
    tbintpg.Text = ""
    tbcomntpag.Text = ""
    tb1pg.Text = ""
    tb2pg.Text = ""
    tb3pg.Text = ""
    tb4pg.Text = ""
    tb5pg.Text = ""
    tb6pg.Text = ""
    
    Me.cbdatospestmo.Enabled = False
    Me.obtnpag.Enabled = True
    Me.obtning.Enabled = True
    Me.obtnegre.Enabled = True
    Me.obtnpag.Value = False
    Me.obtning.Value = False
    Me.obtnegre.Value = False
    
    Me.tbcomntpag.MultiLine = True
    Me.tbsecpag.Enabled = False
    Me.cmbprestpendt.Enabled = False
    Me.tbcodsocio.Enabled = False
    Me.cmbnomsocpg.Enabled = False
    Me.tbmontprstpg.Enabled = False
    Me.tbfechpag1.Enabled = False
    Me.tbcuotpg.Enabled = False
    Me.tbintprestpg.Enabled = False
    Me.tbfechpag2.Enabled = False
    Me.tbintpg.Enabled = False
    Me.tbcomntpag.Enabled = False
End Sub
Private Sub LimpiarCamposFormularioPrestamo()
    Me.tbsecpret.Enabled = False
    Me.tbsecpret.Text = ""
    tbfechprest.Text = Format(Date, "dd/mm/yyyy")
    tbcodsocprest.Text = ""
    Me.tbcodsocprest.Enabled = False
    cbnomsocprest.Text = ""
    tbmontprest.Text = ""
    tbplzprest.Value = ""
    tbintrprest.Value = ""
    tbcomprest.Value = ""
    tbsecpret.Text = ""
    
    Me.obtnayud.Enabled = True
    Me.obtnprest.Enabled = True
    Me.obtnayud.Value = False
    Me.obtnprest.Value = False
    
    Me.tbsecpret.Enabled = False
    Me.tbfechprest.Enabled = False
    Me.tbcodsocprest.Enabled = False
    Me.cbnomsocprest.Enabled = False
    Me.tbmontprest.Enabled = False
    Me.tbplzprest.Enabled = False
    Me.tbintrprest.Enabled = False
    Me.tbcomprest.Enabled = False
End Sub
Function ObtenerID(tbValor As String) As String
    If UCase(Left(tbValor, 5)) = "PAGOS" Then
        ' Extrae solo los números después de "PAGOS"
        Dim i As Integer
        Dim soloNumeros As String
        soloNumeros = ""
        
        For i = 6 To Len(tbValor)
            If Mid(tbValor, i, 1) Like "[0-9]" Then
                soloNumeros = soloNumeros & Mid(tbValor, i, 1)
            End If
        Next i
        
        ObtenerID = soloNumeros
    Else
        ' Devuelve el valor tal como está
        ObtenerID = tbValor
    End If
End Function
Function ObtenerID2(tbValor As String) As String
    If UCase(Left(tbValor, 5)) = "PREST" Then
        ' Extrae solo los números después de "PAGOS"
        Dim i As Integer
        Dim soloNumeros As String
        soloNumeros = ""
        
        For i = 6 To Len(tbValor)
            If Mid(tbValor, i, 1) Like "[0-9]" Then
                soloNumeros = soloNumeros & Mid(tbValor, i, 1)
            End If
        Next i
        
        ObtenerID2 = soloNumeros
    Else
        ' Devuelve el valor tal como está
        ObtenerID2 = tbValor
    End If
End Function
Function ObtenerID3(tbValor As String) As String
    If UCase(Left(tbValor, 5)) = "PREST" Then
        ' Extrae solo los números después de "PAGOS"
        Dim i As Integer
        Dim soloNumeros As String
        soloNumeros = ""
        
        For i = 6 To Len(tbValor)
            If Mid(tbValor, i, 1) Like "[0-9]" Then
                soloNumeros = soloNumeros & Mid(tbValor, i, 1)
            End If
        Next i
        
        ObtenerID3 = soloNumeros
    Else
        ' Devuelve el valor tal como está
        ObtenerID3 = tbValor
    End If
End Function
Sub GuardarRegistroPagos()
Dim ws As Worksheet
Dim tbl As ListObject
Dim nuevaFila As ListRow

    ' Establecer la hoja y la tabla
    Set ws = ThisWorkbook.Sheets("PAGOS")
    Set tbl = ws.ListObjects("PAGOS")
'    Sheets("PAGOS").Select
    ws.Unprotect Password:="1998"
If cmbprestpendt.Value = "" Or tbcodsocio.Value = "" Or cmbnomsocpg.Value = "" _
Or tbmontprstpg.Value = "" Or tbfechpag1.Value = "" Or tbcuotpg.Value = "" _
Or tbintprestpg.Value = "" Or tbfechpag2.Value = "" Or tbintpg.Value = "" Or tbcomntpag.Value = "" Then
    MsgBox "Llene todos los datos para guardar"
Else
        valorBuscado = ObtenerID(Me.tbsecpag.Value)
        valorBuscado2 = ObtenerID2(Me.cmbprestpendt.Value)
'        Set celda = Hoja17.Range("A:A").Find(What:=Me.tbsecpag.Value, _
'        After:=Hoja17.Range("A1"))
        Set celda = ws.Range("A:A").Find(What:=Me.tbsecpag.Value, After:=ws.Range("A3"))
'    Set celda = ws.Range("A:A").Find(What:=CStr(Me.tbsecpag.Value), _
'    LookIn:=xlValues, LookAt:=xlWhole, MatchCase:=False)
    If celda Is Nothing Then
        ' Agregar nueva fila
        Set nuevaFila = tbl.ListRows.Add
        ' Asignar valores a cada columna
        With nuevaFila.Range
            .Cells(1, 1).Value = valorBuscado ' SECUENCIA (número de fila como secuencia)
            Dim fechaFormateada1, fechaFormateada2 As String
            fechaFormateada1 = Format(CDate(Me.tbfechpag1.Text), "mm/dd/yyyy")
            fechaFormateada2 = Format(CDate(Me.tbfechpag2.Text), "mm/dd/yyyy")
            .Cells(1, 2).Value = tbcodsocio.Text
            .Cells(1, 3).Value = cmbnomsocpg.Text
            .Cells(1, 4).Value = valorBuscado2
            .Cells(1, 5).Value = tbmontprstpg.Text
            .Cells(1, 6).Value = fechaFormateada1
            .Cells(1, 7).Value = tbcuotpg.Text
            .Cells(1, 8).Value = tbintprestpg.Text
            .Cells(1, 9).Value = fechaFormateada2
            .Cells(1, 10).Value = tbintpg.Text
            .Cells(1, 11).Value = tbcomntpag.Text
        MsgBox "Registro guardado satisfactoriamente " & Me.tbsecpag.Value
        End With
    Else
        MsgBox "El codigo ya existe, por favor ingrese otro codigo " & Me.tbsecpag.Value
    End If
        Call LimpiarCamposFormularioPagos
End If
    Sheets("PAGOS").Select
    ActiveSheet.Protect Password:="1998"
    Sheets("ESTADO").Select
End Sub
Sub EditarRegistroPagosPorID(idBuscado As Variant)
    Dim ws As Worksheet
    Dim tbl As ListObject
    Dim fila As ListRow

    Set ws = ThisWorkbook.Sheets("PAGOS")
    Set tbl = ws.ListObjects("PAGOS")
    ws.Unprotect Password:="1998"
    Dim fechaFormateada1, fechaFormateada2 As String
    fechaFormateada1 = Format(CDate(Me.tbfechpag1.Text), "mm/dd/yyyy")
    fechaFormateada2 = Format(CDate(Me.tbfechpag2.Text), "mm/dd/yyyy")
    
    For Each fila In tbl.ListRows
        If fila.Range(1, 1).Value = idBuscado Then
            ' Actualiza los valores con los datos del formulario
            fila.Range(1, 2).Value = Me.tbcodsocio.Text
            fila.Range(1, 3).Value = Me.cmbnomsocpg.Text
            fila.Range(1, 4).Value = Me.cmbprestpendt.Text
            fila.Range(1, 5).Value = Me.tbmontprstpg.Text
            fila.Range(1, 6).Value = fechaFormateada1
            fila.Range(1, 7).Value = Me.tbcuotpg.Text
            fila.Range(1, 8).Value = Me.tbintprestpg.Text
            fila.Range(1, 9).Value = fechaFormateada2
            fila.Range(1, 10).Value = Me.tbintpg.Text
            fila.Range(1, 11).Value = Me.tbcomntpag.Text
            
            ws.Protect Password:="1998"
            MsgBox "Registro actualizado correctamente.", vbInformation
            Exit Sub
'        Else
'            MsgBox "No se encontró ningún registro con ese ID para editar." & idBuscado, vbExclamation
        End If
    Next fila

    MsgBox "No se encontró ningún registro con ese ID para editar." & idBuscado, vbExclamation
    Sheets("PAGOS").Select
    ws.Protect Password:="1998"
    Sheets("ESTADO").Select
End Sub
Sub GuardarRegistroPrestamos()
'Dim celda As Range
Dim ws, ws2 As Worksheet
Dim tbl As ListObject
Dim nuevaFila As ListRow
Dim hojaDestino As Worksheet
Dim fila As Long

    ' Establecer la hoja y la tabla
    Set ws = ThisWorkbook.Sheets("PRESTAMOS")
    Set ws2 = ThisWorkbook.Sheets("ESTADO")
    Set tbl = ws.ListObjects("PRESTAMOS")
    ws.Unprotect Password:="1998"
    
If tbsecpret.Value = "" Or tbfechprest.Value = "" Or tbcodsocprest.Value = "" _
Or tbcomprest.Value = "" Or cbnomsocprest.Value = "" Or tbmontprest.Value = "" _
Or tbplzprest.Value = "" Or tbintrprest.Value = "" Then
    MsgBox "Ingrese todos los datos para guardar"
    Exit Sub
'    End
Else
    valorBuscado = ObtenerID3(Me.tbsecpret.Value)
    Set celda = Hoja3.Range("A:A").Find(What:=Me.tbsecpret.Value, _
    After:=Hoja3.Range("A1"))
        If celda Is Nothing And (Val(tbcodsocprest.Text) = 2 Or Val(tbcodsocprest.Text) = 3 Or Val(tbcodsocprest.Text) = 4 Or Val(tbcodsocprest.Text) = 5 _
        Or Val(tbcodsocprest.Text) = 6 Or Val(tbcodsocprest.Text) = 7 Or Val(tbcodsocprest.Text) = 8 Or Val(tbcodsocprest.Text) = 9 Or Val(tbcodsocprest.Text) = 10 _
        Or Val(tbcodsocprest.Text) = 11 Or Val(tbcodsocprest.Text) = 12 Or Val(tbcodsocprest.Text) = 13 Or Val(tbcodsocprest.Text) = 14 Or Val(tbcodsocprest.Text) = 15 _
        Or Val(tbcodsocprest.Text) = 16 Or Val(tbcodsocprest.Text) = 17 Or Val(tbcodsocprest.Text) = 18) Then
        Dim monto, interes As Double
        Dim plazo As Integer
        Dim comentario, nombre As String
        monto = Val(Me.tbmontprest.Text)
        interes = Val(Me.tbintrprest.Text) / 100
        plazo = Val(Me.tbplzprest.Text)
        Dim fechaFormateada1 As String
        fechaFormateada1 = Format(CDate(Me.tbfechprest.Text), "mm/dd/yyyy")
        nombre = cbnomsocprest.Text
        comentario = tbcomprest.Text
        
            Select Case Val(tbcodsocprest.Text)
            Case 2, 3
                Set hojaDestino = Hoja5
            Case 2, 4
                Set hojaDestino = Hoja4
            Case 2, 5
                Set hojaDestino = Hoja6
            Case 2, 6
                Set hojaDestino = Hoja16
            Case 2, 7
                Set hojaDestino = Hoja7
            Case 2, 8
                Set hojaDestino = Hoja8
            Case 2, 9
                Set hojaDestino = Hoja9
            Case 2, 10
                Set hojaDestino = Hoja10
            Case 2, 11
                Set hojaDestino = Hoja11
            Case 2, 12
                Set hojaDestino = Hoja12
            Case 2, 13
                Set hojaDestino = Hoja14
            Case 2, 14
                Set hojaDestino = Hoja13
            Case 2, 15
                Set hojaDestino = Hoja14
            Case 2, 16
                Set hojaDestino = Hoja15
            Case 2, 17
                Set hojaDestino = Hoja22
            Case 2, 18
                Set hojaDestino = Hoja23
            Case Else
                MsgBox "Código de socio no reconocido", vbCritical
                Exit Sub
            End Select
    
            ' Agregar nueva fila a la tabla
            Set nuevaFila = tbl.ListRows.Add
            With nuevaFila.Range
                .Cells(1, 1).Value = valorBuscado
                .Cells(1, 2).Value = tbcodsocprest.Text
                .Cells(1, 3).Value = cbnomsocprest.Text
                .Cells(1, 4).Value = tbmontprest.Text
                .Cells(1, 5).Value = fechaFormateada1
                .Cells(1, 6).Value = tbplzprest.Text
                .Cells(1, 7).Value = interes
                .Cells(1, 10).Value = tbcomprest.Text
            End With
            
            If Not IsNumeric(valorBuscado) Then
                MsgBox "La ayuda al Socio ha sido registrada con éxito", vbExclamation
                Call LimpiarCamposFormularioPrestamo
                ws.Protect Password:="1998"
                ws2.Select
                Exit Sub
            End If
            
            ' Guardar en hoja correspondiente
            fila = hojaDestino.Cells(hojaDestino.Rows.Count, 1).End(xlUp).Row + 1
            hojaDestino.Cells(fila, 2).Value = "PREST" & valorBuscado
            hojaDestino.Cells(fila, 3).Value = cbnomsocprest.Text
            hojaDestino.Cells(fila, 4).Value = fechaFormateada1
            hojaDestino.Cells(fila, 5).Value = monto
            hojaDestino.Cells(fila, 6).Value = plazo
            hojaDestino.Cells(fila, 7).Value = monto / plazo
            hojaDestino.Cells(fila, 8).Value = monto * interes
            hojaDestino.Cells(fila, 9).Value = (monto / plazo) + (monto * interes)
            hojaDestino.Cells(fila, 10).Value = (monto / plazo) * plazo
            hojaDestino.Cells(fila, 11).Value = monto * interes * plazo
            hojaDestino.Cells(fila, 12).Value = monto + (monto * interes * plazo)
            
            MsgBox "Registro guardado satisfactoriamente"
    
    Else
        MsgBox "El codigo ya existe, por favor ingrese otro codigo"
        Exit Sub
'    End
    End If
End If

'CREAR LA TABLA DE AMORTIZACION
'Sheets("Tabla Amortización").Select
Sheets("Tabla Amortización").Visible = True
'COPIAR DATOS A LA MATRIS DE LA TABLA DE AMORTIZACION
    
    With Sheets("Tabla Amortización")
    .Visible = True
    .Range("C3").Value = fechaFormateada1
    .Range("C4").Value = nombre
    .Range("C6").Value = monto
    .Range("C7").Value = plazo
    .Range("E3").Value = interes
    .Range("C5").Value = valorBuscado
    .Range("C9").Value = comentario
    End With
    
    If nombre = "" Or comentario = "" Then
    MsgBox "Por favor completa todos los campos antes de continuar.", vbExclamation
    Exit Sub
    End If

    
Sheets("Tabla Amortización").Activate
Dim plazotabla As Integer
plazotabla = Range("c7")
If plazotabla = "1" Then
    Range("b12").Select
    Range(Selection, Selection.End(xlToRight)).Select
    Range(Selection, Selection.End(xlDown)).Select
    Selection.Clear
    Range("f12").Clear
    Hoja27.Range("B12").Value = "0"

    Range("F12").Select
    Application.CutCopyMode = False
    ActiveCell.FormulaR1C1 = "=R6C3"
    Range("E13").Select
    Application.CutCopyMode = False
    ActiveCell.FormulaR1C1 = "=R5C5"
    Range("D13").Select
    Application.CutCopyMode = False
    ActiveCell.FormulaR1C1 = "=R4C5"
    Range("C13").Select
    Application.CutCopyMode = False
    ActiveCell.FormulaR1C1 = "=RC[1]+RC[2]"
    Range("F13").Select
    Application.CutCopyMode = False
    ActiveCell.FormulaR1C1 = "=R[-1]C-RC[-1]"
    Range("F14").Select

For i = 1 To plazotabla
    Range("B12").Offset(i, 0).Value = i
Next i
Else
    plazotabla = Range("c7")
    Range("b12").Select
    Range(Selection, Selection.End(xlToRight)).Select
    Range(Selection, Selection.End(xlDown)).Select
    Selection.Clear
    Range("f12").Clear
    Hoja27.Range("B12").Value = "0"

    Range("F12").Select
    Application.CutCopyMode = False
    ActiveCell.FormulaR1C1 = "=R6C3"
    Range("E13").Select
    Application.CutCopyMode = False
    ActiveCell.FormulaR1C1 = "=R5C5"
    Range("D13").Select
    Application.CutCopyMode = False
    ActiveCell.FormulaR1C1 = "=R4C5"
    Range("C13").Select
    Application.CutCopyMode = False
    ActiveCell.FormulaR1C1 = "=RC[1]+RC[2]"
    Range("F13").Select
    Application.CutCopyMode = False
    ActiveCell.FormulaR1C1 = "=R[-1]C-RC[-1]"
    Range("F14").Select
    Range("C14:F14").Select
    Selection.FillDown
    Range("C15").Select

For i = 1 To plazotabla
    Range("B12").Offset(i, 0).Value = i
    Range("c14:f14").Copy Range("c15:f15").Offset(i - 3, 0)
Next i
End If
'PARA PONER BORDE A LA TABLA
    Range("F12").Select
    Range(Selection, Selection.End(xlToLeft)).Select
    Range(Selection, Selection.End(xlDown)).Select
    Selection.Borders(xlDiagonalDown).LineStyle = xlNone
    Selection.Borders(xlDiagonalUp).LineStyle = xlNone
    With Selection.Borders(xlEdgeLeft)
        .LineStyle = xlContinuous
        .ColorIndex = 0
        .TintAndShade = 0
        .Weight = xlThin
    End With
    With Selection.Borders(xlEdgeTop)
        .LineStyle = xlContinuous
        .ColorIndex = 0
        .TintAndShade = 0
        .Weight = xlThin
    End With
    With Selection.Borders(xlEdgeBottom)
        .LineStyle = xlContinuous
        .ColorIndex = 0
        .TintAndShade = 0
        .Weight = xlThin
    End With
    With Selection.Borders(xlEdgeRight)
        .LineStyle = xlContinuous
        .ColorIndex = 0
        .TintAndShade = 0
        .Weight = xlThin
    End With
    With Selection.Borders(xlInsideVertical)
        .LineStyle = xlContinuous
        .ColorIndex = 0
        .TintAndShade = 0
        .Weight = xlThin
    End With
    With Selection.Borders(xlInsideHorizontal)
        .LineStyle = xlContinuous
        .ColorIndex = 0
        .TintAndShade = 0
        .Weight = xlThin
    End With
    Range("B8").Select

'PARA PONER LAS FIRMAS PARA LA TABLA
    fila = Hoja27.Cells(1048576, 3).End(xlUp).Row + 6
    Hoja27.Cells(fila, 3).Value = "______________"
    Hoja27.Cells(fila, 5).Value = "______________"
    fila = Hoja27.Cells(1048576, 3).End(xlUp).Row + 1
    Hoja27.Cells(fila, 3).Value = "FIRMA TESORERO"
    Hoja27.Cells(fila, 5).Value = "FIRMA SOCIO"

Sheets("Tabla Amortización").Select
Dim nump, nombreT As String
Dim rutas() As String
Dim rutaValida As String
Dim ii As Integer
nombreT = Range("c4").Value
nump = Range("c5").Value
'    "C:\Users\ADMIN\Documents\TRABAJO\PRUEBAS"
'    "C:\Users\israe\Documents\COOPERATIVA\PRESTAMOS"
'    ChDir "C:\Users\ADMIN\Documents\TRABAJO\PRUEBAS"
'    ActiveSheet.ExportAsFixedFormat Type:=xlTypePDF, Filename:= _
'        "C:\Users\ADMIN\Documents\TRABAJO\PRUEBAS\" & " " & nombreT & " PREST#" & nump & ".pdf", Quality:=xlQualityStandard, _
'        IncludeDocProperties:=True, IgnorePrintAreas:=False, OpenAfterPublish:= _
'        True
        
        ' Lista de rutas posibles
    rutas = Split("C:\Users\ADMIN\Documents\TRABAJO\PRUEBAS|C:\Users\israe\Documents\COOPERATIVA\PRESTAMOS", "|")
    
    ' Buscar la primera ruta válida
    rutaValida = ""
    For ii = LBound(rutas) To UBound(rutas)
        If Dir(rutas(ii), vbDirectory) <> "" Then
            rutaValida = rutas(ii)
            Exit For
        End If
    Next ii
    
    ' Validar si se encontró una ruta válida
    If rutaValida <> "" Then
        ChDir rutaValida
        ActiveSheet.ExportAsFixedFormat Type:=xlTypePDF, Filename:= _
            rutaValida & "\" & " " & nombreT & " PREST#" & nump & ".pdf", Quality:=xlQualityStandard, _
            IncludeDocProperties:=True, IgnorePrintAreas:=False, OpenAfterPublish:=True
    Else
        MsgBox "Ninguna de las rutas especificadas existe.", vbCritical, "Error de ruta"
    End If

    Range("b12").Select
    Range(Selection, Selection.End(xlToRight)).Select
    Range(Selection, Selection.End(xlDown)).Select
    Selection.Clear
    Range("f12").Clear
    Range("F12").Select

'OCULTAR TABLA DE AMORTIZACION
Sheets("Tabla Amortización").Select
ActiveWindow.SelectedSheets.Visible = False

'    Sheets("PRESTAMOS").Select
    ws.Protect Password:="1998"
'    ActiveSheet.Protect Password:="1998"
    ws2.Select
    Call LimpiarCamposFormularioPrestamo
End Sub
Sub EditarRegistroPrestamosPorID(idBuscado As Variant)
    Dim ws As Worksheet
    Dim tbl As ListObject
    Dim fila As ListRow

    Set ws = ThisWorkbook.Sheets("PRESTAMOS")
    Set tbl = ws.ListObjects("PRESTAMOS")
    ws.Unprotect Password:="1998"
    valorBuscado = ObtenerID3(Me.tbsecpret.Value)
    
    Dim monto, interes As Double
        Dim plazo As Integer
        Dim comentario, nombre As String
        monto = Val(Me.tbmontprest.Text)
        interes = Val(Me.tbintrprest.Text) / 100
        plazo = Val(Me.tbplzprest.Text)
        Dim fechaFormateada1 As String
        fechaFormateada1 = Format(CDate(Me.tbfechprest.Text), "mm/dd/yyyy")
        nombre = cbnomsocprest.Text
        comentario = tbcomprest.Text
    
    For Each fila In tbl.ListRows
        If fila.Range(1, 1).Value = idBuscado Then
            ' Actualiza los valores con los datos del formulario
            fila.Range(1, 2).Value = tbcodsocprest.Text
            fila.Range(1, 3).Value = cbnomsocprest.Text
            fila.Range(1, 4).Value = tbmontprest.Text
            fila.Range(1, 5).Value = fechaFormateada1
            fila.Range(1, 6).Value = tbplzprest.Text
            fila.Range(1, 7).Value = interes
            fila.Range(1, 10).Value = tbcomprest.Text
            
            ws.Protect Password:="1998"
            MsgBox "Registro actualizado correctamente.", vbInformation
            Exit Sub
'        Else
'            MsgBox "No se encontró ningún registro con ese ID para editar." & idBuscado, vbExclamation
        End If
    Next fila
    
    If Not IsNumeric(valorBuscado) Then
    MsgBox "La ayuda al Socio ha sido registrada con éxito", vbExclamation
    Call LimpiarCamposFormularioPagos
    ws.Protect Password:="1998"
'    ws.Select
    Exit Sub
    End If

    MsgBox "No se encontró ningún registro con ese ID para editar." & idBuscado, vbExclamation
    Sheets("PRESTAMOS").Select
    ws.Protect Password:="1998"
    Sheets("ESTADO").Select
End Sub

Private Sub cblimpap_Click()
    Call LimpiarCamposAportes
End Sub
Private Sub cblimppr_Click()
    Call LimpiarCamposFormularioPrestamo
End Sub
Private Sub cbbuscap_Click()
    Me.tbsecap.Enabled = True
    Me.tbsecap.SetFocus ' Mueve el cursor al textbox para que el usuario escriba
End Sub
Private Sub tbsecap_KeyDown(ByVal KeyCode As MSForms.ReturnInteger, ByVal Shift As Integer)
    If KeyCode = vbKeyReturn Then
        If IsNumeric(Me.tbsecap.Text) Then
            Call BuscarRegistroAportePorID(CLng(Me.tbsecap.Text))
        Else
            MsgBox "Por favor ingresa un número válido.", vbExclamation
        End If
    End If
End Sub

Sub BuscarRegistroAportePorID(idBuscado As Long)
    Dim ws As Worksheet
    Dim tbl As ListObject
    Dim fila As ListRow

    Set ws = ThisWorkbook.Sheets("SOCIOS")
    Set tbl = ws.ListObjects("APORTACIONESOCIOS")

    For Each fila In tbl.ListRows
        If IsNumeric(fila.Range(1, 1).Value) Then
              If CLng(fila.Range(1, 1).Value) = idBuscado Then
                Me.tbfechap.Text = fila.Range(1, 2).Value
                Me.tbmontap.Text = fila.Range(1, 3).Value
                Me.tbcodsocap.Text = fila.Range(1, 4).Value
                Me.cbmsocap.Text = fila.Range(1, 5).Value
                Me.tbcomap.Text = fila.Range(1, 6).Value
                Exit Sub
            End If
        End If
    Next fila
'    Me.tbmontap.Text = ws.Cells(fila, 3).Value
'    ResaltarCampo Me.tbmontap
    MsgBox "No se encontró ningún registro con ese ID numérico.", vbExclamation
End Sub
Sub BuscarRegistroSPrestamosPorID(idBuscado As Variant)
    Dim ws As Worksheet
    Dim tbl As ListObject
    Dim fila As ListRow

    Set ws = ThisWorkbook.Sheets("PRESTAMOS")
    Set tbl = ws.ListObjects("PRESTAMOS")

    For Each fila In tbl.ListRows
'        If IsNumeric(fila.Range(1, 1).Value) Then
            If fila.Range(1, 1).Value = idBuscado Then
'            If CLng(fila.Range(1, 1).Value) = idBuscado Then
                Me.tbcodsocprest.Text = fila.Range(1, 2).Value
                Me.cbnomsocprest = fila.Range(1, 3).Value
                Me.tbmontprest.Text = fila.Range(1, 4).Value
                Me.tbfechprest.Text = fila.Range(1, 5).Value
                Me.tbplzprest.Text = fila.Range(1, 6).Value
                Me.tbintrprest.Text = fila.Range(1, 7).Value * 100
                Me.tbcomprest.Text = fila.Range(1, 10).Value
                
                Me.obtnayud.Enabled = False
                Me.obtnprest.Enabled = False
'                Me.obtnayud.Value = False
'                Me.obtnprest.Value = False
                Me.tbcomprest.MultiLine = True
                Me.tbsecpret.Enabled = False
                Me.tbfechprest.Enabled = True
                Me.tbcodsocprest.Enabled = False
                Me.cbnomsocprest.Enabled = False
                Me.tbmontprest.Enabled = True
                Me.tbplzprest.Enabled = True
                Me.tbintrprest.Enabled = True
                Me.tbcomprest.Enabled = True
                
                Exit Sub
'            Else
'                MsgBox "No se encontró ningún registro con ese ID numérico limpie los campos para volver a buscar. " & idBuscado, vbExclamation
'                Call LimpiarCamposFormularioPrestamo
'                Exit Sub
            End If
'        End If
    Next fila
'    Me.tbmontap.Text = ws.Cells(fila, 3).Value
'    ResaltarCampo Me.tbmontap
    MsgBox "No se encontró ningún registro con ese ID Prestamo.", vbExclamation
    Call LimpiarCamposFormularioPrestamo
'    Me.tbsecpret.Enabled = True
'    Me.tbsecpret.Value = ""
'    Me.obtnprest.Enabled = True
'    Me.obtnayud.Enabled = True
'    Me.obtnprest.Value = True
'    Me.obtnayud.Value = True
End Sub
Sub BuscarRegistroPagosPorID(idBuscado As Variant)
    Dim ws As Worksheet
    Dim tbl As ListObject
    Dim fila As ListRow

    Set ws = ThisWorkbook.Sheets("PAGOS")
    Set tbl = ws.ListObjects("PAGOS")

    For Each fila In tbl.ListRows
'        If IsNumeric(fila.Range(1, 1).Value) Then
            If fila.Range(1, 1).Value = idBuscado Then
'              If CLng(fila.Range(1, 1).Value) = idBuscado Then
                Me.tbcodsocio.Text = fila.Range(1, 2).Value
                Me.cmbnomsocpg = fila.Range(1, 3).Value
                Me.cmbprestpendt.Text = fila.Range(1, 4).Value
                Me.tbmontprstpg.Text = Replace(Format(fila.Range(1, 5).Value, "0.00"), ",", ".")
                Me.tbfechpag1.Text = fila.Range(1, 6).Value
                Me.tbcuotpg.Text = fila.Range(1, 7).Value
                Me.tbintprestpg.Text = Replace(Format(fila.Range(1, 8).Value, "0.00"), ",", ".")
'                Me.tbintprestpg.Text = fila.Range(1, 8).Value
                Me.tbfechpag2.Text = fila.Range(1, 9).Value
                Me.tbintpg.Text = fila.Range(1, 10).Value
                Me.tbcomntpag.Text = fila.Range(1, 11).Value
                
'            Me.obtning.Value = True
'            Me.obtnegre.Value = True
                Me.cbdatospestmo.Enabled = False
                Me.obtnpag.Enabled = False
                Me.obtning.Enabled = False
                Me.obtnegre.Enabled = False
'                Me.tbcomprest.MultiLine = True
                Me.tbcomntpag.MultiLine = True
                Me.tbsecpag.Enabled = False
                Me.cmbprestpendt.Enabled = False
                Me.tbcodsocio.Enabled = False
                Me.cmbnomsocpg.Enabled = False
                Me.tbmontprstpg.Enabled = True
                Me.tbfechpag1.Enabled = True
                Me.tbcuotpg.Enabled = True
                Me.tbintprestpg.Enabled = True
                Me.tbfechpag2.Enabled = True
                Me.tbintpg.Enabled = True
                Me.tbcomntpag.Enabled = True
                
                Exit Sub
'            Else
'                MsgBox "No se encontró ningún registro con ese ID numérico limpie los campos para volver a buacar.", vbExclamation
'                Call LimpiarCamposFormularioPagos
'                Exit Sub
            End If
    Next fila
    
    MsgBox "No se encontró ningún registro con ese ID.", vbExclamation
    Call LimpiarCamposFormularioPagos
   
End Sub
Sub BuscarRegistroPrestamoPorID(idBuscado As Variant)
'Sub BuscarRegistroPrestamoPorID(idBuscado As Long)
    Dim ws As Worksheet
    Dim tbl As ListObject
    Dim fila As ListRow
    Dim fechaInicio As Date
    Dim fechaFin As Date
    Dim maxMeses As Long
    Dim mesesCalculados As Long
    Dim resultadoFinal As Long

    Set ws = ThisWorkbook.Sheets("PRESTAMOS")
    Set tbl = ws.ListObjects("PRESTAMOS")

    For Each fila In tbl.ListRows
        If IsNumeric(fila.Range(1, 1).Value) Then
'              If fila.Range(1, 1).Value = idBuscado Then
              If CLng(fila.Range(1, 1).Value) = idBuscado Then
                Me.tbcodsocio.Text = fila.Range(1, 2).Value
                Me.cmbnomsocpg.Text = fila.Range(1, 3).Value
                Me.tbmontprstpg.Text = Replace(Round(fila.Range(1, 4).Value / fila.Range(1, 6).Value, 2), ",", ".")
                Me.tbintprestpg.Text = Replace((fila.Range(1, 4).Value * fila.Range(1, 7).Value), ",", ".")
                Me.tb1pg.Text = fila.Range(1, 5).Value
                Me.tb2pg.Text = fila.Range(1, 6).Value
'                Me.tb3pg.Text = fila.Range(1, 3).Value
'                Me.tb4pg.Text = fila.Range(1, 3).Value
                Me.tb5pg.Text = Round(fila.Range(1, 4).Value / fila.Range(1, 6).Value + fila.Range(1, 4).Value * fila.Range(1, 7).Value, 2)
                Me.tb6pg.Text = fila.Range(1, 9).Value
                
                'Cálculo de LETRA NUMERO
                If Me.tbfechpag1.Value <> "" And Me.tb1pg.Value <> "" Then
                    fechaFin = CDate(Me.tbfechpag1.Value)
                    fechaInicio = CDate(Me.tb1pg.Value)
                    maxMeses = CLng(Me.tb2pg.Value)

                    mesesCalculados = DateDiff("m", fechaInicio, fechaFin)
                    If Day(fechaFin) < Day(fechaInicio) Then
                        mesesCalculados = mesesCalculados - 1
                    End If

                    resultadoFinal = WorksheetFunction.Min(mesesCalculados, maxMeses)
                    resultadoFinal = WorksheetFunction.Max(resultadoFinal, 0)

                    Me.tb3pg.Text = resultadoFinal
                    Me.tbcuotpg.Text = resultadoFinal
                    Me.tbintpg.Text = resultadoFinal
                Else
                    Me.tb3pg.Text = ""
                End If
                
                ' Validar que todos los campos tienen contenido
                If Me.tbfechpag1.Value <> "" And Me.tb1pg.Value <> "" And Me.tb2pg.Value <> "" Then
                    fechaFin = CDate(Me.tbfechpag1.Value)
                    fechaInicio = CDate(Me.tb1pg.Value)
                    maxMeses = CLng(Me.tb2pg.Value)
                
                    ' Calcular meses transcurridos
                    letrasPagadas = DateDiff("m", fechaInicio, fechaFin)
                    If Day(fechaFin) < Day(fechaInicio) Then
                        letrasPagadas = letrasPagadas - 1
                    End If
                
                    letrasPagadas = WorksheetFunction.Min(letrasPagadas, maxMeses)
                    letrasPagadas = WorksheetFunction.Max(letrasPagadas, 0)
                
                    ' Calcular letras faltantes
                    letrasFaltantes = maxMeses - letrasPagadas
                
                    Me.tb4pg.Text = letrasFaltantes
                Else
                    Me.tb4pg.Text = ""
                End If
                
                Exit Sub
            End If
        End If
    Next fila
End Sub
Sub ResaltarCampo(ctrl As Control)
    ctrl.BackColor = RGB(255, 255, 200) ' Color suave
    DoEvents
    Application.Wait Now + TimeValue("0:00:01")
    ctrl.BackColor = vbWhite ' Vuelve al color original
End Sub
Sub EliminarRegistroAportePorID(idBuscado As Long)
    Dim ws As Worksheet
    Dim tbl As ListObject
    Dim fila As ListRow

    Set ws = ThisWorkbook.Sheets("SOCIOS")
    Set tbl = ws.ListObjects("APORTACIONESOCIOS")
    ws.Unprotect Password:="1998"

    For Each fila In tbl.ListRows
        If IsNumeric(fila.Range(1, 1).Value) Then
            If CLng(fila.Range(1, 1).Value) = idBuscado Then
                fila.Delete
                Exit Sub
            End If
        End If
    Next fila
    ws.Protect Password:="1998"
End Sub
Sub EliminarRegistroPrestamoPorID(idBuscado As Variant)
    Dim ws As Worksheet
    Dim tbl As ListObject
    Dim fila As ListRow

    Set ws = ThisWorkbook.Sheets("PRESTAMOS")
    Set tbl = ws.ListObjects("PRESTAMOS")
    ws.Unprotect Password:="1998"

    For Each fila In tbl.ListRows
'        If fila.Range(1, 1).Value Then
'        If IsNumeric(fila.Range(1, 1).Value) Then
            If fila.Range(1, 1).Value = idBuscado Then
'            If CLng(fila.Range(1, 1).Value) = idBuscado Then
                fila.Delete
                Exit Sub
            End If
'        End If
    Next fila
    ws.Protect Password:="1998"
End Sub
Sub EliminarRegistroPagosPorID(idBuscado As Variant)
    Dim ws As Worksheet
    Dim tbl As ListObject
    Dim fila As ListRow

    Set ws = ThisWorkbook.Sheets("PAGOS")
    Set tbl = ws.ListObjects("PAGOS")
    ws.Unprotect Password:="1998"

    For Each fila In tbl.ListRows
'        If IsNumeric(fila.Range(1, 1).Value) Then
            If fila.Range(1, 1).Value = idBuscado Then
'            If CLng(fila.Range(1, 1).Value) = idBuscado Then
                fila.Delete
                Exit Sub
            End If
'        End If
    Next fila
    ws.Protect Password:="1998"
End Sub
Private Sub cbelimap_Click()
    If IsNumeric(Me.tbsecap.Text) Then
        Dim respuesta As VbMsgBoxResult
        respuesta = MsgBox("¿Estás seguro de que deseas eliminar este registro?", vbYesNo + vbQuestion, "Confirmar eliminación")

        If respuesta = vbYes Then
            Call EliminarRegistroAportePorID(CLng(Me.tbsecap.Text))
            MsgBox "Registro eliminado (si existía).", vbInformation
            Call LimpiarCamposAportes
            Me.tbsecap.Text = GenerarSiguienteIDAportes("")
        Else
            MsgBox "Eliminación cancelada.", vbInformation
        End If
    Else
        MsgBox "Por favor ingresa un ID numérico válido.", vbExclamation
    End If
End Sub
Private Sub cbdeltepr_Click()
valorBuscado = ObtenerID2(Me.tbsecpret.Value)
'    If IsNumeric(Me.tbsecpret.Text) Then

If tbsecpret.Value = "" Or tbfechprest.Value = "" Or tbcodsocprest.Value = "" _
Or tbcomprest.Value = "" Or cbnomsocprest.Value = "" Or tbmontprest.Value = "" _
Or tbplzprest.Value = "" Or tbintrprest.Value = "" Then
    MsgBox "Busque el id_prestamo para eliminar"
    Exit Sub
'    End
Else

'    If IsNumeric(valorBuscado) Then
'    If IsNumeric(valorBuscado) Then
    
        Dim respuesta As VbMsgBoxResult
        respuesta = MsgBox("¿Estás seguro de que deseas eliminar este registro?", vbYesNo + vbQuestion, "Confirmar eliminación")

        If respuesta = vbYes Then
                Dim clave As String
                clave = InputBox("Ingresa la contraseña para eliminar el registro:", "Autenticación")
        
                If clave <> "1998" Then
                    MsgBox "Contraseña incorrecta. No se puede eliminar el registro.", vbCritical
                    Exit Sub
                End If
            If IsNumeric(valorBuscado) Then
'            Call EliminarRegistroPrestamoPorID(CLng(Me.tbsecpret.Text))
'            Call EliminarRegistroPrestamoPorID(valorBuscado)
            Call EliminarRegistroPrestamoPorID(CLng(valorBuscado))
            MsgBox "Registro eliminado (si existía).", vbInformation
            Call LimpiarCamposFormularioPrestamo
'            Me.tbsecpret.Text = GenerarSiguienteIDPrest("")
            Else
            'Call EliminarRegistroPrestamoPorID(CLng(Me.tbsecpret.Text))
            Call EliminarRegistroPrestamoPorID(valorBuscado)
'            Call EliminarRegistroPrestamoPorID(CLng(valorBuscado))
            MsgBox "Registro eliminado (si existía).", vbInformation
            Call LimpiarCamposFormularioPrestamo
'            Me.tbsecpret.Text = GenerarSiguienteIDPrest("")
            End If
        Else
            MsgBox "Eliminación cancelada.", vbInformation
        End If
'    Else
'        MsgBox "Por favor ingresa un ID numérico válido.", vbExclamation
'    End If
End If
End Sub
Private Sub cbdeletepag_Click()
    valorBuscado = ObtenerID(Me.tbsecpag.Value)

If tbsecpag.Value = "" Or cmbprestpendt.Value = "" Or tbcodsocio.Value = "" _
Or cmbnomsocpg.Value = "" Or tbmontprstpg.Value = "" Or tbfechpag1.Value = "" _
Or tbcuotpg.Value = "" Or tbintprestpg.Value = "" Or tbfechpag2.Value = "" _
Or tbintpg.Value = "" Or tbcomntpag.Value = "" Then
    MsgBox "Busque el id_prestamo para eliminar"
    Exit Sub
Else
        Dim respuesta As VbMsgBoxResult
        respuesta = MsgBox("¿Estás seguro de que deseas eliminar este registro?", vbYesNo + vbQuestion, "Confirmar eliminación")

        If respuesta = vbYes Then
                Dim clave As String
                clave = InputBox("Ingresa la contraseña para eliminar el registro:", "Autenticación")
        
                If clave <> "1998" Then
                    MsgBox "Contraseña incorrecta. No se puede eliminar el registro.", vbCritical
                    Exit Sub
                End If
            If IsNumeric(valorBuscado) Then
            Call EliminarRegistroPagosPorID(CLng(valorBuscado))
            MsgBox "Registro eliminado (si existía).", vbInformation
            Call LimpiarCamposFormularioPagos
'            Me.tbsecpag.Text = GenerarSiguienteIDPrest("")
            Else
            Call EliminarRegistroPagosPorID(valorBuscado)
            MsgBox "Registro eliminado (si existía).", vbInformation
            Call LimpiarCamposFormularioPagos
'            Me.tbsecpag.Text = GenerarSiguienteIDPrest("")
            End If
        Else
            MsgBox "Eliminación cancelada.", vbInformation
        End If
End If
End Sub

Private Sub tbsecpret_KeyDown(ByVal KeyCode As MSForms.ReturnInteger, ByVal Shift As Integer)

    Dim prefijo As String
    prefijo = ""

    ' Detectar cuál prefijo está presente
    If Left(Me.tbsecpret.Value, 5) = "PREST" Then
        prefijo = "PREST"
    ElseIf Left(Me.tbsecpret.Value, 5) = "AYUDA" Then
        prefijo = "AYUDA"
    End If

    ' Bloquear retroceso si el cursor está dentro del prefijo
    If KeyCode = vbKeyBack Then
        If Me.tbsecpret.SelStart <= Len(prefijo) Then
            KeyCode = 0
        End If
    End If

    valorBuscado = ObtenerID2(Me.tbsecpret.Value)
    
    If KeyCode = vbKeyReturn Then
        If IsNumeric(valorBuscado) Then
'            Call BuscarRegistroPrestamosPorID(CLng(Me.tbsecpret.Text))
'            Me.obtnayud.Value = False
            Call BuscarRegistroSPrestamosPorID(CLng(valorBuscado))
'            Call BuscarRegistroSPrestamosPorID(valorBuscado)
'            Me.tbcomprest.MultiLine = True
'            Me.obtnayud.Enabled = False
'            Me.obtnprest.Enabled = False
'            Me.tbsecpret.Enabled = False
'            Me.tbfechprest.Enabled = False
'            Me.tbcodsocprest.Enabled = False
'            Me.cbnomsocprest.Enabled = False
'            Me.tbmontprest.Enabled = False
'            Me.tbplzprest.Enabled = False
'            Me.tbintrprest.Enabled = False
'            Me.tbcomprest.Enabled = False
'            Exit Sub
        Else
'            Me.obtnayud.Value = True
'            Me.tbcomprest.MultiLine = True
'            Me.obtnayud.Enabled = False
'            Me.obtnprest.Enabled = False
            Call BuscarRegistroSPrestamosPorID(valorBuscado)
'            Me.tbsecpret.Enabled = False
'            Me.tbfechprest.Enabled = False
'            Me.tbcodsocprest.Enabled = False
'            Me.cbnomsocprest.Enabled = False
'            Me.tbmontprest.Enabled = False
'            Me.tbplzprest.Enabled = False
'            Me.tbintrprest.Enabled = False
'            Me.tbcomprest.Enabled = False
'            Exit Sub
'            MsgBox "Por favor ingresa un número válido.", vbExclamation
        End If
    End If
End Sub
Private Sub tbsecpag_KeyDown(ByVal KeyCode As MSForms.ReturnInteger, ByVal Shift As Integer)

    Dim prefijo As String
    prefijo = ""

    ' Detectar cuál prefijo está presente
    If Left(Me.tbsecpag.Value, 5) = "PAGOS" Then
        prefijo = "PAGOS"
    ElseIf Left(Me.tbsecpag.Value, 7) = "INGRESO" Then
        prefijo = "INGRESO"
    ElseIf Left(Me.tbsecpag.Value, 6) = "EGRESO" Then
        prefijo = "EGRESO"
    End If

    ' Bloquear retroceso si el cursor está dentro del prefijo
    If KeyCode = vbKeyBack Then
        If Me.tbsecpag.SelStart <= Len(prefijo) Then
            KeyCode = 0
        End If
    End If

    ' Bloquear suprimir si el cursor está sobre el prefijo
    If KeyCode = vbKeyDelete Then
        If Me.tbsecpag.SelStart < Len(prefijo) Then
            KeyCode = 0
        End If
    End If

    valorBuscado = ObtenerID(Me.tbsecpag.Value)
    If KeyCode = vbKeyReturn Then
        If IsNumeric(valorBuscado) Then
            Call BuscarRegistroPagosPorID(CLng(valorBuscado))
        Else
            Call BuscarRegistroPagosPorID(valorBuscado)
        End If
    End If
End Sub

Private Sub cbbuscpr_Click()

'Me.obtnayud.Enabled = True
'Me.obtnprest.Enabled = True
'Me.tbsecpret.Text = "PREST"

If Not (obtnayud.Value Or obtnprest.Value) Then
    MsgBox "Elije una opción de prestamo para buscar", vbExclamation, "Advertencia"
    Exit Sub
End If

If Me.obtnayud.Value = True Then
    Me.tbsecpret.Enabled = True
    Me.tbsecpret.SetFocus ' Mueve el cursor al textbox para que el usuario escriba
    
'    If tbcodsocprest.Value <> "" _
'    Or tbcomprest.Value <> "" Or cbnomsocprest.Value <> "" Or tbmontprest.Value <> "" _
'    Or tbplzprest.Value <> "" Or tbintrprest.Value <> "" Then
'        MsgBox "Limpie todos los campos para buscar"
'        tbsecpret.Enabled = False
'        Exit Sub
'    '    End
'    Else
'        Me.tbsecpret.Enabled = True
'        Me.tbsecpret.SetFocus ' Mueve el cursor al textbox para que el usuario escriba
'    End If
    
ElseIf Me.obtnprest.Value = True Then
    Me.tbsecpret.Enabled = True
    Me.tbsecpret.SetFocus ' Mueve el cursor al textbox para que el usuario escriba
    
    If tbcodsocprest.Value <> "" _
    Or tbcomprest.Value <> "" Or cbnomsocprest.Value <> "" Or tbmontprest.Value <> "" _
    Or tbplzprest.Value <> "" Or tbintrprest.Value <> "" Then
        MsgBox "Limpie todos los campos para buscar"
        tbsecpret.Enabled = False
        Exit Sub
    '    End
    Else
        Me.tbsecpret.Enabled = True
        Me.tbsecpret.SetFocus ' Mueve el cursor al textbox para que el usuario escriba
    End If
    
ElseIf Me.obtnayud.Value = False Then
    If tbcodsocprest.Value <> "" _
    Or tbcomprest.Value <> "" Or cbnomsocprest.Value <> "" Or tbmontprest.Value <> "" _
    Or tbplzprest.Value <> "" Or tbintrprest.Value <> "" Then
        MsgBox "Limpie todos los campos para buscar"
        tbsecpret.Enabled = False
        Exit Sub
    '    End
    Else
        Me.tbsecpret.Enabled = True
        Me.tbsecpret.SetFocus ' Mueve el cursor al textbox para que el usuario escriba
    End If
ElseIf Me.obtnprest.Value = False Then
'Else
    If tbcodsocprest.Value <> "" _
    Or tbcomprest.Value <> "" Or cbnomsocprest.Value <> "" Or tbmontprest.Value <> "" _
    Or tbplzprest.Value <> "" Or tbintrprest.Value <> "" Then
        MsgBox "Limpie todos los campos para buscar"
        tbsecpret.Enabled = False
        Exit Sub
    '    End
    Else
        Me.tbsecpret.Enabled = True
        Me.tbsecpret.SetFocus ' Mueve el cursor al textbox para que el usuario escriba
    End If
End If

End Sub
Private Sub cbbuscarpg_Click()
Me.obtning.Enabled = True
Me.obtnegre.Enabled = True
Me.obtnpag.Enabled = True
'Me.tbsecpag.Text = "PAGOS"

If Not (obtning.Value Or obtnegre.Value Or obtnpag.Value) Then
    MsgBox "Elije una opción de pago para buscar", vbExclamation, "Advertencia"
    Exit Sub
End If

If Me.obtning.Value = True Then
    Me.tbsecpag.Enabled = True
    Me.tbsecpag.SetFocus ' Mueve el cursor al textbox para que el usuario escriba
ElseIf Me.obtnegre.Value = True Then
    Me.tbsecpag.Enabled = True
    Me.tbsecpag.SetFocus ' Mueve el cursor al textbox para que el usuario escriba
ElseIf Me.obtnpag.Value = True Then
    If cmbprestpendt.Value <> "" Or tbcodsocio.Value <> "" _
    Or cmbnomsocpg.Value <> "" Or tbmontprstpg.Value <> "" _
    Or tbcuotpg.Value <> "" Or tbintprestpg.Value <> "" _
    Or tbintpg.Value <> "" Or tbcomntpag.Value <> "" Then
        MsgBox "Limpie todos los campos para buscar"
        Exit Sub
    Else
    Me.tbsecpag.Enabled = True
    Me.tbsecpag.SetFocus ' Mueve el cursor al textbox para que el usuario escriba
    End If
ElseIf Me.obtning.Value = False Then
    If cmbprestpendt.Value <> "" Or tbcodsocio.Value <> "" _
    Or cmbnomsocpg.Value <> "" Or tbmontprstpg.Value <> "" _
    Or tbcuotpg.Value <> "" Or tbintprestpg.Value <> "" _
    Or tbintpg.Value <> "" Or tbcomntpag.Value <> "" Then
        MsgBox "Limpie todos los campos para buscar"
        Exit Sub
    Else
    Me.tbsecpag.Enabled = True
    Me.tbsecpag.SetFocus ' Mueve el cursor al textbox para que el usuario escriba
    End If
ElseIf Me.obtnegre.Value = False Then
    If cmbprestpendt.Value <> "" Or tbcodsocio.Value <> "" _
    Or cmbnomsocpg.Value <> "" Or tbmontprstpg.Value <> "" _
    Or tbcuotpg.Value <> "" Or tbintprestpg.Value <> "" _
    Or tbintpg.Value <> "" Or tbcomntpag.Value <> "" Then
        MsgBox "Limpie todos los campos para buscar "
        Exit Sub
    Else
    Me.tbsecpag.Enabled = True
    Me.tbsecpag.SetFocus ' Mueve el cursor al textbox para que el usuario escriba
    End If
'ElseIf Me.obtnegre.Value = False And Me.obtning.Value = False And Me.obtnpag.Value = False Then
''    Me.tbsecpag.Enabled = True
''    Me.tbsecpag.Text = "PAGOS"
''    Me.tbsecpag.SetFocus ' Mueve el cursor al textbox para que el usuario escriba
'Me.tbsecpag.Text = "PAGOS"
''MsgBox "Elija una forma de pago"
''   Exit Sub
End If
End Sub









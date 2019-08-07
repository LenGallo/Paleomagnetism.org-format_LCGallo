Imports System
Imports System.IO
Imports System.Drawing
Imports System.Threading.Tasks
Imports System.Net.NetworkInformation
Imports System.Diagnostics
Imports IronPython.Hosting
Imports IronPython

Public Class Form1

    Dim statistics As New statistics
    Dim eigenvalues As New eigenvalues

    Private Sub Button1_Click(sender As Object, e As EventArgs)

        'Dim escribir As New StreamWriter("nombre archivo\kappavsGCD.csv")
        'escribir.WriteLine(",,,A95_salida,aMAD_salida,max_GCD_95_degrees")

        'For i = 0 To List.Count - 1
        '    escribir.WriteLine(CStr(List(i).n) + "," + )
        'Next i
        'escribir.Close()

    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click

        Dim lista_vgps As New List(Of GMAP_VGPS)

        Dim openFileDialog1 As New OpenFileDialog()

        openFileDialog1.InitialDirectory = ""
        openFileDialog1.Filter = "txt files (*.jr6)|*.jr6"
        openFileDialog1.FilterIndex = 2
        openFileDialog1.CheckFileExists = False
        openFileDialog1.RestoreDirectory = True
        openFileDialog1.Multiselect = True 'esto me permite abrir varios archivos.

        If openFileDialog1.ShowDialog() = System.Windows.Forms.DialogResult.OK Then

            Dim nombre_path As String = Path.GetDirectoryName(openFileDialog1.FileName) 'recorrido entero hasta el directorio entero del directorio

            Dim extension As String

            If RadioButton1.Checked = True Then
                extension = ".TH"
            Else
                extension = ".AF"
            End If

            Dim escribir As New StreamWriter(nombre_path + "\" + TextBox1.Text + extension)
            escribir.WriteLine("""Melisa"",""Argentina""") 'escribo el encabezado del todo el archivito

            For Each s As String In openFileDialog1.FileNames

                Dim nombre_completo As String
                nombre_completo = s
                Dim dato() As String 'para quedarme solo con el nombre
                dato = Split(nombre_completo, "\")
                Dim nombre_extension = dato.Last
                Dim nombre_especimen = Split(nombre_extension, ".").First

                Dim ioFile As New StreamReader(s)

                Dim liena_temp As String
                Dim separador As String = " "

                Do While Not ioFile.EndOfStream

                    liena_temp = ioFile.ReadLine
                    Dim linea_tmp As New GMAP_VGPS

                    Dim paso_lavado As String = liena_temp.Substring(10, 6)
                    Dim datoo() As String 'para quedarme solo con el nro de 
                    datoo = Split(paso_lavado, " ")
                    Dim paso As String = datoo.First

                    paso = paso.Replace("T", "")
                    paso = paso.Replace("AF", "")

                    linea_tmp.paso_lavado = Val(paso)

                    linea_tmp.exp = Val(liena_temp.Substring(36, 4)) ' como funciona el substring ==========>>>>> el primer caracter es la posicion, empezando a contar de a uno de izq a derecha, el segundo es la cantidad de carcteres que lee

                    linea_tmp.xyz_sp.cos_alfa = Val(liena_temp.Substring(18, 6)) * 10 ^ linea_tmp.exp
                    linea_tmp.xyz_sp.cos_beta = Val(liena_temp.Substring(24, 6)) * 10 ^ linea_tmp.exp
                    linea_tmp.xyz_sp.cos_gama = Val(liena_temp.Substring(30, 6)) * 10 ^ linea_tmp.exp

                    linea_tmp.intensidad = Math.Sqrt(linea_tmp.xyz_sp.cos_alfa * linea_tmp.xyz_sp.cos_alfa + linea_tmp.xyz_sp.cos_beta * linea_tmp.xyz_sp.cos_beta + linea_tmp.xyz_sp.cos_gama * linea_tmp.xyz_sp.cos_gama)

                    linea_tmp.Dir_sp = statistics.fc_datoeje_direccion(linea_tmp.xyz_sp)
                    Dim TEST_DIR_SP As Dato_direccional_rad = statistics.fc_degrees(linea_tmp.Dir_sp) 'testeo la direccion en coordenadas especimen

                    linea_tmp.orientation.direccion_inclinacion = Val(liena_temp.Substring(41, 3)) * Math.PI / 180
                    linea_tmp.orientation.inclinacion = Val(liena_temp.Substring(46, 2)) * Math.PI / 180

                    linea_tmp.bedding.direccion_inclinacion = Val(liena_temp.Substring(49, 3)) * Math.PI / 180
                    linea_tmp.bedding.inclinacion = Val(liena_temp.Substring(54, 2)) * Math.PI / 180

                    statistics.correccion_campo(linea_tmp.Dir_sp, linea_tmp.orientation, linea_tmp.Dir_geo)
                    Dim TEST_DIR_GEO As Dato_direccional_rad = statistics.fc_degrees(linea_tmp.Dir_geo) 'testeo direccion en coordenadas geograficas

                    statistics.correccion_estructura(linea_tmp.Dir_geo, linea_tmp.bedding, linea_tmp.Dir_tc)

                    Dim test_DEC_TC As Dato_direccional_rad = statistics.fc_degrees(linea_tmp.Dir_tc) ' testeo direccion en coordenadas tectonicas

                    linea_tmp.A95 = Val(liena_temp.Substring(76, 4)) '* Math.PI / 180

                    lista_vgps.Add(linea_tmp)

                Loop

                'lograr que diga solo el nombre del especimen
                escribir.WriteLine("""" + nombre_especimen + """" + "," + """" + """" + "," + CStr(0) + "," + CStr(90) + "," + CStr(1) + "," + CStr(lista_vgps(0).bedding.direccion_inclinacion * 180 / Math.PI) + "," + CStr(lista_vgps(0).bedding.inclinacion * 180 / Math.PI))

                'minimo exponente de la lista de exponentes
                Dim min As Integer = lista_vgps(0).exp
                For t = 0 To lista_vgps.Count - 1
                    If min > lista_vgps(t).exp Then
                        min = lista_vgps(t).exp
                    End If
                Next t

                For i = 0 To lista_vgps.Count - 1

                    Dim direccion_tmp As New Dato_direccional_rad

                    direccion_tmp.direccion_inclinacion = lista_vgps(i).Dir_geo.direccion_inclinacion
                    direccion_tmp.inclinacion = lista_vgps(i).Dir_geo.inclinacion

                    direccion_tmp.direccion_inclinacion = direccion_tmp.direccion_inclinacion - Math.PI / 2
                    direccion_tmp.inclinacion = -direccion_tmp.inclinacion

                    Dim dato_eje_tmp As New Dato_eje

                    statistics.direccion_a_dato_eje_RADIANES(direccion_tmp, dato_eje_tmp)

                    dato_eje_tmp.cos_alfa = dato_eje_tmp.cos_alfa * lista_vgps(i).intensidad * 10 ^ min 'tengo que escalar al valor mas Chico o grande de los exponented de la lista
                    dato_eje_tmp.cos_beta = dato_eje_tmp.cos_beta * lista_vgps(i).intensidad * 10 ^ min
                    dato_eje_tmp.cos_gama = dato_eje_tmp.cos_gama * lista_vgps(i).intensidad * 10 ^ min

                    'tomar el paso nomas y transformar NRM a cero
                    escribir.WriteLine(CStr(lista_vgps(i).paso_lavado) + "," + CStr(dato_eje_tmp.cos_gama) + "," + CStr(dato_eje_tmp.cos_beta) + "," + CStr(dato_eje_tmp.cos_alfa) + ",1,""date"",""time""")          '+ CStr(dato_eje_tmp.cos_alfa) + ",1,""date"",""time""")

                Next i

                escribir.WriteLine(CStr(9999))
                lista_vgps.Clear()

            Next
            escribir.WriteLine("""END""")
            escribir.Close()

        End If

    End Sub


End Class

Public Class statistics

    Dim eigen As New Eigenvalues
    Public Sub GetRandomNormal(ByVal mean As Single, ByVal stddev As Single, ByRef x As Single)
        ' This function returns a random number from a normal distribution with a mean of mean and standard
        ' deviation of stddev
        Dim r, phi, z As Single
        Randomize()
        r = Rnd()
        phi = Rnd()
        z = (Math.Cos(2 * Math.PI * r)) * (Math.Sqrt(-2 * Math.Log(phi)))
        x = z * stddev + mean
    End Sub

    Public Sub FshDev(ByVal k As Single, ByRef direction_rad As Dato_direccional_rad)
        'genera una direccion que pertenece a una poblacion de fishher de media "direction_rad" y kappa
        'from Pmagpy
        Dim R1 As Double = Rnd()
        Dim R2 As Double = Rnd()

        Dim L As Double = Math.Exp(-2 * k)
        Dim a As Double = R1 * (1 - L) + L
        Dim fac As Double = Math.Sqrt(-Math.Log(a) / (2 * k))
        Dim inc As Double = Math.PI / 2 - (2 * Math.Asin(fac))
        Dim dec As Double = 2 * Math.PI * R2

        direction_rad.direccion_inclinacion = dec
        direction_rad.inclinacion = inc

    End Sub

    Public Function fc_fsh_0_90_k(k As Double) 'funcion que crea una direccion que pertenece a una poblacion de fishher de media "direction_rad" y kappa 
        Dim direccion As New Dato_direccional_rad
        FshDev(k, direccion)
        Return direccion
    End Function

    Public Sub Rotate_RADIANS(ByVal axe_2b_rotated As Dato_eje, ByVal euler_pole As Dato_eje, ByVal rotation_RADIANS As Double, ByRef rotated_axe As Dato_eje)

        'from allmendinger 2013. ALL ANGLES IN RADIANS
        'input: euler_pole=> rotation axe (director cosines), rot => magnitude of rotation
        'output: rotated axe => direction cosines of the rotated vector
        'el output es un eje, si necesito dec/inc, lo tengo que transformar afuera de esta sub

        Dim a(2, 2) As Single

        Dim x As Single = 1 - Math.Cos(rotation_RADIANS)
        Dim sinRot As Single = Math.Sin(rotation_RADIANS)
        Dim cosRot As Single = Math.Cos(rotation_RADIANS)

        a(0, 0) = cosRot + euler_pole.cos_alfa * euler_pole.cos_alfa * x
        a(0, 1) = -euler_pole.cos_gama * sinRot + euler_pole.cos_alfa * euler_pole.cos_beta * x
        a(0, 2) = euler_pole.cos_beta * sinRot + euler_pole.cos_alfa * euler_pole.cos_gama * x
        a(1, 0) = euler_pole.cos_gama * sinRot + euler_pole.cos_beta * euler_pole.cos_alfa * x
        a(1, 1) = cosRot + euler_pole.cos_beta * euler_pole.cos_beta * x
        a(1, 2) = -euler_pole.cos_alfa * sinRot + euler_pole.cos_beta * euler_pole.cos_gama * x
        a(2, 0) = -euler_pole.cos_beta * sinRot + euler_pole.cos_gama * euler_pole.cos_alfa * x
        a(2, 1) = euler_pole.cos_alfa * sinRot + euler_pole.cos_gama * euler_pole.cos_beta * x
        a(2, 2) = cosRot + euler_pole.cos_gama * euler_pole.cos_gama * x

        Dim plotr(2) As Single 'esto se va a transformar luego en rotated axe 
        Dim temp(2) As Single 'transformo axe_2b_rotated en un vector porque lo copie de un codigo de Matlab
        temp(0) = axe_2b_rotated.cos_alfa
        temp(1) = axe_2b_rotated.cos_beta
        temp(2) = axe_2b_rotated.cos_gama

        For i = 0 To 2
            plotr(i) = 0
            For j = 0 To 2
                plotr(i) = a(i, j) * temp(j) + plotr(i)
            Next j
        Next i

        rotated_axe.cos_alfa = plotr(0)
        rotated_axe.cos_beta = plotr(1)
        rotated_axe.cos_gama = plotr(2)
    End Sub

    Public Sub zerotwopi_RADIANES(ByVal a As Single, ByRef b As Single)
        b = a
        If b < 0 Then
            b = b + 2 * Math.PI
        Else
            If b >= (2 * Math.PI) Then
                b = a - (2 * Math.PI)
            End If
        End If
    End Sub

    Public Function fc_zerotwopi_RADIANES(a As Single)
        Dim b As New Single
        zerotwopi_RADIANES(a, b)
        Return b
    End Function

    Public Sub CircMax_polo(ByVal linea1 As Dato_eje, ByVal linea2 As Dato_eje, ByRef polo_esferico As Dato_direccional_rad) 'FUNCIONA, la salida es Rumbo e inclinacion del plano. ENTRADA => EJES CARTESIANOS. SALIDA => EJE CARTESIANO DEL CIRC + RUMBO INCLINACION EN red/inc
        'hacer un if para ver si son dos lineas iguales 
        'saca un dato_direccional_rad_rad
        'SACA DEC-INC DEL PLANO

        Dim eje_poloplano As New Dato_eje

        Dim x, y, z As New Single

        x = linea1.cos_beta * linea2.cos_gama - linea1.cos_gama * linea2.cos_beta
        y = linea1.cos_gama * linea2.cos_alfa - linea1.cos_alfa * linea2.cos_gama
        z = linea1.cos_alfa * linea2.cos_beta - linea1.cos_beta * linea2.cos_alfa

        Dim r As New Single
        r = Math.Sqrt(x * x + y * y + z * z)

        eje_poloplano.cos_alfa = x / r
        eje_poloplano.cos_beta = y / r
        eje_poloplano.cos_gama = z / r

        Dim east As Single = Math.PI / 2
        'Dim polo_esferico As New dato_direccional_rad

        dato_eje_a_direccion_RADIANES(eje_poloplano, polo_esferico) ' hasta aca bien

        'REM ESTO ES PARA QUE CALCULE EL RUMBO E INCLINACION DEL PLANO

        'Dim dipaz As New Single
        'If polo_esferico.inclinacion >= 0 Then
        '    ru_inc_plano.inclinacion = east - polo_esferico.inclinacion
        '    dipaz = polo_esferico.direccion_inclinacion - Math.PI
        'Else
        '    ru_inc_plano.inclinacion = east + polo_esferico.inclinacion
        '    dipaz = polo_esferico.direccion_inclinacion
        'End If
        'Dim inclinacion As New Single

        'zerotwopi_RADIANES(dipaz - east, ru_inc_plano.direccion_inclinacion)

    End Sub

    Public Sub gcd_esferico_rad(ByVal linea1 As Dato_direccional_rad, ByVal linea2 As Dato_direccional_rad, ByRef GCD_rad As Single)
        ' output en RADIANES - VER PORQUE CREO QUE LA ENTRADA ESTA MAL
        GCD_rad = Math.Acos(Math.Cos(linea1.inclinacion) * Math.Cos(linea2.inclinacion) * Math.Cos(linea1.direccion_inclinacion - linea2.direccion_inclinacion) + Math.Sin(linea1.inclinacion) * Math.Sin(linea2.inclinacion))
    End Sub

    Public Function fc_GCD(ByVal linea1 As Dato_direccional_rad, ByVal linea2 As Dato_direccional_rad)
        Dim gcd As New Single
        gcd_esferico_rad(linea1, linea2, gcd)
        Return gcd
    End Function

    Public Sub calculate_euler_pole_rotation(ByVal linea1 As Dato_direccional_rad, ByVal linea2 As Dato_direccional_rad, ByRef euler_rotation_rad As Dato_direccional_rad)

        'toma una direccion y calcula la rotacion (dec/inc/grados) para llegar a la otra direccion
        'la salida es un dato_direccional donde el A95 es la magnitud del rotacion

        Dim magnitud_rotacion As New Double

        gcd_esferico_rad(linea1, linea2, magnitud_rotacion)

        CircMax_polo(fc_direccion_datoeje(linea1), fc_direccion_datoeje(linea2), euler_rotation_rad) 'me saca rumbo en inclinacion del PLANO, pero me interesa el polo del plano
        zerotwopi_RADIANES(euler_rotation_rad.direccion_inclinacion, euler_rotation_rad.direccion_inclinacion)
        zerotwopi_RADIANES(euler_rotation_rad.inclinacion, euler_rotation_rad.inclinacion)

        'ahora evaluo si la magnitud de la rotacion es positiva o negativa (no se me ocurre mas que rotar las dos veces y ver que cae bien)

        Dim a, b As New Dato_eje
        Rotate_RADIANS(fc_direccion_datoeje(linea1), fc_direccion_datoeje(euler_rotation_rad), magnitud_rotacion, a) ' a es el eje rotado con positivo
        Rotate_RADIANS(fc_direccion_datoeje(linea1), fc_direccion_datoeje(euler_rotation_rad), -magnitud_rotacion, b) 'b es el eje rotado con negativo

        Dim prueba_direA As Dato_direccional_rad = fc_datoeje_direccion(a) ' direccion rotada con positivo
        Dim prueba_direB As Dato_direccional_rad = fc_datoeje_direccion(b) ' direccion rotada con negativo

        Dim aa As Double = fc_gcd_rad(linea2, prueba_direA) 'se puede sacar
        Dim bb As Double = fc_gcd_rad(linea2, prueba_direB) 'se puede sacar

        If Math.Abs(fc_gcd_rad(linea2, fc_datoeje_direccion(a))) < Math.Abs(fc_gcd_rad(linea2, fc_datoeje_direccion(b))) Then
            euler_rotation_rad.A95 = magnitud_rotacion
        Else
            euler_rotation_rad.A95 = -magnitud_rotacion
        End If
    End Sub

    Public Sub direccion_a_dato_eje_RADIANES(ByVal sph As Dato_direccional_rad, ByRef eje As Dato_eje) ' OJO QUE LA ENTRADA ES EN radianes

        eje.cos_alfa = (Math.Cos(sph.inclinacion) * Math.Cos(sph.direccion_inclinacion))
        eje.cos_beta = (Math.Cos(sph.inclinacion) * Math.Sin(sph.direccion_inclinacion))
        eje.cos_gama = (Math.Sin(sph.inclinacion))

    End Sub
    Public Function fc_direccion_datoeje(ByVal sph As Dato_direccional_rad)
        Dim dato_eje As New Dato_eje
        direccion_a_dato_eje_RADIANES(sph, dato_eje)
        Return dato_eje
    End Function

    Public Function fc_datoeje_direccion(ByVal dato_eje As Dato_eje)
        Dim sph As New Dato_direccional_rad
        dato_eje_a_direccion_RADIANES(dato_eje, sph)
        Return sph
    End Function

    Public Function fc_gcd_rad(ByVal a As Dato_direccional_rad, ByVal b As Dato_direccional_rad)
        Dim gcd_rad As New Double
        gcd_esferico_rad(a, b, gcd_rad)
        Return gcd_rad

    End Function

    Public Sub dato_eje_a_direccion_RADIANES(ByVal dato_eje As Dato_eje, ByRef direccion As Dato_direccional_rad) 'LA SALIDA ES EN radianes

        'inclinacion
        direccion.inclinacion = Math.Asin(dato_eje.cos_gama / Math.Sqrt(dato_eje.cos_alfa * dato_eje.cos_alfa + dato_eje.cos_beta * dato_eje.cos_beta + dato_eje.cos_gama * dato_eje.cos_gama))

        'direccion.inclinacion = Math.Asin(dato_eje.cos_gama)
        'direccion de inclinacion

        If dato_eje.cos_alfa = 0 Then
            If dato_eje.cos_beta < 0 Then
                direccion.direccion_inclinacion = 3 / 2 * Math.PI
            Else
                direccion.direccion_inclinacion = Math.PI / 2
            End If
        Else
            direccion.direccion_inclinacion = Math.Atan(dato_eje.cos_beta / dato_eje.cos_alfa)
            If dato_eje.cos_alfa < 0 Then
                direccion.direccion_inclinacion = direccion.direccion_inclinacion + Math.PI
            End If
            zerotwopi_RADIANES(direccion.direccion_inclinacion, direccion.direccion_inclinacion)
        End If
    End Sub


    Public Sub aMAD(ByVal entrada As Muestra_direcciones, ByRef aMAD As Double)
        Dim eje_may_tmp, eje_int_tmp, eje_men_tmp As New Dato_eje
        eigen.calcula_ejes_entrada_muestra(entrada, eje_may_tmp, eje_int_tmp, eje_men_tmp)
        aMAD = Math.Atan((Math.Sqrt(eje_int_tmp.modulo ^ 2 + eje_men_tmp.modulo ^ 2)) / eje_may_tmp.modulo)
    End Sub

    Public Sub Fisher_statistics(ByVal muestra As Muestra_direcciones, ByRef stat As fisher_statistics) 'tengo que agrandar esta rutina para que saque mas parametros
        'falla cuando la media esta entre 90 y 270

        Dim sumatoria_x, sumatoria_y, sumatoria_z As New Double

        For i = 0 To muestra.lista_direcciones.Count - 1
            Dim eje_tmp As New Dato_eje
            eje_tmp = fc_direccion_datoeje(muestra.lista_direcciones(i))
            sumatoria_x = sumatoria_x + eje_tmp.cos_alfa
            sumatoria_y = sumatoria_y + eje_tmp.cos_beta
            sumatoria_z = sumatoria_z + eje_tmp.cos_gama
        Next i

        Dim n As Integer = muestra.lista_direcciones.Count
        stat.R = (sumatoria_x ^ 2 + sumatoria_y ^ 2 + sumatoria_z ^ 2) ^ (1 / 2)
        stat.n = n

        stat.Rm = stat.R / muestra.lista_direcciones.Count ' = R/n
        stat.kappa = (muestra.lista_direcciones.Count - 1) / (muestra.lista_direcciones.Count - stat.R)
        stat.A95 = stat.kappa / ((stat.kappa * muestra.lista_direcciones.Count) ^ (1 / 2))

        stat.A95 = Math.Acos(1 - (((stat.n - stat.R) / stat.R) * ((1 / 0.05) ^ (1 / (stat.n - 1)) - 1)))

        Dim mean_axe As New Dato_eje

        mean_axe.cos_alfa = sumatoria_x / stat.R
        mean_axe.cos_beta = sumatoria_y / stat.R
        mean_axe.cos_gama = sumatoria_z / stat.R

        stat.mean = fc_datoeje_direccion(mean_axe)

        zerotwopi_RADIANES(stat.mean.direccion_inclinacion, stat.mean.direccion_inclinacion)

    End Sub

    Public Function fc_fisher_mean(ByVal muestra As Muestra_direcciones)
        Dim Media As New Dato_direccional_rad
        Dim statistics As New fisher_statistics
        Fisher_statistics(muestra, statistics)
        Return statistics.mean
    End Function

    Public Function fc_degrees(ByVal direccion As Dato_direccional_rad)
        Dim direccion_salida As New Dato_direccional_rad
        direccion_salida.direccion_inclinacion = direccion.direccion_inclinacion * 180 / Math.PI
        direccion_salida.inclinacion = direccion.inclinacion * 180 / Math.PI
        direccion_salida.A95 = direccion.A95 * 180 / Math.PI
        Return direccion_salida
    End Function

    Public Sub calculates_woodcock_universal(ByVal objeto As Object, ByRef parameters As Woodcock_parameters_MAD)

        'toma una muestra y evalua si son direcciones o xyz para luego calcular el tensor y sus parametros de woodcock

        Dim eigen As New Eigenvalues
        Dim eje_may, eje_int, eje_men As New Dato_eje

        If TypeOf objeto Is Muestra_direcciones Or TypeOf objeto Is Muestra_ejes Then '
            If TypeOf objeto Is Muestra_ejes Then 'si son ejes casteo muestra a ejes
                Dim muestra_ejes As Muestra_ejes = CType(objeto, Muestra_ejes)
                eigen.calcula_ejes_entrada_muestra(muestra_ejes, eje_may, eje_int, eje_men)
            Else ' si son direcciones casteo a muestra de direcciones
                Dim muestra_direcciones As Muestra_direcciones = CType(objeto, Muestra_direcciones)
                eigen.calcula_ejes_entrada_muestra(muestra_direcciones, eje_may, eje_int, eje_men)
            End If
        Else
            If TypeOf objeto Is List(Of Dato_direccional_rad) Or TypeOf objeto Is List(Of Dato_eje) Then
                If TypeOf objeto Is List(Of Dato_direccional_rad) Then
                    Dim lista_direccional As List(Of Dato_direccional_rad) = CType(objeto, List(Of Dato_direccional_rad)) 'casteo el objeto de entrada como una lista de direccionales para sacar cada dato como xyz y agregarlo a la lista de xyz
                    eigen.calcula_ejes_entrada_lista(lista_direccional, eje_may, eje_int, eje_men)
                Else
                    Dim lista_ejes As List(Of Dato_eje) = CType(objeto, List(Of Dato_eje))
                    eigen.calcula_ejes_entrada_lista(lista_ejes, eje_may, eje_int, eje_men)
                End If
            Else
                MsgBox("error en la entrada para calcular parametros de woodcock")
            End If
        End If

        REM CALCULO PARAMETROS CON LOS AUTOVALORES QUE YA CALCULE
        parameters.eigenval_may = eje_may.modulo
        parameters.eigenval_int = eje_int.modulo
        parameters.eigenval_men = eje_men.modulo
        parameters.lineation = Math.Log(eje_may.modulo / eje_int.modulo)
        parameters.foliation = Math.Log(eje_int.modulo / eje_men.modulo)
        parameters.colinearity = (Math.Log(eje_may.modulo / eje_int.modulo)) / (Math.Log(eje_int.modulo / eje_men.modulo))
        parameters.coplanarity = Math.Log(eje_may.modulo / eje_men.modulo)
        parameters.MAD = Math.Atan(Math.Sqrt(eje_men.modulo / eje_int.modulo + eje_men.modulo / eje_may.modulo))

        If Double.IsNaN(parameters.eigenval_int) Then parameters.eigenval_int = 0
        If Double.IsNaN(parameters.eigenval_men) Then parameters.eigenval_men = 0
        If Double.IsNaN(parameters.eigenval_may) Then parameters.eigenval_may = 0
        If Double.IsNaN(parameters.lineation) Then parameters.lineation = 0
        If Double.IsNaN(parameters.foliation) Then parameters.foliation = 0
        If Double.IsNaN(parameters.colinearity) Then parameters.colinearity = 0
        If Double.IsNaN(parameters.coplanarity) Then parameters.coplanarity = 0
        If Double.IsNaN(parameters.MAD) Then parameters.MAD = 0

    End Sub

    Public Sub max_gcd_95(ByVal objeto As Object, ByRef max_GCD_95 As Double)
        'input: una muestra o lista de direcciones o ejes bootstrapeados
        'output: un numero que refleja la maxima dispersion de la muestra bootstrapeada

        Dim lista_gcd As New List(Of Single)

        If TypeOf objeto Is Muestra_direcciones Or TypeOf objeto Is Muestra_ejes Then '

            If TypeOf objeto Is Muestra_ejes Then 'si son ejes casteo muestra a ejes
                Dim muestra_ejes As Muestra_ejes = CType(objeto, Muestra_ejes)
                Dim eje_min_tmp, eje_int_tmp, eje_max_tmp As New Dato_eje
                eigen.calcula_ejes_entrada_muestra(muestra_ejes, eje_max_tmp, eje_int_tmp, eje_min_tmp)
                For z = 0 To muestra_ejes.lista_ejes.Count - 1
                    Dim gcd_tmp As Single = fc_GCD(fc_datoeje_direccion(eje_max_tmp), fc_datoeje_direccion(muestra_ejes.lista_ejes(z)))
                    lista_gcd.Add(gcd_tmp)
                Next z
                lista_gcd.Sort()
                max_GCD_95 = lista_gcd(Math.Truncate(muestra_ejes.lista_ejes.Count * 0.95))

            Else ' si son direcciones casteo a muestra de direcciones

                Dim muestra_direcciones As Muestra_direcciones = CType(objeto, Muestra_direcciones)
                Dim eje_min_tmp, eje_int_tmp, eje_max_tmp As New Dato_eje
                eigen.calcula_ejes_entrada_muestra(muestra_direcciones, eje_max_tmp, eje_int_tmp, eje_min_tmp)

                For z = 0 To muestra_direcciones.lista_direcciones.Count - 1
                    Dim gcd_tmp As Single = fc_GCD(fc_datoeje_direccion(eje_max_tmp), muestra_direcciones.lista_direcciones(z))
                    lista_gcd.Add(gcd_tmp)
                Next z
                lista_gcd.Sort()
                max_GCD_95 = lista_gcd(Math.Truncate(muestra_direcciones.lista_direcciones.Count * 0.95))
            End If

        Else

            If TypeOf objeto Is List(Of Dato_direccional_rad) Or TypeOf objeto Is List(Of Dato_eje) Then

                If TypeOf objeto Is List(Of Dato_direccional_rad) Then

                    Dim lista_direccional As List(Of Dato_direccional_rad) = CType(objeto, List(Of Dato_direccional_rad))
                    Dim eje_min_tmp, eje_int_tmp, eje_max_tmp As New Dato_eje
                    eigen.calcula_ejes_entrada_lista(lista_direccional, eje_max_tmp, eje_int_tmp, eje_min_tmp)

                    For z = 0 To lista_direccional.Count - 1
                        Dim gcd_tmp As Single = fc_GCD(fc_datoeje_direccion(eje_max_tmp), lista_direccional(z))
                        lista_gcd.Add(gcd_tmp)
                    Next z

                    lista_gcd.Sort()
                    max_GCD_95 = lista_gcd(Math.Truncate(lista_direccional.Count * 0.95))

                Else
                    Dim lista_ejes As List(Of Dato_eje) = CType(objeto, List(Of Dato_eje))
                    Dim eje_min_tmp, eje_int_tmp, eje_max_tmp As New Dato_eje
                    eigen.calcula_ejes_entrada_lista(lista_ejes, eje_max_tmp, eje_int_tmp, eje_min_tmp)

                    For z = 0 To lista_ejes.Count - 1
                        Dim gcd_tmp As Single = fc_GCD(fc_datoeje_direccion(eje_max_tmp), fc_datoeje_direccion(lista_ejes(z)))
                        lista_gcd.Add(gcd_tmp)
                    Next z
                    lista_gcd.Sort()
                    max_GCD_95 = lista_gcd(Math.Truncate(lista_ejes.Count * 0.95))

                End If

            Else
                MsgBox("error en la entrada para calcular max_GCD")
            End If
        End If


    End Sub

    Public Sub pole_from_direction(ByVal Site_coordinate As Dato_direccional_rad, ByVal direction As Dato_direccional_rad, ByRef pole As Dato_direccional_rad)
        'ecuaciones en pagina 158 de Butler92'

        pole.inclinacion = Math.Asin(Math.Sin(Site_coordinate.inclinacion) * Math.Cos(Math.Atan((2 / Math.Tan(direction.inclinacion)))) + Math.Cos(Site_coordinate.inclinacion) * Math.Sin(Math.Atan((2 / Math.Tan(direction.inclinacion)))) * Math.Cos(direction.direccion_inclinacion))
        Dim test As Dato_direccional_rad = fc_degrees(pole)

        Dim a As Double = Math.Cos(Math.Atan((2 / Math.Tan(direction.inclinacion))))
        Dim b As Double = Math.Sin(Site_coordinate.inclinacion) * Math.Sin(pole.inclinacion)

        If a > b Then
            pole.direccion_inclinacion = Site_coordinate.direccion_inclinacion + Math.Asin((Math.Sin(Math.Atan((2 / Math.Tan(direction.inclinacion)))) * Math.Sin(direction.direccion_inclinacion)) / Math.Cos(pole.inclinacion))
        Else
            pole.direccion_inclinacion = Site_coordinate.direccion_inclinacion + Math.PI - Math.Asin((Math.Sin(Math.Atan((2 / Math.Tan(direction.inclinacion)))) * Math.Sin(direction.direccion_inclinacion)) / Math.Cos(pole.inclinacion))
        End If
        Dim testt As Dato_direccional_rad = fc_degrees(pole)

    End Sub

    Public Sub direction_from_pole(ByVal Site As Dato_direccional_rad, ByVal Pole As Dato_direccional_rad, ByRef Direction As Dato_direccional_rad)

        Dim delphi As Double = Math.Abs(Pole.direccion_inclinacion - Site.direccion_inclinacion)

        Dim thetaSite As Double = Math.PI / 2 - Site.inclinacion
        Dim thetaPole As Double = Math.PI / 2 - Pole.inclinacion
        Dim cosp As Double = Math.Cos(thetaSite) * Math.Cos(thetaPole) + Math.Sin(thetaSite)

        'cosp = np.cos(thetaS) * np.cos(thetaP) + np.sin(thetaS) * \
        'np.sin(thetaP) * np.cos(delphi)
        'thetaM = np.arccos(cosp)
        'cosd = old_div((np.cos(thetaP) - np.cos(thetaM) *
        '                np.cos(thetaS)), (np.sin(thetaM) * np.sin(thetaS)))

    End Sub

    Public Sub correccion_campo(ByVal direccion_spec As Dato_direccional_rad, ByVal correccion_campo As Dato_direccional_rad, ByRef direccion_geo As Dato_direccional_rad)
        'Funciona OK
        'input: direccion en coordenadas de especimen y la correccion de campo en 3 0 3 90
        'salida: direccion en coordenadas geograficas

        Dim euler As New Dato_direccional_rad 'roto con un eje horizontal paralelo al rumbo y llevo a la horizontal
        euler.direccion_inclinacion = 0
        euler.inclinacion = 0
        euler.A95 = correccion_campo.inclinacion

        Dim rotado As New Dato_eje
        Rotate_RADIANS(fc_direccion_datoeje(direccion_spec), fc_direccion_datoeje(euler), euler.A95, rotado)

        Dim dire_tmp_rotada As Dato_direccional_rad = fc_datoeje_direccion(rotado)
        dire_tmp_rotada.direccion_inclinacion = fc_zerotwopi_RADIANES(dire_tmp_rotada.direccion_inclinacion + correccion_campo.direccion_inclinacion)

        direccion_geo = dire_tmp_rotada
        direccion_geo.direccion_inclinacion = fc_zerotwopi_RADIANES(direccion_geo.direccion_inclinacion)

    End Sub

    Public Sub correccion_estructura(ByVal direccion_geo As Dato_direccional_rad, ByVal bedding As Dato_direccional_rad, ByRef Dir_tc As Dato_direccional_rad)
        'Funciona OK
        'usa la funcion rotar
        'input=> le das la direccion (en coord geograficas) a corregir por estructura
        'output=> sale la direccion corregida por estructura

        Dim euler As New Dato_direccional_rad
        euler.direccion_inclinacion = bedding.direccion_inclinacion
        euler.inclinacion = 0
        euler.A95 = -bedding.inclinacion 'la rotaccion es el opuiesto de lo que inclina el banco porque restituye, es la rotacion antihorario (-)

        'Dim test_euler As Dato_direccional_rad = fc_degrees(euler)

        Dim rotado As New Dato_eje
        Rotate_RADIANS(fc_direccion_datoeje(direccion_geo), fc_direccion_datoeje(euler), euler.A95, rotado)

        Dir_tc = fc_datoeje_direccion(rotado)
        Dir_tc.direccion_inclinacion = fc_zerotwopi_RADIANES(Dir_tc.direccion_inclinacion)

        'Dim test_dir_tc As Dato_direccional_rad = fc_degrees(Dir_tc)

    End Sub

    Public Sub paleolat(ByVal inclination As Double, ByRef paleolat As Double)
        paleolat = Math.Atan((Math.Tan(inclination)) / 2)
    End Sub

    Public Function fc_paleolat(k As Double)
        Dim paleolatitud As Double
        paleolat(k, paleolatitud)
        Return paleolatitud
    End Function


    Public Sub calculate_bedding_strike_dip(ByVal dir_geo As Dato_direccional_rad, ByVal dir_tc As Dato_direccional_rad, ByRef bedding As Dato_direccional_rad)

        Dim dir_corr_tmp As New Dato_direccional_rad

        bedding.direccion_inclinacion = 0
        bedding.inclinacion = 0

        Dim a As Single = Math.PI / 2

        Do While a > 0.4

            bedding.direccion_inclinacion = bedding.direccion_inclinacion + 0.005
            bedding.inclinacion = 0

            Do While a > 0.4 'Or bedding.inclinacion < Math.PI / 2

                bedding.inclinacion = bedding.inclinacion + 0.005

                correccion_estructura(dir_geo, bedding, dir_corr_tmp)

                a = fc_GCD(dir_tc, dir_corr_tmp) * 180 / Math.PI 'esta en grados

                If bedding.inclinacion > Math.PI / 2 Then
                    Exit Do
                End If

            Loop

            If bedding.direccion_inclinacion > 2 * Math.PI Then
                Exit Do
            End If
        Loop

    End Sub





End Class

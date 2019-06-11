Public Class eigenvalues
    Private Sub Jacobi(TT, N, NP, contador, V, D)

        Dim B(10) As Double
        Dim Z(10) As Double
        Dim A(5, 5) As Double


        Dim NROT As Integer
        Dim SM As Integer
        Dim TRESH As Double
        Dim THETA As Double
        Dim H As Double
        Dim T As Double
        Dim s As Double
        Dim c As Double
        Dim TAU As Double
        Dim G As Double

        A(1, 1) = TT(1, 1)
        A(1, 2) = TT(1, 2)
        A(1, 3) = TT(1, 3)
        A(2, 1) = TT(2, 1)
        A(2, 2) = TT(2, 2)
        A(2, 3) = TT(2, 3)
        A(3, 1) = TT(3, 1)
        A(3, 2) = TT(3, 2)
        A(3, 3) = TT(3, 3)

        For IP = 1 To N
            For IQ = 1 To N
                V(IP, IQ) = 0
            Next
            V(IP, IP) = 1
        Next

        For IP = 1 To N
            B(IP) = A(IP, IP)
            D(IP) = B(IP)
            Z(IP) = 0
        Next

        NROT = 0

        For i = 1 To 50
            SM = 0
            For IP = 1 To N - 1
                For IQ = IP + 1 To N
                    SM = SM + Math.Abs(A(IP, IQ)) ' OJO ABS parece funsion
                Next
            Next

            REM If SM = 0 Then End Sub 'OJO no se que es el RETURN

            If i < 4 Then
                TRESH = 0.2 * SM / (N ^ 2)
            Else
                TRESH = 0
            End If

            For IP = 1 To N - 1
                For IQ = IP + 1 To N
                    G = 100 * Math.Abs(A(IP, IQ))

                    If i > 4 And Math.Abs(D(IP)) + G = Math.Abs(D(IP)) And Math.Abs(D(IQ)) + G = Math.Abs(D(IQ)) Then
                        A(IP, IQ) = 0
                    ElseIf Math.Abs(A(IP, IQ)) > TRESH Then
                        H = D(IQ) - D(IP)
                        If Math.Abs(H) + G = Math.Abs(H) Then
                            T = A(IP, IQ) / H
                        Else
                            THETA = 0.5 * H / A(IP, IQ)
                            T = 1 / (Math.Abs(THETA) + Math.Sqrt(1 + (THETA ^ 2))) 'OJO SQRT
                            If THETA < 0 Then T = -T
                        End If
                        c = 1 / Math.Sqrt(1 + (T ^ 2))
                        s = T * c
                        TAU = s / (1 + c)
                        H = T * A(IP, IQ)
                        Z(IP) = Z(IP) - H
                        Z(IQ) = Z(IQ) + H
                        D(IP) = D(IP) - H
                        D(IQ) = D(IQ) + H
                        A(IP, IQ) = 0
                        For j = 1 To IP - 1
                            G = A(j, IP)
                            H = A(j, IQ)
                            A(j, IP) = G - s * (H + G * TAU)
                            A(j, IQ) = H + s * (G - H * TAU)
                        Next

                        For j = IP + 1 To IQ - 1
                            G = A(IP, j)
                            H = A(j, IQ)
                            A(IP, j) = G - s * (H + G * TAU)
                            A(j, IQ) = H + s * (G - H * TAU) 'OJO se puede haber equivocado
                        Next

                        For j = IQ + 1 To N
                            G = A(IP, j)
                            H = A(IQ, j)
                            A(IP, j) = G - s * (H + G * TAU)
                            A(IQ, j) = H + s * (G - H * TAU)
                        Next

                        For j = 1 To N
                            G = V(j, IP)
                            H = V(j, IQ)
                            V(j, IP) = G - s * (H + G * TAU)
                            V(j, IQ) = H + s * (G - H * TAU) 'EIGENVECTORS
                        Next

                        NROT = NROT + 1

                    End If
                Next
            Next

            For IP = 1 To N
                B(IP) = B(IP) + Z(IP)
                D(IP) = B(IP) 'EIGENVALUES
                Z(IP) = 0
            Next
        Next

        REM NORMALIZA EIGENVALUES Y CONVIERTE EIGENVECTORS AL COORD ESFER DEL HEMISFERIO SUR
        For i = 1 To 3
            D(i) = D(i) / contador
            'If V(3, i) < 0 Then
            '    V(1, i) = -V(1, i)
            '    V(2, i) = -V(2, i)
            '    V(3, i) = -V(3, i)
            'End If
        Next
        REM NORMALIZA EIGENVALUES Y CONVIERTE EIGENVECTORS AL COORD ESFER DEL HEMISFERIO SUR

    End Sub

    Public Sub calcula_ejes_entrada_lista(ByVal objeto As Object, ByRef eje_mayor As Dato_eje, ByRef eje_intermedio As Dato_eje, ByRef eje_menor As Dato_eje)

        'calcula los ejes de una lista de datos direccionales (dec/inc) o xyz. Como se calcula a partir de ejes xyz, si es lista de direcciones primero la transformo
        'no centra la matriz, la calcula desde [0,0,0] y el modulo de los vectores es 1

        Dim TT(3, 3) As Double 'creo la matriz de covarianza
        Dim lista_datos As New List(Of Dato_eje) 'creo la lista donde van a ir los xyz - si en pcipio son dec/inc primero lo voy a transformar

        If TypeOf objeto Is List(Of Dato_direccional_rad) Or TypeOf objeto Is List(Of Dato_eje) Then 'si el objeto de entrada es una lista de direccionales o xyz

            If TypeOf objeto Is List(Of Dato_direccional_rad) Then

                Dim lista_direccional_tmp As List(Of Dato_direccional_rad) = CType(objeto, List(Of Dato_direccional_rad)) 'casteo el objeto de entrada como una lista de direccionales para sacar cada dato como xyz y agregarlo a la lista de xyz

                For i = 0 To lista_datos.Count - 1

                    Dim xyz_tmp As New Dato_eje

                    xyz_tmp.cos_alfa = Math.Cos(lista_direccional_tmp(i).direccion_inclinacion) * Math.Cos(lista_direccional_tmp(i).inclinacion) 'y
                    xyz_tmp.cos_beta = Math.Sin(lista_direccional_tmp(i).direccion_inclinacion) * Math.Cos(lista_direccional_tmp(i).inclinacion) 'x
                    xyz_tmp.cos_gama = Math.Sin(lista_direccional_tmp(i).inclinacion) 'z                  

                    lista_datos.Add(xyz_tmp)

                Next i
            Else
                lista_datos = CType(objeto, List(Of Dato_eje))
            End If
        Else
            MsgBox("error en la entrada para calcular autovalores") ' salva el caso de que la entrada sea cualquier cosa, es decir, que no sea una lista de datos direcc o xyz
        End If

        For i = 0 To lista_datos.Count - 1
            REM ORIENTATION MATRIX
            TT(1, 1) = TT(1, 1) + (lista_datos(i).cos_alfa) ^ 2
            TT(1, 2) = TT(1, 2) + (lista_datos(i).cos_alfa * lista_datos(i).cos_beta)
            TT(1, 3) = TT(1, 3) + (lista_datos(i).cos_alfa * lista_datos(i).cos_gama)
            TT(2, 1) = TT(1, 2)
            TT(2, 2) = TT(2, 2) + (lista_datos(i).cos_beta) ^ 2
            TT(2, 3) = TT(2, 3) + (lista_datos(i).cos_beta * lista_datos(i).cos_gama)
            TT(3, 1) = TT(1, 3)
            TT(3, 2) = TT(2, 3)
            TT(3, 3) = TT(3, 3) + (lista_datos(i).cos_gama) ^ 2
            REM FIN ORIENTATION MATRIX
        Next i

        Dim V(3, 3) As Double
        Dim D(3) As Double

        If Double.IsNaN(TT(1, 1)) Or Double.IsNaN(TT(1, 2)) Or Double.IsNaN(TT(1, 3)) Or Double.IsNaN(TT(2, 1)) Or Double.IsNaN(TT(2, 2)) Or Double.IsNaN(TT(2, 3)) Or Double.IsNaN(TT(3, 1)) Or Double.IsNaN(TT(3, 2)) Or Double.IsNaN(TT(3, 3)) Then '<> double.NaN Or TT(1, 2) <> double.NaN Or TT(1, 3) <> double.NaN Or TT(2, 2) <> double.NaN Or TT(2, 3) <> double.NaN Or TT(3, 3) <> double.NaN Then ' para evitar que se rompa
        Else
            Jacobi(TT, 3, 3, lista_datos.Count, V, D)
        End If

        Dim max_i As Integer
        Dim int_i As Integer
        Dim min_i As Integer

        Dim maximo As Double = -100000000
        Dim minimo As Double = 100000000

        For i = 1 To 3
            If D(i) > maximo Then
                maximo = D(i)
                max_i = i
            End If
        Next i

        For i = 1 To 3
            If D(i) < minimo Then
                minimo = D(i)
                min_i = i
            End If
        Next i

        For i = 1 To 3
            If i <> min_i And i <> max_i And i <> 0 Then
                int_i = i
            End If
        Next

        eje_mayor.modulo = D(max_i)
        eje_intermedio.modulo = D(int_i)
        eje_menor.modulo = D(min_i)

        eje_mayor.cos_alfa = V(1, max_i)
        eje_mayor.cos_beta = V(2, max_i)
        eje_mayor.cos_gama = V(3, max_i)

        eje_intermedio.cos_alfa = V(1, int_i)
        eje_intermedio.cos_beta = V(2, int_i)
        eje_intermedio.cos_gama = V(3, int_i)

        eje_menor.cos_alfa = V(1, min_i)
        eje_menor.cos_beta = V(2, min_i)
        eje_menor.cos_gama = V(3, min_i)

    End Sub

    Public Sub calcula_ejes_centro_en_media(ByVal objeto As Object, ByRef eje_mayor As Dato_eje, ByRef eje_intermedio As Dato_eje, ByRef eje_menor As Dato_eje)

        'calcula los autovalores/vectores de una lista de datos (tanto dec/ins como xyz) centrada en [Xm,Ym,Zm]

        Dim TT(3, 3) As Double 'creo la matriz de covarianza
        Dim lista_datos As New List(Of Dato_eje) 'creo la lista donde van a ir los xyz - si en pcipio son dec/inc primero lo voy a transformar

        If TypeOf objeto Is List(Of Dato_direccional_rad) Or TypeOf objeto Is List(Of Dato_eje) Then 'si el objeto de entrada es una lista de direccionales o xyz

            If TypeOf objeto Is List(Of Dato_direccional_rad) Then

                Dim lista_direccional_tmp As List(Of Dato_direccional_rad) = CType(objeto, List(Of Dato_direccional_rad)) 'casteo el objeto de entrada como una lista de direccionales para sacar cada dato como xyz y agregarlo a la lista de xyz

                For i = 0 To lista_direccional_tmp.Count - 1

                    Dim xyz_tmp As New Dato_eje

                    xyz_tmp.cos_alfa = Math.Cos(lista_direccional_tmp(i).direccion_inclinacion) * Math.Cos(lista_direccional_tmp(i).inclinacion) 'y
                    xyz_tmp.cos_beta = Math.Sin(lista_direccional_tmp(i).direccion_inclinacion) * Math.Cos(lista_direccional_tmp(i).inclinacion) 'x
                    xyz_tmp.cos_gama = Math.Sin(lista_direccional_tmp(i).inclinacion) 'z                  

                    lista_datos.Add(xyz_tmp)

                Next i
            Else
                lista_datos = CType(objeto, List(Of Dato_eje)) ' si el objeto es una lista de XYZ directamente lo castea a su clase correspondiente
            End If
        Else
            MsgBox("error en la entradad para calcular autovalores") ' salva el caso de que la entrada sea cualquier cosa, es decir, que no sea una lista de datos direcc o xyz
        End If

        REM CALCULO EL BARICENTRO
        Dim sumatoria_x As New Double
        Dim sumatoria_y As New Double
        Dim sumatoria_z As New Double
        For i = 0 To lista_datos.Count - 1 Step 1
            sumatoria_x = sumatoria_x + lista_datos(i).cos_alfa
            sumatoria_y = sumatoria_y + lista_datos(i).cos_beta
            sumatoria_z = sumatoria_z + lista_datos(i).cos_gama
        Next i
        Dim x_m, y_m, z_m As New Double
        x_m = sumatoria_x / lista_datos.Count
        y_m = sumatoria_y / lista_datos.Count
        z_m = sumatoria_z / lista_datos.Count
        REM FIN

        For i = 0 To lista_datos.Count - 1
            REM ORIENTATION MATRIX
            TT(1, 1) = TT(1, 1) + (lista_datos(i).cos_alfa - x_m) ^ 2
            TT(1, 2) = TT(1, 2) + ((lista_datos(i).cos_alfa - x_m) * (lista_datos(i).cos_beta - y_m))
            TT(1, 3) = TT(1, 3) + ((lista_datos(i).cos_alfa - x_m) * (lista_datos(i).cos_gama - z_m))
            TT(2, 1) = TT(1, 2)
            TT(2, 2) = TT(2, 2) + (lista_datos(i).cos_beta - y_m) ^ 2
            TT(2, 3) = TT(2, 3) + ((lista_datos(i).cos_beta - y_m) * (lista_datos(i).cos_gama - z_m))
            TT(3, 1) = TT(1, 3)
            TT(3, 2) = TT(2, 3)
            TT(3, 3) = TT(3, 3) + (lista_datos(i).cos_gama - z_m) ^ 2
            REM FIN ORIENTATION MATRIX
        Next i

        Dim V(3, 3) As Double
        Dim D(3) As Double

        If Double.IsNaN(TT(1, 1)) Or Double.IsNaN(TT(1, 2)) Or Double.IsNaN(TT(1, 3)) Or Double.IsNaN(TT(2, 1)) Or Double.IsNaN(TT(2, 2)) Or Double.IsNaN(TT(2, 3)) Or Double.IsNaN(TT(3, 1)) Or Double.IsNaN(TT(3, 2)) Or Double.IsNaN(TT(3, 3)) Then '<> double.NaN Or TT(1, 2) <> double.NaN Or TT(1, 3) <> double.NaN Or TT(2, 2) <> double.NaN Or TT(2, 3) <> double.NaN Or TT(3, 3) <> double.NaN Then ' para evitar que se rompa
        Else
            Jacobi(TT, 3, 3, lista_datos.Count, V, D)
        End If

        Dim max_i As Integer
        Dim int_i As Integer
        Dim min_i As Integer

        Dim maximo As Double = -100000000
        Dim minimo As Double = 100000000

        For i = 1 To 3
            If D(i) > maximo Then
                maximo = D(i)
                max_i = i
            End If
        Next i

        For i = 1 To 3
            If D(i) < minimo Then
                minimo = D(i)
                min_i = i
            End If
        Next i

        For i = 1 To 3
            If i <> min_i And i <> max_i And i <> 0 Then
                int_i = i
            End If
        Next

        eje_mayor.modulo = D(max_i)
        eje_intermedio.modulo = D(int_i)
        eje_menor.modulo = D(min_i)

        eje_mayor.cos_alfa = V(1, max_i)
        eje_mayor.cos_beta = V(2, max_i)
        eje_mayor.cos_gama = V(3, max_i)

        eje_intermedio.cos_alfa = V(1, int_i)
        eje_intermedio.cos_beta = V(2, int_i)
        eje_intermedio.cos_gama = V(3, int_i)

        eje_menor.cos_alfa = V(1, min_i)
        eje_menor.cos_beta = V(2, min_i)
        eje_menor.cos_gama = V(3, min_i)





    End Sub

    Public Sub calcula_ejes_entrada_muestra(ByVal objeto As Object, ByRef eje_mayor As Dato_eje, ByRef eje_intermedio As Dato_eje, ByRef eje_menor As Dato_eje)

        'calcula los ejes de una lista de datos direccionales (dec/inc) o xyz. Como se calcula a partir de ejes xyz, si es lista de direcciones primero la transformo
        'no centra la matriz, la calcula desde [0,0,0] y el modulo de los vectores es 1

        Dim TT(3, 3) As Double 'creo la matriz de covarianza
        Dim lista_datos As New Muestra_ejes 'creo la lista donde van a ir los xyz - si en pcipio son dec/inc primero lo voy a transformar

        If TypeOf objeto Is Muestra_direcciones Or TypeOf objeto Is Muestra_ejes Then 'si el objeto de entrada es una lista de direccionales o xyz

            If TypeOf objeto Is Muestra_direcciones Then

                Dim lista_direccional_tmp As Muestra_direcciones = CType(objeto, Muestra_direcciones) 'casteo el objeto de entrada como una lista de direccionales para sacar cada dato como xyz y agregarlo a la lista de xyz

                For i = 0 To lista_direccional_tmp.lista_direcciones.Count - 1

                    Dim xyz_tmp As New Dato_eje

                    xyz_tmp.cos_alfa = Math.Cos(lista_direccional_tmp.lista_direcciones(i).direccion_inclinacion) * Math.Cos(lista_direccional_tmp.lista_direcciones(i).inclinacion) 'y
                    xyz_tmp.cos_beta = Math.Sin(lista_direccional_tmp.lista_direcciones(i).direccion_inclinacion) * Math.Cos(lista_direccional_tmp.lista_direcciones(i).inclinacion) 'x
                    xyz_tmp.cos_gama = Math.Sin(lista_direccional_tmp.lista_direcciones(i).inclinacion) 'z                  

                    lista_datos.lista_ejes.Add(xyz_tmp)

                Next i
            Else
                lista_datos = CType(objeto, Muestra_ejes)
            End If
        Else
            MsgBox("error en la entrada para calcular autovalores") ' salva el caso de que la entrada sea cualquier cosa, es decir, que no sea una lista de datos direcc o xyz
        End If

        For i = 0 To lista_datos.lista_ejes.Count - 1
            REM ORIENTATION MATRIX
            TT(1, 1) = TT(1, 1) + (lista_datos.lista_ejes(i).cos_alfa) ^ 2
            TT(1, 2) = TT(1, 2) + (lista_datos.lista_ejes(i).cos_alfa * lista_datos.lista_ejes(i).cos_beta)
            TT(1, 3) = TT(1, 3) + (lista_datos.lista_ejes(i).cos_alfa * lista_datos.lista_ejes(i).cos_gama)
            TT(2, 1) = TT(1, 2)
            TT(2, 2) = TT(2, 2) + (lista_datos.lista_ejes(i).cos_beta) ^ 2
            TT(2, 3) = TT(2, 3) + (lista_datos.lista_ejes(i).cos_beta * lista_datos.lista_ejes(i).cos_gama)
            TT(3, 1) = TT(1, 3)
            TT(3, 2) = TT(2, 3)
            TT(3, 3) = TT(3, 3) + (lista_datos.lista_ejes(i).cos_gama) ^ 2
            REM FIN ORIENTATION MATRIX
        Next i

        Dim V(3, 3) As Double
        Dim D(3) As Double

        If Double.IsNaN(TT(1, 1)) Or Double.IsNaN(TT(1, 2)) Or Double.IsNaN(TT(1, 3)) Or Double.IsNaN(TT(2, 1)) Or Double.IsNaN(TT(2, 2)) Or Double.IsNaN(TT(2, 3)) Or Double.IsNaN(TT(3, 1)) Or Double.IsNaN(TT(3, 2)) Or Double.IsNaN(TT(3, 3)) Then '<> double.NaN Or TT(1, 2) <> double.NaN Or TT(1, 3) <> double.NaN Or TT(2, 2) <> double.NaN Or TT(2, 3) <> double.NaN Or TT(3, 3) <> double.NaN Then ' para evitar que se rompa
        Else
            Jacobi(TT, 3, 3, lista_datos.lista_ejes.Count, V, D)
        End If

        Dim max_i As Integer
        Dim int_i As Integer
        Dim min_i As Integer

        Dim maximo As Double = -100000000
        Dim minimo As Double = 100000000

        For i = 1 To 3
            If D(i) > maximo Then
                maximo = D(i)
                max_i = i
            End If
        Next i

        For i = 1 To 3
            If D(i) < minimo Then
                minimo = D(i)
                min_i = i
            End If
        Next i

        For i = 1 To 3
            If i <> min_i And i <> max_i And i <> 0 Then
                int_i = i
            End If
        Next

        eje_mayor.modulo = D(max_i)
        eje_intermedio.modulo = D(int_i)
        eje_menor.modulo = D(min_i)

        eje_mayor.cos_alfa = V(1, max_i)
        eje_mayor.cos_beta = V(2, max_i)
        eje_mayor.cos_gama = V(3, max_i)

        eje_intermedio.cos_alfa = V(1, int_i)
        eje_intermedio.cos_beta = V(2, int_i)
        eje_intermedio.cos_gama = V(3, int_i)

        eje_menor.cos_alfa = V(1, min_i)
        eje_menor.cos_beta = V(2, min_i)
        eje_menor.cos_gama = V(3, min_i)

    End Sub
End Class

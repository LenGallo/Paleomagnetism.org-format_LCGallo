Public Class Dato_direccional_rad
    Public direccion_inclinacion As Double
    Public inclinacion As Double
    Public A95 As Double
End Class

Public Class Dato_eje
    Public modulo As Double
    Public cos_alfa As Double
    Public cos_beta As Double
    Public cos_gama As Double
End Class

Public Class GMAP_VGPS

    Public nombre As String
    Public xyz_sp As New Dato_eje
    Public Dir_sp As New Dato_direccional_rad
    Public Dir_geo As New Dato_direccional_rad
    Public Dir_tc As New Dato_direccional_rad

    Public orientation As New Dato_direccional_rad
    Public bedding As New Dato_direccional_rad


    Public intensidad As Single
    Public A95 As Single
    Public vgp As New Dato_direccional_rad
    Public paleolat As Single
    Public paso_lavado As Single

    Public espesor As Single
    Public n As Integer

End Class

Public Class Muestra_ejes
    Public lista_ejes As New List(Of Dato_eje)
End Class

Public Class Muestra_direcciones
    Public lista_direcciones As New List(Of Dato_direccional_rad)
End Class

Public Class Woodcock_parameters_MAD

    Public eigenval_may As Double
    Public eigenval_int As Double
    Public eigenval_men As Double
    Public lineation As Double
    Public foliation As Double
    Public colinearity As Double
    Public coplanarity As Double
    Public MAD As Double
    Public n As Double

End Class

Public Class Salida_montecarlo_naive 'esto es como una fila que dice los parametros de la muestra, y la max GCD obtenida con el BST

    Public lista_woodcock_min As New Woodcock_parameters_MAD 'incluye F_min, L_min, K_min y M_min
    Public lista_woodcock_media As New Woodcock_parameters_MAD
    Public lista_woodcock_max As New Woodcock_parameters_MAD
    Public MAD As Double
    Public lista_woodcock_BST As New Woodcock_parameters_MAD
    Public max_GCD_95 As Double
    Public n As Integer

End Class

Public Class fisher_statistics '

    'Public muestra As New Muestra_direcciones

    Public n As Integer
    Public mean As New Dato_direccional_rad
    Public kappa As Double
    ' Public MAD As Double
    Public aMAD As Double
    Public R As Double
    Public Rm As Double
    Public A95 As Double

End Class

Public Class relating_k_maxGCD ' es la salida de generar una muestra fisheriana random de entrada [k,n]. hago estadistica de fisher de la salida. y tambien hago el max_gcd_95 haciendo bootstrap

    Public kapp_entrada As Double
    Public n As Double

    Public fisher_salida As fisher_statistics
    Public aMAD As Double

    Public max_GCD_95 As Single

End Class

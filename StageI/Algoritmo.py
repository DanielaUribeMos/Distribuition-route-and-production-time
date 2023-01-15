from cmath import inf, sin, cos, asin, sqrt
import cmath
from math import radians
from pickle import FALSE
import queue as Q
import os
from fpdf import FPDF

#----------------------------------------------------------------
#LECTURA DEL ARCHIVO
# to get the location of the current python file py -m pip
basedir = os.path.dirname(os.path.abspath(__file__))
# to join it with the filename
categorization_file = os.path.join(basedir,'Coordenadas.txt')
archivo=open(categorization_file)
camiones=int(archivo.readline())

#ARCHIVO SALIDA
pdf = FPDF()
pdf.add_page()
pdf.set_font("Arial", size = 15)
imagen1 = os.path.join(basedir,'Uniandes.png')
pdf.image(imagen1, x = 5, y = 5, w = 100, h = 55, type = '', link = '')
imagen2= os.path.join(basedir,'Empacor.png')
pdf.image(imagen2, x = 100, y = 5, w = 100, h = 55, type = '', link = '')
pdf.cell(200, 10, ln = 1, align = 'C')
pdf.cell(200, 10, ln = 1, align = 'C')
pdf.cell(200, 10, ln = 1, align = 'C')
pdf.cell(200, 10, ln = 1, align = 'C')
pdf.cell(200, 10, ln = 1, align = 'C')
pdf.set_text_color(46,105,171)
pdf.cell(200, 10, txt = "Ruta mínima recorrido clientes", 
        ln = 1, align = 'C')
pdf.cell(200, 10, ln = 1, align = 'C')
pdf.cell(200, 10, ln = 1, align = 'C')

#----------------------------------------------------------------
#CAMIONES

for camion in range(camiones):

    cantidad=int(archivo.readline())
    ubicaciones=[]

    for i in range(cantidad+1):
        ubicacion=list(map(float, archivo.readline().split()))
        ubicaciones.append(ubicacion)
    #----------------------------------------------------------------


    #----------------------------------------------------------------
    #FUNCIÓN DE CONVERSIÓN


    #Código de conversión a km tomado de : https://www.geeksforgeeks.org/degrees-and-radians-in-python/
    def distance(lat1, lat2, lon1, lon2):
        
        # The math module contains a function named
        # radians which converts from degrees to radians.
        lon1 = radians(lon1)
        lon2 = radians(lon2)
        lat1 = radians(lat1)
        lat2 = radians(lat2)
        
        # Haversine formula
        dlon = lon2 - lon1
        dlat = lat2 - lat1
        a = sin(dlat / 2)**2 + cos(lat1) * cos(lat2) * sin(dlon / 2)**2

        c = 2 * asin(sqrt(a))
        
        # Radius of earth in kilometers. Use 3956 for miles
        r = 6371
        
        # calculate the result
        return(c * r)
    #----------------------------------------------------------------


    #----------------------------------------------------------------
    #DISTANCIAS Y ADYACENCIAS

    listaAdyacencia=[ [] for i in range(cantidad+1)]
    listaNodos=[i for i in range(cantidad+1)]

    def agregarNodo(porigen, pdestino, peso):
        listaAdyacencia[porigen].append([peso, porigen, pdestino])
        listaAdyacencia[pdestino].append([peso, pdestino, porigen])

    for i in range(cantidad +1):
        for j in range(i+1, cantidad +1):
            latitud1=ubicaciones[i][1]
            longitud1= ubicaciones[i][2]
            latitud2=ubicaciones[j][1]
            longitud2=ubicaciones[j][2]

            dist= distance(latitud1, latitud2, longitud1, longitud2)
            dist2= round(dist.real,2)
            agregarNodo(i,j,dist2)
    #----------------------------------------------------------------


    #----------------------------------------------------------------
    #PRIM

    def primAlg():
        mst=[]
        visitados=[]
        pQueue= Q.PriorityQueue()
        visitados.append(listaNodos[0])

        for i in listaAdyacencia[0]:
            pQueue.put(i)
        
        while not pQueue.empty():
            actual=pQueue.get()

            if( not visitados.__contains__(actual[2])):
                visitados.append(actual[2])
                mst.append(actual)

                for i in listaAdyacencia[actual[2]]:
                    if( not visitados.__contains__(i[2])):
                        pQueue.put(i)
        return mst

    respuesta= primAlg()
    #----------------------------------------------------------------


    #----------------------------------------------------------------
    #IMPRESIÓN PDF

    texto= "Camión " + str(camion+1)
    pdf.cell(200, 10, txt = texto, ln = 1, align = 'L')

    #----------------------------------------------------------------


    #----------------------------------------------------------------
    #CICLO HAMILTONIANO

    matriz=[[0 for i in range(cantidad +1)] for j in range(cantidad +1)]
    for i in range(cantidad +1):
        for j in range(cantidad +1):
            existe=False
            for k in range(len(respuesta)):
                if respuesta[k][1]==i and respuesta[k][2]==j:
                    existe=True
            if existe==True:
                matriz[i][j]=1
                matriz[j][i]=1

    def isSafe(path, pos, v):
        
        if matriz[path[pos-1]][v]==0:
            return False
        for i in range(len(path)):
            if path[i]==v:
                return False  
        return True

    def hamiltoniano(path, pos):
        if pos== (cantidad +1):
            return True

        for i in range(1, cantidad+1):
            if isSafe(path, pos, i)==True:
                path[pos]=i
                lista=[]
                for i in range(len(path)):
                    if path[i]!= -1:
                        lista.append(path[i])
                pdf.cell(200, 10, txt = str(lista), ln = 1, align = 'L')
                if hamiltoniano(path, pos +1)==True:
                    return True
                path[pos]=-1
        return False


    path=[-1]*(cantidad +1)
    path[0]=0
    ciclo=hamiltoniano(path, 1)
    pdf.cell(200, 10, ln = 1, align = 'C')
    #----------------------------------------------------------------

#----------------------------------------------------------------
#FIN

pdf.output("Ruta.pdf")
#----------------------------------------------------------------

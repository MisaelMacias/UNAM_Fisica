#!/usr/bin/env python
# coding: utf-8

# # SERIES DE TAYLOR SOBRE EL 0 PARA SENO Y COSENO
# 

# ## Macías Márquez Misael Iván

# Este programa aproxima las funciones seno y coseno en un valor por series de Taylor sobre el 0 (series de McLaurin)

# In[3]:


import numpy as np
import math as mh

temp = "s"
op = ""
k = 0
x = 0.
y = 0
z = 0.
sen = 0.
senr = 0.
cos = 0.
cosr = 0.
epsilon = 0.
lista = np.zeros([150,])


while ((temp == "S") or (temp == "s")):
#bucle para mantener el programa corriendo hasta que se requiera
    op = input("¿Qué función quieres aproximar?(sen/cos) \n")
    
    try :
    #excepción para mantener el flujo del programa si se ingresa un valor inválido
    
        epsilon = float(input("¿Cuál es el tolerancia permitida(diferencia entre el valor aproximado y el \"real\")? \n"))
        
    except :
        
        print("valor inválido :C")
        
    else :
        
        try:
            #excepción para mantener el flujo del programa si se ingresan valores inválidos
            
                k = int(input("¿Cuántos términos quieres?(máximo 150)\n"))
                #print(type(k))
                x = float(input("¿Cuál es el valor a evaluar?\n"))
                #print(type(x))
            
        except :
            
            print("valor inválido :C")
            
        else : 
            
                y = mh.floor(x / np.pi)  % 2
                
                if (np.absolute(x) > 2 * np.pi):
                #declaración para evitar problemas de rango sobre el cálculo de los elementos de la serie 
                
                    z = x - (((mh.floor(x / np.pi)) - y) * np.pi)
                    
                else :
                    z = x
                    
                   
                
                if ((k > 0) and (k <= 150)) :
                #declaración para comprobar validez de parámetros
                
                    if op == "sen":
                    #declaración para comprobar validez de la opción elegida
                    
                        for i in range(k):
                        #bucle para producir los elementos de la serie
                    
                            lista[i] = (-1)**i/(np.math.factorial(2*i + 1)) * z**(2*i + 1)
            
            
                        sen = np.sum(lista)
                        senr = np.sin(x)
            
                        print("El valor aproximado del seno en " + str(x) + " es : " + str(sen))
                        #print(lista)
                        print("El valor \"real\"(obtenido por la función nativa de numpy [np.sin(" + str(x) + ")]) es : " + str(senr))
            
                        if (np.absolute(sen - senr) >  epsilon) :
                        #declaración para verificar tolerancia permitida
                
                            print("Deberías aumentar los térmnos de la serie :C")
                
                        else:
                            print("Parece una buena aproximación C:")
                
                    elif op == "cos":
                    #declaración para comprobar validez de la opción elegida
                    
                        for i in range(k):
                        #bucle para producir los elementos de la serie
                
                            lista[i] = float((-1)**i/(np.math.factorial(2*i)) * z**(2*i))
            
            
                        cos = np.sum(lista)
                        cosr = np.cos(x)
            
                        print("El valor aproximado del cos en " + str(x) + " es : " + str(cos))
                        #print(lista)
                        print("El valor \"real\"(obtenido por la función nativa de numpy [np.cos(" + str(x) + ")]) es : " + str(cosr))
            
                        if (np.absolute(cos - cosr) > epsilon) :
                
                            print("Deberías aumentar los térmnos de la serie :C")
                
                        else:
                        
                            print("Es una buena aproximación C:")
                            
                    else :
                        
                        print("Esa opción no está disponible :C")
                    
                else :
                    
                    print("Ingresaste un valor fuera de rango :C")
                    

    temp = input("¿Quieres intentarlo de nuevo?(S/N) \n")
    

   
print("bye C:")


# Los resultados parecen ser buenos aunque los valores de los parametros tienen que ser relativamente pequeños debido a que se puede generar un error de rango en el calculo de los elementos de la serie debido a que se supera la capacidad de una variable tipo float
# 

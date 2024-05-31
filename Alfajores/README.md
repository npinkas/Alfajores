Parcial: alfajores

Debe implementarse una solución que resuelva lo planteado a continuación haciendo uso de las herramientas del paradigma.
No está permitido usar recursividad en ningún punto a menos que se indique lo contrario en el mismo.
Debe haber al menos un uso de composición no trivial en la resolución del parcial.

Nos contrató un kiosco que necesita procesar la venta de la clásica golosina argentina a diversos tipos de clientes que acuden al establecimiento. Para ello, modelaremos el dominio usando las herramientas del paradigma funcional.

Parte 1: ¿qué es un alfajor?
El kiosco nos indicó que un alfajor presenta las siguientes características:
- tiene una o varias capas de relleno (siendo los posibles rellenos dulce de leche, mousse o fruta);
- tiene un peso medido en gramos.
- tiene un dulzor innato (el cual es representado con un número).
- tiene un nombre.

Dadas estas características, queremos representar los siguientes alfajores:
i) Jorgito, que es un alfajor de una capa de dulce de leche, pesa 80g, tiene un dulzor de 8 y su nombre es “Jorgito”,
ii) Havanna, que es un alfajor de dos capas de mousse, pesa 60g, tiene un dulzor de 12 y su nombre es “Havanna”,
iii) Capitán del espacio, que es un alfajor de una capa de dulce de leche, pesa 40g, tiene un dulzor de 12 y su nombre es “Capitán del espacio”.

También, queremos poder calcular las siguientes propiedades de un alfajor:
i) el coeficiente de dulzor de un alfajor: indica cuánto dulzor por gramo tiene el alfajor; se calcula dividiendo su dulzor sobre su peso.
ii) el precio de un alfajor: se calcula como el doble de su peso sumado a la sumatoria de los precios de sus rellenos. Una capa de relleno de dulce de leche cuesta $12; una de mousse, $15; una de fruta, $10.
iii) si un alfajor es potable: lo es si tiene al menos una capa de relleno (¿dónde se ha visto un alfajor sin relleno?), todas sus capas son del mismo sabor, y su coeficiente de dulzor es mayor o igual que 0,1.

Parte 2: escalabilidad vertical
Nos notificaron desde el kiosco que abrieron una planta de producción, en la cual comenzaron a experimentar sobre (o modificar) alfajores existentes, para tener más variedad. Nuestro software debería poder reflejar las siguientes modificaciones:
a) abaratar un alfajor: reduce su peso en 10g y su dulzor en 7. 
b) renombrar un alfajor, que cambia su packaging dándole un nombre completamente nuevo.
c) agregar una capa de cierto relleno a un alfajor.
d) hacer premium a un alfajor: dado un alfajor, le agrega una capa de relleno (del mismo tipo de relleno que ya tiene), y lo renombra a su nombre original + la palabra "premium" al final. Sólo los alfajores potables pueden hacerse premium; si se intenta hacer premium un alfajor no potable, el alfajor queda exactamente igual que como estaba.
Ejemplo: dado un alfajor Havanna, hacerlo premium lo convertiría en un alfajor con tres capas de mousse llamado “Havanna premium”.
e) hacer premium de cierto grado a un alfajor: consiste en hacerlo premium varias veces. Este punto puede ser resuelto usando recursividad.
f) Modelar los siguientes alfajores:
i) Jorgitito, que es un Jorgito abaratado, y cuyo nombre es “Jorgitito”.
ii) Jorgelín, que es un Jorgito pero con una capa extra de dulce de leche, y cuyo nombre es “Jorgelín”.
iii) Capitán del espacio de costa a costa: es un capitán del espacio abaratado, luego hecho premium de grado 4, y luego renombrado a “Capitán del espacio de costa a costa”.

Parte 3: clientes del kiosco
Queremos también representar a los clientes de nuestro kiosco y registrar cuánto dinero tienen para gastar y qué alfajores nos compraron.

Sobre gustos no hay nada escrito. Cada cliente tiene diferentes criterios respecto a los alfajores. A un cliente le gusta un alfajor si cumple con todos sus criterios.

Queremos modelar los siguientes clientes (todos comienzan sin alfajores comprados):

Emi: tiene $120; es busca marca de los capitanes del espacio, lo que significa que solo le gustan alfajores que contengan en su nombre “Capitán del espacio”.

Tomi: tiene $100; es pretencioso (solo le gustan los alfajores que contienen “premium” en su nombre*), y dulcero (le gustan los alfajores cuyo coeficiente de dulzor es mayor a 0,15).

Ejemplo: no le gusta el Jorgelín premium porque a pesar de ser premium, su índice de dulzor es menor a 0,15, así que sólo cumple 1 de sus criterios, no todos. En cambio, el Havanna premium sí cumple ambos, así que ese alfajor le gusta.
*ojo: premium no es una marca.

Dante: tiene $200; es anti-dulce de leche, por lo que los alfajores que le gustan no deben tener ninguna capa de dulce de leche, y además es extraño, lo que significa que solo le gustan alfajores que no son potables.

Juan: tiene $500; es dulcero, busca marca de Jorgito, pretencioso y anti-mousse.

b. indicar, dada una lista de alfajores, cuáles le gustan a cierto cliente.

c. que un cliente pueda comprar un alfajor: esto lo agrega a su lista de alfajores actuales, y gasta el dinero correspondiente al precio del alfajor. Si no tiene suficiente plata, no lo compra y queda como está.

d. que un cliente compre, de una lista de alfajores, todos aquellos que le gustan.

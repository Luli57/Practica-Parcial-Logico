
% Punto 1

% kioskero(Persona, Dia, Entrada, Salida).
kioskero(dodain, lunes, 9, 15).
kioskero(dodain, miercoles, 9, 15).
kioskero(dodain, viernes, 9, 15).
kioskero(lucas, martes, 10, 20).
kioskero(juanC, sabados, 18, 22).
kioskero(juanC, domingos, 18, 22).    
kioskero(juanFdS, jueves, 10, 20).
kioskero(juanFdS, viernes, 12, 20).    
kioskero(leoC, lunes, 14, 18).    
kioskero(leoC, miercoles, 14, 18). 
kioskero(martu, miercoles, 23, 24).
kioskero(vale, Dia, Entrada, Salida):-
    kioskero(juanC, Dia, Entrada, Salida).
kioskero(vale, Dia, Entrada, Salida):-
    kioskero(dodain, Dia, Entrada, Salida).

% - nadie hace el mismo horario que leoC
% por principio de universo cerrado, no agregamos a la base de conocimiento aquello que no tiene sentido agregar
% - maiu está pensando si hace el horario de 0 a 8 los martes y miércoles
% por principio de universo cerrado, lo desconocido se presume falso

% Punto 2

quienAtiende(Dia, Hora, Persona):-
    kioskero(Persona, Dia, Entrada, Salida),
    between(Entrada, Salida, Hora).

% Punto 3

foreverAlone(Persona, Dia, Hora) :-
    quienAtiende(Dia, Hora, Persona),
    not(atiendeConAlguien(Persona, Dia, Hora)).

atiendeConAlguien(Persona, Dia, Hora) :-
    quienAtiende(Dia, Hora, Persona),
    quienAtiende(Dia, Hora, Persona2),
    Persona \= Persona2.

% Punto 4

posibilidadesAtencion(Dia, Personas):-
    findall(Persona, distinct(Persona, quienAtiende(Persona, Dia, _)), PersonasPosibles),
    combinar(PersonasPosibles, Personas).
  
combinar([], []).
combinar([Persona|PersonasPosibles], [Persona|Personas]):-
    combinar(PersonasPosibles, Personas).
combinar([_|PersonasPosibles], Personas):-
    combinar(PersonasPosibles, Personas).

% Punto 5

venta(dodain, fecha(10, 8), [golosinas(1200), cigarrillos(jockey), golosinas(50)]).
venta(dodain, fecha(12, 8), [bebidas(true, 8), bebidas(false, 1), golosinas(10)]).
venta(martu, fecha(12, 8), [golosinas(1000), cigarrillos([chesterfield, colorado, parisiennes])]).
venta(lucas, fecha(11, 8), [golosinas(600)]).
venta(lucas, fecha(18, 8), [bebidas(false, 2), cigarrillos([derby])]).

personaSuertuda(Persona):-
  venta(Persona, _, _),
  forall(venta(Persona, _, [Venta|_]), ventaImportante(Venta)).

ventaImportante(golosinas(Precio)) :-
    Precio > 100.
ventaImportante(cigarrillos(Marcas)) :-
    length(Marcas, CantidadDeMarcas),
    CantidadDeMarcas > 2.
ventaImportante(bebidas(true, _)).
ventaImportante(bebidas(_, Cantidad)) :-
    Cantidad > 5.

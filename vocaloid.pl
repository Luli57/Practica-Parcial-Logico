
% vocaloid(Nombre, Cancion).
% cancion(Cancion, Duracion).

vocaloid(megurineLuka, nigthFever, 4).
vocaloid(megurineLuka, foreverYoung, 5).
vocaloid(hatsuneMiku, tellYourWorld, 4).
vocaloid(gumi, foreverYoung, 4).
vocaloid(gumi, tellYourWorld, 5).
vocaloid(seeU, novemberRain, 6).
vocaloid(seeU, nigthFever, 5).
vocaloid(kaito, _, _).


% 1

cantanteNovedoso(Vocaloid):-
    vocaloid(Vocaloid, _, _),
    cantidadCancionesyMinutos(Vocaloid, Canciones, MinutosTotales),
    Canciones >= 2,
    MinutosTotales < 15.

cantidadCancionesyMinutos(Vocaloid, Canciones, MinutosTotales):-
    vocaloid(Vocaloid, _, _),
    findall(Cancion, vocaloid(Vocaloid, Cancion, _), Canciones1),
    length(Canciones1, Canciones),
    findall(Minuto, vocaloid(Vocaloid, _, Minuto), Minutos),
    sumlist(Minutos, MinutosTotales).

% sabeAlMenos2Canciones(Cantante):-
%   vocaloid(Cantante, Cancion1, _), 
%   vocaloid(Cantante, Cancion2, _),
%   Cancion1 \= Cancion2.

% 2

cantanteAcelerado(Vocaloid):-
    vocaloid(Vocaloid, Cancion, _),
    not(cancionMayorA4Min(Cancion)).
    
cancionMayorA4Min(Cancion):-
    vocaloid(_, Cancion, Minutos),
    Minutos > 4.
    
% 3

% concierto(Nombre, Pais, Fama, Tipo).
% tipoConcierto(Tipo).

concierto(mikuExpo, estadosUnidos, 2000, gigante(2, 6)).
concierto(magicalMiria, japon, 3000, gigante(3, 10)).
concierto(vocalektVisions, estadosUnidos, 1000, mediano(9)).
concierto(mikuFest, argentina, 100, pequeno(4)).

% 4

puedeCantarEn(Concierto, Vocaloid):-
    concierto(Concierto, _, _, Requisitos),
    vocaloid(Vocaloid, _, _),
    Vocaloid \= hatsuneMiku,
    cumpleRequisitos(Vocaloid, Requisitos).

cumpleRequisitos(Vocaloid, gigante(CantCanciones, CantMinutos)):-
    cantidadCancionesyMinutos(Vocaloid, Canciones, Minutos),
    Canciones > CantCanciones,
    Minutos > CantMinutos.
cumpleRequisitos(Vocaloid, mediano(TiempoMax)):-
    cantidadCancionesyMinutos(Vocaloid, _, Minutos),
    Minutos < TiempoMax.
cumpleRequisitos(Vocaloid, pequeno(TiempoMin)):-
    vocaloid(Vocaloid, _, Minutos),
    Minutos > TiempoMin.

puedeCantarEn(Concierto, hatsuneMiku):-
    concierto(Concierto, _, _, _).

% 5

vocaloidMasFamoso(Vocaloid):-
    vocaloid(Vocaloid, _, _),
    not(vocaloidConMayorFama(Vocaloid)).

nivelDeFama(Vocaloid, FamaTotal):-
    cantidadCancionesyMinutos(Vocaloid, Canciones, _),
    findall(Fama, famaEnUnConcierto(Vocaloid, Fama), Famas),
    sumlist(Famas, Famas),
    FamaTotal is Famas * Canciones.

famaEnUnConcierto(Vocaloid, Fama):-
    puedeCantarEn(Vocaloid, Concierto),
    concierto(Concierto, _, Fama, _).
    
vocaloidConMayorFama(Vocaloid):-
    nivelDeFama(Vocaloid, Fama),
    nivelDeFama(Vocaloid2, Fama2),
    Vocaloid \= Vocaloid2,
    Fama =< Fama2.

% 6

conoceA(megurineLuka, hatsuneMiku).
conoceA(megurineLuka, gumi).
conoceA(gumi, seeU).
conoceA(seeU, kaito).

conocido(Persona1, Persona2):-
    conoceA(Persona1, Persona2).

conocido(Persona1, Persona2):-
    conoceA(Persona1, Persona3),
    conoceA(Persona3, Persona2).

unicoParticipante(Concierto, Vocaloid):-
    puedeCantarEn(Concierto, Vocaloid),
    conocido(Vocaloid, Persona1),
    not(puedeCantarEn(Concierto, Persona1)).

% 7

% En la solución planteada habría que agregar una claúsula en el predicado cumpleRequisitos/2  que tenga en cuenta el nuevo functor con sus respectivos requisitos 
% El concepto que facilita los cambios para el nuevo requerimiento es el polimorfismo, que nos permite dar un tratamiento en particular a cada uno de los conciertos en la cabeza de la cláusula.


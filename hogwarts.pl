
% Parte 1

% hechizero(Nombre, Sangre, Cualidades, CasaOdiada).
hechizero(harry, mestiza, cualidades([coraje, amistad, orgullo, inteligencia]), slytherin).
hechizero(draco, pura, cualidades([inteligencia, orgullo]), hufflepuff).
hechizero(hermione, impura, cualidades([inteligencia, ogullo, responsabilidad]), ninguna).

casa(gryffindor).
casa(slytherin).
casa(ravenclaw).
casa(hufflepuff).


% 1

permiteEntrar(Casa, Mago) :-
    hechizero(Mago, _, _, _),
    casa(Casa),
    Casa \= slytherin.

permiteEntrar(slytherin, Mago):-
    hechizero(Mago, _, _, _),
    not(hechizero(Mago, impura, _, _)).

% 2

caracterApropiado(Casa, Mago):-
    hechizero(Mago, _, cualidades(Cualidades), _),
    casa(Casa),
    condicionesCasa(Casa, Cualidades).

condicionesCasa(gryffindor, Cualidades):-
    member(coraje, Cualidades).
condicionesCasa(slytherin, Cualidades):-
    member(orgullo, Cualidades),
    member(inteligencia, Cualidades).
condicionesCasa(ravenclaw, Cualidades):-
    member(responsabilidad, Cualidades),
    member(inteligencia, Cualidades).
condicionesCasa(hufflepuff, Cualidades):-
    member(amistad, Cualidades).

% 3

magoOdiaCasa(Casa, Mago):-
    hechizero(Mago, _, _, Casa).

podriaQuedarEn(Casa, Mago):-
    permiteEntrar(Casa, Mago), 
    caracterApropiado(Casa, Mago), 
    not(magoOdiaCasa(Casa, Mago)).

podriaQuedarEn(gryffindor, hermione).

% 4

cadenaDeAmistad(Magos):-
    findall(Mago, magosAmistosos(Mago), Magos).

magosAmistosos(Mago):-
    hechizero(Mago, _, cualidades(Cualidades), _),
    member(amistad, Cualidades).

podriaQuedarEn(Casa, Mago):-
    cadenaDeAmistad(Magos), 
    member(Mago, Magos),
    member(Mago2, Magos), 
    podriaQuedarEn(Casa, Mago2)
    Mago \= Mago2.

% Parte 2

accion(buena, ajedrezMagico, 50).
accion(buena, salvarAmigos, 50).
accion(buena, ganarVoldemort, 60).
accion(mala, andarDeNoche, -50).
accion(mala, Accion, Puntaje):-
    lugarProhibido(Accion, Puntaje).

lugarProhibido(bosque, -50).
lugarProhibido(seccionRestringidaBiblioteca, -10).
lugarProhibido(tercerPiso, -75).

magoAccion(harry, andarDeNoche).
magoAccion(hermione, tercerPiso).
magoAccion(hermione, seccionRestringidaBiblioteca).
magoAccion(harry, bosque).
magoAccion(harry, tercerPiso).
magoAccion(draco, mazmorras).
magoAccion(ron, ajedrezMagico).
magoAccion(hermione, salvarAmigos).
magoAccion(harry, ganarVoldemort). 
    
esDe(hermione, gryffindor).
esDe(ron, gryffindor).
esDe(harry, gryffindor).
esDe(draco, slytherin).
esDe(luna, ravenclaw).

% 1

buenAlumno(Mago):-
    magoAccion(Mago, _),
    forall(magoAccion(Mago, Accion), accion(buena, Accion, _)).

accionRecurrente(Accion):-
    magoAccion(Mago, Accion),
    magoAccion(Mago2, Accion),
    Mago \= Mago2.

% 2

puntajeTotalCasa(Casa, Total) :-
    casa(Casa), 
    findall(Puntaje, puntajeCasa(Casa, Puntaje), Puntajes),
    sumlist(Puntajes, Total).

puntajeCasa(Casa, Puntaje):-
    esDe(Mago, Casa),
    magoAccion(Mago, Accion),
    accion(_, Accion, Puntaje).

% 3

casaGanadora(Casa):-
    casa(Casa),
    not(casaConMayorPuntaje(Casa)).

casaConMayorPuntaje(Casa):-
    puntajeTotalCasa(Casa, Total),
    puntajeTotalCasa(Casa2, Total2),
    Casa \= Casa2,
    Total < Total2.

% 4

pregunta(dondeEstaBezoar, 20, snape).
pregunta(levitarPluma, 25, flitwick).

magoAccion(hermione, dondeEstaBezoar).
magoAccion(hermione, levitarPluma).

accion(buena, Pregunta, Puntaje):-
    pregunta(Pregunta, _, Profesor),
    puntajeSegunProfesor(Profesor, Pregunta, Puntaje).

puntajeSegunProfesor(Profesor, Pregunta, Puntaje):-
    pregunta(Pregunta, Puntaje, Profesor),
    Profesor \= snape.

puntajeSegunProfesor(snape, Pregunta, Puntaje):-
    pregunta(Pregunta, Dificultad, snape),
    Puntaje is Dificultad / 2.

    

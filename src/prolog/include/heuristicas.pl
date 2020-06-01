

%---------------------------------------------------------------------------------------------
                        /* Heurísticas para os problemas */
%---------------------------------------------------------------------------------------------

% 1) Distancia euclidiana entre dois pontos (simplificado para latitude e longitude)

%       Exemplo: distanciaEuclidiana(183, 595, D).

distanciaEuclidiana(N1, N2, Distance) :-
        paragem(N1,Lat1,Long1,_,_,_,_,_,_,_),
        paragem(N2,Lat2,Long2,_,_,_,_,_,_,_),
        X is (Lat2-Lat1), 
        Y is (Long2-Long1),
        Distance is sqrt(X^2 + Y^2)/1000.

%---------------------------------------------------------------------------------------------

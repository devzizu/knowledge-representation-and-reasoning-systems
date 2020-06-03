

%---------------------------------------------------------------------------------------------
                        /* Heurísticas para os problemas */
%---------------------------------------------------------------------------------------------

% 1) Distancia euclidiana entre dois pontos (Considerando o realismo da latitude e longitude).

%       Exemplo: distanciaEuclidiana(183, 595, D).

distanciaEuclidiana(Node1, Node2, DistanceCost) :-
        paragem(Node1,Lat1,Long1,_,_,_,_,_,_,_),
        paragem(Node2,Lat2,Long2,_,_,_,_,_,_,_),
        ValPi is pi,
        Fi1 is Lat1 * (ValPi/180),
        Fi2 is Lat2 * (ValPi/180),
        DeltaFi is (Lat2-Lat1) * (ValPi/180),
        DeltaLambda is (Long2-Long1) * (ValPi/180),
        A1 is sin(DeltaFi/2) * sin(DeltaFi/2),
        A2 is cos(Fi1) * cos(Fi2),
        A3 is sin(DeltaLambda/2) * sin(DeltaLambda/2),
        ASum is A1 + A2 * A3,
        C1 is sqrt(ASum),
        C2 is sqrt(1.0 - ASum),
        C is 2 * atan2(C1, C2),
        Dist is 6.371*1000 * C,
        DistanceCost is Dist/1000.

%---------------------------------------------------------------------------------------------

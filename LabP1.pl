
% state(Room, HasSteel, HasBrass, HasPackage).

getRoom(X, [(X, HasSteel, HasBrass, HasPackage) | _], (X, HasSteel, HasBrass, HasPackage)) :- !.
getRoom(X, [_ | Xs], Y) :- getRoom(X, Xs, Y).


changeRoom((Room, A, B, C), [(Room, _, _, _)|Y], [(Room, A, B, C)|Y]) :- !.
changeRoom((Room, A, B, C), [R|Y], [R|Rooms]) :- changeRoom((Room, A, B, C), Y, Rooms), !.

move(
    state(r1, Arr, has, HasBrass, HasPackage),
    walk(r1 -> r2),
    state(r2, Arr, has, HasBrass, HasPackage)  
).

move(
    state(r2, Arr, has, HasBrass, HasPackage),
    walk(r2 -> r1),
    state(r1, Arr, has, HasBrass, HasPackage)  
).

move(
    state(r1, Arr, HasSteel, has, HasPackage),
    walk(r1 -> r3),
    state(r3, Arr, HasSteel, has, HasPackage)  
).

move(
    state(r3, Arr, HasSteel, has, HasPackage),
    walk(r3 -> r1),
    state(r1, Arr, HasSteel, has, HasPackage)  
).

% pickupSteelKey
move(
    state(
        CurrentRoom, RoomArray,
        hasNot, HasBrassKey, HasPackage),
    pickupSteelKey,
    state(
        CurrentRoom, NewRoomArray,
        has, HasBrassKey, HasPackage)
    ) :-
        (HasBrassKey = hasNot, !; HasPackage = hasNot),
        getRoom(CurrentRoom, RoomArray, (Room, RHasSteelKey, RHasBrassKey, RHasPackage)),
        RHasSteelKey = has,
        changeRoom((Room, hasNot, RHasBrassKey, RHasPackage), RoomArray, NewRoomArray).

% pickupBrassKey
move(
    state(
        CurrentRoom, RoomArray,
        HasSteelKey, hasNot, HasPackage),
    pickupBrassKey,
    state(
        CurrentRoom, NewRoomArray,
        HasSteelKey, has, HasPackage)
    ) :-
        (HasSteelKey = hasNot, !; HasPackage = hasNot), % check if we can pickup
        getRoom(CurrentRoom, RoomArray, (Room, RHasSteelKey, RHasBrassKey, RHasPackage)),
        RHasBrassKey = has,
        changeRoom((Room, RHasSteelKey, hasNot, RHasPackage), RoomArray, NewRoomArray).

% pickupPackage
move(
    state(
        CurrentRoom, RoomArray,
        HasSteelKey, HasBrassKey, hasNot),
    pickupPackage,
    state(
        CurrentRoom, NewRoomArray,
        HasSteelKey, HasBrassKey, has)
    ) :-
        (HasSteelKey = hasNot, !; HasBrassKey = hasNot),
        getRoom(CurrentRoom, RoomArray, (Room, RHasSteelKey, RHasBrassKey, RHasPackage)),
        RHasPackage = has,
        changeRoom((Room, RHasSteelKey, RHasBrassKey, hasNot), RoomArray, NewRoomArray).



% dropSteelKey
move(
    state(
        CurrentRoom, RoomArray,
        has, HasBrassKey, HasPackage),
    dropSteelKey,
    state(
        CurrentRoom, NewRoomArray,
        hasNot, HasBrassKey, HasPackage)
    ) :-
        getRoom(CurrentRoom, RoomArray, (Room, _, RHasBrassKey, RHasPackage)),
        changeRoom((Room, has, RHasBrassKey, RHasPackage), RoomArray, NewRoomArray).

% dropBrassKey
move(
    state(
        CurrentRoom, RoomArray,
        HasSteelKey, has, HasPackage),
    dropBrassKey,
    state(
        CurrentRoom, NewRoomArray,
        HasSteelKey, hasNot, HasPackage)
    ) :-
        getRoom(CurrentRoom, RoomArray, (Room, RHasSteelKey, _, RHasPackage)),
        changeRoom((Room, RHasSteelKey, hasNot, RHasPackage), RoomArray, NewRoomArray).

% dropPackage
move(
    state(
        CurrentRoom, RoomArray,
        HasSteelKey, HasBrassKey, has),
    dropPackage,
    state(
        CurrentRoom, NewRoomArray,
        HasSteelKey, HasBrassKey, hasNot)
    ) :-
        getRoom(CurrentRoom, RoomArray, (Room, RHasSteelKey, RHasBrassKey, RHasPackage)),
        RHasPackage = hasNot,
        changeRoom((Room, RHasSteelKey, RHasBrassKey, has), RoomArray, NewRoomArray).


canGet(state(_, _, _, _, has), _, [done]) :- !.
canGet(State1, N, [Move | Trace]) :-
    N > 0,
    N1 is N - 1,
    move(State1, Move, State2), 
    canGet(State2, N1, Trace).



% canGet(state(r1, [(r1, has, hasNot, hasNot),(r2, hasNot, has, hasNot),(r3, hasNot, hasNot, has)], hasNot, hasNot, hasNot), 7, R).
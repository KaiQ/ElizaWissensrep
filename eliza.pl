:- use_module(library(readln)).
% From The Art of Prolog 2nd Ed. (Sterling & Shapiro) S.158
strip_punctuation([], []).
strip_punctuation([Word|Tail], [Word|Result]) :-
    \+(member(Word, ['.', ',', '?', '!', '\n'])),
    strip_punctuation(Tail, Result).
strip_punctuation([_|Tail], Result) :-
    strip_punctuation(Tail, Result).

read_sentence(Input) :-
    readln(Input1, _, ".!?\n", "_0123456789", lowercase),
    strip_punctuation(Input1, Input),
    readln(_, _, _, _, _).
%The default setting for readln/1 is
%   - read up till newline
%   - see underscore('_') and numbers 0-9 as part of words
%   - make lowercase


random_response(R, RL) :- length(RL, Len), X is random(Len), nth0(X, RL, R).


eliza :- read_sentence(Input), eliza_main(Input), !.

eliza_main([X]) :- poweroff(X), reply(['Goodbye. I hope I have helped you.']).
%TODO mehr schluss woerter
%verschiedene Arten der Antwort, die aber das selbe aussagen --> mehr abwechslung
%Easter eggs -> i like trains --> Tschu, Tschu!
eliza_main(Input) :-
    pattern(Stimulus, ResponseList),
    match(Stimulus, Dictionary, Input),
    !,
    random_response(Response, ResponseList),
    match(Response, Dictionary, Output),
    reply(Output),
    read_sentence(Input1),
    eliza_main(Input1).

match([N|Pattern], Dictionary, Target) :-
    integer(N), lookup(N, Dictionary, LeftTarget),
    append(LeftTarget, RightTarget, Target),
    match(Pattern, Dictionary, RightTarget).
match([Word|Pattern], Dictionary, [Word|Target]) :-
    atom(Word), match(Pattern, Dictionary, Target).
match([], _, []).

lookup(Key, [(Key, Value)|_], Value).
lookup(Key, [(Key1, _)|Dictionary], Value) :-
    \=(Key, Key1), lookup(Key, Dictionary, Value).

add_memory(X, Predicate, Y) :-
    %print(X), nl, print(Predicate), nl, print(Y),
    Fact =.. [Predicate, X, Y],
    asserta(Fact).

pattern([i, am, 1], [['How', long, have, you, been, 1, '?'],
                     ['Test', satz, mit, 1, '!']]) :-
    add_memory(i, am, X).
  % String wenn Wort Groß gescrieben wird
pattern([1, you, 2, me], [['What', makes, you, think, 'I', 2, you, '?']]).
pattern([i, like, 1], [['Does', anyone, else, in, your, family, like, 1, '?']]).
pattern([i, feel, 1], [['Do', you, often, feel, that, way, '?']]).
pattern([1, X, 2], [['Please', tell, me, more, about, X, '.']]) :- important(X).
%pattern([remember, X, Predicate, Y], ['I', will, remember, that, '.']) :-
% add_memory(X, Predicate, Y).
%pattern([X, Predicate], ['Is', that, because, X, Predicate, Y, '?']) :-
% call(Predicate, X, Y).
pattern([1], [['Please', go, on, '.'], ['weiter', 'bitte']]).

important(father).
important(mother).
important(sister).
important(brother).
important(son).
important(daughter).

poweroff(bye).
poweroff(cu).
poweroff(tschau).

reply([Head|Tail]) :- write(Head), write(' '), reply(Tail).
reply([]) :- nl.


% From The Art of Prolog 2nd Ed. (Sterling & Shapiro) P.158

% library to be able to read commanline input
:- use_module(library(readln)).

% some string-processing (e.g. commandline input)
% 1. remove unneccessary punctuations
    % recursion end
strip_punctuation([], []).
  
  % if the current word is not any of the special chars, recurse and prepend this word to the result of the recursion
strip_punctuation([Word|Tail], [Word|Result]) :-
  \+(member(Word, ['.', ',', '?', '!', '\n'])),
  strip_punctuation(Tail, Result).
  
  % otherwise, if the word is a special char, we do not care about it (hence _) and recurse
strip_punctuation([_|Tail], Result) :-
  strip_punctuation(Tail, Result).

% 2. read the user input: http://www.swi-prolog.org/pldoc/doc/swi/library/readln.pl?show=src
% properties of the predefined readln-predicate:
% Read a sentence up till NewLine and unify <P> with the list of atoms / numbers
%   -> readln(P, LastCh, Arg1, Arg2, Arg3)
%			  P      : the input up to the first newline-char OR upt to one of the chars specified in Arg1
%             LastCh : the ASCII-code of the last character read
%             Arg1   : List of stop characters, everything afterwards will not be added to Input1, if not provided '\n' will be used as stop-char
%			  Arg2   : List of word_part characters (those characters belong to a word and do not form a word itself, 
%                       e.g. Input: "Hallo1234" -> with Arg2 being set to  "0123456789" we will get [Hallo1234]
%                                               -> with Arg2 not being set we will get [Hallo, 1234]
%  		      Arg3   : uppercase / lowercase conversion
read_sentence(Input) :-
  readln(Input1, _, ".!?\n", "_0123456789", lowercase),
  strip_punctuation(Input1, Input),
  readln(_, _, _, _, _).
%The default setting for readln/1 is
%   - read up till newline
%   - see underscore('_') and numbers 0-9 as part of words
%   - make lowercase

random_response(R, RL) :- length(RL, Len), X is random(Len), nth0(X, RL, R).

% 3. eliza
    % recursion start
eliza :- init_state, read_sentence(Input), eliza_main(Input), !.

    % recusion end, exit eliza
eliza_main([X]) :- poweroff(X), reply(['Goodbye. I hope I have helped you.']).

% recursive function: read input -> choose Stimulus / Response pair, and recurse until 'bye.' is read
    %TODO mehr schluss woerter
    %verschiedene Arten der Antwort, die aber das selbe aussagen --> mehr abwechslung
    %Easter eggs -> i like trains --> Tschu, Tschu!
eliza_main(Input) :-
  pattern(Stimulus, ResponseList),              % choose a certain stimulus and its according response-list
  match(Stimulus, Dictionary, Input),           % find the Stimulus that matches the input, we do not simply copy as we have to identify 'slots' and remember them in the dictionary
  !,                                            % do not look for any other stimuli when found one
  random_response(Response, ResponseList),      % choose one of the Responses from the ResponseList      
  match(Response, Dictionary, Output),          % write the according Response to Output, 'slots' will be replaced be tokens from the dictionary
  reply(Output),                                % write the determined answer to the terminal
  state_transition(Output), 
  read_sentence(Input1),                        % read a new input and recurse
  eliza_main(Input1).

% we check for 'free slots' in the stimulus / response pairs
match([N|Pattern], Dictionary, Target) :-
  integer(N), lookup(N, Dictionary, LeftTarget),        % integer(X) will be true if X is an Integer; replace N with a known word from the dictionary (N is the Key to the Value)
  append(LeftTarget, RightTarget, Target),              % we use append here to determine the remainig words after LeftTarget in Target. e.g. append([1],X,[1,2,3]). --> X = [2, 3].
  match(Pattern, Dictionary, RightTarget).              % go on with the (possibly) enlarged Dictionary and the remaing part of the Target

% if the next word is an atom, we prepand this word to the output and proceed    
match([Word|Pattern], Dictionary, [Word|Target]) :-
  atom(Word), match(Pattern, Dictionary, Target).       % atom(X) will be true if X is an Atom

  % recursion end: we completely processed the Stimulus / Response and the Input / Output
  match([], _, []).

% dictionary operations: the dictionary is respresented as a incomplete list of Key-Value-pairs
% we use the incomplete list to perform value-lookups and insertion of Key-Value-pairs
% recursion end - with two functionalities:
%   a) if the Key is inside the dictionary we perform a lookup here and return its associated value
%   b) if the Key is unknown, we save the new Key-Value-pair in the dictionary   
lookup(Key, [(Key, Value)|_], Value).

% basically we traverse the dictionary here and check the Key ('key1') of each Key-Value pair against the provided key
lookup(Key, [(Key1, _)|Dictionary], Value) :-
  \=(Key, Key1), lookup(Key, Dictionary, Value).

add_memory(X, Predicate, Y) :-
  %print(X), nl, print(Predicate), nl, print(Y),
  Fact =.. [Predicate, X, Y],                       % runtime evaluation: converts [Predicate, X, Y] into a rule: rule(X,Y) with 'rule' being the content of PRedicate
  asserta(Fact).                                    % save fact to the current knowledge-base            

% we simulate a state machine here by dynamically adding/replacing a state-predicate and its associated information
reset_state :-
    abolish(state/1),
    asserta(state('norm')).

family_state :-
    abolish(state/1),
    asserta(state('family')).

feelings_state :-
    abolish(state/1),
    asserta(state('feelings')).

init_state :-
    asserta(state('norm')).

% we check eliza's answer here and change into the according state
% Note: we cannot do the state-transitions within the pattern-clauses, as all of them will be executed everytime
%       we'd change states all the time...
state_transition(Response) :-
    (subset(['Does', anyone, else, in, your, family, like], Response), family_state); 
    (subset(['Do', 'you', 'often', 'feel', 'that', 'way','?'],Response), feelings_state); 
    (subset(['Who', 'is', 'it', '?'],Response), reset_state); 
    (subset(['When', 'exactly', 'do', 'you', 'feel', 'that', 'way','?'],Response), reset_state); 
    true.   % do always return true so we do not disturb the program flow

% all the stimulus<->response pairs: pattern(Stimulus,Response). Integers indicate free slots.
% as Variables always start with a capital letter we have to explicitely define capital words as string literals.
pattern([i, am, 1], [['How', long, have, you, been, 1, '?'], ['Test', satz, mit, 1, '!']]).
pattern([1, you, 2, me], [['What', makes, you, think, 'I', 2, you, '?']]).
pattern([i, like, 1], [['Does', anyone, else, in, your, family, like, 1, '?']]).
pattern([i, feel, 1], [['Do', you, often, feel, that, way, '?']]).
pattern([1, X, 2], [['Please', tell, me, more, about, X, '.']]) :- important(X).
pattern([yes], [['Who', 'is', 'it', '?']]) :- state(X), X=='family'.                                        % response only this way, when in family state
pattern([yes], [['When', 'exactly', 'do', 'you', 'feel', 'that', 'way','?']]) :- state(X), X=='feelings'.   % response only this way, when in feelings state
pattern([1], [['Please', go, on, '.'], ['weiter', 'bitte']]) :- reset_state.    % reset the state if any unkown answer occurs (a kind of fallback...)
%pattern([remember, X, Predicate, Y], [['I', will, remember, that, '.']]) :-
% add_memory(X, Predicate, Y).
%pattern([X, be], [['Is', that, because, X, be, Y, '?']]) :-
%  call(be, X, Y).

% keywords to react to
important(father).
important(mother).
important(sister).
important(brother).
important(son).
important(daughter).

poweroff(bye).
poweroff(cu).
poweroff(tschau).

% print a list of words seperated by whitespaces
reply([Head|Tail]) :- write(Head), write(' '), reply(Tail).
reply([]) :- nl.


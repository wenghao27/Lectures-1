%% All functions have to be defined in a "module". They are "private" by default
%% unless you export them.
-module(learning).
-export([first/1, next/1, add/2, print_and_add/2, is_even/1, newfirst/1, second/1, same/2,
abs/1, appropriate_age_for_mlp/1, beach_weather/1, fib/1, len/1, quadratic_solve/3, demo_quadratic/0,
filter/2]).

%% You include the "arity" of the function (how many arguments it takes)
%% when referring to it.

%% Silly functions, just for demonstrating.
add(A, B) ->
  A + B. % no "return" like Clojre -- last thing to be evaluated is returned.
print_and_add(A, B) ->
  io:format("Hello!~n"),
  add(A, B).
is_even(N) ->
  N rem 2 =:= 0.

%% Let's implement a few functions we remember from Clojure.
first([H|_]) -> H.
next([_|T]) -> T.
%% But what if the list is empty? There are many ways to handle this... the first
%% is with an explicit function value specifically for empty.

%% newfirst of empty list is undefined (which is an atom, not a keyword!).
%% Otherwise it is the head of the list given.
newfirst([]) -> undefined;
newfirst([H|_]) -> H.
%% These two lines are called "clauses", separated by semicolons. All the clauses
%% together form a function definition. Pattern matching selects the first clause
%% fitting the arguments when the function is called.

%% Put this in action for a "second" function:
second([]) -> undefined;
second([_]) -> undefined;
second([_,H2|_]) -> H2.
%% undefined if 0 or 1 elements only; otherwise gives second.

%% Remember binding rules...
same(X,X) -> true;
same(_,_) -> false.

%% Pattern matching values is nice, but not good enough. How do we do absolute value
%% with just pattern matching? We need conditions too!
%% Enter "guards":
abs(X) when X>=0 -> X;
abs(X) -> -X.

%% Erlang is naturally recursive.
len([]) -> 0;
len([H|T]) -> 1 + len(T).

%% Fibonacci
fib(N) when N=:=0; N=:=1 -> 1;
fib(N) when N>1 -> fib(N-1) + fib(N-2);
fib(_) -> undefined.

%% "and" and "or" don't work in guards, instead we use , and ;
appropriate_age_for_mlp(Age) when Age >= 4, Age =< 9 -> true;
appropriate_age_for_mlp(_) -> false.

%% combining pattern matching and guards to do interesting things.
beach_weather({fahrenheit, T}) when T >= 70, T =< 100 -> true;
beach_weather({celcius, T}) when T >= 21 -> true;
beach_weather({kelvin, T}) when T >= 294 -> true;
beach_weather(_) -> false.


%% All these functions were simple one-liners. Let's do something more complicated,
%% like quadratic formula.

%% Remember that the # of solutions to a quadratic depends on the sign of the
%% discriminant. My first thought is to use guards:
%quadratic_solve(A, B, C) when B*B-4*A*C < 0 -> undefined;
%quadratic_solve(A, B, C) when B*B-4*A*C =:= 0 -> -B / (2 * A);
%quadratic_solve(A, B, C) -> etc.
%% But repeating the B*B-4*A*C is wasteful. I'd rather have one big block for the function.

quadratic_solve(A, B, C) ->
  Discr = B * B - 4 * A * C,
  if   %% WARNING: "if" DOES NOT ACT LIKE IN MOST LANGUAGES. It is more akin to "cond" in Clojure.
    Discr < 0 -> undefined;
    Discr =:= 0 -> -B / (2*A);
    true ->
      Sqrt = math:sqrt(Discr),
      {(-B + Sqrt) / (2 * A), (-B - Sqrt) / (2 * A)}
  end.

demo_quadratic() ->
  A = 1,
  B = 0,
  C = 1,
  case quadratic_solve(A, B, C) of
    undefined -> io:format("Zero real solutions");
    {X1, X2} -> io:format("Two solutions: ~f and ~f~n", [X1, X2]);
    X -> io:format("One solution: ~f~n", [X])
  end.

%% Challenge: write reverse, which reverses a list.




%% Challenge: write zip, which takes two lists and returns a list of tuples
%% of corresponding elements of the two lists.




%% Final challenge: functions are first class citizens (should be no surprise!).
%% Write the higher-order functions map, filter, and reduce.



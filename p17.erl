-module(p17).

-export([answer/0]).

%% If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
%% If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?

%% NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 23 letters and
%% 115 (one hundred and fifteen) contains 20 letters. The use of "and" when writing out numbers is in compliance with British usage.

answer() ->
    lists:sum(lists:map(fun number_length/1, lists:seq(1,1000))).

number_length(N) ->
    length(integer_to_word(N)).

integer_to_word(0) -> "";
integer_to_word(1) -> "one";
integer_to_word(2) -> "two";
integer_to_word(3) -> "three";
integer_to_word(4) -> "four";
integer_to_word(5) -> "five";
integer_to_word(6) -> "six";
integer_to_word(7) -> "seven";
integer_to_word(8) -> "eight";
integer_to_word(9) -> "nine";
integer_to_word(10) -> "ten";
integer_to_word(11) -> "eleven";
integer_to_word(12) -> "twelve";
integer_to_word(13) -> "thirteen";
integer_to_word(14) -> "fourteen";
integer_to_word(15) -> "fifteen";
integer_to_word(18) -> "eighteen"; % catches 18 before next clause can add extra t
integer_to_word(Teen) when Teen > 15 andalso Teen < 20 ->
    lists:append( [integer_to_word(Teen rem 10), "teen"]);
integer_to_word(20) -> "twenty";
integer_to_word(30) -> "thirty";
integer_to_word(40) -> "forty";
integer_to_word(50) -> "fifty";
integer_to_word(60) -> "sixty";
integer_to_word(70) -> "seventy";
integer_to_word(80) -> "eighty";
integer_to_word(90) -> "ninety";

integer_to_word(Tens) when Tens < 100 ->
    lists:append( [ integer_to_word((Tens div 10) * 10), integer_to_word(Tens rem 10)] );

integer_to_word(Hundreds) when Hundreds < 1000 ->
    And = case Hundreds rem 100 =:= 0 of
              true -> "";
              _Else -> "and"
          end,
    lists:append([ integer_to_word(Hundreds div 100), "hundred", And, integer_to_word(Hundreds rem 100)]);

integer_to_word(1000) -> "onethousand".


-module(slimchat_misc).

-export([now_to_secs/0, now_to_secs/1]).

now_to_secs() ->
    now_to_secs(os:timestamp()).

now_to_secs({MegaSecs, Secs, _MicroSecs}) ->
    MegaSecs * 1000000 + Secs.


%%%-----------------------------------------------------------------------------
%%% Copyright (c) 2015 SlimChat.IO, All Rights Reserved.
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in all
%%% copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%%% SOFTWARE.
%%%-----------------------------------------------------------------------------
%%% @doc
%%% SlimChat Main Module
%%%
%%% @end
%%%-----------------------------------------------------------------------------

-module(slimchat).

-author("Feng Lee <feng@emqtt.io>").

-export([load/0, broker/0, unload/0]).

-export([now_to_secs/0, now_to_secs/1]).

broker() ->
    {ok, Addr} = application:get_env(slimchat, broker), Addr.

load() ->
    slimchat_backend:load(),
    slimchat_mod_message:load([]),
    slimchat_mod_offline:load([]),
    slimchat_mod_presence:load([]).
    
unload() ->
    slimchat_mod_presence:unload([]),
    slimchat_mod_offline:unload([]),
    slimchat_mod_message:unload([]),
    slimchat_backend:unload().

now_to_secs() ->
    now_to_secs(os:timestamp()).

now_to_secs({MegaSecs, Secs, _MicroSecs}) ->
    MegaSecs * 1000000 + Secs.


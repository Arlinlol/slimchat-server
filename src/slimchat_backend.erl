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
%%% SlimChat Backend
%%%
%%% @end
%%%-----------------------------------------------------------------------------

-module(slimchat_backend).

-author("Feng Lee <feng@emqtt.io>").

-export([load/0, unload/0]).

-export([store_message/1, ack_message/2]).

-export([find_rooms/1]).

-export([to_list/1]).

-ifdef(use_specs).

-callback load() -> ok | {error, any()}.

-callback unload() -> ok | {error, any()}.

-callback store_message(mqtt_message()) -> ok | {error, any()}.

-callback acke_message(ClientId :: binary(), mqtt_message()) -> ok | {error, any()}.

-callback find_rooms(Username:: binary()) -> {ok, [slimchat_room()]}.

-else.

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
        [{load, 0}, {unload, 0}, {store_message, 1}, {ack_message, 2}, {find_rooms, 1}];

behaviour_info(_Other) ->
        undefined.

-endif.

%%%=============================================================================
%%% API
%%%=============================================================================

load() ->
    with_backend(load, []).

store_message(Message) ->
    with_backend(store_message, [Message]).

ack_message(ClientId, Message) ->
    with_backend(ack_message, [ClientId, Message]).

find_rooms(Usernaem) ->
    with_backend(find_rooms, [Username]).

unload() ->
    with_backend(unload, []).

to_list(#slimchat_contact{username = Name,
                          nick = Nick,
                          group = Group,
                          presence = Presence,
                          show = Show,
                          status = Status}) ->
    [{id, Name}, {nick, Nick},
     {group, Group},
     {presence, Presence},
     {show, Show}, {status, Status}].

to_list(#slimchat_room{name = Name, nick = Nick}) ->
    [{id, Name}, {nick, Nick}, {avatar, <<"">>}].


%%%=============================================================================
%%% Internal functions
%%%=============================================================================

with_backend(Fun, Args) ->
    {ok, Backend} = application:get_env(slimchat, backend),
    apply(backend_mod(Backed), Fun, Args).

backend_mod(Backend) ->
    list_to_atom(["slimchat_backend_", Backend]).


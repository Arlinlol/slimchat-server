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
%%% SlimChat MySQL Backend.
%%%
%%% @end
%%%-----------------------------------------------------------------------------

-module(slimchat_backend_mongo).

-author("Feng Lee <feng@emqtt.io>").

-include("slimchat.hrl").

-include_lib("emqttd/include/emqttd.hrl").

-behaviour(slimchat_backend).

%% slimchat_backend callbacks
-export([onload/0, onunload/0]).

-export([store_message/1, ack_message/2]).

-export([find_contacts/1, find_rooms/1, find_offline_msg/1]).

onload() ->
    {ok, {mongo, Env}} = application:get_env(slimchat, backend),
    [application:set_env(emongo, Par, Val) || {Par, Val} <- Env],
    {ok, _} = application:ensure_all_started(emongo).

find_contacts(Username) ->
    %%TODO...
    [].

find_rooms(Username) ->
    RelDocs = emongo:find(slimchat, "roomUser", [{<<"userId">>, Username}]),
    RoomIds = [proplists:get_value(<<"roomId">>, Doc) || Doc <- RelDocs],
    lager:info("RoomIds of ~s: ~p", [Username, RoomIds]),
    RoomDocs = emongo:find(slimchat, "room", [{<<"_id">>, [{in, RoomIds}]}]),
    lager:info("RoomDocs of ~s: ~p", [Username, RoomDocs]),
    lists:map(fun(RoomDoc) ->
        #slimchat_room{name  = proplists:get_value(<<"_id">>,  RoomDoc),
                       nick  = proplists:get_value(<<"roomTitle">>, RoomDoc),
                       topic = proplists:get_value(<<"roomTopic">>, RoomDoc)}
    end, RoomDocs).

find_offline_msg(Endpoint) ->
    [].

store_message(#mqtt_message{payload = Payload}) ->
    case catch slimchat_json:decode(Payload) of
        {ok, Message} ->
            emongo:insert(slimchat, "message", Message);
        {'EXIT', Error} ->
            lager:error("JSON Decode Error: ~p", [Error])
    end.

ack_message(ClientId, Message) ->
    ok.

onunload() ->
    application:stop(emongo).


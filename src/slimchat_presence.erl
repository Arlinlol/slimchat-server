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
%%% SlimChat Presence
%%%
%%% @end
%%%-----------------------------------------------------------------------------

-module(slimchat_presence).

-author("Feng Lee <feng@emqtt.io>").

-include("slimchat.hrl").

-include_lib("emqttd/include/emqttd.hrl").

-behaviour(emqttd_gen_mod).

-export([load/1, client_online/3, client_offline/3, unload/1]).

load(Opts) ->
    emqttd_broker:hook('client.connected', {?MODULE, slimchat_client_online},
                       {?MODULE, client_online, [Opts]}),
    emqttd_broker:hook('client.disconnected', {?MODULE, slimchat_client_offline},
                       {?MODULE, client_offline, [Opts]}).

client_online(0, #mqtt_client{username = undefined}, _Opts) ->
    ok;
client_online(0, #mqtt_client{client_id  = ClientId,
                              client_pid = ClientPid,
                              username   = Username}, _Opts) ->
    %% Subscribe chat/node/username
    ClientPid ! {subscribe, [{chat_topic(Username), 1},
                             {pres_topic(Username), 0}]},

    %% Subscribe rooms
    lists:foreach(fun(#slimchat_room{name = Room}) ->
            ClientPid ! {subscribe, [{room_topic(Room), 1}]}
        end, slimchat_mnesia:rooms(Username)),

    %% Broadcast presence
    lists:foreach(fun(#slimchat_contact{username = Contact}) ->
            Json = mochijson2:encode([{from, Username}, {type, online}, {show, available}]),
            PresMsg = emqttd_message:make(ClientId, 1, pres_topic(Contact), iolist_to_binary(Json)),
            emqttd_pubsub:publish(PresMsg)
        end, slimchat_mnesia:contacts(Username));

client_online(_, _Client, _Opts) ->
    ok.

client_offline(_Reason, ClientId, _Opts) ->
    case emqttd_cm:lookup(ClientId) of
        undefined ->
            lager:error("Cannot find clientId: ~p", [ClientId]);
        #mqtt_client{username = undefined} ->
            ingore;
        #mqtt_client{username = Username} ->
        %% Broadcast presence
        lists:foreach(fun(#slimchat_contact{username = Contact}) ->
                Json = mochijson2:encode([{from, Username}, {type, offline}, {show, unavailable}]),
                PresMsg = emqttd_message:make(ClientId, 0, pres_topic(Contact), iolist_to_binary(Json)),
                emqttd_pubsub:publish(PresMsg)
            end, slimchat_mnesia:contacts(Username))
    end.

unload(_Opts) ->
    emqttd_broker:unhook('client.connected', {?MODULE, slimchat_client_online}),
    emqttd_broker:unhook('client.disconnected', {?MODULE, slimchat_client_offline}).

chat_topic(Username) ->
    <<"chat/node/", Username/binary>>.

pres_topic(Username) ->
    <<"pres/node/", Username/binary>>.

room_topic(Room) ->
    <<"chat/room/", Room/binary>>.


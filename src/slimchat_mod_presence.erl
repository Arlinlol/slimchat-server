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

-module(slimchat_mod_presence).

-author("Feng Lee <feng@emqtt.io>").

-include("slimchat.hrl").

-include_lib("emqttd/include/emqttd.hrl").

-behaviour(emqttd_gen_mod).

-export([load/1, client_online/3, client_offline/3, unload/1]).

-define(EMPTY(Field), ((Field =:= undefined) orelse (Field =:= <<>>))).

load(Opts) ->
    emqttd_broker:hook('client.connected', {?MODULE, slimchat_client_online},
                       {?MODULE, client_online, [Opts]}),
    emqttd_broker:hook('client.disconnected', {?MODULE, slimchat_client_offline},
                       {?MODULE, client_offline, [Opts]}).

client_online(0, #mqtt_client{username = Username}, _Opts)
    when ?EMPTY(Username) -> ok;

client_online(0, #mqtt_client{client_id  = ClientId,
                              client_pid = ClientPid,
                              username   = Username}, _Opts) ->
    %% Subscribe chat/node/username
    ClientPid ! {subscribe, [{topic(chat, Username), 1},
                             {topic(pres, Username), 1}]},

    %% Subscribe rooms
    lists:foreach(fun(#slimchat_room{name = Room}) ->
            ClientPid ! {subscribe, [{topic(room, Room), 1}]}
        end, slimchat_backend:find_rooms(Username)),

    %% Broadcast presences
    emqttd_pooler:async_submit(
        fun() -> 
            lists:foreach(fun(#slimchat_contact{username = Contact}) ->
                Presence = presence(online, Username),
                emqttd_pubsub:publish(
                    emqttd_message:make(ClientId, qos1,
                                        topic(pres, Contact),
                                        slimchat_json:encode(Presence)))
            end, slimchat_backend:find_contacts(Username))
        end);

    %% Broadcast presence

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
            emqttd_pooler:async_submit(
                fun() -> 
                    lists:foreach(fun(#slimchat_contact{username = Contact}) ->
                        Presence = presence(offline, Username),
                        emqttd_pubsub:publish(
                            emqttd_message:make(ClientId, qos1,
                                                topic(pres, Contact),
                                                slimchat_json:encode(Presence)))
                    end, slimchat_backend:find_contacts(Username))
                end)
    end.

unload(_Opts) ->
    emqttd_broker:unhook('client.connected', {?MODULE, slimchat_client_online}),
    emqttd_broker:unhook('client.disconnected', {?MODULE, slimchat_client_offline}).

topic(chat, Username) ->
    <<"chat/node/", Username/binary>>;

topic(room, Room) ->
    <<"chat/room/", Room/binary>>;

topic(pres, Username) ->
    <<"pres/node/", Username/binary>>.

presence(online, Username) ->
    [{from, Username},
     {type, online},
     {nick, Username},
     {show, available},
     {status, online}];

presence(offline, Username) ->
    [{from, Username},
     {type, offline},
     {nick, Username},
     {show, unavailable},
     {status, offline}].

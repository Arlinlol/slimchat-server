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
%%% SlimChat Mnesia Backend
%%%
%%% @end
%%%-----------------------------------------------------------------------------

-module(slimchat_backend_mnesia).

-author("Feng Lee <feng@emqtt.io>").

-include("slimchat.hrl").

-include_lib("emqttd/include/emqttd.hrl").

-behaviour(slimchat_backend).

%% slimchat_backend callbacks
-export([onload/0, onunload/0]).

-export([store_message/1, ack_message/2]).

-export([find_contacts/1, find_rooms/1, find_offline_msg/1]).

-export([add_contact/1, del_contact/1, add_room/1, del_room/1]).

onload() ->
    ets:new(slimchat_message, [ordered_set, named_table, public]),
    mnesia:create_table(slimchat_contact, [
                {type, ordered_set},
                {ram_copies, [node()]},
                {record_name, slimchat_contact},
                {attributes, record_info(fields, slimchat_contact)}
            ]),
    mnesia:add_table_copy(slimchat_contact, node(), ram_copies),

    mnesia:create_table(slimchat_roster, [
                {type, bag},
                {ram_copies, [node()]},
                {record_name, slimchat_roster},
                {attributes, record_info(fields, slimchat_roster)}
            ]),
    mnesia:add_table_copy(slimchat_roster, node(), ram_copies),

    mnesia:create_table(slimchat_room, [
                {type, ordered_set},
                {ram_copies, [node()]},
                {record_name, slimchat_room},
                {attributes, record_info(fields, slimchat_room)}
            ]),
    mnesia:add_table_copy(slimchat_room, node(), ram_copies),

    mnesia:create_table(slimchat_member, [
                {type, bag},
                {ram_copies, [node()]},
                {record_name, slimchat_member},
                {attributes, record_info(fields, slimchat_member)}
            ]),
    mnesia:add_table_copy(slimchat_member, node(), ram_copies).

find_contacts(Username) ->
    CNames = [CName || #slimchat_roster{cname = CName}
                         <- mnesia:dirty_read(slimchat_roster, Username)],
    {ok, lists:append([mnesia:dirty_read(slimchat_contact, CName) || CName <- CNames])}.

add_contact(Contact) when is_record(Contact, slimchat_contact) ->
    mnesia:transaction(fun mnesia:write/1, [Contact]).

del_contact(Name)->
    mnesia:transaction(fun mnesia:delete/1, [{slimchat_contact, Name}]).

find_rooms(Username) ->
    Names =[ Room || #slimchat_member{room = Room} <-
              mnesia:dirty_match_object(#slimchat_member{room = '_', uname = Username}) ],
    lists:append([mnesia:dirty_read(slimchat_room, Name) || Name <- Names]).

add_room(Room) when is_record(Room, slimchat_room) ->
    mnesia:transaction(fun mnesia:write/1, [Room]).

del_room(Name) ->
    mnesia:transaction(fun mnesia:delete/1, [{slimchat_room, Name}]).

store_message(Message = #mqtt_message{msgid = MsgId, topic = <<"chat/", To/binary>>}) ->
    ets:insert(slimchat_message, {{To, MsgId}, Message}).

ack_message(_ClientId, #mqtt_message{msgid = MsgId, topic = <<"chat/", To/binary>>}) ->
    ets:delete(slimchat_message, {To, MsgId}).

find_offline_msg(To) ->
    {ok, lists:append(ets:match(slimchat_message, {{To, '_'}, '$1'}))}.

onunload() ->
    ok.
 

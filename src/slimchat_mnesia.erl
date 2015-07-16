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
%%% SlimChat Mnesia Database.
%%%
%%% @end
%%%-----------------------------------------------------------------------------

-module(slimchat_mnesia).

-include("slimchat.hrl").

-behaviour(gen_server).

-define(SERVER, ?MODULE).

%% API Function Exports
-export([start_link/0, add_contact/1, del_contact/1, contacts/1,
         add_room/1, del_room/1, rooms/1]).

%% gen_server Function Exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {}).

%%%=============================================================================
%%% API Function Definitions
%%%=============================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

add_contact(Contact) when is_record(Contact, slimchat_contact) ->
    mnesia:transaction(fun mnesia:write/1, [Contact]).

del_contact(Name)->
    mnesia:transaction(fun mnesia:delete/1, [{slimchat_contact, Name}]).

add_room(Room) when is_record(Room, slimchat_room) ->
    mnesia:transaction(fun mnesia:write/1, [Room]).

del_room(Name) ->
    mnesia:transaction(fun mnesia:delete/1, [{slimchat_room, Name}]).

contacts(Username) ->
    CNames = [CName || #slimchat_roster{cname = CName}
                         <- mnesia:dirty_read(slimchat_roster, Username)],
    lists:append([mnesia:dirty_read(slimchat_contact, CName) || CName <- CNames]).

rooms(Username) ->
    Names =[ Room || #slimchat_member{room = Room} <-
              mnesia:dirty_match_object(#slimchat_member{room = '_', uname = Username}) ],
    lists:append([mnesia:dirty_read(slimchat_room, Name) || Name <- Names]).

%%%=============================================================================
%%% gen_server Function Definitions
%%%=============================================================================

init(_Opts) ->
    slimchat_tables:create_tables(),
    slimchat_tables:copy_tables(),
    load_test_data(),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

load_test_data() ->
    UserName = fun(I) -> list_to_binary([<<"user">>, integer_to_list(I)]) end,
    [add_contact(#slimchat_contact{username = UserName(I),
                                   nick = UserName(I)}) || I <- lists:seq(1, 10)],
    mnesia:transaction(fun() -> 
        [mnesia:write(#slimchat_roster{uname = UserName(I1),
                                       cname = UserName(I2)})
                || I1 <- lists:seq(1, 10), I2 <- lists:seq(1, 10)]
        end),
    
    RoomName = fun(I) -> list_to_binary([<<"room">>, integer_to_list(I)]) end,
    [add_room(#slimchat_room{name = RoomName(I),
                             nick = RoomName(I)}) || I <- lists:seq(1, 10)],
    mnesia:transaction(fun() ->
        [mnesia:write(#slimchat_member{room = RoomName(I1),
                                       uname = UserName(I2)})
                || I1 <- lists:seq(1, 10), I2 <- lists:seq(1, 10)]
        end).


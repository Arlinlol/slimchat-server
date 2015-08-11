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

-module(slimchat_mongodb).

-include("slimchat.hrl").

%% rooms
-export([find_rooms/1]).

%% message
-export([store_message/2, ack_message/2]).

find_room(Username) ->
    with_mongo_connect(fun(Conn) ->
            Cursor = mongo:find(Conn, "roomUser", {"userId", Username}),
            Result = mc_cursor:rest(Cursor),
            mc_cursor:close(Cursor),
            Result
        end, []).

store_message(Key, #mqtt_message{payload = Payload}) ->
    case catch slimchat_json:decode(Payload) of
        {ok, Message} ->
            with_mongo_connect(fun(Conn) ->
                    mongo:insert(Conn, <<"message">>, Message)
                end);
        {'EXIT', Error} ->
            lager:error("JSON Decode Error: ~p", [Error])
    end.

ack_message(ClientId, Message) ->
    ok.

with_mongo_connect(SuccFun) ->
    {ok, MongoEnv} = application:get_env(slimchat, mongodb),
    case mongo:connect(MongoEnv) of
        {ok, Conn} ->
            Result = SuccFun(Conn),
            mongo:disconnect(Conn),
            Result;
        {error, Error} ->
            lager:error("Mongodb Connect Error: ~p", [Error]),
            {error, Error}
    end.


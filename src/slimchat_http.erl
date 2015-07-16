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
%%% Http publish API and websocket client.
%%%
%%% @end
%%%-----------------------------------------------------------------------------

-module(slimchat_http).

-author("Feng Lee <feng@emqtt.io>").

-export([handle_request/1]).

-define(APIVER, "/v1").

handle_request(Req) ->
    Method = Req:get(method),
    Path = Req:get(path),
    lager:info("HTTP ~s ~s", [Method, Path]),
    handle_request(Method, Path, Req).

%%
%% Login
%%
handle_request('POST', ?APIVER ++ "/login", Req) ->
    Params = mochiweb_request:parse_post(Req),
    Username = g("username", Params),
    Password = g("password", Params),
    case slimchat_auth:check(Username, Password) of
        ok ->
            jsonReply(Req, [{status, ok}]);
        {error, Error} ->
            jsonReply(Req, [{status, error}, {error, Error}])
    end;

%% 
%% Online
%%  
handle_request('POST', ?APIVER ++ "/online", Req) ->
    Params = mochiweb_request:parse_post(Req),
    Username = list_to_binary(g("username", Params)),
    Contacts = slimchat_mnesia:contacts(Username),
    Rooms = slimchat_mnesia:rooms(Username),
    Response = [{success, true},
                {server_time, slimchat_misc:now_to_secs()},
                {ticket, slimchat_ticket:token()},
                {broker, list_to_binary(slimchat:broker())},
                {buddies, [slimchat_contact:to_list(Contact) || Contact <- Contacts]},
                {rooms, [slimchat_room:to_list(Room) || Room <- Rooms]},
                {user, [{id, Username}, {nick, Username},
                        {presence, online}, {show, available}]}],
    jsonReply(Req, Response);

%% 
%% Contacts
%%  
handle_request('GET', ?APIVER ++ "/contacts", Req) ->
    %% Params = mochiweb_request:parse_post(Req),
    Req:ok({"text/plain", <<"[]">>});

%% 
%% 404
%%  
handle_request(Method, Path, Req) ->
    lager:error("HTTP Bad Request: ~s ~s", [Method, Path]),
	Req:not_found().

g(Name, Params) ->
    proplists:get_value(Name, Params).

%% g(Name, Params, Default) ->
%%    proplists:get_value(Name, Params, Default).

jsonReply(Req, Data) ->
    Json = mochijson2:encode(Data),
    Req:ok({"application/json", Json}).


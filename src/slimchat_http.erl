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
    handle_request(Req:get(method), Req:get(path), Req).

%%
%% Login
%%
handle_request('POST', ?APIVER ++ "/login", Req) ->
    Params = mochiweb_request:parse_post(Req),
    Username = g("username", Params),
    Password = g("password", Params),
    case slimchat_auth:check(Username, Password) of
        {ok, Token} ->
            Json = mochijson2:encode([{status, ok}, {token, Token}]),
            Req:ok({"application/json", Json});
        {error, Error} ->
            Json = mochijson2:encode([{status, error}, {error, Error}]),
            Req:ok({"text/plain", Json})
    end;

%% 
%% Online
%%  
handle_request('POST', ?APIVER ++ "/online", Req) ->
    Params = mochiweb_request:parse_post(Req),
    Req:ok({"text/plain", <<"[]">>});

%% 
%% Contacts
%%  
handle_request('GET', ?APIVER ++ "/contacts", Req) ->
    Params = mochiweb_request:parse_post(Req),
    Req:ok({"text/plain", <<"[]">>});

%% 
%% 404
%%  
handle_request(Method, Path, Req) ->
    lager:error("HTTP Bad Request: ~s ~s", [Method, Path]),
	Req:not_found().


g(Name, Params) ->
    proplists:get_value(Name, Params).

g(Name, Params, Default) ->
    proplists:get_value(Name, Params, Default).


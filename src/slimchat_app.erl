%%%-----------------------------------------------------------------------------
%%% Copyright (c) 2015 Feng Lee, All Rights Reserved.
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
%%% main application module.
%%%
%%% @end
%%%-----------------------------------------------------------------------------

-module(slimchat_app).

-author("Feng Lee <feng@emqtt.io>").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, Sup} = slimchat_sup:start_link(),
    slimchat:load(),
    start_listeners(),
    {ok, Sup}.
    
start_listeners() ->
    {ok, Listeners} = application:get_env(listeners),
    [start_listener(Listener) || Listener <- Listeners].

start_listener({http, Port, SockOpts}) ->
    MFArgs = {slimchat_http, handle_request, []},
	mochiweb:start_http(Port, SockOpts, MFArgs).

stop(_State) ->
    slimchat:unload().


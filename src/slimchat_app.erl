
-module(slimchat_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    slimchat:load([]),
    slimchat_sup:start_link().

stop(_State) ->
    slimchat:unload([]),
    ok.

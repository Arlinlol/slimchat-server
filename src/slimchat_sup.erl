-module(slimchat_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, [?CHILD(slimchat_msg_store, worker),
                                  ?CHILD(slimchat_roster, worker),
                                  ?CHILD(slimchat_presence, worker),
                                  ?CHILD(slimchat_grpchat, worker)]} }.


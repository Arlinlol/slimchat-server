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
%%% SlimChat Offline Module.
%%%
%%% @end
%%%-----------------------------------------------------------------------------

-module(slimchat_mod_offline).

-author("Feng Lee <feng@emqtt.io>").

-include_lib("emqttd/include/emqttd.hrl").

-behaviour(emqttd_gen_mod).

-export([load/1, publish_offline_msg/3, unload/1]).

%%%=============================================================================
%%% API
%%%=============================================================================

load(Opts) ->
    emqttd_broker:hook('client.subscribe.after', {?MODULE, slimchat_offline_msg},
                       {?MODULE, publish_offline_msg, [Opts]}).

publish_offline_msg(_ClientId, TopicTable, _Opts) ->
    [publish_to(Topic) || {Topic, _Qos} <- TopicTable].

publish_to(<<"chat/", To/binary>>) ->
    OfflineMsgs = slimchat_backend:find_offline_msg(To),
    [emqttd_pubsub:publish(Msg) || Msg <- OfflineMsgs];
    
publish_to(_Topic) ->
    ok.

unload(_) ->
    emqttd_broker:unhook('client.subscribe.after', {?MODULE, slimchat_offline_msg}).


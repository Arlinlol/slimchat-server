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
%%% SlimChat Main Module
%%%
%%% @end
%%%-----------------------------------------------------------------------------

-module(slimchat).

-author("Feng Lee <feng@emqtt.io>").

-include_lib("emqttd/include/emqttd.hrl").

-behaviour(emqttd_gen_mod).

-export([load/1, message_published/2, message_acked/3, unload/1]).

load(Opts) ->
    emqttd_broker:hook('message.publish', {?MODULE, slimchat_published},
                       {?MODULE, message_published, [Opts]}),
    emqttd_broker:hook('message.acked', {?MODULE, slimchat_acked},
                       {?MODULE, message_acked, [Opts]}).

message_published(Message = #mqtt_message{msgid = MsgId,
                                          topic = <<"chat/", To/binary>>,
                                          qos = 1}, _Opts) ->
    slimchat_msg_store:store({To, MsgId}, Message), Message;

message_published(Message, _Opts) ->
    Message.

message_acked(_ClientId, #mqtt_message{msgid = MsgId,
                                       topic = <<"chat/", To/binary>>,
                                       qos = 1}, _Opts) ->
    slimchat_msg_store:ack({To, MsgId});

message_acked(_ClientId, _Message, _Opts) ->
    pass.

unload(_Opts) ->
    emqttd_broker:unhook('message.publish', {?MODULE, slimchat_published}),
    emqttd_broker:unhook('message.acked', {?MODULE, slimchat_acked}).


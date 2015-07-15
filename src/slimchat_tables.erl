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
%%% SlimChat Mnesia Table Management.
%%%
%%% @end
%%%-----------------------------------------------------------------------------

-module(slimchat_tables).

-include("slimchat.hrl").

-export([create_tables/0, copy_tables/0]).

create_tables() ->

    mnesia:create_tables(slimchat_account, [
                {type, set},
                {ram_copies, [node()]},
                {record_name, slimchat_account},
                {attributes, record_info(fields, slimchat_account)}
            ]),

    mnesia:create_table(slimchat_contact, [
                {type, bag},
                {ram_copies, [node()]},
                {record_name, slimchat_contact},
                {attributes, record_info(fields, slimchat_contact)}
            ]),

    mnesia:create_table(slimchat_room, [
                {type, set},
                {ram_copies, [node()]},
                {record_name, slimchat_room},
                {attributes, record_info(fields, slimchat_room)}
            ]),

    mnesia:create_table(slimchat_member, [
                {type, bag},
                {ram_copies, [node()]},
                {record_name, slimchat_contact},
                {attributes, record_info(fields, slimchat_contact)}
            ]).

copy_tables() ->
    [mnesia:add_table_copy(Tab, node(), ram_copies) ||
            Tab <- [slimchat_account, slimchat_contact,
                    slimchat_room, slimchat_member]].



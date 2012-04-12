%% -------------------------------------------------------------------
%%
%% Copyright (c) 2012 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------
-module(sb).

-export([load_config/1, set_config/2, get_config/1, get_config/2]).

%%
%% Application Config
%%

load_config(File) ->
    lager:debug("sb:load_config loading file: ~p", [File]),
    case file:consult(File) of
        {ok, Terms} ->
            [set_config(Key, Value) || {Key, Value} <- Terms],
            ok;
        {error, Reason} ->
            lager:error("Failed to parse config file: ~p with error: ~p", [File, Reason]),
            {error, Reason}
    end.


set_config(Key, Value) ->
    lager:debug("sb:set_config key: ~p  value: ~p ", [Key, Value]),
    ok = application:set_env(stableboy, Key, Value).


get_config(Key) ->
    case application:get_env(stableboy, Key) of
        {ok, Value} ->
            Value;
        undefined  ->
            erlang:error("Missing configuration key", [Key])
    end.

get_config(Key, Default) ->
    case application:get_env(stableboy, Key) of
        {ok, Value} ->
            Value;
        _ ->
            Default
    end.


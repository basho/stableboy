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

-module(sb_file).
-behaviour(stableboy_vm_backend).

-export([list/0, get/1, snapshot/1, rollback/1]).


%% List the available VM's
list () ->
    lager:debug("In sb_file:list"),

    VMS = sb:get_config(vms),
    [io:format("~p~n", [VM]) || VM <- VMS],
    ok.

%% Get by name right now
%% TODO: check if args is a file and read the requirements there
%% TODO: start the VM if it isn't already started
%% TODO: if they want more than they have, halt(1)
get (Args) ->
    % Name will be either the file name or the VM name
    [Name|_] = Args,

    lager:debug("In sb_file:get with args: ~p", [Name]),

    % Get the static listing of IP/User/Pass info
    % will be the stableboy config file
    VMInfo = sb:get_config(vm_info),
    SearchResult = [_S || _S <- VMInfo, element(1, _S) == Name],

    % Return result straight away for now, with multiple
    % counts this will be a list-comprehension
    io:format("~p.~n", SearchResult),

    ok.

%% TODO: Do this
snapshot (Args) ->
    lager:debug("In sb_file:snapshot with args: ~p and ~p", [Args]),
    lager:error("Snapshot is not supported by the sb_file backend"),
    halt(1).

%% TODO: Do this
rollback (Args) ->
    lager:debug("In sb_file:rollback with args: ~p and ~p", [Args]),
    lager:error("Rollback is not supported by the sb_file backend"),
    halt(1).


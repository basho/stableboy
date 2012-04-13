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

-module(sb_vbox).
-behaviour(stableboy_vm_backend).

-export([list/0, get/1, snapshot/1, rollback/1]).


%% List the available VM's
%% TODO: Make dynamic, fetching a list from VirtualBox
list () ->
    lager:debug("In sb_vbox:list"),

    % pseudo dynamic method
    %    Output = os:cmd("vboxmanage list vms"),
    %    SplitUp = re:split(Output, "[\n]", [{return, list}]),
    %    <some re to break the name off>
    %    io:format("list output: ~n~p", [SplitUp]),

    VMS = sb:get_config(vms),
    [io:format("~p~n", [VM]) || VM <- VMS],
    ok.

%% Get by name right now
%% TODO: check if args is a file and read the requirements there
%% TODO: start the VM if it isn't already started
%% TODO: Check the count property and see how many they want
%% TODO: with dynamic, if they need more than one that we don't have
%%       clone vm's on the fly
get (Args) ->
    % Name will be either the file name or the VM name
    [Name|_] = Args,

    lager:debug("In sb_vbox:get with args: ~p", [Name]),

    % Get the static listing of IP/User/Pass info
    % will be the stableboy config file
    VMInfo = sb:get_config(vm_info),
    SearchResult = [_S || _S <- VMInfo, element(1, _S) == Name],

    % Return result straight away for now, with multiple
    % counts this will be a list-comprehension
    io:format("~p~n", SearchResult),

    ok.

%% TODO: Do this
snapshot (Args) ->
    lager:debug("In sb_vbox:snapshot with args: ~p and ~p", [Args]),
    ok.

%% TODO: Do this
rollback (Args) ->
    lager:debug("In sb_vbox:rollback with args: ~p and ~p", [Args]),
    ok.


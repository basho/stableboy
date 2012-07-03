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

-export([list/1, get/1, snapshot/1, rollback/1, brand/2]).


%% List the available VM's
%%
%% In the case of the sb_file backend, this should be in the stableboy
%% config file that is read in at startup
list([]) ->
    lager:debug("In sb_file:list (unfiltered)"),
    VMS = sb:get_config(vms),
    sb_vm_common:list_by_properties([{platform, '*'}], VMS),
    ok;
list([Filename]) ->
    lager:debug("In sb_file:list (filename)"),
    VMS = sb:get_config(vms),
    sb_vm_common:list_by_file(Filename, VMS).

%% Get by file, where file contains a description of what is needed
%% The file format is defined by basha_harness env files
%% Ex: https://github.com/basho/basho_harness/tree/master/envs
get (Args) ->
    lager:debug("In sb_file:get with args: ~p", Args),
    case filelib:is_file(Args) of
        true ->
            ok = get_by_file(Args);
        false ->
            ok = get_by_name(Args)
    end.


%% Search the VM list by env file, Ex: get "test/ubuntu-tester.config"
get_by_file (Args) ->
    lager:debug("In sb_file:get_by_file with args: ~p", Args),

    % Read in environment config file
    case file:consult(Args) of
        {ok, Properties} ->

            VMList = sb:get_config(vms),
            VMInfo = sb:get_config(vm_info),
            Count = sb:get_config(count),

            %% attempt to match the properties in the file with available VM's
            case sb_vm_common:match_by_props(VMList, Properties, Count) of
                {ok, SearchResult} ->
                    % The search results come back as the <vms> structure and
                    % and needs to be of the form of vm_info
                    % so translate each using match_by_name
                    lager:debug("get_by_file: SearchResult: ~p", [SearchResult]),
                    PrintResult = fun(SearchRes) ->
                                          % Get the name out
                                          {ok, MBNRes} = sb_vm_common:match_by_name(VMInfo, element(1, SearchRes)),
                                          sb_vm_common:print_result(MBNRes)
                                  end,

                    % Print each result we got back
                    lists:foreach(PrintResult, SearchResult),
                    ok;
                {error, Reason} ->
                    lager:error("Unable to get VM: ~p", [Reason])
            end;

        {error, Reason} ->
            lager:error("Failed to parse file: ~p with error: ~p", [Args, Reason]),
            {error, Reason}
    end.


%% Search the VM list by name, Ex: get "ubuntu-1104-64"
get_by_name ([Name]) ->
    lager:debug("In sb_file:get_by_name with args: ~p", [Name]),

    % Get the static listing of IP/User/Pass info
    % will be the stableboy config file
    VMInfo = sb:get_config(vm_info),
    case sb_vm_common:match_by_name(VMInfo, Name) of
        {ok, SearchResult} ->
            sb_vm_common:print_result(SearchResult),
            ok;
        {error, Reason} ->
            lager:error("Unable to get VM: ~p", [Reason]),
            halt(1)
    end.


%% Not supported by file backend
snapshot(Args) ->
    lager:debug("In sb_file:snapshot with args: ~p", [[Args]]),
    lager:error("Snapshot is not supported by the sb_file backend"),
    halt(1).

%% Not supported by file backend
rollback(Args) ->
    lager:debug("In sb_file:rollback with args: ~p", [[Args]]),
    lager:error("Rollback is not supported by the sb_file backend"),
    halt(1).

%% Not supported by file backend
brand(Alias,Meta) ->
    lager:debug("In sb_file:rollback with args: ~p", [[Alias, Meta]]),
    lager:error("brand is not supported by the sb_file backend"),
    halt(1).

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

-export([list/0, get/1, snapshot/1, rollback/1, brand/1]).

-define(LIST_CMD, "vboxmanage list vms").
-define(GET_XDATA_CMD(VM), "vboxmanage getextradata " ++ VM ++ " sb_info").
-define(SET_XDATA_CMD(VM,META), "vboxmanage setextradata " ++ VM ++ " sb_info " ++ META).
-define(GET_IPDATA_CMD(VM), "vboxmanage guestproperty get " ++ VM ++ " /VirtualBox/GuestInfo/Net/0/V4/IP").
-define(GET_PORTDATA_CMD(VM), "vboxmanage showvminfo " ++ VM ++ " | egrep \"name = .*ssh,.*host port = \"").
-define(START_CMD(VM), "vboxmanage startvm " ++ VM ++ " --type headless").

%% List the available VM's
list () ->
    lager:debug("In sb_vbox:list"),
    case command(?LIST_CMD, fun format_list/1) of
        VMs ->
            sb_vm_common:print_result(VMs)
    end,
    ok.

% Get a VM by name or via an environment file
get (Args) ->
                                                % Args will be either the file name or the VM name
    lager:debug("In sb_vbox:get/1"),
    case filelib:is_file(hd(Args)) of
        true ->
            ok = get_by_file(Args);
        false ->
            ok = get_by_name(Args)
    end.

%% @doc Gets a VM(s) by the constraints given in the file.
get_by_file(Args) ->
    lager:debug("In sb_vbox:get_by_file with args: ~p", Args),
                                                % Read in environment config file
    case file:consult(Args) of
        {ok, Properties} ->
            VMList = command(?LIST_CMD, fun format_list/1),
            VMInfo = command(?LIST_CMD, fun format_get/1),
            Count = sb:get_config(count),

            %% attempt to match the properties in the file with available VM's
            case sb_vm_common:match_by_props(VMList, Properties, Count) of
                {ok, SearchResult} ->
                                                % The search results come back as the <vms> structure and
                                                % and needs to be of the form of vm_info
                                                % so translate each using match_by_name
                    lager:debug("get_by_file: SearchResult: ~p", [SearchResult]),

                    %% Start the VMs if they aren't already.
                    [ command(?START_CMD(VM), fun started_vm/1) || {VM,_,_,_} <- SearchResult ],

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


%% @doc Get a VM by name (alias).
get_by_name([Alias|_Args]) ->
    VM = get_vm(Alias),
    case VM of
        {error, _Reason} -> ok;
        _VM ->
            command(?START_CMD(Alias), fun started_vm/1)
    end,
    sb_vm_common:print_result([VM]),
    ok.

%% TODO: Do this
snapshot (Args) ->
    lager:debug("In sb_vbox:snapshot with args: ~p and ~p", [Args]),
    ok.

%% TODO: Do this
rollback (Args) ->
    lager:debug("In sb_vbox:rollback with args: ~p and ~p", [Args]),
    ok.

%% @doc Brands a VM with given metadata.
%% vboxmanage setextradata dimplecup sb_info "ubuntu:12.04:64"
brand([Alias, Meta]) ->
    lager:debug("In sb_vbox:brand/1"),
    command(?SET_XDATA_CMD(Alias,Meta),
            fun(Output) -> lager:debug("Branding result: ~p", [Output]) end),
    ok.

%%-------------------
%% Internal functions
%%-------------------

%% @doc Executes a command in the shell. The Callback is
%% called with the stdout stream so that the data can be formatted
%% before being returned.
command(Command, Callback) ->
    Output = os:cmd(Command),
    Callback(Output).

command(Command, Alias, Callback) ->
    Output = os:cmd(Command),
    Callback(Alias, Output).

%% get info from extra data memory of VBox, which must have previously
%% been set by the user to describe their VM guest. For example, type
%% vboxmanage setextradata myVmName sb_info ubuntu:12.04:64
format_extra(Alias,Output) ->
    Unknown = {Alias,undefined,[],undefined,undefined,undefined},
    case re:split(Output, "[ \n]", [{return, list},trim]) of
        ["Value:",Xtra] ->
            case re:split(Xtra, "[:]", [{return, list},trim]) of
                [OS,Vdotted,Arch] ->
                    Vints = version_to_intlist(Vdotted),
                    {Alias,list_to_atom(OS),Vints,list_to_integer(Arch),none,none};
                [OS,Vdotted,Arch,User,Pass] ->
                    Vints = version_to_intlist(Vdotted),
                    {Alias,list_to_atom(OS),Vints,list_to_integer(Arch),
                     list_to_atom(User),list_to_atom(Pass)};
                _ ->
                    %% error msg?
                    io:format("sb_info failed: Xtra = ~p~n", [Xtra]),
                    Unknown
            end;
        _ -> Unknown
    end.

%% @doc Extract the IP address from the output of the virtual box "guestproperty" result.
%% This will look at the first adapter only. The first one is assumed to be the public IP.
format_ipdata(Output) ->
    case re:split(Output, "[ \n]", [{return, list},trim]) of
        ["Value:",IP] -> IP;
        _ -> undefined
    end.

%% Find the SSH port, even when it's Port Forwarded
format_portdata(Output) ->
    case re:split(Output, "host port = ", [{return, list},trim]) of
        [_First|Rest] ->
            [Port|_Other] = re:split(Rest, "[ ,\"]", [{return, list},trim]),
            list_to_integer(Port);
        _ ->
            %% hard-coded. Nowhere to get it from.
            22
    end.

%% @doc Format the output from the virtual box "list vms" command into just the names.
format_names(Output) ->
    Lines = [Line || Line <- re:split(Output, "[\n]", [{return, list}]), Line /= []],
    Lines2 = [re:split(Line, "[\"]", [{return, list}]) || Line <- Lines],
    lists:map(fun([_Blank,Alias|_ShaHash]) -> Alias end, Lines2).

%% @doc Format output for the 'list' command into Erlang terms.
format_list(Output) ->
    Names = format_names(Output),
    Xtras = [command(?GET_XDATA_CMD(Name), Name, fun format_extra/2) || Name <- Names],
    lists:map(fun({VM,OS,Ver,Arch,_User,_Pass}) -> {VM,OS,Ver,Arch} end, Xtras).

%% @doc Format output for the 'get' command into Erlang terms.
%% {Alias,IP,Port,User,Password}
format_get(Output) ->
    %% TODO: take the config file and user default user/password if both here are empty
    Names = format_names(Output),
    [ get_vm(VM) || VM <- Names].

%% @doc Get a VM by it's name
get_vm(VM) ->
    case command(?GET_XDATA_CMD(VM), VM, fun format_extra/2) of
        {_Alias, undefined, _, _, _, _} ->
            Names = command(?LIST_CMD, fun format_names/1),
            Reason = case lists:member(VM, Names) of
                         true -> "it's not branded.";
                         false -> "it doesn't exist."
                     end,
            {error, "sb_vbox:get VM '" ++ VM ++ "' failed because " ++ Reason};
        {_Alias,_OS,_Ver,_Arch,User,Pass} ->
            {IP,Port} = get_conn_data(VM),
            {VM,IP,Port,User,Pass}
    end.

%% @doc Get IP and Port address information for a named VM.
%% Fetches data from virtual box manager, including port forwarded ssh ports
%% providing that the port forwarding rule has the text "ssh" in it's name,
%% otherwise Port defaults to 22. IP addr is taken from the first NIC, namely
%% adapter 0.
get_conn_data(Name) ->
    Addr = command(?GET_IPDATA_CMD(Name), fun format_ipdata/1),
    Port = command(?GET_PORTDATA_CMD(Name), fun format_portdata/1),
    {Addr, Port}.

%% @doc Callback for result of starting a VM
started_vm(Output) ->
    lager:debug("Started VM! ~s", [Output]).

%% @doc Converts a dot-delimited version string into a list of version integers.
version_to_intlist(V) ->
    [ list_to_integer(P) || P <- re:split(V, "[.]", [{return,list},trim]) ].

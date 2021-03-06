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

-module(sb_vm_common).

%% Public api
-export([match_by_name/2,
         match_by_props/2,
         match_by_props/3,
         print_result/1,
         extract_branding/1,
         list_by_properties/2,
         list_by_file/2,
         wait_for_ssh_port/1,
         wait_for_ssh_port/2]).

%% Public API

%% @doc Loads an environment file, then lists matching VMs
list_by_file(EnvFile, VMList) ->
    lager:debug("In sb_vm_common:list_by_file(~p,~p)", [EnvFile, VMList]),
    case filelib:is_file(EnvFile) of
        true ->
            case file:consult(EnvFile) of
                {ok, Properties} ->
                    list_by_properties(Properties, VMList);
                {error, Reason} ->
                    lager:error("Failed to parse file: ~s with error: ~p", [EnvFile, Reason]),
                    {error, Reason}
            end;
        false ->
            lager:error("Invalid environment filename: ~s", [EnvFile]),
            {error, enoent}
    end.

%% @doc Lists VMs that match the given properties
list_by_properties(Props, VMList) ->
    lager:debug("In sb_vm_common:list_by_properties(~p,~p)", [Props, VMList]),
    Count = sb:get_config(count, undefined),
    if
        is_integer(Count) ->
            lager:debug("Number of VMs requested: ~p", [Count]),
            case match_by_props(VMList, Props, Count) of
                {ok, SearchResult} ->
                    print_result(SearchResult);
                {error, Reason} ->
                    lager:error("Unable to find VMs: ~p", [Reason]),
                    {error, Reason}
            end;
        true -> %% If there's no count, print them all
            lager:debug("GET ALL THE VMs!"),
            print_result(VMList)
    end.

%% Match given a list of VM tuples and a VM name
match_by_name(VMList, Name) ->
    lager:debug("In sb_vm_common:match_by_name with VM list: ~p and Name: ~p", [VMList, Name]),
    Result = [_S || _S <- VMList, element(1, _S) == Name],
    case Result of
        [] ->
            {error, "No matches found"};
        _ ->
            {ok, Result}
    end.


%% Match given a list of VM tubles and a list of matching properties
%% For a VM to match, all properties must match or be a wildcard '*' match
%% The only property that is required is {platform, <platform>}
%% Properties {architecture, <arch>} and {version, [ver]} will be substituted with
%% wildcards if not present and all other properties will be dropped
match_by_props(VMList, Props) ->
    match_by_props(VMList, Props, 1).

match_by_props(VMList, Props, Count) ->
    lager:debug("In sb_vm_common:match_by_props with Props: ~p and Count: ~p", [Props, Count]),

    %% Make sure we have a platform
    PlatformProp = case proplists:lookup(platform, Props) of
                       none ->
                           lager:error("Property {platform, } must be defined"),
                           halt(1);
                       _P -> _P
                   end,

    Wildcardify = fun(Proplist, Property) ->
                          case proplists:lookup(Property, Proplist) of
                              none ->
                                  proplists:property(Property, '*');
                              _Pr ->
                                  _Pr
                          end
                  end,

    ArchProp = Wildcardify(Props, architecture),
    VersionProp = Wildcardify(Props, version),


    {ok, Matches} = match_all(VMList, [PlatformProp, VersionProp, ArchProp]),
    lager:debug("Matches we found: ~p", [Matches]),

    %% Ensure we have the proper count available
    ResultList = case length(Matches) >= Count of
                     true ->
                         lists:sublist(Matches, Count);
                     false ->
                         lager:error("Insufficient matches: (~p) meeting count value of: ~p", [length(Matches), Count]),
                         halt(1)
                 end,

    {ok, ResultList}.



%% @doc Common output function for all backends
print_result(Results) ->
    Format = lists:flatten([ "~p.~n" || _ <- Results]),
    io:format(Format, Results).

%% @doc Extract branding information from the CLI input for use in the VM backend.
extract_branding(Meta) ->
    extract_branding(re:split(Meta, ":", [{return, binary}]),
                     [platform,version,architecture,user,password], []).


%% @doc Waits for the SSH port of the VM to become available, perhaps
%% after starting. This ensures that the client program
%% (basho_harness) can actually connect to the port.
%% @end
%% Based on the trick in riak-ruby-client's TCPSocket.wait_for_service_with_timeout
wait_for_ssh_port({_Name,_IP,_Port,_User,_Pass}=VM) ->
    wait_for_ssh_port(VM, 60000).

wait_for_ssh_port({_Name,IP,Port,_User,_Pass}, Timeout) ->
    wait_for_ssh_port(IP,Port,Timeout).

wait_for_ssh_port(IP, Port, Timeout) when is_integer(Timeout) ->
    lager:debug("Waiting for SSH Port ~p:~p within timeout ~p", [IP, Port, Timeout]),
    TRef = erlang:send_after(Timeout, self(), timeout),
    wait_for_ssh_port(IP, Port, TRef);
wait_for_ssh_port(IP, Port, Timeout) ->
    Me = self(),
    erlang:spawn(fun() ->
                         case gen_tcp:connect(IP, Port, [], 1000) of
                             {ok, Socket} ->
                                 gen_tcp:close(Socket),
                                 Me ! connected;
                             {error, Reason} ->
                                 Me ! {error, Reason}
                         end
                 end),
    receive
        timeout ->
            lager:error("SSH port ~p:~p did not come up within timeout: ", [IP, Port]),
            {error, timeout};
        {error, Reason} ->
            lager:debug("Waiting for SSH port ~p:~p failed: ~p", [IP, Port, Reason]),
            wait_for_ssh_port(IP, Port, Timeout);
        connected ->
            lager:debug("SSH port is up ~p:~p", [IP, Port]),
            ok
    end.


%%
%% Private functions
%%

%% given a list of environment properties in tuple form, find the first match
match_all (VMList, Env) ->
    lager:debug("In sb_vm_common:match_all with env: ~p", [Env]),

    %% The VMList has the format of
    %%  { name, platform, version, architecture }
    %% For the case of matching, we drop the name on the LHS of the match
    %% The environment comes in the form of a list of tuples that we need
    %% to extract the keys from for the RHS of the match
    MatchFun = fun (VMTuple) ->
                       lager:debug("Matchfun: VMTuple: ~p Platform: ~p",[VMTuple, Env]),
                       %% drop the name off of the comparison
                       {_, Mp, Mv, Ma} = VMTuple,
                       match({Mp, Mv, Ma}, list_to_tuple([Values || {_, Values} <- Env]))
               end,

    %% Find the matches in the VM list that support the requirements
    case lists:filter(MatchFun, VMList) of
        [] ->
            {error, "No matching VM's for this platform"};
        Matches ->
            {ok, Matches}
    end.


%% match_one (VMList, Platform) ->
%%     {ok, [FirstMatch|_]} = match_all(VMList, Platform),
%%     FirstMatch.

%% %% check to see if the platform matches any of the valid ones
%% match_any ({_,_,_}=Platform,ValidPlatforms) ->
%%     lists:any(fun (P) -> match(Platform,P) end,ValidPlatforms).

%% match (A,B) ensures that platform A >= B
match (_, {'*', '*', '*'}) -> true;
match ({F,Va,A},{F,Vb,A}) -> cmp_version(Va,Vb) == eq;
match ({F,Va,_},{F,Vb,'*'}) -> cmp_version(Va,Vb) == eq;
match ({F,_,_},{F,'*','*'}) -> true;
match (_,_) -> false.

%% compare wildcard versions
cmp_version ('*',_) -> eq;
cmp_version (_,'*') -> eq;

%% compare version lists
cmp_version ([],[]) -> eq;

%% wildcard version enders match anything
cmp_version (['*'],_) -> eq;
cmp_version (_,['*']) -> eq;

%% version [11,0,0,0,0] should match [11]
cmp_version ([0|Rest],[]) -> cmp_version(Rest,[]);
cmp_version ([],[0|Rest]) -> cmp_version([],Rest);

%% terminated with gt or lt result
cmp_version (_,[]) -> gt;
cmp_version ([],_) -> lt;

%% compare version numbers sequentially
cmp_version ([A|_],[B|_]) when A > B -> gt;
cmp_version ([A|_],[B|_]) when A < B -> lt;
cmp_version ([A|As],[B|Bs]) when A == B -> cmp_version(As,Bs).

extract_branding([], _, Acc) ->
    lists:reverse(Acc);
extract_branding(_, [], Acc) ->
    lists:reverse(Acc);
extract_branding([Value|Values],[Key|Keys], Acc) ->
    extract_branding(Values, Keys, [{Key, Value}|Acc]).

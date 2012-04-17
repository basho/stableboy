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

% Public api
-export([match_by_name/2, match_by_props/2, match_by_props/3, print_result/1]).


%% Public API

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

match_by_props(VMList, Props, [Count]) ->
    lager:debug("In sb_vm_common:match_by_props with Props: ~p and Count: ~p", [Props, Count]),

    %% Make sure we have a platform
    PlatformProp = case proplists:lookup(platform, Props) of
                       none ->
                           lager:error("Property {plaform, } must be defined in environment file"),
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



%% Common output function for all backends
print_result(Results) ->
    lists:foreach( fun(T) -> io:format("~p.~n", [T]) end,
                   Results).



%% Private functions

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


match_one (VMList, Platform) ->
    {ok, [FirstMatch|_]} = match_all(VMList, Platform),
    FirstMatch.

%% check to see if the platform matches any of the valid ones
match_any ({_,_,_}=Platform,ValidPlatforms) ->
    lists:any(fun (P) -> match(Platform,P) end,ValidPlatforms).

%% match (A,B) ensures that platform A >= B
match ({F,Va,A},{F,Vb,A}) -> cmp_version(Va,Vb) =/= lt;
match ({F,Va,_},{F,Vb,'*'}) -> cmp_version(Va,Vb) =/= lt;
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





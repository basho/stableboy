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

% api
-export([match_by_name/2, match_by_terms/2, match_by_terms/3, print_result/1]).


%% Match given a list of VM tubles and a VM name
match_by_name(VMList, Name) ->
    lager:debug("In sb_vm_common:match_by_name with VM list: ~p and Name: ~p", [VMList, Name]),
    Result = [_S || _S <- VMList, element(1, _S) == Name],
    case Result of
        [] ->
            {error, "No matches found"};
        _ ->
            {ok, Result}
    end.


match_by_terms(VMList, Terms) ->
    ok.


match_by_terms(VMList, Terms, Count) when Count =:= 1 ->
    match_by_terms(VMList, Terms);

match_by_terms(VMList, Terms, Count) ->
    ok.


%% Common output function for all backends
print_result(Results) ->
    lists:foreach( fun(T) -> io:format("~p.~n", [T]) end,
                   Results).








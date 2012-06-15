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

-module(stableboy_vm_backend).
-export([behaviour_info/1]).

%% define the behavior for a VM backend
behaviour_info (callbacks) ->
    [
     {list, 1},
     {get, 1},
     {snapshot, 1},
     {rollback, 1},
     {brand, 2}
    ];

behaviour_info (_Other) ->
    undefined.

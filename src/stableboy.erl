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
-module(stableboy).

-export([main/1]).

%% Exported so I can use apply()
%% Don't call these
-export([help/2, vm/2, debug/2, cmd/2, config_file/2, count/2]).


%% Entry point for escript
main (Args) ->

    %% Add all deps to the run path
    load_all_deps(),

    %% Start lager application
    start_lager(),

    %% parse command line options
    {ok, {Options, NonOptionArgs}} = parse_options(Args),

    %% set flags
    set_flags(Options, NonOptionArgs),

    %% run command
    run_command(),

    ok.


%%
%% Escript Boostrap
%%

%% add dependencies to path
add_deps (Path) ->
    {ok, Deps} = file:list_dir(Path),
    [code:add_path(lists:append([Path, "/", Dep, "/ebin"])) || Dep <- Deps],
    ok.

%% get a list of all dependencies
load_all_deps () ->
    %% send this_script/deps directory
    add_deps(filename:join(filename:dirname(escript:script_name()), "deps")).

%% Special handling of lager startup
start_lager() ->
    application:load(lager),
    application:set_env(lager, handlers, [{lager_console_backend, error}]),
    application:start(lager).


%%
%% Command Line Option Handling
%%

%% Parse command line options given to escript
parse_options (Args) ->
    case getopt:parse(command_line_options(), Args) of
        {ok, {Options, NonOptionArgs}}  ->
            {ok, {Options, NonOptionArgs}};
        {error, Error} ->
            lager:error("Error parsing command line: ~p", [Error]),
            halt(1)
    end.


set_flags(Options, NonOptionArgs) ->
    %% Cycle through flags and call handler functions
    %% foreach Flag in the properly list, call Flag()
    [?MODULE:KeyFun([proplists:get_value(KeyFun, Options)], NonOptionArgs) || KeyFun <- proplists:get_keys(Options)].


%%
%% Command handlers
%%

%% -i or --vm
vm (Args, _N) ->
    [VM|_] = Args,
    lager:debug("In vm with args: ~p", [VM]),
    lager:warning("Alternate VM backends not yet supported"),
    sb:set_config(vm, VM).

%% -h or --help
help (Args, _N) ->
    lager:debug("In help with args: ~p", Args),
    show_usage(),
    halt().

%% -d or --debug
debug (_A, _N) ->
    lager:set_loglevel(lager_console_backend, debug),
    lager:debug("Debug output enabled").

%% -f or --config_file
config_file(Args, _N) ->
    lager:debug("In config_file with args: ~p", Args),
    case sb:load_config(Args) of
        ok ->
            sb:set_config(config_file, Args);
        {error, enoent} ->
            lager:error("Config file ~p does not exist, exiting", Args),
            halt(1);
        {error, _} ->
            lager:error("Cannot open config file, exiting"),
            halt(1)
    end.

%% -n or --count
count(Args, _N) ->
    lager:debug("In count with args: '~p'", Args),
    sb:set_config(count, Args).


%% the verb to run
cmd (Command, Args) ->
    lager:debug("In cmd with command: ~p and args: ~p", [Command, Args]),
    [Com|_] = Command,
    case {Com, Args} of
        %% Ensure the `get` command is followed by a config file describing type of environment
        %% to create
        {"get", undefined} ->
            lager:error("The get command requires a harness to be specified");
        {"get", EnvFile } ->
            ok = sb:set_config(command, get),
            ok = sb:set_config(command_args, EnvFile);
        {"list", _A} ->
            ok = sb:set_config(command, list);
        {NoneCommand, _A} ->
            lager:error("Command ~p not found", [NoneCommand]),
            show_usage(),
            halt(1)
    end.


%% show help
show_usage () ->
    getopt:usage(command_line_options(),"stableboy").


%%
%% Getopt helpers
%%

%% available command line options
command_line_options () ->
    %% Option Name, Short Code, Long Code, Argument Spec, Help Message
    [
     {help, $h, "help", undefined, "Show available commands"},

     {vm, $i, "vm", {atom, sb_file},
         "Specify the virtual machine interface to use [sb_file(default)|sb_vbox|tbd]"},

     {debug, $d, "debug", undefined, "Print extra debug output"},

     {config_file, $f, "config_file", {string, filename:join([os:getenv("HOME"), ".stableboy"])},
          "Config file to use (defaults to .stableboy then ~/.stableboy)"},

     {count, $n, "count", {integer, 1}, "The number of harnesses to return (used for 'get' command)"},

     {cmd, undefined, undefined, string, "The command to run [list|get|tbd]"}
    ].


%%
%% Command Runner
%%

%% Entry point for the command being run
run_command() ->
    Command = sb:get_config(command),
    CommandArgs = sb:get_config(command_args, undefined),
    lager:debug("Running command: ~p with args: ~p", [Command, CommandArgs]),

    % Run the command against the proper backend
    Backend = sb:get_config(vm),
    lager:debug("Using vm backend: ~p", [Backend]),

    % Call the function associated with the command name
    %   in the case of calling ./stableboy list (with vm=sb_file as the default)
    %   this will call sb_file:list()
    case CommandArgs of
        undefined ->
            Backend:Command();
        _Anything  ->
            Backend:Command(CommandArgs)
    end.





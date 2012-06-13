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

%% escript entry point
main(Args) ->
    start_lager(),
    case getopt:parse(cli_options(), Args) of
        {ok, {Opts, CmdArgs}} ->
            maybe_set_debug(Opts),
            lager:debug("Parsed CLI:~n  options: ~p~n  args: ~p~n", [Opts, CmdArgs]),
            case process_cli(Opts, CmdArgs) of
                help ->
                    help();
                list ->
                    list_vms();
                {get, GetArgs} ->
                    get_vms(GetArgs);
                {brand, BrandArgs} ->
                    brand_vm(BrandArgs);
                undefined ->
                    io:format("ERROR: No command given!~n~n"),
                    help(),
                    halt(1)
            end;
        {error, Error} ->
            io:format("Invalid option sequence given: ~p~n~n", [Error]),
            help(),
            halt(1)
    end.

cli_options() ->
    %% Option Name, Short Code, Long Code, Argument Spec, Help Message
    [
     {help,        $h, "help",   undefined, "Print this usage page"},
     {vm,          $i, "vm",     atom,      "Specify the virtual machine interface to use [sb_file(default)|sb_vbox|sb_smartos]"},
     {debug,       $d, "debug",  undefined, "Print extra debug output"},
     {config,      $f, "config", {string, filename:join([os:getenv("HOME"), ".stableboy"])},
      "Config file to use (defaults to .stableboy then ~/.stableboy)"},
     {count,       $n, "count", {integer, 1}, "The number of harnesses to return (used for 'get' command)"}
    ].

cli_args() ->
    [{" "," "},
     {"Commands:", " "},
     {" ", " "},
     {"help", "Print this usage page"},
     {"list", "List the available VMs"},
     {"get (<name>|<file>)",  "List matching VMs and their login credentials:"},
     {" ", "  <name> for the named VM."},
     {" ", "  <file> for VMs matching the Erlang terms in the named file."},
     {"brand <name> <platform:version:arch[:user:password]>",
      "Set metadata for named VM (supported by non-sb_file backends)"},
     {" ", " <name> of the named VM."},
     {" ", " <platform> of VM, e.g. ubuntu, centos, fedora, osx."},
     {" ", " <version> of VM, e.g. 12.04"},
     {" ", " <arch> of VM, (32|64)."},
     {" ", " <user> username for ssh login of VM."},
     {" ", " <password> password for ssh login of VM."}
    ].

%%------------------
%% Commands
%%------------------

%% The 'help' command/switch
help() ->
    getopt:usage(cli_options(), escript:script_name(), "help | list | get (<name>|<file>)", cli_args()),
    io:format("~nGiven configuration:~n~n~p~n", [application:get_all_env(stableboy)]).

%% The 'list' command
list_vms() ->
    Backend = get_backend(),

    lager:debug("Running command 'list'"),
    Backend:list().

%% The 'get' command
get_vms(Args) ->
    Backend = get_backend(),
    lager:debug("Running command 'get' with args: ~p", [Args]),
    Backend:get(Args).

%% The 'brand' command
brand_vm(Args) ->
    Backend = get_backend(),
    lager:debug("Running command 'brand' with args: ~p", [Args]),
    Backend:brand(Args).

%%-----------------------------
%% Internal functions
%%-----------------------------

%% Special handling of lager startup
start_lager() ->
    application:load(lager),
    application:set_env(lager, handlers, [{lager_console_backend, error}]),
    application:start(lager).

%% Set the logging level at debug if the CLI flag is present, called
%% before any real processing is done.
maybe_set_debug(Opts) ->
    case lists:member(debug, Opts) of
        true -> lager:set_loglevel(lager_console_backend, debug);
        _ -> ok
    end.

%% Get whatever VM backend was set
get_backend() ->
    Backend = sb:get_config(vm, sb_file),
    case code:ensure_loaded(Backend) of
        {module, _Mod} ->
            lager:debug("Using vm backend ~p", [Backend]),
            Backend;
        {error, Reason} ->
            lager:error("Error loading VM backend ~p: ~p~n", [Backend, Reason]),
            halt(1)
    end.

%% Processes CLI options and non-option arguments
process_cli(Opts, NonOptArgs) ->
    process_cli(Opts, NonOptArgs, []).

process_cli([], Args, Result) ->
    process_args(Args, Result);
process_cli([H|T], Args, Result) ->
    process_cli(T, Args, process_option(H, Args, Result)).


%% Processes CLI options (flags)
process_option(help, _Args, Result) ->
    %% Always override the command!
    NewResult = proplists:delete(command, Result),
    [{command,help}|NewResult];
process_option({vm,Backend}, _Args,Result) ->
    %% Override the VM provider
    NewResult = proplists:delete(vm, Result),
    [{vm,Backend}|NewResult];
process_option({config, Filename}, _Args, Result) ->
    case sb:load_config(Filename) of
        ok -> Result;
        {error, _Reason} ->
            halt(1)
    end;
process_option({count, N},["get"|_], Result) ->
    %% If the command is not 'get', we don't care about --count option
    NewResult = proplists:delete(count, Result),
    [{count, N}|NewResult];
process_option(Option, _Args, Result) ->
    %% Ignore anything else.
    lager:debug("Ignoring option: ~p~n", [Option]),
    Result.


%% Processes non-option arguments.
process_args([], Result) ->
    [ sb:set_config(Key,Value) || {Key, Value} <- Result ],
    proplists:get_value(command, Result);
process_args(["help"|_], Result) ->
    NewResult = proplists:delete(command, Result),
    process_args([], [{command,help}|NewResult]);
process_args(["get"|Extra], Result) ->
    %% Don't override the 'help' flag
    case proplists:is_defined(command, Result) of
        true ->
            process_args([], Result);
        false ->
            case Extra of
                [] ->
                    lager:error("The get command requires environment file or harness name to be specified"),
                    halt(1);
                _ ->
                    process_args([], [{command, {get, Extra}}|Result])
            end
    end;
process_args(["brand"|Extra], Result) ->
    %% Don't override the 'help' flag
    case proplists:is_defined(command, Result) of
        true ->
            process_args([], Result);
        false ->
            case length(Extra) of
                2 ->
                    process_args([], [{command, {brand, Extra}}|Result]);
                _ ->
                    lager:error("The brand command requires a VM-name and metadata parameters."),
                    halt(1)
            end
    end;
    
process_args(["list"|_], Result) ->
    case proplists:is_defined(command, Result) of
        true ->
            process_args([], Result);
        false ->
            process_args([], [{command, list}|Result])
    end.

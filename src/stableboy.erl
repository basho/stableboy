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
            do_command(process_cli(Opts, CmdArgs));
        {error, Error} ->
            io:format("Invalid option sequence given: ~p~n~n", [Error]),
            do_command(help),
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
     {count,       $n, "count", {integer, undefined}, "The number of harnesses to return (used for 'list' command)"},
     {force,       $F, "force", undefined, "When creating a snapshot, overwrite any existing snapshot"}
    ].

cli_args(Configs) ->
    cli_args() ++
        [{" "," "},
         {"Given configuration:", " "},
         {" "," "}] ++
        [ {atom_to_list(K), io_lib:format("~p", [V])} || {K,V} <- Configs ].

cli_args() ->
    [{" "," "},
     {"Commands:", " "},
     {" ", " "},
     {"help", "Print this usage page"},
     {"list [<file>]", "List the available VMs"},
     {" ", "  <file> for VMs matching the Erlang terms in the named file."},
     {"get [<name> ...])", "List named VMs and their login credentials:"},
     {" ", "  <name> VM names/aliases"},
     {"brand <name> <platform:version:arch[:user:password]>",
      "Set metadata for named VM (supported by non-sb_file backends)"},
     {" ", "  <name> of the named VM."},
     {" ", "  <platform> of VM, e.g. ubuntu, centos, fedora, osx."},
     {" ", "  <version> of VM, e.g. 12.04"},
     {" ", "  <arch> of VM, (32|64)."},
     {" ", "  <user> username for ssh login of VM."},
     {" ", "  <password> password for ssh login of VM."},
     {"snapshot <name>", "Create a snapshot for the named VM."},
     {"rollback <name>", "Rollback the VM to the existing snapshot"}
    ].

%%------------------
%% Commands
%%------------------

%% The 'help' command/switch
do_command(help) ->
    getopt:usage(cli_options(), escript:script_name(), "help | list | get (<name>|<file>)",
                 cli_args(application:get_all_env(stableboy)));

%% The 'list' command
do_command({list, Args}) ->
    Backend = get_backend(),
    lager:debug("Running command 'list'"),
    Backend:list(Args);

%% The 'get' command
do_command({get, Names}) ->
    Backend = get_backend(),
    lager:debug("Running command 'get'"),
    Backend:get(Names);

%% The 'brand' command
do_command({brand, Name, Meta}) ->
    Backend = get_backend(),
    lager:debug("Running command 'brand'"),
    Backend:brand(Name, Meta);

%% The 'snapshot' command
do_command({snapshot, Name}) ->
    Backend = get_backend(),
    lager:debug("Running command 'snapshot'"),
    Backend:snapshot(Name);

%% The 'rollback' command
do_command({rollback, Name}) ->
    Backend = get_backend(),
    lager:debug("Running command 'rollback'"),
    Backend:rollback(Name);

%% User didn't pass a command, print an error and the help.
do_command(undefined) ->
    io:format("ERROR: No command given!~n~n"),
    do_command(help),
    halt(1);

%% When user gives unknown command, print an error and the help
do_command(Bad) ->
    io:format("ERROR: Bad command given ~p!~n~n", [Bad]),
    do_command(help),
    halt(1).

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
process_option({count, N},["list"|_], Result) ->
    %% If the command is not 'list', we don't care about --count option
    NewResult = proplists:delete(count, Result),
    [{count, N}|NewResult];
process_option(force, ["snapshot"|_], Result) ->
    [{force,true}|Result];
process_option(Option, _Args, Result) ->
    %% Ignore anything else.
    lager:debug("Ignoring option: ~p~n", [Option]),
    Result.


%% Processes non-option arguments.
process_args([], Result) ->
    [ sb:set_config(Key,Value) || {Key, Value} <- Result ],
    proplists:get_value(command, Result);
process_args([Command0|Args], Result) ->
    Command = try
                  list_to_existing_atom(Command0)
              catch
                  _:badarg -> do_command(Command0) %% This is an unknown command, bail
              end,
    case (not proplists:is_defined(command, Result)
          orelse overrides_command(Command)) of
        false ->
            process_args([], Result);
        true ->
            NewResult = proplists:delete(command, Result),
            case validate_command(Command, Args) of
                ok ->
                    process_args([], [{command, construct_command(Command, Args)}|NewResult]);
                {error, Reason} ->
                    lager:error(Reason),
                    halt(1)
            end
    end.

%% Help always wins!
overrides_command(help) -> true;
overrides_command(_) -> false.

%% Validate number of arguments to commands
validate_command(list, []) -> ok;
validate_command(list, [_Filename]) -> ok;
validate_command(list, _) -> {error, "Too many arguments for the list command, which requires nothing or an environment file."};
validate_command(rollback, [_Name]) -> ok;
validate_command(rollback, _) -> {error, "The rollback command requires a single VM name"};
validate_command(snapshot, [_Name]) -> ok;
validate_command(snapshot, _) -> {error, "The snapshot command requires a single VM name"};
validate_command(get, []) -> {error, "The get command requires a list of harness names."};
validate_command(get, _) -> ok;
validate_command(brand,[_VM,_Meta]) -> ok;
validate_command(brand,_) -> {error, "The brand command requires a VM-name and metadata parameters."};
validate_command(_,_) -> ok.

%% Turns command and CLI args into symbolic command
construct_command(rollback, [Name]) -> {rollback, Name};
construct_command(snapshot, [Name]) -> {snapshot, Name};
construct_command(get, Names) -> {get, Names};
construct_command(help, _) -> help;
construct_command(list, Args) -> {list, Args};
construct_command(brand, [VM,Meta]) -> {brand, VM, Meta}.

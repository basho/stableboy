%%
%% SmartOS backend for Stableboy.
%% Configuration needed:
%%
%%    {vm, sb_smartos}. %% Or use `-i sb_smartos`
%%    {vm_host, "smartos.local"}. %% Host/IP of global zone
%%    {vm_port, 22}.  %% Defaults to 22, SSH port of global zone
%%    {vm_user, "root"}. %% defaults to "root"
%%    {vm_pass, "pass"}. %% Root password of global zone
%%
%% Usable VMs should be created with some tags that essentially are
%% the config info for the list/0 and get/1 commands.
%%
%% Example:
%%
%% {"tags":{
%%    "platform":"ubuntu",
%%    "version":"12.4.0",
%%    "architecture":"64",
%%    "user":"user",
%%    "password":"password"
%%  }}
%%
%% Tags should be set on VM creation, or set via the `vmadm update`
%% command.
%%
%% To get this working, I had to adjust the available SSH ciphers
%% since Erlang only supports some less-secure ones, namely aes128-cbc
%% and 3des-cbc. Add this line to the end of /etc/ssh/sshd_config in
%% the global zone, and then run svcadm refresh ssh:
%%
%% Ciphers aes128-ctr,aes192-ctr,aes256-ctr,arcfour128,arcfour256,arcfour,aes128-cbc,3des-cbc

-module(sb_smartos).
-behaviour(stableboy_vm_backend).

-export([list/1,
         get/1,
         snapshot/1,
         rollback/1,
         brand/2]).

%% Command macros for global zone
-define(LISTCMD, "for vm in `vmadm lookup`; do vmadm get $vm | json -o json-0 alias nics tags; done").
-define(STARTCMD(Alias), io_lib:format("vmadm lookup state=stopped alias=\"~s\"| xargs -n1 vmadm start",[Alias])).
-define(BRANDCMD, "vmadm update `vmadm lookup alias=~s` -f ~s").
-define(LOOKUPCMD(N), io_lib:format("vmadm lookup alias=~s",[N])).
-define(STOPCMD(U), io_lib:format("vmadm stop ~s",[U])).
-define(FORCESTOPCMD(U), io_lib:format("~s -f", [?STOPCMD(U)])).
-define(ROLLBACKCMD(U), io_lib:format("zfs rollback ~s", [snapshot_name(U)])).
-define(DETECTDISKCMD(U), io_lib:format("vmadm get ~s | json disks.0.zfs_filesystem zfs_filesystem", [U])).
-define(DETECTSNAPCMD(U), io_lib:format("zfs list -t snapshot ~s", [snapshot_name(U)])).
-define(DETECTSTOPCMD(U), io_lib:format("vmadm list state=stopped | grep ~s", [U])).
-define(DESTROYSNAPCMD(U), io_lib:format("zfs destroy ~s",[snapshot_name(U)])).
-define(CREATESNAPCMD(U), io_lib:format("zfs snapshot ~s",[snapshot_name(U)])).

%% @doc Lists available VMs with platform/version/architecture information.
list([]) ->
    lager:debug("In sb_smartos:list (unfiltered)"),
    sb_vm_common:list_by_properties([{platform, '*'}], gzcommand(?LISTCMD, fun format_list/1));
list([Filename]) ->
    lager:debug("In sb_smartos:list (filename)"),
    sb_vm_common:list_by_file(Filename, gzcommand(?LISTCMD, fun format_list/1)).

%% @doc Gets login information about VMs by name
get([]) ->
    ok;
get([Name|Rest]) ->
    ok = get_by_name(Name),
    ?MODULE:get(Rest).

%% @doc Snapshot a VM(s)
snapshot(Alias) ->
    lager:debug("In sb_smartos:snapshot/1"),
    Force = sb:get_config(force, false),
    UUID = uuid(Alias),
    Exists = snapshot_exists(UUID),
    case Force orelse not Exists of
        false ->
            %% Don't create a new one if it exists
            io:format("Snapshot for ~s already exists.~n", [Alias]),
            ok;
        true ->
            gzcommand(?STOPCMD(UUID)),
            ok = wait_for_stop(UUID),
            case create_snapshot(UUID, Exists) of
                {error, Reason} ->
                    lager:error("sb_smartos: snapshot creation failed with ~p", [Reason]),
                    halt(1);
                _ ->
                    io:format("Successfully created snapshot for ~s.~n", [Alias]),
                    ok
            end
    end.

%% @doc Rollback a VM(s), not yet implemented.
rollback(Alias) ->
    lager:debug("In sb_smartos:rollback/1"),
    UUID = uuid(Alias),
    case snapshot_exists(UUID) of
        false ->
            lager:error("sb_smartos: No snapshot exists for ~s, cannot rollback!", [Alias]),
            halt(1);
        true ->
            gzcommand(?FORCESTOPCMD(UUID)),
            ok = wait_for_stop(UUID),
            case gzcommand(?ROLLBACKCMD(UUID)) of
                {error, Reason} ->
                    lager:error("sb_smartos: Rollback failed for ~s with ~p", [Alias, Reason]),
                    halt(1);
                _ ->
                    io:format("Successfully rolled back VM ~s.", [Alias]),
                    ok
            end
    end.

%% @doc Brands a VM with given metadata.
brand(Alias, Meta) ->
    lager:debug("In sb_smartos:brand/1"),
    Props = sb_vm_common:extract_branding(Meta),
    Data = json2:encode({struct, [{set_tags, Props}]}),
    Filename = json_temp_name(Alias),
    lager:debug("Uploading JSON to ~s: ~s", [Filename, iolist_to_binary(Data)]),
    gzupload(Filename, Data),
    lager:debug("Setting tags on ~s", [Alias]),
    Output = gzcommand(io_lib:format(?BRANDCMD, [Alias, Filename])),
    lager:debug("Branding result: ~p", [Output]),
    ok.

%%-------------------
%% Internal functions
%%-------------------

%% @doc Map alias->UUID and memoize result in pdict.
uuid(Alias) ->
    case erlang:get({uuid, Alias}) of
        undefined ->
            case gzcommand(?LOOKUPCMD(Alias)) of
                {error, Reason} ->
                    lager:error("sb_smartos: VM lookup of ~s failed with ~p:", [Alias, Reason]),
                    halt(1);
                <<>> ->
                    lager:error("sb_smartos: no VM named ~s", [Alias]),
                    halt(1);
                UUID0 ->
                    {match, [UUID]} = re:run(UUID0, "[-/\\w]+", [{capture, first, binary}]),
                    erlang:put({uuid, Alias}, UUID),
                    UUID
            end;
        U -> U
    end.

%% @doc Detect whether a snapshot exists for a given VM UUID
snapshot_exists(UUID) ->
    case gzcommand(?DETECTSNAPCMD(UUID)) of
        {error, _} -> false;
        _ -> true
    end.

%% @doc Waits for a VM to get to the stopped state.
wait_for_stop(UUID) ->
    %% Wait a maximum of 60 secs
    wait_for_stop(UUID, sb:get_config(smartos_stop_timeout, 60)).

wait_for_stop(_UUID, 0) ->
    {error, timeout};
wait_for_stop(UUID, Count) ->
    case gzcommand(?DETECTSTOPCMD(UUID)) of
        {error, _} ->
            timer:sleep(1000),
            wait_for_stop(UUID, Count - 1);
        _ ->
            ok
    end.

%% @doc Creates a snapshot, deleting one if it already exists.
create_snapshot(UUID, true) ->
    lager:debug("sb_smartos: destroying existing ZFS snapshot for ~s", [UUID]),
    gzcommand(?DESTROYSNAPCMD(UUID)),
    create_snapshot(UUID, false);
create_snapshot(UUID, false) ->
    lager:debug("sb_smartos: creating ZFS snapshot for ~s", [UUID]),
    gzcommand(?CREATESNAPCMD(UUID)).


snapshot_name(UUID) ->
    io_lib:format("~s@stableboy", [disk(UUID)]).

%% @doc Get the disk name for a VM UUID and memoize it in the pdict.
disk(UUID) ->
    case erlang:get({disk, UUID}) of
        undefined ->
            case gzcommand(?DETECTDISKCMD(UUID)) of
                {error, _} ->
                    lager:error("sb_smartos: Could not detect filesystem of VM ~s", [UUID]),
                    halt(1);
                Name0 ->
                    {match, [Name]} = re:run(Name0, "[-/\\w]+", [{capture, first, binary}]),
                    erlang:put({disk, UUID}, Name),
                    Name
            end;
        D -> D
    end.

%% @doc Given a base name, generates a file that can be uploaded to in
%% a temporary place.
json_temp_name(Alias) ->
    {Mega,Secs,Micros} = erlang:now(),
    io_lib:format("/tmp/~s-~p~p~p.json", [Alias, Mega, Secs, Micros]).

%% @doc Executes a command in the global zone via SSH and returns the
%% shell output.
%% @equiv gzcommand(Command, fun format_identity/1)
gzcommand(Command) ->
    gzcommand(Command, fun format_identity/1).

%% @doc Executes a command in the global zone via SSH. The Callback is
%% called with the stdout stream so that the data can be formatted
%% before being returned.
gzcommand(Command, Callback) ->
    with_connection(
      fun(Connection) ->
              case ssh_cmd:run(Connection, Command) of
                  {ok, {_,StdOut,_}} ->
                      Callback(StdOut);
                  {error,{Code,_,StdErr}} ->
                      lager:debug("SSH Command failed with code ~p: ~p~n", [Code, StdErr]),
                      {error, {Code, StdErr}}
              end
      end).

%% @doc Writes data into the named file on the global zone.
gzupload(Filename, Data) ->
    with_connection(
      fun(Conn) ->
              case ssh_sftp:start_channel(Conn) of
                  {ok, Channel} ->
                      ssh_sftp:write_file(Channel, Filename, Data);
                  Error -> Error
              end
      end).


%% @doc Passes an SSH connection to the global zone to the Function.
with_connection(Function) ->
    Function(gzconnection()).

%% @doc Gets the existing SSH connection or opens a new one.
gzconnection() ->
    case erlang:get(gz_conn) of
        undefined ->
            Host = sb:get_config(vm_host),
            Port = sb:get_config(vm_port, 22),
            User = sb:get_config(vm_user, "root"),
            Pass = sb:get_config(vm_pass),
            application:start(crypto),
            application:start(ssh),
            lager:debug("Connecting to SmartOS GZ: ~s:~p ~s:~s", [Host, Port, User, Pass]),
            case ssh:connect(Host, Port,[{user,User},{password,Pass},{silently_accept_hosts,true}]) of
                {ok, Connection} ->
                    erlang:put(gz_conn, Connection),
                    Connection;
                {error, Reason} ->
                    lager:error("SSH connection failed with reason: ~p~n", [Reason]),
                    halt(1)
            end;
        C -> C
    end.

%% @doc Get a VM by name (alias).
get_by_name(Alias) ->
    UUID = uuid(Alias),
    Command = io_lib:format("vmadm get ~s | json -o json-0 alias nics tags", [UUID]),
    case gzcommand(Command, fun format_get/1) of
        {error, _Reason} -> %% TODO: print something?
            ok;
        VMs ->
            gzcommand(?STARTCMD(Alias), fun started_vm/1),
            case sb_vm_common:wait_for_ssh_port(hd(VMs)) of
                ok ->
                    sb_vm_common:print_result(VMs);
                {error, Reason} ->
                    lager:error("VM did not open SSH port within allotted time: ~p", [Reason]),
                    halt(1)
            end
    end,
    ok.

%% @doc Format output for the 'list' command into Erlang terms.
format_list(Output) ->
    JSONToVM = fun({struct, JSON}) ->
                       Alias = to_list(proplists:get_value(<<"alias">>, JSON)),
                       {struct, Tags} = proplists:get_value(<<"tags">>, JSON),
                       Platform = to_atom(proplists:get_value(<<"platform">>, Tags)),
                       Version = version_to_intlist(proplists:get_value(<<"version">>, Tags)),
                       Arch = to_integer(proplists:get_value(<<"architecture">>, Tags)),
                       {Alias,Platform,Version,Arch}
               end,
    [ JSONToVM(json2:decode(V)) || V <- re:split(Output, "\n", [{return, binary}, trim]) ].

%% @doc Format output for the 'get' command into Erlang terms.
format_get(Output) ->
    JSONToDetails = fun({struct, JSON}) ->
                            Alias = to_list(proplists:get_value(<<"alias">>, JSON)),
                            {struct, Tags} = proplists:get_value(<<"tags">>, JSON),
                            NICS = proplists:get_value(<<"nics">>, JSON),
                            {struct, NIC} = hd(NICS),
                            IP = to_list(proplists:get_value(<<"ip">>, NIC)),
                            User = to_list(proplists:get_value(<<"user">>, Tags)),
                            Password = to_list(proplists:get_value(<<"password">>, Tags)),
                            Port = 22,
                            {Alias,IP,Port,User,Password}
                    end,
    [ JSONToDetails(json2:decode(V)) || V <- re:split(Output, "\n", [{return, binary}, trim]) ].

%% @doc Don't format anything.
format_identity(Output) ->
    Output.

%% @doc Callback for result of starting a VM
started_vm(Output) ->
    lager:debug("Started VM! ~s", [Output]).

%% @doc Converts a dot-delimited version string into a list of version integers.
version_to_intlist(undefined) ->
    undefined;
version_to_intlist(V) ->
    [ list_to_integer(P) || P <- re:split(V, "[.]", [{return,list},trim]) ].

%%
%% Pessimistic conversion routines for vmadm output
%%
to_integer(undefined) ->
    undefined;
to_integer(L) when is_list(L) ->
    list_to_integer(L);
to_integer(B) when is_binary(B) ->
    list_to_integer(binary_to_list(B)).

to_atom(A) when is_atom(A) ->
    A;
to_atom(L) when is_list(L) ->
    list_to_atom(L);
to_atom(B) when is_binary(B) ->
    binary_to_atom(B, utf8).

to_list(undefined) ->
    undefined;
to_list(Bin) when is_binary(Bin) ->
    binary_to_list(Bin).
